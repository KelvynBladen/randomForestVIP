#' Small Multiple PDPs and Importance Metrics
#' @name pdp_compare
#' @importFrom dplyr arrange desc filter select %>%
#'   summarise group_by left_join case_when
#' @importFrom ggplot2 ggplot geom_point geom_line xlab theme theme_bw
#'   scale_y_continuous aes facet_wrap guides geom_smooth
#' @importFrom gridExtra grid.arrange
#' @importFrom stats model.frame getCall mad sd
#' @importFrom pdp partial
#' @importFrom tidyr all_of
#' @importFrom trelliscopejs facet_trelliscope
#' @importFrom randomForest importance
#' @importFrom gbm relative.influence
#' @description This function takes a randomForest object, generates partial
#'   dependence plots for predictors and converts them to small multiples for
#'   appropriate comparison. Output is a list containing a comparative grid
#'   of PDPs, individual partial dependence plots, and PDP-derived
#'   importance values for assessing effect of predictors on response.
#' @param x An object of class randomForest.
#' @param var_vec Optional vector argument for reducing the number of
#'   variables to consider and compare. Elements should be characters that
#'   match column names from the data used to generate the model x.
#' @param scale For permutation based measures such as MSE or Accuracy, should
#'   the measures be divided by their "standard errors"? Default is FALSE.
#' @param sqrt Boolean value indicating whether importance metrics should be
#'   adjusted via a square root transformation. Default is True.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from
#'   each end of an individual PDP dataset before the trim-range is computed.
#'   The default of 0.1 will be used when values of trim outside that range
#'   are given.
#' @param trellis Logical indicating whether or not to generate trellis plots
#'   as output for comparing PDPs. Default is TRUE.
#' @param which_class Integer specifying which column of the matrix of
#'   predicted probabilities to use as the "focus" class. Default is to use
#'   the first class. Only used for classification problems.
#' @param prob Logical indicating whether or not partial dependence for
#'   classification problems should be returned on the probability scale,
#'   rather than the centered logit. If FALSE, the partial dependence
#'   function is on a scale similar to the logit. Default is TRUE.
#' @param ... Other parameters to pass to the partial function.
#' @return A list of partial dependence plots with adjusted y-axes so all
#'   are on an identical scale. This list includes a comparative facet plot and
#'   pdp importance values for assessing true affect of predictors on response.
#' @examples
#' mtcars.rf <- randomForest::randomForest(formula = mpg ~ ., data = mtcars)
#' car_pd <- pdp_compare(x = mtcars.rf)
#' car_pd$full
#' car_pd$imp
#' gridExtra::grid.arrange(car_pd$wt, car_pd$disp,
#'   car_pd$hp, car_pd$cyl, nrow = 2)
#' @export

pdp_compare <- function(x, var_vec, scale = FALSE, sqrt = TRUE,
                        trim = 0.1, trellis = TRUE,
                        which_class = 2L, prob = TRUE, ...) {
  data <- eval(getCall(x)$data)
  model_frame <- model.frame(getCall(x)$formula, data = data)[-1]

  if (!missing(var_vec)) {
    model_frame <- model_frame %>% dplyr::select(all_of(var_vec))
  }

  if (inherits(x, "randomForest")) {
    im <- as.data.frame(randomForest::importance(x, scale = scale))
    im$var <- rownames(im)
    im <- im[(length(im) - 2):length(im)]
  }

  if (inherits(x, "gbm")) {
    im <- as.data.frame(gbm::relative.influence(x))
    colnames(im) <- "importance"
    im$var <- rownames(im)
  }

  imp0 <- im
  imp0[, ] <- 0
  im <- pmax(im, imp0)

  im[-length(im)] <- ifelse(sqrt == TRUE,
                            sqrt(im[-length(im)]),
                            im[-length(im)])

  trim <- ifelse(!between(trim, 0, 0.5), 0.1, trim)

  vvec <- colnames(model_frame)

  pd_num <- NULL
  pd_fac <- NULL
  for (i in vvec) {
    ifelse(inherits(x, "gbm"),
      tmp <- pdp::partial(x,
        pred.var = i, which.class = which_class,
        prob = prob, n.trees = x$n.trees
      ),
      tmp <- pdp::partial(x,
        pred.var = i, which.class = which_class,
        prob = prob
      )
    ) # , ...)
    # rename(x = i, y = yhat) %>% mutate( = i)
    names(tmp) <- c("x", "y")

    ifelse(inherits(tmp$x, "numeric"),
           pd_num <- rbind(pd_num, cbind(tmp, var = i)),
           pd_fac <- rbind(pd_fac, cbind(tmp, var = i)))
  }

  pd <- as.data.frame(rbind(pd_num[-1], pd_fac[-1]))

  imp <- pd %>%
    group_by(var) %>%
    arrange(y) %>%
    summarise(
      range = max(y) - min(y),
      trim_range = y[length(y) - floor(length(y) * trim)] -
        y[floor(length(y) * trim) + 1],
      sd = sd(y),
      mad = stats::mad(y, center = mean(y))
    ) %>%
    arrange(desc(trim_range), desc(sd))

  ifelse(exists("im"),
         imp <-  dplyr::left_join(imp, im, by = c("var" = "var")),
         imp <- imp)

  vvec <- unique(imp$var)

  # new
  u <- max(pd[1])
  l <- min(pd[1])
  rr <- u - l
  v <- 10^(-3:6)

  indu <- findInterval(abs(u), v)
  newu <- u / (10^(indu - 6))
  ru <- ceiling(newu / 10) * 10

  indl <- findInterval(abs(l), v)
  newl <- l / (10^(indl - 6))
  rl <- floor(newl / 10) * 10

  nu <- ru * (10^(indu - 6))
  nl <- rl * (10^(indl - 6))
  nrr <- nu - nl

  if (any(c(rr / nrr, nl / l, u / nu) < 3 / 4)) {
    ru <- ceiling(newu / 4) * 4
    rl <- floor(newl / 4) * 4
    nu <- ru * (10^(indl - 6))
    nl <- rl * (10^(indl - 6))
    nrr <- nu - nl
  }

  div <- case_when(
    (nrr / 5) %% 5 == 0 ~ 5,
    (nrr / 5) %% 4 == 0 ~ 4,
    (nrr / 5) %% 3 == 0 ~ 3,
    .default = 4
  )

  gl <- list()

  nvec <- vector(length = 0)
  fvec <- vector(length = 0)
  if (length(pd_num) > 1) {
    pd_num <- dplyr::left_join(pd_num, imp, by = "var") %>%
      arrange(desc(range)) %>%
      dplyr::select(!(range:mad))

    nvec <- unique(pd_num$var)

    pd_num <- arrange(transform(pd_num,
                                var = factor(var, levels = nvec)),
                      var)

    ng <- ggplot(pd_num, aes(x, y)) +
      geom_line() +
      facet_wrap(~var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(
        limits = c(nl, nu),
        breaks = seq(nl, nu, by = nrr / div)
      )

    gl$full_num <- ng
    if (trellis) {
      nc <- nchar(length(unique(pd_num$var)))
      s <- formatC(as.numeric(pd_num$var),
        width = nc,
        format = "d", flag = "0"
      )

      pd_num$imp <- as.factor(paste0("v", s, "_", pd_num$var))

      nt <- ggplot(pd_num, aes(x, y)) +
        geom_line() +
        facet_trelliscope(~imp, scales = "free_x") + # ...
        theme_bw() +
        scale_y_continuous(
          limits = c(nl, nu),
          breaks = seq(nl, nu, by = nrr / div)
        )
      gl$trellis_num <- nt
    }
  }

  if (length(pd_fac) > 1) {
    pd_fac <- dplyr::left_join(pd_fac, imp, by = "var") %>%
      arrange(desc(range)) %>%
      dplyr::select(!(range:mad))

    fvec <- unique(pd_fac$var)

    pd_fac <- arrange(transform(pd_fac,
                                var = factor(var, levels = fvec)),
                      var)

    fg <- ggplot(pd_fac, aes(x, y)) +
      geom_point() +
      facet_wrap(~var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(
        limits = c(nl, nu),
        breaks = seq(nl, nu, by = nrr / div)
      )

    gl$full_fac <- fg

    if (trellis) {
      nc <- nchar(length(unique(pd_fac$var)))
      s <- formatC(as.numeric(pd_fac$var),
        width = nc,
        format = "d", flag = "0"
      )

      pd_fac$imp <- as.factor(paste0("v", s, "_", pd_fac$var))

      ft <- ggplot(pd_fac, aes(x, y)) +
        geom_point() +
        facet_trelliscope(~imp, scales = "free_x") + # ...
        theme_bw() +
        scale_y_continuous(
          limits = c(nl, nu),
          breaks = seq(nl, nu, by = nrr / div)
        )

      gl$trellis_fac <- ft
    }
  }

  gl$imp <- imp

  j <- length(gl) + 1
  for (i in vvec) {
    if (i %in% nvec) {
      v <- pd_num %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        xlab(i) +
        scale_y_continuous(
          limits = c(nl, nu),
          breaks = seq(nl, nu, by = nrr / div)
        ) +
        guides(size = "none") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 14, face = "bold")
        ) +
        ggeasy::easy_center_title() +
        ggeasy::easy_plot_legend_size(size = 11)

      g1 <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        xlab(i) +
        guides(size = "none") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 14, face = "bold")
        ) +
        ggeasy::easy_center_title() +
        ggeasy::easy_plot_legend_size(size = 11)
    }

    if (i %in% fvec) {
      v <- pd_fac %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_point() +
        xlab(i) +
        scale_y_continuous(
          limits = c(nl, nu),
          breaks = seq(nl, nu, by = nrr / div)
        ) +
        guides(size = "none") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 14, face = "bold")
        ) +
        ggeasy::easy_center_title() +
        ggeasy::easy_plot_legend_size(size = 11)

      g1 <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        xlab(i) +
        guides(size = "none") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 14, face = "bold")
        ) +
        ggeasy::easy_center_title() +
        ggeasy::easy_plot_legend_size(size = 11)
    }

    gl$j <- g
    names(gl)[j] <- i
    j <- j + 1

    gl$j <- g1
    names(gl)[j] <- paste0("zoom_", i)
    j <- j + 1
  }

  greg <- gregexec("zoom.*", names(gl))
  gmatch <- regmatches(names(gl), greg)
  gl1 <- gl[names(gl)[!names(gl) %in% gmatch]]
  gl2 <- gl[names(gl)[names(gl) %in% gmatch]]

  gl <- c(gl1, gl2)
  gl
}

#######################
# library(ggplot2)
# library(dplyr)
# mtcars_rf <- randomForest(formula = mpg ~ ., data = mtcars)
# car_pd <- pdp_compare(x = mtcars_rf)
# car_pd$imp
# grid.arrange(car_pd$wt, car_pd$disp, car_pd$hp, car_pd$cyl, nrow = 2)
#
# house_rf <- randomForest(
#   formula = medv ~ ., data = boston,
#   importance = TRUE
# )
# house_pd <- pdp_compare(x = house_rf, trim = 0.1, trellis = TRUE)
# house_pd$full_num
# house_pd$imp
# house_pd$trellis_num
# house_pd$trellis_fac
# randomForest::ImpPlot(house.rf)
#
# cor(house_pd$imp[-1])
# plot(house_pd$imp[-1])
#
# set.seed(123)
# library(randomForest)
# house_rf1 <- randomForest(
#   formula = medv ~ ., data = MASS::Boston,
#   importance = TRUE
# )
# ImpPlot(house_rf1)
# house_pd1 <- pdp_compare(x = house_rf1, trim = 0.1)
# house_pd1$imp
# house_pd1$full_num
#
# cor(house_pd1$imp[-1])
# plot(house_pd1$imp[-1])
#
# library(pdp)
# library(randomForest)
# ######################
#
# library(EZtune)
# li <- EZtune::lichen
# li <- li[, -c(1, 3:8)]
# li$StandAgeClass <- as.factor(li$StandAgeClass)
# li$ReserveStatus <- as.factor(li$ReserveStatus)
#
# library(tidyverse)
#
# set.seed(1234)
# lo_rf <- randomForest(
#   formula = factor(LobaOreg) ~ ., data = li,
#   mtry = 5, importance = TRUE
# )
# lo_pd <- pdp_compare(x = lo_rf, which_class = 2L, prob = TRUE, trim = 0.1)
# lo_pd$full_num
# lo_pd$full_fac
# lo_pd$ACONIF
# lo_pd$StandAgeClass
# vip <- lo_pd$imp
# vip
# pairs(vip[-1])
# cor(vip[-1])
# plot(vip$range, vip$trim_range)
# plot(vip$sd, vip$trim_range)
# ImpPlot(Lo.rf, scale = FALSE)
# vi <- as.data.frame(importance(lo_rf, scale = FALSE))
# vi <- dplyr::arrange(vi, rev(MeanDecreaseGini))
# vi
# plot(vi$MeanDecreaseAccuracy, vip$range)
# plot(vi$MeanDecreaseGini, vip$range)
# plot(vi$MeanDecreaseAccuracy, vi$MeanDecreaseGini)
#
# grid.arrange(lo_pd$ACONIF, lo_pd$RelHumidDiff, lo_pd$StandAgeClass,
#   lo_pd$MinTempAve, lo_pd$ReserveStatus, lo_pd$Elevation,
#   nrow = 2
# )
#
# ###############################
#
# iris_rf <- randomForest(formula = factor(Species) ~ ., data = iris)
# iris_pd <- pdp_compare(x = iris_rf)
# iris_pd$full
# iris_pd$imp
# grid.arrange(iris_pd$Petal.Length, iris_pd$Petal.Width,
#   iris_pd$Sepal.Length, iris_pd$Sepal.Width,
#   nrow = 2
# )
#
# p <- pdp::partial(house.rf,
#   pred. = "rm", which_class = 1L, prob = TRUE,
#   plot.engine = "ggplot2"
# )
# plotPartial(p)
#
# #############################
#
# d <- matrix(rnorm(4000), ncol = 4, nrow = 1000)
# y <- d %*% 3:0 + rnorm(1000, sd = 0.1)
# d <- cbind(d, y)
# d <- ifelse(d > 0, 1, 0)
# d <- as.data.frame(d)
# colnames(d)[5] <- "y"
#
# d$V1 <- factor(d$V1)
# d$V2 <- factor(d$V2)
# d$V3 <- factor(d$V3)
# d$V4 <- factor(d$V4)
#
# d_rf <- randomForest(formula = factor(y) ~ ., data = d)
# d_rf$confusion
# d_rf$importance
# d_pd <- pdp_compare(x = d_rf, prob = TRUE)
# d_pd$full_fac
# d_pd$imp
