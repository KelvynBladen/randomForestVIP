
#' Small Multiple PDPs and Importance Metrics
#' @name pdp_compare
#' @importFrom dplyr %>% arrange desc filter select
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
#' @param vars Optional vector argument for reducing the number of
#'   variables to consider and compare. Elements should be characters that
#'   match column names from the data used to generate the model x.
#' @param scale For permutation based measures such as MSE or Accuracy, should
#'   the measures be divided by their "standard errors"? Default is FALSE.
#' @param sqrt Boolean value indicating whether importance metrics should be
#'   adjusted via a square root transformation. Default is True.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from
#'   each end of an individual PDP dataset before the trim-range is computed.
#' @param which.class Integer specifying which column of the matrix of
#'   predicted probabilities to use as the "focus" class. Default is to use
#'   the first class. Only used for classification problems.
#' @param prob Logical indicating whether or not partial dependence for
#'   classification problems should be returned on the probability scale,
#'   rather than the centered logit. If FALSE, the partial dependence
#'   function is on a scale similar to the logit. Default is TRUE.
#' @return A list of partial dependence plots with adjusted y-axes so all
#'   are on an identical scale. This list includes a comparative facet plot and
#'   pdp importance values for assessing true affect of predictors on response.
#' @examples
#' mtcars.rf <- randomForest(formula = mpg ~ ., data = mtcars)
#' car_pd <- pdp_compare(x = mtcars.rf)
#' car_pd$full
#' car_pd$imp
#' grid.arrange(car_pd$wt, car_pd$disp, car_pd$hp, car_pd$cyl, nrow = 2)
#' @export

# maybe edit code so that don't make pd_num1 and pd_fac1
pdp_compare <- function(x = Lo.rf, vars, scale = FALSE, sqrt = TRUE,
                        trim = 0.1, trellis = TRUE,
                        which.class = 2L, prob = TRUE, ...) {

  data <- eval(getCall(x)$data)
  model_frame <- model.frame(getCall(x)$formula, data = data)[-1]

  if(!missing(vars)){
    model_frame <- model_frame %>% dplyr::select(all_of(vars))
  }

  if(inherits(x, "randomForest")){
    im <- as.data.frame(randomForest::importance(x, scale = scale))
    im$var <- rownames(im)
    im <- im[(length(im)-2):length(im)]
  }

  if(inherits(x,"gbm")){
    im = as.data.frame(gbm::relative.influence(x))
    colnames(im) = "importance"
    im$var = rownames(im)
  }

  imp0 <- im
  imp0[, ] <- 0
  im <- pmax(im, imp0)

  if (sqrt == TRUE) {
    im <- sqrt(im)
  }

  if(!between(trim, 0, 0.5)){
    warning("trim is not between 0 and 0.5. Default of 0.1 will be used.")
    trim <- 0.1
  }

  vvec <- colnames(model_frame)

  pd_num <- NULL
  pd_fac <- NULL
  for (i in vvec) {
    ifelse(inherits(x,"gbm"),
      tmp <- pdp::partial(x, pred.var = i, which.class = which.class,
                          prob = prob, n.trees = x$n.trees),
      tmp <- pdp::partial(x, pred.var = i, which.class = which.class,
                          prob = prob))#, ...)
    # rename(x = i, y = yhat) %>% mutate(var = i)
    names(tmp) <- c("x", "y")

    if(inherits(tmp$x, "numeric")){
      pd_num <- rbind(pd_num,  cbind(tmp, var = i))
    } else {
      pd_fac <- rbind(pd_fac,  cbind(tmp, var = i))
    }
  }

  pd <- rbind(pd_num[-1], pd_fac[-1])

  imp <- pd %>% group_by(var) %>%  arrange(y) %>%
    summarise(range = max(y) - min(y),
              trim_range = y[length(y) - floor(length(y)*trim)] -
                y[floor(length(y)*trim)+1],
              sd = sd(y),
              mad = stats::mad(y, center = mean(y))) %>%
    arrange(desc(trim_range), desc(sd))

  if(exists("im")){
    imp <- dplyr::left_join(imp, im, by = c("var" = "var"))
  }

  vvec <- unique(imp$var)

  # new
  u <- max(pd[1])
  l <- min(pd[1])
  rr <- u - l
  v <- 10^(-3:6)

  indu <- findInterval(abs(u), v)
  newu <- u / (10^(indu - 6))
  ru <- ceiling(newu/10)*10

  indl <- findInterval(abs(l), v)
  newl <- l / (10^(indl - 6))
  rl <- floor(newl/10)*10

  nu <- ru * (10^(indu - 6))
  nl <- rl * (10^(indl - 6))
  nrr <- nu - nl

  if(rr/nrr < 3/4 | nl/l < 3/4 | u/nu < 3/4){
    ru <- ceiling(newu/4)*4
    rl <- floor(newl/4)*4
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
  if(length(pd_num) > 1){
    pd_num <- dplyr::left_join(pd_num, imp, by = "var") %>%
      arrange(desc(range)) %>%
      dplyr::select(!(range:mad))

    nvec <- unique(pd_num$var)

    pd_num <- arrange(transform(pd_num, var=factor(var,levels=nvec)),var)

    ng <- ggplot(pd_num, aes(x, y)) +
      geom_line() +
      facet_wrap(~ var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(limits = c(nl, nu),
                         breaks = seq(nl, nu, by = nrr / div))

    gl$full_num <- ng
    if(trellis){
      nc <- nchar(length(unique(pd_num$var)))
      s <- formatC(as.numeric(pd_num$var), width = nc,
                   format = "d", flag = "0")

      pd_num$imp <- as.factor(paste0("v", s, "_", pd_num$var))

      nt <- ggplot(pd_num, aes(x, y)) +
        geom_line() +
        facet_trelliscope(~ imp, scales = "free_x") + #...
        theme_bw() +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div))
      gl$trellis_num <- nt
    }
  }

  if(length(pd_fac) > 1){
    pd_fac <- dplyr::left_join(pd_fac, imp, by = "var") %>%
      arrange(desc(range)) %>% dplyr::select(!(range:mad))

    fvec <- unique(pd_fac$var)

    pd_fac <- arrange(transform(pd_fac, var=factor(var,levels=fvec)),var)

    fg <- ggplot(pd_fac, aes(x, y)) +
      geom_point() +
      facet_wrap(~ var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(limits = c(nl, nu),
                         breaks = seq(nl, nu, by = nrr / div))

    gl$full_fac <- fg

    if(trellis){
      nc <- nchar(length(unique(pd_fac$var)))
      s <- formatC(as.numeric(pd_fac$var), width = nc,
                   format = "d", flag = "0")

      pd_fac$imp <- as.factor(paste0("v", s, "_", pd_fac$var))

      ft <- ggplot(pd_fac, aes(x, y)) +
        geom_point() +
        facet_trelliscope(~ imp, scales = "free_x") + #...
        theme_bw() +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div))

      gl$trellis_fac <- ft
    }
  }

  gl$imp <- imp

  j = length(gl) + 1
  for(i in vvec){
    if(i %in% nvec){
      v <- pd_num %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        xlab(i) +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div)) +
        guides(size = "none") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 14, face = "bold"))
    }

    if(i %in% fvec){
      v <- pd_fac %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_point() +
        xlab(i) +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div)) +
        guides(size = "none") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 14, face = "bold"))
    }

    gl$j <- g
    names(gl)[j] <- i
    j = j + 1
  }

  gl
}

#######################
library(ggplot2)
library(dplyr)
mtcars.rf <- randomForest(formula = mpg ~ ., data = mtcars)
# p <- partial(mtcars.rf, pred.var = c("drat"))
# plotPartial(p)
car_pd <- pdp_compare(x = mtcars.rf)
car_pd$imp
car_pd$full
car_pd$drat
car_pd$cyl
car_pd$imp
grid.arrange(car_pd$wt, car_pd$disp, car_pd$hp, car_pd$cyl, nrow = 2)

house.rf <- randomForest(formula = medv ~ ., data = boston,
                         importance = T)
house_pd <- pdp_compare(x = house.rf, trim = 0.1, trellis = T)
house_pd$full_num
house_pd$imp
house_pd$imp
house_pd$trellis_num
house_pd$trellis_fac
randomForest::varImpPlot(house.rf)
house_pd$imp
house_pd$full_num

cor(house_pd$imp[-1])
plot(house_pd$imp[-1])

set.seed(123)
library(randomForest)
house.rf1 <- randomForest(formula = medv ~ ., data = MASS::Boston,
                          importance = T)
varImpPlot(house.rf1)
house_pd1 <- pdp_compare(x = house.rf1, trim = 0.1)
house_pd1$imp
house_pd1$full_num


cor(house_pd1$imp[-1])
plot(house_pd1$imp[-1])

library(pdp)
library(randomForest)
######################

library(EZtune)
li = EZtune::lichen
?EZtune::lichen
li = li[, -c(1, 3:8)]
li$StandAgeClass <- as.factor(li$StandAgeClass)
li$ReserveStatus <- as.factor(li$ReserveStatus)

#li = li %>% select(-StandAgeClass, -ReserveStatus)
library(tidyverse)
library(randomForest)
library(pdp)

set.seed(1234)
Lo.rf <- randomForest(formula = factor(LobaOreg) ~ ., data = li,
                      mtry = 5, importance = T)
Lo_pd <- pdp_compare(x = Lo.rf, which.class = 2L, prob = T, trim = 0.1)
Lo_pd$full_num
Lo_pd$full_fac
Lo_pd$ACONIF
Lo_pd$StandAgeClass
vip = Lo_pd$imp
vip
pairs(vip[-1])
cor(vip[-1])
plot(vip$range, vip$trim_range)
plot(vip$sd, vip$trim_range)
varImpPlot(Lo.rf, scale = F)
vi = as.data.frame(importance(Lo.rf, scale = F))
vi = dplyr::arrange(vi, rev(MeanDecreaseGini))
vi
plot(vi$MeanDecreaseAccuracy, vip$range)
plot(vi$MeanDecreaseGini, vip$range)
plot(vi$MeanDecreaseAccuracy, vi$MeanDecreaseGini)

grid.arrange(Lo_pd$ACONIF, Lo_pd$RelHumidDiff, Lo_pd$StandAgeClass,
             Lo_pd$MinTempAve, Lo_pd$ReserveStatus, Lo_pd$Elevation, nrow = 2)

###############################

iris.rf <- randomForest(formula = factor(Species) ~ ., data = iris)
iris_pd <- pdp_compare(x = iris.rf)
iris_pd$full
iris_pd$imp
grid.arrange(iris_pd$Petal.Length, iris_pd$Petal.Width,
             iris_pd$Sepal.Length, iris_pd$Sepal.Width, nrow = 2)

p = pdp::partial(house.rf, pred.var = "rm", which.class = 1L, prob = T,
            plot.engine = "ggplot2")
?partial
plotPartial(p)

#############################

d <- matrix(rnorm(4000), ncol = 4, nrow = 1000)
y = d %*% 3:0 + rnorm(1000, sd = 0.1)
d = cbind(d, y)
d <- ifelse(d > 0, 1, 0)
d <- as.data.frame(d)
colnames(d)[5] <- "y"

d$V1 <- factor(d$V1)
d$V2 <- factor(d$V2)
d$V3 <- factor(d$V3)
d$V4 <- factor(d$V4)

d.rf <- randomForest(formula = factor(y) ~ ., data = d)
d.rf$confusion
d.rf$importance
d_pd <- pdp_compare(x = d.rf, prob = T)
d_pd$full_fac
d_pd$imp
