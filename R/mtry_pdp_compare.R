#' Mtry Tune via PDPs
#' @name mtry_pdp_compare
#' @importFrom randomForest randomForest
#' @importFrom dplyr %>% arrange desc filter select
#'   summarise group_by case_when rename mutate bind_rows
#' @importFrom ggplot2 ggplot geom_point geom_line facet_wrap ggtitle theme
#'   aes scale_x_continuous scale_y_continuous theme_bw
#' @importFrom ggeasy easy_center_title easy_plot_legend_size
#' @importFrom gridExtra grid.arrange
#' @importFrom pdp partial
#' @importFrom stats model.frame na.omit mad sd formula
#' @importFrom trelliscopejs facet_trelliscope
#' @importFrom methods is
#' @description This function builds randomForest algorithms, generates PDPs
#'   and combines them across different models. Outputs a list of data.frames
#'   and useful plots for user evaluations of the randomForest
#'   hyperparameter mtry. This also contains PDP-derived importance values for
#'   assessing effect of predictors on response.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data 	an optional data frame containing the variables in the model.
#'   By default the variables are taken from the environment which randomForest
#'   is called from.
#' @param mvec Optional vector argument for defining choices of mtry to have the
#'   function consider. Should be a vector of integers between 1 and the total
#'   number of predictor variables in the model. Or it can be a vector of
#'   proportions (strictly less than 1) of the number of predictor variables.
#' @param var_vec Optional vector argument for reducing the number of
#'   variables to consider and compare. Elements should be characters that
#'   match column names from the data used to generate the model x.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from
#'   each end of an individual PDP dataset before the trim-range is computed.
#'   The default of 0.1 will be used when values of trim outside that range
#'   are given.
#' @param trellis Logical indicating whether or not to generate trellis plots
#'   as output for comparing PDPs. Default is TRUE.
#' @param which_class Integer specifying which column of the matrix of
#'   predicted probabilities to use as the "focus" class. Default is to
#'   use the first class. Only used for classification problems.
#' @param prob 	Logical indicating whether or not partial dependence for
#'   classification problems should be returned on the probability scale,
#'   rather than the centered logit. If FALSE, the partial dependence function
#'   is on a scale similar to the logit. Default is TRUE.
#' @param ... Other parameters to pass to the randomForest function.
#' @return A list of data.frames, useful plots, and forest objects for user
#'   evaluations of the randomForest hyperparameter mtry. This includes a
#'   list of partial dependence plots with adjusted y-axes so all PDPs
#'   are on an identical scale. This also contains comparative facet plots
#'   and PDP importance values for assessing true effect of predictors
#'   on response.
#' @examples
#' m <- mtry_pdp_compare(Petal.Length ~ ., data = iris)
#' m
#' @export

# make sure var_vec are valid,

mtry_pdp_compare <- function(formula, data = NULL, mvec, var_vec,
                             trim = 0.1, trellis = TRUE,
                             which_class = 2L, prob = TRUE, ...) {
  model_frame <- model.frame(formula, data = data)
  num_preds <- ncol(model_frame) - 1

  m1 <- 1
  m2 <- ifelse(is(model_frame[1, 1], "numeric"),
    floor(num_preds / 3), floor(sqrt(num_preds))
  )
  m2 <- max(m2, 1)
  m3 <- ceiling(mean(c(m2, num_preds)))
  new_mvec <- sort(unique(c(m1, m2, m3, num_preds)))

  if (!missing(mvec)) {
    mvec <- ifelse(all(floor(mvec) == 0),
      round(mvec * num_preds),
      round(mvec)
    )

    ch <- mvec %in% seq_len(num_preds)
    mvec <- mvec[ch] |>
      unique() |>
      sort()

    mvec <- ifelse(length(mvec) == 0, new_mvec, mvec)
  } else {
    mvec <- new_mvec
  }

  trim <- ifelse(!between(trim, 0, 0.5), 0.1, trim)

  # check that provided vars are valid

  ifelse(missing(var_vec),
         var_vec <- colnames(model_frame)[-1],
         var_vec <- var_vec)

  pd_num <- NULL
  pd_fac <- NULL
  err_v <- 0
  l <- list()

  for (i in mvec) {
    x <- paste0("srf", i)
    eval(call("<-", as.name(x), randomForest(
      formula = formula, mtry = i,
      importance = TRUE, data = data # , ...
    )))

    ifelse(is(model_frame[1, 1], "numeric"),
      err_v <- c(err_v, get(x)$mse[get(x)$ntree]),
      err_v <- c(err_v, get(x)$err.rate[get(x)$ntree])
    )

    y <- paste0("rf", i)
    l[[y]] <- get(x)

    for (j in var_vec) {
      tmp <- pdp::partial(get(x),
        pred.var = j, train = data,
        which.class = which_class,
        prob = prob # , ...
      ) %>%
        rename(x = j, y = yhat) %>%
        mutate(mtry = i, var = j)

      ifelse(inherits(tmp$x, "numeric"),
        pd_num <- dplyr::bind_rows(pd_num, tmp),
        pd_fac <- dplyr::bind_rows(pd_fac, tmp)
      )
    }
  }

  pd <- rbind(pd_num[-1], pd_fac[-1])

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

  upp <- max(pd[1])
  low <- min(pd[1])
  rr <- upp - low
  v <- 10^(-3:6)

  indu <- findInterval(abs(upp), v)
  newu <- upp / (10^(indu - 6))
  ru <- ceiling(newu / 10) * 10

  indl <- findInterval(abs(low), v)
  newl <- low / (10^(indl - 6))
  rl <- floor(newl / 10) * 10

  nu <- ru * (10^(indu - 6))
  nl <- rl * (10^(indl - 6))
  nrr <- nu - nl

  if (any(c(rr / nrr, nl / low, upp / nu) < 3 / 4)) {
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


  nvec <- vector(length = 0)
  fvec <- vector(length = 0)
  if (length(pd_num) > 1) {
    pd_num <- left_join(pd_num, imp, by = "var") %>%
      arrange(desc(range)) %>%
      dplyr::select(!(range:mad))

    nvec <- unique(pd_num$var)

    pd_num <- arrange(transform(pd_num, var = factor(var, levels = nvec)), var)

    ng <- ggplot(pd_num, aes(x, y, col = mtry, group = mtry)) +
      geom_line() +
      geom_point() +
      facet_wrap(~var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(
        limits = c(nl, nu),
        breaks = seq(nl, nu, by = nrr / div)
      )

    l$full_num <- ng
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
      l$trellis_num <- nt
    }
  }

  if (length(pd_fac) > 1) {
    pd_fac <- left_join(pd_fac, imp, by = "var") %>%
      arrange(desc(range)) %>%
      dplyr::select(!(range:mad))

    fvec <- unique(pd_fac$var)

    pd_fac <- arrange(transform(pd_fac, var = factor(var, levels = fvec)), var)

    fg <- ggplot(pd_fac, aes(x, y, col = mtry, group = mtry)) +
      geom_line() +
      geom_point() +
      facet_wrap(~var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(
        limits = c(nl, nu),
        breaks = seq(nl, nu, by = nrr / div)
      )

    l$full_fac <- fg

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

      l$trellis_fac <- ft
    }
  }

  k <- length(l) + 1
  for (j in var_vec) {
    ifelse(j %in% nvec,
      v <- pd_num %>% filter(var == j),
      v <- pd_fac %>% filter(var == j)
    )

    g <- v %>%
      ggplot(aes(x = x, y = y, col = mtry, group = mtry)) +
      geom_line() +
      geom_point() +
      xlab(j) +
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

    l$k <- g
    names(l)[k] <- j
    k <- k + 1

    g1 <- v %>%
      ggplot(aes(x = x, y = y, col = mtry, group = mtry)) +
      geom_line() +
      geom_point() +
      xlab(j) +
      guides(size = "none") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 14, face = "bold")
      ) +
      ggeasy::easy_center_title() +
      ggeasy::easy_plot_legend_size(size = 11)

    l$k <- g1
    names(l)[k] <- paste0("zoom_", j)
    k <- k + 1
  }

  err_v <- err_v[-1]

  ifelse(is(model_frame[1, 1], "numeric"),
    err_df <- data.frame(mtry = mvec, mse = err_v),
    err_df <- data.frame(mtry = mvec, misclass_rate = err_v)
  )

  m <- max(err_df[2])
  v <- 10^(-3:6)
  ind <- findInterval(m, v)

  newr <- m / (10^(ind - 5))
  rrr <- ceiling(newr / 10) * 10

  rrr <- ifelse(newr / rrr < 3 / 4, ceiling(newr / 4) * 4, rrr)

  newm <- rrr * (10^(ind - 5))

  div <- case_when(
    (rrr / 5) %% 5 == 0 ~ 5,
    (rrr / 5) %% 4 == 0 ~ 4,
    (rrr / 5) %% 3 == 0 ~ 3,
    .default = 4
  )

  g_err <- err_df %>%
    ggplot(aes(
      x = .data[[colnames(err_df)[1]]],
      y = .data[[colnames(err_df)[2]]]
    )) +
    geom_point() +
    geom_line() +
    scale_x_continuous(limits = c(1, num_preds), breaks = mvec) +
    scale_y_continuous(
      limits = c(0, newm),
      breaks = seq(0, newm, by = newm / div)
    ) +
    ggtitle("Model Errors Across mtry")

  l$model_errors <- err_df
  l$gg_model_errors <- g_err

  l$imp <- imp
  l$pdp_num <- pd_num
  l$pdp_fac <- pd_fac

  list_names <- c("gg_model_errors", "full_num", "full_fac",
                  "trellis_num", "trellis_fac")
  l1 <- l[names(l) %in% list_names]
  l2 <- l[names(l)[!names(l) %in% names(l1)]]

  greg2 <- gregexec("rf[[:digit:]]+", names(l2))
  gmatch <- regmatches(names(l2), greg2)
  l3 <- l2[names(l2)[names(l2) %in% gmatch]]
  l2 <- l2[names(l2)[!names(l2) %in% gmatch]]

  greg3 <- gregexec("zoom.*", names(l2))
  gmatch1 <- regmatches(names(l2), greg3)
  l4 <- l2[names(l2)[names(l2) %in% gmatch1]]

  l2 <- l2[names(l2)[!names(l2) %in% gmatch1]]
  l <- c(l1, l2, l4, l3)
  l
}

# m <- mtry_pdp_compare(formula = medv ~ ., data = MASS::Boston)
# m$full_num
# m$full_fac
# m$gg_model_errors
# gridExtra::grid.arrange(m$lstat, m$rm, m$nox, m$dis, nrow = 2)
# gridExtra::grid.arrange(m$nox, m$zoom_nox, m$dis, m$zoom_dis, nrow = 2)
#
# m2 <- mtry_pdp_compare(formula = carb ~ ., data = mtcars)
# m2$full_num
# m2$gg_model_errors
#
# m3 <- mtry_pdp_compare(formula = Petal.Length ~ ., data = iris)
# m3$model_errors
# m3$full_num
# m3$full_fac
