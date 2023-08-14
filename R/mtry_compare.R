#' Mtry Tune via VIPs
#' @name mtry_compare
#' @importFrom randomForest importance randomForest
#' @importFrom dplyr %>% arrange across ends_with desc filter select
#'   summarise group_by case_when
#' @importFrom ggplot2 ggplot geom_point geom_line ylab ggtitle theme
#'   aes_string scale_x_continuous scale_y_continuous
#' @importFrom tidyr pivot_wider
#' @importFrom stats model.frame na.omit quantile
#' @description A list of data.frames and useful plots for user evaluations of
#'   the randomForest hyperparameter mtry.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data 	an optional data frame containing the variables in the model.
#'   By default the variables are taken from the environment which randomForest
#'   is called from.
#' @param scale For permutation based measures such as MSE or Accuracy, should
#'   the measures be divided by their "standard errors"? Default is False.
#' @param sqrt Boolean value indicating whether importance metrics should be
#'   adjusted via a square root transformation. Default is True.
#' @param num_var Optional integer argument for reducing the number of
#'   variables to the top 'num_var'. Should be an integer between 1 and the
#'   total number of predictor variables in the model or it should be a
#'   positive proportion of variables desired.
#' @param mvec Optional vector argument for defining choices of mtry to have the
#'   function consider. Should be a vector of integers between 1 and the total
#'   number of predictor variables in the model. Or it can be a vector of
#'   proportions (strictly less than 1) of the number of predictor variables.
#' @param ... Other parameters to pass to the randomForest function.
#' @return A list of data.frames, useful plots, and forest objects for user
#'   evaluations of the randomForest hyperparameter mtry.
#' @examples
#' m <- mtry_compare(factor(Species) ~ ., data = iris, sqrt = TRUE)
#' m
#' @export

mtry_compare <- function(formula, data = NULL, scale = FALSE, sqrt = TRUE,
                         num_var, mvec, ...) {
  model_frame <- model.frame(formula, data = data)
  num_preds <- ncol(model_frame) - 1

  m1 <- 1
  m2 <- ifelse(is(model_frame[1, 1], "numeric") & num_preds > 2,
    floor(num_preds / 3), floor(sqrt(num_preds))
  )
  m3 <- ceiling(mean(c(m2, num_preds)))
  new_mvec <- sort(unique(c(m1, m2, m3, num_preds)))

  if (missing(mvec)) {
    mvec <- new_mvec
  } else {
    if (all(mvec <= 0)) {
      mvec <- new_mvec
    } else {
      ifelse(all(floor(mvec) == 0),
        mvec <- round(mvec * num_preds), mvec <- round(mvec)
      )
      mvec <- ifelse(floor((mvec - 1) / num_preds) == 0, mvec, NA) %>%
        na.omit() %>%
        unique() %>%
        sort()
    }
  }

  ifelse(!missing(num_var),
    num_var <- ifelse(num_var >= num_preds | num_var <= 0,
      num_preds,
      ifelse(num_var < 1,
        round(num_var * num_preds),
        round(num_var)
      )
    ),
    num_var <- num_preds
  )

  for (i in mvec) {
    x <- paste0("srf", i)
    eval(call("<-", as.name(x), randomForest(
      formula = formula, mtry = i,
      importance = TRUE, data = data # , ...
    )))

    vf <- paste0("sv", i)
    eval(call("<-", as.name(vf), importance(get(paste0("srf", i)),
      scale = scale
    )))

    v <- as.data.frame(get(vf))

    nc <- ncol(v)
    v <- v[(nc - 1):nc]

    v0 <- data.frame(matrix(0, nrow = nrow(v), ncol = ncol(v)))
    v <- pmax(v, v0)

    if (sqrt == TRUE) {
      v <- sqrt(v)
    }

    v$names <- rownames(v)
    eval(call("<-", as.name(vf), v))

    y <- get(vf)
    y$mtry <- i
    eval(call("<-", as.name(vf), y))
  }

  err_v <- 0
  for (i in mvec) {
    mod <- get(paste0("srf", i))

    ifelse(is(model_frame[1, 1], "numeric"),
      err_v <- c(err_v, mod$mse[mod$ntree]),
      err_v <- c(err_v, mod$err.rate[mod$ntree])
    )
  }
  err_v <- err_v[-1]

  ifelse(is(model_frame[1, 1], "numeric"),
    err_df <- data.frame(mtry = mvec, mse = err_v),
    err_df <- data.frame(mtry = mvec, misclass_rate = err_v)
  )

  sd <- data.frame()
  for (i in mvec) {
    sd <- rbind(sd, get(paste0("sv", i)))
  }

  if (ncol(sd) > 0) {
    if (colnames(sd)[1] == "%IncMSE") {
      colnames(sd)[1] <- "PctIncMSE"
    }
  }

  sd_full <- sd

  if (!missing(num_var)) {
    d <- sd %>%
      group_by(names) %>%
      summarise(mean = mean(get(colnames(sd)[1]))) %>%
      arrange(desc(mean)) %>%
      filter(mean >= mean[num_var])

    sd <- sd %>%
      filter(names %in% d$names)
  }

  k <- sd_full[-2] %>%
    pivot_wider(
      id_cols = names, names_from = mtry,
      values_from = colnames(sd_full)[1]
    ) %>%
    as.data.frame()
  colnames(k)[-1] <- paste0("m", colnames(k)[-1])
  rownames(k) <- k$names
  k <- k[-1] %>% arrange(desc(across(ends_with(as.character(num_preds)))))

  n <- sd_full[-1] %>%
    pivot_wider(
      id_cols = names, names_from = mtry,
      values_from = colnames(sd_full)[2]
    ) %>%
    as.data.frame()

  colnames(n)[-1] <- paste0("m", colnames(n)[-1])
  rownames(n) <- n$names
  n <- n[-1] %>% arrange(desc(across(ends_with(as.character(num_preds)))))

  m <- max(sd[1])
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

  g1 <- sd %>%
    ggplot(aes_string(
      x = colnames(sd)[4], y = colnames(sd)[1],
      color = colnames(sd)[3], group = colnames(sd)[3]
    )) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = mvec) +
    scale_y_continuous(
      limits = c(0, newm),
      breaks = seq(0, newm, by = newm / div)
    ) +
    ggtitle("Variable Importance Based on Decrease in Error Across mtry")


  m <- max(sd[2])
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

  g2 <- sd %>%
    ggplot(aes_string(
      x = colnames(sd)[4], y = colnames(sd)[2],
      color = colnames(sd)[3], group = colnames(sd)[3]
    )) +
    geom_point() +
    geom_line() +
    scale_y_continuous(
      limits = c(0, newm),
      breaks = seq(0, newm, by = newm / div)
    ) +
    ggtitle("Variable Importance Based on Purity Contribution Across mtry") +
    scale_x_continuous(breaks = mvec)


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
    ggplot(aes_string(x = colnames(err_df)[1], y = colnames(err_df)[2])) +
    geom_point() +
    geom_line() +
    scale_x_continuous(limits = c(1, num_preds), breaks = mvec) +
    scale_y_continuous(
      limits = c(0, newm),
      breaks = seq(0, newm, by = newm / div)
    ) +
    ggtitle("Model Errors Across mtry")


  rownames(sd_full) <- seq_len(nrow(sd_full))

  l <- list()

  l$importance <- sd_full[c(3, 4, 1, 2)]
  l$model_errors <- err_df
  l$var_imp_error <- k
  l$var_imp_purity <- n
  l$gg_var_imp_error <- g1
  l$gg_var_imp_purity <- g2
  l$gg_model_errors <- g_err

  for (i in mvec) {
    x <- paste0("rf", i)
    l[[x]] <- get(paste0("srf", i))
  }

  l
}
