#' Non-linear Variance Inflation Factors
#' @name robust_vifs
#' @importFrom stats lm model.frame
#' @importFrom ggplot2 ggplot geom_point xlim ylim aes
#'   geom_line ggtitle geom_vline
#' @importFrom ggeasy easy_center_title
#' @importFrom dplyr %>% arrange desc
#' @importFrom car vif
#' @description A list of data.frames and useful plots for user evaluations of
#'   the randomForest hyperparameter mtry.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data 	an optional data frame containing the variables in the model.
#'   By default the variables are taken from the environment which the model
#'   is called from.
#' @param model Model to use for extraction partial correlations. Possible
#'   model choices are rpart.
#' @param log10 Applies a log10 transformation to VIFs when TRUE. Default is
#'   TRUE.
#' @param num_var Optional integer argument for reducing the number of
#'   variables to the top 'num_var'. Should be an integer between 1 and the
#'   total number of predictor variables in the model or it should be a
#'   positive proportion of variables desired.
#' @param ... Additional arguments to be passed to models as needed.
#' @return A list of data.frames and useful plots for user evaluations of
#'   VIFs.
#' @examples
#' rv <- robust_vifs(Petal.Length ~ ., data = iris[-5], model = lm)
#' rv
#' @export

# X correlations
# X scatterplots
# linear & non-parametric VIFs (R^2 for the non-parametric model)

robust_vifs <- function(formula, data = NULL, model = randomForest,
                        log10 = TRUE, num_var, ...) {
  if (missing(formula)) {
    stop("formula argument is a required field.")
  }

  mf <- model.frame(formula, data = data)
  m <- ncol(mf) - 1

  res <- gsub("\\)", "", gsub(".*\\(", "", colnames(mf)[1]))

  if (!missing(num_var)) {
    num_var <- ifelse(num_var > m | num_var <= 0, m,
      ifelse(num_var < 1, round(num_var * m), round(num_var))
    )
  }

  vifs <- car::vif(lm(
    formula = as.numeric(unlist(mf[1])) ~ .,
    data = mf[-1], ...
  ))

  vdf <- data.frame(
    var = colnames(mf)[-1], lm_vif = 0, lm_r2 = 0,
    model_vif = 0, model_R2 = 0
  )
  ifelse(is.null(ncol(vifs)), vdf$lm_vif <- as.vector(vifs),
    vdf$lm_vif <- vifs[, ncol(vifs)]
  )

  vdf$lm_r2 <- (vdf$lm_vif - 1) / vdf$lm_vif

  for (k in seq_len(ncol(mf) - 1) + 1) {
    r <- model(as.numeric(mf[, k]) ~ ., mf[, -c(1, k)])
    # forest yields message regarding regression. Need to change predictor class
    # or if logic for model development

    # Consider Fixes that use a test or OOB or CV error rather than
    # training Error.
    r2 <- 1 - (sum((as.numeric(mf[, k]) - predict(r, mf[, -c(1, k)]))^2) /
      sum((as.numeric(mf[, k]) - mean(as.numeric(mf[, k])))^2))
    vdf[k - 1, 4] <- 1 / (1 - r2)
    vdf[k - 1, 5] <- r2
  }

  if (log10 == TRUE) {
    vdf$lm_vif <- log10(vdf$lm_vif)
    vdf$model_vif <- log10(vdf$model_vif)
    colnames(vdf)[c(2, 4)] <- c("Log10_lm_vif", "Log10_model_vif")
  }

  vdf <- vdf[do.call(base::order, as.list(vdf[2])), ]
  vdf$var <- factor(vdf$var, levels = vdf$var)

  if (!missing(num_var)) {
    d <- vdf %>%
      arrange(desc(get(colnames(vdf)[2]))) %>%
      filter(get(colnames(vdf)[2]) >= get(colnames(vdf)[2])[num_var])

    vdfl <- vdf %>%
      filter(var %in% d$var)
  } else {
    vdfl <- vdf
  }

  m <- max(vdfl[2])
  v <- 10^(-3:6)
  ind <- findInterval(m, v)

  newr <- m / (10^(ind - 5))
  rrr <- ceiling(newr / 10) * 10

  if (newr / rrr < 3 / 4) {
    rrr <- ceiling(newr / 4) * 4
  }

  newm <- rrr * (10^(ind - 5))

  div <- case_when(
    (rrr / 5) %% 5 == 0 ~ 5,
    (rrr / 5) %% 4 == 0 ~ 4,
    (rrr / 5) %% 3 == 0 ~ 3,
    .default = 4
  )

  if (log10 != TRUE) {
    mx <- max(c(newm,10))
    div <- ifelse(mx > 1, div, 4)
    g <- vdfl %>% ggplot(aes(y = var, x = lm_vif)) +
      geom_point() +
      scale_x_continuous(limits = c(0, mx),
                         breaks = seq(0, mx, by = mx / div)) +
      ylab(NULL) +
      xlab("VIFs") +
      ggtitle(paste0("Linear VIFs for ", res)) +
      easy_center_title() +
      geom_vline(xintercept = 10, color = "blue")
  } else {
    mx <- max(c(newm,1))
    div <- ifelse(mx > 1, div, 4)
    g <- vdfl %>% ggplot(aes(y = var, x = Log10_lm_vif)) +
      geom_point() +
      scale_x_continuous(limits = c(0, mx),
                         breaks = seq(0, mx, by = mx / div)) +
      ylab(NULL) +
      xlab("log10(VIFs)") +
      ggtitle(paste0("Log10 Linear VIFs for ", res)) +
      easy_center_title() +
      geom_vline(xintercept = 1, color = "blue")
  }

  g1 <- vdfl %>% ggplot(aes(y = var, x = lm_r2)) +
    geom_point() +
    xlim(0, 1) +
    ylab(NULL) +
    xlab("Linear R2") +
    ggtitle("Linear R2: Variable ~ Other Predictors") +
    easy_center_title() +
    geom_vline(xintercept = 0.9, color = "blue")

  vdf <- vdf[do.call(base::order, as.list(vdf[4])), ]
  vdf$var <- factor(vdf$var, levels = vdf$var)

  if (!missing(num_var)) {
    d <- vdf %>%
      arrange(desc(get(colnames(vdf)[4]))) %>%
      filter(get(colnames(vdf)[4]) >= get(colnames(vdf)[4])[num_var])

    vdfm <- vdf %>%
      filter(var %in% d$var)
  } else {
    vdfm <- vdf
  }

  m <- max(vdfm[4])
  v <- 10^(-3:6)
  ind <- findInterval(m, v)

  newr <- m / (10^(ind - 5))
  rrr <- ceiling(newr / 10) * 10

  if (newr / rrr < 3 / 4) {
    rrr <- ceiling(newr / 4) * 4
  }

  newm <- rrr * (10^(ind - 5))

  div <- case_when(
    (rrr / 5) %% 5 == 0 ~ 5,
    (rrr / 5) %% 4 == 0 ~ 4,
    (rrr / 5) %% 3 == 0 ~ 3,
    .default = 4
  )

  if (log10 != TRUE) {
    mx <- max(c(newm,10))
    div <- ifelse(mx > 1, div, 4)
    g2 <- vdfm %>% ggplot(aes(y = var, x = model_vif)) +
      geom_point() +
      scale_x_continuous(limits = c(0, mx),
                         breaks = seq(0, mx, by = mx / div)) +
      ylab(NULL) +
      xlab("VIFs") +
      ggtitle(paste0("Non-Linear VIFs for ", res)) +
      easy_center_title() +
      geom_vline(xintercept = 10, color = "blue")
  } else {
    mx <- max(c(newm,1))
    div <- ifelse(mx > 1, div, 4)
    g2 <- vdfm %>% ggplot(aes(y = var, x = Log10_model_vif)) +
      geom_point() +
      scale_x_continuous(limits = c(0, mx),
                         breaks = seq(0, mx, by = mx / div)) +
      ylab(NULL) +
      xlab("log10(VIFs)") +
      ggtitle(paste0("Log10 Non-Linear VIFs for ", res)) +
      easy_center_title() +
      geom_vline(xintercept = 1, color = "blue")
  }

  g3 <- vdfm %>% ggplot(aes(y = var, x = model_R2)) +
    geom_point() +
    xlim(0, 1) +
    ylab(NULL) +
    xlab("Non-Linear R2") +
    ggtitle("Non-Linear R2: Variable ~ Other Predictors") +
    easy_center_title() +
    geom_vline(xintercept = 0.9, color = "blue")

  if (log10 != TRUE) {
    vdf <- vdf %>% arrange(desc(lm_vif))
  } else {
    vdf <- vdf %>% arrange(desc(Log10_lm_vif))
  }

  l <- list()

  l$summary <- vdf
  l$plot_lin_vifs <- g
  l$plot_lin_r2 <- g1
  l$plot_nonlin_vifs <- g2
  l$plot_nonlin_r2 <- g3

  l
}


