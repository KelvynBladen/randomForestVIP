
#' Partial Correlations
#' @name partial_cor
#' @importFrom randomForest randomForest
#' @importFrom rpart rpart
#' @importFrom stats lm model.frame cor model.matrix predict
#' @importFrom e1071 svm
#' @importFrom ggplot2 ggplot aes geom_point xlim ylim geom_line ggtitle
#' @importFrom dplyr %>% arrange desc
#' @importFrom minerva mine
#' @importFrom wrapr orderv
#' @description A list of data.frames and useful plots for user evaluations of
#'   correlations and partial correlations of predictors with a given response.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data 	an optional data frame containing the variables in the model.
#'   By default the variables are taken from the environment which the model
#'   is called from.
#' @param model Model to use for extraction partial correlations. Possible
#'   model choices are lm, rpart, randomForest, and svm. Default is lm.
#' @param num_var Optional integer argument for reducing the number of
#'   variables to the top 'num_var'. Should be an integer between 1 and the
#'   total number of predictor variables in the model or it should be a
#'   positive proportion of variables desired.
#' @param ... Additional arguments to be passed to model as needed.
#' @return A list of data.frames and useful plots for user evaluations of
#'   partial correlations.
#' @examples
#' pcs <- partial_cor(Petal.Length ~ ., data = iris[-5], model = lm)
#' pcs$plot_y_part_cors
#' @export

# fix cor for factors, remove or adjust
# unsupervised forest MI

partial_cor <- function(formula, data = NULL, model = lm, num_var, ...) {
  l <- list()

  mm <- model.matrix(formula, data = data)[, -1]
  mfi <- model.frame(formula, data = data)
  mf <- as.data.frame(cbind(mfi[1], mm))

  m <- ncol(mf) - 1
  if (!missing(num_var)) {
    num_var <- ifelse(num_var > m | num_var <= 0, m,
      ifelse(num_var < 1, round(num_var * m), round(num_var))
    )
  }
  # use num_var


  # mark
  mf[1] <- as.numeric(unlist(mf[1]))

  mdf <- data.frame(var = colnames(mf)[-1])

  mn <- minerva::mine(x = mf[-1], y = as.numeric(unlist(mf[1])))
  mdf$Mutual_Info <- as.vector(mn$MIC)

  # gini - divide by rows in data
  # Accuracy
  # Scale importances
  rfu <- randomForest(mf[-1], importance = T)
  imp <- randomForest::importance(rfu)
  rownames(imp) <- rownames(mdf)
  mdf <- cbind(mdf, imp[, 3:4])
  colnames(mdf)[3:4] <- c("urfAccuracy", "urfPurity")

  if (inherits(unlist(mf[1]), "numeric")) {
    cd <- data.frame(var = colnames(mf)[-1], cor = 0)

    cd$cor <- cor(mf)[-1, 1]

    cd <- cd[wrapr::orderv(abs(cd[2])), ]
    cd$var <- factor(cd$var, levels = cd$var)

    if (!missing(num_var)) {
      d <- cd %>%
        arrange(desc(get(colnames(cd)[2]))) %>%
        filter(get(colnames(cd)[2]) >= get(colnames(cd)[2])[num_var])

      cd <- cd %>%
        filter(var %in% d$var)
    }

    # maybe sort by abs but plot regulars
    g <- cd %>% ggplot(aes(y = var, x = cor)) +
      geom_point() +
      xlim(-1, 1) +
      geom_vline(xintercept = 0, color = "blue") +
      ggtitle("Correlations Between Predictor Variables and Response")

    g1 <- cd %>% ggplot(aes(y = var, x = abs(cor))) +
      geom_point() +
      xlim(0, 1) +
      ggtitle("Absolute Value of Correlations")

    cd <- cd %>% arrange(desc(abs(cor)))

    l$y_cors <- cd

    cdf <- data.frame(var = colnames(mf)[-1], part_cor = 0)

    for (k in seq_len(ncol(mf) - 1) + 1) {
      cdf[k - 1, 2] <- cor(
        mf[, 1] - predict(model(mf[, 1] ~ ., mf[, -c(1, k)])),
        mf[, k] - predict(model(mf[, k] ~ ., mf[, -c(1, k)]))
      )
    }
    cdf$part_cor <- ifelse(is.na(cdf$part_cor), 0, cdf$part_cor)

    cdf <- cdf[wrapr::orderv(abs(cdf[2])), ]
    cdf$var <- factor(cdf$var, levels = c(cdf$var))

    if (!missing(num_var)) {
      d <- cdf %>%
        arrange(desc(get(colnames(cdf)[2]))) %>%
        filter(get(colnames(cdf)[2]) >= get(colnames(cdf)[2])[num_var])

      cdf <- cdf %>%
        filter(var %in% d$var)
    }

    g2 <- cdf %>% ggplot(aes(y = var, x = part_cor)) +
      geom_point() +
      xlim(-1, 1) +
      geom_vline(xintercept = 0, color = "blue") +
      ggtitle("Partial Correlations")

    g3 <- cdf %>% ggplot(aes(y = var, x = abs(part_cor))) +
      geom_point() +
      xlim(0, 1) +
      ggtitle("Absolute Value of Partial Correlations")

    cdf <- cdf %>% arrange(desc(abs(part_cor)))

    l$y_partial_cors <- cdf
  }

  mdf <- mdf[wrapr::orderv(mdf[2]), ]
  mdf$var <- factor(mdf$var, levels = mdf$var)

  if (!missing(num_var)) {
    d <- mdf %>%
      arrange(desc(get(colnames(mdf)[2]))) %>%
      filter(get(colnames(mdf)[2]) >= get(colnames(mdf)[2])[num_var])

    mdfm <- mdf %>%
      filter(var %in% d$var)
  } else {
    mdfm <- mdf
  }

  g4 <- mdfm %>% ggplot(aes(y = var, x = Mutual_Info)) +
    geom_point() +
    xlim(0, 1) +
    ggtitle("Mutual Information Between Predictor Variables and Response")

  mdf <- mdf[wrapr::orderv(mdf[3]), ]
  mdf$var <- factor(mdf$var, levels = mdf$var)

  if (!missing(num_var)) {
    d <- mdf %>%
      arrange(desc(get(colnames(mdf)[3]))) %>%
      filter(get(colnames(mdf)[3]) >= get(colnames(mdf)[3])[num_var])

    mdfa <- mdf %>%
      filter(var %in% d$var)
  } else {
    mdfa <- mdf
  }

  g5 <- mdfa %>% ggplot(aes(y = var, x = urfAccuracy)) +
    geom_point() +
    xlim(
      ifelse(any(mdf$urfAccuracy < 0), min(mdf$urfAccuracy), 0),
      max(mdf$urfAccuracy)
    ) +
    ggtitle("URF Accuracy Mutual Information")

  mdf <- mdf[wrapr::orderv(mdf[4]), ]
  mdf$var <- factor(mdf$var, levels = mdf$var)

  if (!missing(num_var)) {
    d <- mdf %>%
      arrange(desc(get(colnames(mdf)[4]))) %>%
      filter(get(colnames(mdf)[4]) >= get(colnames(mdf)[4])[num_var])

    mdfp <- mdf %>%
      filter(var %in% d$var)
  } else {
    mdfp <- mdf
  }

  g6 <- mdfp %>% ggplot(aes(y = var, x = urfPurity)) +
    geom_point() +
    xlim(
      ifelse(any(mdf$urfPurity < 0), min(mdf$urfPurity), 0),
      max(mdf$urfPurity)
    ) +
    ggtitle("URF Purity Mutual Information")

  mdf <- mdf %>% arrange(desc(Mutual_Info))

  l$y_mutual_info <- mdf

  if (inherits(unlist(mf[1]), "numeric")) {
    l$plot_y_cors <- g
    l$plot_abs_y_cors <- g1
    l$plot_mutual_info <- g4
    l$plot_y_part_cors <- g2
    l$plot_abs_y_part_cors <- g3
    l$plot_urf_accuracy <- g5
    l$plot_urf_purity <- g6
  } else {
    l$plot_mutual_info <- g4
    l$plot_urf_accuracy <- g5
    l$plot_urf_purity <- g6
  }

  l
}

# library(minerva)
# library(infotheo)
# library(entropy)
# library(rmi)

# iris1 <- iris %>% filter(Species != "setosa")
# p <- partial_cor(formula = Petal.Length ~ .,
#             data = iris1, model = randomForest)
# p
# p$y_mutual_info
# p$plot_mutual_info
# p$plot_y_part_cors
# p$plot_y_cors
# partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
#               iris$Sepal.Length + iris$Petal.Width)
