#' Mtry Tune via VIPs
#' @name caret_plot
#' @importFrom dplyr %>% arrange across ends_with desc filter select
#'   summarise group_by case_when
#' @importFrom ggplot2 ggplot geom_point geom_line ylab ggtitle theme
#'   aes_string scale_x_continuous scale_y_continuous
#' @importFrom gridExtra arrangeGrob
#' @importFrom tidyr pivot_wider
#' @importFrom stats model.frame na.omit quantile
#' @description A list of data.frames and useful plots for comparing the
#'   performance of models across their hyper-parameters.
#' @param x An object of class train.
#' @param sqrt Boolean value indicating whether assessment metrics should be
#'   adjusted via a square root transformation. Default is FALSE.
#' @param marg1 Boolean value indicating whether to aggregate performance
#'   down to 1 dimension of hyper-parameter and provide the corresponding
#'   data.frames and line-plots for assessment. Default is FALSE.
#' @param marg2 Boolean value indicating whether to aggregate performance
#'   down to 2 dimensions of hyper-parameter and provide the corresponding
#'   data.frames and heatmaps for assessment. Default is FALSE.
#' @param col Name of the variable to plot on the columns of heatmaps. Only
#'   relevant for heatmaps of 3 or more dimensions. Default is NULL.
#' @param row Name of the variable to plot on the rows of heatmaps. Only
#'   relevant for heatmaps of 3 or more dimensions. Default is NULL.
#' @param facet Name of the variable to plot as the facets of heatmaps. Only
#'   relevant for heatmaps of 4 dimensions. Default is NULL.
#' @return A list of data.frames, useful plots, and forest objects for user
#'   evaluations of the randomForest hyperparameter mtry.
#' @examples
#' set.seed(123)
#' fit_control <- trainControl(method = "cv", number = 10)
#' gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
#'                         shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
#' x <- train(factor(Species) ~ ., method = "gbm", tuneGrid = gbm_grid,
#'                  trControl = fit_control, data = iris)
#' p <- caret_plot(x, sqrt = FALSE, col = "n.trees", marg1 = TRUE, marg2 = TRUE)
#' p
#' @export

caret_plot <- function(x = gbmFit, sqrt = FALSE, marg1 = TRUE, marg2 = TRUE,
                       col = NULL, row = NULL, facet = NULL) {
  # class should be train

  res <- x$results
  res[, 1] <- round(res[, 1], 4)
  hvec <- res[, 1] # maybe use plyr::round_any logic

  typ <- x$modelType

  if (typ == "Regression") {
    w <- which(colnames(res) == "Rsquared")
    res <- res[, 1:w]
    r1 <- "rmse"
    r2 <- "rsq"
  } else {
    w <- which(colnames(res) == "Kappa")
    res <- res[, 1:w]
    r1 <- "acc"
    r2 <- "kappa"
  }

  if (length(res) > 3) {
    a <- apply(res[, 1:(length(res) - 1)], 2, function(x) length(unique(x)))
    w <- which(a == 1)
    res <- res %>% select(!all_of(w))
  }

  if (sqrt == TRUE) {
    vc1 <- length(res) - 1
    vc2 <- length(res)
    res[vc1] <- sqrt(res[vc1])
    res[vc2] <- (res[vc2]^2)
    colnames(res)[vc1] <- paste0("sqrt", colnames(res)[vc1])
    colnames(res)[vc2] <- paste0(colnames(res)[vc2], "Sqrd")
  }

  l <- list()

  if (length(res) == 3) {
    m <- unname(apply(res[, (length(res) - 1):length(res)], 2, max))
    v <- 10^(-3:6)
    ind <- findInterval(m, v)

    newr <- m / (10^(ind - 5))
    rrr <- plyr::round_any(newr, 10, ceiling)

    rrr <- ifelse(newr / rrr < .75, plyr::round_any(newr, 4, ceiling), rrr)

    newm <- rrr * (10^(ind - 5))
    div <- case_when(
      (rrr / 5) %% 5 == 0 ~ 5,
      (rrr / 5) %% 4 == 0 ~ 4,
      (rrr / 5) %% 3 == 0 ~ 3,
      .default = 4
    )

    g_ae <- res %>%
      ggplot(aes_string(
        x = colnames(res)[1],
        y = colnames(res)[length(res) - 1]
      )) +
      geom_point() +
      geom_line() +
      scale_x_continuous(
        limits = c(hvec[1], hvec[length(hvec)]),
        breaks = hvec
      ) +
      scale_y_continuous(
        limits = c(0, newm[1]),
        breaks = seq(0, newm[1], by = newm[1] / div[1])
      ) +
      ggtitle(paste0(
        "model ", colnames(res)[length(res) - 1],
        " across ", colnames(res)[1]
      ))

    g_rk <- res %>%
      ggplot(aes_string(
        x = colnames(res)[1],
        y = colnames(res)[length(res)]
      )) +
      geom_point() +
      geom_line() +
      scale_x_continuous(
        limits = c(hvec[1], hvec[length(hvec)]),
        breaks = hvec
      ) +
      scale_y_continuous(
        limits = c(0, newm[2]),
        breaks = seq(0, newm[2], by = newm[2] / div[2])
      ) +
      ggtitle(paste0(
        "model ", colnames(res)[length(res)],
        " across ", colnames(res)[1]
      ))
  }

  if (length(res) == 4) {
    res <- res %>%
      relocate(row, .before = 3) %>%
      relocate(col, .before = 2)
    lava <- res
    lava[[1]] <- as.factor(lava[[1]])
    lava[[2]] <- as.factor(lava[[2]])

    g_ae <- lava %>%
      ggplot(aes(
        x = .data[[colnames(lava)[1]]],
        y = .data[[colnames(lava)[2]]],
        fill = .data[[colnames(lava)[3]]]
      )) +
      geom_tile() +
      scale_fill_gradient()

    g_rk <- lava %>%
      ggplot(aes(
        x = .data[[colnames(lava)[1]]],
        y = .data[[colnames(lava)[2]]],
        fill = .data[[colnames(lava)[4]]]
      )) +
      geom_tile() +
      scale_fill_gradient()
  }

  if (length(res) == 5) {
    res <- res %>%
      relocate(facet, .before = 4) %>%
      relocate(row, .before = 3) %>%
      relocate(col, .before = 2)
    lava <- res
    lava[[1]] <- as.factor(lava[[1]])
    lava[[2]] <- as.factor(lava[[2]])
    lava[[3]] <- as.factor(lava[[3]])

    g_ae <- lava %>%
      ggplot(aes(
        x = .data[[colnames(lava)[1]]],
        y = .data[[colnames(lava)[2]]],
        fill = .data[[colnames(lava)[4]]]
      )) +
      geom_tile() +
      facet_grid(~ .data[[colnames(lava)[3]]])

    g_rk <- lava %>%
      ggplot(aes(
        x = .data[[colnames(lava)[1]]],
        y = .data[[colnames(lava)[2]]],
        fill = .data[[colnames(lava)[5]]]
      )) +
      geom_tile() +
      facet_grid(~ .data[[colnames(lava)[3]]])
  }

  l$full_data <- res

  l$a <- g_ae
  l$b <- g_rk

  names(l)[2] <- paste0("full_", r1)
  names(l)[3] <- paste0("full_", r2)

  if (marg1 == TRUE) {
    if (length(res) > 3) {
      lava <- res
      vc1 <- colnames(lava)[length(lava) - 1]
      vc2 <- colnames(lava)[length(lava)]

      for (i in colnames(lava)[1:(length(lava) - 2)]) {
        marg <- lava %>%
          group_by(.data[[i]]) %>%
          summarise(
            v1 = mean(get(vc1)),
            v2 = mean(get(vc2))
          )

        colnames(marg)[2:3] <- c(vc1, vc2)

        m <- unname(apply(marg[, (length(marg) - 1):length(marg)], 2, max))
        v <- 10^(-3:6)
        ind <- findInterval(m, v)

        newr <- m / (10^(ind - 5))
        rrr <- plyr::round_any(newr, 10, ceiling)

        rrr <- ifelse(newr / rrr < .75, plyr::round_any(newr, 4, ceiling), rrr)

        newm <- rrr * (10^(ind - 5))
        div <- ifelse(0 == (rrr / 5) %% 5, 5,
          ifelse(0 == (rrr / 5) %% 4, 4,
            ifelse(0 == (rrr / 5) %% 3, 3, 4)
          )
        )

        g_m1 <- marg %>%
          ggplot(aes_string(
            x = colnames(marg)[1],
            y = colnames(marg)[length(marg) - 1]
          )) +
          geom_point() +
          geom_line() +
          scale_y_continuous(
            limits = c(0, newm[1]),
            breaks = seq(0, newm[1], by = newm[1] / div[1])
          ) +
          ggtitle(paste0(
            "model ", colnames(marg)[length(marg) - 1],
            " across ", colnames(marg)[1]
          ))

        g_m2 <- marg %>%
          ggplot(aes_string(
            x = colnames(marg)[1],
            y = colnames(marg)[length(marg)]
          )) +
          geom_point() +
          geom_line() +
          scale_y_continuous(
            limits = c(0, newm[2]),
            breaks = seq(0, newm[2], by = newm[2] / div[2])
          ) +
          ggtitle(paste0(
            "model ", colnames(marg)[length(marg)],
            " across ", colnames(marg)[1]
          ))

        l$data <- marg

        l$a <- g_m1
        l$b <- g_m2

        vl <- (length(l) - 2):length(l)
        names(l)[vl] <- paste0(i, "_", c("data", r1, r2))
      }
    }
  }

  if (marg2 == TRUE) {
    if (length(res) > 4) {
      lava <- res
      vc1 <- colnames(lava)[length(lava) - 1]
      vc2 <- colnames(lava)[length(lava)]

      for (i in colnames(lava)[1:(length(lava) - 2)]) {
        mv <- colnames(lava)[1:3][!(colnames(lava)[1:3] %in% i)]

        marg <- lava %>%
          group_by(.data[[mv[1]]], .data[[mv[2]]]) %>%
          summarise(
            v1 = mean(get(vc1)),
            v2 = mean(get(vc2))
          )

        colnames(marg)[3:4] <- c(vc1, vc2)
        marg[[1]] <- as.factor(marg[[1]])
        marg[[2]] <- as.factor(marg[[2]])

        g_mae <- marg %>%
          ggplot(aes(
            x = .data[[colnames(marg)[1]]],
            y = .data[[colnames(marg)[2]]],
            fill = .data[[colnames(marg)[3]]]
          )) +
          geom_tile() +
          scale_fill_gradient()

        g_mrk <- marg %>%
          ggplot(aes(
            x = .data[[colnames(marg)[1]]],
            y = .data[[colnames(marg)[2]]],
            fill = .data[[colnames(marg)[4]]]
          )) +
          geom_tile() +
          scale_fill_gradient()

        l$data <- marg

        l$a <- g_mae
        l$b <- g_mrk

        vl <- (length(l) - 2):length(l)
        names(l)[vl] <- paste0(mv[1], "_", mv[2], "_", c("data", r1, r2))
      }
    }
  }
  l
}


########################################
########################################

fit_control <- trainControl(method = "cv", number = 10)
rf_grid <- expand.grid(
  mtry = c(1, 2, 3, 4),
  ntree = c(25, 100, 200)
)

rffit <- train(factor(Species) ~ .,
  data = iris,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = fit_control
)

rf <- randomForest(factor(Species) ~ .,
  data = iris, mtry = 3
)


x <- rffit
rffit$bestTune
ca <- caret_plot(x)
ca

ca$caret_rmse
ca$caret_rsq

library(mlbench)
data(BostonHousing)

lm_fit <- train(medv ~ . + rm:lstat,
  data = BostonHousing,
  method = "lm"
)

library(rpart)
rpart_fit <- train(medv ~ .,
  data = BostonHousing,
  method = "rpart",
  tuneLength = 5
)
x <- rpart_fit
ca <- caret_plot(x)
ca$caret_rmse
ca$caret_rsq

data(iris)
train_data <- iris[, 1:4]
train_classes <- iris[, 5]

knn_fit1 <- train(train_data, train_classes,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneLength = 10,
  trControl = trainControl(method = "cv")
)

x <- knn_fit1
ca <- caret_plot(x)
ca$results
ca$caret_acc
ca$caret_kappa


library(caret)
library(MASS)

library(plyr)
library(dplyr)
library(tidyr)
set.seed(732)
fit_control <- trainControl(method = "cv", number = 10)

gbm_grid <- expand.grid(
  interaction.depth = c(4, 10),
  n.trees = c(100, 200),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = 10
)

gbm_fit <- train(medv ~ .,
  method = "gbm", tuneGrid = gbm_grid,
  trControl = fit_control, data = Boston
)
x <- gbm_fit


gbm_grid <- expand.grid(
  interaction.depth = c(4, 8, 12, 16),
  n.trees = c(100, 300),
  shrinkage = c(0.05, 0.1, 0.2),
  n.minobsinnode = 10
)

gbm_fit <- train(medv ~ .,
  method = "gbm", tuneGrid = gbm_grid,
  trControl = fit_control, data = Boston
)
x <- gbm_fit

cag <- caret_plot(x, sqrt = FALSE, col = "n.trees", marg1 = TRUE, marg2 = TRUE)
cag <- caret_plot(x, sqrt = FALSE, col = "n.trees", facet = "shrinkage")
cag$caret_rmse
cag$caret_rsq
cag$results



fit_control <- trainControl(method = "cv", number = 10)
gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                        shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
gbm_fit <- train(factor(Species) ~ ., method = "gbm", tuneGrid = gbm_grid,
                 trControl = fit_control, data = iris)
p <- caret_plot(gbm_fit, sqrt = FALSE, col = "n.trees", marg1 = TRUE, marg2 = TRUE)

