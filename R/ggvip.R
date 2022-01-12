#' Variable Importance GGPlot
#' @name ggvip
#' @importFrom randomForest varImpPlot
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_point xlim ylab ggtitle theme aes_string
#' @importFrom gridExtra grid.arrange
#' @description Dotchart of variable importance as measured by a Random Forest
#' @param x An object of class randomForest.
#' @param ... Arguments to be passed to the generic plot function, such as
#' \link{graphical parameters}. Generic plot will accept many arguments
#' including the following:
#'
#' \code{main}: an overall title for the plot: see \link{title}.
#'
#' \code{sub}: a sub title for the plot: see \link{title}.
#'
#' \code{xlab}: a title for the x axis: see \link{title}.
#'
#' \code{ylab}: a title for the y axis: see \link{title}.
#' @return A plot of a 'carsimr' object with cells of white open spaces, blue
#'   cars, and red cars. Plotted lines separate and distinguish unique rows and
#'   columns.
#' @examples
#' rf <- randomForest(factor(Species) ~ ., importance = T, data = iris)
#' ggvip(rf, scale = F, sqrt = T, metrics = "both")
#' @export

ggvip <- function(x, scale = F, sqrt = T, metrics = "both", ...) {
  vf <- as.data.frame(randomForest::varImpPlot(x, scale = scale))
  vf0 <- data.frame(matrix(0, nrow = nrow(vf), ncol = ncol(vf)))
  vf <- pmax(vf, vf0)

  if (sqrt == T) {
    vf <- sqrt(vf)
  }

  vf$var <- rownames(vf)

  if (length(colnames(vf)) == 2) {
    vf <- vf[order(vf[1]), ]
    vf$var <- factor(vf$var, levels = c(rownames(vf)))

    vf %>% ggplot(aes_string(x = colnames(vf)[1], y = colnames(vf)[2])) +
      geom_point() +
      xlim(0, max(vf[1])) +
      ylab(NULL) +
      ggtitle("VIP") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    vf <- vf[order(vf[1]), ]
    vf$var <- factor(vf$var, levels = c(rownames(vf)))

    if (colnames(vf)[1] == "%IncMSE") {
      colnames(vf)[1] <- "IncMSE"
    }

    g <- vf %>% ggplot(aes_string(x = colnames(vf)[1], y = colnames(vf)[3])) +
      geom_point() +
      xlim(0, max(vf[1])) +
      ylab(NULL)

    vf <- vf[order(vf[2]), ]
    vf$var <- factor(vf$var, levels = c(rownames(vf)))

    g1 <- vf %>% ggplot(aes_string(x = colnames(vf)[2], y = colnames(vf)[3])) +
      geom_point() +
      xlim(0, max(vf[2])) +
      ylab(NULL)

    if (metrics %in% c("mse", "acc", 1)) {
      g
    } else if (metrics %in% c("purity", "gini", 2)) {
      g1
    } else {
      gridExtra::grid.arrange(g, g1, nrow = 1, top = "VIP")
    }
  }
}
ggvip(rf, sqrt = T, metrics = "gini")
ggvip(rf1, sqrt = T)


sig <- diag(1, 5, 5)

strobl <- MASS::mvrnorm(1000, mu = rep(0, 5), Sigma = sig)

y <- 5 * strobl[, 1] + 5 * strobl[, 2] + 3 * strobl[, 3] +
  3 * strobl[, 4] + rnorm(1000, mean = 0, sd = 1 / 2)

y <- ifelse(y > 0, 1, 0)

strobl <- data.frame(cbind(strobl, y))

rf <- randomForest::randomForest(y ~ ., mtry = 5, importance = T, data = strobl)
varImpPlot(rf)
vf <- as.data.frame(varImpPlot(rf))
vf
sqrt(vf)

rf1 <- randomForest::randomForest(as.factor(y) ~ .,
                                  mtry = 5,
                                  importance = T, data = strobl
)
varImpPlot(rf1)
vf <- as.data.frame(varImpPlot(rf1))
vf
sqrt(vf1)
