#' Variable Importance GGPlot
#' @name ggvip
#' @importFrom randomForest importance
#' @importFrom dplyr %>% arrange desc filter between case_when
#' @importFrom ggplot2 ggplot geom_point xlim xlab ylab ggtitle theme
#'   aes_string element_text
#' @importFrom gridExtra grid.arrange
#' @importFrom wrapr orderv
#' @importFrom plyr round_any
#' @description A ggplot of variable importance as measured by a Random Forest.
#' @param x An object of class randomForest.
#' @param scale For permutation based measures such as MSE or Accuracy, should
#'   the measures be divided by their "standard errors"? Default is False.
#' @param sqrt Boolean value indicating whether importance metrics should be
#'   adjusted via a square root transformation. Default is True.
#' @param type either 1 or 2, specifying the type of importance measure
#'   (1=mean decrease in accuracy or % increase in MSE, 2 = mean decrease in
#'   node impurity or mean decrease in gini). Default is "both".
#' @param num_var Optional argument for reducing the number of variables to the
#'   top 'num_var'. Must be an integer between 1 and the total number of predictor
#'   variables in the model.
#' @return A ggplot dotchart showing the importance of the variables that were
#'   plotted.
#' @examples
#' rf <- randomForest::randomForest(factor(Species) ~ .,
#'   importance = TRUE,
#'   data = iris
#' )
#' ggvip(rf, scale = FALSE, sqrt = TRUE, type = "both")
#' @export

ggvip <- function(x, scale = FALSE, sqrt = TRUE, type = "both", num_var) {
  imp_values <- as.data.frame(randomForest::importance(x, scale = scale))

  num_cols <- ncol(imp_values)
  imp_values <- imp_values[(num_cols - 1):num_cols]

  imp0 <- imp_values
  imp0[, ] <- 0
  imp_frame <- pmax(imp_values, imp0)

  if (sqrt == TRUE) {
    imp_frame <- sqrt(imp_frame)
  }

  imp_frame$var <- rownames(imp_frame)

  if (!missing(num_var) && is.numeric(num_var)) {
    num_var <- ifelse(between(num_var, 1, nrow(imp_frame)),
      num_var, nrow(imp_frame)
    )
    num_var <- round(num_var)

    d <- imp_frame %>%
      arrange(desc(get(colnames(imp_frame)[1]))) %>%
      filter(get(colnames(imp_frame)[1]) >=
               get(colnames(imp_frame)[1])[num_var])

    imp_frame <- imp_frame %>%
      filter(var %in% d$var)
  }

  if (length(colnames(imp_frame)) == 2) {
    imp_frame <- imp_frame[wrapr::orderv(imp_frame[1]), ]
    imp_frame$var <- factor(imp_frame$var, levels = c(rownames(imp_frame)))

    m <- max(imp_frame[1])
    v <- 10^(-3:6)
    ind <- findInterval(m, v)

    newr <- m / (10^(ind - 5))
    rrr <- plyr::round_any(newr, 10, ceiling)

    if (newr / rrr < 3 / 4) {
      rrr <- plyr::round_any(newr, 4, ceiling)
    }

    newm <- rrr * (10^(ind - 5))

    div <- case_when(
      (rrr / 5) %% 5 == 0 ~ 5,
      (rrr / 5) %% 4 == 0 ~ 4,
      (rrr / 5) %% 3 == 0 ~ 3,
      .default = 4
    )

    g <- imp_frame %>%
      ggplot(aes_string(
        x = colnames(imp_frame)[1],
        y = colnames(imp_frame)[2]
      )) +
      geom_point() +
      scale_x_continuous(
        limits = c(0, newm),
        breaks = seq(0, newm, by = newm / div)
      ) +
      ylab(NULL) +
      xlab(ifelse(sqrt == FALSE, colnames(imp_frame)[1],
        paste0("sqrt(", colnames(imp_frame)[1], ")")
      )) +
      ggtitle("VIP") +
      theme(plot.title = element_text(hjust = 0.5))

    l <- list()
    l$vip <- g
    l$table <- imp_frame
    l
  } else {
    imp_frame <- imp_frame[wrapr::orderv(imp_frame[1]), ]
    imp_frame$var <- factor(imp_frame$var, levels = c(rownames(imp_frame)))

    if (colnames(imp_frame)[1] == "%IncMSE") {
      colnames(imp_frame)[1] <- "PctIncMSE"
    }

    m <- max(imp_frame[1])
    v <- 10^(-3:6)
    ind <- findInterval(m, v)

    newr <- m / (10^(ind - 5))
    rrr <- plyr::round_any(newr, 10, ceiling)

    if (newr / rrr < 3 / 4) {
      rrr <- plyr::round_any(newr, 4, ceiling)
    }

    newm <- rrr * (10^(ind - 5))

    div <- case_when(
      (rrr / 5) %% 5 == 0 ~ 5,
      (rrr / 5) %% 4 == 0 ~ 4,
      (rrr / 5) %% 3 == 0 ~ 3,
      .default = 4
    )

    imp_frame1 <- imp_frame

    imp_frame1 <- imp_frame1[rev(wrapr::orderv(imp_frame1[1])), ]

    g1 <- imp_frame %>%
      ggplot(aes_string(
        x = colnames(imp_frame)[1],
        y = colnames(imp_frame)[3]
      )) +
      geom_point() +
      scale_x_continuous(
        limits = c(0, newm),
        breaks = seq(0, newm, by = newm / div)
      ) +
      ylab(NULL) +
      xlab(ifelse(sqrt == FALSE, colnames(imp_frame)[1],
        paste0("sqrt(", colnames(imp_frame)[1], ")")
      ))

    imp_frame <- imp_frame[wrapr::orderv(imp_frame[2]), ]
    imp_frame$var <- factor(imp_frame$var, levels = c(rownames(imp_frame)))

    m <- max(imp_frame[2])
    ind <- findInterval(m, v)

    newr <- m / (10^(ind - 5))
    rrr <- plyr::round_any(newr, 10, ceiling)

    if (newr / rrr < 3 / 4) {
      rrr <- plyr::round_any(newr, 4, ceiling)
    }

    newm <- rrr * (10^(ind - 5))

    div <- case_when(
      (rrr / 5) %% 5 == 0 ~ 5,
      (rrr / 5) %% 4 == 0 ~ 4,
      (rrr / 5) %% 3 == 0 ~ 3,
      .default = 4
    )

    imp_frame2 <- imp_frame

    imp_frame2 <- imp_frame2[rev(wrapr::orderv(imp_frame2[2])), ]

    g2 <- imp_frame %>%
      ggplot(aes_string(
        x = colnames(imp_frame)[2],
        y = colnames(imp_frame)[3]
      )) +
      geom_point() +
      scale_x_continuous(
        limits = c(0, newm),
        breaks = seq(0, newm, by = newm / div)
      ) +
      ylab(NULL) +
      xlab(ifelse(sqrt == FALSE, colnames(imp_frame)[2],
        paste0("sqrt(", colnames(imp_frame)[2], ")")
      ))

    l <- list()
    if (type %in% c("mse", "acc", 1)) {
      l$vip <- g1
      l$table <- imp_frame1[, -2]
    } else if (type %in% c("purity", "gini", 2)) {
      l$vip <- g2
      l$table <- imp_frame2[, -1]
    } else {
      l$both_vips <- gridExtra::grid.arrange(g1, g2,
        nrow = 1,
        top = "Variable Importances using ggplot Graphics"
      )
      l$accuracy_vip <- g1
      l$purity_vip <- g2
      l$table <- imp_frame1
    }
    l
  }
}
