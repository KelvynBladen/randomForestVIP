
#' Mtry Tune via VIPs
#' @name mtry_compare
#' @importFrom randomForest importance randomForest
#' @importFrom ggplotify as.ggplot
#' @importFrom dplyr %>% arrange across ends_with desc filter select
#'   summarise group_by
#' @importFrom ggplot2 ggplot geom_point geom_line ylab ggtitle theme
#'   aes_string scale_x_continuous scale_y_continuous
#' @importFrom gridExtra arrangeGrob
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

mtry_compare <- function(formula, data = NULL, scale = F, sqrt = T,
                         num_var, mvec, ...) {

  model_frame <- model.frame(formula, data = data)
  num_preds <- ncol(model_frame) - 1

  if (!missing(mvec)) {
    if (all(mvec < 1)) {
      mvec <- round(mvec * num_preds)
    } else {
      mvec <- round(mvec)
    }

    mvec <- as.vector(sort(unique(na.omit(ifelse(mvec <= num_preds & mvec > 0,
      mvec, NA
    )))))
  } else {
    m1 <- 1
    m2 <- ifelse(class(model_frame[1, 1]) == "numeric" & num_preds > 2,
                 floor(num_preds / 3), floor(sqrt(num_preds))
    )
    m3 <- ceiling(mean(c(m2, num_preds)))
    mvec <- sort(unique(c(m1, m2, m3, num_preds)))
  }

  # Think can wrap down to 1 if and 1 ifelse statement
  if (!missing(num_var)) {

    num_var <- ifelse(num_var >= num_preds | num_var <= 0, num_preds,
                      ifelse(num_var < 1, round(num_var * num_preds),
                             round(num_var)))
  } else {
    num_var <- num_preds
  }

  for (i in mvec) {
    x <- paste0("srf", i)
    eval(call("<-", as.name(x), randomForest(
      formula = formula, mtry = i,
      importance = T, data = data#, ...
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

    if (sqrt == T) {
      v <- sqrt(v)
    }

    v$names <- rownames(v)
    eval(call("<-", as.name(vf), v))

    y <- get(vf)
    y$mtry <- i
    eval(call("<-", as.name(vf), y))

    # h <- paste0("h", i)
    # eval(call(
    #   "<-", as.name(h),
    #   ggvip(get(x),
    #     scale = scale, sqrt = sqrt,
    #     type = 1, if (!missing(num_var)) {
    #       num_var <- num_var
    #       }
    #   )
    # ))
    #
    # j <- paste0("j", i)
    # eval(call(
    #   "<-", as.name(j),
    #   ggvip(get(x),
    #     scale = scale, sqrt = sqrt,
    #     type = 2, if (!missing(num_var)) {
    #       num_var <- num_var
    #       }
    #   )
    # ))
  }

  err_v <- 0
  for (i in mvec) {
    mod <- get(paste0("srf", i))

    ifelse(class(model_frame[1, 1]) == "numeric",
      err_v <- c(err_v, mod$mse[mod$ntree]),
      err_v <- c(err_v, mod$err.rate[mod$ntree])
    )
  }
  err_v <- err_v[-1]

  ifelse(class(model_frame[1, 1]) == "numeric",
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
      #select(names, colnames(sd)[1]) %>% # IDK why this is failing
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

  g1 <- sd %>%
    ggplot(aes_string(
      x = colnames(sd)[4], y = colnames(sd)[1],
      color = colnames(sd)[3], group = colnames(sd)[3]
    )) +
    geom_point() +
    geom_line() +
    ylim(0, max(sd[1])) +
    ggtitle("Variable Importance Based on Decrease in Error Across mtry") +
    scale_x_continuous(breaks = mvec)

  g2 <- sd %>%
    ggplot(aes_string(
      x = colnames(sd)[4], y = colnames(sd)[2],
      color = colnames(sd)[3], group = colnames(sd)[3]
    )) +
    geom_point() +
    geom_line() +
    ylim(0, max(sd[2])) +
    ggtitle("Variable Importance Based on Purity Contribution Across mtry") +
    scale_x_continuous(breaks = mvec)

  # hl <- list()
  # for (i in seq_len(length(mvec))) {
  #   hl[[i]] <- get(paste0("h", mvec[i]))
  # }
  #
  # jl <- list()
  # for (i in seq_len(length(mvec))) {
  #   jl[[i]] <- get(paste0("j", mvec[i]))
  # }
  #
  # mv <- mvec
  #
  # if (length(hl) > 4) {
  #   q <- unname(quantile(mvec, c(0.33, 0.67)))
  #   u1 <- which.min(abs(mvec[-c(1, length(mvec))] - q[1])) + 1
  #   u2 <- which.min(abs(mvec[-c(1, length(mvec))] - q[2])) + 1
  #   mv <- mvec[c(1, u1, u2, length(mvec))]
  #
  #   hl[[2]] <- hl[[u1]]
  #   hl[[3]] <- hl[[u2]]
  #   hl[[4]] <- hl[[length(hl)]]
  #
  #   jl[[2]] <- jl[[u1]]
  #   jl[[3]] <- jl[[u2]]
  #   jl[[4]] <- jl[[length(hl)]]
  #
  #   hl <- hl[1:4]
  #   jl <- jl[1:4]
  # }
  #
  # if (length(hl) == 4) {  ########### Problem Here !!!!!!!!!!!!!!!
  #   g3 <- as.ggplot(arrangeGrob(hl[[1]], hl[[2]],
  #     hl[[3]], hl[[4]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  #
  #   g4 <- as.ggplot(arrangeGrob(jl[[1]], jl[[2]],
  #     jl[[3]], jl[[4]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  # }
  #
  # if (length(hl) == 3) {
  #   g3 <- as.ggplot(arrangeGrob(hl[[1]], hl[[2]],
  #     hl[[3]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  #
  #   g4 <- as.ggplot(arrangeGrob(jl[[1]], jl[[2]],
  #     jl[[3]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  # }
  #
  # if (length(hl) == 2) {
  #   g3 <- as.ggplot(arrangeGrob(hl[[1]], hl[[2]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  #
  #   g4 <- as.ggplot(arrangeGrob(jl[[1]], jl[[2]],
  #     nrow = 1,
  #     top = paste("mtry: ", mv[1],
  #       paste0(", ", mv[-1],
  #         collapse = ""
  #       ),
  #       sep = ""
  #     )
  #   ))
  # }
  #
  # if (length(hl) == 1) {
  #   g3 <- hl[[1]]
  #
  #   g4 <- jl[[1]]
  # }

  g_err <- err_df %>%
    ggplot(aes_string(x = colnames(err_df)[1], y = colnames(err_df)[2])) +
    geom_point() +
    geom_line() +
    ggtitle("Model Errors Across mtry") +
    scale_x_continuous(limits = c(1, num_preds), breaks = mvec) +
    scale_y_continuous(limits = c(0, 1.25 * max(err_df[colnames(err_df)[2]])))

  rownames(sd_full) <- seq_len(nrow(sd_full))

  l <- list()

  l$importance <- sd_full[c(3, 4, 1, 2)]
  l$model_errors <- err_df
  l$var_imp_error <- k
  l$var_imp_purity <- n
  l$gg_var_imp_error <- g1
  l$gg_var_imp_purity <- g2
  # l$gg_col_var_imp_error <- g3
  # l$gg_col_var_imp_purity <- g4
  l$gg_model_errors <- g_err

  for (i in mvec) {
    x <- paste0("rf", i)
    l[[x]] <- get(paste0("srf", i))
  }

  l
}


# library(rfvip)
# m <- mtry_compare(formula = medv ~ ., data = Boston, num_var = 7,
#                   mvec = c(-1.2, 3, 4, 5, 7, 9, 11.2, 13.3), sqrt = T)
#
m1 <- mtry_compare(formula = medv ~ ., data = Boston, sqrt = T)
m1$gg_model_errors
m$importance
# m$gg_var_imp_error
# m$gg_var_imp_purity
# m$gg_model_errors
m$gg_col_var_imp_error
m$gg_col_var_imp_error
# m <- mtry_compare(formula = factor(Species) ~ ., data = iris, sqrt = T)
# m
#
# mtry_compare_col <- function(formula, data = NULL, scale = F,
#                              sqrt = T, type = 1) {
#   mf <- model.frame(formula, data = data)
#   m <- ncol(mf) - 1
#
#   m2 <- floor(sqrt(m))
#   m3 <- floor(m/3)
#   m4 <- floor(2*m/3)
#
#   for(i in c(m2, m3, m4, m)) {
#     x <- paste0("srf",i)
#     eval(call("<-", as.name(x), randomForest(formula = formula, mtry = i,
#                                              importance = T, data = data)))
#
#     y <- paste0("g",i)
#     eval(call("<-", as.name(y),
#               rfvip::ggvip(get(x), scale = scale, sqrt = sqrt, type = type)))
#   }
#
#   g1 <- get(paste0("g",m2))
#   g2 <- get(paste0("g",m3))
#   g3 <- get(paste0("g",m4))
#   g4 <- get(paste0("g",m))
#
#   gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 1,
#                           top = paste0("mtry: ", m2,", ", m3,", ",
#                                        m4,", ", m))
# }

# data("Boston")
#
# mtry_compare(mpg ~ ., data = mtcars, type = 2)
#
# # standardize all x variables
#
# mtry_compare(medv ~ ., data = Boston, type = 1)
# mtry_compare_col(medv ~ ., data = Boston, type = 1)
#
# B <- as.data.frame(scale(Boston))
# mtry_compare(medv ~ ., data = B, type = 1)
# mtry_compare_col(medv ~ ., data = B, type = 1)

# rf <- foreach(mtry = 3, .combine=randomForest::combine,
#               .multicombine=TRUE, .packages='randomForest') %dopar% {
#                 randomForest(crim ~ ., Boston, mtry = mtry)
#               }
