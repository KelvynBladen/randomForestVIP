
library(ranger)
#' Non-linear Variance Inflation Factors
#' @name robust_vifs
#' @importFrom rpart rpart
#' @importFrom stats lm model.frame
#' @importFrom ggplot2 ggplot geom_point xlim ylim geom_line ggtitle geom_vline
#' @importFrom dplyr %>% arrange desc
#' @importFrom car vif
#' @importFrom Metrics sse
#' @importFrom e1071 svm
#' @importFrom wrapr orderv
#' @description A list of data.frames and useful plots for user evaluations of
#'   the randomForest hyperparameter mtry.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data 	an optional data frame containing the variables in the model.
#'   By default the variables are taken from the environment which the model
#'   is called from.
#' @param model Model to use for extraction partial correlations. Possible
#'   model choices are rpart.
#' @param log10 Applies a log10 transformation to VIFs when True. Default is
#'   True.
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

  mf <- model.frame(formula, data = data)
  m <- ncol(mf) - 1

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

    # Consider Fixes that use a test or OOB or CV error rather than
    # training Error.
    r2 <- 1 - (Metrics::sse(as.numeric(mf[, k]), predict(r, mf[, -c(1, k)])) /
      Metrics::sse(as.numeric(mf[, k]), mean(as.numeric(mf[, k]))))
    vdf[k - 1, 4] <- 1 / (1 - r2)
    vdf[k - 1, 5] <- r2
  }

  if (log10 == TRUE) {
    vdf$lm_vif <- log10(vdf$lm_vif)
    vdf$model_vif <- log10(vdf$model_vif)
    colnames(vdf)[c(2, 4)] <- c("Log10_lm_vif", "Log10_model_vif")
  }

  vdf <- vdf[wrapr::orderv(vdf[2]), ]
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

  if (log10 != TRUE) {
    g <- vdfl %>% ggplot(aes(y = var, x = lm_vif)) +
      geom_point() +
      xlim(0, max(c(vdf$Log10_lm_vif, 10))) +
      ggtitle("Linear VIFs") +
      geom_vline(xintercept = 10, color = "blue")
  } else {
    g <- vdfl %>% ggplot(aes(y = var, x = Log10_lm_vif)) +
      geom_point() +
      xlim(0, max(c(vdf$Log10_lm_vif, 1))) +
      ggtitle("Log10 Linear VIFs") +
      geom_vline(xintercept = 1, color = "blue")
  }

  g1 <- vdfl %>% ggplot(aes(y = var, x = lm_r2)) +
    geom_point() +
    xlim(0, 1) +
    ggtitle("Linear R2 for Modeling each Predictor on all Others") +
    geom_vline(xintercept = 0.9, color = "blue")

  vdf <- vdf[wrapr::orderv(vdf[4]), ]
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

  if (log10 != TRUE) {
    g2 <- vdfm %>% ggplot(aes(y = var, x = model_vif)) +
      geom_point() +
      xlim(0, max(c(vdf$model_vif, 10))) +
      ggtitle("Non-linear VIFs") +
      geom_vline(xintercept = 10, color = "blue")
  } else {
    g2 <- vdfm %>% ggplot(aes(y = var, x = Log10_model_vif)) +
      geom_point() +
      xlim(0, max(c(vdf$Log10_model_vif, 1))) +
      ggtitle("Log10 Non-Linear VIFs") +
      geom_vline(xintercept = 1, color = "blue")
  }

  g3 <- vdfm %>% ggplot(aes(y = var, x = model_R2)) +
    geom_point() +
    xlim(0, 1) +
    ggtitle("Non-linear R2 for Modeling each Predictor on all Others") +
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

# library(car)
# library(MASS)
# library(rpart)
#
# cor(iris[1:4])
# pairs(iris[1:4])
# robust_vifs(formula = Petal.Length ~ ., data = iris[1:4],
#             model = randomForest)
# robust_vifs(medv ~ ., data = Boston)
#
#### TOY #######################################################################
# set.seed(1234)
# x = -10:10
# y = x^2 #+ rnorm(21, sd = .25)
# w = -x + rnorm(21, sd = 3)
# z = x + y + rnorm(21)
# toy <- as.data.frame(cbind(w, x, y, z))
#
# toy1 <- model.matrix(z ~ ., data = toy)
# m <- mine(x = toy[-4], y = toy$z)
# m$MIC
#
# p = partial_cor(z ~ ., data = toy)
# p
# r = robust_vifs(z ~ ., data = toy)
# r
#
# library(rmi)
# ?knn_mi()
# k <- rmi::knn_mi(toy1[,1:2], splits = c(1,1),
#                  options = list(method = "KSG2", k = 5))
# k

# Average of mutual Infos
# k <- mat.or.vec(3,3)
# for(i in 1:2) {
#   for(j in (i+1):3) {
#     k[i,j] <- rmi::knn_mi(toy1[,c(i+1,j+1)], splits = c(1,1),
#                           options = list(method = "KSG2", k = 10))
#   }
# }
# k
#
# mi1 <- mean(c(k[1,2], k[1,3]))
# mi2 <- mean(c(k[1,2], k[2,3]))
# mi3 <- mean(c(k[1,3], k[2,3]))
# c(mi1, mi2, mi3)
# toy1[,-1]
# kl <- rmi::knn_mi(toy1[,-1], splits = c(1,1,1),
#             options = list(method = "LNC", k = 5))
# kl
#
# p$y_cors

# cor(toy)
# robust_vifs(formula = z ~ ., data = toy, model = randomForest)
#
# robust_vifs(medv ~ ., data = Boston, model = randomForest)
#
#
# g <- gam(y~s(x), data = toy)
# plot(g)
# p <- predict(g, toy[2:3])
#
# cor(p, toy$y)
# Metrics::mse(p, toy$y)
# r2 = 1 - (Metrics::sse(toy$y, p) / Metrics::sse(toy$y, mean(toy$y)))
# vifs = 1 / (1 - r2)
# vifs

### Mutual Information #########################################################
# library(entropy)
# library(infotheo)
#
# car::vif(iris[1:4])
# ?car::vif
#
# ?entropy::Gstat
# ?entropy::entropy()
# ?entropy::KL.plugin()
# ?entropy::mi.plugin()
#
# freqs1 = c(1/5, 2/5, 3/5)
# freqs2 = c(1/10, 1/10, 1/10)
# KL.plugin(freqs1, freqs2)
#
# freqs2d = rbind( c(0.2, 0.1, 0.15), c(0.1, 0.2, 0.25) )
# freqs2d = cbind( c(0.2, 0.1, 0.15), c(0.1, 0.2, 0.25) )
#
# m <- diag(x = 0, nrow = 4, ncol = 4)
# m
#
# for(i in 1:4) {
#   for(j in 1:4) {
#     m[i,j] <- mi.plugin(iris[c(i,j)])
#   }
# }
#
# round(m, 3)
# abs(cor(iris[1:4]))
# pairs(iris[1:4])
#
# mi.plugin(iris[1:4])
#
# freqs2d = iris[1:2]
# mi.plugin(freqs2d)
#
# freqs2d = iris[c(1,3)]
# mi.plugin(freqs2d)
#
# freqs2d = iris[c(1,4)]
# mi.plugin(freqs2d)
#
# freqs2d = iris[2:3]
# mi.plugin(freqs2d)
#
# freqs2d = iris[c(2,4)]
# mi.plugin(freqs2d)
#
# freqs2d = iris[3:4]
# mi.plugin(freqs2d)
#
#
#
# mi.plugin(cbind(iris[1], 2*iris[1]))
# mi.plugin(cbind(iris[1], iris[1]^2))
# mi.plugin(cbind(iris[1], exp(iris[1])))
#
# x = 1:10
# y = x^2
# mi.plugin(cbind(x,y))
# KL.plugin(x,y)
#
# v = -5:5
# x = v + 6
# y = v^2 + 1
# mi.plugin(cbind(x,y))
# KL.plugin(x,y)
#
# y = x/x
# mi.plugin(cbind(x,y))
# KL.plugin(x,y)
#
#
# library(infotheo)
# infotheo::interinformation(iris[1:4]*10)
# infotheo::multiinformation(iris[1:2]*10)
# infotheo::mutinformation(iris[1:4]*10)
# abs(cor(iris[1:4]))
# iris[1:4]
# infotheo::discretize(iris[1:4], nbins = NROW(iris[1:4])^(1/2))
# y2d = discretize2d(iris$Sepal.Length, iris$Petal.Length, 6, 6)
# entropy::entropy(y2d)/ log(36)
# mi.empirical(y2d)
#
# y2d = discretize2d(iris$Sepal.Width, iris$Petal.Length, 6, 6)
# entropy::entropy(y2d)/ log(36)
# mi.empirical(y2d)
#
# y2d = discretize2d(iris$Petal.Width, iris$Petal.Length, 6, 6)
# entropy::entropy(y2d)/ log(36)
# mi.empirical(y2d)
#
# cor(iris[1:4])
# pairs(iris[1:4])
#
# h <- abs(cor(iris[1:4]))
#
# for(i in 1:4) {
#   for(j in 1:4) {
#     y2d = discretize2d(iris[,i], iris[,j], 5, 5)
#     h[i,j] = mi.empirical(y2d)
#   }
# }
# h
# abs(cor(iris[1:4]))
#
#
# h1 <- abs(cor(iris[1:4]))
#
# for(i in 1:4) {
#   for(j in 1:4) {
#     y2d = discretize2d(iris[,i], iris[,j], 7, 7)
#     h1[i,j] = mi.empirical(y2d)
#   }
# }
# h1
# abs(cor(iris[1:4]))
#
# h2 <- abs(cor(iris[1:4]))
#
# for(i in 1:4) {
#   for(j in 1:4) {
#     y2d = discretize2d(iris[,i], iris[,j], 10, 10)
#     h2[i,j] = mi.empirical(y2d)
#   }
# }
# h2
#
# class(iris)
# abs(cor(iris[1:4]))
# install.packages("minerva")
# library(minerva)
# mn <- minerva::mine(iris[2:4], iris[1])
# mn$MIC
