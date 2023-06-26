
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
# library(minerva)
# mn <- minerva::mine(iris[2:4], iris[1])
# mn$MIC
