
# test that v matches rf:imp output

# test that type arg works

# test nvar arg: try more than p, neg, 1 val, prop
#library(testthat)

#replace order() with wrapr::orderv() for xtfrm data frames

# test_that("n.var negative", {
#   rf <- randomForest::randomForest(factor(Species) ~ .,
#                                    importance = TRUE, data = iris)
#   g <- ggvip(rf, scale = FALSE, sqrt = TRUE, n.var = -2)
#   g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
#   expect_equal(g, g1)
# })
#
# test_that("n.var greater than p", {
#   rf <- randomForest::randomForest(factor(Species) ~ .,
#                                    importance = TRUE, data = iris)
#   g <- ggvip(rf, scale = FALSE, sqrt = TRUE, n.var = -2)
#   g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
#   expect_equal(g, g1)
# })
#
# test_that("n.var 0", {
#   rf <- randomForest::randomForest(factor(Species) ~ .,
#                                    importance = TRUE, data = iris)
#   g <- ggvip(rf, scale = FALSE, sqrt = TRUE, n.var = -2)
#   g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
#   expect_equal(g, g1)
# })
#
# test_that("n.var proportion", {
#   rf <- randomForest::randomForest(factor(Species) ~ .,
#                                    importance = TRUE, data = iris)
#   g <- ggvip(rf, scale = FALSE, sqrt = TRUE, n.var = -2)
#   g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
#   expect_equal(g, g1)
# })

#######################################

# test_that("move test_1", {
#   sim <- move_cars(test_1[[1]], trials = length(test_1) - 1)
#   list_len <- length(test_1)
#   for (i in seq_len(list_len)) {
#     expect_equal(sim[[i]], test_1[[i]])
#   }
# })
#
# test_that("move test_2", {
#   sim <- move_cars(test_2[[1]], trials = length(test_2) - 1)
#   list_len <- length(test_2)
#   for (i in seq_len(list_len)) {
#     expect_equal(sim[[i]], test_2[[i]])
#   }
# })
#
# test_that("move test_3", {
#   sim <- move_cars(test_3[[1]], trials = length(test_3) - 1)
#   list_len <- length(test_3)
#   for (i in seq_len(list_len)) {
#     expect_equal(sim[[i]], test_3[[i]])
#   }
# })
#
# test_that("move test_4", {
#   sim <- move_cars(test_4[[1]], trials = length(test_4) - 1)
#   list_len <- length(test_4)
#   for (i in seq_len(list_len)) {
#     expect_equal(sim[[i]], test_4[[i]])
#   }
# })
#
# test_that("Car Count Constant", {
#   trials <- 10
#   sim <- move_cars(grid = initialize_grid(p = .5, rho = .5), trials = trials)
#   expect_equal(sum(sim[[1]] == 2), sum(sim[[trials + 1]] == 2))
#   expect_equal(sum(sim[[1]] == 1), sum(sim[[trials + 1]] == 1))
# })
#
# test_that("Grid Lock maintained", {
#   trials <- 10
#   sim <- move_cars(grid = initialize_grid(rho = .9999), trials = trials)
#   expect_equal(sim[[1]], sim[[trials + 1]])
# })
#
# test_that("Config for 1 row", {
#   grid <- initialize_grid(r = 1)
#   expect_equal(nrow(grid), 1)
# })
#
# test_that("Config for 1 column", {
#   grid <- initialize_grid(c = 1)
#   expect_equal(ncol(grid), 1)
# })
#
# test_that("Config Blue for 1 row", {
#   trials <- 2
#   c <- 5
#   sim <- move_cars(grid = initialize_grid(r = 1, c = c, p = 0), trials = trials)
#   expect_equal(sim[[1]], sim[[trials + 1]])
# })
#
# test_that("Config Red for 1 col", {
#   trials <- 2
#   r <- 5
#   sim <- move_cars(grid = initialize_grid(r = r, c = 1, p = 1), trials = trials)
#   expect_equal(sim[[1]], sim[[trials + 1]])
# })
#
#
# test_that("Grid isn't overriden", {
#   grid1 <- initialize_grid()
#   sim <- move_cars(
#     grid = grid1, rho = 2, r = 2, c = 3, p = .5,
#     trials = 10
#   )
#   expect_equal(sim[[1]], grid1)
# })

#################

# test_that("No Red", {
#   expect_equal(sum(initialize_grid_cpp(p = 0) == 2), 0)
# })
#
# test_that("No Blue", {
#   expect_equal(sum(initialize_grid_cpp(p = 1) == 1), 0)
# })
#
# test_that("No Cars", {
#   expect_equal(sum(initialize_grid_cpp(rho = 0) != 0), 0)
# })
#
# test_that("All Cars", {
#   expect_equal(sum(initialize_grid_cpp(r = 10, c = 10, rho = .999) == 0), 0)
# })
#
# test_that("One Car", {
#   expect_equal(sum(initialize_grid_cpp(rho = 1) != 0), 1)
# })
#
# test_that("Random Rho", {
#   r <- 10
#   c <- 15
#   rho <- runif(1, 2, r * c)
#   expect_equal(sum(initialize_grid_cpp(rho = rho) == 0), (c * r) - round(rho))
# })
