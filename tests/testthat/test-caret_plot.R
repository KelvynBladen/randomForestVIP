
# test col, row, facet, as invalid or duplicates

test_that("sqrt invalid character", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x, sqrt = FALSE)
  p1 <- caret_plot(x, sqrt = "tt")

  expect_equal(p$full_data, p1$full_data)
})

test_that("marg1 invalid character", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x, marg1 = FALSE)
  p1 <- caret_plot(x, marg1 = "tt")

  expect_equal(p$full_data, p1$full_data)
})

test_that("marg2 invalid character", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x, marg2 = FALSE)
  p1 <- caret_plot(x, marg2 = "tt")

  expect_equal(p$full_data, p1$full_data)
})

test_that("col mapping", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x)
  p1 <- caret_plot(x, col = "interaction.depth")
  expect_equal(p$full_data, p1$full_data)
})

test_that("row mapping", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x)
  p1 <- caret_plot(x, row = "interaction.depth")
  expect_equal(p$full_data, p1$full_data)
})

test_that("facet mapping", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x)
  p1 <- caret_plot(x, facet = "n.trees")
  expect_equal(p$full_data, p1$full_data)
})

test_that("duplicate mapping", {
  set.seed(123)
  fit_control <- caret::trainControl(method = "cv", number = 10)
  gbm_grid <- expand.grid(interaction.depth = c(1, 4), n.trees = c(15, 150),
                          shrinkage = c(0.05, 0.1), n.minobsinnode = 10)
  x <- caret::train(factor(Species) ~ .,
                    method = "gbm", tuneGrid = gbm_grid,
                    trControl = fit_control, data = iris)

  p <- caret_plot(x)
  p1 <- caret_plot(x, row = "interaction.depth",
                   col = "interaction.depth")
  expect_equal(p$full_data, p1$full_data)
})

