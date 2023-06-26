# test output for numeric, character, factor response

test_that("num_var negative", {
  set.seed(123)
  p <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width, num_var = -2)
  set.seed(123)
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width)
  expect_equal(p$y_cors, p1$y_cors)
  expect_equal(p$y_partial_cors, p1$y_partial_cors)
  expect_equal(p$y_mutual_info, p1$y_mutual_info)
})

test_that("num_var greater than p", {
  set.seed(123)
  p <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width, num_var = 7)
  set.seed(123)
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width)
  expect_equal(p$y_cors, p1$y_cors)
  expect_equal(p$y_partial_cors, p1$y_partial_cors)
  expect_equal(p$y_mutual_info, p1$y_mutual_info)
})

test_that("num_var invalid character", {
  set.seed(123)
  p <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width, num_var = "aa")
  set.seed(123)
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width)
  expect_equal(p$y_cors, p1$y_cors)
  expect_equal(p$y_partial_cors, p1$y_partial_cors)
  expect_equal(p$y_mutual_info, p1$y_mutual_info)
})

test_that("num_var prop", {
  set.seed(123)
  p <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width, num_var = 0.6)
  set.seed(123)
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width, num_var = 2)
  expect_equal(p$y_cors, p1$y_cors)
  expect_equal(p$y_partial_cors, p1$y_partial_cors)
  expect_equal(p$y_mutual_info, p1$y_mutual_info)
})

test_that("model character", {
  p <- suppressWarnings(partial_cor(formula = iris$Petal.Length ~
    iris$Sepal.Width + iris$Sepal.Length +
    iris$Petal.Width, model = "lm"))
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width)
  expect_equal(p$y_cors, p1$y_cors)
})

test_that("model bad character", {
  p <- suppressWarnings(partial_cor(formula = iris$Petal.Length ~
    iris$Sepal.Width + iris$Sepal.Length +
    iris$Petal.Width, model = "aa"))
  p1 <- partial_cor(formula = iris$Petal.Length ~ iris$Sepal.Width +
    iris$Sepal.Length + iris$Petal.Width)
  expect_equal(p$y_cors, p1$y_cors)
})

test_that("model options run", {
  p <- partial_cor(
    formula = Petal.Length ~ Sepal.Width + Sepal.Length +
      Petal.Width, data = iris,
    model = randomForest::randomForest
  )
  p1 <- partial_cor(formula = Petal.Length ~ Sepal.Width + Sepal.Length +
    Petal.Width, data = iris, model = e1071::svm)
  expect_equal(p$y_cors, p1$y_cors)
})
