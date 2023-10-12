# test nvar arg: try more than p, neg, 1 val, prop

# test mvec arg: proportion, negative values, too large values, 1 value,
# more values than variables,

test_that("num_var negative", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris, num_var = -2,
    sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("num_var greater than p", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris, num_var = 7,
    sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("num_var invalid character", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    num_var = "aa", sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec decimal", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = c(1.8), sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE, mvec = c(2)
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec 0", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = 0, sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec too large", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = 1:6, sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec proportion", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = c(.2, .4, .7, .9), sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec prop to 0", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = c(.01, .7), sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE, mvec = 3
  )
  expect_equal(m$importance, m1$importance)
})

test_that("mvec negative", {
  set.seed(123)
  m <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    mvec = c(-4, -3, -2, 0), sqrt = TRUE
  )
  set.seed(123)
  m1 <- mtry_compare(
    formula = factor(Species) ~ ., data = iris,
    sqrt = TRUE
  )
  expect_equal(m$importance, m1$importance)
})

test_that("sqrt invalid character", {
  set.seed(123)
  m <- mtry_compare(formula = factor(Species) ~ ., data = iris, sqrt = "ss")
  set.seed(123)
  m1 <- mtry_compare(formula = factor(Species) ~ ., data = iris, sqrt = FALSE)
  expect_equal(m$importance, m1$importance)
})

test_that("Non-numeric Numbers", {
  set.seed(123)
  m <- mtry_compare(formula = as.integer(carb) ~ ., data = mtcars)
  set.seed(123)
  m1 <- mtry_compare(formula = carb ~ ., data = mtcars)
  expect_equal(m$importance, m1$importance)
})
