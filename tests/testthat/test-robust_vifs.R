# test model args

test_that("num_var negative", {
  set.seed(123)
  p <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris, num_var = -2)
  set.seed(123)
  p1 <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris)
  expect_equal(p$summary, p1$summary)
})

test_that("num_var greater than p", {
  set.seed(123)
  p <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris, num_var = 7)
  set.seed(123)
  p1 <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris)
  expect_equal(p$summary, p1$summary)
})

test_that("num_var invalid character", {
  set.seed(123)
  p <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris, num_var = "bb")
  set.seed(123)
  p1 <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris)
  expect_equal(p$summary, p1$summary)
})

test_that("num_var prop", {
  set.seed(123)
  p <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris, num_var = .6)
  set.seed(123)
  p1 <- robust_vifs(formula = Petal.Length ~ Sepal.Width +
    Sepal.Length + Petal.Width, data = iris, num_var = 2)
  expect_equal(p$summary, p1$summary)
})
