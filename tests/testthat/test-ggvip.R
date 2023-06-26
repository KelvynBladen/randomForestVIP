test_that("num_var negative", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, num_var = -2)
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
  expect_equal(g$table, g1$table)
})

test_that("num_var greater than p", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, num_var = 5)
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
  expect_equal(g$table, g1$table)
})

test_that("num_var invalid character", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, num_var = "hi")
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
  expect_equal(g$table, g1$table)
})

test_that("num_var proportion", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, num_var = .7)
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
  expect_equal(g$table, g1$table)
})

test_that("type single importance", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = FALSE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, type = "both")
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE, type = 1)
  g2 <- ggvip(rf, scale = FALSE, sqrt = TRUE, type = 2)
  expect_equal(g$table, g1$table)
  expect_equal(g1$table, g2$table)
})

test_that("type invalid character", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, scale = FALSE, sqrt = TRUE, type = "hey")
  g1 <- ggvip(rf, scale = FALSE, sqrt = TRUE)
  expect_equal(g$table, g1$table)
})

test_that("sqrt invalid character", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- ggvip(rf, sqrt = "sup")
  g1 <- ggvip(rf, sqrt = FALSE)
  expect_equal(g$table, g1$table)
})

test_that("ggtable matches importance metrics", {
  rf <- randomForest::randomForest(factor(Species) ~ .,
    importance = TRUE, data = iris
  )
  g <- as.data.frame(importance(rf, scale = FALSE)[, 4:5]) %>%
    arrange(desc(MeanDecreaseAccuracy))
  g1 <- ggvip(rf, sqrt = FALSE)
  g1 <- g1$table[, -3]
  expect_equal(g, g1)
})
