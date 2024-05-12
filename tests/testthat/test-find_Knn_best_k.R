# Improved test file
library(testthat)
library(KMEANS.KNN)

test_that("function handles different k_values correctly", {
  data(iris)
  test_result <- find_Knn_best_k(iris, "Species", 2:7, 0.8)
  expect_type(test_result, "list")

})

test_that("function returns accurate predictions", {
  data(iris)
  test_result <- find_Knn_best_k(iris, "Species", 2:7, 0.8)
  accuracies <- test_result[[1]]$Accuracy
  expect_true(all(accuracies <= 1 & accuracies >= 0))
})





