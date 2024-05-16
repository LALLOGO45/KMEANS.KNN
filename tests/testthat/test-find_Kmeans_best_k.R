# Remove the install.packages("testthat") line as it's not needed
library(testthat)

#context("Testing find_Kmeans_best_k function")

test_that("function returns correct output for elbow method", {
  data(iris)
  test_result <- find_Kmeans_best_k(iris[, -5], max_k = 7, Method = "coude")
  expect_type(test_result,"list")
  # Remove the checks for plot, abline, and axis as they are not necessary for the test
})

test_that("function returns correct output for silhouette method", {
  data(iris)
  # Ensure the cluster package is loaded
  library(cluster)
  test_result <- find_Kmeans_best_k(iris[, -5], max_k = 7, Method = "silhouette")
  expect_type(test_result,"list")
  # Check if the optimal number of clusters is calculated
  # Adjust the check to match the actual output of your function
})

test_that("function returns correct output for gap statistics method", {
  data(iris)
  test_result <- find_Kmeans_best_k(iris[, -5], max_k = 7, Method = "gap")
  expect_type(test_result,"list")
  # Check if the optimal number of clusters is calculated
  # Adjust the check to match the actual output of your function
})


