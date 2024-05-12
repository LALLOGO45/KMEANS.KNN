#' knn_Function
#'
#' This function implements a custom K-Nearest Neighbors (KNN) algorithm with data preprocessing options. It predicts the class of a new point based on the k closest neighbors in the feature space.
#'
#' @param new_points A dataframe of new points to be classified.
#' @param dataset A dataframe of training data.
#' @param k The number of nearest neighbors to consider.
#' @param distance_metric The distance metric for calculating neighbors ('gower', 'euclidean', 'manhattan').
#' @param target_variable The name of the target variable in 'dataset'.
#' @param scale_data A boolean to indicate whether the data should be normalized.
#' @param impute_data The imputation method for missing values ('mean', 'median', 'mode').
#' @param weight_votes A boolean to indicate whether votes should be weighted by the inverse of the distance.
#'
#' @return A list containing 'Predictions' with the predicted class for each new point, 'Data' with the 'new_points' dataframe and an additional column for predictions, 'Distances' with the distances of the k nearest neighbors, and 'Imputed_Values' with the imputed values for missing variables.
#' @export
#' @importFrom stats median
#' @importFrom cluster daisy
#' @examples
#' # Loading training data (e.g., iris)
#' data(iris)
#'
#' # Preparing new points for prediction (e.g., two new observations)
#' new_points <- data.frame(Sepal.Length = c(5.1, 7.7, 1.3, 0.2, 5.1),
#' Sepal.Width = c(3.5, 2.6, 5, 3.7, 3.5),
#' Petal.Length = c(1.4 , 6.9, 4.5, 6, 3.4),
#' Petal.Width = c(10.1, 7.6, 5.6, 8.4, 5.2))
#'
#' # Calling the custom KNN function
#' results <- knn_Function(new_points, dataset = iris, k = 3, target_variable = "Species")
#'
#' # Displaying predictions
#' print(results$Predictions)
knn_Function <- function(new_points, dataset, k = 5, distance_metric = "gower", target_variable , scale_data = TRUE, impute_data = "mean", weight_votes = TRUE) {

  # Data preparation for distance calculation
  dataset_with_new_point <- rbind(dataset[, setdiff(names(dataset), target_variable)], new_points)

  # Function to impute missing values
  impute <- function(dataset_with_new_point, method) {
    imputed_values <- list()
    if (any(is.na(dataset_with_new_point))) {
      for (i in 1:ncol(dataset_with_new_point)) {
        if (method == "mean") {
          imputed_value <- mean(dataset_with_new_point[, i], na.rm = TRUE)
          dataset_with_new_point[is.na(dataset_with_new_point[, i]), i] <- imputed_value
        } else if (method == "median") {
          imputed_value <- median(dataset_with_new_point[, i], na.rm = TRUE)
          dataset_with_new_point[is.na(dataset_with_new_point[, i]), i] <- imputed_value
        } else if (method == "mode") {
          imputed_value <- as.numeric(names(sort(table(dataset_with_new_point[, i]), decreasing = TRUE)[1]))
          dataset_with_new_point[is.na(dataset_with_new_point[, i]), i] <- imputed_value
        } else {
          stop("Unsupported imputation method. Choose 'mean', 'median', or 'mode'.")
        }
        imputed_values[[i]] <- imputed_value
      }
    }
    return(list(dataset_with_new_point = dataset_with_new_point, imputed_values = imputed_values))
  }
  # Imputation of missing values
  imputation_result <- impute(dataset_with_new_point, impute_data)
  dataset_with_new_point <- imputation_result$dataset_with_new_point
  imputed_values <- imputation_result$imputed_values

  # If scale_data is TRUE, scale the data
  if (scale_data) {
    dataset_with_new_point <- scale(dataset_with_new_point)
  }

  predictions <- vector("character", length = nrow(new_points))
  distances_list <- list()
  for (i in 1:nrow(new_points)) {
    new_point <- as.vector(new_points[i, ])

    # Distance calculation using the specified metric
    distances <- switch(
      distance_metric,
      "gower" = as.matrix(daisy(dataset_with_new_point, metric = "gower"))[1:nrow(dataset), nrow(dataset_with_new_point)],
      "euclidean" = sqrt(rowSums((dataset[, setdiff(names(dataset), target_variable)] - new_point)^2)),
      "manhattan" = rowSums(abs(dataset[, setdiff(names(dataset), target_variable)] - new_point))
    )

    # Sorting distances and selecting the K nearest neighbors
    nearest_indices <- order(distances)[1:k]
    nearest_labels <- dataset[[target_variable]][nearest_indices]

    # Majority vote for the class of the new point
    if (weight_votes) {
      # Weight votes by inverse of distance
      weights <- 1 / distances[nearest_indices]
      predicted_class <- names(sort(table(nearest_labels), decreasing = TRUE, useNA = "ifany"))[1]
    } else {
      predicted_class <- names(sort(table(nearest_labels), decreasing = TRUE))[1]
    }
    predictions[i] <- predicted_class
    distances_list[[i]] <- distances[nearest_indices]
  }
  # Check if the imputed_values list is empty
  if(length(unlist(imputed_values)) == 0){
    imputed_values <- "No values imputed"
  }

  # Adding the predictions column to the new_points dataframe
  new_points$Predictions <- predictions
  #return(predictions)
  #return(new_points)
  return(list(Predictions = predictions, Data = new_points, Distances = distances_list, Imputed_Values = imputed_values))
}
