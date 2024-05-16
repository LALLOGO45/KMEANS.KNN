#' find_Knn_best_k
#'
#' This function finds the best k-value for KNN based on the provided data.
#'
#' @param data A dataframe containing the dataset to be used.
#' @param target_column A string specifying the name of the target column in the dataset.
#' @param k_values A numeric vector containing the different k-values to be tested.
#' @param Prop_train A numeric value between 0 and 1 indicating the proportion of the dataset to be used for training.
#'
#' @return A list containing a dataframe with k-values and their corresponding accuracies, and the best k-value with its accuracy.
#' @export
#' @importFrom caret createDataPartition
#' @importFrom caret confusionMatrix
#' @importFrom class knn
#' @importFrom assertthat assert_that
#' @examples
#' data(iris)
#' find_Knn_best_k(iris,"Species",1:10,Prop_train=0.8)

find_Knn_best_k <- function(data, target_column, k_values, Prop_train=0.8) {

  # Verify that data is a dataframe
  assert_that(is.data.frame(data))

  # Verify that target_column is a string and exists in data
  assert_that(is.character(target_column), target_column %in% names(data))

  # Verify that k_values is a numeric vector and contains positive values
  assert_that(is.numeric(k_values), all(k_values > 0))

  # Verify that Prop_train is a number between 0 and 1
  assert_that(is.numeric(Prop_train), Prop_train > 0, Prop_train < 1)

  # Split the data into training and test sets
  #set.seed(123)
  index <- createDataPartition(data[[target_column]], p = Prop_train, list = FALSE)
  train_data <- data[index, ]
  test_data <- data[-index, ]

  # Prepare the data for KNN
  train_labels <- train_data[[target_column]]
  train_data <- train_data[, -which(names(train_data) == target_column)]
  test_labels <- test_data[[target_column]]
  test_data <- test_data[, -which(names(test_data) == target_column)]

  # Normalize the data
  train_data <- scale(train_data)
  test_data <- scale(test_data)

  # Initialize vectors to store accuracies
  accuracy_values <- numeric(length(k_values))

  for (k in k_values) {
    pred <- knn(train = train_data, test = test_data, cl = train_labels, k = k)
    cm <- confusionMatrix(table(pred, test_labels))

    # Corrected indexing for accuracy_values
    accuracy_values[which(k_values == k)] <- cm$overall['Accuracy']
  }
  best_k <- k_values[which.max(accuracy_values)]
  # Return a data frame with the K values and accuracies
  result_df <- data.frame(K = k_values, Accuracy = accuracy_values)

  return(list(result_df,
              Best_K = best_k,Efficacite=max(accuracy_values)))
}
