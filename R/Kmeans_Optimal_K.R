#' find_Kmeans_best_k
#'
#' @param data The dataset for which K-means clustering will be performed.
#' @param max_k The maximum number of clusters to consider. It defaults to 10.
#' @param Method The method used to determine the optimal number of clusters.
#'               Acceptable values are "coude" (elbow method), "silhouette" (silhouette method),
#'               or "gap" (gap statistics).
#'
#' @return This function does not return a value but prints the optimal number of clusters
#'         based on the chosen method and plots the corresponding graph.
#' @export
#' @importFrom stats kmeans
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom stats dist
#' @importFrom grDevices recordPlot
#' @importFrom cluster silhouette
#' @examples
#' data(iris)
#' find_Kmeans_best_k(iris[,-5],9,Method = "coude")

find_Kmeans_best_k <- function(data, max_k = 10, Method = "coude") {

  # Calculate the sum of squared intra-cluster distances (WCSS) for different values of k
  wcss <- numeric(max_k)
  for (k in 1:max_k) {
    kmeans_model <- kmeans(data, centers = k, nstart = 10)
    wcss[k] <- kmeans_model$tot.withinss
  }
  results <- list()
  if (Method == "coude") {
    # Elbow Method
    plot(1:max_k, wcss, type = "b", xlab = "Number of clusters (k)", ylab = "WCSS",
         main = "Elbow Method")
    abline(v = which.min(diff(wcss)), col = "red", lty = 2)
    axis(1, at = 2:max_k, labels = 2:max_k)
    cat("Retain the K value found before the break")
    # Save the plot to the results list
    results$plot <- recordPlot()
    # Save the message to the results list
    results$message <- "Retain the K value found before the break"
  } else if (Method == "silhouette") {
    # Silhouette Score
    silhouette_scores <- numeric(max_k - 1)
    for (k in 2:max_k) {
      kmeans_model <- kmeans(data, centers = k, nstart = 10)
      # Use the silhouette function to get the scores for each point
      sil_scores <- silhouette(kmeans_model$cluster, dist(data))
      # Calculate the average silhouette score for the cluster
      avg_sil_width <- mean(sil_scores[, "sil_width"])
      silhouette_scores[k-1] <- avg_sil_width
    }
    plot(2:max_k, silhouette_scores, type = "b", xlab = "Number of clusters (k)",
         ylab = "Silhouette Score", main = "Silhouette Score")
    axis(1, at = 2:max_k, labels = 2:max_k)
    optimal_clusters <- which.max(silhouette_scores) + 1
    cat("Optimal number of clusters (silhouette score):", optimal_clusters, "\n")
    results$plot <- recordPlot()
    # Save the message to the results list
    results$message <- paste("Optimal number of clusters (silhouette score):", optimal_clusters)
  } else if (Method == "gap") {
    # Gap Statistics
    gap_statistic <- numeric(max_k)
    for (k in 1:max_k) {
      kmeans_model <- kmeans(data, centers = k, nstart = 10)
      gap_statistic[k] <- sum(log(wcss[k])) - log(kmeans_model$tot.withinss)
    }
    plot(1:max_k, gap_statistic, type = "b", xlab = "Number of clusters (k)",
         ylab = "Gap Statistics", main = "Gap Statistics")
    axis(1, at = 2:max_k, labels = 2:max_k)
    cat("Optimal number of clusters (gap statistics):", which.max(gap_statistic), "\n")
    results$plot <- recordPlot()
    # Save the message to the results list
    results$message <- paste("Optimal number of clusters (gap statistics):", which.max(gap_statistic))
  } else {
    cat("Unrecognized method. Please choose among 'coude', 'silhouette', or 'gap'.\n")
  }
  return(results)
}
