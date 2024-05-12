#' KMEANS_FUNCTION
#'
#' This function implements the K-Means algorithm for data clustering.
#' It provides options for data preprocessing, such as normalization
#' and imputation of missing values.
#'
#' @param data A dataframe containing the numerical data to be clustered.
#' @param k The number of clusters to form.
#' @param max_iter The maximum number of iterations for the K-Means algorithm.
#' @param nstart The number of times to randomly initialize the centroids.
#' @param distance_metric The distance metric to use ('euclidean' or 'manhattan').
#' @param scale_data A boolean indicating whether the data should be normalized.
#' @param impute_data The imputation method for missing values ('mean', 'median', 'mode').
#' @return A list containing the following elements:
#'         - clusters: A vector indicating the cluster of each point.
#'         - centers: The coordinates of the centroids of each cluster.
#'         - additional_info: Additional information such as total distance and number of iterations.
#' @export
#' @importFrom stats median
#' @importFrom factoextra fviz_cluster
#' @importFrom ggplot2 theme_bw
#' @examples
#' data(iris)
#' data_iris <- iris[, -5] # Exclude the species column
#' results <- KMEANS_FUNCTION(data_iris, k = 3)
#' print(results$clusters)
KMEANS_FUNCTION <- function(data, k, max_iter = 100, nstart = 25, distance_metric = "euclidean", scale_data = FALSE, impute_data = "mean") {

  # Validation des parametres
  if (!is.numeric(k) || k <= 0 || k != round(k)) {
    stop("k(Nombres de clusters) doit etre un entier positif.")
  }
  if (!is.numeric(max_iter) || max_iter < 0 || max_iter != round(max_iter)) {
    stop("max_iter doit etre un entier non negatif.")
  }
  if (!is.numeric(nstart) || nstart <= 0 || nstart != round(nstart)) {
    stop("nstart doit etre un entier positif.")
  }

  # Fonction pour imputer les valeurs manquantes
  impute <- function(data, method) {
    if (any(is.na(data))) {
      for (i in 1:ncol(data)) {
        if (method == "mean") {
          data[is.na(data[, i]), i] <- mean(data[, i], na.rm = TRUE)
        } else if (method == "median") {
          data[is.na(data[, i]), i] <- median(data[, i], na.rm = TRUE)
        } else if (method == "mode") {
          mode_val <- as.numeric(names(sort(table(data[, i]), decreasing = TRUE)[1]))
          data[is.na(data[, i]), i] <- mode_val
        } else {
          stop("Methode d'imputation non prise en charge. Choisissez 'mean', 'median' ou 'mode'.")
        }
      }
    }
    return(data)
  }

  # Imputation des valeurs manquantes
  data <- impute(data, impute_data)

  # Normalisation des donnees si scale_data est TRUE
  if (scale_data) {
    data <- scale(data)
  }

  best_total_distance <- Inf
  best_clusters <- NULL
  best_centers <- NULL

  # Fonction pour calculer la distance en fonction de la metrique choisie
  distance <- function(point, center) {
    if (distance_metric == "euclidean") {
      return(sqrt(sum((point - center)^2)))
    } else if (distance_metric == "manhattan") {
      return(sum(abs(point - center)))
    } else {
      stop("Metrique de distance non prise en charge. Choisissez 'euclidean' ou 'manhattan'.")
    }
  }

  for (n in 1:nstart) {
    # Initialisation des centroides
    centers <- data[sample(nrow(data), k), ]
    # Boucle principale
    for (i in 1:max_iter) {
      # Assignation des points aux clusters
      clusters <- apply(data, 1, function(point) {
        min_index <- which.min(apply(centers, 1, function(center) distance(point, center)))
        return(min_index)
      })
      # Mise a jour des centroides
      new_centers <- array(dim = dim(centers))
      for (j in 1:k) {
        cluster_points <- data[clusters == j, ]
        if (nrow(cluster_points) > 0) {
          new_centers[j, ] <- colMeans(cluster_points, na.rm = TRUE)
        } else {
          new_centers[j, ] <- centers[j, ] # Aucun point dans le cluster, garder le centroide actuel
        }
      }
      # Verifier la convergence
      if (isTRUE(all.equal(centers, new_centers))) {
        break
      }
      centers <- new_centers
    }
    # Calcul de la distance totale
    total_distance <- sum(apply(data, 1, function(point) {
      min(apply(centers, 1, function(center) distance(point, center)))
    }))
    # Mise a jour du meilleur résultat
    if (total_distance < best_total_distance) {
      best_total_distance <- total_distance
      best_clusters <- clusters
      best_centers <- centers
    }
  }

  # Informations supplementaires a retourner
  additional_info <- list(
    total_distance = best_total_distance,
    iterations = i
  )

  return(list(
    clusters = best_clusters,
    centers = best_centers,
    additional_info = additional_info,
    Graphique = fviz_cluster(list(data = data, cluster = best_clusters), geom = "point", ellipse.type = "convex", ggtheme = ggplot2::theme_bw()
)
  ))
}
