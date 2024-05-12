# Charger les packages nécessaires
library(testthat)
library(KMEANS.KNN)

# Définir les tests
test_that("KMEANS_FUNCTION works", {
  # Créer des données de test
  data <- iris[, -5] # Exclure la colonne des espèces

  # Exécuter la fonction avec des paramètres spécifiques
  resultats <- KMEANS_FUNCTION(data, k = 3, max_iter = 100, nstart = 25, distance_metric = "euclidean", scale_data = FALSE, impute_data = "mean")

  # Vérifier que le résultat est une liste
  expect_type(resultats, "list")

  # Vérifier que la liste contient les éléments attendus
  expect_true(all(c("clusters", "centers", "additional_info", "Graphique") %in% names(resultats)))

  # Vérifier que 'clusters' est un vecteur de la bonne longueur
  expect_equal(length(resultats$clusters), nrow(data))

  # Vérifier que 'centers' est une matrice avec le bon nombre de lignes
  expect_equal(nrow(resultats$centers), 3)

  # Vérifier que 'additional_info' est une liste contenant les éléments attendus
  expect_true(all(c("total_distance", "iterations") %in% names(resultats$additional_info)))

  # Vérifier que 'Graphique' est un ggplot
  # Ensure that 'Graphique' is actually created and returned by the KMEANS_FUNCTION
  expect_s3_class(resultats$Graphique, "ggplot")
})

