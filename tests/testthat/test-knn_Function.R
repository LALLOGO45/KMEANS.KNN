# Charger les packages nécessaires
library(testthat)
library(KMEANS.KNN)

# Définir les tests
test_that("knn_Function works", {
  # Créer des données de test
  data(iris)
  dataset <- iris
  new_points <- data.frame(Sepal.Length = c(5.1, 7.7, 1.3, 0.2, 5.1),Sepal.Width = c(3.5, 2.6, 5, 3.7, 3.5),Petal.Length = c(1.4 , 6.9, 4.5, 6, 3.4),Petal.Width = c(0.2 , 2.3, 5.6, 8.4, 5.2))

  # Exécuter la fonction avec des paramètres spécifiques
  resultats <- knn_Function(new_points, dataset = dataset, k = 3, target_variable = "Species")

  # Vérifier que le résultat est une liste
  expect_type(resultats, "list")

  # Vérifier que la liste contient les éléments attendus
  expect_true(all(c("Predictions", "Data", "Distances", "Imputed_Values") %in% names(resultats)))

  # Vérifier que 'Predictions' est un vecteur de la bonne longueur
  expect_equal(length(resultats$Predictions), nrow(new_points))

  # Vérifier que 'Data' est un dataframe avec le bon nombre de lignes et de colonnes
  expect_equal(dim(resultats$Data), c(nrow(new_points), ncol(new_points) + 1)) # +1 pour la colonne de prédictions

  # Vérifier que 'Distances' est une liste de la bonne longueur
  expect_equal(length(resultats$Distances), nrow(new_points))

  # Vérifier que 'Imputed_Values' est une liste ou une chaîne de caractères
  expect_true(is.list(resultats$Imputed_Values) || is.character(resultats$Imputed_Values))
})
