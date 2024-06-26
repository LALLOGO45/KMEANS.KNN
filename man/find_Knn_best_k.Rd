% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KNN_Optimal_K.R
\name{find_Knn_best_k}
\alias{find_Knn_best_k}
\title{find_Knn_best_k}
\usage{
find_Knn_best_k(data, target_column, k_values, Prop_train = 0.8)
}
\arguments{
\item{data}{A dataframe containing the dataset to be used.}

\item{target_column}{A string specifying the name of the target column in the dataset.}

\item{k_values}{A numeric vector containing the different k-values to be tested.}

\item{Prop_train}{A numeric value between 0 and 1 indicating the proportion of the dataset to be used for training.}
}
\value{
A list containing a dataframe with k-values and their corresponding accuracies, and the best k-value with its accuracy.
}
\description{
This function finds the best k-value for KNN based on the provided data.
}
\examples{
data(iris)
find_Knn_best_k(iris,"Species",1:10,Prop_train=0.8)
}
