% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpp_functions.R
\name{hctree_sort}
\alias{hctree_sort}
\title{HCTree Sort}
\usage{
hctree_sort(distance_matrix, externalOrder = NULL, orderType, flipType)
}
\arguments{
\item{distance_matrix}{A square numeric proximity matrix (either n × n or p × p) representing pairwise distances between items.}

\item{externalOrder}{An integer vector specifying an initial or external ordering of the items (can be empty or NULL if not used).}

\item{orderType}{An integer indicating the type of hierarchical clustering order to apply.}

\item{flipType}{An integer indicating the flipping methods.}
}
\value{
A list representing a dendrogram tree structure, containing:
\code{left}, \code{right}, and \code{height} for tree construction,
and \code{order} for the optimal leaf order.
}
\description{
This function applies a hierarchical clustering tree (HCT) sorting algorithm
to reorder rows of a proximity matrix. It supports external ordering constraints,
different linkage-based order types, and optional flipping for optimal layout.
}
\details{
\strong{distance_matrix}

The input matrix must represent pairwise distances between items.
If you start with a similarity matrix (e.g., a correlation matrix), you must convert it to a dissimilarity matrix before use.
For example, for correlation-based similarities,
use \code{as.matrix(as.dist(1 - cor_matrix))} or other appropriate transformations to convert it to a proper distance matrix.
The matrix should also be symmetric and non-negative.

\strong{orderType}

Specifies the linkage method used for hierarchical clustering:
\itemize{
\item \code{0}: Single-linkage
\item \code{1}: Complete-linkage
\item \code{2}: Average-linkage (UPGMA)
}

\strong{flipType}

Controls how the branches of the clustering tree are flipped:
\itemize{
\item \code{1}: Flip based on \code{externalOrder} This option should be used only when \code{externalOrder} is provided.
\item \code{2}: Uncle-flipping
\item \code{3}: Grandpa-flipping
}

\strong{Important:}
Do not specify both \code{flipType = 1} and a \code{NULL} or missing \code{externalOrder}.
When using \code{flipType = 1}, \code{externalOrder} must be a valid integer vector.
}
