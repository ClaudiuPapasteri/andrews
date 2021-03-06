% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{andrews}
\alias{andrews}
\title{Compute Andrews curves}
\usage{
andrews(df, class_column, samples = 200, reorder = FALSE)
}
\arguments{
\item{df}{Data frame to be used, should encompass column containing class names as well at least 2 numeric columns, preferably normalized to (0.0, 1.0).}

\item{class_column}{String name of the column containing class names used for clustering.}

\item{samples}{Number of points to plot in each curve.}

\item{reorder}{Logical indicating whether to reorder numeric columns based on contributions to first principal component.}
}
\description{
Compute data for Andrews plot, for visualizing clusters of multivariate data with ggplot2.
}
\details{
Andrews curves have the functional form:
f(t) = x_1/sqrt(2) + x_2 sin(t) + x_3 cos(t) + x_4 sin(2t) + x_5 cos(2t) + ...
Where x coefficients correspond to the values of each dimension and t is linearly spaced between -pi and +pi. Each row of frame then corresponds to a single curve.
}
\examples{
library(ggplot2)
df <- andrews(iris, "Species")
ggplot(df, aes(x = t, y = values, color = class_column, group = sample)) +  
  geom_line() +
  scale_x_continuous(n.breaks = 7)

}
