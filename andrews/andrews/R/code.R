#' Compute Andrews curves

#'
#' @description 
#' Compute data for Andrews plot, for visualizing clusters of multivariate data with ggplot2.
#' 
#' @details 
#' Andrews curves have the functional form:
#' f(t) = x_1/sqrt(2) + x_2 sin(t) + x_3 cos(t) + x_4 sin(2t) + x_5 cos(2t) + ...
#' Where x coefficients correspond to the values of each dimension and t is linearly spaced between -pi and +pi. Each row of frame then corresponds to a single curve.
#'
#' @param df Data frame to be used, should encompass column containing class names as well at least 2 numeric columns, preferably normalized to (0.0, 1.0).
#' @param class_column String name of the column containing class names used for clustering.
#' @param samples Number of points to plot in each curve.
#' @param reorder Logical indicating whether to reorder numeric columns based on contributions to first principal component.
#'
#' @examples
#' library(ggplot2)
#' df <- andrews(iris, "Species")
#' ggplot(df, aes(x = t, y = values, color = class_column, group = sample)) +  
#'   geom_line() +
#'   scale_x_continuous(n.breaks = 7)
#'
#'@export

andrews <- function(df, class_column, samples = 200, reorder = FALSE) {
  
  # Validation
  num_cols <- unlist(lapply(df[, -which(names(df) %in% class_column), drop = FALSE], is.numeric))
  if(sum(num_cols) < 2) stop("Data needs to have at least 2 numeric columns (not including class_column).")
  if(any(!num_cols)) {
    excl_cols <- names(num_cols[!num_cols])
    df <- df[, -which(names(df) %in% excl_cols), drop = FALSE]
    warning("Some columns are not numeric. Excluded colums: ", paste(excl_cols, collapse = ", "), ".")  
  }
  
  if(reorder) {
    # Reorder columns based on importance to PC1
    pca <- prcomp(df[, -which(names(df) %in% class_column), drop = FALSE], center = TRUE, scale = TRUE)
    col_order <- names(sort(abs(pca$rotation[, 1]), decreasing = TRUE))
    df <- df[, c(col_order, class_column)]
  }
  
  t <- seq(-pi, pi, length.out = samples) 
  
  vals <- t(
    data.matrix(                    
      df[, -which(names(df) %in% class_column), drop = FALSE]
    )
  )
  
  curves <- outer(vals[1, ] / (2^0.5), rep(1, length(t))) 
  
  for (i in 2:nrow(vals)) {                       
    ft = (i %/% 2) * t                          
    if (i %% 2 == 0) {                                
      curves <- curves + outer(vals[i, ], sin(ft))
    } else {
      curves <- curves + outer(vals[i, ], cos(ft))
    }  
  }
  
  row <- as.vector(row(curves))
  col <- as.vector(col(curves))
  
  andrews_df <- data.frame(
    t = t[col],                      # or simply: t = col
    sample = row,
    values = as.vector(curves),
    class_column = df[[class_column]][row]
  )
  
  andrews_df 
} 
