#' Plot peaks
#'
#' This function plots the peak values above the entered threshold argument.
#'
#' @param x Path to the input file
#' @param threshold the value above which to calculate peaks
#' @return A plot of the peaks at the specified threshold
#' @export

show.peaks <- function(x, threshold = 0, ...) {
  # This function requires an index value (i.e. dates/times) in column 1 and values (numeric) in column 2

  # Identify peaks
  pks <- which(diff(sign(diff(x[, 2], na.pad = FALSE)), na.pad = FALSE) < 0) + 1 # extract all peaks
  # Specify a threshold for the peak identification
  #threshold <- threshold
  # Subset the peaks based on the threshold
  x1pks <- subset(x[pks, ], x[pks, 2] > threshold)

  # Plot the results
  plot(x[,1:2], type="l", ...)
  points(x1pks, col="blue") # Show peaks above threshold
  abline(h = threshold, col = "red") # Show the threshold limit
  text(x = quantile((x1pks[, 1]), 0.25), y = quantile(x1pks[, 2], 0.99), adj = c(0.5, 1), paste0("Threshold = ", threshold), srt=0, col = "red")
  text(x = quantile((x1pks[, 1]), 0.25), y = quantile(x1pks[, 2], 0.99), adj = c(0.5, 3), paste0("# of Peaks = ", nrow(x1pks)), srt=0, col = "blue")
  #points(x1pks,1:2], col="red") # Show all peaks
  return(x1pks)
}

# devtools::create("myfirstpackage")
# Use devtools::document() to prepare documentaiton for the package
