#' Show timeseries peak values above a threshold
#'
#' This function plots the peak values above the entered threshold argument.
#'
#' @param data a dataframe containing the measurements and optional index variables
#' @param index column reference entered as a character variable to the indices. Indices are plotted on the x axis. Observations can be either a date or numeric format.
#' @param values column reference entered as a character variable to the values against which the peak values are calculated. Observations ust be numeric format
#' @param threshold a numeric value above which to calculate peaks.
#' @param ... other arguments to pass to the plot function.
#' @return A plot of the peaks at the specified threshold and Peak value data that can be assigned to an object
#' @examples
#' data(waves)
#' show.peaks(data = waves,
#'            index = "date_time",
#'            values = "height_meters",
#'            threshold = 1.3)
#'
#' # To show data without a date/time index
#' show.peaks(data = waves,
#'            values = "height_meters",
#'            threshold = 1.3)
#' @export

show.peaks <- function(data, index = NULL, values, threshold = 0, ...) {

   if (!is.null(index)) {

     x <- {{data}} %>%
       select(index, {{values}})

   } else {

     x <- {{data}} %>%
       mutate(index = as.numeric(row.names({{data}}))) %>%
       select(index, {{values}})

   }

  # Identify peaks
  pks <- which(diff(sign(diff(x[, 2], na.pad = FALSE)), na.pad = FALSE) < 0) + 1 # extract all peaks

  # Subset the peaks based on the threshold
  x1pks <- subset(x[pks, ], x[pks, 2] > threshold)

  row.names(x1pks) <- NULL

  plot(x[, ], type="l", ...)
  points(x1pks, col="blue") # Show peaks above threshold
  abline(h = threshold, col = "red") # Show the threshold limit

  text(x = quantile((x1pks[, 1]), 0.25),
       y = quantile(x1pks[, 2], 0.99),
       adj = c(0.5, 1), paste0("Threshold = ", threshold), srt=0, col = "red")

  text(x = quantile((x1pks[, 1]), 0.25),
       y = quantile(x1pks[, 2], 0.99),
       adj = c(0.5, 3), paste0("# of Peaks = ", nrow(x1pks)), srt=0, col = "blue")

  return(x1pks)
}

