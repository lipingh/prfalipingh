library(data.table)
library(readxl)
datatable <- setDT(read_excel("data/crime-statistics-2016-17.xlsx"))
setnames(datatable, c("date", "suburb", "postcode", "offence_level_1",
                  "offence_level_2", "offence_level_3", "offence_count"))

## *Liping Huang*
## 4 Nov 2017
#' HW2_function
#' \code{suburbs_crime} The fucntion take the user inputs, including two surburs,
#' offence_descritpyions, and plot the correlation in offence count between the two suburbs
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of offence level 3.
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' suburbs_crime(datatabel,"OFFENCES AGAINST PROPERTY", c("WEST BEACH", "ADELAIDE AIRPORT"))
suburbs_crime <- function(crime_data, offence_description, suburbs) {
  require(data.table)
  require(ggplot2)

  # Error catching

  if (length(suburbs) != 2) {
    stop("Please enter two suburbs")
  }


  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  if (!all.equal(colnames(crime_data), expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
  if (any(!suburbs %in% crime_data$suburb) |
      !offence_description %in% crime_data$offence_level_3) {
    stop("The suburbs doesn't in the crime data or the offence description is not offence level 3")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  plot_data <- crime_data[suburb %in% suburbs, list(suburb, total_offence_count = sum(offence_count)),
                      by = date]
  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]
  #print(plot_data) #for test purpose
  plot_data <- dcast(plot_data, date ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")
  #print(plot_data) #for test purpose
  # Generate the plot
  ggplot(plot_data, aes(x, y) )+
    geom_count() +
    labs(x = suburbs[1],
         y = suburbs[2])
}

suburbs_crime(datatable,"OFFENCES AGAINST PROPERTY", c("WEST BEACH", "ADELAIDE AIRPORT"))
