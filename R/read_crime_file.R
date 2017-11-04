read_crime_file <- function(crime_file) {
  require(data.table)
  require(ggplot2)
  require(readxl)

  crime <- setDT(read_excel(crime_file))

  setnames(crime, c("date", "suburb", "postcode", "offence_level_1",
                    "offence_level_2", "offence_level_3", "offence_count"))
}
