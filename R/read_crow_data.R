#' read_crow_data
#'
#' @returns A dataframe that is crowData.
#'
#' @export
read_crow_data <- function(crow_files){

  # dataLocation <- here("Data")
  # files <- list.files(dataLocation, pattern = "Museum Crow Measurements.csv")
  # dateText  <- str_extract(files, pattern = "^.{10}")
  # datePOSIXct <- as.POSIXct(dateText, format = "%Y-%m-%d")
  # date_Filename <- data.frame(datePOSIXct, files)
  # #pull out the most up to date file based on date
  # updated_Filename <- date_Filename %>%
  #   slice_max(order_by = datePOSIXct, n = 1) %>%
  #   pull(files)
  # data <- read.csv(here("Data", updated_Filename),
  #                  na.strings = "<NA>")

  crowData <- crow_files$Museum_Crow_Measurements %>%
    filter(!Age == "Juvenile" | is.na(Age))

  crowData <- crowData %>%
    mutate(
      ExCu.TaLe = Exposed.culmen / Tarsus.length,
      HaNa.TaLe = Height.at.nares / Tarsus.length,
      NtBT.ExCu = Nares.to.bill.tip / Exposed.culmen,
      ExCuNtBTxHaNaTaLe = ((Exposed.culmen - Nares.to.bill.tip) * Height.at.nares) / Tarsus.length
    )

  return(crowData)

}
