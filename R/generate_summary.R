#' generate_summary
#'
#' @param crowData The crowData with all measurements.
#' @returns A list of objects for in text summaries.
#'
#' @export
generate_summary <- function(crowData){

  # crowData <- crow_measurements
  # crow per museum
  crowPerMuseum <- crowData %>%
    mutate(museum = str_extract(Museum.catalogue.number, "^[:alpha:]{4,5}")) %>%
    group_by(museum) %>%
    summarise(nCrow = n(),
              nLoc = sum(!is.na(Corrected.Latitude)))

  # number of measures
  numberOfMeasures <- crowData %>%
    select("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
           "Exposed.culmen","Tarsus.length") %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise(nMeasures = n() - sum(is.na(value)),
              nCrow = n())

  countSex <- crowData %>%
    count(Sex)

  countAge <- crowData %>%
    count(Age)

  overallMeasureSummary <- crowData %>%
    select("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
           "Exposed.culmen","Tarsus.length","Sex") %>%
    pivot_longer(where(is.numeric), names_to = "measure") %>%
    mutate(measure = gsub("\\.", " ", measure)) %>%
    group_by(measure) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>%
    mutate(label = paste0(round(mean, digits = 1), " ±",
                          round(sd, digits = 2)))

  sexMeasureSummary <- crowData %>%
    select("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
           "Exposed.culmen","Tarsus.length","Sex") %>%
    pivot_longer(where(is.numeric), names_to = "measure") %>%
    mutate(measure = gsub("\\.", " ", measure)) %>%
    group_by(measure, Sex) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>%
    mutate(label = paste0(round(mean, digits = 1), " ±",
                          round(sd, digits = 2)))

  effortSummaryList <- list(
    "crowPerMuseum" = crowPerMuseum,
    "numberOfMeasures" = numberOfMeasures,
    "countSex" = countSex,
    "countAge" = countAge,
    "overallMeasureSummary" = overallMeasureSummary,
    "sexMeasureSummary" = sexMeasureSummary
  )

  return(effortSummaryList)

}
