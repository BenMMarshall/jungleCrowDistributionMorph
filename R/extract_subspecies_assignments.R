#' extract_subspecies_assignments
#'
#' @param crowData The crowData with all measurements.
#' @returns The crowData set but now with subspecies columns, NA in Lat Long removed.
#'
#' @export
extract_subspecies_assignments <- function(crowData, subspeciesList){

  klock <- subspeciesList$klock
  mart <- subspeciesList$mart

  cleanLLcrowData <- crowData %>%
    filter(!is.na(Corrected.Longitude),
           !is.na(Corrected.Latitude))

  sfPoints <- st_as_sf(cleanLLcrowData,
                       coords = c("Corrected.Longitude","Corrected.Latitude"), remove = FALSE)
  st_crs(sfPoints) <- st_crs(klock)

  overlapMart <- st_join(sfPoints, mart, join = st_within) %>%
    rename("martSpecies" = Subspecies) %>%
    select(Crow.ID, martSpecies) %>%
    st_drop_geometry()

  overlapKlock <- st_join(sfPoints, klock, join = st_within) %>%
    rename("klockSpecies" = Subspecies) %>%
    select(Crow.ID, klockSpecies) %>%
    st_drop_geometry()

  crowDataSubspecies <- cleanLLcrowData %>%
    left_join(overlapMart, by = "Crow.ID") %>%
    left_join(overlapKlock, by = "Crow.ID")

  # manually corrections for the missing subspecies assignments
  subspeciesCorrections <- read.csv(here("Data", "Subspecies", "subspecies_corrections.csv"))

  for(id in unique(subspeciesCorrections$Crow.ID)){
    # id <- unique(subspeciesCorrections$Crow.ID)[1]
    if(is.na(crowDataSubspecies[which(crowDataSubspecies$Crow.ID == id),]$martSpecies)){
      crowDataSubspecies[which(crowDataSubspecies$Crow.ID == id),]$martSpecies <-
        subspeciesCorrections[which(subspeciesCorrections$Crow.ID == id),]$martSpecies
    }
    if(is.na(crowDataSubspecies[which(crowDataSubspecies$Crow.ID == id),]$klockSpecies)){
      crowDataSubspecies[which(crowDataSubspecies$Crow.ID == id),]$klockSpecies <-
        subspeciesCorrections[which(subspeciesCorrections$Crow.ID == id),]$klockSpecies
    }
  }

  crowDataSubspecies[which(crowDataSubspecies$martSpecies == "hybrid"),]$martSpecies <- NA

  return(crowDataSubspecies)

}

