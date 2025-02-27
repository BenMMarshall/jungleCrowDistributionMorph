#' read_subspecies_polygons
#'
#' @returns Read in polygons of subspecies from Klockenhoff and Martens.
#'
#' @export
read_subspecies_polygons <- function(){
  
  klock <- read_sf(here("Data", "Subspecies",
                        "Klockenhoff_1969_subspecies_clipped_to_map.GeoJSON"))
  mart <- read_sf(here("Data", "Subspecies",
                       "Martens_2000_subspecies_clipped_to_map.GeoJSON"))
  
  return(list(klock = klock,
              mart = mart))
  
}