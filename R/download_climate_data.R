#' download_climate_data
#'
#' @returns Three climate data
#'
#' @export
download_climate_data <- function(){
  
  climateFiles <- list.files(here("Data", "climate", "wc2.1_30s"), full.names = TRUE,
                             pattern = "wc2.1_30s_bio_")
  
  if(length(climateFiles) < 19){
    worldclim_global(var = "bio", res = 0.5, path = here("Data"),
                     version = "2.1")
  }
  
  return(climateFiles)
}
