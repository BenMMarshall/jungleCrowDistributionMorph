#' download_crow_data
#'
#' @returns Three csv downloaded from Zenodo
#'
#' @export
download_crow_data <- function(){
  zenodo <- ZenodoManager$new(logger = "INFO")
  # # https://zenodo.org/doi/10.5281/zenodo.12788353
  # zen4R::download_zenodo("10.5281/zenodo.12788354",
  #                        path = here::here("Data"))

  rec <- zenodo$getRecordByDOI("10.5281/zenodo.12788354")
  files <- rec$listFiles(pretty = TRUE)

  dataList <- vector("list", length = nrow(files))
  names(dataList) <- files$filename
  for(n in 1:nrow(files)){
    download.file(URLencode(files$download[n]),
                  destfile = here::here("Data", files$filename[n]))
    dataList[[n]] <- read.csv(here::here("Data", files$filename[n]),
                              na.strings = "<NA>")
  }
  names(dataList) <- gsub(" ", "_", stringr::str_remove(names(dataList), ".csv$"))
  return(dataList)
}
