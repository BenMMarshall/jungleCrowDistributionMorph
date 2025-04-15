#' Render the manuscript rmd
#'
#' @name render_rmd
#' @description A
#' @return Nothing, PDF (or output) will be saved to a folder.
#'
#' @export
render_rmd <- function(...){

  rmarkdown::render(input = here::here("Manuscript",
                                       "crowMorphology.Rmd"),
                    output_file = here::here("Manuscript",
                                             "crowMorphology.pdf"))
}
