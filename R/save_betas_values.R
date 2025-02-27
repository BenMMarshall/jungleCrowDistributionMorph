#' save_betas_values
#'
#' @param model
#' @param measure
#' @param group
#' @returns Pull and save the beta values for a brms model
#'
#' @export
save_betas_values <- function(model, measure, group){

  name <- paste(measure, group, sep = "_")
  # model <- modOut

  betasOutputs <- ggdist::median_hdci(tidybayes::gather_draws(model,
                                                              `b_.*`, regex = TRUE),
                                      .width = c(0.95))

  write.csv(betasOutputs, file = here::here("ModelOutput", paste0(name, "_brmsEstResults.csv")),
            row.names = FALSE)

  return(betasOutputs)
}
