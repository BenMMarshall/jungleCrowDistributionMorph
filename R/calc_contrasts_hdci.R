#' calc_contrasts_hdci
#'
#' @param contrastsFull
#' @param measure
#' @param group
#' @returns HDCI estimates from the contrasts
#'
#' @export
calc_contrasts_hdci <- function(contrastsFull, measure, group){

  hdciContrasts_list <- vector("list", length = length(contrastsFull))
  names(hdciContrasts_list) <- sub("contrastsFull", "hdci", names(contrastsFull))
  for(con in names(contrastsFull)){
    medHDCI <- contrastsFull[[con]] %>%
      group_by(comparison) %>%
      median_hdci(contrast)

    compared2Zero <- contrastsFull %>%
      group_by(comparison) %>%
      summarise(probOver0 = mean(contrast > 0),
                probUnder0 = mean(contrast < 0),
                gbase = gbase[1],
                gcomp = gcomp[1])

    hdiContrasts <- medHDCI %>%
      left_join(compared2Zero)

    hdiContrasts$measure <- measure
    hdiContrasts$group <- group

    hdiContrasts <- hdiContrasts %>%
      mutate(label = paste0(round(probUnder0*100, digits = 2), "% < 0 > ",
                            round(probOver0*100, digits = 2), "%"),
             labelLow = paste0(round(probUnder0*100, digits = 2), "% <"),
             labelHigh = paste0("> ", round(probOver0*100, digits = 2), "%"))

    hdciContrasts_list[[sub("contrastsFull", "hdci", con)]] <- hdiContrasts
  }
  return(hdciContrasts_list)
}
