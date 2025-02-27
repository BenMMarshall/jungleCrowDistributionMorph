#' compare_bayes
#'
#' @param crowData
#' @param measure
#' @param group
#' @param priorCentre
#' @returns A brms model object.
#'
#' @export
compare_bayes <- function(compData, measure, group, priorCentre = "normal(0, 20)"){
  # compData <- crow_measurements_subspecies
  # measure <- "HaNa.TaLe"
  # group <- "klockSpecies"
  print(group)
  print(measure)
  compData <- compData %>%
    dplyr::rename("measure" = contains(measure),
                  "group" = contains(group))

  # robustForm <- brms::bf(paste0(measure, "~ 0 + ", group),
  #                        paste0("sigma ~ 0 + ", group))
  robustForm <- brms::bf(measure ~ 0 + group:Sex,
                         sigma ~ 0 + group:Sex)

  priorCentreForMeasure <- priorCentre$priors[priorCentre$measures == measure &
                                                priorCentre$grouping == group]

  brmPriors <- set_prior(
    prior = priorCentreForMeasure,
    class = "b",
    lb = 0
  )
  if(priorCentreForMeasure == "uni"){
    brmPriors <- NULL
  }

  modelSave <- here::here("ModelOutput", paste0(group, "_", measure, ".txt"))
  modelFile <- here::here("ModelOutput", paste0(group, "_", measure))

  modOut <- brms::brm(
    formula = robustForm,
    family = brms::student,
    prior = brmPriors,
    sample_prior = TRUE,
    data = compData,
    iter = 1000,
    warmup = 200,
    chains = 4,
    thin = 2,
    cores = 1,
    seed = 1,
    save_pars = brms::save_pars(all = TRUE),
    save_model = modelSave,
    file = modelFile)

  return(modOut)

}
