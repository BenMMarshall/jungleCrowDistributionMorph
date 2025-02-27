#' calc_contrasts
#'
#' @param compData
#' @param model
#' @param measure
#' @param group
#' @returns List of contrasts and other aspects
#'
#' @export
calc_contrasts <- function(compData, model, measure, group){

  # targets::tar_load("bayesian_comparison_martSpecies_Width.at.nares")
  # targets::tar_load("bayesian_comparison_klockSpecies_HaNa.TaLe")

  # targets::tar_load("crow_measurements_subspecies")
  # targets::tar_load("bayesian_comparison_klockSpecies_Tarsus.length")
  # compData <- crow_measurements_subspecies
  # model <- bayesian_comparison_martSpecies_Width.at.nares
  # model <- bayesian_comparison_klockSpecies_HaNa.TaLe
  # model <- bayesian_comparison_klockSpecies_Tarsus.length
  # measure <- "Width.at.nares"
  # measure <- "HaNa.TaLe"
  # measure <- "Tarsus.length"
  # group <- "martSpecies"

  name <- paste(measure, group, sep = "_")

  compData <- compData %>%
    rename("measure" = matches(measure),
           "group" = matches(group))

  # newdata <- compData %>%
  #   select(-matches(measure)) %>%
  #   filter(!is.na(group))

  uniSubspecies <- unique(compData$group[!is.na(compData$group)])
  uniSex <- unique(compData$Sex[!is.na(compData$Sex)])

  newdata <- data.frame(
    group = rep(uniSubspecies, 100*length(uniSubspecies)),
    Sex = rep(uniSex, each = 100*length(uniSubspecies)/2)
  )

  longPredOut <- newdata %>%
    add_predicted_draws(object = model)

  longPredOut$subspeciesSex <- paste0(longPredOut$group, "_",
                                      longPredOut$Sex)

  # groupInteraction <- unique(longPredOut$subspeciesSex)
  groupSubspecies <- unique(longPredOut$group)
  groupSubspeciesSex <- unique(longPredOut$subspeciesSex)
  groupSex <- unique(longPredOut$Sex)

  # listLength_iterations <- length(groupInteraction)^2
  # i <- 0
  # contrastsTemp_interaction <- vector("list", length = listLength_iterations)
  # hdiConList_interaction <- vector("list", length = listLength_iterations)
  # for(gbase in groupInteraction){
  #   # gbase <- "G0"
  #   for(gcomp in groupInteraction){
  #     i <- i+1
  #     # gcomp <- "G1"
  #     if(gbase == gcomp){
  #       {next}
  #     } else {
  #       comparison <- paste0(gbase, "-", gcomp)
  #       contrast <- longPredOut$.prediction[longPredOut$subspeciesSex == gbase] -
  #         longPredOut$.prediction[longPredOut$subspeciesSex == gcomp]
  #       print(paste0(i, "/", listLength_iterations, ": ", gcomp, " - ", gbase))
  #     }
  #     hdiCon <- median_hdi(contrast)
  #     hdiCon$gbase <- gbase
  #     hdiCon$gcomp <- gcomp
  #     hdiCon$comparison <- comparison
  #     hdiCon$probOver0 <- mean(contrast > 0)
  #     hdiCon$probUnder0 <- mean(contrast < 0)
  #     hdiConList_interaction[[i]] <- hdiCon
  #     # contrastsTemp_interaction[[i]] <- data.frame(comparison, gbase, gcomp, contrast)
  #   }
  # }
  # hdiContrasts_interaction <- do.call(rbind, hdiConList_interaction)
  # hdiContrasts_interaction$measure <- measure
  # contrastsFull_interaction <- do.call(rbind, contrastsTemp_interaction)
  # contrastsFull_interaction$measure <- measure

  # intraSubspeciesSexContrasts <- contrastsFull_interaction %>%
  #   mutate(gbase_sp = str_extract(gbase, "^.*(?=\\_)"),
  #          gcomp_sp = str_extract(gcomp, "^.*(?=\\_)")) %>%
  #   filter(gbase_sp == gcomp_sp)

  intraSubspeciesDF <- newdata %>%
    select(group) %>%
    group_by(group) %>%
    slice_head(n = 1) %>%
    mutate(gbase = paste0(group, "_Female"),
           gcomp = paste0(group, "_Male"))

  listLength_intraSubspeciesContrasts <- nrow(intraSubspeciesDF)
  i <- 0
  # contrastsTemp_intrasubspeciesContrasts <- vector("list",
  #                                                  length = listLength_intrasubspeciesContrasts)
  hdiConList_intraSubspeciesContrasts <- vector("list",
                                                length = listLength_intraSubspeciesContrasts)
  for(comp in 1:nrow(intraSubspeciesDF)){
    gbase <- intraSubspeciesDF$gbase[comp]
    gcomp <- intraSubspeciesDF$gcomp[comp]
    # for(gbase in unique(intraSubspeciesSexContrasts$gbase_sp)){
    #   for(gcomp in unique(intraSubspeciesSexContrasts$gcomp_sp)){
    if(gbase == gcomp){
      {next}
    } else {
      i <- i+1
      comparison <- paste0(gbase, "-", gcomp)
      contrast <- longPredOut$.prediction[longPredOut$subspeciesSex == gbase] -
        longPredOut$.prediction[longPredOut$subspeciesSex == gcomp]
      print(paste0(i, "/", listLength_intraSubspeciesContrasts, ": ", gcomp, " - ", gbase))
    }
    hdiCon <- try(
      median_hdci(contrast[is.finite(contrast)])
    )
    if(class(hdiCon) == "try-error"){
      hdiCon <- data.frame(
        y = NA,
        ymin = NA,
        ymax = NA,
        .width = NA,
        .point = NA,
        .interval = NA)
    }
    hdiCon$gbase <- gbase
    hdiCon$gcomp <- gcomp
    hdiCon$comparison <- comparison
    hdiCon$probOver0 <- mean(contrast > 0)
    hdiCon$probUnder0 <- mean(contrast < 0)
    hdiConList_intraSubspeciesContrasts[[i]] <- hdiCon
    # contrastsTemp_intrasubspeciesContrasts[[i]] <- data.frame(comparison, gbase, gcomp, contrast)
    #   }
    # }
  }
  hdiContrasts_intraSubspeciesContrasts <- do.call(rbind, hdiConList_intraSubspeciesContrasts)
  hdiContrasts_intraSubspeciesContrasts$measure <- measure
  hdiContrasts_intraSubspeciesContrasts$group <- group
  # contrastsFull_intrasubspeciesContrasts <- do.call(rbind, contrastsTemp_intrasubspeciesContrasts)
  # contrastsFull_intrasubspeciesContrasts$measure <- measure

  listLength_subspecies <- length(groupSubspecies)^2
  i <- 0
  # contrastsTemp_subspecies <- vector("list", length = listLength_subspecies)
  hdiConList_subspecies <- vector("list", length = listLength_subspecies)
  for(gbase in groupSubspecies){
    for(gcomp in groupSubspecies){
      if(gbase == gcomp){
        {next}
      } else {
        i <- i+1
        comparison <- paste0(gbase, "-", gcomp)
        contrast <- longPredOut$.prediction[which(longPredOut$group == gbase)] -
          longPredOut$.prediction[which(longPredOut$group == gcomp)]
        print(paste0(i, "/", listLength_subspecies, ": ", gcomp, " - ", gbase))
      }
      hdiCon <- try(
        median_hdci(contrast[is.finite(contrast)])
      )
      if(class(hdiCon) == "try-error"){
        hdiCon <- data.frame(
          y = NA,
          ymin = NA,
          ymax = NA,
          .width = NA,
          .point = NA,
          .interval = NA)
      }
      hdiCon$gbase <- gbase
      hdiCon$gcomp <- gcomp
      hdiCon$comparison <- comparison
      hdiCon$probOver0 <- mean(contrast > 0)
      hdiCon$probUnder0 <- mean(contrast < 0)
      hdiConList_subspecies[[i]] <- hdiCon
      # contrastsTemp_subspecies[[i]] <- data.frame(comparison,
      #                                             gbase, gcomp,
      #                                             contrast)
    }
  }
  hdiContrasts_subspecies <- do.call(rbind, hdiConList_subspecies)
  hdiContrasts_subspecies$measure <- measure
  hdiContrasts_subspecies$group <- group
  # contrastsFull_subspecies <- do.call(rbind, contrastsTemp_subspecies)
  # contrastsFull_subspecies$measure <- measure

  listLength_sex <- length(groupSex)
  i <- 0
  # contrastsTemp_sex <- vector("list", length = listLength_sex)
  hdiConList_sex <- vector("list", length = listLength_sex)
  for(gbase in groupSex){
    for(gcomp in groupSex){
      if(gbase == gcomp){
        {next}
      } else {
        i <- i+1
        comparison <- paste0(gbase, "-", gcomp)
        contrast <- longPredOut$.prediction[which(longPredOut$Sex == gbase)] -
          longPredOut$.prediction[which(longPredOut$Sex == gcomp)]
        print(paste0(i, "/", listLength_sex, ": ", gcomp, " - ", gbase))
      }
      hdiCon <- try(
        median_hdci(contrast[is.finite(contrast)])
      )
      if(class(hdiCon) == "try-error"){
        hdiCon <- data.frame(
          y = NA,
          ymin = NA,
          ymax = NA,
          .width = NA,
          .point = NA,
          .interval = NA)
      }
      hdiCon$gbase <- gbase
      hdiCon$gcomp <- gcomp
      hdiCon$comparison <- comparison
      hdiCon$probOver0 <- mean(contrast > 0)
      hdiCon$probUnder0 <- mean(contrast < 0)
      hdiConList_sex[[i]] <- hdiCon
      # contrastsTemp_sex[[i]] <- data.frame(comparison,
      #                                      gbase, gcomp,
      #                                      contrast)
    }
  }
  hdiContrasts_sex <- do.call(rbind, hdiConList_sex)
  hdiContrasts_sex$measure <- measure
  hdiContrasts_sex$group <- group
  # contrastsFull_sex <- do.call(rbind, contrastsTemp_sex)
  # contrastsFull_sex$measure <- measure

  # contrastsFull_list <- list(
  #   "contrastsFull_interaction" = contrastsFull_interaction,
  #   "contrastsFull_intrasubspeciesContrasts" = contrastsFull_intrasubspeciesContrasts,
  #   "contrastsFull_subspecies" = contrastsFull_subspecies,
  #   "contrastsFull_sex" = contrastsFull_sex
  # )
  hdiContrastsFull_list <- list(
    # "hdiContrasts_interaction" = hdiContrasts_interaction,
    "hdiContrasts_intraSubspeciesContrasts" = hdiContrasts_intraSubspeciesContrasts,
    "hdiContrasts_subspecies" = hdiContrasts_subspecies,
    "hdiContrasts_sex" = hdiContrasts_sex
  )

  return(hdiContrastsFull_list)
}
