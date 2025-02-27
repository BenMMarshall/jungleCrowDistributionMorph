#' generate_idw_maps
#'
#' @param crowData The crowData with all measurements, must have no NAs in lat long.
#' @returns A plot showing IDW on the map.
#'
#' @export
generate_idw_maps <- function(crowData, palette){

  # targets::tar_load("crow_measurements_subspecies")
  #
  # crowData <- crow_measurements_subspecies
  # library(dplyr)
  # library(here)
  # library(ggplot2)
  # library(spatstat)
  # library(sf)
  # library(terra)
  # library(tidyterra)

  # paletteList <- generate_palette()
  # palette <- paletteList

  # add tiny noise to make sure points are unique
  crowData$Corrected.Longitude <- crowData$Corrected.Longitude +
    runif(length(crowData$Corrected.Latitude), -0.0000001, 0.0000001)
  crowData$Corrected.Latitude <- crowData$Corrected.Latitude +
    runif(length(crowData$Corrected.Latitude), -0.0000001, 0.0000001)

  obs_window <- owin(range(crowData$Corrected.Longitude)+ c(-20, 20),
                     range(crowData$Corrected.Latitude)+ c(-20, 20))

  measures <- c("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
                "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
                "Exposed.culmen","Tarsus.length",
                "ExCu.TaLe", "HaNa.TaLe", "NtBT.ExCu", "ExCuNtBTxHaNaTaLe")

  idwList <- vector("list", length = length(measures))
  idwDataList <- vector("list", length = length(measures))
  names(idwList) <- measures
  for(me in measures){
    # me <- measures[11]
    print(me)

    idwData <- crowData %>%
      select("Corrected.Longitude", "Corrected.Latitude", all_of(me)) %>%
      na.omit()
    idwDataList[[me]] <- idwData

    pppOUT <- ppp(idwData$Corrected.Longitude, idwData$Corrected.Latitude,
                  marks = idwData[,me], window = obs_window)

    powers <- seq(0.001, 10, 0.01)
    mse_result <- NULL
    for(power in powers){
      CV_idw <- idw(pppOUT, power=power, at="points")
      mse_result <- c(mse_result,
                      Metrics::mse(pppOUT$marks, CV_idw))
    }
    optimal_power <- powers[which.min(mse_result)]
    optimal_power

    idwOUT <- idw(pppOUT, power = optimal_power, at = "pixels", se = TRUE)
    print("IDW created")

    # Convert to a raster
    idwRASTER_estimate <- rast(idwOUT$estimate)
    idwRASTER_se <- rast(idwOUT$SE)

    names(idwRASTER_estimate) <- "estimate"
    names(idwRASTER_se) <- "se"

    idwRASTER_combo <- c(idwRASTER_estimate, idwRASTER_se)

    idwList[[me]] <- idwRASTER_combo
  }
  print("All IDWs created")

  map <- map_data("world")

  sfMap <- map %>%
    st_as_sf(coords = c("long", "lat")) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  # paletteList <- generate_palette()
  # palette <- paletteList
  # palette

  plotList <- vector("list", length = length(measures))
  names(plotList) <- measures
  for(me in measures){
    # me <- measures[11]
    print(me)
    idwRASTER_combo <- idwList[[me]]
    idwData <- idwDataList[[me]]

    idwRASTER_estimate <- idwRASTER_combo$estimate

    if(diff(terra::minmax(idwRASTER_estimate)) < 1){

      customBreaks <- pretty(terra::minmax(idwRASTER_estimate))
      contourBreaks <- seq(range(customBreaks)[1], range(customBreaks)[2],
                           length.out = 2*length(customBreaks))


    } else if(diff(terra::minmax(idwRASTER_estimate)) < 10){
      customBreaks <- seq(floor(terra::minmax(idwRASTER_estimate)[1,1]),
                          floor(terra::minmax(idwRASTER_estimate)[2,1]),
                          by = 1)
      contourBreaks <- seq(floor(terra::minmax(idwRASTER_estimate)[1,1]),
                           floor(terra::minmax(idwRASTER_estimate)[2,1]),
                           by = 0.25)

    } else {
      customBreaks <- seq(floor(terra::minmax(idwRASTER_estimate)[1,1]),
                          floor(terra::minmax(idwRASTER_estimate)[2,1]),
                          by = 2)
      contourBreaks <- seq(floor(terra::minmax(idwRASTER_estimate)[1,1]),
                           floor(terra::minmax(idwRASTER_estimate)[2,1]),
                           by = 0.5)

    }

    customLabel <- paste0("IDW Range:\n",
                          round(terra::minmax(idwRASTER_estimate)[1,1],
                                digits = 2), " - ", round(terra::minmax(idwRASTER_estimate)[2,1],
                                                          digits = 2),
                          "\nObserved Range:\n",
                          round(min(idwData[,3]),
                                digits = 2), " - ", round(max(idwData[,3]),
                                                          digits = 2))

    if(me == "ExCu.TaLe"){
      customTitle <- gsub("\\.", " ", "Exposed culmen : Tarsus length")
    } else if(me == "HaNa.TaLe"){
      customTitle <- gsub("\\.", " ", "Height at nares : Tarsus length")
    } else if(me == "NtBT.ExCu"){
      customTitle <- gsub("\\.", " ", "Nares to bill tip : Exposed culmen")
    } else if(me == "ExCuNtBTxHaNaTaLe"){
      customTitle <- gsub("\\.", " ", "Proximal bill surface area")
    } else {
      customTitle <- gsub("\\.", " ", me)
    }
    customSubtitle <- ""

    bottom5 <- idwData %>%
      dplyr::rename("measure" = tidyselect::matches(me)) %>%
      dplyr::slice_min(measure, n = 5)
    top5 <- idwData %>%
      dplyr::rename("measure" = tidyselect::matches(me)) %>%
      dplyr::slice_max(measure, n = 5)

    # paletteList
    # intermedius     culminatus  macrorhynchos   levaillantii    cul. x lev. tibetosinensis    lev. x mac.    tib. x lev.     connectens
    # "#003a00"      "#236a20"      "#4e9d48"      "#83d278"      "#236a20"      "#e8b946"      "#83d278"      "#e8b946"      "#583E23"
    # osai       hainanus    philippinus     japonensis   mandshuricus      colonorum
    # "#795600"      "#472900"      "#66717E"      "#af8614"      "#583E23"      "#519872"

    plotList[[me]] <-
      ggplot() +
      geom_sf(data = sfMap, fill = "white", colour = "white", linewidth = 0.1) +
      geom_spatraster(data = idwRASTER_estimate, aes(fill = estimate),
                      alpha = 0.95) +
      geom_sf(data = sfMap, fill = NA, colour = "white", linewidth = 0.1) +
      geom_spatraster_contour(data = idwRASTER_estimate,
                              breaks = customBreaks,
                              linewidth = 0.2, colour = "grey25") +
      geom_spatraster_contour(data = idwRASTER_estimate,
                              breaks = contourBreaks,
                              linewidth = 0.1, colour = "grey25") +
      geom_point(data = idwData, aes(x = Corrected.Longitude, y = Corrected.Latitude),
                 size = 0.15, alpha = 0.65, colour = "grey25", shape = 3) +
      geom_point(data = bottom5, aes(x = Corrected.Longitude, y = Corrected.Latitude),
                 size = 1.5, colour = "grey15", fill = "#003a00", shape = 21,
                 position = position_jitter(width = 0.1, height = 0.1)) +
      geom_point(data = top5, aes(x = Corrected.Longitude, y = Corrected.Latitude),
                 size = 1.5, colour = "grey15", fill = "#795600", shape = 21,
                 position = position_jitter(width = 0.1, height = 0.1)) +
      scale_fill_gradient(low = palette$sex_palette[1],
                          high = palette$sex_palette[2],
                          breaks = customBreaks,
                          limits = terra::minmax(idwRASTER_estimate)[1:2,1]) +
      # facet_wrap(facets = vars(lyr)) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        xlim = c(60, 150), ylim = c(-15, 60),
        expand = 0) +
      scale_y_continuous(breaks = seq(-180,180,20)) +
      scale_x_continuous(breaks = seq(-180,180,20)) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.background = element_rect(fill = "black"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            # legend.position = c(0, 0),
            legend.position = "right",
            legend.justification = c(0,0),
            legend.text = element_text(size = 4, face = 3),
            legend.title = element_text(size = 4.5, face = 3),
            legend.box.margin = margin(0, 0, 0, 0, "pt"),
            legend.box.spacing = unit(1, "pt"),
            plot.title = element_text(face = 4, size = 9,
                                      margin = margin(0,0,2,0)),
            plot.subtitle = element_text(size = 6, face = 3,
                                         margin = margin(0,0,2,0)),
            plot.caption = element_text(face = 3),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5),
            axis.title.x = element_text(face = 2, hjust = 0),
            axis.title.y = element_text(face = 2, hjust = 0),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            strip.text.y.left = element_text(angle = 0, vjust = 1),
            plot.margin = margin(0, 0, 0, 0, "pt")
      ) +
      guides(fill = guide_colourbar(title.position = "top",
                                    barwidth = grid::unit(2, "mm"),
                                    barheight = grid::unit(35, "mm"),
                                    ticks.colour = "grey25"))

    plotList[[me]] <- plotList[[me]] +
      labs(fill = customLabel,
           title = customTitle,
           x = "", y = "")

  }
  print("Plot list created")

  plotList[[10]] <- plotList[[10]] +
    labs(fill = customLabel,
         x = "Longitude", y = "Latitude")

  idwMapCombo <- patchwork::wrap_plots(plotList,
                                       ncol = 3)

  ggsave(idwMapCombo, file = here("Figures", "IDWmaps.png"),
         dpi = 300, width = 320, height = 320, units = "mm")
  ggsave(idwMapCombo, file = here("Figures", "IDWmaps.pdf"),
         width = 320, height = 320, units = "mm")

  return(plotList)

}
