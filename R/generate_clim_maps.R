#' generate_clim_maps
#'
#' @param crowData The crowData with all measurements, must have no NAs in lat long.
#' @returns A plot showing climate versus measures and a map.
#'
#' @export
generate_clim_maps <- function(crowData, palette){

  # library(here)
  # library(terra)
  # library(ggplot2)
  # library(dplyr)
  # library(sf)
  # library(tidyterra)
  # library(patchwork)

  # targets::tar_source()
  # targets::tar_load("crow_measurements_subspecies")
  # crowData <- crow_measurements_subspecies
  # paletteList <- generate_palette()
  # palette <- paletteList

  bio1_temp_hist <- terra::rast(here("Data", "bioData", "BIO1_HIST_1960_99.nc"))
  bio4_season_hist <- terra::rast(here("Data", "bioData", "BIO4_HIST_1960_99.nc"))
  bio12_precip_hist <- terra::rast(here("Data", "bioData", "BIO12_HIST_1960_99.nc"))

  longnames(bio1_temp_hist) <- paste0(longnames(bio1_temp_hist), " (°C)")
  longnames(bio4_season_hist) <- paste0(longnames(bio4_season_hist), " (°C)")
  longnames(bio12_precip_hist) <- paste0(longnames(bio12_precip_hist), " (mm)")

  # Bio1 Annual mean temperature: This indicator, expressed in °C, indicates the
  # total amount of energy inputs for the ecosystems in a year. The annual average
  # of daily mean temperature (Tg) is first computed for each year in the
  # considered long-term period, and then it is further averaged among all years
  # in the period.

  # Bio4 Temperature seasonality: This indicator, expressed in °C, measures the
  # temperature change throughout the year. Based on the formulation developed by
  # Hijmans in WorldClim18, the average of daily mean temperature (Tg) is
  # calculated for each calendar month in the selected period, and then the
  # Standard Deviation is computed among the 12 monthly values obtained. The
  # larger the value of the Standard Deviation, the greater the variability of
  # temperature within the year.

  # Bio12 Annual precipitation: This is a widely used indicator, representing the
  # total amount of water inputs to the ecosystems and to their water cycle. This
  # indicator is expressed in millimeter (mm) per year and it is derived by
  # averaging, along the whole period, the annual sum of daily precipitation
  # amounts P.

  # plot(bio1_hist)

  map <- map_data("world")

  sfMap <- map %>%
    st_as_sf(coords = c("long", "lat")) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  st_crs(sfMap) <- "WGS84"
  j <- 0
  fullPlotList <- list()
  for(me in c("Tarsus.length",
              "Bill.base.width",
              "Bill.width.at.skin.border",
              "Bill.base.length",
              "Width.at.nares",
              "Height.at.nares",
              "Nares.to.bill.tip",
              "Exposed.culmen",
              "ExCu.TaLe",
              "HaNa.TaLe",
              "NtBT.ExCu",
              "ExCuNtBTxHaNaTaLe"
  )){
    # me <- "NtBT.ExCu"
    print(me)

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

    bioMapList <- list()
    i <- 0
    for(b in list(
      bio1_temp_hist,
      bio4_season_hist,
      bio12_precip_hist)){
      # b <- bio1_temp_hist
      print(longnames(b))

      j <- j+1
      i <- i+1
      eCrop <- ext(60, 150, -15, 60)
      bioRast <- crop(b, eCrop)

      names(bioRast) <- "bio"

      coreMap <- ggplot() +
        geom_spatraster(data = bioRast, aes(fill = bio),
                        alpha = 0.95) +
        geom_sf(data = sfMap, fill = NA, colour = "white", linewidth = 0.1) +
        geom_point(data = crowData, aes(x = Corrected.Longitude, y = Corrected.Latitude),
                   position = position_jitter(width = 0.1, height = 0.1), size = 0.5,
                   colour = "grey15",
                   alpha = 0.5) +
        scale_fill_gradient(low = palette$sex_palette[1],
                            high = palette$sex_palette[2],
                            na.value = "white") +
        scale_size_continuous(range = c(0.25, 4)) +
        scale_y_continuous(breaks = seq(-180,180,20)) +
        scale_x_continuous(breaks = seq(-180,180,20)) +
        coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
          xlim = c(60, 150), ylim = c(-15, 60),
          expand = 0) +
        theme_bw() +
        theme(line = element_line(colour = "grey15"),
              text = element_text(colour = "grey15"),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              # legend.position = c(0, 0),
              legend.position = "none",
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
              # axis.line.x.bottom = element_line(linewidth = 0.5),
              # axis.line.y.left = element_line(linewidth = 0.5),
              axis.ticks = element_blank(),
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
                                      ticks.colour = "grey25")) +
        labs(x = "", y = "")

      bioExtract <- terra::extract(bioRast, crowData[c("Corrected.Longitude", "Corrected.Latitude")])
      crowData$bio <- bioExtract[,2]

      crowData_rename <- crowData %>%
        rename("measure" = matches(me))

      linearRel <- lm(measure ~ bio, data = crowData_rename)
      r2 <- performance::r2(linearRel)
      bio_p <- summary(linearRel)$coefficients[,4][2]

      # if(bio_p < 0.05){
      #   pValue <- "p-value < 0.05"
      # } else {
      pValue <- paste0("p-value: ", signif(bio_p, digits = 2))
      # }
      bonAdjP <- 0.05/(12*3)

      relPlot <- ggplot(crowData_rename) +
        # geom_point(aes(x = bio, y = ExCu.NtBT, fill = bio#, size = ExCu.NtBT
        # ), pch = 21,
        # colour = "grey15", size = 1.5) +
        geom_point(aes(x = bio, y = measure, colour = bio), pch = 16,
                   size = 1.5) +
        geom_abline(intercept = linearRel$coefficients[1], slope = linearRel$coefficients[2],
                    linetype = 2, alpha = 0.55, colour = "grey15", linewidth = 1.2) +
        annotate("label", x = -Inf, y = Inf,
                 label = paste0("Gradient: ", signif(linearRel$coefficients[2], digits = 2),
                                "\nR-squared: ", signif(r2$R2, digits = 2),
                                "\n", pValue,
                                "\nAdj. \u03B1: ", signif(bonAdjP, digits = 2),
                                width = 20),
                 fontface = 3, hjust = 0, vjust = 1, lineheight = 0.95, colour = "grey15",
                 alpha = 0.65, label.size = 0.75, size = 3.5) +
        annotate("text", x = Inf, y = Inf, label = stringr::str_wrap(longnames(b), width = 20),
                 fontface = 4, hjust = 1, vjust = 1, lineheight = 0.95) +
        # scale_size_continuous(range = c(0.5, 4)) +
        scale_y_continuous() +
        scale_colour_gradient(low = palette$sex_palette[1],
                              high = palette$sex_palette[2],
                              na.value = "white") +
        scale_fill_gradient(low = palette$sex_palette[1],
                            high = palette$sex_palette[2],
                            na.value = "white") +
        theme_bw() +
        theme(line = element_line(colour = "grey15"),
              text = element_text(colour = "grey15"),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.background = element_blank(),
              legend.position = "none",
              legend.direction = "horizontal",
              # legend.position = c(0.75, 0.1),
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
              # panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
              axis.line.x.bottom = element_line(linewidth = 0.5),
              axis.line.y.left = element_line(linewidth = 0.5),
              axis.line.y.right = element_line(linewidth = 0.5),
              axis.title.x = element_text(face = 2, hjust = 0),
              axis.title.y = element_text(face = 2, hjust = 0),
              strip.background = element_blank(),
              strip.text = element_text(face = 4, vjust = 0, hjust = 0),
              strip.text.y.left = element_text(angle = 0, vjust = 1),
              plot.margin = margin(0, 0, 0, 10, "pt")
        ) +
        guides(fill = guide_colourbar(title.position = "top",
                                      barwidth = grid::unit(35, "mm"),
                                      barheight = grid::unit(2, "mm"),
                                      ticks.colour = "grey25")) +
        labs(y = "", x = "")

      # if(i == 3){
      #   coreMap <- coreMap +
      #     theme(axis.text.x = element_text(),
      #           axis.text.y = element_text()) +
      #     labs(x = "Longitude", y = "Latitude")
      # }

      # coreMap +
      #   inset_element(relPlot, left = 0, bottom = 0, right = 0.35, top = 0.32)

      comboMap <- relPlot + coreMap +
        plot_layout(guides = "collect") +
        plot_annotation(title = paste0(customTitle, "\n",
                                       longnames(b))) &
        theme(title = element_text(face = 2))

      bioMapList[[i]] <- comboMap

    }

    fullPlot <- wrap_plots(bioMapList) +
      plot_layout(ncol = 1) +
      plot_annotation(title = customTitle) &
      theme(title = element_text(face = 4))

    ggsave(plot = fullPlot, filename = here("Figures", paste0("climMap_", me, ".png")),
           units = "mm", width = 210, height = 210)
    ggsave(plot = fullPlot, filename = here("Figures", paste0("climMap_", me, ".pdf")),
           units = "mm", width = 210, height = 210)

    fullPlotList[[j]] <- fullPlot

  }

  return(fullPlotList)

}
