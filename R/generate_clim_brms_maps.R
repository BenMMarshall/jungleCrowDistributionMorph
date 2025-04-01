#' generate_clim_brms_maps
#'
#' @param crowData The crowData with all measurements, must have no NAs in lat long.
#' @returns A plot showing climate versus measures and a map.
#'
#' @export
generate_clim_brms_maps <- function(crowData, palette, climateFiles, measure){
  # library(here)
  # library(terra)
  # library(ggplot2)
  # library(stringr)
  # library(dplyr)
  # library(sf)
  # library(tidyterra)
  # library(patchwork)
  # library(brms)
  # library(bayesplot)
  # library(tidybayes)
  # targets::tar_source()
  # targets::tar_load("climate_files_locations")
  # targets::tar_load("crow_measurements_subspecies")
  # climateFiles <- climate_files_locations
  # crowData <- crow_measurements_subspecies
  # paletteList <- generate_palette()
  # palette <- paletteList
  
  bio1_temp_hist <- terra::rast(climateFiles[str_detect(climateFiles, "bio_1.tif")])
  bio4_season_hist <- terra::rast(climateFiles[str_detect(climateFiles, "bio_4.tif")])
  bio12_precip_hist <- terra::rast(climateFiles[str_detect(climateFiles, "bio_12.tif")])
  
  # longnames(bio1_temp_hist) <- paste0(longnames(bio1_temp_hist), " (°C)")
  # longnames(bio4_season_hist) <- paste0(longnames(bio4_season_hist), " (°C)")
  # longnames(bio12_precip_hist) <- paste0(longnames(bio12_precip_hist), " (mm)")
  names(bio1_temp_hist) <- "Annual mean temperature (°C)"
  names(bio4_season_hist) <- "Temperature seasonality (°C)"
  names(bio12_precip_hist) <- "Annual precipitation (mm)"
  
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
  
  # j <- 0
  # fullPlotList <- list()
  # bioCoefMeList <- list()
  # for(me in measure){
  # me <- "Exposed.culmen"
  me <- measure
  
  print(me)
  
  bioStack <- c(bio1_temp_hist,
                bio4_season_hist,
                bio12_precip_hist)
  eCrop <- ext(60, 150, -15, 60)
  bioRast <- crop(bioStack, eCrop)
  
  bioExtract <- terra::extract(bioRast, crowData[c("Corrected.Longitude", "Corrected.Latitude")])
  crowData$bio_temper <- bioExtract[,2]
  crowData$bio_season <- bioExtract[,3]
  crowData$bio_precip <- bioExtract[,4]
  
  crowData_rename <- crowData %>%
    rename("measure" = matches(me))
  
  brmOUT <- brm(measure ~ bio_temper + bio_season + bio_precip,
                data = crowData_rename,
                family = gaussian,
                iter = 2000,
                warmup = 500,
                thin = 1,
                chains = 4
  )
  
  r2brms <- performance::r2_bayes(brmOUT)
  
  medHDCIcoef <- brmOUT %>%
    gather_draws(b_Intercept, b_bio_temper, b_bio_season, b_bio_precip, sigma) %>%
    median_hdci()
  
  brmSummary <- summary(brmOUT)
  
  medHDCIcoef$measure <- me
  # medHDCIcoef$bio <- names(b)
  medHDCIcoef$r2 <- r2brms$R2_Bayes
  medHDCIcoef$rhat <- c(brmSummary$fixed[row.names(brmSummary$fixed) == "Intercept",]$Rhat,
                        brmSummary$fixed[row.names(brmSummary$fixed) == "bio_temper",]$Rhat,
                        brmSummary$fixed[row.names(brmSummary$fixed) == "bio_season",]$Rhat,
                        brmSummary$fixed[row.names(brmSummary$fixed) == "bio_precip",]$Rhat,
                        brmSummary$spec_pars$Rhat
  )
  
  # bioCoefMeList[[j]] <- medHDCIcoef
  
  vars <- get_variables(brmOUT)
  varsToPlot <- vars[stringr::str_detect(vars, "b_")]
  
  traceplot <- mcmc_trace(brmOUT, pars = varsToPlot)
  ggsave(traceplot,
         filename = here("ModelOutput", paste0("climModel_", me, "_traceplot.png")),
         dpi = 300, width = 210, height = 120,
         units = "mm")
  
  acfplot <- mcmc_acf(brmOUT, pars = varsToPlot)
  ggsave(acfplot,
         filename = here("ModelOutput", paste0("climModel_", me, "_acfplot.png")),
         dpi = 300, width = 210, height = 140,
         units = "mm")
  
  # Map and relationship plot -----------------------------------------------
  
  estimateLinesData <- crowData_rename %>%
    modelr::data_grid(
      bio_temper = modelr::seq_range(bio_temper, n = 5),
      bio_season = modelr::seq_range(bio_season, n = 5),
      bio_precip = modelr::seq_range(bio_precip, n = 5)
    ) %>%
    add_epred_draws(brmOUT) %>%
    ungroup() %>% 
    dplyr::select(-.row, -.chain, -.iteration, -.draw) %>% 
    tidyr::pivot_longer(cols = c(bio_temper, bio_season, bio_precip))
  
  crowData_long <- crowData_rename %>% 
    tidyr::pivot_longer(cols = c(bio_temper, bio_season, bio_precip))
  
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
  
  customTitleRich <- paste0("<b>", customTitle, "</b>",
                            "<i><span style = 'font-size:11pt'> R<sup>2</sup>: ",
                            signif(r2brms$R2_Bayes[1], digits = 2), "</span></i>")
  
  bioMapList <- vector("list", 3)
  names(bioMapList) <- c("bio_temper", "bio_season", "bio_precip")
  for(bioS in c("bio_temper", "bio_season", "bio_precip")){
    # bioS <- "bio_temper"
    
    medHDCIcoef[medHDCIcoef$.variable == paste0("b_", bioS),]
    
    bioLayer <- bioRast[[str_detect(names(bioRast), sub("bio_", "", bioS))]]
    
    relPlot <- estimateLinesData %>% 
      filter(name == bioS) %>% 
      ggplot(aes(x = value, y = .epred)) +
      stat_lineribbon(linetype = 2, alpha = 0.45, colour = "grey15", linewidth = 1.2,
                      .width = 0.95, point_interval = "median_hdci",
                      fill = "grey15") +
      geom_point(data = crowData_long %>% 
                   filter(name == bioS),
                 aes(x = value, y = measure, colour = value), pch = 16,
                 size = 1.5) +
      facet_wrap(facets = vars(name), ncol = 1, scales = "free_x") +
      annotate("label", x = -Inf, y = Inf,
               label = paste0("Gradient: ",
                              signif(medHDCIcoef[medHDCIcoef$.variable == paste0("b_", bioS),]$.value, digits = 2),
                              " (95% CrI: ",
                              signif(medHDCIcoef[medHDCIcoef$.variable == paste0("b_", bioS),]$.lower, digits = 2),
                              " - ",
                              signif(medHDCIcoef[medHDCIcoef$.variable == paste0("b_", bioS),]$.upper, digits = 2),
                              ")"),
               fontface = 3, hjust = 0, vjust = 1, lineheight = 0.95, colour = "grey15",
               alpha = 0.65, label.size = 0.75, size = 3.5) +
      # annotate("text", x = Inf, y = Inf, label = stringr::str_wrap(names(b), width = 20),
      #          fontface = 4, hjust = 1, vjust = 1, lineheight = 0.95) +
      # scale_size_continuous(range = c(0.5, 4)) +
      scale_y_continuous() +
      scale_colour_gradient(low = palette$sex_palette[1],
                            high = palette$sex_palette[2],
                            na.value = "white") +
      # scale_fill_gradient(low = palette$sex_palette[1],
      #                     high = palette$sex_palette[2],
      #                     na.value = "white") +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.background = element_blank(),
            legend.position = "none",
            legend.direction = "horizontal",
            # legend.position.inside = c(0.75, 0.1),
            legend.justification = c(0,0),
            legend.text = element_text(size = 4, face = 3),
            legend.title = element_text(size = 4.5, face = 3),
            legend.box.margin = margin(0, 0, 0, 0, "pt"),
            legend.box.spacing = unit(1, "pt"),
            plot.title = element_text(face = 4, size = 11,
                                      margin = margin(0,0,2,0)),
            plot.subtitle = element_text(size = 6, face = 3,
                                         margin = margin(0,0,2,0)),
            plot.caption = element_text(face = 3),
            # panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5),
            axis.line.y.right = element_line(linewidth = 0.5),
            axis.title.x = element_text(face = 3, hjust = 1),
            axis.title.y = element_text(face = 2, hjust = 0),
            strip.background = element_blank(),
            strip.text = element_blank(),
            # strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            # strip.text.y.left = element_text(angle = 0, vjust = 1),
            plot.margin = margin(5, 0, 0, 10, "pt")
      ) +
      guides(fill = guide_colourbar(title.position = "top",
                                    barwidth = grid::unit(35, "mm"),
                                    barheight = grid::unit(2, "mm"),
                                    ticks.colour = "grey25")) +
      labs(y = "", x = "", title = names(bioLayer))
    
    coreMap <- ggplot() +
      geom_spatraster(data = bioLayer,
                      alpha = 0.95) +
      facet_wrap(facets = vars(lyr), ncol = 1) +
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
            strip.text = element_blank(),
            # strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            # strip.text.y.left = element_text(angle = 0, vjust = 1),
            plot.margin = margin(5, 0, 0, 0, "pt")
      ) +
      guides(fill = guide_colourbar(title.position = "top",
                                    barwidth = grid::unit(2, "mm"),
                                    barheight = grid::unit(35, "mm"),
                                    ticks.colour = "grey25")) +
      labs(x = "", y = "")
    
    comboMap <- relPlot + coreMap +
      plot_layout(guides = "collect") +
      plot_annotation(title = paste0(customTitleRich),
                      subtitle = names(bioLayer)) &
      theme(plot.title = element_markdown(),
            plot.subtitle = element_text(face = 3))
    
    bioMapList[[bioS]] <- comboMap
  }
  
  fullPlot <- wrap_plots(bioMapList) +
    plot_layout(ncol = 1) +
    plot_annotation(title = customTitleRich) &
    theme(title = element_markdown())
  
  ggsave(plot = fullPlot, filename = here("Figures", paste0("climMap_", me, ".png")),
         units = "mm", width = 210, height = 210)
  ggsave(plot = fullPlot, filename = here("Figures", paste0("climMap_", me, ".pdf")),
         units = "mm", width = 210, height = 210)
  
  # fullPlotList[[j]] <- fullPlot
  # }
  
  # allCoefs <- do.call(rbind, bioCoefMeList)
  
  return(list(fullPlot,
              medHDCIcoef,
              r2brms))
}
