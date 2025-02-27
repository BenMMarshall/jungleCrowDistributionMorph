#' generate_subspecies_maps
#'
#' @param crowData The crowData with all measurements, must have no NAs in lat long.
#' @returns A plot showing subspecies maps for Klock and Martens.
#'
#' @export
generate_subspecies_maps <- function(crowData, palette, subspeciesList){

  # targets::tar_load("crow_measurements_subspecies")
  #
  # crowData <- crow_measurements_subspecies

  # palette <- generate_palette()
  list2env(palette, envir = environment())

  klock <- subspeciesList$klock
  mart <- subspeciesList$mart

  klockLabelDF <-
    tribble(
      ~xstart, ~xend, ~ystart, ~yend, ~Subspecies, ~vjust, ~hjust,
      75, 80, 45, 35, "intermedius", 0, 0.5,
      70, 75, 2, 20, "culminatus", 1, 0.5,
      93, 102, -10, -2, "macrorhynchos", 1, 0.5,
      87.5, 95, 11, 18, "levaillantii", 1, 0.5,
      # 75, 80, 50, 35, "cul. x lev.", 0.5, 0.5,
      92, 95, 50, 35, "tibetosinensis", 0, 0.5,
      # 75, 80, 50, 35, "lev. x mac.", 0.5, 0.5,
      # 75, 80, 50, 35, "tib. x lev.", 0.5, 0.5,
      142, 128, 24, 26, "connectens", 1, 0.5,
      135, 125, 19, 24.5, "osai", 1, 0.5,
      113.5, 110, 17.5, 19, "hainanus", 1, 0.5,
      135, 124, 6, 10, "philippinus", 1, 0.5,
      142, 140, 32, 35, "japonensis", 1, 0.5,
      110, 130, 54, 47, "mandshuricus", 0, 0.5,
      119, 110, 22, 25, "colonorum", 1, 0.5
    )
  colourLabelsSubspecies_klock <- left_join(klockLabelDF,
                                      data.frame(colour = palette$klock_palette,
                                                 Subspecies = names(palette$klock_palette))) %>%
    mutate(label = glue::glue(
      "<i style='color:{colour}'>{Subspecies}</i>"
    ))

  martLabelDF <-
    tribble(
      ~xstart, ~xend, ~ystart, ~yend, ~Subspecies, ~vjust, ~hjust,
      # 70, 75, 2, 20, "hybrid", 1, 0.5,
      72, 82, 2, 25, "levaillantii", 1, 0.5,
      93, 102, -10, -2, "macrorhynchos", 1, 0.5,
      135, 124, 16, 10, "philippinus", 0, 0.5,
      142, 140, 24, 35,"japonensis", 1, 0.5
    )
  colourLabelsSubspecies_mart <- left_join(martLabelDF,
                                      data.frame(colour = palette$mart_palette,
                                                 Subspecies = names(palette$mart_palette))) %>%
    mutate(label = glue::glue(
      "<i style='color:{colour}'>{Subspecies}</i>"
    ))

  klock <- klock %>%
    mutate(Subspecies = case_when(
      Subspecies == "levaillantii x tibetosinensis" ~ "tib. x lev.",
      Subspecies == "culminatus x levaillantii" ~ "cul. x lev.",
      Subspecies == "levaillantii x macrorhynchos" ~ "lev. x mac.",
      TRUE ~ Subspecies
    ))

  sfPoints <- st_as_sf(crowData,
                       coords = c("Corrected.Longitude","Corrected.Latitude"), remove = FALSE)
  st_crs(sfPoints) <- st_crs(klock)

  map <- map_data("world")

  sfMap <- map %>%
    st_as_sf(coords = c("long", "lat"), crs = st_crs(klock)) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  (plotKlock <- ggplot() +
      geom_sf(data = sfMap, fill = "grey45", colour = NA) +
      geom_sf_pattern(data = klock, aes(fill = Subspecies, pattern = Subspecies),
                      pattern_density = 0.7,
                      pattern_spacing = 0.05,
                      pattern_alpha = 0.5,
                      pattern_colour = NA,
                      pattern_fill = "white",
                      colour = NA) +
      geom_sf(data = sfPoints, size = 0.5, colour = "grey15", shape = 16) +
      geom_curve(data = colourLabelsSubspecies_klock,
                 aes(x = xstart, xend = xend, y = ystart, yend = yend,
                     colour = Subspecies),
                 curvature = 0) +
      geom_richtext(data = colourLabelsSubspecies_klock,
                    aes(x = xstart, y = ystart,
                        colour = Subspecies, label = label,
                        hjust = hjust, vjust = vjust),
                    fill = NA, label.color = NA, size = 3,
                    label.padding = grid::unit(rep(0, 4), "pt")) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        xlim = c(60, 150), ylim = c(-15, 60),
        expand = 0) +
      scale_y_continuous(breaks = seq(-180,180,20)) +
      scale_x_continuous(breaks = seq(-180,180,20)) +
      scale_fill_manual(values = klock_palette) +
      scale_colour_manual(values = klock_palette) +
      scale_pattern_manual(values = c("none",
                                      "none",
                                      "stripe",
                                      "none",
                                      "none",
                                      "none",
                                      "none",
                                      "stripe",
                                      "none",
                                      "none",
                                      "none",
                                      "none",
                                      "none",
                                      "stripe",
                                      "none")) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = 2),
            legend.text = element_text(face = 3),
            plot.title = element_text(face = 4),
            axis.title.x = element_text(face = 2, hjust = 0),
            axis.title.y = element_text(face = 2, hjust = 0),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5)
      ) +
      labs(title = "Klockenhoff subspecies",
           fill = "Klock. ssp.",
           colour = "Klock. ssp.",
           hjust = "Klock. ssp.",
           vjust = "Klock. ssp.",
           pattern = "Klock. ssp.",
           x = "Longitude", y = "Latitude"))

  (plotMart <- ggplot() +
      geom_sf(data = sfMap, fill = "grey45", colour = NA) +
      geom_sf_pattern(data = mart,
                      aes(fill = Subspecies, pattern = Subspecies),
                      pattern_density = 0.7,
                      pattern_spacing = 0.05,
                      pattern_alpha = 0.5,
                      pattern_colour = NA,
                      pattern_fill = "white",
                      colour = NA) +
      geom_sf(data = sfPoints, size = 0.5, colour = "grey15", shape = 16) +
      geom_curve(data = colourLabelsSubspecies_mart,
                 aes(x = xstart, xend = xend, y = ystart, yend = yend,
                     colour = Subspecies),
                 curvature = 0) +
      geom_richtext(data = colourLabelsSubspecies_mart,
                    aes(x = xstart, y = ystart,
                        colour = Subspecies, label = label,
                        hjust = hjust, vjust = vjust),
                    fill = NA, label.color = NA, size = 3,
                    label.padding = grid::unit(rep(0, 4), "pt")) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        xlim = c(60, 150), ylim = c(-15, 60),
        expand = 0) +
      scale_y_continuous(breaks = seq(-180,180,20)) +
      scale_x_continuous(breaks = seq(-180,180,20)) +
      scale_fill_manual(values = mart_palette) +
      scale_colour_manual(values = mart_palette) +
      scale_pattern_manual(values = c("stripe",
                                      "none",
                                      "none",
                                      "none",
                                      "none")) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = 4),
            plot.title = element_text(face = 4),
            axis.title.x = element_text(face = 2, hjust = 0),
            axis.title.y = element_text(face = 2, hjust = 0),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5)
      ) +
      labs(title = "Martens et al. subspecies",
           fill = "Martens ssp.",
           colour = "Martens ssp.",
           hjust = "Martens ssp.",
           vjust = "Martens ssp.",
           pattern = "Martens ssp.",
           x = "", y = "")
  )

  (comboMaps <- plotKlock + plotMart +
      plot_layout(guides = "collect"))

  ggsave(comboMaps, file = here("Figures", "combined Subspecies Plot.png"),
         width = 260, height = 160, units = "mm")
  ggsave(comboMaps, file = here("Figures", "combined Subspecies Plot.pdf"),
         width = 260, height = 160, units = "mm")


  musColDf <- data.frame("colour" = c(
    "FMNH" = "#236a20",
    "CUMV" = "#5a596e",
    "AMNH" = "#583E23",
    "USNM" = "#795600",
    "NHMUK" ="#48799d"
  ))
  musColDf$museum <- row.names(musColDf)

  sfPointsMus <- sfPoints %>%
    mutate(museum = str_extract(Museum.catalogue.number,
                                "FMNH|CUMV|AMNH|USNM|NHMUK"),
           museumFacet = case_when(
             museum %in% c("FMNH", "CUMV") ~ "FMNH & CUMV",
             TRUE ~ museum
           )) %>%
    left_join(musColDf) %>%
    mutate(musCol = glue::glue(
      "<b><i style='color:{colour}'>{museumFacet}</i></b>"
    ))

  sfPointsMus$musCol[str_detect(sfPointsMus$musCol, "FMNH & CUMV")] <-
    "<b><i style='color:#236a20'>FMNH</i> & <i style='color:#66717E'>CUMV</i></b>"


  (plotLocs <- ggplot() +
      geom_sf(data = sfMap, fill = "grey45", colour = NA) +
      geom_sf(data = klock, fill = palette$klock_palette["japonensis"],
              alpha = 0.5, colour = NA) +
      geom_sf(data = mart, fill = palette$klock_palette["japonensis"],
              alpha = 0.5, colour = NA) +
      # geom_sf(data = sfPointsMus,
      #         aes(), size = 1.5, shape = 16, colour = "#ffffff") +
      geom_sf(data = sfPointsMus,
              aes(colour = museum), size = 1, shape = 16) +
      facet_wrap(.~musCol) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        xlim = c(60, 150), ylim = c(-15, 60),
        expand = 0) +
      scale_y_continuous(breaks = seq(-180,180,20)) +
      scale_x_continuous(breaks = seq(-180,180,20)) +
      # scale_fill_manual(values = klock_palette) +
      scale_colour_manual(values = c(
        "FMNH" = "#236a20",
        "CUMV" = "#5a596e",
        "AMNH" = "#583E23",
        "USNM" = "#795600",
        "NHMUK" ="#48799d"
      )) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = 2),
            legend.text = element_text(face = 3),
            plot.title = element_text(face = 4),
            axis.title.x = element_text(face = 2, hjust = 0),
            axis.title.y = element_text(face = 2, hjust = 0),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5),
            strip.background = element_blank(),
            strip.text = element_markdown(hjust = 0, face = 4)
      ) +
      labs(x = "Longitude", y = "Latitude"))

  ggsave(plotLocs, file = here("Figures", "Speciment Museum Plot.png"),
         width = 180, height = 160, units = "mm")
  ggsave(plotLocs, file = here("Figures", "Speciment Museum Plot.pdf"),
         width = 180, height = 160, units = "mm")

  return(comboMaps)

}
