#' generate_subspecies_maps
#'
#' @param crowData The crowData with all measurements, must have no NAs in lat long.
#' @returns A plot showing measuresments on the map.
#'
#' @export
generate_measurements_maps <- function(crowData,
                                       palette,
                                       subspeciesList){

  # targets::tar_load("crow_measurements_subspecies")
  #
  # crowData <- crow_measurements_subspecies

  paletteList <- palette
  
  klock <- subspeciesList$klock
  mart <- subspeciesList$mart

  # klock <- read_sf(here("Data", "Subspecies Polygons",
  #                       "Klockenhoff_1969_subspecies_clipped_to_map.GeoJSON"))

  map <- map_data("world")

  sfMap <- map %>%
    st_as_sf(coords = c("long", "lat"), crs = st_crs(klock)) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  # matches(c("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
  #           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
  #           "Exposed.culmen","Tarsus.length"))
  crowDataLong <- crowData %>%
    select(c("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
             "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
             "Exposed.culmen","Tarsus.length","Corrected.Longitude","Corrected.Latitude",
             "ExCu.TaLe", "HaNa.TaLe", "NtBT.ExCu", "ExCuNtBTxHaNaTaLe")) %>%
    pivot_longer(cols = c("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
                          "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
                          "Exposed.culmen","Tarsus.length",
                          "ExCu.TaLe", "HaNa.TaLe", "NtBT.ExCu", "ExCuNtBTxHaNaTaLe"),
                 names_to = "measure") %>%
    group_by(measure) %>%
    mutate(min = min(value, na.rm = TRUE),
           max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(measure = case_when(
      measure == "ExCu.TaLe" ~ "Exposed culmen : Tarsus length",
      measure == "HaNa.TaLe" ~ "Height at nares : Tarsus length",
      measure == "NtBT.ExCu" ~ "Nares to bill tip : Exposed culmen",
      measure == "ExCuNtBTxHaNaTaLe" ~ "Proximal bill surface area",
      TRUE ~ gsub("\\.", " ", measure)
    )) %>%
    mutate(scaledValue = (value-min)/(max-min),
           measure = factor(gsub("\\.", " ", measure),
                            levels = c(
                              "Bill base width",
                              "Bill width at skin border",
                              "Bill base length",
                              "Width at nares",
                              "Height at nares",
                              "Nares to bill tip",
                              "Exposed culmen",
                              "Tarsus length",
                              "Exposed culmen : Tarsus length",
                              "Height at nares : Tarsus length",
                              "Nares to bill tip : Exposed culmen",
                              "Proximal bill surface area"
                            )))

  (measuresMap <- crowDataLong %>%
      ggplot() +
      geom_sf(data = sfMap, fill = "grey65", colour = NA) +
      geom_point(aes(x = Corrected.Longitude, y = Corrected.Latitude,
                     colour = scaledValue),
                 size = 0.75, alpha = 0.75, shape = 16,
                 position = position_jitter(seed = 1, width = 1.5, height = 1.5)) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        xlim = c(60, 150), ylim = c(-15, 60),
        expand = 0) +
      scale_y_continuous(breaks = seq(-180,180,20)) +
      scale_x_continuous(breaks = seq(-180,180,20)) +
      facet_wrap(facets = vars(measure), ncol = 3) +
      scale_color_gradient(low = paletteList$sex_palette[1],
                           high = paletteList$sex_palette[2]) +
      scale_fill_gradient(low = paletteList$sex_palette[1],
                          high = paletteList$sex_palette[2]) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(face = 4),
            plot.caption = element_text(face = 3),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            axis.line.y.left = element_line(linewidth = 0.5),
            axis.title.x = element_text(face = 3, hjust = 0),
            axis.title.y = element_text(face = 3, hjust = 0),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            strip.text.y.left = element_text(angle = 0, vjust = 1)
      ) +
      guides(colour = guide_colourbar(title.position = "top",
                                      barwidth = grid::unit(50, "mm"),
                                      barheight = grid::unit(3, "mm"))) +
      labs(colour = "Scaled measure",
           x = "Longitude", y = "Latitude", caption = "Points jittered 1.5 x 1.5.")
  )

  ggsave(measuresMap, file = here("Figures", "measuresMap.png"),
         dpi = 300, width = 320, height = 320, units = "mm")
  ggsave(measuresMap, file = here("Figures", "measuresMap.pdf"),
         width = 320, height = 320, units = "mm")

  return(measuresMap)
}
