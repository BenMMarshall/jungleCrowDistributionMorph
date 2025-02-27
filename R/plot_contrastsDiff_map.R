#' plot_contrastsDiff_map
#'
#' @param plot_contrastsDiff_map
#' @returns A simplified plots just showing the HDCI difference between neighbours
#'
#' @export
plot_contrastsDiff_map <- function(contrastsHDCI,
                                   palette,
                                   subspeciesList){
  # contrastsHDCI <- contrasts_hdci_combined
  subSpeciesHDCIs <- do.call(rbind, lapply(contrastsHDCI, function(x){x[["hdiContrasts_subspecies"]]}))

  contrasts_hdci <- subSpeciesHDCIs %>%
    mutate(plus85diff = case_when(
      probOver0 > 0.85 ~ "85%+ chance larger",
      probUnder0 > 0.85 ~ "85%+ chance smaller",
      TRUE ~ "Little difference"))

  # paletteList <- generate_palette()
  # palette <- paletteList
  list2env(palette, envir = environment())
  
  klock <- subspeciesList$klock
  mart <- subspeciesList$mart

  for(grp in c("martSpecies", "klockSpecies")){
    # grp <- "martSpecies"
    # grp <- "klockSpecies"

    if(grp == "klockSpecies"){

      adjacent <- c(
        #klock
        "mandshuricus-japonensis",
        "mandshuricus-tibetosinensis",
        "mandshuricus-colonorum",
        "japonensis-mandshuricus",
        "japonensis-connectens",
        "connectens-japonensis",
        "connectens-osai",
        "osai-connectens",
        "osai-colonorum",
        "osai-philippinus",
        "philippinus-osai",
        "philippinus-colonorum",
        "philippinus-hainanus",
        "philippinus-macrorhynchos",
        "colonorum-tibetosinensis",
        "colonorum-mandshuricus",
        "colonorum-osai",
        "colonorum-hainanus",
        "colonorum-macrorhynchos",
        "macrorhynchos-colonorum",
        "macrorhynchos-levaillantii",
        "macrorhynchos-philippinus",
        "macrorhynchos-hainanus",
        "hainanus-colonorum",
        "hainanus-macrorhynchos",
        "hainanus-philippinus",
        "levaillantii-macrorhynchos",
        "levaillantii-tibetosinensis",
        "levaillantii-culminatus",
        "culminatus-levaillantii",
        "culminatus-intermedius",
        "intermedius-culminatus",
        "intermedius-tibetosinensis",
        "tibetosinensis-intermedius",
        "tibetosinensis-levaillantii",
        "tibetosinensis-colonorum",
        "tibetosinensis-mandshuricus",

        "culminatus x levaillantii-culminatus",
        "culminatus x levaillantii-levaillantii",
        "culminatus-culminatus x levaillantii",
        "levaillantii-culminatus x levaillantii",

        "levaillantii x tibetosinensis-levaillantii",
        "levaillantii x tibetosinensis-tibetosinensis",
        "levaillantii-levaillantii x tibetosinensis",
        "tibetosinensis-levaillantii x tibetosinensis",

        "levaillantii x macrorhynchos-macrorhynchos",
        "levaillantii x macrorhynchos-levaillantii",
        "levaillantii-levaillantii x macrorhynchos",
        "macrorhynchos-levaillantii x macrorhynchos"
      )

    } else if(grp == "martSpecies"){
      adjacent <- c(
        #martens
        "levaillantii-japonensis",
        "levaillantii-macrorhynchos",
        # "levaillantii-hybrid",
        "japonensis-levaillantii",
        "japonensis-macrorhynchos",
        "japonensis-philippinus",
        "macrorhynchos-japonensis",
        "macrorhynchos-levaillantii",
        "macrorhynchos-philippinus",
        # "macrorhynchos-hybrid",
        "philippinus-japonensis",
        "philippinus-macrorhynchos",
        "hybrid-levaillantii",
        "hybrid-macrorhynchos"
      )
    }

    contrasts_hdci_filtered <- contrasts_hdci %>%
      filter(group == grp) %>%
      mutate(measure = gsub("\\.", " ", measure),
             adjacent = case_when(
               comparison %in% adjacent ~ "Adjacent",
               TRUE ~ "Not adjacent"
             ))

    if(grp == "klockSpecies"){

      # klock <- read_sf(here("Data", "Subspecies Polygons",
      #                       "Klockenhoff_1969_subspecies_clipped_to_map.GeoJSON"))

      centres <- klock %>%
        group_by(Subspecies) %>%
        st_centroid() %>%
        filter(!duplicated(Subspecies)) %>%
        st_coordinates() %>%
        cbind(klock %>%
                st_drop_geometry() %>%
                filter(!duplicated(Subspecies)))

      centres <- cbind(centres,
                       vjust = c(-0.5,1.5,1.5,1.5,0,-0.5,0,0,-0.5,1.5,-0.5,1.5,-0.5,-0.5,-0.5))

      centres <- centres %>%
        mutate(X = case_when(
          Subspecies == "culminatus x levaillantii" ~ X +0,
          Subspecies == "levaillantii x macrorhynchos" ~ X +3.5,
          Subspecies == "levaillantii x tibetosinensis" ~ X +2.5,
          Subspecies == "japonensis" ~ X +1,
          Subspecies == "connectens" ~ X +1,
          Subspecies == "hainanus" ~ X +2.5,
          Subspecies == "mandshuricus" ~ X -2.5,
          Subspecies == "philippinus" ~ X +2.5,
          TRUE ~ X
        ),
        Y = case_when(
          Subspecies == "culminatus x levaillantii" ~ Y +1.5,
          Subspecies == "levaillantii x macrorhynchos" ~ Y +0.5,
          Subspecies == "levaillantii x tibetosinensis" ~ Y +0,
          Subspecies == "hainanus" ~ Y +0,
          Subspecies == "mandshuricus" ~ Y -2,
          TRUE ~ Y
        ),
        vjustCustom = case_when(
          Subspecies == "culminatus x levaillantii" ~ 0,
          Subspecies == "levaillantii x macrorhynchos" ~ 0,
          Subspecies == "levaillantii x tibetosinensis" ~ 0,
          Subspecies == "mandshuricus" ~ -0.75,
          Subspecies == "japonensis" ~ -0.75,
          Subspecies == "tibetosinensis" ~ -0.75,
          Subspecies == "intermedius" ~ -0.75,
          Subspecies == "culminatus" ~ 1.8,
          Subspecies == "levaillantii" ~ 1.85,
          Subspecies == "macrorhynchos" ~ 1.8,
          Subspecies == "philippinus" ~ 1.8,
          Subspecies == "hainanus" ~ -0.3,
          Subspecies == "colonorum" ~ -0.2,
          Subspecies == "osai" ~ 1.5,
          Subspecies == "connectens" ~ 0.05,
          TRUE ~ 1.5
        ),
        hjustCustom = case_when(
          Subspecies == "culminatus x levaillantii" ~ 0,
          Subspecies == "levaillantii x macrorhynchos" ~ 0,
          Subspecies == "levaillantii x tibetosinensis" ~ 0,
          Subspecies == "mandshuricus" ~ 0.5,
          Subspecies == "japonensis" ~ 0.2,
          Subspecies == "tibetosinensis" ~ 0.7,
          Subspecies == "intermedius" ~ 0.6,
          Subspecies == "culminatus" ~ 0.5,
          Subspecies == "levaillantii" ~ 1,
          Subspecies == "macrorhynchos" ~ 0.5,
          Subspecies == "philippinus" ~ 0.3,
          Subspecies == "hainanus" ~ 1.1,
          Subspecies == "colonorum" ~ -0.15,
          Subspecies == "osai" ~ -0.2,
          Subspecies == "connectens" ~ -0.15,
          TRUE ~ 0.5
        )
        )

      colourSubDF <- as.data.frame(palette$klock_palette)
      colourSubDF <- colourSubDF %>%
        mutate(Subspecies = row.names(colourSubDF)) %>%
        rename("colour" = `palette$klock_palette`)

    } else {
      # grp <- "martSpecies"
      # mart <- read_sf(here("Data", "Subspecies Polygons",
      #                      "Martens_2000_subspecies_clipped_to_map.GeoJSON"))

      centres <- mart %>%
        group_by(Subspecies) %>%
        st_centroid() %>%
        filter(!duplicated(Subspecies)) %>%
        st_coordinates() %>%
        cbind(mart %>%
                st_drop_geometry() %>%
                filter(!duplicated(Subspecies)))
      centres <- cbind(centres,
                       vjust = c(-0.5,1.5,-0.5,-0.5,1.5))

      centres <- centres %>%
        mutate(
          vjustCustom = case_when(
            Subspecies == "japonensis" ~ -0.75,
            Subspecies == "levaillantii" ~ 1.8,
            Subspecies == "macrorhynchos" ~ 1.8,
            Subspecies == "philippinus" ~ 1.8,
            TRUE ~ 1.5
          ),
          hjustCustom = case_when(
            Subspecies == "japonensis" ~ 0.5,
            Subspecies == "levaillantii" ~ 0.95,
            Subspecies == "macrorhynchos" ~ 0.5,
            Subspecies == "philippinus" ~ 0.15,
            TRUE ~ 0.5
          )
        )

      colourSubDF <- as.data.frame(palette$mart_palette)
      colourSubDF <- colourSubDF %>%
        mutate(Subspecies = row.names(colourSubDF)) %>%
        rename("colour" = `palette$mart_palette`)
    }

    centres <- centres %>%
      left_join(colourSubDF) %>%
      mutate(textSubspecies = glue::glue("<b><i style='color:{colour}'>{Subspecies}</i></b>"))

    centres <- centres %>%
      filter(!Subspecies == "hybrid")

    segmentData <-
      contrasts_hdci_filtered %>%
      filter(adjacent == "Adjacent") %>%
      group_by(gbase, gcomp, measure) %>%
      slice_head(n = 1) %>%
      left_join(centres %>%
                  rename("gbase" = Subspecies,
                         "Xbase" = X,
                         "Ybase" = Y),
                by = "gbase") %>%
      left_join(centres %>%
                  rename("gcomp" = Subspecies,
                         "Xcomp" = X,
                         "Ycomp" = Y),
                by = "gcomp") %>%
      group_by(measure) %>%
      mutate(min = min(y, na.rm = TRUE),
             max = max(y, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(measure) %>%
      mutate(scaledValue = (y-min)/(max-min),
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
                                "ExCu TaLe",
                                "HaNa TaLe",
                                "NtBT ExCu",
                                "ExCu NtBT HaNa TaLe"
                              ))) %>%
      mutate(
        adjYbase = case_when(
          measure == "Bill base width" ~ Ybase -5,
          measure == "Bill width at skin border" ~ Ybase -4,
          measure == "Bill base length" ~ Ybase -3,
          measure == "Width at nares" ~ Ybase -2,
          measure == "Height at nares" ~ Ybase -1,
          measure == "Nares to bill tip" ~ Ybase +0,
          measure == "Exposed culmen" ~ Ybase +1,
          measure == "Tarsus length" ~ Ybase +2,
          measure == "ExCu TaLe" ~ Ybase +3,
          measure == "HaNa TaLe" ~ Ybase +4,
          measure == "NtBT ExCu" ~ Ybase +5,
          measure == "ExCu NtBT HaNa TaLe" ~ Ybase +6
        ),
        adjYcomp = case_when(
          measure == "Bill base width" ~ Ycomp -5,
          measure == "Bill width at skin border" ~ Ycomp -4,
          measure == "Bill base length" ~ Ycomp -3,
          measure == "Width at nares" ~ Ycomp -2,
          measure == "Height at nares" ~ Ycomp -1,
          measure == "Nares to bill tip" ~ Ycomp +0,
          measure == "Exposed culmen" ~ Ycomp +1,
          measure == "Tarsus length" ~ Ycomp +2,
          measure == "ExCu TaLe" ~ Ycomp +3,
          measure == "HaNa TaLe" ~ Ycomp +4,
          measure == "NtBT ExCu" ~ Ycomp +5,
          measure == "ExCu NtBT HaNa TaLe" ~ Ycomp +6
        )
      ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(alphaComp = paste0(sort(str_split(comparison, pattern = "-", simplify = TRUE)), collapse = ",")) %>%
      ungroup() %>%
      group_by(alphaComp, measure) %>%
      slice_head(n = 1)

    conDiffers <- segmentData %>%
      group_by(alphaComp) %>%
      summarise(
        meanX = mean(c(Xbase, Xcomp)),
        meanY = mean(c(Ybase, Ycomp)),
        nConsiderDiffer = sum(!plus85diff == "Little difference")
      )

    segmentDataPlot <- segmentData %>%
      group_by(alphaComp) %>%
      slice_head(n = 1) %>%
      left_join(conDiffers)

    adjacentDiffPlot <- ggplot() +
      geom_segment(data = segmentDataPlot,
                   aes(x = Xcomp, xend = Xbase,
                       y = Ycomp, yend = Ybase,
                       linewidth = nConsiderDiffer,
                       colour = nConsiderDiffer)) +
      geom_point(data = segmentDataPlot,
                 aes(x = meanX,
                     y = meanY,
                     colour = nConsiderDiffer),
                 size = 5) +
      geom_point(data = segmentDataPlot,
                 aes(x = meanX,
                     y = meanY),
                 size = 3,
                 colour = "white") +
      geom_text(data = segmentDataPlot,
                aes(x = meanX,
                    y = meanY,
                    label = nConsiderDiffer,
                    colour = nConsiderDiffer),
                size = 2, fontface = 2, hjust = 0.5, vjust = 0.5) +
      geom_point(data = centres,
                 aes(x = X, y = Y, fill = Subspecies),
                 size = 6, shape = 21, colour = "white") +
      geom_richtext(data = centres,
                    aes(x = X, y = Y, label = textSubspecies,
                        vjust = vjustCustom, hjust = hjustCustom),
                    fill = NA, label.color = NA,
                    label.padding = grid::unit(rep(0, 4), "pt")) +
      scale_color_gradient(low = "grey45", high = palette$larger_smaller_palette[1]) +
      scale_fill_manual(values = mart_palette) +
      scale_fill_manual(values = klock_palette) +
      scale_linewidth_continuous(range = c(0.75, 2)) +
      coord_sf(# crs = st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"),
        # xlim = c(60, 150), ylim = c(-15, 60),
        xlim = c(70, 150), ylim = c(-5, 50),
        expand = 0) +
      theme_void() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = 2),
            legend.text = element_text(face = 3),
            plot.title = element_text(face = 4)
      )

    if(grp == "klockSpecies"){
      adjacentDiffPlot_klock <- adjacentDiffPlot +
        labs(title = "Klockenhoff subspecies")
    } else {
      adjacentDiffPlot_martens <- adjacentDiffPlot +
        labs(title = "Martens et al. subspecies")
    }

  }

  combinedMaps <- adjacentDiffPlot_klock + adjacentDiffPlot_martens

  ggsave(combinedMaps, file = here("Figures", "combined Subspecies Difference Plot.png"),
         width = 300, height = 200, units = "mm")
  ggsave(combinedMaps, file = here("Figures", "combined Subspecies Difference Plot.pdf"),
         width = 300, height = 200, units = "mm")

  return(combinedMaps)

}




