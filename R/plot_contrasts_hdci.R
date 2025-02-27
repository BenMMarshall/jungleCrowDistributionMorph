#' plot_contrasts_hdci
#'
#' @param plot_contrasts_hdci
#' @returns A simplified plots just showing the HDCI of the pred post dists
#'
#' @export
plot_contrasts_hdci <- function(contrastsHDCI, palette, insetMaps){

  # targets::tar_load("contrasts_hdci_combined")

  subSpeciesHDCIs <- do.call(rbind, lapply(contrastsHDCI, function(x){x[["hdiContrasts_subspecies"]]}))

  # paletteList <- generate_palette()
  paletteList <- palette

  contrasts_hdci <- subSpeciesHDCIs %>%
    mutate(plus85diff = case_when(
      probOver0 > 0.85 ~ "85%+ chance larger",
      probUnder0 > 0.85 ~ "85%+ chance smaller",
      TRUE ~ "Little difference"))

  plotList <- list()
  i <- 0
  for(grp in unique(contrasts_hdci$group)){
    # grp <- unique(contrasts_hdci$group)[1]
    i <- i+1

    contrasts_hdci %>%
      filter(group == grp) %>%
      pull(comparison) %>% unique()

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
        "levaillantii-hybrid",
        "japonensis-levaillantii",
        "japonensis-macrorhynchos",
        "japonensis-philippinus",
        "macrorhynchos-japonensis",
        "macrorhynchos-levaillantii",
        "macrorhynchos-philippinus",
        "macrorhynchos-hybrid",
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
             )) %>%
      mutate(measure = case_when(
        measure == "ExCu TaLe" ~ "Exposed culmen : Tarsus length",
        measure == "HaNa TaLe" ~ "Height at nares : Tarsus length",
        measure == "NtBT ExCu" ~ "Nares to bill tip : Exposed culmen",
        measure == "ExCuNtBTxHaNaTaLe" ~ "Proximal bill surface area",
        TRUE ~ gsub("\\.", " ", measure)
      ))

    if(grp == "klockSpecies"){
      contrasts_hdci_filtered <- contrasts_hdci_filtered %>%
        mutate(gcomp = case_when(
          gcomp == "levaillantii x tibetosinensis" ~ "tib. x lev.",
          gcomp == "culminatus x levaillantii" ~ "cul. x lev.",
          gcomp == "levaillantii x macrorhynchos" ~ "lev. x mac.",
          TRUE ~ gcomp
        ),
        gbase = case_when(
          gbase == "levaillantii x tibetosinensis" ~ "tib. x lev.",
          gbase == "culminatus x levaillantii" ~ "cul. x lev.",
          gbase == "levaillantii x macrorhynchos" ~ "lev. x mac.",
          TRUE ~ gbase
        ))
    }

    alphaValues <- c(0.25, 0)
    names(alphaValues) <- c("Adjacent", "Not adjacent")

    adjDataOnly <- contrasts_hdci_filtered %>%
      group_by(comparison) %>%
      select(gbase, gcomp, comparison, adjacent) %>%
      slice_head(n = 1)

    (constrastsHDCIPlot <-
        contrasts_hdci_filtered %>%
        ggplot() +
        geom_vline(xintercept = 0, linetype = 2, colour = "grey85") +
        # geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
        geom_rect(data = adjDataOnly,
                  aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                      alpha = adjacent), fill = "black") +
        geom_errorbarh(aes(xmin = ymin, xmax = ymax, y = measure,
                           colour = plus85diff), height = 0.2) +
        geom_point(aes(x = y, y = measure,
                       colour = plus85diff), size = 0.75) +
        scale_x_continuous(breaks = seq(-20,20,20),
                           minor_breaks = seq(-20,20,10)) +
        scale_colour_manual(values = paletteList$larger_smaller_palette) +
        scale_alpha_manual(values = alphaValues) +
        facet_grid(cols = vars(gbase), rows = vars(gcomp)) +
        labs(y = "Measure", x = "Contrast",
             colour = "Highlighted differences",
             alpha = "Adjacent subspecies") +
        theme_bw() +
        theme(line = element_line(colour = "grey15"),
              text = element_text(colour = "grey15"),
              legend.justification = c(0.5,0),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line.x.bottom = element_line(linewidth = 0.25),
              axis.ticks = element_line(linewidth = 0.2),
              # axis.line.y.left = element_line(linewidth = 0.85),
              legend.title = element_text(face = 2),
              axis.title.x.bottom = element_text(hjust = 1),
              axis.title.y.left = element_text(hjust = 1),
              axis.text.y.left = element_text(size = 6.5),
              axis.text.x.bottom = element_text(size = 4, margin = margin(0.1,0,0,0)),
              axis.text.y = element_text(face = 1),
              axis.ticks.y = element_blank(),
              axis.title = element_text(face = 2),
              axis.ticks.length = unit(0.5, "mm"),
              strip.background = element_blank(),
              strip.text = element_text(face = 4, vjust = 0, hjust = 0),
              strip.text.y.left = element_text(angle = 0, vjust = 1),
              strip.text.y.right = element_text(angle = 0, vjust = 1)
        )
    )

    if(grp == "martSpecies"){
      constrastsHDCIPlot <- constrastsHDCIPlot +
        scale_x_continuous(breaks = seq(-10,10,10),
                           minor_breaks = seq(-10,10,5)) +
        theme(legend.position = c(0.5, 0.08),
              legend.background = element_blank(),
              legend.box = element_blank())
    } else {
      constrastsHDCIPlot <- constrastsHDCIPlot +
        theme(legend.position = c(0.55, 0.08),
              legend.background = element_blank(),
              legend.box = element_blank())
    }

    # Get ggplot grob
    grobs <- ggplotGrob(constrastsHDCIPlot)

    if(grp == "klockSpecies"){
      rowsAndCols <- 15
    } else {
      rowsAndCols <- 4
    }

    exclusions <- paste0("panel-", 1:rowsAndCols, "-", 1:rowsAndCols, "$")
    k <- 0
    for(i in 1:rowsAndCols){
      for(j in rowsAndCols:i){
        k <- k+1
        print(paste(i, j))
        exclusions[k] <- paste0("panel-", j, "-", i, "$")
      }
    }

    for(ex in exclusions){
      pos <- grep(pattern = ex, grobs$layout$name)
      grobs$grobs[[pos]] <- nullGrob()
    }

    grobs <- gtable_filter(grobs,
                           "axis-b-1$",
                           trim = FALSE, invert = TRUE)
    grobs <- gtable_filter(grobs,
                           "axis-l-1$",
                           trim = FALSE, invert = TRUE)

    grobs <- gtable_filter(grobs,
                           paste0("axis-l-", rowsAndCols, "$"),
                           trim = FALSE, invert = TRUE)

    for(col in 2:rowsAndCols){
      grobs$layout[grobs$layout$name == paste0("axis-b-", col), c("t", "b")] <-
        c(seq(11, 37, 2)[col-1],
          seq(11, 37, 2)[col-1])
      if(col < 15 & grp == "klockSpecies"){
        grobs <- gtable_filter(grobs,
                               paste0("axis-l-", rowsAndCols, "$"),
                               trim = FALSE, invert = TRUE)
      }
    }

    for(row in 2:rowsAndCols){
      grobs$layout[grobs$layout$name == paste0("axis-l-", row), c("l", "r")] <-
        c(seq(10, 40, 2)[row-1],
          seq(10, 40, 2)[row-1])
    }

    # If you want, move the axis
    # grobs$layout[grobs$layout$name == "axis-b-2", c("t", "b")] <- c(11, 11)
    # grobs$layout[grobs$layout$name == "axis-b-3", c("t", "b")] <- c(13, 13)
    # grobs$layout[grobs$layout$name == "axis-b-4", c("t", "b")] <- c(15, 15)
    # Draw the plot
    grid.newpage()
    grid.draw(grobs)

    halfContrastPlot <- ggpubr::as_ggplot(grobs)

    # targets::tar_load("combined_subspecies_plot")
    # insetMaps <- combined_subspecies_plot
    if(grp == "klockSpecies"){
      subMap <- insetMaps[[1]]
    } else {
      subMap <- insetMaps[[2]]
    }
    insetMap <- subMap +
      theme(legend.position = "none",
            panel.border = element_blank(),
            line = element_blank(),
            axis.line.y.left = element_blank(),
            axis.line.x.bottom = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    if(grp == "klockSpecies"){
    halfContrastPlotwMap <- halfContrastPlot +
      patchwork::inset_element(insetMap, 0.0, 0.08, 0.5, 0.45)
    } else {
    halfContrastPlotwMap <- halfContrastPlot +
      patchwork::inset_element(insetMap, 0.0, 0.08, 0.6, 0.55)
    }

    if(grp == "klockSpecies"){
      width = 430; height = 440
    } else if(grp == "martSpecies"){
      width = 300; height = 190
    }

    ggsave(halfContrastPlotwMap,
           filename = here("Figures", paste0(grp, "_HDCI_contrasts.png")),
           dpi = 300, width = width, height = height,
           units = "mm")
    ggsave(halfContrastPlotwMap,
           filename = here("Figures", paste0(grp, "_HDCI_contrasts.pdf")),
           width = width, height = height,
           units = "mm")

    plotList[[i]] <- constrastsHDCIPlot
  }
  return(plotList)
}
