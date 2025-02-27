#' plot_contrastsSex_hdci
#'
#' @param plot_contrastsSex_hdci
#' @returns A simplified plots just showing the HDCI of the pred post dists
#'
#' @export
plot_contrastsSex_hdci <- function(contrastsHDCI, palette){

  # targets::tar_load("contrasts_hdci_combined")

  sexHDCIs <- do.call(rbind, lapply(contrastsHDCI,
                                    function(x){x[["hdiContrasts_sex"]]}))

  # paletteList <- generate_palette()
  paletteList <- palette

  contrasts_hdci <- sexHDCIs %>%
    mutate(plus85diff = case_when(
      probOver0 > 0.85 ~ "85%+ chance larger",
      probUnder0 > 0.85 ~ "85%+ chance smaller",
      TRUE ~ "Little difference"))

  contrasts_hdci_filtered <- contrasts_hdci %>%
    # filter(group == grp) %>%
    mutate(measure = gsub("\\.", " ", measure)) %>%
    filter(comparison == "Female-Male") %>%
    mutate(measure = case_when(
      measure == "ExCu TaLe" ~ "Exposed culmen : Tarsus length",
      measure == "HaNa TaLe" ~ "Height at nares : Tarsus length",
      measure == "NtBT ExCu" ~ "Nares to bill tip : Exposed culmen",
      measure == "ExCuNtBTxHaNaTaLe" ~ "Proximal bill surface area",
      TRUE ~ gsub("\\.", " ", measure)
    ),
    group = case_when(
      group == "martSpecies" ~ "Martens et al subspecies",
      group == "klockSpecies" ~ "Klockenhoff subspecies"
    ))

  (constrastsHDCIPlot <-
      contrasts_hdci_filtered %>%
      ggplot() +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey85") +
      # geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      geom_errorbarh(aes(xmin = ymin, xmax = ymax, y = measure,
                         colour = plus85diff), height = 0.2) +
      geom_point(aes(x = y, y = measure,
                     colour = plus85diff), size = 0.75) +
      scale_x_continuous(breaks = seq(-20,20,20),
                         minor_breaks =seq(-20,20,10)) +
      scale_colour_manual(values = paletteList$larger_smaller_palette) +
      facet_wrap(vars(group)) +
      labs(y = "Measure", x = "Contrast",
           colour = "Highlighted differences") +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            # axis.line.y.left = element_line(linewidth = 0.85),
            legend.title = element_text(face = 2),
            axis.text.y = element_text(face = 1),
            axis.ticks.y = element_blank(),
            axis.title = element_text(face = 2),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            strip.text.y.left = element_text(angle = 0, vjust = 1),
            strip.text.y.right = element_text(angle = 0, vjust = 1)
      )
  )

  # if(grp == "klockSpecies"){
  #   width = 430; height = 420
  # } else if(grp == "martSpecies"){
  #   width = 280; height = 160
  # }
  width = 280; height = 160

  ggsave(constrastsHDCIPlot,
         filename = here("Figures", "_HDCI_sex_contrasts.png"),
         dpi = 300, width = width, height = height,
         units = "mm")
  ggsave(constrastsHDCIPlot,
         filename = here("Figures", "_HDCI_sex_contrasts.pdf"),
         width = width, height = height,
         units = "mm")

  return(constrastsHDCIPlot)
}
