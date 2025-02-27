#' plot_contrasts
#'
#' @param compData
#' @param model
#' @param measure
#' @param group
#' @returns A brms plots showing contrasts between groups
#'
#' @export
plot_contrasts <- function(contrastsList, contrastsHDCI, measure, group){

  name <- paste(measure, group, sep = "_")

  contrastsFull <- contrastsList
  hdiContrasts <- contrastsHDCI

  constrastsPlot <- contrastsFull %>%
      ggplot() +
      geom_density_ridges_gradient(aes(x = contrast, y = 0)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      ## 0 differences
      geom_text(data = hdiContrasts, aes(x = 0, y = Inf, label = labelHigh),
                fontface = 3, size = 3, vjust = 1, hjust = 0) +
      geom_text(data = hdiContrasts, aes(x = 0, y = Inf, label = labelLow),
                fontface = 3, size = 3, vjust = 1, hjust = 1) +
      ## HDCI
      geom_point(data = hdiContrasts, aes(x = y, y = -0.01), size = 2,
                 shape = 18) +
      geom_errorbarh(data = hdiContrasts, aes(xmin = ymin, xmax = ymax,
                                              y = -0.01),
                     height = 0.01) +
      geom_text(data = hdiContrasts, aes(x = y, y = -0.018,
                                         label = round(y, digits = 2)),
                hjust = 0.5, vjust = 1, size = 3, fontface = 4) +

      facet_grid(rows = vars(gbase), cols = vars(gcomp), scales = "free_x",
                 switch = "y") +
      scale_x_continuous(limits = c(-50, 50), breaks = seq(-40, 40, 20)) +
      scale_y_continuous(limits = c(-0.05, NA)) +
      labs(y = "", x = paste0("Contrast (", measure, " ~ ", group, ")")) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line.x.bottom = element_line(linewidth = 0.5),
            # axis.line.y.left = element_line(linewidth = 0.85),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, vjust = 0, hjust = 0),
            strip.text.y.left = element_text(angle = 0, vjust = 1)
      )

  if(group == "Sex"){
    width = 120; height = 120
  } else if(group == "klockSpecies"){
    width = 420; height = 420
  } else if(group == "martSpecies"){
    width = 360; height = 360
  }

  ggsave(constrastsPlot,
         filename = here("ModelOutput", paste0(name, "_contrasts.png")),
         dpi = 300, width = width, height = height,
         units = "mm")
  ggsave(constrastsPlot,
         filename = here("ModelOutput", paste0(name, "_contrasts.pdf")),
         width = width, height = height,
         units = "mm")

  return(constrastsPlot)

}
