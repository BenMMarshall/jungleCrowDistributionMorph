#' diagnose_brms
#'
#' @param model
#' @param measure
#' @param group
#' @returns A brms plots to help see model fit
#'
#' @export
diagnose_brms <- function(model, measure, group){

  vars <- get_variables(model)
  varsToPlot <- vars[stringr::str_detect(vars, "b_")]
  name <- paste(measure, group, sep = "_")

  traceplot <- mcmc_trace(model, pars = varsToPlot) +
    theme_bw() +
    theme(line = element_line(colour = "grey15"),
          text = element_text(colour = "grey15"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = 0.85),
          axis.line.y.left = element_line(linewidth = 0.85),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, vjust = 0, hjust = 0),
          strip.text.y.left = element_text(angle = 0, vjust = 1)
    )
  ggsave(traceplot,
         filename = here("ModelOutput", paste0(name, "_traceplot.png")),
         dpi = 300, width = 320, height = 140,
         units = "mm")

  acfplot <- mcmc_acf(model, pars = varsToPlot) +
    theme_bw() +
    theme(line = element_line(colour = "grey15"),
          text = element_text(colour = "grey15"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = 0.85),
          axis.line.y.left = element_line(linewidth = 0.85),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, vjust = 0, hjust = 0),
          strip.text.y.left = element_text(angle = 0, vjust = 1)
    )
  ggsave(acfplot,
         filename = here("ModelOutput", paste0(name, "_acfplot.png")),
         dpi = 300, width = 320, height = 140,
         units = "mm")

  ppplot <- pp_check(model, ndraw = 100) +
    theme_bw() +
    theme(line = element_line(colour = "grey15"),
          text = element_text(colour = "grey15"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(linewidth = 0.85),
          axis.line.y.left = element_line(linewidth = 0.85)
    )
  ggsave(ppplot,
         filename = here("ModelOutput", paste0(name, "_pPplot.png")),
         dpi = 300, width = 210, height = 140,
         units = "mm")

}
