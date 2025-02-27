#' generate_summary_plots
#'
#' @param crowData The crowData with all measurements.
#' @returns A plot showing measures distributions, mean ±SD.
#'
#' @export
generate_summary_plots <- function(crowData,
                                   palette){

  # #Picking the colour palette for the transects
  # hab_palette <- c('#003a00', '#236a20', '#4e9d48', '#83d278', '#e8b946',
  #                  '#af8614', '#795600', '#472900')
  # sex_palette <- c("#236a20", "#e8b946", "grey65")
  # names(sex_palette) <- c("Male", "Female", "Unknown")
  # targets::tar_source()
  # palette <- generate_palette()

  sex_palette <- palette$sex_palette

  # number of measures
  longCrowMeasures <- crowData %>%
    select("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
           "Exposed.culmen","Tarsus.length","Sex",
           "ExCu.TaLe", "HaNa.TaLe", "NtBT.ExCu", "ExCuNtBTxHaNaTaLe") %>%
    pivot_longer(where(is.numeric), names_to = "measure") %>%
    mutate(measure = case_when(
      measure == "ExCu.TaLe" ~ "Exposed culmen : Tarsus length",
      measure == "HaNa.TaLe" ~ "Height at nares : Tarsus length",
      measure == "NtBT.ExCu" ~ "Nares to bill tip : Exposed culmen",
      measure == "ExCuNtBTxHaNaTaLe" ~ "Proximal bill surface area",
      TRUE ~ gsub("\\.", " ", measure)
    )) %>%
    mutate(Sex = ifelse(is.na(Sex), "Unknown", Sex)) %>%
    mutate(Sex = factor(Sex, levels = rev(c("Male", "Female", "Unknown"))))

  meanLabels <- longCrowMeasures %>%
    group_by(measure) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n = sum(!is.na(value))) %>%
    mutate(label = paste0(round(mean, digits = 1), " ±",
                          round(sd, digits = 2)),
           label2 = paste0("n = ", n))

  summaryPlot <- longCrowMeasures %>%
    group_by(measure) %>%
    mutate(mean = mean(value, na.rm = TRUE)) %>%
    arrange(desc(mean)) %>%
    ungroup() %>%
    mutate(measure = factor(measure,
                            levels = unique(measure))) %>%
    ggplot() +
    geom_density_ridges(aes(x = value, y = measure, fill = Sex, colour = Sex),
                        size = 0.1, #trim = TRUE, stat = "density",
                        rel_min_height = 0.01, scale = 0.75,
                        jittered_points = TRUE, point_shape = 16,
                        alpha = 0.65, point_size = 1,
                        position = position_points_jitter(width = 0, height = 0.05,
                                                          yoffset = -0.1)) +
    geom_point(data = meanLabels, aes(x = mean, y = measure),
               size = 1.75, colour = "grey15", position = position_nudge(y = -0.1)) +
    geom_text(data = meanLabels, aes(x = mean, y = measure, label = label),
              size = 2.5, colour = "grey15", position = position_nudge(y = -0.2),
              hjust = 0.5, vjust = 1, fontface = 3) +
    geom_text(data = meanLabels, aes(x = Inf, y = measure, label = label2),
              size = 2.5, colour = "grey15", position = position_nudge(y = 0.05),
              hjust = 1, vjust = 0, fontface = 3) +
    scale_colour_manual(values = sex_palette) +
    scale_fill_manual(values = sex_palette) +
    labs(colour = "Sex", fill = "Sex", y = "Measure", x = "Value (mm)") +
    theme_bw() +
    theme(line = element_line(colour = "grey15"),
          text = element_text(colour = "grey15"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
          axis.line.x.bottom = element_line(linewidth = 0.85),
          axis.line.y.left = element_line(linewidth = 0.85)
    )

  ggsave(summaryPlot, file = here("Figures", "OverallSummaryPlot.png"),
         width = 180, height = 120, units = "mm")
  ggsave(summaryPlot, file = here("Figures", "OverallSummaryPlot.pdf"),
         width = 180, height = 120, units = "mm")

  return(summaryPlot)

}
