#' plot_prior_likli_post
#'
#' @param compData
#' @param model
#' @param measure
#' @param group
#' @returns A brms plots showing groups predictions versus prior
#'
#' @export
plot_prior_likli_post <- function(compData, model, measure, group){

  # compData <- crow_measurements_subspecies
  # model <- bayesian_comparison_klockSpecies_Exposed.culmen
  # measure = "Exposed.culmen"
  # group = "klockSpecies"

  name <- paste(measure, group, sep = "_")
  # compData <- simData
  # model <- modOut2
  compData <- compData %>%
    rename("measure" = matches(measure),
           "group" = matches(group))

  # priorSummary <- prior_summary(model) %>%
  #   filter(!dpar == "sigma", class == "b") %>%
  #   mutate(variable = ifelse(
  #     coef == "", "b", paste0(class, "_", coef)
  #   ))
  #
  # priorValues <- brms::prior_draws(model, variable = priorSummary$variable)
  #
  # priorLong <- priorValues %>%
  #   tidyr::pivot_longer(everything(), names_to = "variable") %>%
  #   mutate(group = ifelse(str_detect(variable, "group"),
  #                         str_extract(variable, "(?<=b_group).*"),
  #                         variable)) %>%
  #   filter(!group == "b") %>%
  #   select(-variable)

  # priorMeans <- data.frame(
  #   group = c("G0", "G1", "G2", "G3"),
  #   mu = rep(20, 4),
  #   sd = rep(10, 4)
  # )
  hdciMeans <- model %>%
    spread_draws(`^b_group.*`, regex = TRUE) %>%
    select(-.chain, -.iteration, -.draw) %>%
    tidyr::pivot_longer(everything(), names_to = "variable") %>%
    mutate(group = str_extract(variable, "(?<=b_group).*")) %>%
    mutate(group = sub(":SexFemale|:SexMale", "", group)) %>%
    mutate(group = sub("x", " x ", group)) %>%
    group_by(group) %>%
    median_hdci(value)

  hdciSigmas <- model %>%
    spread_draws(`^b_sigma.*`, regex = TRUE) %>%
    select(-.chain, -.iteration, -.draw) %>%
    tidyr::pivot_longer(everything(), names_to = "variable") %>%
    mutate(group = str_extract(variable, "(?<=b_sigma_group).*")) %>%
    mutate(group = sub(":SexFemale|:SexMale", "", group)) %>%
    mutate(group = sub("x", " x ", group)) %>%
    group_by(group) %>%
    median_hdci(value)

  # newdata <- compData %>%
  #   select(-matches(measure)) %>%
  #   filter(!is.na(group))

  subspecies <- compData %>%
    filter(!is.na(group)) %>%
    pull(group) %>%
    unique()
  sex <- compData %>%
    filter(!is.na(Sex)) %>%
    pull(Sex) %>%
    unique()

  newdata <- data.frame(
    "group" = rep(subspecies, each = 100)
  )
  newdata$Sex <- rep(sex, length.out = nrow(newdata))

  longPredOut <- newdata %>%
    add_predicted_draws(object = model)

  (prioLikPostPlot <- longPredOut %>%
      ggplot() +
      # geom_density_ridges(data = priorLong,
      #                     aes(x = value, y = group, height = ..density..,
      #                         colour = "Prior"),
      #                     fill = NA,
      #                     size = 1.5, trim = TRUE, stat = "density",
      #                     rel_min_height = 0.01, scale = 0.5) +
      geom_density_ridges(data = compData,
                          aes(x = measure, y = group, #height = ..density..,
                              colour = "Observed"),
                          fill = NA,
                          size = 1.5, #trim = TRUE, stat = "density",
                          rel_min_height = 0.01, scale = 0.75,
                          jittered_points = TRUE, point_shape = 16,
                          alpha = 0.85, point_size = 1,
                          position = position_points_jitter(width = 0, height = 0.05,
                                                            yoffset = -0.1)) +
      geom_density_ridges(aes(x = .prediction, y = group, height = ..density..,
                              colour = "Posterior"),
                          fill = NA,
                          size = 1.5, trim = TRUE, stat = "density",
                          rel_min_height = 0.01, scale = 0.75) +
      ## POSTERIOR MU ESTIMATES
      geom_point(data = hdciMeans, aes(x = value, y = group, colour = "Posterior"),
                 position = position_nudge(y = -0.4), size = 2,
                 shape = 18) +
      geom_errorbarh(data = hdciMeans, aes(xmin = .lower, xmax = .upper,
                                           y = group, colour = "Posterior"),
                     height = 0.05,
                     position = position_nudge(y = -0.4)) +
      geom_text(data = hdciMeans, aes(x = .upper, y = group, colour = "Posterior",
                                      label = paste0("est. mu = ",
                                                     round(value, digits = 2))),
                position = position_nudge(y = -0.4, x = 1),
                hjust = 0, vjust = 0.5, size = 3, fontface = 4) +
      ## POSTERIOR SIGMA ESTIMATES
      geom_text(data = hdciSigmas, aes(x = 1, y = group, colour = "Posterior",
                                       label = paste0("est. sigma = ",
                                                      round(value, digits = 2))),
                position = position_nudge(y = -0.4),
                hjust = 0, vjust = 0.5, size = 3, fontface = 4) +

      scale_x_continuous(limits = c(0, max(compData$measure, na.rm = TRUE)),
                         expand = expansion(0.1,0.1)) +
      scale_colour_manual(values = c("Observed" = "#2EA8C9",
                                     "Prior" = "#E34BC2",
                                     "Posterior" = "#3E7787",
                                     "Simulated" = "#BDC98F")) +
      labs(colour = "", y = group, x = measure) +
      theme_bw() +
      theme(line = element_line(colour = "grey15"),
            text = element_text(colour = "grey15"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(linetype = 2, linewidth = 0.5),
            axis.line.x.bottom = element_line(linewidth = 0.85),
            axis.line.y.left = element_line(linewidth = 0.85)
      )
  )

  ggsave(prioLikPostPlot,
         filename = here("ModelOutput", paste0(name, "_priLikPost.png")),
         dpi = 300, width = 210, height = 140,
         units = "mm")
  ggsave(prioLikPostPlot,
         filename = here("ModelOutput", paste0(name, "_priLikPost.pdf")),
         width = 210, height = 140,
         units = "mm")

  return(prioLikPostPlot)
}
