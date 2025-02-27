# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(dplyr)

pcks <- c("tibble",
          "here",
          "dplyr",
          "tidyr",
          "stringr",
          "zen4R",
          "geodata",
          "ggplot2",
          "ggridges",
          "ggtext",
          "sf",
          "ggpubr",
          "grid",
          "patchwork",
          "bayesplot",
          "tidybayes",
          "brms",
          "ggpattern",
          "gtable",
          "spatstat",
          "terra",
          "tidyterra",
          "performance")

# Set target options:
tar_option_set(
  packages = pcks,
  controller = crew::crew_controller_local(workers = 3, seconds_idle = 60),
  # error = "continue",
  format = "qs" # Optionally set the default storage format. qs is fast.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

dir.create(here::here("Data"), showWarnings = FALSE)
dir.create(here::here("Figures"), showWarnings = FALSE)
dir.create(here::here("ModelOutput"), showWarnings = FALSE)

grouping <- c("martSpecies", "klockSpecies")

measures <- c("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
              "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
              "Exposed.culmen","Tarsus.length", #"PC1", "PC2", "PC3",
              "ExCu.TaLe", "HaNa.TaLe", "NtBT.ExCu", "ExCuNtBTxHaNaTaLe")

optionsMeasures <- data.frame(
  measures = measures
)

optionsGroupMeasure <- tidyr::expand_grid(grouping, measures)

optionsGroupMeasurePriors <- optionsGroupMeasure %>%
  mutate(priors = case_when(
    measures == "Exposed.culmen" ~ "normal(62, 20)",
    measures == "Nares.to.bill.tip" ~ "normal(42, 20)",
    measures == "Width.at.nares" ~ "normal(17, 20)",
    measures == "Height.at.nares" ~ "normal(22, 20)",
    measures == "Tarsus.length" ~ "normal(57, 20)",
    TRUE ~ "uni"
  ))

# Replace the target list below with your own:
corePipeline <- list(
  tar_target(
    name = crow_files,
    command = download_crow_data(),
    priority = 1
  ),
  tar_target(
    name = climate_files_locations,
    command = download_climate_data(),
    priority = 1
  ),
  tar_target(
    name = crow_measurements,
    command = read_crow_data(crow_files),
    priority = 0.7
    #pull in measurement data and removes JUVENILES
  ),
  tar_target(
    name = subspecies_list,
    command = read_subspecies_polygons(),
    priority = 0.7
    #pull in measurement data and removes JUVENILES
  ),
  tar_target(
    name = measurement_summary,
    command = generate_summary(crow_measurements),
    priority = 0.7
    #creates summary statistics for museum data to be included at the beginning
    #of the results section, e.g. number of crows, balance of male/female, how
    #many of each type of measurement, etc.
  ),
  tar_target(
    name = overall_summary_plot,
    command = generate_summary_plots(crow_measurements,
                                     palette = generate_palette()),
    priority = 0.7
    #creates summary plot for all measures
  ),
  # tar_target(
  #   name = crow_measurements_pca,
  #   command = generate_pca(crow_measurements)
  #   #run a pca on the data
  # ),
  # tar_target(
  #   name = visualise_pca,
  #   command = generate_pca_plots(measurement_pca)
  #   #plot the pca (loading, scree)
  # ),
  tar_target(
    name = crow_measurements_subspecies,
    command = extract_subspecies_assignments(crow_measurements, subspecies_list),
    priority = 0.7
    # get subspecies assignments from the martens and klockenhoff polygons
  ),
  tar_target(
    name = combined_subspecies_plot,
    command = generate_subspecies_maps(crow_measurements_subspecies,
                                       palette = generate_palette(),
                                       subspecies_list),
    priority = 0.7
    # make subspecies maps
  ),
  tar_target(
    name = measurements_map_plot,
    command = generate_measurements_maps(crow_measurements_subspecies,
                                         palette = generate_palette(),
                                         subspecies_list),
    priority = 0.7
    # make measurements maps
  ),
  tar_target(
    name = idw_map_plot,
    command = generate_idw_maps(crow_measurements_subspecies,
                                palette = generate_palette()),
    priority = 0.7
    # make IDW heatmaps maps
  ),
  tar_map(
    unlist = TRUE,
    values = optionsGroupMeasure,
    tar_target(
      name = bayesian_comparison,
      command = compare_bayes(compData = crow_measurements_subspecies,
                              measure = measures,
                              group = grouping,
                              priorCentre = optionsGroupMeasurePriors),
      priority = 0.6
    ),
    tar_target(
      name = bayesian_diagnostics,
      command = diagnose_brms(model = bayesian_comparison,
                              measure = measures,
                              group = grouping),
      priority = 0.6
    ),
    tar_target(
      name = prior_likli_post_plot,
      command = plot_prior_likli_post(compData = crow_measurements_subspecies,
                                      model = bayesian_comparison,
                                      measure = measures,
                                      group = grouping),
      priority = 0.6
    ),
    tar_target(
      name = contrasts_data,
      command = calc_contrasts(compData = crow_measurements_subspecies,
                               model = bayesian_comparison,
                               measure = measures,
                               group = grouping),
      priority = 0.6
    ),
    # tar_target(
    #   name = contrasts_hdci,
    #   command = calc_contrasts_hdci(contrastsFull = contrasts_data,
    #                                 measure = measures,
    #                                 group = grouping)
    # ),
    # tar_target(
    #   name = contrasts_plot,
    #   command = plot_contrasts(contrastsList = contrasts_data,
    #                            contrastsHDCI = contrasts_hdci,
    #                            measure = measures,
    #                            group = grouping)
    # ),
    tar_target(
      name = r2_values,
      command = save_r2_values(model = bayesian_comparison,
                               measure = measures,
                               group = grouping),
      priority = 0.6
    ),
    tar_target(
      name = beta_values,
      command = save_betas_values(model = bayesian_comparison,
                                  measure = measures,
                                  group = grouping),
      priority = 0.6
    )
  ),
  # tar_target(
  #   name = clim_map_list,
  #   command = generate_clim_maps(crow_measurements_subspecies,
  #                               palette = generate_palette()),
  #   priority = 0.5
  #   # make climate comparison heatmaps maps
  # )
  tar_map(
    unlist = TRUE,
    values = optionsMeasures,
    tar_target(
      name = clim_map_list,
      command = generate_clim_brms_maps(crow_measurements_subspecies,
                                        palette = generate_palette(),
                                        climate_files_locations,
                                        measure = measures),
      priority = 0.5
      # make climate comparison heatmaps maps
    )
  )
)

contrastsCombined <- list(
  tar_combine(
    contrasts_hdci_combined,
    corePipeline[[11]][grep("contrasts_data", names(corePipeline[[11]]))],
    command = list(!!!.x),
    priority = 0.5
  ),
  tar_target(
    name = contrasts_hdciSubspecies_plots,
    command = plot_contrasts_hdci(contrastsHDCI = contrasts_hdci_combined,
                                  palette = generate_palette(),
                                  insetMaps = combined_subspecies_plot),
    priority = 0.5
  ),
  tar_target(
    name = contrasts_hdciIntraSex_plots,
    command = plot_contrastsIntraSex_hdci(contrastsHDCI = contrasts_hdci_combined,
                                          palette = generate_palette()),
    priority = 0.5
  ),
  tar_target(
    name = contrasts_hdciSex_plots,
    command = plot_contrastsSex_hdci(contrastsHDCI = contrasts_hdci_combined,
                                     palette = generate_palette()),
    priority = 0.5
  ),
  tar_target(
    name = contrasts_hdciMap_plots,
    command = plot_contrastsDiff_map(contrastsHDCI = contrasts_hdci_combined,
                                     palette = generate_palette(), subspecies_list),
    priority = 0.5
  )
)

resultsOutput <- list(
  tar_target(
    name = results_output,
    command = render_rmd(measurement_summary,
                         overall_summary_plot,
                         measurements_map_plot,
                         combined_subspecies_plot,
                         idw_map_plot,
                         contrasts_hdciSubspecies_plots,
                         contrasts_hdciIntraSex_plots,
                         contrasts_hdciSex_plots,
                         clim_map_list
    ),
    priority = 0.001
    # creates pdf/word/html output of the results
  )
)

list(corePipeline,
     contrastsCombined,
     resultsOutput
)
