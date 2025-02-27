#' generate_palette
#'
#' @returns A list of palettes
#'
#' @export
generate_palette <- function(){

  # #Picking the colour palette for the transects
  hab_palette <- c('#003a00', '#236a20', '#4e9d48', '#83d278',
                   '#e8b946', '#af8614', '#795600', '#472900')

  # "#73683B"
  # "#583E23"
  # "#519872"
  # "#6B654B"
  # "#895B1E"
  # "#66717E"

  klockSub <- tribble(
    ~Subspecies, ~fill,
    "intermedius", "#003a00",
    "culminatus", "#236a20",
    # "macrorhynchos", "#4e9d48",
    "macrorhynchos", "#48799d",
    "levaillantii", "#83d278",
    "cul. x lev.", "#236a20",
    "tibetosinensis", "#e8b946",
    "lev. x mac.", "#83d278",
    "tib. x lev.", "#e8b946",
    "connectens", "#583E23",
    "osai", "#795600",
    "hainanus", "#472900",
    # "philippinus", "#66717E",
    "philippinus", "#5a596e",
    "japonensis", "#af8614",
    "mandshuricus", "#583E23",
    "colonorum", "#519872"
  )
  klock_palette <- klockSub$fill
  names(klock_palette) <- klockSub$Subspecies

  martSub <- tribble(
    ~Subspecies, ~fill,
    "japonensis","#af8614",
    # "macrorhynchos", "#4e9d48",
    "macrorhynchos", "#48799d",
    "levaillantii", "#83d278",
    "philippinus", "#5a596e",
    "hybrid", "#48799d"
    )
  mart_palette <- martSub$fill
  names(mart_palette) <- martSub$Subspecies

  sex_palette <- c("#236a20", "#e8b946", "grey65")
  names(sex_palette) <- c("Male", "Female", "Unknown")

  larger_smaller_palette <- c("#e8b946", "#83d278", "grey45")
  names(larger_smaller_palette) <- c("85%+ chance larger",
    "85%+ chance smaller", "Little difference")

  paletteList <- list("hab_palette" = hab_palette,
                      "klock_palette" = klock_palette,
                      "mart_palette" = mart_palette,
                      "sex_palette" = sex_palette,
                      "larger_smaller_palette" = larger_smaller_palette)

  return(paletteList)
}
