#' save_r2_values
#'
#' @param model
#' @param measure
#' @param group
#' @returns Pull and save the R2 values for a brms model
#'
#' @export
save_r2_values <- function(model, measure, group){

  name <- paste(measure, group, sep = "_")
  # model <- modOut

  r2OUT <- performance::r2_bayes(model)
  r2OUT

  r2df_1 <- data.frame(
    R2 = c(unlist(
      r2OUT[1]),
      unlist(r2OUT[2])
    ),
    SE = c(attr(r2OUT,"SE")$R2_Bayes,
           attr(r2OUT,"SE")$R2_Bayes_marginal),
    rbind(attr(r2OUT,"CI")$R2_Bayes,
          attr(r2OUT,"CI")$R2_Bayes_marginal),
    Component = c("conditional", "marginal")
  )

  write.csv(r2df_1, file = here::here("ModelOutput", paste0(name, "_brmsR2Results.csv")),
            row.names = FALSE)

  return(r2df_1)

}
