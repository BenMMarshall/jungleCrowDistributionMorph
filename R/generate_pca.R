#' generate_pca
#'
#' @param crowData The crowData with all measurements.
#' @returns CrowData with PC 1 2 and 3 added as new columns.
#'
#' @export
generate_pca <- function(crowData){
  # crowData <- crow_measurements
  pcaData <- crowData %>%
    select("Bill.base.width", "Bill.width.at.skin.border", "Bill.base.length",
           "Width.at.nares","Height.at.nares","Nares.to.bill.tip",
           "Exposed.culmen","Tarsus.length",
           "ExCu.TaLe", "HaNa.TaLe", "ExCu.NtBT")

  pcaData <- apply(pcaData, 2, function(x){
    (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))})

  pcaResults <- PCA(pcaData, ncp = 5)

  write.csv(pcaResults$var$contrib,
            file = here("ModelOutput", "PCAcontrib.csv"), row.names = TRUE)

  png(filename = here("ModelOutput", "pca_ind.png"),
      width = 200, height = 140, units = "mm", res = 300)
  plot(pcaResults, choix = "ind", graph.type = "ggplot")
  dev.off()

  png(filename = here("ModelOutput", "pca_var_1_2.png"),
      width = 200, height = 140, units = "mm", res = 300)
  plot(pcaResults, axes = c(1, 2), choix = "var", graph.type = "ggplot")
  dev.off()

  png(filename = here("ModelOutput", "pca_var_2_3.png"),
      width = 200, height = 140, units = "mm", res = 300)
  plot(pcaResults, axes = c(2, 3), choix = "var", graph.type = "ggplot")
  dev.off()

  png(filename = here("ModelOutput", "pca_scree.png"),
      width = 200, height = 140, units = "mm", res = 300)
  eigenvalues <- pcaResults$eig
  barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues),
          main = "Variances",
          xlab = "Principal Components",
          ylab = "Percentage of variances",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(eigenvalues), eigenvalues[, 2],
        type="b", pch=19, col = "red")
  dev.off()


  pcaOUT <- as.data.frame(pcaResults$ind$coord)

  crowData_pca <- crowData %>%
    mutate(
      PC1 = pcaOUT$Dim.1,
      PC2 = pcaOUT$Dim.2,
      PC3 = pcaOUT$Dim.3)

  return(crowData_pca)

}
