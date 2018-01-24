# LEVENE FUNCTION FOR MULTIPLE GROUPS - PROJECT TAPIRUS

levene2.p <- function(dataset, firstCol){
  
  atl <- hembio.homog[hembio.homog$Biome=="Atlantic",]

  lv.test <- list()
  for (i in firstCol:ncol(dataset)){
    
    lv.test$dataset[[i]] <- leveneTest(dataset[,i], group = dataset$Sex) #Biome was used as the grouping factor for the test
    lv.test$dataset[[i]] <- lv.test$dataset[[i]]$`Pr(>F)`[1]
  }
  
  Variables <- colnames(dataset[,firstCol:ncol(dataset)])
  lv.pvalue <- as.numeric(unlist(lv.test$dataset))
  lv.results <- data.frame(Variables, lv.pvalue)
  lv.results$Homogeneity <- NA
  lv.results <- as.data.table(lv.results)
  for (i in 1:nrow(lv.results)){
    if (lv.results$lv.pvalue[i]>0.05){
      lv.results$Homogeneity[i] <- "HOMOGENEOUS" }
  }
  
  dataset.levene <- lv.results
  
  return(dataset.levene)
}
