# LEVENE FUNCTION FOR MULTIPLE GROUPS - PROJECT TAPIRUS

# LEVENE FUNCTION FOR MULTIPLE GROUPS - PROJECT TAPIRUS

levene.p <- function(dataset, firstCol){
  
  pant <- hembio.homog[hembio.homog$Biome=="Pantanal",] #Age was used for grouping: most restrictive comparison
  pant.m <- hembio.homog[hembio.homog$Biome=="Pantanal"&hembio.homog$Sex=="Male",] #Age was used for grouping: most restrictive comparison
  pant.f <- hembio.homog[hembio.homog$Biome=="Pantanal"&hembio.homog$Sex=="Female",]
  cerr.m <- hembio.homog[hembio.homog$Biome=="Cerrado"&hembio.homog$Sex=="Male",]
  cerr.f <- hembio.homog[hembio.homog$Biome=="Cerrado"&hembio.homog$Sex=="Female",]
  
  lv.test <- list()
  lv.pvalue <- numeric()
  for (i in firstCol:ncol(dataset)){
    if (sum(dataset[,i], na.rm=TRUE)>0){
      lv.test$dataset[[i]] <- leveneTest(dataset[,i], group = dataset$Age) #Biome was used as the grouping factor for the test
      lv.test$dataset[[i]] <- lv.test$dataset[[i]]$`Pr(>F)`[1]
      Variables <- colnames(dataset[,firstCol:ncol(dataset)])
      lv.pvalue[i] <- as.numeric(lv.test$dataset[[i]])
    } else
      lv.test$dataset[[i]] <- NA
  }
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