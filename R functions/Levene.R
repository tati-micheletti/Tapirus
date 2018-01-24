# LEVENE FUNCTION FOR MULTIPLE GROUPS - PROJECT TAPIRUS

levene.p <- function(dataset, firstCol){

  pant.m <- hembio.homog[hembio.homog$Biome=="Pantanal"&hembio.homog$Sex=="Male",] #Age was used for grouping: most restrictive comparison
  pant.f <- hembio.homog[hembio.homog$Biome=="Pantanal"&hembio.homog$Sex=="Female",]
  atl.m <- hembio.homog[hembio.homog$Biome=="Atlantic"&hembio.homog$Sex=="Male",]
  atl.f <- hembio.homog[hembio.homog$Biome=="Atlantic"&hembio.homog$Sex=="Female",]
  cerr.m <- hembio.homog[hembio.homog$Biome=="Cerrado"&hembio.homog$Sex=="Male",]
  cerr.f <- hembio.homog[hembio.homog$Biome=="Cerrado"&hembio.homog$Sex=="Female",]
  
lv.test <- list()
for (i in firstCol:ncol(dataset)){

  lv.test$dataset[[i]] <- leveneTest(dataset[,i], group = dataset$Age) #Biome was used as the grouping factor for the test
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