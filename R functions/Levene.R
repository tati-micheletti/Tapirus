# LEVENE FUNCTION FOR MULTIPLE GROUPS - PROJECT TAPIRUS

levene.p <- function(dataset, firstCol, factor){

  # browser()
  
  lv.test <- numeric()
  all.fac <- numeric()
  for (i in firstCol:ncol(dataset)){
      vari.test <- data.table(FACTOR=dataset[,factor],VALUE=dataset[,i])
      sum.fac <- vari.test[,.(VALUE.Sum=sum(!is.na(VALUE))),by=FACTOR]   
  for (j in 1:length(levels(vari.test$FACTOR))){
    all.fac[j] <- sum.fac[j]$VALUE.Sum
  }
      all.fac <- unlist(all.fac)
    if (all(all.fac>0)){
      df <- data.frame(dataset[,i], dataset[,factor])
      lv.test[i] <- leveneTest(df[,1], group=df[,2])$`Pr(>F)`[1]
    } else
      lv.test[i] <- NA
    }
  
  Variables <- colnames(dataset[,firstCol:ncol(dataset)])
  lv.results <- data.frame(Variables, Levene_p = lv.test[firstCol:length(lv.test)])
  lv.results$Homogeneity <- NA
  lv.results <- as.data.table(lv.results)

  for (i in 1:nrow(lv.results)){
  if (is.na(lv.results$Levene_p[i])){
    lv.results$Homogeneity[i] <- "NON-HOMOGENEOUS" 
  } 
else
    if (lv.results$Levene_p[i]>0.05){
      lv.results$Homogeneity[i] <- "HOMOGENEOUS"      
      }
      else
     lv.results$Homogeneity[i] <- "NON-HOMOGENEOUS"
  }
  
  dataset.levene <- lv.results
  
  return(dataset.levene)
}