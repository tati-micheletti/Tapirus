checkComparison <- function(comparisonGroup, tableList = tableList, biome = biome, sex = sex){
  # ~ SEX ~
  browser()
if (comparisonGroup == "SEX"){  
  compSEX <- lapply(biome, function(biome){
    tests1 <- lapply(sex, function(sex){
      listSex <- data.table(tableList[[biome]][[sex]][,c("Biome","Sex","variable","count"), with = FALSE])
      return(listSex)
      
    })
    names(tests1) <- sex
    tests2 <- tests1
    lapply(tests2, function(i) setkey(i, variable))
    biomeTable <- Reduce(function(...) merge(..., all = FALSE), tests2)
    biomeTable$test <- apply(biomeTable, 1, function(x){
      ifelse(as.numeric(x["count.x"])>2&as.numeric(x["count.y"])>2,"T",NA)
    })
    return(biomeTable)
  })
  
  names(compSEX) <- biome
  biomeTable <<- compSEX
  compSEX <- lapply(compSEX, function(Biomes){
    Biomes <- Biomes[,c("variable","test")]
    return(Biomes)
  })
}
  #########################################################################
  
  
  # ~ BIOME ~
if (comparisonGroup == "BIOME"){

  lapply(biomeTable, function(i) setkey(i, variable))
  
  compBIOME <-  Reduce(function(...) merge(..., all = FALSE), biomeTable)
  compBIOME <- compBIOME[,c("variable","count.x.x","count.y.x","count.x.y","count.y.y","count.x","count.y"), with = FALSE]
  compBIOME$Atlantic <- compBIOME[,count.x.x]+compBIOME[,count.y.x]
  compBIOME$Pantanal <- compBIOME[,count.x.y]+compBIOME[,count.y.y]
  compBIOME$Cerrado <- compBIOME[,count.x]+compBIOME[,count.y]
  compBIOME <- compBIOME[,c("variable", biome), with = FALSE]
  
  compBIOME$whichGroups <- apply(compBIOME, 1, function(x){
    
    whichG <- attributes(which(!is.na(x[2:length(x)])&
                                 as.numeric(x[2:length(x)])>2))$names %>%
      paste(collapse = ":")
  })
  
  compBIOME$manyGroups <- apply(compBIOME, 1, function(x){
    subX <- sum(!is.na(x[2:(length(x)-1)])&as.numeric(x[2:(length(x)-1)])>2)
    return(subX)})
  compBIOME$test <- ifelse(compBIOME$manyGroups<2,NA,ifelse(compBIOME$manyGroups==2,"T","ANOVA"))
  compBIOME <- compBIOME[,c("variable","whichGroups","test"), with = FALSE]
}

 if (comparisonGroup == "SEX"){return(compSEX)}
 if (comparisonGroup == "BIOME"){return(compBIOME)}
}