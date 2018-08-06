mergeLastTables <- function(grouping,
                            biome = biome, 
                            sex = sex,
                            compTABLE = compSEX,
                            testsTable = final.table){

  if(grouping == "SEX"){
    SEX.table <- lapply(biome, function(biome){
      TEST <- apply(compTABLE[[biome]], 1, function(rows){
        
        line <- which(testsTable[,VARIABLES]==rows["variable"])
        tst <- testsTable[line,TEST]
        
        TEST <- ifelse(rows["test"]=="T",
                       ifelse(tst=="PARAMETRIC","T-TEST","Mann-WhitneyU"),
                       NA)
        return(TEST)
      })
      return(TEST)
    })
    names(SEX.table) <- biome
    
    SEX <- lapply(biome, function(biome){
      statTest <- cbind(compTABLE[[biome]], statTest = SEX.table[[biome]])
      return(statTest)
    })
    
    names(SEX) <- biome

        
  }
  
  #----------------- BIOME ---------------
  
  if(grouping == "BIOME"){
    
    BIOME.table <- apply(compTABLE, 1, function(rows){
      
      line <- which(testsTable[,VARIABLES]==rows["variable"])
      tst <- testsTable[line,TEST]
      
      TEST <- ifelse(rows["test"]=="T",
                     ifelse(tst=="PARAMETRIC","T-TEST","Mann-WhitneyU"),
                     ifelse(rows["test"]=="ANOVA",
                            ifelse(tst=="PARAMETRIC","ANOVA","Kruskal-Wallis"),
                            NA))
      return(TEST)
    })
    
    BIOME <- cbind(compTABLE, statTest = BIOME.table)
    
  }
  
  if(grouping == "SEX"){return(SEX)}
  if(grouping == "BIOME"){return(BIOME)}
}