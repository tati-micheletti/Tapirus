mergeLastTables <- function(grouping,
                            biome = biome, 
                            sex = sex,
                            compTABLE,
                            testsTable = final.table){

  if(grouping == "SEX"){

    SEX.table <- lapply(biome, function(biome){
      TEST <- apply(compSEX[[biome]], 1, function(rows){
        
        line <- which(final.table[,VARIABLES]==rows["variable"])
        tst <- final.table[line,TEST]
        
        TEST <- ifelse(rows["test"]=="T",
                       ifelse(tst=="PARAMETRIC","T-TEST","Mann-WhitneyU"),
                       NA)
        return(TEST)
      })
      return(TEST)
    })
    names(SEX.table) <- biome
    
    SEX <- lapply(biome, function(biome){
      statTest <- cbind(compSEX[[biome]], statTest = SEX.table[[biome]])
      return(statTest)
    })
    
    names(SEX) <- biome

        
  }
  
  #----------------- BIOME ---------------
  
  if(grouping == "BIOME"){
    
    BIOME.table <- apply(compBIOME, 1, function(rows){
      
      line <- which(final.table[,VARIABLES]==rows["variable"])
      tst <- final.table[line,TEST]
      
      TEST <- ifelse(rows["test"]=="T",
                     ifelse(tst=="PARAMETRIC","T-TEST","Mann-WhitneyU"),
                     ifelse(rows["test"]=="ANOVA",
                            ifelse(tst=="PARAMETRIC","ANOVA","Kruskal-Wallis"),
                            NA))
      return(TEST)
    })
    
    BIOME <- cbind(compBIOME, statTest = BIOME.table)
    
  }
  
  if(grouping == "SEX"){return(SEX)}
  if(grouping == "BIOME"){return(BIOME)}
}