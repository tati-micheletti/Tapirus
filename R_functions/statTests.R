
statTests <- function(listTests, 
                      testsPerVariable = final.table, 
                      originalDT = hembio, 
                      logDT = hembio.log, 
                      biome = biome, 
                      sex = sex,
                      exclude = NA){
suppressWarnings({

  depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)

  # ========= FOR SEX ==========
  if (depth(listTests)==2){
  sexStats <- lapply(biome, function(biome){
      TEST <- apply(listTests[[biome]], 1, function(rows){

          # Extracting information for the variable to make the analysis
          line <- which(testsPerVariable[,VARIABLES]==rows["variable"])
          tst <- testsPerVariable[line,TEST]
          trans <- testsPerVariable[line,TRANSFORMATION]
          nor <-  testsPerVariable[line,NORMALITY]
          
          namesRows <- names(rows)
          rows <- data.table(matrix(rows, nrow=1))
          names(rows) <- namesRows
          
          statTest <- rows[,statTest]
          
          # If statTest is not NA, execute statistics
          # Composing the dataset
          invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                           dataToUse <- data.table(cbind(SEX=originalDT$sex, originalDT[rows[,variable]])),
                           dataToUse <- data.table(cbind(SEX=logDT$sex, logDT[rows[,variable]]))))
          dataToUse <- dataToUse[as.vector(!is.na(dataToUse[,2])),]
          
          invisible(ifelse(is.na(statTest), testResult <- NA,
                           ifelse(statTest=="T-TEST",
                                  testResult <- t.test(dataToUse[,get(rows[,variable])]~dataToUse[,SEX]),
                                  ifelse(statTest=="Mann-WhitneyU",
                                         testResult <- wilcox.test(dataToUse[,get(rows[,variable])]~dataToUse[,SEX]),
                                         ifelse(statTest=="Kruskal-Wallis",
                                                testResult <- kruskal.test(dataToUse[,get(rows[,variable])]~dataToUse[,SEX]),
                                                testResult <- aov(dataToUse[,get(rows[,variable])]~dataToUse[,SEX]))))))
          
          invisible(ifelse(!is.na(testResult),{
            ifelse(!statTest=="ANOVA",
                   p <-  testResult$p.value,
                   p <- summary(testResult)[[1]][["Pr(>F)"]][1])
          }, p <- NA))
          
          return(p)
        })
      
       return(TEST)
    })
    
    names(sexStats) <- biome

    SEX <- lapply(biome, function(biome){
    p.SEX <- cbind(listTests[[biome]], p.value = sexStats[[biome]])
    return(p.SEX)
  })
  names(SEX) <- biome
  
  # Adding 'SIGNIFICANT'
  SEX <- lapply(biome, function(biome){
       sig.SEX <- cbind(SEX[[biome]], Significancy = ifelse(!is.na(SEX[[biome]]$p.value)&SEX[[biome]]$p.value<0.05,"Significant",""))
      return(sig.SEX)
    })
  names(SEX) <- biome
 # Variables that are out for the sex analysis (those that were significant for SEX)
 outBIOME.list <- lapply(biome, function(biome){
     DF <- data.frame(listTests[[biome]])
     sig.rows <- as.character(DF$variable[which(DF$Significancy=="Significant")])
     return(sig.rows)
   })
 names(outBIOME.list) <- biome
 outBIOME.list <<- outBIOME.list
 
}

  # ========= FOR BIOME ==========

  if (depth(listTests)==1){
    
       biomeStats <- apply(listTests, 1, function(rows){
       
        # Extracting information for the variable to make the analysis
         groups <- strsplit(rows[["whichGroups"]], ":") %>%
           .[[1]]
        line <- which(testsPerVariable[,VARIABLES]==rows["variable"])
        tst <- testsPerVariable[line,TEST]
        trans <- testsPerVariable[line,TRANSFORMATION]
        nor <-  testsPerVariable[line,NORMALITY]
        
        namesRows <- names(rows)
        rows <- data.table(matrix(rows, nrow=1))
        names(rows) <- namesRows
        
        statTest <- rows[,statTest]
        
        # If statTest is not NA, execute statistics
        # Composing the dataset
        invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                         dataToUse <- data.table(cbind(BIOME=originalDT$biome, originalDT[rows[,variable]])),
                         dataToUse <- data.table(cbind(BIOME=logDT$biome, logDT[rows[,variable]]))))
        dataToUse <- dataToUse[as.vector(!is.na(dataToUse[,2])),] %>%
          .[BIOME %in% groups,]
        
        # subset by specific groups
      
        suppressMessages(invisible(ifelse(is.na(statTest), testResult <- NA,
                         ifelse(statTest=="T-TEST",
                                testResult <- t.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                ifelse(statTest=="Mann-WhitneyU",
                                       testResult <- wilcox.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                       ifelse(statTest=="Kruskal-Wallis",
                                              testResult <- kruskal.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                              testResult <- aov(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME])))))))
        
        invisible(ifelse(!is.na(testResult),{
          ifelse(!statTest=="ANOVA",
                 p <-  testResult$p.value,
                 p <- summary(testResult)[[1]][["Pr(>F)"]][1])
        }, p <- NA))
        
        return(p)
      })

 BIOME <- cbind(listTests, p.value = biomeStats)

  # Adding 'SIGNIFICANT'
  BIOME$Significancy <- ifelse(!is.na(BIOME$p.value) & BIOME$p.value < 0.05, "Significant","")
  }
  
  # ======== RETURNS

  if (exists("sexStats")) return(SEX)
  if (exists("biomeStats")) return(BIOME)
  
})
  }
