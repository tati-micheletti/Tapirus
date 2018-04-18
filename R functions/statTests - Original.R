
# statAGE <- statTests(listTests = AGE, testsPerVariable = final.table, originalDT = hembio, logDT = hembio.log, biome = biome, sex = sex)

statTests <- function(listTests, 
                      testsPerVariable = final.table, 
                      originalDT = hembio, 
                      logDT = hembio.log, 
                      biome = biome, 
                      sex = sex,
                      exclude = NA){
suppressWarnings({
  depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)

  # ========= FOR AGE ==========

  
  if (depth(listTests)==3){
    
    ageStats <- lapply(biome, function(biome){
       TEST <- lapply(sex, function(sex){
         TEST <- apply(AGE[[biome]][[sex]], 1, function(rows){

           # Extracting information for the variable to make the analysis
           line <- which(final.table[,VARIABLES]==rows["variables"])
           tst <- final.table[line,TEST]
           trans <- final.table[line,TRANSFORMATION]
           nor <-  final.table[line,NORMALITY]
           
           namesRows <- names(rows)
           rows <- data.table(matrix(rows, nrow=1))
           names(rows) <- namesRows
           
           # Identifying if the dataset has enough information to be compared and which groups to compare
           statTest <- rows[,statTest]
           
    library(stringr)
           howManyGroups <- str_count(rows[,whichGroups], ":")+1
           rows <- tidyr::separate(data = rows, 
                           col = whichGroups, 
                           sep = ":", 
                           into = paste0("GC", seq(1, howManyGroups)))
          
           vectorGroups <- which(rows[,grepl(pattern = "GC", x = names(rows))])
           groupsTest <- names(rows)[vectorGroups]
           groupsToTest <- as.numeric()
           for (i in groupsTest){
            groupsToTest[i] <- rows[,i, with=FALSE]
           }
           groupsToTest <- as.vector(unlist(groupsToTest))
           
             # If statTest is not NA, execute statistics
             # Composing the dataset
                  invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                                   dataToUse <- data.table(cbind(AGE=originalDT$Age, originalDT[rows[,variables]])),
                                   dataToUse <- data.table(cbind(AGE=logDT$Age, logDT[rows[,variables]]))))
             dataToUse <- dataToUse[dataToUse[,AGE %in% groupsToTest],] %>%
             .[as.vector(!is.na(.[,2])),]

                  invisible(ifelse(is.na(statTest), testResult <- NA,
                                   ifelse(statTest=="T-TEST",
                                          testResult <- t.test(dataToUse[,get(rows[,variables])]~dataToUse[,AGE]),
                                     ifelse(statTest=="Mann-WhitneyU",
                                          testResult <- wilcox.test(dataToUse[,get(rows[,variables])]~dataToUse[,AGE]),
                                       ifelse(statTest=="Kruskal-Wallis",
                                          testResult <- kruskal.test(dataToUse[,get(rows[,variables])]~dataToUse[,AGE]),
                                          testResult <- aov(dataToUse[,get(rows[,variables])]~dataToUse[,AGE]))))))

                  # Make posthoc if it applies and save in global environment
                  
      invisible(ifelse(!is.na(testResult),{
            ifelse(!statTest=="ANOVA",
                   p <-  testResult$p.value,
                   p <- summary(testResult)[[1]][["Pr(>F)"]][1])
          }, p <- NA))
          return(p)
        })
         
         return(TEST)
       })
       names(TEST) <- sex
       return(TEST)
     })
    
     names(ageStats) <- biome
     
     # Adding p value to original table
  AGE <- lapply(biome, function(biome){
    newAGE <- lapply(sex, function(sex){
      p.AGE <- cbind(AGE[[biome]][[sex]], p.value = ageStats[[biome]][[sex]])
      return(p.AGE)
    })
    names(newAGE) <- sex
    return(newAGE)
  })
  names(AGE) <- biome

  # Adding 'SIGNIFICANT'
  AGE <- lapply(biome, function(biome){
    newAGE <- lapply(sex, function(sex){
      sig.AGE <- cbind(AGE[[biome]][[sex]], Significancy = ifelse(!is.na(AGE[[biome]][[sex]]$p.value)&AGE[[biome]][[sex]]$p.value<0.05,"Significant",""))
      return(sig.AGE)
    })
    names(newAGE) <- sex
    return(newAGE)
  })
  names(AGE) <- biome
  
# Variables that are out for the sex analysis (those that were significant for AGE)
  outSEX.list <- lapply(biome, function(biome){
    TEST <- lapply(sex, function(sex){
      DF <- data.frame(AGE[[biome]][[sex]])
      sig.rows <- as.character(DF$variables[which(DF$Significancy=="Significant")])
      return(sig.rows)
      })
    names(TEST) <- sex
    return(TEST)
  })
  names(outSEX.list) <- biome
  outSEX.list <<- outSEX.list
  
  }
  
  # ========= FOR SEX ==========

  if (depth(listTests)==2){
    
  sexStats <- lapply(biome, function(biome){
      TEST <- apply(SEX[[biome]], 1, function(rows){

          # Extracting information for the variable to make the analysis
          line <- which(final.table[,VARIABLES]==rows["variable"])
          tst <- final.table[line,TEST]
          trans <- final.table[line,TRANSFORMATION]
          nor <-  final.table[line,NORMALITY]
          
          namesRows <- names(rows)
          rows <- data.table(matrix(rows, nrow=1))
          names(rows) <- namesRows
          
          statTest <- rows[,statTest]
          
          # If statTest is not NA, execute statistics
          # Composing the dataset
          invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                           dataToUse <- data.table(cbind(SEX=originalDT$Sex, originalDT[rows[,variable]])),
                           dataToUse <- data.table(cbind(SEX=logDT$Sex, logDT[rows[,variable]]))))
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
    p.SEX <- cbind(SEX[[biome]], p.value = sexStats[[biome]])
    return(p.SEX)
  })
  names(SEX) <- biome
  
  # Adding 'SIGNIFICANT'
  SEX <- lapply(biome, function(biome){
       sig.SEX <- cbind(SEX[[biome]], Significancy = ifelse(!is.na(SEX[[biome]]$p.value)&SEX[[biome]]$p.value<0.05,"Significant",""))
      return(sig.SEX)
    })
  names(SEX) <- biome
  
  # Removing the significant ones from AGE for SEX
 SEX <- lapply(biome, function(biome){
   exc <- unique(unlist(exclude[[biome]], recursive = FALSE, use.names = FALSE))
    ifelse(length(exc)==0,{
      SEX.biome <- SEX[[biome]]
    },{
      SEX.biome <- SEX[[biome]][!variable %in% exc,]
    }
   ) 
   return(SEX.biome)
 })
 names(SEX) <- biome

 # Variables that are out for the sex analysis (those that were significant for SEX)
 outBIOME.list <- lapply(biome, function(biome){
     DF <- data.frame(SEX[[biome]])
     sig.rows <- as.character(DF$variable[which(DF$Significancy=="Significant")])
     return(sig.rows)
   })
 names(outBIOME.list) <- biome
 outBIOME.list <<- outBIOME.list
 
}

  # ========= FOR BIOME ==========

  if (depth(listTests)==1){
    
       biomeStats <- apply(BIOME, 1, function(rows){
       
        # Extracting information for the variable to make the analysis
         groups <- strsplit(rows[["whichGroups"]], ":") %>%
           .[[1]]
        line <- which(final.table[,VARIABLES]==rows["variable"])
        tst <- final.table[line,TEST]
        trans <- final.table[line,TRANSFORMATION]
        nor <-  final.table[line,NORMALITY]
        
        namesRows <- names(rows)
        rows <- data.table(matrix(rows, nrow=1))
        names(rows) <- namesRows
        
        statTest <- rows[,statTest]
        
        # If statTest is not NA, execute statistics
        # Composing the dataset
        invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                         dataToUse <- data.table(cbind(BIOME=originalDT$Biome, originalDT[rows[,variable]])),
                         dataToUse <- data.table(cbind(BIOME=logDT$Biome, logDT[rows[,variable]]))))
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

 BIOME <- cbind(BIOME, p.value = biomeStats)
  
  }
  
  # Adding 'SIGNIFICANT'
  BIOME$Significancy <- ifelse(!is.na(BIOME$p.value)&BIOME$p.value<0.05,"Significant","")

  # ======== RETURNS

  if (exists("ageStats")) return(AGE)
  if (exists("sexStats")) return(SEX)
  if (exists("biomeStats")) return(BIOME)
  
})
  }
