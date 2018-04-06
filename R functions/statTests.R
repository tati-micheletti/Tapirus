
# statAGE <- statTests(listTests = AGE, testsPerVariable = final.table, originalDT = hembio, logDT = hembio.log, biome = biome, sex = sex)

statTests <- function(listTests, 
                      testsPerVariable = final.table, 
                      originalDT = hembio, 
                      logDT = hembio.log, 
                      biome = biome, 
                      sex = sex){

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
     
  AGE <- lapply(biome, function(biome){
    newAGE <- lapply(sex, function(sex){
      p.AGE <- cbind(AGE[[biome]][[sex]], p.value = ageStats[[biome]][[sex]])
      return(p.AGE)
    })
    names(newAGE) <- sex
    return(newAGE)
  })
  names(AGE) <- biome

  }
  
  # ========= FOR SEX ==========

  if (depth(listTests)==2){
  # FROM HERE ON. Need to copy from lines 18:82. Final merging code is alsready down (lines 102:106 and for biome 110.
  # !!! REMEMBER !!! I can only compare on a superior level IF the Age was not significant for each variable/sex.

  SEX <- lapply(biome, function(biome){
    p.SEX <- cbind(SEX[[biome]], p.value = sexStats[[biome]])
    return(p.SEX)
  })
  names(SEX) <- biome
  
  }
  # ========= FOR BIOME ==========
  if (depth(listTests)==1){
    
  BIOME <- cbind(BIOME, p.value = biomeStats)
  
  }
  # ======== RETURNS
  
  if (exists("ageStats")) return(AGE)
  if (exists("sexStats")) return(SEX)
  if (exists("biomeStats")) return(BIOME)
  
}
