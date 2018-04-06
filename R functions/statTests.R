
# statAGE <- statTests(listTests = AGE, testsPerVariable = final.table, originalDT = hembio, logDT = hembio.log, biome = biome, sex = sex)

statTests <- function(listTests, 
                      testsPerVariable = final.table, 
                      originalDT = hembio, 
                      logDT = hembio.log, 
                      biome = biome, 
                      sex = sex){

  depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
  
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

  }
 
  # FROM HERE ON. Finish AGE and do the same for sex and biome
  
  # return(AGE with the ageStats as a cbind) I have done this before, there is code ready for that.
  
}
