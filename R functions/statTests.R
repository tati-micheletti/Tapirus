statTests <- function(listTests, testsPerVariable = final.table, originalDT = hembio, logDT = hembio.log, biome = biome, sex = sex){
  
  listTests <- BIOME
  listTests <- SEX
  listTests <- AGE

   if (grepl(pattern = "AGE", x = listTests)){
    
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
           
           # -------------- SUBSTITUTE: a for rows ------------------
           vectorGroups <- which(a[,grepl(pattern = "GC", x = names(a))])
           groupsTest <- names(a)[vectorGroups]
           groupsToTest <- as.numeric()
           for (i in groupsTest){
            groupsToTest[i] <- a[,i, with=FALSE]
           }
           groupsToTest <- as.vector(unlist(groupsToTest))
           ifelse(!is.na(statTest),{
             
             # If statTest is not NA, execute statistics
             # Composing the dataset
                  invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL", 
                                   dataToUse <- data.table(cbind(AGE=originalDT$Age, originalDT[rows[,variables]])),
                                   dataToUse <- data.table(cbind(AGE=logDT$Age, logDT[rows[,variables]]))))
             dataToUse <- dataToUse[dataToUse[,AGE %in% groupsToTest],]
             dataToUse <- dataToUse[as.vector(!is.na(dataToUse[,2])),]
             
             # From here on, not working... Fix the tests...
             
                  testResult <- ifelse(statTest=="T-TEST", t.test(as.vector(dataToUse[,2])~as.vector(dataToUse[,1])),
                         ifelse(statTest=="Mann-WhitneyU"))

          },next)


     
           
           TEST <- ifelse(rows["test"]=="T",
                          ifelse(tst=="PARAMETRIC","T-TEST","Mann-WhitneyU"),
                          ifelse(rows["test"]=="ANOVA",
                                 ifelse(tst=="PARAMETRIC","ANOVA","Kruskal-Wallis"),
                                 NA))
           return(TEST)
         })
         return(TEST)
       })
       names(TEST) <- sex
       return(TEST)
     })
     names(AGE.table) <- biome

  }
 
     
    
    # What to do:
    # AGE has 3 lists (biome), composed of 2 lists (sex). For each DT of [[biome]][[sex]] need to reference hembio or hembio.log for data. 
    # use apply. For each 'variables', check final.table for transformation: if NORMALITY=="NORMAL" or TRANSFORMATION=="NON-NORMAL", use hembio. If TRANSFORMATION=="NORMAL", use hembio.log. 
    # 2. Unsplit whichGroups as a vector
    # 3. Subset hembio or hembio.log ("dataToUse" data.table) based on [[biome]][[sex]] and whichGroups (if statTest != NA) as: Age, variables
  
    # For the tests
    
    # Identifies the depth of the lists (BIOME = 1, SEX = 2, AGE = 3) not so sure if use this, though. It doesn't have to be super flexible... Better: identify with grepl in listsTests which group that is and make 3 if statements for the function for each one of them.
    
    
    depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)  
  
}
