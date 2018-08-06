# Posthoc tests

posthocTests <- function(statTable,
                         testsTable = final.table,
                         originalDT = hembio, 
                         logDT = hembio.log, 
                         biome = biome,
                         sex = sex){
  
  suppressWarnings({
    depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
    na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

    source(file.path(getwd(),"R_functions/dunnTest.R"))
    
                         # ========= FOR AGE ==========
    
    if (depth(statTable)==3){
      
      posthocAGE <- lapply(biome, function(biome){
        TEST <- lapply(sex, function(sex){
          TEST <- apply(statTable[[biome]][[sex]], 1, function(rows){
            
            # Extracting information for the variable to make the analysis
            line <- which(testsTable[,VARIABLES]==rows["variables"])
            tst <- testsTable[line,TEST]
            trans <- testsTable[line,TRANSFORMATION]
            nor <-  testsTable[line,NORMALITY]
            
            namesRows <- names(rows)
            rows <- data.table(matrix(rows, nrow=1))
            names(rows) <- namesRows
            
            # Identifying the groups for data subsetting   
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
            statTest <- rows[,statTest]
            
            # Composing the dataset
            invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL",
                             dataToUse <- data.table(cbind(AGE=originalDT$ageClass, originalDT[rows[,variables]])),
                             dataToUse <- data.table(cbind(AGE=logDT$ageClass, logDT[rows[,variables]]))))
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

# =============== FOR ANOVA

            if(rows[,"Significancy", with = FALSE] == "Significant"&
                      rows[,"statTest", with = FALSE] == "ANOVA"){

                  ph <- TukeyHSD(testResult)
                  phResult <- as.data.frame(matrix((ph[[1]][,"p adj"]), nrow = 1))
                  colnames(phResult) <- names(ph[[1]][,"p adj"])
                  phMeans <- aggregate(get(rows[,variables]) ~ AGE, data = dataToUse, FUN = mean)
                  phMensVal <- as.data.frame(matrix(phMeans[,2], nrow=1))
                  colnames(phMensVal) <- phMeans[,1]
                  
                  phResult <- cbind(phResult,phMensVal)
                }
            
# =============== FOR KRUSKAL-WALLIS
            
            if(rows[,"Significancy", with = FALSE] == "Significant"&
                      rows[,"statTest", with = FALSE] == "Kruskal-Wallis"){
              dataToUse <- data.frame(dataToUse)
              sink(tempfile())
              ph <- dunn(dataToUse[,2], dataToUse[,1], list = TRUE)
              sink()
              phResult <- as.data.frame(matrix(ph$P.adjusted, nrow = 1))
              colnames(phResult) <- ph$comparisons
              
              phMeans <- aggregate(get(rows[,variables]) ~ AGE, data = dataToUse, FUN = mean)
              phMensVal <- as.data.frame(matrix(phMeans[,2], nrow=1))
              colnames(phMensVal) <- phMeans[,1]
              
              phResult <- cbind(phResult,phMensVal)
                  
                
            } # if statement KW

            if(is.na(statTest)|
               statTest == "T-TEST"|
               statTest == "Mann-WhitneyU"|
               statTest=="ANOVA"&!rows[,"Significancy", with = FALSE] == "Significant"|
               statTest=="Kruskal-Wallis"&!rows[,"Significancy", with = FALSE] == "Significant"){
              phResult <-  NA}

      return(invisible(phResult))
          }) # apply
          
          return(invisible(TEST))
          
        }) # lapply sex
        
        names(TEST) <- sex
        return(invisible(TEST))
        
      }) # lapply biome and AGE

    names(posthocAGE) <- biome
    
      posthocAGE <- lapply(biome, function(biome){
      TEST <- lapply(sex, function(sex){
        if(all(is.na(posthocAGE[[biome]][[sex]]))){
          TEST <- NA
        } else {
          TEST <- posthocAGE[[biome]][[sex]]
          names(TEST) <- variables
        }
        TEST <- na.omit.list(TEST)
        return(TEST)
      })
      names(TEST) <- sex
      TEST <- na.omit.list(TEST)
      return(TEST)
    })
    names(posthocAGE) <- biome
    posthocAGE <- na.omit.list(posthocAGE)
    
    }
    
    if(exists("posthocAGE")){posthocAGE <- posthocAGE[[1]][[1]]} # Clean list nesting. If add more data, take this out.
    
    # ========= FOR BIOME ==========
    
    if (depth(statTable)==1){
      
      posthocBIOME <- apply(statTable, 1, function(rows){
        
        # Extracting information for the variable to make the analysis
        line <- which(testsTable[,VARIABLES] == rows["variable"])
        tst <- testsTable[line,TEST]
        trans <- testsTable[line,TRANSFORMATION]
        nor <-  testsTable[line,NORMALITY]
        
        namesRows <- names(rows)
        rows <- data.table(matrix(rows, nrow=1))
        names(rows) <- namesRows
        
        # Identifying the groups for data subsetting   
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
        statTest <- rows[,statTest]
        
        # Composing the dataset
        invisible(ifelse(nor=="NORMAL"|trans=="NON-NORMAL",
                         dataToUse <- data.table(cbind(BIOME=originalDT$biome, originalDT[rows[,variable]])),
                         dataToUse <- data.table(cbind(BIOME=logDT$biome, logDT[rows[,variable]]))))
        dataToUse <- dataToUse[dataToUse[,BIOME %in% groupsToTest],] %>%
          .[as.vector(!is.na(.[,2])),]
        
        invisible(ifelse(is.na(statTest), testResult <- NA,
                         ifelse(statTest=="T-TEST",
                                testResult <- t.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                ifelse(statTest=="Mann-WhitneyU",
                                       testResult <- wilcox.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                       ifelse(statTest=="Kruskal-Wallis",
                                              testResult <- kruskal.test(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]),
                                              testResult <- aov(dataToUse[,get(rows[,variable])]~dataToUse[,BIOME]))))))
        
        # =============== FOR ANOVA
        
        if(rows[,"Significancy", with = FALSE] == "Significant"&
           rows[,"statTest", with = FALSE] == "ANOVA"){
          
          ph <- TukeyHSD(testResult)
          phResult <- as.data.frame(matrix((ph[[1]][,"p adj"]), nrow = 1))
            colnames(phResult) <- names(ph[[1]][,"p adj"])
           phMeans <- aggregate(get(rows[,variable]) ~ BIOME, data = dataToUse, FUN = mean)
            phMensVal <- as.data.frame(matrix(phMeans[,2], nrow=1))
            colnames(phMensVal) <- phMeans[,1]
            
            phResult <- cbind(phResult,phMensVal)

        }
        
        # =============== FOR KRUSKAL-WALLIS
        
        if(rows[,"Significancy", with = FALSE] == "Significant"&
           rows[,"statTest", with = FALSE] == "Kruskal-Wallis"){
          
          dataToUse <- data.frame(dataToUse)
          sink(tempfile())
          ph <- dunn(dataToUse[,2], dataToUse[,1], list = TRUE)
          sink()
          phResult <- as.data.frame(matrix(ph$P.adjusted, nrow = 1))
          colnames(phResult) <- ph$comparisons
          
          phMeans <- aggregate(get(rows[,variable]) ~ BIOME, data = dataToUse, FUN = mean)
          phMensVal <- as.data.frame(matrix(phMeans[,2], nrow=1))
          colnames(phMensVal) <- phMeans[,1]
          
          phResult <- cbind(phResult,phMensVal)
          
        } # if statement KW
        
        if(is.na(statTest)|
           statTest == "T-TEST"|
           statTest == "Mann-WhitneyU"|
           statTest=="ANOVA"&!rows[,"Significancy", with = FALSE] == "Significant"|
           statTest=="Kruskal-Wallis"&!rows[,"Significancy", with = FALSE] == "Significant"){
          phResult <-  NA}
        
        return(invisible(phResult))
  }) # apply
      browser()
      names(posthocBIOME) <- statTable$variable
      posthocBIOME <- na.omit.list(posthocBIOME)
      
  }

    if (exists("posthocAGE")) return(invisible(posthocAGE))
    if (exists("posthocBIOME")) return(invisible(posthocBIOME))
    
  }) # suppressWarnings
  
} # End function

