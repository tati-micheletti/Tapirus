createTestsTable <- function(fDataCol = fDataCol, 
                             hembio = hembio,
                             nat.normal = nat.normal,
                             normal.trans = normal.trans,
                             homo.test = homo.test,
                             non.homo = non.homo){
  
  all.var <- colnames(hembio[,c(fDataCol:ncol(hembio))])
  final.table <- data.frame(VARIABLES = all.var, NORMALITY = NA, TRANSFORMATION = NA, HOMOGENEITY = NA, TEST = NA)
  for (i in 1:nrow(final.table)){
    if (final.table[i,"VARIABLES"] %in% nat.normal)
      final.table[i,"NORMALITY"] <- "NORMAL" else 
        final.table[i,"NORMALITY"] <- "NON-NORMAL"
      if (final.table[i,"VARIABLES"] %in% normal.trans) 
        final.table[i,"TRANSFORMATION"] <- "NORMAL" else
          if (final.table[i,"NORMALITY"]=="NORMAL") 
            final.table[i,"TRANSFORMATION"] <- "NA" else
              final.table[i,"TRANSFORMATION"] <- "NON-NORMAL"
            if (!(final.table[i,"VARIABLES"] %in% homo.test))
              final.table[i,"HOMOGENEITY"]  <- "NA" else
                if (final.table[i,"VARIABLES"] %in% non.homo) 
                  final.table[i,"HOMOGENEITY"] <- "NON-HOMOGENEOUS" else 
                    final.table[i,"HOMOGENEITY"] <- "HOMOGENEOUS"
                  if (!(final.table[i,"TRANSFORMATION"] == "NON-NORMAL") & final.table[i,"HOMOGENEITY"] == "HOMOGENEOUS")
                    final.table[i,"TEST"] <- "PARAMETRIC" else
                      final.table[i,"TEST"] <- "NON-PARAMETRIC"
  }
  
  final.table <- data.table(final.table)
  return(final.table)
  
}