# For each variable in the table, check if there are at least 2 Age groups to compare

# ATLANTIC FOREST
atF.J <- At.F[Age=="Juvenile", sum(count>1)]
atF.Sa <- At.F[Age=="Sub-adult", sum(count>1)]
atF.Ad <- At.F[Age=="Adult", sum(count>1)] # No variables can be compared among female groups

atM.J <- At.M[Age=="Juvenile", sum(count>1)]
atM.Sa <- At.M[Age=="Sub-adult", sum(count>1)]
atM.Ad <- At.M[Age=="Adult", sum(count>1)]

# PANTANAL
ptF.J <- Pt.F[Age=="Juvenile", sum(count>1)]
ptF.Sa <- Pt.F[Age=="Sub-adult", sum(count>1)]
ptF.Ad <- Pt.F[Age=="Adult", sum(count>1)] # No variables can be compared among female groups

ptM.J <- Pt.M[Age=="Juvenile", sum(count>1)]
ptM.Sa <- Pt.M[Age=="Sub-adult", sum(count>1)]
ptM.Ad <- Pt.M[Age=="Adult", sum(count>1)]

# CERRADO
crF.J <- Cr.F[Age=="Juvenile", sum(count>1)]
crF.Sa <- Cr.F[Age=="Sub-adult", sum(count>1)]
crF.Ad <- Cr.F[Age=="Adult", sum(count>1)] # No variables can be compared among female groups

crM.J <- Cr.M[Age=="Juvenile", sum(count>1)]
crM.Sa <- Cr.M[Age=="Sub-adult", sum(count>1)]
crM.Ad <- Cr.M[Age=="Adult", sum(count>1)]

#### Atlantic Forest
#Females
atF.Ad # Atlantic Forest Adult Females
atF.Sa # Atlantic Forest Sub-adult Females
atF.J # Atlantic Forest Juvenile Females
#** Conclusion:** No comparison can be made by age among females from Atlantic forest due to the lack of data

#Males
# ** Conclusion:** Comparison among males from Pantanal can be made for the following variables based on `Adults`:
atM.Ad # Atlantic Forest Adult Females
atM.Sa # Atlantic Forest Sub-adult Females
atM.J # Atlantic Forest Juvenile Females

# <!-- ** Conclusion:** Comparison among males from Atlantic forest can be made only for: `Ureia`, `ColesterolTotal` and `ProteinasTotais` for Adults and Sub-Adults. -->
  #### Pantanal

#  Females
#** Conclusion:** Comparison among Adults and Sub-adults can be made for the following 41 variables:
ptF.Ad # Pantanal Adult Females
ptF.Sa # Pantanal Sub-adult Females
ptF.J # Pantanal Juvenile Females

#Males
#** Conclusion:** Comparison among males from Pantanal can be made for the following variables based on `Adults`:
ptM.Ad # Pantanal Adult Females
ptM.Sa # Pantanal Sub-adult Females
ptM.J # Pantanal Juvenile Females

#### Cerrado
#Females
# ** Conclusion:** Comparison among Adults and Sub-adults can be made for the following 42 variables:
crF.Ad # Cerrado Adult Females
crF.Sa # Cerrado Sub-adult Females
crF.J # Cerrado Juvenile Females

Males
# ** Conclusion:** Comparison among males from Cerrado can be made for the following variables based on `Adults`:
crM.Ad # Cerrado Adult Males
crM.Sa # Cerrado Sub-adult Males
crM.J # Cerrado Juvenile Males

# TRUE / FALSE TABLE FOR THE JUVENILE AND SUB-ADULT VARIABLES THAT ARE ALSO IN ADULTS
At.SubA <- At.F[Age=="Sub-adult"]$variable
At.Adu <- At.F[Age=="Adult"]$variable
At.Juv <- At.F[Age=="Juvenile"]$variable
At.Adu %in% At.SubA#Are all variables the same between adults and sub-adults?
At.Adu %in% At.Juv #Are all variables the same between adults and sub-adults?
At.F.Age <- data.table(Variables=At.Adu,SubAdults=(At.Adu %in% At.SubA),Juvenile=(At.Adu %in% At.Juv))
At.F.Age <- merge(vari.table,At.F.Age, all = TRUE)

# Atlantic Males
At.MSubA <- At.M[Age=="Sub-adult"]$variable
At.MAdu <- At.M[Age=="Adult"]$variable
At.MJuv <- At.M[Age=="Juvenile"]$variable
At.MAdu %in% at.MSubA #Are all variables the same between adults and sub-adults?
At.MAdu %in% at.MJuv #Are all variables the same between adults and sub-adults?
At.M.Age <- data.table(Variables=At.MAdu,SubAdults=(At.MAdu %in% At.MSubA),Juvenile=(At.MAdu %in% At.MJuv))
At.M.Age <- merge(vari.table,At.M.Age, all=TRUE)
knitr::kable(At.M.Age)