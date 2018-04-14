# Project Description

The present project aimed at understanding the general health of Tapir (**Tapirus terrestris**) populations on three different biomes.

# Hematology and Biochemestry

## Analysis summary

The following analysis were performed for the results to be comparable to the methods used in `"Health Assessment of Wild Lowland Tapirs"` *[Journal of Wildlife Disease, 2014]*.

1. Shapiro-Wilk to test Normality of Samples
  + 1a. Non-normal try transform [log(e)] --> test SW again
    -  1a1. Non-normal again --> NON PARAMETRIC
  + 1b. Normal, pass to 2.
2. Levene's test of Variance homogeneity
  + 2a. Non-homogeneous --> NON PARAMETRIC
  + 2b. Normal, use --> PARAMETRIC

FOR NON-PARAMETRIC SAMPLES: `Kruskal-Wallis` test for more than 2 classes or `Mann-Whitney U` test for 2 classes
FOR PARAMETRIC SAMPLES: `ANOVA` for more than 2 classes or `T-TEST` for 2 classes

The groups were hierarchically designed as: Biome > Sex > Age, i.e. Pantanal Males Juv x Pantanal Males Adults. Only the variables that were *not signifficant* for each group were compared on the next level.

