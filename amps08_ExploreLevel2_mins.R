# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set08_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min.rds")

# LEVEL 2

# # Subsetting only on the variable I intend to use in this section:
# set08_min <- set08_min[,-c(1:2,8:12,14:21)]

# ## Determine Number of Factors to Extract
strt <- which(names(set08_min) == "Business.Day")
ev <- eigen(cor(set08_min[,strt:ncol(set08_min)]))
ap <- parallel(subject=nrow(set08_min[,strt:ncol(set08_min)]),var=ncol(set08_min[,strt:ncol(set08_min)]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_08_min")
plotnScree(nS, main = "National") # optimal = 6
dev.off()
# 
# # will set them at six for both Jhb and CT for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_08_min <- PCA(set08_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# saveRDS(pca_08_min, "pca_08_min.rds")

# pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_08 <- fa(set08_min[strt:ncol(set08_min)], nfactors = 7, fm = "ml", scores = "Bartlett") # default rotation oblimin, so does allow correlation between factors
fact_08_loadings <- fact_08$loadings
fact_08_scores <- fact_08$scores

# save model
saveRDS(fact_08, "fact_08.rds")

# save loadings:
saveRDS(fact_08_loadings, "fact_08_loadings.rds")

# save scores:
saveRDS(fact_08_scores, "fact_08_scores.rds")

write.csv(round(loadings(fact_08, sort = TRUE), 2), file = "loadings_min_08.csv")
