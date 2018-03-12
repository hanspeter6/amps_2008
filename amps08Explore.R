# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
# 

# read datafiles
magazines_engagement_08 <- readRDS("magazines_engagement_08.rds")
newspapers_engagement_08 <- readRDS("newspapers_engagement_08.rds")
radio_engagement_08 <- readRDS("radio_engagement_08.rds")
tv_engagement_08 <- readRDS("tv_engagement_08.rds")
internet_engagement_08 <- readRDS("internet_engagement_08.rds")
internet_engagement_08_simple <- readRDS("internet_engagement_08_simple.rds")

media_type_08 <- readRDS("media_type_08.rds")
media_type_08_simple <- readRDS("media_type_08_simple.rds")
media_vehicles_08 <- readRDS("media_vehicles_08.rds")
media_vehicles_08_simple <- readRDS("media_vehicles_08_simple.rds")


demographics_08 <- readRDS("demographics_08.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_08$age <- ifelse(demographics_08$age %in% c(1,2), 1, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(3,4), 2, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(5,6), 3, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(7,8), 4, demographics_08$age)
demographics_08$age <- factor(demographics_08$age, ordered = TRUE)

# sex:
demographics_08$sex <- factor(demographics_08$sex, ordered = FALSE)

#edu:
demographics_08$edu <- ifelse(demographics_08$edu %in% c(1,2,3,4), 1, demographics_08$edu)
demographics_08$edu <- ifelse(demographics_08$edu %in% c(5), 2, demographics_08$edu)
demographics_08$edu <- ifelse(demographics_08$edu %in% c(6,7,8), 3, demographics_08$edu)
demographics_08$edu <- factor(demographics_08$edu, ordered = TRUE)

#hh_inc
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(1,2,3,4), 1, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(5,6), 2, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(7), 3, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(8), 4, demographics_08$hh_inc)
demographics_08$hh_inc <- factor(demographics_08$hh_inc, ordered = TRUE)

demographics_08$race <- factor(demographics_08$race, ordered = FALSE)
demographics_08$province <- factor(demographics_08$province, ordered = FALSE)
demographics_08$metro <- factor(demographics_08$metro, ordered = FALSE)
demographics_08$lang <- factor(demographics_08$lang, ordered = FALSE)
demographics_08$lifestages <- factor(demographics_08$lifestages, ordered = FALSE)
demographics_08$mar_status <- factor(demographics_08$mar_status, ordered = FALSE)
# demographics_08$pers_inc <- factor(demographics_08$pers_inc, ordered = TRUE)

# lsm
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(1,2), 1, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(3,4), 2, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(5,6), 3, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(7,8), 4, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(9,10), 5, demographics_08$lsm)
demographics_08$lsm <- factor(demographics_08$lsm, ordered = TRUE)

# demographics_08$lifestyle <- factor(demographics_08$lifestyle, ordered = FALSE)
demographics_08$attitudes <- factor(demographics_08$attitudes, ordered = FALSE)

# #create single dataset minus non metropolitans
set08 <- demographics_08 %>%
        left_join(media_type_08) %>%
        left_join(media_vehicles_08) %>%
        filter(metro != 0)

set08_simple <- demographics_08 %>%
        left_join(media_type_08_simple) %>%
        left_join(media_vehicles_08_simple) %>%
        filter(metro != 0)

# consider some correlations

png('corTypePlot2008.png')
corrplot(cor(set08[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist08 <- dist(set08[,c("newspapers","magazines","radio", "tv", "internet")])
# clust08 <- hclust(dist08, method = "complete")
# plot(clust08) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,08,11,10)) {
        temp <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2008.png')
plot(c(3,4,5,6,7,8,9,08,11,10), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans08 <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)
set.seed(56)
kmeans08_simple <- kmeans(set08_simple[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20,
                   iter.max = 20)

table(kmeans08$cluster) #

# add cluster labels to the dataset
set08 <- set08 %>%
        mutate(cluster = factor(kmeans08$cluster))
set08_simple <- set08_simple %>%
        mutate(cluster = factor(kmeans08_simple$cluster))



# trying out idea of first pc scores as measure of media type mix...kinda engagement...think about this

pc_type <- princomp(set08[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type, type = "lines")

set08 <- set08 %>%
        mutate(typePC = scale(pc_type$scores[,1]))

pc_type_simple <- princomp(set08_simple[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type_simple, type = "lines")

set08_simple <- set08_simple %>%
        mutate(typePC = scale(pc_type_simple$scores[,1]))


saveRDS(set08, "set08.rds")
saveRDS(set08_simple, "set08_simple.rds")

set08 <- readRDS("set08.rds")
set08_simple <- readRDS("set08_simple.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub08 <- set08[sample(nrow(set08), size = 1000),]

# distance matrix and MDS
sub08_dist <- dist(sub08[,c("newspapers","magazines","radio", "tv", "internet")])
mds08 <- cmdscale(sub08_dist)
plot(mds08, col = as.numeric(sub08$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub08[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2008.jpeg')
plot(mds08, col = as.numeric(sub08$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2008.jpeg')
scatterplot3d(mds3, color = as.numeric(sub08$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2008.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub08$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub08[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 10000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub08['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub08['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub08['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub08['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub08['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub08$cluster ) # not very good organising??

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_08))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_08))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_08))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_08[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_08))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set08[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set08$cluster
cor(test$cluster, as.numeric(set08$cluster))

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set08$cluster, p = 0.7, list = FALSE)
training <- set08[ind_train,]
testing <- set08[-ind_train,]

# # using random forest:
forest08_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest08_type <- predict(forest08_type, newdata = testing)

confusionMatrix(pred_forest08_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda08 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda08)

pred_lda08 <- predict(lda08, newdata = testing)
confusionMatrix(pred_lda08$class, testing$cluster) # 

# using only demographic information
forest08_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest08_demogr <- predict(forest08_demogr, newdata = testing)

confusionMatrix(pred_forest08_demogr, testing$cluster)

# with lda
set.seed(56)
lda08_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda08_demogr <- predict(lda08_demogr, newdata = testing)
confusionMatrix(pred_lda08_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree08 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set08,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree08, uniform = TRUE, margin = 0.2)
text(tree08, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree08, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set08$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_08.jpeg', quality = 080, type = "cairo")
par(mfrow = c(2,3))
plot(set08$radio ~ set08$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set08$tv ~ set08$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set08$newspapers ~ set08$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set08$magazines ~ set08$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set08$internet ~ set08$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_08.jpeg', quality = 080, type = "cairo")
par(mfrow = c(2,2))
plot(set08$cluster ~ factor(set08$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_08.jpeg', quality = 080, type = "cairo")
par(mfrow = c(2,2))
plot(set08$cluster ~ factor(set08$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=12000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set08$cluster ~ set08$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
plot(set08$cluster ~ set08$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
