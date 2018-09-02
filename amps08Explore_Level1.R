# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
library(gridExtra)
library(ggplot2)

#  read in datasets
set08 <- readRDS("set08.rds")

# consider some correlations
jpeg('corTypePlot2008.jpeg')
corrplot(cor(set08[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeansTypePlot2008.jpeg')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans08 <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 4,
                   iter.max = 100)

table(kmeans08$cluster)

# align with interpretation of 2012....
# green to lilac:  2 to 4
# lilac to red: 4 to 1
# blue to blue: 3 to 3
# red to green: 1 to 2
kmeans08$cluster <- ifelse(kmeans08$cluster == 1, 7, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 2, 9, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 3, 8, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 4, 6, kmeans08$cluster)
kmeans08$cluster <- kmeans08$cluster - 5

# add cluster labels to the dataset
set08c <- set08 %>%
        mutate(cluster = factor(kmeans08$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save them
saveRDS(set08c, "set08c.rds")
# read back
set08c <- readRDS("set08c.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types

boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_08.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set08c, type = "all"),
             boxplot(set08c, type = "newspapers"),
             boxplot(set08c, type = "magazines"),
             boxplot(set08c, type = "radio"),
             boxplot(set08c, type = "tv"),
             boxplot(set08c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set08c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster

bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2008"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2008"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2008"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2008"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2008"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2008"
        }
        
        ggplot(data = set08c, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_08.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set08c, "sex"),
             bars_by_cluster(set08c, "age"),
             bars_by_cluster(set08c, "race"),
             bars_by_cluster(set08c, "edu"),
             bars_by_cluster(set08c, "hh_inc"),
             bars_by_cluster(set08c, "lsm"),
             ncol=2, nrow = 3)
dev.off()

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub08 <- set08c[sample(nrow(set08c), size = 1000),]

# distance matrix and MDS
sub08_dist <- dist(sub08[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds08 <- cmdscale(sub08_dist)
plot(mds08, col = as.numeric(sub08$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub08[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub08$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2008.jpeg')
plot(mds08, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set08c$cluster, p = 0.7, list = FALSE)
training <- set08c[ind_train,]
testing <- set08c[-ind_train,]

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
confusionMatrix(pred_lda08$class, testing$cluster) # collinearity meant took out 

# using only demographic information
forest08_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lsm,
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
                    + lsm,
                    data = training)

pred_lda08_demogr <- predict(lda08_demogr, newdata = testing)
confusionMatrix(pred_lda08_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree08 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set08c,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree08, uniform = TRUE, margin = 0.2)
text(tree08, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree08, type = 4, extra = 1, cex = 0.5)