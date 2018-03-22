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
library(ggplot2)

#  read in datasets
set08 <- readRDS("set08.rds")
set08_simple <- readRDS("set08_simple.rds")

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

## consider kmeans
wss <- vector()
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 10,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2008.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans08 <- kmeans(set08[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)
set.seed(56)
kmeans08_simple <- kmeans(set08_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)

table(kmeans08$cluster)

# Comparing 2008 with 2010... will change colours if necessary to reflect meaning based on 2012:

# green becomes red:  2 becomes 1
# red becomes blue: 1 becomes 3
#  lilac becomes green: 4 becomes 2
#  blue becomes lilac: 3 becomes 4
kmeans08$cluster <- ifelse(kmeans08$cluster == 1, 8, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 2, 6, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 3, 9, kmeans08$cluster)
kmeans08$cluster <- ifelse(kmeans08$cluster == 4, 7, kmeans08$cluster)
kmeans08$cluster <- kmeans08$cluster - 5


# add cluster labels to the dataset
set08c <- set08 %>%
        mutate(cluster = factor(kmeans08$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())
# 
set08c_simple <- set08_simple %>% ### sort out bloody internet thingy
        mutate(cluster = factor(kmeans08_simple$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

saveRDS(set08c, "set08c.rds")
saveRDS(set08c_simple, "set08c_simple.rds")

set08c <- readRDS("set08c.rds")
set08c_simple <- readRDS("set08c_simple.rds")

# some plots
# boxplots of clusters and media types
p1 <- ggplot(set08c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set08c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set08c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set08c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set08c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set08c, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_08.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5,p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set08c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set08c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set08c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set08c, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_08.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set08c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set08c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set08c, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d8 <- ggplot(set08c, aes(lifestyle, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_08.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, d8, ncol=2, nrow = 2)
dev.off()






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
jpeg('typeBoxPlots_08.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set08$radio ~ set08$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set08$tv ~ set08$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set08$newspapers ~ set08$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set08$magazines ~ set08$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set08$internet ~ set08$cluster, col = c(2,3,4,6), main = "internet", xlab = "cluster", ylab = '')
plot(set08$all ~ set08$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_08.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set08$cluster ~ factor(set08$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_08.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set08$cluster ~ factor(set08$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set08$cluster ~ factor(set08$hh_inc, labels = c("<5000","5000-10999","11000-19999",">=20000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
plot(set08$cluster ~ set08$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "")
plot(set08$cluster ~ set08$lifestyle, col = c(2,3,4,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
