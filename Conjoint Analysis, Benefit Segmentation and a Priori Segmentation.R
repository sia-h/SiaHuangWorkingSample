setwd("D:/R_wd")

colnames(desmat) <- c("price", "height","motion", "style")
write.csv(desmat, file = "desmat.csv")

summary(lm(ratings~desmat))

desmatf <- cbind(rep(1,nrow(desmat)),desmat); 
partworths <- matrix(nrow=sampsize,ncol=ncol(desmatf))
for(i in 1:sampsize){
  partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
head(partworths)

# segmenting individuals
library(cluster) 
library(fpc)

######-----cluster analysis-----######
toclust <- partworths;
pm1 <- pamk(toclust,scaling=TRUE)
pm1
# get the optimal number of cluster
pm1$nc #3

wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km1 <- kmeans(toclust,3,iter.max = 20, nstart=2)
print(km1)
# transfer into excel
write.csv(km1$centers, file = "km1.center.csv")

plotClust <- function(km1,discPlot=FALSE){
  nc <- length(km1$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(1,3))}
  percsize <- paste(1:nc," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
  pie(km1$size,labels=percsize,col=1:nc)
  
  clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toclust, km1$cluster,col=km1$cluster); #plot against discriminant functions ()
  }
  rng <- range(km1$centers)
  dist <- rng[2]-rng[1]
  locs <- km1$centers+.05*dist*ifelse(km1$centers>0,1,-1)
  bm <- barplot(km1$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km1$centers,format="f",digits=1))
}
plotClust(km1)

km2 <- kmeans(toclust,2,iter.max = 20, nstart=2)
km2$centers
write.csv(km2$centers, file = "km2.center.csv")
print(km2)
attLevs

percsize <- paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km1$size,labels=percsize)

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km1$cluster); #plot against discriminant functions ()

# 5 clusters
percsize <- paste(1:2," = ",format(km2$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)

clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km2$cluster);
# 3 cluster
clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0); #plot clusters against principal components
plotcluster(toclust, km1$cluster)

# draw regression
km1m <- aggregate(toclust, by = list(km1$cluster), FUN = mean)
km2m <- aggregate(toclust, by = list(km2$cluster), FUN = mean)

rscale <- function(x){(x-min(x))/(max(x)-min(x));}
km1ms <- apply(km1m[,2:ncol(km1m)],2,rscale)
par(mar=c(8.1,4.1,4.1,2.1))
matplot(t(km1ms), col = c(1,4,2), ylab = "Mean Value (Range Normalized)", xaxt = "n")


#####-----a Priori segment age&gender-----#####

summary(lm(ratings~desmat*ageD))
summary(lm(ratings~desmat*genderD));
summary(lm(ratings~desmat,subset=genderD==1))
summary(lm(ratings~desmat,subset=genderD==0))


#####-----predicting missing cells (preparing for market simulation)-----#####

## repeat individual level partworths for multiplication
partworths
partworths.full <- matrix(rep(partworths,each=16),ncol=5)
pratings <- rowSums(desmatf*partworths.full)
finalratings <- ifelse(is.na(ratings),pratings,ratings); #combining actual when available and predicted ratings

attLevs

## market simulation
## a scenario is a set of products, each with a set of levels. 
## create a vector with the indexes for the product profiles from the status quo
scen0 <- c(7,13,15,1)
scen1 <- c(8,14,15,2)
scen2 <- c(7,15,3)
scen3 <- c(8,16,4)
scen4 <- c(7,13,15,1,5)
scen5 <- c(7,13,15,1,6)
scen6 <- c(8,14,15,2,6)
scen7 <- c(7,15,3,5)
scen8 <- c(8,16,4,6)
scen9 <- c(7,15,3,6)
scencomp <- c(7,5,13)

## market simulations
## tranform final ratings into matrix
simDecInput <- matrix(finalratings,nrow=nprofiles) ##this has 16 rows for profiles and sampsize columns

simDec <- function(inputmat,scen){
  inmkt <- inputmat[scen,]
  max <- apply(inmkt,2,max)
  firstChoices <- (inmkt==rep(max,each=length(scen)))
  shares <- firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}

simDec0 <- simDec(simDecInput,scen0) 
simDec0
simDec1 <- simDec(simDecInput,scen1) 
simDec1
simDec2 <- simDec(simDecInput,scen2)
simDec2
simDec3 <- simDec(simDecInput,scen3)
simDec3
simDec4 <- simDec(simDecInput,scen4)
simDec4
simDec5 <- simDec(simDecInput,scen5)
simDec5
simDec6 <- simDec(simDecInput,scen6)
simDec6
simDec7 <- simDec(simDecInput,scen7)
simDec7
simDec8 <- simDec(simDecInput,scen8)
simDec9 <- simDec(simDecInput,scen9)
simDeccomp <- simDec(simDecInput,scencomp)

## inputmat and scen is as above. myprods are indicators of which prods are the firms, 
## prices are the prices for all products, vcosts are the variable costs
## fcosts are the fixed costs for the firm (need to calculate in already the number of products)
simProfit <- function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
  mktshr <- simDec(inputmat,scen);
  vprofit <- mktshr * (prices-vcosts)*mktsize;
  sum(vprofit[myProds])-fcosts
}

simProf0 <- simProfit(simDecInput,scen0,c(2,3,4),c(119.99,119.99,119.99,119.99),c(41,33,41,21),20000,4000)
simProf1 <- simProfit(simDecInput,scen1,c(2,3,4),c(95.99,95.99,119.99,95.99),c(41,33,41,21),20000,4000)
simProf2 <- simProfit(simDecInput,scen2,c(2,3),c(119.99,119.99,119.99),c(41,41,29),20000,4000)
simProf3 <- simProfit(simDecInput,scen3,c(2,3),c(95.99,95.99,95.99),c(41,41,29),20000,4000)
simProf4 <- simProfit(simDecInput,scen4,c(2,3,4,5),c(119.99,119.99,119.99,119.99,119.99),c(41,33,41,21,33),20000,4000)
simProf5 <- simProfit(simDecInput,scen5,c(2,3,4,5),c(119.99,119.99,119.99,119.99,95.99),c(41,33,41,21,33),20000,4000)
simProf6 <- simProfit(simDecInput,scen6,c(2,3,4,5),c(95.99,95.99,119.99,95.99,95.99),c(41,33,41,21,33),20000,4000)
simProf7 <- simProfit(simDecInput,scen7,c(2,3,4),c(119.99,119.99,119.99,119.99),c(41,41,29,33),20000,4000)
simProf8 <- simProfit(simDecInput,scen8,c(2,3,4),c(95.99,95.99,95.99,95.99),c(41,41,29,33),20000,4000)
simProf9 <- simProfit(simDecInput,scen9,c(2,3,4),c(119.99,119.99,119.99,95.99),c(41,41,29,33),20000,4000)
simProfcomp <- simProfit(simDecInput,scencomp,c(2,3),c(139.99,139.99,139.99),c(41,33,33),40000,4000)

print(simProf0)
print(simProf1)
print(simProf2)
print(simProf3)
print(simProf4)
print(simProf5)
print(simProf6)
print(simProf7)
print(simProf8)
print(simProf9)

