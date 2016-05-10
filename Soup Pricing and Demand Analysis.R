###-----Part 1-----###
#a) load the datasets
itemDF <- read.csv("itemDataFrame.csv")[,2:7]
consumerDF <- read.csv("consumerDataFrame.csv")[,2:7]

#b) readable names
names(itemDF) <- c("volPerUnit", "isChicken","isPrivateLabel","flavorName","isTomato","flavorID")
names(consumerDF) <- c("units","dollars","weekNum","storeID", "panelistID", "flavorID")

#c) merge
# for loop
all <- data.frame()
for (i in 1:nrow(consumerDF)){
    ID <- consumerDF[i,"flavorID"]
    colBind <- cbind(consumerDF[i,], itemDF[itemDF$flavorID==ID, 1:5])
    all <- rbind(all, colBind)
}

# built-in function
allDF <- merge(itemDF, consumerDF, by="flavorID")

#d) new variables
allDF$pricePerUnit <- allDF$dollars / allDF$units
allDF$totalVolume <- allDF$volPerUnit * allDF$units

#e) save database
save(allDF, file="finalDB.Rdata")

#f) descriptive statistics and market share
summary(allDF)
lapply(allDF,mean)
lapply(allDF, sd)
# market share function
share <- function(DataFrame){
    units <- DataFrame$units
    all <- sum(units)
    marketShare <- units/all*100 #in %
}
# market share for each store
storeSales <- aggregate(units~storeID, allDF, sum)
storeShare <- data.frame(storeSales$storeID, share(storeSales))
storeShare[order(storeShare$share.storeSales., decreasing = TRUE),] #in %
# market share for top 10 flavors
flavorSales <- aggregate(units~flavorID, allDF, sum)
flavorShare <- data.frame(flavorSales$flavorID, share(flavorSales))
top10flavorShare <- flavorShare[order(flavorShare$share.flavorSales., decreasing = TRUE),][1:10,]
top10flavorShare #in %
flavorIdentifier <- allDF[,c("flavorID","flavorName")]
# market share for the leading brand and for private label
BrandSales <- aggregate(units~isPrivateLabel, allDF, sum)
leadingBrandShare <- BrandSales[1,2]/sum(BrandSales$units)*100 
leadingBrandShare #in %
privateBrandShare <- BrandSales[2,2]/sum(BrandSales$units)*100 
privateBrandShare #in %

###-----Part 2-----###
#a) metric: average purchase price
avgPurchasePrice <- aggregate(pricePerUnit~panelistID, allDF, mean)
colnames(avgPurchasePrice) <- c("panelistID", "averagePurchasePrice")

#b) my metric: linear.regression.coefficient * P / Q >>> elasticity metric
# create vector to host coefficients for each consumer
panelist <- unique(allDF$panelistID)
toReplace <- rep(NA,length(panelist))
priceCoefByConsumer <- data.frame(panelist, toReplace)
colnames(priceCoefByConsumer) <- c("ConsumerID","Coefficient")
# start a for loop - for each consumer >>> result is  data frame of coefficient
for(id in panelist){
    #Subset each consumer
    consumerData <- allDF[allDF$panelistID==id, c("units","pricePerUnit")]
    #Run the regression on this subset
    if (nrow(consumerData) > 1) {
        consumerLM <- lm(units~pricePerUnit,data=consumerData)
        #Get the coefficient
        priceCoefByConsumer[priceCoefByConsumer$ConsumerID==id, "Coefficient"] <- consumerLM$coefficients[2]
    }
}
# build up metric
colnames(priceCoefByConsumer) <- c("panelistID", "Coefficient")
metric <- merge(priceCoefByConsumer, avgPurchasePrice, by="panelistID")
metric$elasticity <- metric$Coefficient * metric$averagePurchasePrice
# the elasticity column of metric is my result

#c) scatter plot comparing my metric and average purchase price metric
library(ggplot2)
ggplot(metric, aes(x=averagePurchasePrice, y=elasticity)) +
    geom_point(colour="midnightblue") +
    labs(x="Average Purchase Price", y="Elasticity") +
    ggtitle("Scatter Plot Comparing My Metric of Elasticity and Metric of Average Purchase Price")

#f) my choice of metric is: my elasticity metric
absoluteValue <- abs(metric$elasticity)
sensitiveRank <- metric[order(absoluteValue, decreasing = TRUE),]
top100sensitive <- sensitiveRank[1:100,"panelistID"]
top100sensitive

###-----Part 3-----###
# a) consumer segment respond to price promotions
allSensitive <- sensitiveRank[sensitiveRank$elasticity>1|sensitiveRank$elasticity< -1, ]
sensitiveDB <- merge(allSensitive, allDF, by="panelistID")
sensitiveDB$count <- rep(1, length(sensitiveDB$panelistID))
# find the most popular stores sensitive consumers visits
popularStore <- aggregate(count~storeID, sensitiveDB, sum)
popularStoreOder <- popularStore[order(popularStore$count, decreasing = TRUE),]
popularStoreOder
# popularStoreList: ID5,7,2
# find the popular flavor names in each store
popularFlavor <- aggregate(count~storeID+flavorName, sensitiveDB, sum)
popularFlavorOrder <- popularFlavor[order(popularFlavor$count, decreasing = TRUE), ][popularFlavor$storeID==5|7|2,]
popularFlavorOrder[1:10,]
