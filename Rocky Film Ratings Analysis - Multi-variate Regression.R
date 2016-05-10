###-----Part 1-----###
#a) load the data
Rocky <- read.csv("recommendDB.csv")[,-1]
#b) define data frame, fill with NA
consumer <- sort(unique(Rocky$consumerID))
rating <- rep(NA, length(consumer))
finalDB <- data.frame(r1=rating, r2=rating, r3=rating, r4=rating, r5=rating)
#c) loop to replace data
row <- 1
ptm <- proc.time()
for (id in consumer){
    oneConsumer <- Rocky[Rocky$consumerID==id,]
    for(movieID in oneConsumer$rockyID){
        finalDB[row,movieID] = oneConsumer[oneConsumer$rockyID==movieID,2]
    }
    row <- row+1
}
proc.time() - ptm 
#user 428.42; elapsed 502.41 seconds


###-----Part 2-----###
#a) correlations between ratings
correlations <- cor(finalDB, method = "pearson", use = "pairwise.complete.obs")
print(round(correlations,4))
#b) mean rating of each movie
allColMeans <- colMeans(finalDB, na.rm = TRUE)
print(round(allColMeans,2))
#c) subset those who rated Rocky4 & mean rating of each movie & changed or not
whoRatedR4 <- finalDB[!is.na(finalDB$r4), ]
R4ColMeans <- colMeans(whoRatedR4, na.rm = TRUE)
print(round(R4ColMeans,2))
print(round(allColMeans - R4ColMeans,2))
#d) subset those who rated all movies
whoRatedAll <- finalDB[!rowSums(is.na(finalDB)), ]

###-----Part 3-----###
#a) comment on code
##--1--##
rocky1Vec <- c('r1','I(r1^2)')
rocky2Vec <- c('r2','I(r2^2)')
rocky3Vec <- c('r3','I(r3^2)')
rocky4Vec <- c('r4','I(r4^2)')
# all the above code is prepared for building linear and quadratic terms in prediction models
##--2--##
fullSet <- expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec)
print(fullSet)
# the above code creates a data frame containing 16 rows, 
# one for each combination of the rocky vectors;
# i.e. the 1st row contains r1 r2 r3 r4 ;
# the 2nd row contains I(r1^2) r2 r3 r4 ;
# the 4th row contains I(r1^2) I(r2^2) r3 r4 ;
# this is prepared for building predictors in prediction models.
##--3--##
formulaSet <- paste("r5~", apply(fullSet, 1, paste, collapse='+'))
print(formulaSet)
# the above code concatenates the 'dependent'(rocky5) variable and 'independent' variables,
# creating 16 strings in one vector;
# each string in this vector will be looped through as a formula in regression models.
##--4--##
for (i in 1:nrow(fullSet)){
    print(lm(as.formula(formulaSet[i]), data = whoRatedAll))
}
# the above code runs 16 times of regression using different combinations of predictors,
# and prints out the result of each regression model.
###------------------------###
###---enhance above code---###
#b) treat rocky1 as a categorical variable
rocky1Fac <- c('r1','I(r1^2)','I(factor(r1))')
fullSet2 <- expand.grid(rocky1Fac,rocky2Vec,rocky3Vec,rocky4Vec)
print(fullSet2)
formulaSet2 <- paste("r5~", apply(fullSet2, 1, paste, collapse='+'))
print(formulaSet2)
#c) AIC & BIC
# create data frame to store AIC, BIC, and out of sample MSE
toReplace <- rep(NA, 24)
performance <- data.frame(AIC=toReplace,BIC=toReplace, out.of.sample.MSE=toReplace)
for (i in 1:nrow(fullSet2)){
    lm <- lm(as.formula(formulaSet2[i]), data = whoRatedAll)
    performance[i,"AIC"] <- AIC(lm)
    performance[i,"BIC"] <- BIC(lm)
}
#d) hold out sample (test) (10%) for evaluation
random <- order(runif(nrow(whoRatedAll)))
training <- subset(whoRatedAll,random < .9 * nrow(whoRatedAll))
test <- subset(whoRatedAll,random >= .9 * nrow(whoRatedAll))
for (i in 1:nrow(fullSet2)){
    lm <- lm(as.formula(formulaSet2[i]), data = training)
    performance[i,"out.of.sample.MSE"] <- mean((test$r5-predict(lm,test))^2)
}
#e) compare results
print(performance)
write.csv(performance, file = "performance.csv")
