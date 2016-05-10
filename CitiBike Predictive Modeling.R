library(doParallel)
registerDoParallel(cores = 4)

######################################################################
#Read data (combine all files), sampling with 2% of each file.
######################################################################
year <- c(201307:201312, 201401:201412, 201501:201512)
sample <- data.frame()
for (i in year){
    filename <- paste(i,"-citibike-tripdata.csv", sep= "")
    print(1)
    database <- read.csv(filename, stringsAsFactors = FALSE)
    set.seed(67)
    sampleIndex <- sample(1:nrow(database), nrow(database)*0.02)
    sampleSubset <- database[sampleIndex,]
    sample <- rbind(sample,sampleSubset)
}
#clean workspace
rm(database, sampleSubset, filename, i, sampleIndex, year)

######################################################################
#Basic cleaning: age, trip duration in minutes, overage.
######################################################################
#clean age
sample$age <- 2016-as.numeric(sample$birth.year)
#clean trip duration
sample$TDminutes <- sample$tripduration/60
summary(sample$TDminutes) 
summary(sample[sample$TDminutes>45,"TDminutes"]) #determine cut-off level?
#set up overage column
sample$overage <- sample$TDminutes>45
nrow(sample[sample$overage==TRUE,])
#save
save.image(file = "sample.RData")
write.csv(sample, file = "sample.csv")

######################################################################
#Calculate the distance between start-station and stop-station,
#which is treated as trip distance (in meter).
######################################################################
library(geosphere)
sample$distance <- distHaversine(matrix(sample$start.station.latitude,sample$start.station.longitude, nrow = 461112, ncol = 2),
                                 matrix(sample$end.station.latitude,sample$end.station.longitude, nrow = 461112, ncol = 2))

###############################################################################
#Create a column for date for later conversion of weather.
#Only convert starttime to date because if weather is a influencer, the weather
#on start date determines whether a consumer will delay returning bike.
#For how long they will delay, this is determined by the weather in between
#start date and stop date, which is not discussed in this case.
###############################################################################
sample$date <- NA
transform <- as.Date(sample$starttime[1:208145], format = "%Y-%m-%d")
sample$date[1:208145] <- transform
#because of differences in format, seperately deal with date
transform <- as.Date(sample$starttime[208146:461112], format = "%m/%d/%Y")
sample$date[208146:461112] <- transform
sample$date = as.Date(sample$date, origin = "1970-01-01")
summary(sample$date)
rm(transform)

######################################################################
#Categorize to quarter
######################################################################
library(zoo)
sample$quarter <- NA
Temp <- as.Date(sample$stoptime[1:208145],"%Y-%m-%d")
Temp <- quarters(Temp)
Temp <- substr(Temp,2,2)
#because of differences in format, seperately deal with date
sample$quarter[1:208145] <- Temp
Temp <- as.Date(sample$stoptime[208146:461112], format = "%m/%d/%Y")
Temp <- quarters(Temp)
Temp <- substr(Temp,2,2)
sample$quarter[208146:461112] = Temp
rm(Temp)
summary(sample$quarter)
sample$quarter <- as.factor(sample$quarter)

save.image(file = "sample_v2.RData")
write.csv(sample, file = "sample_v2.csv")

######################################################################
#create column for weather
######################################################################
weatherData <- read.csv("weather.data.csv")
weatherData$date <- as.Date(as.character(weatherData$date), format = "%Y/%m/%d")
summary(weatherData$date)
sample.sample <- merge(x=sample, y=weatherData, by="date", all.x = TRUE)
save(sample.sample, file = "merged.RData")

######################################################################
#correcting variable class, remove the NAs
#and take train and test data while preserving the class distribution
######################################################################
sample <- sample.sample
sample <- sample[is.na(sample$precipitation)==FALSE,]
sample <- sample[is.na(sample$average_wind_speed)==FALSE,]
sample <- sample[is.na(sample$age)==FALSE,]
sample$usertype <- as.factor(sample$usertype)
sample$gender <- as.factor(sample$gender)


library(caret)
set.seed(41)
train <- createDataPartition(y=sample$overage, 
                             p = 0.66667,  
                             list=F)     
sample.train <- sample[train,]
sample.test <- sample[-train,]

######################################################################
#take a look at the dataset using logistic regression 
######################################################################
fitsample <- sample[,-2]
fit.glm <- glm(overage~.,data=fitsample, family=binomial)
summary(fit.glm)

######################################################################
#since our data set is unbalanced
#we use SMOTE technique to create a balanced training dataset
######################################################################
library(unbalanced)
set.seed(288)
bal <- ubBalance(sample.train[,-4], sample.train$overage, 
                type='ubSMOTE', positive='yes', 
                percOver=200, 
                percUnder=200,  
                k=5,
                verbose=T)
bal.train <- cbind(bal$X, overage=bal$Y)
prop.table(table(bal.train$overage))
detach("package:unbalanced", unload=TRUE)

######################################################################
#using rpart to generate a basic tree model and post prune it
######################################################################
library(rpart)
fit <- rpart(overage ~ ., 
             data=bal.train, method="class",
             control=rpart.control(xval=10, minsplit=10, cp=0))
nrow(fit$frame) 
plot(fit, uniform=T, branch=0.5, compress=T,
     main="Tree for Default", margin=0.1)
text(fit,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=1.2)
 
plotcp(fit, upper="size")
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
fit.post <- prune.rpart(fit, cp= 0.0003958828)
nrow(fit.post$frame)
plot(fit.post, uniform=T, branch=0.5, compress=T,
     main="Tree for Default", margin=0.1)
text(fit.post,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=1.2)

overage.pred <- predict(fit.post, sample.test, type="class")
overage.actual <- sample.test$overage
cm.tree <- table(overage.actual, overage.pred)
cm.tree
confusionMatrix(cm.tree)
#accuracy: 87.53%, kappa: 13.73%
#the kappa of the post pruned tree model is pretty low

######################################################################
# cross-validation with a tree model on smoted sample
######################################################################
library(caret)
trc <- trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 5,
                    classProbs = TRUE,
                    allowParallel = T)
set.seed(689)    # for better comparison
tune.bal <- train(overage ~ ., data=bal.train, 
                  method = "rpart", 
                  metric = 'Kappa',
                  control=rpart.control(minsplit=2, xval=0),
                  tuneGrid = data.frame(cp=c(0.0001, 0.0005, 0.001, 0.0015)),
                  tuneLength = 20,
                  trControl = trc)
tune.bal
plot(tune.bal)
# the best tree
fit.tree = tune.bal$finalModel
nrow(fit.tree$frame) 
plot(fit.tree, uniform=TRUE, branch=0.5, 
     main="Classification Tree for Marketing", margin=0.1)
text(fit.tree,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F,  cex=0.6)

# check on the test set using test data
pred.bal <- predict(tune.bal, sample.test, type='raw')
cm.out <- confusionMatrix(pred.bal, sample.test$overage, positive='yes')
cm.out
# Accuracy: 89.36%, kappa: 15.67%

######################################################
# Reweighting the original dataset and the tree model 
######################################################
myweights <- ifelse(sample.train$overage=='no', 0.05, 1.0) 
set.seed(100)    # for better comparison 
tune.w <- train(overage ~ ., data=sample.train, 
               method="rpart",
               metric="Kappa",
               control=rpart.control(minsplit = 200,xval=0),
               tuneLength=20,
               weights=myweights,
               trControl=trc)
tune.w
plot(tune.w)
fit.tree=tune.w$finalModel
plot(fit.tree,uniform = TRUE,branch = 0.5,
     main="Classification Tree for Marketing",margin = 0.1)
text(fit.tree,use.n=F,pretty=F,cex=0.6)
nrow(fit.tree$frame)
pred.w <- predict(tune.w, sample.test, type='raw') 
cm.out <- confusionMatrix(pred.w, test$overage, positive='yes') 
cm.out 
# Accuracy: 94.45%, kappa: 25% highest kappa score

######################################################
# SVM
######################################################
# kernel: radial 
library(e1071)
set.seed(41)
# since svm takes long time to run, we take a small subset to shorten the time
small <- sample(1:nrow(sample.train),nrow(sample.train)*0.05)
sample.small <- sample.train[small,]

# polynomial kernel
tune.def <- tune(svm, overage ~ usertype + precipitation + gender + starthour + distance, 
                 data=sample.small,
                 kernel='polynomial',
                 ranges=list(cost=c(0.001,0.01,0.1,1,10,100, 1000)))
ggplot(tune.def$performance, aes(x=cost, y=error)) +
  geom_line() + scale_x_log10() +
  theme(text = element_text(size=20))
# cost=1 seems to have the lowest error rate

# radial
set.seed(623)
tune.def <- tune(svm, overage ~ usertype + precipitation + gender + starthour + distance, 
                 data=sample.small,
                 kernel='radial',
                 ranges=list(cost=c(0.001,0.01,0.1,1,10,100, 1000)))
ggplot(tune.def$performance, aes(x=cost, y=error)) +
  geom_line() + scale_x_log10() +
  theme(text = element_text(size=20))
# cost =1 seems to have the lowest error rate

# linear
set.seed(623)
tune.def <- tune(svm, overage ~ usertype + precipitation + gender + starthour + distance, 
                 data=sample.small,
                 kernel='linear',
                 ranges=list(cost=c(0.001,0.01,0.1,1,10,100, 1000)))
ggplot(tune.def$performance, aes(x=cost, y=error)) +
  geom_line() + scale_x_log10() +
  theme(text = element_text(size=20))
# cost =1 seems to have the lowest error rate

# we choose the polynomial kernel since it has the lowest error rate
fit.def <- svm(overage ~ usertype + precipitation + gender + starthour + distance, 
              data=sample.small,
              kernel='polynomial', cost=1,
              degree=3, coef0=50, gamma=1/2,
              class.weights=c(no=0.05, yes=1))   
summary(fit.def)

overage.pred <- predict(fit.def, sample.test[,-4])
cm <- confusionMatrix(overage.pred, sample.test$overage)
cm
# Accuracy: 88.79%, kappa: 15%
