library(randomForest)

#join episode statistcs with demographic data
data <- read.csv("QueensList.csv")
data$Town_Pop <- as.numeric(data$Town_Pop)

#Calculate proportion of instragram followers by season
data$Insta_total <- ave(data$Instagram, data$Season, FUN = sum, na.rm=TRUE)
data$Insta_perc <- data$Instagram / data$Insta_total

#Classify Queens by outcome (Top, Middle, Bottom)
data$Class <- ifelse(data$Outcome == "Winner" | data$Outcome == "Runner-up", "TOP", 
                     ifelse(data$Outcome == "TBD", "TBD", "Bottom"))

#non-season 12 queens
rf_data <- data[data$Outcome!="TBD",]
rf_data$Class <- as.factor(rf_data$Class) 

##RANDOM FORESTS#
features = c('State','Town_Pop','Age','Ethnicity','Type','perc_win','perc_btm','Insta_perc','Class')
#select traning data
train_index <- sample(1:nrow(rf_data),80)
train_data <- rf_data[train_index, features]
#train rf
statRF <- randomForest(formula = Class ~ ., data = train_data,mtry=5, ntree=400,importance=TRUE)
statRF
#Variable importance plot
varImpPlot(statRF)

#select testing data
test_data <- rf_data[-train_index, features]
#prediction classes
testRF <- predict(statRF, test_data)
#confusion matrix
table(observed=test_data[,9],predicted=testRF)
#prediction class probabilities
testRF_prob <- predict(statRF, test_data, type="prob")
#Who was misclassified???
test_pred <- data.frame(test_data$Contestant, test_data$Class, testRF, testRF_prob)

##SEASON 12 PREDICTIONS##
seas12 <- data[data$Outcome=='TBD',]
features_12 = c('State','Town_Pop','Age','Ethnicity','Type','perc_win','perc_btm','Insta_perc')
seas12_pred <- predict(statRF,seas12[,features_12],type="prob")

seas12 <- cbind(seas12,seas12_pred)
seas12 <-write.csv(seas12,'seas12_predEp3.csv' )

