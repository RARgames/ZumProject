library(devtools)

options(encoding = "UTF-8"); par(ask=F)
dmrpkglist<-c('dmr.data','dmr.util','dmr.claseval','dmr.stats','dmr.trans','dmr.linreg','dmr.regeval','dmr.dissim',
              'dmr.dectree','dmr.linclas','dmr.disc','dmr.kcenters','dmr.cluseval','dmr.regtree','dmr.attrsel',
              'dmr.ensemble','dmr.kernel','dmr.bayes','dmr.hierclus','dmr.miscost','dmr.rpartutil')
pkgcheck <- dmrpkglist %in% row.names(installed.packages())
dmrpkglist[!pkgcheck]
for(i in dmrpkglist[!pkgcheck]){install_github(paste("42n4/", i, sep=""),force = TRUE)}
dmrpkglist<-c("dmr.util",
              "dmr.trans",
              "dmr.claseval",
              "dmr.rpartutil")
for(i in dmrpkglist) library(i, character.only = TRUE);

pkglist<-c("rpart",
           "rpart.plot",
           "randomForest")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#COMMMENT the line below if you installed packages earlier e.g on root
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#this command is for root instalation of missing packages:
if(length(pkglist[!pkgcheck])) cat("install.packages(c(");j=0; for(i in pkglist[!pkgcheck]) { j<-j+1 ;  if(j == length(pkglist[!pkgcheck])) cat(paste('"',i,'"',sep="")) else cat(paste('"',i,'",',sep=""));} ; cat("),depend=TRUE)")
#loading all libraries - necessary to avoid errors of execution
for(i in pkglist) library(i, character.only = TRUE);




#Read data
data <- read.table("marvel-wikia-data.csv", sep = ",", stringsAsFactors = TRUE)
#Assigning names from the first row
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
#Remove attributes that are not needed
data$page_id <- NULL
data$name <- NULL
data$urlslug <- NULL
data$GSM <- NULL


#TODO CHANGE TO CORRECT FACTORS
data$APPEARANCES  <- NULL
data$`FIRST APPEARANCE`  <- NULL
data$Year <- NULL


#Divide data into training and testing sets
rci <- runif(nrow(data))
training <- data[rci>=0.33,]
testing <- data[rci<0.33,]

#ROC - receiver operating characteristics 
roc <- function(pred.s, true.y)
{
  cutoff <- Inf  # start with all instances classified as negative
  tp <- fp <- 0
  tn <- sum(2-as.integer(true.y))  # all negative instances
  fn <- sum(as.integer(true.y)-1)  # all positive instances
  rt <- data.frame()
  
  sord <- order(pred.s, decreasing=TRUE)  # score ordering
  for (i in 1:length(sord))
  {
    if (pred.s[sord[i]] < cutoff)
    {
      rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
      cutoff <- pred.s[sord[i]]
    }
    p <- as.integer(true.y[sord[i]])-1  # next positive classified as positive
    n <- 2-as.integer(true.y[sord[i]])  # next negative classified as positive
    tp <- tp+p
    fp <- fp+n
    tn <- tn-n
    fn <- fn-p
  }
  rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
}


#Rpart Tree (cp = 0.01 minsplit = 1)
set.seed(12354)
#classifier
ci.tree.d <- rpart(ALIGN~., training, minsplit = 1, cp = 0.01)
#prunning of the tree
prp(ci.tree.d)
plotcp(ci.tree.d)
printcp(ci.tree.d)
ci.tree.d.pruned<-prune(ci.tree.d, minsplit = 1, cp = 0.01) #complexity parameter
prp(ci.tree.d.pruned)
#predicted values
ci.tree.d.pred <- predict(ci.tree.d, testing, type="c")
#misclassification error for given vectors of predicted and true class labels 
err(ci.tree.d.pred, testing$ALIGN)
#confusion matrix
ci.tree.d.cm <- confmat(ci.tree.d.pred, testing$ALIGN)
ci.tree.d.cm
#complementary pairs tpr - fpr, precision - recall, sensitivity - specificity
ci.tree.d.tpr <- tpr(ci.tree.d.cm)
ci.tree.d.tpr
ci.tree.d.fpr <- fpr(ci.tree.d.cm)
ci.tree.d.fpr
ci.tree.d.fmeasure <- f.measure(ci.tree.d.cm)
ci.tree.d.fmeasure 
#ROC
ci.tree.d.prob<-predict(ci.tree.d, testing)[,2]
ci.tree.d.roc <- roc(ci.tree.d.prob, testing$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)





#NOWE-------------------
pkglist<-c("rpart", "rpart.plot", "randomForest")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#COMMMENT the line below if you installed packages earlier e.g on root
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#this command is for root instalation of missing packages:
if(length(pkglist[!pkgcheck])) cat("install.packages(c(");j=0; for(i in pkglist[!pkgcheck]) { j<-j+1 ;  if(j == length(pkglist[!pkgcheck])) cat(paste('"',i,'"',sep="")) else cat(paste('"',i,'",',sep=""));} ; cat("),depend=TRUE)")
#loading all libraries - necessary to avoid errors of execution
for(i in pkglist) library(i, character.only = TRUE);

install_version("caret", version = "5.10-13", repos = "http://cran.us.r-project.org")
library("caret")

#Read data
data <- read.table("marvel-wikia-data.csv", sep = ",", stringsAsFactors = TRUE)
#Assigning names from the first row
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
#Remove attributes that are not needed
data$page_id <- NULL
data$name <- NULL
data$urlslug <- NULL
data$GSM <- NULL


#TODO CHANGE TO CORRECT FACTORS
data$APPEARANCES  <- NULL
data$`FIRST APPEARANCE`  <- NULL
data$Year <- NULL


#Divide data into training and testing sets
rci <- runif(nrow(data))
training <- data[rci>=0.33,]
testing <- data[rci<0.33,]

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

#Rpart Tree (cp = 0.01 minsplit = 1)
set.seed(12354)
#classifier
ci.tree.d <- rpart(ALIGN~., training, minsplit = 1, cp = 0.01)
#prunning of the tree
prp(ci.tree.d)
plotcp(ci.tree.d)
printcp(ci.tree.d)
ci.tree.d.pruned<-prune(ci.tree.d, minsplit = 1, cp = 0.01) #complexity parameter
prp(ci.tree.d.pruned)
#predicted values
ci.tree.d.pred <- predict(ci.tree.d, testing, type="c")
#misclassification error for given vectors of predicted and true class labels 
err(ci.tree.d.pred, testing$ALIGN)
#confusion matrix
#ci.tree.d.cm <- confusionMatrix(ci.tree.d.pred, testing$ALIGN)
#ci.tree.d.cm
#ROC
ci.tree.d.roc <- roc(predict(ci.tree.d, testing)[,5], testing$ALIGN)
plot(1-ci.tree.d.roc[,3], ci.tree.d.roc[,2], type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)


#ROCR
#ci.tree.d.pred <- prediction(predict(ci.tree.d, testing)[,2], testing$ALIGN)
#ci.tree.d.perf <- performance(ci.tree.d.pred, measure="tpr", x.measure="fpr")
#plot(ci.tree.d.perf)
#slot(performance(ci.tree.d.pred, measure="auc"), "y.values")





















#wersja 3
pkglist<-c("rpart", "rpart.plot", "RRF", "klaR", "gbm")
pkgcheck <- pkglist %in% row.names(installed.packages())
#COMMMENT the line below if you installed packages earlier e.g on root
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#this command is for root installation of missing packages:
if(length(pkglist[!pkgcheck])) cat("install.packages(c(");j=0; for(i in pkglist[!pkgcheck]) { j<-j+1 ;  if(j == length(pkglist[!pkgcheck])) cat(paste('"',i,'"',sep="")) else cat(paste('"',i,'",',sep=""));} ; cat("),depend=TRUE)")
#loading all libraries
for(i in pkglist) library(i, character.only = TRUE);


tpr <- function(cm) { if (is.nan(p <- cm[2,2]/(cm[2,2]+cm[1,2]))) 1 else p }
fpr <- function(cm) { if (is.nan(p <- cm[2,1]/(cm[2,1]+cm[1,1]))) 1 else p }
err <- function(pred.y, true.y) { mean(pred.y!=true.y) }
auc <- function(roc) { n <- nrow(roc); sum((roc$tpr[1:n-1]+roc$tpr[2:n])*diff(roc$fpr)/2) }
confmat <- function(pred.y, true.y) { table(pred.y, true.y, dnn=c("predicted", "true")) }
precision <- function(cm) { if (is.nan(p <- cm[2,2]/(cm[2,2]+cm[2,1]))) 1 else p }
f.measure <- function(cm) { 1/mean(c(1/precision(cm), 1/tpr(cm))) }

#ROC - receiver operating characteristics 
roc <- function(pred.s, true.y)
{
  cutoff <- Inf  # start with all instances classified as negative
  tp <- fp <- 0
  tn <- sum(2-as.integer(true.y))  # all negative instances
  fn <- sum(as.integer(true.y)-1)  # all positive instances
  rt <- data.frame()
  
  sord <- order(pred.s, decreasing=TRUE)  # score ordering
  for (i in 1:length(sord))
  {
    if (pred.s[sord[i]] < cutoff)
    {
      rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
      cutoff <- pred.s[sord[i]]
    }
    p <- as.integer(true.y[sord[i]])-1  # next positive classified as positive
    n <- 2-as.integer(true.y[sord[i]])  # next negative classified as positive
    tp <- tp+p
    fp <- fp+n
    tn <- tn-n
    fn <- fn-p
  }
  rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
}


#Read data
data <- read.table("marvel-wikia-data.csv", sep = ",", stringsAsFactors = TRUE)
#Assigning names from the first row
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
#Remove attributes that are not needed
data$page_id <- NULL
data$name <- NULL
data$urlslug <- NULL
data$GSM <- NULL
data$APPEARANCES <- as.numeric(data$APPEARANCES)
data$Year <- as.numeric(data$Year)

#TODO CHANGE TO CORRECT FACTORS
data$`FIRST APPEARANCE`  <- NULL


#Divide data into training and testing sets
rci <- runif(nrow(data))
training <- data[rci>=0.33,]
testing <- data[rci<0.33,]

#Rpart Tree (cp = 0.01 minsplit = 1)
set.seed(12354)
#classifier
ci.tree.d <- rpart(ALIGN~., training, minsplit = 1, cp = 0.01)
#prunning of the tree
prp(ci.tree.d)
plotcp(ci.tree.d)
printcp(ci.tree.d)
ci.tree.d.pruned<-prune(ci.tree.d, minsplit = 1, cp = 0.01)
prp(ci.tree.d.pruned)
#predicted values
ci.tree.d.pred <- predict(ci.tree.d, testing, type="c")
#misclassification error for given vectors of predicted and true class labels 
err(ci.tree.d.pred, testing$ALIGN)
#confusion matrix
ci.tree.d.cm <- confmat(ci.tree.d.pred, testing$ALIGN)
ci.tree.d.cm
#complementary pairs tpr - fpr, precision - recall, sensitivity - specificity
ci.tree.d.tpr <- tpr(ci.tree.d.cm)
ci.tree.d.tpr
ci.tree.d.fpr <- fpr(ci.tree.d.cm)
ci.tree.d.fpr
ci.tree.d.fmeasure <- f.measure(ci.tree.d.cm)
ci.tree.d.fmeasure 
#ROC
ci.tree.d.prob<-predict(ci.tree.d, testing)[,2]
ci.tree.d.roc <- roc(ci.tree.d.prob, testing$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)

#GBM
myclassifier_gbm <- gbm(ALIGN ~ ., data=training, distribution="gaussian", 
                        bag.fraction = 0.5, n.trees = 10, interaction.depth =6, 
                        shrinkage = 0.1, n.minobsinnode = 1)
print(myclassifier_gbm) # show classification outcome
summary(myclassifier_gbm)
pred_labels6 <- predict(myclassifier_gbm, testing,n.trees = 10)   # predict labels
round(pred_labels6)
#ROC
ci.tree.d.roc <- roc(pred_labels6, testing$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)




















#old test
options(encoding = "UTF-8"); par(ask=F)
dmrpkglist<-c('dmr.data','dmr.util','dmr.claseval','dmr.stats','dmr.trans','dmr.linreg','dmr.regeval','dmr.dissim',
              'dmr.dectree','dmr.linclas','dmr.disc','dmr.kcenters','dmr.cluseval','dmr.regtree','dmr.attrsel',
              'dmr.ensemble','dmr.kernel','dmr.bayes','dmr.hierclus','dmr.miscost','dmr.rpartutil')
pkgcheck <- dmrpkglist %in% row.names(installed.packages())
dmrpkglist[!pkgcheck]
for(i in dmrpkglist[!pkgcheck]){install_github(paste("42n4/", i, sep=""),force = TRUE)}
dmrpkglist<-c("dmr.util",
              "dmr.trans",
              "dmr.claseval",
              "dmr.rpartutil")
for(i in dmrpkglist) library(i, character.only = TRUE);

pkglist<-c("rpart",
           "rpart.plot",
           "randomForest")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#COMMMENT the line below if you installed packages earlier e.g on root
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#this command is for root instalation of missing packages:
if(length(pkglist[!pkgcheck])) cat("install.packages(c(");j=0; for(i in pkglist[!pkgcheck]) { j<-j+1 ;  if(j == length(pkglist[!pkgcheck])) cat(paste('"',i,'"',sep="")) else cat(paste('"',i,'",',sep=""));} ; cat("),depend=TRUE)")
#loading all libraries - necessary to avoid errors of execution
for(i in pkglist) library(i, character.only = TRUE);



#Read data
data <- read.table("high_diamond_ranked_10min.csv", sep = ",", stringsAsFactors = TRUE)
#Assing names from the first row
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]

#remove attributes that are not needed
data$gameId <- NULL
data$blueWardsDestroyed <- NULL
data$blueWardsPlaced <- NULL
data$blueEliteMonsters <- NULL
data$blueAssists <- NULL
data$blueDeaths <- NULL
data$blueTotalMinionsKilled <- NULL
data$blueTotalJungleMinionsKilled <- NULL
data$blueCSPerMin <- NULL
data$blueGoldPerMin <- NULL
data$blueExperienceDiff <- NULL
data$redWardsDestroyed <- NULL
data$redWardsPlaced <- NULL
data$redEliteMonsters <- NULL
data$redAssists <- NULL
data$redDeaths <- NULL
data$redDragons <- NULL
data$redHeralds <- NULL
data$redTowersDestroyed <- NULL
data$redTotalGold <- NULL
data$redAvgLevel <- NULL
data$redTotalMinionsKilled <- NULL
data$redTotalJungleMinionsKilled <- NULL
data$redCSPerMin <- NULL
data$redGoldPerMin <- NULL
data$redExperienceDiff <- NULL
data$redTotalExperience <- NULL
data$redFirstBlood <- NULL
data$redGoldDiff <- NULL

#initialize new data set with modified data
dataModified <- data.frame(blueWins = factor(), blueKills = factor(), blueFirstBlood = factor(), redKills = factor(), blueDragons = factor(), blueHeralds = factor(), blueTowersDestroyed = factor(), blueGoldDiff = numeric(), blueAvgLevel = factor(), blueTotalGold = numeric(), blueTotalExperience = numeric())
#populate new data set


for (i in 1:nrow(data)){
  blueWins2 = as.factor("FALSE")
  if (data$blueWins[i] == 1){
    blueWins2 = as.factor("TRUE")
  }
  blueFirstBlood2 = as.factor("FALSE")
  if (data$blueFirstBlood[i] == 1){
    blueFirstBlood2 = as.factor("TRUE")
  }
  blueHeralds2 = as.factor("FALSE")
  if (data[i, 5] == 1){
    blueHeralds2 = as.factor("TRUE")
  }
  blueDragon2 = as.factor("FALSE")
  if (data[i, 4] == 1){
    blueDragon2 = as.factor("TRUE")
  }
  
  dataModified <- rbind(dataModified, data.frame(
    blueWins = blueWins2,
    blueKills = data$blueKills[i],
    blueFirstBlood = blueFirstBlood2,
    redKills = data$redKills[i],
    blueDragons = blueDragon2,
    blueHeralds = blueHeralds2,
    blueTowersDestroyed = data$blueTowersDestroyed[i],
    blueGoldDiff = as.numeric(data$blueGoldDiff[i]),
    blueAvgLevel = data$blueAvgLevel[i],
    blueTotalGold = as.numeric(data$blueTotalGold[i]),
    blueTotalExperience = as.numeric(data$blueTotalExperience[i])
  ))
}

#Divide data into training and testing sets
rci <- runif(nrow(dataModified))
training <- dataModified[rci>=0.33,]
testing <- dataModified[rci<0.33,]


library("gbm")
myclassifier_gbm <- gbm(blueWins ~ ., data=training, distribution="gaussian", 
                        bag.fraction = 0.5, n.trees = 10, interaction.depth =6, 
                        shrinkage = 0.1, n.minobsinnode = 1)
print(myclassifier_gbm)                     # show classification outcome
summary(myclassifier_gbm)
pred_labels6 <- predict(myclassifier_gbm, testing,n.trees = 10)   # predict labels
round(pred_labels6)
#ROC
ci.tree.d.roc <- roc(pred_labels6, testing$blueWins)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)



#Rpart Tree (cp = 0.01 minsplit = 1)
set.seed(12354)
#classifier
ci.tree.d <- rpart(blueWins~., training, minsplit = 1, cp = 0.01)
#prunning of the tree
prp(ci.tree.d)
plotcp(ci.tree.d)
printcp(ci.tree.d)
ci.tree.d.pruned<-prune(ci.tree.d, minsplit = 1, cp = 0.01) #complexity parameter
prp(ci.tree.d.pruned)
#predicted values
ci.tree.d.pred <- predict(ci.tree.d, testing, type="c")
#misclassification error for given vectors of predicted and true class labels 
err(ci.tree.d.pred, testing$blueWins)
#confusion matrix
ci.tree.d.cm <- confmat(ci.tree.d.pred, testing$blueWins)
ci.tree.d.cm
#complementary pairs tpr - fpr, precision - recall, sensitivity - specificity
ci.tree.d.tpr <- tpr(ci.tree.d.cm)
ci.tree.d.tpr
ci.tree.d.fpr <- fpr(ci.tree.d.cm)
ci.tree.d.fpr
ci.tree.d.fmeasure <- f.measure(ci.tree.d.cm)
ci.tree.d.fmeasure 
#ROC
ci.tree.d.prob<-predict(ci.tree.d, testing)[,2]
ci.tree.d.roc <- roc(ci.tree.d.prob, testing$blueWins)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)

#Rpart Tree (cp = 0.01 minsplit = 25)
#classifier
ci.tree.d <- rpart(blueWins~., training, minsplit = 25, cp = 0.01)
#prunning of the tree
prp(ci.tree.d)
plotcp(ci.tree.d)
printcp(ci.tree.d)
ci.tree.d.pruned<-prune(ci.tree.d, minsplit = 25, cp = 0.01) #complexity parameter
prp(ci.tree.d.pruned)
#predicted values
ci.tree.d.pred <- predict(ci.tree.d, testing, type="c")
#misclassification error for given vectors of predicted and true class labels 
err(ci.tree.d.pred, testing$blueWins)
#confusion matrix
ci.tree.d.cm <- confmat(ci.tree.d.pred, testing$blueWins)
ci.tree.d.cm
#complementary pairs tpr - fpr, precision - recall, sensitivity - specificity
ci.tree.d.tpr <- tpr(ci.tree.d.cm)
ci.tree.d.tpr
ci.tree.d.fpr <- fpr(ci.tree.d.cm)
ci.tree.d.fpr
ci.tree.d.fmeasure <- f.measure(ci.tree.d.cm)
ci.tree.d.fmeasure
#ROC
ci.tree.d.prob<-predict(ci.tree.d, testing)[,2]
ci.tree.d.roc <- roc(ci.tree.d.prob, testing$blueWins)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)