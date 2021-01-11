pkglist<-c("arules", "randomForest", "rpart", "rpart.plot", "RRF", "klaR", "gbm", "readr", "dplyr")
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


prepare_data = function(marvel = TRUE){
  #Read data
  if(marvel) {
    data <- read.table("marvel-wikia-data.csv", sep = ",", stringsAsFactors = TRUE)
  } else {
    data <- read.table("dc-wikia-data.csv", sep = ",", stringsAsFactors = TRUE)
  }
  #Assigning names from the first row
  colnames(data) <- as.character(unlist(data[1,]))
  data = data[-1, ]
  #Remove attributes that are not needed
  data$page_id <- NULL
  data$urlslug <- NULL
  data$name <- NULL
  data$GSM <- NULL
  data$APPEARANCES <- as.numeric(data$APPEARANCES)
  data$Year <- as.numeric(as.character(data$Year))
  
  data <- data[rowSums(is.na(data[,c('ALIGN', 'ALIVE')]) | data[,c('ALIGN', 'ALIVE')] == "") == 0,]
  
  # Refactor first appearance column
  colnames(data)[colnames(data) == 'FIRST APPEARANCE'] <- 'Month'
  # Leave only month information
  if(marvel) {
    format <- '%b-%y'
  } else {
    format <- '%Y, %B'
  }
  data$Month <- as.numeric(format(parse_date(as.character(data$Month), 
                                             format = format, locale = locale('en')), '%m'))
  
  # Introduce new attributes
  # Get appearances per year since introduction
  popularity <- data$APPEARANCES/(2014-data$Year)
  tertile <- quantile(popularity[!is.na(popularity)], c(0:3/3))
  data$Popularity <- discretize(popularity, method = "fixed", breaks = tertile, labels = c('Low', 'Medium', 'High'))
  
  data$IntroducedAfter1990 <- factor(data$Year > 1990)
  
  return(data)
}

unify_values <- function(data) {
  data$ALIGN[data$ALIGN == 'Reformed Criminals'] <- 'Good Characters'
  
  # Unify hair column
  data$HAIR[data$HAIR == 'Bald'] <- 'No Hair'
  data$HAIR[data$HAIR == 'Auburn Hair'] <- 'Red Hair'
  data$HAIR[data$HAIR %in% c('Bronze Hair','Light Brown Hair','Reddish Brown Hair')] <- 'Brown Hair'
  data$HAIR[data$HAIR %in% c('Gold Hair','Reddish Blond Hair','Strawberry Blond Hair',
                             'Yellow Hair','Platinum Blond Hair')] <- 'Blond Hair'
  levels(data$HAIR) <- c(levels(data$HAIR), 'Other')
  data$HAIR[data$HAIR %in% c('Blue Hair','Dyed Hair','Magenta Hair','Orange-brown Hair','Orange Hair',
                             'Pink Hair','Purple Hair','Silver Hair','Variable Hair','Violet Hair')] <- 'Other'
  # Unify eyes column
  data$EYE[data$EYE == 'Amber Eyes'] <- 'Brown Eyes'
  data$EYE[data$EYE == 'Black Eyeballs'] <- 'Black Eyes'
  data$EYE[data$EYE == 'Silver Eyes'] <- 'Grey Eyes'
  data$EYE[data$EYE %in% c('Gold Eyes','Yellow Eyeballs')] <- 'Yellow Eyes'
  data$EYE[data$EYE %in% c('Magenta Eyes','Pink Eyes','Violet Eyes')] <- 'Purple Eyes'
  levels(data$EYE) <- c(levels(data$EYE), 'Other')
  data$EYE[data$EYE %in% c('Compound Eyes', 'Multiple Eyes', 'No Eyes', 'One Eye', 'Orange Eyes', 
                           'Variable Eyes', 'Auburn Hair', 'Photocellular Eyes')] <- 'Other'
  # Unify sex column
  levels(data$SEX)[levels(data$SEX) %in% c('Agender Characters', 'Genderfluid Characters', 
                                           'Genderless Characters', 'Transgender Characters')] <- 'Other'
  
  # Drop unused factor levels
  data[] <- lapply(data, function(x) if(is.factor(x)) factor(x) else x)
  return(data)
}

data <- rbind(prepare_data(marvel = TRUE), prepare_data(marvel = FALSE))
data <- unify_values(data)

#Divide data into training and testing sets
rci <- runif(nrow(data))
training <- data[rci>=0.33,]
testing <- data[rci<0.33,]

set.seed(12354)

#GBM
myclassifier_gbm <- gbm(ALIGN ~ ., data=training, distribution="gaussian", 
                        bag.fraction = 0.5, n.trees = 10, interaction.depth =6, 
                        shrinkage = 0.1, n.minobsinnode = 1)
print(myclassifier_gbm) # show classification outcome
summary(myclassifier_gbm)
pred_labels6 <- predict(myclassifier_gbm, testing,n.trees = 10)   # predict labels
# round(pred_labels6)
#ROC
ci.tree.d.roc <- roc(pred_labels6, testing$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)


#RRF
prepare_rrf_data = function(data) {
  data$ID <- factor(data$ID)
  data$ALIGN <- factor(data$ALIGN)
  data$EYE <- factor(data$EYE)
  data$HAIR <- factor(data$HAIR)
  data$SEX <- factor(data$SEX)
  data$ALIVE <- factor(data$ALIVE)
  return(data)
}
data.rrf <- prepare_rrf_data(data)
training.rrf <- data.rrf[rci>=0.33,]
testing.rrf <- data.rrf[rci<0.33,]

# Remove NAs from data
training.rrf <- rfImpute(ALIGN  ~ ., training.rrf)

myclassifier_rrf <- RRF(ALIGN ~ ., data=training.rrf)
print(myclassifier_rrf)                     # show classification outcome
#summary(myclassifier_rrf)
importance(myclassifier_rrf)                # importance of each predictor 
pred_labels3 <- predict(myclassifier_rrf, testing.rrf)# predict labels

ci.tree.d.roc <- roc(pred_labels3, testing.rrf$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)

#Regularized Discriminant Analysis
myclassifier_rda <- rda(ALIGN ~ ., data=training)
print(myclassifier_rda)                     # show classification outcome
summary(myclassifier_rda)
pred_labels5 <- predict(myclassifier_rda, testing)   # predict labels

ci.tree.d.roc <- roc(pred_labels5, testing$ALIGN)
plot(ci.tree.d.roc$fpr, ci.tree.d.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(ci.tree.d.roc)


#Rpart Tree (cp = 0.01 minsplit = 1)
#classifier
ci.tree.d <- rpart(ALIGN~., training, minsplit = 15, cp = 0.01)

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

