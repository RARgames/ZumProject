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