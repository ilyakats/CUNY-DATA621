# Required libraries
library(dplyr)

# Import data and select columns
dt <- read.csv('F:/CUNY/GitHub/CUNY-DATA602/hw2/classification-output-data.csv')
dt <- dt %>% select(class, predicted=scored.class, prob=scored.probability)

# Raw confusion matrix
table(dt[,1:2])

# Generic function to return TP, TN, FP, FN based on two columns and 
# provided negative and positive values
TF.values <- function(tbl, actual, predicted, pos_value, neg_value){
   conf.matrix <- as.data.frame(table(tbl[,c(actual)], tbl[,c(predicted)]))
   FP <- filter(conf.matrix, Var1==neg_value & Var2==pos_value)$Freq
   FN <- filter(conf.matrix, Var1==pos_value & Var2==neg_value)$Freq
   TP <- filter(conf.matrix, Var1==pos_value & Var2==pos_value)$Freq
   TN <- filter(conf.matrix, Var1==neg_value & Var2==neg_value)$Freq
   if (length(FN)==0) FN<-0;
   if (length(FP)==0) FP<-0;
   if (length(TP)==0) TP<-0;
   if (length(TN)==0) TN<-0;
   return (list(TP=TP, FP=FP, TN=TN, FN=FN))
}

pos_value <- 1
neg_value <- 0

# Accuracy
accuracy <- function(tbl, actual, predicted, pos_value, neg_value){
  tf <- TF.values(tbl, actual, predicted, pos_value, neg_value)
  return ((tf$TP+tf$TN)/(tf$TP+tf$TN+tf$FP+tf$FN))
}

(acc <- accuracy(dt, actual='class', predicted='predicted', 
                 pos_value, neg_value))

# Classification Error Rate
error.rate <- function(tbl, actual, predicted, pos_value, neg_value){
  tf <- TF.values(tbl, actual, predicted, pos_value, neg_value)
  return ((tf$FP+tf$FN)/(tf$TP+tf$TN+tf$FP+tf$FN))
}

(cer <- error.rate(dt, actual='class', predicted='predicted', 
                   pos_value, neg_value))

# Precision
precision <- function(tbl, actual, predicted, pos_value, neg_value){
  tf <- TF.values(tbl, actual, predicted, pos_value, neg_value)
  return (tf$TP/(tf$TP+tf$FP))
}

(pre <- precision(dt, actual='class', predicted='predicted', 
                  pos_value, neg_value))

# Sensitivity
sensitivity <- function(tbl, actual, predicted, pos_value, neg_value){
  tf <- TF.values(tbl, actual, predicted, pos_value, neg_value)
  return (tf$TP/(tf$TP+tf$FN))
}

(sens <- sensitivity(dt, actual='class', predicted='predicted', 
                     pos_value, neg_value))

# Specificity
specificity <- function(tbl, actual, predicted, pos_value, neg_value){
  tf <- TF.values(tbl, actual, predicted, pos_value, neg_value)
  return (tf$TN/(tf$TN+tf$FP))
}

(spec <- specificity(dt, actual='class', predicted='predicted', 
                     pos_value, neg_value))

# F1 Score
f1.score <- function(tbl, actual, predicted, pos_value, neg_value){
  p <- precision(dt, actual='class', predicted='predicted', 
                 pos_value, neg_value)
  s <- sensitivity(dt, actual='class', predicted='predicted', 
                   pos_value, neg_value)
  return ((2*p*s)/(p+s))
}

(f1 <- f1.score(dt, actual='class', predicted='predicted', 
                pos_value, neg_value))

# Caret package
library(caret)
caret::sensitivity(as.factor(dt$class), as.factor(dt$predicted), positive='1')
caret::specificity(as.factor(dt$class), as.factor(dt$predicted), negative='0')
(cm <- confusionMatrix(dt$class, dt$predicted, positive='1'))
cm$byClass

# pROC package
library(pROC)
roc(dt$class, dt$prob, levels=c(0,1), percent=TRUE, plot=TRUE, ci=TRUE)

# Manual ROC curve
library(ggplot2)

manual.roc <- function(tbl, actual, prob, pos_value, neg_value) {
  tbl2 <- tbl[, c(actual, prob)]
  tbl2 <- cbind(tbl2, predicted = rep(0, nrow(tbl)))

  cutoff <- seq(0,1,0.01)
  sens <- rep(0,length(cutoff))
  spec <- rep(0,length(cutoff))
  for (i in 1:length(cutoff)) {
    tbl3 <- within(tbl2, predicted[prob>cutoff[i]] <- 1)
    sens[i] <- sensitivity(tbl3, actual='class', predicted='predicted', pos_value, neg_value)
    spec[i] <- specificity(tbl3, actual='class', predicted='predicted', pos_value, neg_value)
  }
  roc.values <- as.data.frame(cbind(cutoff, sens, spec))
  roc.values[,'spec'] <- 1 - roc.values[,'spec']
  
  # Prepare plot
  pl <- ggplot(arrange(roc.values, desc(cutoff)), aes(x=spec, y=sens)) + 
    geom_step() + 
    xlab("1-Specificity") + ylab("Sensitivity") + 
    ggtitle("ROC Curve")
  
  # Find AUC
  auc <- roc.values %>% 
    arrange(desc(cutoff)) %>%
    mutate(auc =  (spec-lag(spec))*sens) %>%
    replace(is.na(.),0) %>%
    summarize(sum(auc))
  
  return (list(plot=pl, AUC=auc))
}

mROC <- manual.roc(dt, 'class', 'prob', pos_value, neg_value)
mROC$plot
mROC$AUC
