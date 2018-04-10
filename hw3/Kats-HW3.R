# Required libraries
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caTools)
library(pscl)
library(ROCR)
library(MASS)
library(caret)

# Import data
crime <- read.csv(url("https://raw.githubusercontent.com/ilyakats/CUNY-DATA621/master/hw3/crime-training-data_modified.csv"))

# Basic statistic
nrow(crime); ncol(crime)
summary(crime)

# Summary table
sumCrime = data.frame(Variable = character(),
                      Min = integer(),
                      Median = integer(),
                      Mean = double(),
                      SD = double(),
                      Max = integer(),
                      Num_NAs = integer(),
                      Num_Zeros = integer())
for (i in 1:13) {
  sumCrime <- rbind(sumCrime, data.frame(Variable = colnames(crime)[i],
                                         Min = min(crime[,i], na.rm=TRUE),
                                         Median = median(crime[,i], na.rm=TRUE),
                                         Mean = mean(crime[,i], na.rm=TRUE),
                                         SD = sd(crime[,i], na.rm=TRUE),
                                         Max = max(crime[,i], na.rm=TRUE),
                                         Num_NAs = sum(is.na(crime[,i])),
                                         Num_Zeros = length(which(crime[,i]==0)))
  )
}
colnames(sumCrime) <- c("", "Min", "Median", "Mean", "SD", "Max", "Num of NAs", "Num of Zeros")
sumCrime

# Proportion of target variable
table(crime$target)
table(crime$target)/sum(table(crime$target))

# Exploratory plots (repeated for each variable)
kable(sumCrime[sumCrime[,1]=="zn",2:8], row.names=FALSE)

# Get descriptive plots:
# Variables: zn indus chas nox rm age dis rad tax ptratio lstat medv target
v <- "dis" # Variable to view
pd <- as.data.frame(cbind(crime[, v], crime$target))
colnames(pd) <- c("X", "Y")

# Boxplot
bp <- ggplot(pd, aes(x = 1, y = X)) + 
  stat_boxplot(geom ='errorbar') + geom_boxplot() + 
  xlab("Boxplot") + ylab("") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Density plot
hp <- ggplot(pd, aes(x = X)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + ylab("") + xlab("Density Plot with Mean") +
  geom_vline(aes(xintercept=mean(X, na.rm=TRUE)), color="red", linetype="dashed", size=1)

# Scatterplot
sp <- ggplot(pd, aes(x=X, y=Y)) + 
  geom_point() +   
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  xlab("Scatterplot with Logistic Regression Line")

grid.arrange(bp, hp, sp, layout_matrix=rbind(c(1,2,2),c(1,3,3)))

# Correlation matrix
cm <- cor(crime, use="pairwise.complete.obs")
cm <- round(cm, 2)
cmout <- as.data.frame(cm) %>% mutate_all(function(x) {
  cell_spec(x, "html", color = ifelse(x>0.5 | x<(-0.5),"blue","black"))
  })
rownames(cmout) <- colnames(cmout)
cmout %>%
  kable("html", escape = F, align = "c", row.names = TRUE) %>%
  kable_styling("striped", full_width = F)

crimeBackup <- crime

# Split into train and validation sets
set.seed(88)
split <- sample.split(crime$target, SplitRatio = 0.75)
crimeTRAIN <- subset(crime, split == TRUE)
crimeTEST <- subset(crime, split == FALSE)

# Model 1
model <- glm (target ~ ., data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
pred <- predict(model, newdata=subset(crimeTEST, select=c(1:12)), type='response')
cm <- confusionMatrix(as.factor(crimeTEST$target), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# ROC
pr <- prediction(pred, crimeTEST$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])


# Model 2
model <- glm (target ~ ., data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
model <- glm (target ~ .-rm, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
model <- glm (target ~ .-rm-chas, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
model <- glm (target ~ .-rm-chas-lstat, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
model <- glm (target ~ .-rm-chas-lstat-indus, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
model <- glm (target ~ .-rm-chas-lstat-indus-zn, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
pred <- predict(model, newdata=subset(crimeTEST, select=c(1:12)), type='response')
cm <- confusionMatrix(as.factor(crimeTEST$target), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# ROC
pr <- prediction(pred, crimeTEST$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])

# Take out 'tax' because it is highly correlated with 'rad'
model <- glm (target ~ .-tax, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
pred <- predict(model, newdata=subset(crimeTEST, select=c(1:12)), type='response')
cm <- confusionMatrix(as.factor(crimeTEST$target), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# ROC
pr <- prediction(pred, crimeTEST$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])
# Slight improvement

# Step AIC method
model <- glm (target ~ ., data = crimeTRAIN, family = binomial(link="logit"))
model <- stepAIC(model, trace=TRUE)
summary(model)
pred <- predict(model, newdata=subset(crimeTEST, select=c(1:12)), type='response')
cm <- confusionMatrix(as.factor(crimeTEST$target), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# ROC
pr <- prediction(pred, crimeTEST$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])


# Bad model for testing of code
model <- glm (target ~ age+tax, data = crimeTRAIN, family = binomial(link="logit"))
summary(model)
pred <- predict(model, newdata=subset(crimeTEST, select=c(1:12)), type='response')
cm <- confusionMatrix(as.factor(crimeTEST$target), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# ROC
pr <- prediction(pred, crimeTEST$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])

