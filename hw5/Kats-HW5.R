# Required libraries
library(ggplot2)    # plotting
library(dplyr)      # data manipulation
library(gridExtra)  # display
library(knitr)      # display
library(kableExtra) # display
library(mice)       # imputation
library(caTools)    # train-test split
library(MASS)       # boxcox
library(Metrics)    # rmse
library(caret)      # confusion matrix
library(VIM)        # plotting NAs
library(ggfortify)  # plotting lm diagnostic
library(car)        # VIF
library(pscl)       # zero-inflated model

# Import data
wine <- read.csv(url(paste0("https://raw.githubusercontent.com/",
                           "ilyakats/CUNY-DATA621/master/hw5/",
                           "wine-training-data.csv")),
                 na.strings=c("","NA"))
colnames(wine)[1] <- "INDEX"

# Basic statistic
nrow(wine); ncol(wine)
summary(wine)

# Summary table
sumtbl = data.frame(Variable = character(),
                    Class = character(),
                    Min = integer(),
                    Median = integer(),
                    Mean = double(),
                    SD = double(),
                    Max = integer(),
                    Num_NAs = integer(),
                    Num_Zeros = integer(),
                    Num_Neg = integer())
for (i in c(3:16)) {
  sumtbl <- rbind(sumtbl, data.frame(Variable = colnames(wine)[i],
                                     Class = class(wine[,i]),
                                     Min = min(wine[,i], na.rm=TRUE),
                                     Median = median(wine[,i], na.rm=TRUE),
                                     Mean = mean(wine[,i], na.rm=TRUE),
                                     SD = sd(wine[,i], na.rm=TRUE),
                                     Max = max(wine[,i], na.rm=TRUE),
                                     Num_NAs = sum(is.na(wine[,i])),
                                     Num_Zeros = length(which(wine[,i]==0)),
                                     Num_Neg = sum(wine[,i]<0 & !is.na(wine[,i]))))
}
colnames(sumtbl) <- c("Variable", "Class", "Min", "Median", "Mean", "SD", "Max", 
                      "Num of NAs", "Num of Zeros", "Num of Neg Values")
sumtbl

# Categorical variables
table(wine$LabelAppeal)
table(wine$AcidIndex)
table(wine$STARS)

# Exploratory plots
v <- "FixedAcidity"
v <- "VolatileAcidity"
v <- "CitricAcid"
v <- "ResidualSugar"
v <- "Chlorides"
v <- "FreeSulfurDioxide"
v <- "TotalSulfurDioxide"
v <- "Density"
v <- "pH"
v <- "Sulphates"
v <- "Alcohol"
v <- "LabelAppeal" 
v <- "AcidIndex"
v <- "STARS"
pd <- as.data.frame(cbind(wine[, v], wine$TARGET)); colnames(pd) <- c("X", "Y")
bp <- ggplot(pd, aes(x = 1, y = X)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
  xlab("Boxplot") + ylab("") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
hp <- ggplot(pd, aes(x = X)) + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + ylab("") + xlab("Density Plot with Mean") +
  geom_vline(aes(xintercept=mean(X, na.rm=TRUE)), color="red", linetype="dashed", size=1)
sp <- ggplot(pd, aes(x=X, y=Y)) + geom_point() + xlab("Scatterplot")
grid.arrange(bp, hp, sp, layout_matrix=rbind(c(1,2,2),c(1,3,3)))

ggplot(wine, aes(x = as.factor(TARGET), y = Chlorides)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
  xlab("Boxplots per No of Wine Cases") + ylab("pH") + theme(axis.ticks.x=element_blank())

# Correlation matrix
cm <- cor(wine[,2:16], use="pairwise.complete.obs")
cm <- round(cm, 2)
cmout <- as.data.frame(cm) %>% mutate_all(function(x) {
  cell_spec(x, "html", color = ifelse(x>0.5 | x<(-0.5),"blue","black"))
})
rownames(cmout) <- colnames(cmout)
cmout %>%
  kable("html", escape = F, align = "c", row.names = TRUE) %>%
  kable_styling("striped", full_width = F)

# IMPUTATION / TRANSFORMATION
wineOriginal <- wine # Backup of original data

wine$STARS[is.na(wine$STARS)] <- 0 # Missing STARS are 0 score
# Missing values - table
md.pattern(wine)
# Missing values - plot
aggr_plot <- aggr(wine, col=c('navyblue','red'), 
                  numbers=FALSE, sortVars=TRUE, labels=names(wine), 
                  cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
# Imputation
wineImputed <- mice(wine, m=5, maxit=20, meth='norm', seed=500)
summary(wineImputed)
wine <- complete(wineImputed)
summary(wine)

# Proportion of target variable
table(wine$TARGET)
table(wine$TARGET)/sum(table(wine$TARGET))

wine$Alcohol <- abs(wine$Alcohol)

# Split into train and validation sets
set.seed(88)
split <- sample.split(wine$TARGET, SplitRatio = 0.75)
wineTRAIN <- subset(wine, split == TRUE)
wineTEST <- subset(wine, split == FALSE)
table(wineTRAIN$TARGET)/sum(table(wineTRAIN$TARGET))

# LINEAR MODEL

# All variables
lmModel <- lm(TARGET ~ .-INDEX,data = wineTRAIN)
summary(lmModel)
# stepAIC
lmModel <- stepAIC(lmModel, trace=FALSE, direction='both')
summary(lmModel)
# Model returned by step AIC
lmModel <- lm(TARGET ~ VolatileAcidity + CitricAcid + Chlorides + FreeSulfurDioxide + 
                TotalSulfurDioxide + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS,
              data = wineTRAIN)
summary(lmModel)
# Manual variations
lmModel <- lm(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                TotalSulfurDioxide + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS,
              data = wineTRAIN)
summary(lmModel)
lmModel <- lm(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                TotalSulfurDioxide + Alcohol + LabelAppeal + AcidIndex + STARS,
              data = wineTRAIN)
summary(lmModel)

# Calculate RMSE
pred <- predict(lmModel, newdata=wineTEST)
rmse(wineTEST$TARGET, pred)

# Confusion matrix
predRound <- as.factor(round(pred,0))
table(predRound)
levels(predRound) <- levels(as.factor(wineTEST$TARGET))
confusionMatrix(predRound, as.factor(wineTEST$TARGET))

autoplot(lmModel)

# Model plots
plot(lmModel$residuals, ylab="Residuals")
abline(h=0)

plot(lmModel$fitted.values, lmModel$residuals, xlab="Fitted Values", ylab="Residuals")
abline(h=0)

qqnorm(lmModel$residuals)
qqline(lmModel$residuals)

# POISSON and NB REGRESSION MODEL

# Poisson 1
glmModel <- glm (TARGET ~ .-INDEX, data = wineTRAIN, family = poisson)
summary(glmModel)
pred <- predict(glmModel, newdata=wineTEST, type='response')
rmse(wineTEST$TARGET, pred)
predRound <- as.factor(round(pred,0))
testData <- as.factor(wineTEST$TARGET)
levels(predRound) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")
levels(testData) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
confusionMatrix(predRound, testData)

# Poisson 2
glmModel2 <- stepAIC(glmModel, trace=FALSE, direction='both')
summary(glmModel2)
pred <- predict(glmModel2, newdata=wineTEST, type='response')
rmse(wineTEST$TARGET, pred)
predRound <- as.factor(round(pred,0))
testData <- as.factor(wineTEST$TARGET)
levels(predRound) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")
levels(testData) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
confusionMatrix(predRound, testData)

# Poisson 3
glmModel3 <- glm(TARGET ~ VolatileAcidity + Chlorides + FreeSulfurDioxide + 
                 Sulphates + Alcohol + LabelAppeal + 
                 AcidIndex + STARS, family = poisson, data = wineTRAIN)
summary(glmModel3)
pred <- predict(glmModel3, newdata=wineTEST, type='response')
rmse(wineTEST$TARGET, pred)
predRound <- as.factor(round(pred,0))
testData <- as.factor(wineTEST$TARGET)
levels(predRound) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")
levels(testData) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
confusionMatrix(predRound, testData)

# NB
nbModel <- glm.nb(TARGET ~ .-INDEX, data = wineTRAIN)
summary(nbModel)
pred <- predict(nbModel, newdata=wineTEST, type='response')
rmse(wineTEST$TARGET, pred)
predRound <- as.factor(round(pred,0))
testData <- as.factor(wineTEST$TARGET)
levels(predRound) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")
levels(testData) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
confusionMatrix(predRound, testData)

# Zero Inflated
zrModel <- zeroinfl(TARGET ~ .-INDEX, data = wineTRAIN, dist = "negbin")
summary(zrModel)
pred <- predict(zrModel, newdata=wineTEST, type='response')
rmse(wineTEST$TARGET, pred)
predRound <- as.factor(round(pred,0))
testData <- as.factor(wineTEST$TARGET)
confusionMatrix(predRound, testData)

# Deviance residuals
anova(glmModel, test="Chisq")
anova(glmModel2, test="Chisq")
anova(glmModel3, test="Chisq")
anova(nbModel, test="Chisq")
anova(zrModel, test="Chisq")

# VIF
vif(glmModel)
vif(nbModel)
vif(zrModel)

# Coefficients
coef <- as.data.frame(lmModel$coefficients)
coef <- cbind(coef, as.data.frame(glmModel$coefficients))
coef <- cbind(coef, as.data.frame(nbModel$coefficients))
coef <- cbind(coef, as.data.frame(zrModel$coefficients))

# Prediction
eval <- read.csv(url(paste0("https://raw.githubusercontent.com/",
                            "ilyakats/CUNY-DATA621/master/hw5/",
                            "wine-evaluation-data.csv")),
                 na.strings=c("","NA"))
colnames(eval)[1] <- "INDEX"

sumtbl = data.frame(Variable = character(),
                    Class = character(),
                    Min = integer(),
                    Median = integer(),
                    Mean = double(),
                    SD = double(),
                    Max = integer(),
                    Num_NAs = integer(),
                    Num_Zeros = integer(),
                    Num_Neg = integer())
for (i in c(3:16)) {
  sumtbl <- rbind(sumtbl, data.frame(Variable = colnames(eval)[i],
                                     Class = class(eval[,i]),
                                     Min = min(eval[,i], na.rm=TRUE),
                                     Median = median(eval[,i], na.rm=TRUE),
                                     Mean = mean(eval[,i], na.rm=TRUE),
                                     SD = sd(eval[,i], na.rm=TRUE),
                                     Max = max(eval[,i], na.rm=TRUE),
                                     Num_NAs = sum(is.na(eval[,i])),
                                     Num_Zeros = length(which(eval[,i]==0)),
                                     Num_Neg = sum(eval[,i]<0 & !is.na(eval[,i]))))
}
colnames(sumtbl) <- c("Variable", "Class", "Min", "Median", "Mean", "SD", "Max", 
                      "Num of NAs", "Num of Zeros", "Num of Neg Values")
sumtbl

eval$STARS[is.na(eval$STARS)] <- 0
eval$Alcohol <- abs(eval$Alcohol)

evalImputed <- mice(eval, m=5, maxit=10, meth='norm', seed=500)
eval <- complete(evalImputed)

pred <- predict(zrModel, newdata=eval, type="response")
results <- eval[, c("INDEX")]
results <- cbind(results, prob=round(pred,4))
results <- cbind(results, predict=round(pred,0))
colnames(results) <- c("Index", "Predicted Value", "Predicted Outcome")
pander(head(results, 100))
