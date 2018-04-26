# Required libraries
library(knitr)
library(kableExtra)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(caTools)
library(pscl)
library(ROCR)
library(MASS)
library(caret)
library(car)
library(Metrics)
library(quantreg)

# Import data
ins <- read.csv(url(paste0("https://raw.githubusercontent.com/",
                           "ilyakats/CUNY-DATA621/master/hw4/",
                           "insurance_training_data.csv")),
                na.strings=c("","NA"))

# Basic statistic
nrow(ins); ncol(ins)
summary(ins)

# TARGET_FLAG - 6008 are 0, 2153 are 1
table(ins$TARGET_FLAG)
class(ins$TARGET_FLAG) 
# Integer (0/1)

# TARGET_AMT
summary(ins[ins$TARGET_FLAG==0, 'TARGET_AMT'])
summary(ins[ins$TARGET_FLAG==1, 'TARGET_AMT'])
class(ins$TARGET_AMT) 
# Only available for TARGET_FLAG=1
# Numeric: Ranges from 30.28 to 107600

# KIDSDRIV - No of Driving Children
table(ins$KIDSDRIV)
class(ins$KIDSDRIV)
# Integer - Ranges from 0 to 4

# AGE
summary(ins$AGE)
class(ins$AGE)
# Integer - Ranges from 16 to 81
# 6 NAs

# HOMEKIDS
table(ins$HOMEKIDS)
class(ins$HOMEKIDS)
# Integer - Ranges from 0 to 5

# YOJ - Years on Job
summary(ins$YOJ)
class(ins$YOJ)
# Integer - Ranges from 0 to 23
# 454 NAs - 5.56% of observations

# INCOME
class(ins$INCOME)
summary(ins$INCOME)
# Convert to Numeric - Ranges from $0 to $367,000
# 445 NAs - 5.45% of observations

# PARENT1 - Single Parent?
table(ins$PARENT1)
class(ins$PARENT1); levels(ins$PARENT1)
# Factor - No, Yes

# HOME_VAL - Home Value
class(ins$HOME_VAL)
summary(ins$HOME_VAL)
# Converted to Numeric - Ranges from $0 to $885,300
# 464 NAs - 5.69% of observations

# MSTATUS
table(ins$MSTATUS)
class(ins$MSTATUS); levels(ins$MSTATUS)
# Factor - Yes, No

# SEX
table(ins$SEX)
class(ins$SEX); levels(ins$SEX)
# Factor - M, F

# EDUCATION
table(ins$EDUCATION)
class(ins$EDUCATION); levels(ins$EDUCATION)
# Factor - <HS, HS, BA, MA, PhD

# JOB
table(ins$JOB)
class(ins$JOB); levels(ins$JOB)
# Factor - [Blank], Clerical, Doctor, Home Maker, Lawyer, Manager, Professional, Student, Blue Collar

# TRAVTIME - Distance to work
summary(ins$TRAVTIME)
class(ins$TRAVTIME)
# Integer - Ranges from 5 to 142

# CAR_USE
class(ins$CAR_USE); levels(ins$CAR_USE)
table(ins$CAR_USE)
# Factor - Commercial, Private

# BLUEBOOK
class(ins$BLUEBOOK)
summary(ins$BLUEBOOK)
# Numeric - Ranges from $1,500 to $69,740

# TIF - Time in Force
class(ins$TIF)
summary(ins$TIF)
# Integer - Ranges from 1 to 25

# CAR_TYPE
class(ins$CAR_TYPE); levels(ins$CAR_TYPE)
table(ins$CAR_TYPE)
# FActor - Minivan, Panel Truck, Pickup, Sports Car, Van, SUV

# RED_CAR
class(ins$RED_CAR); levels(ins$RED_CAR)
table(ins$RED_CAR)
# Factor - No, Yes

# OLDCLAIM
class(ins$OLDCLAIM)
summary(ins$OLDCLAIM)
# Numeric - Ranges from $0 to $57,040

# CLM_FREQ
class(ins$CLM_FREQ)
summary(ins$CLM_FREQ)
# Integer - Ranges from 0 to 5

# REVOKED
class(ins$REVOKED); levels(ins$REVOKED)
table(ins$REVOKED)
# Factor - No, Yes

# MVR_PTS
class(ins$MVR_PTS)
summary(ins$MVR_PTS)
# Integer - Ranges from 0 to 13

# CAR_AGE
class(ins$CAR_AGE)
summary(ins$CAR_AGE)
nrow(ins[ins$CAR_AGE<0 & !is.na(ins$CAR_AGE), ])
nrow(ins[ins$CAR_AGE==0 & !is.na(ins$CAR_AGE), ])
nrow(ins[ins$CAR_AGE==1 & !is.na(ins$CAR_AGE), ])
# Integer - Ranges from -3 to 28
# 1 observation of -3 - invalid
# 3 observations of 0 - likely invalid
# 1,934 observations of 1 - reasonable (new car)
# 510 NAs - 6.25% of observations

# URBANICITY
class(ins$URBANICITY); levels(ins$URBANICITY)
table(ins$URBANICITY)
# Factor - Urban, Rural

ins$INCOME <- as.numeric(gsub('[$,]', '', ins$INCOME)) # Convert income from Factor to Numeric
ins$HOME_VAL <- as.numeric(gsub('[$,]', '', ins$HOME_VAL)) # Convert home value from Factor to Numeric
levels(ins$MSTATUS)[match("z_No",levels(ins$MSTATUS))] <- "No" # Remove 'z_'
levels(ins$SEX)[match("z_F",levels(ins$SEX))] <- "F" # Remove 'z_'
levels(ins$EDUCATION)[match("z_High School",levels(ins$EDUCATION))] <- "High School" # Remove 'z_'
ins$EDUCATION <- factor(ins$EDUCATION,levels(ins$EDUCATION)[c(1,5,2:4)]) # Reorder levels
levels(ins$JOB)[match("z_Blue Collar",levels(ins$JOB))] <- "Blue Collar" # Remove 'z_'
ins$BLUEBOOK <- as.numeric(gsub('[$,]', '', ins$BLUEBOOK)) # Convert value from Factor to Numeric
levels(ins$CAR_TYPE)[match("z_SUV",levels(ins$CAR_TYPE))] <- "SUV" # Remove 'z_'
levels(ins$RED_CAR)[match("no",levels(ins$RED_CAR))] <- "No"
levels(ins$RED_CAR)[match("yes",levels(ins$RED_CAR))] <- "Yes"
ins$OLDCLAIM <- as.numeric(gsub('[$,]', '', ins$OLDCLAIM)) # Convert from Factor to Numeric
levels(ins$URBANICITY)[match("Highly Urban/ Urban",levels(ins$URBANICITY))] <- "Urban"
levels(ins$URBANICITY)[match("z_Highly Rural/ Rural",levels(ins$URBANICITY))] <- "Rural"
ins$JOB <- factor(ins$JOB,levels(ins$JOB)[c(7, 8, 3, 1, 6, 5, 4, 2)]) # Reorder levels

# Drop index column
ins <- ins[-c(1)]

insFull <- ins

ins <- ins[complete.cases(ins), ]
ins[ins$CAR_AGE<1,'CAR_AGE'] <- NA
ins <- ins[complete.cases(ins), ]
# Cuts down from 8,161 to 6,045

# Get only complete cases
nrow(ins[complete.cases(ins), ])
nrow(ins)

insBackup <- ins

# Summary table
sumIns = data.frame(Variable = character(),
                    Min = integer(),
                    Median = integer(),
                    Mean = double(),
                    SD = double(),
                    Max = integer(),
                    Num_NAs = integer(),
                    Num_Zeros = integer())
for (i in c(3:7,9,14,16,17,20,21,23,24)) {
  sumIns <- rbind(sumIns, data.frame(Variable = colnames(ins)[i],
                                     Min = min(ins[,i], na.rm=TRUE),
                                     Median = median(ins[,i], na.rm=TRUE),
                                     Mean = mean(ins[,i], na.rm=TRUE),
                                     SD = sd(ins[,i], na.rm=TRUE),
                                     Max = max(ins[,i], na.rm=TRUE),
                                     Num_NAs = sum(is.na(ins[,i])),
                                     Num_Zeros = length(which(ins[,i]==0)))
  )
}
colnames(sumIns) <- c("", "Min", "Median", "Mean", "SD", "Max", "Num of NAs", "Num of Zeros")
sumIns

# Proportion of target variable
table(ins$TARGET_FLAG)
table(ins$TARGET_FLAG)/sum(table(ins$TARGET_FLAG))

# Exploratory plots (repeated for each variable)
# Get descriptive plots:
# Variables: 
# INDEX, TARGET_FLAG, TARGET_AMT, KIDSDRIV, AGE, HOMEKIDS, YOJ, INCOME, PARENT1, HOME_VAL, 
# MSTATUS, SEX, EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, TIF, CAR_TYPE, RED_CAR, 
# OLDCLAIM, CLM_FREQ, REVOKED, MVR_PTS, CAR_AGE, URBANICITY, 
v <- "TARGET_AMT" # Variable to view
pd <- as.data.frame(cbind(ins[, v], ins$TARGET_FLAG))
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
cm <- cor(ins, use="pairwise.complete.obs")
cm <- round(cm, 2)
cmout <- as.data.frame(cm) %>% mutate_all(function(x) {
  cell_spec(x, "html", color = ifelse(x>0.5 | x<(-0.5),"blue","black"))
  })
rownames(cmout) <- colnames(cmout)
cmout %>%
  kable("html", escape = F, align = "c", row.names = TRUE) %>%
  kable_styling("striped", full_width = F)

pairs(ins)

# Split into train and validation sets
set.seed(88)
split <- sample.split(ins$TARGET_FLAG, SplitRatio = 0.75)
insTRAIN <- subset(ins, split == TRUE)
insTEST <- subset(ins, split == FALSE)

# BINARY REGRESSION MODEL

# Modelling - Basic model
model <- glm (TARGET_FLAG ~ .-TARGET_AMT, data = insTRAIN, family = binomial(link="logit"))
summary(model)
pred <- predict(model, newdata=subset(insTEST, select=c(1:25)), type='response')
cm <- confusionMatrix(as.factor(insTEST$TARGET_FLAG), as.factor(ifelse(pred > 0.5,1,0)))
cm$table
cm$overall['Accuracy']
pR2(model) # McFadden R^2

# Stepwise approach
model <- stepAIC(model, trace=FALSE, direction='both')

# Model tweaking
model <- glm(formula = TARGET_FLAG ~ KIDSDRIV + log(INCOME+1) + PARENT1 + log(HOME_VAL+1) + 
               MSTATUS + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + 
               TIF + CAR_TYPE + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + 
               URBANICITY, family = binomial(link = "logit"), data = insTRAIN)

# ROC
pr <- prediction(pred, insTEST$TARGET_FLAG)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
(auc <- auc@y.values[[1]])

# K-Fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- train(TARGET_FLAG ~ KIDSDRIV + log(INCOME+1) + PARENT1 + log(HOME_VAL+1) + 
                     MSTATUS + EDUCATION + TRAVTIME + CAR_USE + BLUEBOOK + 
                     TIF + CAR_TYPE + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + 
                     URBANICITY,  data=insTRAIN, method="glm", family="binomial",
                   trControl = ctrl, tuneLength = 5)
pred <- predict(model_fit, newdata=insTEST)
confusionMatrix(as.factor(insTEST$TARGET_FLAG), as.factor(ifelse(pred > 0.5,1,0)))

# Deviance residuals
anova(model, test="Chisq")

# VIF
vif(model)
# Take out JOB 
ggplot(data = ins, aes(JOB, EDUCATION)) +
  geom_jitter()
model <- glm(formula = TARGET_FLAG ~ KIDSDRIV + log(INCOME+1) + PARENT1 + log(HOME_VAL+1) + 
               MSTATUS + EDUCATION + TRAVTIME + CAR_USE + BLUEBOOK + 
               TIF + CAR_TYPE + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + 
               URBANICITY, family = binomial(link = "logit"), data = insTRAIN)

# LINEAR MODEL

insLM <- ins
insLM <- ins[ins$TARGET_FLAG==1,]

# Split into training and testing sets
split <- sample.split(insLM$TARGET_AMT, SplitRatio = 0.75)
insLMtrain <- subset(insLM, split == TRUE)
insLMtest <- subset(insLM, split == FALSE)

# Initial models
lmModel <- lm(TARGET_AMT ~ .-TARGET_FLAG,data = insLMtrain)
summary(lmModel)
lmModel <- stepAIC(lmModel, trace=FALSE, direction='both')
summary(lmModel)
lmModel <- lm(TARGET_AMT ~ PARENT1 + MSTATUS + BLUEBOOK + OLDCLAIM + CLM_FREQ + REVOKED + JOB,data = insLMtrain)
summary(lmModel)
lmModel <- lm(TARGET_AMT ~ BLUEBOOK + OLDCLAIM + CLM_FREQ + REVOKED,data = insLMtrain)
summary(lmModel)
lmModel <- lm(TARGET_AMT ~ BLUEBOOK + CAR_AGE + CAR_TYPE,data = insLMtrain)
summary(lmModel)
lmModel <- lm(TARGET_AMT ~ BLUEBOOK,data = insLMtrain)
summary(lmModel)

# Calculate RMSE
pred <- predict(lmModel, newdata=insLMtest)
rmse(insLMtest$TARGET_AMT, pred)

# Model plots
plot(lmModel$residuals, ylab="Residuals")
abline(h=0)

plot(lmModel$fitted.values, lmModel$residuals, xlab="Fitted Values", ylab="Residuals")
abline(h=0)

qqnorm(lmModel$residuals)
qqline(lmModel$residuals)

boxcox(lmModel)

lmModel <- lm(log(TARGET_AMT) ~ BLUEBOOK, data = insLMtrain)
summary(lmModel)
pred <- predict(lmModel, newdata=insLMtest)
rmse(log(insLMtest$TARGET_AMT), pred)
lmModel2 <- rlm(log(TARGET_AMT) ~ BLUEBOOK, data = insLMtrain)
summary(lmModel2)
pred <- predict(lmModel2, newdata=insLMtest)
rmse(log(insLMtest$TARGET_AMT), pred)
lmModel3 <- rq(log(TARGET_AMT) ~ BLUEBOOK, data = insLMtrain)
summary(lmModel3)
pred <- predict(lmModel3, newdata=insLMtest)
rmse(log(insLMtest$TARGET_AMT), pred)


plot(log(TARGET_AMT) ~ BLUEBOOK, data = insLMtrain)
abline(lmModel)
abline(lmModel2, col="red")
abline(lmModel3, col="blue")
legend("topright", inset=0.05, bty="n",
       legend=c("lm fit", "rlm fit", "rq fit"),
       lty=c(1,1,1),
       col=c("black", "red", "blue"))

# Prediction
eval <- read.csv(url(paste0("https://raw.githubusercontent.com/",
                            "ilyakats/CUNY-DATA621/master/hw4/",
                            "insurance-evaluation-data.csv")),
                 na.strings=c("","NA"))
results <- eval[,1]
eval$INCOME <- as.numeric(gsub('[$,]', '', eval$INCOME)) # Convert income from Factor to Numeric
eval$HOME_VAL <- as.numeric(gsub('[$,]', '', eval$HOME_VAL)) # Convert home value from Factor to Numeric
levels(eval$MSTATUS)[match("z_No",levels(eval$MSTATUS))] <- "No" # Remove 'z_'
levels(eval$SEX)[match("z_F",levels(eval$SEX))] <- "F" # Remove 'z_'
levels(eval$EDUCATION)[match("z_High School",levels(eval$EDUCATION))] <- "High School" # Remove 'z_'
eval$EDUCATION <- factor(eval$EDUCATION,levels(eval$EDUCATION)[c(1,5,2:4)]) # Reorder levels
levels(eval$JOB)[match("z_Blue Collar",levels(eval$JOB))] <- "Blue Collar" # Remove 'z_'
eval$BLUEBOOK <- as.numeric(gsub('[$,]', '', eval$BLUEBOOK)) # Convert value from Factor to Numeric
levels(eval$CAR_TYPE)[match("z_SUV",levels(eval$CAR_TYPE))] <- "SUV" # Remove 'z_'
levels(eval$RED_CAR)[match("no",levels(eval$RED_CAR))] <- "No"
levels(eval$RED_CAR)[match("yes",levels(eval$RED_CAR))] <- "Yes"
eval$OLDCLAIM <- as.numeric(gsub('[$,]', '', eval$OLDCLAIM)) # Convert from Factor to Numeric
levels(eval$URBANICITY)[match("Highly Urban/ Urban",levels(eval$URBANICITY))] <- "Urban"
levels(eval$URBANICITY)[match("z_Highly Rural/ Rural",levels(eval$URBANICITY))] <- "Rural"
eval$JOB <- factor(eval$JOB,levels(eval$JOB)[c(7, 8, 3, 1, 6, 5, 4, 2)]) # Reorder levels
eval <- eval[-c(1)]

pred <- predict(model, newdata=eval, type="response")
results <- cbind(results, prob=round(pred,4))
results <- cbind(results, predict=round(pred,0))

pred <- predict(lmModel, newdata=eval, type="response")
results <- cbind(results, exp(pred))
results <- as.data.frame(results)

results[results$predict==0 & !is.na(results$predict),'V4'] <- NA
results[is.na(results$predict),'V4'] <- NA
colnames(results) <- c("Index", "TARGET_FLAG Prob.", "TARGET_FLAG", "TARGET_AMT")
pander(head(results, 100))
