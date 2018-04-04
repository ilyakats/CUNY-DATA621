# Required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(Hmisc)

# Import data
bb <- read.csv("moneyball-training-data.csv")

# Basic statistic
nrow(bb); ncol(bb)
summary(bb)

# Get summary table
sumBB = data.frame(Variable = character(),
                   Min = integer(),
                   Median = integer(),
                   Mean = double(),
                   SD = double(),
                   Max = integer(),
                   Num_NAs = integer(),
                   Num_Zeros = integer())

for (i in 2:17) {
  sumBB <- rbind(sumBB, data.frame(Variable = colnames(bb)[i],
                                   Min = min(bb[,i], na.rm=TRUE),
                                   Median = median(bb[,i], na.rm=TRUE),
                                   Mean = mean(bb[,i], na.rm=TRUE),
                                   SD = sd(bb[,i], na.rm=TRUE),
                                   Max = max(bb[,i], na.rm=TRUE),
                                   Num_NAs = sum(is.na(bb[,i])),
                                   Num_Zeros = length(which(bb[,i]==0)))
                 )
}

# Exploratory plots (repeated for each variable)
kable(sumBB[sumBB[,1]=="TEAM_BASERUN_SB",2:8], row.names=FALSE)

# Boxplot
bp <- ggplot(bb, aes(x = 1, y = TEAM_BASERUN_SB)) + 
  stat_boxplot(geom ='errorbar') + geom_boxplot() + 
  xlab("Boxplot") + ylab("") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Density plot
hp <- ggplot(bb, aes(x = TEAM_BASERUN_SB)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + ylab("") + xlab("Density Plot with Mean") +
  geom_vline(aes(xintercept=mean(TEAM_BASERUN_SB, na.rm=TRUE)), color="red", linetype="dashed", size=1)

# Scatterplot
sp <- ggplot(data=bb, aes(x=TEAM_BASERUN_SB, y=TARGET_WINS)) + 
  geom_point() + geom_smooth(method = "loess") +
  xlab("Scatterplot with Best Fit Line")

grid.arrange(bp, hp, sp, layout_matrix=rbind(c(1,2,2),c(1,3,3)))

# Correlation matrix
cm <- cor(bb, use="pairwise.complete.obs")
cm <- cm[2:17,2:17]
names <- c("Wins", "H", "2B", "3B", "HR", "BB", "SO", "SB", "CS", "HBP", "P-H", "P-HR", "P-BB", "P-SO", "E", "DP")
colnames(cm) <- names; rownames(cm) <- names
cm <- round(cm, 2)
cmout <- as.data.frame(cm) %>% mutate_all(function(x) {
  cell_spec(x, "html", color = ifelse(x>0.5 | x<(-0.5),"blue","black"))
  })
rownames(cmout) <- names
cmout %>%
  kable("html", escape = F, align = "c", row.names = TRUE) %>%
  kable_styling("striped", full_width = F)

bbBackup <- bb

# Remove observations with no target
bb <- bb[which(bb$TARGET_WINS!=0), ]

# Reset zero values
bb[which(bb$TEAM_BATTING_H==0),"TEAM_BATTING_H"] <- NA
bb[which(bb$TEAM_BATTING_2B==0),"TEAM_BATTING_2B"] <- NA
bb[which(bb$TEAM_BATTING_3B==0),"TEAM_BATTING_3B"] <- NA
bb[which(bb$TEAM_BATTING_HR==0),"TEAM_BATTING_HR"] <- NA
bb[which(bb$TEAM_BATTING_BB==0),"TEAM_BATTING_BB"] <- NA
bb[which(bb$TEAM_BATTING_SO==0),"TEAM_BATTING_SO"] <- NA
bb[which(bb$TEAM_BASERUN_SB==0),"TEAM_BASERUN_SB"] <- NA
bb[which(bb$TEAM_BASERUN_CS==0),"TEAM_BASERUN_CS"] <- NA
bb[which(bb$TEAM_FIELDING_E==0),"TEAM_FIELDING_E"] <- NA
bb[which(bb$TEAM_FIELDING_DP==0),"TEAM_FIELDING_DP"] <- NA
bb[which(bb$TEAM_PITCHING_BB==0),"TEAM_PITCHING_BB"] <- NA
bb[which(bb$TEAM_PITCHING_H==0),"TEAM_PITCHING_H"] <- NA
bb[which(bb$TEAM_PITCHING_HR==0),"TEAM_PITCHING_HR"] <- NA
bb[which(bb$TEAM_PITCHING_SO==0),"TEAM_PITCHING_SO"] <- NA

# Impute mimssing values
bbImpute <- aregImpute(~ TARGET_WINS + TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + 
                         TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
                         TEAM_BASERUN_CS + TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_PITCHING_BB + 
                         TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_SO, 
                       data = bb, n.impute = 10)
bbImpute
bbImpute$rsq

bbI <- impute.transcan(bbImpute, imputation=10, data=bb, 
                       list.out=TRUE, pr=FALSE, check=FALSE)
bb$TEAM_BASERUN_SB <- bbI$TEAM_BASERUN_SB
bb$TEAM_BASERUN_CS <- bbI$TEAM_BASERUN_CS
bb$TEAM_BATTING_3B <- bbI$TEAM_BATTING_3B
bb$TEAM_BATTING_HR <- bbI$TEAM_BATTING_HR
bb$TEAM_BATTING_SO <- bbI$TEAM_BATTING_SO
bb$TEAM_FIELDING_DP <- bbI$TEAM_FIELDING_DP
bb$TEAM_PITCHING_HR <- bbI$TEAM_PITCHING_HR
bb$TEAM_PITCHING_SO <- bbI$TEAM_PITCHING_SO

# Adjust outliers
bb[which(bb$TEAM_PITCHING_SO>2500),"TEAM_PITCHING_SO"] <- 2500
bb[which(bb$TEAM_PITCHING_H>13000),"TEAM_PITCHING_H"] <- 13000
bb[which(bb$TEAM_PITCHING_BB>1100),"TEAM_PITCHING_BB"] <- 1100

# Creat singles
bb$TEAM_BATTING_S <- bb$TEAM_BATTING_H - bb$TEAM_BATTING_2B - bb$TEAM_BATTING_3B - bb$TEAM_BATTING_HR
summary(bb$TEAM_BATTING_S)

# Create log fielding error
bb$TEAM_FIELDING_E_LOG <- log(bb$TEAM_FIELDING_E)

# Model building
m1 <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_FIELDING_E, data=bb)
summary(m1)

m2 <- lm(TARGET_WINS ~ TEAM_BATTING_S + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
           TEAM_BATTING_BB + TEAM_FIELDING_E, data=bb)
summary(m2)
m2b <- lm(TARGET_WINS ~ TEAM_BATTING_S + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
           TEAM_BATTING_BB + TEAM_FIELDING_E_LOG, data=bb)
summary(m2b)

m3 <- lm(TARGET_WINS ~ TEAM_BATTING_SO:TEAM_BATTING_H + TEAM_BATTING_BB:TEAM_BATTING_H + TEAM_BATTING_SO + 
           TEAM_BASERUN_SB + TEAM_FIELDING_DP + TEAM_PITCHING_HR + TEAM_FIELDING_E_LOG, data=bb)
summary(m3)

m4 <- lm(TARGET_WINS ~ TEAM_BATTING_S + TEAM_BATTING_2B + TEAM_BATTING_3B + 
           TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
           TEAM_BASERUN_CS + TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_PITCHING_BB + 
           TEAM_PITCHING_H + TEAM_PITCHING_SO + TEAM_PITCHING_HR, data=bb)
summary(m4)
m4 <- lm(TARGET_WINS ~ TEAM_BATTING_S + TEAM_BATTING_2B + TEAM_BATTING_3B + 
           TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
           TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_PITCHING_BB + 
           TEAM_PITCHING_H + TEAM_PITCHING_SO + TEAM_PITCHING_HR, data=bb)
summary(m4)

m5 <- lm(TARGET_WINS ~ TEAM_BATTING_S  + TEAM_BATTING_3B + 
           TEAM_BATTING_HR + TEAM_BASERUN_SB  + 
           TEAM_FIELDING_E_LOG*TEAM_PITCHING_H, data=bb)
summary(m5)

# Residuals plots
plot(m4$residuals, ylab="Residuals")
abline(h=0)

plot(m4$fitted.values, m4$residuals, xlab="Fitted Values", ylab="Residuals")
abline(h=0)

qqnorm(m4$residuals)
qqline(m4$residuals)

# Test data for prediction
bbTest <- read.csv("moneyball-evaluation-data.csv")

bbTest[which(bbTest$TEAM_BATTING_H==0),"TEAM_BATTING_H"] <- NA
bbTest[which(bbTest$TEAM_BATTING_2B==0),"TEAM_BATTING_2B"] <- NA
bbTest[which(bbTest$TEAM_BATTING_3B==0),"TEAM_BATTING_3B"] <- NA
bbTest[which(bbTest$TEAM_BATTING_HR==0),"TEAM_BATTING_HR"] <- NA
bbTest[which(bbTest$TEAM_BATTING_BB==0),"TEAM_BATTING_BB"] <- NA
bbTest[which(bbTest$TEAM_BATTING_SO==0),"TEAM_BATTING_SO"] <- NA
bbTest[which(bbTest$TEAM_BASERUN_SB==0),"TEAM_BASERUN_SB"] <- NA
bbTest[which(bbTest$TEAM_BASERUN_CS==0),"TEAM_BASERUN_CS"] <- NA
bbTest[which(bbTest$TEAM_FIELDING_E==0),"TEAM_FIELDING_E"] <- NA
bbTest[which(bbTest$TEAM_FIELDING_DP==0),"TEAM_FIELDING_DP"] <- NA
bbTest[which(bbTest$TEAM_PITCHING_BB==0),"TEAM_PITCHING_BB"] <- NA
bbTest[which(bbTest$TEAM_PITCHING_H==0),"TEAM_PITCHING_H"] <- NA
bbTest[which(bbTest$TEAM_PITCHING_HR==0),"TEAM_PITCHING_HR"] <- NA
bbTest[which(bbTest$TEAM_PITCHING_SO==0),"TEAM_PITCHING_SO"] <- NA

# Impute mimssing values
bbImpute <- aregImpute(~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + 
                         TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
                         TEAM_BASERUN_CS + TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_PITCHING_BB + 
                         TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_SO, 
                       data = bbTest, n.impute = 10)
bbImpute
bbImpute$rsq

bbI <- impute.transcan(bbImpute, imputation=10, data=bbTest, 
                       list.out=TRUE, pr=FALSE, check=FALSE)
bbTest$TEAM_BATTING_HR <- bbI$TEAM_BATTING_HR
bbTest$TEAM_BATTING_SO <- bbI$TEAM_BATTING_SO
bbTest$TEAM_BASERUN_SB <- bbI$TEAM_BASERUN_SB
bbTest$TEAM_BASERUN_CS <- bbI$TEAM_BASERUN_CS
bbTest$TEAM_FIELDING_DP <- bbI$TEAM_FIELDING_DP
bbTest$TEAM_PITCHING_HR <- bbI$TEAM_PITCHING_HR
bbTest$TEAM_PITCHING_SO <- bbI$TEAM_PITCHING_SO

# Adjust outliers
bbTest[which(bbTest$TEAM_PITCHING_SO>2500),"TEAM_PITCHING_SO"] <- 2500
bbTest[which(bbTest$TEAM_PITCHING_H>13000),"TEAM_PITCHING_H"] <- 13000
bbTest[which(bbTest$TEAM_PITCHING_BB>1100),"TEAM_PITCHING_BB"] <- 1100

bbTest$TEAM_BATTING_S <- bbTest$TEAM_BATTING_H - bbTest$TEAM_BATTING_2B - bbTest$TEAM_BATTING_3B - bbTest$TEAM_BATTING_HR

bbTest$PREDICT_WIN <- predict(m4, newdata=bbTest, interval="confidence")

bbPredict <- cbind(bbTest$INDEX, bbTest$PREDICT_WIN[, 1], bbTest$PREDICT_WIN[, 2], bbTest$PREDICT_WIN[, 3])
colnames(bbPredict) <- c("Index", "Predicted Wins", "CI Lower", "CI Upper")
round(bbPredict,0)
