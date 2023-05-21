library(tidyverse)
library(ggplot2)
library(plotly)
library(data.table)

df <- read.csv("parkinsons.csv")

dim(df)
str(df)
summary(df)
#Dropping Subject column
df <- subset (df, select = -subject.)
df <- distinct(df)
df1 <- unique(df)

#Lowercase all columns
names(df) <- tolower(names(df))
names(df)

#Renaming all columns for better handling
setnames(df, old = c('jitter...','jitter.abs.','jitter.rap','jitter.ppq5','jitter.ddp','shimmer.db.','shimmer.apq3','shimmer.apq5','shimmer.apq11','shimmer.dda'), new = c('jitter','jitter_abs','jitter_rap','jitter_ppq5','jitter_ddp','shimmer_db','shimmer_apq3','shimmer_apq5','shimmer_apq11','shimmer_dda'))

View(df)
glimpse(df)
y <- df[6:21]

#Initializing DF to Null
parkinson_data_clean <- NULL

for (x in y){
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    x[x < (qnt[1] - H)] <- NA
    x[x > (qnt[2] + H)] <- NA
    x
  }
  
  parkinson_data_clean <- rbind(parkinson_data_clean,apply(df[, 0:21], 2, remove_outliers))
  parkinson_data_clean <- na.omit(parkinson_data_clean)
  
}

parkinson_data_clean <- as.data.frame(parkinson_data_clean)
parkinson_data_clean <- distinct(parkinson_data_clean)

boxplot(parkinson_data_clean[6:10])
boxplot(parkinson_data_clean[11:16])
boxplot(parkinson_data_clean[16:21])
#Looping through all attributes to remove outliers, combined the tables and eliminate dupes
for (x in y){
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    x[x < (qnt[1] - H)] <- NA
    x[x > (qnt[2] + H)] <- NA
    x
  }
  
  parkinson_data_clean <- rbind(parkinson_data_clean,apply(df[, 0:21], 2, remove_outliers))
  parkinson_data_clean <- na.omit(parkinson_data_clean)
  
}

parkinson_data_clean <- as.data.frame(parkinson_data_clean)
parkinson_data_clean <- distinct(parkinson_data_clean)

#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[6:10])
boxplot(df[6:10])

#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[11:16])
boxplot(df[11:16])


#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[16:21])
boxplot(df[16:21])

#Data exploration
#univariate analysis
motor_UPDRS <- parkinson_data_clean$motor_updrs
total_UPDRS <- parkinson_data_clean$total_updrs
#mean
mean(motor_UPDRS)
mean(total_UPDRS)
#median
median(motor_UPDRS)
median(total_UPDRS)
#iqr(interquartile range)
IQR(motor_UPDRS)
IQR(total_UPDRS)
#variance
var(motor_UPDRS)
var(total_UPDRS)
#standard deviation
sd(motor_UPDRS)
sd(total_UPDRS)

#density plot
ggplot(parkinson_data_clean, aes(x=motor_UPDRS), fill=motor_UPDRS) + geom_density()+ ggtitle("Density Plot of Motor UPDRS")
ggplot(parkinson_data_clean, aes(x=total_UPDRS), fill=total_UPDRS) + geom_density()+ggtitle("Density Plot of Total UPDRS")

# Create scatterplot
ggplot(parkinson_data_clean, aes(x = motor_updrs, y = total_updrs)) +geom_smooth() +
  geom_point()
ggplot(parkinson_data_clean, aes(x = jitter, y = total_updrs)) + geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = shimmer, y = total_updrs)) + geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = nhr, y = total_UPDRS)) +geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = hnr, y = total_UPDRS)) +geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = rpde, y = total_UPDRS)) +geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = dfa, y = total_UPDRS)) +geom_point() +
  geom_smooth()
ggplot(parkinson_data_clean, aes(x = ppe, y = total_UPDRS)) +geom_point() +
  geom_smooth()

# ANOVA test
#total_UPDRS
anova_1 <- aov( total_updrs ~ sex , data = parkinson_data_clean)
# Print the ANOVA table
summary(anova_1)
#motor_UPDRS
anova_2 <- aov(motor_updrs ~ sex, data = parkinson_data_clean)
summary(anova_2)
#Jitter
anova_3 <- aov(jitter ~ sex, data = parkinson_data_clean)
summary(anova_3)
#Shimmer
anova_4 <- aov(shimmer ~ sex, data = parkinson_data_clean)
summary(anova_4)
#NHR
anova_5 <- aov(nhr ~ sex, data = parkinson_data_clean)
summary(anova_5)
#HNR
anova_6 <- aov(hnr ~ sex, data = parkinson_data_clean)
summary(anova_6)
#RPDE
anova_7 <- aov(rpde ~ sex, data = parkinson_data_clean)
summary(anova_7)
#DFA
anova_8 <- aov(dfa ~ sex, data = parkinson_data_clean)
summary(anova_8)
#PPE
anova_9 <- aov(ppe ~ sex, data = parkinson_data_clean)
summary(anova_9)

#correlation matrix
library(corrplot)
res <- cor(parkinson_data_clean)
round(res, 2)
corrplot(res, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#t-test
male_total<- parkinson_data_clean$total_updrs[parkinson_data_clean$sex==0]
female_total<- parkinson_data_clean$total_updrs[parkinson_data_clean$sex==1]

index <- round(runif(250,1,2000))
male_total <- male_total[index]
female_total <- female_total[index]

t.test(male_total, female_total, var.equal=T)

