library(tidyverse)
library(ggplot2)
library(plotly)
library(data.table)

df <- read.csv("parkinsons.csv")

dim(df)
str(df)
summary(parkinson_data_clean)
#Dropping Subject column
df <- subset (df, select = -subject.)
df <- distinct(df)

#Lowercase all columns
names(df) <- tolower(names(df))
names(df)

df <- tibble::rowid_to_column(df, 'uid')

#Renaming all columns for better handling
setnames(df, old = c('jitter...','jitter.abs.','jitter.rap','jitter.ppq5','jitter.ddp','shimmer.db.','shimmer.apq3','shimmer.apq5','shimmer.apq11','shimmer.dda'), 
         new = c('jitter','jitter_abs','jitter_rap','jitter_ppq5','jitter_ddp','shimmer_db','shimmer_apq3','shimmer_apq5','shimmer_apq11','shimmer_dda'))

View(df)
glimpse(df)


#Exporting df to csv for checking (rmb to change file name to prevent overwrite)

filename <- paste(pathout, '/dataframe1.csv', sep = '')
write.csv(df, filename, row.names = FALSE)

#Plotting with outliers
boxplot.default(df[,12:17])


#Initializing DFs to NA
df2 <- NA

##First Round data treatment : Eliminate measurement error in outliers##
y <- df[7:22]
#Looping through all attributes to remove outliers, combined the tables and eliminate dupes
for (x in y){
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    x[x < (qnt[1] - H)] <- -1
    x[x > (qnt[2] + H)] <- 1
    x
  }
  
  df2 <- apply(df[, 7:22], 2, remove_outliers)   #exclude the first 6 columns when applying function
  df2 <- na.omit(df2)
  
}

#converge the columns in a df together before processing
df2 <- as.data.frame(df2)
df2 <- cbind(df[1:6],df2)
df2 <- distinct(df2)

#tallying up the total outliers
df2$outliers <- rowSums(df2[,7:22] == -1 | df2[,7:22] == 1)

#removing rows with outliers more than 7
df2 <- df2[df2$outliers<7,]

# Restoring remaining outliers value (-1 or 1) to their original value
# Check if uid column exists in df

parkinson_data_clean <- NA

if ("uid" %in% colnames(df2)) {
  parkinson_data_clean <- df2[df2$uid %in% df$uid, ] # Filter rows from df2 where uid exists in df
}

#Exporting first treatment to csv for checking (rmb to change file name to prevent overwrite)

filename <- paste(pathout, '/cleanedremoved1.csv', sep = '')
write.csv(df2, filename, row.names = FALSE)

##Second round data treatment : to impute remaining outliers##

# Replacing outliers value with their mean quartile value
for (x in y){
  impute_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    x[x < (qnt[1] - H)] <- qnt[1] - H
    x[x > (qnt[2] + H)] <- qnt[2] + H
    x
  }
  parkinson_data_clean <- apply(parkinson_data_clean[, 0:22], 2, impute_outliers) 
  parkinson_data_clean <- na.omit(parkinson_data_clean)
  
}


parkinson_data_clean <- as.data.frame(parkinson_data_clean)
parkinson_data_clean <- distinct(parkinson_data_clean)

# Removing columns if unnecessary / adding if necessary
parkinson_data_clean <- subset (parkinson_data_clean, select = -uid) #dont run if want to cross check uid
parkinson_data_clean <- cbind(parkinson_data_clean, df2[23]) #run if want to include outlier count


#Exporting second treatment to csv for checking (rmb to change file name to prevent overwrite)
filename <- paste(pathout, '/cleanedimputed3.csv', sep = '')
write.csv(parkinson_data_clean, filename, row.names = FALSE)

#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[6:10])
boxplot(df[7:11])

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

