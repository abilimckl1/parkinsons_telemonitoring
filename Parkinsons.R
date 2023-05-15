library(tidyverse)
library(data.table)
df <- read.csv("parkinsons.csv")

dim(df)
str(df)

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


<<<<<<< HEAD
#Plotting with outliers
boxplot.default(df[,11:16])


#Plotting without outliers
boxplot.default(df_Shimmer[,11:16])


#######################################################################
#Jitter Data Outliers processing
quartiles_Jitter <- quantile(df$jitter, probs=c(.25, .75), na.rm = TRUE)
IQR_Jitter <- IQR(df$jitter)
Lower_Jitter <- quartiles_Jitter[1] - 1.5*IQR_Jitter
Upper_Jitter <- quartiles_Jitter[2] + 1.5*IQR_Jitter

#Assigning Jitter data without Outliers to df_Jitter
df_Jitter <- subset(df, df$jitter > Lower_Jitter & df$jitter < Upper_Jitter)

#See outliers value
df_JitterLower <- subset(df, df$jitter < Lower_Jitter)
df_JitterUpper <- subset(df, df$jitter > Upper_Jitter)

View(df_JitterLower)
View(df_JitterUpper)
range(df_JitterLower$jitter) #Lower Outlier Jitter Range
range(df_JitterUpper$jitter) #Upper Outlier Jitter Range

range (df_Jitter$jitter) #Jitter without Outlier Range

#Plotting Jitter with outliers
boxplot(df[,6:10])

#Plotting Jitter without outliers  ERROR
boxplot.default(df_Jitter[,6:10])


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
=======
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


>>>>>>> e3ae3b46d4d942b4e1fc308786aa6838be8dbb0b

# ###################################################################### UNUSED CODE
# #Shimmers Data Outliers processing
# quartiles_Shimmer <- quantile(df$shimmer, probs=c(.25, .75), na.rm = FALSE)
# IQR_Shimmer <- IQR(df$shimmer)
# Lower_Shimmer <- quartiles_Shimmer[1] - 1.5*IQR_Shimmer
# Upper_Shimmer <- quartiles_Shimmer[2] + 1.5*IQR_Shimmer
# 
# #Assigning Shimmer data without Outliers to df_Shimmer
# df_Shimmer <- subset(df, df$shimmer > Lower_Shimmer & df$shimmer < Upper_Shimmer)
# 
# #See outliers value
# df_ShimmerLower <- subset(df, df$shimmer < Lower_Shimmer)
# df_ShimmerUpper <- subset (df, df$shimmer > Upper_Shimmer)
# 
# View(df_ShimmerLower)
# View(df_ShimmerUpper)
# 
# range(df_ShimmerLower$shimmer) #Lower Outlier Shimmer Range
# range(df_ShimmerUpper$shimmer) #Upper Outlier Shimmer Range
# 
# range(df_Shimmer$shimmer) #Shimmer without Outliers range
# 
# 
# #Plotting with outliers
# boxplot.default(df$shimmer, df$shimmer_db, df$shimmer_apq3, df$shimmer_apq5, df$shimmer_apq11, df$shimmer_dda)
# 
# 
# #Plotting without outliers
# boxplot.default(df_Shimmer$shimmer, df_Shimmer$shimmer_db, df_Shimmer$shimmer_apq3, df_Shimmer$shimmer_apq5, df_Shimmer$shimmer_apq11, df_Shimmer$shimmer_dda)
# 
# 
# ####################################################################### UNUSED CODE
# #Jitter Data Outliers processing
# quartiles_ Jitter <- quantile(df$jitter, probs=c(.25, .75), na.rm = FALSE)
# IQR_Jitter <- IQR(df$jitter)
# Lower_Jitter <- quartiles_Jitter[1] - 1.5*IQR_Jitter
# Upper_Jitter <- quartiles_Jitter[2] + 1.5*IQR_Jitter
# 
# #Assigning Jitter data without Outliers to df_Jitter
# df_Jitter <- subset(df, df$jitter > Lower_Jitter & df$jitter < Upper_Jitter)
# 
# #See outliers value
# df_JitterLower <- subset(df, df$jitter < Lower_Jitter)
# df_JitterUpper <- subset(df, df$jitter > Upper_Jitter)
# 
# View(df_JitterLower)
# View(df_JitterUpper)
# range(df_JitterLower$jitter) #Lower Outlier Jitter Range
# range(df_JitterUpper$jitter) #Upper Outlier Jitter Range
# 
# range (df_Jitter$jitter) #Jitter without Outlier Range
# 
# #Plotting Jitter with outliers
# boxplot.default(df$jitter, df$jitter_abs, df$jitter_rap, df$jitter_ppq5, df$jitter_ddp)
# 
# #Plotting Jitter without outliers  ERROR
# boxplot.default(df_Jitter$jitter, df_Jitter$jitter_abs, df_Jitter$jitter_rap, df_Jitter$jitter_ppq5, df_Jitter$jitter_ddp)
