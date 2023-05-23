pathout <- "C:/Users/Mackhem/Desktop/CDS501/parkinsons_telemonitoring"
setwd(pathout)

library(tidyverse)
library(data.table)
library(ggplot2)
library(reshape2)
df <- read.csv("parkinsons.csv")

dim(df)
str(df)
summary(df)
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
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H)] <- -1 
  x[x > (qnt[2] + H)] <- 1
  x
}

#Looping through all attributes to mark outliers, 1 = upper bound outlier, -1 = lower bound outlier
#Once done, combined the tables and eliminate dupes
for (x in y){
  df2 <- apply(df[, 7:22], 2, remove_outliers)   #exclude the first 6 columns when applying function
  df2 <- na.omit(df2)
  
}


df2 <- apply(df[, 7:22], 2, remove_outliers)   #exclude the first 6 columns when applying function
df2 <- na.omit(df2)

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

filename <- paste(pathout, '/cleanedremoved111.csv', sep = '')
write.csv(df2, filename, row.names = FALSE)

##Second round data treatment : to impute remaining outliers##

# Replacing outliers value with their upper/lower bound value
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
for(x in 7:22){
  
}
boxplot(parkinson_data_clean[7])
boxplot(df[7])


#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[11:16])
boxplot(df[11:16])


#Comparing cleaned and uncleaned data for the attributes
boxplot(parkinson_data_clean[17:21])
boxplot(df[17:21])


# Create a combined dataset
combined_df <- data.frame(
  NHR = c(df$nhr, parkinson_data_clean$nhr),
  Dataset = c(rep("Uncleaned NHR", nrow(df)), rep("Cleaned NHR", nrow(parkinson_data_clean)))
)

# Create a boxplot
ggplot(combined_df, aes(x = Dataset, y = NHR)) +
  geom_boxplot() +
  labs(title = "Boxplot of Uncleaned NHR vs Cleaned NHR") +
  xlab("Dataset") +
  ylab("NHR")




























































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
