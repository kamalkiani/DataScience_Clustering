
#----------- Installing required packages -----------
if (!require("janitor")) {
  install.packages("janitor")    
  library(janitor)              
}
if (!require("skimr")) {
  install.packages("skimr")    
  library(skimr)              
}
if (!require("dplyr")) {
  install.packages("dplyr")    
  library(dplyr)              
}
if (!require("caret")) {
  install.packages("caret")    
  library(caret)              
}
#----------- loading and preparing the data set -----------

data_name = "Credit_Card.csv"
data <- read.csv(data_name, header= TRUE)

#using the janitor package and converting all names to lowercase.
data<- data %>% janitor::clean_names()

#----------- exploring the data -----------
names(data)
summary(data)
str(data)
skim(data)

#----------- dealing with missing values -----------

#droping rows with missing values in the target columns (1 missing in credit_limit)
data<- data[!is.na(data$credit_limit),]
skim(data)

#replace missing values with mean value (313 missing values in minimum_payments)
data <- data %>% mutate(minimum_payments = 
                    ifelse(is.na(minimum_payments), mean(minimum_payments, na.rm=T), minimum_payments))
skim(data)

#----------- NORMALIZATION (IF NEEDED!) -----------
normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}
#remove non-numeric column before processing
data_numeric<-data[,2:18]
data_numeric <- as.data.frame(lapply(data_numeric,normalise)) 
boxplot(data_numeric)
#----------- identifying the outliers -----------

#Winsorizing replaces outliers with the values of the closest whisker.
winsorize <- function(in_col){
  #find the values of the outliers
  outlier_values<- boxplot.stats(in_col)$out
  #find the upper outliers
  upper_outliers<- outlier_values[outlier_values>mean(in_col)]
  #find the lower outliers
  lower_outliers<- outlier_values[outlier_values<mean(in_col)]
  #find the indices that correspond to upper and lower outliers
  upper_indices<- which(in_col %in% upper_outliers)
  lower_indices<- which(in_col %in% lower_outliers)
  #replace the upper and lower outliers with the upper and lower whisker values respectively
  in_col[upper_indices]<-boxplot.stats(in_col)$stats[5]
  in_col[lower_indices]<-boxplot.stats(in_col)$stats[1]
  return(in_col)
}

#calling my winsorizing function for replacing the outlier values 
data_numeric$balance = winsorize(data_numeric$balance)
data_numeric$purchases = winsorize(data_numeric$purchases)
data_numeric$oneoff_purchases = winsorize(data_numeric$oneoff_purchases)
data_numeric$installments_purchases = winsorize(data_numeric$installments_purchases)
data_numeric$cash_advance = winsorize(data_numeric$cash_advance)
data_numeric$credit_limit = winsorize(data_numeric$credit_limit)
data_numeric$payments = winsorize(data_numeric$payments)
data_numeric$minimum_payments = winsorize(data_numeric$minimum_payments)
data_numeric$purchases_trx = winsorize(data_numeric$purchases_trx)
data_numeric$cash_advance_trx = winsorize(data_numeric$cash_advance_trx)

boxplot(data_numeric)

#----------- exporting the data set -----------

data[,2:18] <- data_numeric
skim(data)
write.csv(data,"Credit_Card_Ready.csv", row.names = FALSE)



