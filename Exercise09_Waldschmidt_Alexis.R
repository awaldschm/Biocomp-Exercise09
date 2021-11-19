#Write a function that takes a directory name as an argument called dir plus 
#any other arguments required to accomplish the specified task.

#The function should read data from each file in the specified directory and 
#calculate the coefficient of variation (standard deviation divided by the mean)
#for a user specified column. These values should be returned as a vector
#   ie the user can only specify one column across all files

#To calculate a reliable coefficient of variation we would like to have 50 
#observations, but we also don't want to force the user to use our high standard 
#for the data. Make your function, by default, report an error if any file has 
#less than 50 observations, but allow the user to override this behavior and 
#only recieve a warning if 50 observations are not present in a file.

#For an extra credit point, add arguments and associated code to your function 
#to situations where a file doesn't have the correct number of columns or the 
#provided data includes NA's 

#set working directory
setwd("~/Intro to Biocomputing/R/Biocomp-Exercise09")


co_var_function <- function(directory_path, column, na_ignore){
setwd(directory_path)
file_names <- Sys.glob("./*.csv")
na_ignore <- as.logical(na_ignore)

#create an empty vector that is the length of the number of files (since we
#will only be looking at the coefficent of variation in one specified column across all files)
co_var <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
st_dev <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
mean <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))

input <- numeric(length =1)
for(i in 1:length(co_var)){
  data <- read.csv(file_names[i], header = T, sep = ",")
  if(nrow(data[i]) < 50){
    print(file_names[i])
    print("Warning: there are less than 50 observations present in the above file. Type Yes to proceed with the analysis, hit any other key to exit.")
    input <- readline()
    if(input == "Yes"){
      st_dev[i] <- sd(data[, column])
      mean[i] <- mean(data[, column])
      co_var[i] <- st_dev[i] / mean[i]
    }else{
      print("Analysis on this file was skipped")
      st_dev[i] <- NA
      mean[i] <- NA
      co_var[i] <- NA
    }
  }else if(na_ignore==TRUE){
    st_dev[i] <- sd(data[, column], na.rm = T)
    mean[i] <- mean(data[, column], na.rm = T)
    co_var[i] <- st_dev[i] / mean[i]
  }else{
    st_dev[i] <- sd(data[, column])
    mean[i] <- mean(data[, column])
    co_var[i] <- st_dev[i] / mean[i]
  }
}  
print("Here is the final vector of the coefficient of variation for each file")
return(co_var)  
}  

co_var_function(directory_path = "~/Intro to Biocomputing/R/Biocomp-Exercise09/test", column = 2, na_ignore = TRUE)

##Trying to add the EC
#For an extra credit point, add arguments and associated code to your function 
#to situations where a file doesn't have the correct number of columns or the 
#provided data includes NA's 
file_names <- Sys.glob("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/*.csv")

#create an empty vector that is the length of the number of files (since we
#will only be looking at the coefficent of variation in one specified column across all files)
co_var <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
st_dev <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
mean <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))


input <- numeric(length =1)
for(i in 1:length(co_var)){
  data <- read.csv(file_names[i], header = T, sep = ",")
  if(nrow(data[i]) < 50){
    print(file_names[i])
    print("Warning: there are less than 50 observations present in the above file. Type Yes to proceed with the analysis, hit any other key to exit.")
    input <- readline()
    if(input == "Yes"){
      st_dev[i] <- sd(data[,2])
      mean[i] <- mean(data[,2])
      co_var[i] <- st_dev[i] / mean[i]
    }else{
      print("Analysis on this file was skipped")
    }
  }else if(ncol(data) < ARGUMENT){
      print(file_name[i])
      print("This file was skipped because it does not have the appropriate number of columns")
  }else if(na_ignore (ARGUMENT) == "TRUE"){
    st_dev[i] <- sd(data[,2], na.rm = T)
    mean[i] <- mean(data[,2], na.rm = T)
    co_var[i] <- st_dev[i] / mean[i]
  } else{
    st_dev[i] <- sd(data[,2])
    mean[i] <- mean(data[,2])
    co_var[i] <- st_dev[i] / mean[i]
  }
}  
print(co_var)  








#attempt

Coef_of_var <- function(directory, col){
  files <- list.files(path = directory, full.names = T)
  for(i in 1:ncol(file))
  
  for (file in files){
  sample <- read.table(file, header = T, sep = ",")
  st_dev <- sd(file[,])
  }#end for loop
  return()
}#end function


Coef_of_var(directory = "~/Intro to Biocomputing/R/Biocomp-Exercise09/test", col = 2)

#make a test dataframe
df1 = data.frame(Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
                 Grade_score=c(4,6,2,9,5,7,8),
                 Mathematics1_score=c(45,78,44,89,66,49,72),
                 Science_score=c(56,52,45,88,33,90,47))
df1
write.table(x = df1, file = "df1.csv", sep = ",", col.names = T)
#load in data
df1 <- read.csv("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/df1.csv", header = T)
#head
head(df1)

df2 = data.frame(Name = c('George2','Andrea2', 'Micheal2','Maggie2','Ravi2','Xien2','Jalpa2'), 
                 Grade_score=c(42,62,22,92,52,72,82),
                 Mathematics1_score=c(452,782,442,892,662,492,722),
                 Science_score=c(562,522,452,882,332,902,472))
df2
write.table(x = df2, file = "df2.csv", sep = ",", col.names = T)
#load in data
df2 <- read.csv("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/df2.csv", header = T)
#head
head(df2)

df3 = data.frame(Name = c(1:100), 
                 Grade_score=c(1:100),
                 Mathematics1_score=c(101:200),
                 Science_score=c(101:200))
df3
write.table(x = df3, file = "df3.csv", sep = ",", col.names = T)
#load in data
df3 <- read.csv("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/df3.csv", header = T)
#head
head(df3)

df4 = data.frame(Name = c(1:99, NA), 
                 Grade_score=c(NA, 2:100),
                 Mathematics1_score=c(101:199, NA),
                 Science_score=c(101:200))
df4
write.table(x = df4, file = "df4.csv", sep = ",", col.names = T)
#load in data
df4 <- read.csv("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/df4.csv", header = T)
#head
head(df4)
