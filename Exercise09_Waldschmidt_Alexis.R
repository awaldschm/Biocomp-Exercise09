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


#I created a function with arguments that allow the user to receive a warning if
#there are less than 50 observations and override it with user input, as well as to chose:
#1. the directory path, 
#2. column analyzed (EC: with an ability to report if a file does not have that column 
#and outputs this non-existent column's coefficient value as NA), 
#3. EC: a logical argument that will allow them to chose whether or not they want 
#to ignore NA values in their code. na_ignore will only accurately take the 
#arguments TRUE, T, FALSE, or F
co_var_function <- function(dir, column, na_ignore){
  
#navigate to the directory with the files we want to analyze
setwd(dir)
  
#save all the files from the directory into an object
file_names <- Sys.glob("./*.csv")

#make the na_ignore argument logical
na_ignore <- as.logical(na_ignore)

#create empty vectors that are the length of the number of files in the directory (since we
#will only be looking at the coefficent of variation in one specified column across all files)
co_var <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
st_dev <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))
mean <- numeric(length(list.files("~/Intro to Biocomputing/R/Biocomp-Exercise09/test/")))

#create an empty vector for the user's input (for if that becomes necessary based on 
#the number of observations in the files)
input <- numeric(length = 1)

#create a for loop that will loop over all the files in the directory
for(i in 1:length(co_var)){
  
  #read in each data file for the loop
  data <- read.csv(file_names[i], header = T, sep = ",")
  
  #this will report if a file does not have the column specified in the arguments 
  #by the user. It will output this non-existent column's coefficient value as NA
  if(ncol(data) < column){
    print(file_names[i])
    print("The above file was skipped because it does not have the appropriate number of columns")
    st_dev[i] <- NA
    mean[i] <- NA
    co_var[i] <- NA
  
   #if the column does exist in the file, then the following code is used:
  }else{
    
    #This gives the user a warning if there are less than 50 observations and 
    #allows them to override it with user input if they still want to calculate
    #the coefficient. If they chose not to override the warning, the coefficient 
    #of that file's column will be reported as NA
    if(nrow(data) < 50){
    print(file_names[i])
    print("Warning: there are less than 50 observations present in the above file. Type Yes to proceed with the analysis, hit any other key to exit.")
    input <- readline()
    if(input == "Yes"){
      st_dev[i] <- sd(data[, column])
      mean[i] <- mean(data[, column])
      co_var[i] <- st_dev[i] / mean[i]
    }else{
      print("Analysis on the above file was skipped as per the user's request")
      st_dev[i] <- NA
      mean[i] <- NA
      co_var[i] <- NA
    }
  
  #this allows the user to chose whether or not they want to ignore NA values in their code
  }else if(na_ignore==TRUE){
    st_dev[i] <- sd(data[, column], na.rm = T)
    mean[i] <- mean(data[, column], na.rm = T)
    co_var[i] <- st_dev[i] / mean[i]
  
  #if none of the special cases above apply to the file, the coefficient will be 
  #calculated using the mean and standard deviation of the data in the file's column
  }else{
    st_dev[i] <- sd(data[, column])
    mean[i] <- mean(data[, column])
    co_var[i] <- st_dev[i] / mean[i]
  }
  }  
}

#Return the vector for the coefficient of variation of each file
print("Here is the final vector of the coefficient of variation for each file")
return(co_var)  
}  

#Test the function with random arguments:
co_var_function(dir = "~/Intro to Biocomputing/R/Biocomp-Exercise09/test", column = 2, na_ignore = F) #only some of the columns in df4.csv had NA in it (column 2 is one of them)
co_var_function(dir = "~/Intro to Biocomputing/R/Biocomp-Exercise09/test", column = 4, na_ignore = T)
