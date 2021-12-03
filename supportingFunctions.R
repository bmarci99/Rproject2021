library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)



## WE HAVE A COUPLE OF ASSUMTIONS:
#There are some assumptions: 
# the main folder is JustTrying
#We are grabbing the data from inside JusTrying - country copy and country copy
#and our end directory is HERE inside the Just Trying


### Converting the files
setwd("~/Desktop/JustTrying/countryY copy/")
##Function to convert .txt to .csv
txt_to_csv <- function(Directory){
  setwd(Directory)
  TXTs <- list.files( pattern = "\\.txt$")
  for (i in 1:length(TXTs)){
    FILE=read.table(file=TXTs[i],header=T,sep="")
    write.table(FILE,file=paste0(sub(".txt","",TXTs[i]),".csv"),row.names=FALSE,quote=FALSE,sep=",")
  }
}
  
txt_to_csv("~/Desktop/JustTrying/countryY copy/")



### Combining files
combining_csv <- function(a,b,l){
  
  ycsv<- list.files(path = a, pattern = "\\.csv$", full.names = FALSE)
  setwd(a)
  file.rename(ycsv, paste0('Y_', ycsv))
  ycsv<- list.files(path = a, pattern = "\\.csv$", full.names = FALSE)
  for(file in ycsv) {file.copy(file, l)}
  #for Country X
  xcsv<- list.files(path = b, pattern = "\\.csv$", full.names = FALSE)
  setwd(b)
  file.rename(xcsv, paste0('X_', xcsv))
  xcsv<- list.files(path = b, pattern = "\\.csv$", full.names = FALSE)
  for(file in xcsv) {file.copy(file, l)}
  
  
  # read file path
  all_paths <-list.files(path = c(a, b), pattern = "\\.csv$", full.names = TRUE) 
  
  # read file content
  all_content <-
    all_paths%>%
    lapply(read.csv, header = TRUE)
  
  # read file name
  all_filenames <- all_paths %>%
    basename() %>%
    as.list()
  
  # combine file content list and file name list
  all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)
  
  
  # unlist all lists and change column name
  all_result <- as.data.frame(rbindlist(all_lists, fill = T))
  
  # add columns
  all_result <- cbind(all_result, str_split_fixed(all_result$V1, "_screen_", 2))
  
  # Editing columns
  names(all_result)[c(14,15)] <- c('country', 'dayofYear')
  all_result$dayofYear <- gsub('.{4}$', '', all_result$dayofYear)
  
  #deleting column
  all_result <- all_result[,-13]
  
  
  answer <- readline(" What do you want with the NAs? (1) remove rows with NA’s in (2) include rows, but get warning (3) include, no warning ")
  if(answer == "1"){
    all_result <- na.omit(all_result)}
  if(answer == "2"){
    print("these rows contain NA observations: ")
    print(is.na(all_result))}
  
  setwd(l)
  do.call(file.remove, list(list.files(full.names = TRUE)))
  write.csv(all_result, "allData.csv", row.names=FALSE)
  
  
}



ww <- "~/Desktop/JustTrying/countryY copy/"
qq <- "~/Desktop/JustTrying/countryX copy/"
pp<-   "~/Desktop/JustTrying/HERE/" # target dir


combining_csv(ww,qq,pp)



## Question 3
summarize_function <- function(directory_1,data_given){
  
  setwd(directory_1)
  data <- read.csv(data_given)
  
  ##obtaining male and female counts
  female_count <- sum(data$gender=="female")
  percent_female = female_count/length(data$gender)
  percent_male = 1-percent_female
  
  print("Percent Female: ")
  print(percent_female)
  print("Percent Male: ")
  print(percent_male)
  
  ##infection rate 
  ## here i also added a column to say whether a patient was infected or not. this is indicated by a 1 in the column
  count_infection = 0
  data$infected <- 0
  data_marker <- data[,3:12]
  data
  for(i in 1:nrow(data_marker)){
    for(j in range(1,10)){
      if(data_marker[i,j] == 1){
        count_infection = count_infection + 1
        data$infected[i] <- 1
        break}
    }
  }
  infection_rate = count_infection/nrow(data_marker)
  print("the infection rate is: ")
  print(infection_rate)
  
  ##number of screens run
  print("the number of screens run is: ")
  print(nrow(data_marker)*ncol(data_marker))
  ##age distribution
  print(ggplot(data = data, aes(x=age)) + geom_histogram(bins = 30) +  labs(x= "Age", y = "count", title = "Age Distribution"))
}

dir<-   "~/Desktop/JustTrying/HERE/" # target dir

data_given <- 'allData.csv'
allData <-  read.csv("~/Desktop/JustTrying/HERE/allData.csv")
summarize_function(dir,data_given)


rm(list = data_given, pp, qq, ww)
