setwd("~/Desktop/")
#source to get all our data from the other R file
source(file="supportingFunctions.R") 

library(ggplot2)

### QUESTION 1
count_infection = 0
allData$infected <- 0
data_marker <- allData[,3:12]
allData
for(i in 1:nrow(data_marker)){
  for(j in range(1,10)){
    if(data_marker[i,j] == 1){
      count_infection = count_infection + 1
      allData$infected[i] <- 1
      break}
  }
}

#allData <- read.csv("~/Desktop/JustTrying/allData.csv")

sick_people = (allData %>%
           group_by(dayofYear, country, infected) %>%
           summarise(count = n()) %>%
           filter(infected == 1)
)

ggplot(data = sick_people, aes(x = dayofYear, y = count, color = country)) +
  geom_point(alpha = 0.5, size = 4) +
  labs(x = "Day of Year", y = "Number of Sick People", color = "Country") +
  geom_smooth(aes(x = dayofYear, y = count, color = country), inherit.aes = FALSE,
              method = "lm") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())  # Remove grid 



## here we see that Y isnt infected until day almost 140, this just means once again that this started in country x
##given the graphs above, We would think that the outbreak began in country X. Due to the large number infections in country X, We believe this is where the infection originated.




### QUESTION 2
## Question 2 
## Here I create a table that looks at the presence of each marker by country
coollistx <- c()  
coollisty <- c()
##I am creating two lists, each spot in the list relfects the number of times the marker is present by country. At the end I combine the two lists and am able to look at the number of times the maker is present by country.
data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker01 == "1")
coollistx[1] <- nrow(data_number) 

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker02 == "1")
coollistx[2] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker03 == "1")
coollistx[3] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker04 == "1")
coollistx[4] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker05 == "1")
coollistx[5] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker06 == "1")
coollistx[6] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker07 == "1")
coollistx[7] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker08 == "1")
coollistx[8] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker09 == "1")
coollistx[9] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "X") %>%
  filter(marker10 == "1")
coollistx[10] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker01 == "1")
coollisty[1] <- nrow(data_number) 

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker02 == "1")
coollisty[2] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker03 == "1")
coollisty[3] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker04 == "1")
coollisty[4] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker05 == "1")
coollisty[5] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker06 == "1")
coollisty[6] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker07 == "1")
coollisty[7] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker08 == "1")
coollisty[8] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker09 == "1")
coollisty[9] <- nrow(data_number)

data_number <- allData %>%
  filter(country == "Y") %>%
  filter(marker10 == "1")
coollisty[10] <- nrow(data_number)


##adding 
markers_per_country <- data.frame(coollistx,coollisty)
rownames(markers_per_country)<- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07", "marker08","marker09","marker10")
plot(markers_per_country$coollistx)
plot(markers_per_country$coollisty)

##after looking at the table and graphs, there is a very even divide between markers 1-5 in country X and markers 6-10 in country y. That being said, I would think that a vaccine developed in counry y would not be succesfull in country x. The genes present in each country are very different.country y would focus on combatting markers 6-10 which would ultimately be ineffective in country x. it would be the reverse if country x developed a vaccine (ie. they would focus on markers 1-5 which are not apparent in country y). 





