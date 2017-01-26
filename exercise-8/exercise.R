#Use "dplyr" 
#Install.packages("dplyr")
library(dplyr)

#Load in SwissData from data set from data folder and view it to understand what is in it. 
data <- read.csv('data/SwissData.csv', stringsAsFactors = FALSE)
View(data)

#Add a column (using dpylr) that is the absolute difference between Education and Examination and call it 
# Educated.Score
data <- mutate(data, Educated.Score = abs(Education - Examination))


#Which area(s) had the largest difference 
largest.difference <- filter(data, max(Educated.Score) == Educated.Score) %>% select(Region)
print(largest.difference)

#Find which region has the highest percent of men in agriculture and retunr only the 
#percent and region name.  Use pipe operators to accomplish this. 
highest.percent <- filter(data, max(Agriculture) == Agriculture) %>% 
  select(Region, Agriculture)

print(highest.percent)

#Find the average of all infant.mortality rates and create a column (Mortality.Difference)
# showing the difference between a regions mortality rate and the mean. Arrange the dataframe in 
# Descending order based on this new column. Use pipe operators.
data <- mutate(data, Mortality.Difference = Infant.Mortality - mean(Infant.Mortality)) %>% 
  arrange(-Mortality.Difference)

# Create a new data frame that only is that of regions that have a Infant mortality rate less than the 
# mean.  Have this data frame only have the regions name, education and mortality rate. 
low.mr <- filter(data, Infant.Mortality < mean(Infant.Mortality)) %>% 
  select(Region, Education, Infant.Mortality)
print(low.mr)

#Filter one of the columns based on a question that you may have (which regions have a higher
#education rate, etc.) and write that to a csv file
higher.education <- filter(data, Infant.Mortality > mean(Infant.Mortality)) %>% 
  select(Region, Education, Infant.Mortality) %>% 
  arrange(-Education)
print(higher.education)
write.csv(higher.education, 'data/higher-edu.csv')
  
# Create a function that can take in two different region names and compare them based on a statistic 
# Of your choice (education, Examination, ect.)  print out a statment describing which one is greater 
# and return a data frame that holds the selected region and the compared variable.  If your feeling adventurous
# also have your function write to a csv file.

CompareRegion <- function(region1, region2) {
  region1.edu <- filter(data, region1 == Region) %>% 
    select(Education)
  region2.edu <- filter(data, region2 == Region) %>% 
    select(Education)
  if (region1.edu > region2.edu) {
    return(paste(region1, 'eduaction is greater than', region2, 'education'))
  } else {
    return(paste(region2, 'education is greater than', region1, 'education'))
  }
}

print(CompareRegion('Glane', 'Sarine'))






