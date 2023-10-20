library(tidyverse)

#Import dataset
rental_data <- read_csv("Data/training_data.csv")

#Explore dataset
str(rental_data)
summary(rental_data)

# 1. Missing Values -------------------------------------------------------

#Identify columns with the greatest number of NAs
#Calculate the number of NAs for every column ad save result to named vector
num_nas <- sapply(rental_data, function(x) sum(is.na(x))) 

total_nas <- data.frame(num_nas) %>% 
  rownames_to_column(var = "Variable") %>% #Convert rownames to column
  mutate(fraction_na=round(num_nas/nrow(rental_data),digits=5)) %>%  # calculate fraction of nas
  arrange(fraction_na) # arrange in order of fewest missing

#Create vector where all values are missing
all_nas <- total_nas %>% 
  filter(fraction_na==1)
all_nas_vector <- all_nas$Variable

#Drop values where 100% of data is missing
rental_dropped_na <- rental_data %>% 
  select(-all_of(all_nas_vector))

# 2. Investigate Anomalies in certain Variables ---------------------------

#Area variable seems to have strangely low and high values
area_histogram <- ggplot(data=rental_dropped_na, aes(x=area))+
  geom_histogram()

#Most values seem to lie between 40m3 and 250 m3

str(rental_dropped_na)
summary(rental_dropped_na)

table(rental_dropped_na$rooms)



