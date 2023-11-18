rm(list = ls())
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
# rental_dropped_na <- rental_data %>% 
#   select(-all_of(all_nas_vector))   # AL: I changed it, so I don't need to change all the codes - is it okay?
rental_data <- rental_data %>% 
  select(-all_of(all_nas_vector))

# Drop more columns (based on data profiling):
rental_data <- rental_data %>% 
  select(-address, # no useful information
         -area_useable, # 93% NAs
         -bath, # 98% NAs
         -date, # no useful information (?)
         -dishwasher, # 98% NAs
         -dryer, # 99% NAs
         -gardenshed, # almost all NAs
         -heating_air, -heating_earth, -heating_electro, -heating_far, -heating_gas, 
         -heating_oil, -heating_pellets, # almost all NAs
         -lat, -lon, # coordinates
         -laundry, # almost all NAs
         -manlift, # almost all NAs
         -middle_house, # almost all NAs
         -month, # no useful information
         -oven, # almost all NAs
         -quarter_general, -quarter_specific, # no useful information
         -shower, # almost all NAs
         -toilets, # no useful information
         -year, # no useful information
  )


# 2. Dealing with NAs -----------------------------------------------------

# Insert mean-value for numerical values:
mean_area <- mean(rental_data$area, na.rm  = TRUE)
rental_data$area <- as.numeric(ifelse(is.na(rental_data$area), mean_area, rental_data$area))

mean_year_built <- mean(rental_data$year_built, na.rm  = TRUE)
rental_data$year_built <- as.numeric(ifelse(is.na(rental_data$year_built), mean_year_built, rental_data$year_built))

mean_wgh <- mean(rental_data$wgh_avg_sonnenklasse_per_egid, na.rm  = TRUE)
rental_data$wgh_avg_sonnenklasse_per_egid <- as.numeric(ifelse(is.na(rental_data$wgh_avg_sonnenklasse_per_egid), mean_wgh, rental_data$wgh_avg_sonnenklasse_per_egid))

mean_Anteil_auslaend <- mean(rental_data$Anteil_auslaend, na.rm  = TRUE)
rental_data$Anteil_auslaend <- as.numeric(ifelse(is.na(rental_data$Anteil_auslaend), mean_Anteil_auslaend, rental_data$Anteil_auslaend))

mean_avg_age <- mean(rental_data$Avg_age, na.rm  = TRUE)
rental_data$Avg_age <- as.numeric(ifelse(is.na(rental_data$Avg_age), mean_avg_age, rental_data$Avg_age))

mean_avg_size <- mean(rental_data$Avg_size_household, na.rm  = TRUE)
rental_data$Avg_size_household <- as.numeric(ifelse(is.na(rental_data$Avg_size_household), mean_avg_size, rental_data$Avg_size_household))

mean_anteil_efh <- mean(rental_data$anteil_efh, na.rm  = TRUE)
rental_data$anteil_efh <- as.numeric(ifelse(is.na(rental_data$anteil_efh), mean_anteil_efh, rental_data$anteil_efh))

mean_anz_geschosse <- mean(rental_data$avg_anzhl_geschosse, na.rm  = TRUE)
rental_data$avg_anzhl_geschosse <- as.numeric(ifelse(is.na(rental_data$avg_anzhl_geschosse), mean_anz_geschosse, rental_data$avg_anzhl_geschosse))

mean_avg_bauperiode <- mean(rental_data$avg_bauperiode, na.rm  = TRUE)
rental_data$avg_bauperiode <- as.numeric(ifelse(is.na(rental_data$avg_bauperiode), mean_avg_bauperiode, rental_data$avg_bauperiode))

rm(mean_Anteil_auslaend, mean_anteil_efh, mean_anz_geschosse, mean_area, 
   mean_avg_age, mean_avg_bauperiode, mean_avg_size, mean_wgh, mean_year_built)

# Not cleaned yet (question to teacher):                
# dist_to_haltst
# dist_to_lake
# dist_to_main_stat
# dist_to_school_1
# rooms: insert mode-value or use text analysis?


# Insert mode-value for categorical values:
mode <- function(x, na.rm = TRUE) {
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}
mode_floors <- mode(rental_data$floors)
rental_data$floors <- as.numeric(ifelse(is.na(rental_data$floors), mode_floors, rental_data$floors)) # numeric or factor?!
rm(mode_floors)


# Insert "No" = 0 for infrastructure:
rental_data$balcony <- as.factor(ifelse(is.na(rental_data$balcony), 0, 1))
rental_data$basement <- as.factor(ifelse(is.na(rental_data$basement), 0, 1))
rental_data$cabletv <- as.factor(ifelse(is.na(rental_data$cabletv), 0, 1))
rental_data$cheminee <- as.factor(ifelse(is.na(rental_data$cheminee), 0, 1))
rental_data$elevator <- as.factor(ifelse(is.na(rental_data$elevator), 0, 1))
rental_data$furnished <- as.factor(ifelse(is.na(rental_data$furnished), 0, 1))
rental_data$kids_friendly <- as.factor(ifelse(is.na(rental_data$kids_friendly), 0, 1))
rental_data$oldbuilding <- as.factor(ifelse(is.na(rental_data$oldbuilding), 0, 1))
rental_data$parking_indoor <- as.factor(ifelse(is.na(rental_data$parking_indoor), 0, 1))
rental_data$parking_outside <- as.factor(ifelse(is.na(rental_data$parking_outside), 0, 1))
rental_data$playground <- as.factor(ifelse(is.na(rental_data$playground), 0, 1))
rental_data$pool <- as.factor(ifelse(is.na(rental_data$pool), 0, 1))
rental_data$quiet <- as.factor(ifelse(is.na(rental_data$quiet), 0, 1))
rental_data$raised_groundfloor <- as.factor(ifelse(is.na(rental_data$raised_groundfloor), 0, 1))
rental_data$sunny <- as.factor(ifelse(is.na(rental_data$sunny), 0, 1))
rental_data$terrace <- as.factor(ifelse(is.na(rental_data$terrace), 0, 1))
rental_data$topstorage <- as.factor(ifelse(is.na(rental_data$topstorage), 0, 1))
rental_data$veranda <- as.factor(ifelse(is.na(rental_data$veranda), 0, 1))
rental_data$water <- as.factor(ifelse(is.na(rental_data$water), 0, 1))

# Insert 0 when probably not existing:
rental_data$size_land <- ifelse(is.na(rental_data$size_land), 0, rental_data$size_land)

# Check for NAs again:
colSums(is.na(rental_data))

## Assign correct data type in columns without NAs:
rental_data <- rental_data %>% 
  mutate(GDENAMK = as.factor(GDENAMK), 
         GDENR = as.factor(GDENR), 
         KTKZ = as.factor(KTKZ), 
         home_type = as.factor(home_type), 
         msregion = as.factor(msregion), 
         newly_built = as.factor(newly_built))


# 3. Investigate Anomalies in certain Variables ---------------------------

summary(rental_data)

#Area variable seems to have strangely low and high values
area_histogram <- ggplot(data=rental_dropped_na, aes(x=area))+
  geom_histogram()

#Most values seem to lie between 40m3 and 250 m3

str(rental_dropped_na)
summary(rental_dropped_na)

table(rental_dropped_na$rooms)


# Looking further into suspicious numbers:
# area: max 
ggplot(rental_data, aes(x = area)) + 
  geom_histogram()
rental_data %>% 
  filter(area > 250) %>% 
  ggplot(aes(x = rent_full)) + 
  geom_histogram()
# data cleaning needed, but not sure what to do - question...

# size_land: max 
ggplot(rental_data, aes(x = size_land)) + 
  geom_histogram()
rental_data %>% 
  filter(size_land > 100) %>% 
  ggplot(aes(x = size_land)) + 
  geom_histogram()
# data cleaning needed, but not sure what to do - question...

# year_built: min 
ggplot(rental_data, aes(x = year_built)) + 
  geom_histogram()
rental_data_year_built <- rental_data %>% 
  filter(year_built < 1750)
summary(rental_data_year_built)
ggplot(rental_data_year_built, aes(x = year_built)) + 
  geom_histogram()
rm(rental_data_year_built)
# might be possible in CH... therefore no adaptation (?)

# Avg_age: max 
ggplot(rental_data, aes(x = Avg_age)) + 
  geom_histogram()
# no adaption as this number comes from another source (?)

# Avg_size_household: max
ggplot(rental_data, aes(x = Avg_size_household)) + 
  geom_histogram()
# no adaption as this number comes from another source (?)


## Looking at rooms:
table(rental_data$rooms)
rental_data$rooms <- round(rental_data$rooms * 2, digits = 0) / 2 # round to the nearest 0.5
ggplot(rental_data, aes(x = rooms)) + 
  geom_histogram()

rental_data %>% 
  ggplot(aes(x = rooms, y = rent_full)) + 
  geom_jitter()
# apartment with 15 rooms only costs CHF 1000.- -> should probably be 1.5 rooms
# how to detect wrong numbers?
rental_data %>% 
  filter(rooms == 15) %>% 
  select(rent_full, anteil_efh, home_type)
rental_data$rooms <- ifelse((rental_data$rooms == 15) & (rental_data$home_type == "Wohnung"), 1.5, rental_data$rooms)

# Addressing NAs:
prop.table(table(is.na(rental_data$rooms))) * 100 # 1844 = 2.6%
# More information in description?
roomsNA_descr <- rental_data %>% 
  filter(is.na(rooms)) %>% 
  select(descr)
prop.table(table(is.na(roomsNA_descr))) * 100 # 77% with content -> text analysis possible, but is it effective?!
# Alternative for the moment: insert mode value, and round it to the nearest 0.5
mode_rooms <- mode(rental_data$rooms)
rental_data$rooms <- as.numeric(ifelse(is.na(rental_data$rooms), mode_rooms, rental_data$rooms))
rm(mode_rooms)




