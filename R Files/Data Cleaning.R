rm(list = ls())
library(tidyverse)

#Import dataset
# rental_data <- read_csv("Data/training_data.csv") # AL: with this import, it puts all values variable size_land to NA...
rental_data <- read.csv("Data/training_data.csv", header = TRUE, sep = ",") # AL: with this import, I get values in size_land


#Explore dataset
str(rental_data)
rental_data <- rental_data[, -1]
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
data <- rental_data %>% 
  select(-all_of(all_nas_vector))   # AL: I changed it from "rental_dropped_na" to "data" to make it shorter - is it okay?
rm(all_nas, total_nas, all_nas_vector, num_nas)

# Drop more columns (based on data profiling):
data <- data %>% 
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
mean_area <- mean(data$area, na.rm  = TRUE)
data$area <- as.numeric(ifelse(is.na(data$area), mean_area, data$area))

mean_year_built <- mean(data$year_built, na.rm  = TRUE)
data$year_built <- as.numeric(ifelse(is.na(data$year_built), mean_year_built, data$year_built))

mean_wgh <- mean(data$wgh_avg_sonnenklasse_per_egid, na.rm  = TRUE)
data$wgh_avg_sonnenklasse_per_egid <- as.numeric(ifelse(is.na(data$wgh_avg_sonnenklasse_per_egid), mean_wgh, data$wgh_avg_sonnenklasse_per_egid))

mean_Anteil_auslaend <- mean(data$Anteil_auslaend, na.rm  = TRUE)
data$Anteil_auslaend <- as.numeric(ifelse(is.na(data$Anteil_auslaend), mean_Anteil_auslaend, data$Anteil_auslaend))

mean_avg_age <- mean(data$Avg_age, na.rm  = TRUE)
data$Avg_age <- as.numeric(ifelse(is.na(data$Avg_age), mean_avg_age, data$Avg_age))

mean_avg_size <- mean(data$Avg_size_household, na.rm  = TRUE)
data$Avg_size_household <- as.numeric(ifelse(is.na(data$Avg_size_household), mean_avg_size, data$Avg_size_household))

mean_anteil_efh <- mean(data$anteil_efh, na.rm  = TRUE)
data$anteil_efh <- as.numeric(ifelse(is.na(data$anteil_efh), mean_anteil_efh, data$anteil_efh))

mean_anz_geschosse <- mean(data$avg_anzhl_geschosse, na.rm  = TRUE)
data$avg_anzhl_geschosse <- as.numeric(ifelse(is.na(data$avg_anzhl_geschosse), mean_anz_geschosse, data$avg_anzhl_geschosse))

mean_avg_bauperiode <- mean(data$avg_bauperiode, na.rm  = TRUE)
data$avg_bauperiode <- as.numeric(ifelse(is.na(data$avg_bauperiode), mean_avg_bauperiode, data$avg_bauperiode))

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
mode_floors <- mode(data$floors)
data$floors <- as.numeric(ifelse(is.na(data$floors), mode_floors, data$floors)) # numeric or factor?!
rm(mode_floors)


# Insert "No" = 0 for infrastructure:
data$balcony <- as.factor(ifelse(is.na(data$balcony), 0, 1))
data$basement <- as.factor(ifelse(is.na(data$basement), 0, 1))
data$cabletv <- as.factor(ifelse(is.na(data$cabletv), 0, 1))
data$cheminee <- as.factor(ifelse(is.na(data$cheminee), 0, 1))
data$elevator <- as.factor(ifelse(is.na(data$elevator), 0, 1))
data$furnished <- as.factor(ifelse(is.na(data$furnished), 0, 1))
data$kids_friendly <- as.factor(ifelse(is.na(data$kids_friendly), 0, 1))
data$oldbuilding <- as.factor(ifelse(is.na(data$oldbuilding), 0, 1))
data$parking_indoor <- as.factor(ifelse(is.na(data$parking_indoor), 0, 1))
data$parking_outside <- as.factor(ifelse(is.na(data$parking_outside), 0, 1))
data$playground <- as.factor(ifelse(is.na(data$playground), 0, 1))
data$pool <- as.factor(ifelse(is.na(data$pool), 0, 1))
data$quiet <- as.factor(ifelse(is.na(data$quiet), 0, 1))
data$raised_groundfloor <- as.factor(ifelse(is.na(data$raised_groundfloor), 0, 1))
data$sunny <- as.factor(ifelse(is.na(data$sunny), 0, 1))
data$terrace <- as.factor(ifelse(is.na(data$terrace), 0, 1))
data$topstorage <- as.factor(ifelse(is.na(data$topstorage), 0, 1))
data$veranda <- as.factor(ifelse(is.na(data$veranda), 0, 1))
data$water <- as.factor(ifelse(is.na(data$water), 0, 1))

# Insert 0 when probably not existing:
data$size_land <- ifelse(is.na(data$size_land), 0, data$size_land)

# Check for NAs again:
colSums(is.na(data))

## Assign correct data type in columns without NAs:
data <- data %>% 
  mutate(GDENAMK = as.factor(GDENAMK), 
         GDENR = as.factor(GDENR), 
         KTKZ = as.factor(KTKZ), 
         home_type = as.factor(home_type), 
         msregion = as.factor(msregion), 
         newly_built = as.factor(newly_built))


# 3. Investigate Anomalies in certain Variables ---------------------------

summary(data)

#Area variable seems to have strangely low and high values
area_histogram <- ggplot(data=data, aes(x=area))+
  geom_histogram()

#Most values seem to lie between 40m3 and 250 m3

str(data)
summary(data)

table(data$rooms)


# Looking further into suspicious numbers:
# area: max 
ggplot(data, aes(x = area)) + 
  geom_histogram()
data %>% 
  filter(area > 250) %>% 
  ggplot(aes(x = rent_full)) + 
  geom_histogram()
# data cleaning needed, but not sure what to do - question...

# size_land: max 
ggplot(data, aes(x = size_land)) + 
  geom_histogram()
data %>% 
  filter(size_land > 100) %>% 
  ggplot(aes(x = size_land)) + 
  geom_histogram()
# data cleaning needed, but not sure what to do - question...

# year_built: min 
ggplot(data, aes(x = year_built)) + 
  geom_histogram()
data_year_built <- data %>% 
  filter(year_built < 1750)
summary(data_year_built)
ggplot(data_year_built, aes(x = year_built)) + 
  geom_histogram()
rm(data_year_built)
# might be possible in CH... therefore no adaptation (?)

# Avg_age: max 
ggplot(data, aes(x = Avg_age)) + 
  geom_histogram()
# no adaption as this number comes from another source (?)

# Avg_size_household: max
ggplot(data, aes(x = Avg_size_household)) + 
  geom_histogram()
# no adaption as this number comes from another source (?)


## Looking at rooms:
table(data$rooms)
data$rooms <- round(data$rooms * 2, digits = 0) / 2 # round to the nearest 0.5
ggplot(data, aes(x = rooms)) + 
  geom_histogram()

data %>% 
  ggplot(aes(x = rooms, y = rent_full)) + 
  geom_jitter()
# apartment with 15 rooms only costs CHF 1000.- -> should probably be 1.5 rooms
# how to detect wrong numbers?
data %>% 
  filter(rooms == 15) %>% 
  select(rent_full, anteil_efh, home_type)
data$rooms <- ifelse((data$rooms == 15) & (data$home_type == "Wohnung"), 1.5, data$rooms)

# Addressing NAs:
prop.table(table(is.na(data$rooms))) * 100 # 1844 = 2.6%
# More information in description?
roomsNA_descr <- data %>% 
  filter(is.na(rooms)) %>% 
  select(descr)
prop.table(table(is.na(roomsNA_descr))) * 100 # 77% with content -> text analysis possible, but is it effective?!
# Alternative for the moment: insert mode value, and round it to the nearest 0.5
mode_rooms <- mode(data$rooms)
data$rooms <- as.numeric(ifelse(is.na(data$rooms), mode_rooms, data$rooms))
rm(mode_rooms)

summary(data$rooms)

# 4. Data Description ---------------------------------------------------------

data %>% 
  ggplot(aes(x = rent_full, y = rooms)) + 
  geom_jitter() + 
  ylim(0, 16) + 
  stat_smooth()

data %>% 
  ggplot(aes(x = rooms, y = rent_full)) + 
  geom_jitter() + 
  labs(title = "Number of rooms vs. rent", 
       x = "Number of rooms", 
       y = "Full rent (in CHF)")

data %>% 
  group_by(KTKZ) %>% 
  mutate(count = n()) %>% 
  ggplot(aes(y = reorder(KTKZ, count))) + 
  geom_bar()




