library(tidyverse)
library(knitr)
library(lubridate)
library(readr)
library(corrplot)
library(caret)
autos <- read_csv("data/autos.csv")


## DATA EXPLORATION

nrow(autos)
#354687 cars

summary(autos)

## DATA CLEANING

autos_clean <- autos

#First in the dataset we see two columns that for sure are only used by ebay. Those columns are abtest and nrOfPictures (all zeros)

#There is only 3 cars offered by dealers and the others all are private. The price by a dealer is generally higher than a private.
table(autos$seller)
#    dealer    private
#gewerblich     privat 
#         3     354684 

#There is only 12 cars requested and generally when the car is requested the price is lower.
table(autos$offerType)
#  offer  request
#Angebot   Gesuch 
# 354675       12 

#Also the postalCode variable is are not usefull in our project

#We will remove the 6 columns.
autos_clean <- autos_clean %>% select(-abtest, -nrOfPictures, -seller, -offerType, -postalCode)


### Cleaning yearOfRegistration

summary(autos$yearOfRegistration)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1000    1999    2003    2005    2008    9999 

#Let´s take only the cars until the 100 years of age
autos_clean <- autos_clean %>% filter(yearOfRegistration >= year(max(dateCreated)) - 100 & 
                                        yearOfRegistration <= year(max(dateCreated)))


### Generating the age variable

#In the market of used car is very important know the age of the car. This we can calculate from the difference between the add in ebay and the year of the registration.
#When a byer search a car look the age of the car in years, there is no usefull have a monthOfRegistration variable.
autos_clean <- autos_clean %>%  mutate(age = year(dateCreated) - yearOfRegistration) %>% 
  select(-monthOfRegistration)


### Generating the daysOnEbay variable

#Other data that can be usefull is the days that the add were posted in eBay. this data we can obtain from the difference between the dataCrawled and the dataCreated.
#The lastSeen variable is not usefull for us 

autos_clean <- autos_clean %>% 
  mutate(daysOnEbay = round(as.numeric(difftime(dateCrawled, dateCreated, units = "days")))) %>%
  select(-dateCrawled, -dateCreated, -lastSeen) 


### Cleaning Price

summary(autos$price)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 1.150e+03 2.950e+03 1.760e+04 7.200e+03 2.147e+09

#In the prices columns we see prices from 0 to 2x10^9. A supercar like a Bugatti or Ferrari cost 2 millions. Also we can ignore cars with the price lower than 100$ (ussually only for repair)

#First we will try cleaning the low prices zone inspecting the dataset for cars with prices under the 1000$ and a age 4 years or below. We soppose that this are not functional cars.
autos_clean %>% filter(age<=4 & price<=500) %>% ggplot(aes(price)) + geom_histogram()

#There are 3300 "cars" below 500$ and a age of 4 or less but 1000 of them have a 0 price.
#Also we will delete all cars below a price of 200$ because we need a functional car.
#We can delete this data for a first step and set a top limit of 300000$ for the price
autos_clean <- autos_clean %>% filter(price > 200 & price < 300000 & !(price <= 500 & age <=4))

#Let´s inspect the remaining cars with a age of 2 or below and a price lower than 1000$
#Looking in the names we found that exist many cars in Leasing or "financierung" (financed), this explain the lower prices.
#Also there are names like "zum schlachten" (to sacrifice) or "schaden" (dañado) or "bastler" (hobbyest car).
#There are so many financed cars, damaged cars, parts and other merchandicing related to the car market in the range under the 200$ that we can eliminate all data below the 500$

#Let´s look in all names the leasing and finnced cars
key_words <- c("leasing", "finanzierung")
autos_clean %>% filter(str_detect(str_to_lower(autos_clean$name), 
                                  paste(key_words, collapse = "|"))) %>%
  select(name, price, age) %>% arrange(desc(price)) %>% slice(70:80)

#There are very new financed or leasing cars and the prices sometimes are the cash and others the quotes
#We can delete all the cars with 2 or less years and the price below 3000$
autos_clean <- autos_clean %>% filter(!(str_detect(str_to_lower(autos_clean$name), 
                                                 paste(key_words, collapse = "|")) &
                                      price <= 3000 & age <= 2))

#Now let´s eliminate all damage and hobbyest cars
key_words <- c("schlachten", "schaden", "bastler")
autos_clean <- autos_clean %>% filter(!(str_detect(str_to_lower(autos_clean$name), 
                                                   paste(key_words, collapse = "|"))))


### Cleaning model

#If we convert the "andere" to NA and look the model variable we see 41259, is a great ammount of data without the model.
#In the name variable that is a free camp writed by the user whith the relevant information and the model is there in almost all cases.
#We will search in the name the model of the car.

autos_clean$model[autos_clean$model == "andere"] <- NA
ind_no_model <- which(is.na(autos_clean$model))
#41259 no model

all_models <- unique(na.omit(autos_clean$model))

search_model_in_name <- function(name){
  ind <- which(all_models %in% str_split(str_to_lower(name), pattern = "_")[[1]])
  ifelse(length(ind) == 1, return(all_models[ind]), return(NA))
}

autos_clean$model[ind_no_model] <- sapply(autos_clean$name[ind_no_model], 
                                          search_model_in_name, USE.NAMES = FALSE)
sum(is.na(autos_clean$model))
#31111 NA in model

#After the search we found 10148 missing models in the name variable. All the unknown models are NA and we need to remove them
autos_clean <- autos_clean %>% drop_na(model)


### Cleaning brand

#We can do the same method of the model cleaning searching the brand in the name variable. 
#There aren´t NA in the brand but if we look the dataset there is a "sonstige_autos" (others cars)

ind_no_brand <- which(autos_clean$brand == "sonstige_autos")
#306 no brand

all_brands <- unique(autos_clean$brand[-ind_no_brand])

search_brand_in_name <- function(name){
  ind <- which(all_brands %in% str_split(str_to_lower(name), pattern = "_")[[1]])
  ifelse(length(ind) == 1, return(all_brands[ind]), return(NA))
}

autos_clean$brand[ind_no_brand] <- sapply(autos_clean$name[ind_no_brand], 
                                          search_brand_in_name, USE.NAMES = FALSE)
length(which(is.na(autos_clean$brand)))
#261 NA in model

#After the search we found 45 missing brands in the name variable. All the unknown brands are NA and we need to remove them
autos_clean <- autos_clean %>% drop_na(brand)


### Generating the brandModel variable to help the cleaning process

#Now that we have the brand and model clean we can create a varible that combine both. This varible is usefull to the data exploring.
#Only the brand sometimes is not sufficient, into a brand exist many models with a great spread of prices.

autos_clean <- autos_clean %>% mutate(brandModel = paste(brand, model, sep = "_"))


### Generating the vehicleClass variable to help the cleaning process

#Add the vehicle class column: high, mid, low
autos_clean <- autos_clean %>% 
  mutate( vehicleClass = case_when(brand %in% c("mercedes_benz", "bmw", "audi", "jaguar",
                                                "porsche", "land_rover", "saab", "jeep") ~ "high",
                                   brand %in% c("volvo", "chrysler", "mini",
                                                "volkswagen", "alfa_romeo", "subaru") ~ "mid_high",
                                   brand %in% c("ford", "mazda", "nissan", "opel",
                                                "honda", "toyota", "skoda", "seat", 
                                                "mitsubishi", "suzuki") ~ "mid",
                                   brand %in% c("chevrolet", "kia", "hyundai", "fiat", "lancia",
                                                "citroen", "peugeot", "renault", 
                                                "smart") ~ "mid_low",
                                   brand %in% c("daewoo", "trabant", "lada",
                                                "rover", "daihatsu", "dacia") ~ "low"
  )
  )

#Plot of the vehicle classes
autos_clean %>% ggplot(aes(price, color=vehicleClass)) + geom_boxplot() + scale_x_log10()


## Cleaning gearbox

#Same process like the model and brand searching the gearbox in the name

ind_no_gear <- which(is.na(autos_clean$gearbox))
#13062 no gear

all_gear <- unique(autos_clean$gearbox[-ind_no_gear])

search_gear_in_name <- function(name){
  ind <- which(all_gear %in% str_split(str_to_lower(name), pattern = "_")[[1]])
  ifelse(length(ind) == 1, return(all_gear[ind]), return(NA))
}

autos_clean$gearbox[ind_no_gear] <- sapply(autos_clean$name[ind_no_gear], 
                                           search_gear_in_name, USE.NAMES = FALSE)
length(which(is.na(autos_clean$gearbox)))
#12931 NA in model

#After the search we found 131 missing gearbox in the name variable. All the unknown are NA and we need to remove them
autos_clean <- autos_clean %>% drop_na(gearbox)


## Cleaning powerPS

summary(autos_clean$powerPS)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    70.0   105.0   115.5   150.0 20000.0 

#In the brands we found trabant and porsche like the brands with less and more power in their cars.
autos_clean %>% filter(brand %in% c("trabant", "porsche") & powerPS >= 10) %>% 
  ggplot(aes(powerPS, brand)) + geom_point() + scale_x_log10()

#With this we can limit the powerPS variable between 20 and 800
autos_clean <- autos_clean %>% filter(powerPS >= 20 & powerPS <= 800)

#Plot of the powerPS per vehicleClass
autos_clean %>% ggplot(aes(vehicleClass, powerPS)) + geom_point(alpha=0.2) + scale_y_log10()

#If we inspect the data we find:

#Less power in high class
autos_clean %>% filter(vehicleClass == "high") %>%
  select(brandModel, powerPS) %>% top_n(-10, powerPS) %>% arrange(powerPS)
#power >= 50 

#Most power in mid_high class
autos_clean %>% filter(vehicleClass == "mid_high") %>%
  select(brandModel, powerPS) %>% top_n(10, powerPS) %>% arrange(desc(powerPS))
#power <= 550 (volkswagen golf)

#Less power in mid_high class
autos_clean %>% filter(vehicleClass == "mid_high") %>%
  select(brandModel, powerPS) %>% top_n(-10, powerPS) %>% arrange(powerPS)
#power >=30

#Most power in mid class
autos_clean %>% filter(vehicleClass == "mid") %>%
  select(brandModel, powerPS) %>% top_n(10, powerPS) %>% arrange(desc(powerPS))
#power <= 550 (ford mustang)

#Most power in mid_low class
autos_clean %>% filter(vehicleClass == "mid_low") %>%
  select(brandModel, powerPS) %>% top_n(60, powerPS) %>% arrange(desc(powerPS)) %>% kable()
#power <= 290 (renault megane)

#Most power in low class
autos_clean %>% filter(vehicleClass == "low") %>%
  select(brandModel, powerPS) %>% top_n(10, powerPS) %>% arrange(desc(powerPS))
#power <= 150

#Viewing this data we find many errors between the rover and land_rover brands
#To fix this we can only include the rover_200 and discard all the others with the rover brand

autos_clean <- autos_clean %>% filter(!(brand=="rover" & model != "200"))


autos_clean <- autos_clean %>% filter((vehicleClass == "high" & powerPS >= 50) | 
                                        (vehicleClass == "mid_high" & powerPS <= 550) |
                                        (vehicleClass == "mid_high" & powerPS >= 30) |
                                        (vehicleClass == "mid" & powerPS <= 550) | 
                                        (vehicleClass == "mid_low" & powerPS <= 290) | 
                                        (vehicleClass == "low"  & powerPS <= 150))


### Cleaning vehicleType

table(autos_clean$vehicleType)
# otros                                   small_car    station      sedan
#andere        bus     cabrio      coupe kleinwagen      kombi  limousine        suv 
#  1720      22139      18188      12727      62251      55751      76596      10414 
sum(is.na(autos_clean$vehicleType))

#All we can do is remove the NA and "andere"
autos_clean <- autos_clean %>% filter(vehicleType != "andere" & !is.na(vehicleType))

#Let´s look a pic with the vehicle types and the number of individual models into them
autos_clean %>% group_by(vehicleType, brandModel) %>% summarize(count=n()) %>%
  ggplot(aes(count, color=vehicleType)) + geom_density(bw=0.1) + scale_x_log10()

#We can find that there is a lot of uniques models into each type of cars

#Let´s look for example in the kleinwagen type the brandModel that count only 1. There are many errors like subaru_impreza, suzuki_grand, mercedes_benz_e_klasse

autos_clean %>% group_by(vehicleType, brandModel) %>% summarize(count=n()) %>%
  filter(vehicleType=="kleinwagen", count == 1) %>% .$brandModel

#With this in consideration we can think that this uniques brandModel entries are error and we will remove all these uniques entries from all the vehicle types

autos_clean <- autos_clean %>% group_by(vehicleType, brandModel) %>%
  mutate(count=n()) %>% filter(n() != 1) %>% select(-count) %>% ungroup()


### Cleaning fuelType

# $fuelType
table(autos$fuelType)
#andere  benzin     cng  diesel elektro  hybrid     lpg 
#   197  213642     547  102917      99     266    5121
sum(is.na(autos_clean$fuelType))


#Same process like the model, brand and gearbox searching the fuelType in the name

autos_clean$fuelType[autos_clean$fuelType == "andere"] <- NA

ind_no_fuel <- which(is.na(autos_clean$fuelType))


all_fuel <- unique(autos_clean$fuelType[-ind_no_fuel])

search_fuel_in_name <- function(name){
  ind <- which(all_fuel %in% str_split(str_to_lower(name), pattern = "_")[[1]])
  ifelse(length(ind) == 1, return(all_fuel[ind]), return(NA))
}

autos_clean$fuelType[ind_no_fuel] <- sapply(autos_clean$name[ind_no_fuel], 
                                            search_fuel_in_name, USE.NAMES = FALSE)
sum(is.na(autos_clean$fuelType))
#8195 NA in fuelType

#After the search we found 368 missing fuelType in the name variable. All the unknown are NA and we need to remove them
autos_clean <- autos_clean %>% drop_na(fuelType)


### Cleaning the notRepairedDamage

table(autos_clean$notRepairedDamage)
sum(is.na(autos_clean$notRepairedDamage))
#  yes      no   
#   ja    nein     NA
#21628  199986  28249

#This variable tells us if the car have a damage to be repaired by the byer.

#Plot of the notRepairedDamage vs the price variable
autos_clean %>% ggplot(aes(notRepairedDamage, price)) + geom_boxplot() + scale_y_log10()

#From the plot we can see that the avg price of a damage vehicle is near 2000$ and the avg price for the not damaged is near 8000$.
#The NA are in the middle with a avg price of 4000$
#With this we conclude that a damaged car is 1/4 the price of an andameged. Our dataset will only include the not damaged cars because the price of a damaged car is unpredictble.
#The NAs datas has 1/2 the price of an undamaged car. There is a great chance that are included damaged cars in the NAs datas. Also we will not include the NAs.

#Deleting the damaged cars and the notRepairedDamage NAs and then removing the column
autos_clean <- autos_clean %>% filter(notRepairedDamage != "ja" & !is.na(notRepairedDamage)) %>%
  select(-notRepairedDamage)


### Deleting models with low offres

autos_clean %>% group_by(brandModel) %>% summarize(count=n()) %>% filter(count <= 3) %>% 
  kable()

#As we can see there are many errors in this table.
#We can eliminate this entries. In our proyect we are interested in search prices of populars cars that are offered in ebay. 

autos_clean <- autos_clean %>% group_by(brandModel) %>% mutate(count=n()) %>% 
  filter(count > 3) %>% ungroup() %>% select(-count)


nrow(autos_clean)
#199604 rows

summary(autos_clean)


## DATA ANALYSIS

#In this section we will analyze the relation that have the variables with the price, that is the variable more important for us for our predictor algorithm.


### Price vs age

#A very important factor to buy a car is the age of the car. As we know, a 0km car in only a day can loose 20% in value.
#But also we know that cars with a high age can increase their values.

#Lets make a graph of all dataset cars vs their ages
autos_clean %>% ggplot(aes(age, price)) + geom_point(alpha=0.05) + scale_y_log10()

#We can see that the price in general is decreasing in the first 20 years and then begins to rise.

#Let´s find the 5 most models offered to analyze the prices in time.
most_offer_models <- autos_clean %>% group_by(brandModel) %>% summarize(count=n()) %>% 
  top_n(5, count) %>% arrange(desc(count))

most_offer_models %>% ggplot(aes(count, reorder(brandModel, count))) + geom_col()

#Let´s view what happened with this models prices in time
autos_clean %>% filter(brandModel %in% most_offer_models$brandModel & age <= 40) %>% 
  ggplot(aes(age, price, color=brandModel)) + geom_smooth() + scale_y_log10()

#All the brands have the same curve, in the first 20 years their price decrease to 1/10 of the original price. Then begin to rice in the next 20 years.

#Another factor that can be usefull is analize the number of cars offered by age
autos_clean %>% group_by(age) %>% summarize(count=n()) %>%
  ggplot(aes(age, count)) + geom_col()

#We can see that the mayor number of offered cars is for cars near the 10 years of age.

summary(autos_clean$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.00    8.00   12.00   11.97   16.00   78.00 

#If we need to classify the cars per age we can create 4 groups:
#0-7    8-11    12-15   16-80


### Price vs brand

#If we make a plot of average price per brand we look as we can imagine that are brands more expensive like others.

#Plot of the brands versus their average prices
autos_clean %>% group_by(brand) %>% summarize(avg_price = mean(price)) %>%
  ggplot(aes(avg_price, reorder(brand, avg_price))) + geom_point() + scale_x_log10()

#Is courious that brands like mini, kia and hyundai are in average more expensive than others brands like volvo, jaguar and chrysler.

#We can think that mini, kia and hyundai are relative new brands in comparision with the others that have vehicles with more age and less price.

#Now we take the cars near 12 years of age to eliminate the influence of the age.
autos_clean %>% group_by(brand) %>% filter(age>10 & age<14) %>%
  summarize(avg_price = mean(price)) %>%
  ggplot(aes(avg_price, reorder(brand, avg_price))) + geom_point() + scale_x_log10()

#Now the graph shows the price of the cars as we expected, and also confirm the importance of the car age.


### Price vs powerPS

#Generally the price of a car is asociated to the power of the engine.
#Small urban cars have engine less than 100HP and in the way that the car is bigger the engine and the power is increasing.

autos_clean %>% ggplot(aes(powerPS, price)) + geom_point(aes(color=vehicleClass)) +
  geom_smooth() + scale_x_log10() + scale_y_log10()

#In the graph we can see a linear relation between the power and price of the car.
#Also we can note that the high is the class of a vehicle the high is the power and price.


### Price vs kilometer

#A important variable to consider by a car byer is the kilometer that have the car. To more kilometer the car have, the more wear that present and the price must be lower.

#Let´s look the distribution of the kilometer variable.
autos_clean %>% ggplot(aes(kilometer)) + geom_histogram()

#60% of the market is dominated by cars with 150000 kilometers on it. With this we can consider that the mayority off users sells their cars after make a lot of kilometers and the car begins to present some problems.

#Is really important analize the kilometers per year. Let´s create a new variable kmsYear and plot it.

autos_clean %>% filter(age != 0) %>% mutate(kmsYear=round(kilometer/age)) %>%
  ggplot(aes(kmsYear)) + geom_histogram() + scale_y_log10() + scale_x_log10()

#We can find that a few people only drive their cars only less than 1000 kms/year and other few drivers use their cars over 50000 kms/year.
#The mayor part of drivers use their cars around 10000 kms/year.

#Lets analize whats going on with the most offered cars that we used previusly.
#When comparing the price with the kms/year of this 5 cars and pick only the ones around a age of 12 years (to lower the age effect) we have:
autos_clean %>% mutate(kmsYear=round(kilometer/age)) %>% 
  filter(brandModel %in% most_offer_models$brandModel & age>10 & age<14) %>%
  ggplot(aes(kmsYear, price, color=brandModel)) + geom_smooth()

#If we look at the audi_a4 and the bmw_3er we can see a great difference in prices when the kms/year change.
#For kms/year lower than 8000, we note a significant increase in the prices and for kms/year higher than 8000 we have a decreace in the prices.
#With the opel_corsa and volkswagen polo (that are the most cheaper from this group) we note very little variation in the prices against the kms/year of the car.
#The volkswagen_golf (the medium range price of the group) have a similar behaviour of the bmw and audi but the difference in price are lower.

#We can conclude that in the way the price of a car became higher the more higher is the inflence of the of the kilometers that have the car. A expensive car with many kms can be more expensive to repair and mantain, for that reason the price is lower.


### Price vs vehicleClass

#As we saw in the last pic the vehicle class is influenced by the price, we have elaborated this classification with the level of prices of each brand.

#Let´s plot the avg price of each class
autos_clean %>% group_by(vehicleClass) %>% 
  summarize(avg_price=mean(price)) %>%
  ggplot(aes(reorder(vehicleClass, avg_price), avg_price)) + geom_col()

#As we can see, the differences in classes is not as we expected. This is because the effect of the age is pressent. Perhaps are more older or newer cars in one class than others.

#If we eliminate the effect of the age taking only the ages with more offers (11-13)
autos_clean %>% filter(age>10 & age<14) %>% group_by(vehicleClass) %>% 
  summarize(avg_price=mean(price)) %>%
  ggplot(aes(reorder(vehicleClass, avg_price), avg_price)) + geom_col()

#Now is clear the differences class by class, and we can see the strong relation between the class and the prices.


### Price vs vehicleType

#If we look the entire dataset we obtain this

autos_clean %>% group_by(vehicleType) %>% 
  summarize(avg_price=mean(price)) %>%
  ggplot(aes(reorder(vehicleType, avg_price), avg_price)) + geom_col()

#There is a logic here, the kleiwagen (small car) is the cheaper and the cabrio, coupe and suv are the more expensive types.
#We expected that the coupe type were the more expensive than the suv type. 
#The suv market is relatively a new segment in the car market and for sure there are more newest models offering on ebay than the cabrio type,  for example, that have been always in the car industry.

#Lets eliminate the age effect to corroborate this
autos_clean %>% filter(age>10 & age<14) %>% group_by(vehicleType) %>% 
  summarize(avg_price=mean(price)) %>%
  ggplot(aes(reorder(vehicleType, avg_price), avg_price)) + geom_col()

#Now we can see that the sports cars (more powerfull) like the coupes and cabrios are the types with higher prices.


### Price vs model

#To analyze the price of a car against it´s model we need to examine a specific brand. 
#Let´s choose volkswagen, the most offered brand on ebay, and cars until 5 years old to discard the effect off the age.

autos_clean %>% filter(brand=="volkswagen" & age>10 & age<14) %>% group_by(model) %>%
  summarize(avg_price=mean(price)) %>%
  ggplot(aes(avg_price, reorder(model, avg_price))) + geom_col()

#It´s obvious that are models more expensive like others.

#If we group models per price range this will be usefull for byers that search models in a price range


### Price vs fuelType

#CNG (compressed natural gas) is cheapest fuel in the market and the 3rd more used type of fuel. There are many models generally in the small cars market that came with this fuel from fabric. Also any car with benzin or diesel can be adapted to work with cng.
#The LPG is a gas fuel like the cng but have the twice of heat power. Is used in more heavy trucks, generally to work purpouses.

#The used car market is clearly dominated by the benzin and diesel cars, only 16 electric and 116 hybrid cars are offered on eBay.
#Less than the 2% of the used car don´t have benzin or diesel engines.
autos_clean %>% group_by(fuelType) %>% summarize(count=n()) %>%
  ggplot(aes(reorder(fuelType, count), count)) +
  geom_col() + coord_flip(y=c(0,150000)) +
  geom_text(aes(label= count), hjust=-0.1, size=3)

#The market of electric and hybrid cars is new (20 years) and there are few offered.
autos_clean %>% ggplot(aes(age, price, color=fuelType)) + geom_smooth() + scale_y_log10()

#Let´s plot fuelType vs prices considering only benzin and diesel cars and suppressing the age effect.
autos_clean %>% filter(fuelType %in% c("benzin", "diesel") & age>10 & age<14) %>% 
  ggplot(aes(fuelType, price)) + geom_boxplot() + scale_y_log10()

#Diesel cars are more expensive then the benzin cars.


### Price vs gearbox

#Let´s plot the distribution of both types of gearbox (manual and automatic)
autos_clean %>% ggplot(aes(gearbox)) + geom_histogram(stat="count")

#The market of the manual used cars is 3 times bigger than the automatic cars

#Now let´s see what occur when comparing the gearbox type vs the prices
autos_clean %>% ggplot(aes(price, color=gearbox)) + geom_density() + scale_x_log10()

#The automatic cars have a higher price than the manual cars as we expected.

#Let´s look if this is true inspecting the gearbox difference in prices for a unique model like the volkswagen golf(more offered car) with a age around 12 years (to cancel the age effect).
autos_clean %>% filter(brandModel=="volkswagen_golf" & age>10 & age<14) %>%
  ggplot(aes(price, color=gearbox)) + geom_density() + scale_x_log10()

#Inspecting the unique model (golf) we can find the same distribution as we found in general,  more manual cars and the automatics more expensive


### Price vs daysOnEbay

#First we will analyze the disribution of this variable
autos_clean %>% ggplot(aes(daysOnEbay)) + geom_histogram() + scale_y_log10() + scale_x_log10()

#The 50% of the cars on ebay have been offered only 1 day and a few ones more than 10 days. The olders adds have been on ebay 200 days

#Let´s plot the price of the car vs the days offered on ebay.
autos_clean %>% ggplot(aes(daysOnEbay, price)) + geom_smooth() +geom_point(alpha=0.05) + 
  scale_y_log10() + scale_x_log10()

#We cannot see a significant relation between the price and the days offered on ebay.


### Correlation 

autos_correlation <- autos_clean %>% select(price, kilometer, powerPS, age)
corrplot.mixed(cor(autos_correlation))


## Linear regression


### Creating the train and test dataset

#With all the anatysis job we conclude that the variables that affect the price are: brandModel, age, powerPS, kilometer, price, vehicleType, vehicleClass, gearbox and fuelType
#Let´s remove all others variables
autos_pred <- autos_clean %>% select(price, powerPS, kilometer, age, brandModel, 
                                          vehicleType, vehicleClass, fuelType, gearbox)

#The train dataset will have the 80% of the data and the test dataset the other 20%.
set.seed(1)
test_index <- createDataPartition(y = autos_pred$price, times = 1, p = 0.2, list = FALSE)
autos_train <- autos_pred[-test_index[,1],]
temp <- autos_pred[test_index[,1],]

# Make sure brandModel in test set are also in train set
autos_test <- temp %>% semi_join(autos_train, by = "brandModel")

# Add rows removed from test set back into train set
removed <- anti_join(temp, autos_test)
autos_train <- rbind(autos_train, removed)

#Remove unused 
rm(test_index, temp, removed)


### Linear regression modeling

#The chosen model is the linear regression with a 5 cross fold validation.

control <- trainControl(method = "cv", number = 5, p = 0.8)
fit_lm <- train(price ~ ., method = "lm", data = autos_train,
                trControl=control)

#On the test dataset we predict the prices and store them in pred_price
autos_test$pred_price <- predict(fit_lm, autos_test)

### Result 

#The total error of the prediction is 4785$
fit_lm$results$RMSE

#This is the distribution of the error
autos_test %>% mutate(error=pred_price-price) %>%
  ggplot(aes(error)) + geom_histogram() + scale_y_log10() + scale_x_log10()

#This is the error by type of car
autos_test %>% group_by(vehicleType) %>% 
  summarize(error = sqrt(mean((pred_price-price)^2)))

#This is the error by class of car
autos_test %>% group_by(vehicleClass) %>% 
  summarize(error = sqrt(mean((pred_price-price)^2)))

#Let´s see what happen in the 5 most offered models on ebay
autos_test %>% filter(brandModel %in% most_offer_models$brandModel) %>%
  ggplot(aes(price, pred_price)) + geom_point(alpha=0.05) + geom_smooth()+
  facet_wrap(~brandModel) + coord_cartesian(xlim=c(0,20000), ylim=c(0, 20000))

#We can see that the predicted price is higher than the actual price for cars with prices lower than 10000$.
#But for cars whith prices higher than 10000$ the predicted price tend to be lower than the actual.
#This affect more to the low class and small cars because they are affordable cars with low prices.

#Now look this 5 five cars but let´s take the ones around 10 years age to reduce the age effect
autos_test %>% filter(brandModel %in% most_offer_models$brandModel & age>9 & age<12) %>% 
  group_by(brandModel) %>%
  summarize(error = sqrt(mean((pred_price-price)^2)), avg_price=mean(price), 
            avg_pred=mean(pred_price)) %>%
  mutate(perc_error=error/avg_price*100)

#The error in the opel_corsa is 67% of the actual price. In the bmw_3er the error is 25% of the price.
#In average the predicted prices in this models are higher than the average actual prices.

#As we look earlier, the age of the car have a great effect in the price of a used car. 
#Because is not the same an error of 2000$ in a 3000$ car than a 2000$ error in a 12000$ we will calculate the percentage error from the actual price.
#Let´s plot the percentage error or this 5 models against the age.
autos_test %>% filter(brandModel %in% most_offer_models$brandModel & age<50) %>%
  mutate(perc_error=(abs(pred_price-price))/price*100)  %>%
  ggplot(aes(age, perc_error, color=brandModel)) + geom_smooth()

#We can see in the graph that for cars with 15 years of age the percentage error is increasing but still admissible.
#But for cars with more than a age of 15 the error is very high, near 700% in the opel and polo.

#This happen because we can find in the dataset a lot of cars near the 200$ price with a age near 20 years. 
#The range of prices in a car with more than 20 years is great. We can find the same car in 200$ or 2000$, all depends of the condition of the car.


## Linear regression in reduced dataset(age < 20 and price < 20000)

#As we explained in the previous model with the complete dataset the prediction generated for cars with an age greater than 20 is very imprecise because the principal variable became the condition of the car and we don´t have that data.
#There are few cars in the dataset with elevated prices, and this high prices causes high errors too.

#Let´s reduce the clean dataset to the cars with ages of 20 or lower and a price lower than 20000$

red_autos_pred <- autos_clean %>%
  filter(age<=20 & price<=20000) %>% select(price, powerPS, kilometer, age, brandModel, 
                                              vehicleType, vehicleClass, fuelType, gearbox)

data.frame(autos_clean = nrow(autos_clean), red_autos_pred=nrow(red_autos_pred), 
           perc_reduced=(1-nrow(red_autos_pred)/nrow(autos_clean))*100)
#This change represent only a 12% reduction of our clean dataset, the reduced dataset count 174134 cars.

#Reducing the train and test datasets
red_autos_train <- autos_train %>% filter(age<=20 & price<=20000)
red_autos_test <- autos_test %>% filter(age<=20 & price<=20000)

### Linear regression modeling

control <- trainControl(method = "cv", number = 5, p = 0.8)
red_fit_lm <- train(price ~ ., method = "lm", data = red_autos_train,
                trControl=control)

#On the test dataset we predict the prices and store them in pred_price
red_autos_test$pred_price <- predict(red_fit_lm, red_autos_test)

### Result 

#The total error of the prediction on the reduced dataset is 2068$, 57% lower than the complete dataset (5370$)
red_fit_lm$results$RMSE

#This is the distribution of the reduced error
red_autos_test %>% mutate(error=pred_price-price) %>%
  ggplot(aes(error)) + geom_histogram()

#This is the error by type of car
red_autos_test %>% group_by(vehicleType) %>% 
  summarize(error = sqrt(mean((pred_price-price)^2)))

#This is the error by class of car
red_autos_test %>% group_by(vehicleClass) %>% 
  summarize(error = sqrt(mean((pred_price-price)^2)))

#Let´s see what happen in the 5 most offered models on ebay
red_autos_test %>% filter(brandModel %in% most_offer_models$brandModel) %>%
  ggplot(aes(price, pred_price)) + geom_point(alpha=0.05) + geom_smooth()+
  facet_wrap(~brandModel)

#We can see that the predicted price is higher than the actual price for cars with prices lower than 10000$.
#But for cars whith prices higher than 10000$ the predicted price tend to be lower than the actual.
#The errors are less disperse like the complete model thanks to the lowers errors.

#Now look the five most offered cars but let´s take the ones with a age of 10 years to eliminate the age effect
red_autos_test %>% filter(brandModel %in% most_offer_models$brandModel & age==10) %>% 
  group_by(brandModel) %>%
  summarize(error = sqrt(mean((pred_price-price)^2)), avg_price=mean(price), 
            avg_pred=mean(pred_price)) %>%
  mutate(perc_error=error/avg_price*100)

#The errors now are in a range of 22% - 52% of the actual prices. In the complete dataset the range were 25% - 67% in the same cars.

#Finally we can inspect 10 ramdom cars to look how close are the predictions

set.seed(102)
ind <- sample(c(1:nrow(red_autos_test)), 10)
red_autos_test[ind,] %>% select(brandModel, price, pred_price, age, kilometer)

#IT´S NOT TOO BAD!!!

