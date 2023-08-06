#Step 1, Writing Mayors Action Center Tickets to new Data Frame
library(readr)
library(readxl)
MACDF <- read_csv("C:/Users/mresner/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACTicket.csv")
View(MACDF)

#STEP 2, Adding an Indexing variable to iterate over as well as the state
MACDF$index <- 1:nrow(MACDF)
MACDF$State <- 'IN'

# #STEP 3, Install required packages for Connecting to US Census GEOCODER API
# install.packages(c("httr","jsonlite"))
# library(httr)
# library(jsonlite)
# 
# #Step 4, Connect to US Census GEOCODER API
# 
# #https://geocoding.geo.census.gov/geocoder/returntype/searchtype?parameters
# #https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=4600+Silver+Hill+Rd%2C+Washington%2C+DC+20233&benchmark=2020&format=json
# base <- 'https://geocoding.geo.census.gov/geocoder/'
# returntype <- '/locations'
# benchmark 'Public_AR_Census2020'
# 
# 
# for(i in 1:length(MACDF$index)) {
#   
#   # Build the API URL with the new county code
#   API_URL <- paste0(base, counties[i], info_key)
#   
#   # Store the raw and processed API results in temporary objects
#   temp_raw <- GET(API_URL)
#   temp_list <- fromJSON(rawToChar(temp_raw$content), flatten = TRUE)
#   
#   # Add the most recent results to your data frame
#   df <- rbind(df, temp_list$actualsTimeseries)

#TidyCensus
install.packages("tibble")
install.packages("dplyr")
install.packages("tidygeocoder")
install.packages("tidycensus")
install.packages("tidyverse")

library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)

TestDF <- MACDF %>%
  geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
TestDF = data.frame()
TestDF <- MACDF[1:1000,]
APITEST=data.frame()
APITEST <- TestDF %>%
  geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
View(APITEST)

#How to write this to not exceed the batch limit?
#Need to understand the package dplyr
#How to repeat iteration with subsets for the remainder of the data?



#Splitting the MACDF into many data frames

chunk <- 10000
n <- nrow(MACDF)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(MACDF,r)
nrow(d$`1`)
View(d$`1`)

for (i in 1:nrow(d)) {

ChunkTest <- d %>%
  geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
View(ChunkTest)

output <- vector("double",ncol(df)) # 1. Output
for (i in seq_along(df)) {          # 2. OuterSequence
  output[[i]] <- median(df[[i]])    # 3. Body
}
output

#Step 1, Writing Mayors Action Center Tickets to new Data Frame
library(readr)
library(readxl)
MACDF <- read_csv("C:/Users/mresner.Keteres/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/1. MACTicketOriginal.csv")
View(MACDF)

#STEP 2, Adding an Indexing variable to iterate over as well as the state
MACDF$index <- 1:nrow(MACDF)
MACDF$State <- 'IN'

# #STEP 3, Install required packages for Connecting to US Census GEOCODER API
# install.packages(c("httr","jsonlite"))
# library(httr)
# library(jsonlite)
# 
# #Step 4, Connect to US Census GEOCODER API
# 
# #https://geocoding.geo.census.gov/geocoder/returntype/searchtype?parameters
# #https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=4600+Silver+Hill+Rd%2C+Washington%2C+DC+20233&benchmark=2020&format=json
# base <- 'https://geocoding.geo.census.gov/geocoder/'
# returntype <- '/locations'
# benchmark 'Public_AR_Census2020'
# 
# 
# for(i in 1:length(MACDF$index)) {
#   
#   # Build the API URL with the new county code
#   API_URL <- paste0(base, counties[i], info_key)
#   
#   # Store the raw and processed API results in temporary objects
#   temp_raw <- GET(API_URL)
#   temp_list <- fromJSON(rawToChar(temp_raw$content), flatten = TRUE)
#   
#   # Add the most recent results to your data frame
#   df <- rbind(df, temp_list$actualsTimeseries)

#TidyCensus
install.packages("tibble")
install.packages("dplyr")
install.packages("tidygeocoder")
install.packages("tidycensus")
install.packages("tidyverse")

library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)

TestDF <- MACDF %>%
  geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
TestDF = data.frame()
TestDF <- MACDF[1:1000,]
APITEST=data.frame()
APITEST <- TestDF %>%
  geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
View(APITEST)

#How to write this to not exceed the batch limit?
#Need to understand the package dplyr
#How to repeat iteration with subsets for the remainder of the data?



#Splitting the MACDF into many data frames

chunk <- 10000
n <- nrow(MACDF)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(MACDF,r)
nrow(d$`1`)
View(d$`1`)
#nrow(d))
for (i in 1:2 {
  
  ChunkTest <- d %>%
    geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
  View(ChunkTest)
  
  output <- vector("double",ncol(df)) # 1. Output
  for (i in seq_along(df)) {          # 2. OuterSequence
    output[[i]] <- median(df[[i]])    # 3. Body
  }
  output
  
  
  DFTEST=data.frame()
  for (j in d[[1]]$index){
    # Function
    output = d[[1]]$index[j]
    DFTEST=rbind(DFTEST,output)
  }
  
  View(DFTEST)
  
  #Testing my list 
  
  View(d[[1]]$index[1])
  # 
  length(d)
  #i <- 1
  nrow(d[[1]]$index)
  
  #Testing my list 
  
  #View(d[[1]]$index[1])
  # 
  # length(d)
  # #i <- 1
  # nrow(d[[1]]$index)
  
  View(DFTEST)
  
  
  #Inner Loop Test
  DFTEST=data.frame()
  for (j in d[[1]]$index){
    # Function
    output = d[[1]]$index[j]
    DFTEST=rbind(DFTEST,output)
  }
  
  View(DFTEST)
  
  #Outer Loop Test
  DFTEST=data.frame()
  
  #Outer Loop
  for (i in length(d)){
    # inner loop
    for (j in d[[1]]$index){
      # Function
      output = d[[1]]$index[j]
      DFTEST=rbind(DFTEST,output)
    }
  }
  View(DFTEST)
  
  #Testing API Call on first dataframe in d list d[[1]]
  DFTESTLIST =data.frame()
  DFTESTLIST <- d[[1]] %>%
    geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
  
  View(DFTESTLIST)
  #functions perfectly
  
  
  MarionPop <- get_decennial(
    geography = "county",
    variables = "H8_001N",
    state = "IN",
    county = "Marion",
    output = "wide",
    year = "2020",
    sumfile = "dhc"
    show_call = TRUE
  )
  
  #Calling Population on the Tract Level
  #tract = "TTRACT",
  
  censusVars <- c(White = "P2_005N", 
                  Black = "P2_006N", 
                  Asian = "P2_008N", 
                  Hispanic = "P2_002N")
  
  
  censusVars <- c(population = "P1_001N",
                  Hispanic = "P2_002N")
  #True Variables H8_001N is not working for some reason
  censusVars <- c(population = "P1_001N",
                  occHousingPop = "H8_001N")
  
  
  MarionCo <- get_decennial(
    geography = "TRACT",
    variables = censusVars,
    state = "IN",
    county = "Marion",
    output = "wide",
    year = "2020")
                 