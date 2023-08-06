#Step 1: Creating Data frame and Matching Addresses to get the Census Tract

    # Reading in the CSV to a new Data Frame
    library(readr)
    library(readxl)
    MACDF <- read_csv("C:/Users/mresner.Keteres/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/1. MACTicketOriginal.csv")
    View(MACDF)

    # Adding an Indexing variable to iterate over as well as the state for geocoding
    MACDF$index <- 1:nrow(MACDF)
    MACDF$State <- 'IN'

    #Splitting the MACDF into many data frames within a list so we can iterate through them using the tidy geocoder
    chunk <- 10000
    n <- nrow(MACDF)
    r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
    d <- split(MACDF,r)
    #Checking to see if the chunking operation was successful to be below the US Census API row limit of 10000
    nrow(d$`1`)
    View(d$`1`)

    
    # How to use Tidy Census to call Census API Reference
    #https://jessecambon.github.io/tidygeocoder/

    # Load Tidy Census and Geocoder Libraries
    # you will need to get an access key from the US Census to call the API. They are free
    
      library(tidygeocoder)
      library(tidycensus)
      library(tidyverse)

   
    #Testing Tidy Census Call on first dataframe in d list d[[1]]
        DFTESTLIST =data.frame()
        DFTESTLIST <- d[[1]] %>%
          geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
        
        View(DFTESTLIST)
        #functions perfectly for 9,087 unique addresses in the dataframe of 10000 tickets. 
  
    # Writing Tidy Geocoder For-Loop for all Addresses in all subset data frames in the list "d"
        # Beware this process will take a few hours to iterate.
        # Make sure your computer is in a stable place with power before beginning
          MACDFMatch=data.frame()
        View(d) 
        i =0
        for (i in 1:length(d)){
            # inner loop
              output = d[[i]] %>%
                geocode(street = INCIDENT_ADDRESS__C, city = CITY__C, state = State, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
              MACDFMatch=rbind(MACDFMatch,output)
            
          }
          #length(d)
          View(MACDFMatch)
    
        # Writing a CSV to save the matched results for later analysis   
        write.csv(MACDFMatch, "C:/Users/mresner/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACGeocoded.csv", row.names=FALSE)

# Step 2. Analyizing, Cleaning, and Grouping the Data by Census Tract, Block Group, and Address Alternatively
    
        
    #Reading in the saved CSV data to resume after a pause that removed the MACDFMatch data frame from memory
        MACDFMatched <- read_csv("C:/Users/mresner/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACGeocoded.csv")
    
    # Exploring and Removing Anomalous data using tidyr and dplyr
      
        view(MACDFMatched)
        #install.packages("plyr") 
        library("plyr")
        #install.packages("dplyr")                         
        library("dplyr")     
    
        #Subsetting All Non Exact Matches and Non Matched Data
        MACDFCLEAN <- MACDFMatched[MACDFMatched$match_type == "Exact",]
        #Removing Rows with NA in every row. 
        MACDFCLEAN <- MACDFCLEAN %>% drop_na(OBJECTID)
        # Removing the city county building from the records as the types of violations occurring there indicate potential data entry errors 
        MACDFCLEAN <- filter(MACDFCLEAN, matched_address != "200 E WASHINGTON ST, INDIANAPOLIS, IN, 46204")

    # Selecting and Renaming the variables desired to lower camel case
      MACDFCategoryBool <- MACDFCLEAN%>% select(OBJECTID,CASENUMBER,SOURCE_ID__C,KEYWORD__C,SUBCATEGORY__C,INCIDENT_ADDRESS__C,TOWNSHIP__C,CITY__C,ZIP__C,COUNCIL_DISTRICT__C,CREATEDDATE,CLOSEDDATE,STATUS,ORIGIN,census_tract,census_block) 
      MACDFCategoryBool <- plyr::rename(MACDFCategoryBool, c("OBJECTID" = "objectId","CASENUMBER" = "caseNumber", "SOURCE_ID__C" = "sourceId", "KEYWORD__C" = "category" , "SUBCATEGORY__C" = "subcategory", "INCIDENT_ADDRESS__C"= "incidentAddress","TOWNSHIP__C" = "township","CITY__C" = "city","ZIP__C" ="zip","COUNCIL_DISTRICT__C" = "councilDistrict","CREATEDDATE" ="createdDate","CLOSEDDATE"="closedDate","STATUS" = "status","ORIGIN" = "origin","census_tract" = "censusTract","census_block"="censusBlock"))
    
    # Generating Dummy Variables for each category 
      MACDFCategoryBool$category2 <- MACDFCategoryBool$category
      MACDFCategoryBool <- MACDFGroupedAddress %>% mutate(dummy=1) %>%
        spread(key=category2,value=dummy, fill=0)

    #Writing a CSV to save progress on MACDFCategoryBool
      write.csv(MACDFCategoryBool, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/3.MACDFCategoryBool", row.names=FALSE)
    
    #Reading the CSV back in  
      library(readr)
      MACDFCategoryBool <- read_csv("C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/3.MACDFCategoryBool.csv")
    
      #adding in Date Extracts for Year and Month from Create Date
      
      #Testing Substring Methods
      
      date <-  '2022-01-31'
      Year <- substring(date,1,4)
      month <- substring(date,6,7)
      
      #writing year and month columns to MACDFCategoryBool using substring
   
      MACDFCategoryBool$createYear <-substring(MACDFCategoryBool$createdDate,1,4)
      MACDFCategoryBool$createMonth <-substring(MACDFCategoryBool$createdDate,6,7)
      
      # Saving the new columns in CSV
      write.csv(MACDFCategoryBool, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/3.MACDFCategoryBool.csv", row.names=FALSE)
    
      # Grouping by address using SQLDF due to familiarity with SQL syntax
      install.packages("sqldf")  
      library("sqldf")
     
       
      
      #choosing to merge traffic signs and signal categories with each other as well as illegal dumping and trash together
      MACDFGroupedAddress <- sqldf("select incidentAddress ,township ,city ,zip ,councilDistrict ,censusTract ,censusBlock 
                                  ,COUNT(objectid) AS totalTickets
                                  ,SUM([Abandoned Vehicle]) AS abandonedVehicle ,SUM(Alley) As alley ,SUM(Animal) AS animal ,SUM([Basketball Goal]) AS basketballGoal ,SUM([Berms/Shoulder]) AS bermsShoulder ,SUM(Bridge) AS bridge ,SUM([CEG Referral]) AS CEGReferral
                                  ,SUM(Chuckhole) AS chuckhole ,SUM(Contractor) AS contractor ,SUM([Debris/Litter]) AS debrisLitter ,SUM(Disability) AS disability ,SUM([Emergency Management]) AS emergencyManagement ,SUM([Environmental Concern]) AS environmentalConcern 
                                  ,SUM(Forestry) AS forestry ,SUM(Graffiti) AS graffiti ,SUM(Guardrail) AS guardrail ,SUM([Illegal Dumping]+[Illegal Dumping and Junk/Trash]) AS illegalDumping ,SUM([Infrastructure Violation]) AS infrastructureViolation
                                  ,SUM(Manhole) AS manhole ,SUM([New Construction]+[Ongoing Construction]) AS construction,SUM(Odor) AS odor,SUM([Operation Night Light]) AS operationNightLight ,SUM([Outside Entity]) AS outsideEntity ,SUM(Parks) AS parks ,SUM([Sanitary Sewer]) AS sanitarySewer
                                  ,SUM([Sidewalks/Curbs]) AS sidewalksCurbs ,SUM([Signs/Traffic Signs]+[Signs/Traffic+Signs]+[Traffic Signals]) AS trafficSignsSignals ,SUM([Snow/Ice]) AS snowIce ,SUM([Spray Ground]) AS sprayGround ,SUM(Streets) AS streets
                                   FROM MACDFCategoryBool GROUP BY incidentAddress, township, city, zip, councilDistrict, censusTract, censusBlock")
      
      #Writing a CSV to save MACDFGroupedAddress
      write.csv(MACDFGroupedAddress, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACDFGroupedAddress.csv", row.names=FALSE)
      
    # Grouping by Census Tract Block Group
      
      MACDFGroupedBlock <- sqldf("select township ,city ,zip ,councilDistrict ,censusTract ,censusBlock 
                                  ,COUNT(objectid) AS totalTickets
                                  ,SUM([Abandoned Vehicle]) AS abandonedVehicle ,SUM(Alley) As alley ,SUM(Animal) AS animal ,SUM([Basketball Goal]) AS basketballGoal ,SUM([Berms/Shoulder]) AS bermsShoulder ,SUM(Bridge) AS bridge ,SUM([CEG Referral]) AS CEGReferral
                                  ,SUM(Chuckhole) AS chuckhole ,SUM(Contractor) AS contractor ,SUM([Debris/Litter]) AS debrisLitter ,SUM(Disability) AS disability ,SUM([Emergency Management]) AS emergencyManagement ,SUM([Environmental Concern]) AS environmentalConcern 
                                  ,SUM(Forestry) AS forestry ,SUM(Graffiti) AS graffiti ,SUM(Guardrail) AS guardrail ,SUM([Illegal Dumping]+[Illegal Dumping and Junk/Trash]) AS illegalDumping ,SUM([Infrastructure Violation]) AS infrastructureViolation
                                  ,SUM(Manhole) AS manhole ,SUM([New Construction]+[Ongoing Construction]) AS construction,SUM(Odor) AS odor,SUM([Operation Night Light]) AS operationNightLight ,SUM([Outside Entity]) AS outsideEntity ,SUM(Parks) AS parks ,SUM([Sanitary Sewer]) AS sanitarySewer
                                  ,SUM([Sidewalks/Curbs]) AS sidewalksCurbs ,SUM([Signs/Traffic Signs]+[Signs/Traffic+Signs]+[Traffic Signals]) AS trafficSignsSignals ,SUM([Snow/Ice]) AS snowIce ,SUM([Spray Ground]) AS sprayGround ,SUM(Streets) AS streets
                                   FROM MACDFCategoryBool GROUP BY township, city, zip, councilDistrict, censusTract, censusBlock")
      
      #Writing a CSV to save MACDFGroupedBlock
      write.csv(MACDFGroupedBlock, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACDFGroupedBlock.csv", row.names=FALSE)
      
      
      MACDFGroupedTract <- sqldf("select township ,city ,zip ,councilDistrict ,censusTract 
                                  ,COUNT(objectid) AS totalTickets
                                  ,SUM([Abandoned Vehicle]) AS abandonedVehicle ,SUM(Alley) As alley ,SUM(Animal) AS animal ,SUM([Basketball Goal]) AS basketballGoal ,SUM([Berms/Shoulder]) AS bermsShoulder ,SUM(Bridge) AS bridge ,SUM([CEG Referral]) AS CEGReferral
                                  ,SUM(Chuckhole) AS chuckhole ,SUM(Contractor) AS contractor ,SUM([Debris/Litter]) AS debrisLitter ,SUM(Disability) AS disability ,SUM([Emergency Management]) AS emergencyManagement ,SUM([Environmental Concern]) AS environmentalConcern 
                                  ,SUM(Forestry) AS forestry ,SUM(Graffiti) AS graffiti ,SUM(Guardrail) AS guardrail ,SUM([Illegal Dumping]+[Illegal Dumping and Junk/Trash]) AS illegalDumping ,SUM([Infrastructure Violation]) AS infrastructureViolation
                                  ,SUM(Manhole) AS manhole ,SUM([New Construction]+[Ongoing Construction]) AS construction,SUM(Odor) AS odor,SUM([Operation Night Light]) AS operationNightLight ,SUM([Outside Entity]) AS outsideEntity ,SUM(Parks) AS parks ,SUM([Sanitary Sewer]) AS sanitarySewer
                                  ,SUM([Sidewalks/Curbs]) AS sidewalksCurbs ,SUM([Signs/Traffic Signs]+[Signs/Traffic+Signs]+[Traffic Signals]) AS trafficSignsSignals ,SUM([Snow/Ice]) AS snowIce ,SUM([Spray Ground]) AS sprayGround ,SUM(Streets) AS streets
                                   FROM MACDFCategoryBool GROUP BY township, city, zip, councilDistrict, censusTract")
      
      MACDFGroupedTractYearMonth <- sqldf("select township ,city ,zip ,councilDistrict ,censusTract, createYear, createMonth
                                  ,COUNT(objectid) AS totalTickets
                                  ,SUM([Abandoned Vehicle]) AS abandonedVehicle ,SUM(Alley) As alley ,SUM(Animal) AS animal ,SUM([Basketball Goal]) AS basketballGoal ,SUM([Berms/Shoulder]) AS bermsShoulder ,SUM(Bridge) AS bridge ,SUM([CEG Referral]) AS CEGReferral
                                  ,SUM(Chuckhole) AS chuckhole ,SUM(Contractor) AS contractor ,SUM([Debris/Litter]) AS debrisLitter ,SUM(Disability) AS disability ,SUM([Emergency Management]) AS emergencyManagement ,SUM([Environmental Concern]) AS environmentalConcern 
                                  ,SUM(Forestry) AS forestry ,SUM(Graffiti) AS graffiti ,SUM(Guardrail) AS guardrail ,SUM([Illegal Dumping]+[Illegal Dumping and Junk/Trash]) AS illegalDumping ,SUM([Infrastructure Violation]) AS infrastructureViolation
                                  ,SUM(Manhole) AS manhole ,SUM([New Construction]+[Ongoing Construction]) AS construction,SUM(Odor) AS odor,SUM([Operation Night Light]) AS operationNightLight ,SUM([Outside Entity]) AS outsideEntity ,SUM(Parks) AS parks ,SUM([Sanitary Sewer]) AS sanitarySewer
                                  ,SUM([Sidewalks/Curbs]) AS sidewalksCurbs ,SUM([Signs/Traffic Signs]+[Signs/Traffic+Signs]+[Traffic Signals]) AS trafficSignsSignals ,SUM([Snow/Ice]) AS snowIce ,SUM([Spray Ground]) AS sprayGround ,SUM(Streets) AS streets
                                   FROM MACDFCategoryBool GROUP BY township, city, zip, councilDistrict, censusTract, createYear, createMonth")
      
      MACDFGroupedTractYear <- sqldf("select township ,city ,zip ,councilDistrict ,censusTract, createYear
                                  ,COUNT(objectid) AS totalTickets
                                  ,SUM([Abandoned Vehicle]) AS abandonedVehicle ,SUM(Alley) As alley ,SUM(Animal) AS animal ,SUM([Basketball Goal]) AS basketballGoal ,SUM([Berms/Shoulder]) AS bermsShoulder ,SUM(Bridge) AS bridge ,SUM([CEG Referral]) AS CEGReferral
                                  ,SUM(Chuckhole) AS chuckhole ,SUM(Contractor) AS contractor ,SUM([Debris/Litter]) AS debrisLitter ,SUM(Disability) AS disability ,SUM([Emergency Management]) AS emergencyManagement ,SUM([Environmental Concern]) AS environmentalConcern 
                                  ,SUM(Forestry) AS forestry ,SUM(Graffiti) AS graffiti ,SUM(Guardrail) AS guardrail ,SUM([Illegal Dumping]+[Illegal Dumping and Junk/Trash]) AS illegalDumping ,SUM([Infrastructure Violation]) AS infrastructureViolation
                                  ,SUM(Manhole) AS manhole ,SUM([New Construction]+[Ongoing Construction]) AS construction,SUM(Odor) AS odor,SUM([Operation Night Light]) AS operationNightLight ,SUM([Outside Entity]) AS outsideEntity ,SUM(Parks) AS parks ,SUM([Sanitary Sewer]) AS sanitarySewer
                                  ,SUM([Sidewalks/Curbs]) AS sidewalksCurbs ,SUM([Signs/Traffic Signs]+[Signs/Traffic+Signs]+[Traffic Signals]) AS trafficSignsSignals ,SUM([Snow/Ice]) AS snowIce ,SUM([Spray Ground]) AS sprayGround ,SUM(Streets) AS streets
                                   FROM MACDFCategoryBool GROUP BY township, city, zip, councilDistrict, censusTract, createYear")
      
      
      #Writing a CSV to save MACDFGroupedTract
      write.csv(MACDFGroupedTract, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACDFGroupedTract.csv", row.names=FALSE)
      
      #Writing a CSV to save MACDFGroupedTractYearMonth
      write.csv(MACDFGroupedTractYearMonth, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/4.MACDFGroupedTractYearMonth.csv", row.names=FALSE)
      
      #Writing a CSV to save MACDFGroupedTractYear
      write.csv(MACDFGroupedTractYear, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/4.MACDFGroupedTractYear.csv", row.names=FALSE)
      
#Step 3. Calling Census API to Connect Census Tracts with Demographic Information

      #Loading the Excel File
      library(readr)
      library(readxl)
      MACDFGroupedTractYearMonth <- read_csv("C:/Users/mresner.Keteres/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/4.MACDFGroupedTractYearMonth.CSV")
      View(MACDFGroupedTractYearMonth)
      
      # Tidy Census Use
      install.packages("tidycensus")  
      library(tidycensus)
      library(dplyr)
      install.packages("dplyr")
      library(tidyverse)
      census_api_key("81ec59eceb4f13d873b70e45a9a8e5a1a88a984b", install = TRUE, overwrite = TRUE)
      
      #Exploring census Variables https://api.census.gov/data/2020/dec/dhc/variables.html
      DHCVAR <- load_variables(2020,"dhc" )
      
      #Exploring ACS Variables
      
      ACSVAR <- load_variables(2021,"acs5" )
      
      #We are going to Connect to ACS estimates for median income, GINI COEF, population & population density
      # Total Population B01003_001
      # GINI B19083
      # Median Income in the last 12 months B06011_001
      
      my_vars <- c(
        totalPop = "B01003_001",
        medianIncome = "B19013_001",
        gini = "B19083_001"
      )
      
      YearsinData <- sqldf("select DISTINCT createYear as Year FROM MACDFGroupedTractYear")
      
      Years <- lst("2015", "2016", "2017", "2018", "2019", "2021", "2022")
      
      MyData <- get_acs(
        geography = "TRACT",
        variables =  my_vars,
        state = "IN",
        county = "Marion",
        output = "wide",
        year = Years,
        survey = "acs5"
      )
      
      AcsData <- map_dfr(
        years,
        ~ get_acs(
          geography = "TRACT",
          variables =  my_vars,
          state = "IN",
          county = "Marion",
          output = "wide",
          year = .x,
          survey = "acs5",
          geometry = FALSE
        ),
       .id = "Year"
      )
        
      
      
      
       
    







