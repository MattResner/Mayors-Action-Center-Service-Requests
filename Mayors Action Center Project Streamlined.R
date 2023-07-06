


#Step 1: Creating Dataframe and Matching Addresses to get the Census Tract

    # Geocoder Reference
    #https://jessecambon.github.io/tidygeocoder/

    # Census Geocoder Libraries
    
      library(tidygeocoder)
      library(tidycensus)
      library(tidyverse)

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
  
    #Writing API For-Loop for all Addresses in all subset dataframes in the list "d"
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
    # Renaming variables with Spaces
      MACDFCategoryBool <- plyr::rename(MACDFCategoryBool, c())
      
    #Writing a CSV to save MACDFCategoryBool
      write.csv(MACDFCategoryBool, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACDFCategoryBool.csv", row.names=FALSE)
       
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
      
      #Writing a CSV to save MACDFGroupedTract
      write.csv(MACDFGroupedTract, "C:/Users/mresner.KETERES/OneDrive - Keter Environmental Services/Personal/Data Science Projects/Mayors Action Center Ticket Analysis/MACDFGroupedTract.csv", row.names=FALSE)
      
      

#Step 3. Calling Census API to Connect Census Tracts with Demographic Information
        
  


# Tidy Census Use









