# Analysis of Indianapolis Mayors Action Center Ticket Counts on US Census Demographic Characteristics by Census Tract

Welcome to my analysis of Indianapolis Mayor's Action Center service requests. The Mayor's Action Center (and the Request Indy App) are avenues of submitting citizen service requests and complaints to the City of Indianapolis. Requests can represent resident issues such as malfunctioning street lights, loose dogs, or tall grass. 

For more information surrounding the Mayor's Action Center visit their website at https://www.indy.gov/agency/mayors-action-center

The initial Service Case dataset can be downloaded at https://data.indy.gov/datasets/mayors-action-center-service-cases/explore

Model inspiration taken from https://scholar.harvard.edu/files/jfeigenbaum/files/feigenbaum_hall_respublica.pdf. 

In this project I take the initial data on Mayor's Action Center Service Requests and transform the data by:

	1. Loading, batching and geocoding the data with  tidygeocoder (Complete)
 
	2. Generating categorical variables based on request category with plyr (Complete)
 
	3. Selecting/renaming/grouping the data with sqldf (Complete)
 
	4. Joining demographic data by census tract and block with tidycensus (Complete)
 
  5. Analyzing the data using inference (Complete)
   

You can explore the commented code in the file named "Mayors Action Center Project Streamlined.R" which can be found in the sidebar. 

If you have any feedback or recommendations for how to analyize this data please reach out to me on LinkedIn at https://www.linkedin.com/in/matthewresner/.
