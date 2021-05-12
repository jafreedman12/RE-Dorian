#search geographic twitter data for Hurricane Dorian, by Joseph Holler, 2019,2020
#to search, you first need a twitter API token: https://rtweet.info/articles/auth.html 

#this script contains the code that was used to create the dorian and november data frames in dorian.Rdata. 
#note that this code will no longer work as originally run, because the code is time-sensitive.
#for students in GG323, read the code for how the data was searched, and then run the final block
#of code to download and load the resutls in Rdata format.

#install package for twitter and initialize the library
packages = c("rtweet","here")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())), quietly=TRUE)

library(rtweet)
library(here)
library(tidyverse)

############# SEARCH TWITTER API ############# 

#reference for search_tweets function: https://rtweet.info/reference/search_tweets.html 
#don't add any spaces in between variable name and value. i.e. n=1000 is better than n = 1000
#the first parameter in quotes is the search string, searching tweet contents and hashtags
#n=10000 asks for 10,000 tweets
#if you want more than 18,000 tweets, change retryonratelimit to TRUE and wait 15 minutes for every batch of 18,000
#include_rts=FALSE excludes retweets.
#token refers to the twitter token you defined above for access to your twitter developer account
#geocode is equal to a string with three parts: longitude, latidude, and distance with the units mi for miles or km for kilometers



#set up twitter API information
#this should launch a web browser and ask you to log in to twitter
#replace app, consumer_key, and consumer_secret data with your own developer acct info
twitter_token <- create_token(
  app = "SpatialTwitterAnalysis_jfreed",  					#replace yourapp with your app name
  consumer_key = api_key,  		#replace yourkey with your consumer key
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret_key)


#get tweets about covid mask mandate in Florida, searched on May 4th, 2021
florida_covid <- search_tweets("covid OR restriction OR mask",
                        n=200000, include_rts=FALSE, 
                        token=twitter_token, 
                        geocode="28.56,-82.63,500mi", 
                        retryonratelimit=TRUE)


#get tweets without any text filter for the same geographic region in November, searched on November 19, 2019
#the query searches for all verified or unverified tweets, so essentially everything
florida_baseline <- search_tweets("-filter:verified OR filter:verified", 
                                  n=200000, include_rts=FALSE, 
                                  token=twitter_token, geocode="28.56,-82.63,500mi", 
                                  retryonratelimit=TRUE)


# Save files
save(florida_covid, file = here("data", "derived", "private", "florida_covid.rData"))
save(florida_baseline, file = here("data", "derived", "private", "florida_baseline.rData"))


############# LOAD THESE RESULTS - GEOG323 STUDENTS ONLY ############# 

# Please download the file from here: https://github.com/GIS4DEV/literature/raw/master/dorian/dorian.RData
# into the data\derived\private folder
# then run the following line of code to load the data into your environment

load(here("data","derived","private","florida_covid.RData"))
load(here("data","derived","private","florida_baseline.RData"))

# In the following code, you can practice running the queries on dorian3

############# FIND ONLY PRECISE GEOGRAPHIES ############# 

#reference for lat_lng function: https://rtweet.info/reference/lat_lng.html
#adds a lat and long field to the data frame, picked out of the fields you indicate in the c() list
#sample function: lat_lng(x, coords = c("coords_coords", "bbox_coords"))

# list unique/distinct place types to check if you got them all
unique(florida_covid$place_type)

# list and count unique place types
# NA results included based on profile locations, not geotagging / geocoding. If you have these, it indicates that you exhausted the more precise tweets in your search parameters
count(florida_covid, place_type)

#convert GPS coordinates into lat and lng columns
#do not use geo_coords! Lat/Lng will come out inverted
florida_covid <- lat_lng(florida_covid,coords=c("coords_coords"))
florida_baseline <- lat_lng(florida_baseline,coords=c("coords_coords"))

#select any tweets with lat and lng columns (from GPS) or designated place types of your choosing
florida_covid <- subset(florida_covid, place_type == 'city'| place_type == 'neighborhood'| place_type == 'poi' | !is.na(lat))
florida_baseline <- subset(florida_baseline, place_type == 'city'| place_type == 'neighborhood'| place_type == 'poi' | !is.na(lat))

#convert bounding boxes into centroids for lat and lng columns
florida_covid <- lat_lng(florida_covid,coords=c("bbox_coords"))
florida_baseline <- lat_lng(florida_baseline,coords=c("bbox_coords"))

# Save filtered data
save(florida_covid, file = here("data","derived","private","florida_covid_cleaned.RData"))
save(florida_baseline, file = here("data","derived","private","florida_baseline_cleaned.RData"))


############# SAVE FILTERED TWEET IDS TO DATA/DERIVED/PUBLIC ############# 

write.table(florida_covid$status_id,
            here("data","derived","public","florida_covid_cleaned.txt"), 
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

write.table(florida_baseline$status_id,
            here("data","derived","public","florida_baseline_cleaned.txt"), 
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

############# SAVE TWEETs TO DATA/DERIVED/PRIVATE ############# 

saveRDS(florida_covid, here("data","derived","private","florida_covid_cleaned.RDS"))
saveRDS(florida_baseline, here("data","derived","private","florida_baseline_cleaned.RDS"))


