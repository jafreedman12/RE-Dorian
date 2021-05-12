## Spatial Clustering Analysis for Hurricane Dorian Twitter Analysis
# Code by Joseph Holler (2021) and Casey Lilley (2019)

packages = c("dplyr", "tidyr", "here", "spdep", "sf", "ggplot2")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())),
                 quietly=TRUE)

library(dplyr)
library(tidyr)
library(here)
library(spdep)
library(sf)
library(ggplot2)

######## SPATIAL JOIN TWEETS and COUNTIES ######## 
# This code was developed by Joseph Holler, 2021
# This section may not be necessary if you have already spatially joined
# and calculated normalized tweet rates in PostGIS

# load dorian and november data if not already loaded
dorian = readRDS(here("data","derived","private","dorian.RDS"))
november = readRDS(here("data","derived","private","november.RDS"))

florida_covid_sf = florida_covid %>%
  st_as_sf(coords = c("lng","lat"), crs=4326) %>%  # make point geometries
  st_transform(4269) %>%  # transform to NAD 1983
  st_join(select(counties,GEOID))  # spatially join counties to each tweet

florida_by_county = florida_covid_sf %>%
  st_drop_geometry() %>%   # drop geometry / make simple table
  group_by(GEOID) %>%      # group by county using GEOID
  summarise(florida_tweetcount_county = n(), # count # of tweets
            tweet_value = mean(value_int.y))  

counties = counties %>%
  left_join(florida_by_county, by="GEOID") %>% # join count of tweets to counties
  mutate(florida_tweetcount_county = replace_na(florida_tweetcount_county,0))       # replace nulls with 0's

rm(florida_covid)

# Repeat the workflow above for tweets in November
florida_baseline_by_county = florida_baseline %>% 
  st_as_sf(coords = c("lng","lat"), crs=4326) %>%
  st_transform(4269) %>%
  st_join(select(counties,GEOID)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>% 
  summarise(baseline = n())

counties = counties %>%
  left_join(florida_baseline_by_county, by="GEOID") %>%
  mutate(baseline = replace_na(baseline,0))

counties = counties %>%
  mutate(flocovrate = florida_tweetcount_county / POP * 10000) %>%  # dorrate is tweets per 10,000
  mutate(ntdi = (florida_tweetcount_county - baseline) / (florida_tweetcount_county + baseline)) %>%  # normalized tweet diff
  mutate(ntdi = replace_na(ntdi,0))   # replace NULLs with 0's

rm(nov_by_county)

# save counties geographic data with derived tweet rates
saveRDS(counties,here("data","derived","public","counties_tweet_counts.RDS"))

# optionally, reload counties
counties = readRDS(here("data","derived","public","counties_tweet_counts.RDS"))


### Plot findings
ggplot() +
  geom_sf(data=counties, aes(fill=factor(tweet_value)), color="grey")+
  scale_fill_brewer(palette="BrBG")+
  guides(fill=guide_legend(title="Population Density"))


ggplot() +
  geom_sf(data=counties, aes(fill=cut_number(DENSITY,5)), color="grey")+
  scale_fill_brewer(palette="GnBu")+
  guides(fill=guide_legend(title="Population Density")) #+
  geom_point(data = florida_covid, 
             aes(x=lng,y=lat),
             color = value_int.y, 
             alpha = .2) +
  labs(title = "Tweet Locations in Florida during COVID")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

######## SPATIAL CLUSTER ANALYSIS ######## 
# This code was originally developed by Casey Lilley (2019)
# and edited by Joseph Holler (2021)
# See https://caseylilley.github.io/finalproj.html

thresdist = counties %>% 
  st_centroid() %>%     # convert polygons to centroid points
  st_coordinates() %>%  # convert to simple x,y coordinates to play with stdep
  dnearneigh(0, 110, longlat = TRUE) %>%  # use geodesic distance of 110km
	# distance should be long enough for every feature to have >= one neighbor
  include.self()       # include a county in its own neighborhood (for G*)

# three optional steps to view results of nearest neighbors analysis
thresdist # view statistical summary of the nearest neighbors 
plot(counties, border = 'lightgrey')  # plot counties background
plot(selfdist, coords, add=TRUE, col = 'red') # plot nearest neighbor ties

#Create weight matrix from the neighbor objects
dwm = nb2listw(thresdist, zero.policy = T)

######## Local G* Hotspot Analysis ######## 
#Get Ord G* statistic for hot and cold spots
counties$locG = as.vector(localG(counties$dorrate, listw = dwm, 
                                 zero.policy = TRUE))

# optional step to check summary statistics of the local G score
summary(counties$locG)

# classify G scores by significance values typical of Z-scores
# where 1.15 is at the 0.125 confidence level,
# and 1.95 is at the 0.05 confidence level for two tailed z-scores
# based on Getis and Ord (1995) Doi: 10.1111/j.1538-4632.1992.tb00261.x
# to find other critical values, use the qnorm() function as shown here:
# https://methodenlehre.github.io/SGSCLM-R-course/statistical-distributions.html
# Getis Ord also suggest applying a Bonferroni correction 

siglevel = c(1.15,1.95)
counties = counties %>% 
  mutate(sig = cut(locG, c(min(counties$locG),
                           siglevel[2]*-1,
                           siglevel[1]*-1,
                           siglevel[1],
                           siglevel[2],
                           max(counties$locG))))
rm(siglevel)

# Map hot spots and cold spots!
# breaks and colors from http://michaelminn.net/tutorials/r-point-analysis/
# based on 1.96 as the 95% confidence interval for z-scores
# if your results don't have values in each of the 5 categories, you may need
# to change the values & labels accordingly.
ggplot() +
  geom_sf(data=counties, aes(fill=sig), color="white", lwd=0.1)+
  scale_fill_manual(
    values = c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080"),
    labels = c("low","", "insignificant","","high"),
    aesthetics = "fill"
  ) +
  guides(fill=guide_legend(title="Hot Spots"))+
  labs(title = "Clusters of Hurricane Dorian Twitter Activity")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
