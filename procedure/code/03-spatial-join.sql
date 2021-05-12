/* This SQL script serves as a guide to spatially joining tweets to counties
 - Joseph Holler, 2021 */

/*
############### SPATIAL JOIN AND MAPPING NORMALIZED TWEETS ###############

# Either in R or in PostGIS (via QGIS DB Manager)...

# Count the number of dorian points in each county
# Count the number of november points in each county
# Set counties with no points to 0 for the november count
# Calculate the normalized difference tweet index (made this up, based on NDVI),
# where ndti = (tweets about storm – baseline twitter activity) /
#              (tweets about storm + baseline twitter activity)
# remember to multiply something by 1.0 so that you'll get decimal division
# also if the denominator would end up being 0, set the result to 0

# See 03-spatial-join.sql for tips on managing the data in PostGIS

# Either in QGIS or in R...
# Map the normalized tweet difference index for Hurricane Dorian
# Try using the heatmap symbology in QGIS to visualize kernel density of tweets
*/

-- Add geometry column named 'geom' of type point and coordinate reference system NAD 1983:
SELECT AddGeometryColumn('schemaname','dorian','geom',4269,'POINT',2, false);

-- SQL to create point geometry, set it's SRID to WGS 1984, and transform it to NAD 1983:
UPDATE dorian set geom = st_transform(st_setsrid(st_makepoint(lng,lat),4326),4269);

-- SQL to register the geometry column for counties, replacing schemaname with your schema's name
SELECT populate_geometry_columns('schemaname.dorian'::regclass)

-- Add your steps to complete the spatial join and NDTI calculation here!

--Count the number of dorian points in each county
## Create new column
ALTER TABLE dorian
ADD COLUMN county_geoid text;

## attach geoid from `counties` to `dorian` data
UPDATE dorian
SET county_geoid = geoid
FROM counties
WHERE st_intersects(st_transform(counties.geometry,4269), st_transform(dorian.geom,4269));

## count number of dorian tweet points in each county
SELECT county_geoid, count(county_geoid) as counttweets
FROM dorian
group by county_geoid;

## add new column for number of dorian tweets to each county
ALTER TABLE counties
ADD COLUMN doriantweet_count float;

##add dorian tweet count to counties
UPDATE counties
SET doriantweet_count = counttweets
FROM
(SELECT county_geoid, count(county_geoid) as counttweets
from dorian
group by county_geoid) AS a
WHERE a.county_geoid = counties.geoid;




-- Count the number of november points in each county
## Create new column
ALTER TABLE november
ADD COLUMN county_geoid text;

## attach geoid from `counties` to `november` data
UPDATE november
SET county_geoid = geoid
FROM counties
WHERE st_intersects(st_transform(counties.geometry,4269), st_transform(november.geom,4269));

## count number of november tweet points in each county
SELECT county_geoid, count(county_geoid) as counttweets
FROM november
group by county_geoid;

## add new column for number of november tweets to each county
ALTER TABLE counties
ADD COLUMN novembertweet_count float;

##add november tweet count to counties
UPDATE counties
SET novembertweet_count = novcounttweets
FROM
(SELECT county_geoid, count(county_geoid) as novcounttweets
from november
group by county_geoid) AS a
WHERE a.county_geoid = counties.geoid;


-- Set counties with no points to 0 for the november count
UPDATE counties
SET novembertweet_count = 0
WHERE novembertweet_count IS NULL;

# Set counties with no points to 0 for the dorian count
UPDATE counties
SET doriantweet_count = 0
WHERE doriantweet_count IS NULL;


-- Calculate the normalized difference tweet index (made this up, based on NDVI), where
# ndti = (tweets about storm – baseline twitter activity) / (tweets about storm + baseline twitter activity)
# remember to multiply something by 1.0 so that youll get decimal devision, not integer division
# also if the denominator would end up being 0, set the result to 0

ALTER TABLE counties
ADD COLUMN ndti float;

UPDATE counties
SET ndti = ((doriantweet_count - novembertweet_count) / (doriantweet_count + novembertweet_count)) * 1.0
WHERE doriantweet_count != 0 OR novembertweet_count != 0;

UPDATE counties
SET ndti = 0
WHERE ndti IS NULL;
