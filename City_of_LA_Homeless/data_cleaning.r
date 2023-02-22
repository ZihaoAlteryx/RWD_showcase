#load('data.Rdata')
library(jsonlite) # Transfer JSON data into R
library(dplyr)
library(tidyr) 
library(xlsx) # read excel file
library(sqldf) # using SQL commands in R
library(caret) # package for creating predictive models 


# Read in data files
shelters <- read.csv('shelters_w_CTs20171102134808.csv')
crimes <- read.csv('crime_w_CTs20171102103130.csv')
calls <- read.csv('311_calls_w_CTs20171102134828.csv')
victims <- read.csv('Crime__Homeless_Victim_8_16_-_8_17.csv')
requests <- read.csv('311_Homeless_Encampments_Requests.csv')

# Make victims tidy
victims <- victims %>%
  separate(Location, into = c('lat', 'lon'), sep = ',') %>%
  mutate(lat = trimws(substr(lat, 2, nchar(lat)), which = 'both'),
         lon = trimws(substr(lon, 2, nchar(lon)-1), which = 'both'))

# Use API to get census tract numbers on data sets
# Add longitude and latitude information to the shelter dataset
for (i in 1:nrow(shelters)) {
  lon <- shelters[i, 'LONGITUDE']
  lat <- shelters[i, 'LATITUDE']
  url <- paste0('http://data.fcc.gov/api/block/find?format=json&latitude=', lat, '&longitude=', lon, '&showall=true')
  api_data <- fromJSON(url) 
  api_county_fips <- api_data$County$FIPS
  api_county_name <- api_data$County$name
  api_block_fips <- api_data$Block$FIPS
  
  shelters[i, 'api_county_fips'] <- api_county_fips
  shelters[i, 'api_county_name'] <- api_county_name
  shelters[i, 'api_block_fips'] <- api_block_fips
  
}

# Add longitude and latitude information to the victims dataset
for (i in 1:nrow(victims)) {

  lon <- victims[i, 'lon']
  lat <- victims[i, 'lat']
  url <- paste0('http://data.fcc.gov/api/block/find?format=json&latitude=', lat, '&longitude=', lon, '&showall=true')
  api_data <- fromJSON(url) 
  api_county_fips <- api_data$County$FIPS
  api_county_name <- api_data$County$name
  api_block_fips <- api_data$Block$FIPS
  
  victims[i, 'api_county_fips'] <- ifelse(is.null(api_county_fips), 0, api_county_fips)
  victims[i, 'api_county_name'] <- ifelse(is.null(api_county_name), 0, api_county_name)
  victims[i, 'api_block_fips'] <- ifelse(is.null(api_block_fips), 0, api_block_fips)
  print(i)
}

# Add longitude and latitude information to the crimes dataset
for (i in 1:nrow(crimes)) {
  
  lon <- crimes[i, 'LONGITUDE']
  lat <- crimes[i, 'LATITUDE']
  url <- paste0('http://data.fcc.gov/api/block/find?format=json&latitude=', lat, '&longitude=', lon, '&showall=true')
  api_data <- fromJSON(url) 
  api_county_fips <- api_data$County$FIPS
  api_county_name <- api_data$County$name
  api_block_fips <- api_data$Block$FIPS
  
  crimes[i, 'api_county_fips'] <- ifelse(is.null(api_county_fips), 0, api_county_fips)
  crimes[i, 'api_county_name'] <- ifelse(is.null(api_county_name), 0, api_county_name)
  crimes[i, 'api_block_fips'] <- ifelse(is.null(api_block_fips), 0, api_block_fips)
  print(i)
}


# Add longitude and latitude information to the request dataset
for (i in 1:nrow(requests)) {
  lon <- ifelse(is.na(requests[i, 'Longitude']), 0, requests[i, 'Longitude'])
  lat <- ifelse(is.na(requests[i, 'Latitude']), 0, requests[i, 'Latitude'])
  url <- paste0('http://data.fcc.gov/api/block/find?format=json&latitude=', lat, '&longitude=', lon, '&showall=true')
  api_data <- fromJSON(url) 
  api_county_fips <- api_data$County$FIPS
  api_county_name <- api_data$County$name
  api_block_fips <- api_data$Block$FIPS
  
  requests[i, 'api_county_fips'] <- ifelse(is.null(api_county_fips), 0, api_county_fips)
  requests[i, 'api_county_name'] <- ifelse(is.null(api_county_name), 0, api_county_name)
  requests[i, 'api_block_fips'] <- ifelse(is.null(api_block_fips), 0, api_block_fips)
  print(i)  
}

# Add longitude and latitude information to the calls dataset
for (i in 1:nrow(calls)) {
  lon <- ifelse(is.na(calls[i, 'LONGITUDE']), 0, calls[i, 'LONGITUDE'])
  lat <- ifelse(is.na(calls[i, 'LATITUDE']), 0, calls[i, 'LATITUDE'])
  url <- paste0('http://data.fcc.gov/api/block/find?format=json&latitude=', lat, '&longitude=', lon, '&showall=true')
  api_data <- fromJSON(url) 
  api_county_fips <- api_data$County$FIPS
  api_county_name <- api_data$County$name
  api_block_fips <- api_data$Block$FIPS
  
  calls[i, 'api_county_fips'] <- ifelse(is.null(api_county_fips), 0, api_county_fips)
  calls[i, 'api_county_name'] <- ifelse(is.null(api_county_name), 0, api_county_name)
  calls[i, 'api_block_fips'] <- ifelse(is.null(api_block_fips), 0, api_block_fips)
}

# Read in PIT files and manipulate
CT_2015 = read.xlsx("HC2015_CensusTracts_LACOC.xlsx", sheetName = 'Update 07 27 15')
CT_2016 = read.xlsx("HC2016_Total_Counts_by_Census_Tract_LA_CoC_07132016.xlsx", sheetName = 'Data')
CT_2017 = read.xlsx("homeless-count-2017-results-by-census-tract.xlsx", sheetName = 'Count_by_Tract')

CT_2015_TOT <- CT_2015[,c('Year', 'Tract', 'Unsheltered')]
CT_2016_TOT <- CT_2016[,c('Year', 'censusTract', 'totUnsheltPeople')]
CT_2017_TOT <- CT_2017[,c('Year', 'tract', 'totUnsheltPeople')]

colnames(CT_2015_TOT) <- c('Year', 'Tract', 'Total')
colnames(CT_2016_TOT) <- c('Year', 'Tract', 'Total')
colnames(CT_2017_TOT) <- c('Year', 'Tract', 'Total')

# Full join the dataframes 
CT <- merge(x = CT_2015_TOT, y = CT_2016_TOT, by = "Tract", all = TRUE)
CT <- merge(x = CT, y = CT_2017_TOT, by = "Tract", all = TRUE)
# Rename the column names
colnames(CT) <- c('TRACT', '2015', 'TOT_2015', '2016', 'TOT_2016', '2017', 'TOT_2017')

# Replace all the NAs to 0
CT <- CT %>%
  select(TRACT, TOT_2015, TOT_2016, TOT_2017) %>%
  mutate(TOT_2015 = ifelse(is.na(TOT_2015), 0, TOT_2015),
         TOT_2016 = ifelse(is.na(TOT_2016), 0, TOT_2016),
         TOT_2017 = ifelse(is.na(TOT_2017), 0, TOT_2017)) %>%
  mutate(PER_DELTA = ifelse(TOT_2016 == 0, TOT_2017, (TOT_2017-TOT_2016)/TOT_2016))
CT[c('TOT_2015', "TOT_2016", "TOT_2017")][is.na(CT[c('TOT_2015', "TOT_2016", "TOT_2017")])] <- 0

# Read in type of crime file
vc <- read.csv('C://Users//Zihao//Desktop//crime_dataset.csv')
vc <- vc %>%
  select(CRIME.CODE.DESCRIPTION, Type)

crime_type <- unique(vc)

# Assign crime type to crimes file, left join
crimes <- merge(x = crimes, y = crime_type, by = "CRIME.CODE.DESCRIPTION", all.x = TRUE)

# Mutate crimes file to get hour
crimes <- crimes %>%
  mutate(HOUR = ifelse(TIME.OCCURRED < 1000, substr(TIME.OCCURRED, 1, 1), substr(TIME.OCCURRED, 1, 2)))

# Read in substance abuse file
substance_abuse <- read.csv('C:/Users/Zihao/Google Drive/USC MSBA Course Materials/DSO 545 - Statistical Computing and Visualization/Homeless Project/Substance_Abuse_Programs.csv')

# Extract census tract numbers 
crimes$CENSUS_TRACT <- substr(crimes$api_block_fips, 6, 11)
calls$CENSUS_TRACT <- substr(calls$api_block_fips, 6, 11)
shelters$CENSUS_TRACT <- substr(shelters$api_block_fips, 6, 11)

# Create a precinct lookup data frame
precinct_lookup <- calls %>%
  group_by(POLICEPRECINCT, CENSUS_TRACT) %>%
  summarise(count = n()) %>%
  select(POLICEPRECINCT, CENSUS_TRACT) %>%
  unique()

# Remove all the empty rows
precinct_lookup <- precinct_lookup[complete.cases(precinct_lookup),]
precinct_lookup <- precinct_lookup %>%
  filter(POLICEPRECINCT != '')

# Load in district lookup file
district_lookup <- read.csv('C:/Users/Zihao/Google Drive/USC MSBA Course Materials/DSO 545 - Statistical Computing and Visualization/Homeless Project/district_tract_lookup.csv')

# Load in zip code lookup file
zip_lookup <- read.csv('C:/Users/Zihao/Google Drive/USC MSBA Course Materials/DSO 545 - Statistical Computing and Visualization/Homeless Project/TRACT_ZIP_092017.csv')
colnames(zip_lookup) <- c('CENSUS_TRACT', 'ZIPCODE')
zip_lookup$CENSUS_TRACT <- substr(zip_lookup$CENSUS_TRACT, 5, nchar(zip_lookup$CENSUS_TRACT))
library(sqldf)

# Join aggregation methods from lookup files
q <- 'select crimes.*, district_lookup.CongressionalDistrict, precinct_lookup.POLICEPRECINCT, zip_lookup.ZIPCODE
from crimes
left join district_lookup on district_lookup.Tract = crimes.CENSUS_TRACT
left join precinct_lookup on precinct_lookup.CENSUS_TRACT = crimes.CENSUS_TRACT
left join zip_lookup on zip_lookup.CENSUS_TRACT = crimes.CENSUS_TRACT'

crimes <- sqldf(q)

q <- 'select shelters.*, district_lookup.CongressionalDistrict, precinct_lookup.POLICEPRECINCT, zip_lookup.ZIPCODE
from shelters
left join district_lookup on district_lookup.Tract = shelters.CENSUS_TRACT
left join precinct_lookup on precinct_lookup.CENSUS_TRACT = shelters.CENSUS_TRACT
left join zip_lookup on zip_lookup.CENSUS_TRACT = shelters.CENSUS_TRACT'

shelters <- sqldf(q)

q <- 'select calls.*, district_lookup.CongressionalDistrict
from calls
left join district_lookup on district_lookup.Tract = calls.CENSUS_TRACT
left join zip_lookup on zip_lookup.CENSUS_TRACT = calls.CENSUS_TRACT'

calls <- sqldf(q)

q <- "
select CT.CENSUS_TRACT, CT.TOT_2015, CT.TOT_2016, CT.TOT_2017, AVG(calls.LATITUDE) as LATITUDE, AVG(calls.LONGITUDE) as LONGITUDE
from CT
left join calls on calls.CENSUS_TRACT = CT.CENSUS_TRACT
group by CT.CENSUS_TRACT, CT.TOT_2015, CT.TOT_2016, CT.TOT_2017
"

CT <- sqldf(q)

q <- 'select CT.*, district_lookup.CongressionalDistrict, precinct_lookup.POLICEPRECINCT, zip_lookup.ZIPCODE
from CT
left join district_lookup on district_lookup.Tract = CT.CENSUS_TRACT
left join precinct_lookup on precinct_lookup.CENSUS_TRACT = CT.CENSUS_TRACT
left join zip_lookup on zip_lookup.CENSUS_TRACT = CT.CENSUS_TRACT'

CT <- sqldf(q)

q <- 'select tot.*, district_lookup.CongressionalDistrict, precinct_lookup.POLICEPRECINCT, zip_lookup.ZIPCODE
from tot
left join district_lookup on district_lookup.Tract = tot.CENSUS_TRACT
left join precinct_lookup on precinct_lookup.CENSUS_TRACT = tot.CENSUS_TRACT
left join zip_lookup on zip_lookup.CENSUS_TRACT = tot.CENSUS_TRACT'

tot <- sqldf(q)


nrow(CT[which(is.na(CT$CongressionalDistrict)),])
nrow(CT[which(is.na(CT$ZIPCODE)),])
nrow(CT[which(is.na(CT$POLICEPRECINCT)),])

# Use kNN to find missing data for police precinct
# Use kNN model to categorize locations with missing info into the nearest police precinct 
library(caret)

knn_model <- train(calls[, c('LATITUDE', 'LONGITUDE')], calls[,'POLICEPRECINCT'], method = 'knn')

test <- shelters[which(is.na(shelters$POLICEPRECINCT)), c('LATITUDE', 'LONGITUDE')]
predictions <- predict(object= knn_model, test[,c('LATITUDE', 'LONGITUDE')])
shelters[which(is.na(shelters$POLICEPRECINCT)), 'POLICEPRECINCT'] <- predictions

test <- crimes[which(is.na(crimes$POLICEPRECINCT)), c('LATITUDE', 'LONGITUDE')]
nrow(test)
predictions <- predict(object= knn_model, test[,c('LATITUDE', 'LONGITUDE')])
crimes[which(is.na(crimes$POLICEPRECINCT)), 'POLICEPRECINCT']<- predictions


# Read in shape files
library(rgdal)

DSN = 'C:/Users/Zihao/Google Drive/USC MSBA Course Materials/DSO 545 - Statistical Computing and Visualization/Homeless Project'
congressional_district_shp <- readOGR(DSN, 'tl_2014_us_cd114')
police_precinct_shp <- readOGR(DSN, 'SHERIFF_LAW_ENFORCEMENT_STATIONS')
zip_shp <- readOGR(DSN, 'cb_2016_us_zcta510_500k')
flood_shp <- readOGR(DSN, 'Flood_Prone_Areas')

# Remove duplicates
calls <- subset(calls, !duplicated(SRNUMBER))
crimes <- subset(crimes, !duplicated(DR.NUMBER))
shelters <- subset(shelters, !duplicated(OBJECTID_left))
CT <- subset(CT, !duplicated(CENSUS_TRACT))

# Rename the police precincts into a shorter version
police_conversion <- c('LAPD Central Division' = 'CENTRAL',
                       'LAPD Newton Division' = 'NEWTON',
                       'LAPD Devonshire Division' = 'DEVONSHIRE',
                       'LAPD Pacific Division' = 'PACIFIC',
                       'LAPD Mission Division' = 'MISSION',
                       'LAPD Rampart Division' = 'RAMPART',
                       'LAPD Southwest Division' = 'SOUTHWEST',
                       'LAPD West Los Angeles Division' = 'WEST LOS ANGELES',
                       'LAPD Northeast Division' = 'NORTHEAST',
                       'LAPD Harbor Division' = 'HARBOR',
                       'LAPD Hollywood Division' = 'HOLLYWOOD',
                       'LAPD Topanga Division' = 'TOPANGA',
                       'LAPD Olympic Division' = 'OLYMPIC',
                       'LAPD 77th Street Division' = '77TH STREET',
                       'LAPD Hollenbeck Division' = 'HOLLENBECK',
                       'LAPD Van Nuys Division' = 'VAN NUYS',
                       'LAPD North Hollywood Division' = 'NORTH HOLLYWOOD',
                       'LAPD Foothill Division' = 'FOOTHILL',
                       'LAPD West Valley Division' = 'WEST VALLEY',
                       'LAPD Southeast Division' = 'SOUTHEAST')
shelters$POLICEPRECINCT <- names(police_conversion)[match(shelters$POLICEPRECINCT, police_conversion)]
calls$POLICEPRECINCT <- names(police_conversion)[match(calls$POLICEPRECINCT, police_conversion)]
crimes$POLICEPRECINCT <- names(police_conversion)[match(crimes$POLICEPRECINCT, police_conversion)]
CT$POLICEPRECINCT <- names(police_conversion)[match(CT$POLICEPRECINCT, police_conversion)]
tot$POLICEPRECINCT <- names(police_conversion)[match(tot$POLICEPRECINCT, police_conversion)]
