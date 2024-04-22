#Michelle Stuhlmacher

#GOAL: Loop to clean census data

#Variables = 
#Population
#White, Black, Native American, Asian
#Hispanic
#Educational attainment (percent of population with a bachelor's degree or higher)
#Per capita income
#Year housing built (percent of housing older than 30yrs)
#Median age

#STEPS:
#1. Read in data and import libraries
#2. Subset to counties of interest
#3. Subset variables of interest
#4. Merge with SHP and export

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(dplyr)
library(sf)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity") #work laptop

#Import files
csv2020 = read.csv('./Data/Census/nhgis0024_csv/nhgis0024_ds249_20205_tract.csv')
shp2020 = read_sf('./Data/Census/nhgis0024_shapefile_tl2020_us_tract_2020/US_tract_2020.shp')

# STEP 2 -----------------------------------------------
#Subset to the counties of interest

csv2020_s = subset(csv2020, STATE == "Illinois" & COUNTY == "Cook County" |     #Chicago
                     STATE == "California" & COUNTY == "Los Angeles County" |   #LA
                     STATE == "Washington" & COUNTY == "King County" |          #Seattle
                     STATE == "Oregon" & COUNTY == "Multnomah County" |         #Portland
                     STATE == "Oregon" & COUNTY == "Washington County" |         #Portland
                     STATE == "Oregon" & COUNTY == "Clackamas County" |         #Portland
                     STATE == "Arizona" & COUNTY == "Maricopa County"  |        #Phoenix
                     STATE == "Missouri" & COUNTY == "St. Louis city"  |        #St. Louis
                     STATE == "Texas" & COUNTY == "Harris County"  |            #Houston
                     STATE == "Texas" & COUNTY == "Montgomery County"  |        #Houston
                     STATE == "Texas" & COUNTY == "Fort Bend County"  |         #Houston
                     STATE == "Indiana" & COUNTY == "Marion County"  |          #Indianapolis
                     STATE == "New York" & COUNTY == "New York County"  |       #New York
                     STATE == "New York" & COUNTY == "Kings County"  |       #New York
                     STATE == "New York" & COUNTY == "Queens County"  |       #New York
                     STATE == "New York" & COUNTY == "Bronx County"  |       #New York
                     STATE == "New York" & COUNTY == "Richmond County"  |       #New York
                     STATE == "Florida" & COUNTY == "Duval County")             #Jacksonville 

# STEP 3 -----------------------------------------------
#Subset to variables of interest

#AMPVE001:    Total Population
#AMPWE002:    White alone
#AMPWE003:    Black or African American alone
#AMPWE004:    American Indian and Alaska Native alone
#AMPWE005:    Asian alone
#AMPWE006:    Native Hawaiian and Other Pacific Islander alone
#AMP3E001:    Total (Hispanic or Latino)
#AMP3E012:    Hispanic or Latino
#AMRZE001:    Total (Educational attainment)
#AMRZE002:    No schooling completed
#AMRZE003:    Nursery school
#AMRZE004:    Kindergarten
#AMRZE005:    1st grade
#AMRZE006:    2nd grade
#AMRZE007:    3rd grade
#AMRZE008:    4th grade
#AMRZE009:    5th grade
#AMRZE010:    6th grade
#AMRZE011:    7th grade
#AMRZE012:    8th grade
#AMRZE013:    9th grade
#AMRZE014:    10th grade
#AMRZE015:    11th grade
#AMRZE016:    12th grade, no diploma
#AMRZE022:    Bachelor's degree
#AMRZE023:    Master's degree
#AMRZE024:    Professional school degree
#AMRZE025:    Doctorate degree
#AMTCE001:    Per capita income in the past 12 months (in 2020 inflation-adjusted dollars)
#AMU7E001:    Total housing units
#AMU7E005:    Built 1990 to 1999
#AMU7E006:    Built 1980 to 1989
#AMU7E007:    Built 1970 to 1979
#AMU7E008:    Built 1960 to 1969
#AMU7E009:    Built 1950 to 1959
#AMU7E010:    Built 1940 to 1949
#AMU7E011:    Built 1939 or earlier
#AMPLE001:    Median age: Total

#Add educational attainment columns
csv2020_s$gteBachDeg = csv2020_s$AMRZE022 + csv2020_s$AMRZE023 + csv2020_s$AMRZE024 + csv2020_s$AMRZE025 #Bachelor's degree or later
csv2020_s$pop25 = csv2020_s$AMRZE001
csv2020_s$pctCedu = csv2020_s$gteBachDeg/csv2020_s$pop25

#No high school diploma (or equivalent - GED)
csv2020_s$noHS = csv2020_s$AMRZE002 + csv2020_s$AMRZE003 + csv2020_s$AMRZE004 + csv2020_s$AMRZE005 + csv2020_s$AMRZE006 +
  csv2020_s$AMRZE007 + csv2020_s$AMRZE008 + csv2020_s$AMRZE009 + csv2020_s$AMRZE010 + csv2020_s$AMRZE011 + 
  csv2020_s$AMRZE012 + csv2020_s$AMRZE013 + csv2020_s$AMRZE014 + csv2020_s$AMRZE015 + csv2020_s$AMRZE016
csv2020_s$pctNoHS = csv2020_s$noHS/csv2020_s$pop25

#Add housing age columns
csv2020_s$gteH30yr = csv2020_s$AMU7E005 + csv2020_s$AMU7E006 + csv2020_s$AMU7E007 + csv2020_s$AMU7E008 +
  csv2020_s$AMU7E009 + csv2020_s$AMU7E010 + csv2020_s$AMU7E011
csv2020_s$pct_H30yro = csv2020_s$gteH30yr/csv2020_s$AMU7E001
csv2020_s$tot_HU = csv2020_s$AMU7E001

#Subset to columns of interest
csv2020_sn = csv2020_s[, c("GISJOIN","STATE","COUNTY","AMPVE001","AMPWE002","AMPWE003","AMPWE004","AMPWE005","AMPWE006",
                           "AMP3E012","AMTCE001","pct_H30yro","AMPLE001","pctCedu","pctNoHS","pop25", "tot_HU","gteH30yr","gteBachDeg","noHS")]

#Rename                                 
colnames(csv2020_sn) =  c("GISJOIN","STATE","COUNTY",   "totPop",   "white",   "black",   "ntvAm",   "asian",  "ntvHPI",
                              "hisp",  "incPCap","pctH30yro",  "medAge", "pctCedu","pctNoHS","pop25", "tot_HU","gteH30yr","gteBachDeg","noHS")

# STEP 4 -----------------------------------------------    
#Merge with SHP and export
export2020 = inner_join(shp2020,csv2020_sn, by='GISJOIN')

#Calculate population density
#st_crs(export2020) #Our map units are m
export2020$area = st_area(export2020)
#Remove units
export2020$areaSqM = as.numeric(lapply(export2020$area, function(x) as.numeric(sub("\\s+\\D+$", "", x))))
#Square km
export2020$areaSqKm = export2020$areaSqM/1000000
#Calculate # of people per acre
export2020$ppl_acre = export2020$totPop/(export2020$areaSqM/4046.85642)

#Select columns for export
export2020 = export2020[, c("GISJOIN","STATE","COUNTY",   "totPop",   "white",   "black",   "ntvAm",   "asian",  "ntvHPI",
                            "hisp",  "incPCap","pctH30yro",  "medAge", "pctCedu","pctNoHS","areaSqKm","ppl_acre","pop25",
                            "tot_HU","gteH30yr","gteBachDeg","noHS","geometry")]

#Split out by city and export
##Chicago
export2020_Chicago = subset(export2020, STATE == "Illinois" & COUNTY == "Cook County")
st_write(export2020_Chicago, "./Data/Census/Cleaned/census2020_Chicago.shp")

##LA
export2020_LA = subset(export2020, STATE == "California" & COUNTY == "Los Angeles County")
st_write(export2020_LA, "./Data/Census/Cleaned/census2020_LA.shp")

##Seattle
export2020_Seattle = subset(export2020, STATE == "Washington" & COUNTY == "King County")
st_write(export2020_Seattle, "./Data/Census/Cleaned/census2020_Seattle.shp")

##Portland
export2020_Portland = subset(export2020, STATE == "Oregon")
st_write(export2020_Portland, "./Data/Census/Cleaned/census2020_Portland.shp")

##Phoenix
export2020_Phoenix = subset(export2020, STATE == "Arizona" & COUNTY == "Maricopa County")
st_write(export2020_Phoenix, "./Data/Census/Cleaned/census2020_Phoenix.shp")

##St. Louis
export2020_StLouis = subset(export2020, STATE == "Missouri" & COUNTY == "St. Louis city")
st_write(export2020_StLouis, "./Data/Census/Cleaned/census2020_StLouis.shp")

##Houston
export2020_Houston = subset(export2020, STATE == "Texas")
st_write(export2020_Houston, "./Data/Census/Cleaned/census2020_Houston.shp")

##Indianapolis
export2020_Indianapolis = subset(export2020, STATE == "Indiana" & COUNTY == "Marion County")
st_write(export2020_Indianapolis, "./Data/Census/Cleaned/census2020_Indianapolis.shp")

##New York
export2020_NewYork = subset(export2020, STATE == "New York")
st_write(export2020_NewYork, "./Data/Census/Cleaned/census2020_NewYork.shp")

##Jacksonville
export2020_Jacksonville = subset(export2020, STATE == "Florida" & COUNTY == "Duval County")
st_write(export2020_Jacksonville, "./Data/Census/Cleaned/census2020_Jacksonville.shp")
                                       