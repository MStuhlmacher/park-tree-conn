#Michelle Stuhlmacher

##GOALS:
#1) Calculate landscape metrics for each census tract in city list
#     a) park green space
#     b) all green space (NDVI)
#2) Format results in a way that can be used in later analysis

#STEPS:
#1. Import data and libraries
#2. Rasterize park polygon
#3. Loop that clips raster to census tract boundary, calculates landscape metrics, appends results to table, and exports

# STEP 1 -----------------------------------------------
# Import data and libraries
library(sf)
library(raster)
library(landscapemetrics)
library(landscapetools)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EJLSAMultiCity") #work laptop

##Re-project all files to match, using spatial reference: https://spatialreference.org/ref/epsg/32616/
#sr = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs";

cityList = c('Chicago','Houston','Indianapolis','Jacksonville','LA','NewYork','Phoenix','Portland','Seattle','StLouis')

# STEP 2 -----------------------------------------------
# Loop for calculating NDVI metrics 
#Create data frame for new values
DF_NDVI = data.frame(matrix(ncol = 7, nrow = 0))
colnames(DF_NDVI) = c('layer','level','class','id','metric','value','city')

for (city in cityList) {
  print(city)
  
  #census tracts
  file2 = paste("./Data/Census/Cleaned/census2020_",city,".shp",sep="") #census data for each city
  census = st_read(file2)
  
  #NDVI image
  file1 = paste("D:/EJLSAMultiCity/",city,"_Sentinel2_NDVI.tif",sep="") #NDVI image for each city
  ndviR = raster(file1)
  ndvi = projectRaster(ndviR, crs = crs(census), method = 'ngb')
  
  #city boundaries
  file3 = paste("./Data/CityBoundaries/",city,"/CityBoundary.shp",sep="") #boundary for each city
  bound = st_read(file3)
  bound_rp = st_transform(bound, crs(census))
  
  #Clip tracts to city boundaries
  #tractCity = st_intersection(census,bound_rp)
  tractCity = census[bound_rp,]
  
  #Non-connectivity measures
  for(i in 1:nrow(tractCity)) {
    tract = tractCity[i,]
    crop = crop(ndvi,tract)
    mask = mask(crop,tract)
    lsm = calculate_lsm(mask, what = c('lsm_c_pland'),verbose = T)
    lsm$id = tract$GISJOIN
    lsm$city = city
    DF_NDVI = rbind(DF_NDVI,lsm)
  }
  
  #Quarter mile buffer around census tract to include any surrounding green space within a 5min walk
  for(i in 1:nrow(tractCity)) {
    tract = tractCity[i,]
    tract_buff = st_buffer(tract,dist = 402.336)
    crop = crop(ndvi,tract_buff)
    mask = mask(crop,tract)
    lsm = calculate_lsm(mask, what = c('lsm_c_area_mn','lsm_c_contig_mn','lsm_c_ca','lsm_c_ai'), directions = 8, verbose = T)
    lsm$id = tract$GISJOIN
    lsm$city = city
    DF_NDVI = rbind(DF_NDVI,lsm)
    print(tract$GISJOIN)
  }
}

#Drop non-green space values
DF_NDVI = subset(DF_NDVI, class == 1)

#Drop columns and rename for combining
DF_NDVIS = DF_NDVI[ , c("id","metric","value","city")]
colnames(DF_NDVIS)[colnames(DF_NDVIS) == "value"] <- "ndviValue"

#Export
write.csv(DF_NDVIS,'./Data/LandscapeMetrics/allCities_NDVILSA.csv')

# STEP 3 -----------------------------------------------
# Loop for calculating park metrics

#Create data frame for new values
DF_park = data.frame(matrix(ncol = 7, nrow = 0))
colnames(DF_park) = c('layer','level','class','id','metric','value','city')

for (city in cityList) {
  print(city)
  
  #census tracts
  file2 = paste("./Data/Census/Cleaned/census2020_",city,".shp",sep="")
  census = st_read(file2)
  
  ##NDVI image
  file1 = paste("D:/EJLSAMultiCity/",city,"_Sentinel2_NDVI.tif",sep="")
  ndviR = raster(file1)
  ndvi = projectRaster(ndviR, crs = crs(census), method = 'ngb')
  
  #city boundaries
  file3 = paste("./Data/CityBoundaries/",city,"/CityBoundary.shp",sep="")
  bound = st_read(file3)
  bound_rp = st_transform(bound, crs(census))
  
  #park image
  file4 = paste("./Data/Park/CityBoxClip/ParkServe_Parks_05182021_",city,"Box.shp",sep="")
  park = st_read(file4)
  park_rp = st_transform(park, crs(census))
  #Clip to city bounds
  parkClip = st_intersection(park_rp,bound_rp)
  parkClip$read = 1
  #plot(parkClip)
  #rasterize (10m pixels)
  parkRaster = rasterize(parkClip,ndvi,field = "read",background = 0)
  #plot(parkRaster)
  #check_landscape(parkRaster)
  
  #Clip tracts to city boundaries
  #tractCity = st_intersection(census,bound_rp)
  tractCity = census[bound_rp,]

  #Non-connectivity measures
  for(i in 1:nrow(tractCity)) {
    tract = tractCity[i,]
    crop = crop(parkRaster,tract)
    mask = mask(crop,tract)
    lsm = calculate_lsm(mask, what = c('lsm_c_pland'))
    lsm$id = tract$GISJOIN
    lsm$city = city
    DF_park = rbind(DF_park,lsm)
    print(tract$GISJOIN)
  }

  #Connectivity measures
  for(i in 1:nrow(tractCity)) {
    tract = tractCity[i,]
    tract_buff = st_buffer(tract,dist = 402.336)
    crop = crop(parkRaster,tract_buff)
    mask = mask(crop,tract)
    lsm = calculate_lsm(mask, what = c('lsm_c_area_mn','lsm_c_contig_mn','lsm_c_ca','lsm_c_ai'), directions = 8)
    lsm$id = tract$GISJOIN
    lsm$city = city
    DF_park = rbind(DF_park,lsm)
    print(tract$GISJOIN)
  }
}

#Drop non-green space values
DF_park2 = subset(DF_park, class == 1)

#Drop columns and rename for combining
DF_parkS = DF_park2[ , c("id","metric","value","city")]
colnames(DF_parkS)[colnames(DF_parkS) == "value"] <- "parkValue"

#Export
write.csv(DF_parkS,'./Data/LandscapeMetrics/allCities_ParkLSA.csv')

