//GOAL: Compute NDVI for 2020 to identify green space in LA and PHX

//STEPS:
//1. Read in Sentinel imagery and mask clound and cloud shadows
//2. Calculate NDVI
//4. Create and export binary green space raster (using 0.2 thresh)

//IMPORTS:
var s2 = ee.ImageCollection("COPERNICUS/S2"),
    s2c = ee.ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY"),
    s2Sr = ee.ImageCollection("COPERNICUS/S2_SR"),
    LA = ee.FeatureCollection("users/mfstuhlmacher/EJLSAMultiCity/LA_CityBounds"),
    PHX = ee.FeatureCollection("users/mfstuhlmacher/EJLSAMultiCity/Phoenix_CityBounds");

// The ROI is city boundaries
var fc = ee.FeatureCollection([LA.geometry(),PHX.geometry()]);
Map.addLayer(fc,{},'City boundaries',false);
//print(fc);

//---1. Cloud Masking (From Example Code)---//
//https://code.earthengine.google.com/81fd649a65220f2c1cd114c4300395c3

// Dates over which to create a median composite.
var start = ee.Date('2020-06-01');
var end = ee.Date('2020-09-01');

// S2 L1C for Cloud Displacement Index (CDI) bands.
s2 = s2.filterBounds(fc).filterDate(start, end)
    .select(['B7', 'B8', 'B8A', 'B10']);
// S2Cloudless for the cloud probability band.
s2c = s2c.filterDate(start, end).filterBounds(fc);
// S2 L2A for surface reflectance bands.
s2Sr = s2Sr.filterDate(start, end).filterBounds(fc)
    .select(['B2', 'B3', 'B4', 'B5']);

// Join two collections on their 'system:index' property.
// The propertyName parameter is the name of the property
// that references the joined image.
function indexJoin(collectionA, collectionB, propertyName) {
  var joined = ee.ImageCollection(ee.Join.saveFirst(propertyName).apply({
    primary: collectionA,
    secondary: collectionB,
    condition: ee.Filter.equals({
      leftField: 'system:index',
      rightField: 'system:index'})
  }));
  // Merge the bands of the joined image.
  return joined.map(function(image) {
    return image.addBands(ee.Image(image.get(propertyName)));
  });
}

// Mask clouds and shadows.
function maskImage(image) {
  // Compute the cloud displacement index from the L1C bands.
  var cdi = ee.Algorithms.Sentinel2.CDI(image);
  var s2c = image.select('probability');
  var cirrus = image.select('B10').multiply(0.0001);

  // Assume low-to-mid atmospheric clouds to be pixels where probability
  // is greater than 65%, and CDI is less than -0.5. For higher atmosphere
  // cirrus clouds, assume the cirrus band is greater than 0.01.
  // The final cloud mask is one or both of these conditions.
  var isCloud = s2c.gt(65).and(cdi.lt(-0.5)).or(cirrus.gt(0.01));

  // Reproject is required to perform spatial operations at 20m scale.
  // 20m scale is for speed, and assumes clouds don't require 10m precision.
  isCloud = isCloud.focal_min(3).focal_max(16);
  isCloud = isCloud.reproject({crs: cdi.projection(), scale: 20});

  // Project shadows from clouds we found in the last step. This assumes we're working in
  // a UTM projection.
  var shadowAzimuth = ee.Number(90)
      .subtract(ee.Number(image.get('MEAN_SOLAR_AZIMUTH_ANGLE')));

  // With the following reproject, the shadows are projected 5km.
  isCloud = isCloud.directionalDistanceTransform(shadowAzimuth, 50);
  isCloud = isCloud.reproject({crs: cdi.projection(), scale: 100});

  isCloud = isCloud.select('distance').mask();
  return image.select('B2','B3','B4','B8').updateMask(isCloud.not());
}

// Join the cloud probability dataset to surface reflectance.
var withCloudProbability = indexJoin(s2Sr, s2c, 'cloud_probability');
// Join the L1C data to get the bands needed for CDI.
var withS2L1C = indexJoin(withCloudProbability, s2, 'l1c');

// Map the cloud masking function over the joined collection.
var masked = ee.ImageCollection(withS2L1C.map(maskImage));

// Take the median, specifying a tileScale to avoid memory errors.
var median = masked.reduce(ee.Reducer.median(), 8);

// Display the results.
var viz = {bands: ['B4_median', 'B3_median', 'B2_median'], min: 0, max: 3000};
Map.addLayer(median, viz, 'Sentinel',false);

//---2. Calculate NDVI---//
var ndvi = median.normalizedDifference(['B8_median', 'B4_median']).rename('NDVI');
var ndvi_palette = {min:-0.1, max:1, palette: ['FFFFFF','CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901', '66A000','529400',
                                              '3E8601', '207401', '056201', '004C00', '023B01', '012E01', '011D01', '011301']};
Map.addLayer(ndvi,ndvi_palette,'NDVI',false);

//---3. Calculate 80th percentile of NDVI---//
//Testing 80th percentile for threshold comparison
// var pct80NDVI = ndvi.reduceRegion({
//   reducer: ee.Reducer.percentile([80]), 
//   geometry: LA, 
//   scale: 10,
//   maxPixels: 1e9
// }).get('NDVI');
// print(ee.Number(pct80NDVI),'80th NDVI threshold'); //0.4179607939816114

//---4. Create and export binary green space raster---//
var green = ndvi.gte(ee.Number(0.2)); //Threshold at 0.2 NDVI 
Map.addLayer(green.selfMask(),{},'Binary Greenspace Raster',false);

Export.image.toDrive({
  image: green,
  description: "LA_Sentinel2_NDVI02",
  folder:"EJLSAMultiCity",
  fileNamePrefix: "LA_Sentinel2_NDVI02",
  region: LA,
  scale: 10,
  maxPixels: 1e13,
  crs: 'EPSG:32616'
});

Export.image.toDrive({
  image: green,
  description: "Phoenix_Sentinel2_NDVI02",
  folder:"EJLSAMultiCity",
  fileNamePrefix: "Phoenix_Sentinel2_NDVI02",
  region: PHX,
  scale: 10,
  maxPixels: 1e13,
  crs: 'EPSG:32616'
});