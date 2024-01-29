
//------------------------------------------------------------
// mbita-schisto-gsw-google-ee.js
//
// This script loads the global surface water
// layer from Google Earth Engine, crops it to the
// study region, and calculates the minimum distance from
// every point in the region to surface water
//
// NOTE: you cannot run this without the lat/lon coordinates
//       of the study villages. As of this date, 2020 Apr 7,
//       Google fusion tables are also deprecated, but we have
//       not updated the script (last run 2019 Aug 19).
//------------------------------------------------------------

//------------------------------------------------------------
// Load a Kenya boundary from a Fusion Table
// (from: https://developers.google.com/earth-engine/reducers_reduce_to_vectors)
//------------------------------------------------------------
var kenya = ee.FeatureCollection('ft:1tdSwUL7MVpOauSgRzqVTOwdfy17KDbw-1d9omPw')
  .filter(ee.Filter.eq('Country', 'Kenya'));
  
var region = ee.Geometry.Rectangle([33.8969, -0.3207, 34.5747, -0.5905]);

//------------------------------------------------------------
// Load a Fusion Table from the ID using the FeatureCollection constructor.
// This is the village level GPS locations
//------------------------------------------------------------
var vilgps = ee.FeatureCollection('ft:1ilRnSAVvcAiA-D7_Fxiw3fbpx3drGz5V1ECMRM7M');


//------------------------------------------------------------
// Import global surface water, clipped to Kenya study region
//------------------------------------------------------------
var gsw = ee.Image('JRC/GSW1_0/GlobalSurfaceWater')
  .select('occurrence')
  .clipToCollection(ee.FeatureCollection(region));
  
//------------------------------------------------------------
// center the map and add the village GPS data to the map
//------------------------------------------------------------
Map.setCenter(34.21897888183594,-0.45730105289266143, 11);
Map.addLayer(vilgps,{'color':'#992288'});

//------------------------------------------------------------
// reduce global surface water raster to a polygon vector
// calculate the distance from each village to the closest water
//------------------------------------------------------------

var water_mask = gsw.gt(100);

print('water_mask',water_mask);

var VIS_WATER_MASK = {
  palette: ['blue']
};

Map.addLayer({
  eeObject: water_mask,
  visParams: VIS_WATER_MASK,
  name: '100% occurrence water mask'
});

var watpoly = water_mask.reduceToVectors({
  geometry: region,
  crs: water_mask.projection(),
  scale: water_mask.projection().nominalScale(),
  geometryType: 'polygon',
  eightConnected: false,
  labelProperty: 'water',
  bestEffort:true
//  reducer: ee.Reducer.mean()
});

// create a withinDistance filter to identify features w/in
// 10km (10,000m) of villages.
var distFilter = ee.Filter.withinDistance({
  distance: 10000,
  leftField: '.geo',
  rightField: '.geo',
  maxError: 10
});

// Define a saveBest join.
// This will be used to join village GPS points to
// water polygons, and save the first join based on distance
var distSaveBest = ee.Join.saveBest({
  matchKey: 'mindistance',
  measureKey: 'distance'
});

// Define a saveFirst join. (not used)
// This will be used to join village GPS points to
// water polygons, and save the first join based on distance
//var distSaveFirst = ee.Join.saveFirst({
//  matchKey: 'mindistance',
//  ordering: 'distance',
//  measureKey: 'distance'
//});

// Apply the join.
var spatialJoined = distSaveBest.apply(vilgps, watpoly, distFilter);
print('spatialJoined',spatialJoined);

var getdist = function(x){
  var wdist = x.get('mindistance');
  return wdist;
};

var waterdist = spatialJoined.map(getdist);
print('waterdist',waterdist);


//Map.addLayer(waterdist,{'color':'#992288'},'water distance');

// Export dataset with distance to water
Export.table.toDrive({
    collection: waterdist,
    description:'mbita-water-distance',
    folder:'Mbita-results',
    fileFormat: 'CSV'});

// Export the 100% surface water GeoTIFF.
Export.image.toDrive({
  image: gsw,
  description: 'mbita_gsw',
  folder:'Mbita-results',
  scale: 50,
  region: region,
  fileFormat: 'GeoTIFF'
});

// Export the 100% surface water mask polygon
Export.table.toDrive({
  collection: watpoly,
  description: 'mbita_gswpoly',
  folder:'Mbita-results',
  fileFormat: 'GeoJSON'
});

