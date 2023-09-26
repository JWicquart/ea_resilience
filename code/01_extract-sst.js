// 1. Load site coordinates ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/ea_resilience_site-coords");

//Map.addLayer(site_coords);

// 2. Import NOAA SST data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('2015-06-01', '2015-12-31'))
                  .select('sst');

// 3. Empty Collection to fill

var ft = ee.FeatureCollection(ee.List([]));

// 4. Create function to extract SST ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions(site_coords, ee.Reducer.first(),30);

  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 5. Apply the function ----

var data_sst = ee.FeatureCollection(data_sst.iterate(fill, ft));

// 6. Export the data ----

Export.table.toDrive({
  collection:data_sst,
  folder:"GEE",
  fileNamePrefix:"ea_resilience_sst",
  fileFormat:"CSV",
  description:"ea_resilience_sst",
  selectors:["date", "first", "site_id"]
});
