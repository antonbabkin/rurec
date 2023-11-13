
# S3 methods for automatic reticulate conversion of GeoDataFrame and GeoSeries ----

# Convert Python geopandas.GeoSeries to R sfc object
py_to_r.geopandas.geoseries.GeoSeries <- function(x) {
  crs <- x$crs$to_epsg()
  # GeoSeries.to_wkt() returns numpy array of WKT strings, which is automatically converted to R char array
  x <- x$to_wkt()
  # convert char array to sfc, keeping original CRS
  sf::st_as_sfc(x, crs = sf::st_crs(crs))
}

# Convert Python geopandas.GeoDataFrame to R sf object
py_to_r.geopandas.geodataframe.GeoDataFrame <- function(x) {
  # GeoSeries automatically converts to sfc
  geom_col <- x$geometry
  # convert geopandas.GeoDataFrame to pandas.DataFrame,
  # which automatically converts to R data.frame
  pd <- reticulate::import("pandas")
  x <- pd$DataFrame(x)
  # replace geometry with sfc object
  x$geometry <- geom_col
  # convert data.frame to sf object
  sf::st_as_sf(x)
}
