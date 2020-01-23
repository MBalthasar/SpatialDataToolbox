#' UTM Conversion Tool
#'
#' This function automatically converts a raster or shapefile from a longlat
#' projection into the corresponding UTM projection.
#'
#' @param user_file Can either be a sp/sf object or a raster file.
#'
#' @return The user_file in UTM projection.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(rgdal)
#' library(geosphere)
#'
#' # Create a test polygon within Nigeria
#' my_extent <- extent(5,7,8,10)
#' my_poly <- as(my_extent, 'SpatialPolygons')
#' my_proj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#' proj4string(my_poly) <- CRS(my_proj)
#'
#' # Apply the UTM conversion function
#' my_poly_utm <- UTMConversion(my_poly)
#' crs(my_poly_utm)
#'
#' @export
UTMConversion <- function(user_file){
  # Check if crs of user_file is longlat
  user_crs <- raster::crs(user_file)
  if (substr(user_crs, 0, 13) != "+proj=longlat"){
    stop("User file has to be in longlat projection.")
  }
  # Define extent
  my_extent <- as(raster::extent(user_file), 'SpatialPolygons')
  sp::proj4string(my_extent) <- sp::CRS(as.character(raster::crs(user_file)))
  # Get centroid of extent
  my_centroid <- geosphere::centroid(my_extent)
  # Define Zone for UTM projection
  my_zone <- as.character(floor((sp::coordinates(my_centroid)[1] + 180) / 6) + 1)
  # Define hemisphere for UTM projection.
  # For the nothern hemisphere no information is required, only for the souther hemisphere
  my_hemisphere <- if(sp::coordinates(my_centroid)[2] >= 0){""} else {" +south"}
  # Define character string for the respective UTM projection
  utm_proj <- paste0("+proj=utm +zone=", my_zone, my_hemisphere,
                     " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # Check whether the user_file is either a shapefile or a rasterfile
  my_file_class <- grepl("raster", tolower(class(user_file)))[1]
  # If TRUE, it is a raster file
  if (my_file_class == TRUE){
    final_file <- raster::projectRaster(user_file, crs = utm_proj, method = 'bilinear')
    # If FALSE, we're dealing with a shapefile
  } else {
    # Check if file is a sp or sf object
    if (class(user_file)[1] == "sf"){
      final_file <- sf::st_transform(user_file, utm_proj)
    } else {
      final_file <- sp::spTransform(user_file, CRS=utm_proj)
    }
  }
  return(final_file)
}
