#' Shapefile Dissolve Tool
#'
#' This function automatically dissolves a shapefile based on its feature attributes.
#'
#' @param my_poly Either a SpatialPolygonsDataFrame, SpatialPointsDataFrame or SpatialLinesDataFrame.
#' @param my_col The attribute column after which the shapefile should be dissolved.
#'
#' @return The dissolved SpatialPolygonsDataFrame.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(rgdal)
#' library(rgeos)
#'
#' # Create test polygons from extents
#' my_extent1 <- extent(5,7,8,10)
#' my_extent2 <- extent(7,9,8,10)
#' my_extent3 <- extent(9,11,8,10)
#'
#' # Convert extents into "SpatialPolygons"
#' my_poly1 <- as(my_extent1, 'SpatialPolygons')
#' my_poly2 <- as(my_extent2, 'SpatialPolygons')
#' my_poly3 <- as(my_extent3, 'SpatialPolygons')
#'
#' # Create dataframes
#' data1 <- data.frame(Id=1)
#' data2 <- data.frame(Id=2)
#'
#' # Add the data.drames to the spatial polyongs
#' spdf1 = SpatialPolygonsDataFrame(my_poly1, data1)
#' spdf2 = SpatialPolygonsDataFrame(my_poly2, data1)
#' spdf3 = SpatialPolygonsDataFrame(my_poly3, data2)
#'
#' # Combine all polygons
#' spdf_comb <- rbind(spdf1,spdf2,spdf3)
#'
#' # Dissolve based on Id value
#' spdf_diss <- SHPDissolve(my_poly = spdf_comb, my_col = "Id")
#'
#' plot(spdf_comb)
#' plot(spdf_diss)
#'
#' @export
SHPDissolve <- function(my_poly, my_col){
  # Get colnames of poly
  poly_names <- names(my_poly)
  # Get position of desired column within the names
  col_position <- match(my_col, poly_names)
  # Subset poly to the desired column
  my_poly_subset <- my_poly[,col_position]
  # get unique values from poly
  unique_vals <- unique(my_poly_subset@data)
  # for loop extracting polygons for each unique value and aggregating them
  for (i in 1:nrow(unique_vals)){
    # Define the current value
    current_val <- unique_vals[i,]
    # Create empty polygon based on the input polygon
    # which will be used to add the matching polygons
    current_rbind <- my_poly_subset[0,]
    # Check for every polygon if the current val is set
    for (j in 1:nrow(my_poly_subset)){
      # Get current polygon
      current_poly <- my_poly_subset[j,]
      # Define entry
      present_val <- current_poly@data[1,]
      # Check if present_val matches current_val
      if (present_val == current_val){
        current_rbind <- rbind(current_rbind, current_poly)
      }
    }
    # Dissolve current poly -> This will delete the data.frame
    current_agg <- raster::aggregate(current_rbind)
    # Create a data.frame with a placeholder columname and
    # the current variable as the value
    current_data <- data.frame(x = current_val)
    # Change colname to my_col
    names(current_data) <- my_col
    # Create SpatialPolygonsDataFrame by combining the current
    # aggregated poly with the data.frame
    current_spdf = sp::SpatialPolygonsDataFrame(current_agg, current_data)
    # Combine the aggregated spdfs
    if (i == 1){
      final_spdf <- current_spdf
    } else {
      final_spdf <- rbind(final_spdf, current_spdf)
    }
  }
  return(final_spdf)
}
