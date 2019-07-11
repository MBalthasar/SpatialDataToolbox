#' Fishnet Tool
#'
#' This function creates a fishnet polygon from an input shapefile. The output fishnet will
#' be reprojected to UTM.
#'
#' @param my_poly A sp objetct.
#'
#' @param extent_only (optional) TRUE (default) or FALSE. If the user input file is a polygon,
#'                    the extent_only can be set to FALSE and the ouptut fishnet polygon will
#'                    be cropped to the input file.
#'
#' @param diff_factor Integer. Faktor after which the extent of the input file will be divided
#'                    into x and y direction.
#'
#' @param length_factor Integer. Can be used instead of the diff_factor. Distance in meters of
#'                      one tile n x and y direction.
#'
#' @return A fishnet polygon in UTM projection.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(rgdal)
#' library(rgeos)
#' library(geosphere)
#'
#' # Load sample shapefile including the borders of vietnam
#' vnm_shp <- raster::shapefile(system.file(package = "SpatialDataToolbox", "extdata",
#'                                          "vietnam_borders.shp"))
#'
#' # Apply diff_factor
#' x1 <- FishnetFunction(my_poly = vnm_shp, extent_only = FALSE, diff_factor = 10)
#'
#' # Apply length_factor
#' x2 <- FishnetFunction(my_poly = vnm_shp, extent_only = FALSE, length_factor = 100000)
#'
#' @export
FishnetFunction <- function(my_poly, extent_only, diff_factor, length_factor){
  ##################################################################
  ### Check if user wants to apply both methods at the same time ###
  ##################################################################
  if (missing(length_factor) == F){
    if (missing(diff_factor) == F){
      stop("Choose EITHER a diff_factor OR a length_factor")
    }
  }

  #################################################
  ### Check if extent_only parameter is missing ###
  #################################################
  if (missing(extent_only)){
    extent_only <- TRUE
  } else {
    if (extent_only == FALSE){
      # Check whether a polygon has been provided
      is_poly <- grepl("polygon", tolower(class(my_poly)))
      if (is_poly == FALSE){
        stop("For extend_only == FALSE a polygon must be provided as an input file.")
      }
    }
  }

  #####################################
  ### Check projection of input data ###
  ######################################
  # Get crs string from extent
  current_proj <- as.character(raster::crs(my_poly))
  longlat_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Check if input data is not in UTM projection
  if (substr(current_proj, 0, 9) == "+proj=utm"){
    print(paste0("Data has been provided in UTM projection."))
    my_poly_reproj <- my_poly
    # If Projection is not UTM, data will be reprojected
  } else {
    print(paste0("Data will be reprojected to UTM."))
    # If data is not longlat, first reproject to longlat
    if (substr(current_proj, 0, 13) != "+proj=longlat"){
      # Reproject the my_poly to longlat
      my_poly_reproj <- sp::spTransform(my_poly, CRS=longlat_proj)
      # If data is in longlat, don't do anything
    } else {
      my_poly_reproj <- my_poly
    }
    # Get extent of input poly
    my_extent <- as(raster::extent(my_poly), 'SpatialPolygons')
    sp::proj4string(my_extent) <- sp::CRS(as.character(raster::crs(my_poly)))
    # Get centroid of extent
    my_centroid <- geosphere::centroid(my_extent)
    # Define zone and hemisphere based on longlat information for UTM projection
    my_zone <- as.character(floor((sp::coordinates(my_centroid)[1] + 180) / 6) + 1)
    my_hemisphere <- if(sp::coordinates(my_centroid)[2] >= 0){""} else {" +south"}
    # Define character string for the respective UTM projection
    my_utm_proj <- paste0("+proj=utm +zone=",
                          my_zone,
                          my_hemisphere,
                          " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    # Reproject the user_file to UTM
    my_poly_reproj <- sp::spTransform(my_poly_reproj, CRS=my_utm_proj)
  }

  ##################################
  ### Apply diff_factor approach ###
  ##################################
  if (missing(length_factor)){
    print(paste0("Apply diff_factor of ", diff_factor,"."))
    # Define the extent
    my_ext <- raster::extent(my_poly_reproj)
    # Caclulate horizontal and vertical difference
    x_diff <- my_ext[2] - my_ext[1]
    y_diff <- my_ext[4] - my_ext[3]
    # Divide x_diff and y_diff by the diff_factor to get distance of chunks in each direction
    x_diff_chunk <- x_diff/diff_factor
    y_diff_chunk <- y_diff/diff_factor
    # Define vector with letters for naming the polygons
    my_letters <- LETTERS
    # Start with for-loop to create a polygon out of each x-y chunk
    for (i in 1:diff_factor){
      # First define y start and end
      # If i == 1 use the max y as a start and subtract one y_diff_chunk as end
      if (i == 1){
        y_start <- my_ext[4]
        y_end <- my_ext[4] - y_diff_chunk
        # If i != 1 use the max y - (i-1) * the y_diff_chunk as a start and subtract i*y_diff_chunk as end
      } else {
        y_start <- my_ext[4] - (i-1)*y_diff_chunk
        y_end <- my_ext[4] - i*y_diff_chunk
      }
      # Now define x start and end
      for (j in 1:diff_factor){
        # If j == 1 use the x min as a start and add one x_diff_chunk as end
        if (j == 1){
          x_start <- my_ext[1]
          x_end <- my_ext[1] + x_diff_chunk
          # If j != 1 use the min x - (j-1) * the x_diff_chunk as a start and add j*x_diff_chunk as end
        } else {
          x_start <- my_ext[1] + (j-1)*x_diff_chunk
          x_end <- my_ext[1] + j*x_diff_chunk
        }
        # Create extent from coordinates
        # Choose this order for starting in the upper left corner
        ext <- raster::extent(x_start,x_end,y_end,y_start)
        # Convert coordinates to a SpatialPolygons object
        sp <- as(ext, 'SpatialPolygons')
        # Create SpatialPolygonsDataFrame from SpatialPolygons object
        # Define the number of polygon
        current_number <- paste0(my_letters[i],j)
        # Use information about current polygon number and create a "SpatialPolygonsDataFrame"
        data = data.frame(Id=current_number)
        spdf = sp::SpatialPolygonsDataFrame(sp,data)
        # Combine single polygons into a "row"
        if (j == 1){
          final_row <- spdf
        } else {
          final_row <- rbind(final_row, spdf)
        }
      }
      # Combine all "rows" to have the final polygon
      if (i == 1){
        final_polygon <- final_row
      } else {
        final_polygon <- rbind(final_polygon, final_row)
      }
    }
    # Assign original projection to polygon
    sp::proj4string(final_polygon) <- sp::CRS(my_utm_proj)

    ####################################
    ### Apply length_factor approach ###
    ####################################
  } else {
    print(paste0("Apply length_factor of ", length_factor, " m."))
    # Define the extent
    my_ext <- raster::extent(my_poly_reproj)
    # Caclulate horizontal and vertical difference
    x_diff <- my_ext[2] - my_ext[1]
    y_diff <- my_ext[4] - my_ext[3]
    # Divide x_diff and y_diff by the length_factor to get the number of tiles in each direction
    x_number <- x_diff/length_factor
    y_number <- y_diff/length_factor
    # use ceiling round up the number of tiles in each direction
    x_number_round <- ceiling(x_number)
    y_number_round <- ceiling(y_number)
    # Calculate x and y tile length of last tile in each direction
    x_tail <- x_diff - floor(x_number) * length_factor
    y_tail <- y_diff - floor(y_number) * length_factor
    # If x_tail and/or y_tail is 0, use the length_factor instead
    if (x_tail == 0){
      x_tail = length_factor
    }
    if (y_tail == 0){
      y_tail = length_factor
    }
    # Define vector with letters for naming the polygons
    my_letters <- LETTERS
    # Start for loop creating polygons for each tile
    for (i in 1:y_number_round){
      # First define y start and end
      # If i == 1 use the max y as a start and subtract the length_factor as end
      if (i == 1){
        y_start <- my_ext[4]
        y_end <- my_ext[4] - length_factor
        # If the last tile is processed, use ymin as the end and ymin + y_tail as start
      } else if (i == y_number_round) {
        y_start <- my_ext[3] + y_tail
        y_end <- my_ext[3]
        # If i != 1 or != y_number_round: use the max y - (i-1) * the length_factor as a start
        # and subtract i*length_factor as end
      } else {
        y_start <- my_ext[4] - (i-1)*length_factor
        y_end <- my_ext[4] - i*length_factor
      }
      # Now define x start and end
      for (j in 1:x_number_round){
        # If j == 1 use the x min as a start and add length_factor as end
        if (j == 1){
          x_start <- my_ext[1]
          x_end <- my_ext[1] + length_factor
          # If the last tile is processed, use xmax as the end and xmax - x_tail as start
        } else if (j == x_number_round){
          x_start <- my_ext[2] - x_tail
          x_end <- my_ext[2]
          # If j != 1 or != x_number_round: use the min x - (j-1) * the length_factor as a start
          # and add j*length_factor as end
        } else {
          x_start <- my_ext[1] + (j-1)*length_factor
          x_end <- my_ext[1] + j*length_factor
        }
        # Create extent from coordinates
        # Choose this order for starting in the upper left corner
        ext <- raster::extent(x_start,x_end,y_end,y_start)
        # Convert coordinates to a SpatialPolygons object
        sp <- as(ext, 'SpatialPolygons')
        # Create SpatialPolygonsDataFrame from SpatialPolygons object
        # Define the number of polygon
        current_number <- paste0(my_letters[i],j)
        # Use information about current polygon number and create a "SpatialPolygonsDataFrame"
        data = data.frame(Id=current_number)
        spdf = sp::SpatialPolygonsDataFrame(sp,data)
        # Combine single polygons into a "row"
        if (j == 1){
          final_row <- spdf
        } else {
          final_row <- rbind(final_row, spdf)
        }
      }
      # Combine all "rows" to have the final polygon
      if (i == 1){
        final_polygon <- final_row
      } else {
        final_polygon <- rbind(final_polygon, final_row)
      }
    }
    # Assign original projection to polygon
    sp::proj4string(final_polygon) <- sp::CRS(my_utm_proj)
  }

  ##############################
  ### Return fishnet polygon ###
  ##############################
  # Check if extent_only is TRUE
  if (extent_only == TRUE){
    return(final_polygon)
    # Else crop the fishent polygon by the input file
  } else {
    print(paste0("Cropping the fishnet polygon to the input file."))
    final_polygon <- raster::crop(final_polygon, my_poly_reproj)
    return(final_polygon)
  }
}
