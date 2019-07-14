#' Tiles Maker Tool
#'
#' This function divides the input file in tiles based on user preferences.
#' The output data will be reprojected to UTM.
#'
#' @param user_file A raster or sp objetct.
#'
#' @param user_aoi (optional) A sp object. Will be used to crop input data and create tiles.
#'
#' @param diff_factor Integer. Faktor after which the extent of the input file will be divided
#'                    into x and y direction.
#'
#' @param length_factor Vector of two integer. Can be used instead of the diff_factor.
#'                      Distance in meters of one tile n x and y direction.
#'
#' @param out_folder Character string. Path to which the output tiles and fishnet polygon
#'                   should be written.
#'
#' @return A fishnet polygon in UTM projection.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(rgdal)
#' library(geosphere)
#'
#' # Load sample raster file
#' my_raster <- raster::brick(system.file(package = "SpatialDataToolbox", "extdata", "landsat_sample.tif"))
#'
#' # Apply diff_factor
#' x1 <- TilesMaker(user_file = my_raster, diff_factor = 5, out_folder = "./")
#'
#' # Delete created folder with tiles and fishnet polygon
#' unlink("./Fishnet", recursive = TRUE)
#'
#' # Apply length_factor
#' x2 <- TilesMaker(user_file = my_raster, length_factor = c(10000,10000), out_folder = "./")
#'
#'
#' @export
TilesMaker <- function(user_file, user_aoi, diff_factor, length_factor, out_folder){

  ##################################################################
  ### Check if user wants to apply both methods at the same time ###
  ##################################################################
  if (missing(length_factor) == F){
    if (missing(diff_factor) == F){
      stop("Choose EITHER a diff_factor OR a length_factor")
    }
  }

  ##########################################
  ### Check and prepare output directory ###
  ##########################################
  # First check the ending of the given out_folder
  last_char <- substr(out_folder, nchar(out_folder), nchar(out_folder))
  # if "/" is missing, add one and also add "Tiles" as the output folder
  if (last_char == "/"){
    output <- paste0(out_folder, "Fishnet")
  } else {
    output <- paste0(out_folder, "/Fishnet")
  }
  # Check if directory is already existing
  dir_check <- dir.exists(output)
  if (dir_check == T){
    stop("There is already a folder named Fishnet in the output directory")
  }
  # Create a folder for the data
  dir.create(output)

  ######################################
  ### Check users preferences on aoi ###
  ######################################
  # Define the extent by checking whether a user_aoi is defined or not
  # If no user_aoi is defined, use the extent of the user_file and convert it to a "SpatialPolygons"
  if (missing(user_aoi)){
    print(paste0("No user_aoi defined. Using extent of user_file instead."))
    my_aoi <- as(extent(user_file), 'SpatialPolygons')
    proj4string(my_aoi) <- sp::CRS(as.character(crs(user_file)))
    # Since the extent of the user file is being used, no croping is needed
    my_file <- user_file
  } else {
    # Check if user_aoi has same projection as user_file
    proj_check <- identical(crs(user_file), crs(user_aoi))
    if (proj_check == F){
      stop("Projections of user_file and user_aoi are not identical.")
    }
    my_aoi <- user_aoi
    # Since a seperate aoi is used, the user_file will be cropped to the aoi
    print(paste0("Cropping user_file to user_aoi."))
    my_file <- raster::crop(user_file, my_aoi)
  }

  #############################################
  ### Check the data type of the input data ###
  #############################################
  # Check whether the user_file is either a shapefile or a rasterfile
  my_file_class <- grepl("raster", tolower(class(my_file)))
  # Check if input data is a raster or shapefile
  if (my_file_class == TRUE){
    data_type <- "raster"
  } else {
    data_type <- "polygon"
  }

  ######################################
  ### Check projection of input data ###
  ######################################
  # Get crs string from extent
  current_proj <- as.character(crs(my_aoi))
  longlat_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Check if input data is not in UTM projection
  if (substr(current_proj, 0, 9) == "+proj=utm"){
    print(paste0("Data has been provided in UTM projection."))
    # If Projection is not UTM, data will be reprojected
  } else {
    print(paste0("Data will be reprojected to UTM."))
    # If data is not longlat, first reproject to longlat
    if (substr(current_proj, 0, 13) != "+proj=longlat"){
      # Reproject the user_file to longlat
      # Depending on data type use different reprojection functions for the user_file
      if (data_type == "raster"){
        my_file <- raster::projectRaster(my_file, crs = longlat_proj, method = 'bilinear')
      } else {
        my_file <- sp::spTransform(my_file, CRS=longlat_proj)
      }
      # Reproject my_aoi to longlat
      my_aoi <- sp::spTransform(my_aoi, CRS=longlat_proj)
    }
    # Get centroid of extent
    my_centroid <- geosphere::centroid(my_aoi)
    # Define zone and hemisphere based on longlat information for UTM projection
    my_zone <- as.character(floor((coordinates(my_centroid)[1] + 180) / 6) + 1)
    my_hemisphere <- if(sp::coordinates(my_centroid)[2] >= 0){""} else {" +south"}
    # Define character string for the respective UTM projection
    my_utm_proj <- paste0("+proj=utm +zone=",
                          my_zone,
                          my_hemisphere,
                          " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    # Reproject the user_file to UTM
    # Depending on data type use different reprojection functions for the user_file
    if (data_type == "raster"){
      my_file <- raster::projectRaster(my_file, crs = my_utm_proj, method = 'bilinear')
    } else {
      my_file <- sp::spTransform(my_file, CRS=my_utm_proj)
    }
    # Reproject my_aoi to UTM
    my_aoi <- sp::spTransform(my_aoi, CRS=my_utm_proj)
  }

  ##################################
  ### Apply diff_factor approach ###
  ##################################
  if (missing(length_factor)){
    print(paste0("Apply diff_factor of ", diff_factor,"."))
    # Define the extent
    my_ext <- raster::extent(my_aoi)
    # Caclulate horizontal and vertical difference
    x_diff <- my_ext[2] - my_ext[1]
    y_diff <- my_ext[4] - my_ext[3]
    # Divide x_diff and y_diff by the diff_factor to get distance of chunks in each direction
    x_diff_chunk <- x_diff/diff_factor
    y_diff_chunk <- y_diff/diff_factor
    # Define vector with letters for naming the polygons
    my_letters <- LETTERS
    # Start with for-loop to create a polygon for out of each x-y chunk
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
    sp::proj4string(final_polygon) <- sp::CRS(as.character(crs(my_aoi)))

    ####################################
    ### Apply length_factor approach ###
    ####################################
  } else {
    print(paste0("Apply length_factor of x = ", length_factor[1], " m and y = ", length_factor[2], " m."))
    # Define the extent
    my_ext <- raster::extent(my_aoi)
    # Caclulate horizontal and vertical difference
    x_diff <- my_ext[2] - my_ext[1]
    y_diff <- my_ext[4] - my_ext[3]
    # Divide x_diff and y_diff by the length_factor to get the number of tiles in each direction
    x_number <- x_diff/length_factor[1]
    y_number <- y_diff/length_factor[2]
    # use ceiling round up the number of tiles in each direction
    x_number_round <- ceiling(x_number)
    y_number_round <- ceiling(y_number)
    # Calculate x and y tile length of last tile in each direction
    x_tail <- x_diff - floor(x_number) * length_factor[1]
    y_tail <- y_diff - floor(y_number) * length_factor[2]
    # If x_tail and/or y_tail is 0, use the length_factor instead
    if (x_tail == 0){
      x_tail = length_factor[1]
    }
    if (y_tail == 0){
      y_tail = length_factor[2]
    }
    # Define vector with letters for naming the polygons
    my_letters <- LETTERS
    # Start for loop creating polygons for each tile
    for (i in 1:y_number_round){
      # First define y start and end
      # If i == 1 use the max y as a start and subtract the length_factor as end
      if (i == 1){
        y_start <- my_ext[4]
        y_end <- my_ext[4] - length_factor[2]
        # If the last tile is processed, use ymin as the end and ymin + y_tail as start
      } else if (i == y_number_round) {
        y_start <- my_ext[3] + y_tail
        y_end <- my_ext[3]
        # If i != 1 or != y_number_round: use the max y - (i-1) * the length_factor as a start
        # and subtract i*length_factor as end
      } else {
        y_start <- my_ext[4] - (i-1)*length_factor[2]
        y_end <- my_ext[4] - i*length_factor[2]
      }
      # Now define x start and end
      for (j in 1:x_number_round){
        # If j == 1 use the x min as a start and add length_factor as end
        if (j == 1){
          x_start <- my_ext[1]
          x_end <- my_ext[1] + length_factor[1]
          # If the last tile is processed, use xmax as the end and xmax - x_tail as start
        } else if (j == x_number_round){
          x_start <- my_ext[2] - x_tail
          x_end <- my_ext[2]
          # If j != 1 or != x_number_round: use the min x - (j-1) * the length_factor as a start
          # and add j*length_factor as end
        } else {
          x_start <- my_ext[1] + (j-1)*length_factor[1]
          x_end <- my_ext[1] + j*length_factor[1]
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
    sp::proj4string(final_polygon) <- sp::CRS(as.character(crs(my_aoi)))
  }

  #################################
  ### Crop data to each polygon ###
  #################################
  # Create folder for the tiles
  output_tiles <- paste0(output, "/Tiles")
  dir.create(output_tiles)
  # In case of polygons, create variable that counts the number of empty tiles which will be omitted
  empty_tiles <- c()
  # Start for loop using each polygon for cropping
  for (i in 1:nrow(final_polygon)){
    print(paste0("Cropping user_file into subset ", i, " of ", nrow(final_polygon)))
    current_poly <- final_polygon[i,]
    current_file <- raster::crop(my_file, current_poly)
    current_name <- as.character(current_poly$Id)
    # Save file depending on data type
    if (data_type == "raster"){
      raster::writeRaster(current_file, filename=paste0(output_tiles, "/", current_name, ".tif"), format="GTiff")
      # Before writing polygon check if it is emtpy
    } else if (is.null(current_file) != T){
      # If tile is not empty, write new shapefile
      rgdal::writeOGR(obj=current_file, dsn=output_tiles, layer=current_name, driver="ESRI Shapefile")
    } else if (is.null(current_file) == T){
      # If tile is empty, add on the the empty tile counter
      empty_tiles <- c(empty_tiles, current_name)
    }
  }
  # If number of empty (omitted) tiles is > 0, print number of omitted tiles
  if (length(empty_tiles) > 0){
    print(paste0(length(empty_tiles), " tiles didn't contain any geoinformation and were therefore omitted"))
    # Remove empty tiles from fishnet polygon
    for (i in 1:length(empty_tiles)){
      current_empty_tile <- empty_tiles[i]
      final_polygon <- final_polygon[final_polygon$Id != current_empty_tile,]
    }
  }

  # Write fishnet polygon into a new folder
  output_fishnet <- paste0(output, "/Fishnet_poly")
  dir.create(output_fishnet)
  rgdal::writeOGR(obj=final_polygon, dsn=output_fishnet, layer="Fishnet_poly", driver="ESRI Shapefile")

  print(paste0("Done."))
  # Return the fishnet shapefile
  return(final_polygon)
}
