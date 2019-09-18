#' Map Booklet Maker
#'
#' This function uses a fishnet polygon to create a map booklet pdf based on a ggplot.
#' The booklet contains an overview map with the original ggplot and the fishnet polygon
#' added on top, as well as sub-maps for each tile of the fishnet polygon.
#'
#' @param user_ggplot A gg object. Input plot which will be used as a basis for the booklet.
#'
#' @param user_fishnet A sp object. For each tile of the fishnet polygon a submap will be created.
#'                     See \code{\link{FishnetFunction}}.
#'
#' @param fishnet_col (optional) Colour of the fishnet polygon within the overview map
#'
#' @param user_width (optional) Width of the output pdf. Default is 7.
#'
#' @param user_height (optional) Height of the output pdf. Default is 7.
#'
#' @param user_pointsize (optional) The pointsize to be used. Default is 12.
#'
#' @param user_buffer (optional) Buffer around each tile in m. Default is 0.
#'
#' @param add_north (optional) TRUE or FALSE. Adds north arrow.
#'
#' @param add_scale (optional) TRUE or FALSE. Adds scalebar.
#'
#' @param out_dir A character string. The directory to which the output pdf should be written.
#'
#' @return A gg object containing the original ggplot with the fishnet polygon added on top.
#'
#' @examples
#'
#' library(sp)
#' library(raster)
#' library(ggplot2)
#' library(ggsn)
#' library(geosphere)
#' library(RStoolbox)
#' library(pdftools)
#' library(magick)
#'
#' # Load sample srtm and hillshade
#' my_srtm <- raster::brick(system.file(package = "SpatialDataToolbox",
#'                                      "extdata", "srtm_sample.tif"))
#' my_hill <- raster::brick(system.file(package = "SpatialDataToolbox",
#'                                      "extdata", "hillshade_sample.tif"))
#'
#' # Use ggR to plot raster files
#' my_plot <- ggR(my_hill, # Add hillshade
#'                # maxpixels = my_hill@ncols*my_hill@nrows # Use full resolution
#' ) +
#'   ggtitle("SRTM with hillshade") + # Add title
#'   ggR(my_srtm, geom_raster = TRUE, ggLayer = TRUE, alpha = 0.5, # Add srtm with alpha = 0.5
#'       # maxpixels = my_srtm@ncols*my_srtm@nrows # Use full resolution
#'   ) +
#'   scale_fill_gradientn(colours = terrain.colors(100), # Choose colors for srtm
#'                        name = "Elevation\nin m") + # Choose name for legend
#'   theme(plot.title = element_text(hjust = 0.5, face="bold", size=14), # Adjust position + font of title
#'         axis.text.y = element_text(angle = 90, hjust = 0.5)) + # Vertical y axis labels
#'   xlab("") + # Remove x lab
#'   ylab("") # Remove y lab
#'
#' # Plot map
#' my_plot
#'
#' # Create fishnet polygon from raster extent
#' my_extent <- as(raster::extent(my_srtm), 'SpatialPolygons')
#' proj4string(my_extent) <- sp::CRS(as.character(raster::crs(my_srtm)))
#' my_fishnet <- FishnetFunction(my_poly = my_extent, extent_only = TRUE, diff_factor = 4)
#'
#' # Execute with all parameters defined.
#' # In orer to have a landscape DinA4 format, multiply the width by 1.414286.
#' my_booklet <- BookletMaker(user_ggplot = my_plot,
#'                            user_fishnet = my_fishnet,
#'                            fishnet_col = "black",
#'                            user_width  = 7 * 1.414286,
#'                            user_height = 7,
#'                            user_pointsize = NULL,
#'                            out_dir = "./",
#'                            user_buffer = 0,
#'                            add_scale = TRUE,
#'                            add_north = TRUE)
#'
#' @export
BookletMaker <- function(user_ggplot, user_fishnet, fishnet_col, user_width,
                         user_height, user_pointsize, user_buffer, out_dir,
                         add_north, add_scale){
  ###########################################
  ### Check for missing output parameters ###
  ###########################################
  # For every missing output parameter set the default
  if (missing(user_width)){
    user_width <- NULL
  }
  if (missing(user_height)){
    user_height <- NULL
  }
  if (missing(user_pointsize)){
    user_pointsize <- NULL
  }
  if (missing(user_buffer)){
    user_buffer <- 0
  }
  if (missing(fishnet_col)){
    fishnet_col <- "black"
  }
  if (missing(add_scale)){
    add_scale <- FALSE
  } else if (add_scale == T){
    # Check projection of data
    user_proj <- crs(user_fishnet)
    # if projection is not in UTM, stop function
    if (substr(user_proj, 1,9) != "+proj=utm"){
      stop("Data must be in UTM projection for adding a scalebar")
    }
  }
  if (missing(add_north)){
    add_north <- FALSE
  }

  ########################################
  ### Create temp folder for the tiles ###
  ########################################
  # use out_dir as parent directory
  temp_folder <- paste0(out_dir, "/temp")
  # Check if directory is already existing
  dir_check <- dir.exists(temp_folder)
  if (dir_check == T){
    stop("There is already a folder named temp in the output directory")
  }
  dir.create(temp_folder)
  # Also create second temp folder for png
  temp_folder2 <- paste0(temp_folder, "/PNG")
  dir.create(temp_folder2)
  # Also create third temp folder for pdf
  temp_folder3 <- paste0(temp_folder, "/PDF")
  dir.create(temp_folder3)

  #######################
  ### Fishnet Polygon ###
  #######################
  # Calculate centroid for each polygon
  for (i in 1:nrow(user_fishnet)){
    current_poly <- user_fishnet[i,]
    current_centroid <- geosphere::centroid(current_poly)
    if (i == 1){
      aoi_centroids <- current_centroid
    } else {
      aoi_centroids <- rbind(aoi_centroids, current_centroid)
    }
    rm(i, current_poly, current_centroid)
  }
  # Create data.frame from centroids and add ID
  aoi_centroids_df <- data.frame(x = aoi_centroids[,1], y = aoi_centroids[,2], Id = user_fishnet$Id)
  # Convert SpatialPolygonsDataFrame into data.frame
  suppressMessages(
    fishnet_df <- ggplot2::fortify(user_fishnet)
  )

  ##################
  ### Cover Plot ###
  ##################
  print(paste0("Working on the overview map"))
  # Add fishnet polygon to user_ggplot
  cover_plot <- user_ggplot +
    geom_polygon(data = fishnet_df, # Define data for polygon
                 aes(x = long, y = lat, group=group), # Define aesthetics for polygon
                 colour = fishnet_col, fill = NA) # Choose line colour and fill with NA
  # Add annotations to srtm_fishnet
  for (i in 1:nrow(aoi_centroids_df)){
    cover_plot <- cover_plot + annotate(geom="text", # Define text as annotation type
                                        x=aoi_centroids_df[i,1], # Choose longitude
                                        y=aoi_centroids_df[i,2], # Choose latitude
                                        label=aoi_centroids_df[i,3], # Choose Id
                                        colour = fishnet_col) # Define colour
    rm(i)
  }
  # Add north arrow
  if (add_north == T){
    cover_plot <- cover_plot +
      ggsn::north(fishnet_df) # Use extent of fishnet polygon for north arrow
  }
  # Define paramters for scalebar
  if (add_scale == T){
    # Get horizontal distance of plot
    scale_dist_cover <- raster::extent(user_fishnet)[2] - raster::extent(user_fishnet)[1]
    # Get 1/8 of the horizontal distance
    scale_dist_cover <- scale_dist_cover/8
    # Convert m to km
    scale_dist_cover <- scale_dist_cover/1000
    # Round number
    scale_dist_cover <- round(scale_dist_cover)
    # Add scalebar to current plot
    cover_plot <- cover_plot +
      ggsn::scalebar(fishnet_df, # Use extent of fishnet polygon for scalebar
                     dist = scale_dist_cover, dist_unit = "km", # Define segment distance of scalebar
                     transform = FALSE, # Define decimal degrees
                     height = 0.02) # Define height of scalebar
  }
  # Save Cover Map as PNG
  grDevices::png(paste0(temp_folder2, "/1_Cover.png"),
                 width = user_width, height = user_height, pointsize = user_pointsize,
                 units = "in", res = 300)
  print(cover_plot)
  grDevices::dev.off()

  #############
  ### Tiles ###
  #############
  # Write pdf for every tile
  for (i in 1:nrow(user_fishnet)){
    print(paste0("Working on tile ", i, " of ", nrow(user_fishnet)))
    # Define current tile
    current_tile <- user_fishnet[i,]
    # Turn current tile into data.frame
    suppressMessages(
      current_tile_df <- fortify(current_tile)
    )
    # Define x and y limits by looking at he extent of the tile and add buffer
    current_xlim <- c(raster::extent(user_fishnet[i,])[1] - user_buffer,
                      raster::extent(user_fishnet[i,])[2] + user_buffer)
    current_ylim <- c(raster::extent(user_fishnet[i,])[3] - user_buffer,
                      raster::extent(user_fishnet[i,])[4] + user_buffer)
    current_id <- user_fishnet[i,]$Id
    # Add zoom information to plot
    suppressMessages(
      current_plot <- user_ggplot +
        ggtitle(paste0("Tile ", current_id)) + # Change title
        theme(legend.position="none") + # Remove legend
        coord_fixed(xlim = c(current_xlim[1], current_xlim[2]), # Change x limits
                    ylim = c(current_ylim[1], current_ylim[2])) # Change y limits
    )
    # Add tile outlines
    current_plot <- current_plot +
      geom_polygon(data = current_tile_df, # Define data for polygon
                   aes(x = long, y = lat, group=group), # Define aesthetics for polygon
                   colour = fishnet_col, fill = NA) # Choose line colour and fill with NA
    # Add north arrow to current plot
    if (add_north == T){
      current_plot <- current_plot +
        ggsn::north(current_tile_df) # Use extent of fishnet polygon for north arrow
    }
    # Define paramters for scalebar
    if (add_scale == T){
      # Get horizontal distance of plot
      scale_dist_cover <- raster::extent(current_tile)[2] - raster::extent(current_tile)[1]
      # Get 1/8 of the horizontal distance
      scale_dist_cover <- scale_dist_cover/8
      # Convert m to km
      scale_dist_cover <- scale_dist_cover/1000
      # Round number
      scale_dist_cover <- round(scale_dist_cover)
      # Add scalebar to plot
      current_plot <- current_plot +
        ggsn::scalebar(current_tile_df, # Use extent of current tile for scalebar
                       dist = scale_dist_cover, dist_unit = "km", # Define segment distance of scalebar
                       transform = FALSE, # Define decimal degrees
                       height = 0.02, # Define height of scalebar
                       y.min = y_min_cover)
    }
    # Save current tile as png
    grDevices::png(paste0(temp_folder2,"/", current_id,".png"),
                   width = user_width, height = user_height, pointsize = user_pointsize,
                   units = "in", res = 300)
    print(current_plot)
    grDevices::dev.off()
    # Remove redundant variables
    rm(current_tile, current_xlim, current_ylim, current_id, current_plot)
  }

  #################################################
  ### Join the individual pdf into one pdf file ###
  #################################################
  print(paste0("Combining the individual plots into one map booklet"))
  # List paths of the overview image and all tiles
  temp = list.files(path = temp_folder2, pattern="*.png$", full.names = T)
  # Define vector with names for cover plot and tiles
  file_names <- c("1_Cover", as.character(user_fishnet$Id))
  # Convert all png files to pdf
  for (i in 1:length(temp)){
    current_file_name <- file_names[i]
    # Load current png
    current_png <- magick::image_read(temp[i])
    # Convert current png to pdf
    current_pdf <- magick::image_convert(current_png, "pdf")
    # Write pdf
    magick::image_write(current_pdf, path = paste0(temp_folder3, "/", current_file_name, ".pdf"),
                        format = "pdf")
  }
  # List paths of the overview image and all tiles as pdf
  temp2 = list.files(path = temp_folder3, pattern="*.pdf$", full.names = T)
  # Join the individual pdf files into one file
  suppressMessages(
    pdftools::pdf_combine(input = temp2, output = paste0(out_dir, "Booklet.pdf"))
  )
  # Remove the temp folder with its content
  unlink(temp_folder, recursive = TRUE)

  return(cover_plot)
}
