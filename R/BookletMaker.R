#' Map Booklet Maker
#'
#' This function uses a fishnet polygon to create a map booklet pdf based on a ggplot.
#' The booklet contains a overview map with the original ggplot and the fishnet polygon
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
#' @param out_dir A character string. The directory to which the output pdf should be written.
#'
#' @return A gg object containing the original ggplot with the fishnet polygon added on top.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(ggplot2)
#' library(geosphere)
#' library(RStoolbox)
#' library(pdftools)
#'
#' # Load sample raster file
#' my_raster <- raster::brick(system.file(package = "SpatialDataToolbox",
#'                            "extdata", "landsat_sample.tif"))
#'
#' # Create fishnet polygon from extent
#' my_extent <- as(raster::extent(my_raster), 'SpatialPolygons')
#' proj4string(my_extent) <- sp::CRS(as.character(raster::crs(my_raster)))
#' my_fishnet <- FishnetFunction(my_poly = my_extent, extent_only = TRUE, diff_factor = 4)
#'
#' # Use ggRGB to make RGB plot of raster
#' my_plot <- RStoolbox::ggRGB(my_raster, r = 3, g = 2, b = 1, stretch = "lin") +
#'   ggtitle("Landsat-5 RGB") + # Add title
#'   theme(plot.title = element_text(hjust = 0.5, face="bold", size=14), # Adjust title
#'         axis.text.y = element_text(angle = 90, # Vertical y axis labels
#'                                    hjust = 0.5)) + # Adjust position of axis labels
#'   scale_x_continuous(expand = c(0,0)) + # Remove expansion in x direction
#'   scale_y_continuous(expand = c(0,0)) + # Remove expansion in y direction
#'   xlab("") + # Edit x lab
#'   ylab("") # Edit y lab
#'
#' # Plot the ggplot
#' my_plot
#'
#' # Execute with all parameters defined.
#' # In orer to have a landscape DinA4 format, multiply the width by 1.414286.
#' test_complex <- BookletMaker(user_ggplot = my_plot,
#'                              user_fishnet = my_fishnet,
#'                              fishnet_col <- "red",
#'                              user_width  = 7 * 1.414286,
#'                              user_height = 7,
#'                              user_pointsize = NULL,
#'                              out_dir = "./",
#'                              user_buffer = 0)
#'
#' @export
BookletMaker <- function(user_ggplot, user_fishnet, fishnet_col, user_width,
                         user_height, user_pointsize, user_buffer, out_dir){
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
  # Save Cover Map
  grDevices::pdf(paste0(temp_folder, "/1_Cover.pdf"),
      width = user_width, height = user_height, pointsize = user_pointsize)
  print(cover_plot)
  grDevices::dev.off()

  #############
  ### Tiles ###
  #############
  # Write pdf for every tile
  for (i in 1:nrow(user_fishnet)){
    print(paste0("Working on tile ", i, " of ", nrow(user_fishnet)))
    current_tile <- user_fishnet[i,]
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
    # Safe current tile
    grDevices::pdf(paste0(temp_folder,"/", current_id,".pdf"),
        width = user_width, height = user_height, pointsize = user_pointsize)
    print(current_plot)
    grDevices::dev.off()
    # Remove redundant variables
    rm(current_tile, current_xlim, current_ylim, current_id, current_plot)
  }

  #################################################
  ### Join the individual pdf into one pdf file ###
  #################################################
  # List paths of the overview image and all tiles
  temp = list.files(path = temp_folder, pattern="*.pdf$", full.names = T)
  # Join the individual pdf files into one file
  pdftools::pdf_combine(input = temp, output = paste0(out_dir, "Booklet.pdf"))
  # Remove the temp folder with its content
  unlink(temp_folder, recursive = TRUE)

  return(cover_plot)
}
