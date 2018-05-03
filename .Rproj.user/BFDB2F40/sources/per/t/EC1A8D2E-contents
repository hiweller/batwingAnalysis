# countColorInDirectory: wrapper for countColor, applies to every picture in a folder and returns a dataframe with % cover by image 
#
# countColor: count the number of pixels within a target range and return fraction with diagnostic plot, which is either printed to the plot window or saved in a target folder
#
# countMultipleColors: get a couple of target ranges?
# 
# path = path to image you want to calculate color fraction for
#
# lower = lower bounds for target color (0 to 1 range)
#
# upper = upper bounds for target color (0 to 1 range)
#
# bg.lower and bg.upper = same as upper and lower for regular colordistance functions
#
# target.color = a color name or RGB triplet to mask pixels in the specified color range
#
# plotting = logical. Should masked image be plotted in the plot window?
#
# save.image = logical OR path for saving combined image. If TRUE, saves image to the
# same directory as the original image as 'originalimagename_masked.png'; if a
# path is provided, saves it to that directory/name instead.
#
# returns: saved image (if save.image != FALSE), fraction of image inside of color range
countColor <- function(path, lower, upper, 
                       bg.lower=rep(0.8, 3), bg.upper=rep(1, 3),
                       target.color="magenta", plotting=F, save.image=FALSE) {
  
  # read in image, ignoring background if provided
  # doesn't necessarily matter for calculating number of pixels though
  img <- colordistance::loadImage(path, lower=bg.lower, upper=bg.upper)
  original <- img$original.rgb
  
  # find all pixels within target range + get their locations
  idx <- which((lower[1]<=original[, , 1] & original[, , 1]<=upper[1]) & 
                 (lower[2]<=original[, , 2] & original[, , 2]<=upper[2]) &
                 (lower[3]<=original[, , 3] & original[, , 3]<=upper[3]), arr.ind=T)
  
  # make sure target color is an RGB triplet
  if (is.character(target.color)) {
    target.color <- as.vector(col2rgb(target.color)/255)
  }
  
  if (length(target.color) != 3) {
    stop("'target.color' must be a numeric vector of length 3 with values between 0 and 1")
  } else if (range(target.color)[2] > 1) {
    target.color <- target.color/255
  }
  
  # change all target pixels to target color to check mapping
  masked <- original
  for (i in 1:nrow(idx)) {
    masked[idx[i,1], idx[i,2], ] <- target.color
  }
  
  # if plotting is on, plot masked image
  if (plotting) {
    op <- par(mar=rep(0,4))
    asp <- dim(masked)[1]/dim(masked)[2]
    main <- tail(strsplit(path, split="[.]")[[1]], 1)
    plot(0:1, 0:1, type="n", ann=F, axes=F, asp=asp, main=main)
    graphics::rasterImage(masked, 0, 0, 1, 1)
    par(op)
  }
  
  # if saving is on but no path was specified, set it to same directory as original image
  if (isTRUE(save.image)) {
    destination <- paste(tools::file_path_sans_ext(path), "_masked.png", sep="")
  } else if (is.character(save.image)) {
    if (dir.exists(save.image)) {
      destination <- paste(save.image, tools::file_path_sans_ext(path), "_masked.png", sep="")
    } else {
      destination <- save.image
    }
  }
  
  # if destination specified (save.image was either TRUE or a path), save a
  # comparison image with original and masked images
  if (exists("destination")) {
    output <- array(data=NA, dim=c(dim(original)[1]*2, dim(original)[2:3]))
    output[1:dim(original)[1],,] <- original
    output[(dim(original)[1]+1):dim(output)[1], , ] <- masked
    png::writePNG(output, target=destination, dpi = 30)
    message(paste("Output image written to", destination))
  }
  
  # calculate area
  area <- nrow(idx)/dim(img$filtered.rgb.2d)[1]
  
  return(area)
}