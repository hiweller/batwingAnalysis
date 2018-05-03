path <- "Wing_images/DSC_1678.jpeg"
img <- colordistance::loadImage(path, lower=rep(0, 3), upper=rep(0.25, 3), hsv=FALSE)
original <- img$original.rgb
plotArrayAsImage(original)

upper <- c(255, 100, 255)/255
lower <- c(140, 0, 140)/255
pixel.array <- img$original.rgb

center <- c(200, 70, 200)/255
radius <- 0.1

# rectangularRange: find pixels within a target range defined by ranges in each channel
rectangularRange <- function(pixel.array, upper, lower, target.color="green", main="") {
  
  # find all pixels within target range + get their locations
  idx <- which((lower[1]<=pixel.array[, , 1] & pixel.array[, , 1]<=upper[1]) & 
                 (lower[2]<=pixel.array[, , 2] & pixel.array[, , 2]<=upper[2]) &
                 (lower[3]<=pixel.array[, , 3] & pixel.array[, , 3]<=upper[3]), arr.ind=T)
  
  # make sure target color is an RGB triplet
  if (is.character(target.color)) {
    target.color <- as.vector(col2rgb(target.color)/255)
  }
  
  if (length(target.color) != 3) {
    stop("'target.color' must be a numeric vector of length 3 with values between 0 and 1")
  } else if (range(target.color)[2] > 1) {
    target.color <- target.color/255
  }
  
  # change colors
  for (i in 1:nrow(idx)) {
    pixel.array[idx[i,1], idx[i,2], ] <- target.color
  }
  
  plotArrayAsImage(pixel.array, main=main)
  
  # what to return? color target array? idx? pct in range?
}

# sphericalRange: find pixels within a given radius of a specified point
sphericalRange <- function(pixel.array, center, radius, target.color="green", main="") {
  
  # assuming RGB, change radius percent to fraction of maximum distance in RGB space
  radius <- radius*sqrt(sum((rep(0, 3) - rep(1, 3))^2))
  
  pixel.distances <- matrix(NA, nrow = dim(pixel.array)[1], ncol=dim(pixel.array)[2])
  
  # for every pixel, calculate distance from center
  for (i in 1:dim(pixel.array)[1]) {
    for (j in 1:dim(pixel.array)[2]) {
      pixel.distances[i, j] <- sqrt(sum((pixel.array[i, j, ] - center)^2))
    }
  }
  
  # index every pixel with distance <= radius
  idx <- which(pixel.distances <= radius, arr.ind = T)
  
  # change colors
  # make sure target color is an RGB triplet
  if (is.character(target.color)) {
    target.color <- as.vector(col2rgb(target.color)/255)
  }
  
  if (length(target.color) != 3) {
    stop("'target.color' must be a numeric vector of length 3 with values between 0 and 1")
  } else if (range(target.color)[2] > 1) {
    target.color <- target.color/255
  }
  
  # change colors
  for (i in 1:nrow(idx)) {
    pixel.array[idx[i,1], idx[i,2], ] <- target.color
  }
  
  plotArrayAsImage(pixel.array, main=main)
  
}

target.color <- "cyan"

rectangularRange(img$original.rgb, 
                 upper=c(255, 100, 255)/255, 
                 lower=c(140, 0, 140)/255, 
                 target.color="cyan", 
                 main="Rectangular")

sphericalRange(img$original.rgb, 
               center=c(200, 70, 200)/255, 
               radius=0.16, 
               target.color="tomato",
               main="")


# how to do multiple targets?
# time various functions