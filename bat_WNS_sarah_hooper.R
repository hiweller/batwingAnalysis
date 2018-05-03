source("./define_color_ranges.R")

require(colordistance)
paths <- getImagePaths("Wing_images/")

#### rectangularRange and sphericalRange ####

# time to run 6 small images (400 x 600 = 240,000 pixels)
small_imgs <- colordistance::getImagePaths("Wing_images/")
par(mfrow=c(2, 3))
small_img_benchmark <- rbenchmark::benchmark(for (path in small_imgs) {
  img <- colordistance::loadImage(path, lower=NULL, upper=NULL)
  rectangularRange(img$original.rgb, 
                   upper=c(255, 100, 255)/255, 
                   lower=c(140, 0, 140)/255, 
                   target.color="#FFFF96")
}, replications = 100)
# takes ~3.08 seconds per 6 images (avg)

large_imgs <- colordistance::getImagePaths("Wing_images/Original/")
large_img_benchmark <- rbenchmark::benchmark(for (path in large_imgs) {
  img <- colordistance::loadImage(path, lower=NULL, upper=NULL)
  rectangularRange(img$original.rgb, 
                   upper=c(255, 100, 255)/255, 
                   lower=c(140, 0, 140)/255, 
                   target.color="#FFFF96")
}, replications=1)

# size test
size_imgs <- colordistance::getImagePaths("Images/Size_range/")
times <- c()
for (path in size_imgs) {
  img <- colordistance::loadImage(path, lower=NULL, upper=NULL, hsv=FALSE)
  t <- rbenchmark::benchmark(rectangularRange(img$original.rgb,
                                              upper=c(255, 255, 255)/255,
                                              lower=c(200, 200, 200)/255,
                                              target.color="magenta"), replications=10)
  times <- c(times, t$elapsed/10)
}

pixel_columns <- seq(4000, 400, length.out = 10)
pixel_rows <- pixel_columns*0.75
pixel_counts <- pixel_columns*pixel_rows

plot(pixel_counts, times, pch=19)

pelicans <- "Images/Fun/pelicans.jpg"
img <- colordistance::loadImage(pelicans)
radii <- seq(0.2, 1, 0.2)
par(mfrow=c(2, 3))
plotArrayAsImage(img$original.rgb)
for (i in radii) {
  sphericalRange(img$original.rgb, 
                 center=c(220, 220, 220)/255,
                 radius=i,
                 target.color = "tomato")
}

lizard <- "Images/Fun/gecko.jpg"
img <- colordistance::loadImage(lizard)
par(mfrow=c(2, 1))
plotArrayAsImage(img$original.rgb)
sphericalRange(img$original.rgb, 
               center=c(20, 20, 20)/255,
               radius=0.1,
               target.color = "red")


lower <- c(110,70,80)/255
upper <- c(165,155,150)/255
par(mfrow=c(2, 3))

for (i in 1:length(paths)) {
  area <- countColor(paths[i], lower=lower, upper=upper, bg.lower=rep(0, 3), bg.upper=rep(0.2, 3), plotting=TRUE, target.color = "yellow")
  print(area*100)
}


# doing a whole folder at once? (countColor wrapper)

# doing a sphere around a center - middle + search radius?

# countColorInDirectory: wrapper for countColor, applies to every picture in a folder and returns a dataframe with % cover by image 
#
# countColor: count the number of pixels within a target range and return fraction with diagnostic plot, which is either printed to the plot window or saved in a target folder
# 
# changePixelColor: change all pixels within a target range to a target (diagnostic) color and return 3D array
# 
# plotArrayAsImage: plot a 3D array as an RGB image
# 
# rectangularRange: find pixels within a target range defined by ranges in each channel
#
# sphericalRange: find pixels within a given radius of a specified point

target.color <- "yellow"

save.image <- "~/Downloads/combined.png"



countColor(path=path, lower=lower, upper=upper, 
      bg.lower=rep(0,3), bg.upper=rep(0.2, 3),
      target.color=target.color, save.image=save.image)
