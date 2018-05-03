# changePixelColor: change all pixels within a target range to a target (diagnostic) color and return 3D array
# 
# plotArrayAsImage: plot a 3D array as an RGB image
# needs error handling; only accepts 3D arrays, interprets them in RGB space
plotArrayAsImage <- function(RGB_array, main="") {
  op <- par(mar=rep(0,4))
  asp <- dim(RGB_array)[1]/dim(RGB_array)[2]
  plot(0:1, 0:1, type="n", ann=F, axes=F, asp=asp, main=main)
  graphics::rasterImage(RGB_array, 0, 0, 1, 1)
  par(op)
}
