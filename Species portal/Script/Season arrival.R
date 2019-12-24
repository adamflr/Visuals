# Read season map
library(magick)
img <- image_read("Species portal/Data/seasonal_arrival/ank_var_20190221.gif")
unlist(img)
magick:::`print.magick-image`

library(caTools)
img <- read.gif("Species portal/Data/seasonal_arrival/ank_var_20190221.gif")
table(img$image)
img
image(t(img$image), col = img$col)

img$image
