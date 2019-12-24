# Import
library(readxl)

dat_sm <- read_excel("Species portal/Data/ExcelExport_1.xlsx", skip = 2)

library(rgdal)
dat_swe <- readOGR("Species portal/Data/rt90_swe/rt90_swe.shp")
dat_swe_border <- data.frame(dat_swe@polygons[[1]]@Polygons[[1]]@coords)
