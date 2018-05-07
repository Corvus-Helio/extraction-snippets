library(ncdf4)
library(maptools)
library(raster)
library(rgdal)

## Assuming that fdata is the incomplete data extraction template binding in the workspace. ##

map <-
  raster("**The correct path**/nlcd_2011_landcover_2011_edi2.tif")
reclasForest <-
  calc(
    map,
    fun = function(x) {
      x[x == 41] <-
        1
      x[x == 42] <- 1
      x[x == 43] <- 1
      x[x != 1] <- 0
      return(x)
    }
  )
reclasUrb <-
  calc(
    map,
    fun = function(x) {
      x[x == 22] <-
        1
      x[x == 23] <- 1
      x[x == 24] <- 1
      x[x != 1] <- 0
      return(x)
    }
  )
reclasShrub <-
  calc(
    map,
    fun = function(x) {
      x[x == 51] <- 1
      x[x == 52] <- 1
      x[x != 1] <- 0
      return(x)
    }
  )

reclasOther <-
  calc(
    map,
    fun = function(x) {
      x[x == 41] <- 1
      x[x == 42] <- 1
      x[x == 43] <- 1
      return(x)
    }
  )
reclasOther <-
  calc(
    reclasOther,
    fun = function(x) {
      x[x == 22] <- 1
      x[x == 23] <- 1
      x[x == 24] <- 1
      return(x)
    }
  )
reclasOther <-
  calc(
    reclasOther,
    fun = function(x) {
      x[x == 51] <- 1
      x[x == 52] <- 1
      return(x)
    }
  )
reclasOther <-
  calc(
    reclasOther,
    fun = function(x) {
      x[x == 1] <- 0
      x[x != 0] <- 1
      return(x)
    }
  )


forestFoc <-
  focal(reclasForest, matrix(1, nrow = 7, ncol = 7), mean)
UrbFoc <- focal(reclasUrb, matrix(1, nrow = 7, ncol = 7), mean)
ShrubFoc <- focal(reclasShrub, matrix(1, nrow = 7, ncol = 7), mean)
OtherFoc <- focal(reclasOther, matrix(1, nrow = 7, ncol = 7), mean)

for (i in 1:nrow(fdata))
{
  ex1 <- extract(forestFoc, fdata[i, c("LONG", "LAT")])
  ex2 <- extract(UrbFoc, fdata[i, c("LONG", "LAT")])
  ex3 <- extract(ShrubFoc, fdata[i, c("LONG", "LAT")])
  ex4 <- extract(OtherFoc, fdata[i, c("LONG", "LAT")])
  fdata$Forest[i] <- ex1
  fdata$Urban[i] <- ex2
  fdata$Shrub[i] <- ex3
  fdata$Other[i] <- ex4
}
