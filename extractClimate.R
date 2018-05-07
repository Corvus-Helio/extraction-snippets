library(ncdf4)
library(maptools)
library(raster)
library(rgdal)

# General set up. The date information was read as string vector. Transform it to a date vector.
setwd("**The correct path**")
fdata <-
  read.csv(
    file = "FullData.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )
fdata$Date <- as.Date(fdata$Date, format = "%Y/%m/%d")
names(fdata)[c(5, 6)] <- c("LAT", "LONG")

# Import all the PRISM rasters and put them in a rack.
setwd("**The correct path**/PRISMPrecip")
env.grids <- list.files(pattern = ".bil$")
n.env.variables <- length(env.grids)
env.stack <- stack(env.grids)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      extract <-
        extract(
          env.stack,
          focuscoords,
          method = "simple",
          layer = (i - 3) * 12 + j,
          nl = 1
        )
      extract_result <- c(extract_result, extract)
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Precipitation <- extract_result

# Import all the PRISM rasters and put them in a rack.
setwd("**The correct path**/PRISMTmean")
env.grids <- list.files(pattern = ".bil$")
n.env.variables <- length(env.grids)
env.stack <- stack(env.grids)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      extract <-
        extract(
          env.stack,
          focuscoords,
          method = "simple",
          layer = (i - 3) * 12 + j,
          nl = 1
        )
      extract_result <- c(extract_result, extract)
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Tmean <- extract_result


# Import all the PRISM rasters and put them in a rack.
setwd("**The correct path**/PRISMTmax")
env.grids <- list.files(pattern = ".bil$")
n.env.variables <- length(env.grids)
env.stack <- stack(env.grids)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      extract <-
        extract(
          env.stack,
          focuscoords,
          method = "simple",
          layer = (i - 3) * 12 + j,
          nl = 1
        )
      extract_result <- c(extract_result, extract)
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Tmax <- extract_result


# Import all the PRISM rasters and put them in a rack.
setwd("**The correct path**/PRISMTmin")
env.grids <- list.files(pattern = ".bil$")
n.env.variables <- length(env.grids)
env.stack <- stack(env.grids)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      extract <-
        extract(
          env.stack,
          focuscoords,
          method = "simple",
          layer = (i - 3) * 12 + j,
          nl = 1
        )
      extract_result <- c(extract_result, extract)
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Tmin <- extract_result



# Import all the PRISM rasters and put them in a rack.
### Reprojection is needed ##
setwd("**The correct path**/PRISMVdpmax")
env.grids <- list.files(pattern = ".bil$")

notuniform <- c()
for (i in 1:length(env.grids))
{
  if (projection(raster(env.grids[i])) != "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  {
    newgrid <- c(newgrid, env.grids[i])
  }
}


old <- "+proj=longlat +ellps=WGS84 +no_defs"
new <-
  "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

November <-
  projectRaster(raster("PRISM_vpdmax_stable_4kmM1_201611_bil.bil"), crs = new)
December <-
  projectRaster(raster("PRISM_vpdmax_stable_4kmM1_201612_bil.bil"), crs = new)

env.stack <- stack(env.grids[-c(167, 168)])
special.stack <- stack(November, December)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      if ((i == 16 && j == 11) || (i == 16 && j == 12))
      {
        extract <-
          extract(
            special.stack,
            focuscoords,
            method = "simple",
            layer = i - 15,
            nl = 1
          )
        extract_result <- c(extract_result, extract)
      }
      
      else
      {
        extract <-
          extract(
            env.stack,
            focuscoords,
            method = "simple",
            layer = (i - 3) * 12 + j,
            nl = 1
          )
        extract_result <- c(extract_result, extract)
      }
      
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Vdpmax <- extract_result


setwd("**The correct path**/PRISMVdpmin")
env.grids <- list.files(pattern = ".bil$")

notuniform <- c()
for (i in 1:length(env.grids))
{
  if (projection(raster(env.grids[i])) != "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  {
    newgrid <- c(newgrid, env.grids[i])
  }
}
## "PRISM_vpdmin_stable_4kmM1_201611_bil.bil" and "PRISM_vpdmin_stable_4kmM1_201612_bil.bil" are of "+proj=longlat +ellps=WGS84 +no_defs" projection.##
old <- "+proj=longlat +ellps=WGS84 +no_defs"
new <-
  "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

November <-
  projectRaster(raster("PRISM_vpdmin_stable_4kmM1_201611_bil.bil"), crs = new)
December <-
  projectRaster(raster("PRISM_vpdmin_stable_4kmM1_201612_bil.bil"), crs = new)

env.stack <- stack(env.grids[-c(167, 168)])
special.stack <- stack(November, December)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      if ((i == 16 && j == 11) || (i == 16 && j == 12))
      {
        extract <-
          extract(
            special.stack,
            focuscoords,
            method = "simple",
            layer = i - 15,
            nl = 1
          )
        extract_result <- c(extract_result, extract)
      }
      
      else
      {
        extract <-
          extract(
            env.stack,
            focuscoords,
            method = "simple",
            layer = (i - 3) * 12 + j,
            nl = 1
          )
        extract_result <- c(extract_result, extract)
      }
      
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Vdpmin <- extract_result

setwd("**The correct path**/PRISMTdp")
env.grids <- list.files(pattern = ".bil$")
env.stack <- stack(env.grids)

# initialize an empty accumulator vector
extract_result <- c()
for (i in 3:17)
{
  for (j in 1:12)
  {
    focuscoords <-
      fdata[fdata$Date >= dateFormatter(i, j) &
              fdata$Date < dateFormatter(i, j + 1), c("LONG", "LAT")]
    if ((i - 3) * 12 + j <= 176)
    {
      extract <-
        extract(
          env.stack,
          focuscoords,
          method = "simple",
          layer = (i - 3) * 12 + j,
          nl = 1
        )
      extract_result <- c(extract_result, extract)
    }
    else
    {
      extract_result <- c(extract_result, rep(NA, nrow(focuscoords)))
    }
  }
}
fdata$Tdewp <- extract_result



