library(ncdf4)
library(maptools)
library(raster)
library(rgdal)

# declare a few helper functions to help format dates.
spacer <- function(x) {
  if (x < 10) {
    "0"
  } else {
    ""
  }
}
dateFormatter <- function(i, j) {
  if (j <= 12)
  {
    as.Date(paste("20", spacer(i), i, "-", spacer(j), j, "-01", sep = ""))
  }
  else
  {
    as.Date(paste("20", spacer(i + 1), i + 1, "-", "01", "-01", sep = ""))
  }
}

flatten <- function(x) {
  result <- c()
  for (j in 1:length(x[[1]]))
  {
    for (i in 1:length(x))
    {
      result <- c(result, x[[i]][j])
    }
  }
  return(result)
}

checkUseful <- function(x) {
  if (grepl("Total\\s", x))
  {
    return(TRUE)
  }
  return(FALSE)
}

checkCounty <- function(x) {
  if (grepl("(?<=[a-z])\\s{2,}+(?=[A-Z])", x, perl = T))
  {
    return(TRUE)
  }
  return(FALSE)
}

checkLetter <- function(x) {
  if (!grepl("[0-9]", x, perl = T))
  {
    return(TRUE)
  }
  return(FALSE)
}

checkNumber <- function(x) {
  if (!grepl("[aA-zZ]", x, perl = T))
  {
    return(TRUE)
  }
  return(FALSE)
}

## Assuming that fdata is the incomplete data extraction template binding in the workspace. ##

setwd("**The correct path**/PopData")
pdata <-
  read.csv(
    file = "PopDataCleaned.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )
pdata$County <- sub(" County", replacement = "", pdata$County)
pdata$County <- gsub("\\.", replacement = "", pdata$County)
colnames(pdata) <- sub("X", replacement = "", colnames(pdata))

for (i in 1:nrow(fdata))
{
  pop <-
    pdata[pdata$County == fdata$County[i], format(fdata$Date[i], "%Y")]
  fdata$Population[i] <- pop
}
