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

setwd("**The correct path**/Deer")

library(pdftools)
text <- pdf_text("2016deerrpt.pdf")
for (j in 2016:2007)
{
  text <- pdf_text(paste(j, "deerrpt.pdf", sep = ""))
  start <-
    which(
      grepl(
        "Listed below are counties, towns and cities from which deer harvests were reported.",
        text
      )
    )
  end <- which(grepl('Statewide Totals', text))
  end <- end[end > start][1]
  result <- c()
  if (j == 2016)
  {
    cnames <- c()
  }
  
  for (i in start:end)
  {
    page <- strsplit(text[i], "\r\n")[[1]]
    if (i == start)
    {
      page <- page[5:68]
    }
    page <-
      gsub("Statewide Totals", "Statewide Totals  121212 121212", page)
    useful <- page[sapply(page, checkUseful)]
    
    split <-
      strsplit(useful,
               "(?<=[0-9])\\s+(?=[A-Z])|(?<=[aA-zZ])\\s{2,}+(?=[A-Z])",
               perl = T)
    flat <- flatten(split)
    result <- c(result, flat[sapply(flat, checkUseful)])
    
    if (j == 2016)
    {
      countysplit <-
        strsplit(page,
                 "(?<=[0-9])\\s+(?=[A-Z])|(?<=[aA-zZ])\\s{2,}+(?=[A-Z])",
                 perl = T)
      flat <- flatten(countysplit)
      cnames <- c(cnames, flat[sapply(flat, checkLetter)])
    }
  }
  
  if (j == 2016)
  {
    County <- gsub(" ", "", na.omit(cnames[-c(54)])[1:56])
    ddata <-
      data.frame(gsub("StLawrence", "St Lawrence", County),
                 stringsAsFactors = FALSE)
  }
  
  totalTake <- strsplit(result, "(?<=[0-9])\\s+(?=[0-9])", perl = T)
  stringdata <-
    gsub(",", "" , flatten(totalTake)[sapply(flatten(totalTake), checkNumber)])
  data <-  as.numeric(na.omit(stringdata))
  ddata <- cbind(ddata, data)
}
names(ddata) <- c("County", 2016:2007)

for (i in 1:nrow(fdata))
{
  deer <-
    ddata[ddata$County == fdata$County[i], format(fdata$Date[i], "%Y")]
  if (is.null(deer) || (length(deer) == 0))
  {
    fdata$DeerHarvest[i] <- NA
  }
  else
  {
    fdata$DeerHarvest[i] <- deer
  }
}
