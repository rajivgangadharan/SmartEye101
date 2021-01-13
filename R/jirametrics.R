require(readr)
require(ggplot2)
require(lubridate)
require(knitr)
require(dplyr)
require(kableExtra)
require(formattable)
require(tidyquant)
require(tidyverse)
require(blogdown)
require(DT)

finastra_colors <- list(
  `crimson`        = "#F9423A",
  `charcoal`      = "#414141",
  `blue`       = "#009CBD",
  `orange`     = "#ED8B00",
  `yellow`     = "#FFD100",
  `pink` = "#F04E96",
  `fuchsia`  = "#C137A2",
  `violet` = "#694ED6",
  `green` = "#26D07C",
  `tan` = "#C7C8CA",
  `grey` = "#8D96A3"
  )



finastra_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (finastra_colors)
  finastra_colors[cols]
}

color_point <- c(finastra_cols("crimson"),
                 finastra_cols("orange"),
                 finastra_cols("fuchsia"),
                 finastra_cols("blue")
)

branded_pal <- function(primary = "fuchsia", other="violet", direction = 1) {
  stopifnot(primary %in% names(finastra_colors))
  function(n) {
    if (n > 11) warning("Branded Color Palette only has 11 colors.")
    if (n == 2) {
      other <- if (! other %in% names(finastra_colors) ) {
        other
      } else {
        finastra_colors[other]
      }
      color_list <- c(other, branded_colors[primary])
      
    } else {
      color_list <- finastra_colors[1:n]
    }
      color_list <- unname(unlist(color_list))
      if (direction >= 0) color_list else rev(color_list)
    
  }
}

scale_colour_branded <- function(primary = "fuchsia",  other = "violet", 
                          direction = 1, 
                          ...) {
  ggplot2::discrete_scale("colour", "branded", 
                          branded_pal(primary, other, direction), 
                          ...
  )
}

scale_color_branded <- scale_colour_branded


toDate <- function(argString, dateFormat="%Y-%m-%d %H:%M:%S") {
  return(as.Date(argString, format = dateFormat))
}

maxDate <- function(dateVec) {
  return (max(dateVec, na.rm=TRUE))
}

minDate <- function(dateVec) {
  return (min(dateVec, na.rm=TRUE))
}

getAge <- function(beginDate, endDate=Sys.Date()) {
  if (! is.Date(beginDate) || ! is.Date(endDate)) {
    ageInDays <- difftime(toDate(endDate),toDate(beginDate), units="days")
  } else {
    ageInDays <- difftime(endDate,beginDate,units="days")
  }
  ageInDays
}

readDataset <- function(fileName, sep='|') {
  # Loading the tab separated file
  df <- read_delim(fileName, sep,
                   escape_double = FALSE, 
                   trim_ws = TRUE)
  df
}


readCSVDataset <- function(fileName, sep='|') {
  if (! file.exists(fileName)) {
    paste0("'", fileName, "' does not exist, check the file name and full path.")
    return(NULL)
  }
  df <- read.csv2(fileName, header=TRUE, sep=sep)
  df
}

finastra_theme <- 
  theme_bw() +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black" ),
        panel.grid.minor.y = element_blank())
