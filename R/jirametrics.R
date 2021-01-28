require(readr)
require(ggplot2)
require(lubridate)
require(knitr)
require(dplyr)
require(formattable)
require(tidyquant)
require(tidyverse)
require(blogdown)
require(plotly)
require(r2d3)

if (!require("DT")) install.packages('rstudio/DT')
if (!require("rlist")) devtools::install_github("renkun-ken/rlist")
if (!require("highcharter")) install.packages("highcharter")
if (!require("r2d3")) install.packages("r2d3")
if (!require("blogdown")) install.packages("blogdown")

library(flexdashboard)
library(knitr)
library(DT)
library(rpivotTable)
library(ggplot2)
library(plotly)
library(dplyr)
library(openintro)
library(highcharter)
library(ggvis)

finastra_colors <- list(
  `crimson`        = "#F9423A",
  `blue`       = "#009CBD",
  `green` = "#26D07C",
  `pink` = "#F04E96",
  `orange`     = "#ED8B00",
  `yellow`     = "#FFD100",
  `fuchsia`  = "#C137A2",
  `violet` = "#694ED6",
  `tan` = "#C7C8CA",
  `charcoal`= "#414141"
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
      color_list <- c(other, finastra_colors[primary])
      
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
  stopifnot(file.exists(fileName))
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


isWIP <- function(cldt, loopdt) {
  stopifnot(is.Date(cldt) && is.Date(loopdt))
  ifelse(cldt >= loopdt, TRUE, FALSE)
}

getAge <- function(crdt, loopdt) {
  stopifnot(is.Date(crdt) && is.Date(loopdt))
  ifelse(crdt < loopdt, as.numeric(loopdt - crdt), 0)
}


