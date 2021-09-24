## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders

#libraries-----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(ICPIutilities)
library(glamr)
library(glitr)
library(googledrive)
library(googlesheets4)
library(here)

Data <- "./Data"

Dataout <- "./Dataout"

dir_msds <- si_path("path_msd")

# URLs
home_url <- "https://pepfar-panorama.org"

download_path <- "/forms/downloads"

fy <- 2021

qtr <- 3

version <- "Pre" # or "Post"

mer_path <- paste0("MER FY", fy, " Q", qtr, " ", version, "-Cleaning")

data_url <- home_url %>% 
  paste0(download_path, "/", mer_path)


