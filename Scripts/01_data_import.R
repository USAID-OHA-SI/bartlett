## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Import and stitch 1) FY 19-21 MSD 2) FY 15-18 MSD and 3) NAT_SUBNAT 15-21 data
## CREATED:  4.30.21


files <- list.files(Data, "*.txt", full.names = TRUE)

import_data <- function(file) {

  df<- purrr::map_dfr(.x=files, .f=read_msd)
  
  return(df)
}

df_raw<-import_data(files)
