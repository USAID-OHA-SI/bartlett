## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Import and stitch 1) FY 19-21 MSD 2) FY 15-18 MSD and 3) NAT_SUBNAT 15-21 data
## CREATED:  4.30.21

source("./Scripts/00_setup_project.R")

#load secrets
glamr::load_secrets()

fnames <- c("^MER_.*_OU_IM_FY19-2.*",
            "^MER_.*_PSNU_IM_FY19-2.*",
            "^MER_.*_PSNU_IM_DREAMS_FY19-2.*",
            "^MER_.*_PSNU_IM_FY15-18_.*",
            "^MER_.*_NAT_SUBNAT_FY15-2.*")

# Pano Session
sess <- pano_session(username = pano_user(),
                     password = pano_pwd())

# main data sets
df_pano_main <- pano_extract(item = "mer",
                             version = "clean",
                             fiscal_year = 2021,
                             quarter = 3,
                             unpack = FALSE,
                             username = pano_user(),
                             password = pano_pwd())

df_pano_details <- pano_extract(item = "mer",
                                version = "clean",
                                fiscal_year = 2021,
                                quarter = 3,
                                unpack = TRUE,
                                username = pano_user(),
                                password = pano_pwd())

df_pano <- df_pano_main %>% 
  bind_rows(df_pano_details) %>% 
  filter(!is.na(parent))

if (nrow(df_pano) == 0) {
  stop("EMPTY - No results to match your query")
}

# Download only current and previous global PSNU x IM & NAT_SUBNAT
df_pano %>% 
  filter(type == "file zip_file",
         str_detect(parent, "FY15.*|MER.*"),
         str_detect(item, paste(fnames, collapse = "|"))) %>% #pull(item) 
  pull(path) %>% 
  walk(~pano_download(item_url = .x, 
                     session = sess,
                     dest = Data,
                     uncompress = TRUE))



# Try moving your data files outside of R Projects
# files <- fnames %>% 
#   map_chr(~return_latest(folderpath = dir_msds, pattern = .x))

files <- list.files(Data, "*.txt", full.names = TRUE)

import_data <- function(file) {

  df<- purrr::map_dfr(.x=files, .f=read_msd)
  
  return(df)
}

df_raw <- import_data(files)

# Rows: 16,839,776
# Columns: 43
#df_raw %>% glimpse()

