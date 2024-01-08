##  PROJECT: Quaterly Analytics ETL Process
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Extract Input MSD files for hyperfile processing
##  REF. ID: b51c7bd4
##  LICENCE: MIT
##  DATE:    2023-02-13
##  UPDATE:  2023-02-13

# LIBRARIES ----
  
  library(tidyverse)
  library(glamr)
  library(grabr)
  library(here)
  library(fs)

# NOTES ----

# DISCLAIMERS ----

# GLOBAL PARAMS ----

  # Pano Access
  
  user <- pano_user()
  pass <- pano_pwd()
  
  sess <- pano_session(username = user, password = pass)
  
  url <- "https://pepfar-panorama.org/forms/downloads/"
  
  # Recent MSD Release 
  
  recent_fldr <- url %>% 
    pano_items(user, pass) %>%
    dplyr::filter(stringr::str_detect(item, "^MER")) %>%
    dplyr::pull(item)
  
  ## Identify Latest Release
  curr_release <- stringr::str_extract(recent_fldr, "(?<=Q\\d{1}[:space:]).*")
  curr_status <- base::ifelse(stringr::str_detect(recent_fldr, "Post|Clean"), "clean", "initial")
  curr_fy <- stringr::str_extract(recent_fldr, "[:digit:]{4}") %>% as.numeric()
  curr_qtr <- stringr::str_extract(recent_fldr, "(?<=Q)[:digit:]") %>% as.numeric()
  
  src_msds <- paste0("FY", str_sub(curr_fy, 3, 4), "Q", curr_qtr, "-", curr_release)

  ## Directories 

  dir_mer <- si_path(type = "path_msd")
  
  # Get current date
  curr_dt <- curr_date()
  
  # Set up input files structure
  dir_hypers <- paste0("MSDs-4HyprProcess-", src_msds) %>% 
    file.path(dir_mer, .)
  
  dir_hnats <- file.path(dir_hypers, "NATs")
  dir_hous <- file.path(dir_hypers, "OUs")
  dir_hpsnus <- file.path(dir_hypers, "PSNUs")
  
  dir_hsites <- file.path(dir_hypers, "Sites")
  dir_hsites_curr <- file.path(dir_hsites, "Current")
  dir_hsites_prev <- file.path(dir_hsites, "Previous")
  
  # Create non-existant folders
  
  if (!dir.exists(dir_hypers)) {
    
    dir.create(dir_hypers)
    
    dir.create(dir_hnats)
    dir.create(dir_hous)
    dir.create(dir_hpsnus)
    
    dir.create(dir_hsites)
    dir.create(dir_hsites_curr)
    dir.create(dir_hsites_prev)
  }
  
  #open_path(dir_hypers)
  
# FUNCTIONS ----
  
  msd_unzip <- function(.file, dest = NULL) {
    
    if (is.null(dest)) dest <- dirname(.file)
  }
  
# DATA EXTRACTION ----
  
  ## Extract MSD file paths
  items <- pano_extract(item = "mer",
                        version = curr_status,
                        fiscal_year = curr_fy,
                        quarter = curr_qtr,
                        username = user,
                        password = pass,
                        unpack = TRUE)
  
  ## Downloads zip files
  
  ## Nat / SUBNAT
  
  items %>% 
    filter(type == "file zip_file",
           str_detect(parent, paste0(recent_fldr, "$|", recent_fldr, "/FY")),
           str_detect(item, ".*_NAT_SUBNAT_.*.zip$")) %>% 
    pull(path) %>% 
    walk(~pano_download(item_url = .x, 
                        session = sess, 
                        dest_path = dir_hnats,
                        uncompress = T))
  
  ## OUs
  
  items %>% 
    filter(type == "file zip_file",
           str_detect(parent, paste0(recent_fldr, "$|", recent_fldr, "/FY")),
           str_detect(item, ".*_OU_IM_FY.*.zip$")) %>% 
    pull(path) %>%
    walk(~pano_download(item_url = .x, 
                        session = sess, 
                        dest_path = dir_hous,
                        uncompress = T))
  
  ## PSNUs
  
  items %>% 
    filter(type == "file zip_file",
           str_detect(parent, paste0(recent_fldr, "$|", recent_fldr, "/FY")),
           str_detect(item, ".*_PSNU_IM_FY.*.zip$")) %>% 
    pull(path) %>%
    walk(~pano_download(item_url = .x, 
                        session = sess, 
                        dest_path = dir_hpsnus,
                        uncompress = T))
  
  ## Sites / Current
  
  items %>% 
    filter(type == "file zip_file",
           str_detect(parent, paste0(recent_fldr, "/Site Level$")),
           str_detect(item, ".*_Site_IM_FY.*.zip$")) %>% 
    pull(path) %>%
    walk(~pano_download(item_url = .x, 
                        session = sess, 
                        dest_path = dir_hsites_curr))
  
  ## Sites / Previous
  
  items %>% 
    filter(type == "file zip_file",
           str_detect(parent, paste0(recent_fldr, "/FY.*/Site Level$")),
           str_detect(item, ".*_Site_IM_FY.*.zip$")) %>% 
    pull(path) %>%
    walk(~pano_download(item_url = .x, 
                        session = sess, 
                        dest_path = dir_hsites_prev))
  
  
# UN-COMPRESS Files
  
  ## check list of files
  dir_hypers %>% 
    dir_map(
      path = .,
      fun = basename,
      recurse = TRUE
    ) %>% unlist()
  
  # Rename and unzip files
  dir_hypers %>% 
    dir_ls(recurse = TRUE, regexp = ".zip$") %>% 
    walk(function(.file){
      
      dir_dest <- dirname(.file)
      
      # Remove mis-formatted files
      if (stringr::str_detect(.file, ".*(\\%20|\\%27).*.txt")) {
        .file %>% 
          stringr::str_replace(".zip$", ".txt") %>% 
          fs::dir_delete()
      }
      
      # Rename
      filename <- basename(.file) %>% 
        stringr::str_replace(".zip$", ".txt") %>% 
        stringr::str_replace_all("\\%20", " ") %>% 
        stringr::str_replace_all("\\%27", "'")
      
      if (!file.exists(file.path(dir_dest, filename))) {
        # Unzip
        utils::unzip(
          zipfile = .file,
          files = filename,
          overwrite = TRUE,
          exdir = dir_dest
        )
      }
      
      print(filename)
    })
  
  