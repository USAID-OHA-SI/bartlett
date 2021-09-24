## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Reshape numeric variables and create targets, cumulative vars in line with Tableau prep flow
## CREATED:  4.30.21
## MODIFIED: 5.11.20

#install package tidylog, mimics dplyr/tidyr functions, printed results- tidylog::dropna, tells you how many rows are dropped
#tidylog() tidylog::drop_na()
#Can print out table by not assigning or surround entire thing in ()


##Results
df_results <- df_raw %>%
  select(-targets, -cumulative) %>% 
  rename("q1"=qtr1,
         "q2"=qtr2,
         "q3"=qtr3,
         "q4"=qtr4) %>%
  pivot_longer(c("q1", "q2", "q3", "q4"),
               names_to = "results",
               values_drop_na = TRUE) %>%
  mutate(quarter=paste(fiscal_year, results),
         results_or_targets = replace(results, results %in% c("q1", "q2", "q3", "q4"), "results"),
         results=value) %>%
  rename("values"=value) %>%
  drop_na(results)

##Cumulative
df_cumulative <- df_raw %>%
  select(-targets, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  pivot_longer(c("cumulative"),
               names_to="cumulative",
               values_drop_na = TRUE) %>%
  mutate(results_or_targets = replace(cumulative, cumulative %in% c("cumulative"), "cumulative"),
         cumulative=value) %>%
  rename("values"=value) %>%
  drop_na(cumulative)

##Targets
df_targets <- df_raw %>%
  select(-cumulative, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  pivot_longer(c("targets"),
               names_to="targets",
               values_drop_na = TRUE) %>%
  #rewrite this as results or targets = targets
  mutate(results_or_targets = replace(targets, targets %in% c("targets"), "targets"),
         targets=value) %>%
  rename("values"=value) %>%
  drop_na(targets)

##Quarterly Targets
#May be easier way to do this? map_dfr

df_targets_q1 <-df_targets %>%
  mutate(quarter=paste(fiscal_year, "Q1"),
         #results_or_targets = replace(results_or_targets, results_or_targets %in% c("q1", "q2", "q3", "q4"), "results")) %>%
         results_or_targets = "targets_(for_quarterly_achievement)") %>%
  rename("targets_(for_quarterly_achievement)"=targets) 

df_targets_q2 <- df_targets_q1 %>% 
  mutate(quarter=paste(fiscal_year, "Q2")) 

df_targets_q3 <- df_targets_q1 %>% 
  mutate(quarter=paste(fiscal_year, "Q3"))

df_targets_q4 <- df_targets_q1 %>% 
  mutate(quarter=paste(fiscal_year, "Q4"))

##Running Cumulative

# Code adapted from:
# https://github.com/ICPI/ICPIutilities/blob/b332fd81dd81070891ae25b2e961d3b9e439ca1e/data-raw/stored_data.R
# https://github.com/ICPI/ICPIutilities/blob/b332fd81dd81070891ae25b2e961d3b9e439ca1e/R/reshape_msd.R#L77-L102
snapshot_ind <- c("AGYW_PREV_N",
                  "AGYW_PREV_D",
                  "OVC_SERV",
                  "PrEP_CURR",
                  "OVC_HIVSTAT",
                  "TX_CURR",
                  "TX_ML",
                  "TX_TB_D", #only TX_TB denom, not num
                  "TX_PVLS",
                  "TX_PVLS_D",
                  "SC_CURR")

nd_ind <- c("AGYW_PREV","TX_TB","TX_PVLS")

var_char <- df_results %>%
  select(!where(is.numeric)) %>%
  select(-quarter) %>%
  names()


df_rcum <- df_results %>%
  mutate(indicator_nd=case_when(
    indicator %in% nd_ind ~ paste(indicator, numeratordenom, sep="_"), 
    TRUE ~ indicator)) %>%
  group_by(dplyr::across(var_char)) %>%
  arrange(quarter, .by_group = TRUE) %>%
  mutate(running_cumulative = cumsum(values),
         running_cumulative = ifelse(indicator_nd %in% snapshot_ind, 
                                     values, 
                                     running_cumulative),
         results_or_targets="running_cumulative") %>%
  ungroup() %>%
  select(-results, -indicator_nd) %>%
  mutate(values=running_cumulative)


## NET_NEW_Targets

df_nn <- df_raw %>% 
  filter(indicator=="TX_CURR") %>% 
  group_by(operatingunit, operatingunituid, countryname, snu1, snu1uid, psnu, psnuuid, snuprioritization,
           typemilitary, dreams, primepartner, fundingagency, mech_code, mech_name, pre_rgnlztn_hq_mech_code,
           prime_partner_duns, award_number, indicator, numeratordenom, indicatortype, disaggregate, 
           standardizeddisaggregate, categoryoptioncomboname, ageasentered, trendsfine, trendssemifine, trendscoarse,
           sex, statushiv, hiv_treatment_status, otherdisaggregate, otherdisaggregate_sub, modality, source_name) %>%
  mutate(nn_targets = targets - lag(cumulative, order_by = fiscal_year)) %>% 
  ungroup() %>% 
  select(-targets, -cumulative, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  mutate(indicator = "TX_NET_NEW",
         values = nn_targets,
         results_or_targets = "targets") %>% 
  rename(targets = nn_targets) 


#Combine output from above steps into one df
df <- bind_rows(df_cumulative, 
                df_results, 
                df_targets, 
                df_targets_q1, 
                df_targets_q2, 
                df_targets_q3, 
                df_targets_q4, 
                df_nn)

#Partner type join
partner_sheet <- "1tGk1TR8l3WacR8qMIK0AQvFynABijAaLHeIctE1nUoM"

#googledrive::drive_browse(as_id(partner_sheet))

df_partners <- googlesheets4::read_sheet(as_id(partner_sheet)) %>% 
  rename("mech_code"="Mechanism ID",
         "Partner_Type"="Partner Type") %>% 
  mutate(mech_code = as.character(mech_code)) 

df <- df %>% 
  left_join(df_partners, by=("mech_code")) %>% 

#Field changes
df1 <- df %>%
  clean_agency() %>% 
  mutate(
    #fundingagency = str_replace(fundingagency, "HHS/CDC", "CDC"),
    #fundingagency = str_replace(fundingagency, "HHS/HRSA", "HRSA"),
    fundingagency = case_when(
      str_detect(fundingagency, "PC") ~ "Peace Corps",
      #str_detect(fundingagency, "CDC") ~ "CDC",
      #str_detect(fundingagency, "HRSA") ~ "HRSA",
      str_detect(fundingagency, "State") ~ "State Dept",
      TRUE ~ fundingagency),
    operatingunit = str_replace(operatingunit, "Democratic Republic of the Congo", "DRC"),
    operatingunit = str_replace(operatingunit, "Papua New Guinea", "PNG"),
    countryname = str_replace(countryname, "Democratic Republic of the Congo", "DRC"),
    countryname = str_replace(countryname, "Papua New Guinea", "PNG"),
    modality = str_replace(modality, "Inpat", "Inpatient"),
    modality = str_replace(modality, "HomeMod", "Community Home-Based"),
    modality = str_replace(modality, "Index", "Index (Facility"),
    modality = str_replace(modality, "IndexMod", "Index (Community)"),
    modality = str_replace(modality, "MobileMod", "Community Mobile"),
    modality = str_replace(modality, "TBClinic", "TB Clinic"),
    modality = str_replace(modality, "OtherPITC", "Other PITC"),
    modality = str_replace(modality, "VCTMod", "Community VCT"),
    modality = str_replace(modality, "OtherMod", "Other Community"),
    modality = str_replace(modality, "Emergency Ward", "Emergency"),
    index = case_when(
      standardizeddisaggregate=="3:Age Aggregated/Sex/Contacts" ~ "Contacts",
      standardizeddisaggregate=="2:Age/Sex/IndexCasesAccepted" ~ "Accepted",
      standardizeddisaggregate== "1:Age/Sex/IndexCasesOffered" ~ "Offered",
      standardizeddisaggregate== "4:Age/Sex/Result" & otherdisaggregate == "Known at Entry" ~ "Not Tested - positive at entry",
      standardizeddisaggregate== "4:Age/Sex/Result" & otherdisaggregate == "Newly Identified" & statushiv == "Positive" ~ "Tested - positive",
      standardizeddisaggregate== "4:Age/Sex/Result" & otherdisaggregate == "Newly Identified" & statushiv =="Negative" ~ "Tested - negative",
      TRUE ~ "NA"),
    community_facility = case_when(
      str_detect(disaggregate, "IndexMod/") ~ "Community",
      str_detect(disaggregate, "Index/") ~ "Facility",
      TRUE ~ "NA"),
    key_pops = case_when(
      str_detect(otherdisaggregate, "MSM") ~ "MSM",
      str_detect(otherdisaggregate, "FSW") ~ "FSW",
      str_detect(otherdisaggregate, "PWID") ~ "PWID",
      str_detect(otherdisaggregate, "People in prisons") ~ "People in prisons",
      str_detect(otherdisaggregate, "TG") ~ "TG",
      str_detect(otherdisaggregate, "Other Key Populations") ~ "Other key populations",
      TRUE ~ "NA"),
    age_vmmc = case_when(
      ageasentered=="01-04" | ageasentered=="05-09" | ageasentered=="<=02 Months" | ageasentered=="<01"
      | ageasentered=="<05"| ageasentered=="<10" | ageasentered=="<15" | ageasentered=="01-09" 
      | ageasentered=="02 - 12 Months" | ageasentered=="10-14" ~ "<15",
      ageasentered=="15-17" | ageasentered=="15-19" | ageasentered=="18-24" | ageasentered=="20-24" 
      | ageasentered=="25-29" ~ "15-29",
      ageasentered=="30-34" | ageasentered=="35-39" | ageasentered=="40-44" | ageasentered=="40-49" |
      ageasentered=="45-49" | ageasentered=="50+" ~ "30+",
      TRUE ~ "NA"),
    current_quarter="2021 Q2",
    fy = paste0("FY", str_sub(
      fiscal_year, start = 3, end = 4))) %>%
  select(-fiscal_year) %>%
  rename(
    "operating_unit"=operatingunit,
    "operating_unit_UID"=operatingunituid,
    "country_name"=countryname,
    "funding_agency"=fundingagency,
    "SNU"=snu1,
    "SNU_UID"=snu1uid,
    "PSNU"=psnu,
    "PSNU_UID"=psnuuid,
    "SNU_prioritization"=snuprioritization,
    "type_military"=typemilitary,
    "DREAMS"=dreams,
    "prime_partner"=primepartner,
    "numerator_denom"=numeratordenom,
    "indicator_type"=indicatortype,
    "standardized_disaggregate"=standardizeddisaggregate,
    "category_option_combo_name"=categoryoptioncomboname,
    "other_disaggregate"=otherdisaggregate,
    "HIV_treatment_status"=hiv_treatment_status,
    "status_HIV"=statushiv,
    "status_TB"=statustb,
    "status_CX"=statuscx,
    "age_as_entered"=ageasentered,
    "age_fine"=trendsfine,
    "age_semifine"=trendssemifine,
    "age_coarse"=trendscoarse,
    "prime_partner_duns"=primepartner_duns) %>%
  rename_all(~str_to_title(gsub("_", " ", .x, fixed=TRUE))) %>% 
  rename(
    "operating unit UID"= "Operating Unit Uid",
    "SNU"=Snu,
    "SNU prioritization"="Snu Prioritization",
    "Status HIV"="Status Hiv",
    "Status TB"="Status Tb",
    "Status CX"="Status Cx",
    "HIV treatment status"="Hiv Treatment Status",
    "Results or Targets"="Results Or Targets",
    "Targets (for Q. Ach)"="Targets (For Quarterly Achievement)",
    "Age VMMC"="Age Vmmc",
    "FY"="Fy",
    "G2G"="G2g") 

df1 %>% 
  glimpse()
  View()
  
  
  #next steps: Use the msd/natsubnat/etc
  #QC in tableau
  #Discuss with CAC, ask for feedback on issue with mech_name changes


###########################################################################

#Scratch/Notes

#for local partners can use google drive api


df_targets <- df_raw %>%
  #Rename quarters to match Noah's flow and pivot numeric fields
  rename("q1"=qtr1,
         "q2"=qtr2,
         "q3"=qtr3,
         "q4"=qtr4) %>%
  pivot_longer(c("q1", "q2", "q3", "q4", "targets", "cumulative"),
               names_to="results_or_targets",
               values_drop_na = TRUE) %>%
  #Create quarter variable as combo FY and qtr, create results_or_targets label based on qtr
  mutate(quarter=paste(fiscal_year, results_or_targets),
         results_or_targets = replace(results_or_targets, results_or_targets %in% c("q1", "q2", "q3", "q4"), "results"),
         cumulative=ifelse(results_or_targets=="cumulative", value, NA_real_),
         targets=ifelse(results_or_targets=="targets", value, NA_real_),
         results=ifelse(results_or_targets=="results", value, NA_real_),
         quarter=str_replace(quarter, "targets", "q4")) %>%
  View()

#QC
df_raw %>% 
  group_by(indicator, fiscal_year, standardizeddisaggregate) %>%
  dplyr::summarise(across(c(targets), sum, na.rm=TRUE)) %>%
  View()
  
df_targets %>% 
  group_by(indicator, fiscal_year, standardizeddisaggregate) %>%
  dplyr::summarise(across(c(targets), sum, na.rm=TRUE)) %>%
  View()

df_targets %>% 
  filter(indicator=="TX_NEW",
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(results_or_targets, indicator, fiscal_year, quarter) %>%
  dplyr::summarise(across(c(targets, value), sum, na.rm=TRUE)) %>%
  View()
  


