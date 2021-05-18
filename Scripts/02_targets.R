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
df_results <-df_raw %>%
  select(-targets, -cumulative) %>% 
  rename("q1"=qtr1,
         "q2"=qtr2,
         "q3"=qtr3,
         "q4"=qtr4) %>%
  pivot_longer(c("q1", "q2", "q3", "q4"),
               names_to="results",
               values_drop_na = TRUE) %>%
  mutate(quarter=paste(fiscal_year, results),
         results_or_targets = replace(results, results %in% c("q1", "q2", "q3", "q4"), "results"),
         results=value) %>%
  rename("values"=value) %>%
  drop_na(results) %>% 
  View()

##Cumulative
df_cumulative <-df_raw %>%
  select(-targets, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  pivot_longer(c("cumulative"),
               names_to="cumulative",
               values_drop_na = TRUE) %>%
  mutate(results_or_targets = replace(cumulative, cumulative %in% c("cumulative"), "cumulative"),
         cumulative=value) %>%
  rename("values"=value) %>%
  drop_na(cumulative) %>% 
  View()

##Targets
df_targets <-df_raw %>%
  select(-cumulative, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  pivot_longer(c("targets"),
               names_to="targets",
               values_drop_na = TRUE) %>%
  #rewrite this as results or targets = targets
  mutate(results_or_targets = replace(targets, targets %in% c("targets"), "targets"),
         targets=value) %>%
  rename("values"=value) %>%
  drop_na(targets) %>% 
  View()

##Quarterly Targets
#May be easier way to do this? map_dfr

df_targets_q1 <-df_targets %>%
  mutate(quarter=paste(fiscal_year, "Q1"),
         #results_or_targets = replace(results_or_targets, results_or_targets %in% c("q1", "q2", "q3", "q4"), "results")) %>%
         results_or_targets = "targets_(for_quarterly_achievement)") %>%
  rename("targets_(for_quarterly_achievement)"=targets) %>% 
  View()

df_targets_q2 <- df_targets_q1 %>% 
  mutate(quarter=paste(fiscal_year, "Q2")) %>% 
  View()

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
  mutate(indicator_nd=case_when(indicator %in% nd_ind ~ paste(indicator, numeratordenom, sep="_"), TRUE ~ indicator)) %>%
  group_by(dplyr::across(var_char)) %>%
  arrange(quarter, .by_group = TRUE) %>%
  mutate(running_cumulative = cumsum(values),
                running_cumulative = ifelse(indicator_nd %in% snapshot_ind, values, running_cumulative),
         results_or_targets="running_cumulative") %>%
  ungroup() %>%
  select(-results, -indicator_nd) %>%
  mutate(values=running_cumulative) %>% 
  View()

#check
df_rcum %>% 
  filter(indicator=="TX_TB") %>% 
  View()

## NET_NEW_Targets

df_tx<-df_raw %>% 
  filter(indicator=="TX_CURR") %>% 
  group_by(operatingunit, operatingunituid, countryname, snu1, snu1uid, psnu, psnuuid, snuprioritization,
           typemilitary, dreams, primepartner, fundingagency, mech_code, mech_name, pre_rgnlztn_hq_mech_code,
           primepartner_duns, award_number, indicator, numeratordenom, indicatortype, disaggregate, 
           standardizeddisaggregate, categoryoptioncomboname, ageasentered, trendsfine, trendssemifine, trendscoarse,
           sex, statushiv, hiv_treatment_status, otherdisaggregate, otherdisaggregate_sub, modality, source_name) %>%
  mutate(nn_targets = targets - lag(cumulative, order_by = fiscal_year)) %>% 
  ungroup() %>% 
  select(-targets, -cumulative, -qtr1, -qtr2, -qtr3, -qtr4) %>% 
  mutate(indicator = "TX_NET_NEW",
         values=nn_targets,
         results_or_targets="targets") %>% 
  rename(targets = nn_targets) 

#check
df_nn %>% 
  filter(standardizeddisaggregate=="Total Numerator",
         psnu=="Juba County") %>% 
  View()

#Combine output from above steps into one df






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
  


