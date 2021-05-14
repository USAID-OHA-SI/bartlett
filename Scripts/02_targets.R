## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Reshape numeric variables and create targets, cumulative vars in line with Tableau prep flow
## CREATED:  4.30.21
## MODIFIED: 5.11.20


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
  mutate(results_or_targets = replace(targets, targets %in% c("targets"), "targets"),
         targets=value) %>%
  rename("values"=value) %>%
  drop_na(targets) %>% 
  View()

##Quarterly Targets

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
## Try doing similar to quarterly targets

df_nn_21<-df_raw %>% 
  filter(indicator=="TX_CURR",
         fiscal_year==2021) %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
         results_or_targets="targets") %>%
  rename("fy21_targets"=targets,
         "fy21_cumulative"=cumulative)

df_nn_20<-df_raw %>% 
  filter(indicator=="TX_CURR",
         fiscal_year==2020) %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
       results_or_targets="targets") %>%
  rename("fy20_targets"=targets,
         "fy20_cumulative"=cumulative)

df_nn_19<-df_raw %>% 
  filter(indicator=="TX_CURR",
         fiscal_year==2019) %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
         results_or_targets="targets") %>%
  rename("fy19_targets"=targets,
         "fy19_cumulative"=cumulative)

df_nn_18<-df_raw %>% 
  filter(indicator=="TX_CURR",
         fiscal_year==2018) %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
         results_or_targets="targets") %>%
  rename("fy18_targets"=targets,
         "fy18_cumulative"=cumulative)

df_nn_17<-df_raw %>% 
  filter(indicator=="TX_CURR",
         fiscal_year==2017) %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
         results_or_targets="targets") %>%
  rename("fy17_targets"=targets,
         "fy17_cumulative"=cumulative)


df_nn_join <- bind_rows(df_nn_21, df_nn_20, df_nn_19, df_nn_18, df_nn_17) %>%
  mutate(fy21_nn_targets=fy21_targets-fy20_cumulative,
         fy20_nn_targets=fy20_targets-fy19_cumulative,
         fy19_nn_targets=fy19_targets-fy18_cumulative,
         fy18_nn_targets=fy18_targets-fy17_cumulative) %>%
  select(-qtr1, -qtr2, -qtr3, -qtr4, -fy21_targets, -fy20_targets, -fy19_targets, -fy18_targets, -fy17_targets,
         -fy21_cumulative, -fy20_cumulative, -fy19_cumulative, -fy18_cumulative, -fy17_cumulative) %>% 
  View()


#Check
df_nn_join %>% 
  select(-psnuuid, -sex, -statushiv, -statustb, -statuscx, -hiv_treatment_status, -trendsfine) %>% 
  View()



df_nnt <- df_raw %>% 
  filter(indicator=="TX_CURR") %>% 
  mutate(indicator = replace(indicator, indicator %in% c("TX_CURR"), "TX_NET_NEW"),
         results_or_targets="targets",
         fy21targets=ifelse(fiscal_year=="2021", targets, NA_real_),
         fy20targets=ifelse(fiscal_year=="2020", targets, NA_real_),
         fy19targets=ifelse(fiscal_year=="2019", targets, NA_real_),
         fy18targets=ifelse(fiscal_year=="2018", targets, NA_real_),
         fy20cum=ifelse(fiscal_year=="2020", cumulative, NA_real_),
         fy19cum=ifelse(fiscal_year=="2019", cumulative, NA_real_),
         fy18cum=ifelse(fiscal_year=="2018", cumulative, NA_real_),
         fy17cum=ifelse(fiscal_year=="2017", cumulative, NA_real_),
         fy21_nn_targets=fy21targets-fy20cum,
         fy20_nn_targets=fy20targets-fy19cum,
         fy19_nn_targets=fy19targets-fy18cum,
         fy18_nn_targets=fy18targets-fy17cum) %>%
  select(-source_name, -modality, -statuscx, -statustb, -statushiv, -sex, -indicatortype) %>% 
  View()


#Scratch/Notes
  
#function grouping by vars I have, create cum sum for snapshot indicators, take original
#df, take the old data drop old quarter data and append new one
#For net new use lag indicator, can take from quarter before. TX_CURR target- TX_CURR q4
#Group by PSNU, with only TX_CURR in data frame, target-lag cumulative. Default lag is one period
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
  


