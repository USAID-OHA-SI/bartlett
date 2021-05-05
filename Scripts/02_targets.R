## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Reshape numeric variables and create targets, cumulative vars in line with Tableau prep flow
## CREATED:  4.30.21

df_targets <- df_raw %>%
  #Rename quarters to match Noah's flow and pivot numeric fields
  rename("q1"=qtr1,
         "q2"=qtr2,
         "q3"=qtr3,
         "q4"=qtr4) %>%
  pivot_longer(c("q1", "q2", "q3", "q4", "targets", "cumulative"),
               names_to="qtr",
               values_drop_na = TRUE) %>%
  #Create quarter variable as combo FY and qtr, create results_or_targets label based on qtr
  mutate(quarter=paste(fiscal_year, qtr)) %>%
  mutate(qtr = replace(qtr, qtr %in% c("q1", "q2", "q3", "q4"), "results")) %>%
  rename("results_or_targets"=qtr) %>%
  #Create cumulative + targets + results columns and make quarter = q4 for targets
  mutate(cumulative=ifelse(results_or_targets=="cumulative", value, NA_real_)) %>%
  mutate(targets=ifelse(results_or_targets=="targets", value, NA_real_)) %>%
  mutate(results=ifelse(results_or_targets=="results", value, NA_real_)) %>%
  mutate(quarter=str_replace(quarter, "targets", "q4")) %>%
  View()

# remove quarter value for cumulatives, also do the same for targets?
# create current quarter col

#Reference
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
  
  df_targets %>%
    filter(indicator=="TX_NEW") %>% 
    View()



