## PROJECT:  bartlett
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE: Reshape numeric variables and create targets in line with Tableau prep flow
## CREATED:  4.30.21

df_targets <- df_raw %>%
  rename("q1"=qtr1,
         "q2"=qtr2,
         "q3"=qtr3,
         "q4"=qtr4) %>%
  pivot_longer(c("q1", "q2", "q3", "q4", "targets", "cumulative"),
               names_to="quarter",
               values_drop_na = TRUE) %>% 
  mutate(quarter_2=paste(fiscal_year, quarter)) %>%
  mutate(quarter_2=replace(quarter_2, quarter_2 %in% c(""), "")) %>% 
  mutate(quarter = replace(quarter, quarter %in% c("q1", "q2", "q3", "q4"), "results")) %>%
  rename("results_or_targets"=quarter) %>%
  mutate(targets=ifelse(results_or_targets=="targets", value, NA_real_)) %>%
  ##Working on line below
  mutate(quarter_2=case_when(str_detect(quarter_2,"targets") ~ "q4", TRUE ~ quarter_2)) %>% 
  View()


#Reference
df_raw %>% 
  group_by(indicator, fiscal_year, standardizeddisaggregate) %>%
  dplyr::summarise(across(c(targets), sum, na.rm=TRUE)) %>%
  View()
  
df_targets %>% 
  group_by(indicator, fiscal_year, standardizeddisaggregate) %>%
  dplyr::summarise(across(c(targets), sum, na.rm=TRUE)) %>%
  View()

