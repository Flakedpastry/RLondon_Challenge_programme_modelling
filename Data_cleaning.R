install.packages("tidyverse")
install.packages("tidyquant")
install.packages("fixest")
install.packages("readxl")
install.packages("dplyr")
install.packages("Synth")
install.packages("modelsummary")
install.packages("gt")

library("readxl")
library("tidyverse")
library("tidyquant")
library("fixest")
library("dplyr")
library("Synth")
library("modelsummary")
library("gt")

#Cleaning data

post_data_early <- read_xlsx("GCSEDATA_2000to2015.xlsx", sheet = "2000_2005", col_names = FALSE)
post_data_later <- read_xlsx("GCSEDATA_2000to2015.XLSX", sheet = "2006_2015", col_names = FALSE)

cleaned_post <- post_data_early %>% slice(3:n()) %>% select(1:8)

colnames(cleaned_post) <- c(
  "la",
  "lacode",
  "y2000",
  "y2001",
  "y2002",
  "y2003",
  "y2004",
  "y2005"
)

cleaned_post <- cleaned_post %>% 
  filter(!is.na(lacode)) %>%
  filter(str_detect(lacode, "^E[0-9]+$"))

cleaned_post <- cleaned_post %>%
  filter(str_detect(lacode, "^E(06|07|08|09|10)"))

cleaned_post <- cleaned_post %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "pct_5ac"
  ) %>%
  mutate(
    year = str_remove(year, "y"),
    year = as.integer(year),
    pct_5ac_ = as.numeric(pct_5ac)
  )


cleaned_post <- cleaned_post %>% 
  mutate(
    london = if_else(str_detect(lacode, "^E09"),1,0)
  )

cleaned_post <- cleaned_post %>%
  select(la,lacode,year,pct_5ac_,london) %>%
  rename(pct_5ac = pct_5ac_)
  

cleaned_post2 <- post_data_later %>% slice(7:n()) %>% select(1:11)

colnames(cleaned_post2) <- c(
  "la",
  "lacode",
  "y2006",
  "y2007",
  "y2008",
  "y2009",
  "y2010",
  "y2011",
  "y2012",
  "y2013",
  "y2015"
)

cleaned_post2 <- cleaned_post2 %>% 
  filter(!is.na(lacode)) %>%
  filter(str_detect(lacode, "^E[0-9]+$"))

cleaned_post2 <- cleaned_post2 %>%
  filter(str_detect(lacode, "^E(06|07|08|09|10)"))

cleaned_post2 <- cleaned_post2 %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "pct_5ac"
  ) %>%
  mutate(
    year = str_remove(year, "y"),
    year = as.integer(year),
    pct_5ac_ = as.numeric(pct_5ac)
  )

cleaned_post2 <- cleaned_post2 %>%
  mutate(
    london = if_else(str_detect(lacode, "^E09"),1,0)
  )

cleaned_post2 <- cleaned_post2 %>%
  select(la,lacode,year,pct_5ac_,london) %>%
  rename(pct_5ac = pct_5ac_)

gcse_panel_data <- bind_rows(cleaned_post, cleaned_post2)
gcse_panel_data <- gcse_panel_data %>%
  arrange(lacode,year)

gcse_panel_data <- gcse_panel_data %>%
  mutate(
    post = if_else(year >= 2003,1,0)
  )

gcse_panel_data <- gcse_panel_data %>%
  mutate(
    DiD = london * post
  )

#merging controls
income_dt <- read_xlsx("Income_data/Final_data_2000to2015.xlsx", sheet = "Final_Income_99-15", col_names = FALSE)
claim_cnt <- read_xlsx("Claimant_cnt_data/Final_Claimant_cnt_Data.xlsx", col_names = FALSE)
pop <- read_xlsx("Population_data_2000to2015.xlsx", col_names = FALSE)

cleaned_inc <- income_dt %>% slice(3:n()) %>% select(1:17)
cleaned_claim_cnt <- claim_cnt %>% slice(3:n()) %>% select(1:17)
cleaned_pop <- pop %>% slice(3:n()) %>% select(1:17)

colnames(cleaned_inc) <- c(
  "la",
  "lacode",
  "y2000",
  "y2001",
  "y2002",
  "y2003",
  "y2004",
  "y2005",
  "y2006",
  "y2007",
  "y2008",
  "y2009",
  "y2010",
  "y2011",
  "y2012",
  "y2013",
  "y2015"
)

cleaned_inc <-cleaned_inc %>% 
  filter(!is.na(lacode)) %>%
  filter((str_detect(lacode, "^E[0-9]+$")))

cleaned_inc <- cleaned_inc %>%
  filter(str_detect(lacode, "^E(06|07|08|09|10)"))

cleaned_inc <- cleaned_inc %>% 
  mutate(across(starts_with("y"), ~na_if(.x,"x")))

cleaned_inc <- cleaned_inc %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "income_val"
  ) %>%
  mutate(
    year = str_remove(year, "y"),
    year = as.integer(year),
    income_la = as.numeric(income_val)
  )

cleaned_inc <- cleaned_inc %>%
  select("la","lacode", "year", "income_la") %>%
  rename(income = income_la)

colnames(cleaned_claim_cnt) <- c(
  "la",
  "lacode",
  "y2000",
  "y2001",
  "y2002",
  "y2003",
  "y2004",
  "y2005",
  "y2006",
  "y2007",
  "y2008",
  "y2009",
  "y2010",
  "y2011",
  "y2012",
  "y2013",
  "y2015"
)

cleaned_claim_cnt <-cleaned_claim_cnt %>% 
  filter(!is.na(lacode)) %>%
  filter((str_detect(lacode, "^E[0-9]+$")))

cleaned_claim_cnt <- cleaned_claim_cnt %>%
  filter(str_detect(lacode, "^E(06|07|08|09|10)"))

cleaned_claim_cnt <- cleaned_claim_cnt %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "claim_vals"
  ) %>%
  mutate(
    year = str_remove(year, "y"),
    year = as.integer(year),
    claim_num = as.numeric(claim_vals)
  )

cleaned_claim_cnt <- cleaned_claim_cnt %>%
  select("la","lacode", "year", "claim_num") %>%
  rename(claim_cnt = claim_num)


colnames(cleaned_pop) <- c(
  "la",
  "lacode",
  "y2000",
  "y2001",
  "y2002",
  "y2003",
  "y2004",
  "y2005",
  "y2006",
  "y2007",
  "y2008",
  "y2009",
  "y2010",
  "y2011",
  "y2012",
  "y2013",
  "y2015"
)

cleaned_pop <-cleaned_pop %>% 
  filter(!is.na(lacode)) %>%
  filter((str_detect(lacode, "^E[0-9]+$")))

cleaned_pop <- cleaned_pop %>%
  filter(str_detect(lacode, "^E(06|07|08|09|10)"))

cleaned_pop <- cleaned_pop %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "pop_vals"
  ) %>%
  mutate(
    year = str_remove(year, "y"),
    year = as.integer(year),
    pop_num = as.numeric(pop_vals)
  )

cleaned_pop <- cleaned_pop %>%
  select("la","lacode", "year", "pop_num") %>%
  rename(pop_la = pop_num)

gm_la <- c(
  "E08000001",
  "E08000002",
  "E08000003",
  "E08000004",
  "E08000005",
  "E08000006",
  "E08000007",
  "E08000008",
  "E08000009",
  "E08000010"
)

bc_la <- c(
  "E08000027",
  "E08000028",
  "E08000030",
  "E08000031"
)

exclude_la <- c(gm_la, bc_la)

gcse_panel_restricted <- gcse_panel_data %>% 
  filter(!lacode %in% exclude_la)

dropped_la <- c(
  "E06000049", #Cheshire East
  "E06000050", #Cheshire West & Chester
  "E06000055", #Bedford
  "E06000056", #Central Bedfordshire
  "E06000052", #Cornwall
  "E06000053" #Isle of Scilly
)


gcse_controls <- gcse_panel_restricted %>%
  left_join(cleaned_inc %>% select(lacode, year, income), by = c("lacode", "year")) %>%
  left_join(cleaned_claim_cnt %>% select(lacode, year, claim_cnt), by = c("lacode", "year")) %>%
  left_join(cleaned_pop %>% select(lacode, year, pop_la), by = c("lacode", "year"))

gcse_controls <- gcse_controls %>%
  mutate(claimant_rate = 100 * (claim_cnt / pop_la))

gcse_controls <- gcse_controls %>%
  filter(!lacode %in% dropped_la)

#Heterogeneity by Deprivation

income_dep <- read_xls("Deprivation/councilcountysummaries.xls", sheet = "Final deprivation", col_names = FALSE)

cleaned_income_dep <- income_dep %>% slice(5:n())
colnames(cleaned_income_dep) <- c(
  "la",
  "lacode",
  "income_depr"
)

cleaned_income_dep <- cleaned_income_dep %>%
  filter(!is.na(income_depr))

pop_2001 <- cleaned_pop %>% 
  filter(year == 2001) %>%
  select(lacode, pop_la) %>%
  rename(pop_2001 = pop_la)
  

income_dep_merged <- cleaned_income_dep %>%
  left_join(pop_2001, by = "lacode")

income_dep_merged <- income_dep_merged %>%
  mutate(income_depreciation = as.numeric(income_depr),
         dep_income_rate = income_depreciation / pop_2001)

med_dep <- median(income_dep_merged$dep_income_rate, na.rm = TRUE)

income_dep_merged <- income_dep_merged %>%
  mutate(high_dep = if_else(dep_income_rate > med_dep, 1, 0))

final_income_dep <- income_dep_merged %>%
  select("lacode", "dep_income_rate", "high_dep")


gcse_heterogeneity <- gcse_panel_restricted %>%
  left_join(final_income_dep, by = "lacode")

gcse_heterogeneity <- gcse_heterogeneity %>%
  mutate(
    dep_income_c = dep_income_rate - mean(dep_income_rate, na.rm = TRUE),
    DiD_dep = DiD * dep_income_c,
    DiD_highdep = DiD * high_dep
  )

reg_het1 <- feols(pct_5ac ~ DiD + DiD_highdep | lacode + year, data = gcse_heterogeneity, cluster = ~lacode)
summary(reg_het1)

reghet2 <- feols(pct_5ac ~ DiD + DiD_dep | lacode + year, data = gcse_heterogeneity, cluster = ~lacode)
summary(reghet2)

#Regressions

reg1 <- feols(pct_5ac ~ london + post + DiD, data = gcse_panel_data, cluster = ~lacode)
summary(reg1)

reg1_rest <- feols(pct_5ac ~ london + post + DiD, data = gcse_panel_restricted, cluster = ~lacode)
summary(reg1_rest)

reg1_restfe <- feols(pct_5ac ~ DiD | lacode + year, data = gcse_controls, cluster = ~lacode)
summary(reg1_restfe)

regcon_1 <- feols(pct_5ac ~ DiD + income + claimant_rate + pop_la | lacode + year, data = gcse_controls, cluster = ~lacode)
summary(regcon_1)

reg2 <- feols(pct_5ac ~ i(year, london, ref = 2003) | lacode + year, data = gcse_panel_data)
summary(reg2)

reg2_rest <- feols(pct_5ac ~ i(year, london, ref = 2003)|lacode + year, data = gcse_panel_restricted, cluster = ~lacode)
summary(reg2_rest)

regcon_2 <- feols(pct_5ac ~ i(year, london, ref = 2003) + income + pop_la + claim_cnt | lacode + year, data = gcse_controls, cluster = ~lacode)
summary(regcon_2)

iplot(regcon_2)

dyn_reg1 <- feols(
  pct_5ac ~ i(year, london, ref = 2003),
  data = gcse_panel_data,
  cluster = ~lacode
)

iplot(dyn_reg1)

dyn_reg1_rest <- feols(
  pct_5ac ~ i(year, london, ref = 2003) | lacode + year,
  data = gcse_panel_restricted,
  cluster ~lacode
)
summary(dyn_reg1_rest)

iplot(dyn_reg1_rest)

etable(reg2_rest)





#Robustness checks

london_la <- c(
  "E09000007",
  "E09000012",
  "E09000013",
  "E09000014",
  "E09000019",
  "E09000020",
  "E09000022",
  "E09000023",
  "E09000025",
  "E09000028",
  "E09000030",
  "E09000032",
  "E09000033",
  "E09000002",
  "E09000003",
  "E09000004",
  "E09000005",
  "E09000006",
  "E09000008",
  "E09000009",
  "E09000010",
  "E09000011",
  "E09000015",
  "E09000016",
  "E09000017",
  "E09000018",
  "E09000021",
  "E09000024",
  "E09000026",
  "E09000027",
  "E09000029",
  "E09000031"
)

Manchester_robchk_data <- gcse_panel_data 
BC_robchk_data <- gcse_panel_data

Manchester_robchk_data <- Manchester_robchk_data %>%
  mutate(
    Manchester = if_else(lacode %in% gm_la,1,0),
    post_2008 = if_else(year >= 2008, 1, 0),
    DiD_manc = post_2008*Manchester
  )

Manchester_robchk_data <- Manchester_robchk_data %>%
  select("la","lacode", "year", "pct_5ac", "Manchester", "post_2008", "DiD_manc")

exclude_la_manch <- c(london_la, bc_la)

Manchester_robchk_data_restricted <- Manchester_robchk_data %>%
  filter(!lacode %in% exclude_la_manch)
  
regManc1_restfe <- feols(pct_5ac ~ DiD_manc | lacode + year, data = Manchester_robchk_data_restricted, cluster = ~lacode)
summary(regManc1_restfe)

dyn_regManc1_restfe <- feols(pct_5ac ~ i(year, Manchester, ref = 2007) | lacode + year,
                             data = Manchester_robchk_data_restricted, cluster = ~lacode)
iplot(dyn_regManc1_restfe)
etable(dyn_regManc1_restfe)

BC_robchk_data <- BC_robchk_data %>%
  mutate(
    BC = if_else(lacode %in% bc_la,1,0),
    post_2008 = if_else(year >= 2008,1,0),
    DiD_BC = post_2008*BC
  )

BC_robchk_data <- BC_robchk_data %>%
  select("la", "lacode", "year", "pct_5ac", "BC", "post_2008", "DiD_BC")

exclude_la_bc <- c(london_la, gm_la)

BC_robchk_data_restricted <- BC_robchk_data %>%
  filter(!lacode %in% exclude_la_bc)

regBC1_restfe <- feols(pct_5ac ~ DiD_BC | lacode + year, data = BC_robchk_data_restricted, cluster = ~lacode)
summary(regBC1_restfe)

dynBC1_restfe <- feols(pct_5ac ~ i(year, BC, ref = 2007) | lacode + year,
                       data = BC_robchk_data_restricted, cluster = ~lacode)
iplot(dynBC1_restfe)
etable(dynBC1_restfe)


#Synthetic control

london_avg <- gcse_controls %>%
  select(la, lacode, year, pct_5ac, income, pop_la, claimant_rate)%>%
  filter(lacode %in% london_la) %>%
  group_by(year) %>%
  summarise(pct_5ac = weighted.mean(pct_5ac, pop_la,na.rm = TRUE),
            income = weighted.mean(income, pop_la, na.rm = TRUE),
            claimant_rate = weighted.mean(claimant_rate, pop_la, na.rm = TRUE),
            pop_la = sum(pop_la, na.rm = TRUE)) %>%
  mutate(la = "London", lacode = "LONDON") %>%
  filter(year != 2002)


synth_control_data <- gcse_controls %>%
  filter(!lacode %in% london_la) %>%
  select(la, lacode, year, pct_5ac, income, claimant_rate, pop_la) %>%
  filter(year != 2002)

synth_final_data <-bind_rows(london_avg, synth_control_data)

synth_final_data <- synth_final_data %>%
  filter(lacode != "E09000001") #Removing city of london

synth_final_data <- synth_final_data %>%
  mutate(unit_id = as.numeric(factor(la)))

london_id <- synth_final_data %>% 
  distinct(la, unit_id) %>%
  filter(la == "London") %>%
  pull(unit_id)

control_ids <- synth_final_data %>%
  distinct(la, unit_id) %>%
  filter(la != "London") %>%
  pull(unit_id)

synth_prep <- dataprep(
  foo = as.data.frame(synth_final_data),
  predictors = c("income", "claimant_rate"),
  predictors.op = "mean",
  special.predictors = list(
    list("pct_5ac", 2000, "mean"),
    list("pct_5ac", 2001, "mean")
  ),
  dependent = "pct_5ac",
  unit.variable = "unit_id",
  unit.names.variable = "la",
  time.variable = "year",
  treatment.identifier = london_id,
  controls.identifier = control_ids,
  time.predictors.prior = 2000:2001,
  time.optimize.ssr = 2000:2001,
  time.plot = c(2000, 2001, 2003:2013,2015)
)

synth_output <- synth(synth_prep)

synth.tab(dataprep.res = synth_prep, synth.res = synth_output)

path.plot(
  synth.res = synth_output,
  dataprep.res = synth_prep,
  Ylab = "GCSE attainment",
  Xlab = "Year",
  Main = "London vs synthetic London"
)

gaps.plot(
  synth.res = synth_output,
  dataprep.res = synth_prep,
  Ylab = "GCSE gap",
  Xlab = "Year",
  Main = "Gaps: London - Synthetic London"
) 

synth_tables <- synth.tab(dataprep.res = synth_prep, synth.res = synth_output)

#Descriptive summary

desc_summary <- gcse_controls %>%
  summarise(
    "GCSE attainment (%)" = list(c(
      Mean = mean(pct_5ac, na.rm = TRUE),
      SD = sd(pct_5ac, na.rm = TRUE),
      Min = min(pct_5ac, na.rm = TRUE),
      Max = max(pct_5ac, na.rm = TRUE)
    )),
    "Income" = list(c(
      Mean = mean(income, na.rm = TRUE),
      SD = sd(income, na.rm = TRUE),
      Min = min(income, na.rm = TRUE),
      Max = max(income, na.rm = TRUE)
    )),
    "Claimant Rate" = list(c(
      Mean = mean(claimant_rate, na.rm = TRUE),
      SD = sd(claimant_rate, na.rm = TRUE),
      Min = min(claimant_rate, na.rm = TRUE),
      Max = max(claimant_rate, na.rm = TRUE)
    )),
    "Population" = list(c(
      Mean = mean(pop_la, na.rm = TRUE),
      SD = sd(pop_la, na.rm = TRUE),
      Min = min(pop_la, na.rm = TRUE),
      Max = max(pop_la, na.rm = TRUE)
    ))
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Stats") %>%
  unnest_wider(Stats)

desc_gt <- desc_summary %>%
  gt() %>%
  fmt_number(columns = c(Mean, SD, Min, Max), decimals = 2) %>%
  tab_header(
    title = "Descriptive Statistics",
    subtitle = "Main Estimation Sample"
  )



#Presentation

models_main <- list(
  "Restricted DiD" = reg1_rest,
  "FE" = reg1_restfe,
  "FE + Controls" = regcon_1
)

tab_main <- modelsummary(
  models_main,
  coef_map = c(
    "DiD" = "London x Post",
    "income" =  "Income",
    "claimant_rate" = "Claimant Rate",
    "pop_la" = "Population"
  ),
  gof_map = c("nobs", "r.squared", "adj r.squared"),
  stars = TRUE,
  output = "gt"
)

models_het <- list(
  "Heterogeneity" = reg_het1
)

tab_het <- modelsummary(
  models_het,
  coef_map = c(
    "DiD" = "London x Post",
    "DiD_highdep" = "London x Post x High Deprivation"
  ),
  gof_map = c("nobs", "r.squared", "adjusted r.squared"),
  stars = TRUE,
  output = "gt"
)

models_robust <- list(
  "London FE" = reg1_restfe,
  "Manchester" = regManc1_restfe,
  "Black Country FE" = regBC1_restfe
)

tab_robust <- modelsummary(
  models_robust,
  coef_map = c(
    "DiD" = "London x Post",
    "DiD_manc" = "Manchester x Post_2008",
    "DiD_BC" = "Black Country x Post_2008"
  ),
  gof_map = c("nobs", "r.squared", "adjusted r.squared"),
  stars = TRUE,
  output = "gt"
)

png("figure_dynamic_london.png", width = 2200, height = 1200, res = 300)

iplot(
  dyn_reg1_rest,
  main = "Dynamic Effects of London Challenge on GCSE Attainment",
  xlab = "Year",
  ylab = "Estimated effect on pct_5ac"
)

dev.off()

png("figure_dynamic_black_country.png", width = 2200, height = 1200, res = 300)

iplot(
  dynBC1_restfe,
  main = "Dynamic Effects of the Black Country Challenge on GCSE Attainment",
  xlab = "Year",
  ylab = "Coefficient estimate"
)

dev.off()

png("GCSE_attainment_synthetic.png", width = 2200, height = 1200, res = 300)

path.plot(
  synth.res = synth_output,
  dataprep.res = synth_prep,
  Ylab = "GCSE attainment",
  Xlab = "Year",
  Main = "Synthetic Control Path Plot: London and Synthetic London"
)

dev.off()


#Appendix data:
#Dynamic regressions:

dyn_labels <- c(
  "year::2000:london" = "2000 × London",
  "year::2001:london" = "2001 × London",
  "year::2004:london" = "2004 × London",
  "year::2005:london" = "2005 × London",
  "year::2006:london" = "2006 × London",
  "year::2007:london" = "2007 × London",
  "year::2008:london" = "2008 × London",
  "year::2009:london" = "2009 × London",
  "year::2010:london" = "2010 × London",
  "year::2011:london" = "2011 × London",
  "year::2012:london" = "2012 × London",
  "year::2013:london" = "2013 × London",
  "year::2015:london" = "2015 × London"
)


models_dynamic <- list(
  "Full Sample FE" = reg2,
  "Restricted FE" = reg2_rest,
  "Restricted FE + Controls" = regcon_2
)

tab_dynamic <- modelsummary(
  models_dynamic,
  coef_map = dyn_labels,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = TRUE,
  output = "gt"
) %>%
  tab_header(
    title = md("**Appendix Table A1.1: Dynamic Regression Results**"),
    subtitle = md("Full sample, restricted sample, and restricted sample with controls")
  )

tab_dynamic <- tab_dynamic %>%
  tab_source_note(
    source_note = "Notes: Standard errors clustered at the local authority level. Omitted reference year is 2003."
  )

dyn_map_manc <- c(
  "year::2000:Manchester" = "2000 × Manchester",
  "year::2001:Manchester" = "2001 × Manchester",
  "year::2003:Manchester" = "2003 × Manchester",
  "year::2004:Manchester" = "2004 × Manchester",
  "year::2005:Manchester" = "2005 × Manchester",
  "year::2006:Manchester" = "2006 × Manchester",
  "year::2007:Manchester" = "2007 × Manchester",
  "year::2009:Manchester" = "2009 × Manchester",
  "year::2010:Manchester" = "2010 × Manchester",
  "year::2011:Manchester" = "2011 × Manchester",
  "year::2012:Manchester" = "2012 × Manchester",
  "year::2013:Manchester" = "2013 × Manchester",
  "year::2015:Manchester" = "2015 × Manchester"
)

tab_dyn_manc <- modelsummary(
  list("Manchester Dynamic FE" = dyn_regManc1_restfe),
  coef_map = dyn_map_manc,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = TRUE,
  output = "gt"
) %>%
  tab_header(
    title = md("**Appendix Table A1.2: Dynamic Robustness Results, Manchester**"),
    subtitle = md("Event-study estimates for the Manchester placebo treatment")
  ) %>%
  tab_source_note(
    source_note = "Notes: Standard errors clustered at the local authority level. Omitted reference year is 2008."
  )


dyn_map_bc <- c(
  "year::2000:BC" = "2000 × Black Country",
  "year::2001:BC" = "2001 × Black Country",
  "year::2003:BC" = "2003 × Black Country",
  "year::2004:BC" = "2004 × Black Country",
  "year::2005:BC" = "2005 × Black Country",
  "year::2006:BC" = "2006 × Black Country",
  "year::2007:BC" = "2007 × Black Country",
  "year::2009:BC" = "2009 × Black Country",
  "year::2010:BC" = "2010 × Black Country",
  "year::2011:BC" = "2011 × Black Country",
  "year::2012:BC" = "2012 × Black Country",
  "year::2013:BC" = "2013 × Black Country",
  "year::2015:BC" = "2015 × Black Country"
)

tab_dyn_bc <- modelsummary(
  list("Black Country Dynamic FE" = dynBC1_restfe),
  coef_map = dyn_map_bc,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = TRUE,
  output = "gt"
) %>%
  tab_header(
    title = md("**Appendix Table A1.3: Dynamic Robustness Results, Black Country**"),
    subtitle = md("Event-study estimates for the Black Country placebo treatment")
  ) %>%
  tab_source_note(
    source_note = "Notes: Standard errors clustered at the local authority level. Omitted reference year is 2008."
  )

png("figure_dynamic_london_fullsample.png", width = 2500, height = 1200, res = 300)

iplot(
  dyn_reg1,
  main = "Dynamic Effects of the London Challenge on GCSE Attainment (Unrestricted)",
  xlab = "Year",
  ylab = "Coefficient estimate"
)
mtext("Appendix Figure A1.4", side = 3, line = 3, font = 2)

dev.off() 

png("figure_dynamic_Manchester.png", width = 2200, height = 1200, res = 300)

iplot(
  dyn_regManc1_restfe,
  main = "Dynamic Effects of the City Challenge on Manchester GCSE Attainment",
  xlab = "Year",
  ylab = "Coefficient estimate"
)
mtext("Appendix Figure A1.5", side = 3, line = 3, font = 2)

dev.off()

png("figure_synth_gap.png", width = 2200, height = 1200, res = 300)

gaps.plot(
  synth.res = synth_output,
  dataprep.res = synth_prep,
  Ylab = "GCSE attainment gap",
  Xlab = "Year",
  Main = "Gap Between London and Synthetic London in GCSE Attainment",
)
mtext("Appendix Figure A1.6", side = 3, line = 3, font = 2)

dev.off()

weights_df <- as.data.frame(synth_tables$tab.w) %>%
  rownames_to_column("Donor Unit") %>%
  filter(w.weights > 0) %>%
  arrange(desc(w.weights))

n_donors <- nrow(weights_df)

tab_weights <- weights_df %>%
  slice_head(n = 10) %>%
  gt() %>%
  fmt_number(columns = w.weights, decimals = 4) %>%
  tab_header(
    title = md("**Appendix Table A1.7a: Synthetic Control Donor Weights**"),
    subtitle = md("Ten largest donor weights used to construct Synthetic London")
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Notes: Table reports the 10 largest non-zero donor weights. ",
      "Synthetic London is constructed from ", n_donors, " donor units with non-zero weights in total."
    )
  )

tab_predictors <- as.data.frame(synth_tables$tab.pred) %>%
  tibble::rownames_to_column("Predictor") %>%
  gt() %>%
  tab_header(
    title = md("**Appendix Table A1.7b: Synthetic Control Predictor Balance**"),
    subtitle = md("Comparison of London and Synthetic London on pre-treatment predictors")
  )



#Saving data
gtsave(tab_main, "table_main.html")
gtsave(tab_het, "table_heterogeneity.html")
gtsave(tab_robust, "table_robustness.html")
gtsave(desc_gt, "descriptive_statistics_table.html")
gtsave(tab_dynamic, "table_dynamic_reg2_rest.html")
gtsave(tab_dyn_manc, "table_dynamic_manchester.html")
gtsave(tab_dyn_bc, "table_dynamic_black_country.html")
gtsave(tab_weights, "appendix_table_synth_weights.html")
gtsave(tab_predictors, "appendix_table_synth_predictors.html")
