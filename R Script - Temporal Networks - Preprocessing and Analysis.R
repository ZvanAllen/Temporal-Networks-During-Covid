################################################################################
################ iCARE Project - Temporal Network Analysis #####################
################################################################################


# README ####

#### Outputs from script:
# 1) data.wide <- a dataset in wide format
# 2) data.long <- a dataset in long format

##### This script will execute provide that:
# 1) The R Project folder contains all necessary datasets
# 2) Longitudinal files are renamed via format: "iCARE Long [#] [EN/FR]"
# 3) Cross-sectional files are renamed via format:"iCARE CS [#] [EN/FR] [1/2]"
# 4) The tidyverse and panelr packages are installed and loaded

##### Workflow:
# 1) Longitudinal data sets are loaded and EN/FR merged; this produces datasets
#    data.8.long to data.11.long
# 2) Cross sectional data sets are loaded and EN/FR merged; this produces data sets
#    data.7.cs to data.10.cs
# 3) Participants' first responses on cross-sectional surveys are extracted and
#    added to the appropriate longitudinal data sets (e.g., those who completed
#    their first longitudinal survey in Wave 9 will have their data from data.8.cs 
#    extracted and added to data.8.long 
# 4) Longitudinal data sets data.7.long to data.11.long are merged by "clspcode"
#    into data.wide
# 5) Duplicate "clspcode" are removed from data.wide
# 6) data.wide is reshaped into long format and dataset is labeled data.long

##### For questions or concerns contact: vanallen.22@gmail.com (Zack van Allen)


# Load libraries ####
library(tidyverse) # data wrangling and visualization 
library(panelr) # covert wide data to long format
library(corrplot) # correlation plots
library(naniar) # replace_with_na function
library(gt) # nice tables
library(psych) # cronbach's alphas
library(ggdist) # boxplot/raincloud plts
library(psychonetrics) # network analysis for panel data
library(qgraph)

# Load longitudinal data sets ####

## Longitudinal Wave 8
data.8.en <- read_csv("iCARE Long 8 EN.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

data.8.fr <- read_csv("iCARE Long 8 FR.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

data.8.long <- merge(data.8.en, data.8.fr,  all = TRUE) %>%
  as_tibble()

## Longitudinal Wave 9
data.9.en <- read_csv("iCARE Long 9 EN.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

data.9.fr <- read_csv("iCARE Long 9 FR.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

data.9.long <- merge(data.9.en, data.9.fr,  all = TRUE) %>%
  as_tibble()

## Longitudinal Wave 10
data.10.en <- read_csv("iCARE Long 10 EN.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W10_", x)}) 

data.10.fr <- read_csv("iCARE Long 10 FR.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W10_", x)}) 

data.10.long <- merge(data.10.en, data.10.fr,  all = TRUE) %>%
  as_tibble()

## Longitudinal Wave 11
data.11.en <- read_csv("iCARE Long 11 EN.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W11_", x)}) 

data.11.fr <- read_csv("iCARE Long 11 FR.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W11_", x)}) 

data.11.long <- merge(data.11.en, data.11.fr,  all = TRUE) %>%
  as_tibble()


# Load cross-sectional data sets ####

## Cross sectional data wave 7
# load individual data sets
cs.7.en.1 <- read_csv("iCARE CS 7 EN 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W7_", x)}) 

cs.7.en.2 <- read_csv("iCARE CS 7 EN 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W7_", x)}) 

cs.7.fr.1 <- read_csv("iCARE CS 7 FR 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W7_", x)}) 

cs.7.fr.2 <- read_csv("iCARE CS 7 FR 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W7_", x)}) 

# merge french and english
cs.7.en <- merge(cs.7.en.1, cs.7.en.2,  all = TRUE) %>% as_tibble()
cs.7.fr <- merge(cs.7.fr.1, cs.7.fr.2,  all = TRUE) %>% as_tibble()

# final merge
data.7.cs <- merge(cs.7.en, cs.7.fr, all = TRUE) %>% as_tibble()

## Cross sectional data wave 8
# load individual data sets
cs.8.en.1 <- read_csv("iCARE CS 8 EN 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

cs.8.en.2 <- read_csv("iCARE CS 8 EN 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

cs.8.fr.1 <- read_csv("iCARE CS 8 FR 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

cs.8.fr.2 <- read_csv("iCARE CS 8 FR 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W8_", x)}) 

# merge french and english
cs.8.en <- merge(cs.8.en.1, cs.8.en.2,  all = TRUE) %>% as_tibble()
cs.8.fr <- merge(cs.8.fr.1, cs.8.fr.2,  all = TRUE) %>% as_tibble()

# final merge
data.8.cs <- merge(cs.8.en, cs.8.fr, all = TRUE) %>% as_tibble()


## Cross sectional data wave 9
# load individual data sets
cs.9.en.1 <- read_csv("iCARE CS 9 EN 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

cs.9.en.2 <- read_csv("iCARE CS 9 EN 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

cs.9.fr.1 <- read_csv("iCARE CS 9 FR 1.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

cs.9.fr.2 <- read_csv("iCARE CS 9 FR 2.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W9_", x)}) 

# merge french and english
cs.9.en <- merge(cs.9.en.1, cs.9.en.2,  all = TRUE) %>% as_tibble()
cs.9.fr <- merge(cs.9.fr.1, cs.9.fr.2,  all = TRUE) %>% as_tibble()

# final merge
data.9.cs <- merge(cs.9.en, cs.9.fr, all = TRUE) %>% as_tibble()

## Cross sectional data wave 10
# load individual data sets
cs.10.en <- read_csv("iCARE CS 10 EN.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W10_", x)}) 

cs.10.fr <- read_csv("iCARE CS 10 FR.csv") %>%
  rename_with(.cols = -clspcode, function(x){paste0("W10_", x)}) 

# final merge
data.10.cs <- merge(cs.10.en, cs.10.fr, all = TRUE) %>% as_tibble()

# Add first responses from cross-sectional to longitudinal ####

# cs 7 will be it's own data set (1st survey for those in wave 8)
# only those with clspcode are retained
data.7.long <-data.7.cs %>% drop_na(clspcode)

# cs 8 rows to be added to long.8 (1st survey for those in wave 9)
cs.8.select<-data.8.cs %>% drop_na(clspcode)
data.8.long <-merge(data.8.long, cs.8.select, all = TRUE) 

# cs 9 rows to be added to long.9 (1st survey for those in wave 10)
cs.9.select<-data.9.cs %>% drop_na(clspcode)
data.9.long <-merge(data.9.long, cs.9.select, all = TRUE) 

# cs 10 rows to be added to long.10 (1st survey for those in wave 11)
cs.10.select<-data.10.cs %>% drop_na(clspcode)
data.10.long <-merge(data.10.long, cs.10.select, all = TRUE)

# Remove [m=149] duplicate clspcode ####
# Participants have multiple clspcode when they exit/restart a survey
# When multiple clspcode the code below preserves most complete row
# Where most complete is determined by last survey page reached.

data.7.long <- data.7.long %>% # take the dataset, and then
  arrange(desc(W7_lastpage)) %>% # sort by highest 'last page' reached, and then
  distinct(clspcode, .keep_all= TRUE) # preserve first row of each clspcode duplicate

data.8.long <- data.8.long %>% 
  arrange(desc(W8_lastpage)) %>% 
  distinct(clspcode, .keep_all= TRUE)

data.9.long <- data.9.long %>% 
  arrange(desc(W9_lastpage)) %>% 
  distinct(clspcode, .keep_all= TRUE)

data.10.long <- data.10.long %>% 
  arrange(desc(W10_lastpage)) %>% 
  distinct(clspcode, .keep_all= TRUE)

data.11.long <- data.11.long %>% 
  arrange(desc(W11_lastpage)) %>% 
  distinct(clspcode, .keep_all= TRUE)

# Merge data into wide format (waves 7-11) ####

x.1 <- merge(data.7.long, data.8.long, by = "clspcode", all=TRUE)
x.2 <- merge(x.1, data.9.long, by = "clspcode", all=TRUE)
x.3 <- merge(x.2, data.10.long, by = "clspcode", all = TRUE)

# Longitudinal data set in wide format ####

data.wide <- merge(x.3, data.11.long, by = "clspcode", all = TRUE) %>% as_tibble()

write.csv(data.wide,"C:...csv", row.names = T)

# Longitudinal data set in long format ####

data.long <- long_panel(data.wide, prefix = "W", suffix = "_", 
                        label_location = "beginning", begin = 7, end = 11) 

write.csv(data.long,"C:/Users/...iCARE Longitudinal Long.csv", row.names = T)


# Data set: prep health variables ####
hb.data.long <- data.long %>%
  select(clspcode, lang, sex, age, canprov, edu, lastpage, hoinc, "hebehch[SQ001]", "hebehch[SQ002]",
         "hebehch[SQ003]", "hebehch[SQ004]", "hebehch[SQ005]", "hebehch[SQ006]",
         "actfreq[SQ001]","actfreq[SQ003]","actfreq[SQ028]", "actfreq[SQ007]",
         "actfreq[SQ008]", "actfreq[SQ009]","actfreq[SQ010]","actfreq[SQ014]", 
         "actfreq[SQ025]", "actfreq[SQ026]", "actfreq[SQ027]","actfreq[SQ020]",
         "impacvd[SQ001]", "impacvd[SQ002]", "impacvd[SQ003]", "impacvd[SQ004]",
         qualife) %>%
  rename(phys.act = "hebehch[SQ001]", healthy.diet = "hebehch[SQ002]", 
         alcohol = "hebehch[SQ003]", cigarette = "hebehch[SQ004]",
         vaping = "hebehch[SQ005]", drugs = "hebehch[SQ006]",
         wash.hands ="actfreq[SQ001]", indoor.mask = "actfreq[SQ003]",
         outside.mask = "actfreq[SQ028]", social.dist = "actfreq[SQ007]",
         stay.home = "actfreq[SQ008]", quarantine.travel = "actfreq[SQ009]",
         quarantine.sick = "actfreq[SQ010]", avoid.bars = "actfreq[SQ014]",
         avoid.large.gather = "actfreq[SQ025]", avoid.small.gather = "actfreq[SQ026]",
         avoid.indoor.gather =  "actfreq[SQ027]", avoid.ne.travel = "actfreq[SQ020]",
         nervous = "impacvd[SQ001]", sad = "impacvd[SQ002]", lonely = "impacvd[SQ003]",
         irritable = "impacvd[SQ004]") %>%
  filter(wave != 7)%>% # exclude wave 7 for small n
  ungroup()

# convert to panel_data object for easy conversion to wide
hb.data.long <- panel_data(hb.data.long, id = clspcode, wave = "wave")

# convert back to wide for network analysis
hb.data.wide <- widen_panel(hb.data.long, separator = "_", )

# Data set: cleaning and scoring  ####

hb.network.data.full <- hb.data.wide %>%
  select(clspcode, lang_8, sex_8, age_8, canprov_8, edu_8, hoinc_8,
         phys.act_8, healthy.diet_8, alcohol_8, cigarette_8, vaping_8, drugs_8,
         wash.hands_8, social.dist_8, nervous_8, outside.mask_8, indoor.mask_8, quarantine.sick_8,
         quarantine.travel_8, avoid.large.gather_8,avoid.indoor.gather_8, avoid.small.gather_8, avoid.ne.travel_8, stay.home_8,
         sad_8, irritable_8, lonely_8,qualife_8,
         phys.act_9, healthy.diet_9, alcohol_9, cigarette_9, vaping_9, drugs_9,
         wash.hands_9, social.dist_9, nervous_9,outside.mask_9,indoor.mask_9,quarantine.sick_9,
         quarantine.travel_9,avoid.large.gather_9,avoid.indoor.gather_9, avoid.small.gather_9, avoid.ne.travel_9, stay.home_9,
         sad_9, irritable_9, lonely_9,qualife_9,
         phys.act_10, healthy.diet_10, alcohol_10, cigarette_10, vaping_10, drugs_10, 
         wash.hands_10, social.dist_10, nervous_10, outside.mask_10,indoor.mask_10, quarantine.sick_10,
         quarantine.travel_10,avoid.large.gather_10,avoid.indoor.gather_10, avoid.small.gather_10, avoid.ne.travel_10, stay.home_10,
         sad_10, irritable_10, lonely_10,qualife_10,
         phys.act_11, healthy.diet_11, alcohol_11, cigarette_11, vaping_11, drugs_11,
         wash.hands_11, social.dist_11, nervous_11, outside.mask_11, indoor.mask_11,quarantine.sick_11,
         quarantine.travel_11, avoid.large.gather_11,avoid.indoor.gather_11, avoid.small.gather_11, avoid.ne.travel_11, stay.home_11,
         sad_11, irritable_11, lonely_11,qualife_11) %>%
  replace_with_na(replace = list(sex_8=(4), phys.act_8= (7), healthy.diet_8= (7), alcohol_8= (7),
         cigarette_8= (7), vaping_8= (7), drugs_8= (7),wash.hands_8= c(5,6),
         social.dist_8= c(5,6), nervous_8= c(5,6), outside.mask_8=c(5,6),
         indoor.mask_8=c(5,6), quarantine.sick_8=c(5,6), quarantine.travel_8=c(5,6),
         avoid.large.gather_8=c(5,6), avoid.indoor.gather_8=c(5,6), avoid.small.gather_8=c(5,6), avoid.ne.travel_8=c(5,6), stay.home_8=c(5,6),
         sad_8=c(5,6), irritable_8=c(5,6), lonely_8=c(5,6),qualife_8=(6),
         phys.act_9= (7), healthy.diet_9= (7), alcohol_9= (7), cigarette_9= (7),
         vaping_9= (7), drugs_9= (7),
         wash.hands_9= c(5,6), social.dist_9= c(5,6), nervous_9= c(5,6),outside.mask_9=c(5,6),
         indoor.mask_9=c(5,6), quarantine.sick_9=c(5,6), quarantine.travel_9=c(5,6),
         avoid.large.gather_9=c(5,6), avoid.small.gather_9=c(5,6),avoid.indoor.gather_9=c(5,6), avoid.ne.travel_9=c(5,6), stay.home_9=c(5,6),
         sad_9=c(5,6), irritable_9=c(5,6), lonely_9=c(5,6),qualife_9=(6),
         phys.act_10= (7), healthy.diet_10= (7), alcohol_10= (7), cigarette_10= (7),
         vaping_10= (7), drugs_10= (7), wash.hands_10= c(5,6), social.dist_10= c(5,6), nervous_10= c(5,6), outside.mask_10=c(5,6),
         indoor.mask_10=c(5,6), quarantine.sick_10=c(5,6), quarantine.travel_10=c(5,6),
         avoid.large.gather_10=c(5,6),avoid.indoor.gather_10=c(5,6), avoid.small.gather_10=c(5,6), avoid.ne.travel_10=c(5,6), stay.home_10=c(5,6),
         sad_10=c(5,6), irritable_10=c(5,6), lonely_10=c(5,6),qualife_10=(6),
         phys.act_11= (7), healthy.diet_11= (7), alcohol_11= (7), cigarette_11= (7),
         vaping_11= (7), drugs_11= (7),
         wash.hands_11= c(5,6), social.dist_11= c(5,6), nervous_11= c(5,6), outside.mask_11=c(5,6),
         indoor.mask_11=c(5,6), quarantine.sick_11=c(5,6), quarantine.travel_11=c(5,6),
         avoid.large.gather_11=c(5,6),avoid.indoor.gather_11=c(5,6), avoid.small.gather_11=c(5,6), avoid.ne.travel_11=c(5,6), stay.home_11=c(5,6),
         sad_11=c(5,6), irritable_11=c(5,6), lonely_11=c(5,6), qualife_11=(6))) %>%
  mutate(across(phys.act_9:drugs_9, ~recode(., '1'='6', '2'='5', '3'='4','4'='3','5'='2', '6'='1')),
         across(qualife_8, ~recode(., '1'=5, '2'=4, '3'=3,'4'=2,'5'=1)),
         across(wash.hands_8:lonely_8, ~recode(., '1'='4', '2'='3', '3'='2','4'='1')),
         across(phys.act_9:drugs_9, ~recode(., '1'='6', '2'='5', '3'='4','4'='3','5'='2', '6'='1')),
         across(wash.hands_9:lonely_9, ~recode(., '1'='4', '2'='3', '3'='2','4'='1')),
         across(qualife_9, ~recode(., '1'=5, '2'=4, '3'=3,'4'=2,'5'=1)),
         across(phys.act_10:drugs_10, ~recode(., '1'='6', '2'='5', '3'='4','4'='3','5'='2', '6'='1')),
         across(wash.hands_10:lonely_10, ~recode(., '1'='4', '2'='3', '3'='2','4'='1')),
         across(qualife_10, ~recode(., '1'='5', '2'='4', '3'='3','4'='2','5'='1')),
         across(phys.act_11:drugs_11, ~recode(., '1'='6', '2'='5', '3'='4','4'='3','5'='2', '6'='1')),
         across(qualife_11, ~recode(., '1'='5', '2'='4', '3'='3','4'='2','5'='1')),
         across(wash.hands_11:lonely_11, ~recode(., '1'='4', '2'='3', '3'='2','4'='1')),
         across(c(lang_8, sex_8, canprov_8, edu_8), as.character),
         sex_8 = recode(sex_8, '1' = "Male", '2' = "Female", '3' = "Other"),
         canprov_8 = recode(canprov_8, '1' = 'British Columbia', '2' = "Alberta",
                            '3' = "Saskatchewan",'4' = "Manitoba", '5' = "Ontario",
                            '6' = "Québec",'7' = "New Brunswick", '8' = "Nova Scotia",
                            '9' = "Prince Edward Island", '10'= "Newfoundland",
                            '11'= "Northwest Territories", '12'= "Yukon",
                            '13'= "Nunavut"),
         edu_8 = recode(edu_8, '1' = "Primary/elementary school or less",
                        '2' = "Secondary/high school", '3' = "College or University degree",
                        '4' = "Graduate/Postgraduate degree", '5' = "I have never been to school",
                        '6' = "I don’t know/I prefer not to answer"),
         across(phys.act_8:qualife_11, as.numeric)) %>%
  rowwise() %>% 
  mutate(neg.emotion_8 = mean(c(nervous_8, sad_8, irritable_8, lonely_8), na.rm=T), 
         neg.emotion_9 = mean(c(nervous_9, sad_9, irritable_9, lonely_9), na.rm=T),
         neg.emotion_10 = mean(c(nervous_10, sad_10, irritable_10, lonely_10), na.rm=T),
         neg.emotion_11 = mean(c(nervous_11, sad_11, irritable_11, lonely_11), na.rm=T),
         qaur.mean_8 = mean(c(quarantine.sick_8, quarantine.travel_8, stay.home_8), na.rm=T),
         qaur.mean_9 = mean(c(quarantine.sick_9, quarantine.travel_9, stay.home_9), na.rm=T),
         qaur.mean_10 = mean(c(quarantine.sick_10, quarantine.travel_10, stay.home_10), na.rm=T),
         qaur.mean_11 = mean(c(quarantine.sick_11, quarantine.travel_11, stay.home_11), na.rm=T))


# Analysis: demographics table ####

demographics.dataset <- hb.network.data.full %>% 
  drop_na(c(phys.act_8, healthy.diet_8, alcohol_8, cigarette_8, vaping_8, drugs_8,
            wash.hands_8, social.dist_8, outside.mask_8, 
            phys.act_9, healthy.diet_9, alcohol_9, cigarette_9, vaping_9, drugs_9,
            wash.hands_9, social.dist_9, outside.mask_9,
            phys.act_10, healthy.diet_10, alcohol_10, cigarette_10, vaping_10, drugs_10, 
            wash.hands_10, social.dist_10, outside.mask_10, 
            phys.act_11, healthy.diet_11, alcohol_11, cigarette_11, vaping_11, drugs_11,
            wash.hands_11, social.dist_11, outside.mask_11))

sex.demographics <-  demographics.dataset %>%
  select(sex_8)%>%
  group_by(sex_8)%>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(desc(count)) %>%
  rename(col1 = sex_8)

prov.demographics <-  demographics.dataset %>%
  select(canprov_8)%>%
  group_by(canprov_8)%>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(desc(count))%>%
  rename(col1 = canprov_8)

edu.demographics <-  demographics.dataset %>%
  select(edu_8)%>%
  group_by(edu_8)%>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(desc(count))%>%
  rename(col1 = edu_8)

age.demographics <- demographics.dataset %>%
  select(age_8) %>%
  summarize(mean = mean(age_8),
            sd = sd(age_8)) %>%
  mutate(col1 = c("Age")) %>%
  relocate(col1, before = everything()) %>%
  rename(count = mean, percent = sd)

age.demographics <-  demographics.dataset%>%
  mutate(Age.group = case_when(age_8 >= 15  & age_8  <= 24 ~ '15-24',
                               age_8  >= 25  & age_8  <= 34 ~ '25-34',
                               age_8  >= 35  & age_8  <= 44 ~ '45-44',
                               age_8  >= 45  & age_8  <= 54 ~ '45-54',
                               age_8  >= 55  & age_8  <= 64 ~ '55-64',
                               age_8  >= 65  & age_8  <= 74 ~ '65-74',
                               age_8  >= 75  & age_8  <= 84 ~ '75-84',
                               age_8  >= 85  & age_8  <= 94 ~ '85-99')) %>%
  select(Age.group)%>%
  group_by(Age.group)%>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  rename(col1 = Age.group)


range(demographics.dataset$age_8, na.rm=T)

demographics.dataset %>%
  select(hoinc_8) %>%
  group_by(hoinc_8) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(desc(hoinc_8))

# Label	Values
# Bottom third	1
# Middle third	2
# Top third	3
# I don’t know/I prefer not to answer	4



combined.demographics <- rbind(sex.demographics, prov.demographics,
                               edu.demographics, age.demographics) 

gt_tbl <- combined.demographics %>% 
  gt(rowname_col = "col1") %>%
  tab_row_group(group = "Sex (n,%)", rows = 1:3) %>%
  tab_row_group(group = "Canadian Province (n,%)", rows = 4:12) %>%
  tab_row_group(group = "Education (n,%)", rows = 13:16)%>%
  tab_row_group(group = "Age (M,SD)", rows = 17)%>%
  fmt_number(columns = 3, , decimals = 2)%>%
  fmt_percent(columns = "percent", rows = c("Male", "Female", "Other",
      'British Columbia', "Alberta", "Saskatchewan",
      "Manitoba", "Ontario", "Québec", "New Brunswick", "Nova Scotia",
      "Newfoundland", "Secondary/high school", 
      "College or University degree", "Graduate/Postgraduate degree", 
       "I don’t know/I prefer not to answer"), decimals = 1, scale_values=FALSE) %>%
  row_group_order(groups = c( "Sex (n,%)", "Age (M,SD)","Education (n,%)", "Canadian Province (n,%)"))%>% 
  tab_header(title = md("**Table 1. Demographic Information**")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  fmt_number(columns = 2, drop_trailing_zeros = TRUE) %>%
  tab_options(column_labels.hidden = TRUE, column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3), column_labels.border.bottom.color = "black",
              table_body.hlines.color = "white", table_body.vlines.color = "none",
              table.border.bottom.color = "black",table.border.bottom.width = px(3),
              table.border.top.color = "black", table.border.top.width = px(3)) %>%
  gtsave("iCARE Demographics.pdf")


# Analysis: summary statistics for included variables ####

demo.long <- demographics.dataset %>% select(-clspcode, -lang_8, -sex_8, -age_8, -canprov_8,
                                             -edu_8)

demo.long<-long_panel(demo.long , prefix = "_",
                        label_location = "end", begin = 8, end = 11) 

small.multiples<-demo.long %>%
  ungroup() %>%
  group_by(wave) %>%
  summarize(count = n(),
            physact_m = mean(phys.act, na.rm = T),
            physact_sd = sd(phys.act, na.rm = T),
            healthydiet_m = mean(healthy.diet, na.rm=T),
            healthydiet_sd = sd(healthy.diet, na.rm=T),
            alcohol_m = mean(alcohol, na.rm=T),
            alcohol_sd = sd(alcohol, na.rm=T),
            cigarette_m = mean(cigarette, na.rm=T),
            cigarette_sd = sd(cigarette, na.rm=T),
            vaping_m = mean(vaping, na.rm =T),
            vaping_sd = sd(vaping, na.rm =T),
            drugs_m = mean(drugs, na.rm =T),
            drugs_sd = sd(drugs, na.rm =T),
            washhands_m = mean(wash.hands, na.rm =T),
            washhands_sd = sd(wash.hands, na.rm =T),
            outsidemask_m = mean(outside.mask, na.rm =T),
            outsidemask_sd = sd(outside.mask, na.rm =T),
            socialdist_m = mean(social.dist, na.rm =T),
            socialdist_sd = sd(social.dist, na.rm =T))%>%
  round(2) %>%
  select(-count)%>%
  mutate(wave=as.factor(wave))%>%
  pivot_longer(cols=2:19,
               names_to = c("variable", ".value"), names_sep="_")%>%
  mutate(variable=as.factor(variable))


variable_names <- c(
  `washhands` = "Hand Washing",
  `socialdist` = "Social Distancing",
  `outsidemask` = "Mask Outdoors")

pand.multiples<-small.multiples %>%
  filter(variable== "washhands"| variable== "socialdist"| variable== "outsidemask") %>%
  ggplot(aes(x = wave, y = m)) +
  geom_col(fill="steelblue4", alpha=0.5) +
  geom_errorbar(aes(x=wave, ymin=m-sd, ymax=m+sd),
                 width=0.4, colour="orange", alpha=0.9, size=1.3)+
  facet_wrap(~variable, ncol=3, labeller = as_labeller(variable_names))+
  labs(y = NULL, x = NULL) +
  coord_cartesian(ylim=c(1,4.2)) +
  scale_y_continuous(breaks = 1:4,
                     labels=c("1" = "Never","2"="Seldom","3"="Some of the Time",
                              "4"="Most of the Time")) +
  scale_x_discrete(labels=c("8" = "Feb-Mar","9"="Mar-May","10"="May-Jun",
                            "11"="Jun-July"), guide = guide_axis(angle = 45)) +
  theme_classic() 
  ggsave("pandemic multiples.pdf", width = 7, height = 4)
  

variable_names2 <- c(
  `healthydiet` = "Healthy Diet",
  `alcohol` = "Alcohol",
  `physact` = "Physical Activity")

health.multiples1<-small.multiples %>%
  filter( variable== "healthydiet"| variable== "alcohol"| variable== "physact") %>%
  ggplot(aes(x = wave, y = m)) +
  geom_col(fill="steelblue4", alpha=0.5) +
  #geom_boxplot(width = .2, alpha=.6)+
  geom_errorbar(aes(x=wave, ymin=m-sd, ymax=m+sd),
                width=0.4, colour="orange", alpha=0.9, size=1.3)+
  facet_wrap(~variable, ncol=3, labeller = as_labeller(variable_names2))+
  labs(y = NULL, x = NULL) +
  coord_cartesian(ylim=c(1,6.2)) +
  scale_y_continuous(breaks = 1:6,
                     labels=c("1" = "Don’t do This","2"="Do This a Lot Less","3"="Do This Less",
                              "4"="As Much as Before", "5" = "Do This More",
                              "6" = "Do This a Lot More ")) +
  scale_x_discrete(labels=c("8" = "Feb-Mar","9"="Mar-May","10"="May-Jun",
                            "11"="Jun-July"), guide = guide_axis(angle = 45)) +
  theme_classic() 
ggsave("hb1 multiples.pdf", width = 7, height = 4)


variable_names3 <- c(
  `cigarette` = "Cigarettes",
  `drugs` = "Drugs",
  `vaping` = "Vaping")

health.multiples2<-small.multiples %>%
  filter(variable== "cigarette"| variable== "drugs"| variable== "vaping") %>%
  ggplot(aes(x = wave, y = m)) +
  geom_col(fill="steelblue4", alpha=0.5) +
  #geom_boxplot(width = .2, alpha=.6)+
  geom_errorbar(aes(x=wave, ymin=m-sd, ymax=m+sd),
                width=0.4, colour="orange", alpha=0.9, size=1.3)+
  facet_wrap(~variable, ncol=3, labeller = as_labeller(variable_names3))+
  labs(y = NULL, x = NULL) +
  coord_cartesian(ylim=c(1,6.5)) +
  scale_y_continuous(breaks = 1:6,
                     labels=c("1" = "Don’t do This","2"="Do This a Lot Less","3"="Do This Less",
                              "4"="As Much as Before", "5" = "Do This More",
                              "6" = "Do This a Lot More ")) +
  scale_x_discrete(labels=c("8" = "Feb-Mar","9"="Mar-May","10"="May-Jun",
                            "11"="Jun-July"), guide = guide_axis(angle = 45)) +
  theme_classic() 
ggsave("hb2 multiples.pdf", width = 7, height = 4)


sum.tbl.behaviours <- demo.long %>%
  ungroup() %>%
  group_by(wave) %>%
  summarize(count = n(),
            phys.act.m = mean(phys.act, na.rm = T),
            phys.act_sd = sd(phys.act, na.rm = T),
            healthy.diet.m = mean(healthy.diet, na.rm=T),
            healthy.diet.sd = sd(healthy.diet, na.rm=T),
            alcohol.m = mean(alcohol, na.rm=T),
            alcohol.sd = sd(alcohol, na.rm=T),
            cigarette.m = mean(cigarette, na.rm=T),
            cigarette.sd = sd(cigarette, na.rm=T),
            vaping.m = mean(vaping, na.rm =T),
            vaping.sd = sd(vaping, na.rm =T),
            drugs.m = mean(drugs, na.rm =T),
            drugs.sd = sd(drugs, na.rm =T),
            wash.hands.m = mean(wash.hands, na.rm =T),
            wash.hands.sd = sd(wash.hands, na.rm =T),
            outside.mask.m = mean(outside.mask, na.rm =T),
            outside.mask.sd = sd(outside.mask, na.rm =T),
            social.dist.m = mean(social.dist, na.rm =T),
            social.dist.sd = sd(social.dist, na.rm =T),
            indoor.gather.m = mean(avoid.indoor.gather, na.rm =T),
            indoor.gather.sd = sd(avoid.indoor.gather, na.rm =T))%>%
  round(2)%>%
  gt()%>%
  tab_spanner(label = md("**Physical Activity**"),columns = vars("phys.act.m", "phys.act_sd"))%>%
  tab_spanner(label = md("**Healthy Diet**"),columns = vars("healthy.diet.m", "healthy.diet.sd"))%>%
  tab_spanner(label = md("**Alcohol**"),columns = vars("alcohol.m","alcohol.sd"))%>%
  tab_spanner(label = md("**Cigarette**"),columns = vars("cigarette.m","cigarette.sd"))%>%
  tab_spanner(label = md("**Vape**"),columns = vars("vaping.m","vaping.sd"))%>%
  tab_spanner(label = md("**Drug Usage**"),columns = vars("drugs.m","drugs.sd"))%>%
  tab_spanner(label = md("**Hand Washing**"),columns = vars("wash.hands.m","wash.hands.sd"))%>%
  tab_spanner(label = md("**Mask Outside**"),columns = vars("outside.mask.m","outside.mask.sd"))%>%
  tab_spanner(label = md("**Social Distance**"),columns = vars("social.dist.m","social.dist.sd"))%>%
  tab_spanner(label = md("**Avoid Gathering**"),columns = vars("indoor.gather.m","indoor.gather.sd"))%>%
  cols_label("phys.act.m" = "M", "phys.act_sd" = "SD",	"healthy.diet.m" = "M",
             "healthy.diet.sd" = "SD",	"alcohol.m" = "M", "alcohol.sd" = "SD",
             "cigarette.m" = "M", "cigarette.sd" = "SD",	"vaping.m" = "M",
             "vaping.sd" = "SD",	"drugs.m" = "M",	"drugs.sd"="SD",
             "wash.hands.m"="M","wash.hands.sd"="SD",	"outside.mask.m"="M",
             "outside.mask.sd"="SD","social.dist.m"="M","social.dist.sd"="SD",
             "indoor.gather.m"="M",	"indoor.gather.sd"="SD", "wave" = "Wave",
             "count" = "n")%>%
  tab_options( column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3), column_labels.border.bottom.color = "black",
              table_body.hlines.color = "white", table_body.vlines.color = "none",
              table.border.bottom.color = "black",table.border.bottom.width = px(3),
              table.border.top.color = "black", table.border.top.width = px(3))%>%
  tab_style(style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())) %>%
  gtsave("iCARE Summary Statistics.pdf")

# Network analysis [vars match chapter 3 analysis]####

# note: zero variability on indoor masks; will substitute with outdoor

hb.network.data.final <- hb.network.data.full %>%
  select(phys.act_8, healthy.diet_8, alcohol_8, cigarette_8, vaping_8, drugs_8,
         wash.hands_8, social.dist_8, outside.mask_8, 
         phys.act_9, healthy.diet_9, alcohol_9, cigarette_9, vaping_9, drugs_9,
         wash.hands_9, social.dist_9, outside.mask_9,
         phys.act_10, healthy.diet_10, alcohol_10, cigarette_10, vaping_10, drugs_10, 
         wash.hands_10, social.dist_10, outside.mask_10,
         phys.act_11, healthy.diet_11, alcohol_11, cigarette_11, vaping_11, drugs_11,
         wash.hands_11, social.dist_11, outside.mask_11) %>%
  drop_na() %>%
  mutate_if(is.character,as.numeric)%>%
  mutate_each_(list(~scale(.) %>% as.vector), vars= 1:36) %>% # mean center all vars
  as_tibble()

# step 3: summary stats and design matrix

# Write summary statistics:
n <- nrow(hb.network.data.final)
covMat <- (n-1)/n * cov(hb.network.data.final) # Note: Maximum likelihood estimate

# form the design matrix:
design <- matrix(colnames(covMat),9)

# step 4: form panel-gvar model

library(Rcpp)
library(psychonetrics)
library(qgraph)

model <- panelgvar(hb.network.data.final, vars = design)

#estimator = "FIML"

# step 5: run, prune, optimize 

# Run model:
Model <- model %>% runmodel

# Re warning message (from package creator: https://github.com/SachaEpskamp/psychonetrics/issues/10)
# The warning “ The optimizer encountered at least one non-positive definite matrix
# and used a pseudoinverse in parameter estimation. Results may not be accurate.”
# can safely be ignored if the parameters look ok (e.g. no partial correlations of 1/-1).
# I would use the nlminb optimizer, it works the best.

# Prune model:
Model_pruned <- Model %>% prune(adjust = "fdr", recursive = FALSE, alpha = 0.05)

# Stepup search to optimize BIC:
Model_pruned_stepup <- Model_pruned %>% stepup(criterion = "bic")


# step 6: compare, adjust models

# Compare all models:
compare(
  full = Model,
  pruned = Model_pruned, 
  pruned_stepup = Model_pruned_stepup #smallest AIC and BIC, p > .05 (good), RMSEA too high
)

# Model: Pruned setup ####

# Print some results:
Model_pruned_stepup

# Inspect fit:
Model_pruned_stepup %>% fit # SEM fit indices

# Inspect parameters:
Model_pruned_stepup %>% parameters

# Extract networks:
temporal <- Model_pruned_stepup %>% getmatrix("PDC")
contemporaneous <- Model_pruned_stepup%>% getmatrix("omega_zeta_within")
betweensubjects <- Model_pruned_stepup %>% getmatrix("omega_zeta_between")

# Labels:
Labels <- c("Physical\nActivity", "Healthy\nEating", "Alcohol", "Cigarette", 
            "Vaping", "Drugs", "Wash\nHands", "Social\nDistance","Outside\nMask")

# step 7: print networks
# Plot and save to PDF file:
pdf("iCARE Temporal Network - Pruned Setup.pdf",width = 12, height = 4)
layout(t(1:2))
qgraph(temporal, theme = "colorblind", labels = Labels,
       asize = 6, vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Temporal", 
       label.scale = FALSE, directed = T, arrows = T,layout="circle", edge.labels = T)

qgraph(contemporaneous,   theme = "colorblind", labels = Labels,
       vsize =  10, label.cex = .8, mar = c(8,8,8,8), 
       title = "Contemporaneous", label.scale = FALSE,layout="circle", edge.labels = T)

# Close pdf device and finalize image:
dev.off()

qgraph(betweensubjects,   theme = "colorblind", labels = Labels,
       vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Between-subjects",
       label.scale = FALSE,layout="circle", edge.labels = T)
box("figure")


# Model: Pruned ####

# Print some results:
Model_pruned

# Inspect fit:
Model_pruned %>% fit # SEM fit indices

# Inspect parameters:
Model_pruned %>% parameters

# Extract networks:
temporal <- Model_pruned %>% getmatrix("PDC")
contemporaneous <- Model_pruned%>% getmatrix("omega_zeta_within")
betweensubjects <- Model_pruned %>% getmatrix("omega_zeta_between")

# Labels:
Labels <- c("Physical\nActivity", "Healthy\nEating", "Alcohol", "Cigarette", 
            "Vaping", "Drugs", "Wash\nHands", "Social\nDistance","Outside\nMask")

# step 7: print networks
# Plot and save to PDF file:
pdf("iCARE Temporal Network - Pruned.pdf",width = 12, height = 4)
layout(t(1:2))
qgraph(temporal, theme = "colorblind", labels = Labels,
       asize = 6, vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Temporal", 
       label.scale = FALSE, directed = T, arrows = T,layout="circle", edge.labels = T)

qgraph(contemporaneous,   theme = "colorblind", labels = Labels,
       vsize =  10, label.cex = .8, mar = c(8,8,8,8), 
       title = "Contemporaneous", label.scale = FALSE,layout="circle", edge.labels = T)

# Close pdf device and finalize image:
dev.off()

qgraph(betweensubjects,   theme = "colorblind", labels = Labels,
       vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Between-subjects",
       label.scale = FALSE,layout="circle", edge.labels = T)
box("figure")

# Model: Full / Saturated ####

# Print some results:
Model

# Inspect fit:
Model %>% fit # SEM fit indices

# Inspect parameters:
Model %>% parameters

# Extract networks:
temporal <- Model %>% getmatrix("PDC")
contemporaneous <- Model%>% getmatrix("omega_zeta_within")
betweensubjects <- Model %>% getmatrix("omega_zeta_between")

# Labels:
Labels <- c("Physical\nActivity", "Healthy\nEating", "Alcohol", "Cigarette", 
            "Vaping", "Drugs", "Wash\nHands", "Social\nDistance","Outside\nMask")

# step 7: print networks
# Plot and save to PDF file:
pdf("iCARE Temporal Network - Full Saturated.pdf",width = 12, height = 4)
layout(t(1:2))
qgraph(temporal, theme = "colorblind", labels = Labels,
       asize = 6, vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Temporal", 
       label.scale = FALSE, directed = T, arrows = T,layout="circle", edge.labels = T)

qgraph(contemporaneous,   theme = "colorblind", labels = Labels,
       vsize =  10, label.cex = .8, mar = c(8,8,8,8), 
       title = "Contemporaneous", label.scale = FALSE,layout="circle", edge.labels = T)

# Close pdf device and finalize image:
dev.off()

qgraph(betweensubjects,   theme = "colorblind", labels = Labels,
       vsize = 10, label.cex = .8, mar = c(8,8,8,8), title = "Between-subjects",
       label.scale = FALSE,layout="circle", edge.labels = T)
box("figure")




