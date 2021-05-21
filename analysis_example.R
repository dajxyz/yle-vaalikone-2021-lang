library(tidyverse)

df_yle_full <- read_csv("df_yle_anon.csv")

# df_yle_full has a row for each of the 16 835 YLE vaalikone responses without 
# personally identifying information. 

# The dataset is nearly equivalent to the open data sets provided
# by YLE in previous years and has been obtained by *politely* scraping  
# the YLE website, following the rules outlined in https://vaalikone.yle.fi/robots.txt

# id = candidate id within the YLE public-facing api
# kunta_fi = Finnish municipality name
# puolue_fi = Finnish party name

# create columns yle_na_{$language}, which count the number of answers left empty for each
# candidate in the particular $language 

yle_na_en <-
  df_yle_full %>%
  tibble() %>%
  select(contains("en")) %>%
  rowwise() %>% 
  mutate(na_en = sum(is.na(c_across(everything())))) %>%
  select(na_en) %>%
  as_tibble_col

yle_na_ru <-
  df_yle_full %>%
  tibble() %>%
  select(contains("ru")) %>%
  rowwise() %>% 
  mutate(na_ru = sum(is.na(c_across(everything())))) %>%
  select(na_ru) %>%
  as_tibble_col

yle_na_sv <-
  df_yle_full %>%
  tibble() %>%
  select(contains("sv")) %>%
  rowwise() %>% 
  mutate(na_sv = sum(is.na(c_across(everything())))) %>%
  select(na_sv) %>%
  as_tibble_col

yle_na_fi <-
  df_yle_full %>%
  tibble() %>%
  select(contains("fi")) %>%
  rowwise() %>% 
  mutate(na_fi = sum(is.na(c_across(everything())))) %>%
  select(na_fi) %>%
  as_tibble_col

yle_na_sme <-
  df_yle_full %>%
  tibble() %>%
  select(contains("sme")) %>%
  rowwise() %>% 
  mutate(na_sme = sum(is.na(c_across(everything())))) %>%
  select(na_sme) %>%
  as_tibble_col

# join these columns into a dataframe for analysis...

df_analysis <- 
  df_yle_full %>%
  select(id, kunta_fi, puolue_fi) %>%
  bind_cols(c(yle_na_en, yle_na_ru, yle_na_sv, yle_na_fi, yle_na_sme))

# ... or use the longer format tidy data

df_analysis_long <- 
  df_analysis %>%
  pivot_longer(cols = starts_with("na_"), names_to = "language", values_to = "empty_answers")


# Now we can answer questions like 
# What proportion of questions were answered, by language

df_analysis_long %>%
  group_by(language) %>%
  summarise(percent_of_qs_answered = 100 - sum(empty_answers)/16835/23*100)

# How many candidates from Helsinki gave at least one answer in English:

df_analysis_long %>%
  filter(kunta_fi == "Helsinki") %>%
  filter(language == "na_en") %>%
  filter(empty_answers < 23) %>%
  summarise(at_least_one_english = n())

