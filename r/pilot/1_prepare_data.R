library(tidyverse)
library(data.table)
source("simulations/calculate_SMRD.R")

# Read data
aspects <- read_csv("data/pilot/aspects.csv", na = "NULL")
participants <- read_csv("data/pilot/participants_anonymous.csv")
ratings <- read_csv("data/pilot/ratings.csv", na = "NULL")

### clean an merge data

# list of words to explude if the aspect consist only of them
excl_aspects <- c("none", "nope")
# minimum number of characters a listed aspect must have to not be deleted
min_n_char <- 4 

pl <- participants %>%
  full_join(ratings, by = c("partid", "cond_order", "cond_soepframe")) %>%
  select(-id) %>%
  full_join(aspects, by = c("partid", "cond_order", "cond_soepframe",
                            "soep_item")) %>%
  select(-id) %>%
  # filter for participants that completed the whole study and passed the checks
  filter(done == 1 & (imc1 == "correct" | imc2 == "correct") & qual_effort >= 25 &
           qual_focused >= 25 & !(tolower(aspect) %in% excl_aspects) &
           nchar(aspect) >= min_n_char & !is.na(r_risk)) %>%
  group_by(partid) %>%
  # prepare variables to later use in the analyses and plots
  mutate(aspect_ind = row_number(),
         aspect_ind_cat = case_when(aspect_ind == 1 ~ "Aspect #1",
                                    aspect_ind == 2 ~ "Aspect #2",
                                    aspect_ind == 3 ~ "Aspect #3",
                                    aspect_ind == 4 ~ "Aspect #4",
                                    aspect_ind == 5 ~ "Aspect #5",
                                    aspect_ind >= 6 ~ "Aspect #6+"),
         r_risk_binary = case_when(r_risk >= 0 & r_risk <= 49 ~ "avoiding",
                                   r_risk == 50 ~ "neutral",
                                   TRUE ~ "seeking"),
         n_seeking = sum(r_risk_binary == "seeking"),
         n_avoiding = sum(r_risk_binary == "avoiding"),
         r_risk_s = scale(r_risk),
         r_social_s = scale(r_social),
         r_situation_s = scale(r_situation),
         r_frequency_s = scale(r_frequency),
         r_risk = r_risk / 10,
         r_social = r_social / 10,
         r_situation = r_situation / 10,
         r_frequency = r_frequency / 10,
         med_risk = median(r_risk),
         med_social = median(r_social),
         med_situation = median(r_situation),
         med_frequency = median(r_frequency)) %>%
  ungroup()

# get sentiments from the listed aspects
pl <- pl %>%
  select(partid, aspect, aspect_ind) %>%
  unnest_tokens(word, aspect, format = "text")%>%
  inner_join(get_sentiments("afinn")) %>% 
  anti_join(stop_words) %>%
  group_by(partid, aspect_ind) %>%
  summarise(sentiment = sum(score)) %>%
  right_join(pl, by = c("partid", "aspect_ind")) %>%
  group_by(partid) %>%
  mutate(m_sentiment = mean(sentiment, na.rm = TRUE),
         l_sentiment = max(sentiment, na.rm = TRUE),
         s_sentiment = min(sentiment, na.rm = TRUE)) %>%
  ungroup()

# calculate and add SMRD
pl <- pl %>%
  left_join(calculate_SMRD(as.data.table(pl)), by = "partid")

# create a data frame with only one row per participant for the statistical analyses
pd <- pl %>%
  select(partid, cond_order, cond_soepframe, age, sex, rating, n_seeking,
         n_avoiding, med_risk, med_social, med_situation, m_sentiment, med_frequency,
         l_sentiment, s_sentiment, SMRD, aspect_ind, med_frequency) %>%
  group_by(partid) %>%
  top_n(n = 1, aspect_ind) %>%
  ungroup()  %>%
  rename(n_aspects = aspect_ind) %>%
  mutate(cond_soepframe = case_when(cond_soepframe == "rev" ~ "Reversed Formulation",
                                    cond_soepframe == "std" ~ "Standard Formulation"),
         cond_order = case_when(cond_order == "asp_judg" ~ "Aspects First",
                                cond_order == "judg_asp" ~ "Judgment First"))

# write data
write_csv(pl, "data/pilot/pilot_long.csv")
write_csv(pd, "data/pilot/pilot.csv")