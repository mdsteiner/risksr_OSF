library(tidyverse)
library(data.table)
library(tidytext)
source("r/simulations/calculate_SMRD.R")

# Read data
# funky way of reading data needed to deal with escaped quotation marks in aspects
aspects <- read.csv("data/study_1/aspects.csv", na.strings = c("NULL", "NA")) %>%
  as_tibble() %>% mutate_if(is.factor, as.character)
participants <- read_csv("data/study_1/participants_anonymous.csv")
ratings <- read_csv("data/study_1/ratings.csv", na = "NULL")

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
  # filter for participants who completed the whole study and passed the checks
  filter(done == 1 & imc1 == "correct" & imc2 == "correct" & qual_effort >= 25 &
           qual_focused >= 25 & !(tolower(aspect) %in% excl_aspects) &
           nchar(aspect) >= min_n_char & !is.na(r_risk) &
           device %in% c("desktop", "laptop")) %>%
  select(-imc1, -imc2, -done, -device, -soep_item, -cond_order, -cond_soepframe) %>%
  group_by(partid) %>%
  # prepare variables to later use in the analyses and plots
  mutate(aspect_ind = row_number(),
         aspect_ind_cat = case_when(aspect_ind == 1 ~ "Aspect #1",
                                    aspect_ind == 2 ~ "Aspect #2",
                                    aspect_ind == 3 ~ "Aspect #3",
                                    aspect_ind == 4 ~ "Aspect #4",
                                    aspect_ind == 5 ~ "Aspect #5",
                                    aspect_ind == 6 ~ "Aspect #6",
                                    TRUE ~ NA_character_),
         r_risk_binary = case_when(r_risk >= -50 & r_risk < 0 ~ "avoiding",
                                   r_risk == 0 ~ "neutral",
                                   TRUE ~ "seeking"),
         n_aspects = max(aspect_ind),
         n_seeking = sum(r_risk_binary == "seeking"),
         n_avoiding = sum(r_risk_binary == "avoiding"),
         m_risk = mean(r_risk),
         p_social = mean(r_social, na.rm = TRUE),
         p_situation = mean(r_situation, na.rm = TRUE),
         p_control = mean(r_control == "controllable"),
         p_active = mean(r_active == "active", na.rm = TRUE),
         rank_frequency = case_when(r_frequency == "day" ~ 1,
                                    r_frequency == "week" ~ 2,
                                    r_frequency == "month" ~ 3,
                                    r_frequency == "year" ~ 4,
                                    r_frequency == "less_reg" ~ 5,
                                    r_frequency == "less_once" ~ 6,
                                    TRUE ~ NA_real_),
         med_frequency = median(rank_frequency, na.rm = TRUE)) %>%
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
  mutate(mean_sentiment = mean(sentiment, na.rm = TRUE),
         max_sentiment = max(sentiment, na.rm = TRUE),
         min_sentiment = min(sentiment, na.rm = TRUE)) %>%
  ungroup()

# calculate and add SMRD
pl <- pl %>%
  left_join(calculate_SMRD(as.data.table(pl)), by = "partid") %>%
  mutate(r_risk_binary = case_when(r_risk_binary == "avoiding" ~ "Contra",
                                   r_risk_binary == "seeking" ~ "Pro",
                                   TRUE ~ r_risk_binary))

# check how many aspects cannot be matched
mean(is.na(pl$sentiment))
# -> 14% of aspects have a missing sentiment
pl <- pl %>% select(-ends_with("sentiment"))

# get sentiments from the listed aspects
pl <- pl %>%
  select(partid, aspect, aspect_ind) %>%
  unnest_tokens(word, aspect, format = "text")%>%
  inner_join(get_sentiments("bing")) %>% 
  anti_join(stop_words) %>%
  count(partid, aspect_ind, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  mutate(positive = case_when(is.na(positive) ~ 0L,
                              TRUE ~ positive),
         negative = case_when(is.na(negative) ~ 0L,
                              TRUE ~ negative),
         sentiment = positive - negative) %>%
  select(-positive, -negative) %>%
  right_join(pl, by = c("partid", "aspect_ind")) %>%
  group_by(partid) %>%
  mutate(mean_sentiment = mean(sentiment, na.rm = TRUE),
         max_sentiment = max(sentiment, na.rm = TRUE),
         min_sentiment = min(sentiment, na.rm = TRUE)) %>%
  ungroup()

# check how many aspects cannot be matched
mean(is.na(pl$sentiment))
# -> still has a high number of missings, but this is the lowest number of all
#    lexicons. We thus use the "bing" method as preregistered.

# create a data frame with only one row per participant for the statistical analyses
pd <- pl %>%
  select(partid, age, sex, rating, n_seeking, n_avoiding, m_risk, p_control,
         p_social, p_situation, med_frequency, p_active, mean_sentiment,
         min_sentiment, max_sentiment, SMRD, aspect_ind, yearsofedu, job,
         income, n_aspects, duration) %>%
  group_by(partid) %>%
  top_n(n = 1, aspect_ind) %>%
  ungroup()

last_ids <- tail(pd$partid, 5)

# exclude the last five participants, as these were oversampled due to an error
# in the MTurk sampling script.
pd <- pd %>% filter(!partid %in% last_ids)
pl <- pl %>% filter(!partid %in% last_ids)

# write data
write_csv(pl, "data/study_1/study_long.csv")
write_csv(pd, "data/study_1/study.csv")
