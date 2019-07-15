library(gtools)
library(tidyverse)
library(data.table)
library(tidytext)
library(Rcpp)
source("r/simulations/calculate_SMRD.R")
sourceCpp("cpp/vum_functions.cpp")

### Prepare data of study 2 to be in same form as that of study 1 ==============

# Read data
# funky way of reading data needid to deal with escaped quotation marks in aspects
aspects <- fread("data/study_2/aspects.csv", na.strings = c("NULL", "NA"))
aspects <- apply(aspects, c(1, 2), function(x){
  if (!is.na(x) && x == "NA"){
    return(NA)
  } else {
    return(x)
  }
})
aspects <- aspects %>%
  as_tibble() %>% type_convert() %>%
  mutate(r_risk = as.numeric(r_risk))

participants <- read_csv("data/study_2/participants_anonymous.csv")
ratings <- read_csv("data/study_2/ratings.csv", na = "NULL")

### clean an merge data

# list of words to exclude if the aspect consist only of them
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
  filter(status == "done" & imc1 == "correct" & imc2 == "correct" & qual_effort >= 25 &
           qual_focused >= 25 & !(tolower(aspect) %in% excl_aspects) &
           nchar(aspect) >= min_n_char & !is.na(r_risk) &
           device %in% c("desktop", "laptop")) %>%
  select(-imc1, -imc2, -status, -device, -soep_item, -cond_order, -cond_soepframe) %>%
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

# calculate and add SMRD
pl <- pl %>%
  left_join(calculate_SMRD(as.data.table(pl)), by = "partid") %>%
  mutate(r_risk_binary = case_when(r_risk_binary == "avoiding" ~ "Contra",
                                   r_risk_binary == "seeking" ~ "Pro",
                                   TRUE ~ r_risk_binary))

# create a data frame with only one row per participant for the statistical analyses
pd <- pl %>%
  select(partid, age, sex, rating, n_seeking, n_avoiding, m_risk, p_control,
         p_social, p_situation, med_frequency, p_active, mean_sentiment,
         min_sentiment, max_sentiment, SMRD, aspect_ind, yearsofedu, job,
         income, n_aspects, remembered, similarity, duration) %>%
  group_by(partid) %>%
  top_n(n = 1, aspect_ind) %>%
  ungroup()


# write data
write_csv(pl, "data/study_2/study_long.csv")
write_csv(pd, "data/study_2/study.csv")

### Prepare data of similarity ratings =========================================

aspects <- read_csv("data/similarities/aspects.csv")
participants <- read_csv("data/similarities/participants.csv", na = "NULL")
ratings <- read_csv("data/similarities/ratings.csv")
logins <- read_csv("data/similarities/logins.csv")

keep_ids <- participants %>%
  filter(done == 1) %>%
  mutate(imc4 = case_when(is.na(imc4) ~ "correct",
                         TRUE ~ imc4),
         imc5 = case_when(is.na(imc5) ~ "correct",
                         TRUE ~ imc5),
         imc6 = case_when(is.na(imc6) ~ "correct",
                         TRUE ~ imc6),
         imcs = (imc1 == "correct") + (imc2 == "correct") + (imc3 == "correct") +
           (imc4 == "correct") + (imc5 == "correct") + (imc6 == "correct") +
           (imc7 == "correct") + (imcscreening == "correct")) %>%
  filter(imcs >= 6) %>%
  select(ratid) %>% pull()

# check keep_ids; should be 63
length(keep_ids)

ratings <- ratings %>%
  select(-id) %>%
  filter(ratid %in% keep_ids) %>%
  distinct() %>% # remove duplictace entries, that occured from reloading the page
  gather(variable, value, rating) %>%
  unite(temp, variable, ratid) %>%
  spread(temp, value)

ratings$m_similarity <- rowMeans(ratings %>% select(starts_with("rating_")),
                                 na.rm = TRUE)

sim_dat <- aspects %>%
  left_join(ratings, by = c("id" = "id_aspects"))

saveRDS(sim_dat, "data/similarities/similarity_ratings.RDS")

### Prepare joint data of both studies with similarity ratings =================

study_long_s1 <- read_csv("data/study_1/study_long.csv")
study_long_s2 <- read_csv("data/study_2/study_long.csv")
study_s1 <- read_csv("data/study_1/study.csv")
study_s2 <- read_csv("data/study_2/study.csv")
sim_dat <- readRDS("data/similarities/similarity_ratings.RDS")

# get VUM predictions
phi <- .91
VUM_s1 <- study_long_s1 %>%
  group_by(partid) %>%
  summarize(pred_VUM_s1 = vum(r_risk, phi)) %>%
  ungroup()

VUM_s2 <- study_long_s2 %>%
  group_by(partid) %>%
  summarize(pred_VUM_s2 = vum(r_risk, phi)) %>%
  ungroup()

sim_dat <- sim_dat %>%
  left_join(study_s1 %>% select(partid, n_aspects), by = "partid") %>%
  mutate(overlap = case_when(m_similarity >= 4 ~ 1,
                             TRUE ~ 0),
         aspects = case_when(substr(aspect_id_1, 1, 2) == "s1" &
                               substr(aspect_id_2, 1, 2) == "s1" ~ "within_s1",
                             substr(aspect_id_1, 1, 2) == "s2" &
                               substr(aspect_id_2, 1, 2) == "s2" ~ "within_s2",
                             TRUE ~ "between")) %>%
  select(partid, aspects, overlap, m_similarity, n_aspects) %>%
  group_by(partid, aspects) %>%
  summarise(p_overlap = mean(overlap),
            m_similarity = mean(m_similarity)) %>%
  gather(variable, value, p_overlap, m_similarity) %>%
  unite(temp, variable, aspects) %>%
  spread(temp, value)

study_s1 <- study_s1 %>%
  select(partid, rating, n_seeking, n_avoiding, m_risk, p_control, p_social,
         p_situation, med_frequency, p_active, mean_sentiment, n_aspects)
names(study_s1)[2:12] <- paste0(names(study_s1)[2:12], "_s1")

study_s2 <- study_s2 %>%
  select(partid, rating, n_seeking, n_avoiding, m_risk, p_control, p_social,
         p_situation, med_frequency, p_active, mean_sentiment, n_aspects,
         remembered, similarity)
names(study_s2)[2:12] <- paste0(names(study_s2)[2:12], "_s2")

study_joint <- study_s2 %>%
  left_join(study_s1, by = "partid") %>%
  left_join(VUM_s1, by = "partid") %>%
  left_join(VUM_s2, by = "partid") %>%
  left_join(sim_dat, by = "partid") %>%
  mutate(delta_rating = abs(rating_s2 - rating_s1),
         delta_VUM = abs(pred_VUM_s2 - pred_VUM_s1))

write_csv(study_joint, "data/study_2/study_joint.csv")
