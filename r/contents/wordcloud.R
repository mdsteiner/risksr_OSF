library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)
library(textstem)

study_long <- read_csv("data/study_1/study_long.csv")

pal <- brewer.pal(8,"Dark2")


# remove unique stop words that snuck in there
uni_sw <- data.frame(word = c("tend","dont","makes", "lead"))

pdf("plots/study_1/wordcloud.pdf", width = 12, height = 6)

par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))

# risk seeking
study_long %>%
  filter(r_risk_binary == "Pro") %>% 
  select(aspect) %>%
  unnest_tokens(word, aspect, format = "text") %>%
  anti_join(stop_words) %>% 
  anti_join(uni_sw, by = "word") %>% 
  mutate(word = case_when(
    str_detect(word, "[^e|aies$]ies$") ~
      str_replace(word, "ies$", "y"),
    str_detect(word, "[^e|a|oes$]es$") ~
      str_replace(word, "es$", "e"),
    str_detect(word, "[^ss$|us$]s$") ~
      str_remove(word, "s$"),
    TRUE ~ word),
    word = lemmatize_words(word)) %>% 
  filter(word != "risk", word != "take") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup() %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal,
                 min.freq = 2, scale = c(4.25, 0.6)))

text(.5, .95, "Pro-Aspects", cex = 1.75, font = 2, adj = 0.5)

# risk avoiding
study_long %>%
  filter(r_risk_binary == "Contra") %>% 
  select(aspect) %>%
  unnest_tokens(word, aspect, format = "text") %>%
  anti_join(stop_words) %>% 
  anti_join(uni_sw, by = "word") %>% 
  mutate(word = case_when(
    str_detect(word, "[^e|aies$]ies$") ~
      str_replace(word, "ies$", "y"),
    str_detect(word, "[^e|a|oes$]es$") ~
      str_replace(word, "es$", "e"),
    str_detect(word, "[^ss$|us$]s$") ~
      str_remove(word, "s$"),
    TRUE ~ word),
  word = lemmatize_words(word)) %>% 
  filter(word != "risk", word != "take", word != "avoid") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup() %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal,
                 min.freq = 2, scale = c(4.25, 0.6)))

text(.5, .95, "Contra-Aspects", cex = 1.75, font = 2, adj = 0.5)

dev.off()


### sentiments of the word clouds ==============================================

# Contra aspects
study_long %>%
  filter(r_risk_binary == "Contra") %>% 
  select(aspect) %>%
  unnest_tokens(word, aspect, format = "text") %>%
  inner_join(get_sentiments("afinn")) %>% 
  anti_join(stop_words) %>% 
  anti_join(uni_sw, by = "word") %>% 
  mutate(word = case_when(
    str_detect(word, "[^e|aies$]ies$") ~
      str_replace(word, "ies$", "y"),
    str_detect(word, "[^e|a|oes$]es$") ~
      str_replace(word, "es$", "e"),
    str_detect(word, "[^ss$|us$]s$") ~
      str_remove(word, "s$"),
    TRUE ~ word),
    word = lemmatize_words(word)) %>% 
  filter(word != "risk", word != "take", word != "avoid") %>% 
  group_by(word) %>% 
  summarise(
    n = n(),
    sentiment = sum(value)
  ) %>% 
  ungroup() %>%
  filter(n > 1) %>% pull(sentiment) %>% 
  summary(na.rm = TRUE)

# pro aspects
study_long %>%
  filter(r_risk_binary == "Pro") %>% 
  select(aspect) %>%
  unnest_tokens(word, aspect, format = "text") %>%
  inner_join(get_sentiments("afinn")) %>% 
  anti_join(stop_words) %>% 
  anti_join(uni_sw, by = "word") %>% 
  mutate(word = case_when(
    str_detect(word, "[^e|aies$]ies$") ~
      str_replace(word, "ies$", "y"),
    str_detect(word, "[^e|a|oes$]es$") ~
      str_replace(word, "es$", "e"),
    str_detect(word, "[^ss$|us$]s$") ~
      str_remove(word, "s$"),
    TRUE ~ word),
    word = lemmatize_words(word)) %>% 
  filter(word != "risk", word != "take", word != "avoid") %>% 
  group_by(word) %>% 
  summarise(
    n = n(),
    sentiment = sum(value)
  ) %>% 
  ungroup() %>%
  filter(n > 1) %>% pull(sentiment) %>% 
  summary(na.rm = TRUE)
