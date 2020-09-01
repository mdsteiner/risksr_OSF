library(tidyverse)
library(vegan)
library(forcats)
library(viridis)
library(caret)
library(ggpubr)
library(ggrepel)


### Load and prepare data ======================================================
study_long <- read_csv("data/study_1/study_long.csv")
aspects <- read_csv("data/contents/all/aspects.csv", na = "NULL") %>% 
  select(-contains(c("rq5uph92ju", "r1414o4nqf")))
sessions <- read_csv("data/contents/all/rating_sessions.csv", na = "NULL")
properties <- read_csv("data/contents/all/properties.csv")

aspects <- aspects %>% 
  mutate(
    # domains
    ethical = if_else(rowSums(select(., starts_with("ethical")),
                              na.rm = TRUE) < 2, 0, 1),
    financial = if_else(rowSums(select(., starts_with("financial")),
                                na.rm = TRUE) < 2, 0, 1),
    health_safety = if_else(rowSums(select(., starts_with("health_safety")),
                                    na.rm = TRUE) < 2, 0, 1),
    recreational = if_else(rowSums(select(., starts_with("recreational")),
                                   na.rm = TRUE) < 2, 0, 1),
    social = if_else(rowSums(select(., starts_with("social")),
                             na.rm = TRUE) < 2, 0, 1),
    faith = if_else(rowSums(select(., starts_with("faith")),
                            na.rm = TRUE) < 2, 0, 1),
    traffic = if_else(rowSums(select(., starts_with("traffic")),
                              na.rm = TRUE) < 2, 0, 1),
    occupation = if_else(rowSums(select(., starts_with("occupation")),
                                 na.rm = TRUE) < 2, 0, 1),
    general = if_else(rowSums(select(., starts_with("general")),
                              na.rm = TRUE) < 2, 0, 1),
    competition_between = if_else(rowSums(select(., starts_with("competition_between")),
                                          na.rm = TRUE) < 2, 0, 1),
    competition_within = if_else(rowSums(select(., starts_with("competition_within")),
                                         na.rm = TRUE) < 2, 0, 1),
    status_power = if_else(rowSums(select(., starts_with("status_power")),
                                   na.rm = TRUE) < 2, 0, 1),
    exploration = if_else(rowSums(select(., starts_with("exploration")),
                                  na.rm = TRUE) < 2, 0, 1),
    food_selection = if_else(rowSums(select(., starts_with("food_selection")),
                                     na.rm = TRUE) < 2, 0, 1),
    food_acquisition = if_else(rowSums(select(., starts_with("food_acquisition")),
                                       na.rm = TRUE) < 2, 0, 1),
    parent_offspring_conflict = if_else(rowSums(select(.,starts_with("parent_offspring_conflict")),
                                                na.rm = TRUE) < 2, 0, 1),
    kinship = if_else(rowSums(select(., starts_with("kinship")),
                              na.rm = TRUE) < 2, 0, 1),
    mate_attraction = if_else(rowSums(select(., starts_with("mate_attraction")),
                                      na.rm = TRUE) < 2, 0, 1),
    mate_retention = if_else(rowSums(select(., starts_with("mate_retention")),
                                     na.rm = TRUE) < 2, 0, 1),
    
    # properties
    gains = if_else(rowSums(select(., starts_with("gains")),
                            na.rm = TRUE) < 2, 0, 1),
    p_gain = if_else(rowSums(select(., starts_with("p_gain")),
                             na.rm = TRUE) < 2, 0, 1),
    losses = if_else(rowSums(select(., starts_with("losses")),
                             na.rm = TRUE) < 2, 0, 1),
    p_loss = if_else(rowSums(select(., starts_with("p_loss")),
                             na.rm = TRUE) < 2, 0, 1),
    need = if_else(rowSums(select(., starts_with("need")),
                           na.rm = TRUE) < 2, 0, 1),
    ability = if_else(rowSums(select(., starts_with("ability")),
                              na.rm = TRUE) < 2, 0, 1),
    keep_status_quo = if_else(rowSums(select(., starts_with("keep_status_quo")),
                                      na.rm = TRUE) < 2, 0, 1),
    change_status_quo = if_else(rowSums(select(., starts_with("change_status_quo")),
                                        na.rm = TRUE) < 2, 0, 1),
    experience = if_else(rowSums(select(., starts_with("experience")),
                                 na.rm = TRUE) < 2, 0, 1),
    signaling = if_else(rowSums(select(., starts_with("signaling")),
                                na.rm = TRUE) < 2, 0, 1),
    social_norm = if_else(rowSums(select(., starts_with("social_norm")),
                                  na.rm = TRUE) < 2, 0, 1),
    personality = if_else(rowSums(select(., starts_with("personality")),
                                  na.rm = TRUE) < 2, 0, 1),
    religion_fate = if_else(rowSums(select(., starts_with("religion_fate")),
                                    na.rm = TRUE) < 2, 0, 1),
    pos_affect = if_else(rowSums(select(., starts_with("pos_affect")),
                                 na.rm = TRUE) < 2, 0, 1),
    thrill = if_else(rowSums(select(., starts_with("thrill")),
                             na.rm = TRUE) < 2, 0, 1),
    regret = if_else(rowSums(select(., starts_with("regret")),
                             na.rm = TRUE) < 2, 0, 1),
    fear = if_else(rowSums(select(., starts_with("fear")),
                           na.rm = TRUE) < 2, 0, 1),
    neg_aff_oth = if_else(rowSums(select(., starts_with("neg_aff_oth")),
                                  na.rm = TRUE) < 2, 0, 1),
    long_term = if_else(rowSums(select(., starts_with("long_term")),
                                na.rm = TRUE) < 2, 0, 1),
    short_term = if_else(rowSums(select(., starts_with("short_term")),
                                 na.rm = TRUE) < 2, 0, 1),
    age = if_else(rowSums(select(., starts_with("age")),
                          na.rm = TRUE) < 2, 0, 1),
    sex = if_else(rowSums(select(., starts_with("sex")),
                          na.rm = TRUE) < 2, 0, 1),
    parental_status = if_else(rowSums(select(., starts_with("parental_status")),
                                      na.rm = TRUE) < 2, 0, 1),
    birth_order = if_else(rowSums(select(., starts_with("birth_order")),
                                  na.rm = TRUE) < 2, 0, 1),
    n_siblings = if_else(rowSums(select(., starts_with("n_siblings")),
                                 na.rm = TRUE) < 2, 0, 1),
    rep_goal = if_else(rowSums(select(., starts_with("rep_goal")),
                               na.rm = TRUE) < 2, 0, 1),
    life_exp = if_else(rowSums(select(., starts_with("life_exp")),
                               na.rm = TRUE) < 2, 0, 1),
    family = if_else(rowSums(select(., starts_with("family")),
                             na.rm = TRUE) < 2, 0, 1),
    relativize = if_else(rowSums(select(., starts_with("relativize")),
                                 na.rm = TRUE) < 2, 0, 1),
    nonword = if_else(rowSums(select(., starts_with("nonword")),
                              na.rm = TRUE) < 2, 0, 1),
    ses = if_else(rowSums(select(., starts_with("ses")),
                          na.rm = TRUE) < 2, 0, 1),
    
    soe = rowMeans(select(., starts_with("soe")), na.rm = TRUE),
    soe_med = apply(select(aspects, starts_with("soe")), 1, median, na.rm = TRUE),
    soe_v = apply(select(aspects, starts_with("soe")), 1, sd, na.rm = TRUE),
    soe_min = apply(select(aspects, starts_with("soe")), 1, min, na.rm = TRUE),
    soe_max = apply(select(aspects, starts_with("soe")), 1, max, na.rm = TRUE)) %>% 
  filter(nonword == 0) %>% 
  select(-contains(c("rhf2vmahb5", "r1t56uo71a", "rk9dg7rv13")), -nonword)

# sessions
sessions %>% 
  group_by(ratid) %>% 
  summarise(
    dur = sum(session_dur, na.rm = TRUE) / 60 / 60,
    min_dur = min(session_dur)
  )


### MDS analyses ===============================================================

create_mds <- function(dat, col_vec = NULL, pal = "black") {
  if (is.null(col_vec)) {
    col_vec <- rep("a", ncol(dat))
  }
  dat_cor <- cor(dat)
  mds_cor <- (1 - dat_cor) %>%
    cmdscale() %>%
    as_tibble() %>% 
    mutate(Categories = col_vec)
  colnames(mds_cor)[1:2] <- c("d1", "d2")
  
  p <- ggplot(mds_cor, aes(d1, d2, col = Categories)) +
    geom_point() +
    geom_label_repel(label = colnames(dat_cor)) + 
    scale_color_manual(values = pal) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_light() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16,face = "bold")
    )
  
  print(p)
}

# which domains have never been ticked
keep_dom <- aspects %>% 
  select(ethical:mate_retention) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(everything(), names_to = "domain", values_to = "freq") %>% 
  filter(freq > 0) %>% 
  pull(domain)

# domains
aspects %>% 
  select(any_of(keep_dom)) %>% 
  rename("Health/Safety" = "health_safety", "Occupation" = "occupation",
         "General" = "general") %>% 
  create_mds()


# which categories have never been ticked
keep_cat <- aspects %>% 
  select(gains:ses) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  pivot_longer(everything(), names_to = "category", values_to = "freq") %>% 
  filter(freq > 0) %>% 
  pull(category)

# categories
aspects %>% 
  select(any_of(keep_cat))%>% 
  rename("Pos. Outc." = "gains", "p(Neg. Outc.)" = "p_loss",
         "Keep Status Quo" = "keep_status_quo", "Experience/Habit" = "experience",
         "Personality" = "personality", "Pos. Affect" = "pos_affect",
         "Regret" = "regret", "Neg. Affect" = "neg_aff_oth", "Age" = "age",
         "Parental Status" = "parental_status", "Relativize" = "relativize") %>% 
  create_mds()

# and across domains and categories together
aspects %>% 
  select(any_of(keep_cat), any_of(keep_dom)) %>% 
  rename("Health/Safety" = "health_safety", "Occupation" = "occupation",
         "General" = "general") %>% 
  rename("Pos. Outc." = "gains", "p(Neg. Outc.)" = "p_loss",
         "Keep Status Quo" = "keep_status_quo", "Experience/Habit" = "experience",
         "Personality" = "personality", "Pos. Affect" = "pos_affect",
         "Regret" = "regret", "Neg. Affect" = "neg_aff_oth", "Age" = "age",
         "Parental Status" = "parental_status", "Relativize" = "relativize") %>%
  create_mds(rep(c("Property", "Domain"),
                 times = c(length(keep_cat), length(keep_dom))),
             viridis(2, begin = .2, end = .8))


### Frequencies ================================================================
# domains
aspects %>% 
  select(aspect_id, ethical:mate_retention) %>% 
  pivot_longer(cols = ethical:mate_retention, names_to = "domain",
               values_to = "rating") %>% 
  group_by(domain) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(domain, rating)) +
  geom_bar(stat = "identity") +
  labs(x = "Domain", y = "Proportion") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

# proportions
aspects %>% 
  select(aspect_id, ethical:mate_retention) %>% 
  pivot_longer(cols = ethical:mate_retention, names_to = "domain",
               values_to = "rating") %>% 
  group_by(domain) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup() %>% 
  select(domain, rating) %>% arrange(desc(rating))

# Properties
d1 <- aspects %>% 
  filter(soe < 0) %>% 
  select(aspect_id, gains:ses, -relativize) %>% 
  pivot_longer(cols = gains:ses, names_to = "property_id",
               values_to = "rating") %>% 
  group_by(property_id) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup() %>% 
  left_join(properties, by = c("property_id")) %>%
  mutate(property = factor(property, levels = c("Positive affect", "Thrill",
                                                "Fear", "Regret / disappointment",
                                                "Other negative affect", "Experience / habit",
                                                "Personality", "Religion/ fate",
                                                "Social norm / - pressure", "Need",
                                                "Ability", "Keep status quo",
                                                "Change status quo", "Signaling",
                                                "Age", "Sex", "SES", "Family",
                                                "Parental status",
                                                "Reproductive goal", "Birth order",
                                                "Number of siblings",
                                                "Subjective life expectancy",
                                                "Positive outcomes", 
                                                "Probability of positive outcomes",
                                                "Negative outcomes",
                                                "Probability of negative outcomes",
                                                "Short-term perspective",
                                                "Long-term perspective"),
                           labels = c("Positive affect", "Thrill",
                                      "Fear", "Regret/Disappointment",
                                      "Other negative affect", "Experience/Habit",
                                      "Personality", "Religion/Fate",
                                      "Social norm/- pressure", "Need",
                                      "Ability", "Keep status quo",
                                      "Change status quo", "Signaling",
                                      "Age", "Sex", "SES", "Family",
                                      "Parental status",
                                      "Reproductive goal", "Birth order",
                                      "Number of siblings",
                                      "Subjective life expectancy",
                                      "Positive outcomes", 
                                      "p(Positive outcomes)",
                                      "Negative outcomes",
                                      "p(Negative outcomes)",
                                      "Short-term perspective",
                                      "Long-term perspective")),
         valence = "Contra-Aspects") %>% 
  rename(Category = category) 

d2 <- aspects %>% 
  filter(soe > 0) %>% 
  select(aspect_id, gains:ses, -relativize) %>% 
  pivot_longer(cols = gains:ses, names_to = "property_id",
               values_to = "rating") %>% 
  group_by(property_id) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup() %>% 
  left_join(properties, by = c("property_id")) %>%
  mutate(property = factor(property, levels = c("Positive affect", "Thrill",
                                                "Fear", "Regret / disappointment",
                                                "Other negative affect", "Experience / habit",
                                                "Personality", "Religion/ fate",
                                                "Social norm / - pressure", "Need",
                                                "Ability", "Keep status quo",
                                                "Change status quo", "Signaling",
                                                "Age", "Sex", "SES", "Family",
                                                "Parental status",
                                                "Reproductive goal", "Birth order",
                                                "Number of siblings",
                                                "Subjective life expectancy",
                                                "Positive outcomes", 
                                                "Probability of positive outcomes",
                                                "Negative outcomes",
                                                "Probability of negative outcomes",
                                                "Short-term perspective",
                                                "Long-term perspective"),
                           labels = c("Positive affect", "Thrill",
                                      "Fear", "Regret/Disappointment",
                                      "Other negative affect", "Experience/Habit",
                                      "Personality", "Religion/Fate",
                                      "Social norm/- pressure", "Need",
                                      "Ability", "Keep status quo",
                                      "Change status quo", "Signaling",
                                      "Age", "Sex", "SES", "Family",
                                      "Parental status",
                                      "Reproductive goal", "Birth order",
                                      "Number of siblings",
                                      "Subjective life expectancy",
                                      "Positive outcomes", 
                                      "p(Positive outcomes)",
                                      "Negative outcomes",
                                      "p(Negative outcomes)",
                                      "Short-term perspective",
                                      "Long-term perspective")),
         valence = "Pro-Aspects") %>% 
  rename(Category = category)  

pdf("plots/study_1/aspect_contents.pdf", width = 12, height = 6)
d1 %>% 
  bind_rows(d2) %>% 
  mutate(valence = factor(valence, levels = c("Pro-Aspects", "Contra-Aspects"))) %>% 
  ggplot(aes(property, rating, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_grid(valence~ .) +
  scale_fill_manual(values = viridis(6)) +
  labs(x = "Property", y = "Proportion") +
  theme_bw() +
  theme( 
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )
dev.off()



### SoE validation =============================================================

soe_dat <- aspects %>% 
  left_join(study_long, by = c("partid", "aspect_ind")) %>% 
  select(soe, r_risk)

cor(soe_dat, method = "pearson")
cor(soe_dat, method = "spearman")

postResample(soe_dat$soe, soe_dat$r_risk)

mean(sign(soe_dat$soe) == sign(soe_dat$r_risk))

plot(soe_dat$soe, soe_dat$r_risk)
