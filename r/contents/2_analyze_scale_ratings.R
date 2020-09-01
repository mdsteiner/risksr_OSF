library(tidyverse)
library(vegan)
library(forcats)
library(viridis)


### Load and prepare data ======================================================
scales <- read_csv("data/scale_items.csv")
aspects <- read_csv("data/contents/scales/aspects.csv", na = "NULL")
sessions <- read_csv("data/contents/scales/rating_sessions.csv", na = "NULL")
properties <- read_csv("data/contents/scales/properties.csv")
domains <- read_csv("data/contents/scales/domains.csv")

aspects <- aspects %>% 
  left_join(scales, by = c("aspect", "aspect_id", "partid", "aspect_ind")) %>% 
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
  select(-contains(c("r03u949r33", "rik0vddciu", "riohvnaedc")))

# sessions
sessions %>% 
  group_by(ratid) %>% 
  summarise(
    m_dur = sum(session_dur, na.rm = TRUE) / 60
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
  create_mds()


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
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Properties
aspects %>% 
  select(aspect_id, gains:ses, -nonword, -relativize) %>% 
  pivot_longer(cols = gains:ses, names_to = "property_id",
               values_to = "rating") %>% 
  group_by(property_id) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup() %>% 
  left_join(properties, by = c("property_id")) %>% 
  mutate(property = fct_reorder(property, category)) %>% 
  rename(Category = category) %>% 
  ggplot(aes(property, rating, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = viridis(6)) +
  labs(x = "Property", y = "Proportion") +
  theme_bw() +
  theme( 
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold")
  )

### Domain recoveries ==========================================================
# by domain
aspects %>% 
  select(domain_id, ethical:mate_retention) %>% 
  mutate(domains_rated = apply(select(., ethical:mate_retention), 1, function(x, nam){
    paste(nam[which(x == 1)], collapse = "; ")
    }, nam = names(.)[-1])) %>% 
  mutate(
    recovered = apply(select(., domain_id, domains_rated), 1,
                      function(x) grepl(x[1], x[2])),
    single_recovered = apply(select(., domain_id, domains_rated), 1,
                             function(x) grepl(x[1], x[2]) & nchar(x[1]) < nchar(x[2]))) %>% 
  group_by(domain_id) %>% 
  summarise(
    m_recovered = mean(recovered),
    N = n()
  )

# across domains
aspects %>% 
  select(domain_id, ethical:mate_retention) %>% 
  mutate(domains_rated = apply(select(., ethical:mate_retention), 1, function(x, nam){
    paste(nam[which(x == 1)], collapse = "; ")
  }, nam = names(.)[-1])) %>% 
  mutate(
    recovered = apply(select(., domain_id, domains_rated), 1,
                      function(x) grepl(x[1], x[2])),
    single_recovered = apply(select(., domain_id, domains_rated), 1,
                             function(x) grepl(x[1], x[2]) & nchar(x[1]) < nchar(x[2]))) %>% 
  summarise(
    m_recovered = mean(recovered),
    N = n()
  )
