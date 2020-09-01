library(tidyverse)


### Load and prepare data ======================================================

aspects <- read_csv("data/contents/pilot/aspects.csv", na = "NULL")
sessions <- read_csv("data/contents/pilot/rating_sessions.csv", na = "NULL")
domains <- read_csv("data/contents/pilot/domains.csv")
properties <- read_csv("data/contents/pilot/properties.csv")

aspects <- aspects %>% 
  mutate(
    # domains
    ethical = rowMeans(select(., starts_with("ethical")), na.rm = TRUE),
    financial = rowMeans(select(., starts_with("financial")), na.rm = TRUE),
    health_safety = rowMeans(select(., starts_with("health_safety")), na.rm = TRUE),
    recreational = rowMeans(select(., starts_with("recreational")), na.rm = TRUE),
    social = rowMeans(select(., starts_with("social")), na.rm = TRUE),
    faith = rowMeans(select(., starts_with("faith")), na.rm = TRUE),
    traffic = rowMeans(select(., starts_with("traffic")), na.rm = TRUE),
    occupation = rowMeans(select(., starts_with("occupation")), na.rm = TRUE),
    general = rowMeans(select(., starts_with("general")), na.rm = TRUE),
    competition_between = rowMeans(select(., starts_with("competition_between")), na.rm = TRUE),
    competition_within = rowMeans(select(., starts_with("competition_within")), na.rm = TRUE),
    status_power = rowMeans(select(., starts_with("status_power")), na.rm = TRUE),
    exploration = rowMeans(select(., starts_with("exploration")), na.rm = TRUE),
    food_selection = rowMeans(select(., starts_with("food_selection")), na.rm = TRUE),
    food_acquisition = rowMeans(select(., starts_with("food_acquisition")), na.rm = TRUE),
    parent_offspring_conflict = rowMeans(select(.,starts_with("parent_offspring_conflict")), na.rm = TRUE),
    kinship = rowMeans(select(., starts_with("kinship")), na.rm = TRUE),
    mate_attraction = rowMeans(select(., starts_with("mate_attraction")), na.rm = TRUE),
    mate_retention = rowMeans(select(., starts_with("mate_retention")), na.rm = TRUE),
    
    # properties
    gains = rowMeans(select(., starts_with("gains")), na.rm = TRUE),
    p_gain = rowMeans(select(., starts_with("p_gain")), na.rm = TRUE),
    losses = rowMeans(select(., starts_with("losses")), na.rm = TRUE),
    p_loss = rowMeans(select(., starts_with("p_loss")), na.rm = TRUE),
    need = rowMeans(select(., starts_with("need")), na.rm = TRUE),
    ability = rowMeans(select(., starts_with("ability")), na.rm = TRUE),
    keep_status_quo = rowMeans(select(., starts_with("keep_status_quo")), na.rm = TRUE),
    change_status_quo = rowMeans(select(., starts_with("change_status_quo")), na.rm = TRUE),
    experience = rowMeans(select(., starts_with("experience")), na.rm = TRUE),
    signaling = rowMeans(select(., starts_with("signaling")), na.rm = TRUE),
    social_norm = rowMeans(select(., starts_with("social_norm")), na.rm = TRUE),
    personality = rowMeans(select(., starts_with("personality")), na.rm = TRUE),
    religion_fate = rowMeans(select(., starts_with("religion_fate")), na.rm = TRUE),
    pos_affect = rowMeans(select(., starts_with("pos_affect")), na.rm = TRUE),
    thrill = rowMeans(select(., starts_with("thrill")), na.rm = TRUE),
    regret = rowMeans(select(., starts_with("regret")), na.rm = TRUE),
    fear = rowMeans(select(., starts_with("fear")), na.rm = TRUE),
    neg_aff_oth = rowMeans(select(., starts_with("neg_aff_oth")), na.rm = TRUE),
    long_term = rowMeans(select(., starts_with("long_term")), na.rm = TRUE),
    short_term = rowMeans(select(., starts_with("short_term")), na.rm = TRUE),
    age = rowMeans(select(., starts_with("age")), na.rm = TRUE),
    sex = rowMeans(select(., starts_with("sex")), na.rm = TRUE),
    parental_status = rowMeans(select(., starts_with("parental_status")), na.rm = TRUE),
    birth_order = rowMeans(select(., starts_with("birth_order")), na.rm = TRUE),
    n_siblings = rowMeans(select(., starts_with("n_siblings")), na.rm = TRUE),
    rep_goal = rowMeans(select(., starts_with("rep_goal")), na.rm = TRUE),
    life_exp = rowMeans(select(., starts_with("life_exp")), na.rm = TRUE),
    family = rowMeans(select(., starts_with("family")), na.rm = TRUE),
    relativize = rowMeans(select(., starts_with("relativize")), na.rm = TRUE),
    nonword = rowMeans(select(., starts_with("nonword")), na.rm = TRUE),
    ses = rowMeans(select(., starts_with("ses")), na.rm = TRUE),
    
    soe = rowMeans(select(., starts_with("soe")), na.rm = TRUE),
    soe_v = apply(select(aspects, starts_with("soe")), 1, sd, na.rm = TRUE),
    soe_min = apply(select(aspects, starts_with("soe")), 1, min, na.rm = TRUE),
    soe_max = apply(select(aspects, starts_with("soe")), 1, max, na.rm = TRUE)) %>% 
  select(-contains(c("rq5uph92ju", "r1414o4nqf", "r7sfbvv0kh", "r4f5uhhj3h",
                     "rjjtklihoh")))



# view domains
aspects %>% select(aspect, ethical:mate_retention, soe:soe_max) %>% View()
aspects %>% select(aspect, gains:ses) %>% View()


# sessions
sessions %>% 
  group_by(ratid) %>% 
  summarise(
    m_dur = mean(session_dur, na.rm = TRUE) / 60,
    min_dur = min(session_du)
  )





