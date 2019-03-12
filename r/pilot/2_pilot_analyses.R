# Anlysis Script Pilot Study
library(tidyverse)
library(patchwork)
library(psych)
library(tidytext)
library(rstanarm)
options(mc.cores = parallel::detectCores())

pilot <- read_csv("data/pilot/pilot.csv")
pilot_long <- read_csv("data/pilot/pilot_long.csv")

### gauge n aspects ============================================================

describe(pilot[, c("n_seeking", "n_avoiding", "n_aspects")])


ggplot(pilot, aes(x = n_aspects)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Number of Aspects Listed",
       y = "Proportion") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )


### Investigating the impact of aspect-rating order and the SOEP ratings =======

# rating
cond_mod_rat <- stan_aov(rating ~ cond_soepframe * cond_order, data = pilot,
                     prior = NULL, iter = 4000, chains = 4)

cond_mod_rat$stan_summary[, c("mean", "2.5%", "97.5%")]

# n_aspects
cond_mod_n_asp <- stan_aov(n_aspects ~ cond_soepframe * cond_order,
                           data = pilot, prior = NULL, iter = 4000,
                           chains = 4)

cond_mod_n_asp$stan_summary[, c("mean", "2.5%", "97.5%")]

# histogram of SOEP ratings ====================================================
vline_data <- pilot %>%
  group_by(cond_soepframe, cond_order) %>%
  summarize(z = mean(rating))

ggplot(pilot, aes(x = rating)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "SOEP Rating",
       y = "Proportion") +
  facet_grid(cond_order ~ cond_soepframe) +
  geom_vline(aes(xintercept = z), vline_data, colour = "#d20537", size = 1.25,
             linetype = 2) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )


### Frequency ratings ==========================================================

vline_data <- pilot %>%
  group_by(cond_soepframe, cond_order) %>%
  summarize(z = mean(med_frequency))

ggplot(pilot, aes(x = med_frequency)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Median Frequency Rating",
       y = "Proportion") +
  facet_grid(cond_order ~ cond_soepframe) +
  geom_vline(aes(xintercept = z), vline_data, colour = "#d20537", size = 1.25,
             linetype = 2) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

### situations ratings =========================================================

vline_data <- pilot %>%
  group_by(cond_soepframe, cond_order) %>%
  summarize(z = mean(med_situation))

ggplot(pilot, aes(x = med_situation)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Median Situation Rating",
       y = "Proportion") +
  facet_grid(cond_order ~ cond_soepframe) +
  geom_vline(aes(xintercept = z), vline_data, colour = "#d20537", size = 1.25,
             linetype = 2) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )


### social comparison ratings ==================================================

vline_data <- pilot %>%
  group_by(cond_soepframe, cond_order) %>%
  summarize(z = mean(med_social))

ggplot(pilot, aes(x = med_social)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Median Social Rating",
       y = "Proportion") +
  facet_grid(cond_order ~ cond_soepframe) +
  geom_vline(aes(xintercept = z), vline_data, colour = "#d20537", size = 1.25,
             linetype = 2) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

### pairs panel plot for the different sources and contents ratings ============
pdf("plots/pilot/ratings_panel.pdf", width = 6, height = 6)
pilot %>%
  select(starts_with("med_")) %>%
  rename(riskiness = med_risk,
         social = med_social,
         situation = med_situation,
         frequency = med_frequency) %>%
  pairs.panels(method = "spearman", lm = TRUE, hist.col = "skyblue", breaks = 10,
               cex = 1, cex.labels = 2)
dev.off()
### sentiment ratings ==========================================================

vline_data <- pilot %>%
  group_by(cond_soepframe, cond_order) %>%
  summarize(z = mean(m_sentiment, na.rm = TRUE))

ggplot(pilot, aes(x = m_sentiment)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Mean Sentiment Rating",
       y = "Proportion") +
  facet_grid(cond_order ~ cond_soepframe) +
  geom_vline(aes(xintercept = z), vline_data, colour = "#d20537", size = 1.25,
             linetype = 2) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

### Correlations of aspect position and risk ratings ===========================

# overall correlation
cor(pilot$rating, pilot$med_risk, method = "spearman")

pilot_long <- pilot_long %>%
  mutate(n_aspects = n_seeking + n_avoiding,
         n_aspects_cat = case_when(n_aspects == 1 ~ "1 Aspect",
                                   n_aspects == 2 ~ "2 Aspects",
                                   n_aspects == 3 ~ "3 Aspects",
                                   n_aspects == 4 ~ "4 Aspects",
                                   n_aspects == 5 ~ "5 Aspects",
                                   n_aspects >= 6 ~ "6+ Aspects"))

# correlation dataset with correlations as a function of aspect position
cors <- pilot_long %>%
  group_by(aspect_ind_cat, n_aspects_cat) %>%
  summarise(
    cors = cor(r_risk, rating, use = "pairwise", method = "spearman"),
    N = n()
  )

# show n participants per row
cors %>% group_by(n_aspects_cat) %>% summarise(nn = N[1])

ggplot(pilot_long, aes(x = r_risk, y = rating)) +
  geom_point(col = "#8c9196") +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  geom_label(data = cors, aes(label = paste0("rho = ", round(cors, 2))), 
             x = 3, y = 10.1)+
  facet_grid(n_aspects_cat ~ aspect_ind_cat) +
  theme_bw() +
  labs(
    x = "Aspect's Strength of Evidence",
    y = "Risk-Taking Propensity"
  ) + 
  ylim(-.1, 11) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

ggsave("plots/pilot/evidencestrength_propensity_corrs_grid.pdf",
       width = 14, height = 8)


### Variation in Ratings ===========================

# correlation dataset with correlations as a function of aspect position
p_risk <- pilot_long %>% 
  filter(n_aspects_cat != "1 Aspect") %>%
  mutate(aspect_ind_cat = substr(aspect_ind_cat, 8, 9)) %>%
  ggplot(aes(x = aspect_ind_cat, y = r_risk, group = partid)) +
  geom_point(alpha = .2) + 
  geom_line(alpha = .2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, group = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1) +
  facet_wrap( ~ n_aspects_cat, ncol = 5, scales = "free_x") +
  theme_bw() +
  labs(
    x = "",
    y = "Risk"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )



# correlation dataset with correlations as a function of aspect position
p_social <- pilot_long %>% 
  filter(n_aspects_cat != "1 Aspect") %>%
  mutate(aspect_ind_cat = substr(aspect_ind_cat, 8, 9)) %>%
  ggplot(aes(x = aspect_ind_cat, y = r_social, group = partid)) +
  geom_point(alpha = .2) + 
  geom_line(alpha = .2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, group = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1) +
  facet_wrap( ~ n_aspects_cat, ncol = 5, scales = "free_x") +
  theme_bw() +
  labs(
    x = "",
    y = "Social"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

# correlation dataset with correlations as a function of aspect position
p_situation <- pilot_long %>% 
  filter(n_aspects_cat != "1 Aspect") %>%
  mutate(aspect_ind_cat = substr(aspect_ind_cat, 8, 9)) %>%
  ggplot(aes(x = aspect_ind_cat, y = r_situation, group = partid)) +
  geom_point(alpha = .2) + 
  geom_line(alpha = .2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, group = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1) +
  facet_wrap( ~ n_aspects_cat, ncol = 5, scales = "free_x") +
  theme_bw() +
  labs(
    x = "",
    y = "Situation"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )


# correlation dataset with correlations as a function of aspect position
p_frequency <- pilot_long %>% 
  filter(n_aspects_cat != "1 Aspect") %>%
  mutate(aspect_ind_cat = substr(aspect_ind_cat, 8, 9)) %>%
  ggplot(aes(x = aspect_ind_cat, y = r_frequency, group = partid)) +
  geom_point(alpha = .2) + 
  geom_line(alpha = .2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, group = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1) +
  facet_wrap( ~ n_aspects_cat, ncol = 5, scales = "free_x") +
  theme_bw() +
  labs(
    x = "Aspect Position",
    y = "Frequency"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )


pdf("plots/pilot/ratings_variation.pdf",
    width = 12, height = 6)

p_risk + p_social + p_situation + p_frequency + plot_layout(ncol = 1)
dev.off()

