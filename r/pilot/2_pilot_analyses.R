# Anlysis Script Pilot Study
library(tidyverse)
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

# correlation dataset with correlations as a function of aspect position
cors <- pilot_long %>%
  group_by(aspect_ind_cat) %>%
  summarise(
    cors = cor(r_risk, rating, use = "pairwise", method = "spearman"),
    N = n()
  )

ggplot(pilot_long, aes(x = r_risk, y = rating)) +
  geom_point(col = "#8c9196") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(data = cors, aes(label = paste0("rho = ", round(cors, 2), "; N = ", N)), 
             x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2)+
  facet_wrap(~ aspect_ind_cat, ncol = 3) +
  theme_bw() +
  labs(
    x = "Aspect's strength of evidence",
    y = "Risk-taking propensity"
  ) + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

ggsave("plots/pilot/evidencestrength_propensity_corrs.pdf", width = 8, height = 6)
