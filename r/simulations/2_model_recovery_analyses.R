library(data.table)
library(tidyverse)
library(scales)
theme_set(theme_bw())

# get and prepare data (we don't look at mean of evidence as it can be mathematically
# shown that it is a special case of VUM with phi = 1)
model_recov <- fread("data/simulations/model_recovery/model_recov.csv")
model_recov <- model_recov[!is.na(i.sim), ]

# Table of r2
model_recov[, .(R2 = mean(r2)), by = c("simulating.model", "fitting.model")]

best_models <- model_recov[, .(best.fitting.model = fitting.model[r2 == max(r2)]),
                           by = c("i.sim", "simulating.model")]
model_recov_table <- prop.table(table(best_models$simulating.model,
                                      best_models$best.fitting.model), 1)

write.table(x = data.frame(unclass(model_recov_table)),
            file = "data/simulations/model_recovery/model_recov_table_r2.csv", sep = ";")

# Plot model recovery based on r2 as barplot
ggplot(best_models, aes(best.fitting.model)) +
  geom_bar(aes(fill = best.fitting.model)) + 
  facet_wrap(~simulating.model) + 
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(name = "Best fitting models") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Best fitting models") +
  ylab("Percentage") +
  ggtitle(bquote("Model Recovery based on R"^2))

ggsave("plots/simulations/model_recov_r2.pdf")

# best fitting model table
best_models <- model_recov[, .(best.fitting.model = fitting.model[loglik == min(loglik)]),
                           by = c("simulating.model", "i.sim")]
model_recov_table <- prop.table(table(best_models$simulating.model,
                                      best_models$best.fitting.model), 1)

write.table(x = data.frame(unclass(model_recov_table)),
            file = "data/simulations/model_recovery/model_recov_table_loglik.csv",
            sep = ";")

# Plot model recovery based on log likelihood as barplot
ggplot(best_models, aes(best.fitting.model)) +
  geom_bar(aes(fill = best.fitting.model)) + 
  facet_wrap(~simulating.model) + 
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(name = "Best fitting models") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Best fitting models") +
  ylab("Percentage") +
  ggtitle("Model Recovery based on Log Likelihood")

ggsave("plots/simulations/model_recov_loglik.pdf")

# Relative evidence weight of r2
sumr2 <- model_recov[, .(sum_r2 = sum(r2)), by = list(i.sim, N, error_sd,
                                                      simulating.model)]
model_recov <- merge(model_recov, sumr2, by = c("i.sim", "N", "error_sd",
                                                "simulating.model"))
model_recov[, rel_ev_weight := r2 / sum_r2]

# Plot model comparison using r2 raw
ggplot(model_recov, aes(x = fitting.model, y = r2)) +
  geom_boxplot() +
  facet_wrap(~simulating.model) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Fitting models") +
  ylab(bquote("R"^2)) +
  ggtitle("R2 of Fitting Models Splitted by Simulating Models")

ggsave("plots/simulations/r2_boxplot.pdf")

model_recov <- model_recov %>%
  mutate(simulating.model = case_when(simulating.model == "mostext" ~ "EXTREME",
                                      simulating.model == "qt" ~ "QT",
                                      simulating.model == "sumofevid" ~ "SUM",
                                      simulating.model == "takethefirst" ~ "FIRST",
                                      simulating.model == "takethelast" ~ "LAST",
                                      simulating.model == "meanofevid" ~ "MEAN",
                                      simulating.model == "tally" ~ "UNIT",
                                      simulating.model == "vu" ~ "VUM"),
         fitting.model = case_when(fitting.model == "mostext" ~ "EXTREME",
                                   fitting.model == "qt" ~ "QT",
                                   fitting.model == "sumofevid" ~ "WADD",
                                   fitting.model == "takethefirst" ~ "FIRST",
                                   fitting.model == "takethelast" ~ "LAST",
                                   fitting.model == "meanofevid" ~ "MEAN",
                                   fitting.model == "tally" ~ "UNIT",
                                   fitting.model == "vu" ~ "VUM"))
  
model_recov$use_color <- ifelse(model_recov$simulating.model == model_recov$fitting.model,
                                "black", "grey")

# Plot model comparison using relative evidence weights
ggplot(model_recov, aes(x = fitting.model, y = rel_ev_weight, color = use_color)) +
      geom_boxplot(alpha = .4) +
      facet_wrap(~simulating.model) +
      ylim(c(0, .85)) +
      xlab("Fitted models") +
      ylab("Relative evidence weight") +
      theme(strip.text.x = element_text(size = 12),
            axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15,face = "bold"),
            legend.position = "none") + 
  scale_color_manual(values = alpha(c("black", "grey40"), .9))

ggsave("plots/simulations/relative_evidence_weights_boxplot.pdf",
       width = 9, height = 7)
