# Script to create the plots with the model correlations shown in the Appendix
library(corrplot)
library(data.table)
library(tidyverse)
library(scales)


# Load data
files <- list.files(path = "data/simulations/learningset_predictions/", pattern = "b179",
                    full.names = TRUE)
learn_pred <- do.call(rbind, lapply(files, fread, fill = TRUE))

learn_pred <- learn_pred[!is.na(i.sim), ]
learn_pred[, c("pred_raw", "pred_noise") := NULL]

learn_pred <- learn_pred %>%
  mutate(simulating.model = case_when(simulating.model == "mostext" ~ "EXTREME",
                                      simulating.model == "qt" ~ "QT",
                                      simulating.model == "sumofevid" ~ "WADD",
                                      simulating.model == "takethefirst" ~ "FIRST",
                                      simulating.model == "takethelast" ~ "LAST",
                                      simulating.model == "meanofevid" ~ "MEAN",
                                      simulating.model == "tally" ~ "UNIT",
                                      simulating.model == "vu" ~ "VUM"))

cormats <- list(cor_mats_11 = list(), cor_mats_12 = list(), cor_mats_13 = list(),
                cor_mats_21 = list(), cor_mats_22 = list(), cor_mats_23 = list(),
                cor_mats_31 = list(), cor_mats_32 = list(), cor_mats_33 = list(),
                cor_mats_41 = list(), cor_mats_42 = list(), cor_mats_43 = list())

mat_ind <- names(cormats)
mat_ind <- cbind(mat_ind, expand.grid(c(150, 200, 250), c(0, 1, 2, 3)))

for (ii in 1:max(learn_pred$i.sim)) {
  
  df <- learn_pred[learn_pred$i.sim == ii, ]
  
  # four error criteria, eight models, three Ns
  df$id <- c(rep(1:150, 4*8), rep(1:200, 4*8), rep(1:250, 4*8))
  
  df_wide <- df %>%
    spread(simulating.model, pred_noise_rescale)
  
  for (jj in 1:nrow(mat_ind)) {
    cormats[[mat_ind$mat_ind[jj]]][[ii]] <-
      cor(df_wide[df_wide$N == mat_ind$Var1[jj] &
                    df_wide$error_sd == mat_ind$Var2[jj], 5:12])
  }
}


m_cormats <- lapply(cormats, function(x) apply(simplify2array(x), 1:2, mean))

pdf(file="plots/simulations/model_correlations.pdf", width=9, height = 12)

layout(matrix(c(1, 1, 1, 1, 2:17), 5, 4, byrow = TRUE),
       widths=c(1, 10, 10, 10), heights=c(1, 10, 10, 10, 10))

# first row indicating number of observations
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(c("N = 150", "N = 200", "N = 250"), x = c(1 / 13 * c(3, 7, 11)), y = c(.5),
     font = 2, cex = 2)

# error = 0
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(expression(bold(paste(bold(sigma[e]), " = 0"))), x = c(.5), y = c(.5),
     font = 2, cex = 2, srt = 90)


# corrplots
corrplot(m_cormats[[1]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[2]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[3]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")

# error = 1
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(expression(bold(paste(bold(sigma[e]), " = 1"))), x = c(.5), y = c(.5),
     font = 2, cex = 2, srt = 90)

# corrplots
corrplot(m_cormats[[4]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[5]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[6]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")


# error = 2
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(expression(bold(paste(bold(sigma[e]), " = 2"))), x = c(.5), y = c(.5),
     font = 2, cex = 2, srt = 90)

# corrplots
corrplot(m_cormats[[7]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[8]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[9]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")


# error = 3
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(expression(bold(paste(bold(sigma[e]), " = 3"))), x = c(.5), y = c(.5),
     font = 2, cex = 2, srt = 90)

# corrplots
corrplot(m_cormats[[10]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[11]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")
corrplot(m_cormats[[12]], method="color", type="lower", diag=TRUE, is.cor=TRUE,
         mar = c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=0.85,
         tl.col="black", cl.cex=.8, title = "")

dev.off()