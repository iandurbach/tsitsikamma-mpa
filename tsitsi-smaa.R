library(tidyverse)
library(reshape2)
library(hitandrun)
library(smaa)
library(viridis)

load("tsitsi-data.RData")

source("tsitsi-fns.R")
    
### analysis 1: just as is, no discounting

# ranking at t_0
scores_t0 <- (as.matrix(data_t0) %*% matrix(wts,ncol=1))
sc_t0 <- scores_t0[order(scores_t0,decreasing = T),]

# ranking at t_5
scores_t5 <- (as.matrix(data_t5) %*% matrix(wts,ncol=1))
sc_t5 <- scores_t5[order(scores_t5,decreasing = T),]

# ranking at t_10
scores_t10 <- (as.matrix(data_t10) %*% matrix(wts,ncol=1))
sc_t10 <- scores_t10[order(scores_t10,decreasing = T),]

# overall ranking depends on discount rate, first try no discounting
ddata <- get_scores(d=0, dt0 = data_t0, dt5 = data_t5, dt10 = data_t10)
scores <- (as.matrix(ddata) %*% matrix(wts,ncol=1))
sc_all <- scores[order(scores,decreasing = T),]

# heavy discounting won't change much, because similar rankings at each t
ddata <- get_scores(d=0.06, dt0 = data_t0, dt5 = data_t5, dt10 = data_t10)
scores <- (as.matrix(ddata) %*% matrix(wts,ncol=1))
sc_d6 <- scores[order(scores,decreasing = T),]

ddata <- get_scores(d=0.1, dt0 = data_t0, dt5 = data_t5, dt10 = data_t10)
scores <- (as.matrix(ddata) %*% matrix(wts,ncol=1))
sc_d10 <- scores[order(scores,decreasing = T),]

res_knownwts <- cbind(sc_t0,sc_t5,sc_t10,sc_all,sc_d6,sc_d10)

write.csv(res_knownwts,"results/scores-with-given-wts.csv")

# weights are uncertain and stakeholder dependent, vary these with SMAA

N <- 300000
names.att <- names(data_t0)
names.alt <- row.names(data_t0)

meas <- smaa_copy(data_t0, N = N)
smaa_t0 <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_t0.png", smaa_t0$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_t0.png", smaa_t0$plotcw_red, width=7, height=6.5, dpi=200)

meas <- smaa_copy(data_t5, N = N)
smaa_t5 <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_t5.png", smaa_t5$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_t5.png", smaa_t5$plotcw_red, width=7, height=6.5, dpi=200)

meas <- smaa_copy(data_t10, N = N)
smaa_t10 <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_t10.png", smaa_t10$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_t10.png", smaa_t10$plotcw_red, width=7, height=6.5, dpi=200)

meas <- smaa_copy(get_scores(d = 0.05, dt0 = data_t0, dt5 = data_t5, dt10 = data_t10), N = N)
smaa_tall <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_tall.png", smaa_tall$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_tall.png", smaa_tall$plotcw_red, width=7, height=6.5, dpi=200)

# assessments of management scenarios also uncertain, test these

# generic uncertainty, applied to all scenarios equally
meas_t0u <- apply_uncertainty(meas = smaa_copy(data_t0, N = N), alts = 1:7, ecou = c(-10,10), envu = c(-10,10))
meas_t5u <- apply_uncertainty(meas = smaa_copy(data_t5, N = N), alts = 1:7, ecou = c(-20,20), envu = c(-20,20))
meas_t10u <- apply_uncertainty(meas = smaa_copy(data_t10, N = N), alts = 1:7, ecou = c(-30,30), envu = c(-30,30))

meas <- get_scores(d=0.06, dt0 = meas_t0u, dt5 = meas_t5u, dt10 = meas_t10u)
smaa_tall_wu <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_tall_wu.png", smaa_tall_wu$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_tall_wu.png", smaa_tall_wu$plotcw_red, width=7, height=6.5, dpi=200)
rm(meas_t0u, meas_t5u, meas_t10u)

# only downside uncertainty, applied to MPA closed scenarios
meas_t0u <- apply_uncertainty(meas = smaa_copy(data_t0, N = N), alts = 1:2, ecou = c(-10,10), envu = c(-10,0))
meas_t0u <- apply_uncertainty(meas = meas_t0u, alts = 3:7, ecou = c(-10,10), envu = c(0,10))
meas_t5u <- apply_uncertainty(meas = smaa_copy(data_t5, N = N), alts = 1:2, ecou = c(-10,10), envu = c(-20,0))
meas_t5u <- apply_uncertainty(meas = meas_t5u, alts = 3:7, ecou = c(-20,20), envu = c(0,20))
meas_t10u <- apply_uncertainty(meas = smaa_copy(data_t10, N = N), alts = 1:2, ecou = c(-10,10), envu = c(-30,0))
meas_t10u <- apply_uncertainty(meas = meas_t10u, alts = 3:7, ecou = c(-30,30), envu = c(0,30))

meas <- get_scores(d=0.06, dt0 = meas_t0u, dt5 = meas_t5u, dt10 = meas_t10u)
smaa_tall_wu2 <- do_smaa(meas = meas, attnames = names.att, altnames = names.alt)
ggsave("results/ai_tall_wu_antiMPA.png", smaa_tall_wu2$plotra, width=7, height=6.5, dpi=200)
ggsave("results/cw_tall_wu_antiMPA.png", smaa_tall_wu2$plotcw_red, width=7, height=6.5, dpi=200)
rm(meas, meas_t0u, meas_t5u, meas_t10u)

#mydata_red <- data.frame(Env.Scarcity = apply(mydata[,1:3],1,sum),
#                         Env.Risk = apply(mydata[,4:6],1,sum),
#                         Eco = apply(mydata[,7:11],1,sum),
#                         Eq.Soc.CR = mydata[,12],
#                         Eq.Soc.PR = mydata[,13],
#                         Eq.Soc.NR = apply(mydata[,14:16],1,sum),
#                         Eq.PolGain = apply(mydata[,17:20],1,sum))


