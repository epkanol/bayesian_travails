# the below initially from Anders' code
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(brms)
library(dagitty)
library(stringr)
library(rethinking)
library(bayesplot)

df <- read.csv("samples/authors-team-impact.csv")

scale_cloc      <- data.frame(t(c(count=1771, mean=123.6285, stddev=303.9526, median=56)))
scale_mccabe    <- data.frame(t(c(count=1771, mean=13.70073, stddev=40.93051, median=5)))
scale_duplines  <- data.frame(t(c(count=1771, mean=18.04743, stddev=59.00211, median=0)))
scale_dupblocks <- data.frame(t(c(count=1771, mean=0.9994353, stddev=3.071005, median=0)))
scale_added <- df %>% summarize(count=n(), mean=mean(added), stddev=sd(added), median=median(added))
scale_removed <- df %>% summarize(count=n(), mean=mean(removed), stddev=sd(removed), median=median(removed))

data_centered <- df %>% mutate(file=fileid, author=authorid, team=authorteamid, 
                               ADD=(added-scale_added$mean)/scale_added$stddev, 
                               DEL=(removed-scale_removed$mean)/scale_removed$stddev, 
                               CLOC=(currCloc-scale_cloc$mean)/scale_cloc$stddev, 
                               COMPLEX=(currComplex-scale_mccabe$mean)/scale_mccabe$stddev, 
                               DUP=(prevDupBlocks-scale_dupblocks$mean)/scale_dupblocks$stddev,
                               INTROD=if_else(delta >= 0, delta, as.integer(0)),
                               REMOVED=if_else(delta <= 0, delta, as.integer(0)),
                               y=INTROD) %>%
  select(file, author, team, ADD, DUP, y)

data_scaled <- df %>% mutate(file=fileid, author=authorid, team=authorteamid, 
                             ADD=(added)/scale_added$stddev, 
                             DEL=(removed)/scale_removed$stddev, 
                             CLOC=(currCloc)/scale_cloc$stddev, 
                             COMPLEX=(currComplex)/scale_mccabe$stddev, 
                             DUP=(prevDupBlocks)/scale_dupblocks$stddev, 
                             INTROD=if_else(delta >= 0, delta, as.integer(0)),
                             REMOVED=if_else(delta <= 0, delta, as.integer(0)),
                             y=INTROD) %>%
  select(file, author, team, ADD, DUP, y)
data <- data_scaled

data %>% summarize(count = n(), mean(y), sd(y))

data %>% filter(y > 0) %>% summarize(count = n(), mean(y), sd(y))

# ADD added lines for each commit
# DUP num of duplicated code snippet before commit
# y num of introduced duplicates

# Richard's additions
# Using a ziNB since a ziPoisson is likely not a good fit
# Create a null model
# m0 <-
#   brm(data = data,
#       family = zero_inflated_negbinomial,
#       y ~ 1,
#       chains = 4, cores = 4, threads = threading(2))
#
# Our proposed model where I only sample from priors
# I've only done very rudimentary PriPC ad hoc in terminal
# m_prior <-
#   brm(data = data,
#       family = zero_inflated_negbinomial,
#       y ~ 1 + author + (1 + author | team),
#       prior = c(prior(normal(0, 2), class = Intercept),
#                 prior(normal(0, 0.5), class = b),
#                 prior(exponential(1), class = sd),
#                 prior(lkj(2), class = cor)),
#       chains = 4, cores = 4, threads = threading(2), sample_prior = "only")
#
# A model where each team gets an intercept, and then we have individual slopes
# for each author (in each team) (varying effects model)
# I have 8 cores so I run two threads on each core with: threads = threading(2)
m_nb <-
  brm(data = data,
      family = zero_inflated_negbinomial,
      y ~ 0 + author + (1 + author | team) + ADD + DUP, # fix
      prior = c(prior(normal(0, 0.25), class = b),
                prior(weibull(2, 1), class = sd),
                prior(lkj(2), class = cor),
                prior(beta(1, 1), class = zi),
                prior(gamma(0.01, 0.01), class = shape)),
      chains = 4, cores = 4, threads = threading(2))

# Output:
#  Family: zero_inflated_negbinomial 
#   Links: mu = log; shape = identity; zi = identity 
# Formula: y ~ 0 + author + (0 + author | team) 
#    Data: data (Number of observations: 9413) 
#   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#          total post-warmup draws = 4000

# Group-Level Effects: 
# ~team (Number of levels: 11) 
#            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(author)     0.01      0.01     0.01     0.03 1.00     1371     1836

# Population-Level Effects: 
#        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# author    -0.02      0.01    -0.03    -0.01 1.00     1340     1944

# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     0.21      0.03     0.15     0.28 1.00     2430     2234
# zi        0.76      0.03     0.70     0.80 1.00     2288     2373

# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# Diagnostics
stopifnot(rhat(m_nb) < 1.01)
stopifnot(neff_ratio(m_nb) > 0.20)
mcmc_trace(m_nb) # ok
np <- nuts_params(m_nb)
lp <- log_posterior(m_nb)
mcmc_nuts_divergence(np, lp) # ok
loo(m_nb) # ok

# Check our prior v. posterior concerning cor
# check how the posterior for the correlation of the random effects
# compares to prior
post <- as_draws_df(m_nb)

# we used lkj(2)
r_2 <-
  rlkjcorr(1e4, K = 2, eta = 2) |>
  data.frame()

# # plot and compare
# # fairly strong negative correlations, and data has told its story
post %>%
      ggplot() +
      geom_density(data = r_2, aes(x = X2),
                   color = "transparent", fill = "blue", alpha = 3/4) +
      geom_density(aes(x = cor_team__Intercept__author),
                   color = "transparent", fill = "#A65141", alpha = 9/10) +
      annotate(geom = "text", 
               x = c(-0.6, 0), y = c(2.2, 1.0), 
               label = c("posterior", "prior"), 
               color = c("#A65141", "blue"), family = "Courier") +
      scale_y_continuous(NULL, breaks = NULL) +
      labs(subtitle = "Correlation between intercepts and slopes, prior and posterior",
      x = "correlation")
# it seems to be strong negative effects but we clearly see data has told its
# story

# How does the variance differ bw team and author?
mcmc_areas_ridges(m_nb, regex_pars = "sd_team__")

# between teams and authors?
mcmc_areas_ridges(m_nb, pars = c("r_team[1,author]","r_team[2,author]","r_team[3,author]", "r_team[4,author]","r_team[5,author]","r_team[6,author]","r_team[7,author]","r_team[8,author]","r_team[9,author]","r_team[10,author]","r_team[11,author]"), prob = 0.95)

# between teams
mcmc_areas_ridges(m_nb, pars = c("r_team[1,Intercept]","r_team[2,Intercept]","r_team[3,Intercept]","r_team[4,Intercept]","r_team[5,Intercept]","r_team[6,Intercept]","r_team[7,Intercept]","r_team[8,Intercept]","r_team[9,Intercept]","r_team[10,Intercept]",
    "r_team[11,Intercept]"), prob = 0.95)
