
remove(list = objects())
setwd('~/research/agri/AD/ad_r/')

library(tidyverse)
library(xtable)
# library(reticulate)
# source_python('../nwr_app/nwr_model.py')


# data mgmt ---------------------------------------------------------------

# AD input data
feed_df <- read.csv('feedstocks.csv') %>% 
  replace(is.na(.), 0)

# data units
## lbs: blood, fish
## gal: pulp, ob, gt
## daily manure measurement L/day

# convert L to gal 0.264172 L/gal
# 228000 L d^(-1), Ma et al. (2017)
manure <- rep(228000, nrow(feed_df))*0.264172

bloodfish <- feed_df %>%
  select(contains(c('blood', 'fish'))) %>% 
  transmute(blood = rowSums(.[grep('Blood', names(.))]),
            fish = rowSums(.[grep('Fish', names(.))]))

# convert bloodfish lbs to gal, 
bloodfish <- bloodfish*1/8.34

# data units are in gal
ad_in <- cbind(bloodfish,
               feed_df[grep('Pulp|Sugar|Trap', names(feed_df))],
               manure) %>% 
  as_tibble()

colnames(ad_in)[grep('Pulp|Sugar|Trap', colnames(ad_in))] <- c('pulp', 'ob', 'gt')

# dairy acre data
dairies <- read_csv('WA_Dairies.csv')

acres <- strsplit(dairies$R_Current_Acres, split = ' to ') %>% 
  reduce(rbind) %>% 
  data.frame()

acres <- acres[-which(acres$X1 == '14001 and above'), ] %>%
  sapply(.,
         function(i)
           as.numeric(as.character(i))) %>% 
  data.frame()

acres <- acres %>% 
  mutate(midp = (X1 + X2)/2)

acre_sd <- sd(acres$midp)
ex_acres <- mean(acres$midp)

# 2020 MWh assumed pricing
pMwh <- 69*1.19

# AD model ----------------------------------------------------------------

# AD output and revenue function
# Ma et al. (2017)
## input volume in gal
## convert to l, 3.79 
## convert to kg, 1
ad_fn <- function(X, acres, ndraw, nsd_acres) {
  
  
  # data in lbs, need gal, 0.119826427 lb to gal
  # 3.79 constant converts gallon to liter (1L = 1kg)
  # 0.9112 nutrient portion remaining post liquid-solid separation after AD output to lagoon
  # 0.76 nutrient portion of TKN remaining
  # 2.20462 constant converts kilogram to pound
  # 2.14 m^3 to kWh
  
  # X inputs (blood, fish, pulp, ob, gt, manure)
  X <- as.matrix(X)
  acres_sd <- nsd_acres*acre_sd
  acres <- acres + acres_sd
  
  # nutrient contribution percentages
  p_mu <- c(0.035, 0.107, 0.34, 0.011, 0.13, 0.023)
  p_sd <- c(0.003, 0.005, 0.02, 0, 0.05, 0.007)
  p_draw <- matrix(rnorm(length(p_mu)*ndraw, p_mu, p_sd),
                   ncol = ndraw)/100
  
  n_mu <- c(1.07, 0.69, 1.9, 0.024, 0.3, 0.15)
  n_sd <- c(0.12, 0.32, 0.33, 0, 0.3, 0.04)
  n_draw <- matrix(rnorm(length(n_mu)*ndraw, n_mu, n_sd),
                   ncol = ndraw)/100
  
  # biogas coefs, vs/ts percentage
  bio_mu <- c(0.73, 0.475, 0.59, 1.45, 1.9, 0.75)
  vsts_mu <- c(83.5, 79.8, 72.8, 94.5, 96.6, 67.3)
  vsts_sd <- c(3.5, 4.7, 1.71, 0, 2.1, 0)
  vsts_draw <- matrix(rnorm(length(vsts_mu)*ndraw, vsts_mu, vsts_sd),
                      ncol = ndraw)/100
  
  # tipping fees in 2020 dollars
  tip_fee <- c(0.03609, 0.05, 0.0417, 0.05, 0.05, 0)*1.19
  
  # P and N retention
  p_ret <- 0.9112
  n_ret <- 0.76
  # unit conversions
  gal_kg <- 3.79
  kg_lb <- 2.20462
  
  # computes daily contribution of inputs
  # P and N output, lbs units
  p_samp <- p_ret * ((gal_kg * X) %*% p_draw) * kg_lb
  p_out <- apply(p_samp, 1,
                 function(i)
                   c(mean(i), quantile(i, c(.25,.75)))) %>%
    t()
  
  n_samp <- n_ret * ((gal_kg * X) %*% n_draw) * kg_lb
  n_out <- apply(n_samp, 1,
                 function(i)
                   c(mean(i), quantile(i, c(.25,.75)))) %>%
    t()
  
  # biogas output in MWh
  # 0.01073918 MWh in m3
  mcub_mwh <- 0.01073918
  ## Ma et al. (2017) vs/ts ratio and biogas potential
  ## digester:
  ### 23.5C, 17 day ret, 6100000 L capacity
  ### 7.68 yield factor
  # IRENA formula
  ## biogas production m3/d
  ## Y: temp/time yield factor
  ## Vd: digester volume in m^3
  ## S: initial volatile solids kg/m3
  ##    VS concentration(kg/d) / feedstock vol(m3/d)
  ##    1000 kg in m3
  
  Y <- 7.68
  Vd <- 6100000/1000
  feed_vol <- rowSums(gal_kg * X)/1000
  
  vs_weight_samp <- gal_kg * X %*% vsts_draw
  vs_weight_out <- apply(vs_weight_samp, 1,
                         function(i)
                           c(mean(i), quantile(i, c(.25,.75)))) %>%
    t()
  
  # map2_dbl each vol to weight dist
  S <- sapply(1:nrow(vs_weight_out),
              function(i)
                vs_weight_out[i, ]/feed_vol[i]) %>% 
    t()
  
  bio_out <- ((Y * Vd * S)/1000) * mcub_mwh
  
  # tipping fee revenue
  tip_rev <- c(X %*% tip_fee)
  
  # computes cumulative nutrient lb/acre
  p_lbacre_out <- apply(p_out, 2, function(i) cumsum(i))/acres
  n_lbacre_out <- apply(n_out, 2, function(i) cumsum(i))/acres
  
  out <- list(phos = p_out,
              nit = n_out,
              bio = bio_out,
              rev = tip_rev,
              phos_lbacre = p_lbacre_out,
              nit_lbacre = n_lbacre_out)
  
  return(out)
  
}

# AD nutr application decision function
ad_nutrapp <- function(X, acres, ndraw, nsd_acres,
                       soil_test, loc) {
  
  # 1: apply lagoon contents to field
  # 0: overapplication of nutrients occurs if applied
  
  p_appvol <- ad_fn(X, acres, ndraw, nsd_acres)$phos_lbacre
  
  if (loc == 'west') {
    
    if (soil_test < 20) {
      
      d <- ifelse(p_appvol <= 300, 1, 0)
      
    } else if (soil_test >= 20 & soil_test < 40) {
      
      d <- ifelse(p_appvol <= 200, 1, 0)
      
    } else if (soil_test >= 40 & soil_test < 100) {
      
      d <- ifelse(p_appvol <= 30, 1, 0)
      
    } else {
      
      d <- 0
      
    }
    
  }
  
  if (loc == 'east') {
    
    if (soil_test <= 10) {
      
      d <- ifelse(p_appvol <= 300, 1, 0)
      
    } else if (soil_test >= 10 & soil_test < 25) {
      
      d <- ifelse(p_appvol <= 200, 1, 0)
      
    } else if (soil_test >= 25 & soil_test <= 50) {
      
      d <- ifelse(p_appvol <= 30, 1, 0)
      
    } else {
      
      d <- 0
      
    }
    
  }
  
  d_idx <- which(d == 1)
  
  out <- list(app_desc = d,
              desc_idx = d_idx)
  
  return(out)
  
}


# model results -----------------------------------------------------------

# nutrient production
ad_mod <- ad_fn(ad_in, ex_acres, 1000, 1)

ad_mod_nutr <- bind_cols(1:nrow(ad_in),
                         as_tibble(ad_mod$phos),
                         as_tibble(ad_mod$nit))
colnames(ad_mod_nutr) <- c('day', 'phos', 'phos25', 'phos75',
                           'nit', 'nit25', 'nit75')

ad_mod_pacre <- bind_cols(1:nrow(ad_in),
                          as_tibble(ad_mod$phos_lbacre))
colnames(ad_mod_pacre) <- c('day', 'pacre', 'pacre25', 'pacre75')

ad_mod_bio <- bind_cols(1:nrow(ad_in),
                          as_tibble(ad_mod$bio))
colnames(ad_mod_bio) <- c('day', 'bio', 'bio25', 'bio75')

ad_mod_rev <- bind_cols(1:nrow(ad_in),
                        ad_mod$rev,
                        ad_mod_bio$bio*pMwh*0.1) # sells 10 percent energy
colnames(ad_mod_rev) <- c('day', 't_rev', 'b_rev')

# nutrient application decision
w_opt <- sapply(1:3,
                function(j)
                  sapply(1:2,
                         function(s)
                           mean(ad_nutrapp(ad_in, ex_acres, 1000, s, 15, 'west')$app_desc[, j])))

e_opt <- sapply(1:3,
                function(j)
                  sapply(1:2,
                         function(s)
                           mean(ad_nutrapp(ad_in, ex_acres, 1000, s, 15, 'east')$app_desc[, j])))

# constrained optimal production
ex_nut <- bind_cols(phos = ad_mod$phos[, 1],
                    nit = ad_mod$nit[, 1],
                    bio = ad_mod$bio[, 1]) %>% 
  sapply(., function(j) mean(j))

w_ex_opt <- apply(w_opt, 2,
                  function(j)
                    sapply(ex_nut,
                           function(i)
                             j*i)) %>% 
  t()

e_ex_opt <- apply(e_opt, 2,
                  function(j)
                    sapply(ex_nut,
                           function(i)
                             j*i)) %>% 
  t()


# tables and plots --------------------------------------------------------

# feedstock summary stats
fs_ss <- ad_in %>%
  select(!contains('manure')) %>%
  sapply(.,
         function(i)
           c(mean(i), min(i), max(i), sd(i)))
rownames(fs_ss) <- c('mean', 'min', 'max', 'sd')
xtable(fs_ss)

# phosphorus and nitrogen summary stats
nutrbio_ss <- bind_cols(ad_mod$phos[, 1], ad_mod$nit[, 1], ad_mod$bio[, 1]) %>%
  sapply(.,
         function(i)
           c(mean(i), min(i), max(i), sd(i))) %>%
  round(digits = 2)
rownames(nutrbio_ss) <- c('mean', 'min', 'max', 'sd')
colnames(nutrbio_ss) <- c('Phosphorus (lbs)', 'Nitrogen (lbs)', 'Biogas (MWh)')
xtable(nutrbio_ss)

# phosphorus lb/acre plot
# ggplot(ad_mod_nutr, aes(day, phos)) +
#   theme_classic() +
#   geom_line()+
#   geom_line(aes(y = phos25), color = 'gray', alpha = 0.75) +
#   geom_line(aes(y = phos75), color = 'gray', alpha = 0.75) +
#   labs(y = 'Phosphorus', x = 'Day') +
#   theme(axis.title = element_text(family = 'serif'))

# phosphorus cumulative production
# ggplot(ad_mod_pacre, aes(day, pacre)) +
#   theme_classic() +
#   geom_line()+
#   geom_line(aes(y = pacre25), color = 'gray', alpha = 0.75) +
#   geom_line(aes(y = pacre75), color = 'gray', alpha = 0.75) +
#   labs(y = 'Cumulative Phosphorus/acre', x = 'Day') +
#   theme(axis.title = element_text(family = 'serif'))

# biogas plot
# ggplot(ad_mod_bio, aes(day, bio)) +
#   theme_classic() +
#   geom_line()+
#   geom_line(aes(y = bio25), color = 'gray', alpha = 0.75) +
#   geom_line(aes(y = bio75), color = 'gray', alpha = 0.75) +
#   labs(y = 'Biogas', x = 'Day') +
#   theme(axis.title = element_text(family = 'serif'))

# # revenue generation table and plot
rev_ss <- ad_mod_rev %>%
  summarise(mean = mean(t_rev), min = min(t_rev), max = max(t_rev), sd = sd(t_rev)) %>%
  round(digits = 2) %>%
  data.frame()
rownames(rev_ss) <- 'Revenue'

# ggplot(ad_mod_rev, aes(day, rev)) +
#   theme_classic() +
#   geom_line()+
#   labs(y = 'Revenue', x = 'Day') +
#   theme(axis.title = element_text(family = 'serif'))

# nutrient constrained percentage tables
df_opt <- rbind(w_opt, e_opt) %>%
  t() %>% 
  as_tibble(.name_repair = ~ c('w1', 'w2', 'e1', 'e2'))
xtable(df_opt)

# expected contstrained production
ex_opt <- rbind(w_ex_opt, e_ex_opt) %>% 
  as_tibble(.name_repair = ~ c('pml', 'pl', 'nml', 'nl', 'bml', 'bl'))
xtable(ex_opt)

