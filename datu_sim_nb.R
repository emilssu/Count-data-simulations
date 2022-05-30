library(tidyverse)
library(MASS)
library(xtable)
library(melt)
library(ggpubr)


# seed, mainigie, tuksas tabulas ---- 


coverage <- function(mod, lev){
  # mod <- mp_1
  #  lev <-  0.95
  
  ci_cov <- NULL
  ci_len <- NULL
  
  if(isS4(mod) == TRUE){ 
    conf <- melt::confint(mod, level = lev)
    
    ci_cov[1] <- ifelse(b0 > conf[1,1] & b0 < conf[1,2], 1, 0)
    ci_cov[2] <- ifelse(b1 > conf[2,1] & b1 < conf[2,2], 1, 0)
    ci_cov[3] <- ifelse(b2 > conf[3,1] & b2 < conf[3,2], 1, 0)  
    
    ci_len[1] <- conf[1,2] - conf[1,1]
    ci_len[2] <- conf[2,2] - conf[2,1]
    ci_len[3] <- conf[3,2] - conf[3,1]
    
  }else{
    conf <- confint.default(mod, level = lev)
    
    ci_cov[1] <- ifelse(b0 > conf[1,1] & b0 < conf[1,2], 1, 0)
    ci_cov[2] <- ifelse(b1 > conf[2,1] & b1 < conf[2,2], 1, 0)
    ci_cov[3] <- ifelse(b2 > conf[3,1] & b2 < conf[3,2], 1, 0)  
    
    ci_len[1] <- conf[1,2] - conf[1,1]
    ci_len[2] <- conf[2,2] - conf[2,1]
    ci_len[3] <- conf[3,2] - conf[3,1]
  }
  return(list(coverage = ci_cov,
              ci_length = ci_len,
              conf_int = c(conf[1,1], conf[1,2],conf[2,1],conf[2,2],conf[3,1],conf[3,2])))
  
}

# coverage(mel_5,lev = 0.95)

set.seed(3759)
reps <- 1000

n1 <- 20
n2 <- 50
n3 <- 100
n4 <- 200
n5 <- 500
n6 <- 1000

b0 <- 0.2
b1 <- 0.5
b2 <- 0.7

theta_1 <- 0.5
theta_2 <- 2

# tables ----



x1_1 <- runif(n1, -1, 1)
x2_1 <- round(runif(n1,0,1))

x1_2 <- runif(n2, -1, 1)
x2_2 <- round(runif(n2,0,1))

x1_3 <- runif(n3, -1, 1)
x2_3 <- round(runif(n3,0,1))

x1_4 <- runif(n4, -1, 1)
x2_4 <- round(runif(n4,0,1))

x1_5 <- runif(n5, -1, 1)
x2_5 <- round(runif(n5,0,1))

x1_6 <- runif(n6, -1, 1)
x2_6 <- round(runif(n6,0,1))

# est_pois_1_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_1_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_1_1 <- matrix(nrow = reps, ncol = 3)
# est_el_1_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_2_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_2_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_2_1 <- matrix(nrow = reps, ncol = 3)
# est_el_2_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_3_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_3_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_3_1 <- matrix(nrow = reps, ncol = 3)
# est_el_3_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_4_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_4_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_4_1 <- matrix(nrow = reps, ncol = 3)
# est_el_4_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_5_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_5_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_5_1 <- matrix(nrow = reps, ncol = 3)
# est_el_5_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_6_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_6_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_6_1 <- matrix(nrow = reps, ncol = 3)
# est_el_6_1 <- matrix(nrow = reps, ncol = 3)
# 
# 
# est_pois_1_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_1_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_1_2 <- matrix(nrow = reps, ncol = 3)
# est_el_1_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_2_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_2_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_2_2 <- matrix(nrow = reps, ncol = 3)
# est_el_2_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_3_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_3_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_3_2 <- matrix(nrow = reps, ncol = 3)
# est_el_3_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_4_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_4_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_4_2 <- matrix(nrow = reps, ncol = 3)
# est_el_4_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_5_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_5_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_5_2 <- matrix(nrow = reps, ncol = 3)
# est_el_5_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_6_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_6_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_6_2 <- matrix(nrow = reps, ncol = 3)
# est_el_6_2 <- matrix(nrow = reps, ncol = 3)
# 
# 
# 
# cov_95_pois_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_1_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_2_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_3_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_4_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_5_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_6_1 <- matrix(nrow = reps, ncol = 6)
# 
# 
# 
# cov_99_pois_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_1_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_1_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_2_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_2_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_3_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_3_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_4_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_4_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_5_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_5_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_6_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_6_1 <- matrix(nrow = reps, ncol = 6)
# 
# 
# 
# cov_95_pois_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_1_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_2_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_3_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_4_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_5_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_6_2 <- matrix(nrow = reps, ncol = 6)
# 
# 
# 
# cov_99_pois_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_1_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_1_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_2_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_2_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_3_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_3_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_4_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_4_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_5_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_5_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_6_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_6_2 <- matrix(nrow = reps, ncol = 6)




conf_in_pois_1_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_95_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_95_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_95_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_95_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_95_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_95_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_95_1 <- matrix(nrow = reps, ncol = 6)


conf_in_pois_1_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_99_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_99_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_99_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_99_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_99_1 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_99_1 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_99_1 <- matrix(nrow = reps, ncol = 6)



conf_in_pois_1_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_95_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_95_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_95_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_95_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_95_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_95_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_95_2 <- matrix(nrow = reps, ncol = 6)


conf_in_pois_1_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_99_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_99_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_99_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_99_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_99_2 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_99_2 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_99_2 <- matrix(nrow = reps, ncol = 6)
# data sim ----




for(i in 1 : reps){

  # i <- 1
  Y_1_1 <- rnbinom(n1,size = theta_1, mu = exp(b0 + x1_1*b1 + x2_1*b2))
  Y_2_1 <- rnbinom(n2,size = theta_1, mu = exp(b0 + x1_2*b1 + x2_2*b2))
  Y_3_1 <- rnbinom(n3,size = theta_1, mu = exp(b0 + x1_3*b1 + x2_3*b2))
  Y_4_1 <- rnbinom(n4,size = theta_1, mu = exp(b0 + x1_4*b1 + x2_4*b2))
  Y_5_1 <- rnbinom(n5,size = theta_1, mu = exp(b0 + x1_5*b1 + x2_5*b2))
  Y_6_1 <- rnbinom(n6,size = theta_1, mu = exp(b0 + x1_6*b1 + x2_6*b2))
  
  
  Y_1_2 <- rnbinom(n1,size = theta_2, mu = exp(b0 + x1_1*b1 + x2_1*b2))
  Y_2_2 <- rnbinom(n2,size = theta_2, mu = exp(b0 + x1_2*b1 + x2_2*b2))
  Y_3_2 <- rnbinom(n3,size = theta_2, mu = exp(b0 + x1_3*b1 + x2_3*b2))
  Y_4_2 <- rnbinom(n4,size = theta_2, mu = exp(b0 + x1_4*b1 + x2_4*b2))
  Y_5_2 <- rnbinom(n5,size = theta_2, mu = exp(b0 + x1_5*b1 + x2_5*b2))
  Y_6_2 <- rnbinom(n6,size = theta_2, mu = exp(b0 + x1_6*b1 + x2_6*b2))

  mp_1_1_ <- glm(Y_1_1~x1_1+x2_1, family = "poisson")
  mqp_1_1_ <- glm(Y_1_1~x1_1+x2_1, family = "quasipoisson")
  mnb_1_1_ <- glm.nb(Y_1_1~x1_1+x2_1)
  mel_1_1_ <- el_glm(Y_1_1~x1_1+x2_1, family = poisson)

  mp_2_1_ <- glm(Y_2_1~x1_2+x2_2, family = "poisson")
  mqp_2_1_ <- glm(Y_2_1~x1_2+x2_2, family = "quasipoisson")
  mnb_2_1_ <- glm.nb(Y_2_1~x1_2+x2_2)
  mel_2_1_ <- el_glm(Y_2_1~x1_2+x2_2, family = poisson)

  mp_3_1_ <- glm(Y_3_1~x1_3+x2_3, family = "poisson")
  mqp_3_1_ <- glm(Y_3_1~x1_3+x2_3, family = "quasipoisson")
  mnb_3_1_ <- glm.nb(Y_3_1~x1_3+x2_3)
  mel_3_1_ <- el_glm(Y_3_1~x1_3+x2_3, family = poisson)

  mp_4_1_ <- glm(Y_4_1~x1_4+x2_4, family = "poisson")
  mqp_4_1_ <- glm(Y_4_1~x1_4+x2_4, family = "quasipoisson")
  mnb_4_1_ <- glm.nb(Y_4_1~x1_4+x2_4)
  mel_4_1_ <- el_glm(Y_4_1~x1_4+x2_4, family = poisson)

  mp_5_1_ <- glm(Y_5_1~x1_5+x2_5, family = "poisson")
  mqp_5_1_ <- glm(Y_5_1~x1_5+x2_5, family = "quasipoisson")
  mnb_5_1_ <- glm.nb(Y_5_1~x1_5+x2_5)
  mel_5_1_ <- el_glm(Y_5_1~x1_5+x2_5, family = poisson)

  mp_6_1_ <- glm(Y_6_1~x1_6+x2_6, family = "poisson")
  mqp_6_1_ <- glm(Y_6_1~x1_6+x2_6, family = "quasipoisson")
  mnb_6_1_ <- glm.nb(Y_6_1~x1_6+x2_6)
  mel_6_1_ <- el_glm(Y_6_1~x1_6+x2_6, family = poisson)
  
  
  mp_1_2_ <- glm(Y_1_2~x1_1+x2_1, family = "poisson")
  mqp_1_2_ <- glm(Y_1_2~x1_1+x2_1, family = "quasipoisson")
  mnb_1_2_ <- glm.nb(Y_1_2~x1_1+x2_1)
  mel_1_2_ <- el_glm(Y_1_2~x1_1+x2_1, family = poisson)
  
  mp_2_2_ <- glm(Y_2_2~x1_2+x2_2, family = "poisson")
  mqp_2_2_ <- glm(Y_2_2~x1_2+x2_2, family = "quasipoisson")
  mnb_2_2_ <- glm.nb(Y_2_2~x1_2+x2_2)
  mel_2_2_ <- el_glm(Y_2_2~x1_2+x2_2, family = poisson)
  
  mp_3_2_ <- glm(Y_3_2~x1_3+x2_3, family = "poisson")
  mqp_3_2_ <- glm(Y_3_2~x1_3+x2_3, family = "quasipoisson")
  mnb_3_2_ <- glm.nb(Y_3_2~x1_3+x2_3)
  mel_3_2_ <- el_glm(Y_3_2~x1_3+x2_3, family = poisson)
  
  mp_4_2_ <- glm(Y_4_2~x1_4+x2_4, family = "poisson")
  mqp_4_2_ <- glm(Y_4_2~x1_4+x2_4, family = "quasipoisson")
  mnb_4_2_ <- glm.nb(Y_4_2~x1_4+x2_4)
  mel_4_2_ <- el_glm(Y_4_2~x1_4+x2_4, family = poisson)
  
  mp_5_2_ <- glm(Y_5_2~x1_5+x2_5, family = "poisson")
  mqp_5_2_ <- glm(Y_5_2~x1_5+x2_5, family = "quasipoisson")
  mnb_5_2_ <- glm.nb(Y_5_2~x1_5+x2_5)
  mel_5_2_ <- el_glm(Y_5_2~x1_5+x2_5, family = poisson)
  
  mp_6_2_ <- glm(Y_6_2~x1_6+x2_6, family = "poisson")
  mqp_6_2_ <- glm(Y_6_2~x1_6+x2_6, family = "quasipoisson")
  mnb_6_2_ <- glm.nb(Y_6_2~x1_6+x2_6)
  mel_6_2_ <- el_glm(Y_6_2~x1_6+x2_6, family = poisson)
  
  

  # coverage----

  
  cov_p_1_95_1 <- coverage(mp_1_1_, 0.95)
  cov_qp_1_95_1 <- coverage(mqp_1_1_, 0.95)
  cov_nb_1_95_1 <- coverage(mnb_1_1_, 0.95)
  cov_el_1_95_1 <- coverage(mel_1_1_, 0.95)

  cov_p_2_95_1 <- coverage(mp_2_1_, 0.95)
  cov_qp_2_95_1 <- coverage(mqp_2_1_, 0.95)
  cov_nb_2_95_1 <- coverage(mnb_2_1_, 0.95)
  cov_el_2_95_1 <- coverage(mel_2_1_, 0.95)

  cov_p_3_95_1 <- coverage(mp_3_1_, 0.95)
  cov_qp_3_95_1 <- coverage(mqp_3_1_, 0.95)
  cov_nb_3_95_1 <- coverage(mnb_3_1_, 0.95)
  cov_el_3_95_1 <- coverage(mel_3_1_, 0.95)

  cov_p_4_95_1 <- coverage(mp_4_1_, 0.95)
  cov_qp_4_95_1 <- coverage(mqp_4_1_, 0.95)
  cov_nb_4_95_1 <- coverage(mnb_4_1_, 0.95)
  cov_el_4_95_1 <- coverage(mel_4_1_, 0.95)

  cov_p_5_95_1 <- coverage(mp_5_1_, 0.95)
  cov_qp_5_95_1 <- coverage(mqp_5_1_, 0.95)
  cov_nb_5_95_1 <- coverage(mnb_5_1_, 0.95)
  cov_el_5_95_1 <- coverage(mel_5_1_, 0.95)

  cov_p_6_95_1 <- coverage(mp_6_1_, 0.95)
  cov_qp_6_95_1 <- coverage(mqp_6_1_, 0.95)
  cov_nb_6_95_1 <- coverage(mnb_6_1_, 0.95)
  cov_el_6_95_1 <- coverage(mel_6_1_, 0.95)


  cov_p_1_99_1 <- coverage(mp_1_1_, 0.99)
  cov_qp_1_99_1 <- coverage(mqp_1_1_, 0.99)
  cov_nb_1_99_1 <- coverage(mnb_1_1_, 0.99)
  cov_el_1_99_1 <- coverage(mel_1_1_, 0.99)

  cov_p_2_99_1 <- coverage(mp_2_1_, 0.99)
  cov_qp_2_99_1 <- coverage(mqp_2_1_, 0.99)
  cov_nb_2_99_1 <- coverage(mnb_2_1_, 0.99)
  cov_el_2_99_1 <- coverage(mel_2_1_, 0.99)

  cov_p_3_99_1 <- coverage(mp_3_1_, 0.99)
  cov_qp_3_99_1 <- coverage(mqp_3_1_, 0.99)
  cov_nb_3_99_1 <- coverage(mnb_3_1_, 0.99)
  cov_el_3_99_1 <- coverage(mel_3_1_, 0.99)

  cov_p_4_99_1 <- coverage(mp_4_1_, 0.99)
  cov_qp_4_99_1 <- coverage(mqp_4_1_, 0.99)
  cov_nb_4_99_1 <- coverage(mnb_4_1_, 0.99)
  cov_el_4_99_1 <- coverage(mel_4_1_, 0.99)

  cov_p_5_99_1 <- coverage(mp_5_1_, 0.99)
  cov_qp_5_99_1 <- coverage(mqp_5_1_, 0.99)
  cov_nb_5_99_1 <- coverage(mnb_5_1_, 0.99)
  cov_el_5_99_1 <- coverage(mel_5_1_, 0.99)

  cov_p_6_99_1 <- coverage(mp_6_1_, 0.99)
  cov_qp_6_99_1 <- coverage(mqp_6_1_, 0.99)
  cov_nb_6_99_1 <- coverage(mnb_6_1_, 0.99)
  cov_el_6_99_1 <- coverage(mel_6_1_, 0.99)




  cov_p_1_95_2 <- coverage(mp_1_2_, 0.95)
  cov_qp_1_95_2 <- coverage(mqp_1_2_, 0.95)
  cov_nb_1_95_2 <- coverage(mnb_1_2_, 0.95)
  cov_el_1_95_2 <- coverage(mel_1_2_, 0.95)

  cov_p_2_95_2 <- coverage(mp_2_2_, 0.95)
  cov_qp_2_95_2 <- coverage(mqp_2_2_, 0.95)
  cov_nb_2_95_2 <- coverage(mnb_2_2_, 0.95)
  cov_el_2_95_2 <- coverage(mel_2_2_, 0.95)

  cov_p_3_95_2 <- coverage(mp_3_2_, 0.95)
  cov_qp_3_95_2 <- coverage(mqp_3_2_, 0.95)
  cov_nb_3_95_2 <- coverage(mnb_3_2_, 0.95)
  cov_el_3_95_2 <- coverage(mel_3_2_, 0.95)

  cov_p_4_95_2 <- coverage(mp_4_2_, 0.95)
  cov_qp_4_95_2 <- coverage(mqp_4_2_, 0.95)
  cov_nb_4_95_2 <- coverage(mnb_4_2_, 0.95)
  cov_el_4_95_2 <- coverage(mel_4_2_, 0.95)

  cov_p_5_95_2 <- coverage(mp_5_2_, 0.95)
  cov_qp_5_95_2 <- coverage(mqp_5_2_, 0.95)
  cov_nb_5_95_2 <- coverage(mnb_5_2_, 0.95)
  cov_el_5_95_2 <- coverage(mel_5_2_, 0.95)

  cov_p_6_95_2 <- coverage(mp_6_2_, 0.95)
  cov_qp_6_95_2 <- coverage(mqp_6_2_, 0.95)
  cov_nb_6_95_2 <- coverage(mnb_6_2_, 0.95)
  cov_el_6_95_2 <- coverage(mel_6_2_, 0.95)




  cov_p_1_99_2 <- coverage(mp_1_2_, 0.99)
  cov_qp_1_99_2 <- coverage(mqp_1_2_, 0.99)
  cov_nb_1_99_2 <- coverage(mnb_1_2_, 0.99)
  cov_el_1_99_2 <- coverage(mel_1_2_, 0.99)

  cov_p_2_99_2 <- coverage(mp_2_2_, 0.99)
  cov_qp_2_99_2 <- coverage(mqp_2_2_, 0.99)
  cov_nb_2_99_2 <- coverage(mnb_2_2_, 0.99)
  cov_el_2_99_2 <- coverage(mel_2_2_, 0.99)

  cov_p_3_99_2 <- coverage(mp_3_2_, 0.99)
  cov_qp_3_99_2 <- coverage(mqp_3_2_, 0.99)
  cov_nb_3_99_2 <- coverage(mnb_3_2_, 0.99)
  cov_el_3_99_2 <- coverage(mel_3_2_, 0.99)

  cov_p_4_99_2 <- coverage(mp_4_2_, 0.99)
  cov_qp_4_99_2 <- coverage(mqp_4_2_, 0.99)
  cov_nb_4_99_2 <- coverage(mnb_4_2_, 0.99)
  cov_el_4_99_2 <- coverage(mel_4_2_, 0.99)

  cov_p_5_99_2 <- coverage(mp_5_2_, 0.99)
  cov_qp_5_99_2 <- coverage(mqp_5_2_, 0.99)
  cov_nb_5_99_2 <- coverage(mnb_5_2_, 0.99)
  cov_el_5_99_2 <- coverage(mel_5_2_, 0.99)

  cov_p_6_99_2 <- coverage(mp_6_2_, 0.99)
  cov_qp_6_99_2 <- coverage(mqp_6_2_, 0.99)
  cov_nb_6_99_2 <- coverage(mnb_6_2_, 0.99)
  cov_el_6_99_2 <- coverage(mel_6_2_, 0.99)
  
  
  
  conf_in_pois_1_95_1[i,] <-  cov_p_1_95_1$conf_int
  conf_in_qp_1_95_1[i,] <-  cov_qp_1_95_1$conf_int
  conf_in_nb_1_95_1[i,] <-  cov_nb_1_95_1$conf_int
  conf_in_el_1_95_1[i,] <-  cov_el_1_95_1$conf_int
  
  conf_in_pois_2_95_1[i,] <-  cov_p_2_95_1$conf_int
  conf_in_qp_2_95_1[i,] <-  cov_qp_2_95_1$conf_int
  conf_in_nb_2_95_1[i,] <-  cov_nb_2_95_1$conf_int
  conf_in_el_2_95_1[i,] <-  cov_el_2_95_1$conf_int
  
  conf_in_pois_3_95_1[i,] <-  cov_p_3_95_1$conf_int
  conf_in_qp_3_95_1[i,] <-  cov_qp_3_95_1$conf_int
  conf_in_nb_3_95_1[i,] <-  cov_nb_3_95_1$conf_int
  conf_in_el_3_95_1[i,] <-  cov_el_3_95_1$conf_int
  
  conf_in_pois_4_95_1[i,] <-  cov_p_4_95_1$conf_int
  conf_in_qp_4_95_1[i,] <-  cov_qp_4_95_1$conf_int
  conf_in_nb_4_95_1[i,] <-  cov_nb_4_95_1$conf_int
  conf_in_el_4_95_1[i,] <-  cov_el_4_95_1$conf_int
  
  conf_in_pois_5_95_1[i,] <-  cov_p_5_95_1$conf_int
  conf_in_qp_5_95_1[i,] <-  cov_qp_5_95_1$conf_int
  conf_in_nb_5_95_1[i,] <-  cov_nb_5_95_1$conf_int
  conf_in_el_5_95_1[i,] <-  cov_el_5_95_1$conf_int
  
  conf_in_pois_6_95_1[i,] <-  cov_p_6_95_1$conf_int
  conf_in_qp_6_95_1[i,] <-  cov_qp_6_95_1$conf_int
  conf_in_nb_6_95_1[i,] <-  cov_nb_6_95_1$conf_int
  conf_in_el_6_95_1[i,] <-  cov_el_6_95_1$conf_int
  
  
  conf_in_pois_1_99_1[i,] <-  cov_p_1_99_1$conf_int
  conf_in_qp_1_99_1[i,] <-  cov_qp_1_99_1$conf_int
  conf_in_nb_1_99_1[i,] <-  cov_nb_1_99_1$conf_int
  conf_in_el_1_99_1[i,] <-  cov_el_1_99_1$conf_int
  
  conf_in_pois_2_99_1[i,] <-  cov_p_2_99_1$conf_int
  conf_in_qp_2_99_1[i,] <-  cov_qp_2_99_1$conf_int
  conf_in_nb_2_99_1[i,] <-  cov_nb_2_99_1$conf_int
  conf_in_el_2_99_1[i,] <-  cov_el_2_99_1$conf_int
  
  conf_in_pois_3_99_1[i,] <-  cov_p_3_99_1$conf_int
  conf_in_qp_3_99_1[i,] <-  cov_qp_3_99_1$conf_int
  conf_in_nb_3_99_1[i,] <-  cov_nb_3_99_1$conf_int
  conf_in_el_3_99_1[i,] <-  cov_el_3_99_1$conf_int
  
  conf_in_pois_4_99_1[i,] <-  cov_p_4_99_1$conf_int
  conf_in_qp_4_99_1[i,] <-  cov_qp_4_99_1$conf_int
  conf_in_nb_4_99_1[i,] <-  cov_nb_4_99_1$conf_int
  conf_in_el_4_99_1[i,] <-  cov_el_4_99_1$conf_int
  
  conf_in_pois_5_99_1[i,] <-  cov_p_5_99_1$conf_int
  conf_in_qp_5_99_1[i,] <-  cov_qp_5_99_1$conf_int
  conf_in_nb_5_99_1[i,] <-  cov_nb_5_99_1$conf_int
  conf_in_el_5_99_1[i,] <-  cov_el_5_99_1$conf_int
  
  conf_in_pois_6_99_1[i,] <-  cov_p_6_99_1$conf_int
  conf_in_qp_6_99_1[i,] <-  cov_qp_6_99_1$conf_int
  conf_in_nb_6_99_1[i,] <-  cov_nb_6_99_1$conf_int
  conf_in_el_6_99_1[i,] <-  cov_el_6_99_1$conf_int
  

  
  conf_in_pois_1_95_2[i,] <-  cov_p_1_95_2$conf_int
  conf_in_qp_1_95_2[i,] <-  cov_qp_1_95_2$conf_int
  conf_in_nb_1_95_2[i,] <-  cov_nb_1_95_2$conf_int
  conf_in_el_1_95_2[i,] <-  cov_el_1_95_2$conf_int
  
  conf_in_pois_2_95_2[i,] <-  cov_p_2_95_2$conf_int
  conf_in_qp_2_95_2[i,] <-  cov_qp_2_95_2$conf_int
  conf_in_nb_2_95_2[i,] <-  cov_nb_2_95_2$conf_int
  conf_in_el_2_95_2[i,] <-  cov_el_2_95_2$conf_int
  
  conf_in_pois_3_95_2[i,] <-  cov_p_3_95_2$conf_int
  conf_in_qp_3_95_2[i,] <-  cov_qp_3_95_2$conf_int
  conf_in_nb_3_95_2[i,] <-  cov_nb_3_95_2$conf_int
  conf_in_el_3_95_2[i,] <-  cov_el_3_95_2$conf_int
  
  conf_in_pois_4_95_2[i,] <-  cov_p_4_95_2$conf_int
  conf_in_qp_4_95_2[i,] <-  cov_qp_4_95_2$conf_int
  conf_in_nb_4_95_2[i,] <-  cov_nb_4_95_2$conf_int
  conf_in_el_4_95_2[i,] <-  cov_el_4_95_2$conf_int
  
  conf_in_pois_5_95_2[i,] <-  cov_p_5_95_2$conf_int
  conf_in_qp_5_95_2[i,] <-  cov_qp_5_95_2$conf_int
  conf_in_nb_5_95_2[i,] <-  cov_nb_5_95_2$conf_int
  conf_in_el_5_95_2[i,] <-  cov_el_5_95_2$conf_int
  
  conf_in_pois_6_95_2[i,] <-  cov_p_6_95_2$conf_int
  conf_in_qp_6_95_2[i,] <-  cov_qp_6_95_2$conf_int
  conf_in_nb_6_95_2[i,] <-  cov_nb_6_95_2$conf_int
  conf_in_el_6_95_2[i,] <-  cov_el_6_95_2$conf_int
  
  
  conf_in_pois_1_99_2[i,] <-  cov_p_1_99_2$conf_int
  conf_in_qp_1_99_2[i,] <-  cov_qp_1_99_2$conf_int
  conf_in_nb_1_99_2[i,] <-  cov_nb_1_99_2$conf_int
  conf_in_el_1_99_2[i,] <-  cov_el_1_99_2$conf_int
  
  conf_in_pois_2_99_2[i,] <-  cov_p_2_99_2$conf_int
  conf_in_qp_2_99_2[i,] <-  cov_qp_2_99_2$conf_int
  conf_in_nb_2_99_2[i,] <-  cov_nb_2_99_2$conf_int
  conf_in_el_2_99_2[i,] <-  cov_el_2_99_2$conf_int
  
  conf_in_pois_3_99_2[i,] <-  cov_p_3_99_2$conf_int
  conf_in_qp_3_99_2[i,] <-  cov_qp_3_99_2$conf_int
  conf_in_nb_3_99_2[i,] <-  cov_nb_3_99_2$conf_int
  conf_in_el_3_99_2[i,] <-  cov_el_3_99_2$conf_int
  
  conf_in_pois_4_99_2[i,] <-  cov_p_4_99_2$conf_int
  conf_in_qp_4_99_2[i,] <-  cov_qp_4_99_2$conf_int
  conf_in_nb_4_99_2[i,] <-  cov_nb_4_99_2$conf_int
  conf_in_el_4_99_2[i,] <-  cov_el_4_99_2$conf_int
  
  conf_in_pois_5_99_2[i,] <-  cov_p_5_99_2$conf_int
  conf_in_qp_5_99_2[i,] <-  cov_qp_5_99_2$conf_int
  conf_in_nb_5_99_2[i,] <-  cov_nb_5_99_2$conf_int
  conf_in_el_5_99_2[i,] <-  cov_el_5_99_2$conf_int
  
  conf_in_pois_6_99_2[i,] <-  cov_p_6_99_2$conf_int
  conf_in_qp_6_99_2[i,] <-  cov_qp_6_99_2$conf_int
  conf_in_nb_6_99_2[i,] <-  cov_nb_6_99_2$conf_int
  conf_in_el_6_99_2[i,] <-  cov_el_6_99_2$conf_int
  
  # 
  # # coeficients ---- 
  # 
  # # n1
  # 
  # est_pois_1_1[i,1] <- coefficients(mp_1_1_)[1]
  # est_pois_1_1[i,2] <- coefficients(mp_1_1_)[2]
  # est_pois_1_1[i,3] <- coefficients(mp_1_1_)[3]
  # 
  # est_qp_1_1[i,1] <- coefficients(mqp_1_1_)[1]
  # est_qp_1_1[i,2] <- coefficients(mqp_1_1_)[2]
  # est_qp_1_1[i,3] <- coefficients(mqp_1_1_)[3]
  # 
  # est_nb_1_1[i,1] <- coefficients(mnb_1_1_)[1]
  # est_nb_1_1[i,2] <- coefficients(mnb_1_1_)[2]
  # est_nb_1_1[i,3] <- coefficients(mnb_1_1_)[3]
  # 
  # est_el_1_1[i,1] <- mel_1_1_@coefficients[1]
  # est_el_1_1[i,2] <- mel_1_1_@coefficients[2]
  # est_el_1_1[i,3] <- mel_1_1_@coefficients[3]
  # 
  # 
  # est_pois_1_2[i,1] <- coefficients(mp_1_2_)[1]
  # est_pois_1_2[i,2] <- coefficients(mp_1_2_)[2]
  # est_pois_1_2[i,3] <- coefficients(mp_1_2_)[3]
  # 
  # est_qp_1_2[i,1] <- coefficients(mqp_1_2_)[1]
  # est_qp_1_2[i,2] <- coefficients(mqp_1_2_)[2]
  # est_qp_1_2[i,3] <- coefficients(mqp_1_2_)[3]
  # 
  # est_nb_1_2[i,1] <- coefficients(mnb_1_2_)[1]
  # est_nb_1_2[i,2] <- coefficients(mnb_1_2_)[2]
  # est_nb_1_2[i,3] <- coefficients(mnb_1_2_)[3]
  # 
  # est_el_1_2[i,1] <- mel_1_2_@coefficients[1]
  # est_el_1_2[i,2] <- mel_1_2_@coefficients[2]
  # est_el_1_2[i,3] <- mel_1_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_1_1[i,] <- c(cov_p_1_95_1$coverage, cov_p_1_95_1$ci_length)
  # cov_95_qp_1_1[i,] <- c(cov_qp_1_95_1$coverage, cov_qp_1_95_1$ci_length)
  # cov_95_nb_1_1[i,] <- c(cov_nb_1_95_1$coverage, cov_nb_1_95_1$ci_length)
  # cov_95_el_1_1[i,] <- c(cov_el_1_95_1$coverage, cov_el_1_95_1$ci_length)
  # 
  # cov_99_pois_1_1[i,] <- c(cov_p_1_99_1$coverage, cov_p_1_99_1$ci_length)
  # cov_99_qp_1_1[i,] <- c(cov_qp_1_99_1$coverage, cov_qp_1_99_1$ci_length)
  # cov_99_nb_1_1[i,] <- c(cov_nb_1_99_1$coverage, cov_nb_1_99_1$ci_length)
  # cov_99_el_1_1[i,] <- c(cov_el_1_99_1$coverage, cov_el_1_99_1$ci_length)
  # 
  # cov_95_pois_1_2[i,] <- c(cov_p_1_95_2$coverage, cov_p_1_95_2$ci_length)
  # cov_95_qp_1_2[i,] <- c(cov_qp_1_95_2$coverage, cov_qp_1_95_2$ci_length)
  # cov_95_nb_1_2[i,] <- c(cov_nb_1_95_2$coverage, cov_nb_1_95_2$ci_length)
  # cov_95_el_1_2[i,] <- c(cov_el_1_95_2$coverage, cov_el_1_95_2$ci_length)
  # 
  # cov_99_pois_1_2[i,] <- c(cov_p_1_99_2$coverage, cov_p_1_99_2$ci_length)
  # cov_99_qp_1_2[i,] <- c(cov_qp_1_99_2$coverage, cov_qp_1_99_2$ci_length)
  # cov_99_nb_1_2[i,] <- c(cov_nb_1_99_2$coverage, cov_nb_1_99_2$ci_length)
  # cov_99_el_1_2[i,] <- c(cov_el_1_99_2$coverage, cov_el_1_99_2$ci_length)
  # 
  # 
  # #n2
  # 
  # est_pois_2_1[i,1] <- coefficients(mp_2_1_)[1]
  # est_pois_2_1[i,2] <- coefficients(mp_2_1_)[2]
  # est_pois_2_1[i,3] <- coefficients(mp_2_1_)[3]
  # 
  # est_qp_2_1[i,1] <- coefficients(mqp_2_1_)[1]
  # est_qp_2_1[i,2] <- coefficients(mqp_2_1_)[2]
  # est_qp_2_1[i,3] <- coefficients(mqp_2_1_)[3]
  # 
  # est_nb_2_1[i,1] <- coefficients(mnb_2_1_)[1]
  # est_nb_2_1[i,2] <- coefficients(mnb_2_1_)[2]
  # est_nb_2_1[i,3] <- coefficients(mnb_2_1_)[3]
  # 
  # est_el_2_1[i,1] <- mel_2_1_@coefficients[1]
  # est_el_2_1[i,2] <- mel_2_1_@coefficients[2]
  # est_el_2_1[i,3] <- mel_2_1_@coefficients[3]
  # 
  # 
  # est_pois_2_2[i,1] <- coefficients(mp_2_2_)[1]
  # est_pois_2_2[i,2] <- coefficients(mp_2_2_)[2]
  # est_pois_2_2[i,3] <- coefficients(mp_2_2_)[3]
  # 
  # est_qp_2_2[i,1] <- coefficients(mqp_2_2_)[1]
  # est_qp_2_2[i,2] <- coefficients(mqp_2_2_)[2]
  # est_qp_2_2[i,3] <- coefficients(mqp_2_2_)[3]
  # 
  # est_nb_2_2[i,1] <- coefficients(mnb_2_2_)[1]
  # est_nb_2_2[i,2] <- coefficients(mnb_2_2_)[2]
  # est_nb_2_2[i,3] <- coefficients(mnb_2_2_)[3]
  # 
  # est_el_2_2[i,1] <- mel_2_2_@coefficients[1]
  # est_el_2_2[i,2] <- mel_2_2_@coefficients[2]
  # est_el_2_2[i,3] <- mel_2_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_2_1[i,] <- c(cov_p_2_95_1$coverage, cov_p_2_95_1$ci_length)
  # cov_95_qp_2_1[i,] <- c(cov_qp_2_95_1$coverage, cov_qp_2_95_1$ci_length)
  # cov_95_nb_2_1[i,] <- c(cov_nb_2_95_1$coverage, cov_nb_2_95_1$ci_length)
  # cov_95_el_2_1[i,] <- c(cov_el_2_95_1$coverage, cov_el_2_95_1$ci_length)
  # 
  # cov_99_pois_2_1[i,] <- c(cov_p_2_99_1$coverage, cov_p_2_99_1$ci_length)
  # cov_99_qp_2_1[i,] <- c(cov_qp_2_99_1$coverage, cov_qp_2_99_1$ci_length)
  # cov_99_nb_2_1[i,] <- c(cov_nb_2_99_1$coverage, cov_nb_2_99_1$ci_length)
  # cov_99_el_2_1[i,] <- c(cov_el_2_99_1$coverage, cov_el_2_99_1$ci_length)
  # 
  # cov_95_pois_2_2[i,] <- c(cov_p_2_95_2$coverage, cov_p_2_95_2$ci_length)
  # cov_95_qp_2_2[i,] <- c(cov_qp_2_95_2$coverage, cov_qp_2_95_2$ci_length)
  # cov_95_nb_2_2[i,] <- c(cov_nb_2_95_2$coverage, cov_nb_2_95_2$ci_length)
  # cov_95_el_2_2[i,] <- c(cov_el_2_95_2$coverage, cov_el_2_95_2$ci_length)
  # 
  # cov_99_pois_2_2[i,] <- c(cov_p_2_99_2$coverage, cov_p_2_99_2$ci_length)
  # cov_99_qp_2_2[i,] <- c(cov_qp_2_99_2$coverage, cov_qp_2_99_2$ci_length)
  # cov_99_nb_2_2[i,] <- c(cov_nb_2_99_2$coverage, cov_nb_2_99_2$ci_length)
  # cov_99_el_2_2[i,] <- c(cov_el_2_99_2$coverage, cov_el_2_99_2$ci_length)
  # 
  # #n3
  # 
  # est_pois_3_1[i,1] <- coefficients(mp_3_1_)[1]
  # est_pois_3_1[i,2] <- coefficients(mp_3_1_)[2]
  # est_pois_3_1[i,3] <- coefficients(mp_3_1_)[3]
  # 
  # est_qp_3_1[i,1] <- coefficients(mqp_3_1_)[1]
  # est_qp_3_1[i,2] <- coefficients(mqp_3_1_)[2]
  # est_qp_3_1[i,3] <- coefficients(mqp_3_1_)[3]
  # 
  # est_nb_3_1[i,1] <- coefficients(mnb_3_1_)[1]
  # est_nb_3_1[i,2] <- coefficients(mnb_3_1_)[2]
  # est_nb_3_1[i,3] <- coefficients(mnb_3_1_)[3]
  # 
  # est_el_3_1[i,1] <- mel_3_1_@coefficients[1]
  # est_el_3_1[i,2] <- mel_3_1_@coefficients[2]
  # est_el_3_1[i,3] <- mel_3_1_@coefficients[3]
  # 
  # 
  # est_pois_3_2[i,1] <- coefficients(mp_3_2_)[1]
  # est_pois_3_2[i,2] <- coefficients(mp_3_2_)[2]
  # est_pois_3_2[i,3] <- coefficients(mp_3_2_)[3]
  # 
  # est_qp_3_2[i,1] <- coefficients(mqp_3_2_)[1]
  # est_qp_3_2[i,2] <- coefficients(mqp_3_2_)[2]
  # est_qp_3_2[i,3] <- coefficients(mqp_3_2_)[3]
  # 
  # est_nb_3_2[i,1] <- coefficients(mnb_3_2_)[1]
  # est_nb_3_2[i,2] <- coefficients(mnb_3_2_)[2]
  # est_nb_3_2[i,3] <- coefficients(mnb_3_2_)[3]
  # 
  # est_el_3_2[i,1] <- mel_3_2_@coefficients[1]
  # est_el_3_2[i,2] <- mel_3_2_@coefficients[2]
  # est_el_3_2[i,3] <- mel_3_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_3_1[i,] <- c(cov_p_3_95_1$coverage, cov_p_3_95_1$ci_length)
  # cov_95_qp_3_1[i,] <- c(cov_qp_3_95_1$coverage, cov_qp_3_95_1$ci_length)
  # cov_95_nb_3_1[i,] <- c(cov_nb_3_95_1$coverage, cov_nb_3_95_1$ci_length)
  # cov_95_el_3_1[i,] <- c(cov_el_3_95_1$coverage, cov_el_3_95_1$ci_length)
  # 
  # cov_99_pois_3_1[i,] <- c(cov_p_3_99_1$coverage, cov_p_3_99_1$ci_length)
  # cov_99_qp_3_1[i,] <- c(cov_qp_3_99_1$coverage, cov_qp_3_99_1$ci_length)
  # cov_99_nb_3_1[i,] <- c(cov_nb_3_99_1$coverage, cov_nb_3_99_1$ci_length)
  # cov_99_el_3_1[i,] <- c(cov_el_3_99_1$coverage, cov_el_3_99_1$ci_length)
  # 
  # cov_95_pois_3_2[i,] <- c(cov_p_3_95_2$coverage, cov_p_3_95_2$ci_length)
  # cov_95_qp_3_2[i,] <- c(cov_qp_3_95_2$coverage, cov_qp_3_95_2$ci_length)
  # cov_95_nb_3_2[i,] <- c(cov_nb_3_95_2$coverage, cov_nb_3_95_2$ci_length)
  # cov_95_el_3_2[i,] <- c(cov_el_3_95_2$coverage, cov_el_3_95_2$ci_length)
  # 
  # cov_99_pois_3_2[i,] <- c(cov_p_3_99_2$coverage, cov_p_3_99_2$ci_length)
  # cov_99_qp_3_2[i,] <- c(cov_qp_3_99_2$coverage, cov_qp_3_99_2$ci_length)
  # cov_99_nb_3_2[i,] <- c(cov_nb_3_99_2$coverage, cov_nb_3_99_2$ci_length)
  # cov_99_el_3_2[i,] <- c(cov_el_3_99_2$coverage, cov_el_3_99_2$ci_length)
  # #n4
  # 
  # est_pois_4_1[i,1] <- coefficients(mp_4_1_)[1]
  # est_pois_4_1[i,2] <- coefficients(mp_4_1_)[2]
  # est_pois_4_1[i,3] <- coefficients(mp_4_1_)[3]
  # 
  # est_qp_4_1[i,1] <- coefficients(mqp_4_1_)[1]
  # est_qp_4_1[i,2] <- coefficients(mqp_4_1_)[2]
  # est_qp_4_1[i,3] <- coefficients(mqp_4_1_)[3]
  # 
  # est_nb_4_1[i,1] <- coefficients(mnb_4_1_)[1]
  # est_nb_4_1[i,2] <- coefficients(mnb_4_1_)[2]
  # est_nb_4_1[i,3] <- coefficients(mnb_4_1_)[3]
  # 
  # est_el_4_1[i,1] <- mel_4_1_@coefficients[1]
  # est_el_4_1[i,2] <- mel_4_1_@coefficients[2]
  # est_el_4_1[i,3] <- mel_4_1_@coefficients[3]
  # 
  # 
  # est_pois_4_2[i,1] <- coefficients(mp_4_2_)[1]
  # est_pois_4_2[i,2] <- coefficients(mp_4_2_)[2]
  # est_pois_4_2[i,3] <- coefficients(mp_4_2_)[3]
  # 
  # est_qp_4_2[i,1] <- coefficients(mqp_4_2_)[1]
  # est_qp_4_2[i,2] <- coefficients(mqp_4_2_)[2]
  # est_qp_4_2[i,3] <- coefficients(mqp_4_2_)[3]
  # 
  # est_nb_4_2[i,1] <- coefficients(mnb_4_2_)[1]
  # est_nb_4_2[i,2] <- coefficients(mnb_4_2_)[2]
  # est_nb_4_2[i,3] <- coefficients(mnb_4_2_)[3]
  # 
  # est_el_4_2[i,1] <- mel_4_2_@coefficients[1]
  # est_el_4_2[i,2] <- mel_4_2_@coefficients[2]
  # est_el_4_2[i,3] <- mel_4_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_4_1[i,] <- c(cov_p_4_95_1$coverage, cov_p_4_95_1$ci_length)
  # cov_95_qp_4_1[i,] <- c(cov_qp_4_95_1$coverage, cov_qp_4_95_1$ci_length)
  # cov_95_nb_4_1[i,] <- c(cov_nb_4_95_1$coverage, cov_nb_4_95_1$ci_length)
  # cov_95_el_4_1[i,] <- c(cov_el_4_95_1$coverage, cov_el_4_95_1$ci_length)
  # 
  # cov_99_pois_4_1[i,] <- c(cov_p_4_99_1$coverage, cov_p_4_99_1$ci_length)
  # cov_99_qp_4_1[i,] <- c(cov_qp_4_99_1$coverage, cov_qp_4_99_1$ci_length)
  # cov_99_nb_4_1[i,] <- c(cov_nb_4_99_1$coverage, cov_nb_4_99_1$ci_length)
  # cov_99_el_4_1[i,] <- c(cov_el_4_99_1$coverage, cov_el_4_99_1$ci_length)
  # 
  # cov_95_pois_4_2[i,] <- c(cov_p_4_95_2$coverage, cov_p_4_95_2$ci_length)
  # cov_95_qp_4_2[i,] <- c(cov_qp_4_95_2$coverage, cov_qp_4_95_2$ci_length)
  # cov_95_nb_4_2[i,] <- c(cov_nb_4_95_2$coverage, cov_nb_4_95_2$ci_length)
  # cov_95_el_4_2[i,] <- c(cov_el_4_95_2$coverage, cov_el_4_95_2$ci_length)
  # 
  # cov_99_pois_4_2[i,] <- c(cov_p_4_99_2$coverage, cov_p_4_99_2$ci_length)
  # cov_99_qp_4_2[i,] <- c(cov_qp_4_99_2$coverage, cov_qp_4_99_2$ci_length)
  # cov_99_nb_4_2[i,] <- c(cov_nb_4_99_2$coverage, cov_nb_4_99_2$ci_length)
  # cov_99_el_4_2[i,] <- c(cov_el_4_99_2$coverage, cov_el_4_99_2$ci_length)
  # #n5
  # 
  # est_pois_5_1[i,1] <- coefficients(mp_5_1_)[1]
  # est_pois_5_1[i,2] <- coefficients(mp_5_1_)[2]
  # est_pois_5_1[i,3] <- coefficients(mp_5_1_)[3]
  # 
  # est_qp_5_1[i,1] <- coefficients(mqp_5_1_)[1]
  # est_qp_5_1[i,2] <- coefficients(mqp_5_1_)[2]
  # est_qp_5_1[i,3] <- coefficients(mqp_5_1_)[3]
  # 
  # est_nb_5_1[i,1] <- coefficients(mnb_5_1_)[1]
  # est_nb_5_1[i,2] <- coefficients(mnb_5_1_)[2]
  # est_nb_5_1[i,3] <- coefficients(mnb_5_1_)[3]
  # 
  # est_el_5_1[i,1] <- mel_5_1_@coefficients[1]
  # est_el_5_1[i,2] <- mel_5_1_@coefficients[2]
  # est_el_5_1[i,3] <- mel_5_1_@coefficients[3]
  # 
  # 
  # est_pois_5_2[i,1] <- coefficients(mp_5_2_)[1]
  # est_pois_5_2[i,2] <- coefficients(mp_5_2_)[2]
  # est_pois_5_2[i,3] <- coefficients(mp_5_2_)[3]
  # 
  # est_qp_5_2[i,1] <- coefficients(mqp_5_2_)[1]
  # est_qp_5_2[i,2] <- coefficients(mqp_5_2_)[2]
  # est_qp_5_2[i,3] <- coefficients(mqp_5_2_)[3]
  # 
  # est_nb_5_2[i,1] <- coefficients(mnb_5_2_)[1]
  # est_nb_5_2[i,2] <- coefficients(mnb_5_2_)[2]
  # est_nb_5_2[i,3] <- coefficients(mnb_5_2_)[3]
  # 
  # est_el_5_2[i,1] <- mel_5_2_@coefficients[1]
  # est_el_5_2[i,2] <- mel_5_2_@coefficients[2]
  # est_el_5_2[i,3] <- mel_5_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_5_1[i,] <- c(cov_p_5_95_1$coverage, cov_p_5_95_1$ci_length)
  # cov_95_qp_5_1[i,] <- c(cov_qp_5_95_1$coverage, cov_qp_5_95_1$ci_length)
  # cov_95_nb_5_1[i,] <- c(cov_nb_5_95_1$coverage, cov_nb_5_95_1$ci_length)
  # cov_95_el_5_1[i,] <- c(cov_el_5_95_1$coverage, cov_el_5_95_1$ci_length)
  # 
  # cov_99_pois_5_1[i,] <- c(cov_p_5_99_1$coverage, cov_p_5_99_1$ci_length)
  # cov_99_qp_5_1[i,] <- c(cov_qp_5_99_1$coverage, cov_qp_5_99_1$ci_length)
  # cov_99_nb_5_1[i,] <- c(cov_nb_5_99_1$coverage, cov_nb_5_99_1$ci_length)
  # cov_99_el_5_1[i,] <- c(cov_el_5_99_1$coverage, cov_el_5_99_1$ci_length)
  # 
  # cov_95_pois_5_2[i,] <- c(cov_p_5_95_2$coverage, cov_p_5_95_2$ci_length)
  # cov_95_qp_5_2[i,] <- c(cov_qp_5_95_2$coverage, cov_qp_5_95_2$ci_length)
  # cov_95_nb_5_2[i,] <- c(cov_nb_5_95_2$coverage, cov_nb_5_95_2$ci_length)
  # cov_95_el_5_2[i,] <- c(cov_el_5_95_2$coverage, cov_el_5_95_2$ci_length)
  # 
  # cov_99_pois_5_2[i,] <- c(cov_p_5_99_2$coverage, cov_p_5_99_2$ci_length)
  # cov_99_qp_5_2[i,] <- c(cov_qp_5_99_2$coverage, cov_qp_5_99_2$ci_length)
  # cov_99_nb_5_2[i,] <- c(cov_nb_5_99_2$coverage, cov_nb_5_99_2$ci_length)
  # cov_99_el_5_2[i,] <- c(cov_el_5_99_2$coverage, cov_el_5_99_2$ci_length)
  # # n6
  # 
  # est_pois_6_1[i,1] <- coefficients(mp_6_1_)[1]
  # est_pois_6_1[i,2] <- coefficients(mp_6_1_)[2]
  # est_pois_6_1[i,3] <- coefficients(mp_6_1_)[3]
  # 
  # est_qp_6_1[i,1] <- coefficients(mqp_6_1_)[1]
  # est_qp_6_1[i,2] <- coefficients(mqp_6_1_)[2]
  # est_qp_6_1[i,3] <- coefficients(mqp_6_1_)[3]
  # 
  # est_nb_6_1[i,1] <- coefficients(mnb_6_1_)[1]
  # est_nb_6_1[i,2] <- coefficients(mnb_6_1_)[2]
  # est_nb_6_1[i,3] <- coefficients(mnb_6_1_)[3]
  # 
  # est_el_6_1[i,1] <- mel_6_1_@coefficients[1]
  # est_el_6_1[i,2] <- mel_6_1_@coefficients[2]
  # est_el_6_1[i,3] <- mel_6_1_@coefficients[3]
  # 
  # 
  # est_pois_6_2[i,1] <- coefficients(mp_6_2_)[1]
  # est_pois_6_2[i,2] <- coefficients(mp_6_2_)[2]
  # est_pois_6_2[i,3] <- coefficients(mp_6_2_)[3]
  # 
  # est_qp_6_2[i,1] <- coefficients(mqp_6_2_)[1]
  # est_qp_6_2[i,2] <- coefficients(mqp_6_2_)[2]
  # est_qp_6_2[i,3] <- coefficients(mqp_6_2_)[3]
  # 
  # est_nb_6_2[i,1] <- coefficients(mnb_6_2_)[1]
  # est_nb_6_2[i,2] <- coefficients(mnb_6_2_)[2]
  # est_nb_6_2[i,3] <- coefficients(mnb_6_2_)[3]
  # 
  # est_el_6_2[i,1] <- mel_6_2_@coefficients[1]
  # est_el_6_2[i,2] <- mel_6_2_@coefficients[2]
  # est_el_6_2[i,3] <- mel_6_2_@coefficients[3]
  # 
  # 
  # 
  # cov_95_pois_6_1[i,] <- c(cov_p_6_95_1$coverage, cov_p_6_95_1$ci_length)
  # cov_95_qp_6_1[i,] <- c(cov_qp_6_95_1$coverage, cov_qp_6_95_1$ci_length)
  # cov_95_nb_6_1[i,] <- c(cov_nb_6_95_1$coverage, cov_nb_6_95_1$ci_length)
  # cov_95_el_6_1[i,] <- c(cov_el_6_95_1$coverage, cov_el_6_95_1$ci_length)
  # 
  # cov_99_pois_6_1[i,] <- c(cov_p_6_99_1$coverage, cov_p_6_99_1$ci_length)
  # cov_99_qp_6_1[i,] <- c(cov_qp_6_99_1$coverage, cov_qp_6_99_1$ci_length)
  # cov_99_nb_6_1[i,] <- c(cov_nb_6_99_1$coverage, cov_nb_6_99_1$ci_length)
  # cov_99_el_6_1[i,] <- c(cov_el_6_99_1$coverage, cov_el_6_99_1$ci_length)
  # 
  # cov_95_pois_6_2[i,] <- c(cov_p_6_95_2$coverage, cov_p_6_95_2$ci_length)
  # cov_95_qp_6_2[i,] <- c(cov_qp_6_95_2$coverage, cov_qp_6_95_2$ci_length)
  # cov_95_nb_6_2[i,] <- c(cov_nb_6_95_2$coverage, cov_nb_6_95_2$ci_length)
  # cov_95_el_6_2[i,] <- c(cov_el_6_95_2$coverage, cov_el_6_95_2$ci_length)
  # 
  # cov_99_pois_6_2[i,] <- c(cov_p_6_99_2$coverage, cov_p_6_99_2$ci_length)
  # cov_99_qp_6_2[i,] <- c(cov_qp_6_99_2$coverage, cov_qp_6_99_2$ci_length)
  # cov_99_nb_6_2[i,] <- c(cov_nb_6_99_2$coverage, cov_nb_6_99_2$ci_length)
  # cov_99_el_6_2[i,] <- c(cov_el_6_99_2$coverage, cov_el_6_99_2$ci_length)
  
  print(i)
}



# est tables ----

# est_pois_1_1 <- as.data.frame(est_pois_1_1)
# est_qp_1_1 <- as.data.frame(est_qp_1_1)
# est_nb_1_1 <- as.data.frame(est_nb_1_1)
# est_el_1_1 <- as.data.frame(est_el_1_1)
# 
# est_pois_2_1 <- as.data.frame(est_pois_2_1)
# est_qp_2_1 <- as.data.frame(est_qp_2_1)
# est_nb_2_1 <- as.data.frame(est_nb_2_1)
# est_el_2_1 <- as.data.frame(est_el_2_1)
# 
# est_pois_3_1 <- as.data.frame(est_pois_3_1)
# est_qp_3_1 <- as.data.frame(est_qp_3_1)
# est_nb_3_1 <- as.data.frame(est_nb_3_1)
# est_el_3_1 <- as.data.frame(est_el_3_1)
# 
# est_pois_4_1 <- as.data.frame(est_pois_4_1)
# est_qp_4_1 <- as.data.frame(est_qp_4_1)
# est_nb_4_1 <- as.data.frame(est_nb_4_1)
# est_el_4_1 <- as.data.frame(est_el_4_1)
# 
# est_pois_5_1 <- as.data.frame(est_pois_5_1)
# est_qp_5_1 <- as.data.frame(est_qp_5_1)
# est_nb_5_1 <- as.data.frame(est_nb_5_1)
# est_el_5_1 <- as.data.frame(est_el_5_1)
# 
# est_pois_6_1 <- as.data.frame(est_pois_6_1)
# est_qp_6_1 <- as.data.frame(est_qp_6_1)
# est_nb_6_1 <- as.data.frame(est_nb_6_1)
# est_el_6_1 <- as.data.frame(est_el_6_1)
# 
# 
# 
# 
# est_pois_1_2 <- as.data.frame(est_pois_1_2)
# est_qp_1_2 <- as.data.frame(est_qp_1_2)
# est_nb_1_2 <- as.data.frame(est_nb_1_2)
# est_el_1_2 <- as.data.frame(est_el_1_2)
# 
# est_pois_2_2 <- as.data.frame(est_pois_2_2)
# est_qp_2_2 <- as.data.frame(est_qp_2_2)
# est_nb_2_2 <- as.data.frame(est_nb_2_2)
# est_el_2_2 <- as.data.frame(est_el_2_2)
# 
# est_pois_3_2 <- as.data.frame(est_pois_3_2)
# est_qp_3_2 <- as.data.frame(est_qp_3_2)
# est_nb_3_2 <- as.data.frame(est_nb_3_2)
# est_el_3_2 <- as.data.frame(est_el_3_2)
# 
# est_pois_4_2 <- as.data.frame(est_pois_4_2)
# est_qp_4_2 <- as.data.frame(est_qp_4_2)
# est_nb_4_2 <- as.data.frame(est_nb_4_2)
# est_el_4_2 <- as.data.frame(est_el_4_2)
# 
# est_pois_5_2 <- as.data.frame(est_pois_5_2)
# est_qp_5_2 <- as.data.frame(est_qp_5_2)
# est_nb_5_2 <- as.data.frame(est_nb_5_2)
# est_el_5_2 <- as.data.frame(est_el_5_2)
# 
# est_pois_6_2 <- as.data.frame(est_pois_6_2)
# est_qp_6_2 <- as.data.frame(est_qp_6_2)
# est_nb_6_2 <- as.data.frame(est_nb_6_2)
# est_el_6_2 <- as.data.frame(est_el_6_2)
# 
# 
# 
# cov_95_pois_1_1 <- as.data.frame(cov_95_pois_1_1)
# cov_95_qp_1_1 <- as.data.frame(cov_95_qp_1_1)
# cov_95_nb_1_1 <- as.data.frame(cov_95_nb_1_1)
# cov_95_el_1_1 <- as.data.frame(cov_95_el_1_1)
# 
# cov_95_pois_2_1 <- as.data.frame(cov_95_pois_2_1)
# cov_95_qp_2_1 <- as.data.frame(cov_95_qp_2_1)
# cov_95_nb_2_1 <- as.data.frame(cov_95_nb_2_1)
# cov_95_el_2_1 <- as.data.frame(cov_95_el_2_1)
# 
# cov_95_pois_3_1 <- as.data.frame(cov_95_pois_3_1)
# cov_95_qp_3_1 <- as.data.frame(cov_95_qp_3_1)
# cov_95_nb_3_1 <- as.data.frame(cov_95_nb_3_1)
# cov_95_el_3_1 <- as.data.frame(cov_95_el_3_1)
# 
# cov_95_pois_4_1 <- as.data.frame(cov_95_pois_4_1)
# cov_95_qp_4_1 <- as.data.frame(cov_95_qp_4_1)
# cov_95_nb_4_1 <- as.data.frame(cov_95_nb_4_1)
# cov_95_el_4_1 <- as.data.frame(cov_95_el_4_1)
# 
# cov_95_pois_5_1 <- as.data.frame(cov_95_pois_5_1)
# cov_95_qp_5_1 <- as.data.frame(cov_95_qp_5_1)
# cov_95_nb_5_1 <- as.data.frame(cov_95_nb_5_1)
# cov_95_el_5_1 <- as.data.frame(cov_95_el_5_1)
# 
# cov_95_pois_6_1 <- as.data.frame(cov_95_pois_6_1)
# cov_95_qp_6_1 <- as.data.frame(cov_95_qp_6_1)
# cov_95_nb_6_1 <- as.data.frame(cov_95_nb_6_1)
# cov_95_el_6_1 <- as.data.frame(cov_95_el_6_1)
# 
# 
# cov_99_pois_1_1 <- as.data.frame(cov_99_pois_1_1)
# cov_99_qp_1_1 <- as.data.frame(cov_99_qp_1_1)
# cov_99_nb_1_1 <- as.data.frame(cov_99_nb_1_1)
# cov_99_el_1_1 <- as.data.frame(cov_99_el_1_1)
# 
# cov_99_pois_2_1 <- as.data.frame(cov_99_pois_2_1)
# cov_99_qp_2_1 <- as.data.frame(cov_99_qp_2_1)
# cov_99_nb_2_1 <- as.data.frame(cov_99_nb_2_1)
# cov_99_el_2_1 <- as.data.frame(cov_99_el_2_1)
# 
# cov_99_pois_3_1 <- as.data.frame(cov_99_pois_3_1)
# cov_99_qp_3_1 <- as.data.frame(cov_99_qp_3_1)
# cov_99_nb_3_1 <- as.data.frame(cov_99_nb_3_1)
# cov_99_el_3_1 <- as.data.frame(cov_99_el_3_1)
# 
# cov_99_pois_4_1 <- as.data.frame(cov_99_pois_4_1)
# cov_99_qp_4_1 <- as.data.frame(cov_99_qp_4_1)
# cov_99_nb_4_1 <- as.data.frame(cov_99_nb_4_1)
# cov_99_el_4_1 <- as.data.frame(cov_99_el_4_1)
# 
# cov_99_pois_5_1 <- as.data.frame(cov_99_pois_5_1)
# cov_99_qp_5_1 <- as.data.frame(cov_99_qp_5_1)
# cov_99_nb_5_1 <- as.data.frame(cov_99_nb_5_1)
# cov_99_el_5_1 <- as.data.frame(cov_99_el_5_1)
# 
# cov_99_pois_6_1 <- as.data.frame(cov_99_pois_6_1)
# cov_99_qp_6_1 <- as.data.frame(cov_99_qp_6_1)
# cov_99_nb_6_1 <- as.data.frame(cov_99_nb_6_1)
# cov_99_el_6_1 <- as.data.frame(cov_99_el_6_1)
# 
# 
# cov_95_pois_1_1 <- as.data.frame(cov_95_pois_1_1)
# cov_95_qp_1_1 <- as.data.frame(cov_95_qp_1_1)
# cov_95_nb_1_1 <- as.data.frame(cov_95_nb_1_1)
# cov_95_el_1_1 <- as.data.frame(cov_95_el_1_1)
# 
# cov_95_pois_2_1 <- as.data.frame(cov_95_pois_2_1)
# cov_95_qp_2_1 <- as.data.frame(cov_95_qp_2_1)
# cov_95_nb_2_1 <- as.data.frame(cov_95_nb_2_1)
# cov_95_el_2_1 <- as.data.frame(cov_95_el_2_1)
# 
# cov_95_pois_3_1 <- as.data.frame(cov_95_pois_3_1)
# cov_95_qp_3_1 <- as.data.frame(cov_95_qp_3_1)
# cov_95_nb_3_1 <- as.data.frame(cov_95_nb_3_1)
# cov_95_el_3_1 <- as.data.frame(cov_95_el_3_1)
# 
# cov_95_pois_4_1 <- as.data.frame(cov_95_pois_4_1)
# cov_95_qp_4_1 <- as.data.frame(cov_95_qp_4_1)
# cov_95_nb_4_1 <- as.data.frame(cov_95_nb_4_1)
# cov_95_el_4_1 <- as.data.frame(cov_95_el_4_1)
# 
# cov_95_pois_5_1 <- as.data.frame(cov_95_pois_5_1)
# cov_95_qp_5_1 <- as.data.frame(cov_95_qp_5_1)
# cov_95_nb_5_1 <- as.data.frame(cov_95_nb_5_1)
# cov_95_el_5_1 <- as.data.frame(cov_95_el_5_1)
# 
# cov_95_pois_6_1 <- as.data.frame(cov_95_pois_6_1)
# cov_95_qp_6_1 <- as.data.frame(cov_95_qp_6_1)
# cov_95_nb_6_1 <- as.data.frame(cov_95_nb_6_1)
# cov_95_el_6_1 <- as.data.frame(cov_95_el_6_1)
# 
# 
# 
# 
# cov_99_pois_1_2 <- as.data.frame(cov_99_pois_1_2)
# cov_99_qp_1_2 <- as.data.frame(cov_99_qp_1_2)
# cov_99_nb_1_2 <- as.data.frame(cov_99_nb_1_2)
# cov_99_el_1_2 <- as.data.frame(cov_99_el_1_2)
# 
# cov_99_pois_2_2 <- as.data.frame(cov_99_pois_2_2)
# cov_99_qp_2_2 <- as.data.frame(cov_99_qp_2_2)
# cov_99_nb_2_2 <- as.data.frame(cov_99_nb_2_2)
# cov_99_el_2_2 <- as.data.frame(cov_99_el_2_2)
# 
# cov_99_pois_3_2 <- as.data.frame(cov_99_pois_3_2)
# cov_99_qp_3_2 <- as.data.frame(cov_99_qp_3_2)
# cov_99_nb_3_2 <- as.data.frame(cov_99_nb_3_2)
# cov_99_el_3_2 <- as.data.frame(cov_99_el_3_2)
# 
# cov_99_pois_4_2 <- as.data.frame(cov_99_pois_4_2)
# cov_99_qp_4_2 <- as.data.frame(cov_99_qp_4_2)
# cov_99_nb_4_2 <- as.data.frame(cov_99_nb_4_2)
# cov_99_el_4_2 <- as.data.frame(cov_99_el_4_2)
# 
# cov_99_pois_5_2 <- as.data.frame(cov_99_pois_5_2)
# cov_99_qp_5_2 <- as.data.frame(cov_99_qp_5_2)
# cov_99_nb_5_2 <- as.data.frame(cov_99_nb_5_2)
# cov_99_el_5_2 <- as.data.frame(cov_99_el_5_2)
# 
# cov_99_pois_6_2 <- as.data.frame(cov_99_pois_6_2)
# cov_99_qp_6_2 <- as.data.frame(cov_99_qp_6_2)
# cov_99_nb_6_2 <- as.data.frame(cov_99_nb_6_2)
# cov_99_el_6_2 <- as.data.frame(cov_99_el_6_2)
# 
# 
# cov_95_pois_1_2 <- as.data.frame(cov_95_pois_1_2)
# cov_95_qp_1_2 <- as.data.frame(cov_95_qp_1_2)
# cov_95_nb_1_2 <- as.data.frame(cov_95_nb_1_2)
# cov_95_el_1_2 <- as.data.frame(cov_95_el_1_2)
# 
# cov_95_pois_2_2 <- as.data.frame(cov_95_pois_2_2)
# cov_95_qp_2_2 <- as.data.frame(cov_95_qp_2_2)
# cov_95_nb_2_2 <- as.data.frame(cov_95_nb_2_2)
# cov_95_el_2_2 <- as.data.frame(cov_95_el_2_2)
# 
# cov_95_pois_3_2 <- as.data.frame(cov_95_pois_3_2)
# cov_95_qp_3_2 <- as.data.frame(cov_95_qp_3_2)
# cov_95_nb_3_2 <- as.data.frame(cov_95_nb_3_2)
# cov_95_el_3_2 <- as.data.frame(cov_95_el_3_2)
# 
# cov_95_pois_4_2 <- as.data.frame(cov_95_pois_4_2)
# cov_95_qp_4_2 <- as.data.frame(cov_95_qp_4_2)
# cov_95_nb_4_2 <- as.data.frame(cov_95_nb_4_2)
# cov_95_el_4_2 <- as.data.frame(cov_95_el_4_2)
# 
# cov_95_pois_5_2 <- as.data.frame(cov_95_pois_5_2)
# cov_95_qp_5_2 <- as.data.frame(cov_95_qp_5_2)
# cov_95_nb_5_2 <- as.data.frame(cov_95_nb_5_2)
# cov_95_el_5_2 <- as.data.frame(cov_95_el_5_2)
# 
# cov_95_pois_6_2 <- as.data.frame(cov_95_pois_6_2)
# cov_95_qp_6_2 <- as.data.frame(cov_95_qp_6_2)
# cov_95_nb_6_2 <- as.data.frame(cov_95_nb_6_2)
# cov_95_el_6_2 <- as.data.frame(cov_95_el_6_2)
# 

conf_in_pois_1_95_1 <- as.data.frame(conf_in_pois_1_95_1)
conf_in_qp_1_95_1 <- as.data.frame(conf_in_qp_1_95_1)
conf_in_nb_1_95_1 <- as.data.frame(conf_in_nb_1_95_1)
conf_in_el_1_95_1 <- as.data.frame(conf_in_el_1_95_1)

conf_in_pois_2_95_1 <- as.data.frame(conf_in_pois_2_95_1)
conf_in_qp_2_95_1 <- as.data.frame(conf_in_qp_2_95_1)
conf_in_nb_2_95_1 <- as.data.frame(conf_in_nb_2_95_1)
conf_in_el_2_95_1 <- as.data.frame(conf_in_el_2_95_1)

conf_in_pois_3_95_1 <- as.data.frame(conf_in_pois_3_95_1)
conf_in_qp_3_95_1 <- as.data.frame(conf_in_qp_3_95_1)
conf_in_nb_3_95_1 <- as.data.frame(conf_in_nb_3_95_1)
conf_in_el_3_95_1 <- as.data.frame(conf_in_el_3_95_1)

conf_in_pois_4_95_1 <- as.data.frame(conf_in_pois_4_95_1)
conf_in_qp_4_95_1 <- as.data.frame(conf_in_qp_4_95_1)
conf_in_nb_4_95_1 <- as.data.frame(conf_in_nb_4_95_1)
conf_in_el_4_95_1 <- as.data.frame(conf_in_el_4_95_1)

conf_in_pois_5_95_1 <- as.data.frame(conf_in_pois_5_95_1)
conf_in_qp_5_95_1 <- as.data.frame(conf_in_qp_5_95_1)
conf_in_nb_5_95_1 <- as.data.frame(conf_in_nb_5_95_1)
conf_in_el_5_95_1 <- as.data.frame(conf_in_el_5_95_1)

conf_in_pois_6_95_1 <- as.data.frame(conf_in_pois_6_95_1)
conf_in_qp_6_95_1 <- as.data.frame(conf_in_qp_6_95_1)
conf_in_nb_6_95_1 <- as.data.frame(conf_in_nb_6_95_1)
conf_in_el_6_95_1 <- as.data.frame(conf_in_el_6_95_1)



conf_in_pois_1_99_1 <- as.data.frame(conf_in_pois_1_99_1)
conf_in_qp_1_99_1 <- as.data.frame(conf_in_qp_1_99_1)
conf_in_nb_1_99_1 <- as.data.frame(conf_in_nb_1_99_1)
conf_in_el_1_99_1 <- as.data.frame(conf_in_el_1_99_1)

conf_in_pois_2_99_1 <- as.data.frame(conf_in_pois_2_99_1)
conf_in_qp_2_99_1 <- as.data.frame(conf_in_qp_2_99_1)
conf_in_nb_2_99_1 <- as.data.frame(conf_in_nb_2_99_1)
conf_in_el_2_99_1 <- as.data.frame(conf_in_el_2_99_1)

conf_in_pois_3_99_1 <- as.data.frame(conf_in_pois_3_99_1)
conf_in_qp_3_99_1 <- as.data.frame(conf_in_qp_3_99_1)
conf_in_nb_3_99_1 <- as.data.frame(conf_in_nb_3_99_1)
conf_in_el_3_99_1 <- as.data.frame(conf_in_el_3_99_1)

conf_in_pois_4_99_1 <- as.data.frame(conf_in_pois_4_99_1)
conf_in_qp_4_99_1 <- as.data.frame(conf_in_qp_4_99_1)
conf_in_nb_4_99_1 <- as.data.frame(conf_in_nb_4_99_1)
conf_in_el_4_99_1 <- as.data.frame(conf_in_el_4_99_1)

conf_in_pois_5_99_1 <- as.data.frame(conf_in_pois_5_99_1)
conf_in_qp_5_99_1 <- as.data.frame(conf_in_qp_5_99_1)
conf_in_nb_5_99_1 <- as.data.frame(conf_in_nb_5_99_1)
conf_in_el_5_99_1 <- as.data.frame(conf_in_el_5_99_1)

conf_in_pois_6_99_1 <- as.data.frame(conf_in_pois_6_99_1)
conf_in_qp_6_99_1 <- as.data.frame(conf_in_qp_6_99_1)
conf_in_nb_6_99_1 <- as.data.frame(conf_in_nb_6_99_1)
conf_in_el_6_99_1 <- as.data.frame(conf_in_el_6_99_1)



conf_in_pois_1_95_2 <- as.data.frame(conf_in_pois_1_95_2)
conf_in_qp_1_95_2 <- as.data.frame(conf_in_qp_1_95_2)
conf_in_nb_1_95_2 <- as.data.frame(conf_in_nb_1_95_2)
conf_in_el_1_95_2 <- as.data.frame(conf_in_el_1_95_2)

conf_in_pois_2_95_2 <- as.data.frame(conf_in_pois_2_95_2)
conf_in_qp_2_95_2 <- as.data.frame(conf_in_qp_2_95_2)
conf_in_nb_2_95_2 <- as.data.frame(conf_in_nb_2_95_2)
conf_in_el_2_95_2 <- as.data.frame(conf_in_el_2_95_2)

conf_in_pois_3_95_2 <- as.data.frame(conf_in_pois_3_95_2)
conf_in_qp_3_95_2 <- as.data.frame(conf_in_qp_3_95_2)
conf_in_nb_3_95_2 <- as.data.frame(conf_in_nb_3_95_2)
conf_in_el_3_95_2 <- as.data.frame(conf_in_el_3_95_2)

conf_in_pois_4_95_2 <- as.data.frame(conf_in_pois_4_95_2)
conf_in_qp_4_95_2 <- as.data.frame(conf_in_qp_4_95_2)
conf_in_nb_4_95_2 <- as.data.frame(conf_in_nb_4_95_2)
conf_in_el_4_95_2 <- as.data.frame(conf_in_el_4_95_2)

conf_in_pois_5_95_2 <- as.data.frame(conf_in_pois_5_95_2)
conf_in_qp_5_95_2 <- as.data.frame(conf_in_qp_5_95_2)
conf_in_nb_5_95_2 <- as.data.frame(conf_in_nb_5_95_2)
conf_in_el_5_95_2 <- as.data.frame(conf_in_el_5_95_2)

conf_in_pois_6_95_2 <- as.data.frame(conf_in_pois_6_95_2)
conf_in_qp_6_95_2 <- as.data.frame(conf_in_qp_6_95_2)
conf_in_nb_6_95_2 <- as.data.frame(conf_in_nb_6_95_2)
conf_in_el_6_95_2 <- as.data.frame(conf_in_el_6_95_2)



conf_in_pois_1_99_2 <- as.data.frame(conf_in_pois_1_99_2)
conf_in_qp_1_99_2 <- as.data.frame(conf_in_qp_1_99_2)
conf_in_nb_1_99_2 <- as.data.frame(conf_in_nb_1_99_2)
conf_in_el_1_99_2 <- as.data.frame(conf_in_el_1_99_2)

conf_in_pois_2_99_2 <- as.data.frame(conf_in_pois_2_99_2)
conf_in_qp_2_99_2 <- as.data.frame(conf_in_qp_2_99_2)
conf_in_nb_2_99_2 <- as.data.frame(conf_in_nb_2_99_2)
conf_in_el_2_99_2 <- as.data.frame(conf_in_el_2_99_2)

conf_in_pois_3_99_2 <- as.data.frame(conf_in_pois_3_99_2)
conf_in_qp_3_99_2 <- as.data.frame(conf_in_qp_3_99_2)
conf_in_nb_3_99_2 <- as.data.frame(conf_in_nb_3_99_2)
conf_in_el_3_99_2 <- as.data.frame(conf_in_el_3_99_2)

conf_in_pois_4_99_2 <- as.data.frame(conf_in_pois_4_99_2)
conf_in_qp_4_99_2 <- as.data.frame(conf_in_qp_4_99_2)
conf_in_nb_4_99_2 <- as.data.frame(conf_in_nb_4_99_2)
conf_in_el_4_99_2 <- as.data.frame(conf_in_el_4_99_2)

conf_in_pois_5_99_2 <- as.data.frame(conf_in_pois_5_99_2)
conf_in_qp_5_99_2 <- as.data.frame(conf_in_qp_5_99_2)
conf_in_nb_5_99_2 <- as.data.frame(conf_in_nb_5_99_2)
conf_in_el_5_99_2 <- as.data.frame(conf_in_el_5_99_2)

conf_in_pois_6_99_2 <- as.data.frame(conf_in_pois_6_99_2)
conf_in_qp_6_99_2 <- as.data.frame(conf_in_qp_6_99_2)
conf_in_nb_6_99_2 <- as.data.frame(conf_in_nb_6_99_2)
conf_in_el_6_99_2 <- as.data.frame(conf_in_el_6_99_2)


conf_in_names <- c("b0_low", "b0_upper", "b1_low", "b1_upper", "b2_low", "b2_upper")
est_names <- c("b0_nov", "b1_nov", "b2_nov")
cov_names <- c("b0_cov", "b1_cov", "b2_cov", "b0_ci_len", "b1_ci_len", "b2_ci_len")

names(conf_in_pois_1_95_1) <- conf_in_names
names(conf_in_qp_1_95_1) <- conf_in_names
names(conf_in_nb_1_95_1) <- conf_in_names
names(conf_in_el_1_95_1) <- conf_in_names

names(conf_in_pois_2_95_1) <- conf_in_names
names(conf_in_qp_2_95_1) <- conf_in_names
names(conf_in_nb_2_95_1) <- conf_in_names
names(conf_in_el_2_95_1) <- conf_in_names

names(conf_in_pois_3_95_1) <- conf_in_names
names(conf_in_qp_3_95_1) <- conf_in_names
names(conf_in_nb_3_95_1) <- conf_in_names
names(conf_in_el_3_95_1) <- conf_in_names

names(conf_in_pois_4_95_1) <- conf_in_names
names(conf_in_qp_4_95_1) <- conf_in_names
names(conf_in_nb_4_95_1) <- conf_in_names
names(conf_in_el_4_95_1) <- conf_in_names

names(conf_in_pois_5_95_1) <- conf_in_names
names(conf_in_qp_5_95_1) <- conf_in_names
names(conf_in_nb_5_95_1) <- conf_in_names
names(conf_in_el_5_95_1) <- conf_in_names

names(conf_in_pois_6_95_1) <- conf_in_names
names(conf_in_qp_6_95_1) <- conf_in_names
names(conf_in_nb_6_95_1) <- conf_in_names
names(conf_in_el_6_95_1) <- conf_in_names


names(conf_in_pois_1_99_1) <- conf_in_names
names(conf_in_qp_1_99_1) <- conf_in_names
names(conf_in_nb_1_99_1) <- conf_in_names
names(conf_in_el_1_99_1) <- conf_in_names

names(conf_in_pois_2_99_1) <- conf_in_names
names(conf_in_qp_2_99_1) <- conf_in_names
names(conf_in_nb_2_99_1) <- conf_in_names
names(conf_in_el_2_99_1) <- conf_in_names

names(conf_in_pois_3_99_1) <- conf_in_names
names(conf_in_qp_3_99_1) <- conf_in_names
names(conf_in_nb_3_99_1) <- conf_in_names
names(conf_in_el_3_99_1) <- conf_in_names

names(conf_in_pois_4_99_1) <- conf_in_names
names(conf_in_qp_4_99_1) <- conf_in_names
names(conf_in_nb_4_99_1) <- conf_in_names
names(conf_in_el_4_99_1) <- conf_in_names

names(conf_in_pois_5_99_1) <- conf_in_names
names(conf_in_qp_5_99_1) <- conf_in_names
names(conf_in_nb_5_99_1) <- conf_in_names
names(conf_in_el_5_99_1) <- conf_in_names

names(conf_in_pois_6_99_1) <- conf_in_names
names(conf_in_qp_6_99_1) <- conf_in_names
names(conf_in_nb_6_99_1) <- conf_in_names
names(conf_in_el_6_99_1) <- conf_in_names




names(conf_in_pois_1_95_2) <- conf_in_names
names(conf_in_qp_1_95_2) <- conf_in_names
names(conf_in_nb_1_95_2) <- conf_in_names
names(conf_in_el_1_95_2) <- conf_in_names

names(conf_in_pois_2_95_2) <- conf_in_names
names(conf_in_qp_2_95_2) <- conf_in_names
names(conf_in_nb_2_95_2) <- conf_in_names
names(conf_in_el_2_95_2) <- conf_in_names

names(conf_in_pois_3_95_2) <- conf_in_names
names(conf_in_qp_3_95_2) <- conf_in_names
names(conf_in_nb_3_95_2) <- conf_in_names
names(conf_in_el_3_95_2) <- conf_in_names

names(conf_in_pois_4_95_2) <- conf_in_names
names(conf_in_qp_4_95_2) <- conf_in_names
names(conf_in_nb_4_95_2) <- conf_in_names
names(conf_in_el_4_95_2) <- conf_in_names

names(conf_in_pois_5_95_2) <- conf_in_names
names(conf_in_qp_5_95_2) <- conf_in_names
names(conf_in_nb_5_95_2) <- conf_in_names
names(conf_in_el_5_95_2) <- conf_in_names

names(conf_in_pois_6_95_2) <- conf_in_names
names(conf_in_qp_6_95_2) <- conf_in_names
names(conf_in_nb_6_95_2) <- conf_in_names
names(conf_in_el_6_95_2) <- conf_in_names


names(conf_in_pois_1_99_2) <- conf_in_names
names(conf_in_qp_1_99_2) <- conf_in_names
names(conf_in_nb_1_99_2) <- conf_in_names
names(conf_in_el_1_99_2) <- conf_in_names

names(conf_in_pois_2_99_2) <- conf_in_names
names(conf_in_qp_2_99_2) <- conf_in_names
names(conf_in_nb_2_99_2) <- conf_in_names
names(conf_in_el_2_99_2) <- conf_in_names

names(conf_in_pois_3_99_2) <- conf_in_names
names(conf_in_qp_3_99_2) <- conf_in_names
names(conf_in_nb_3_99_2) <- conf_in_names
names(conf_in_el_3_99_2) <- conf_in_names

names(conf_in_pois_4_99_2) <- conf_in_names
names(conf_in_qp_4_99_2) <- conf_in_names
names(conf_in_nb_4_99_2) <- conf_in_names
names(conf_in_el_4_99_2) <- conf_in_names

names(conf_in_pois_5_99_2) <- conf_in_names
names(conf_in_qp_5_99_2) <- conf_in_names
names(conf_in_nb_5_99_2) <- conf_in_names
names(conf_in_el_5_99_2) <- conf_in_names

names(conf_in_pois_6_99_2) <- conf_in_names
names(conf_in_qp_6_99_2) <- conf_in_names
names(conf_in_nb_6_99_2) <- conf_in_names
names(conf_in_el_6_99_2) <- conf_in_names













# names(est_pois_1_1) <- est_names
# names(est_qp_1_1) <- est_names
# names(est_nb_1_1) <- est_names
# names(est_el_1_1) <- est_names
# 
# names(est_pois_2_1) <- est_names
# names(est_qp_2_1) <- est_names
# names(est_nb_2_1) <- est_names
# names(est_el_2_1) <- est_names
# 
# names(est_pois_3_1) <- est_names
# names(est_qp_3_1) <- est_names
# names(est_nb_3_1) <- est_names
# names(est_el_3_1) <- est_names
# 
# names(est_pois_4_1) <- est_names
# names(est_qp_4_1) <- est_names
# names(est_nb_4_1) <- est_names
# names(est_el_4_1) <- est_names
# 
# names(est_pois_5_1) <- est_names
# names(est_qp_5_1) <- est_names
# names(est_nb_5_1) <- est_names
# names(est_el_5_1) <- est_names
# 
# names(est_pois_6_1) <- est_names
# names(est_qp_6_1) <- est_names
# names(est_nb_6_1) <- est_names
# names(est_el_6_1) <- est_names
# 
# 
# names(cov_95_pois_1_1) <- cov_names
# names(cov_95_qp_1_1) <- cov_names
# names(cov_95_nb_1_1) <- cov_names
# names(cov_95_el_1_1) <- cov_names
# 
# names(cov_95_pois_2_1) <- cov_names
# names(cov_95_qp_2_1) <- cov_names
# names(cov_95_nb_2_1) <- cov_names
# names(cov_95_el_2_1) <- cov_names
# 
# names(cov_95_pois_3_1) <- cov_names
# names(cov_95_qp_3_1) <- cov_names
# names(cov_95_nb_3_1) <- cov_names
# names(cov_95_el_3_1) <- cov_names
# 
# names(cov_95_pois_4_1) <- cov_names
# names(cov_95_qp_4_1) <- cov_names
# names(cov_95_nb_4_1) <- cov_names
# names(cov_95_el_4_1) <- cov_names
# 
# names(cov_95_pois_5_1) <- cov_names
# names(cov_95_qp_5_1) <- cov_names
# names(cov_95_nb_5_1) <- cov_names
# names(cov_95_el_5_1) <- cov_names
# 
# names(cov_95_pois_6_1) <- cov_names
# names(cov_95_qp_6_1) <- cov_names
# names(cov_95_nb_6_1) <- cov_names
# names(cov_95_el_6_1) <- cov_names
# 
# 
# names(cov_99_pois_1_1) <- cov_names
# names(cov_99_qp_1_1) <- cov_names
# names(cov_99_nb_1_1) <- cov_names
# names(cov_99_el_1_1) <- cov_names
# 
# names(cov_99_pois_2_1) <- cov_names
# names(cov_99_qp_2_1) <- cov_names
# names(cov_99_nb_2_1) <- cov_names
# names(cov_99_el_2_1) <- cov_names
# 
# names(cov_99_pois_3_1) <- cov_names
# names(cov_99_qp_3_1) <- cov_names
# names(cov_99_nb_3_1) <- cov_names
# names(cov_99_el_3_1) <- cov_names
# 
# names(cov_99_pois_4_1) <- cov_names
# names(cov_99_qp_4_1) <- cov_names
# names(cov_99_nb_4_1) <- cov_names
# names(cov_99_el_4_1) <- cov_names
# 
# names(cov_99_pois_5_1) <- cov_names
# names(cov_99_qp_5_1) <- cov_names
# names(cov_99_nb_5_1) <- cov_names
# names(cov_99_el_5_1) <- cov_names
# 
# names(cov_99_pois_6_1) <- cov_names
# names(cov_99_qp_6_1) <- cov_names
# names(cov_99_nb_6_1) <- cov_names
# names(cov_99_el_6_1) <- cov_names
# 
# 
# 
# names(est_pois_1_2) <- est_names
# names(est_qp_1_2) <- est_names
# names(est_nb_1_2) <- est_names
# names(est_el_1_2) <- est_names
# 
# names(est_pois_2_2) <- est_names
# names(est_qp_2_2) <- est_names
# names(est_nb_2_2) <- est_names
# names(est_el_2_2) <- est_names
# 
# names(est_pois_3_2) <- est_names
# names(est_qp_3_2) <- est_names
# names(est_nb_3_2) <- est_names
# names(est_el_3_2) <- est_names
# 
# names(est_pois_4_2) <- est_names
# names(est_qp_4_2) <- est_names
# names(est_nb_4_2) <- est_names
# names(est_el_4_2) <- est_names
# 
# names(est_pois_5_2) <- est_names
# names(est_qp_5_2) <- est_names
# names(est_nb_5_2) <- est_names
# names(est_el_5_2) <- est_names
# 
# names(est_pois_6_2) <- est_names
# names(est_qp_6_2) <- est_names
# names(est_nb_6_2) <- est_names
# names(est_el_6_2) <- est_names
# 
# 
# names(cov_95_pois_1_2) <- cov_names
# names(cov_95_qp_1_2) <- cov_names
# names(cov_95_nb_1_2) <- cov_names
# names(cov_95_el_1_2) <- cov_names
# 
# names(cov_95_pois_2_2) <- cov_names
# names(cov_95_qp_2_2) <- cov_names
# names(cov_95_nb_2_2) <- cov_names
# names(cov_95_el_2_2) <- cov_names
# 
# names(cov_95_pois_3_2) <- cov_names
# names(cov_95_qp_3_2) <- cov_names
# names(cov_95_nb_3_2) <- cov_names
# names(cov_95_el_3_2) <- cov_names
# 
# names(cov_95_pois_4_2) <- cov_names
# names(cov_95_qp_4_2) <- cov_names
# names(cov_95_nb_4_2) <- cov_names
# names(cov_95_el_4_2) <- cov_names
# 
# names(cov_95_pois_5_2) <- cov_names
# names(cov_95_qp_5_2) <- cov_names
# names(cov_95_nb_5_2) <- cov_names
# names(cov_95_el_5_2) <- cov_names
# 
# names(cov_95_pois_6_2) <- cov_names
# names(cov_95_qp_6_2) <- cov_names
# names(cov_95_nb_6_2) <- cov_names
# names(cov_95_el_6_2) <- cov_names
# 
# 
# names(cov_99_pois_1_2) <- cov_names
# names(cov_99_qp_1_2) <- cov_names
# names(cov_99_nb_1_2) <- cov_names
# names(cov_99_el_1_2) <- cov_names
# 
# names(cov_99_pois_2_2) <- cov_names
# names(cov_99_qp_2_2) <- cov_names
# names(cov_99_nb_2_2) <- cov_names
# names(cov_99_el_2_2) <- cov_names
# 
# names(cov_99_pois_3_2) <- cov_names
# names(cov_99_qp_3_2) <- cov_names
# names(cov_99_nb_3_2) <- cov_names
# names(cov_99_el_3_2) <- cov_names
# 
# names(cov_99_pois_4_2) <- cov_names
# names(cov_99_qp_4_2) <- cov_names
# names(cov_99_nb_4_2) <- cov_names
# names(cov_99_el_4_2) <- cov_names
# 
# names(cov_99_pois_5_2) <- cov_names
# names(cov_99_qp_5_2) <- cov_names
# names(cov_99_nb_5_2) <- cov_names
# names(cov_99_el_5_2) <- cov_names
# 
# names(cov_99_pois_6_2) <- cov_names
# names(cov_99_qp_6_2) <- cov_names
# names(cov_99_nb_6_2) <- cov_names
# names(cov_99_el_6_2) <- cov_names


# 


ci_gr_pois_1_95_1 <- cbind(conf_in_pois_1_95_1, est_pois_1_1, cov_95_pois_1_1)
ci_gr_qp_1_95_1 <- cbind(conf_in_qp_1_95_1, est_qp_1_1, cov_95_qp_1_1)
ci_gr_nb_1_95_1 <- cbind(conf_in_nb_1_95_1, est_nb_1_1, cov_95_nb_1_1)
ci_gr_el_1_95_1 <- cbind(conf_in_el_1_95_1, est_el_1_1, cov_95_el_1_1)

ci_gr_pois_2_95_1 <- cbind(conf_in_pois_2_95_1, est_pois_2_1, cov_95_pois_2_1)
ci_gr_qp_2_95_1 <- cbind(conf_in_qp_2_95_1, est_qp_2_1, cov_95_qp_2_1)
ci_gr_nb_2_95_1 <- cbind(conf_in_nb_2_95_1, est_nb_2_1, cov_95_nb_2_1)
ci_gr_el_2_95_1 <- cbind(conf_in_el_2_95_1, est_el_2_1, cov_95_el_2_1)

ci_gr_pois_3_95_1 <- cbind(conf_in_pois_3_95_1, est_pois_3_1, cov_95_pois_3_1)
ci_gr_qp_3_95_1 <- cbind(conf_in_qp_3_95_1, est_qp_3_1, cov_95_qp_3_1)
ci_gr_nb_3_95_1 <- cbind(conf_in_nb_3_95_1, est_nb_3_1, cov_95_nb_3_1)
ci_gr_el_3_95_1 <- cbind(conf_in_el_3_95_1, est_el_3_1, cov_95_el_3_1)

ci_gr_pois_4_95_1 <- cbind(conf_in_pois_4_95_1, est_pois_4_1, cov_95_pois_4_1)
ci_gr_qp_4_95_1 <- cbind(conf_in_qp_4_95_1, est_qp_4_1, cov_95_qp_4_1)
ci_gr_nb_4_95_1 <- cbind(conf_in_nb_4_95_1, est_nb_4_1, cov_95_nb_4_1)
ci_gr_el_4_95_1 <- cbind(conf_in_el_4_95_1, est_el_4_1, cov_95_el_4_1)

ci_gr_pois_5_95_1 <- cbind(conf_in_pois_5_95_1, est_pois_5_1, cov_95_pois_5_1)
ci_gr_qp_5_95_1 <- cbind(conf_in_qp_5_95_1, est_qp_5_1, cov_95_qp_5_1)
ci_gr_nb_5_95_1 <- cbind(conf_in_nb_5_95_1, est_nb_5_1, cov_95_nb_5_1)
ci_gr_el_5_95_1 <- cbind(conf_in_el_5_95_1, est_el_5_1, cov_95_el_5_1)

ci_gr_pois_6_95_1 <- cbind(conf_in_pois_6_95_1, est_pois_6_1, cov_95_pois_6_1)
ci_gr_qp_6_95_1 <- cbind(conf_in_qp_6_95_1, est_qp_6_1, cov_95_qp_6_1)
ci_gr_nb_6_95_1 <- cbind(conf_in_nb_6_95_1, est_nb_6_1, cov_95_nb_6_1)
ci_gr_el_6_95_1 <- cbind(conf_in_el_6_95_1, est_el_6_1, cov_95_el_6_1)


ci_gr_pois_1_99_1 <- cbind(conf_in_pois_1_99_1, est_pois_1_1, cov_99_pois_1_1)
ci_gr_qp_1_99_1 <- cbind(conf_in_qp_1_99_1, est_qp_1_1, cov_99_qp_1_1)
ci_gr_nb_1_99_1 <- cbind(conf_in_nb_1_99_1, est_nb_1_1, cov_99_nb_1_1)
ci_gr_el_1_99_1 <- cbind(conf_in_el_1_99_1, est_el_1_1, cov_99_el_1_1)

ci_gr_pois_2_99_1 <- cbind(conf_in_pois_2_99_1, est_pois_2_1, cov_99_pois_2_1)
ci_gr_qp_2_99_1 <- cbind(conf_in_qp_2_99_1, est_qp_2_1, cov_99_qp_2_1)
ci_gr_nb_2_99_1 <- cbind(conf_in_nb_2_99_1, est_nb_2_1, cov_99_nb_2_1)
ci_gr_el_2_99_1 <- cbind(conf_in_el_2_99_1, est_el_2_1, cov_99_el_2_1)

ci_gr_pois_3_99_1 <- cbind(conf_in_pois_3_99_1, est_pois_3_1, cov_99_pois_3_1)
ci_gr_qp_3_99_1 <- cbind(conf_in_qp_3_99_1, est_qp_3_1, cov_99_qp_3_1)
ci_gr_nb_3_99_1 <- cbind(conf_in_nb_3_99_1, est_nb_3_1, cov_99_nb_3_1)
ci_gr_el_3_99_1 <- cbind(conf_in_el_3_99_1, est_el_3_1, cov_99_el_3_1)

ci_gr_pois_4_99_1 <- cbind(conf_in_pois_4_99_1, est_pois_4_1, cov_99_pois_4_1)
ci_gr_qp_4_99_1 <- cbind(conf_in_qp_4_99_1, est_qp_4_1, cov_99_qp_4_1)
ci_gr_nb_4_99_1 <- cbind(conf_in_nb_4_99_1, est_nb_4_1, cov_99_nb_4_1)
ci_gr_el_4_99_1 <- cbind(conf_in_el_4_99_1, est_el_4_1, cov_99_el_4_1)

ci_gr_pois_5_99_1 <- cbind(conf_in_pois_5_99_1, est_pois_5_1, cov_99_pois_5_1)
ci_gr_qp_5_99_1 <- cbind(conf_in_qp_5_99_1, est_qp_5_1, cov_99_qp_5_1)
ci_gr_nb_5_99_1 <- cbind(conf_in_nb_5_99_1, est_nb_5_1, cov_99_nb_5_1)
ci_gr_el_5_99_1 <- cbind(conf_in_el_5_99_1, est_el_5_1, cov_99_el_5_1)

ci_gr_pois_6_99_1 <- cbind(conf_in_pois_6_99_1, est_pois_6_1, cov_99_pois_6_1)
ci_gr_qp_6_99_1 <- cbind(conf_in_qp_6_99_1, est_qp_6_1, cov_99_qp_6_1)
ci_gr_nb_6_99_1 <- cbind(conf_in_nb_6_99_1, est_nb_6_1, cov_99_nb_6_1)
ci_gr_el_6_99_1 <- cbind(conf_in_el_6_99_1, est_el_6_1, cov_99_el_6_1)




ci_gr_pois_1_95_2 <- cbind(conf_in_pois_1_95_2, est_pois_1_2, cov_95_pois_1_2)
ci_gr_qp_1_95_2 <- cbind(conf_in_qp_1_95_2, est_qp_1_2, cov_95_qp_1_2)
ci_gr_nb_1_95_2 <- cbind(conf_in_nb_1_95_2, est_nb_1_2, cov_95_nb_1_2)
ci_gr_el_1_95_2 <- cbind(conf_in_el_1_95_2, est_el_1_2, cov_95_el_1_2)

ci_gr_pois_2_95_2 <- cbind(conf_in_pois_2_95_2, est_pois_2_2, cov_95_pois_2_2)
ci_gr_qp_2_95_2 <- cbind(conf_in_qp_2_95_2, est_qp_2_2, cov_95_qp_2_2)
ci_gr_nb_2_95_2 <- cbind(conf_in_nb_2_95_2, est_nb_2_2, cov_95_nb_2_2)
ci_gr_el_2_95_2 <- cbind(conf_in_el_2_95_2, est_el_2_2, cov_95_el_2_2)

ci_gr_pois_3_95_2 <- cbind(conf_in_pois_3_95_2, est_pois_3_2, cov_95_pois_3_2)
ci_gr_qp_3_95_2 <- cbind(conf_in_qp_3_95_2, est_qp_3_2, cov_95_qp_3_2)
ci_gr_nb_3_95_2 <- cbind(conf_in_nb_3_95_2, est_nb_3_2, cov_95_nb_3_2)
ci_gr_el_3_95_2 <- cbind(conf_in_el_3_95_2, est_el_3_2, cov_95_el_3_2)

ci_gr_pois_4_95_2 <- cbind(conf_in_pois_4_95_2, est_pois_4_2, cov_95_pois_4_2)
ci_gr_qp_4_95_2 <- cbind(conf_in_qp_4_95_2, est_qp_4_2, cov_95_qp_4_2)
ci_gr_nb_4_95_2 <- cbind(conf_in_nb_4_95_2, est_nb_4_2, cov_95_nb_4_2)
ci_gr_el_4_95_2 <- cbind(conf_in_el_4_95_2, est_el_4_2, cov_95_el_4_2)

ci_gr_pois_5_95_2 <- cbind(conf_in_pois_5_95_2, est_pois_5_2, cov_95_pois_5_2)
ci_gr_qp_5_95_2 <- cbind(conf_in_qp_5_95_2, est_qp_5_2, cov_95_qp_5_2)
ci_gr_nb_5_95_2 <- cbind(conf_in_nb_5_95_2, est_nb_5_2, cov_95_nb_5_2)
ci_gr_el_5_95_2 <- cbind(conf_in_el_5_95_2, est_el_5_2, cov_95_el_5_2)

ci_gr_pois_6_95_2 <- cbind(conf_in_pois_6_95_2, est_pois_6_2, cov_95_pois_6_2)
ci_gr_qp_6_95_2 <- cbind(conf_in_qp_6_95_2, est_qp_6_2, cov_95_qp_6_2)
ci_gr_nb_6_95_2 <- cbind(conf_in_nb_6_95_2, est_nb_6_2, cov_95_nb_6_2)
ci_gr_el_6_95_2 <- cbind(conf_in_el_6_95_2, est_el_6_2, cov_95_el_6_2)


ci_gr_pois_1_99_2 <- cbind(conf_in_pois_1_99_2, est_pois_1_2, cov_99_pois_1_2)
ci_gr_qp_1_99_2 <- cbind(conf_in_qp_1_99_2, est_qp_1_2, cov_99_qp_1_2)
ci_gr_nb_1_99_2 <- cbind(conf_in_nb_1_99_2, est_nb_1_2, cov_99_nb_1_2)
ci_gr_el_1_99_2 <- cbind(conf_in_el_1_99_2, est_el_1_2, cov_99_el_1_2)

ci_gr_pois_2_99_2 <- cbind(conf_in_pois_2_99_2, est_pois_2_2, cov_99_pois_2_2)
ci_gr_qp_2_99_2 <- cbind(conf_in_qp_2_99_2, est_qp_2_2, cov_99_qp_2_2)
ci_gr_nb_2_99_2 <- cbind(conf_in_nb_2_99_2, est_nb_2_2, cov_99_nb_2_2)
ci_gr_el_2_99_2 <- cbind(conf_in_el_2_99_2, est_el_2_2, cov_99_el_2_2)

ci_gr_pois_3_99_2 <- cbind(conf_in_pois_3_99_2, est_pois_3_2, cov_99_pois_3_2)
ci_gr_qp_3_99_2 <- cbind(conf_in_qp_3_99_2, est_qp_3_2, cov_99_qp_3_2)
ci_gr_nb_3_99_2 <- cbind(conf_in_nb_3_99_2, est_nb_3_2, cov_99_nb_3_2)
ci_gr_el_3_99_2 <- cbind(conf_in_el_3_99_2, est_el_3_2, cov_99_el_3_2)

ci_gr_pois_4_99_2 <- cbind(conf_in_pois_4_99_2, est_pois_4_2, cov_99_pois_4_2)
ci_gr_qp_4_99_2 <- cbind(conf_in_qp_4_99_2, est_qp_4_2, cov_99_qp_4_2)
ci_gr_nb_4_99_2 <- cbind(conf_in_nb_4_99_2, est_nb_4_2, cov_99_nb_4_2)
ci_gr_el_4_99_2 <- cbind(conf_in_el_4_99_2, est_el_4_2, cov_99_el_4_2)

ci_gr_pois_5_99_2 <- cbind(conf_in_pois_5_99_2, est_pois_5_2, cov_99_pois_5_2)
ci_gr_qp_5_99_2 <- cbind(conf_in_qp_5_99_2, est_qp_5_2, cov_99_qp_5_2)
ci_gr_nb_5_99_2 <- cbind(conf_in_nb_5_99_2, est_nb_5_2, cov_99_nb_5_2)
ci_gr_el_5_99_2 <- cbind(conf_in_el_5_99_2, est_el_5_2, cov_99_el_5_2)

ci_gr_pois_6_99_2 <- cbind(conf_in_pois_6_99_2, est_pois_6_2, cov_99_pois_6_2)
ci_gr_qp_6_99_2 <- cbind(conf_in_qp_6_99_2, est_qp_6_2, cov_99_qp_6_2)
ci_gr_nb_6_99_2 <- cbind(conf_in_nb_6_99_2, est_nb_6_2, cov_99_nb_6_2)
ci_gr_el_6_99_2 <- cbind(conf_in_el_6_99_2, est_el_6_2, cov_99_el_6_2)

##################

colorset <-  c('0'='red','1'='black')

detach("package:MASS", unload=TRUE)

ci_gr_pois_1_95_1 <- ci_gr_pois_1_95_1[1:250,]

ci_b0_95_p_1_1 <- ci_gr_pois_1_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_1 <- ci_gr_qp_1_95_1[1:250,]

ci_b0_95_qp_1_1 <- ci_gr_qp_1_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_1 <- ci_gr_nb_1_95_1[1:250,]

ci_b0_95_nb_1_1 <- ci_gr_nb_1_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_1 <- ci_gr_el_1_95_1[1:250,]

ci_b0_95_el_1_1 <- ci_gr_el_1_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_1_1, ci_b0_95_qp_1_1, ci_b0_95_nb_1_1, ci_b0_95_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n1_t1_95.png")

# 

ci_gr_pois_2_95_1 <- ci_gr_pois_2_95_1[1:250,]

ci_b0_95_p_2_1 <- ci_gr_pois_2_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_1 <- ci_gr_qp_2_95_1[1:250,]

ci_b0_95_qp_2_1 <- ci_gr_qp_2_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_1 <- ci_gr_nb_2_95_1[1:250,]

ci_b0_95_nb_2_1 <- ci_gr_nb_2_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_1 <- ci_gr_el_2_95_1[1:250,]

ci_b0_95_el_2_1 <- ci_gr_el_2_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_2_1, ci_b0_95_qp_2_1, ci_b0_95_nb_2_1, ci_b0_95_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n2_t1_95.png")


# 

ci_gr_pois_3_95_1 <- ci_gr_pois_3_95_1[1:250,]

ci_b0_95_p_3_1 <- ci_gr_pois_3_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_1 <- ci_gr_qp_3_95_1[1:250,]

ci_b0_95_qp_3_1 <- ci_gr_qp_3_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_1 <- ci_gr_nb_3_95_1[1:250,]

ci_b0_95_nb_3_1 <- ci_gr_nb_3_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_1 <- ci_gr_el_3_95_1[1:250,]

ci_b0_95_el_3_1 <- ci_gr_el_3_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_3_1, ci_b0_95_qp_3_1, ci_b0_95_nb_3_1, ci_b0_95_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n3_t1_95.png")

# 

ci_gr_pois_4_95_1 <- ci_gr_pois_4_95_1[1:250,]

ci_b0_95_p_4_1 <- ci_gr_pois_4_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_1 <- ci_gr_qp_4_95_1[1:250,]

ci_b0_95_qp_4_1 <- ci_gr_qp_4_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_1 <- ci_gr_nb_4_95_1[1:250,]

ci_b0_95_nb_4_1 <- ci_gr_nb_4_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_1 <- ci_gr_el_4_95_1[1:250,]

ci_b0_95_el_4_1 <- ci_gr_el_4_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_4_1, ci_b0_95_qp_4_1, ci_b0_95_nb_4_1, ci_b0_95_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n4_t1_95.png")



ci_gr_pois_5_95_1 <- ci_gr_pois_5_95_1[1:250,]

ci_b0_95_p_5_1 <- ci_gr_pois_5_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_1 <- ci_gr_qp_5_95_1[1:250,]

ci_b0_95_qp_5_1 <- ci_gr_qp_5_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_1 <- ci_gr_nb_5_95_1[1:250,]

ci_b0_95_nb_5_1 <- ci_gr_nb_5_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_1 <- ci_gr_el_5_95_1[1:250,]

ci_b0_95_el_5_1 <- ci_gr_el_5_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_5_1, ci_b0_95_qp_5_1, ci_b0_95_nb_5_1, ci_b0_95_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n5_t1_95.png")




ci_gr_pois_6_95_1 <- ci_gr_pois_6_95_1[1:250,]

ci_b0_95_p_6_1 <- ci_gr_pois_6_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_1 <- ci_gr_qp_6_95_1[1:250,]

ci_b0_95_qp_6_1 <- ci_gr_qp_6_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_1 <- ci_gr_nb_6_95_1[1:250,]

ci_b0_95_nb_6_1 <- ci_gr_nb_6_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_1 <- ci_gr_el_6_95_1[1:250,]

ci_b0_95_el_6_1 <- ci_gr_el_6_95_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_6_1, ci_b0_95_qp_6_1, ci_b0_95_nb_6_1, ci_b0_95_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n6_t1_95.png")

#


ci_gr_pois_1_99_1 <- ci_gr_pois_1_99_1[1:250,]

ci_b0_99_p_1_1 <- ci_gr_pois_1_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_1 <- ci_gr_qp_1_99_1[1:250,]

ci_b0_99_qp_1_1 <- ci_gr_qp_1_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_1 <- ci_gr_nb_1_99_1[1:250,]

ci_b0_99_nb_1_1 <- ci_gr_nb_1_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_1 <- ci_gr_el_1_99_1[1:250,]

ci_b0_99_el_1_1 <- ci_gr_el_1_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_1_1, ci_b0_99_qp_1_1, ci_b0_99_nb_1_1, ci_b0_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n1_t1_99.png")

# 

ci_gr_pois_2_99_1 <- ci_gr_pois_2_99_1[1:250,]

ci_b0_99_p_2_1 <- ci_gr_pois_2_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_1 <- ci_gr_qp_2_99_1[1:250,]

ci_b0_99_qp_2_1 <- ci_gr_qp_2_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_1 <- ci_gr_nb_2_99_1[1:250,]

ci_b0_99_nb_2_1 <- ci_gr_nb_2_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_1 <- ci_gr_el_2_99_1[1:250,]

ci_b0_99_el_2_1 <- ci_gr_el_2_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_2_1, ci_b0_99_qp_2_1, ci_b0_99_nb_2_1, ci_b0_99_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n2_t1_99.png")


# 

ci_gr_pois_3_99_1 <- ci_gr_pois_3_99_1[1:250,]

ci_b0_99_p_3_1 <- ci_gr_pois_3_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_1 <- ci_gr_qp_3_99_1[1:250,]

ci_b0_99_qp_3_1 <- ci_gr_qp_3_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_1 <- ci_gr_nb_3_99_1[1:250,]

ci_b0_99_nb_3_1 <- ci_gr_nb_3_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_1 <- ci_gr_el_3_99_1[1:250,]

ci_b0_99_el_3_1 <- ci_gr_el_3_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_3_1, ci_b0_99_qp_3_1, ci_b0_99_nb_3_1, ci_b0_99_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n3_t1_99.png")

# 

ci_gr_pois_4_99_1 <- ci_gr_pois_4_99_1[1:250,]

ci_b0_99_p_4_1 <- ci_gr_pois_4_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_1 <- ci_gr_qp_4_99_1[1:250,]

ci_b0_99_qp_4_1 <- ci_gr_qp_4_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_1 <- ci_gr_nb_4_99_1[1:250,]

ci_b0_99_nb_4_1 <- ci_gr_nb_4_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_1 <- ci_gr_el_4_99_1[1:250,]

ci_b0_99_el_4_1 <- ci_gr_el_4_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_4_1, ci_b0_99_qp_4_1, ci_b0_99_nb_4_1, ci_b0_99_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n4_t1_99.png")



ci_gr_pois_5_99_1 <- ci_gr_pois_5_99_1[1:250,]

ci_b0_99_p_5_1 <- ci_gr_pois_5_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_1 <- ci_gr_qp_5_99_1[1:250,]

ci_b0_99_qp_5_1 <- ci_gr_qp_5_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_1 <- ci_gr_nb_5_99_1[1:250,]

ci_b0_99_nb_5_1 <- ci_gr_nb_5_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_1 <- ci_gr_el_5_99_1[1:250,]

ci_b0_99_el_5_1 <- ci_gr_el_5_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_5_1, ci_b0_99_qp_5_1, ci_b0_99_nb_5_1, ci_b0_99_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n5_t1_99.png")




ci_gr_pois_6_99_1 <- ci_gr_pois_6_99_1[1:250,]

ci_b0_99_p_6_1 <- ci_gr_pois_6_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_1 <- ci_gr_qp_6_99_1[1:250,]

ci_b0_99_qp_6_1 <- ci_gr_qp_6_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_1 <- ci_gr_nb_6_99_1[1:250,]

ci_b0_99_nb_6_1 <- ci_gr_nb_6_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_1 <- ci_gr_el_6_99_1[1:250,]

ci_b0_99_el_6_1 <- ci_gr_el_6_99_1 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_6_1, ci_b0_99_qp_6_1, ci_b0_99_nb_6_1, ci_b0_99_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n6_t1_99.png")


################

ci_gr_pois_1_99_2 <- ci_gr_pois_1_99_2[1:250,]

ci_b0_99_p_1_2 <- ci_gr_pois_1_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_2 <- ci_gr_qp_1_99_2[1:250,]

ci_b0_99_qp_1_2 <- ci_gr_qp_1_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_2 <- ci_gr_nb_1_99_2[1:250,]

ci_b0_99_nb_1_2 <- ci_gr_nb_1_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_2 <- ci_gr_el_1_99_2[1:250,]

ci_b0_99_el_1_2 <- ci_gr_el_1_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_1_1, ci_b0_99_qp_1_1, ci_b0_99_nb_1_1, ci_b0_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n1_t2_99.png")

# 

ci_gr_pois_2_99_2 <- ci_gr_pois_2_99_2[1:250,]

ci_b0_99_p_2_2 <- ci_gr_pois_2_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_2 <- ci_gr_qp_2_99_2[1:250,]

ci_b0_99_qp_2_2 <- ci_gr_qp_2_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_2 <- ci_gr_nb_2_99_2[1:250,]

ci_b0_99_nb_2_2 <- ci_gr_nb_2_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_2 <- ci_gr_el_2_99_2[1:250,]

ci_b0_99_el_2_2 <- ci_gr_el_2_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_2_2, ci_b0_99_qp_2_2, ci_b0_99_nb_2_2, ci_b0_99_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n2_t2_99.png")


# 

ci_gr_pois_3_99_2 <- ci_gr_pois_3_99_2[1:250,]

ci_b0_99_p_3_2 <- ci_gr_pois_3_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_2 <- ci_gr_qp_3_99_2[1:250,]

ci_b0_99_qp_3_2 <- ci_gr_qp_3_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_2 <- ci_gr_nb_3_99_2[1:250,]

ci_b0_99_nb_3_2 <- ci_gr_nb_3_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_2 <- ci_gr_el_3_99_2[1:250,]

ci_b0_99_el_3_2 <- ci_gr_el_3_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_3_2, ci_b0_99_qp_3_2, ci_b0_99_nb_3_2, ci_b0_99_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n3_t2_99.png")

# 

ci_gr_pois_4_99_2 <- ci_gr_pois_4_99_2[1:250,]

ci_b0_99_p_4_2 <- ci_gr_pois_4_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_2 <- ci_gr_qp_4_99_2[1:250,]

ci_b0_99_qp_4_2 <- ci_gr_qp_4_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_2 <- ci_gr_nb_4_99_2[1:250,]

ci_b0_99_nb_4_2 <- ci_gr_nb_4_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_2 <- ci_gr_el_4_99_2[1:250,]

ci_b0_99_el_4_2 <- ci_gr_el_4_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_4_2, ci_b0_99_qp_4_2, ci_b0_99_nb_4_2, ci_b0_99_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n4_t2_99.png")



ci_gr_pois_5_99_2 <- ci_gr_pois_5_99_2[1:250,]

ci_b0_99_p_5_2 <- ci_gr_pois_5_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_2 <- ci_gr_qp_5_99_2[1:250,]

ci_b0_99_qp_5_2 <- ci_gr_qp_5_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_2 <- ci_gr_nb_5_99_2[1:250,]

ci_b0_99_nb_5_2 <- ci_gr_nb_5_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_2 <- ci_gr_el_5_99_2[1:250,]

ci_b0_99_el_5_2 <- ci_gr_el_5_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_5_2, ci_b0_99_qp_5_2, ci_b0_99_nb_5_2, ci_b0_99_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n5_t2_99.png")




ci_gr_pois_6_99_2 <- ci_gr_pois_6_99_2[1:250,]

ci_b0_99_p_6_2 <- ci_gr_pois_6_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_2 <- ci_gr_qp_6_99_2[1:250,]

ci_b0_99_qp_6_2 <- ci_gr_qp_6_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_2 <- ci_gr_nb_6_99_2[1:250,]

ci_b0_99_nb_6_2 <- ci_gr_nb_6_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_2 <- ci_gr_el_6_99_2[1:250,]

ci_b0_99_el_6_2 <- ci_gr_el_6_99_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_99_p_6_2, ci_b0_99_qp_6_2, ci_b0_99_nb_6_2, ci_b0_99_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n6_t2_99.png")







ci_gr_pois_1_95_2 <- ci_gr_pois_1_95_2[1:250,]

ci_b0_95_p_1_2 <- ci_gr_pois_1_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_2 <- ci_gr_qp_1_95_2[1:250,]

ci_b0_95_qp_1_2 <- ci_gr_qp_1_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_2 <- ci_gr_nb_1_95_2[1:250,]

ci_b0_95_nb_1_2 <- ci_gr_nb_1_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_2 <- ci_gr_el_1_95_2[1:250,]

ci_b0_95_el_1_2 <- ci_gr_el_1_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_1_2, ci_b0_95_qp_1_2, ci_b0_95_nb_1_2, ci_b0_95_el_1_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n1_t2_95.png")

# 

ci_gr_pois_2_95_2 <- ci_gr_pois_2_95_2[1:250,]

ci_b0_95_p_2_2 <- ci_gr_pois_2_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_2 <- ci_gr_qp_2_95_2[1:250,]

ci_b0_95_qp_2_2 <- ci_gr_qp_2_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_2 <- ci_gr_nb_2_95_2[1:250,]

ci_b0_95_nb_2_2 <- ci_gr_nb_2_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_2 <- ci_gr_el_2_95_2[1:250,]

ci_b0_95_el_2_2 <- ci_gr_el_2_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_2_2, ci_b0_95_qp_2_2, ci_b0_95_nb_2_2, ci_b0_95_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n2_t2_95.png")


# 

ci_gr_pois_3_95_2 <- ci_gr_pois_3_95_2[1:250,]

ci_b0_95_p_3_2 <- ci_gr_pois_3_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_2 <- ci_gr_qp_3_95_2[1:250,]

ci_b0_95_qp_3_2 <- ci_gr_qp_3_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_2 <- ci_gr_nb_3_95_2[1:250,]

ci_b0_95_nb_3_2 <- ci_gr_nb_3_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_2 <- ci_gr_el_3_95_2[1:250,]

ci_b0_95_el_3_2 <- ci_gr_el_3_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_3_2, ci_b0_95_qp_3_2, ci_b0_95_nb_3_2, ci_b0_95_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n3_t2_95.png")

# 

ci_gr_pois_4_95_2 <- ci_gr_pois_4_95_2[1:250,]

ci_b0_95_p_4_2 <- ci_gr_pois_4_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_2 <- ci_gr_qp_4_95_2[1:250,]

ci_b0_95_qp_4_2 <- ci_gr_qp_4_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_2 <- ci_gr_nb_4_95_2[1:250,]

ci_b0_95_nb_4_2 <- ci_gr_nb_4_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_2 <- ci_gr_el_4_95_2[1:250,]

ci_b0_95_el_4_2 <- ci_gr_el_4_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_4_2, ci_b0_95_qp_4_2, ci_b0_95_nb_4_2, ci_b0_95_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n4_t2_95.png")



ci_gr_pois_5_95_2 <- ci_gr_pois_5_95_2[1:250,]

ci_b0_95_p_5_2 <- ci_gr_pois_5_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_2 <- ci_gr_qp_5_95_2[1:250,]

ci_b0_95_qp_5_2 <- ci_gr_qp_5_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_2 <- ci_gr_nb_5_95_2[1:250,]

ci_b0_95_nb_5_2 <- ci_gr_nb_5_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_2 <- ci_gr_el_5_95_2[1:250,]

ci_b0_95_el_5_2 <- ci_gr_el_5_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_5_2, ci_b0_95_qp_5_2, ci_b0_95_nb_5_2, ci_b0_95_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n5_t2_95.png")




ci_gr_pois_6_95_2 <- ci_gr_pois_6_95_2[1:250,]

ci_b0_95_p_6_2 <- ci_gr_pois_6_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_2 <- ci_gr_qp_6_95_2[1:250,]

ci_b0_95_qp_6_2 <- ci_gr_qp_6_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_2 <- ci_gr_nb_6_95_2[1:250,]

ci_b0_95_nb_6_2 <- ci_gr_nb_6_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_2 <- ci_gr_el_6_95_2[1:250,]

ci_b0_95_el_6_2 <- ci_gr_el_6_95_2 %>%
  select(c(b0_low, b0_upper, b0_nov, b0_cov)) %>%
  mutate(b0_cov = as.factor(b0_cov))%>%
  ggplot(aes(x = 1 : 250, y = b0_nov))+
  geom_point(aes(color = b0_cov))+
  geom_errorbar(aes(ymin = b0_low, ymax = b0_upper, color = b0_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b0, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b0_95_p_6_2, ci_b0_95_qp_6_2, ci_b0_95_nb_6_2, ci_b0_95_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b0_n6_t2_95.png")










####################



ci_gr_pois_1_95_1 <- ci_gr_pois_1_95_1[1:250,]

ci_b1_95_p_1_1 <- ci_gr_pois_1_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_1 <- ci_gr_qp_1_95_1[1:250,]

ci_b1_95_qp_1_1 <- ci_gr_qp_1_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_1 <- ci_gr_nb_1_95_1[1:250,]

ci_b1_95_nb_1_1 <- ci_gr_nb_1_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_1 <- ci_gr_el_1_95_1[1:250,]

ci_b1_95_el_1_1 <- ci_gr_el_1_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_1_1, ci_b1_95_qp_1_1, ci_b1_95_nb_1_1, ci_b1_95_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n1_t1_95.png")

# 

ci_gr_pois_2_95_1 <- ci_gr_pois_2_95_1[1:250,]

ci_b1_95_p_2_1 <- ci_gr_pois_2_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_1 <- ci_gr_qp_2_95_1[1:250,]

ci_b1_95_qp_2_1 <- ci_gr_qp_2_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_1 <- ci_gr_nb_2_95_1[1:250,]

ci_b1_95_nb_2_1 <- ci_gr_nb_2_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_1 <- ci_gr_el_2_95_1[1:250,]

ci_b1_95_el_2_1 <- ci_gr_el_2_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_2_1, ci_b1_95_qp_2_1, ci_b1_95_nb_2_1, ci_b1_95_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n2_t1_95.png")


# 

ci_gr_pois_3_95_1 <- ci_gr_pois_3_95_1[1:250,]

ci_b1_95_p_3_1 <- ci_gr_pois_3_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_1 <- ci_gr_qp_3_95_1[1:250,]

ci_b1_95_qp_3_1 <- ci_gr_qp_3_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_1 <- ci_gr_nb_3_95_1[1:250,]

ci_b1_95_nb_3_1 <- ci_gr_nb_3_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_1 <- ci_gr_el_3_95_1[1:250,]

ci_b1_95_el_3_1 <- ci_gr_el_3_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_3_1, ci_b1_95_qp_3_1, ci_b1_95_nb_3_1, ci_b1_95_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n3_t1_95.png")

# 

ci_gr_pois_4_95_1 <- ci_gr_pois_4_95_1[1:250,]

ci_b1_95_p_4_1 <- ci_gr_pois_4_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_1 <- ci_gr_qp_4_95_1[1:250,]

ci_b1_95_qp_4_1 <- ci_gr_qp_4_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_1 <- ci_gr_nb_4_95_1[1:250,]

ci_b1_95_nb_4_1 <- ci_gr_nb_4_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_1 <- ci_gr_el_4_95_1[1:250,]

ci_b1_95_el_4_1 <- ci_gr_el_4_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_4_1, ci_b1_95_qp_4_1, ci_b1_95_nb_4_1, ci_b1_95_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n4_t1_95.png")



ci_gr_pois_5_95_1 <- ci_gr_pois_5_95_1[1:250,]

ci_b1_95_p_5_1 <- ci_gr_pois_5_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_1 <- ci_gr_qp_5_95_1[1:250,]

ci_b1_95_qp_5_1 <- ci_gr_qp_5_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_1 <- ci_gr_nb_5_95_1[1:250,]

ci_b1_95_nb_5_1 <- ci_gr_nb_5_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_1 <- ci_gr_el_5_95_1[1:250,]

ci_b1_95_el_5_1 <- ci_gr_el_5_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_5_1, ci_b1_95_qp_5_1, ci_b1_95_nb_5_1, ci_b1_95_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n5_t1_95.png")




ci_gr_pois_6_95_1 <- ci_gr_pois_6_95_1[1:250,]

ci_b1_95_p_6_1 <- ci_gr_pois_6_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_1 <- ci_gr_qp_6_95_1[1:250,]

ci_b1_95_qp_6_1 <- ci_gr_qp_6_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_1 <- ci_gr_nb_6_95_1[1:250,]

ci_b1_95_nb_6_1 <- ci_gr_nb_6_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_1 <- ci_gr_el_6_95_1[1:250,]

ci_b1_95_el_6_1 <- ci_gr_el_6_95_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_6_1, ci_b1_95_qp_6_1, ci_b1_95_nb_6_1, ci_b1_95_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n6_t1_95.png")

#


ci_gr_pois_1_99_1 <- ci_gr_pois_1_99_1[1:250,]

ci_b1_99_p_1_1 <- ci_gr_pois_1_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_1 <- ci_gr_qp_1_99_1[1:250,]

ci_b1_99_qp_1_1 <- ci_gr_qp_1_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_1 <- ci_gr_nb_1_99_1[1:250,]

ci_b1_99_nb_1_1 <- ci_gr_nb_1_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_1 <- ci_gr_el_1_99_1[1:250,]

ci_b1_99_el_1_1 <- ci_gr_el_1_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_1_1, ci_b1_99_qp_1_1, ci_b1_99_nb_1_1, ci_b1_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n1_t1_99.png")

# 

ci_gr_pois_2_99_1 <- ci_gr_pois_2_99_1[1:250,]

ci_b1_99_p_2_1 <- ci_gr_pois_2_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_1 <- ci_gr_qp_2_99_1[1:250,]

ci_b1_99_qp_2_1 <- ci_gr_qp_2_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_1 <- ci_gr_nb_2_99_1[1:250,]

ci_b1_99_nb_2_1 <- ci_gr_nb_2_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_1 <- ci_gr_el_2_99_1[1:250,]

ci_b1_99_el_2_1 <- ci_gr_el_2_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_2_1, ci_b1_99_qp_2_1, ci_b1_99_nb_2_1, ci_b1_99_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n2_t1_99.png")


# 

ci_gr_pois_3_99_1 <- ci_gr_pois_3_99_1[1:250,]

ci_b1_99_p_3_1 <- ci_gr_pois_3_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_1 <- ci_gr_qp_3_99_1[1:250,]

ci_b1_99_qp_3_1 <- ci_gr_qp_3_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_1 <- ci_gr_nb_3_99_1[1:250,]

ci_b1_99_nb_3_1 <- ci_gr_nb_3_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_1 <- ci_gr_el_3_99_1[1:250,]

ci_b1_99_el_3_1 <- ci_gr_el_3_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_3_1, ci_b1_99_qp_3_1, ci_b1_99_nb_3_1, ci_b1_99_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n3_t1_99.png")

# 

ci_gr_pois_4_99_1 <- ci_gr_pois_4_99_1[1:250,]

ci_b1_99_p_4_1 <- ci_gr_pois_4_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_1 <- ci_gr_qp_4_99_1[1:250,]

ci_b1_99_qp_4_1 <- ci_gr_qp_4_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_1 <- ci_gr_nb_4_99_1[1:250,]

ci_b1_99_nb_4_1 <- ci_gr_nb_4_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_1 <- ci_gr_el_4_99_1[1:250,]

ci_b1_99_el_4_1 <- ci_gr_el_4_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_4_1, ci_b1_99_qp_4_1, ci_b1_99_nb_4_1, ci_b1_99_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n4_t1_99.png")



ci_gr_pois_5_99_1 <- ci_gr_pois_5_99_1[1:250,]

ci_b1_99_p_5_1 <- ci_gr_pois_5_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_1 <- ci_gr_qp_5_99_1[1:250,]

ci_b1_99_qp_5_1 <- ci_gr_qp_5_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_1 <- ci_gr_nb_5_99_1[1:250,]

ci_b1_99_nb_5_1 <- ci_gr_nb_5_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_1 <- ci_gr_el_5_99_1[1:250,]

ci_b1_99_el_5_1 <- ci_gr_el_5_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_5_1, ci_b1_99_qp_5_1, ci_b1_99_nb_5_1, ci_b1_99_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n5_t1_99.png")




ci_gr_pois_6_99_1 <- ci_gr_pois_6_99_1[1:250,]

ci_b1_99_p_6_1 <- ci_gr_pois_6_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_1 <- ci_gr_qp_6_99_1[1:250,]

ci_b1_99_qp_6_1 <- ci_gr_qp_6_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_1 <- ci_gr_nb_6_99_1[1:250,]

ci_b1_99_nb_6_1 <- ci_gr_nb_6_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_1 <- ci_gr_el_6_99_1[1:250,]

ci_b1_99_el_6_1 <- ci_gr_el_6_99_1 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_6_1, ci_b1_99_qp_6_1, ci_b1_99_nb_6_1, ci_b1_99_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n6_t1_99.png")


################

ci_gr_pois_1_99_2 <- ci_gr_pois_1_99_2[1:250,]

ci_b1_99_p_1_2 <- ci_gr_pois_1_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_2 <- ci_gr_qp_1_99_2[1:250,]

ci_b1_99_qp_1_2 <- ci_gr_qp_1_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_2 <- ci_gr_nb_1_99_2[1:250,]

ci_b1_99_nb_1_2 <- ci_gr_nb_1_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_2 <- ci_gr_el_1_99_2[1:250,]

ci_b1_99_el_1_2 <- ci_gr_el_1_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_1_1, ci_b1_99_qp_1_1, ci_b1_99_nb_1_1, ci_b1_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n1_t2_99.png")

# 

ci_gr_pois_2_99_2 <- ci_gr_pois_2_99_2[1:250,]

ci_b1_99_p_2_2 <- ci_gr_pois_2_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_2 <- ci_gr_qp_2_99_2[1:250,]

ci_b1_99_qp_2_2 <- ci_gr_qp_2_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_2 <- ci_gr_nb_2_99_2[1:250,]

ci_b1_99_nb_2_2 <- ci_gr_nb_2_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_2 <- ci_gr_el_2_99_2[1:250,]

ci_b1_99_el_2_2 <- ci_gr_el_2_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_2_2, ci_b1_99_qp_2_2, ci_b1_99_nb_2_2, ci_b1_99_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n2_t2_99.png")


# 

ci_gr_pois_3_99_2 <- ci_gr_pois_3_99_2[1:250,]

ci_b1_99_p_3_2 <- ci_gr_pois_3_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_2 <- ci_gr_qp_3_99_2[1:250,]

ci_b1_99_qp_3_2 <- ci_gr_qp_3_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_2 <- ci_gr_nb_3_99_2[1:250,]

ci_b1_99_nb_3_2 <- ci_gr_nb_3_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_2 <- ci_gr_el_3_99_2[1:250,]

ci_b1_99_el_3_2 <- ci_gr_el_3_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_3_2, ci_b1_99_qp_3_2, ci_b1_99_nb_3_2, ci_b1_99_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n3_t2_99.png")

# 

ci_gr_pois_4_99_2 <- ci_gr_pois_4_99_2[1:250,]

ci_b1_99_p_4_2 <- ci_gr_pois_4_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_2 <- ci_gr_qp_4_99_2[1:250,]

ci_b1_99_qp_4_2 <- ci_gr_qp_4_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_2 <- ci_gr_nb_4_99_2[1:250,]

ci_b1_99_nb_4_2 <- ci_gr_nb_4_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_2 <- ci_gr_el_4_99_2[1:250,]

ci_b1_99_el_4_2 <- ci_gr_el_4_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_4_2, ci_b1_99_qp_4_2, ci_b1_99_nb_4_2, ci_b1_99_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n4_t2_99.png")



ci_gr_pois_5_99_2 <- ci_gr_pois_5_99_2[1:250,]

ci_b1_99_p_5_2 <- ci_gr_pois_5_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_2 <- ci_gr_qp_5_99_2[1:250,]

ci_b1_99_qp_5_2 <- ci_gr_qp_5_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_2 <- ci_gr_nb_5_99_2[1:250,]

ci_b1_99_nb_5_2 <- ci_gr_nb_5_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_2 <- ci_gr_el_5_99_2[1:250,]

ci_b1_99_el_5_2 <- ci_gr_el_5_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_5_2, ci_b1_99_qp_5_2, ci_b1_99_nb_5_2, ci_b1_99_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n5_t2_99.png")




ci_gr_pois_6_99_2 <- ci_gr_pois_6_99_2[1:250,]

ci_b1_99_p_6_2 <- ci_gr_pois_6_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_2 <- ci_gr_qp_6_99_2[1:250,]

ci_b1_99_qp_6_2 <- ci_gr_qp_6_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_2 <- ci_gr_nb_6_99_2[1:250,]

ci_b1_99_nb_6_2 <- ci_gr_nb_6_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_2 <- ci_gr_el_6_99_2[1:250,]

ci_b1_99_el_6_2 <- ci_gr_el_6_99_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_99_p_6_2, ci_b1_99_qp_6_2, ci_b1_99_nb_6_2, ci_b1_99_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n6_t2_99.png")







ci_gr_pois_1_95_2 <- ci_gr_pois_1_95_2[1:250,]

ci_b1_95_p_1_2 <- ci_gr_pois_1_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_2 <- ci_gr_qp_1_95_2[1:250,]

ci_b1_95_qp_1_2 <- ci_gr_qp_1_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_2 <- ci_gr_nb_1_95_2[1:250,]

ci_b1_95_nb_1_2 <- ci_gr_nb_1_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_2 <- ci_gr_el_1_95_2[1:250,]

ci_b1_95_el_1_2 <- ci_gr_el_1_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_1_2, ci_b1_95_qp_1_2, ci_b1_95_nb_1_2, ci_b1_95_el_1_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n1_t2_95.png")

# 

ci_gr_pois_2_95_2 <- ci_gr_pois_2_95_2[1:250,]

ci_b1_95_p_2_2 <- ci_gr_pois_2_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_2 <- ci_gr_qp_2_95_2[1:250,]

ci_b1_95_qp_2_2 <- ci_gr_qp_2_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_2 <- ci_gr_nb_2_95_2[1:250,]

ci_b1_95_nb_2_2 <- ci_gr_nb_2_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_2 <- ci_gr_el_2_95_2[1:250,]

ci_b1_95_el_2_2 <- ci_gr_el_2_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_2_2, ci_b1_95_qp_2_2, ci_b1_95_nb_2_2, ci_b1_95_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n2_t2_95.png")


# 

ci_gr_pois_3_95_2 <- ci_gr_pois_3_95_2[1:250,]

ci_b1_95_p_3_2 <- ci_gr_pois_3_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_2 <- ci_gr_qp_3_95_2[1:250,]

ci_b1_95_qp_3_2 <- ci_gr_qp_3_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_2 <- ci_gr_nb_3_95_2[1:250,]

ci_b1_95_nb_3_2 <- ci_gr_nb_3_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_2 <- ci_gr_el_3_95_2[1:250,]

ci_b1_95_el_3_2 <- ci_gr_el_3_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_3_2, ci_b1_95_qp_3_2, ci_b1_95_nb_3_2, ci_b1_95_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n3_t2_95.png")

# 

ci_gr_pois_4_95_2 <- ci_gr_pois_4_95_2[1:250,]

ci_b1_95_p_4_2 <- ci_gr_pois_4_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_2 <- ci_gr_qp_4_95_2[1:250,]

ci_b1_95_qp_4_2 <- ci_gr_qp_4_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_2 <- ci_gr_nb_4_95_2[1:250,]

ci_b1_95_nb_4_2 <- ci_gr_nb_4_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_2 <- ci_gr_el_4_95_2[1:250,]

ci_b1_95_el_4_2 <- ci_gr_el_4_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_4_2, ci_b1_95_qp_4_2, ci_b1_95_nb_4_2, ci_b1_95_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n4_t2_95.png")



ci_gr_pois_5_95_2 <- ci_gr_pois_5_95_2[1:250,]

ci_b1_95_p_5_2 <- ci_gr_pois_5_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_2 <- ci_gr_qp_5_95_2[1:250,]

ci_b1_95_qp_5_2 <- ci_gr_qp_5_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_2 <- ci_gr_nb_5_95_2[1:250,]

ci_b1_95_nb_5_2 <- ci_gr_nb_5_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_2 <- ci_gr_el_5_95_2[1:250,]

ci_b1_95_el_5_2 <- ci_gr_el_5_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_5_2, ci_b1_95_qp_5_2, ci_b1_95_nb_5_2, ci_b1_95_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n5_t2_95.png")




ci_gr_pois_6_95_2 <- ci_gr_pois_6_95_2[1:250,]

ci_b1_95_p_6_2 <- ci_gr_pois_6_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_2 <- ci_gr_qp_6_95_2[1:250,]

ci_b1_95_qp_6_2 <- ci_gr_qp_6_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_2 <- ci_gr_nb_6_95_2[1:250,]

ci_b1_95_nb_6_2 <- ci_gr_nb_6_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_2 <- ci_gr_el_6_95_2[1:250,]

ci_b1_95_el_6_2 <- ci_gr_el_6_95_2 %>%
  select(c(b1_low, b1_upper, b1_nov, b1_cov)) %>%
  mutate(b1_cov = as.factor(b1_cov))%>%
  ggplot(aes(x = 1 : 250, y = b1_nov))+
  geom_point(aes(color = b1_cov))+
  geom_errorbar(aes(ymin = b1_low, ymax = b1_upper, color = b1_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b1, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b1_95_p_6_2, ci_b1_95_qp_6_2, ci_b1_95_nb_6_2, ci_b1_95_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b1_n6_t2_95.png")


###############

ci_gr_pois_1_95_1 <- ci_gr_pois_1_95_1[1:250,]

ci_b2_95_p_1_1 <- ci_gr_pois_1_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_1 <- ci_gr_qp_1_95_1[1:250,]

ci_b2_95_qp_1_1 <- ci_gr_qp_1_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_1 <- ci_gr_nb_1_95_1[1:250,]

ci_b2_95_nb_1_1 <- ci_gr_nb_1_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_1 <- ci_gr_el_1_95_1[1:250,]

ci_b2_95_el_1_1 <- ci_gr_el_1_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_1_1, ci_b2_95_qp_1_1, ci_b2_95_nb_1_1, ci_b2_95_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n1_t1_95.png")

# 

ci_gr_pois_2_95_1 <- ci_gr_pois_2_95_1[1:250,]

ci_b2_95_p_2_1 <- ci_gr_pois_2_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_1 <- ci_gr_qp_2_95_1[1:250,]

ci_b2_95_qp_2_1 <- ci_gr_qp_2_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_1 <- ci_gr_nb_2_95_1[1:250,]

ci_b2_95_nb_2_1 <- ci_gr_nb_2_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_1 <- ci_gr_el_2_95_1[1:250,]

ci_b2_95_el_2_1 <- ci_gr_el_2_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_2_1, ci_b2_95_qp_2_1, ci_b2_95_nb_2_1, ci_b2_95_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n2_t1_95.png")


# 

ci_gr_pois_3_95_1 <- ci_gr_pois_3_95_1[1:250,]

ci_b2_95_p_3_1 <- ci_gr_pois_3_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_1 <- ci_gr_qp_3_95_1[1:250,]

ci_b2_95_qp_3_1 <- ci_gr_qp_3_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_1 <- ci_gr_nb_3_95_1[1:250,]

ci_b2_95_nb_3_1 <- ci_gr_nb_3_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_1 <- ci_gr_el_3_95_1[1:250,]

ci_b2_95_el_3_1 <- ci_gr_el_3_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_3_1, ci_b2_95_qp_3_1, ci_b2_95_nb_3_1, ci_b2_95_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n3_t1_95.png")

# 

ci_gr_pois_4_95_1 <- ci_gr_pois_4_95_1[1:250,]

ci_b2_95_p_4_1 <- ci_gr_pois_4_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_1 <- ci_gr_qp_4_95_1[1:250,]

ci_b2_95_qp_4_1 <- ci_gr_qp_4_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_1 <- ci_gr_nb_4_95_1[1:250,]

ci_b2_95_nb_4_1 <- ci_gr_nb_4_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_1 <- ci_gr_el_4_95_1[1:250,]

ci_b2_95_el_4_1 <- ci_gr_el_4_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_4_1, ci_b2_95_qp_4_1, ci_b2_95_nb_4_1, ci_b2_95_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n4_t1_95.png")



ci_gr_pois_5_95_1 <- ci_gr_pois_5_95_1[1:250,]

ci_b2_95_p_5_1 <- ci_gr_pois_5_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_1 <- ci_gr_qp_5_95_1[1:250,]

ci_b2_95_qp_5_1 <- ci_gr_qp_5_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_1 <- ci_gr_nb_5_95_1[1:250,]

ci_b2_95_nb_5_1 <- ci_gr_nb_5_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_1 <- ci_gr_el_5_95_1[1:250,]

ci_b2_95_el_5_1 <- ci_gr_el_5_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_5_1, ci_b2_95_qp_5_1, ci_b2_95_nb_5_1, ci_b2_95_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n5_t1_95.png")




ci_gr_pois_6_95_1 <- ci_gr_pois_6_95_1[1:250,]

ci_b2_95_p_6_1 <- ci_gr_pois_6_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_1 <- ci_gr_qp_6_95_1[1:250,]

ci_b2_95_qp_6_1 <- ci_gr_qp_6_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_1 <- ci_gr_nb_6_95_1[1:250,]

ci_b2_95_nb_6_1 <- ci_gr_nb_6_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_1 <- ci_gr_el_6_95_1[1:250,]

ci_b2_95_el_6_1 <- ci_gr_el_6_95_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_6_1, ci_b2_95_qp_6_1, ci_b2_95_nb_6_1, ci_b2_95_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n6_t1_95.png")

#


ci_gr_pois_1_99_1 <- ci_gr_pois_1_99_1[1:250,]

ci_b2_99_p_1_1 <- ci_gr_pois_1_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_1 <- ci_gr_qp_1_99_1[1:250,]

ci_b2_99_qp_1_1 <- ci_gr_qp_1_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_1 <- ci_gr_nb_1_99_1[1:250,]

ci_b2_99_nb_1_1 <- ci_gr_nb_1_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_1 <- ci_gr_el_1_99_1[1:250,]

ci_b2_99_el_1_1 <- ci_gr_el_1_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_1_1, ci_b2_99_qp_1_1, ci_b2_99_nb_1_1, ci_b2_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n1_t1_99.png")

# 

ci_gr_pois_2_99_1 <- ci_gr_pois_2_99_1[1:250,]

ci_b2_99_p_2_1 <- ci_gr_pois_2_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_1 <- ci_gr_qp_2_99_1[1:250,]

ci_b2_99_qp_2_1 <- ci_gr_qp_2_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_1 <- ci_gr_nb_2_99_1[1:250,]

ci_b2_99_nb_2_1 <- ci_gr_nb_2_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_1 <- ci_gr_el_2_99_1[1:250,]

ci_b2_99_el_2_1 <- ci_gr_el_2_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_2_1, ci_b2_99_qp_2_1, ci_b2_99_nb_2_1, ci_b2_99_el_2_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n2_t1_99.png")


# 

ci_gr_pois_3_99_1 <- ci_gr_pois_3_99_1[1:250,]

ci_b2_99_p_3_1 <- ci_gr_pois_3_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_1 <- ci_gr_qp_3_99_1[1:250,]

ci_b2_99_qp_3_1 <- ci_gr_qp_3_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_1 <- ci_gr_nb_3_99_1[1:250,]

ci_b2_99_nb_3_1 <- ci_gr_nb_3_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_1 <- ci_gr_el_3_99_1[1:250,]

ci_b2_99_el_3_1 <- ci_gr_el_3_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_3_1, ci_b2_99_qp_3_1, ci_b2_99_nb_3_1, ci_b2_99_el_3_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n3_t1_99.png")

# 

ci_gr_pois_4_99_1 <- ci_gr_pois_4_99_1[1:250,]

ci_b2_99_p_4_1 <- ci_gr_pois_4_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_1 <- ci_gr_qp_4_99_1[1:250,]

ci_b2_99_qp_4_1 <- ci_gr_qp_4_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_1 <- ci_gr_nb_4_99_1[1:250,]

ci_b2_99_nb_4_1 <- ci_gr_nb_4_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_1 <- ci_gr_el_4_99_1[1:250,]

ci_b2_99_el_4_1 <- ci_gr_el_4_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_4_1, ci_b2_99_qp_4_1, ci_b2_99_nb_4_1, ci_b2_99_el_4_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n4_t1_99.png")



ci_gr_pois_5_99_1 <- ci_gr_pois_5_99_1[1:250,]

ci_b2_99_p_5_1 <- ci_gr_pois_5_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_1 <- ci_gr_qp_5_99_1[1:250,]

ci_b2_99_qp_5_1 <- ci_gr_qp_5_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_1 <- ci_gr_nb_5_99_1[1:250,]

ci_b2_99_nb_5_1 <- ci_gr_nb_5_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_1 <- ci_gr_el_5_99_1[1:250,]

ci_b2_99_el_5_1 <- ci_gr_el_5_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_5_1, ci_b2_99_qp_5_1, ci_b2_99_nb_5_1, ci_b2_99_el_5_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n5_t1_99.png")




ci_gr_pois_6_99_1 <- ci_gr_pois_6_99_1[1:250,]

ci_b2_99_p_6_1 <- ci_gr_pois_6_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_1 <- ci_gr_qp_6_99_1[1:250,]

ci_b2_99_qp_6_1 <- ci_gr_qp_6_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_1 <- ci_gr_nb_6_99_1[1:250,]

ci_b2_99_nb_6_1 <- ci_gr_nb_6_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_1 <- ci_gr_el_6_99_1[1:250,]

ci_b2_99_el_6_1 <- ci_gr_el_6_99_1 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_6_1, ci_b2_99_qp_6_1, ci_b2_99_nb_6_1, ci_b2_99_el_6_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n6_t1_99.png")


################

ci_gr_pois_1_99_2 <- ci_gr_pois_1_99_2[1:250,]

ci_b2_99_p_1_2 <- ci_gr_pois_1_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_99_2 <- ci_gr_qp_1_99_2[1:250,]

ci_b2_99_qp_1_2 <- ci_gr_qp_1_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_99_2 <- ci_gr_nb_1_99_2[1:250,]

ci_b2_99_nb_1_2 <- ci_gr_nb_1_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_99_2 <- ci_gr_el_1_99_2[1:250,]

ci_b2_99_el_1_2 <- ci_gr_el_1_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_1_1, ci_b2_99_qp_1_1, ci_b2_99_nb_1_1, ci_b2_99_el_1_1, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n1_t2_99.png")

# 

ci_gr_pois_2_99_2 <- ci_gr_pois_2_99_2[1:250,]

ci_b2_99_p_2_2 <- ci_gr_pois_2_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_99_2 <- ci_gr_qp_2_99_2[1:250,]

ci_b2_99_qp_2_2 <- ci_gr_qp_2_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_99_2 <- ci_gr_nb_2_99_2[1:250,]

ci_b2_99_nb_2_2 <- ci_gr_nb_2_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_99_2 <- ci_gr_el_2_99_2[1:250,]

ci_b2_99_el_2_2 <- ci_gr_el_2_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_2_2, ci_b2_99_qp_2_2, ci_b2_99_nb_2_2, ci_b2_99_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n2_t2_99.png")


# 

ci_gr_pois_3_99_2 <- ci_gr_pois_3_99_2[1:250,]

ci_b2_99_p_3_2 <- ci_gr_pois_3_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_99_2 <- ci_gr_qp_3_99_2[1:250,]

ci_b2_99_qp_3_2 <- ci_gr_qp_3_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_99_2 <- ci_gr_nb_3_99_2[1:250,]

ci_b2_99_nb_3_2 <- ci_gr_nb_3_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_99_2 <- ci_gr_el_3_99_2[1:250,]

ci_b2_99_el_3_2 <- ci_gr_el_3_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_3_2, ci_b2_99_qp_3_2, ci_b2_99_nb_3_2, ci_b2_99_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n3_t2_99.png")

# 

ci_gr_pois_4_99_2 <- ci_gr_pois_4_99_2[1:250,]

ci_b2_99_p_4_2 <- ci_gr_pois_4_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_99_2 <- ci_gr_qp_4_99_2[1:250,]

ci_b2_99_qp_4_2 <- ci_gr_qp_4_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_99_2 <- ci_gr_nb_4_99_2[1:250,]

ci_b2_99_nb_4_2 <- ci_gr_nb_4_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_99_2 <- ci_gr_el_4_99_2[1:250,]

ci_b2_99_el_4_2 <- ci_gr_el_4_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_4_2, ci_b2_99_qp_4_2, ci_b2_99_nb_4_2, ci_b2_99_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n4_t2_99.png")



ci_gr_pois_5_99_2 <- ci_gr_pois_5_99_2[1:250,]

ci_b2_99_p_5_2 <- ci_gr_pois_5_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_99_2 <- ci_gr_qp_5_99_2[1:250,]

ci_b2_99_qp_5_2 <- ci_gr_qp_5_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_99_2 <- ci_gr_nb_5_99_2[1:250,]

ci_b2_99_nb_5_2 <- ci_gr_nb_5_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_99_2 <- ci_gr_el_5_99_2[1:250,]

ci_b2_99_el_5_2 <- ci_gr_el_5_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_5_2, ci_b2_99_qp_5_2, ci_b2_99_nb_5_2, ci_b2_99_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n5_t2_99.png")




ci_gr_pois_6_99_2 <- ci_gr_pois_6_99_2[1:250,]

ci_b2_99_p_6_2 <- ci_gr_pois_6_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_99_2 <- ci_gr_qp_6_99_2[1:250,]

ci_b2_99_qp_6_2 <- ci_gr_qp_6_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_99_2 <- ci_gr_nb_6_99_2[1:250,]

ci_b2_99_nb_6_2 <- ci_gr_nb_6_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_99_2 <- ci_gr_el_6_99_2[1:250,]

ci_b2_99_el_6_2 <- ci_gr_el_6_99_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_99_p_6_2, ci_b2_99_qp_6_2, ci_b2_99_nb_6_2, ci_b2_99_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n6_t2_99.png")







ci_gr_pois_1_95_2 <- ci_gr_pois_1_95_2[1:250,]

ci_b2_95_p_1_2 <- ci_gr_pois_1_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_1_95_2 <- ci_gr_qp_1_95_2[1:250,]

ci_b2_95_qp_1_2 <- ci_gr_qp_1_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_1_95_2 <- ci_gr_nb_1_95_2[1:250,]

ci_b2_95_nb_1_2 <- ci_gr_nb_1_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_1_95_2 <- ci_gr_el_1_95_2[1:250,]

ci_b2_95_el_1_2 <- ci_gr_el_1_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_1_2, ci_b2_95_qp_1_2, ci_b2_95_nb_1_2, ci_b2_95_el_1_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n1_t2_95.png")

# 

ci_gr_pois_2_95_2 <- ci_gr_pois_2_95_2[1:250,]

ci_b2_95_p_2_2 <- ci_gr_pois_2_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_2_95_2 <- ci_gr_qp_2_95_2[1:250,]

ci_b2_95_qp_2_2 <- ci_gr_qp_2_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_2_95_2 <- ci_gr_nb_2_95_2[1:250,]

ci_b2_95_nb_2_2 <- ci_gr_nb_2_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_2_95_2 <- ci_gr_el_2_95_2[1:250,]

ci_b2_95_el_2_2 <- ci_gr_el_2_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_2_2, ci_b2_95_qp_2_2, ci_b2_95_nb_2_2, ci_b2_95_el_2_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n2_t2_95.png")


# 

ci_gr_pois_3_95_2 <- ci_gr_pois_3_95_2[1:250,]

ci_b2_95_p_3_2 <- ci_gr_pois_3_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_3_95_2 <- ci_gr_qp_3_95_2[1:250,]

ci_b2_95_qp_3_2 <- ci_gr_qp_3_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_3_95_2 <- ci_gr_nb_3_95_2[1:250,]

ci_b2_95_nb_3_2 <- ci_gr_nb_3_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_3_95_2 <- ci_gr_el_3_95_2[1:250,]

ci_b2_95_el_3_2 <- ci_gr_el_3_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_3_2, ci_b2_95_qp_3_2, ci_b2_95_nb_3_2, ci_b2_95_el_3_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n3_t2_95.png")

# 

ci_gr_pois_4_95_2 <- ci_gr_pois_4_95_2[1:250,]

ci_b2_95_p_4_2 <- ci_gr_pois_4_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_4_95_2 <- ci_gr_qp_4_95_2[1:250,]

ci_b2_95_qp_4_2 <- ci_gr_qp_4_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_4_95_2 <- ci_gr_nb_4_95_2[1:250,]

ci_b2_95_nb_4_2 <- ci_gr_nb_4_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_4_95_2 <- ci_gr_el_4_95_2[1:250,]

ci_b2_95_el_4_2 <- ci_gr_el_4_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_4_2, ci_b2_95_qp_4_2, ci_b2_95_nb_4_2, ci_b2_95_el_4_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n4_t2_95.png")



ci_gr_pois_5_95_2 <- ci_gr_pois_5_95_2[1:250,]

ci_b2_95_p_5_2 <- ci_gr_pois_5_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_5_95_2 <- ci_gr_qp_5_95_2[1:250,]

ci_b2_95_qp_5_2 <- ci_gr_qp_5_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_5_95_2 <- ci_gr_nb_5_95_2[1:250,]

ci_b2_95_nb_5_2 <- ci_gr_nb_5_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_5_95_2 <- ci_gr_el_5_95_2[1:250,]

ci_b2_95_el_5_2 <- ci_gr_el_5_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_5_2, ci_b2_95_qp_5_2, ci_b2_95_nb_5_2, ci_b2_95_el_5_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n5_t2_95.png")




ci_gr_pois_6_95_2 <- ci_gr_pois_6_95_2[1:250,]

ci_b2_95_p_6_2 <- ci_gr_pois_6_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_qp_6_95_2 <- ci_gr_qp_6_95_2[1:250,]

ci_b2_95_qp_6_2 <- ci_gr_qp_6_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_nb_6_95_2 <- ci_gr_nb_6_95_2[1:250,]

ci_b2_95_nb_6_2 <- ci_gr_nb_6_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())


ci_gr_el_6_95_2 <- ci_gr_el_6_95_2[1:250,]

ci_b2_95_el_6_2 <- ci_gr_el_6_95_2 %>%
  select(c(b2_low, b2_upper, b2_nov, b2_cov)) %>%
  mutate(b2_cov = as.factor(b2_cov))%>%
  ggplot(aes(x = 1 : 250, y = b2_nov))+
  geom_point(aes(color = b2_cov))+
  geom_errorbar(aes(ymin = b2_low, ymax = b2_upper, color = b2_cov))+
  scale_color_manual(values = colorset) +
  coord_flip() +
  geom_hline(yintercept = b2, color = "blue")+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(ci_b2_95_p_6_2, ci_b2_95_qp_6_2, ci_b2_95_nb_6_2, ci_b2_95_el_6_2, ncol = 4, labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom", common.legend = TRUE)

ggsave("b2_n6_t2_95.png")


# beta theta 1 sad  ----

# n1

est_el_1_1$name <- "EL"
est_nb_1_1$name <- "NB"
est_qp_1_1$name <- "QP"
est_pois_1_1$name <- "P"

beta_estimates_1_1 <- rbind(est_pois_1_1,
                            est_qp_1_1,
                            est_nb_1_1,
                            est_el_1_1)

beta0_n1_sad_t1 <- beta_estimates_1_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+
  ylab(" ")+
  xlim(c(-1,2))+ 
  theme(legend.position="top",
        legend.title=element_blank())


beta1_n1_sad_t1 <- beta_estimates_1_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+
  ylab(" ")+
  xlim(c(-1,2))+ 
  theme(legend.position="top",
        legend.title=element_blank())


beta2_n1_sad_t1 <- beta_estimates_1_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

ggarrange(beta0_n1_sad_t1, beta1_n1_sad_t1, beta2_n1_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)

ggsave("beta_n1_t1.png", scale = 0.7)

# n2

est_el_2_1$name <- "EL"
est_nb_2_1$name <- "NB"
est_qp_2_1$name <- "QP"
est_pois_2_1$name <- "P"

beta_estimates_2_1 <- rbind(est_pois_2_1,
                            est_qp_2_1,
                            est_nb_2_1,
                            est_el_2_1)

beta0_n2_sad_t1 <- beta_estimates_2_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

beta1_n2_sad_t1 <- beta_estimates_2_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

beta2_n2_sad_t1 <- beta_estimates_2_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

ggarrange(beta0_n2_sad_t1, beta1_n2_sad_t1, beta2_n2_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n2_t1.png", scale = 0.7)

# n3

est_el_3_1$name <- "EL"
est_nb_3_1$name <- "NB"
est_qp_3_1$name <- "QP"
est_pois_3_1$name <- "P"

beta_estimates_3_1 <- rbind(est_pois_3_1,
                            est_qp_3_1,
                            est_nb_3_1,
                            est_el_3_1)

beta0_n3_sad_t1 <- beta_estimates_3_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n3_sad_t1 <- beta_estimates_3_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n3_sad_t1 <- beta_estimates_3_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n3_sad_t1, beta1_n3_sad_t1, beta2_n3_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n3_t1.png", scale = 0.7)

# n4

est_el_4_1$name <- "EL"
est_nb_4_1$name <- "NB"
est_qp_4_1$name <- "QP"
est_pois_4_1$name <- "P"

beta_estimates_4_1 <- rbind(est_pois_4_1,
                            est_qp_4_1,
                            est_nb_4_1,
                            est_el_4_1)

beta0_n4_sad_t1 <- beta_estimates_4_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n4_sad_t1 <- beta_estimates_4_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n4_sad_t1 <- beta_estimates_4_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n4_sad_t1, beta1_n4_sad_t1, beta2_n4_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n4_t1.png", scale = 0.7)

# n5

est_el_5_1$name <- "EL"
est_nb_5_1$name <- "NB"
est_qp_5_1$name <- "QP"
est_pois_5_1$name <- "P"

beta_estimates_5_1 <- rbind(est_pois_5_1,
                            est_qp_5_1,
                            est_nb_5_1,
                            est_el_5_1)

beta0_n5_sad_t1 <- beta_estimates_5_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n5_sad_t1 <- beta_estimates_5_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n5_sad_t1 <- beta_estimates_5_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n5_sad_t1, beta1_n5_sad_t1, beta2_n5_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n5_t1.png", scale = 0.7)


# n6

est_el_6_1$name <- "EL"
est_nb_6_1$name <- "NB"
est_qp_6_1$name <- "QP"
est_pois_6_1$name <- "P"

beta_estimates_6_1 <- rbind(est_pois_6_1,
                            est_qp_6_1,
                            est_nb_6_1,
                            est_el_6_1)

beta0_n6_sad_t1 <- beta_estimates_6_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n6_sad_t1 <- beta_estimates_6_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n6_sad_t1 <- beta_estimates_6_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n6_sad_t1, beta1_n6_sad_t1, beta2_n6_sad_t1, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n6_t1.png", scale = 0.7)


# beta theta 2 sad  ----

est_el_1_2$name <- "EL"
est_nb_1_2$name <- "NB"
est_qp_1_2$name <- "QP"
est_pois_1_2$name <- "P"

beta_estimates_1_2 <- rbind(est_pois_1_1,
                            est_qp_1_1,
                            est_nb_1_1,
                            est_el_1_1)

beta0_n1_sad_t2 <- beta_estimates_1_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+
  ylab(" ")+
  xlim(c(-1,2))+ 
  theme(legend.position="top",
        legend.title=element_blank())


beta1_n1_sad_t2 <- beta_estimates_1_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+
  ylab(" ")+
  xlim(c(-1,2))+ 
  theme(legend.position="top",
        legend.title=element_blank())


beta2_n1_sad_t2 <- beta_estimates_1_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

ggarrange(beta0_n1_sad_t2, beta1_n1_sad_t2, beta2_n1_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)

ggsave("beta_n1_t2.png", scale = 0.7)

# n2

est_el_2_2$name <- "EL"
est_nb_2_2$name <- "NB"
est_qp_2_2$name <- "QP"
est_pois_2_2$name <- "P"

beta_estimates_2_2 <- rbind(est_pois_2_1,
                            est_qp_2_1,
                            est_nb_2_1,
                            est_el_2_1)

beta0_n2_sad_t2 <- beta_estimates_2_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

beta1_n2_sad_t2 <- beta_estimates_2_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

beta2_n2_sad_t2 <- beta_estimates_2_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())+
  xlim(c(-1,2))

ggarrange(beta0_n2_sad_t2, beta1_n2_sad_t2, beta2_n2_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n2_t2.png", scale = 0.7)

# n3

est_el_3_2$name <- "EL"
est_nb_3_2$name <- "NB"
est_qp_3_2$name <- "QP"
est_pois_3_2$name <- "P"

beta_estimates_3_2 <- rbind(est_pois_3_1,
                            est_qp_3_1,
                            est_nb_3_1,
                            est_el_3_1)

beta0_n3_sad_t2 <- beta_estimates_3_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n3_sad_t2 <- beta_estimates_3_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n3_sad_t2 <- beta_estimates_3_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n3_sad_t2, beta1_n3_sad_t2, beta2_n3_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n3_t2.png", scale = 0.7)

# n4

est_el_4_2$name <- "EL"
est_nb_4_2$name <- "NB"
est_qp_4_2$name <- "QP"
est_pois_4_2$name <- "P"

beta_estimates_4_2 <- rbind(est_pois_4_1,
                            est_qp_4_1,
                            est_nb_4_1,
                            est_el_4_1)

beta0_n4_sad_t2 <- beta_estimates_4_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n4_sad_t2 <- beta_estimates_4_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n4_sad_t2 <- beta_estimates_4_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n4_sad_t2, beta1_n4_sad_t2, beta2_n4_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n4_t2.png", scale = 0.7)

# n5

est_el_5_2$name <- "EL"
est_nb_5_2$name <- "NB"
est_qp_5_2$name <- "QP"
est_pois_5_2$name <- "P"

beta_estimates_5_2 <- rbind(est_pois_5_1,
                            est_qp_5_1,
                            est_nb_5_1,
                            est_el_5_1)

beta0_n5_sad_t2 <- beta_estimates_5_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n5_sad_t2 <- beta_estimates_5_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n5_sad_t2 <- beta_estimates_5_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n5_sad_t2, beta1_n5_sad_t2, beta2_n5_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n5_t2.png", scale = 0.7)


# n6

est_el_6_2$name <- "EL"
est_nb_6_2$name <- "NB"
est_qp_6_2$name <- "QP"
est_pois_6_2$name <- "P"

beta_estimates_6_2 <- rbind(est_pois_6_1,
                            est_qp_6_1,
                            est_nb_6_1,
                            est_el_6_1)

beta0_n6_sad_t2 <- beta_estimates_6_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta1_n6_sad_t2 <- beta_estimates_6_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

beta2_n6_sad_t2 <- beta_estimates_6_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  xlab(" ")+ 
  ylab(" ")+
  theme(legend.position="top",
        legend.title=element_blank())

ggarrange(beta0_n6_sad_t2, beta1_n6_sad_t2, beta2_n6_sad_t2, ncol = 3, labels = c("A)", "B)", "C)"),
          legend = "bottom", common.legend = TRUE)


ggsave("beta_n6_t2.png", scale = 0.7)



# ci pārklajumu t1 ----

cov_95_el_1_1$name <- "EL"
cov_95_nb_1_1$name <- "NB"
cov_95_pois_1_1$name <- "P"
cov_95_qp_1_1$name <- "QP"

ci_len_95_1_1 <- rbind(cov_95_el_1_1,
                     cov_95_nb_1_1,
                     cov_95_pois_1_1,
                     cov_95_qp_1_1)

ci_len_95_1_1$skaits <- "n1"


cov_95_el_2_1$name <- "EL"
cov_95_nb_2_1$name <- "NB"
cov_95_pois_2_1$name <- "P"
cov_95_qp_2_1$name <- "QP"

ci_len_95_2_1 <- rbind(cov_95_el_2_1,
                     cov_95_nb_2_1,
                     cov_95_pois_2_1,
                     cov_95_qp_2_1)


ci_len_95_2_1$skaits <- "n2"

cov_95_el_3_1$name <- "EL"
cov_95_nb_3_1$name <- "NB"
cov_95_pois_3_1$name <- "P"
cov_95_qp_3_1$name <- "QP"

ci_len_95_3_1 <- rbind(cov_95_el_3_1,
                     cov_95_nb_3_1,
                     cov_95_pois_3_1,
                     cov_95_qp_3_1)


ci_len_95_3_1$skaits <- "n3"

cov_95_el_4_1$name <- "EL"
cov_95_nb_4_1$name <- "NB"
cov_95_pois_4_1$name <- "P"
cov_95_qp_4_1$name <- "QP"

ci_len_95_4_1 <- rbind(cov_95_el_4_1,
                     cov_95_nb_4_1,
                     cov_95_pois_4_1,
                     cov_95_qp_4_1)


ci_len_95_4_1$skaits <- "n4"

cov_95_el_5_1$name <- "EL"
cov_95_nb_5_1$name <- "NB"
cov_95_pois_5_1$name <- "P"
cov_95_qp_5_1$name <- "QP"

ci_len_95_5_1 <- rbind(cov_95_el_5_1,
                     cov_95_nb_5_1,
                     cov_95_pois_5_1,
                     cov_95_qp_5_1)

ci_len_95_5_1$skaits <- "n5"

cov_95_el_6_1$name <- "EL"
cov_95_nb_6_1$name <- "NB"
cov_95_pois_6_1$name <- "P"
cov_95_qp_6_1$name <- "QP"

ci_len_95_6_1 <- rbind(cov_95_el_6_1,
                     cov_95_nb_6_1,
                     cov_95_pois_6_1,
                     cov_95_qp_6_1)


ci_len_95_6_1$skaits <- "n6"


ci_len_95_all_1 <- rbind(ci_len_95_1_1,
                       ci_len_95_2_1,
                       ci_len_95_3_1,
                       ci_len_95_4_1,
                       ci_len_95_5_1,
                       ci_len_95_6_1)



ci_95_1 <- ci_len_95_all_1 %>%
  group_by(skaits, name)%>%
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) 



cov_99_el_1_1$name <- "EL"
cov_99_nb_1_1$name <- "NB"
cov_99_pois_1_1$name <- "P"
cov_99_qp_1_1$name <- "QP"

ci_len_99_1_1 <- rbind(cov_99_el_1_1,
                       cov_99_nb_1_1,
                       cov_99_pois_1_1,
                       cov_99_qp_1_1)

ci_len_99_1_1$skaits <- "n1"


cov_99_el_2_1$name <- "EL"
cov_99_nb_2_1$name <- "NB"
cov_99_pois_2_1$name <- "P"
cov_99_qp_2_1$name <- "QP"

ci_len_99_2_1 <- rbind(cov_99_el_2_1,
                       cov_99_nb_2_1,
                       cov_99_pois_2_1,
                       cov_99_qp_2_1)


ci_len_99_2_1$skaits <- "n2"

cov_99_el_3_1$name <- "EL"
cov_99_nb_3_1$name <- "NB"
cov_99_pois_3_1$name <- "P"
cov_99_qp_3_1$name <- "QP"

ci_len_99_3_1 <- rbind(cov_99_el_3_1,
                       cov_99_nb_3_1,
                       cov_99_pois_3_1,
                       cov_99_qp_3_1)


ci_len_99_3_1$skaits <- "n3"

cov_99_el_4_1$name <- "EL"
cov_99_nb_4_1$name <- "NB"
cov_99_pois_4_1$name <- "P"
cov_99_qp_4_1$name <- "QP"

ci_len_99_4_1 <- rbind(cov_99_el_4_1,
                       cov_99_nb_4_1,
                       cov_99_pois_4_1,
                       cov_99_qp_4_1)


ci_len_99_4_1$skaits <- "n4"

cov_99_el_5_1$name <- "EL"
cov_99_nb_5_1$name <- "NB"
cov_99_pois_5_1$name <- "P"
cov_99_qp_5_1$name <- "QP"

ci_len_99_5_1 <- rbind(cov_99_el_5_1,
                       cov_99_nb_5_1,
                       cov_99_pois_5_1,
                       cov_99_qp_5_1)

ci_len_99_5_1$skaits <- "n5"

cov_99_el_6_1$name <- "EL"
cov_99_nb_6_1$name <- "NB"
cov_99_pois_6_1$name <- "P"
cov_99_qp_6_1$name <- "QP"

ci_len_99_6_1 <- rbind(cov_99_el_6_1,
                       cov_99_nb_6_1,
                       cov_99_pois_6_1,
                       cov_99_qp_6_1)


ci_len_99_6_1$skaits <- "n6"


ci_len_99_all_1 <- rbind(ci_len_99_1_1,
                         ci_len_99_2_1,
                         ci_len_99_3_1,
                         ci_len_99_4_1,
                         ci_len_99_5_1,
                         ci_len_99_6_1)



ci_99_1 <- ci_len_99_all_1 %>%
  group_by(skaits, name)%>%
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) 


# ci parklajumi t2 ----


cov_95_el_1_2$name <- "EL"
cov_95_nb_1_2$name <- "NB"
cov_95_pois_1_2$name <- "P"
cov_95_qp_1_2$name <- "QP"

ci_len_95_1_2 <- rbind(cov_95_el_1_2,
                       cov_95_nb_1_2,
                       cov_95_pois_1_2,
                       cov_95_qp_1_2)

ci_len_95_1_2$skaits <- "n1"


cov_95_el_2_2$name <- "EL"
cov_95_nb_2_2$name <- "NB"
cov_95_pois_2_2$name <- "P"
cov_95_qp_2_2$name <- "QP"

ci_len_95_2_2 <- rbind(cov_95_el_2_2,
                       cov_95_nb_2_2,
                       cov_95_pois_2_2,
                       cov_95_qp_2_2)


ci_len_95_2_2$skaits <- "n2"

cov_95_el_3_2$name <- "EL"
cov_95_nb_3_2$name <- "NB"
cov_95_pois_3_2$name <- "P"
cov_95_qp_3_2$name <- "QP"

ci_len_95_3_2 <- rbind(cov_95_el_3_2,
                       cov_95_nb_3_2,
                       cov_95_pois_3_2,
                       cov_95_qp_3_2)


ci_len_95_3_2$skaits <- "n3"

cov_95_el_4_2$name <- "EL"
cov_95_nb_4_2$name <- "NB"
cov_95_pois_4_2$name <- "P"
cov_95_qp_4_2$name <- "QP"

ci_len_95_4_2 <- rbind(cov_95_el_4_2,
                       cov_95_nb_4_2,
                       cov_95_pois_4_2,
                       cov_95_qp_4_2)


ci_len_95_4_2$skaits <- "n4"

cov_95_el_5_2$name <- "EL"
cov_95_nb_5_2$name <- "NB"
cov_95_pois_5_2$name <- "P"
cov_95_qp_5_2$name <- "QP"

ci_len_95_5_2 <- rbind(cov_95_el_5_2,
                       cov_95_nb_5_2,
                       cov_95_pois_5_2,
                       cov_95_qp_5_2)

ci_len_95_5_2$skaits <- "n5"

cov_95_el_6_2$name <- "EL"
cov_95_nb_6_2$name <- "NB"
cov_95_pois_6_2$name <- "P"
cov_95_qp_6_2$name <- "QP"

ci_len_95_6_2 <- rbind(cov_95_el_6_2,
                       cov_95_nb_6_2,
                       cov_95_pois_6_2,
                       cov_95_qp_6_2)


ci_len_95_6_2$skaits <- "n6"


ci_len_95_all_2 <- rbind(ci_len_95_1_2,
                         ci_len_95_2_2,
                         ci_len_95_3_2,
                         ci_len_95_4_2,
                         ci_len_95_5_2,
                         ci_len_95_6_2)



ci_95_2 <- ci_len_95_all_2 %>%
  group_by(skaits, name)%>%
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) 




cov_99_el_1_2$name <- "EL"
cov_99_nb_1_2$name <- "NB"
cov_99_pois_1_2$name <- "P"
cov_99_qp_1_2$name <- "QP"

ci_len_99_1_2 <- rbind(cov_99_el_1_2,
                       cov_99_nb_1_2,
                       cov_99_pois_1_2,
                       cov_99_qp_1_2)

ci_len_99_1_2$skaits <- "n1"


cov_99_el_2_2$name <- "EL"
cov_99_nb_2_2$name <- "NB"
cov_99_pois_2_2$name <- "P"
cov_99_qp_2_2$name <- "QP"

ci_len_99_2_2 <- rbind(cov_99_el_2_2,
                       cov_99_nb_2_2,
                       cov_99_pois_2_2,
                       cov_99_qp_2_2)


ci_len_99_2_2$skaits <- "n2"

cov_99_el_3_2$name <- "EL"
cov_99_nb_3_2$name <- "NB"
cov_99_pois_3_2$name <- "P"
cov_99_qp_3_2$name <- "QP"

ci_len_99_3_2 <- rbind(cov_99_el_3_2,
                       cov_99_nb_3_2,
                       cov_99_pois_3_2,
                       cov_99_qp_3_2)


ci_len_99_3_2$skaits <- "n3"

cov_99_el_4_2$name <- "EL"
cov_99_nb_4_2$name <- "NB"
cov_99_pois_4_2$name <- "P"
cov_99_qp_4_2$name <- "QP"

ci_len_99_4_2 <- rbind(cov_99_el_4_2,
                       cov_99_nb_4_2,
                       cov_99_pois_4_2,
                       cov_99_qp_4_2)


ci_len_99_4_2$skaits <- "n4"

cov_99_el_5_2$name <- "EL"
cov_99_nb_5_2$name <- "NB"
cov_99_pois_5_2$name <- "P"
cov_99_qp_5_2$name <- "QP"

ci_len_99_5_2 <- rbind(cov_99_el_5_2,
                       cov_99_nb_5_2,
                       cov_99_pois_5_2,
                       cov_99_qp_5_2)

ci_len_99_5_2$skaits <- "n5"

cov_99_el_6_2$name <- "EL"
cov_99_nb_6_2$name <- "NB"
cov_99_pois_6_2$name <- "P"
cov_99_qp_6_2$name <- "QP"

ci_len_99_6_2 <- rbind(cov_99_el_6_2,
                       cov_99_nb_6_2,
                       cov_99_pois_6_2,
                       cov_99_qp_6_2)


ci_len_99_6_2$skaits <- "n6"


ci_len_99_all_2 <- rbind(ci_len_99_1_2,
                         ci_len_99_2_2,
                         ci_len_99_3_2,
                         ci_len_99_4_2,
                         ci_len_99_5_2,
                         ci_len_99_6_2)



ci_99_2 <- ci_len_99_all_2 %>%
  group_by(skaits, name)%>%
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) 


  openxlsx::write.xlsx(ci_95_1, "ci_95_1.xlsx", row.names = TRUE, overwrite = TRUE)
  openxlsx::write.xlsx(ci_99_1, "ci_99_1.xlsx", row.names = TRUE, overwrite = TRUE)
  openxlsx::write.xlsx(ci_95_2, "ci_95_2.xlsx", row.names = TRUE, overwrite = TRUE)
  openxlsx::write.xlsx(ci_99_2, "ci_99_2.xlsx", row.names = TRUE, overwrite = TRUE)


#  ci garumi ---- 

ci_len_95_1_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b0_t1.png")


ci_len_95_2_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b0_t1.png")


ci_len_95_3_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b0_t1.png")

ci_len_95_4_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b0_t1.png")

ci_len_95_5_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b0_t1.png")

ci_len_95_6_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b0_t1.png")






ci_len_99_1_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b0_t1.png")


ci_len_99_2_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b0_t1.png")


ci_len_99_3_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b0_t1.png")

ci_len_99_4_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b0_t1.png")

ci_len_99_5_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b0_t1.png")

ci_len_99_6_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b0_t1.png")




ci_len_95_1_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b1_t1.png")


ci_len_95_2_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b1_t1.png")


ci_len_95_3_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b1_t1.png")

ci_len_95_4_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b1_t1.png")

ci_len_95_5_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b1_t1.png")

ci_len_95_6_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b1_t1.png")






ci_len_99_1_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b1_t1.png")


ci_len_99_2_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b1_t1.png")


ci_len_99_3_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b1_t1.png")

ci_len_99_4_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b1_t1.png")

ci_len_99_5_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b1_t1.png")

ci_len_99_6_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b1_t1.png")





ci_len_95_1_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b2_t1.png")


ci_len_95_2_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b2_t1.png")


ci_len_95_3_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b2_t1.png")

ci_len_95_4_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b2_t1.png")

ci_len_95_5_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b2_t1.png")

ci_len_95_6_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b2_t1.png")






ci_len_99_1_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b2_t1.png")


ci_len_99_2_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b2_t1.png")


ci_len_99_3_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b2_t1.png")

ci_len_99_4_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b2_t1.png")

ci_len_99_5_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500, theta = 0.5")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b2_t1.png")

ci_len_99_6_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000, theta = 0.5") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b2_t1.png")



# theta 2 ----



ci_len_95_1_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b0_t2.png")


ci_len_95_2_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b0_t2.png")


ci_len_95_3_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b0_t2.png")

ci_len_95_4_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b0_t2.png")

ci_len_95_5_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b0_t2.png")

ci_len_95_6_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b0_t2.png")






ci_len_99_1_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b0_t2.png")


ci_len_99_2_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b0_t2.png")


ci_len_99_3_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b0_t2.png")

ci_len_99_4_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b0_t2.png")

ci_len_99_5_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b0_t2.png")

ci_len_99_6_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b0_t2.png")




ci_len_95_1_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b1_t2.png")


ci_len_95_2_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b1_t2.png")


ci_len_95_3_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b1_t2.png")

ci_len_95_4_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b1_t2.png")

ci_len_95_5_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b1_t2.png")

ci_len_95_6_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b1_t2.png")






ci_len_99_1_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b1_t2.png")


ci_len_99_2_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b1_t2.png")


ci_len_99_3_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b1_t2.png")

ci_len_99_4_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b1_t2.png")

ci_len_99_5_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b1_t2.png")

ci_len_99_6_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b1_t2.png")





ci_len_95_1_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b2_t2.png")


ci_len_95_2_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b2_t2.png")


ci_len_95_3_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b2_t2.png")

ci_len_95_4_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b2_t2.png")

ci_len_95_5_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b2_t2.png")

ci_len_95_6_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b2_t2.png")






ci_len_99_1_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b2_t2.png")


ci_len_99_2_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b2_t2.png")


ci_len_99_3_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b2_t2.png")

ci_len_99_4_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b2_t2.png")

ci_len_99_5_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500, theta = 2")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b2_t2.png")

ci_len_99_6_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000, theta = 2") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b2_t2.png")



