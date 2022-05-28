library(tidyverse)
library(MASS)
library(xtable)
library(melt)



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

# est_pois_1 <- matrix(nrow = reps, ncol = 3)
# est_qp_1 <- matrix(nrow = reps, ncol = 3)
# est_nb_1 <- matrix(nrow = reps, ncol = 3)
# est_el_1 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_2 <- matrix(nrow = reps, ncol = 3)
# est_qp_2 <- matrix(nrow = reps, ncol = 3)
# est_nb_2 <- matrix(nrow = reps, ncol = 3)
# est_el_2 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_3 <- matrix(nrow = reps, ncol = 3)
# est_qp_3 <- matrix(nrow = reps, ncol = 3)
# est_nb_3 <- matrix(nrow = reps, ncol = 3)
# est_el_3 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_4 <- matrix(nrow = reps, ncol = 3)
# est_qp_4 <- matrix(nrow = reps, ncol = 3)
# est_nb_4 <- matrix(nrow = reps, ncol = 3)
# est_el_4 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_5 <- matrix(nrow = reps, ncol = 3)
# est_qp_5 <- matrix(nrow = reps, ncol = 3)
# est_nb_5 <- matrix(nrow = reps, ncol = 3)
# est_el_5 <- matrix(nrow = reps, ncol = 3)
# 
# est_pois_6 <- matrix(nrow = reps, ncol = 3)
# est_qp_6 <- matrix(nrow = reps, ncol = 3)
# est_nb_6 <- matrix(nrow = reps, ncol = 3)
# est_el_6 <- matrix(nrow = reps, ncol = 3)
# 
# 
# 
# cov_95_pois_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_1 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_2 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_3 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_3 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_3 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_3 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_4 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_4 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_4 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_4 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_5 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_5 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_5 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_5 <- matrix(nrow = reps, ncol = 6)
# 
# cov_95_pois_6 <- matrix(nrow = reps, ncol = 6)
# cov_95_qp_6 <- matrix(nrow = reps, ncol = 6)
# cov_95_nb_6 <- matrix(nrow = reps, ncol = 6)
# cov_95_el_6 <- matrix(nrow = reps, ncol = 6)
# 
# 
# 
# cov_99_pois_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_1 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_1 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_2 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_2 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_3 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_3 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_3 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_3 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_4 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_4 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_4 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_4 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_5 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_5 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_5 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_5 <- matrix(nrow = reps, ncol = 6)
# 
# cov_99_pois_6 <- matrix(nrow = reps, ncol = 6)
# cov_99_qp_6 <- matrix(nrow = reps, ncol = 6)
# cov_99_nb_6 <- matrix(nrow = reps, ncol = 6)
# cov_99_el_6 <- matrix(nrow = reps, ncol = 6)



conf_in_pois_1_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_95 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_95 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_95 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_95 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_95 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_95 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_95 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_95 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_95 <- matrix(nrow = reps, ncol = 6)


conf_in_pois_1_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_1_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_1_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_1_99 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_2_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_2_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_2_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_2_99 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_3_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_3_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_3_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_3_99 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_4_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_4_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_4_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_4_99 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_5_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_5_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_5_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_5_99 <- matrix(nrow = reps, ncol = 6)

conf_in_pois_6_99 <- matrix(nrow = reps, ncol = 6)
conf_in_qp_6_99 <- matrix(nrow = reps, ncol = 6)
conf_in_nb_6_99 <- matrix(nrow = reps, ncol = 6)
conf_in_el_6_99 <- matrix(nrow = reps, ncol = 6)
# data sim ----



for(i in 1 : reps){
  # i <- 1
  Y_1 <- rpois(n1, exp(b0 + x1_1*b1 + x2_1*b2))
  Y_2 <- rpois(n2, exp(b0 + x1_2*b1 + x2_2*b2))
  Y_3 <- rpois(n3, exp(b0 + x1_3*b1 + x2_3*b2))
  Y_4 <- rpois(n4, exp(b0 + x1_4*b1 + x2_4*b2))
  Y_5 <- rpois(n5, exp(b0 + x1_5*b1 + x2_5*b2))
  Y_6 <- rpois(n6, exp(b0 + x1_6*b1 + x2_6*b2))

  mp_1 <- glm(Y_1~x1_1+x2_1, family = "poisson")
  mqp_1 <- glm(Y_1~x1_1+x2_1, family = "quasipoisson")
  mnb_1 <- glm.nb(Y_1~x1_1+x2_1)
  mel_1 <- el_glm(Y_1~x1_1+x2_1, family = poisson)

  mp_2 <- glm(Y_2~x1_2+x2_2, family = "poisson")
  mqp_2 <- glm(Y_2~x1_2+x2_2, family = "quasipoisson")
  mnb_2 <- glm.nb(Y_2~x1_2+x2_2)
  mel_2 <- el_glm(Y_2~x1_2+x2_2, family = poisson)

  mp_3 <- glm(Y_3~x1_3+x2_3, family = "poisson")
  mqp_3 <- glm(Y_3~x1_3+x2_3, family = "quasipoisson")
  mnb_3 <- glm.nb(Y_3~x1_3+x2_3)
  mel_3 <- el_glm(Y_3~x1_3+x2_3, family = poisson)

  mp_4 <- glm(Y_4~x1_4+x2_4, family = "poisson")
  mqp_4 <- glm(Y_4~x1_4+x2_4, family = "quasipoisson")
  mnb_4 <- glm.nb(Y_4~x1_4+x2_4)
  mel_4 <- el_glm(Y_4~x1_4+x2_4, family = poisson)

  mp_5 <- glm(Y_5~x1_5+x2_5, family = "poisson")
  mqp_5 <- glm(Y_5~x1_5+x2_5, family = "quasipoisson")
  mnb_5 <- glm.nb(Y_5~x1_5+x2_5)
  mel_5 <- el_glm(Y_5~x1_5+x2_5, family = poisson)

  mp_6 <- glm(Y_6~x1_6+x2_6, family = "poisson")
  mqp_6 <- glm(Y_6~x1_6+x2_6, family = "quasipoisson")
  mnb_6 <- glm.nb(Y_6~x1_6+x2_6)
  mel_6 <- el_glm(Y_6~x1_6+x2_6, family = poisson)
  
  

  
  cov_p_1_95 <- coverage(mp_1, 0.95)
  cov_qp_1_95 <- coverage(mqp_1, 0.95)
  cov_nb_1_95 <- coverage(mnb_1, 0.95)
  cov_el_1_95 <- coverage(mel_1, 0.95)

  cov_p_2_95 <- coverage(mp_2, 0.95)
  cov_qp_2_95 <- coverage(mqp_2, 0.95)
  cov_nb_2_95 <- coverage(mnb_2, 0.95)
  cov_el_2_95 <- coverage(mel_2, 0.95)

  cov_p_3_95 <- coverage(mp_3, 0.95)
  cov_qp_3_95 <- coverage(mqp_3, 0.95)
  cov_nb_3_95 <- coverage(mnb_3, 0.95)
  cov_el_3_95 <- coverage(mel_3, 0.95)

  cov_p_4_95 <- coverage(mp_4, 0.95)
  cov_qp_4_95 <- coverage(mqp_4, 0.95)
  cov_nb_4_95 <- coverage(mnb_4, 0.95)
  cov_el_4_95 <- coverage(mel_4, 0.95)

  cov_p_5_95 <- coverage(mp_5, 0.95)
  cov_qp_5_95 <- coverage(mqp_5, 0.95)
  cov_nb_5_95 <- coverage(mnb_5, 0.95)
  cov_el_5_95 <- coverage(mel_5, 0.95)
  #
  cov_p_6_95 <- coverage(mp_6, 0.95)
  cov_qp_6_95 <- coverage(mqp_6, 0.95)
  cov_nb_6_95 <- coverage(mnb_6, 0.95)
  cov_el_6_95 <- coverage(mel_6, 0.95)
  
  cov_p_1_99 <- coverage(mp_1, 0.99)
  cov_qp_1_99 <- coverage(mqp_1, 0.99)
  cov_nb_1_99 <- coverage(mnb_1, 0.99)
  cov_el_1_99 <- coverage(mel_1, 0.99)

  cov_p_2_99 <- coverage(mp_2, 0.99)
  cov_qp_2_99 <- coverage(mqp_2, 0.99)
  cov_nb_2_99 <- coverage(mnb_2, 0.99)
  cov_el_2_99 <- coverage(mel_2, 0.99)

  cov_p_3_99 <- coverage(mp_3, 0.99)
  cov_qp_3_99 <- coverage(mqp_3, 0.99)
  cov_nb_3_99 <- coverage(mnb_3, 0.99)
  cov_el_3_99 <- coverage(mel_3, 0.99)

  cov_p_4_99 <- coverage(mp_4, 0.99)
  cov_qp_4_99 <- coverage(mqp_4, 0.99)
  cov_nb_4_99 <- coverage(mnb_4, 0.99)
  cov_el_4_99 <- coverage(mel_4, 0.99)

  cov_p_5_99 <- coverage(mp_5, 0.99)
  cov_qp_5_99 <- coverage(mqp_5, 0.99)
  cov_nb_5_99 <- coverage(mnb_5, 0.99)
  cov_el_5_99 <- coverage(mel_5, 0.99)
  #
  cov_p_6_99 <- coverage(mp_6, 0.99)
  cov_qp_6_99 <- coverage(mqp_6, 0.99)
  cov_nb_6_99 <- coverage(mnb_6, 0.99)
  cov_el_6_99 <- coverage(mel_6, 0.99)
  # #
  # # n1
  # 
  # est_pois_1[i,1] <- coefficients(mp_1)[1]
  # est_pois_1[i,2] <- coefficients(mp_1)[2]
  # est_pois_1[i,3] <- coefficients(mp_1)[3]
  # 
  # est_qp_1[i,1] <- coefficients(mqp_1)[1]
  # est_qp_1[i,2] <- coefficients(mqp_1)[2]
  # est_qp_1[i,3] <- coefficients(mqp_1)[3]
  # 
  # est_nb_1[i,1] <- coefficients(mnb_1)[1]
  # est_nb_1[i,2] <- coefficients(mnb_1)[2]
  # est_nb_1[i,3] <- coefficients(mnb_1)[3]
  # 
  # est_el_1[i,1] <- mel_1@coefficients[1]
  # est_el_1[i,2] <- mel_1@coefficients[2]
  # est_el_1[i,3] <- mel_1@coefficients[3]
  
  
  
  conf_in_pois_1_95[i,] <-  cov_p_1_95$conf_int
    conf_in_qp_1_95[i,] <-  cov_qp_1_95$conf_int
    conf_in_nb_1_95[i,] <-  cov_nb_1_95$conf_int
    conf_in_el_1_95[i,] <-  cov_el_1_95$conf_int
    
    conf_in_pois_2_95[i,] <-  cov_p_2_95$conf_int
    conf_in_qp_2_95[i,] <-  cov_qp_2_95$conf_int
    conf_in_nb_2_95[i,] <-  cov_nb_2_95$conf_int
    conf_in_el_2_95[i,] <-  cov_el_2_95$conf_int
    
    conf_in_pois_3_95[i,] <-  cov_p_3_95$conf_int
    conf_in_qp_3_95[i,] <-  cov_qp_3_95$conf_int
    conf_in_nb_3_95[i,] <-  cov_nb_3_95$conf_int
    conf_in_el_3_95[i,] <-  cov_el_3_95$conf_int
    
    conf_in_pois_4_95[i,] <-  cov_p_4_95$conf_int
    conf_in_qp_4_95[i,] <-  cov_qp_4_95$conf_int
    conf_in_nb_4_95[i,] <-  cov_nb_4_95$conf_int
    conf_in_el_4_95[i,] <-  cov_el_4_95$conf_int
    
    conf_in_pois_5_95[i,] <-  cov_p_5_95$conf_int
    conf_in_qp_5_95[i,] <-  cov_qp_5_95$conf_int
    conf_in_nb_5_95[i,] <-  cov_nb_5_95$conf_int
    conf_in_el_5_95[i,] <-  cov_el_5_95$conf_int
    
    conf_in_pois_6_95[i,] <-  cov_p_6_95$conf_int
    conf_in_qp_6_95[i,] <-  cov_qp_6_95$conf_int
    conf_in_nb_6_95[i,] <-  cov_nb_6_95$conf_int
    conf_in_el_6_95[i,] <-  cov_el_6_95$conf_int
    
    
    conf_in_pois_1_99[i,] <-  cov_p_1_99$conf_int
    conf_in_qp_1_99[i,] <-  cov_qp_1_99$conf_int
    conf_in_nb_1_99[i,] <-  cov_nb_1_99$conf_int
    conf_in_el_1_99[i,] <-  cov_el_1_99$conf_int
    
    conf_in_pois_2_99[i,] <-  cov_p_2_99$conf_int
    conf_in_qp_2_99[i,] <-  cov_qp_2_99$conf_int
    conf_in_nb_2_99[i,] <-  cov_nb_2_99$conf_int
    conf_in_el_2_99[i,] <-  cov_el_2_99$conf_int
    
    conf_in_pois_3_99[i,] <-  cov_p_3_99$conf_int
    conf_in_qp_3_99[i,] <-  cov_qp_3_99$conf_int
    conf_in_nb_3_99[i,] <-  cov_nb_3_99$conf_int
    conf_in_el_3_99[i,] <-  cov_el_3_99$conf_int
    
    conf_in_pois_4_99[i,] <-  cov_p_4_99$conf_int
    conf_in_qp_4_99[i,] <-  cov_qp_4_99$conf_int
    conf_in_nb_4_99[i,] <-  cov_nb_4_99$conf_int
    conf_in_el_4_99[i,] <-  cov_el_4_99$conf_int
    
    conf_in_pois_5_99[i,] <-  cov_p_5_99$conf_int
    conf_in_qp_5_99[i,] <-  cov_qp_5_99$conf_int
    conf_in_nb_5_99[i,] <-  cov_nb_5_99$conf_int
    conf_in_el_5_99[i,] <-  cov_el_5_99$conf_int
    
    conf_in_pois_6_99[i,] <-  cov_p_6_99$conf_int
    conf_in_qp_6_99[i,] <-  cov_qp_6_99$conf_int
    conf_in_nb_6_99[i,] <-  cov_nb_6_99$conf_int
    conf_in_el_6_99[i,] <-  cov_el_6_99$conf_int
  
  
  # 
  # 
  # 
  # cov_95_pois_1[i,] <- c(cov_p_1_95$coverage, cov_p_1_95$ci_length)
  # cov_95_qp_1[i,] <- c(cov_qp_1_95$coverage, cov_qp_1_95$ci_length)
  # cov_95_nb_1[i,] <- c(cov_nb_1_95$coverage, cov_nb_1_95$ci_length)
  # cov_95_el_1[i,] <- c(cov_el_1_95$coverage, cov_el_1_95$ci_length)
  # 
  # cov_99_pois_1[i,] <- c(cov_p_1_99$coverage, cov_p_1_99$ci_length)
  # cov_99_qp_1[i,] <- c(cov_qp_1_99$coverage, cov_qp_1_99$ci_length)
  # cov_99_nb_1[i,] <- c(cov_nb_1_99$coverage, cov_nb_1_99$ci_length)
  # cov_99_el_1[i,] <- c(cov_el_1_99$coverage, cov_el_1_99$ci_length)
  # 
  # 
  # #n2
  # 
  # est_pois_2[i,1] <- coefficients(mp_2)[1]
  # est_pois_2[i,2] <- coefficients(mp_2)[2]
  # est_pois_2[i,3] <- coefficients(mp_2)[3]
  # 
  # est_qp_2[i,1] <- coefficients(mqp_2)[1]
  # est_qp_2[i,2] <- coefficients(mqp_2)[2]
  # est_qp_2[i,3] <- coefficients(mqp_2)[3]
  # 
  # est_nb_2[i,1] <- coefficients(mnb_2)[1]
  # est_nb_2[i,2] <- coefficients(mnb_2)[2]
  # est_nb_2[i,3] <- coefficients(mnb_2)[3]
  # 
  # est_el_2[i,1] <- mel_2@coefficients[1]
  # est_el_2[i,2] <- mel_2@coefficients[2]
  # est_el_2[i,3] <- mel_2@coefficients[3]
  # 
  # cov_95_pois_2[i,] <- c(cov_p_2_95$coverage, cov_p_2_95$ci_length)
  # cov_95_qp_2[i,] <- c(cov_qp_2_95$coverage, cov_qp_2_95$ci_length)
  # cov_95_nb_2[i,] <- c(cov_nb_2_95$coverage, cov_nb_2_95$ci_length)
  # cov_95_el_2[i,] <- c(cov_el_2_95$coverage, cov_el_2_95$ci_length)
  # 
  # cov_99_pois_2[i,] <- c(cov_p_2_99$coverage, cov_p_2_99$ci_length)
  # cov_99_qp_2[i,] <- c(cov_qp_2_99$coverage, cov_qp_2_99$ci_length)
  # cov_99_nb_2[i,] <- c(cov_nb_2_99$coverage, cov_nb_2_99$ci_length)
  # cov_99_el_2[i,] <- c(cov_el_2_99$coverage, cov_el_2_99$ci_length)
  # 
  # #n3
  # 
  # est_pois_3[i,1] <- coefficients(mp_3)[1]
  # est_pois_3[i,2] <- coefficients(mp_3)[2]
  # est_pois_3[i,3] <- coefficients(mp_3)[3]
  # 
  # est_qp_3[i,1] <- coefficients(mqp_3)[1]
  # est_qp_3[i,2] <- coefficients(mqp_3)[2]
  # est_qp_3[i,3] <- coefficients(mqp_3)[3]
  # 
  # est_nb_3[i,1] <- coefficients(mnb_3)[1]
  # est_nb_3[i,2] <- coefficients(mnb_3)[2]
  # est_nb_3[i,3] <- coefficients(mnb_3)[3]
  # 
  # est_el_3[i,1] <- mel_3@coefficients[1]
  # est_el_3[i,2] <- mel_3@coefficients[2]
  # est_el_3[i,3] <- mel_3@coefficients[3]
  # 
  # cov_95_pois_3[i,] <- c(cov_p_3_95$coverage, cov_p_3_95$ci_length)
  # cov_95_qp_3[i,] <- c(cov_qp_3_95$coverage, cov_qp_3_95$ci_length)
  # cov_95_nb_3[i,] <- c(cov_nb_3_95$coverage, cov_nb_3_95$ci_length)
  # cov_95_el_3[i,] <- c(cov_el_3_95$coverage, cov_el_3_95$ci_length)
  # 
  # cov_99_pois_3[i,] <- c(cov_p_3_99$coverage, cov_p_3_99$ci_length)
  # cov_99_qp_3[i,] <- c(cov_qp_3_99$coverage, cov_qp_3_99$ci_length)
  # cov_99_nb_3[i,] <- c(cov_nb_3_99$coverage, cov_nb_3_99$ci_length)
  # cov_99_el_3[i,] <- c(cov_el_3_99$coverage, cov_el_3_99$ci_length)
  # #n4
  # 
  # est_pois_4[i,1] <- coefficients(mp_4)[1]
  # est_pois_4[i,2] <- coefficients(mp_4)[2]
  # est_pois_4[i,3] <- coefficients(mp_4)[3]
  # 
  # est_qp_4[i,1] <- coefficients(mqp_4)[1]
  # est_qp_4[i,2] <- coefficients(mqp_4)[2]
  # est_qp_4[i,3] <- coefficients(mqp_4)[3]
  # 
  # est_nb_4[i,1] <- coefficients(mnb_4)[1]
  # est_nb_4[i,2] <- coefficients(mnb_4)[2]
  # est_nb_4[i,3] <- coefficients(mnb_4)[3]
  # 
  # est_el_4[i,1] <- mel_4@coefficients[1]
  # est_el_4[i,2] <- mel_4@coefficients[2]
  # est_el_4[i,3] <- mel_4@coefficients[3]
  # 
  # cov_95_pois_4[i,] <- c(cov_p_4_95$coverage, cov_p_4_95$ci_length)
  # cov_95_qp_4[i,] <- c(cov_qp_4_95$coverage, cov_qp_4_95$ci_length)
  # cov_95_nb_4[i,] <- c(cov_nb_4_95$coverage, cov_nb_4_95$ci_length)
  # cov_95_el_4[i,] <- c(cov_el_4_95$coverage, cov_el_4_95$ci_length)
  # 
  # cov_99_pois_4[i,] <- c(cov_p_4_99$coverage, cov_p_4_99$ci_length)
  # cov_99_qp_4[i,] <- c(cov_qp_4_99$coverage, cov_qp_4_99$ci_length)
  # cov_99_nb_4[i,] <- c(cov_nb_4_99$coverage, cov_nb_4_99$ci_length)
  # cov_99_el_4[i,] <- c(cov_el_4_99$coverage, cov_el_4_99$ci_length)
  # #n5
  # 
  # est_pois_5[i,1] <- coefficients(mp_5)[1]
  # est_pois_5[i,2] <- coefficients(mp_5)[2]
  # est_pois_5[i,3] <- coefficients(mp_5)[3]
  # 
  # est_qp_5[i,1] <- coefficients(mqp_5)[1]
  # est_qp_5[i,2] <- coefficients(mqp_5)[2]
  # est_qp_5[i,3] <- coefficients(mqp_5)[3]
  # 
  # est_nb_5[i,1] <- coefficients(mnb_5)[1]
  # est_nb_5[i,2] <- coefficients(mnb_5)[2]
  # est_nb_5[i,3] <- coefficients(mnb_5)[3]
  # 
  # est_el_5[i,1] <- mel_5@coefficients[1]
  # est_el_5[i,2] <- mel_5@coefficients[2]
  # est_el_5[i,3] <- mel_5@coefficients[3]
  # 
  # cov_95_pois_5[i,] <- c(cov_p_5_95$coverage, cov_p_5_95$ci_length)
  # cov_95_qp_5[i,] <- c(cov_qp_5_95$coverage, cov_qp_5_95$ci_length)
  # cov_95_nb_5[i,] <- c(cov_nb_5_95$coverage, cov_nb_5_95$ci_length)
  # cov_95_el_5[i,] <- c(cov_el_5_95$coverage, cov_el_5_95$ci_length)
  # 
  # cov_99_pois_5[i,] <- c(cov_p_5_99$coverage, cov_p_5_99$ci_length)
  # cov_99_qp_5[i,] <- c(cov_qp_5_99$coverage, cov_qp_5_99$ci_length)
  # cov_99_nb_5[i,] <- c(cov_nb_5_99$coverage, cov_nb_5_99$ci_length)
  # cov_99_el_5[i,] <- c(cov_el_5_99$coverage, cov_el_5_99$ci_length)
  # # n6
  # 
  # est_pois_6[i,1] <- coefficients(mp_6)[1]
  # est_pois_6[i,2] <- coefficients(mp_6)[2]
  # est_pois_6[i,3] <- coefficients(mp_6)[3]
  # 
  # est_qp_6[i,1] <- coefficients(mqp_6)[1]
  # est_qp_6[i,2] <- coefficients(mqp_6)[2]
  # est_qp_6[i,3] <- coefficients(mqp_6)[3]
  # 
  # est_nb_6[i,1] <- coefficients(mnb_6)[1]
  # est_nb_6[i,2] <- coefficients(mnb_6)[2]
  # est_nb_6[i,3] <- coefficients(mnb_6)[3]
  # 
  # est_el_6[i,1] <- mel_6@coefficients[1]
  # est_el_6[i,2] <- mel_6@coefficients[2]
  # est_el_6[i,3] <- mel_6@coefficients[3]
  # 
  # cov_95_pois_6[i,] <- c(cov_p_6_95$coverage, cov_p_6_95$ci_length)
  # cov_95_qp_6[i,] <- c(cov_qp_6_95$coverage, cov_qp_6_95$ci_length)
  # cov_95_nb_6[i,] <- c(cov_nb_6_95$coverage, cov_nb_6_95$ci_length)
  # cov_95_el_6[i,] <- c(cov_el_6_95$coverage, cov_el_6_95$ci_length)
  # 
  # cov_99_pois_6[i,] <- c(cov_p_6_99$coverage, cov_p_6_99$ci_length)
  # cov_99_qp_6[i,] <- c(cov_qp_6_99$coverage, cov_qp_6_99$ci_length)
  # cov_99_nb_6[i,] <- c(cov_nb_6_99$coverage, cov_nb_6_99$ci_length)
  # cov_99_el_6[i,] <- c(cov_el_6_99$coverage, cov_el_6_99$ci_length)
  
  print(i)
}



# est tables ----

est_pois_1 <- as.data.frame(est_pois_1)
est_qp_1 <- as.data.frame(est_qp_1)
est_nb_1 <- as.data.frame(est_nb_1)
est_el_1 <- as.data.frame(est_el_1)

est_pois_2 <- as.data.frame(est_pois_2)
est_qp_2 <- as.data.frame(est_qp_2)
est_nb_2 <- as.data.frame(est_nb_2)
est_el_2 <- as.data.frame(est_el_2)

est_pois_3 <- as.data.frame(est_pois_3)
est_qp_3 <- as.data.frame(est_qp_3)
est_nb_3 <- as.data.frame(est_nb_3)
est_el_3 <- as.data.frame(est_el_3)

est_pois_4 <- as.data.frame(est_pois_4)
est_qp_4 <- as.data.frame(est_qp_4)
est_nb_4 <- as.data.frame(est_nb_4)
est_el_4 <- as.data.frame(est_el_4)

est_pois_5 <- as.data.frame(est_pois_5)
est_qp_5 <- as.data.frame(est_qp_5)
est_nb_5 <- as.data.frame(est_nb_5)
est_el_5 <- as.data.frame(est_el_5)

est_pois_6 <- as.data.frame(est_pois_6)
est_qp_6 <- as.data.frame(est_qp_6)
est_nb_6 <- as.data.frame(est_nb_6)
est_el_6 <- as.data.frame(est_el_6)



cov_95_pois_1 <- as.data.frame(cov_95_pois_1)
cov_95_qp_1 <- as.data.frame(cov_95_qp_1)
cov_95_nb_1 <- as.data.frame(cov_95_nb_1)
cov_95_el_1 <- as.data.frame(cov_95_el_1)

cov_95_pois_2 <- as.data.frame(cov_95_pois_2)
cov_95_qp_2 <- as.data.frame(cov_95_qp_2)
cov_95_nb_2 <- as.data.frame(cov_95_nb_2)
cov_95_el_2 <- as.data.frame(cov_95_el_2)

cov_95_pois_3 <- as.data.frame(cov_95_pois_3)
cov_95_qp_3 <- as.data.frame(cov_95_qp_3)
cov_95_nb_3 <- as.data.frame(cov_95_nb_3)
cov_95_el_3 <- as.data.frame(cov_95_el_3)

cov_95_pois_4 <- as.data.frame(cov_95_pois_4)
cov_95_qp_4 <- as.data.frame(cov_95_qp_4)
cov_95_nb_4 <- as.data.frame(cov_95_nb_4)
cov_95_el_4 <- as.data.frame(cov_95_el_4)

cov_95_pois_5 <- as.data.frame(cov_95_pois_5)
cov_95_qp_5 <- as.data.frame(cov_95_qp_5)
cov_95_nb_5 <- as.data.frame(cov_95_nb_5)
cov_95_el_5 <- as.data.frame(cov_95_el_5)

cov_95_pois_6 <- as.data.frame(cov_95_pois_6)
cov_95_qp_6 <- as.data.frame(cov_95_qp_6)
cov_95_nb_6 <- as.data.frame(cov_95_nb_6)
cov_95_el_6 <- as.data.frame(cov_95_el_6)



cov_99_pois_1 <- as.data.frame(cov_99_pois_1)
cov_99_qp_1 <- as.data.frame(cov_99_qp_1)
cov_99_nb_1 <- as.data.frame(cov_99_nb_1)
cov_99_el_1 <- as.data.frame(cov_99_el_1)

cov_99_pois_2 <- as.data.frame(cov_99_pois_2)
cov_99_qp_2 <- as.data.frame(cov_99_qp_2)
cov_99_nb_2 <- as.data.frame(cov_99_nb_2)
cov_99_el_2 <- as.data.frame(cov_99_el_2)

cov_99_pois_3 <- as.data.frame(cov_99_pois_3)
cov_99_qp_3 <- as.data.frame(cov_99_qp_3)
cov_99_nb_3 <- as.data.frame(cov_99_nb_3)
cov_99_el_3 <- as.data.frame(cov_99_el_3)

cov_99_pois_4 <- as.data.frame(cov_99_pois_4)
cov_99_qp_4 <- as.data.frame(cov_99_qp_4)
cov_99_nb_4 <- as.data.frame(cov_99_nb_4)
cov_99_el_4 <- as.data.frame(cov_99_el_4)

cov_99_pois_5 <- as.data.frame(cov_99_pois_5)
cov_99_qp_5 <- as.data.frame(cov_99_qp_5)
cov_99_nb_5 <- as.data.frame(cov_99_nb_5)
cov_99_el_5 <- as.data.frame(cov_99_el_5)

cov_99_pois_6 <- as.data.frame(cov_99_pois_6)
cov_99_qp_6 <- as.data.frame(cov_99_qp_6)
cov_99_nb_6 <- as.data.frame(cov_99_nb_6)
cov_99_el_6 <- as.data.frame(cov_99_el_6)



est_names <- c("b0_nov", "b1_nov", "b2_nov")
cov_names <- c("b0_cov", "b1_cov", "b2_cov", "b0_ci_len", "b1_ci_len", "b2_ci_len")

names(est_pois_1) <- est_names
names(est_qp_1) <- est_names
names(est_nb_1) <- est_names
names(est_el_1) <- est_names

names(est_pois_2) <- est_names
names(est_qp_2) <- est_names
names(est_nb_2) <- est_names
names(est_el_2) <- est_names

names(est_pois_3) <- est_names
names(est_qp_3) <- est_names
names(est_nb_3) <- est_names
names(est_el_3) <- est_names

names(est_pois_4) <- est_names
names(est_qp_4) <- est_names
names(est_nb_4) <- est_names
names(est_el_4) <- est_names

names(est_pois_5) <- est_names
names(est_qp_5) <- est_names
names(est_nb_5) <- est_names
names(est_el_5) <- est_names

names(est_pois_6) <- est_names
names(est_qp_6) <- est_names
names(est_nb_6) <- est_names
names(est_el_6) <- est_names


names(cov_95_pois_1) <- cov_names
names(cov_95_qp_1) <- cov_names
names(cov_95_nb_1) <- cov_names
names(cov_95_el_1) <- cov_names

names(cov_95_pois_2) <- cov_names
names(cov_95_qp_2) <- cov_names
names(cov_95_nb_2) <- cov_names
names(cov_95_el_2) <- cov_names

names(cov_95_pois_3) <- cov_names
names(cov_95_qp_3) <- cov_names
names(cov_95_nb_3) <- cov_names
names(cov_95_el_3) <- cov_names

names(cov_95_pois_4) <- cov_names
names(cov_95_qp_4) <- cov_names
names(cov_95_nb_4) <- cov_names
names(cov_95_el_4) <- cov_names

names(cov_95_pois_5) <- cov_names
names(cov_95_qp_5) <- cov_names
names(cov_95_nb_5) <- cov_names
names(cov_95_el_5) <- cov_names

names(cov_95_pois_6) <- cov_names
names(cov_95_qp_6) <- cov_names
names(cov_95_nb_6) <- cov_names
names(cov_95_el_6) <- cov_names


names(cov_99_pois_1) <- cov_names
names(cov_99_qp_1) <- cov_names
names(cov_99_nb_1) <- cov_names
names(cov_99_el_1) <- cov_names

names(cov_99_pois_2) <- cov_names
names(cov_99_qp_2) <- cov_names
names(cov_99_nb_2) <- cov_names
names(cov_99_el_2) <- cov_names

names(cov_99_pois_3) <- cov_names
names(cov_99_qp_3) <- cov_names
names(cov_99_nb_3) <- cov_names
names(cov_99_el_3) <- cov_names

names(cov_99_pois_4) <- cov_names
names(cov_99_qp_4) <- cov_names
names(cov_99_nb_4) <- cov_names
names(cov_99_el_4) <- cov_names

names(cov_99_pois_5) <- cov_names
names(cov_99_qp_5) <- cov_names
names(cov_99_nb_5) <- cov_names
names(cov_99_el_5) <- cov_names

names(cov_99_pois_6) <- cov_names
names(cov_99_qp_6) <- cov_names
names(cov_99_nb_6) <- cov_names
names(cov_99_el_6) <- cov_names

# beta0 sad  ----

# n1

est_el_1$name <- "EL"
est_nb_1$name <- "NB"
est_qp_1$name <- "QP"
est_pois_1$name <- "P"

beta_estimates_1 <- rbind(est_pois_1,
                        est_qp_1,
                        est_nb_1,
                        est_el_1)

beta_estimates_1 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 20")+
  xlab(" ")

ggsave("beta0_n1_sad.png")

# n2

est_el_2$name <- "EL"
est_nb_2$name <- "NB"
est_qp_2$name <- "QP"
est_pois_2$name <- "P"

beta_estimates_2 <- rbind(est_pois_2,
                          est_qp_2,
                          est_nb_2,
                          est_el_2)

beta_estimates_2 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 50")+
  xlab(" ")

ggsave("beta0_n2_sad.png")

# n3

est_el_3$name <- "EL"
est_nb_3$name <- "NB"
est_qp_3$name <- "QP"
est_pois_3$name <- "P"

beta_estimates_3 <- rbind(est_pois_3,
                          est_qp_3,
                          est_nb_3,
                          est_el_3)

beta_estimates_3 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 100")+
  xlab(" ")

ggsave("beta0_n3_sad.png")

# n4

est_el_4$name <- "EL"
est_nb_4$name <- "NB"
est_qp_4$name <- "QP"
est_pois_4$name <- "P"

beta_estimates_4 <- rbind(est_pois_4,
                          est_qp_4,
                          est_nb_4,
                          est_el_4)

beta_estimates_4 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 200")+
  xlab(" ")

ggsave("beta0_n4_sad.png")


# n5

est_el_5$name <- "EL"
est_nb_5$name <- "NB"
est_qp_5$name <- "QP"
est_pois_5$name <- "P"

beta_estimates_5 <- rbind(est_pois_5,
                          est_qp_5,
                          est_nb_5,
                          est_el_5)

beta_estimates_5 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 500")+
  xlab(" ")

ggsave("beta0_n5_sad.png")

# n6

est_el_6$name <- "EL"
est_nb_6$name <- "NB"
est_qp_6$name <- "QP"
est_pois_6$name <- "P"

beta_estimates_6 <- rbind(est_pois_6,
                          est_qp_6,
                          est_nb_6,
                          est_el_6)

beta_estimates_6 %>%
  ggplot(aes(x = b0_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b0))+
  ggtitle("B0, n = 1000")+
  xlab(" ")

ggsave("beta0_n6_sad.png")

# beta1 sad ----

# n1



beta_estimates_1 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 20")+
  xlab(" ")

ggsave("beta1_n1_sad.png")

# n2


beta_estimates_2 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 50")+
  xlab(" ")

ggsave("beta1_n2_sad.png")

# n3



beta_estimates_3 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 100")+
  xlab(" ")

ggsave("beta1_n3_sad.png")

# n4



beta_estimates_4 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 200")+
  xlab(" ")

ggsave("beta1_n4_sad.png")


# n5


beta_estimates_5 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 500")+
  xlab(" ")

ggsave("beta1_n5_sad.png")



# n6

beta_estimates_6 %>%
  ggplot(aes(x = b1_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b1))+
  ggtitle("B1, n = 1000")+
  xlab(" ")

ggsave("beta1_n6_sad.png")

# beta2 sad ----

# n1



beta_estimates_1 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 20")+
  xlab(" ")

ggsave("beta2_n1_sad.png")

# n2


beta_estimates_2 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 50")+
  xlab(" ")

ggsave("beta2_n2_sad.png")

# n3



beta_estimates_3 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 100")+
  xlab(" ")

ggsave("beta2_n3_sad.png")

# n4



beta_estimates_4 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 200")+
  xlab(" ")

ggsave("beta2_n4_sad.png")


# n5


beta_estimates_5 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 500")+
  xlab(" ")

ggsave("beta2_n5_sad.png")

# n6

beta_estimates_6 %>%
  ggplot(aes(x = b2_nov, col = name))+
  geom_density()+
  geom_vline(aes(xintercept = b2))+
  ggtitle("B2, n = 1000")+
  xlab(" ")

ggsave("beta2_n6_sad.png")



# ci pārklajumu prec ----

cov_95_el_1$name <- "EL"
cov_95_nb_1$name <- "NB"
cov_95_pois_1$name <- "P"
cov_95_qp_1$name <- "QP"

ci_len_95_1 <- rbind(cov_95_el_1,
                     cov_95_nb_1,
                     cov_95_pois_1,
                     cov_95_qp_1)


cov_95_el_2$name <- "EL"
cov_95_nb_2$name <- "NB"
cov_95_pois_2$name <- "P"
cov_95_qp_2$name <- "QP"

ci_len_95_2 <- rbind(cov_95_el_2,
                     cov_95_nb_2,
                     cov_95_pois_2,
                     cov_95_qp_2)


cov_95_el_3$name <- "EL"
cov_95_nb_3$name <- "NB"
cov_95_pois_3$name <- "P"
cov_95_qp_3$name <- "QP"

ci_len_95_3 <- rbind(cov_95_el_3,
                     cov_95_nb_3,
                     cov_95_pois_3,
                     cov_95_qp_3)


cov_95_el_4$name <- "EL"
cov_95_nb_4$name <- "NB"
cov_95_pois_4$name <- "P"
cov_95_qp_4$name <- "QP"

ci_len_95_4 <- rbind(cov_95_el_4,
                     cov_95_nb_4,
                     cov_95_pois_4,
                     cov_95_qp_4)


cov_95_el_5$name <- "EL"
cov_95_nb_5$name <- "NB"
cov_95_pois_5$name <- "P"
cov_95_qp_5$name <- "QP"

ci_len_95_5 <- rbind(cov_95_el_5,
                     cov_95_nb_5,
                     cov_95_pois_5,
                     cov_95_qp_5)


cov_95_el_6$name <- "EL"
cov_95_nb_6$name <- "NB"
cov_95_pois_6$name <- "P"
cov_95_qp_6$name <- "QP"

ci_len_95_6 <- rbind(cov_95_el_6,
                     cov_95_nb_6,
                     cov_95_pois_6,
                     cov_95_qp_6)


cov_99_el_1$name <- "EL"
cov_99_nb_1$name <- "NB"
cov_99_pois_1$name <- "P"
cov_99_qp_1$name <- "QP"

ci_len_99_1 <- rbind(cov_99_el_1,
                     cov_99_nb_1,
                     cov_99_pois_1,
                     cov_99_qp_1)


cov_99_el_2$name <- "EL"
cov_99_nb_2$name <- "NB"
cov_99_pois_2$name <- "P"
cov_99_qp_2$name <- "QP"

ci_len_99_2 <- rbind(cov_99_el_2,
                     cov_99_nb_2,
                     cov_99_pois_2,
                     cov_99_qp_2)


cov_99_el_3$name <- "EL"
cov_99_nb_3$name <- "NB"
cov_99_pois_3$name <- "P"
cov_99_qp_3$name <- "QP"

ci_len_99_3 <- rbind(cov_99_el_3,
                     cov_99_nb_3,
                     cov_99_pois_3,
                     cov_99_qp_3)


cov_99_el_4$name <- "EL"
cov_99_nb_4$name <- "NB"
cov_99_pois_4$name <- "P"
cov_99_qp_4$name <- "QP"

ci_len_99_4 <- rbind(cov_99_el_4,
                     cov_99_nb_4,
                     cov_99_pois_4,
                     cov_99_qp_4)


cov_99_el_5$name <- "EL"
cov_99_nb_5$name <- "NB"
cov_99_pois_5$name <- "P"
cov_99_qp_5$name <- "QP"

ci_len_99_5 <- rbind(cov_99_el_5,
                     cov_99_nb_5,
                     cov_99_pois_5,
                     cov_99_qp_5)


cov_99_el_6$name <- "EL"
cov_99_nb_6$name <- "NB"
cov_99_pois_6$name <- "P"
cov_99_qp_6$name <- "QP"

ci_len_99_6 <- rbind(cov_99_el_6,
                     cov_99_nb_6,
                     cov_99_pois_6,
                     cov_99_qp_6)




# ci parklajums 95----

ci_len_95_1 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()

ci_len_95_2 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_95_3 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_95_4 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_95_5 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()

ci_len_95_6 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()






ci_len_99_1 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()

ci_len_99_2 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_99_3 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_99_4 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()


ci_len_99_5 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()

ci_len_99_6 %>% 
  group_by(name) %>% 
  summarise(B0 = sum(b0_cov)/reps,
            B1 = sum(b1_cov)/reps,
            B2 = sum(b2_cov)/reps) %>% 
  xtable()

#  ci garumi

ci_len_95_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b0.png")


ci_len_95_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b0.png")


ci_len_95_3 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b0.png")

ci_len_95_4 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b0.png")

ci_len_95_5 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b0.png")

ci_len_95_6 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b0.png")






ci_len_99_1 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b0.png")


ci_len_99_2 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b0.png")


ci_len_99_3 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b0.png")

ci_len_99_4 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b0.png")

ci_len_99_5 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b0.png")

ci_len_99_6 %>%
  dplyr::select(name, b0_ci_len) %>%
  ggplot(aes(x = 1: length(b0_ci_len), y = b0_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B0, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b0.png")




ci_len_95_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b1.png")


ci_len_95_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b1.png")


ci_len_95_3 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b1.png")

ci_len_95_4 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b1.png")

ci_len_95_5 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b1.png")

ci_len_95_6 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b1.png")






ci_len_99_1 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b1.png")


ci_len_99_2 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b1.png")


ci_len_99_3 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b1.png")

ci_len_99_4 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b1.png")

ci_len_99_5 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b1.png")

ci_len_99_6 %>%
  dplyr::select(name, b1_ci_len) %>%
  ggplot(aes(x = 1: length(b1_ci_len), y = b1_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B1, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b1.png")





ci_len_95_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_1_b2.png")


ci_len_95_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_2_b2.png")


ci_len_95_3 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_3_b2.png")

ci_len_95_4 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_4_b2.png")

ci_len_95_5 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_5_b2.png")

ci_len_95_6 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_95_6_b2.png")






ci_len_99_1 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 20")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_1_b2.png")


ci_len_99_2 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 50")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_2_b2.png")


ci_len_99_3 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 100")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_3_b2.png")

ci_len_99_4 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 200")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_4_b2.png")

ci_len_99_5 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 500")+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_5_b2.png")

ci_len_99_6 %>%
  dplyr::select(name, b2_ci_len) %>%
  ggplot(aes(x = 1: length(b2_ci_len), y = b2_ci_len)) +
  geom_point()+
  facet_wrap(~name, scales = "free_x")+
  ggtitle("B2, n = 1000") +
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("ci_len_99_6_b2.png")


