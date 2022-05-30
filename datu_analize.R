library(tidyverse)
library(MASS)
library(Epi)
library(melt)
# install.packages("bbmle")

dati <- readxl::read_excel("DNS_datiR.xlsx", na = "NA")
#faktorizacija

dati$AUC<-floor(dati$AU_dupl_kom) 


overdisp_fun <- function(model) {
  
  # model <- mod_p
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
  
}

p_val <- function(x){
  
  # x <- cor_no_ret_groups[,4]
  
  p <- NULL
  
  x <- as.numeric(as.matrix(x))
  
  for(i in 1 : length(x)){
    
    if (is.nan(x[i]) | is.na(x[i])){
      
      p[i] <- NaN
      
    } else if(round(x[i], 3) <= 0.001){
      
      p[i] <- "<0.001*"
      
    } else if(round(x[i], 3) <= 0.05){
      
      p[i] <- paste(round(x[i], 3), "*", sep = "")
      
    } else if(round(x[i], 3) == 1){
      
      p[i] <- ">0.999"
    } 
    
    else {
      p[i] <- round(x[i], 3)
    }
    
  }
  p
}


dati_nb <- dati %>% 
  dplyr::select(AUC, Diabets, nitriti_serums,
         nitrati_serums, Vecums, Dzimums,
         KMI, HbA1c, TG,
         Smeke2, glikoze, Kop_hol, AH, LDL) %>% 
  na.omit()

dati_nb %>% head() %>% 
  xtable::xtable()

dati_nb %>% 
  ggplot(aes(x = AUC))+
  geom_histogram(aes(y = ..density..), bins = 10)+
  xlab(NULL)+
  ylab(NULL)

ggsave("hist.png", scale = 0.7)

dati_nb %>% 
  ggplot(aes(x = as.factor(Diabets), y = AUC))+
  geom_boxplot()+
  xlab(NULL)+
  ylab(NULL)

ggsave("box.png", scale = 0.7)


mod_nb <- glm.nb(AUC ~ Diabets + Smeke2 + HbA1c + Kop_hol,
                 data=dati_nb)


mod_p <- glm(AUC ~ Diabets + Smeke2 + HbA1c + Kop_hol,
                 data=dati_nb, family = poisson)

mod_el <- el_glm(AUC ~ Diabets + Smeke2 + HbA1c + Kop_hol,
             data=dati_nb, family = poisson)


mod_qp <- glm(AUC ~ Diabets + Smeke2 + HbA1c + Kop_hol,
             data=dati_nb, family = quasipoisson)


y_el <- exp(3.4326718 + as.numeric(as.character(dati_nb$Diabets)) * 0.7893894 + 
              as.numeric(as.character(dati_nb$Smeke2)) * (-0.4171920) +
              dati_nb$HbA1c * (-0.1160056) + dati_nb$Kop_hol * 0.1755498)


pearson_chisq <- sum((dati_nb$AUC - y_el)^2/y_el)
pchisq(pearson_chisq, df=60, lower.tail=FALSE)

# Pearson.chisq <- sum(rp^2)
# prat <- Pearson.chisq/rdf
# pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)


pearson_chisq <- sum((dati_nb$AUC - y_el)^2/y_el)

pearson_p <- as.data.frame(rbind(overdisp_fun(mod_p)[4],
overdisp_fun(mod_qp)[4],
overdisp_fun(mod_nb)[4],
pchisq(pearson_chisq, df=60, lower.tail=FALSE)))


names(pearson_p) <- "p"

pearson_p <- pearson_p %>% 
  mutate(p = p_val(p))

row.names(pearson_p) <- c("P", "QP", "NB", "EL")

pearson_p %>% 
  xtable::xtable()
coef_p_ci <- as.data.frame(cbind(ci.exp(mod_p), summary(mod_p)$coefficients[,4]))
coef_qp_ci <- as.data.frame(cbind(ci.exp(mod_qp), summary(mod_qp)$coefficients[,4]))
coef_nb_ci <- as.data.frame(cbind(ci.exp(mod_nb), summary(mod_nb)$coefficients[,4]))
coef_el_ci <- as.data.frame(cbind(exp(mod_el@coefficients), exp(confint(mod_el)), summary(mod_el)@parMatrix[,3]))

nam <- c("exp_beta", "lower", "upper", "p")

names(coef_p_ci) <- nam
names(coef_qp_ci) <- nam
names(coef_nb_ci) <- nam
names(coef_el_ci) <- nam

P <- coef_p_ci %>% 
  dplyr::mutate(exp_beta = paste(round(exp_beta, 2), " (", round(lower, 1), "-", round(upper, 2), ")", sep = ""),
                p = p_val(p)) %>% 
  dplyr::select(exp_beta, p)

QP <- coef_qp_ci %>% 
  dplyr::mutate(exp_beta = paste(round(exp_beta, 2), " (", round(lower, 1), "-", round(upper, 2), ")", sep = ""),
                p = p_val(p)) %>% 
  dplyr::select(exp_beta, p)

NB <- coef_nb_ci %>% 
  dplyr::mutate(exp_beta = paste(round(exp_beta, 2), " (", round(lower, 1), "-", round(upper, 2), ")", sep = ""),
                p = p_val(p)) %>% 
  dplyr::select(exp_beta, p)

EL <- coef_el_ci %>% 
  dplyr::mutate(exp_beta = paste(round(exp_beta, 2), " (", round(lower, 1), "-", round(upper, 2), ")", sep = ""),
                p = p_val(p)) %>% 
  dplyr::select(exp_beta, p)




openxlsx::write.xlsx(P, "Poisson.xlsx", row.names = TRUE, overwrite = TRUE)
openxlsx::write.xlsx(QP, "QP.xlsx", row.names = TRUE, overwrite = TRUE)
openxlsx::write.xlsx(NB, "NB.xlsx", row.names = TRUE, overwrite = TRUE)
openxlsx::write.xlsx(EL, "EL.xlsx", row.names = TRUE, overwrite = TRUE)



ggplot(aes(x = log(fitted(mod_p)),
           log((AUC-fitted(mod_p))^2)), data = dati_nb)+
  geom_point()+
  xlab(expression(hat(lambda)))+
  ylab(expression((y-hat(lambda))^2))+
  geom_abline(slope = 1,intercept = 0)

ggsave("var_p.png", scale = 0.7)


ggplot(aes(x = log(fitted(mod_qp)),
           log((AUC-fitted(mod_qp))^2)), data = dati_nb)+
  geom_point()+
  xlab(expression(hat(lambda)))+
  ylab(expression((y-hat(lambda))^2))+
  geom_abline(slope = 1,intercept = 0)

ggsave("var_qp.png", scale = 0.7)


ggplot(aes(x = log(fitted(mod_nb)),
           log((AUC-fitted(mod_nb))^2)), data = dati_nb)+
  geom_point()+
  xlab(expression(hat(lambda)))+
  ylab(expression((y-hat(lambda))^2))+
  geom_abline(slope = 1,intercept = 0)

ggsave("var_nb.png", scale = 0.7)



ggplot(aes(x = log(y_el),
           log((AUC-y_el)^2)), data = dati_nb)+
  geom_point()+
  xlab(expression(hat(lambda)))+
  ylab(expression((y-hat(lambda))^2))+
  geom_abline(slope = 1,intercept = 0)

ggsave("var_el.png", scale = 0.7)