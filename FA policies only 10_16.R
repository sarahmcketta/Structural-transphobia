library(foreign)
library(haven)
library(tidyverse)
library(psych)
library(beepr)
library(lavaan)
library(xlsx)


laws<-read.xlsx("C:/Users/sarah/Desktop/Research/Maggi transphobia/MAP GI Data for SM 2-23-21.xlsx", sheetName="map_gi_tally")
colnames(laws)

laws$rp_sub_rc<--1*laws$rp_sub
laws$dis_sub_rc<--1*laws$dis_sub
laws$rel_sub_rc<--1*laws$rel_sub
laws$you_sub_rc<--1*laws$you_sub
laws$hlt_sub_rc<--1*laws$hlt_sub
laws$cj_sub_rc<--1*laws$cj_sub
laws$id_sub_rc<--1*laws$id_sub


cor(laws$rp_sub_rc, laws$rp_sub)
cor(laws$dis_sub_rc, laws$dis_sub)
cor(laws$rel_sub_rc, laws$rel_sub)
cor(laws$you_sub_rc, laws$you_sub)
cor(laws$hlt_sub_rc, laws$hlt_sub)
cor(laws$cj_sub_rc, laws$cj_sub)
cor(laws$id_sub_rc, laws$id_sub)

FA_set<-data.frame(laws$rp_sub_rc, 
                     laws$dis_sub_rc, 
                     laws$rel_sub_rc, 
                     laws$you_sub_rc, 
                     laws$hlt_sub_rc, 
                     laws$cj_sub_rc, 
                     laws$id_sub_rc)
FA_set<-FA_set %>% drop_na()
library(psych)
library(lavaan)
par(mar=c(1,1,1,1))

fa.parallel(FA_set)
# 1 factor solution suggested yaaaaay

efa1<-fa(FA_set, nfactors=1)
efa1
#Factor Analysis using method =  minres
#Call: fa(r = FA_set, nfactors = 1)
#Standardized loadings (pattern matrix) based upon correlation matrix
#              MR1   h2    u2 com
#laws.rp_sub  0.74 0.55 0.448   1
#laws.dis_sub 0.88 0.77 0.225   1
#laws.rel_sub 0.40 0.16 0.840   1
#laws.you_sub 0.96 0.92 0.076   1
#laws.hlt_sub 0.84 0.70 0.299   1
#laws.cj_sub  0.85 0.71 0.285   1
#laws.id_sub  0.77 0.60 0.403   1

#                MR1
#SS loadings    4.42
#Proportion Var 0.63

#Mean item complexity =  1
#Test of the hypothesis that 1 factor is sufficient.

#The degrees of freedom for the null model are  21  and the objective function was  5.53 with Chi Square of  258.99
#The degrees of freedom for the model are 14  and the objective function was  0.41 

#The root mean square of the residuals (RMSR) is  0.04 
#The df corrected root mean square of the residuals is  0.05 

#The harmonic number of observations is  51 with the empirical chi square  3.92  with prob <  1 
#The total number of observations was  51  with Likelihood Chi Square =  18.76  with prob <  0.17 

#Tucker Lewis Index of factoring reliability =  0.97
#RMSEA index =  0.079  and the 90 % confidence intervals are  0 0.17
#BIC =  -36.28
#Fit based upon off diagonal values = 1
#Measures of factor score adequacy             
                  #                                 MR1
#Correlation of (regression) scores with factors   0.99
#Multiple R square of scores with factors          0.97
#Minimum correlation of possible factor scores     0.94

# holy shit, those loaded SHOCKINGLY WELL
# I'm going to remove the ones <.6 because why not, yeah?
# remove rel_sub

FA_set2<-data.frame(
                   laws$rp_sub_rc, 
                   laws$dis_sub_rc, 
                   laws$you_sub_rc, 
                   laws$hlt_sub_rc, 
                   laws$cj_sub_rc, 
                   laws$id_sub_rc)

FA_set2<-FA_set2 %>% drop_na()

summary(FA_set2)
fa.parallel(FA_set2)
# 1 factor solution

efa2<-fa(FA_set2, nfactors=1)
efa2
#Factor Analysis using method =  minres
#Call: fa(r = FA_set2, nfactors = 1)
#Standardized loadings (pattern matrix) based upon correlation matrix
#              MR1   h2    u2 com
#laws.rp_sub  0.75 0.56 0.444   1
#laws.dis_sub 0.88 0.77 0.233   1
#laws.you_sub 0.95 0.90 0.097   1
#laws.hlt_sub 0.84 0.70 0.301   1
#laws.cj_sub  0.85 0.72 0.277   1
#laws.id_sub  0.78 0.61 0.385   1

#                MR1
#SS loadings    4.26
#Proportion Var 0.71#

#Mean item complexity =  1
#Test of the hypothesis that 1 factor is sufficient.#

#The degrees of freedom for the null model are  15  and the objective function was  5.22 with Chi Square of  246.17
#The degrees of freedom for the model are 9  and the objective function was  0.28 

#The root mean square of the residuals (RMSR) is  0.04 
#The df corrected root mean square of the residuals is  0.05 

#The harmonic number of observations is  51 with the empirical chi square  2.32  with prob <  0.99 
#The total number of observations was  51  with Likelihood Chi Square =  12.88  with prob <  0.17 

#Tucker Lewis Index of factoring reliability =  0.972
#RMSEA index =  0.09  and the 90 % confidence intervals are  0 0.198
#BIC =  -22.5
#Fit based upon off diagonal values = 1
#Measures of factor score adequacy             
#                                                   MR1
#Correlation of (regression) scores with factors   0.98
#Multiple R square of scores with factors          0.96
#Minimum correlation of possible factor scores     0.92

cor(FA_set2)


scored<-predict.psych(efa2, FA_set2)
scored<-as.data.frame(scored)


policy_FA<-as.data.frame(cbind(laws[,1:2], scored))

policy_FA<-policy_FA %>% rename( "trans_policy_factor"="MR1")



library(openxlsx)
write.xlsx(policy_FA, "C:/Users/sarah/Desktop/Research/Maggi transphobia/trans policy factor scores reverse scored 10_16.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
