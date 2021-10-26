library(foreign)
library(haven)
library(tidyverse)
library(psych)
library(beepr)
library(lavaan)
library(xlsx)

trans<-read_sav("C:/Users/sarah/Desktop/Research/Maggi transphobia/Transgender IAT.public.2020.sav")

#select vars
colnames(trans)


trans_short<-trans %>% select(c(STATE, att7, Strans_0to100, Ttrans_0to10, policysupport1, policysupport2, policysupport3, policysupport4, policysupport5, policysupport6, policysupport7, policysupport8, policysupport9, policysupport10, policysupport11, policysupport12, transphobia1, transphobia2, transphobia3, transphobia4, transphobia5, transphobia6, transphobia7, transphobia8, transphobia9
))%>% filter(STATE %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")) 

colnames(trans_short)
# recode so high = more transphobic
trans_short$Strans_0to100_rc<-100-trans_short$Strans_0to100
trans_short$Ttrans_0to10_rc<-10-trans_short$Ttrans_0to10
trans_short$policysupport4_rc<-7-trans_short$policysupport4
trans_short$policysupport5_rc<-7-trans_short$policysupport5
trans_short$policysupport6_rc<-7-trans_short$policysupport6
trans_short$policysupport8_rc<-7-trans_short$policysupport8
trans_short$policysupport10_rc<-7-trans_short$policysupport10
trans_short$policysupport11_rc<-7-trans_short$policysupport11
trans_short$policysupport12_rc<-7-trans_short$policysupport12

summary(trans_short)


trans_short_std<-trans_short %>% select(c("STATE","att7", "Strans_0to100_rc", "Ttrans_0to10_rc", "policysupport1", "policysupport2", "policysupport3", "policysupport4_rc", "policysupport5_rc", "policysupport6_rc", "policysupport7", "policysupport8_rc", "policysupport9", "policysupport10_rc", "policysupport11_rc", "policysupport12_rc",  "transphobia1", "transphobia2", "transphobia3", "transphobia4", "transphobia5", "transphobia6", "transphobia7", "transphobia8", "transphobia9")) %>% mutate_at(c("att7", "Strans_0to100_rc", "Ttrans_0to10_rc", "policysupport1", "policysupport2", "policysupport3", "policysupport4_rc", "policysupport5_rc", "policysupport6_rc", "policysupport7", "policysupport8_rc", "policysupport9", "policysupport10_rc", "policysupport11_rc", "policysupport12_rc",  "transphobia1", "transphobia2", "transphobia3", "transphobia4", "transphobia5", "transphobia6", "transphobia7", "transphobia8", "transphobia9"), ~scale(.) %>% as.vector)

summary(trans_short_std)


# aggregate to state level

trans_short_state<-aggregate(trans_short_std[, 2:25], list(trans_short_std$STATE), mean, na.rm=TRUE)

as.data.frame(colnames(trans_short_state))
# assign NA for states w/ <50 obs for each

trans_short_state[which(trans_short_state$Group.1=="AK"), 5:16]<- NA

trans_short_state[which(trans_short_state$Group.1=="WY"), 5:25]<- NA

trans_short_state[which(trans_short_state$Group.1=="ND"), c(5, 8:16, 19, 22)]<- NA

trans_short_state[which(trans_short_state$Group.1=="SD"), c(5:16,20)]<- NA

trans_short_state[which(trans_short_state$Group.1=="MT"), c(18,23)]<- NA

view(trans_short_state)

trans_vars<-trans_short_state

FA_set<-data.frame(trans_vars$att7, 
                   trans_vars$Strans_0to100_rc, 
                   trans_vars$Ttrans_0to10_rc,
                   trans_vars$policysupport1,
                     trans_vars$policysupport2,
                     trans_vars$policysupport3,
                     trans_vars$policysupport4_rc, 
                     trans_vars$policysupport5_rc,
                     trans_vars$policysupport6_rc,
                     trans_vars$policysupport7,
                     trans_vars$policysupport8_rc,
                     trans_vars$policysupport9,
                     trans_vars$policysupport10_rc,
                     trans_vars$policysupport11_rc, 
                     trans_vars$policysupport12_rc, 
                     trans_vars$transphobia1, 
                     trans_vars$transphobia2, 
                     trans_vars$transphobia3, 
                     trans_vars$transphobia4, 
                     trans_vars$transphobia5, 
                     trans_vars$transphobia6, 
                     trans_vars$transphobia7, 
                     trans_vars$transphobia8, 
                     trans_vars$transphobia9)

FA_set<-FA_set %>% drop_na()
library(psych)
library(lavaan)
par(mar=c(1,1,1,1))

fa.parallel(FA_set)
# 1 factor solution suggested yaaaaay

efa1<-fa(FA_set, nfactors=1)
efa1

#
#Factor Analysis using method =  minres
#Call: fa(r = FA_set, nfactors = 1)
#Standardized loadings (pattern matrix) based upon correlation matrix
#                              MR1   h2   u2 com
#trans_vars.att7           0.86 0.74 0.26   1
#trans_vars.Strans_0to100     0.92 0.85 0.15   1
#trans_vars.Ttrans_0to10      0.93 0.87 0.13   1
#trans_vars.policysupport1 0.94 0.89 0.11   1
#trans_vars.policysupport2 0.92 0.85 0.15   1
#trans_vars.policysupport3 0.94 0.88 0.12   1
#trans_vars.policysupport4    0.91 0.84 0.16   1
#trans_vars.policysupport5    0.90 0.80 0.20   1
#trans_vars.policysupport6    0.91 0.82 0.18   1
#trans_vars.policysupport7 0.85 0.73 0.27   1
#trans_vars.policysupport8    0.94 0.88 0.12   1
#trans_vars.policysupport9 0.87 0.75 0.25   1
#trans_vars.policysupport10   0.85 0.72 0.28   1
#trans_vars.policysupport11   0.88 0.78 0.22   1
#trans_vars.policysupport12   0.95 0.90 0.10   1
#trans_vars.transphobia1   0.85 0.72 0.28   1
#trans_vars.transphobia2   0.93 0.86 0.14   1
#trans_vars.transphobia3   0.86 0.73 0.27   1
#trans_vars.transphobia4   0.85 0.72 0.28   1
#trans_vars.transphobia5   0.80 0.64 0.36   1
#trans_vars.transphobia6   0.88 0.77 0.23   1
#trans_vars.transphobia7   0.79 0.62 0.38   1
#trans_vars.transphobia8   0.94 0.89 0.11   1
#trans_vars.transphobia9   0.92 0.84 0.16   1

#                MR1
#SS loadings    19.1
#Proportion Var  0.8

#Mean item complexity =  1
#Test of the hypothesis that 1 factor is sufficient.

#The degrees of freedom for the null model are  276  and the objective function was  56.1 with Chi Square of  2028.82
#The degrees of freedom for the model are 252  and the objective function was  20.81 

#The root mean square of the residuals (RMSR) is  0.06 
#The df corrected root mean square of the residuals is  0.07 

#The harmonic number of observations is  46 with the empirical chi square  100.66  with prob <  1 
#The total number of observations was  46  with Likelihood Chi Square =  738.91  with prob <  2.5e-49 

#Tucker Lewis Index of factoring reliability =  0.689
#RMSEA index =  0.204  and the 90 % confidence intervals are  0.19 0.225
#BIC =  -225.91
#Fit based upon off diagonal values = 0.99
#Measures of factor score adequacy             
#                                                   MR1
#Correlation of (regression) scores with factors   1.00
#Multiple R square of scores with factors          0.99
#Minimum correlation of possible factor scores     0.99
# holy shit, those loaded SHOCKINGLY WELL



scored<-predict.psych(efa1, FA_set)
scored<-as.data.frame(scored)

colnames(trans_vars)
trans_vars_na<-trans_vars %>%  drop_na()

trans_FA<-as.data.frame(cbind(trans_vars_na, scored))%>%select(c(Group.1, att7, 
                   Strans_0to100_rc, 
                   Ttrans_0to10_rc,
                   policysupport1,
                   policysupport2,
                   policysupport3,
                   policysupport4_rc, 
                   policysupport5_rc,
                   policysupport6_rc,
                   policysupport7,
                   policysupport8_rc,
                   policysupport9,
                   policysupport10_rc,
                   policysupport11_rc, 
                   policysupport12_rc, 
                   transphobia1, 
                   transphobia2, 
                   transphobia3, 
                   transphobia4, 
                   transphobia5, 
                   transphobia6, 
                   transphobia7, 
                   transphobia8, 
                   transphobia9, MR1))


trans_FA<-trans_FA %>% rename( "transphobia_factor"="MR1")

save(trans_FA, file="C:/Users/sarah/Desktop/Research/Maggi transphobia/trans factor scores and vars NORMS 07_29.R")


library(openxlsx)
write.xlsx(trans_FA, "C:/Users/sarah/Desktop/Research/Maggi transphobia/trans factor scores and vars NORMS 07_29.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
