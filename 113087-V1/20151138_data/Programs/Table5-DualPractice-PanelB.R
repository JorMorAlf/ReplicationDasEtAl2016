### Replication of Table 5 - Dual Practice - Panel B ##
## Produces a Latex file with the output, replicates original results 

# Load necessary packages
library(haven)
library(stats)
library(sandwich)
library(lmtest)
library(stargazer)

## Need to change the directory 
setwd("/Users/jorgeantoniomorales/Documents/GitHub/ReplicationDasEtAl2016/113087-V1/20151138_data")

data2 <- read_dta("Data/SPDataset.dta")
data <-  subset(data2, round == 2 & dual==1 & case != 2 )
yvars <- c("correct_treat", "helpful_treat", "wrong_treat", "correct_only", "antibiotic", "totalmeds")

# Specify the cluster variable
clusters <- data$finprovid
weights <- data$wt
distid <- factor(data$distid)

model_ct <- lm(correct_treat ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
               data = data )
vcov_ct <- vcovCL(model_ct, cluster = clusters,  type="HC1")
summary_model_ct <- coeftest(model_ct, vcov = vcov_ct)
n_obs_ct<- nobs(model_ct)


#Helpful Treat
model_ht <- lm(helpful_treat ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
               data = data )
vcov_ht <- vcovCL(model_ht, cluster = clusters,  type="HC1")
summary_model_ht <- coeftest(model_ht, vcov = vcov_ht)
n_obs_ht<- nobs(model_ht)


#Wrong Treat 
model_wt <- lm(wrong_treat ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
               data = data )
vcov_wt <- vcovCL(model_wt, cluster = clusters,  type="HC1")
summary_model_wt <- coeftest(model_wt, vcov = vcov_wt)
n_obs_wt<- nobs(model_wt)


#Correct Only 
model_co <- lm(correct_only ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
               data = data )
vcov_co <- vcovCL(model_co, cluster = clusters,  type="HC1")
summary_model_co <- coeftest(model_co, vcov = vcov_co)
n_obs_co<- nobs(model_co)


#Antiobiotic
model_ant <- lm(antibiotic ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
               data = data )
vcov_ant <- vcovCL(model_ant, cluster = clusters,  type="HC1")
summary_model_ant <- coeftest(model_ant, vcov = vcov_ant)
n_obs_ant<- nobs(model_ant)

#Total Med
model_tm<- lm(totalmeds ~ private + case3 + ssp2 + ssp3 + ssp4 + ssp5 + ssp6 + ssp7 + ssp8 + ssp9 + ssp10 + ssp11 + ssp12 + ssp13 + ssp14 + ssp15 +factor(distid) ,
                data = data )
vcov_tm <- vcovCL(model_tm, cluster = clusters,  type="HC1")
summary_model_tm <- coeftest(model_tm, vcov = vcov_tm)
n_obs_tm<- nobs(model_tm)


obs_line1 <- c("Observations", n_obs_ct, n_obs_ht, n_obs_wt, n_obs_co )
obs_line2 <- c("Observations",  n_obs_ant, n_obs_tm )
r2_1 <- c("R2", round(summary(model_ct)$r.squared,3), round(summary(model_ht)$r.squared,3), round(summary(model_wt)$r.squared,3) , round(summary(model_co)$r.squared,3))
r2_2 <- c("R2", round(summary(model_ant)$r.squared,3), round(summary(model_tm)$r.squared,3))

# Report results with stargazer
table1<-stargazer(summary_model_ct, summary_model_ht, summary_model_wt, summary_model_co,  
                 type = "latex", digits = 3 , add.lines=list( obs_line1, r2_1) , keep="private" , 
                 style="aer", title="Table 5 - Panel B",  column.labels   = c("Correct Treatment", "Palliative Treatment", "Unnecessary treatment", "Correct treatment only"))
                 
table2<-stargazer( summary_model_ant , summary_model_tm,  type = "latex", digits = 3 , 
                   add.lines=list( obs_line2, r2_2) , keep="private" , style="aer", 
                   title="Table 5 - Panel B",   column.labels = c("Antibiotic", "Number of medicines" ))



