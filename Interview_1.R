#Survival Analysis
#Import Dataset
library(readxl)
D1 <- read_excel("Desktop/Dataset_1.xlsx")
View(D1)

#Checking and Editing Dataset
D1$gender <- factor(D1$gender,
                    labels = c("female",
                               "male"))
D1$his_disease <- factor(D1$his_disease,
                         labels = c("no",
                                    "yes"))
D1$status <- factor(D1$status,
                    labels = c("censored",
                               "event"))
D1$intervention <- factor(D1$intervention,
                          labels = c("A",
                                     "B"))
D1["age_group"] = cut(D1$age, c(30, 40, 50, 60, Inf), 
                      c("30-40", "40-50", "50-60", ">60"), 
                      include.lowest=TRUE)
D1["weight_grp"] = cut(D1$weight, c(50, 75, 100, Inf),
                       c("50-75", "75-100", ">100"))
D1["VO2max_grp"] = cut(D1$VO2max, c(27, 36, 45, 54, 63),
                       c ("27-36", "36-45", "45-54", "54-63"))
summary(D1)

#Survival Analysis
#Log-rank test for Gender
surv_table_gender <- survfit(Surv(D1$time, D1$status) ~ D1$gender)
plot(surv_table_gender, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("bottom", inset = 0.1,
       c("female","male"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
D1$status <- as.numeric(D1$status)
logrank_peto_gender <- survdiff(Surv(D1$time,D1$status) ~ D1$gender, rho = 0)
logrank_peto_gender

#Log-rank test for Age
surv_table_age <- survfit(Surv(D1$time, D1$status) ~ D1$age_group)
plot(surv_table_age, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("top", inset = 0.1,
       c("30 yrs ~ 40 yrs",
         "40 yrs ~ 50 yrs",
         "50 yrs ~ 60 yrs",
         "greater than 60 yrs"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
logrank_peto_age <- survdiff(Surv(D1$time,D1$status) ~ D1$age, rho = 0)
logrank_peto_age

#Log-rank test for weight
surv_table_weight <- survfit(Surv(D1$time, D1$status) ~ D1$weight_grp)
plot(surv_table_weight, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("topright", inset = 0.1,
       c("50 kg ~ 75 kg",
         "75 kg ~ 100 kg",
         "greater than 100 kg"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
logrank_peto_weight <- survdiff(Surv(D1$time,D1$status) ~ D1$weight, rho = 0)
logrank_peto_weight

#Log-rank test for VO2max
surv_table_VO2 <- survfit(Surv(D1$time, D1$status) ~ D1$VO2max_grp)
plot(surv_table_VO2, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("topright", inset = 0.1,
       c("27 ~ 36",
         "36 ~ 45",
         "45 ~ 54",
         "54 ~ 63"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
logrank_peto_VO2 <- survdiff(Surv(D1$time,D1$status) ~ D1$VO2max, rho = 0)
logrank_peto_VO2

#Log-rank test for History Disease
surv_table_his <- survfit(Surv(D1$time, D1$status) ~ D1$his_disease)
plot(surv_table_his, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("topright", inset = 0.1,
       c("No History Disease",
         "With History Disease"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
D1$his_disease <- as.numeric(D1$his_disease)
logrank_peto_his <- survdiff(Surv(D1$time,D1$status) ~ D1$his_disease, rho = 0)
logrank_peto_his

#Log-rank test for intervention
surv_table_int <- survfit(Surv(D1$time, D1$status) ~ D1$intervention)
plot(surv_table_int, xlab = "Months",
     lty = 1:2, ylab = "Survival Probability", xmax = 104)
legend("topright", inset = 0.1,
       c("Intervention A",
         "Intervention B"),
       lty = 1:2,
       lwd = 2,
       cex = 1)
D1$intervention <- as.numeric(D1$intervention)
logrank_peto_int <- survdiff(Surv(D1$time,D1$status) ~ D1$intervention, rho = 0)
logrank_peto_int






#Linearity Test
#Age
skim(D1$age)
cox_age_grp <- coxph(Surv(time, status) ~ age_grp, data = D1)
summary(cox_age_grp)

age_coef = c(0, cox_age_grp$coef["30-40"], cox_age_grp$coef["40-50"], 
             cox_age_grp$coef["50-60"], cox_age_grp$coef[">60"])
age_mid = c(32, 36.5, 42.1, 59.6)
cox_age_grp1 <- coxph(Surv(time, status) ~ age, data = D1)
AIC(cox_age_grp)
AIC(cox_age_grp1)
#Using age as continuous variable


#Weight
skim(D1$weight)
cox_weight_grp <- coxph(Surv(time, status) ~ weight_grp, data = D1)
summary(cox_weight_grp)

weight_coef = c(0, cox_weight_grp$coef["50-75"], cox_weight_grp$coef["75-100"], 
             cox_weight_grp$coef[">100"])
age_mid = c(59.85, 74.45, 84.55, 102.45)
cox_weight_grp_c <- coxph(Surv(time, status) ~ weight, data = D1)
AIC(cox_weight_grp)
AIC(cox_weight_grp_c)
#Using weight as continous variable


#VO2max
skim(D1$VO2max)
cox_VO2_grp <- coxph(Surv(time, status) ~ VO2max_grp, data = D1)
summary(cox_VO2_grp)

VO2_coef = c(0, cox_VO2_grp$coef["27-36"], cox_VO2_grp$coef["36-45"],
             cox_VO2_grp$coef["45-54"], cox_VO2_grp$coef["54-63"])
VO2_mid = c(32, 39.65, 46, 55.9)
cox_VO2_grp_c <- coxph(Surv(time, status) ~ VO2max, data = D1)
AIC(cox_VO2_grp)
AIC(cox_VO2_grp_c)
#Using VO2 categorical






#Determine Hazard Ratios
#Effect of age along (NO)
cox_d1_age <- coxph(Surv(time, status) ~ age, data = D1)
summary(cox_d1_age)

#Effect of weight alone
cox_d1_weight <- coxph(Surv(time, status) ~ weight, data = D1)
summary(cox_d1_weight)

#Effect of gender alone
cox_d1_gender <- coxph(Surv(time, status) ~ gender, data = D1)
summary(cox_d1_gender)

#Effect of VO2 max (NO)
cox_d1_VO2 <- coxph(Surv(time, status) ~ VO2max_grp, data = D1)
summary(cox_d1_VO2)

#Effect of History Disease (NO)
cox_d1_his <- coxph(Surv(time, status) ~ his_disease, data = D1)
summary(cox_d1_his)

#Effect of Intervention alone
cox_d1_int <- coxph(Surv(time, status) ~ intervention, data = D1)
summary(cox_d1_int)

#Age, VO2max, Historical Disease will be excluded

#Construct base model
D1$gender <- factor(D1$gender)
cox_d1_i <- coxph(Surv(time, status) ~ weight + gender +
                                 intervention , data = D1)
summary(cox_d1_i)
Anova(cox_d1_i)