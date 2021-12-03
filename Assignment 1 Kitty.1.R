#Assignment 1 part 1 

#Basics 

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 

Data1 <- data_sample_1
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(psych)
library(gridExtra)
library(lm.beta)

describe(Data1)

Data1 %>%
  summary()

Data1 %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Model1 <- lm(pain ~ age + sex, data = Data1)

summary(Model1)

Model1 %>%
  plot(which = 4)

Model1 %>%
  plot(which = 2)

Model1 %>%
  residualPlots()

residuals_Model1 = enframe(residuals(Model1))
residuals_Model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

#Removal of outliers

Data_corrected <- Data1 %>%
  slice(-c(88, 34))

describe(Data_corrected)

Model1_corrected <- lm(pain ~ age + sex, data = Data_corrected)

summary(Model1_corrected)
confint(Model1_corrected)
lm.beta(Model1_corrected)
AIC(Model1_corrected)


coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(Model1_corrected)

#Assumptions 

Model1_corrected %>%
  plot(which = 4)

Model1_corrected %>%
  plot(which = 2)

Model1_corrected %>%
  residualPlots()

Model1_corrected %>%
  plot(which = 3)

Model1_corrected %>%
  ncvTest()

Model1_corrected %>%
  bptest()

Model1_corrected %>%
  vif()

Model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = Data_corrected)

summary(Model2)

#Assumptions etc. 

Model2 %>%
  plot(which = 4)

Model2 %>%
  plot(which = 5)

Model2 %>%
  plot(which = 2)

Model2 %>%
  residualPlots()

Model2 %>%
  plot(which = 3)

Model2 %>%
  ncvTest()

Model2 %>%
  bptest()

Model2 %>%
  vif()

#Cortisol serum vs cortisol saliva 

Model2.0 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +  mindfulness, data = Data_corrected)

Model2.0 %>%
  vif()

Model2.1 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva +  mindfulness, data = Data_corrected)

Model2.1 %>%
  vif()

Data_corrected %>%
  select(cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

summary(Model2.0)
summary(Model2.1)

Data_corrected %>%
  ggplot() +
  aes(x = cortisol_saliva, y = cortisol_serum, fill = pain) +
  geom_violin() +
  geom_jitter(width = 0.2)

anova(Model2.0, Model2.1)
anova(Model2.0, Model1_corrected)
anova(Model2.1, Model1_corrected)

summary(Model2.0)$adj.r.squared
AIC(Model2.0)
summary(Model2.1)$adj.r.squared
AIC(Model2.1)
summary(Model1_corrected)$adj.r.squared
AIC(Model1_corrected)

confint(Model2.0)
summary(Model2.0)
lm.beta(Model2.0)

coef_table(Model2.0)

#Assignment part 2 

ModelX <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = Data_corrected)

ModelX %>%
  summary()

AIC(ModelX)

ModelX %>%
  plot(which = 4)

ModelX %>%
  plot(which = 5)

ModelX %>%
  plot(which = 2)

ModelX %>%
  residualPlots()

ModelX %>%
  plot(which = 3)

ModelX %>%
  ncvTest()

ModelX %>%
  bptest()

ModelX %>%
  vif()

anova(ModelX, Model1_corrected)

#theory & backward model etc 

Theorybased_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = Data_corrected)
Initial.Model <- lm(pain ~ age + pain_cat + STAI_trait + mindfulness + cortisol_serum + weight + IQ + household_income, data = Data_corrected)
Backward_Model.1 = step(Initial.Model, direction = "backward")

summary(Initial.Model)
summary(Backward_Model.1)
summary(Theorybased_model)

lm.beta(Backward_Model.1)
confint(Backward_Model.1)

coef_table(Backward_Model.1)

anova(Theorybased_model, Backward_Model.1)
anova(Backward_Model.1, Initial.Model)
AIC(Theorybased_model)
AIC(Backward_Model.1)
AIC(Initial.Model)

#New Data 

home_sample_2.csv = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

Data2 <- home_sample_2.csv

Theorybased_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = Data2)
summary(Theorybased_model)

Backward_Model.1 <- lm(pain  ~  age + mindfulness + cortisol_serum + pain_cat, data = Data2)
summary(Backward_Model.1)

Backward_Model.1 = step(Initial.Model, direction = "backward")

Predict_theorybased <- predict(Theorybased_model, Data2)
Predict_back <- predict(Backward_Model.1, Data2) 

anova(Backward_Model.1, Theorybased_model)
                 
RSS_theorybased = sum((Data2[,"pain"] - Predict_theorybased)^2) 
RSS_back = sum((Data2[,"pain"] - Predict_back)^2) 

RSS_theorybased    
RSS_back

AIC(Theorybased_model)
AIC(Backward_Model.1)

anova(Backward_Model.1, Theorybased_model)
anova(Backward_Model.1, ModelX)

summary(Theorybased_model)
summary(Backward_Model.1)

confint(Theorybased_model)
lm.beta(Theorybased_model)
confint(Backward_Model.1)
lm.beta(Backward_Model.1)

coef_table(Backward_Model.1)


