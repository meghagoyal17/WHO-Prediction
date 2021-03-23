#Import Cleaned Dataset and libraries that we may need
library(readr)
library(tidyverse)
library(caret)
library(broom)

led.cleaned <- read.csv(file.choose(), sep=',')
View(led.cleaned)

#Creating bins for income composition
bins <- c(0, 0.4, 0.6, 0.8, 1.0)
names <- c("Low", "Medium","Medium-High", "High")

led.cleaned$Income.composition.categorical <- cut(led.cleaned$Income.composition.of.resources, breaks = bins, labels = names)
led.cleaned

#Create dataframes for each category so that we can run separate models on each

led.low<-filter(led.cleaned, Income.composition.categorical == "Low")
led.medium<-filter(led.cleaned, Income.composition.categorical == "Medium")
led.medium.high<-filter(led.cleaned, Income.composition.categorical == "Medium-High")
led.high<-filter(led.cleaned, Income.composition.categorical == "High")

#Models for low Income composition group
set.seed(142)
dt <- sort(sample(nrow(led.low), nrow(led.low)*0.8))
train.low <- led.low[dt, ]
test.low <- led.low[-dt,]

model1.low <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                   Alcohol+percentage.expenditure+Measles+BMI+Polio+
                   Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                   Income.composition.of.resources+Schooling+
                   under.five.deaths + HIV.AIDS + 
                   thinness.five.to.nine.years, data = train.low)
summary(model1.low)


#perform a stepwise variable selection
library(olsrr)
ols_step_both_p(model.low1)

#Correlation
data_num <- led.low %>% 
  select_if(is.numeric)
correlation <-cor(data_num, use="complete.obs")
correlation

#Model with only those variables selected from stepwise
model2.low <- lm(Life.expectancy ~ Adult.Mortality+under.five.deaths+Alcohol+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+HIV.AIDS, data = train.low)
summary(model2.low)

#Polynomials model
model3.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                   poly(HIV.AIDS, 3, raw = TRUE), data = train.low)
summary(model3.low)

#Polynomial with interactions model
model4.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                   I(HIV.AIDS^2) + I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.low)
summary(model4.low)
#In the above model, both the interactions of Alcohol and HIV aids with Adult mortality is very significant and overall R square value is increase to 0.8323 from 0.76

model5.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                   I(HIV.AIDS^2) + I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality +Adult.Mortality*Income.composition.of.resources+ Alcohol*Adult.Mortality, data = train.low)
summary(model5.low)
#Adult mortality with income composition is also significant but doesnt improves models that much

library(broom)
glance(model1.low)
glance(model2.low)
glance(model3.low)
glance(model4.low)
glance(model5.low)
#Model 5 seems best model with highest R square value and lowest AIC

anova(model4.low, model5.low) #Model5 and model 4 differs and model5 is better
plot(model5.low)


predictions.low <- model5.low %>% predict(led.low)
library(modelr)
data.frame(
  R2 = rsquare(model5.low, data = train.low),
  RMSE = rmse(model5.low, data = train.low),
  MAE = mae(model5.low, data = train.low))
data.frame(
  R2 = R2(predictions.low, led.low$Life.expectancy),
  RMSE = RMSE(predictions.low, led.low$Life.expectancy),
  MAE = MAE(predictions.low, led.low$Life.expectancy)
)
#Model 5 fits testing data well like training data

library(lattice)
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
get.rmse <- function(model, data, response) {
  rmse(actual = data[, response], 
       predicted = predict(model, data))
}
#Compile all models
model_list = list(model1.low,model2.low,model3.low,model4.low,model5.low)
#obtain train RMSE, test RMSE
train_rmse = sapply(model_list, get.rmse, data = train.low, response = "Life.expectancy")
test_rmse = sapply(model_list, get.rmse, data = test.low, response = "Life.expectancy")



#       ******Create very preliminary model for category MEDIUM *********
set.seed(142)
dt <- sort(sample(nrow(led.medium), nrow(led.medium)*0.8))
train.medium <- led.medium[dt, ]
test.medium <- led.medium[-dt,]

model1.medium <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                      Alcohol+percentage.expenditure+Measles+BMI+Polio+
                      Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                      Income.composition.of.resources+Schooling+
                      under.five.deaths + HIV.AIDS + 
                      thinness.five.to.nine.years, data = train.medium)
summary(model1.medium)


#perform a stepwise variable selection
library(olsrr)
ols_step_both_p(model1.medium)

#Model with only those variables selected from stepwise
model2.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      HIV.AIDS, data = train.medium)
summary(model2.medium)

#Polynomials model- little betterment
model3.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      poly(HIV.AIDS, 3, raw = TRUE), data = train.medium)
summary(model3.medium)

#Polynomial with interactions model 
model4.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      I(HIV.AIDS^2) + I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.medium)
summary(model4.medium)
#In the above model, both the interactions of Alcohol and HIV aids with Adult mortality is very significant and overall R square value is increase to 0.8323 from 0.76

library(broom)
glance(model1.medium)
glance(model2.medium)
glance(model3.medium)
glance(model4.medium)

#Model 4 seems best model with highest R square value and lowest AIC

anova(model4.medium, model3.medium) #Model3 and model 4 does not differs 
plot(model4.medium)


predictions.medium <- model4.medium %>% predict(led.medium)
library(modelr)
data.frame(
  R2 = rsquare(model4.medium, data = train.medium),
  RMSE = rmse(model4.medium, data = train.medium),
  MAE = mae(model4.medium, data = train.medium))
data.frame(
  R2 = R2(predictions.medium, led.medium$Life.expectancy),
  RMSE = RMSE(predictions.medium, led.medium$Life.expectancy),
  MAE = MAE(predictions.medium, led.medium$Life.expectancy)
)
#Model 4 fits testing data well like training data with R square little lower and RMSE and MAE higher

#       ******Create very preliminary model for category MEDIUM_HIGH *********


set.seed(142)
dt <- sort(sample(nrow(led.medium.high), nrow(led.medium.high)*0.8))
train.medium.high <- led.medium.high[dt, ]
test.medium.high <- led.medium.high[-dt,]

model1.medium.high <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                           Alcohol+percentage.expenditure+Measles+BMI+Polio+
                           Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                           Income.composition.of.resources+Schooling+
                           under.five.deaths + HIV.AIDS + 
                           thinness.five.to.nine.years, data = train.medium.high)
summary(model1.medium.high)


#perform a stepwise variable selection
library(olsrr)
ols_step_both_p(model1.medium.high)

#Model with only those variables selected from stepwise
model2.medium.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+BMI+Polio +Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources
                         , data = train.medium.high)
summary(model2.medium.high)

#With interactions model 
model3.medium.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+BMI+Polio +Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources
                         + HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.medium.high)
summary(model3.medium.high)
#In the above model,interactions of Adult mortality and HIV aids is not significant whereas with Alcohol is very significant and overall R square value is increase to 0.8323 from 0.76

library(broom)
glance(model1.medium.high)
glance(model2.medium.high)
glance(model3.medium.high)


#Model 3 seems best model with highest R square value and lowest AIC

anova(model3.medium.high, model2.medium.high) #Model3 and model 4 significantly differs 
plot(model3.medium.high)


predictions.medium.high <- model3.medium.high %>% predict(led.medium.high)
library(modelr)
data.frame(
  R2 = rsquare(model3.medium.high, data = train.medium.high),
  RMSE = rmse(model3.medium.high, data = train.medium.high),
  MAE = mae(model3.medium.high, data = train.medium.high))
data.frame(
  R2 = R2(predictions.medium.high, led.medium.high$Life.expectancy),
  RMSE = RMSE(predictions.medium.high, led.medium.high$Life.expectancy),
  MAE = MAE(predictions.medium.high, led.medium.high$Life.expectancy)
)
#Model 3 fits testing data well like training data

#       ******Create very preliminary model for category HIGH *********


set.seed(142)
dt <- sort(sample(nrow(led.high), nrow(led.high)*0.8))
train.high <- led.high[dt, ]
test.high <- led.high[-dt,]

model1.high <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                    Alcohol+percentage.expenditure+Measles+BMI+Polio+
                    Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                    Income.composition.of.resources+Schooling+
                    under.five.deaths + HIV.AIDS + 
                    thinness.five.to.nine.years, data = train.high)
summary(model1.high)
alias( lm( Life.expectancy ~ Adult.Mortality+infant.deaths+
             Alcohol+percentage.expenditure+Measles+BMI+Polio+
             Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
             Income.composition.of.resources+Schooling+
             under.five.deaths + 
             thinness.five.to.nine.years, data = train.high ) )
#Very low R2 and warning - there are aliased coefficients in the model because perfect multicollinearity is seen
library(olsrr)
ols_vif_tol(model1.high)

model1.high <- lm(Life.expectancy ~ Adult.Mortality+
                    Alcohol+percentage.expenditure+Measles+BMI+
                    Total.expenditure+Diphtheria+
                    Income.composition.of.resources+Schooling+
                    under.five.deaths + HIV.AIDS + 
                    thinness.five.to.nine.years, data = train.high)
summary(model1.high)

#Because of low R-square value, I checked the plots and found that life expectancy forms random pattern with
#predictors and there is not that good relation- this may be because of discrepancies in data like Hiv aids is constant  for all values
#So its standard deviation is zero
ggplot(led.high, aes(Adult.Mortality, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Income.composition.of.resources, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Schooling, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(HIV.AIDS, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Total.expenditure, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(under.five.deaths, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Alcohol, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Total.expenditure, Life.expectancy)) + 
  geom_jitter()

#perform a stepwise variable selection
library(olsrr)
ols_step_both_p(model1.high)

#Correlation
data_num <- led.high %>% 
  select_if(is.numeric)
correlation <-cor(data_num, use="complete.obs")
correlation
table(led.high$HIV.AIDS)
#Model with only those variables selected from stepwise
model2.high <- lm(Life.expectancy ~ Adult.Mortality+under.five.deaths+Measles+Alcohol+Total.expenditure+thinness.five.to.nine.years+Income.composition.of.resources, data = train.high)
summary(model2.high)

#Polynomial with interactions model
model3.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                    Alcohol*Adult.Mortality, data = train.high)
summary(model3.high)
#In the above model, both the interactions of Alcohol and HIV aids with Adult mortality is very significant and overall R square value is increase to 0.8323 from 0.76

model4.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                    Alcohol*Adult.Mortality +Adult.Mortality*Income.composition.of.resources, data = train.high)
summary(model4.high)
#Adult mortality with income composition is not significant and doesnt improves.

library(broom)
glance(model1.high)
glance(model2.high)
glance(model3.high)
glance(model4.high)
#Model 3 seems best model with highest R square value and lowest AIC

anova(model3.high, model4.high) #Model3 and model 4 does not differs but model 3 is better because of low AIC


predictions.high <- model3.high %>% predict(led.high)
library(modelr)
data.frame(
  R2 = rsquare(model3.high, data = train.high),
  RMSE = rmse(model3.high, data = train.high),
  MAE = mae(model3.high, data = train.high))
data.frame(
  R2 = R2(predictions.high, led.high$Life.expectancy),
  RMSE = RMSE(predictions.high, led.high$Life.expectancy),
  MAE = MAE(predictions.high, led.high$Life.expectancy)
)
#Model 3 fits testing data well like training data


#----------------Running the above best models on original dataset-----------------

#Best Model 5 of low income countries on whole dataset;
led <- read.csv(file.choose())
m1 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.5.9.years+Income.composition.of.resources+
                   I(HIV.AIDS^2) + I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality +Adult.Mortality*Income.composition.of.resources+ Alcohol*Adult.Mortality, data = led)
summary(m1)

#Best Model 4 of medium income countries on whole dataset;
m2 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      I(HIV.AIDS^2)+I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = led)
summary(m2)

#Best Model 3 of medium-high income countries on whole dataset;
m3 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+BMI+Polio +Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources
                         + HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = led)
summary(m3)

#Best Model 3 of high income countries on whole dataset;
m4 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.5.9.years+Income.composition.of.resources+
                    Alcohol*Adult.Mortality, data = led)
summary(m4) #R2 value os 0.7375 in whole dataset as compared to 0.53 in high-income group

#Overall all models performed quite better on the whole dataset

