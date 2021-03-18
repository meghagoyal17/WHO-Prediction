
#==============================Functions========================================
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  print(qnt)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  print(caps)
  H <- 1.5 * IQR(x, na.rm = T)
  print(H)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  print(x)
  return(x)
}
#=================================Libraries=====================================
library(readr)
install.packages('psych')
library(psych)
install.packages("dlookr", dep = TRUE)
library(dlookr)
library(tidyr)
library(mice)
library(tidyverse)
library(caret)
library(caTools)
install.packages("corrr")
library(corrr)
install.packages("corrplot")
library(corrplot)
library(tidyr)
#================================Reading File===================================
led <- read_csv("C:\\Users\\joseb\\Documents\\GitHub\\R\\WHO Prediction\\Life Expectancy Data.csv")
led <- Life.Expectancy.Data


# to fill the income composition of resources for USA and BAHAMAS
HDI <- Human_Development_Index_HDI_
HDI[is.na(HDI)] <- 0
led[is.na(led)] <- 0
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2000 & 
                                      led$Country=='Bahamas']<- HDI$'2000'[HDI$Country=='Bahamas']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2000 & 
                                      led$Country=='United States of America']<- HDI$'2000'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2001 & 
                                      led$Country=='United States of America']<- HDI$'2001'[HDI$Country=='United States']

led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2002 & 
                                      led$Country=='United States of America']<- HDI$'2002'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2003 & 
                                      led$Country=='United States of America']<- HDI$'2003'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2004 & 
                                      led$Country=='United States of America']<- HDI$'2004'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2005 & 
                                      led$Country=='United States of America']<- HDI$'2005'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2006 & 
                                      led$Country=='United States of America']<- HDI$'2006'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2007 & 
                                      led$Country=='United States of America']<- HDI$'2007'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2008 & 
                                      led$Country=='United States of America']<- HDI$'2008'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2009 & 
                                      led$Country=='United States of America']<- HDI$'2009'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2010 & 
                                      led$Country=='United States of America']<- HDI$'2010'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2011 & 
                                      led$Country=='United States of America']<- HDI$'2011'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2012 & 
                                      led$Country=='United States of America']<- HDI$'2012'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2013 & 
                                      led$Country=='United States of America']<- HDI$'2013'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2014 & 
                                      led$Country=='United States of America']<- HDI$'2014'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2015 & 
                                      led$Country=='United States of America']<- HDI$'2015'[HDI$Country=='United States']






#================================Correlation====================================

#to perform correlation on all numerical variables we need to drop 2 categorical columns, we also need to get rid of na values (deleted for now)

led.train.num = subset(led, select = -c(Country, Status))
led.num1<- led.train.num %>% drop_na()
#Correlation without dropping na values
res.cor <- correlate(led.train.num)
res.cor
#Check Correlation of all columns with respect to life expectancy 
res.cor %>% 
  focus(Life.expectancy)

# for those who do not have the library install.packages("corrplot")
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

clean_LED <- clean_LED %>% drop_na(Life.expectancy)


#=========================Cleaning Pipeline=====================================
#Cleaning Pipeline

#Drops all the columns that are not going to be used
drops<-c("Year","Status","infant deaths","percentage expenditure","Hepatitis B","under-five","HIV/AIDS","GDP","Population","thinness 5-9 years")
led_prep<-led[,!(names(led)%in%drops)]
led_prep[is.na(led_prep)]<-0
led_clean<-data.frame(Country=character(),
                      "Life.expectancy"=double(),
                      "Adult.Mortality"=double(),
                      "Alcohol"=double(),
                      "BMI"=double(),
                      "Polio"=double(),
                      "Diphteria"=double(),
                      "thinness.5-9.years"=double(),
                      "Income.composition.of.resources"=double(),
                      "Schooling"=double())

#Country vector
countries<-unique(led$Country)
#Imputation of missing values/ Outlier Treatment 
for (country in countries){
  mask <- led_prep %>% filter(Country==country)
  prep_mask<-mask[,-c(1,2)]
  no_outlier_winsor<-winsor(prep_mask,trim=0.37,na.rm=TRUE)
  no_outlier_mask<-cbind(mask[,c(1)],no_outlier_winsor)
  #Mice goes here drop NA as placeholder
  imp <- mice(no_outlier_mask, method = "cart", m = 1,ignore=NULL)
  clean_mask<-complete(imp)
  #Append dataframes
  led_clean<-rbind(led_clean,clean_mask)
  
}
warnings()
summary(led_clean)
summary(led)
led_clean_complete<-cbind(led_prep[,c(2)],led_clean)
write.csv(led_clean,"C:\\Users\\joseb\\Documents\\GitHub\\R\\WHO Prediction\\Life Expectancy Data Cleaned.csv",
          row.names=FALSE)



#=============================MODEL BUILDING=====================================



#building base model

set.seed(142)
sample = sample.split(final_clean.LED,SplitRatio = 0.8)
led.train =subset(final_clean.LED,sample ==TRUE) 
led.test=subset(final_clean.LED, sample==FALSE)



basic.model <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+
                    Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+thinness..1.19.years+
                    Income.composition.of.resources+Schooling, data =final_clean.LED )


summary(basic.model)
library(olsrr)
library(tidyverse)#for easy data manipulation and visualization

library(broom)

pred.1 <-  predict(basic.model, led.test)
data.frame(
  RMSE = RMSE(pred.1, led.test$Life.expectancy),
  R2 = R2(pred.1, led.test$Life.expectancy),
  MAE= MAE(pred.1, led.test$Life.expectancy)
)

library(modelr)
data.frame(
  R2 = rsquare(basic.model, data = led.train),
  RMSE = rmse(basic.model, data = led.train),
  MAE = mae(basic.model, data = led.train)
)
ols_vif_tol(basic.model)
#=========================DATA EXPLORATION=====================================
#diagnose the variables more deeply 
diagnose(led)

#pair plot


summary(led)


#diagnose the variables more deeply 
diagnose_category(led)

diagnose_numeric(led)

diagnose_outlier(led)



#create histoframs to look at the distrinution of numeric variables
hist(led$`Adult Mortality`)
hist(led$`Hepatitis B`)
hist(led$Polio)
hist(led$GDP)
hist(led$`Income composition of resources`)
hist(led$`infant deaths`)
hist(led$Measles)
hist(led$`Total expenditure`)
hist(led$Population)
hist(led$Schooling)
hist(led$Status)
hist(led$Alcohol)
hist(led$BMI)
hist(led$Diphtheria)
hist(led$`thinness  1-19 years`)
hist(led$`Life expectancy`)
hist(led$`percentage expenditure`)
hist(led$`under-five deaths`)
hist(led$`HIV/AIDS`)
hist(led$`thinness 5-9 years`)
