
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
drops<-c("Year","Status","infant deaths","percentage expenditure","Hepatitis B","under-five","GDP","Population","thinness 5-9 years")
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
write.csv(led_clean_complete,"C:\\Users\\joseb\\Documents\\GitHub\\R\\WHO Prediction\\Life Expectancy Data Cleaned.csv",
          row.names=FALSE)

#=========================DATA EXPLORATION=====================================
#diagnose the variables more deeply 
diagnose(led)
summary(led)

#diagnose the variables more deeply 
diagnose_category(led)

diagnose_numeric(led)

diagnose_outlier(led)

#Missing values table
sort(sapply(led,function(x) sum(is.na(x))),decreasing = T)
md.pattern(led)

#Dropping 10 missing values of target variable
led<- led %>% drop_na(Life.expectancy)

#All histograms of numerical variables
names<-names(led)
classes<-sapply(led,class)
for(name in names[classes == 'numeric'])
{
  dev.new()
  hist(led[,name]) # subset with [] not $
} 
 
#Barplot and table for categorical variable       
barplot(table(led$Status))
table(led$Status) #Developed 512 and developing 2426

#Box plots for continous variables:
ggplot(data=led) + geom_boxplot(aes(x=Adult.Mortality), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=infant.deaths), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=BMI), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=GDP), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=population), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=Life.expectancy), size=1.2)

#Scatter plots with target variable
ggplot(led, aes(GDP, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(GDP, Life.expectancy, color = Year)) + 
  geom_jitter()

ggplot(led, aes(Adult.Mortality, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(Income.composition.of.resources, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(Schooling, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(HIV.AIDS, Life.expectancy)) + 
  geom_jitter()
            
#Boxplot of life expectancy on the basis of status of country
ggplot(data = led, aes(x=Status, y=Life.expectancy, fill = Status)) + geom_boxplot()
#Life Expectancy is high for developed countries as already expected

#Cheking difference in means of developed and developing countries using ANOVA
summary(aov(Life.expectancy ~ Status, data = led))
#p-value is less than 0.05 - reject null hypothesis
#There is significant differences in life expectancy for developed and developing countries

#Correlation
data_num <- led %>% 
  select_if(is.numeric)
cor(data_num, use="complete.obs")

#Life expenctancy has strong positive correlation with Schooling and Income composition of resources
#And negative strong correlation with Adult mortality
#Weak correlation with population and measles
#Strong correlation between infant deaths and under 5 deaths - multicollinearity
#Therefore removing under 5 deaths

#Hepatitis, Polio and diphtheria is converted into 2 groups according to World health assembly rule by 2020
#Under 90% and above 90%

led3 <- led %>% 
  select(-Country, -Year) %>%
  mutate(Hepatitis.B = ifelse(Hepatitis.B < 90, "<90% Covered", ">=90% Covered"),
         Polio = ifelse(Polio < 90, "<90% Covered", ">=90% Covered"),
         Diphtheria = ifelse(Diphtheria < 90, "<90% Covered", ">=90% Covered"),
         Hepatitis.B = as.factor(Hepatitis.B),
         Polio = as.factor(Polio),
         Diphtheria = as.factor(Diphtheria))
str(led3)
table(led3$Hepatitis.B)
table(led3$Polio)
table(led3$Diphtheria)

#Box plot of life expectancy on hepatitis
library(ggplot2)
ggplot(data = led3, aes(x=Hepatitis.B, y=Life.expectancy, fill = Hepatitis.B)) + geom_boxplot()
#anova
summary(aov(Life.expectancy ~ Hepatitis.B, data = led3)) #p less than 0.05

#Box plot of life expectancy on polio
ggplot(data = led3, aes(x=Polio, y=Life.expectancy, fill = Polio)) + geom_boxplot()
summary(aov(Life.expectancy ~ Polio, data = led3)) #p less than 0.05
dev.off()

#Box plot of life expectancy on Diphtheria
ggplot(data = led3, aes(x=Diphtheria, y=Life.expectancy, fill = Diphtheria)) + geom_boxplot()
summary(aov(Life.expectancy ~ Diphtheria, data = led3)) #p less than 0.05


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

