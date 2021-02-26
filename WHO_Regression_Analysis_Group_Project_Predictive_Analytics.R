#import the dataset
library(readr)
led <- read_csv("LANGARA/DANA 4810/Group project/Life Expectancy Data.csv")

#create descriptive statistics to look at missing values and means, medians, max min
summary (led)

str(led)


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

#there are 10 missing values on the target variable, those were dropped

library(tidyr)
led.complete.target<- led %>% drop_na(`Life expectancy`)

#split the dataset into training and testing 80:20

library(tidyverse)
library(caret)
library(caTools)
theme_set(theme_classic())


set.seed(142)
sample = sample.split(led.complete.target,SplitRatio = 0.8)
led.train =subset(led.complete.target,sample ==TRUE) 
led.test=subset(led.complete.target, sample==FALSE)

#to perform correlation on all numerical variables we need to drop 2 categorical columns, we also need to get rid of na values (deleted for now)

led.train.num = subset(led, select = -c(Country, Status))

#led.num1<- led.num %>% drop_na()


#run correlation on all variables  - correlation matrix and generate a graph using corrplot
correlation <-cor(led.train.num)
correlation

# for those who do not have the library install.packages("corrplot")
library(corrplot)
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)