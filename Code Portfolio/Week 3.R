#Week 3 Practice
#Exploratory Data Analysis Checklist

setwd("/Users/yichunliu/HU/Summer_2019")
read.csv("US EPA data 2017.csv")

install.packages("readr")
library(readr)

epa <- read_csv("US EPA data 2017.csv")

#check columns and rows
nrow(epa)
ncol(epa)

str(epa) #a summary of data
head(epa[, c(6:7, 10)]) #look up the top and bottom of data with head and tail function
tail(epa[, c(6:7, 10)])


table(epa) #file is too large to run
install.packages("dplyr")
library(dplyr)
filter(epa, Time.Local == "13:14") %>% 
  +         select(State.Name, County.Name, Date.Local, 
                   +                Time.Local, Sample.Measurement)
#for large data set, filter the data in a specified range

unique(epa$`Method Name`)
summary(epa$`Method Name`)
summary(epa$`Arithmetic Mean`) #provides minimum, mean. maximum, 1st and 3rd quantile values

quantile(epa$`Arithmetic Mean`, seq(0, 1, 0.1)) #detailed distribution

ranking <- group_by(epa, State.Name, County.Name) 
+         summarize(epa = mean(epa$`Arithmetic Mean`)) 
+         as.data.frame 
+         arrange(desc(epa))
#rank the data with provided requirements, use head and tail function to find top and bottom of the rank

epa <- mutate(epa, Date.Local = as.Date(Date.Local)) #adds new variables and preserves existing ones



