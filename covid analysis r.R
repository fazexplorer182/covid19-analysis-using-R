rm(list=ls())
library(Hmisc)#To install Hmisc package execute the code , install.packages("Hmisc")


data <- read.csv("~/R projects/covid analysis/COVID19_line_list_data.csv")
describe(data)
#We can find that there is 14 distinct values for death,this happened because some entries reported date of the death while usually it is given as 0 or 1.
unique(data$death_dummy) #check whether our data is clean or not
data$death_dummy <- as.integer(data$death != 0) #if the death value not equal to 0,then the person is dead,if it is not equal to 0 then the person had survived
unique(data$death_dummy) #confirm whether our data is clean
#if the above code's output is 0 and 1 ,then we had cleaned up our data
sum(data$death_dummy)/nrow(data) #This tells us how many people died out of how many total people got infected.


#claim : the average people who dies due to covid is older than the person who survives
#so we check this based on the age
dead = subset(data,death_dummy == 1) 
#here we have made a variable called dead which is a subset of our data where the variable death_dummy was equal to 1
#that includes all those people who are dead 
alive = subset(data,death_dummy == 0)
#that includes all those people who have survived
mean(dead$age,na.rm=TRUE)
mean(alive$age,na.rm=TRUE)
#Now we have to check whether this is statistically significant or not.
#so here we can use t test
t.test(alive$age,dead$age,alternative = "two.sided",conf.level = 0.95)
#since here the p value is less than 0.05,we reject the null hypothesis
#So we can conclude that the people who die from the corona virus are much older than the people who survives from the covid


#claim2 : gender has no effect
men = subset(data,gender == "male")
women = subset(data,gender == "female")
mean(men$death_dummy,na.rm=TRUE)
mean(women$death_dummy,na.rm=TRUE)
#now we can conduct t test
t.test(men$death_dummy,women$death_dummy,alternative = "two.sided",conf.level = 0.95)
#since here p value is less than 0.05 we can reject null hypothesis 
#so we can conclude that men had more fatality rates due to covid than women