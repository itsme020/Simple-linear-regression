#Linear Regression on calories consumed

mydata<- Salary_Data

#Plot a Scatter Plot
plot(mydata$YearsExperience,mydata$Salary)

#Other EDA and Plots
boxplot(mydata)

#Histogram
hist(mydata$YearsExperience)
hist(mydata$Salary)

summary(mydata$Salary)

#Correation 
YE <-mydata$YearsExperience
SA <- mydata$Salary
cor(YE,SA)

#Simple reggression
simreg <-lm(YE~SA)
summary(reg)
#R-Squraed 0.957 stongly positive corelated

#Cubic Model
cureg <- lm(SA~YE+I(YE^2)+I(YE^3),data = Salary_Data)
p2 <-predict(reg2,interval = "predict")
plot(cureg)
summary(cureg)

#Multi R-Squared error is 0.9636, Adjusted R-squared method is 0.9594

#Exponential Transformation
Expreg <- lm(SA~log(YE))
p1 <- predict(Expreg,interval = "predict")
plot(Expreg)
summary(Expreg)
#Multi R-Squared error is 0.8539, Adjusted R-squared method is 0.8487




#cubic model is more strongly corelated so predicted salary will be based on cubic model.
Final <- cbind(YearsofExp=Salary_Data$YearsExperience,Sal_Hike = Salary_Data$Salary,Pred_sal_hike=p2)
View(Final)

rmse<-sqrt(mean((p2-SA)^2))
rmse










