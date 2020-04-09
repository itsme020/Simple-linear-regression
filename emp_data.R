View(emp_data)

summary(emp_data)

#box plot
boxplot(emp_data$Salary_hike,emp_data$Churn_out_rate)
#scatter plot
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)  ### Scatter Plot 

#Correlation

Salhike<-emp_data$Salary_hike
CoR<-emp_data$Churn_out_rate

cor(Salhike,CoR)# its having strong negative corelation

#Model building

reg<- lm(Salhike~CoR)    
summary(reg)
##  R-squared:  0.8312,	Adjusted R-squared:  0.8101  ##
#prediction
pred<- predict(reg)
pred

reg$residuals
sum(reg$residuals)
mean(reg$residuals)

## RMSE Value
sqrt(sum(reg$residuals^2)/nrow(emp_data))  
sqrt(mean(reg$residuals)^2)


## Confidance Interval     
confint(reg,level=0.95)              
predict(reg,interval = 'predict')

#Cubic Model
reg_log <- lm(Churn_out_rate~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3),data=emp_data)
summary(reg_log)
###Multiple R-squared:  0.9893,	Adjusted R-squared:  0.984 ###
confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval="predict")
pred

reg_log$residuals
sum(reg_log$residuals)
mean(reg_log$residuals)

rmse<-sqrt(mean((pred-Churn_out_rate)^2))
rmse             ###############(3.2281)

plot(reg_log)





########  LOGARITHMIC MODEL  #######
attach(emp_data)
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)#strong negative correlation

reg_log<- lm(Churn_out_rate~log(Salary_hike))  ##SLR Log Method
summary(reg_log)###### R-squared:  0.8486,	Adjusted R-squared:  0.8297  #######


pred_1<- predict(reg_log)

reg_log$residuals
sum(reg_log$residuals)
mean(reg_log$residuals)

sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  ###  RMSE Value(3.7)

confint(reg_log,level = 0.95)
predict(reg_log,interval = 'predict')

ggplot(data = emp_data,aes(x= Salary_hike,y= Churn_out_rate)) + 
  geom_point(colour='Blue') +
  geom_line(colour='red',data = emp_data,aes(x= Salary_hike,y= pred_1))



###### Exponential Method ######

plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))#strong negative cor

reg_exp<- lm(log(Churn_out_rate)~Salary_hike)
summary(reg_exp)
##  R-squared:  0.8735,	Adjusted R-squared:  0.8577  ##

pred_2<- predict(reg_exp)
pred_2
exp<- exp(pred_2)

reg_exp$residuals
sum(reg_exp$residuals)
mean(reg_exp$residuals)

sqrt(mean(reg_exp$residuals^2))

error = emp_data$Churn_out_rate - exp
error
sqrt(sum(error^2)/(nrow(emp_data)))  ###  RMSE Value(3.541)

confint(reg_exp,level = 0.95)
predict(reg_exp,interval = 'predict')

ggplot(data = emp_data,aes(x= Salary_hike,y= Churn_out_rate)) + 
  geom_point(colour='Blue') +
  geom_line(colour='red',data = emp_data,aes(x= Salary_hike,y= exp))


####  Polynomial Model With 3 Degree ####

plot(Salary_hike^3,log(Churn_out_rate))
cor(Salary_hike^3,log(Churn_out_rate))

reg_poly3<- lm(log(Churn_out_rate)~ Salary_hike +I(Salary_hike^2)
               +I(Salary_hike^3))
summary(reg_poly3)
###   R-squared:  0.992,	Adjusted R-squared:  0.9879  ###

pred_3<- predict(reg_poly3)
exp_1<- exp(pred_3)

reg_poly3$residuals
sum(reg_poly3$residuals)
mean(reg_poly3$residuals)

## gg plot ##
ggplot(data = emp_data,aes(x= Salary_hike+I(Salary_hike^2)
                           +I(Salary_hike^3),y = Churn_out_rate))+
  geom_point(color= 'blue')+
  geom_line(data = emp_data,color='red',aes(x= Salary_hike+I(Salary_hike^2)
                                            +I(Salary_hike^3),y = exp_1))

