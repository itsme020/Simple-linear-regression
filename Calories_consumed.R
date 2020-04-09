#Linear Regression on calories consumed

mydata<- calories_consumed

#Plot a Scatter Plot
plot(mydata$Calories.Consumed,mydata$Weight.gained..grams.)

#Boxplot
boxplot(mydata)

#Histogram
hist(mydata$Calories.Consumed)

hist(mydata$Weight.gained..grams.)

summary(mydata)

#Correation for weight gain and calories consumed
CC <-mydata$Calories
WG <- mydata$Weight.gained..grams.
cor(WG,CC)

#Correlation coefficient is 0.946 means it is strongly corelated

reg <-lm(WG~CC)
summary(reg)
# R-squared value for the above model is 0.8968 

plot(reg)

# Function to Predict the above model 
p <-predict(reg,interval="predict")

#Applying Transformations
#Logarthimic transformations
reg1 <- lm(WG~log(CC))
plot(reg1)
summary(reg1)
p1<-predict(reg1,interval="predict")
# R-squared value for the above model is 0.8077
# Adjusted R-squared:  0.7917 

#Exponential Transformation
reg2<- lm(log(WG)~CC)
plot(reg2)
summary(reg2)
p2 <- predict(reg2,interval = "predict")
# R-squared value has increased from 0.8776
# Adjusted R SQuare Value - 0.8674 
# Higher the R-sqaured value - Better chances of getting good model 

#Cubic Model
reg3 <- lm(WG~CC+I(CC^2)+I(CC^3),data = CalConWG)
plot(reg3)
hist(residuals(reg3))
summary(reg3)
p3 <-predict(reg3,interval = "predict")
rmse<-sqrt(mean((p3-WG)^2))
rmse
plot(reg3)