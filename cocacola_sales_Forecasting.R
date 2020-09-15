library('readxl')
setwd('C:/Users/Ashwin/Desktop/Assignments/Forcasting')
cd <- read_excel("CocaCola_Sales_Rawdata.xlsx")
View(cd)
plot(cd$Sales,type="o")

#dummy variable for quarter
Q1 <-  ifelse(grepl("Q1",cd$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cd$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cd$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cd$Quarter),'1','0')

cd<-cbind(cd,Q1,Q2,Q3,Q4)
View(cd)


#Assign time 't' for time series

cd["t"] <- c(1:42)
View(cd)
cd["log_sales"] <- log(cd["Sales"])
cd["tsqure"] <- cd["t"]*cd["t"]
View(cd)

attach(cd)
#data patitioning
train <- cd[1:38,]
test <- cd[39:42,]

#linear Regression

linemodel <- lm(Sales~t,data=train)
summary(linemodel)
line_pred <- data.frame(predict(linemodel,interval="predict",newdata=test))
line_pred
rmse_line <- sqrt(mean((test$Sales-line_pred$fit)^2,na.rm=T))
rmse_line

#exponential model
expo <- lm(log_sales~t,data=train)
summary(expo)
expo_pred <- data.frame(predict(expo,interval="predict",newdata=test))
expo_pred
rmse_expo <- sqrt(mean((test$Sales-expo_pred$fit)^2,na.rm=T))
rmse_expo

#quadratic model
quad <- lm(Sales~t+tsqure,data=train)
quad
quad_pred <- data.frame(predict(quad,interval="predict",newdata=test))
quad_pred
rmse_quad <- sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm=T))
rmse_quad

#Additive Seasonality
sea <- lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea)
sea_pred <- data.frame(predict(sea,interval="predict",newdata=test))
sea_pred
rmse_sea <- sqrt(mean((test$Sales-sea_pred$fit)^2,na.rm=T))
rmse_sea

#Additive seasonality with quadratic model
sea_quad <- lm(Sales~t+tsqure+Q1+Q2+Q3+Q4,data=train)
summary(sea_quad)
sea_quad_pred <- data.frame(predict(sea_quad,interval="predict",newdata=test))
sea_quad_pred
rmse_sea_quad <- sqrt(mean((test$Sales-sea_quad_pred$fit)^2,na.rm=T))
rmse_sea_quad

#multiplicative seasonality
multi_sea <- lm(log_sales~ Q1+Q2+Q3+Q4,data=train)
summary(multi_sea)
multi_sea_pred <- data.frame(predict(multi_sea,newdata=test,interval='predict'))
multi_sea_pred
rmse_multi_sea <- sqrt(mean((test$Sales-multi_sea_pred$fit)^2,na.rm=T))
rmse_multi_sea
#table for models and its RMSE values
table_rmse <- data.frame(c("rmse_line","rmse_expo","rmse_quad","rmse_sea","rmse_sea_quad","rmse_multi_sea"),c(rmse_line,rmse_expo,rmse_quad,rmse_sea,rmse_sea_quad,rmse_multi_sea)) 
table_rmse
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

#RMSE value of seasonal quadratic model is less than any other model

write.csv(ad,file="cd.csv",row.names=F)
View(cd)

#Combining Training & test data to build Additive seasonality using Quadratic Trend

sea_qud_final <- lm(Sales~t+tsqure+Q1+Q2+Q3+Q4,data=cd)
summary(sea_qud_final)

#Prediction for new model
pred_new <- predict(sea_qud_final,newdata=test,interval="predict")
View(pred_new)

plot(sea_qud_final)

#Take residual value and build&plot ACFplot
acf(sea_qud_final$residuals,lag.max=10)

Ar_cd <- arima(sea_qud_final$residuals, order=c(1,0,0))
Ar_cd$residuals
Ar_errors <- Ar_cd$residuals
acf(Ar_errors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))
install.packages("forecast")
library("forecast")
errors <- forecast(Ar_ad,h=4)
future_errors <- data.frame(errors)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predictednewvalues <- pred_new+future_errors
View(predictednewvalues)  
write.csv(predictednewvalues,file="cd_prednewvalues.csv",row.names=F)  

#Exponential Smoothing

library(forecast)
#install.packages("fpp")
library(fpp)
#install.packages("smooth")
library(smooth) # forsmoothing and MAPE
#install.packages("tseries")
library(tseries)
library(readxl)
CocaCola_Sales_Rawdata <- read_excel("CocaCola_Sales_Rawdata-.xlsx")
View(CocaCola_Sales_Rawdata)


# Converting data into time series object
?ts

tssales<-ts(CocaCola_Sales_Rawdata$Sales,frequency = 4,start=c(42))
View(tssales)

# dividing entire data into training and testing data 
train<-tssales[1:38]
test<-tssales[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(tssales) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
?forecast
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape

# with alpha = 0.2, beta = 0.15
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape

# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
############################## STOP HERE ###############################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma

new_model <- HoltWinters(tssales)
new_model

plot(forecast(new_model,n.ahead=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=4))
forecast_new
########################################################################

############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) # 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape

# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.15)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.05 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new

# With out optimum values 

# simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape

# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape

# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

new_model <- hw(amts,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))










