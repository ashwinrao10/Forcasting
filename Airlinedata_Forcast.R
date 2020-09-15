library('readxl')
setwd('C:/Users/Ashwin/Desktop/Assignments/Forcasting')
ad <- read_excel("Airlines+data.xlsx")
View(ad)

#dummy variable for month
month <- data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
View(month)
colnames(month) <- month.abb
View(month)
ad <- cbind(ad,month)
View(ad)

#Assign time 't' for time series

ad["t"] <- c(1:96)
View(ad)
ad["log_passengers"] <- log(ad["Passengers"])
ad["tsqure"] <- ad["t"]*ad["t"]
View(ad)

attach(ad)
#data patitioning
train <- ad[1:84,]
test <- ad[85:96,]

#linear Regression

linemodel <- lm(Passengers~t,data=train)
summary(linemodel)
line_pred <- data.frame(predict(linemodel,interval="predict",newdata=test))
line_pred
rmse_line <- sqrt(mean((test$Passengers-line_pred$fit)^2,na.rm=T))
rmse_line

#exponential model
expo <- lm(log_passengers~t,data=train)
summary(expo)
expo_pred <- data.frame(predict(expo,interval="predict",newdata=test))
expo_pred
rmse_expo <- sqrt(mean((test$Passengers-expo_pred$fit)^2,na.rm=T))
rmse_expo

#quadratic model
quad <- lm(Passengers~t+tsqure,data=train)
quad
quad_pred <- data.frame(predict(quad,interval="predict",newdata=test))
quad_pred
rmse_quad <- sqrt(mean((test$Passengers-quad_pred$fit)^2,na.rm=T))
rmse_quad

#Additive Seasonality
sea <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea)
sea_pred <- data.frame(predict(sea,interval="predict",newdata=test))
sea_pred
rmse_sea <- sqrt(mean((test$Passengers-sea_pred$fit)^2,na.rm=T))
rmse_sea
 
#Additive seasonality with quadratic model
sea_quad <- lm(Passengers~t+tsqure+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_quad)
sea_quad_pred <- data.frame(predict(sea_quad,interval="predict",newdata=test))
sea_quad_pred
rmse_sea_quad <- sqrt(mean((test$Passengers-sea_quad_pred$fit)^2,na.rm=T))
rmse_sea_quad

#multiplicative seasonality
multi_sea <- lm(log_passengers~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(multi_sea)
multi_sea_pred <- data.frame(predict(multi_sea,newdata=test,interval='predict'))
multi_sea_pred
rmse_multi_sea <- sqrt(mean((test$Passengers-multi_sea_pred$fit)^2,na.rm=T))
rmse_multi_sea
#table for models and its RMSE values
table_rmse <- data.frame(c("rmse_line","rmse_expo","rmse_quad","rmse_sea","rmse_sea_quad","rmse_multi_sea"),c(rmse_line,rmse_expo,rmse_quad,rmse_sea,rmse_sea_quad,rmse_multi_sea)) 
table_rmse
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

#RMSE value of seasonal quadratic model is less than any other model

write.csv(ad,file="ad.csv",row.names=F)
View(ad)

#Combining Training & test data to build Additive seasonality using Quadratic Trend

sea_qud_final <- lm(Passengers~t+tsqure+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=ad)
summary(sea_qud_final)

#Prediction for new model
pred_new <- predict(sea_qud_final,newdata=test,interval="predict")
View(pred_new)

plot(sea_qud_final)

#Take residual value and build&plot ACFplot
 acf(sea_qud_final$residuals,lag.max=10)

 Ar_ad <- arima(sea_qud_final$residuals, order=c(1,0,0))
Ar_ad$residuals
Ar_errors <- Ar_ad$residuals
acf(Ar_errors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))
install.packages("forecast")
library("forecast")
errors <- forecast(Ar_ad,h=12)
future_errors <- data.frame(errors)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predictednewvalues <- pred_new+future_errors
View(predictednewvalues)  
write.csv(predictednewvalues,file="ad_prednewvalues.csv",row.names=F)  

###exponential smoothing
install.packages('fpp')
library(fpp)  
install.packages('smooth')  
library(smooth)  
install.packages('tseries')  
library(tseries)  

#converting data into time series
tspassengers <- ts(ad$Passengers,frequency=12,start=c(96))

#dividing data
train <- tspassengers[1:84]
test <- tspassengers[85:96]

#converting test and train data to timeseries
train <- ts(train,frequency=12)
test <- ts(test,frequency=12)

plot(tspassengers)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_ad <- HoltWinters(train,alpha=0.2,beta=F,gamma=F)
hw_ad
hwad_pred <- data.frame(predict(hw_ad,n.ahead=12))
plot(forecast(hw_ad,h=12))

#Calculate mean absolute percentage error
hwad_mape <- MAPE(hwad_pred$fit,test)*100
hwad_mape

# with alpha = 0.2, beta = 0.15
# Assuming time series data has level and trend parameter

hw_ad1 <- HoltWinters(train,alpha=0.2,beta=0.15,gamma=F)
hwad1_pred <- data.frame(predict(hw_ad1,n.ahead=12))
plot(forecast(hw_ad1,h=12))

hwad1_mape <- MAPE(hwad1_pred$fit,test)*100
hwad1_mape

# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_ad2 <- HoltWinters(train,alpha=0.2,beta=0.15,gamma=0.05)
hwad2_pred <- data.frame(predict(hw_ad2,n.ahead=12))
plot(forecast(hw_ad2,h=12))

hwad2_mape <- MAPE(hwad2_pred$fit,test)*100
hwad2_mape

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape

df_mape <- data.frame(c("hwad_mape","hwad1_mape","hwad2_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwad_mape,hwad1_mape,hwad2_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
 #With all the coefficietnt default value gives best prediction with small error i.e.hwnabg
new_model <- HoltWinters(tspassengers)
new_model

plot(forecast(new_model,n.ahead=12))

# Forecasted values for the next 12 months
forecast_new <- data.frame(predict(new_model,n.ahead=12))
forecast_new

############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2)  
ses_a
sesa_pred<-data.frame(predict(ses_a,h=12))
plot(forecast(ses_a,n.ahead=12))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape

# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.15)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=12))
plot(forecast(holt_ab,h=12))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.05 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 12))
plot(forecast(hw_abg_new,h=12))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new

# With out optimum values 

# simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 12))
sesna_pred
plot(forecast(ses_na,h=12))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape

# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=12))
holtnab_pred
plot(forecast(holt_nab,h=12))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape

# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=12))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=12))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

new_model <- hw(tspassengers,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=12))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=12))







