
#reading the necesary libraries
library(lattice)
library(ggplot2)
library(car)

#reading the dataset
day <- read.csv('/Users/abhirajvinnakota/Desktop/regressions/1.final_project/Bike-Sharing-Dataset/day.csv')

summary(day)

# ditribution of response variables
ggplot(day, aes(cnt, color = 'grey'))+
  geom_histogram(bins = 15)

ggplot(day, aes(registered, color = 'grey'))+
  geom_histogram(bins = 15)

#ggplot(day, aes(casual, color = 'grey'))+
#  geom_histogram(bins = 15)
#ggplot(day, aes(log(casual), color = 'grey'))+
#  geom_histogram(bins = 15)

ggplot(day, aes(sqrt(casual), color = 'grey'))+
  geom_histogram(bins = 15)

#relationship between both types of users
ggplot(day, aes(y = casual, x = registered, color ='grey'))+
  geom_point(alpha = 0.3)

day$season <- as.factor(day$season)
day$weekday <- as.factor(day$weekday)
day$yr <- as.factor(day$yr)
day$mnth <- as.factor(day$mnth)
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day$weathersit <- as.factor(day$weathersit)


ggplot(day, aes(y = casual, x = registered))+
  geom_point(alpha = 0.6) +
  labs(title ='Casual Users vs Registered Users')
#working day captures the relationship

ggplot(day, aes(y = casual, x = registered, color = workingday))+
  geom_point(alpha = 0.6) +
  labs(title ='Casual Users vs Registered Users (colored by working day')

#relationship between both types of users

# makes a difference for both
ggplot(day, aes(x = season, y = casual))+
  geom_boxplot(color = 'maroon3', alpha = 0.5)+
  labs(title ='Distribution of Casual Users across Seasons')


ggplot(day, aes(x = season, y = registered))+
  geom_boxplot(color ='maroon3', alpha = 0.5)+
  labs(title ='Distribution of Registered Users across Seasons')


# month - seems like , it's less across winter months as compared to summer months, 
# so the temperature should capture it
ggplot(day, aes(x = mnth, y = casual))+
  geom_boxplot(color = 'limegreen', alpha = 0.5)+
  labs(title ='Distribution of Casual Users across Months')


ggplot(day, aes(x = mnth, y = registered))+
  geom_boxplot(color ='limegreen', alpha = 0.5)+
  labs(title ='Distribution of Registered Users across Months')


# weekday trends are opposiote for each type of users, same as we saw earlier.
ggplot(day, aes(x = weekday, y = casual))+
  geom_boxplot(color = 'firebrick', alpha = 0.5)+
  labs(title ='Distribution of Casual Users across the Week')


ggplot(day, aes(x = weekday, y = registered))+
  geom_boxplot(color = 'firebrick', alpha = 0.5)+
  labs(title ='Distribution of Registered Users across the Week')


# temp has a good positive correlation with temperature
ggplot(day, aes(x = temp, y = casual))+
  geom_smooth(method = 'lm', color = 'maroon3') +
  geom_point(color = 'maroon3', alpha = 0.5)+
  labs(title ='Association between Casual Users and Temperature')


ggplot(day, aes(x = temp, y = registered))+
  geom_smooth(method = 'lm', color = 'maroon3') +
  geom_point(color = 'maroon3', alpha = 0.5)+
  labs(title ='Association between Registered Users and Temperature')


# humidity has a negative correlation.. 
ggplot(day, aes(x = hum, y = casual))+
  geom_smooth(method = 'lm', color = 'limegreen') +
  geom_point(color = 'limegreen', alpha = 0.5)+
  labs(title ='Association between Casual Users and Humidity')


ggplot(day, aes(x = hum, y = registered))+
  geom_smooth(method = 'lm', color = 'limegreen') +
  geom_point(color = 'limegreen', alpha = 0.5)+
  labs(title ='Association between Registered Users and Humidity')

# windspeed has a more negative correlation.. 
ggplot(day, aes(x = hum, y = casual))+
  geom_smooth(method = 'lm', color = 'firebrick') +
  geom_point(color = 'firebrick', alpha = 0.5)+
  labs(title ='Association between Casual Users and Windspeed')


ggplot(day, aes(x = hum, y = registered))+
  geom_smooth(method = 'lm', color = 'firebrick') +
  geom_point(color = 'firebrick', alpha = 0.5)+
  labs(title ='Association between Registered Users and Windspeed')




# the users seem to have grown, could be used to control for year, to understand other factors
ggplot(day, aes(x = yr, y = casual, color ='grey'))+
  geom_boxplot()+
  labs(title ='Distribution of Casual Users across Years')

ggplot(day, aes(x = yr, y = registered, color ='grey'))+
  geom_boxplot()+
  labs(title ='Distribution of Casual Users across Years')

# holiday - its interesting to see opposite trends across holiday here (not about)
ggplot(day, aes(x = holiday, y = casual, color ='grey'))+
  geom_boxplot()

ggplot(day, aes(x = holiday, y = registered, color ='grey'))+
  geom_boxplot()

# weathersit very much effects the #users similarly in both cases
ggplot(day, aes(x = weathersit, y = casual, color ='grey'))+
  geom_boxplot()

ggplot(day, aes(x = weathersit, y = registered, color ='grey'))+
  geom_boxplot()


# registered users are big on working days, casual users are big on holidays
ggplot(day, aes(x = workingday, y = casual, color ='grey'))+
  geom_boxplot()

ggplot(day, aes(x = workingday, y = registered, color ='grey'))+
  geom_boxplot()

# same as atemp , pick one
ggplot(day, aes(x = atemp, y = casual, color ='grey'))+
  geom_smooth(method = 'lm') +
  geom_point()

ggplot(day, aes(x = atemp, y = registered, color ='grey'))+
  geom_smooth(method = 'lm') +
  geom_point()


## Linear Regression for Registered Users
# putting all the variables in the linear regression # rsquared = 84.79
fit_registered <- lm(registered ~ season + mnth + yr + holiday + weekday + workingday 
                     + weathersit + hum + windspeed + temp + atemp + holiday, data = day)
summary(fit_registered)

# removing temp without effecting the rsquared (removing temp effected it a little)
fit_registered_2 <- lm(registered ~ season + mnth + yr + holiday + weekday + workingday 
                       + weathersit + hum + windspeed + atemp + holiday, data = day)
summary(fit_registered_2)

# removing workingday as it isn't converging # rsquared = 84.79
fit_registered_3 <- lm(registered ~ season + mnth + yr + holiday + weekday
                       + weathersit + hum + windspeed + atemp + holiday, data = day)
summary(fit_registered_3)

#removing month reduced rsquared to 83.22 . hence keeping it in there.

# no problems with VIF
vif(fit_registered_3)

# residual plots
plot(fit_registered_3)







## Linear Regression for Casual Users
# putting all the variables in the linear regression #rsquared = 0.7347
fit_casual<- lm(casual ~ season + mnth + yr + holiday + weekday + workingday 
                + weathersit + hum + windspeed + temp + atemp + holiday + instant, data = day)
summary(fit_casual)



# removing instant, helped in making year significant, #rsuared = 0.7348
fit_casual_2<- lm(casual ~ season + mnth + yr + holiday + weekday + workingday 
                  + weathersit + hum + windspeed + temp + atemp + holiday, data = day)
summary(fit_casual_2)


# removing workingday, #rsuared = 0.7348
fit_casual_3<- lm(casual ~ season + mnth + yr + holiday + weekday 
                  + weathersit + hum + windspeed + temp + atemp + holiday, data = day)
summary(fit_casual_3)


# removing instant, removed atemp , slight increase in Rsquared , #rsuared = 0.7352,
fit_casual_4 <- lm(casual ~ season + mnth + yr + holiday + weekday 
                   + weathersit + hum + windspeed + temp + holiday, data = day)
summary(fit_casual_4)


#removing month reduced rsquared . hence keeping it in there.

# no problems with VIF
vif(fit_casual_4)

# residual plots
plot(fit_casual_4)









#time series analysis .... 
library(tseries)
library(forecast)

# initial auto arima outputs
#however, we need to establish stationarity 

# definately looks like a linear trend...
ts_registered <- ts(day$registered)
ts.plot(ts_registered,col="red3")


#"There still seem to be a certain trend that isn't being captured"

res_3 <- ts(fit_registered_3$residuals)
ts.plot(res_3,col="red3")

# need to do the stationery test to figure if the residuals are stationery.. 

#Tests for stationarity
adf_test <- adf.test(res_3, alternative = 'stationary')
print(adf_test)
#note that the alternative hypothesis here is "stationary"
#so that low p-values support stationarity
# p = 0.01 , hence supports stationarity

kpss_test <- kpss.test(res_3)
print(kpss_test)
#by the way, KPSS stands for Kwiatkowski-Philips-Schmidt-Shin
#here, the null hypothesis is actually "stationary"
#so that high p-values support stationarity
# p = 0.1 , hence supports stationarity 

# auto arima also suggests stationarity 
auto.arima(res_3)

# fitting the line with 'instant' for stationarity, instant isn't significant, 
fit_registered_4 <- lm(registered ~ season + mnth + yr + holiday + weekday
                       + weathersit + hum + windspeed + atemp + holiday + instant, data = day)
summary(fit_registered_4)

res_4 <- fit_registered_4$residuals
auto.arima(res_4)

# tried fitting the line with instant . Doesn't seem to be significant
# sugguests that the linear trend is being completely captured by the 3rd model

# looking at the acf and pacf values and plots
acf(res_3, plot = F)
acf(res_3)

pacf(res_3, plot = F)
pacf(res_3)
# seems like the first lag only matters

## failing to converge
ar_1 <- arima(day$registered/100000000000, order = c(1,0,0), xreg = cbind(day$season , day$mnth ,  day$yr, day$holiday , day$weekday , day$weathersit , day$hum , day$windspeed , day$atemp , day$holiday))

ma_1 <- arima(day$registered, order = c(0,0,1), xreg = cbind(day$season , day$mnth ,  day$yr, day$holiday , day$weekday , day$weathersit , day$hum , day$windspeed , day$atemp , day$holiday))


ar_1_res <- arima(res_3, order = c(1,0,0))
ar_1_res

ggplot(day, aes(x=instant, y=ar_1_res$residuals)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = 'loess',colour = 'turquoise') + 
  geom_hline(yintercept=0,col="red3")+
  labs(title="Residuals vs Days")

acf(ar_1_res$residuals, plot = F)

acf(ar_1_res$residuals)

pacf(ar_1_res$residuals, plot = F)
pacf(ar_1_res$residuals)


ma_1_res <- arima(res_3, order = c(0,0,1))

ggplot(day, aes(x=instant, y=ma_1_res$residuals)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = 'loess',colour = 'turquoise') + 
  geom_hline(yintercept=0,col="red3")+
  labs(title="Residuals vs Days")

# doesnt look great.. so chucking it
acf(ma_1_res$residuals, plot = F)
acf(ma_1_res$residuals)
pacf(ma_1_res$residuals, plot = F)
pacf(ma_1_res$residuals)

# confirming what needs to be done from the auto.arima function..
auto.arima(res_3)



### Have to figure out a model for casual renters



# definately looks like a linear trend...
ts_casual <- ts(day$casual)
ts.plot(ts_casual,col="red3")

# lets do a stationarity test to figure this out... 
#Tests for stationarity
adf_test <- adf.test(ts_casual, alternative = 'stationary')
print(adf_test)
#note that the alternative hypothesis here is "stationary"
#so that low p-values support stationarity
# p = 0.27 , does not support stationarity

kpss_test <- kpss.test(ts_casual)
print(kpss_test)
#by the way, KPSS stands for Kwiatkowski-Philips-Schmidt-Shin
#here, the null hypothesis is actually "stationary"
#so that high p-values support stationarity
# p = 0.01 , hence does not support stationarity 

#Therefore, fitting a linear regression line to figure out stationarity

## fitting a linear regression
day$season <- as.factor(day$season)
day$weekday <- as.factor(day$weekday)
day$yr <- as.factor(day$yr)
day$mnth <- as.factor(day$mnth)
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day$weathersit <- as.factor(day$weathersit)

# putting all the variables in the linear regression #rsquared = 0.7347
fit_casual<- lm(casual ~ season + mnth + yr + holiday + weekday + workingday 
                     + weathersit + hum + windspeed + temp + atemp + holiday + instant, data = day)
summary(fit_casual)



# removing instant, helped in making year significant, #rsuared = 0.7348
fit_casual_2<- lm(casual ~ season + mnth + yr + holiday + weekday + workingday 
                + weathersit + hum + windspeed + temp + atemp + holiday, data = day)
summary(fit_casual_2)


# removing workingday, #rsuared = 0.7348
fit_casual_3<- lm(casual ~ season + mnth + yr + holiday + weekday 
                  + weathersit + hum + windspeed + temp + atemp + holiday, data = day)
summary(fit_casual_3)


# removing instant, removed atemp , slight increase in Rsquared , #rsuared = 0.7352,
fit_casual_4 <- lm(casual ~ season + mnth + yr + holiday + weekday 
                  + weathersit + hum + windspeed + temp + holiday, data = day)
summary(fit_casual_4)


#removing month reduced rsquared . hence keeping it in there.

# no problems with VIF
vif(fit_casual_4)

# residual plots
plot(fit_casual_4)

#"There still seem to be a certain trend that isn't being captured"

res_4 <- ts(fit_casual_4$residuals)
ts.plot(res_4,col="red3")

# need to do the stationery test to figure if the residuals are stationery.. 

#Tests for stationarity
adf_test <- adf.test(res_4, alternative = 'stationary')
print(adf_test)
#note that the alternative hypothesis here is "stationary"
#so that low p-values support stationarity
# p = 0.01 , hence supports stationarity

kpss_test <- kpss.test(res_4)
print(kpss_test)
#by the way, KPSS stands for Kwiatkowski-Philips-Schmidt-Shin
#here, the null hypothesis is actually "stationary"
#so that high p-values support stationarity
# p = 0.1 , hence supports stationarity 

# auto arima also suggests stationarity 
auto.arima(res_4)

# looking at the acf and pacf values and plots
acf(res_4, plot = F)
acf(res_4)

# can clearly see the weekly trend.. !

pacf(res_4, plot = F)
pacf(res_4)
# seems like the first 3 lags matter

ar_2_res <- arima(res_4, order = c(2,0,0))
ar_2_res 
res_4_step0 <- ar_2_res$residuals

# looking at the acf and pacf values and plots
acf(res_4_step0, plot = F)
acf(res_4_step0)

pacf(res_4_step0, plot = F)
pacf(res_4_step0)


ar_3_res <- arima(res_4, order = c(3,0,0))
ar_3_res 
res_4_step2 <- ar_3_res$residuals

# looking at the acf and pacf values and plots
acf(res_4_step2, plot = F)
acf(res_4_step2)

pacf(res_4_step2, plot = F)
pacf(res_4_step2)

# both the plots seem similar, time to look at the shocks with MA.

ar_3_ma_1_res <- arima(res_4, order = c(3,0,1))
res_4_step3 <- ar_3_ma_1_res$residuals

acf(res_4_step3, plot = F)
acf(res_4_step3)

pacf(res_4_step3, plot = F)
pacf(res_4_step3)

# not much difference..

ar_3_ma_2_res <- arima(res_4, order = c(3,0,2))
ar_3_ma_2_res

res_4_step4 <- ar_3_ma_2_res$residuals

acf(res_4_step4, plot = F)
acf(res_4_step4)

pacf(res_4_step4, plot = F)
pacf(res_4_step4)

# good difference...


#final...

ar_2_ma_3_res <- arima(res_4, order = c(2,0,3))
ar_2_ma_3_res
res_4_step5 <- ar_3_ma_3_res$residuals

acf(res_4_step5, plot = F)
acf(res_4_step5)

pacf(res_4_step5, plot = F)
pacf(res_4_step5)




ggplot(day, aes(x=instant, y=res_4_step5)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = 'loess',colour = 'turquoise') + 
  geom_hline(yintercept=0,col="red3")+
  labs(title="Residuals vs Days")

