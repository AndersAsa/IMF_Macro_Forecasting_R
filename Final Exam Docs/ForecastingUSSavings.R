## Load Data
require(vars)
require(tsDyn)
data = read.csv("http://www.aachristiansen.com/usa_cy.csv")

## Convert data to Time Series
for(i in 1:dim(data)[2])
  assign(names(data)[i],window(ts(data[,i], 
                                  frequency = 4, 
                                  start = c(1945,1), 
                                  end=c(2015, 4)),
                               start = c(1961,1),
                               end = c(2007,4)))

for(i in 1:dim(data)[2])
  assign(paste0(names(data)[i],"Test"),window(ts(data[,i], 
                                  frequency = 4, 
                                  start = c(1945,1)),
                               start = c(2008,1),
                               end = c(2014,3)))

saving_rate = ts(100*(rdy - (rc + ((gov_transfers + interest_payments/y_deflator))))/rdy, 
               frequency = 4, start = c(1961,1))
saving_rate_test = ts(100*(rdyTest - (rcTest + ((gov_transfersTest + interest_paymentsTest/y_deflatorTest))))/rdyTest,
                     frequency = 4, start = c(2008,1))

## QUESTION 8.1

par(mar=c(5,4,4,5)+.1)
plot.ts(log(rc/rdy), 
        type="l", 
        col="red")
par(new=TRUE)
plot.ts(log(rnw/rdy), 
        type="l", 
        col="blue",
        xaxt="n", yaxt="n",xlab="",ylab="")
axis(4)
mtext("log(rnw/rdy)",side=4,line=3)

## QUESTION 8.2 - 8.3

fit = lm(log(rc/rdy) ~ log(rnw/rdy))
residuals = ts(fit$residuals,frequency = 4, start = c(1961,1))
plot.ts(residuals)
abline(h=0)

## Set up dummy var

n = length(dateid01)
zeros = (1975-1961)*4 + 3
sb_1975_4 = ts(c(rep(0,zeros), rep(1, n-zeros)),frequency = 4, start = c(1961,1))

## QUESTION 8.4 - 8.5 

summary(ur.df(y = log(rc), type = "drift", lags = 2))
summary(ur.df(y = log(rdy), type = "drift", lags = 0))
summary(ur.df(y = log(rnw), type = "drift", lags = 4))

## QUESTION 8.6 

VARselect(data.frame(rc=log(rc),rdy=log(rdy),rnw=log(rnw)),
          type="const", 
          exogen = data.frame(sb_1975_4 = sb_1975_4))

## QUESTION 8.7 - 8.9

SimpleVar = VAR(data.frame(rc=log(rc),rdy=log(rdy),rnw=log(rnw)),
                p = 2,
                type="const", 
                exogen = data.frame(sb_1975_4 = sb_1975_4))

arch.test(SimpleVar,4)
normality.test(SimpleVar)

## ASIDE How to forecast Real Consumption with VAR model.

VARForecast = predict(SimpleVar,n.ahead = 27,dumvar=data.frame(sb_1975_4 = rep(1,27)))
par(mfrow=c(2,2))
for(i in 1:3) {
  variables = c("rc","rdy","rnw")
  Predicted = ts(c(window(get(variables[i]),start=c(2004,1)),exp(VARForecast$fcst[[variables[i]]][,1])), 
                   frequency = 4, start=c(2004,1))
  PredLo = ts(c(window(get(variables[i]),start=c(2004,1)),exp(VARForecast$fcst[[variables[i]]][,2])), 
                frequency = 4, start=c(2004,1))
  PredHi = ts(c(window(get(variables[i]),start=c(2004,1)),exp(VARForecast$fcst[[variables[i]]][,3])), 
                frequency = 4, start=c(2004,1))
  
  Actual = window(ts(data[,variables[i]], 
                       frequency = 4, 
                       start = c(1945,1), 
                       end=c(2015, 4)),
                    start = c(2004,1),
                    end = c(2014,3))
  
  plot(Actual, col="red",
       ylim = c(min(c(Actual,PredLo)), max(c(PredHi,Actual)) ),
       ylab = variables[i])
  lines(PredLo, col="green")
  lines(PredHi, col="green")
  lines(Predicted, col="blue")
}
par(mfrow = c(1,1))

## Impulse responses

VARImpulse = irf(SimpleVar, impulse = "rdy", n.ahead = 27, ci = .95)

rcPredicted = exp(as.numeric(VARImpulse$irf[[1]][,1]))-1
rcPredLo = exp(as.numeric(VARImpulse$Lower[[1]][,1]))-1
rcPredHi = exp(as.numeric(VARImpulse$Upper[[1]][,1]))-1

plot(rcPredicted, col="red", type = "l",
     ylim=c(min(rcPredLo),max(rcPredHi)))
lines(rcPredLo, col="green")
lines(rcPredHi, col="green")
abline(h=0)

## QUESTION 8.10

summary(ca.jo(cbind(log(rc),log(rdy),log(rnw)), 
              type="eigen", ecdet = "const", K = 2, 
              dumvar = data.frame(sb_1975_4 = sb_1975_4)))

## QUESTION 8.11

summary(ur.df(y = diff(unemp), type = "drift", selectlags = "AIC"))
summary(ur.df(y = log(consumer_sentiment), type = "drift", selectlags = "AIC"))

## QUESTION 8.12

difUnem = window(diff(ts(data$unemp, frequency = 4, start = c(1945,1))),
                 start = c(1961,1),
                 end = c(2007,4))

VECMurca = ca.jo(cbind(log(rc),log(rdy),log(rnw)), 
                 type="eigen", ecdet = "const", K = 2, 
                 dumvar = data.frame(sb_1975_4 = sb_1975_4, 
                                     difUnem = difUnem, 
                                     consConf = log(consumer_sentiment)),
                 spec="longrun")

cajorls(VECMurca, 1)

SimpleVECM = VECM(cbind(log(rc),log(rdy),log(rnw)), lag=1, r = 1, include = "const",
     estim ="ML", LRinclude = "const", 
     exogen = data.frame(sb_1975_4 = sb_1975_4))

VECMDynForecast = predict(SimpleVECM,n.ahead = 27,exoPred=data.frame(sb_1975_4 = rep(1,27)))

saving_rate_forecastD = ts(100*(exp(VECMDynForecast[,"log(rdy)"]) - (exp(VECMDynForecast[,"log(rc)"]) + ((gov_transfersTest + interest_paymentsTest/y_deflatorTest))))/exp(VECMDynForecast[,"log(rdy)"]),
                           frequency = 4, start = c(2008,1))

VECMStaticForecast = VECMDynForecast

rc2=rc;rdy2=rdy;rnw2=rnw;sb_1975_4_2=sb_1975_4;
for(i in 1:dim(VECMDynForecast)[1]) {
  rc2 = ts(c(rc2,rcTest[i]), frequency = 4, start = c(1961,1))
  rdy2 = ts(c(rdy2,rdyTest[i]), frequency = 4, start = c(1961,1))
  rnw2 = ts(c(rnw2,rnwTest[i]), frequency = 4, start = c(1961,1))
  sb_1975_4_2 = ts(c(sb_1975_4_2,1), frequency = 4, start = c(1961,1))
  SimpleVECM = VECM(cbind(log(rc2),log(rdy2),log(rnw2)), lag=1, r = 1, include = "const",
       estim ="ML", LRinclude = "const", 
       exogen = data.frame(sb_1975_4_2 = sb_1975_4_2))
  VECMStaticForecast[i,] = predict(SimpleVECM,n.ahead = 1,exoPred=data.frame(sb_1975_4_2=1))
}
  

saving_rate_forecastS = ts(100*(exp(VECMStaticForecast[,"log(rdy)"]) - (exp(VECMStaticForecast[,"log(rc)"]) + ((gov_transfersTest + interest_paymentsTest/y_deflatorTest))))/exp(VECMStaticForecast[,"log(rdy)"]),
                           frequency = 4, start = c(2008,1))

SavingsRate = ts(c(saving_rate,saving_rate_test), frequency = 4, start = c(1961,1))
srForecastD = ts(c(saving_rate[length(saving_rate)],saving_rate_forecastD), frequency = 4, start = c(2007,4))
srForecastS = ts(c(saving_rate[length(saving_rate)],saving_rate_forecastS), frequency = 4, start = c(2007,4))

plot(SavingsRate, col="red",
     ylim = c(2,9),
     xlim = c(2000,2015))
lines(srForecastD, col="blue")
lines(srForecastS, col="purple")

rmse <- function(error) sqrt(mean(error^2))

## Static Forecast RMSE
rmse(saving_rate_forecastS - saving_rate_test)
## Baseline using last quarters Savings Rate
rmse(c(saving_rate[length(saving_rate)],saving_rate_test[1:(length(saving_rate_test)-1)]) -
       saving_rate_test)
## Dynamic Forecast RMSE
rmse(saving_rate_forecastD - saving_rate_test)
## Baseline 2007 Q1
rmse(saving_rate[length(saving_rate)] - saving_rate_test)