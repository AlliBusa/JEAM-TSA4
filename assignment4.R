library(ggplot2)
library(lubridate)
library(marima)
library(forecast)
library(plotly)
library(dplyr)

source("step.slow.p.marima_2017.R") # make sure you have the right workign directory

df <- read.table("A4_data.csv", sep = "\t", header = TRUE)
df$ww2 <- df$yyyymm
df$ww2 <- if_else(df$ww2 < 194208, 1,0)
#### Question 1 

# create plotting df 
plottingDf <- df
plottingDf$date <- lubridate::ym(plottingDf$yyyymm)
plottingDf <- plottingDf[, 2:11]

# set colors for graph
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#C59900")

# create graph of all Ys
presenting <- ggplot() + 

  geom_line(data=plottingDf, aes(x=date, y=Other, color = colors[1])) + 
  geom_line(data=plottingDf, aes(x=date, y=Cnsmr, color = colors[2])) + 
  geom_line(data=plottingDf, aes(x=date, y=HiTec, color = colors[3])) + 
  geom_line(data=plottingDf, aes(x=date, y=Hlth, color = colors[4])) + 
  geom_line(data=plottingDf, aes(x=date, y=Manuf, color = colors[5])) 

ggplotly(presenting)

# create individual graphs 
OtherPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=Other, color = colors[1]))
ggplotly(OtherPlot)

CnsmrPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=Cnsmr, color = colors[1]))
ggplotly(CnsmrPlot)

ManufPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=Manuf, color = colors[1]))
ggplotly(ManufPlot)

HiTecPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=HiTec, color = colors[1]))
ggplotly(HiTecPlot)

HlthPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=Hlth, color = colors[1]))
ggplotly(HlthPlot)


b.mPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=b.m, color = colors[1]))
ggplotly(b.mPlot)

ntisfPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=ntis, color = colors[1]))
ggplotly(ntisfPlot)

inflPlot <- ggplot() + geom_line(data=plottingDf, aes(x=date, y=infl, color = colors[1]))
ggplotly(inflPlot)

#plot of all data together
par(mar=c(3,3,1.5,1), mgp=c(2,0.7,0))
matplot(plottingDf$date, plottingDf[1:5],type="l", lty=1,col=1:2,xlab="Years")

par(mfrow=c(5,1))
plot(Manuf~date, plottingDf, type="l", lty=1,col=1:2)
plot(Hlth~date, plottingDf, type="l", lty=1,col=1:2)
plot(Cnsmr~date, plottingDf, type="l", lty=1,col=1:2)
plot(HiTec~date, plottingDf, type="l", lty=1,col=1:2)
plot(Other~date, plottingDf, type="l", lty=1,col=1:2)
summary(plottingDf)
##### Question 2

Ys <- colnames(df)[2:6]

par(mfrow=c(3,2)) 

#plot acf
for(y in Ys){
  acf(df[[y]], main = y)
}

#plot pacf
for(y in Ys){
  pacf(df[[y]], main = y)
}
pacf(df$Cnsmr)

# plot ccf
for (i in 1:5) {
  for (j in 1:5) {
    ccf(df[i], df[j], main = paste0(Ys[i]," and ",Ys[j]))
  }
}

acf(df)
par(mfrow=c(1,1)) 

#plot pacf
for(y in Ys){
  auto.arima(df[[y]])
}

##### Question 3

auto.arima(df$Cnsmr)
auto.arima(df$Hlth)
auto.arima(df$HiTec)
auto.arima(df$Manuf)
auto.arima(df$Other)



struct11 <- define.model(kvar=10, ar=1, ma=1, rem.var=c(1,7:9), reg.var = 10, indep=NULL) # rem.var is to ignore the years
M1 <- marima(t(df), means=1, ar.pattern=struct11$ar.pattern,
             ma.pattern=struct11$ma.pattern, Check=FALSE, Plot="log.det", penalty=0)


acf(t(M1$residuals)[-1,])
pacf(t(M1$residuals)[-1,])
M1

#####
# Question 3 using slow step
structFIRST <- define.model(kvar=10, ar=c(1:3), ma=c(1:3), rem.var=c(1,7:9), reg.var = 10, indep=NULL) # rem.var is to ignore the years
MFIRST <- marima(t(df), means=1, ar.pattern=structFIRST$ar.pattern,
             ma.pattern=structFIRST$ma.pattern, Check=FALSE, Plot="log.det", penalty=0)

MFIRST

slM2 <- step.slow.p(MFIRST, data=t(df))


slM2

structSECOND <- define.model(kvar=10, ar=2, ma=2, rem.var=c(1,7:9), reg.var = 10, indep=NULL) # rem.var is to ignore the years
MSECOND <- marima(t(df), means=1, ar.pattern=structSECOND$ar.pattern,
             ma.pattern=structSECOND$ma.pattern, Check=FALSE, Plot="log.det", penalty=0)

MSECOND

slMSECOND <- step.slow.p(MSECOND, data=t(df))
slMSECOND


## Forecast
predictiondf <- df[1:1077,] 
actualdf <- df

#add new rows 
newData <- matrix(0, nrow = 4, ncol = 10)
newData[,1] <- seq(201609,201612,1) #add dates we want to predict for
newData[,2:10] <- 0 
colnames(newData) <- colnames(predictiondf)
#add prediction data to main df
MatPredicts <- rbind(predictiondf, newData)
#do forecast
Forecasts <-  arma.forecast(t(MatPredicts), nstart=1077, nstep=4, marima=slM2) # named smething different####

forecastdf <- data.frame(
  t(Forecasts$forecasts)
)
forecastdf[,1] <- df[,1]
colnames(forecastdf) <- colnames(predictiondf)
forecastdf$date <- lubridate::ym(forecastdf$yyyymm)
forecastdf <- forecastdf[, 2:11]


presenting <- ggplot() + 
  geom_line(data=forecastdf, aes(x=date, y=Other, color = colors[1])) + 
  geom_line(data=plottingDf, aes(x=date, y=Other, color = colors[2]))
ggplotly(presenting)

OtherPlot <- ggplot() + geom_line(data=Forecasts$forecasts, aes(x=date, y=Other, color = colors[1]))
ggplotly(OtherPlot)

plot(Forecasts$forecasts)
#Extract predictions and upper and lower limits
Year<-t(df2MatPredicts[103:112,1]);
Predict<-Forecasts$forecasts[3,103:112]
stdv<-sqrt(Forecasts$pred.var[3,3,2:11])
upper.lim=Predict+stdv*1.96
lower.lim=Predict-stdv*1.96
Out<-rbind(Year,Predict,upper.lim,lower.lim)
print(Out)

# plot results:
plot(df2MatPredicts[1:112,1], Forecasts$forecasts[3,],type="l", xlab="Year",
     ylab="Change in Log Number of Words", main="Prediction of change in log number of words")
lines(df2MatPredicts[1:102,1], df2MatPredicts[1:102,3], type="p")
grid(lty=3, lwd=1, col="grey")
Years<-seq(2021,2030,1)
lines(Years, Predict, type="l", col=2)
lines(Years, upper.lim, type="l", col=2)
lines(Years, lower.lim, type="l", col=2)
lines(c(2021,2020), c(0,2), col=4)



