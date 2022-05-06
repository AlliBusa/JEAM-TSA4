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

# create plotting df 
plottingDf <- df
plottingDf$date <- lubridate::ym(plottingDf$yyyymm)
plottingDf <- plottingDf[, 2:11]

##### Question 3 ###########
# Question 3 using slow step
structSECOND <- define.model(kvar=10, ar=1, ma=1, rem.var=c(1,7:9), reg.var = 10, indep=NULL) # rem.var is to ignore the years
MSECOND <- marima(t(df), means=1, ar.pattern=structSECOND$ar.pattern,
             ma.pattern=structSECOND$ma.pattern, Check=FALSE, Plot="log.det", penalty=0)

MSECOND

slMSECOND <- step.slow.p(MSECOND, data=t(df))
slMSECOND


## Forecast ######

####### THIS PART NEEDS TO BE REWRITTEN ##########

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
Forecasts <-  arma.forecast(t(MatPredicts), nstart=1077, nstep=4, marima=slMSECOND) # named smething different####

forecastdf <- data.frame(
  t(Forecasts$forecasts),
  
)
forecastdf[,1] <- df[,1]
colnames(forecastdf) <- colnames(predictiondf)
forecastdf$date <- lubridate::ym(forecastdf$yyyymm)
forecastdf <- forecastdf[, 2:11]
forecastdf[1:1076,1:9] <- NA 
forecastdf[1077,1:9] <- plottingDf[1077,1:9]

#Extract predictions and upper and lower limits
intervalFunc <- function(i, plottingDf){
Predict<-Forecasts$forecasts[i,1078:1081]
stdv<-sqrt(Forecasts$pred.var[i,i,])
upper.lim=Predict+stdv*1.96
lower.lim= Predict-stdv*1.96
df <- data.frame(lower.lim, upper.lim)
emptyMatrix <- matrix(NA, nrow = 6, ncol = 2)
colnames(emptyMatrix) <- colnames(df)
df <- rbind(emptyMatrix,c(plottingDf[1077,i-1], plottingDf[1077,i-1]), df)
return(df)
}


plottingDfReduced <- plottingDf[1031:1081,]
plottingDfReduced <- forecastDf[1031:1081,]

# set colors for graph
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#C59900")

otherInterval <- intervalFunc(6, plottingDf)
presenting <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=Other, color = 'forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=Other, color = 'data')) +
  geom_vline(xintercept = plottingDf$date[1077], linetype = "dashed", color = 'n1') +
  geom_ribbon(data = otherInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim), 
              alpha = 0.1,
              fill = "grey70",
              color = "grey70")+
  theme_light()+
  scale_color_manual(values = c(colors[1], colors[2], colors[3]),
                      labels = c('Data', 'Forecast', 'n1') )+
  labs(colour="")+
  theme(legend.position="bottom")
presenting

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



