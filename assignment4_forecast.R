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

# step slow ("step.slow.p.marima_2017.R" function in the same working directory is necessary!)
ReducedMARIMAModel <- step.slow.p(MSECOND, data=t(df))
ReducedMARIMAModel


##################### Forecast #############################

predictiondf <- df[1:1077,] 
#add new rows for predicted/forecasted values
newData <- matrix(0, nrow = 4, ncol = 10)
newData[,1] <- seq(201609,201612,1) #add dates we want to predict for
newData[,2:10] <- 0 
colnames(newData) <- colnames(predictiondf)

#add prediction data to main df
MatPredicts <- rbind(predictiondf, newData)
#do forecasts
Forecasts <-  arma.forecast(t(MatPredicts), nstart=1077, nstep=4, marima=ReducedMARIMAModel) # named smething different####
# move forecasted values to dataframe
forecastdf <- data.frame(
  t(Forecasts$forecasts)
)
# adjust dataframe with right values
forecastdf[,1] <- df[,1]
colnames(forecastdf) <- colnames(predictiondf)
forecastdf$date <- lubridate::ym(forecastdf$yyyymm) # attention: moves date column to last column of the table
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
# set colors for graph
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#C59900","black","grey70")


######################### Forecast PLOTS ################################

######### PLOT Cnsmr #################
# Extracts the right intervals for the different columns to plot (e.g.: Column "2" -> "Cnsmr" to "6" -> "Other")
CnsmrInterval <- intervalFunc(2, plottingDf)
# prepares the plot function
presentingCnsmr <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=Cnsmr, color = 'Forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=Cnsmr, color = 'Actual Data')) +
  geom_vline( aes(xintercept = plottingDf$date[1077], color = 'Last Observation'),linetype = "dashed") +
  geom_ribbon(data = CnsmrInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim, color = '95% Prediction Interval'), 
              alpha = 0.1,
              fill = "grey70")+
  theme_light()+
  scale_linetype_manual(values=c("solid","solid","dashed","solid"))+
  scale_color_manual(values = c(colors[7], colors[1], colors[2], colors[6]))+
  labs(colour="")+
  theme(legend.position="bottom")
# plots the forecast plot
presentingCnsmr

######### PLOT Manuf #################
# Extracts the right intervals for the different columns to plot (e.g.: Column "2" -> "Cnsmr" to "6" -> "Other")
ManufInterval <- intervalFunc(3, plottingDf)
# prepares the plot function
presentingManuf <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=Manuf, color = 'Forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=Manuf, color = 'Actual Data')) +
  geom_vline( aes(xintercept = plottingDf$date[1077], color = 'Last Observation'),linetype = "dashed") +
  geom_ribbon(data = ManufInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim, color = '95% Prediction Interval'), 
              alpha = 0.1,
              fill = "grey70")+
  theme_light()+
  scale_linetype_manual(values=c("solid","solid","dashed","solid"))+
  scale_color_manual(values = c(colors[7], colors[1], colors[2], colors[6]))+
  labs(colour="")+
  theme(legend.position="bottom")
# plots the forecast plot
presentingManuf

######### PLOT HiTec #################
# Extracts the right intervals for the different columns to plot (e.g.: Column "2" -> "Cnsmr" to "6" -> "Other")
HiTecInterval <- intervalFunc(4, plottingDf)
# prepares the plot function
presentingHiTec <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=HiTec, color = 'Forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=HiTec, color = 'Actual Data')) +
  geom_vline( aes(xintercept = plottingDf$date[1077], color = 'Last Observation'),linetype = "dashed") +
  geom_ribbon(data = HiTecInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim, color = '95% Prediction Interval'), 
              alpha = 0.1,
              fill = "grey70")+
  theme_light()+
  scale_linetype_manual(values=c("solid","solid","dashed","solid"))+
  scale_color_manual(values = c(colors[7], colors[1], colors[2], colors[6]))+
  labs(colour="")+
  theme(legend.position="bottom")
# plots the forecast plot
presentingHiTec

######### PLOT Hlth #################
# Extracts the right intervals for the different columns to plot (e.g.: Column "2" -> "Cnsmr" to "6" -> "Other")
HlthInterval <- intervalFunc(5, plottingDf)
# prepares the plot function
presentingHlth <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=Hlth, color = 'Forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=Hlth, color = 'Actual Data')) +
  geom_vline( aes(xintercept = plottingDf$date[1077], color = 'Last Observation'),linetype = "dashed") +
  geom_ribbon(data = HlthInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim, color = '95% Prediction Interval'), 
              alpha = 0.1,
              fill = "grey70")+
  theme_light()+
  scale_linetype_manual(values=c("solid","solid","dashed","solid"))+
  scale_color_manual(values = c(colors[7], colors[1], colors[2], colors[6]))+
  labs(colour="")+
  theme(legend.position="bottom")
# plots the forecast plot
presentingHlth

######### PLOT Other #################
# Extracts the right intervals for the different columns to plot (e.g.: Column "2" -> "Cnsmr" to "6" -> "Other")
OtherInterval <- intervalFunc(6, plottingDf)
# prepares the plot function
presentingOther <- ggplot() + 
  geom_line(data=forecastdf[1070:1081,], aes(x=date, y=Other, color = 'Forecast')) + 
  geom_line(data=plottingDf[1070:1081,], aes(x=date, y=Other, color = 'Actual Data')) +
  geom_vline( aes(xintercept = plottingDf$date[1077], color = 'Last Observation'),linetype = "dashed") +
  geom_ribbon(data = OtherInterval, 
              aes(x = forecastdf[1071:1081,'date'],
                  ymin = lower.lim, ymax = upper.lim, color = '95% Prediction Interval'), 
              alpha = 0.1,
              fill = "grey70")+
  theme_light()+
  scale_linetype_manual(values=c("solid","solid","dashed","solid"))+
  scale_color_manual(values = c(colors[7], colors[1], colors[2], colors[6]))+
  labs(colour="")+
  theme(legend.position="bottom")
# plots the forecast plot
presentingOther




