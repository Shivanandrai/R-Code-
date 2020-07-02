install.packages("readxl")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
library(readxl)
SectorData <- read_excel("/Users/shivarai/Desktop/ESCP/R Class/Table_2.1_Energy_Consumption_by_Sector.xlsx"
)

SectorData<- SectorData[ -c(2,4,6,8,10,12) ]
SectorDataTS <-ts(SectorData, start=1973, frequency = 12)

library(ggplot2)
library(ggfortify)
library(forecast)

help(ggplot) 

df<- data.frame(SectorData)

plot.ts(SectorDataTS)
autoplot(SectorDataTS, ts.colour = 'blue', xlab="Month", ylab="Trillion BTU")

ggseasonplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"], season.labels = NULL, year.labels= FALSE,
             year.labels.left= FALSE)

ggseasonplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"], polar = TRUE, season.labels = NULL, year.labels = FALSE,
             year.labels.left = FALSE)

ggsubseriesplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"])

gglagplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"], lags=40, nrow=NULL, ncol=NULL)

component.ts=decompose(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"])
plot(component.ts)

withoutSeasonal <- SectorDataTS[,"Total Energy Consumed by the Commercial Sector"]-component.ts$seasonal
plot(withoutSeasonal)

ndiffs(withoutSeasonal)
StationarydataAfterDiff<-diff(withoutSeasonal)
plot(StationarydataAfterDiff)

Acf(StationarydataAfterDiff,plot=FALSE)
Acf(StationarydataAfterDiff,pl5ot=TRUE)
Pacf(StationarydataAfterDiff,plot=TRUE)

#The ideal P & Q would be 1 & 2. 

AIC(arima(StationarydataAfterDiff, order=c(1,0,2)), arima(StationarydataAfterDiff, order=c(1,0,4)))

trainingModel <-auto.arima(SectorData[1:100,"Total Energy Consumed by the Commercial Sector"], stationary = FALSE, seasonal = 
                             TRUE)
testModel<- Arima(SectorData[101:561,"Total Energy Consumed by the Commercial Sector"], model=trainingModel)

#I had to tweak the number of rows because ARIMA was giving an error which said there was not enough data to do a fitting

accuracy(testModel)

trainingModel_pred <-auto.arima(SectorData[,"Total Energy Consumed by the Commercial Sector"], stationary = FALSE, seasonal = 
                             TRUE)
forecast_year<-forecast(trainingModel_pred, h=12)
plot(forecast_year)



