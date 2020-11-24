library(tidyverse)
library(zoo)
library(forecast)
library(xts)
library(grid)
library(kableExtra)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(dygraphs)
library(lubridate)
library(hrbrthemes)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
dati<-read.csv("newdati3.csv", header=T, sep=";", fileEncoding="latin1")


# dati<-as_tibble(dati)
# dati$datainizio<-dmy(dati$datainizio)
# dati<- mutate(dati, mese=paste(year(datainizio),'-',month(datainizio),sep=''))
# dati$mese<-as.Date((paste(dati$mese,"-01",sep="")))
# dati<-mutate(dati,anno=year(datainizio))
# dati$anno<-as.Date((paste(dati$anno,"-01","-01",sep="")))
# setvar<-summarise(group_by(dati,settore, mese),esami=sum(esami,na.rm=TRUE))%>%
#   mutate(var_change = (esami/lag(esami) - 1) * 100)


dati$Date<-as.Date(dati$datareg, format="%d/%m/%Y")
dati$anno <- year(dati$Date)

bg <- dati %>% 
  filter(reparto=="Sezione di Bergamo" & anno >= 2019) %>% 
  #filter(reparto=="Sezione di Bergamo") %>% 
  group_by(Date) %>% 
  summarise(es=sum(esami, na.rm = T))

esami <-xts(bg[,-1],order.by=bg$Date) 

#esami <- apply.weekly(esami$es, FUN = sum)




#esami <- subset(esami, esami$es >71)

#mseries <- cbind(esami, rollmean(esami,4.3), rollmean(esami, 52)) 

mseries <- cbind(esami, rollmean(esami,5), rollmean(esami, 25)) 




names(mseries) <- c("esami/giorno", "media mobile settimanale", "media mobile mensile")
index(mseries) <- as.Date(index(mseries))
# library(broom)
# tidy(mseries) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()
# dygraph(mseries$`media mobile mensile`)

#dygraph(mseries$es)

autoplot(mseries, geom = c("line")) +
  theme_ipsum_rc()

library(forecast)

fit <-auto.arima(esami)
fc_fit <- forecast(fit, h=3)


Obs<-fortify(fc_fit$x)
Pred<-fortify(fc_fit$mean)
Upper95<-fc_fit$upper[,2]
Upper80<-fc_fit$upper[,1]
Lower95<-fc_fit$lower[,2]
Lower80<-fc_fit$lower[,1]
#Lower<-fortify(fc4$lower)
Dates<-seq.Date(from=min(bg$Date), by="day", length.out = nrow(Obs)+nrow(Pred))

DFX<-data.frame(Date=Dates, y=c(Obs$y, Pred$y), UL95=c(rep(NA, nrow(Obs)), Upper95), LL95=c(rep(NA, nrow(Obs)), Lower95),
                UL80=c(rep(NA, nrow(Obs)), Upper80), LL80=c(rep(NA, nrow(Obs)), Lower80), 
                value=rep(c("observed", "predicted"), c(nrow(Obs), nrow(Pred))))

tozero<-function(x) { 
  x[x<0]<-0 
  x
}

DFX[,c(2:6)]<-apply(DFX[,c(2:6)], 2, tozero)
OBSERVED<-PREDICTED<-DFX$y
OBSERVED[DFX$value == "predicted"]<-NA
PREDICTED[DFX$value != "predicted"]<-NA

DFX2<-xts(data.frame(OBSERVED, PREDICTED=ceiling(PREDICTED), LCL= ceiling(DFX$LL95), UCL= ceiling(DFX$UL95)), order.by = as.Date(DFX$Date))

dygraph(DFX2, "esami previsti") %>%
  dyRangeSelector() %>% 
  dyRangeSelector(height = 50) %>%
  dySeries("OBSERVED", label = "osservati") %>%
  dySeries(c("LCL", "PREDICTED", "UCL"), label = "previsti") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")








```{r predvalue, echo=FALSE, warning=FALSE, results="asis", message=FALSE}


kable_styling(knitr::kable(data.frame(Data=format(DFX$Date[Pred$x], format="%d/%m/%Y"), "Casi predetti"=ceiling(DFX$y[Pred$x]), 
                                      #"ICL 80%"=round(DFX$LL80[Pred$x]), "ICU 80%"=round(DFX$UL80[Pred$x]), 
                                      "ICL 95%"=ceiling(DFX$LL95[Pred$x]), "ICU 95%"=ceiling(DFX$UL95[Pred$x]) ), format="html", 
                           col.names = c("Data", "Casi predetti", #"ICL 80%", "ICU 80%", 
                                         "ICL 95%", "ICU 95%")), full_width = F)

```
