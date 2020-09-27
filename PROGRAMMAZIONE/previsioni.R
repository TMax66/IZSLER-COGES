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


 
bg <- dati %>% 
  filter(reparto=="Sezione di Bergamo") %>% 
  group_by(Date) %>% 
  summarise(es=sum(esami, na.rm = T))

esami <-xts(bg[,-1],order.by=bg$Date) 

esami <- apply.monthly(esami$es, FUN = sum)
mseries <- cbind(esami, rollmean(esami,30))
names(mseries) <- c("esami", "media mobile mensile")
index(mseries) <- as.Date(index(mseries))
library(broom)
tidy(mseries) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()
dygraph(mseries$`media mobile mensile`)

autoplot(mseries, geom = c("line"))

library(forecast)
esamiprev <- HoltWinters(esami,beta=FALSE, gamma=FALSE)
#esamiprev2 <- forecast:::forecast.HoltWinters(esamiprev, h=12)


p <- predict(esamiprev, n.ahead = 12, prediction.interval = TRUE)
all <- cbind(esami, p)

dygraph(all, "Deaths from Lung Disease (UK)") %>%
  dySeries("ldeaths", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")





dygraph(esamiprev2)





dataX<-as.xts(Data, frequency =1)
#fitHW<-ets(dataX)
fitARIMA <- Arima(dataX, order=c(0,2,1), seasonal = list(order = c(0,1,1), period=7), lambda="auto", biasadj = T)
fc4<-forecast(fitARIMA, h=7)#, lev=80)
#fc4<-forecast(fitHW, h=7)

Obs<-fortify(fc4$x)
Pred<-fortify(fc4$mean)
Upper95<-fc4$upper[,2]
Upper80<-fc4$upper[,1]
Lower95<-fc4$lower[,2]
Lower80<-fc4$lower[,1]
#Lower<-fortify(fc4$lower)
Dates<-seq.Date(from=min(CovidIT$Date), by="day", length.out = nrow(Obs)+nrow(Pred))

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



dygraph(DFX2, "nuovi casi") %>%
  dyRangeSelector() %>% 
  dyRangeSelector(height = 50) %>%
  dySeries("OBSERVED", label = "osservati") %>%
  dySeries(c("LCL", "PREDICTED", "UCL"), label = "previsti") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")



#ggplot(DFX, aes(x=Date, y=y, col=value))+scale_x_date(date_labels = "%d/%m", date_breaks = "7 day",  minor_breaks = "1 day")+
#  geom_ribbon(aes(ymin = LL95, ymax = UL95), fill =  "slategray2", linetype=0)+
#   geom_ribbon(aes(ymin = LL80, ymax = UL80), fill = "slateblue1", alpha=.7, linetype=0)+
#  geom_line(size=1, aes(linetype=value)) +labs(x="data",  y="nuovi casi") +
#  scale_color_manual(values=c("black", "black"))+
#  scale_linetype_manual(values=c("solid", "dashed"))+
#  theme_minimal()+scale_y_continuous(n.breaks = 6)+
#  theme(legend.position = "none",  plot.title = element_text(hjust = 0.5))+
#  ggtitle(paste("previsione (al ", format(max(CovidIT$Date), format="%d/%m/%Y"), ")", sep=""))




```


```{r predvalue, echo=FALSE, warning=FALSE, results="asis", message=FALSE}


kable_styling(knitr::kable(data.frame(Data=format(DFX$Date[Pred$x], format="%d/%m/%Y"), "Casi predetti"=ceiling(DFX$y[Pred$x]), 
                                      #"ICL 80%"=round(DFX$LL80[Pred$x]), "ICU 80%"=round(DFX$UL80[Pred$x]), 
                                      "ICL 95%"=ceiling(DFX$LL95[Pred$x]), "ICU 95%"=ceiling(DFX$UL95[Pred$x]) ), format="html", 
                           col.names = c("Data", "Casi predetti", #"ICL 80%", "ICU 80%", 
                                         "ICL 95%", "ICU 95%")), full_width = F)

```
