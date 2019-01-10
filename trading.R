install.packages("quantmod")
install.packages("forecast")
install.packages("smooth")
install.packages("scales")
install.packages("rlist")
rm(list=ls())
dev.off()

library(rlist)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

symbols <- stockSymbols()

#DG <- getSymbols("WTI", src='av', api.key="N3JTQVF9QLHMA3BJ")
# from = as.Date("2017-07-23"), to = as.Date("2018-07-23"))

start <- Sys.Date()-183
end <- Sys.Date()+1
stock <- c("MSFT", "AMD", "DG", "FB", "TSLA", "WTI", "SPLK", "AAPL", "TSM", "CAT", 
           "NFLX", "GOOGL", "AMZN", "SOXL", "TWTR", "DIS", "GPRO", "F", "SBUX",
           "BAC", "BABA", "GE", "NVDA", "AVGO", "MTN", "HD", "INTC", "CMCSA", "EMTY",
           "CLIX", "NAIL", "VSLR", "AIEQ", "BOTZ", "ROBO", "SGOL", "ABX", "MIME", "WIX",
           "TEAM", "HUBS", "INST", "NOW", "ZEN", "APPF", "SD", "INTU", "CRM", "SHOP", 
           "TDOC", "SQ", "TTD", "CGC", "TLRY", "CRON", "TLRY", "MJ", "HQY", 
           "PYPL", "LOGM", "NIO", "SVMK", "TWLO", "ETSY", "ROKU", "DBX", "TECS",
           "TZA", "SPXS", "SOXS")

getSymbols(stock, src='yahoo',
           from = as.Date(start), to = as.Date(end))
TLRY <- na.omit(TLRY)


#testit <- function(x)
#{
# p1 <- proc.time()
# Sys.sleep(x)
# proc.time() - p1 # The cpu usage should be negligible
#}


#getOpen <- function(stock) {
#getSymbols(stock[order(stock)[1:5]], src="av", api.key=" N3JTQVF9QLHMA3BJ")
# 1+5
# testit(305)
#getSymbols(stock[order(stock)[6:10]], src="av", api.key=" N3JTQVF9QLHMA3BJ")
# 1+5
#}
#getOpen(stock)


#my_list=sapply(ls(),get)
#my_list <- my_list[-c(end, start)]


datasets <- list(MSFT$MSFT.Close, AMD$AMD.Close, DG$DG.Close, FB$FB.Close, TSLA$TSLA.Close, 
                 SPLK$SPLK.Close, AAPL$AAPL.Close, TSM$TSM.Close, CAT$CAT.Close,
                 NFLX$NFLX.Close, GOOGL$GOOGL.Close, AMZN$AMZN.Close, SOXL$SOXL.Close, 
                 TWTR$TWTR.Close, DIS$DIS.Close, GPRO$GPRO.Close, F$F.Close, SBUX$SBUX.Close, 
                 BAC$BAC.Close, BABA$BABA.Close, GE$GE.Close, NVDA$NVDA.Close, AVGO$AVGO.Close, MTN$MTN.Close,
                 HD$HD.Close, INTC$INTC.Close, CMCSA$CMCSA.Close, EMTY$EMTY.Close,
                 CLIX$CLIX.Close, NAIL$NAIL.Close, VSLR$VSLR.Close, AIEQ$AIEQ.Close,
                 BOTZ$BOTZ.Close, ROBO$ROBO.Close, SGOL$SGOL.Close, ABX$ABX.Close, 
                 MIME$MIME.Close, WIX$WIX.Close, TEAM$TEAM.Close, HUBS$HUBS.Close, INST$INST.Close, 
                 NOW$NOW.Close, ZEN$ZEN.Close, APPF$APPF.Close, SD$SD.Close, INTU$INTU.Close, 
                 CRM$CRM.Close, SHOP$SHOP.Close, TDOC$TDOC.Close, SQ$SQ.Close, TTD$TTD.Close, CGC$CGC.Close, 
                 CRON$CRON.Close, TLRY$TLRY.Close, MJ$MJ.Close, HQY$HQY.Close,
                 PYPL$PYPL.Close, LOGM$LOGM.Close, NIO$NIO.Close, SVMK$SVMK.Close, 
                 TWLO$TWLO.Close, ETSY$ETSY.Close, ROKU$ROKU.Close, DBX$DBX.Close, TECS$TECS.Close,
                 TZA$TZA.Close, SPXS$SPXS.Close, SOXS$SOXS.Close)

datasetsOpen <- list(MSFT$MSFT.Open, AMD$AMD.Open, DG$DG.Open, FB$FB.Open, TSLA$TSLA.Open, 
                     SPLK$SPLK.Open, AAPL$AAPL.Open, TSM$TSM.Open, CAT$CAT.Open,
                     NFLX$NFLX.Open, GOOGL$GOOGL.Open, AMZN$AMZN.Open, SOXL$SOXL.Open, 
                     TWTR$TWTR.Open, DIS$DIS.Open, GPRO$GPRO.Open, F$F.Open, SBUX$SBUX.Open, 
                     BAC$BAC.Open, BABA$BABA.Open, GE$GE.Open, NVDA$NVDA.Open, AVGO$AVGO.Open, MTN$MTN.Open,
                     HD$HD.Open, INTC$INTC.Open, CMCSA$CMCSA.Open, EMTY$EMTY.Open,
                     CLIX$CLIX.Open, NAIL$NAIL.Open, VSLR$VSLR.Open, AIEQ$AIEQ.Open,
                     BOTZ$BOTZ.Open, ROBO$ROBO.Open, SGOL$SGOL.Open, ABX$ABX.Open, 
                     MIME$MIME.Open, WIX$WIX.Open, TEAM$TEAM.Open, HUBS$HUBS.Close, INST$INST.Close, 
                     NOW$NOW.Close, ZEN$ZEN.Close, APPF$APPF.Close, SD$SD.Close, INTU$INTU.Close, 
                     CRM$CRM.Close, SHOP$SHOP.Close, TDOC$TDOC.Close, SQ$SQ.Close, TTD$TTD.Close, 
                     CGC$CGC.CLose, CRON$CRON.Close, TLRY$TLRY.Close, MJ$MJ.Close, 
                     HQY$HQY.Close, PYPL$PYPL.Close, LOGM$LOGM.Close, NIO$NIO.Close, SVMK$SVMK.Close,
                     TWLO$TWLO.Close, ETSY$ETSY.Close, ROKU$ROKU.Close, DBX$DBX.Close, TECS$TECS.Close,
                     TZA$TZA.Close, SPXS$SPXS.Close, SOXS$SOXS.Close)


names(datasets) <- c("MSFT", "AMD", "DG", "FB", "TSLA", "SPLK", "AAPL", "TSM", "CAT",
                     "NFLX", "GOOGL", "AMZN", "SOXL", "TWTR", "DIS", "GPRO", "F", "SBUX",
                     "BAC", "BABA", "GE", "NVDA", "AVGO", "MTN", "HD", "INTC", "CMCSA",
                     "EMTY", "CLIX", "NAIL", "VSLR", "AIEQ", "BOTZ", "ROBO", "SGOL", "ABX",
                     "MIME", "WIX", "TEAM", "HUBS", "INST", "NOW", "ZEN", "APPF", "SD", 
                     "INTU", "CRM", "SHOP", "TDOC", "SQ", "TTD", "CGC", "CRON", "TLRY", "MJ",
                     "HQY", "PYPL", "LOGM", "NIO", "SVMK", "TWLO", "ETSY", "ROKU", "DBX", "TECS",
                     "TZA", "SPXS", "SOXS")

names(datasetsOpen) <- c("MSFT", "AMD", "DG", "FB", "TSLA", "SPLK", "AAPL", "TSM", "CAT",
                         "NFLX", "GOOGL", "AMZN", "SOXL", "TWTR", "DIS", "GPRO", "F", "SBUX",
                         "BAC", "BABA", "GE", "NVDA", "AVGO", "MTN", "HD", "INTC", "CMCSA",
                         "EMTY", "CLIX", "NAIL", "VSLR", "AIEQ", "BOTZ", "ROBO", "SGOL", "ABX",
                         "MIME", "WIX", "TEAM", "HUBS", "INST", "NOW", "ZEN", "APPF", "SD", 
                         "INTU", "CRM", "SHOP", "TDOC", "SQ", "TTD", "CGC", "CRON", "TLRY", "MJ", 
                         "HQY", "PYPL", "LOGM", "NIO", "SVMK", "TWLO", "ETSY", "ROKU", "DBX", "TECS",
                         "TZA", "SPXS", "SOXS")
 
Buy <- data.frame(Stock=(character()), stringsAsFactors=FALSE)
Sell <- data.frame(Stock=(character()), stringsAsFactors=FALSE) 
Hold <- data.frame(Stock=(character()), stringsAsFactors=FALSE) 
DontOwn <- data.frame(Stock=(character()), stringsAsFactors=FALSE)
Slopes <- data.frame(Slopes=(character()), Stock=(character()), Price=(numeric()), stringsAsFactors=FALSE)
 
   
   
   
   makeplots <- function(data) {
       
         macd = MACD(data, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
         
           slope <- tail(data, 5)
           slope <- data.frame(slope)
           slope$day <- seq.int(nrow(slope))
           model <-lm(slope[,1] ~ slope$day)
           modelSum <- summary(model)
           thing <- as.data.frame(coef(model))
           thing <- as.data.frame(thing[2,])
           thing$Slopes <- thing[1,]
           thing$Stock <- names(data)
           thing$Price <- tail(data, 1)
           thing <- thing[,-1]
           Slopes <<- rbind(Slopes, thing)
           
             
             if (as.numeric(macd[nrow(data),1]) > 0 & as.numeric(macd[((nrow(data)) - 1), 1]) < 0) {
                 temp <- data.frame(names(data))
                 Buy <<- rbind(Buy, temp)
                 
                   ggplot(data) +
                     geom_line(aes(x = Index, data), size = 1) +
                     geom_line(aes(x = Index, y = EMA(data, n = 5)), color = "blue") +
                     geom_line(aes(x = Index, y = EMA(data, n = 20)), color = "red") +
                     scale_x_date(date_breaks = "1 week", labels = date_format("%d-%b")) +
                     theme(axis.text.x=element_text(angle=35,hjust=1)) +
                     labs(title = paste("BUY",names(data)), x = "Date", y = "Price") +
                     theme(plot.title = element_text(color="darkseagreen3", size=20, face="bold.italic"))
                 
                   
                   #plot(data, main = paste("BUY", names(data)), tck = -.05)
                   #lines(EMA(data, n = 5), col = "blue")
                   #lines(EMA(data, n = 20), col = "red")
                 } else if (as.numeric(macd[nrow(data),1]) < 0 & as.numeric(macd[((nrow(data)) - 1), 1]) > 0) {
                     temp <- data.frame(names(data))
                     Sell <<- rbind(Sell, temp)
                     
                       ggplot(data) +
                         geom_line(aes(x = Index, data), size = 1) +
                         geom_line(aes(x = Index, y = EMA(data, n = 5)), color = "blue") +
                         geom_line(aes(x = Index, y = EMA(data, n = 20)), color = "red") +
                         scale_x_date(date_breaks = "1 week", labels = date_format("%d-%b")) +
                         theme(axis.text.x=element_text(angle=35,hjust=1)) +
                         labs(title = paste("SELL",names(data)), x = "Date", y = "Price") +
                       theme(plot.title = element_text(color="orange", size=20, face="bold.italic"))
                     
                       
                       #plot(data, main = paste("SELL", names(data)), tck = -.05)
                       #lines(EMA(data, n = 5), col = "blue")
                       #lines(EMA(data, n = 20), col = "red")
                     } else if (as.numeric(macd[nrow(data),1]) > 0 & as.numeric(macd[((nrow(data)) - 1), 1]) > 0) {
                         temp <- data.frame(names(data))
                         Hold <<- rbind(Hold, temp)
                         
                           ggplot(data) +
                             geom_line(aes(x = Index, data), size = 1) +
                             geom_line(aes(x = Index, y = EMA(data, n = 5)), color = "blue") +
                             geom_line(aes(x = Index, y = EMA(data, n = 20)), color = "red") +
                             scale_x_date(date_breaks = "1 week", labels = date_format("%d-%b")) +
                             theme(axis.text.x=element_text(angle=35,hjust=1)) +
                             labs(title = paste("HOLD",names(data)), x = "Date", y = "Price") +
                             theme(plot.title = element_text(color="darkseagreen4", size=14, face="bold.italic"))
                         
                           
                          # plot(data, main = paste("HOLD",names(data)), tck = -.05)
                           #lines(EMA(data, n = 5), col = "blue")
                           #lines(EMA(data, n = 20), col = "red")
                         } else if(as.numeric(macd[nrow(data),1]) < 0 & as.numeric(macd[((nrow(data)) - 1), 1]) < 0) {
                             temp <- data.frame(names(data))
                             DontOwn <<- rbind(DontOwn, temp)
                             
                               ggplot(data) +
                               geom_line(aes(x = Index, data), size = 1) +
                               geom_line(aes(x = Index, y = EMA(data, n = 5)), color = "blue") +
                               geom_line(aes(x = Index, y = EMA(data, n = 20)), color = "red") +
                               scale_x_date(date_breaks = "1 week", labels = date_format("%d-%b")) +
                               theme(axis.text.x=element_text(angle=35,hjust=1)) +
                               labs(title = paste("DON'T OWN",names(data)), x = "Date", y = "Price") +
                               theme(plot.title = element_text(color="red", size=14, face="bold.italic"))
                            
                               
                              # plot(data, main = paste("Don't Own", names(data)), tck = -.05)
                              # lines(EMA(data, n = 5), col = "blue")
                             #lines(EMA(data, n = 20), col = "red")
                             }
          
          }
 
 lapply(datasets, makeplots)
 
Slopes$y2 <- Slopes$Slopes + Slopes$Price
Slopes$Change <- ((Slopes$y2-Slopes$Price)/Slopes$Price)*100
Slopes <- Slopes[order(-Slopes$Change),]
Slopes$Change <- round(Slopes$Change, digits = 2)

backtesting <- data.frame(Performance=(numeric()),Stock=(character()), stringsAsFactors=FALSE)

backtest <- function(data) {
  
  test = MACD(data, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
  new <- cbind(data, test)
  library(data.table)
  new <- data.table(new)
  new <- na.omit(new)
  
  new$action<- ifelse(new$macd > 0 & shift(new$macd) < 0,"buy","sell")
  new$action<- ifelse(new$macd < 0 & shift(new$macd) > 0 | new$macd > 0 & 
                        shift(new$macd) < 0,paste(new$action,""),1)
  
  lastprice <- data.frame(tapply(seq_along(new$action), new$action, max))
  if (lastprice["buy",1] > lastprice["sell",1]) new$action[nrow(new)] <- "sell"
  
  new <- dplyr::filter(new, grepl("buy|sell",new$action))
  new$math <- ifelse(new$action == "buy", new$AMD.Close - shift(new$AMD.Close), 21)
  
  new$math <- ifelse(grepl("sell", new$action), (new[,4] - shift(new[,4]))/new[,4]*100, "")
  new$math <- as.numeric(new$math)
  Performance <- data.frame(sum(new$math, na.rm = TRUE))
  Performance$Stock <- NA
  colnames(Performance) <- c("Performance", "Stock")
  Performance$Stock <- names(data)
  backtesting <- data.frame(Performance=(numeric()),Stock=(character()), stringsAsFactors=FALSE)
  backtesting <<- rbind(backtesting, Performance)
  
}

lapply(duh, backtest)

duh <- stock






backtest <- function(data) {

test = MACD(TLRY$TLRY.Close, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
new <- cbind(TLRY, test)
library(data.table)
new <- data.table(new)
new <- na.omit(new)

new$action<- ifelse(new$macd > 0 & shift(new$macd) < 0,"buy","sell")
new$action<- ifelse(new$macd < 0 & shift(new$macd) > 0 | new$macd > 0 & 
                      shift(new$macd) < 0,paste(new$action,""),1)

lastprice <- data.frame(tapply(seq_along(new$action), new$action, max))
if (lastprice["buy",1] > lastprice["sell",1]) new$action[nrow(new)] <- "sell"
  
new <- dplyr::filter(new, grepl("buy|sell",new$action))
#new$math <- ifelse(new$action == "buy", new$AMD.Close - shift(new$AMD.Close), 21)

new$math <- ifelse(grepl("sell", new$action), (new[,4] - shift(new[,4]))/new[,4]*100, "")
new$math <- as.numeric(new$math)
Performance <- data.frame(sum(new$math, na.rm = TRUE))
Performance$Stock <- NA
colnames(Performance) <- c("Performance", "Stock")
Performance$Stock <- names(TLRY$TLRY.Close)
backtesting <- data.frame(Performance=(numeric()),Stock=(character()), stringsAsFactors=FALSE)
backtesting <<- rbind(backtesting, Performance)

}



if (as.numeric(new$macd) > 0 & shift(new$macd) < 0) {
  yo
} 

else if (new$macd < 0 & shift(new$macd) > 0) {
  new$action <<- "sell"
} else {
  new$action <<- "nope"
}
new[, ya := macd + shift(macd)]


new[, try := 1]
DT[ , D := C + shift(B)]


 