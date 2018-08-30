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
           "TDOC", "SQ", "TTD", "CGC", "TLRY", "CRON", "TLRY", "MJ", "ACBFF")

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
                 CRON$CRON.Close, TLRY$TLRY.Close, MJ$MJ.Close, ACBFF$ACBFF.Close)

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
                 CGC$CGC.CLose, CRON$CRON.Close, TLRY$TLRY.Close, MJ$MJ.Close, ACBFF$ACBFF.Close)
                 

names(datasets) <- c("MSFT", "AMD", "DG", "FB", "TSLA", "SPLK", "AAPL", "TSM", "CAT",
                     "NFLX", "GOOGL", "AMZN", "SOXL", "TWTR", "DIS", "GPRO", "F", "SBUX",
                     "BAC", "BABA", "GE", "NVDA", "AVGO", "MTN", "HD", "INTC", "CMCSA",
                     "EMTY", "CLIX", "NAIL", "VSLR", "AIEQ", "BOTZ", "ROBO", "SGOL", "ABX",
                     "MIME", "WIX", "TEAM", "HUBS", "INST", "NOW", "ZEN", "APPF", "SD", 
                     "INTU", "CRM", "SHOP", "TDOC", "SQ", "TTD", "CGC", "CRON", "TLRY", "MJ", "ACBFF")

names(datasetsOpen) <- c("MSFT", "AMD", "DG", "FB", "TSLA", "SPLK", "AAPL", "TSM", "CAT",
                     "NFLX", "GOOGL", "AMZN", "SOXL", "TWTR", "DIS", "GPRO", "F", "SBUX",
                     "BAC", "BABA", "GE", "NVDA", "AVGO", "MTN", "HD", "INTC", "CMCSA",
                     "EMTY", "CLIX", "NAIL", "VSLR", "AIEQ", "BOTZ", "ROBO", "SGOL", "ABX",
                     "MIME", "WIX", "TEAM", "HUBS", "INST", "NOW", "ZEN", "APPF", "SD", 
                     "INTU", "CRM", "SHOP", "TDOC", "SQ", "TTD", "CGC", "CRON", "TLRY", "MJ", "ACBFF")

Buy <- data.frame(Stock=(character()),
                 stringsAsFactors=FALSE)
Sell <- data.frame(Stock=(character()),
                  stringsAsFactors=FALSE) 
Hold <- data.frame(Stock=(character()),
                  stringsAsFactors=FALSE) 
DontOwn <- data.frame(Stock=(character()),
                  stringsAsFactors=FALSE)



makeplots <- function(data) {
  
  macd = MACD(data, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
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

lapply(datasetsOpen, makeplots)

testing <- ggplot(MSFT) +
  geom_line(aes(x = Index, MSFT$MSFT.Close), size = 1) +
  geom_line(aes(x = Index, y = EMA(MSFT$MSFT.Close, n = 5)), color = "blue") +
  geom_line(aes(x = Index, y = EMA(MSFT$MSFT.Close, n = 20)), color = "red") +
  scale_x_date(date_breaks = "1 week", labels = date_format("%d-%b")) +
  theme(axis.text.x=element_text(angle=35,hjust=1)) +
  labs(title = names(MSFT$MSFT.Close), x = "Date", y = "Price") 



testing

makeDate <- funciton(data) {
  names(data) <<- ggplot(data, aes(x = index, y = data$Data.Close)) + geom_point()
}
makeDate(data)


getSymbols("IBM", src="av", api.key=" N3JTQVF9QLHMA3BJ")


eval <- function (data) {
  macd = MACD(data, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
  data$macd <<- macd$macd 
  
}

lapply(datasets, eval)

macdDG = MACD(DG$DG.Close, nFast=5, nSlow=20,nSig=1,maType=EMA, percent = FALSE)
DG$macd <- macdDG$macd
if (as.numeric(macdSPLK[nrow(SPLK$SPLK.Close),1]) > 0 & as.numeric(macdSPLK[((nrow(SPLK$SPLK.Close)) - 1), 1]) < 0) {
  buy <- SPLK
  temp <- data.frame(names(data))
  Buy <<- rbind(Buy, temp)
  plot(data, main = paste("BUY", names(data)), tck = -.05)
  lines(SMA(data, n = 5), col = "blue")
  lines(SMA(data, n = 30), col = "red")
} else if (as.numeric(macd[nrow(data),1]) < 0 & as.numeric(macd[((nrow(data)) - 1), 1]) > 0) {
  temp <- data.frame(names(data))
  Sell <<- rbind(Sell, temp)
  plot(data, main = paste("SELL", names(data)), tck = -.05)
  lines(SMA(data, n = 5), col = "blue")
  lines(SMA(data, n = 30), col = "red")
}


for (row in 1:nrow(SPLK$macd)) {
  SPLK$macd[row, ""]
  price <- stock[row, "apple"]
  date  <- stock[row, "date"]
  
  if(price > 117) {
    print(paste("On", date, 
                "the stock price was", price))
  }
}
