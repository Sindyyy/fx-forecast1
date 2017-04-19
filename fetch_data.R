library(quantmod)
library(lubridate)
library(Rblpapi)


fetchData <- function( tickers,fields="PX_LAST"){
  # startdate = ISOdate( year(Sys.Date())-10, 1, 1 ) # start from 6 years ago, because if the month is not before current month we will not include current year
  enddate = date(ISOdate(2017,4,2))
  conn <- blpConnect()
  opt = c("periodicitySelection"="DAILY", "nonTradingDayFillOption" = "ALL_CALENDAR_DAYS")
  rawdat <- bdh(con = conn, c(tickers), fields, start.date = date(ISOdate(2004,1,1)), end.date = date(ISOdate(2017,4,2)), options = opt )
  
  #na.omit(rawdat)
  return(rawdat)
}

# merge the data according to date
mergeData <- function(frame.list){
  cols =  c("date",names(frame.list))
  rows = frame.list[[1]]$date
  mergedDat = data.frame(matrix( ncol = length(cols), nrow = length(rows) ))
  colnames(mergedDat) =  cols
  rownames(mergedDat) = rows
  mergedDat$date = rows
  for (t in names(frame.list)){
    dat = factorData[[t]]
    if (any(is.na(dat))){
      cat(t, "is null")
    }
 
    mergedDat[[t]] = factorData[[t]]$PX_LAST
  }
  
  return (mergedDat)
}

currency = "NTN+1M BGN Curncy"

factors = c("CPI YOY Index","TWBKREP Index","SPX Index",
  "TWSE Index","XAU BGN Curncy","TWLFADJ Index","TBEXTWAN Index","TWEDUSA Index",
            "TWMSAM2Y Index","TWTREXPY Index","TWINDPIY Index", "TWIRFE Index","TWBCR Index",
            "TAREDSC  Index","TWTRBAL  Index","GDP CQOQ Index","TWBOPCUR Index","JPMVXYGL Index",
            "FDTR Index",  "TWGDCONY Index","TWWPIYOY Index",
            "CL1 COMB Comdty", "USDJPY BGN Curncy") # "PMITWMA Index",
tech.factors = c("RSI_3D","RSI_9D","RSI_14D","RSI_30D",
                 "PX_HIGH","PX_LOW")
factorNames = c("US CPI (RHS)","TW repo rate","SPX Index",
  "TW Stock Exchange Index"," Gold (RHS)","TW Unemployment Rate","US Exports to TW","TW Export to US", 
                "TW M2","TW Export YoY"," TW Industrial Production YoY", "TW Foreign Reserves","TW Bounced Check Ratio",
                "CBC Discount Rate","TW Merchandise Trade Balance"," US GDP QoQ (RHS) ","TW Current Account Balance","FX VIX",
                 "US FEd Fund Rate","TW GDP","TW WPI",
                "WTI Oil","USDJPY" ) # 
usdtwd =fetchData(currency) 
factorData = fetchData(factors)
mergedFactorData = mergeData(factorData)
conn <- blpConnect()
opt = c("periodicitySelection"="DAILY", "nonTradingDayFillOption" = "ALL_CALENDAR_DAYS")
techData <- bdh(con = conn, c(currency), tech.factors, start.date = date(ISOdate(2004,1,1)), end.date=date(ISOdate(2017,4,2)), options = opt )
dat = cbind( usdtwd, mergedFactorData[,-1], techData[,-1])
names(dat) = c("date", currency, factorNames, tech.factors)

write.table(dat,"USDTWD.txt", col.names = T, row.names = F )



