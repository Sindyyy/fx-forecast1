mydf = read.table("USDTWD.txt", sep = " ",header = T)
names(mydf) = c("date", currency, factorNames, tech.factors)
# Compute technical indicators
preprocess <- function(mydf, lookback){
  
  p = mydf$NTN.1M.BGN.Curncy
  date = mydf$date[(1+lookback):(nrow(mydf))]

  ret = log(p[(lookback+1):nrow(mydf)]/p[1:(nrow(mydf)-lookback)]) # col-1: ret, col-2:price
  fac = mydf[1:(nrow(mydf)-lookback),c(2:(ncol(mydf)))]
  price = p[(lookback+1):nrow(mydf)]
  return(cbind(date,ret,price,fac)) 
}
lookback = 1
segmentationData = preprocess(mydf, lookback)
#segmentationData = segmentationData[sign(segmentationData$ret)!=0,] # eliminate non-trading days

ma5 = matrix(1,length(segmentationData$ret),0)
ma10 = matrix(1,length(segmentationData$ret),0)
momentum = matrix(1,length(segmentationData$ret),0)
roc = matrix(1,length(segmentationData$ret),0)
disparity5 = matrix(1,length(segmentationData$ret),0)
disparity10 =matrix(1,length(segmentationData$ret),0)
oscp = matrix(1,length(segmentationData$ret),0)
p = segmentationData$price
h10 = matrix(1,length(segmentationData$ret),0)
l10 = matrix(1,length(segmentationData$ret),0)
stoch.oscillator = matrix(1,length(segmentationData$ret),0)
ma.k = matrix(1,length(segmentationData$ret),0)
for (i in c(11:nrow(segmentationData))) {
  #calculate technical statistics
  ma5[i] = mean(p[(i-4):i])
  ma10[i] = mean(p[(i-9):i])
  momentum[i] = p[i]-p[i-4]
  roc[i] = p[i]/p[i-10]
  disparity5[i] = p[i]/ma5[i]
  disparity10[i] = p[i]/ma10[i]
  oscp[i] =( ma5[i]-ma10[i])/ma5[i]
  h10[i] = max(p[(i-9):i])
  l10[i] = min(p[(i-9):i])
  stoch.oscillator[i] = (p[i]-l10[i])/(h10[i] -l10[i] )
  ma.k[i] = mean(stoch.oscillator[(i-2):i])
}

tech = cbind(ma5,ma10,momentum,roc,disparity5,disparity10,oscp,stoch.oscillator,h10,l10,ma.k)
final.table = cbind(segmentationData,tech)[(12+lookback):nrow(tech),]
write.table(final.table,file = "lookback1.txt",row.names = FALSE)
