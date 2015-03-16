library(zoo)
library(tseries)

args = commandArgs(T)

if(length(args)>1)
{
  qfn = args[1]
  winsize = args[2]
}else
{
  qfn = 'sz800.csv'
  idxfn = 'index.csv'
  idx = 1
  winsize = 252
}

quote = read.csv(qfn, stringsAsFactors=F)
quote.zoo = zoo(quote[,-1], order.by=as.Date(quote[,1]))

# NOTE: divide between zoo object is automatically merged to common indexes.

# clean quote series: fill 0 with most recent non-zeros
quote.zoo[which(quote.zoo==0)] = NA
quote.zoo = na.locf(quote.zoo)

# quote -> daily return
ret.zoo = diff(log(quote.zoo))

# calc volatility
vol.zoo = rollapplyr(ret.zoo, width=winsize, FUN=sd, fill=NA)

adf.test(sprd, alternative="stationary", k=0)

# how many NA in each timestamps
na.count = apply(is.na(vol.zoo), 1, sum)
vol.out = vol.zoo[which(na.count!=NCOL(vol.zoo)),]

# if NA in vol.zoo, it means history quote is not long enough
# if 0 in vol.zoo, it means the price is really steady in the range

write.zoo(vol.out, file='sz800.vol.csv', sep=',')
