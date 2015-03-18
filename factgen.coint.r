library(zoo)
library(tseries)

args = commandArgs(T)

if(length(args)>1)
{
  qfn = args[1]
  idxfn = args[2]
  winsize = args[3]
  idx = args[4]
}else
{
  qfn = 'sz800.csv'
  idxfn = 'index.csv'
  winsize = 252
  idx = 1
}

quote = read.zoo(qfn, stringsAsFactors=F, header=T, sep=',')
index = read.zoo(idxfn, stringsAsFactors=F, header=T, sep=',')

# clean quote series: fill 0 with most recent non-zeros
alldata = merge(index, quote, all=F)
alldata[which(alldata==0)] = NA
alldata = na.locf(alldata)

test.coint <- function(x, ind=1)
{
  pvalues = rep(0, NCOL(x)-1)
  testcol = setdiff(1:NCOL(x), ind)
  for(i in 1:length(testcol))
  {
    c = testcol[i]
    ratio = as.vector(x[,c]/x[,ind])
    tr = adf.test(ratio, alternative="stationary")
    pvalues[i] = tr$p.value
  }
  return(pvalues)
}

# calc volatility
pvalues = rollapplyr(ret.zoo, width=winsize, FUN=test.coint, fill=NA, by.column=F)

# how many NA in each timestamps
na.count = apply(is.na(vol.zoo), 1, sum)
vol.out = vol.zoo[which(na.count!=NCOL(vol.zoo)),]

# if NA in vol.zoo, it means history quote is not long enough
# if 0 in vol.zoo, it means the price is really steady in the range

write.zoo(vol.out, file='sz800.vol.csv', sep=',')
