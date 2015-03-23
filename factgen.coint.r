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
alldata = merge(index[,idx], quote, all=F)
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
    tr = try(adf.test(ratio, alternative="stationary"), silent=T)
    if(is(tr)=='try-error')
    {
      pvalues[i] = NA
    }else{
      pvalues[i] = tr$p.value
    }
  }
  #print(c(NROW(x), index(x)[1]))
  return(pvalues)
}

# calc volatility
pvalues = rollapplyr(alldata, width=winsize, FUN=test.coint, fill=NA, by.column=F)

# how many NA in each timestamps
na.count = apply(is.na(pvalues), 1, sum)
pvalues.out = pvalues[which(na.count!=NCOL(pvalues)),]

# if NA in vol.zoo, it means history quote is not long enough
# if 0 in vol.zoo, it means the price is really steady in the range

write.zoo(pvalues.out, file='sz800.coint.csv', sep=',')
