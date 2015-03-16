library(zoo)

args = commandArgs(T)

if(length(args)>1)
{
  qfn = args[1]
  factfn = args[2]
  winsize = args[2]
}else
{
  qfn = 'sz800.csv'
  factfn = 'sz800.vol.csv'
  # rebalance frequency
  freq = 20
  # number of quantiles
  quantN = 5
}

quote = read.zoo(qfn, stringsAsFactors=F, header=T, sep=',')
fact = read.zoo(factfn, stringsAsFactors=F, header=T, sep=',')

# clean quote series: fill 0 with most recent non-zeros
quote[which(quote==0)] = NA
quote = na.locf(quote)

# quote -> daily return
ret.zoo = diff(log(quote))

# TODO: need this line?
#ret.zoo[which(is.na(ret.zoo))] = 0

getrb <-function(fact, freq)
{
  # return data-frame of two columns, as start/end points of rebalancing.
  ind = index(fact)
  ind.seq = seq(from=1, to=length(ind), by=freq)
  if(ind.seq[length(ind.seq)] != length(ind))
  {
    ind.seq = c(ind.seq, length(ind))
  }
  # (starttk, endtk]
  starttk = ind.seq[-length(ind.seq)] + 1
  endtk = ind.seq[-1]
  
  starttk = ind[starttk]
  endtk = ind[endtk]
  
  return(data.frame(starttk, endtk))
}

rbtick = getrb(fact, freq)
fact.ret = matrix(0, ncol=quantN, nrow=NROW(ret.zoo))
fact.ret = zoo(fact.ret, order.by=index(ret.zoo))

# TODO: there're NA and 0 in fact, and 0 in ret
for(i in 1:NROW(rbtick))
{
  starttk = rbtick[i,1]
  endtk = rbtick[i,2]
  
  subret = window(ret.zoo, start=starttk, end=endtk)
  fact.row = as.vector(coredata(fact[starttk,]))
  fact.exclude = union(which(is.na(fact.row)), which(fact.row==0))
  fact.ord = setdiff(order(fact.row), fact.exclude)
  
  fact.grp = split(fact.ord, ceiling(seq_along(fact.ord)/(length(fact.ord)/quantN)))
  for(j in 1:length(fact.grp))
  {
    fact.weight = apply(subret[,fact.grp[[j]]] + 1, 2, cumprod)
    fact.weight = diff(log(c(1, apply(fact.weight, 1, mean))))
    window(fact.ret[,j], start=starttk, end=endtk) = fact.weight
  }
}

fact.perf = apply(fact.ret+1, 2, cumprod)
#fact.perf = rbind(0, fact.perf)
fact.perf = zoo(fact.perf, order.by=index(quote)[-1])

qcol = rainbow(quantN+5)
plot(fact.perf, plot.type='s', col=qcol)
legend('topleft', title='quantiles', as.character(1:quantN), col=qcol, lty=1, lwd=2)


