
auc.calc <- function(TIME,CONC)
{
  auc.total <- 0
  n <- length(TIME)
  for(i in 1:(n - 1)) {
    auc.int <- 0.5 * (TIME[i + 1] - TIME[i]) * (CONC[i + 1] + CONC[i])
    auc.total <- auc.int + auc.total
  }
  return(auc.total)
}

auc.calc <- function(TIME,CONC)
{
  n <- length(unique(TIME))
  sum(sapply(1:(n-1), FUN=function(x) {0.5*(TIME[x+1]-TIME[x])*(CONC[x+1]+CONC[x])}))
}


tmax.calc <- function(TIME,CONC) TIME[which.max(CONC)]
