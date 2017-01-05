## Select libraries
library(metrumrg)
library(mrgsolve)
library(dplyr)


## Set model path and filename
projpath <- getwd()
runpath <- file.path(path.package("mrgsolve"),"nonmem","1005")
runno <- 1005

## Get model estimates 
nm.est <- read.csv(file.path(runpath,paste(runno,"ext",sep=".")),header=T,skip=1,sep="") %>% filter(ITERATION==-1E9)
theta <- nm.est[grep("THETA",names(nm.est))]
omega <- as_bmat(nm.est,"OMEGA")[[1]]
sigma <- as_bmat(nm.est,"SIGMA")[[1]]

## Get variance-covariance matrix
nm.cov <- read.csv(file.path(runpath,paste(runno,"cov",sep=".")),header=T,skip=1,sep="") %>% mutate(NAME=NULL)
th.index <- grep("THETA",names(nm.cov))
cov <- nm.cov[th.index,th.index]

## Generate simulation dataset where each row is one draw from var-cov matrix
simdta <- metrumrg::simpar(nsim=100,                # number of replicates
                           theta=unlist(theta),     # theta vector of fixed effects
                           covar=cov,               # var-covar matrix for fixed effects
                           omega=omega,sigma=sigma, # var-covar matrix for random effects
                           odf=1000,sdf=1000)       # degrees of freedom (df>length(matrix))

## Export dataset as csv file; can be used as input for PSN VPC for simulation
write.csv(sim.dta,paste(runpath,"simparam.csv",sep=""))