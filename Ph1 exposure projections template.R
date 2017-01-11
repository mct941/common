#' ---
#' title: "Cmax and AUC Projections"
#' author: "Max Tsai"
#' date: "`r format(Sys.Date())`"
#' output: html_document
#' ---
#'
#' Initialization settings
rm(list=ls())
set.seed(12345)
source(file.path("C:","Users","c242871","Documents","common","ggplot themes.R"))
qc=F
## Set working directory
projpath <- getwd()
file <- ""

## Load libraries
library(ggplot2)
library(dplyr)

#' Get NCA parameters
#data <- read.csv(paste(file.path(projpath),file,"."))
## temporary placeholder
reps <- rlnorm(50,mean=log(10),sdlog=log(2))
data <- data.frame(DOSE=rep(c(3,10,30,100,300),10)) %>%
  mutate(CMAX=reps*DOSE,AUC=reps*(DOSE/(DOSE+30)))

## Data summary
head(data)

#' Run linear regression for PK parameters
## Log-transformed parameters are modeled
cmax.lm <- lm(log(CMAX)~log(DOSE),data=data)
auc.lm <- lm(log(AUC)~log(DOSE),data=data)
#' Set up new doses or dose range
## Discrete values are good for tables, continous values are good for figures
new.doses <- data.frame(DOSE=c(300,400,500))
new.doserange <- data.frame(DOSE=seq(0.3,1000,0.1))
#'  Define function to calculate confidence and prediction intervals 
calc_int <- function(lm.obj,new.range,...) {
  lm.ci <- predict(lm.obj,newdata=new.range,interval="confidence",level=0.95) %>% 
    data.frame() %>% rename(pred.ci=fit,lwr.ci=lwr,upr.ci=upr)
  lm.pi <- predict(lm.obj,newdata=new.range,interval="prediction",level=0.9) %>% 
    data.frame() %>% rename(pred.pi=fit,lwr.pi=lwr,upr.pi=upr)
  pred <-  bind_cols(lm.ci,lm.pi) %>% mutate_all(exp) 
  pred <-  pred %>% bind_cols(new.range,.)
  return(pred)
}

#' Predicted PK values with 95% CI (confidence intervals) and 90% PI (prediction intervals) using `calc_int`
## Cmax values
calc_int(cmax.lm,new.doses) %>% mutate_all(round, digits=1)

## AUC values
calc_int(auc.lm,new.doses) %>% mutate_all(round, digits=1)

#' Plot of predicted PK values over proposed dose range using `calc_int`
## Cmax values (solid line: prediction, dashed line: 95% CI, shaded area: 90% PI)
ggplot(data=calc_int(cmax.lm,new.doserange)) +
  geom_point(data=data,aes(x=DOSE,y=CMAX)) +
  geom_line(aes(x=DOSE,y=pred.ci)) +
  geom_line(aes(x=DOSE,y=lwr.ci),linetype="dashed") +
  geom_line(aes(x=DOSE,y=upr.ci),linetype="dashed") +
  geom_ribbon(aes(x=DOSE,ymin=lwr.pi,ymax=upr.pi),alpha="0.1",fill="red") +
  xlab("Dose (mg)") + ylab("Cmax (ng/mL)") + theme_basic()
if(qc) ggsave(paste(paste(projpath,paste("DOSEvsCMAX",format(Sys.Date(),"%d%b%Y"),sep="_"),sep="/"),"jpg",sep="."))
## AUC values (solid line: prediction, dashed line: 95% CI, shaded area: 90% PI)
ggplot(data=calc_int(auc.lm,new.doserange)) +
  geom_point(data=data,aes(x=DOSE,y=AUC)) +
  geom_line(aes(x=DOSE,y=pred.ci)) +
  geom_line(aes(x=DOSE,y=lwr.ci),linetype="dashed") +
  geom_line(aes(x=DOSE,y=upr.ci),linetype="dashed") +
  geom_ribbon(aes(x=DOSE,ymin=lwr.pi,ymax=upr.pi),alpha="0.1",fill="red") +
  xlab("Dose (mg)") + ylab("AUC (ng*hr/mL)") + theme_basic()
if(qc) ggsave(paste(paste(projpath,paste("DOSEvsAUC",format(Sys.Date(),"%d%b%Y"),sep="_"),sep="/"),"jpg",sep="."))
