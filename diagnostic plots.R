

##
# Author: Max Tsai
# Date: 
# Purpose:  Generate goodness-of-fit plots for model evaluation and data exploration


## Select libraries
library(ggplot)
library(gridExtra)
library(dplyr)

## Clear environment
rm(list=ls())

## Set model path and filename
projpath <- getwd()
runpath <- file.path(path.package("mrgsolve"),"nonmem","1005")
runno <- 1005

file.tab <- read.nonmem(file.path(runpath,paste(runno,".tab",sep=""))) %>% tbl_df() %>% filter(EVID==0)

## Generate diagnostic plots
gof_plot <- function(xvar,yvar,colorvar=NULL,shapevar=NULL,sizevar=NULL,caption=NULL,...) {
  if(xvar=="DV") plot.range <- file.tab %>% select_(xvar,yvar) %>% summarise_each(c("min","max")) %>% range() 
  if(xvar!="DV") plot.range <- file.tab %>% select_(yvar) %>% summarise(min=-max(abs(.)),max=(max(abs(.)))) %>% unlist()
  p <- ggplot(file.tab) + 
         geom_smooth(aes_string(x=xvar,y=yvar),method="lm") + 
         theme(aspect.ratio=1)
         
  if(xvar=="DV") p <- p + geom_abline(slope=1,intercept=0,linetype="dashed") + coord_cartesian(xlim=plot.range, ylim=plot.range)
  if(xvar!="DV") p <- p + geom_hline(yintercept=0,linetype="dashed") + coord_cartesian(ylim=plot.range)
  if(!is.null(colorvar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,color=colorvar)) 
  else p <- p + geom_point(aes_string(x=xvar,y=yvar)) 
  if(!is.null(shapevar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,shape=shapevar)) 
  else p <- p + geom_point(aes_string(x=xvar,y=yvar)) 
  if(!is.null(sizevar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,size=sizevar)) 
  else p <- p + geom_point(aes_string(x=xvar,y=yvar))
  if(!is.null(caption)) p <- p + labs(caption=paste(runpath,runno,sep=":"))
#  ggsave(filename=paste(paste(xvar,yvar,sep="vs"),"jpg",sep="."),path=projpath)
  p
}

# Remove comment symbol for interactive plots
#library(plotly)
#ggplotly(gof_plot("PRED","CWRES", sizevar="ID"))


p <- grid.arrange(gof_plot("DV","PRED"),gof_plot("PRED","RES"),gof_plot("PRED","CWRES"),
                  gof_plot("DV","IPRE"),gof_plot("TIME","RES"),gof_plot("TIME","CWRES"),
                  nrow=2)
ggsave(plot=p,filename=paste(runno,"gof_plot.jpg",sep="-"),path=projpath)             
