##
# Author: Max Tsai
# Date: 
# Purpose:  Generate goodness-of-fit plots for model evaluation and data exploration


## Select libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

## Clear environment
rm(list=ls())

## Set model path and filename
projpath <- getwd()
runpath <- file.path(path.package("mrgsolve"),"nonmem","1005")
runno <- 1005

file.tab <- read_nonmem(file.path(runpath,paste(runno,".tab",sep=""))) %>% tbl_df() %>% filter(EVID==0)

# Remove comment symbol for interactive plots
#library(plotly)
#ggplotly(gof_plot("PRED","CWRES", sizevar="ID"))

## define x and y variables to select gof plots
p <- grid.arrange(gof_plot("DV","PRED"),gof_plot("PRED","RES"),gof_plot("PRED","CWRES"),
                  gof_plot("DV","IPRE"),gof_plot("TIME","RES"),gof_plot("TIME","CWRES"),
                  nrow=2)
ggsave(plot=p,filename=paste(runno,"gof_plot.jpg",sep="-"),path=projpath)             
