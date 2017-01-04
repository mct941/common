read_nonmem <- function(file, n=-1) {
  ## auxiliary function to split text lines at blanks
  my.split <- function(line, numeric=FALSE) {
    pieces <- unlist(strsplit(line, split=" +"))[-1]
    if( numeric )
      return(as.numeric(pieces))
    else
      return(pieces)
  }
  
  cat(sprintf("Reading NONMEM data from '%s'\n", file))
  lines <- readLines(file, n) # read file as text
  cat(sprintf("- %d lines\n", length(lines)))
  
  idx <- substring(lines,1,1)!="T"
  cat(sprintf("- %d tables\n", sum(!idx)))
  lines <- lines[idx] # strip lines starting with T (TABLE NO ...)
  
  ## do we have header lines??
  if( length(grep("^ +[A-Za-z]", lines[1]))>0 ) { # yes!
    data <- sapply(lines[lines!= lines[1]], my.split, numeric=TRUE)
    header <-  my.split(lines[1])
    cat(sprintf("- file has column names (%s)\n", paste(header,collapse=", ")))
  } else {                                        # no
    data <- sapply(lines, my.split, numeric=TRUE)
    header <- sprintf("Column%02d", 1:nrow(data)) # make fake column names
    cat("- file has NO header names - creating names Column01, Column02, ...\n")
  }
  cat(sprintf("- %d columns\n", nrow(data)))
  
  ## transpose data and make a data.frame
  df <- data.frame(data[1,])
  for( i in 2:nrow(data))
    df <- cbind(df, data[i,])
  
  ## set column and row names
  rownames(df) <- NULL
  colnames(df) <- header
  cat("ok.\n")
  return(df)
}

## Generate diagnostic plots
gof_plot <- function(data,xvar,yvar,colorvar=NULL,shapevar=NULL,sizevar=NULL,smooth=NULL,caption=F,...) {
  dta <- data
  if(xvar=="DV") plot.range <- dta %>% select_(xvar,yvar) %>% summarise_each(c("min","max")) %>% range() 
  if(xvar!="DV") plot.range <- dta %>% select_(yvar) %>% summarise(min=-max(abs(.)),max=(max(abs(.)))) %>% unlist()
  
  p <- ggplot(dta) + geom_point(aes_string(x=xvar,y=yvar)) + theme(aspect.ratio=1)
  
  # Add points
  if(!is.null(colorvar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,color=colorvar)) 
  if(!is.null(shapevar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,shape=shapevar)) 
  if(!is.null(sizevar)) p <- p + geom_point(aes_string(x=xvar,y=yvar,size=sizevar)) 
  
  # Add unity lines
  if(xvar=="DV") p <- p + geom_abline(slope=1,intercept=0,linetype="dashed") + coord_cartesian(xlim=plot.range, ylim=plot.range)
  if(xvar!="DV") p <- p + geom_hline(yintercept=0,linetype="dashed") + coord_cartesian(ylim=plot.range)
  
  # Add trendlines
  if(!is.null(smooth)) {
    if(smooth=="lm") p <- p + geom_smooth(aes_string(x=xvar,y=yvar),method="lm") 
    if(smooth=="loess") p <- p + geom_smooth(aes_string(x=xvar,y=yvar),method="loess") 
  }
  # Add caption and save
  if(caption) {
    p <- p + labs(caption=paste(runpath,runno,sep=":"))
    ggsave(filename=paste(paste(xvar,yvar,sep="vs"),"jpg",sep="."),path=projpath)
  }  
  p
}

## Generate individual plots
indiv_plot <- function(data,
                       minID=NULL,maxID=NULL,
                       nrow=NULL,ncol=NULL,
                       xlog=F,ylog=F,
                       xlab="Time (hr)",ylab="Concentration (ng/mL)",
                       scales="fixed",index=1,caption=F,...) 
{ 
  # If subset of data are to be plotted
  if(!is.null(minID) &  !is.null(maxID)) dta <- data %>% filter(between(ID,minID,maxID)) else dta <- data
  
  p <- ggplot(dta) + 
    facet_wrap(~ID,labeller=label_both,scales=scales) +
    geom_point(aes(x=TIME,y=DV)) + 
    geom_line(aes(x=TIME,y=IPRE,color="red")) + 
    geom_line(aes(x=TIME,y=PRED,color="blue")) +
    scale_color_discrete(name="Prediction",labels=c("Individual","Population")) +
    labs(x=xlab,y=ylab)
  
  # Specify number of rows and columns
  if(!is.null(nrow)) p <- p + facet_wrap(~ID,labeller=label_both,scales=scales,nrow=nrow)  
  if(!is.null(ncol)) p <- p + facet_wrap(~ID,labeller=label_both,scales=scales,ncol=ncol)
  if(!is.null(nrow) &  !is.null(ncol)) p <- p + facet_wrap(~ID,labeller=label_both,scales=scales,nrow=nrow,ncol=ncol) 
  
  # Linear or log-linear scale
  if(xlog & !ylog) p <- p + scale_x_log10()
  if(!xlog & ylog) p <- p + scale_y_log10()
  if(xlog & ylog) p <- p + scale_x_log10() + scale_y_log10()
  
  # Add caption and save
  if(caption) {
    p <- p + labs(caption=paste(runpath,runno,sep=":"))
    ggsave(filename=paste(paste("individual plots",index,sep="_"),"jpg",sep="."),path=projpath)
  }
  p
}         
