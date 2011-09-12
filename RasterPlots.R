PlotRasterFromSweeps<-function(sweepdir,sweeps,odourRange=c(2000,2500),xlim=c(0,5000),
  dotcolour='black',dotsize=0.5,odourCol=rgb(1,0,0,alpha=.3),
  main,sub,relabelfun){
  # Read in all spike times
  ff=dir(sweepdir,'^[0-9]{3}_SP_',full=T)
  rasterd=lapply(ff,read.table,col.names=c("Time","Sweep"),header=T, 
    na.strings='NAN')
  names(rasterd)=substring(basename(ff),1,3)
  
  if(missing(sweeps)){
    sweeps=names(rasterd)
  } else {
    sweeps=sprintf("%03d",sweeps)
    rasterd=rasterd[sweeps]
  }

  # read in ODD protocol
  oddfiles=file.path(sweepdir,paste(basename(sweepdir),sweeps,"odd.txt",sep="_"))
  oddconf=read.table(oddfiles[1],col.names=make.unique(c('odour',rep(c('del','dur','chan'),5))))

  last_sweep=max(sapply(rasterd,function(x) max(x$Sweep,na.rm=T)))
  plot(NA,xlim=xlim,ylim=c(last_sweep+1,0),xlab='Time/ms',ylab='Odour',axes=F)
  axis(side=2,at=seq(last_sweep+1)-0.5,labels=oddconf$odour,tick=F,las=1)
  axis(1)
  nreps=length(rasterd)
  for(i in seq(rasterd)){
    yoff=i/(nreps+1)
    print(yoff)
    df=rasterd[[i]]
    points(x=df$Time,y=df$Sweep+yoff,pch=22,col=NA,bg=dotcolour,cex=dotsize)
  }
  rect(odourRange[1],-0.5,odourRange[2],last_sweep+1.5,col=odourCol,border=NA)
  for(i in seq(last_sweep+1)){
    rect(0,i-1,5000,i,col=NA,border='black')
  }
  if(missing(main)) main=""
  if(missing(sub)) sub=paste("Cell:",basename(sweepdir),"sweeps:",paste(sweeps,collapse=","))  
  title(main=main,sub=sub)
}