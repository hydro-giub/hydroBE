drawColorBar <- function(breaks=NULL,col=NULL,usr=par('usr'),
                         xleft=0.05,xright=0.08,
                         ybottom=0.25,ytop=0.25,
                         main='',main.cex=1.5,main.mar=0.01,
                         lab.cex=1,lwd=0.5,seg.len=0.2) {

    n <- length(col)
    lb <- pretty(breaks)

    ## set plot region
    dx <- usr[2]-usr[1]
    dy <- usr[4]-usr[3]
    xlim <- c(usr[2]+dx*xleft,usr[2]+dx*xright)
    ylim <- usr[3:4]+c(dy*ytop,-dy*ybottom)

    ## plot color bar
    xgap <- xlim[2]-xlim[1]
    y <- seq(ylim[1],ylim[2],length=n+1)
    rect(rep(xlim[1],n),y[1:n],rep(xlim[2],n),y[-1],
         col=col,border=col,xpd=NA,lwd=0)
    rect(xlim[1],ylim[1],xlim[2],ylim[2],
         border='black',xpd=NA,lwd=lwd)

    ## set labels
    at <- approx(x=breaks,y=y,xout=lb)$y
    na <- is.na(at)
    at <- at[!na]
    segments(x0=xlim[2],y0=at,x1=xlim[2]+xgap*seg.len,
             y1=at,xpd=NA,lwd=lwd)
    text(x=xlim[2]+xgap*seg.len,y=at,labels=lb[!na],
         pos=4,xpd=NA,cex=lab.cex)

    ## set title
    text(x=mean(xlim),y=ylim[2]+dy*main.mar,labels=main,
         pos=3,xpd=NA,cex=main.cex)

    return(invisible())

}
