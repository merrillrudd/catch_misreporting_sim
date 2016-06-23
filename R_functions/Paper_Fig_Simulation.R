Paper_Fig_Simulation <- function(results, bw=FALSE){
	par(mfrow=c(2,5), mar=c(3,0,0,0), omi=c(1,2,1,1))
## catch time series
xplot <- 1:datyrs
if(bw==FALSE){
	cols <- brewer.pal(4, "Set1")
	lty_type <- 2
}
if(bw==TRUE){
	cols <- rep(gray(0.2), 4)
	lty_type <- 2
}
up <- (0.8-0.2)/(datyrs-1)*xplot + 0.2
down <- (0.2-0.8)/(datyrs-1)*xplot + 0.8
    plot(x=xplot, y=catch1, type="l", lwd=3, ylim=c(0, max(catch1)*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.2))
    axis(2, at=pretty(c(1,catch1*1.6)), cex.axis=3)
    mtext("Reported\ncatch",  side=2, line=5, cex=3)
    mtext("100% Reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, ylim=c(0, max(catch1)*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.2), lty=lty_type)
    lines(x=xplot, y=catch1*0.5, lwd=3, col=cols[1])
     mtext("Constant Under-reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.2), lty=lty_type)
    lines(x=xplot, y=catch1*1.5, lwd=3, col=cols[2])
     mtext("Constant Over-reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.2), lty=lty_type)
    lines(x=xplot, y=catch1*up, lwd=3, col=cols[3])
     mtext("Increasing Reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.2), lty=lty_type)
    lines(x=xplot, y=catch1*down, lwd=3, col=cols[4])
     mtext("Decreasing Reporting",  side=3, line=1.5, cex=1.9)

	plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(1, at=seq(10,30, by=10), cex.axis=3)
	axis(2, at=seq(0, 1850, by=500), cex.axis=3)
	mtext("Estimated\nbiomass",  side=2, line=5, cex=3)
	plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), lty=lty_type, type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	lines(x=xplot, y=res$b_est[1,4,], col=cols[1], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(1, at=seq(10,30, by=10), cex.axis=3)
	plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), lty=lty_type, type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	mtext("Year", side=1, line=5, cex=3)
	lines(x=xplot, y=res$b_est[1,6,], col=cols[2], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(1, at=seq(10,30, by=10), cex.axis=3)
	plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), lty=lty_type, type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	lines(x=xplot, y=res$b_est[1,12,], col=cols[3], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(1, at=seq(10,30, by=10), cex.axis=3)
	plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), lty=lty_type, type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	lines(x=xplot, y=res$b_est[1,14,], col=cols[4], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(1, at=seq(10,30, by=10), cex.axis=3)
}