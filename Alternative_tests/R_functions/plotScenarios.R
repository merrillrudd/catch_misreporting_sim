plotScenarios <- function(plot_catch){

	lmat <- matrix(c(1,2,3,4,5,
        					6,7,8,9,10,
        					6,7,8,9,10,
        					11,12,13,14,15,
        					11,12,13,14,15), nrow=5, ncol=5, byrow=TRUE)
	par(omi=c(1.5,1.5,1,1), mar=c(0,0,0,0))
    layout(lmat)
    # layout.show(n=10)
    cols <- brewer.pal(3, "Set1")

    xplot <- 1:datyrs
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        lines(x=xplot, y=rep(1,datyrs), col=cols[1], lwd=4)
        axis(2, at=seq(0.5, 1.5, by=0.5), las=2, cex.axis=1.7)
        mtext("100% Reporting", font=2, side=3, line=1.5, cex=1.25)
        mtext("Reporting\n  Rate", font=2, side=2, line=4, cex=1.5)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols[1], lty=2, lwd=2)
        lines(x=xplot, y=rep(0.5, datyrs), col=cols[2], lwd=4)
        mtext("Constant Under-reporting", font=2, side=3, line=1.5, cex=1.25)
		plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols[1], lty=2, lwd=2)
        lines(x=xplot, y=rep(1.5, datyrs), col=cols[2], lwd=4)
        mtext("Constant Over-reporting", font=2, side=3, line=1.5, cex=1.25)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        up <- (0.8-0.2)/(datyrs-1)*xplot + 0.2
        abline(h=1, col=cols[1], lty=2, lwd=2)
        lines(x=xplot, y=up, col=cols[3], lwd=4)
        mtext("Increasing Reporting", font=2, side=3, line=1.5, cex=1.25)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols[1], lty=2, lwd=2)
        down <- (0.2-0.8)/(datyrs-1)*xplot + 0.8
        lines(x=xplot, y=down, col=cols[3], lwd=4)
        mtext("Decreasing Reporting", font=2, side=3, line=1.5, cex=1.25)

	if(plot_catch %in% c(2,"all")){
		plot(x=xplot, y=catch1, type="l", lwd=5, ylim=c(0, max(catch1)*1.6), 
			xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=cols[1])
		lines(x=xplot, y=catch1, lwd=3)
		if(plot_catch!="all") axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		axis(side=2, at=pretty(c(1,catch1*1.6)), las=2, cex.axis=2)
		if(plot_catch=="all") mtext("2-way Catch", font=2, side=2, line=4.5, cex=1.5)
		plot(x=xplot, y=catch1, type="l", lwd=3, ylim=c(0, max(catch1)*1.6), 
			xaxs="i", yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch1*0.5, lwd=3, col=cols[2])
		if(plot_catch!="all") axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch1*1.5, lwd=3, col=cols[2])
		if(plot_catch!="all") axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch1*up, lwd=3, col=cols[3])
		if(plot_catch!="all") axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch1*down, lwd=3, col=cols[3])
		if(plot_catch!="all") axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		if(plot_catch!="all") mtext("Catch", font=2, side=2, line=4, cex=1.5)
	}

	if(plot_catch %in% c(1,"all")){
		plot(x=xplot, y=catch2, type="l", lwd=5, ylim=c(0, max(catch2)*1.6), 
			xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=cols[1])
		lines(x=xplot, y=catch2, lwd=3)
		axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		axis(side=2, at=pretty(c(1,catch2*1.6)), las=2, cex.axis=2)
		if(plot_catch=="all") mtext("1-way Catch", font=2, side=2, line=4.5, cex=1.5)
		plot(x=xplot, y=catch2, type="l", lwd=3, ylim=c(0, max(catch2)*1.6), 
			xaxs="i", yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch2*0.5, lwd=3, col=cols[2])
		axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch2, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch2)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch2*1.5, lwd=3, col=cols[2])
		axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch2, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch2)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch2*up, lwd=3, col=cols[3])
		axis(side=1, at=seq(10,30, by=10), cex.axis=2)
		plot(x=xplot, y=catch2, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch2)*1.6), 
			yaxs="i", xaxt="n", yaxt="n")
		lines(x=xplot, y=catch2*down, lwd=3, col=cols[3])
		axis(side=1, at=seq(10,30, by=10), cex.axis=2)
	}

	mtext("Year", outer=TRUE, font=2, line=3.5, cex=1.5, side=1)

}