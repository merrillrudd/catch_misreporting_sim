plotTruth <- function(plot_index=FALSE, plotOM){
	
	par(mfrow=c(1,1), mar=c(6,7,6,6))
	OM <- plotOM
	plot(OM$Biomass, type="l", col="forestgreen", lwd=4,
		xaxs="i", yaxs="i", ylim=c(0,1500), yaxt="n",
		cex.axis=2, xlab="Year", ylab="",
		cex.lab=2, font.lab=2)
	axis(2, at=pretty(c(0, 1500)), cex.axis=2, las=2)
	mtext("True Biomass", side=2, line=5, cex=2, font=2)

	if(plot_index==TRUE){
		par(new=TRUE)
		plot(OM$Index, pch=19, col="purple", xaxs="i", cex=2,
			yaxs="i", xlab="", ylab="", ylim=c(0,1500*q_true), yaxt="n", xaxt="n")
		axis(4, at=pretty(c(0, 1500*q_true)), cex.axis=2, las=2)
		mtext("Abundance Index", side=4, line=3, cex=2, font=2)
		legend(x=21, y=14.7, legend="True Biomass", lty=1, lwd=4, 
			col="forestgreen", bty="n", cex=2)
		legend(x=21, y=14.2, legend="Abundance Index", pch=19,
			col="purple", bty="n", cex=2)
	}
}