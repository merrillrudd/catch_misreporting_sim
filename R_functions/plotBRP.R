plotBRP <- function(results, process_err, bmsy_factor, 
	nproject, report=0.5, title=TRUE, bmodel, dirs){

par(mar=c(0,0,0,0), omi=c(1,1.5,1,0.5))
layout_mat <- matrix(c(0,rep(1,8), 0,rep(2,8),rep(3,8), rep(4,8), 
				0,rep(5,8),0,rep(6,8), rep(7,8), rep(8,8),
				0,rep(9,8),0,rep(10,8), rep(11,8), rep(12,8)),
			  nrow=3, ncol=34, byrow=TRUE)
layout(mat=layout_mat)
layout.show(layout(mat=layout_mat))

   bmodel_name <- ifelse(bmodel=="s", "Schaefer", "Pella-Tomlinson")

   plotdirs <- dirs[grepl(paste0("sigma", process_err), dirs) & grepl(bmodel_name, dirs)]
   index_plotdirs <- which(dirs %in% plotdirs)

   yrs <- 1:(35+nproject)
   datyrs <- 1:35
   simyrs <- 36:yrs[length(yrs)]
   full <- rep(1,length(yrs))
   ylim <- c(0,1.1)
   xlim <- c(yrs[1], max(yrs))

   repcols <- c("#D3191C", "#266DA7")

   plot(x=yrs, y=full, xlim=xlim, 
   	ylim=ylim, type="n", 
   	xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
   polygon(x=c(simyrs, rev(simyrs)), 
   	y=c(rep(0,length(simyrs)), rep(ylim[2],length(simyrs))),
   	col="#88888850", border=NA)
   lines(x=yrs, y=full, col=repcols[1], lty=2, lwd=3)
   axis(2, at=c(0.2, 0.5, 0.8, 1), labels=c("20%","50%","80%", "100%"), las=2, cex.axis=1.7)
   mtext("Reporting\nRate    ", side=2, font=2, line=5, cex=1.3, las=2)
   mtext("100% Reported", font=2, cex=1.3, side=3, line=0.25)   

   plot(x=yrs, y=full, xlim=xlim, 
   	ylim=ylim, type="n", 
   	xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
   polygon(x=c(simyrs, rev(simyrs)), 
   	y=c(rep(0,length(simyrs)), rep(ylim[2],length(simyrs))),
   	col="#88888850", border=NA)
   lines(x=yrs, y=full, col=repcols[1], lty=2, lwd=3)
   lines(x=yrs, y=full*report, lty=2, col=repcols[2], lwd=3)
   mtext("All Underreported", font=2, cex=1.3, side=3, line=0.25)   

   plot(x=yrs, y=full, xlim=xlim, 
   	ylim=ylim, type="n", 
   	xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
   polygon(x=c(simyrs, rev(simyrs)), 
   	y=c(rep(0,length(simyrs)), rep(ylim[2],length(simyrs))),
   	col="#88888850", border=NA)
   lines(x=yrs, y=full, col=repcols[1], lty=2, lwd=3)
   rep1 <- 0.2
   rep2 <- 0.8
   slope <- (rep2 - rep1)/(length(datyrs))
    y <- rep1+yrs*slope
   	y[which(y>=rep2)] <- rep2
    lines(x=yrs, y=y, col=repcols[2], lwd=3, lty=2)
   mtext("Reporting Increasing", font=2, cex=1.3, side=3, line=0.25)  

   plot(x=yrs, y=full, xlim=xlim, 
   	ylim=ylim, type="n", 
   	xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
   polygon(x=c(simyrs, rev(simyrs)), 
   	y=c(rep(0,length(simyrs)), rep(ylim[2],length(simyrs))),
   	col="#88888850", border=NA)
   lines(x=yrs, y=full, col=repcols[1], lty=2, lwd=3)
   rep1 <- 0.8
   rep2 <- 0.2
   slope <- (rep2 - rep1)/(length(datyrs))
   y <- rep1+yrs*slope
   y[which(y<=rep2)] <- rep2
     lines(x=yrs, y=y, col=repcols[2], lwd=3, lty=2)
   mtext("Reporting Decreasing", font=2, cex=1.3, side=3, line=0.25)   

   max <- min(max(results$Expected[,,index_plotdirs]/bmsy_true, 
   	results$True[,,index_plotdirs]/bmsy_true),4)
   shared_ylim <- c(0,max)
   for(mod in index_plotdirs){
      bbproj <- results$Expected[,,mod]/bmsy_true
      bbtrue <- results$True[,,mod]/bmsy_true
      bbproj_quants <- apply(bbproj, 1, function(x) quantile(probs=c(0.025, 0.5, 0.975), x))
      bbtrue_quants <- apply(bbtrue, 1, function(x) quantile(probs=c(0.025, 0.5, 0.975), x))
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=yrs, y=full, xlim=c(1,nrow(bbproj)), ylim=shared_ylim,
      	    type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(simyrs, rev(simyrs)), 
      	    y=c(rep(0,length(simyrs)), rep(shared_ylim[2],length(simyrs))),
      	    col="#88888850", border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(bbproj_quants[1,], rev(bbproj_quants[3,])),
      	col=paste0(color, "70"), border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(bbtrue_quants[1,], rev(bbtrue_quants[3,])),
      	col="#55555570", border=NA)
      	lines(bbproj_quants[2,], col=color, lwd=4)
      	lines(bbtrue_quants[2,], col="black", lwd=2)
      	abline(h=1, col="goldenrod", lwd=2)
      if(mod==index_plotdirs[1]){
      	axis(2, at=pretty(shared_ylim), cex.axis=1.7, las=1)
    	mtext("B/Bmsy", side=2, font=2, line=4.5, cex=1.3, las=2)
      }
   }

   max <- max((results$PredCatch[,,index_plotdirs]/results$Expected[,,index_plotdirs])/emsy_true,
   	(results$TrueCatch[,,index_plotdirs]/results$True[,,index_plotdirs])/emsy_true)
   shared_ylim <- c(0,7)
   for(mod in index_plotdirs){
   	eproj <- (results$PredCatch[,,mod]/results$Expected[,,mod])
   	etrue <- (results$TrueCatch[,,mod]/results$True[,,mod])
      eeproj <- (results$PredCatch[,,mod]/results$Expected[,,mod])/emsy_true
      eetrue <- (results$TrueCatch[,,mod]/results$True[,,mod])/emsy_true
      eeproj_quants <- apply(eeproj, 1, function(x) quantile(probs=c(0.025, 0.5, 0.975), x))
      eetrue_quants <- apply(eetrue, 1, function(x) quantile(probs=c(0.025, 0.5, 0.975), x))
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=yrs, y=full, xlim=c(1,nrow(bbproj)), ylim=shared_ylim,
      	    type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(simyrs, rev(simyrs)), 
      	    y=c(rep(0,length(simyrs)), rep(shared_ylim[2],length(simyrs))),
      	    col="#88888850", border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(eeproj_quants[1,], rev(eeproj_quants[3,])),
      	col=paste0(color, "70"), border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(eetrue_quants[1,], rev(eetrue_quants[3,])),
      	col="#55555570", border=NA)
      	lines(eeproj_quants[2,], col=color, lwd=4)
      	lines(eetrue_quants[2,], col="black", lwd=2)
      	abline(h=1, col="goldenrod", lwd=2)
      if(mod==index_plotdirs[1]){
      	axis(2, at=pretty(shared_ylim), cex.axis=1.7, las=1)
    	mtext("HR/HRmsy", side=2, font=2, line=4.5, cex=1.3, las=2)
      }
        axis(1, at=pretty(yrs), cex.axis=1.7)
   }

mtext("Year", font=2, outer=TRUE, side=1, cex=1.3, line=4)

if(title==TRUE){
	mtext(paste("Process & Observation Error =", process_err), font=2, cex=1.5, outer=TRUE, side=3, line=3)
   if(bmsy_factor > 1) mtext(paste0("Reporting Rate Patterns and Expected vs. True Population Trajectories Managed Aiming for ", bmsy_factor, "*Bmsy"), font=2, cex=1.5, outer=TRUE, side=3, line=5)
   if(bmsy_factor==1) mtext(paste0("Reporting Rate Patterns and Expected vs. True Population Trajectories Managed Aiming for Bmsy"), font=2, cex=1.5, outer=TRUE, side=3, line=5)


}

}