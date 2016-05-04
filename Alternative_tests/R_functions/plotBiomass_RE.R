plotBiomass_RE <- function(dirs, results, process_err, 
   bmsy_factor, nproject, report=0.5,
   title=TRUE, bmodel,cmodel){

par(mar=c(0,0,0,0), omi=c(1,1.5,1,0.5))
layout_mat <- matrix(c(0,rep(1,8), 0,rep(2,8),rep(3,8), rep(4,8), 
				0,rep(5,8),0,rep(6,8), rep(7,8), rep(8,8)),
			  nrow=2, ncol=34, byrow=TRUE)
layout(mat=layout_mat)
layout.show(layout(mat=layout_mat))


   bmodel_name <- ifelse(bmodel=="s", "Schaefer", "Pella-Tomlinson")

   plotdirs <- dirs[grepl(paste0("sigma", process_err), dirs) & grepl(bmodel_name, dirs)]
   index_plotdirs <- which(dirs %in% plotdirs)
   
   yrs <- 1:(35+nproject)
   datyrs <- 1:35
   if(length(yrs) > 35){ simyrs <- 36:yrs[length(yrs)]}
   if(length(yrs) <= 35){ simyrs <- NULL}
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
  
   bproj_all <- results$Expected[,,index_plotdirs]
   btrue_all <- results$True[,,index_plotdirs]
    shared_ylim <- c(-1,1)
   for(mod in index_plotdirs){
      bproj <- results$Expected[,,mod]
      btrue <- results$True[,,mod]
      re <- (btrue - bproj)/btrue
      bre_quants <- apply(re, 1, function(x) quantile(probs=c(0.025, 0.45, 0.5, 0.55, 0.975), x))
      plot(x=yrs, y=full, xlim=c(1,nrow(bproj)), ylim=shared_ylim,
      	    type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(simyrs, rev(simyrs)), 
      	    y=c(rep(0,length(simyrs)), rep(shared_ylim[2],length(simyrs))),
      	    col="#88888850", border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(bre_quants[1,], rev(bre_quants[5,])),
      	col=paste0("#005500", "70"), border=NA)
      lines(bre_quants[3,], col="#005500", lwd=4)
      abline(h=0, col="goldenrod", lwd=2)
      if(mod==index_plotdirs[1]){
      	axis(2, at=pretty(shared_ylim), cex.axis=1.7, las=1)
      	mtext("Biomass", side=2, font=2, line=5, cex=1.3, las=2)
      }
      axis(1, at=pretty(yrs), cex.axis=1.7)
      axis(1, at=pretty(yrs), cex.axis=1.7)
   }
   mtext("Year", font=2, outer=TRUE, side=1, cex=1.3, line=4)
   if(title==TRUE){
      mtext(paste("Process & Observation Error =", process_err), font=2, cex=1.5, outer=TRUE, side=3, line=3)
      if(bmsy_factor > 1) mtext(paste0("Reporting Rate Patterns and Expected vs. True Population Trajectories Managed Aiming for ", bmsy_factor, "*Bmsy"), font=2, cex=1.5, outer=TRUE, side=3, line=5)
      if(bmsy_factor==1) mtext(paste0("Reporting Rate Patterns and Expected vs. True Population Trajectories Managed Aiming for Bmsy"), font=2, cex=1.5, outer=TRUE, side=3, line=5)

   }

}