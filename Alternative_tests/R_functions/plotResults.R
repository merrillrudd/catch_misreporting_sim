plotResults <- function(to.plot, dirs, process_err, results){

  if(process_err == 0.001) plotdirs <- dirs[grep(as.character(0.001), dirs)]
  if(process_err != 0.001) plotdirs <- dirs[-c(grep(as.character(0.001), dirs))]
	nvals <- ifelse(process_err=="compare", length(to.plot)*length(process_vec), length(to.plot))
	par(mar=c(0,0,0,0), omi=c(1,2.2,1,0.5))
  nscen <- 5
	pcol <- 8*nscen
	plotnum <- 1:(nscen*nvals)
	matnum <- unlist(lapply(plotnum, function(x) rep(x, 8)))
	layout_mat <- matrix(matnum, nrow=nvals, ncol=pcol, byrow=TRUE)
	layout(mat=layout_mat)
	layout.show(layout(mat=layout_mat))

   index_plotdirs <- which(dirs %in% plotdirs)
   yrs <- 1:datyrs
   ylim <- c(0,1.6)
   xlim <- c(yrs[1], max(yrs))

   repcols <- c("#D3191C", "#266DA7")
  
  if("biomass" %in% to.plot){
    bproj_all <- results$b_est[,index_plotdirs,]
    btrue_all <- results$b_true[,index_plotdirs,]
    shared_ylim <- c(0, min(max(bproj_all), 3000))
   for(mod in index_plotdirs){
      bproj <- results$b_est[,mod,]
      btrue <- results$b_true[,mod,]
      bproj_quants <- apply(bproj, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      btrue_quants <- apply(btrue, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=1, y=1, xlim=c(1,ncol(bproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(bproj_quants[1,], rev(bproj_quants[3,])),
        col=paste0(color, "70"), border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(btrue_quants[1,], rev(btrue_quants[3,])),
        col="#55555570", border=NA)
        lines(bproj_quants[2,], col=color, lwd=4)
        lines(btrue_quants[2,], col="black", lwd=2)
      if(mod < 6 & "biomass"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "biomass"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)
      if(mod==index_plotdirs[1]){
       axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
      mtext("Biomass", side=2, font=2, line=5, cex=1.7, las=2)
      }
      if("biomass"==to.plot[length(to.plot)]){
        axis(1, at=seq(10,35, by=10), cex.axis=2)
      }
   }
  }

  if("biomasslines" %in% to.plot){
    bproj_all <- results$b_est[,index_plotdirs,]
    btrue_all <- results$b_true[,index_plotdirs,]
    shared_ylim <- c(0, min(max(bproj_all), 3000))
   for(mod in index_plotdirs){
      bproj <- results$b_est[,mod,]
      btrue <- results$b_true[,mod,]
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=1, y=1, xlim=c(1,ncol(bproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      for(iter in 1:nrow(bproj)){
        lines(btrue[iter,])
        lines(bproj[iter,], col=color)
      }
      if(mod < 6 & "biomasslines"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "biomasslines"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)
      if(mod==index_plotdirs[1]){
       axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
       mtext("Biomass\nIterations", side=2, font=2, line=5, cex=1.7, las=2)
      }
      if("biomasslines"==to.plot[length(to.plot)]){
        axis(1, at=pretty(yrs), cex.axis=1.7)
      }
   }
  }

  if("biomassRE" %in% to.plot){
    shared_ylim <- c(-1,1)
   for(mod in index_plotdirs){
      bproj <- results$b_est[,mod,]
      btrue <- results$b_true[,mod,]
      re <- (bproj-btrue)/btrue
      bre_quants <- apply(re, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      plot(x=1, y=1, xlim=c(1,ncol(bproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(bre_quants[1,], rev(bre_quants[3,])),
        col=paste0("#005500", "70"), border=NA)
      lines(bre_quants[2,], col="#005500", lwd=4)
      abline(h=0, col="black", lwd=3,lty=2)
      if(mod < 6 & "biomassRE"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "biomassRE"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)
      if(process_err != "compare"){
         if(mod==index_plotdirs[1]){
           axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
           mtext("Relative\nError", side=2, font=2, line=5, cex=1.7, las=2)
         }
         if("biomassRE"==to.plot[length(to.plot)]){
           axis(1, at=seq(10,35,by=10), cex.axis=1.7)
         }
      }

      if(process_err=="compare"){
        if(grepl("allrep", dirs[mod])){
          axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
          lab <- process_vec[which(sapply(process_vec, function(x) grepl(x, dirs[mod]))==TRUE)[1]]
          mtext(ifelse(lab=="0.001", "Deter.", paste0("Sigma\n", lab)), side=2, font=2, line=5, cex=1.7, las=2)
      }

        if(lab==process_vec[length(process_vec)]){
          axis(1, at=pretty(yrs), cex.axis=2)
        }
      }
   }
  }

   
  if("BBmsy" %in% to.plot){
    max <- min(max(results$b_est[,index_plotdirs,]/bmsy_true, 
    results$b_true[,index_plotdirs,]/bmsy_true),4)
   shared_ylim <- c(0,max)
   for(mod in index_plotdirs){
      bbproj <- results$b_est[,mod,]/results$bmsy_est[,mod]
      bbtrue <- results$b_true[,mod,]/bmsy_true
      bbproj_quants <- apply(bbproj, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      bbtrue_quants <- apply(bbtrue, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=1, y=1, xlim=c(1,ncol(bbproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(bbproj_quants[1,], rev(bbproj_quants[3,])),
        col=paste0(color, "70"), border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(bbtrue_quants[1,], rev(bbtrue_quants[3,])),
        col="#55555570", border=NA)
        lines(bbproj_quants[2,], col=color, lwd=4)
        lines(bbtrue_quants[2,], col="black", lwd=2)
        abline(h=1, col="black", lwd=3, lty=2)
      if(mod < 6 & "BBmsy"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "BBmsy"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)

      if(mod==index_plotdirs[1]){
        axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
        mtext("B/Bmsy", side=2, font=2, line=4.5, cex=1.7, las=2)
      }
      if("BBmsy"==to.plot[length(to.plot)]){
        axis(1, at=pretty(yrs), cex.axis=2)
      }
   }
  }

  if("EEmsy" %in% to.plot){
    max <- max((results$c_rep[,index_plotdirs,]/results$b_est[,index_plotdirs,])/emsy_true,
    (results$c_true[,index_plotdirs,]/results$b_est[,index_plotdirs,])/emsy_true)
   shared_ylim <- c(0,min(8,max))
   for(mod in index_plotdirs){
    eproj <- (results$c_rep[,mod,]/results$b_est[,mod,])
    etrue <- (results$c_true[,mod,]/results$b_true[,mod,])
      eeproj <- (results$c_rep[,mod,]/results$b_est[,mod,])/(results$msy_est[,mod]/results$bmsy_est[,mod])
      eetrue <- (results$c_true[,mod,]/results$b_true[,mod,])/emsy_true
      eeproj_quants <- apply(eeproj, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      eetrue_quants <- apply(eetrue, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      color <- ifelse(grepl("allrep", dirs[mod]), repcols[1], repcols[2])
      plot(x=1, y=1, xlim=c(1,ncol(eproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(eeproj_quants[1,], rev(eeproj_quants[3,])),
        col=paste0(color, "70"), border=NA)
      polygon(x=c(yrs, rev(yrs)), y=c(eetrue_quants[1,], rev(eetrue_quants[3,])),
        col="#55555570", border=NA)
        lines(eeproj_quants[2,], col=color, lwd=4)
        lines(eetrue_quants[2,], col="black", lwd=2)
        abline(h=1, col="black", lwd=3, lty=2)
      if(mod < 6 & "EEmsy"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "EEmsy"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)

      if(mod==index_plotdirs[1]){
        axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
        mtext("E/Emsy", side=2, font=2, line=4.5, cex=1.7, las=2)
      }
       if("EEmsy"==to.plot[length(to.plot)]){
        axis(1, at=seq(10, 35, by=10), cex.axis=2)
      }
   }
  }

  if("BBmsyRE" %in% to.plot){
    shared_ylim <- c(-1,1)
   for(mod in index_plotdirs){
      bbproj <- results$b_est[,mod,]/results$bmsy_est[,mod]
      bbtrue <- results$b_true[,mod,]/bmsy_true
      re <- (bbproj-bbtrue)/bbtrue
      bre_quants <- apply(re, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      plot(x=1, y=1, xlim=c(1,ncol(bbproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(bre_quants[1,], rev(bre_quants[3,])),
        col=paste0("#005500", "70"), border=NA)
      lines(bre_quants[2,], col="#005500", lwd=4)
      abline(h=0, col="black", lwd=3,lty=2)
      if(mod < 6 & "BBmsyRE"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "BBmsyRE"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)

      if(process_err != "compare"){
         if(mod==index_plotdirs[1]){
           axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
           mtext("Relative\nError", side=2, font=2, line=5, cex=1.7, las=2)
         }
         if("BBmsyRE"==to.plot[length(to.plot)]){
           axis(1, at=seq(10, 35, by=10), cex.axis=2)
         }
      }

      if(process_err=="compare"){
        if(grepl("allrep", dirs[mod])){
          axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
          lab <- process_vec[which(sapply(process_vec, function(x) grepl(x, dirs[mod]))==TRUE)[1]]
          mtext(ifelse(lab=="0.001", "Deter.", paste0("Sigma\n", lab)), side=2, font=2, line=5, cex=1.7, las=2)
      }

        if(lab==process_vec[length(process_vec)]){
          axis(1, at=seq(10,30, by=10), cex.axis=2)
        }
      }
   }
  }

  if("EEmsyRE" %in% to.plot){
   shared_ylim <- c(-1,2)
   for(mod in index_plotdirs){
    eproj <- (results$c_rep[,mod,]/results$b_est[,mod,])
    etrue <- (results$c_true[,mod,]/results$b_true[,mod,])
      eeproj <- (results$c_rep[,mod,]/results$b_est[,mod,])/(results$msy_est[,mod]/results$bmsy_est[,mod])
      eetrue <- (results$c_true[,mod,]/results$b_true[,mod,])/emsy_true
      re <- (eeproj - eetrue)/eetrue
      ere_quants <- apply(re, 2, function(x) quantile(probs=c(0.05, 0.5, 0.95), x))
      plot(x=1, y=1, xlim=c(1,ncol(eeproj)), ylim=shared_ylim,
            type="n", xaxs="i", yaxs="i",xaxt="n",yaxt="n", ann=F)
      polygon(x=c(yrs, rev(yrs)), y=c(ere_quants[1,], rev(ere_quants[3,])),
        col=paste0("#005500", "70"), border=NA)
      lines(ere_quants[2,], col="#005500", lwd=4)
      abline(h=0, col="black", lwd=3,lty=2)
      if(mod < 6 & "EEmsyRE"==to.plot[1]) mtext(rmodel_vec_names[mod], side=3, font=2, line=1.5, cex=1.3)
      if(mod >= 6 & "EEmsyRE"==to.plot[1]) mtext(rmodel_vec_names[mod-5], side=3, font=2, line=1.5, cex=1.3)

      if(process_err != "compare"){
         if(mod==index_plotdirs[1]){
           axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
           mtext("Relative\nError", side=2, font=2, line=5, cex=1.7, las=2)
         }
         if("EEmsyRE"==to.plot[length(to.plot)]){
           axis(1, at=seq(10,35,by=10), cex.axis=2)
         }
      }

      if(process_err=="compare"){
        if(grepl("allrep", dirs[mod])){
          axis(2, at=pretty(shared_ylim), cex.axis=2, las=1)
          lab <- process_vec[which(sapply(process_vec, function(x) grepl(x, dirs[mod]))==TRUE)[1]]
          mtext(ifelse(lab=="0.001", "Deter.", paste0("Sigma\n", lab)), side=2, font=2, line=5, cex=1.7, las=2)
      }

        if(lab==process_vec[length(process_vec)]){
          axis(1, at=seq(10,35,by=10), cex.axis=2)
        }
   }
  }
}

  mtext("Year", font=2, outer=TRUE, side=1, cex=2, line=4)

on.exit(setwd(init_dir))
}