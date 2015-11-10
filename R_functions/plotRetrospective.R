plotRetrospective <- function(dirs, results, to.plot, scenario, nres=1){


    if(nres==1){
	    msy_est <- results$msy_est
    	K_est <- results$K_est
    	r_deriv <- (msy_est*((z_true+1)^(1/z_true + 1)))/(K_est*z_true)  

        b_est <- results$b_est ## iter by dir by year
        b_true <- results$b_true ## iter by dir by year
        c_rep <- results$c_rep ## iter by dir by year
        c_true <- results$c_true ## iter by dir by year
        bmsy_est <- results$bmsy_est ## iter by dir
        e_est <- results$c_rep/results$b_est
        e_true <- results$c_true/results$b_true    

        msy_re <- (msy_est - MSY_true)/MSY_true
        K_re <- (K_est - K_true)/K_true
        r_re <- (r_deriv - r_true)/r_true
        bbmsy_re <- ((b_est/bmsy_est) - (b_true/bmsy_true))/(b_true/bmsy_true)
        eemsy_re <- (((c_rep/b_est)/(msy_est/bmsy_est)) - ((c_true/b_true)/(MSY_true/bmsy_true)))/((c_true/b_true)/(MSY_true/bmsy_true))
        b_re <- (b_est - b_true)/b_true
        e_re <- (e_est - e_true)/e_true
    }

    if(nres==2){
        b_est <- list(results[[1]]$b_est, results[[2]]$b_est)
        b_true <- list(results[[1]]$b_true, results[[2]]$b_true)
        bmsy_est <- list(results[[1]]$bmsy_est, results[[2]]$bmsy_est)
        c_rep <- list(results[[1]]$c_rep, results[[2]]$c_rep)
        c_true <- list(results[[1]]$c_true, results[[2]]$c_true)
        msy_est <- list(results[[1]]$msy_est, results[[2]]$msy_est)
        bmsy_est <- list(results[[1]]$bmsy_est, results[[2]]$bmsy_est)
        K_est <- list(results[[1]]$K_est, results[[2]]$K_est)
        e_est <- e_true <- NULL
        for(i in 1:length(c_rep)){
            e_est[[i]] <- c_rep[[i]]/b_est[[i]]
            e_true[[i]] <- c_true[[i]]/b_true[[i]]
        }
        
        # if(all(grepl("sigma0.001", dirs))==TRUE) index <- 1:5
        # if(all(grepl("sigma0.1", dirs))==TRUE) index <- 6:10

        msy_re <- K_re <- r_re <- bbmsy_re <- eemsy_re <- b_re <- e_re <- NULL
        for(i in 1:length(msy_est)){
            msy_re[[i]] <- (msy_est[[i]] - MSY_true)/MSY_true
            K_re[[i]] <- (K_est[[i]] - K_true)/K_true
            r_deriv <- (msy_est[[i]]*((z_true+1)^(1/z_true + 1)))/(K_est[[i]]*z_true)
            r_re[[i]] <- (r_deriv - r_true)/r_true
            bbmsy_re[[i]] <- ((b_est[[i]]/bmsy_est[[i]]) - (b_true[[i]]/bmsy_true))/(b_true[[i]]/bmsy_true)
            eemsy_re[[i]] <- (((c_rep[[i]]/b_est[[i]])/(msy_est[[i]]/bmsy_est[[i]])) - ((c_true[[i]]/b_true[[i]])/(MSY_true/bmsy_true)))/((c_true[[i]]/b_true[[i]])/(MSY_true/bmsy_true))
            b_re[[i]] <- (b_est[[i]] - b_true[[i]])/b_true[[i]]
            e_re[[i]] <- (e_est[[i]] - e_true[[i]])/e_true[[i]]
        }
    }
  
	nrow <- 2
	if(length(to.plot) > 1) ncol <- length(to.plot)
 	if(length(to.plot)==1 & scenario=="all") ncol <- length(rmodel_vec[-c(grep("changeq", rmodel_vec))])
 	if(length(to.plot) > 1 & scenario=="all") stop("Choose 1 parameter or scenario to plot")

 	if(scenario != "all"){
        choose_scen <- grep(scenario, dirs)
        choose_det <- grep("sigma0.001", dirs)
        choose_stoch <- grep("sigma0.1", dirs)
        det_index <- choose_det[which(choose_det %in% choose_scen)]
        stoch_index <- choose_stoch[which(choose_stoch %in% choose_scen)]
        if(nres==1){
            dirs_row1 <- dirs[det_index]
            dirs_row2 <- dirs[stoch_index]
        }
        if(nres==2){
            dirs_row1 <- dirs[which(grep("2way", dirs)%in% choose_scen)] 
            dirs_row2 <- dirs[which(grep("1way", dirs)%in% choose_scen)] 
        }
 	}
 	if(scenario=="all"){
        if(nres==1){
            dirs_row1 <- dirs[grep("sigma0.001", dirs)]
            dirs_row2 <- dirs[grep("sigma0.1", dirs)]
        }
 		if(nres==2){
            dirs_row2 <- dirs[grep("1way", dirs)]
            dirs_row1 <- dirs[grep("2way", dirs)]
        }
 	}

 	if(scenario!="all") par(mfcol=c(nrow,ncol), mar=c(0,0,0,0), omi=c(1,1,1,1))
    if(scenario=="all") par(mfrow=c(nrow,ncol), mar=c(0,0,0,0), omi=c(1,1,1,1))

	if("MSY" %in% to.plot){
	    ylim <- c(-1,2)
    	for(i in 1:length(dirs_row1)){
    		if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
    		plot(x=1, y=1, ylim=ylim, type="n", 
    		   xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("MSY"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
    		abline(h=0, col="red", lwd=2)
    		par(new=TRUE)
            if(nres==1) plot_re <- msy_re[,index,]
            if(nres==2) plot_re <- msy_re[[1]][,index,]
    		boxplot(plot_re, ylim=ylim, col="forestgreen",
    			xaxs="i", yaxs="i", xaxt="n", yaxt="n")
    		if("MSY"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("MSY"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("MSY"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("MSY"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("MSY"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
    		if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("MSY", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
    	}
    	for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }           
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
    		plot(x=1, y=1, ylim=ylim, type="n",
    		   xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("MSY"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
    		abline(h=0, col="red", lwd=2)
    		par(new=TRUE)
            if(nres==1) plot_re <- msy_re[,index,]
            if(nres==2) plot_re <- msy_re[[2]][,index,]
    		boxplot(plot_re, ylim=ylim, col="forestgreen",
    			xaxs="i", yaxs="i", xaxt="n", yaxt="n")
    		axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
    	}
	}

	if("K" %in% to.plot){
	    ylim <- c(-1,2)
        for(i in 1:length(dirs_row1)){
            if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("K"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- K_re[,index,]
            if(nres==2) plot_re <- K_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col="goldenrod",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("K"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("K"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("K"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("K"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("K"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("K", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("K"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- K_re[,index,]
            if(nres==2) plot_re <- K_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col="goldenrod",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
	}

	if("r" %in% to.plot){
	    ylim <- c(-1,2)
        for(i in 1:length(dirs_row1)){
            if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("r"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- r_re[,index,]
            if(nres==2) plot_re <- r_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col="steelblue",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("r"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("r"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("r"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("r"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("r"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("r", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("r"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- r_re[,index,]
            if(nres==2) plot_re <- r_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col="steelblue",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
	}

    if("BBmsy" %in% to.plot){
        ylim <- c(-1,2)
        for(i in 1:length(dirs_row1)){
            if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("BBmsy"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- bbmsy_re[,index,]
            if(nres==2) plot_re <- bbmsy_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col="plum",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("BBmsy"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("BBmsy"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("BBmsy"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("BBmsy"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("BBmsy"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("BBmsy", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("BBmsy"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- bbmsy_re[,index,]
            if(nres==2) plot_re <- bbmsy_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col="plum",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
    }

    if("EEmsy" %in% to.plot){
        ylim <- c(-2,10)
        for(i in 1:length(dirs_row1)){
            if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("EEmsy"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- eemsy_re[,index,]
            if(nres==2) plot_re <- eemsy_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col="orange",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("EEmsy"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("EEmsy"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("EEmsy"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("EEmsy"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("EEmsy"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("EEmsy", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("EEmsy"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- eemsy_re[,index,]
            if(nres==2) plot_re <- eemsy_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col="orange",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
    }

    if("biomass" %in% to.plot){
        ylim <- c(-1,2)
        for(i in 1:length(dirs_row1)){
            if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("biomass"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- b_re[,index,]
            if(nres==2) plot_re <- b_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col=gray(0.5),
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("biomass"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("biomass"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("biomass"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("biomass"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("biomass"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("biomass", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
           if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("biomass"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- b_re[,index,]
            if(nres==2) plot_re <- b_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col=gray(0.5),
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
    }

    if("exploit" %in% to.plot){
        ylim <- c(-1,2)
        for(i in 1:length(dirs_row1)){
           if(nres==1){
                index <- which(dirs %in% dirs_row1[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n", 
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("exploit"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- e_re[,index,]
            if(nres==2) plot_re <- e_re[[1]][,index,]
            boxplot(plot_re, ylim=ylim, col="salmon",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            if("exploit"==to.plot[1]) if(grepl("allrep", dirs_row1[i])) label <- "100% Reporting"
            if("exploit"==to.plot[1]) if(grepl("allunder", dirs_row1[i])) label <- "Constant Underreporting"
            if("exploit"==to.plot[1]) if(grepl("repinc", dirs_row1[i])) label <- "Increasing Reporting"
            if("exploit"==to.plot[1]) if(grepl("repdec", dirs_row1[i])) label <- "Decreasing Reporting"
            if("exploit"==to.plot[1]) if(grepl("allover", dirs_row1[i])) label <- "Constant Overreporting"
            if(scenario!="all") mtext(label, font=2, line=1, cex=2, outer=TRUE)
            if(scenario!="all") mtext("exploit", font=2, line=-3, cex=2)
            if(scenario=="all") mtext(label, font=2, line=1, cex=1.2)
        }
        for(i in 1:length(dirs_row2)){
            if(nres==1){
                index <- which(dirs %in% dirs_row2[i])
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
            }   
            if(nres==2){
                if(scenario=="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- i
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- i+5

                }
                if(scenario!="all"){
                    if(all(grepl("sigma0.001", dirs))==TRUE) index <- which(rmodel_vec==scenario)
                    if(all(grepl("sigma0.1", dirs))==TRUE) index <- which(rmodel_vec==scenario)+5
                }
            }
            plot(x=1, y=1, ylim=ylim, type="n",
               xaxt="n", yaxt="n", xaxs="i", yaxs="i")
            if("exploit"==to.plot[1] & scenario != "all") axis(2, at=pretty(ylim), las=2, cex.axis=2)
            if(scenario=="all" & i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
            abline(h=0, col="red", lwd=2)
            par(new=TRUE)
            if(nres==1) plot_re <- e_re[,index,]
            if(nres==2) plot_re <- e_re[[2]][,index,]
            boxplot(plot_re, ylim=ylim, col="salmon",
                xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            axis(1, at=1:7, labels=seq(5,datyrs,by=5), cex.axis=1.5)
        }
    }
    
	
	
	mtext("Relative Error", font=2, outer=TRUE, cex=1.2, line=4.5, side=2)
	mtext("Years of Data", font=2, outer=TRUE, cex=1.2, line=3.5, side=1)

}

