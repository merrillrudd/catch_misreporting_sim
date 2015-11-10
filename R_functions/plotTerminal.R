plotTerminal <- function(dirs, results, nres=1){

	if(nres==1){
		b_est <- results$b_est ## iter by dir by year
	    b_true <- results$b_true ## iter by dir by year
    	bmsy_est <- results$bmsy_est ## iter by dir
    	c_rep <- results$c_rep ## iter by dir by year
    	c_true <- results$c_true ## iter by dir by year
    	msy_est <- results$msy_est ## iter by dir
    	bmsy_est <- results$bmsy_est ## iter by dir
    	K_est <- results$K_est ## iter by dir    

    	dirs_row1 <- dirs[grep("sigma0.001", dirs)]
    	dirs_row2 <- dirs[grep("sigma0.1", dirs)]

    	term_b_est <- b_est[,,datyrs]
	    term_b_true <- b_true[,,datyrs]
    	
    	term_c_rep <- c_rep[,,datyrs]
    	term_c_true <- c_true[,,datyrs]
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

		dirs_row2 <- dirs[grep("1way", dirs)]
		dirs_row1 <- dirs[grep("2way", dirs)]

		if(all(grepl("sigma0.001", dirs))==TRUE) index <- 1:5
		if(all(grepl("sigma0.1", dirs))==TRUE) index <- 6:10

		if(all(grepl("changeq",dirs))){
			dirs_row1 <- dirs[grep("sigma0.001", dirs)]
			dirs_row2 <- dirs[grep("sigma0.1", dirs)]
			index <- 1:2
		}
		if(all(grepl("allunder",dirs))){
			dirs_row1 <- dirs[grep("sigma0.001", dirs)]
			dirs_row2 <- dirs[grep("sigma0.1", dirs)]
			index <- grep("allunder", dirs2)

			term_b_est <- cbind(b_est[[1]][,index,datyrs], b_est[[2]][,,datyrs])
		    term_b_true <- cbind(b_true[[1]][,index,datyrs], b_true[[2]][,,datyrs])
        	
    		term_c_rep <- cbind(c_rep[[1]][,index,datyrs], c_rep[[2]][,,datyrs])
        	term_c_true <- cbind(c_true[[1]][,index,datyrs], c_true[[2]][,,datyrs])    

        	msy_est <- cbind(msy_est[[1]][,index], msy_est[[2]])
        	bmsy_est <- cbind(bmsy_est[[1]][,index], bmsy_est[[2]])
        	K_est <- cbind(K_est[[1]][,index], K_est[[2]])
		}
		if(all(grepl("allunder", dirs))==FALSE){
			term_b_est <- cbind(b_est[[1]][,index,datyrs], b_est[[2]][,index,datyrs])
		    term_b_true <- cbind(b_true[[1]][,index,datyrs], b_true[[2]][,index,datyrs])
        	
    		term_c_rep <- cbind(c_rep[[1]][,index,datyrs], c_rep[[2]][,index,datyrs])
        	term_c_true <- cbind(c_true[[1]][,index,datyrs], c_true[[2]][,index,datyrs])    

        	msy_est <- cbind(msy_est[[1]][,index], msy_est[[2]][,index])
        	bmsy_est <- cbind(bmsy_est[[1]][,index], bmsy_est[[2]][,index])
        	K_est <- cbind(K_est[[1]][,index], K_est[[2]][,index])
		}

	}

	term_bbmsy_est <- term_b_est/bmsy_est
	term_bbmsy_true <- term_b_true/bmsy_true
	term_eemsy_est <- (term_c_rep/term_b_est)/(msy_est/bmsy_est)
    term_eemsy_true <- (term_c_true/term_b_true)/(MSY_true/bmsy_true)

	b_re <- (term_b_est - term_b_true)/term_b_true
	bbmsy_re <- (term_bbmsy_est - term_bbmsy_true)/term_bbmsy_true
	eemsy_re <- (term_eemsy_est - term_eemsy_true)/term_eemsy_true   
	msy_re <- (msy_est - MSY_true)/MSY_true
    K_re <- (K_est - K_true)/K_true
    	
    r_est <- (msy_est*((z_true+1)^(1/z_true + 1)))/(K_est*z_true)
    r_re <- (r_est - r_true)/r_true

    if(any(grepl("sigma0.001", dirs))) ylim <- c(-1,3)
    if(any(grepl("sigma0.1", dirs))) ylim <- c(quantile(c(b_re, bbmsy_re, eemsy_re, msy_re, K_re, r_re), probs=c(0.025, 0.975)))
	ncol <- length(dirs_row1)

	par(mfrow=c(2,ncol), mar=c(0,0,0,0), omi=c(1.5,1,0.5,0.1))
	if(all(grepl("allunder", dirs))==FALSE){
		loop <- 1:length(dirs_row1)
	}	
	if(all(grepl("allunder", dirs))){
		loop <- c(1,3)
	}
	for(i in loop){
		if(all(grepl("allunder", dirs))==FALSE) index <- which(dirs %in% dirs_row1[i])
		if(all(grepl("allunder", dirs))) index <- i
		plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,7),
		   xaxt="n", yaxt="n", xaxs="i", yaxs="i")
		if(i==1) axis(2, at=pretty(ylim), las=2, cex.axis=2)
		abline(h=0, col="red", lwd=2)
		# polygon(x=c(-1,6,6,-1), y=c(rep(0,2), rep(ylim[2],2)),
		# 	col="#00008880", border="gray")
		# polygon(x=c(-1,6,6,-1), y=c(rep(0,2), rep(ylim[1],2)),
		# 	col="#88000080", border="gray")
		# par(new=TRUE)
		vioplot(b_re[,index], ylim=ylim, add=TRUE, at=1, lwd=3, col="tomato", colMed="black")
		vioplot(bbmsy_re[,index], ylim=ylim, add=TRUE, at=2, lwd=3, col="tomato", colMed="black")
		vioplot(eemsy_re[,index], ylim=ylim, add=TRUE, at=3, lwd=3, col="tomato", colMed="black")
		if(all(round(msy_re[,index],5)==round(msy_re[,index][1],5))){ 
			points(x=4, y=msy_re[,index][1], pch=19, cex=2)
		} else vioplot(msy_re[,index], ylim=ylim, add=TRUE, at=4, lwd=3, col="tomato", colMed="black") 
		if(all(round(K_re[,index],5)==round(K_re[,index][1],5))){
			points(x=5, y=K_re[,index][1], pch=19, cex=2)
		}	else vioplot(K_re[,index], ylim=ylim, add=TRUE, at=5, lwd=3, col="tomato", colMed="black") 		
		if(all(round(r_re[,index],5)==round(r_re[,index][1],5))){
			points(x=6, y=r_re[,index][1], pch=19, cex=2)
		}	else vioplot(r_re[,index], ylim=ylim, add=TRUE, at=6, lwd=3, col="tomato", colMed="black") 		

		  #xaxt="n", yaxt="n", xaxs="i", yaxs="i")
		if(grepl("allrep", dirs[index])) label <- "100% Reporting"
        if(grepl("allunder", dirs[index])) label <- "Constant Underreporting"
        if(grepl("repinc", dirs[index])) label <- "Reporting Rate Increasing"
        if(grepl("repdec", dirs[index])) label <- "Reporting Rate Decreasing"
        if(grepl("allover", dirs[index])) label <- "Constant Overreporting"
		mtext(label, font=2, line=1, cex=1.2)
	}
	if(all(grepl("allunder", dirs))==FALSE){
		loop <- 1:length(dirs_row2)
	}	
	if(all(grepl("allunder", dirs))){
		loop <- c(2,4)
	}
	for(i in loop){
		if(all(grepl("allunder", dirs))==FALSE) index <- which(dirs %in% dirs_row1[i])
		if(all(grepl("allunder", dirs))) index <- i
		plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,7),
			xaxt="n", yaxt="n", xaxs="i", yaxs="i")
		if(i==loop[1]) axis(2, at=pretty(ylim)[-length(pretty(ylim))], las=2, cex.axis=2)
		abline(h=0, col="red", lwd=2)
		# polygon(x=c(-1,6,6,-1), y=c(rep(0,2), rep(ylim[2],2)),
		# 	col="#00008880", border="gray")
		# polygon(x=c(-1,6,6,-1), y=c(rep(0,2), rep(ylim[1],2)),
		# 	col="#88000080", border="gray")
		# par(new=TRUE)
		vioplot(b_re[,index], ylim=ylim, add=TRUE, at=1, lwd=3, col="steelblue")
		vioplot(bbmsy_re[,index], ylim=ylim, add=TRUE, at=2, lwd=3, col="steelblue")
		vioplot(eemsy_re[,index], ylim=ylim, add=TRUE, at=3, lwd=3, col="steelblue")
		if(all(round(msy_re[,index],5)==round(msy_re[,index][1],5))){
			points(x=4, y=msy_re[,index][1], pch=19, cex=2)
		}	else vioplot(msy_re[,index], ylim=ylim, add=TRUE, at=4, lwd=3, col="steelblue", colMed="black") 
		if(all(round(K_re[,index],5)==round(K_re[,index][1],5))){
			points(x=5, y=K_re[,index][1], pch=19, cex=2)
		}	else vioplot(K_re[,index], ylim=ylim, add=TRUE, at=5, lwd=3, col="steelblue", colMed="black") 		
		if(all(round(r_re[,index],5)==round(r_re[,index][1],5))){
			points(x=6, y=r_re[,index][1], pch=19, cex=2)
		}	else vioplot(r_re[,index], ylim=ylim, add=TRUE, at=6, lwd=3, col="steelblue", colMed="black") 		
		 # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
		axis(1, at=1:6, labels=FALSE)
		text(y=-1, x=seq(1.2, 6.2, by=1), par("usr")[1], labels = c("B", "B/Bmsy", "E/Emsy", "MSY", "K", "r"),
		 srt = 45, pos = 2, xpd = NA, cex=2)
	}
	mtext("Relative Error", font=2, outer=TRUE, cex=1.5, line=4, side=2)
	mtext("Parameter", font=2, outer=TRUE, cex=1.5, line=7, side=1)

}