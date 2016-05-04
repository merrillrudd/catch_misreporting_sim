plotTime <- function(dirs, results, datyrs, trend, to.plot){
	b_est <- results$b_est ## iter by dir by year
	b_true <- results$b_true ## iter by dir by year
	c_rep <- results$c_rep ## iter by dir by year
	c_true <- results$c_true ## iter by dir by year
	msy_est <- results$msy_est ## iter by dir
	bmsy_est <- results$bmsy_est ## iter by dir
	K_est <- results$K_est
	r_deriv <- (msy_est*(z_true+1)^(1/z_true + 1))/(K_est*z_true)

	det_dirs <- dirs[grep("sigma0.001", dirs)]
	stoch_dirs <- dirs[grep("sigma0.1", dirs)]

	assess_seq <- seq(5, datyrs, by=5)

	term_b_est <- b_est[,,length(assess_seq)]
	term_b_true <- b_true[,,length(assess_seq)]
	term_c_rep <- c_rep[,,length(assess_seq)]
	term_c_true <- c_true[,,length(assess_seq)]
	term_msy_est <- msy_est[,,length(assess_seq)]
	term_bmsy_est <- bmsy_est[,,length(assess_seq)]
	term_K_est <- K_est[,,length(assess_seq)]
	term_r_deriv <- r_deriv[,,length(assess_seq)]

	term_bbmsy_est <- term_b_est/term_bmsy_est
	term_bbmsy_true <- term_b_true/bmsy_true
	term_eemsy_est <- (term_c_rep/term_b_est)/(term_msy_est/term_bmsy_est)
	term_eemsy_true <- (term_c_true/term_b_true)/(MSY_true/bmsy_true)

	b_re <- (term_b_est - term_b_true)/term_b_true
	bbmsy_re <- (term_bbmsy_est - term_bbmsy_true)/term_bbmsy_true
	eemsy_re <- (term_eemsy_est - term_eemsy_true)/term_eemsy_true
	msy_re <- (term_msy_est - MSY_true)/MSY_true
	K_re <- (term_K_est - K_true)/K_true
	r_re <- (term_r_deriv - r_true)/r_true

	if(trend=="stochastic") plotdirs <- stoch_dirs
	if(trend=="deterministic") plotdirs <- det_dirs
	
	par(mfrow=c(6,2), mar=c(0,0,0,0), omi=c(1,1,0.5,3), xpd=FALSE)
	colors <- brewer.pal(5, "Set1")

	ylim <- c(-2,2)
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_b_est <- as.matrix(b_est[,index,])
		temp_b_true <- as.matrix(b_true[,index,])
		temp_est_med <- apply(temp_b_est, 2, function(x) median(x))
		temp_true_med <- apply(temp_b_true, 2, function(x) median(x))
		temp_re <- (temp_est_med - temp_true_med)/temp_true_med
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	axis(2, at=seq(0, ylim[2], by=1), cex.axis=2, las=2)
	mtext("Biomass", line=-2, font=2, cex=1.5)
	# mtext("Deterministic", cex=1.5, side=3, line=1, font=2)
	boxplot(b_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)
	# mtext("Stochastic", cex=1.5, side=3, line=1, font=2)
	par(xpd=NA)
	legend("topright", inset=c(-0.4,0), pch=15, col=colors, bty="n",
		legend=c("100% Reporting",
				 "\nConstant\nUnderreporting\n",
				 "\nReporting Rate\nIncreasing\n",
				 "\nReporting Rate\nDecreasing\n",
				 "\nConstant\nOverreporting\n"), cex=2)

	par(xpd=FALSE)
	ylim <- c(-1, 1.75)
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_K_est <- as.matrix(K_est[,index,])
		temp_est_med <- apply(temp_K_est, 2, function(x) median(x))
		temp_re <- (temp_est_med - K_true)/K_true
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	axis(2, at=seq(0,ylim[2],by=1), cex.axis=2, las=2)
	mtext("K", line=-2, font=2, cex=1.5)
	boxplot(K_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)

	ylim <- c(-1.2, max(r_re))
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_r_deriv <- as.matrix(r_deriv[,index,])
		temp_est_med <- apply(temp_r_deriv, 2, function(x) median(x))
		temp_re <- (temp_est_med - r_true)/r_true
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	axis(2, at=seq(0,ylim[2],by=3), cex.axis=2, las=2)
	mtext("r", line=-2, font=2, cex=1.5)
	boxplot(r_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)
	
	ylim <- c(-1.3, 1.5)
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_msy_est <- as.matrix(msy_est[,index,])
		temp_est_med <- apply(temp_msy_est, 2, function(x) median(x))
		temp_re <- (temp_est_med - MSY_true)/MSY_true
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	# legend("topleft", lwd=3, col=colors, legend=rmodel_vec_names, cex=1.2)
	axis(2, at=seq(0,ylim[2],by=1), cex.axis=2, las=2)
	mtext("MSY", line=-2, font=2, cex=1.5)
	boxplot(msy_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)


	ylim <- c(-1, 1)
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_b_est <- as.matrix(b_est[,index,])
		temp_b_true <- as.matrix(b_true[,index,])
		temp_bmsy_est <- as.matrix(bmsy_est[,index,])
		temp_bbmsy_est <- temp_b_est/temp_bmsy_est
		temp_bbmsy_true <- temp_b_true/bmsy_true
		temp_est_med <- apply(temp_bbmsy_est, 2, function(x) median(x))
		temp_true_med <- apply(temp_bbmsy_true, 2, function(x) median(x))
		temp_re <- (temp_est_med - temp_true_med)/temp_true_med
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	axis(2, at=seq(0,ylim[2],by=1), cex.axis=2, las=2)
	mtext("B/Bmsy", line=-2, font=2, cex=1.5)
	boxplot(bbmsy_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)

	ylim <- c(-1.5,5.5)
	plot(x=1, y=1, ylim=ylim, xlim=c(1, length(assess_seq)), type="n", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
	for(i in 1:length(plotdirs)){
		index <- which(dirs %in% plotdirs[i])
		temp_b_est <- as.matrix(b_est[,index,])
		temp_b_true <- as.matrix(b_true[,index,])
		temp_c_rep <- as.matrix(c_rep[,index,])
		temp_c_true <- as.matrix(c_true[,index,])
		temp_msy_est <- as.matrix(msy_est[,index,])
		temp_bmsy_est <- as.matrix(bmsy_est[,index,])
		temp_eemsy_est <- (temp_c_rep/temp_b_est)/(temp_msy_est/temp_bmsy_est)
		temp_eemsy_true <- (temp_c_true/temp_b_true)/(MSY_true/bmsy_true)
		temp_est_med <- apply(temp_eemsy_est, 2, function(x) median(x))
		temp_true_med <- apply(temp_eemsy_true, 2, function(x) median(x))
		temp_re <- (temp_est_med - temp_true_med)/temp_true_med
		lines(temp_re, lwd=3, col=colors[i], type="o")
		abline(h=0, lty=2, lwd=2)
	}
	axis(2, at=seq(0,ylim[2],by=3), cex.axis=2, las=2)
	mtext("E/Emsy", line=-2, font=2, cex=1.5)
	axis(1, at=1:length(assess_seq), labels=assess_seq, cex.axis=2)
	mtext("Years of Data", font=2, cex=1.5, line=3, side=1)
	
	boxplot(eemsy_re[,which(dirs %in% stoch_dirs)], xaxs="i", yaxs="i", ylim=ylim,
		col=colors, xaxt="n", yaxt="n")
	abline(h=0, col="black", lwd=2, lty=2)

	mtext("Terminal Year Uncertainty", font=2, cex=1.5, line=3, side=1)
	mtext("Relative Error", outer=TRUE, font=2, cex=1.5, line=3, side=2)

	


}
