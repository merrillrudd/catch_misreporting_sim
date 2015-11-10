Paper_Fig_Retro <- function(results, dirs){

	msy_est <- results$msy_est
    K_est <- results$K_est
    r_deriv <- (msy_est*((z_init+1)^(1/z_init + 1)))/(K_est*z_init)  
    msy_true <- results$msy_true
    K_true <- results$K_true
    r_true <- results$r_true

    b_est <- results$b_est ## iter by dir by year
    b_true <- results$b_true ## iter by dir by year
    c_rep <- results$c_rep ## iter by dir by year
    c_true <- results$c_true ## iter by dir by year
    bmsy_est <- results$bmsy_est ## iter by dir
    bmsy_true <- results$bmsy_true
    e_est <- results$c_rep/results$b_est
    e_true <- results$c_true/results$b_true

    msy_re <- (msy_est - msy_true)/msy_true
    K_re <- (K_est - K_true)/K_true
    r_re <- (r_deriv - r_true)/r_true
    bbmsy_re <- ((b_est/bmsy_est) - (b_true/bmsy_true))/(b_true/bmsy_true)
    eemsy_re <- (((c_rep/b_est)/(msy_est/bmsy_est)) - ((c_true/b_true)/(msy_true/bmsy_true)))/((c_true/b_true)/(msy_true/bmsy_true))
    b_re <- (b_est - b_true)/b_true
    e_re <- (e_est - e_true)/e_true

    assess_seq <- seq(15, 35, by=10)

    extend <- NULL
    for(i in 1:7){
    	seq <- c(1,2,3,4,5)+5*i
    	extend <- c(extend, rep(seq, 2))
    }
    lmat <- matrix(c(1,2,3,4,5, extend), nrow=7*2+1, ncol=5, byrow=TRUE)
    par(omi=c(1,1.5,0.5,1), mar=c(0,0,0,0))
    layout(lmat)
    layout.show(7*5 + 5)
    cols1 <- brewer.pal(3, "Set1")
    ##use highintegritydesign.com/tools/tinter-shader/ to get shades of rcolorbrewer colors
    cols2 <- c("#F38D8F", "#9BBFDB", "#95CF92")
    cols3 <- c("#FCD2D4", "#D7E6F0", "#DDEFDA") 

    	xplot <- 1:datyrs
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        lines(x=xplot, y=rep(1,datyrs), col=cols1[1], lwd=4)
        axis(2, at=seq(0.5, 1.5, by=0.5), las=2, cex.axis=1.7)
        mtext("100% Reporting", font=2, side=3, line=1.5, cex=1.25)
        mtext("Reporting\n  Rate", font=2, side=2, line=4, cex=1.5)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols1[1], lty=2, lwd=2)
        lines(x=xplot, y=rep(0.5, datyrs), col=cols1[2], lwd=4)
        mtext("Constant Under-reporting", font=2, side=3, line=1.5, cex=1.25)
		plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols1[1], lty=2, lwd=2)
        lines(x=xplot, y=rep(1.5, datyrs), col=cols1[2], lwd=4)
        mtext("Constant Over-reporting", font=2, side=3, line=1.5, cex=1.25)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        up <- (0.8-0.2)/(datyrs-1)*xplot + 0.2
        abline(h=1, col=cols1[1], lty=2, lwd=2)
        lines(x=xplot, y=up, col=cols1[3], lwd=4)
        mtext("Increasing Reporting", font=2, side=3, line=1.5, cex=1.25)
        plot(x=1, y=1, type="n", ylim=c(0,1.7), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=cols1[1], lty=2, lwd=2)
        down <- (0.2-0.8)/(datyrs-1)*xplot + 0.8
        lines(x=xplot, y=down, col=cols1[3], lwd=4)
        mtext("Decreasing Reporting", font=2, side=3, line=1.5, cex=1.25)

    yr1 <- which(assess_seq==15)
    yr2 <- which(assess_seq==25)
    yr3 <- which(assess_seq==35)

    ylim <- c(-1,3)
    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-c(1,5)], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(b_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(b_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(b_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("B", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(b_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(b_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(b_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(b_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(b_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(b_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(b_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(b_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(b_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(b_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(b_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(b_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

        plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-c(1,5)], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(e_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(e_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(e_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("E", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(e_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(e_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(e_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(e_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(e_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(e_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(e_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(e_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(e_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(e_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(e_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(e_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])


    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-1], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(bbmsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(bbmsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(bbmsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("B/Bmsy", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(bbmsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(bbmsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(bbmsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(bbmsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(bbmsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(bbmsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(bbmsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(bbmsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(bbmsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(bbmsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(bbmsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(bbmsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-1], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(eemsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(eemsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(eemsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("E/Emsy", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(eemsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(eemsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(eemsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(eemsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(eemsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(eemsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(eemsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(eemsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(eemsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(eemsy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(eemsy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(eemsy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-1], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(msy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(msy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(msy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("MSY", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(msy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(msy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(msy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(msy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(msy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(msy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(msy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(msy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(msy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(msy_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(msy_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(msy_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim)[-1], las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(K_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(K_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(K_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("K", font=2, side=2, line=4, cex=1.5)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(K_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(K_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(K_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(K_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(K_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(K_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(K_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(K_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(K_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(K_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(K_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(K_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])


    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allrep") + 5
    vioplot(r_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[1])
    vioplot(r_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[1])
    vioplot(r_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[1])
    mtext("r", font=2, side=2, line=4, cex=1.5)
    axis(side=1, at=1:3, labels=c(15,25,35), cex.axis=1.7)

    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allunder") + 5
    vioplot(r_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(r_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(r_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])
        axis(side=1, at=1:3, labels=c(15,25,35), cex.axis=1.7)


    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="allover") + 5
    vioplot(r_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[2])
    vioplot(r_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[2])
    vioplot(r_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[2])
        axis(side=1, at=1:3, labels=c(15,25,35), cex.axis=1.7)


    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repinc") + 5
    vioplot(r_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(r_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(r_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])
        axis(side=1, at=1:3, labels=c(15,25,35), cex.axis=1.7)


    plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,4),
		xaxt="n", yaxt="n", xaxs="i", yaxs="i")
    abline(h=0, lwd=2)
    # axis(2, at=pretty(ylim), las=2, cex.axis=1.7)
    model <- which(rmodel_vec=="repdec") + 5
    vioplot(r_re[,model,yr1], ylim=ylim, add=TRUE, at=1, col=cols3[3])
    vioplot(r_re[,model,yr2], ylim=ylim, add=TRUE, at=2, col=cols2[3])
    vioplot(r_re[,model,yr3], ylim=ylim, add=TRUE, at=3, col=cols1[3])
        axis(side=1, at=1:3, labels=c(15,25,35), cex.axis=1.7)


    mtext("Relative Error", font=2, side=2, line=7, outer=TRUE, adj=0.45, cex=1.5)
    mtext("Years of Data", font=2, side=1, line=4, outer=TRUE, cex=1.5)



}