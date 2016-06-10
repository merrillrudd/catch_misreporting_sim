## relerr==1 , relative error
## relerr==2, estimated/true

Paper_Fig_Terminal <- function(results, deterministic=FALSE, relerr=1, show_catch=TRUE, print_letter=TRUE, 
    show_params=TRUE, pres=FALSE, catch){

    require(beanplot)

    if(deterministic==TRUE){
       # index <- seq(1, 14, by=2)[-c(4,5)]
        index <- 1:5
    }
    if(deterministic==FALSE){
       index <- seq(2, 10, by=2)
       index_det <- seq(1, 10, by=2)
    }

		b_est <- results$b_est ## iter by dir by year
	    b_true <- results$b_true ## iter by dir by year
    	c_rep <- results$c_rep ## iter by dir by year
    	c_true <- results$c_true ## iter by dir by year
    	msy_est <- results$msy_est ## iter by dir
    	bmsy_est <- results$bmsy_est ## iter by dir
    	K_est <- results$K_est ## iter by dir    
        bmsy_true <- results$bmsy_true
        msy_true <- results$msy_true
        K_true <- results$K_true
        r_true <- results$r_true

    	term_b_est <- b_est[,,datyrs]
	    term_b_true <- b_true[,,datyrs]
    	
    	term_c_rep <- c_rep[,,datyrs]
    	term_c_true <- c_true[,,datyrs]

    	term_bbmsy_est <- term_b_est/bmsy_est
	    term_bbmsy_true <- term_b_true/bmsy_true
    	term_eemsy_est <- (term_c_rep/term_b_est)/(msy_est/bmsy_est)
        term_eemsy_true <- (term_c_true/term_b_true)/(msy_true/bmsy_true)
        emsy_est <- msy_est/bmsy_est
        emsy_true <- msy_true/bmsy_true
        term_e_est <- term_c_rep/term_b_est
        term_e_true <- term_c_true/term_b_true    

        r_est <- (msy_est*((z_true+1)^(1/z_true + 1)))/(K_est*z_true)


if(relerr==1){
    b_re <- (term_b_est - term_b_true)/term_b_true
    bbmsy_re <- (term_bbmsy_est - term_bbmsy_true)/term_bbmsy_true
    eemsy_re <- (term_eemsy_est - term_eemsy_true)/term_eemsy_true
    msy_re <- (msy_est - msy_true)/msy_true
    K_re <- (K_est - K_true)/K_true 
    r_re <- (r_est - r_true)/r_true
    e_re <- (term_e_est - term_e_true)/term_e_true
    emsy_re <- (emsy_est - emsy_true)/emsy_true
    bmsy_re <- (bmsy_est - bmsy_true)/bmsy_true
}
if(relerr==2){
    b_re <- term_b_est/term_b_true
    bbmsy_re <- term_bbmsy_est/term_bbmsy_true
    eemsy_re <- term_eemsy_est/term_eemsy_true
    msy_re <- msy_est/msy_true
    K_re <- K_est/K_true
    r_re <- r_est/r_true
    e_re <- term_e_est/term_e_true
    emsy_re <- emsy_est/emsy_true
    bmsy_re <- bmsy_est/bmsy_true
}
        
if(show_catch==TRUE){
        	lmat <- matrix(c(1,2,3,4,5,
	        				1,2,3,4,5,
                            6,7,8,9,10,
                            6,7,8,9,10,
                            6,7,8,9,10,
                  11,12,13,14,15,
                  16,17,18,19,20,
                  16,17,18,19,20,
                  16,17,18,19,20,
                  16,17,18,19,20,
                  16,17,18,19,20), nrow=11, ncol=5, byrow=TRUE)
}
if(show_catch==FALSE){
    lmat <- matrix(c(1,2,3,4,5,
                            1,2,3,4,5,
                  6,7,8,9,10,
                  11,12,13,14,15,
                  11,12,13,14,15,
                  11,12,13,14,15,
                  11,12,13,14,15,
                  11,12,13,14,15), nrow=8, ncol=5, byrow=TRUE)
}
if(show_params==FALSE){
    lmat <- matrix(c(1,2,3,4,5,
                    1,2,3,4,5,
                    6,7,8,9,10,
                    6,7,8,9,10), nrow=4, ncol=5, byrow=TRUE)
}

        if(pres==FALSE) par(omi=c(1.5,1.5,1,1), mar=c(0,0,0,0))
        if(pres==TRUE) par(omi=c(1.5, 1.5, 1, 1), mar=c(0,0,0,0), mgp=c(3.1, 1.2, 0))
        layout(lmat)
        # layout.show(n=10)
        cols <- brewer.pal(4, "Set1")

    ## reporting rates over time
        xplot <- 1:datyrs
        plot(x=1, y=1, type="n", ylim=c(0,1.9), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        lines(x=xplot, y=rep(1,datyrs), col=gray(0.2), lwd=4)
        if(show_catch==FALSE) axis(1, at=seq(10,30, by=10), cex.axis=2)
        axis(2, at=seq(0.5, 1.5, by=0.5), las=2, cex.axis=2)
        if(pres==FALSE) mtext("100% Reporting",  side=3, line=1.5, cex=1.6)
        if(pres==TRUE) mtext("Truth", side=3, line=1.5, cex=2.5)
        mtext("Reporting\n  Rate",  side=2, line=4, cex=1.6)
        if(print_letter==TRUE) print.letter(label="(a)", xy=c(0.05,0.90), cex=2,  font=2, col="black", xpd=NA)
        plot(x=1, y=1, type="n", ylim=c(0,1.9), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=gray(0.6), lwd=2)
        if(show_catch==FALSE) axis(1, at=seq(10,30, by=10), cex.axis=2)
        lines(x=xplot, y=rep(0.5, datyrs), col=cols[1], lwd=4)
        if(pres==FALSE) mtext("Constant Under-reporting",  side=3, line=1.5, cex=1.6)
        if(pres==TRUE) mtext("Constant Under",  side=3, line=1.5, cex=2.5)
        if(print_letter==TRUE) print.letter(label="(b)", xy=c(0.05,0.90), cex=2,  font=2, col="black", xpd=NA)
		plot(x=1, y=1, type="n", ylim=c(0,1.9), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=gray(0.6), lwd=2)
        if(show_catch==FALSE)    axis(1, at=seq(10,30, by=10), cex.axis=2)
        lines(x=xplot, y=rep(1.5, datyrs), col=cols[2], lwd=4)
        if(pres==FALSE) mtext("Constant Over-reporting",  side=3, line=1.5, cex=1.6)
        if(pres==TRUE) mtext("Constant Over",  side=3, line=1.5, cex=2.5)
        if(show_catch==FALSE)    mtext("Year",  side=1, line=3, cex=1.6)
        if(print_letter==TRUE) print.letter(label="(c)", xy=c(0.05,0.90), cex=2,  font=2, col="black", xpd=NA)
        plot(x=1, y=1, type="n", ylim=c(0,1.9), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        up <- (0.9-0.4)/(datyrs-1)*xplot + 0.4
        abline(h=1, col=gray(0.6), lwd=2)
        if(show_catch==FALSE)     axis(1, at=seq(10,30, by=10), cex.axis=2)
        lines(x=xplot, y=up, col=cols[3], lwd=4)
        if(pres==FALSE) mtext("Increasing Reporting",  side=3, line=1.5, cex=1.6)
        if(pres==TRUE) mtext("Increasing",  side=3, line=1.5, cex=2.5)
        if(print_letter==TRUE) print.letter(label="(d)", xy=c(0.05,0.90), cex=2,  font=2, col="black", xpd=NA)
        plot(x=1, y=1, type="n", ylim=c(0,1.9), xlim=c(1,35), ann=F, 
        	xaxt="n",  yaxt="n", xaxs="i", yaxs="i")
        abline(h=1, col=gray(0.6), lwd=2)
        if(show_catch==FALSE)     axis(1, at=seq(10,30, by=10), cex.axis=2)
        down <- (0.4-0.9)/(datyrs-1)*xplot + 0.9
        lines(x=xplot, y=down, col=cols[4], lwd=4)
        if(pres==FALSE) mtext("Decreasing Reporting",  side=3, line=1.5, cex=1.6)
        if(pres==TRUE) mtext("Decreasing",  side=3, line=1.5, cex=2.5)
        if(print_letter==TRUE) print.letter(label="(e)", xy=c(0.05,0.90), cex=2,  font=2, col="black", xpd=NA)



if(show_catch==TRUE){
    ## catch time series
    plot(x=xplot, y=results$c_true[1,1,], type="l", lwd=4, ylim=c(0, max(results$c_true[1,1,])*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.2))
    # lines(x=xplot, y=results$c_true[1,1,], lwd=3, col=gray(0.6))
    axis(1, at=seq(10,30, by=10), cex.axis=2)
    axis(2, at=pretty(c(1,results$c_true[1,1,]*1.6)), las=2, cex.axis=2)
    mtext("Catch",  side=2, line=4, cex=1.6)
    if(print_letter==TRUE) print.letter(label="(f)", xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
    plot(x=xplot, y=results$c_true[1,1,], type="l", lwd=2, ylim=c(0, max(results$c_true[1,1,])*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=results$c_rep[1,2,], lwd=4, col=cols[1])
    axis(1, at=seq(10,30, by=10), cex.axis=2)
    if(print_letter==TRUE) print.letter(label="(g)", xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
    plot(x=xplot, y=results$c_true[1,1,], type="l", lwd=2, xaxs="i", ylim=c(0, max(results$c_true[1,1,])*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=results$c_rep[1,3,], lwd=4, col=cols[2])
    axis(1, at=seq(10,30, by=10), cex.axis=2)
    mtext("Year",  side=1, line=3, cex=1.6)
    if(print_letter==TRUE) print.letter(label="(h)", xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
    plot(x=xplot, y=results$c_true[1,1,], type="l", lwd=2, xaxs="i", ylim=c(0, max(results$c_true[1,1,])*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=results$c_rep[1,4,], lwd=4, col=cols[3])
    axis(1, at=seq(10,30, by=10), cex.axis=2)
    if(print_letter==TRUE) print.letter(label="(i)", xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
    plot(x=xplot, y=results$c_true[1,1,], type="l", lwd=2, xaxs="i", ylim=c(0, max(results$c_true[1,1,])*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=results$c_rep[1,5,], lwd=4, col=cols[4])
    axis(1, at=seq(10,30, by=10), cex.axis=2)
    if(print_letter==TRUE) print.letter(label="(j)", xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
}

if(show_params==TRUE){

    plot(x=1,y=1,type="n", axes=F, ann=F)
    plot(x=1,y=1,type="n", axes=F, ann=F)
    plot(x=1,y=1,type="n", axes=F, ann=F)
    plot(x=1,y=1,type="n", axes=F, ann=F)
    plot(x=1,y=1,type="n", axes=F, ann=F)

    ## results
    if(deterministic==FALSE){
    if(relerr==1) ylim <- c(-1, 2.5)
    if(relerr==2) ylim <- c(0, 2.5)
    medcol <- "black"
      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
			   # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allrep") + index_add
        if(pres==FALSE) beanplot(b_re[,index[1]], msy_re[,index[1]], e_re[,index[1]], K_re[,index[1]], r_re[,index[1]], bbmsy_re[,index[1]], eemsy_re[,index[1]], 
            what=c(0,1,1,0),
            cex=0.5, beanlinewd=5, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(gray(0.2),medcol ,NA, medcol), overalline="median", beanlines="median",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(pres==TRUE) beanplot(msy_re[,index[1]], K_re[,index[1]], b_re[,index[1]], e_re[,index[1]],
            what=c(0,1,0,0),
            cex=0.5, beanlinewd=3, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(gray(0.2),medcol ,NA, medcol), overalline="median", beanlines="quantiles",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        # if(pres==TRUE){
        # segments(x0=0.6, x1=1.4, y0=median(msy_re[,index[1]]), y1=median(msy_re[,index[1]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(K_re[,index[1]]), y1=median(K_re[,index[1]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(b_re[,index[1]]), y1=median(b_re[,index[1]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(e_re[,index[1]]), y1=median(e_re[,index[1]]), lwd=6)
        # }
        # if(pres==FALSE){
        # segments(x0=0.6, x1=1.4, y0=median(b_re[,index[1]]), y1=median(b_re[,index[1]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(msy_re[,index[1]]), y1=median(msy_re[,index[1]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(e_re[,index[1]]), y1=median(e_re[,index[1]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(K_re[,index[1]]), y1=median(K_re[,index[1]]), lwd=6)
        # segments(x0=4.6, x1=5.4, y0=median(r_re[,index[1]]), y1=median(r_re[,index[1]]), lwd=6)
        # segments(x0=5.6, x1=6.4, y0=median(bbmsy_re[,index[1]]), y1=median(bbmsy_re[,index[1]]), lwd=6)
        # segments(x0=6.6, x1=7.4, y0=median(eemsy_re[,index[1]]), y1=median(eemsy_re[,index[1]]), lwd=6)
        # }


        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(2, at=pretty(ylim), las=2, cex.axis=2)
        if(pres==FALSE) axis(1, at=1:7, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)
        if(pres==TRUE) axis(1, at=1:4, labels=c(expression(italic("MSY")), expression(italic("K")), expression(italic("B")), expression(italic("u"))), cex.axis=3)
        if(relerr==1) mtext("Relative Error",  side=2, line=4, cex=1.6) 
        if(relerr==2) mtext("Estimated / True",  side=2, line=4, cex=1.6)
        if(print_letter==TRUE) print.letter(label="(k)", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)


      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
			   # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allunder") + index_add
        if(pres==FALSE) beanplot(b_re[,index[2]], msy_re[,index[2]], e_re[,index[2]], K_re[,index[2]], r_re[,index[2]], bbmsy_re[,index[2]], eemsy_re[,index[2]], 
            what=c(0,1,1,0),
            cex=0.5, beanlinewd=5, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[1],medcol,NA, medcol), overalline="median", beanlines="median",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(pres==TRUE) beanplot(msy_re[,index[2]], K_re[,index[2]], b_re[,index[2]], e_re[,index[2]],
            what=c(0,1,0,0),
            cex=0.5, beanlinewd=3, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[1],medcol,NA, medcol), overalline="median", beanlines="quantiles",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        # if(pres==TRUE){
        # segments(x0=0.6, x1=1.4, y0=median(msy_re[,index[2]]), y1=median(msy_re[,index[2]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(K_re[,index[2]]), y1=median(K_re[,index[2]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(b_re[,index[2]]), y1=median(b_re[,index[2]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(e_re[,index[2]]), y1=median(e_re[,index[2]]), lwd=6)
        # }
        # if(pres==FALSE){
        # segments(x0=0.6, x1=1.4, y0=median(b_re[,index[2]]), y1=median(b_re[,index[2]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(msy_re[,index[2]]), y1=median(msy_re[,index[2]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(e_re[,index[2]]), y1=median(e_re[,index[2]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(K_re[,index[2]]), y1=median(K_re[,index[2]]), lwd=6)
        # segments(x0=4.6, x1=5.4, y0=median(r_re[,index[2]]), y1=median(r_re[,index[2]]), lwd=6)
        # segments(x0=5.6, x1=6.4, y0=median(bbmsy_re[,index[2]]), y1=median(bbmsy_re[,index[2]]), lwd=6)
        # segments(x0=6.6, x1=7.4, y0=median(eemsy_re[,index[2]]), y1=median(eemsy_re[,index[2]]), lwd=6)
        # }
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        if(pres==FALSE) axis(1, at=1:7, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)
        if(pres==TRUE) axis(1, at=1:4, labels=c(expression(italic("MSY")), expression(italic("K")), expression(italic("B")), expression(italic("u"))), cex.axis=3)
        if(print_letter==TRUE) print.letter(label="(l)", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)


      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
			   # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allover") + index_add
        if(pres==FALSE) beanplot(b_re[,index[3]], msy_re[,index[3]], e_re[,index[3]], K_re[,index[3]], r_re[,index[3]], bbmsy_re[,index[3]], eemsy_re[,index[3]], 
            what=c(0,1,1,0),
            cex=0.5, beanlinewd=5, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[2],medcol,NA, medcol), overalline="median", beanlines="median",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(pres==TRUE) beanplot(msy_re[,index[3]], K_re[,index[3]], b_re[,index[3]], e_re[,index[3]],
            what=c(0,1,0,0),
            cex=0.5, beanlinewd=3, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[2],medcol,NA, medcol), overalline="median", beanlines="quantiles",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        # if(pres==TRUE){
        # segments(x0=0.6, x1=1.4, y0=median(msy_re[,index[3]]), y1=median(msy_re[,index[3]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(K_re[,index[3]]), y1=median(K_re[,index[3]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(b_re[,index[3]]), y1=median(b_re[,index[3]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(e_re[,index[3]]), y1=median(e_re[,index[3]]), lwd=6)
        # }
        # if(pres==FALSE){
        # segments(x0=0.6, x1=1.4, y0=median(b_re[,index[3]]), y1=median(b_re[,index[3]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(msy_re[,index[3]]), y1=median(msy_re[,index[3]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(e_re[,index[3]]), y1=median(e_re[,index[3]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(K_re[,index[3]]), y1=median(K_re[,index[3]]), lwd=6)
        # segments(x0=4.6, x1=5.4, y0=median(r_re[,index[3]]), y1=median(r_re[,index[3]]), lwd=6)
        # segments(x0=5.6, x1=6.4, y0=median(bbmsy_re[,index[3]]), y1=median(bbmsy_re[,index[3]]), lwd=6)
        # segments(x0=6.6, x1=7.4, y0=median(eemsy_re[,index[3]]), y1=median(eemsy_re[,index[3]]), lwd=6)
        # }

        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        if(pres==FALSE) axis(1, at=1:7, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)
        if(pres==TRUE) axis(1, at=1:4, labels=c(expression(italic("MSY")), expression(italic("K")), expression(italic("B")), expression(italic("u"))), cex.axis=3)
        if(print_letter==TRUE) print.letter(label="(m)", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
			   # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="repinc") + index_add
        if(pres==FALSE) beanplot(b_re[,index[4]], msy_re[,index[4]], e_re[,index[4]], K_re[,index[4]], r_re[,index[4]], bbmsy_re[,index[4]], eemsy_re[,index[4]], 
            what=c(0,1,1,0),
            cex=0.5, beanlinewd=5, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[3],medcol,NA, medcol), overalline="median", beanlines="median",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(pres==TRUE) beanplot(msy_re[,index[4]], K_re[,index[4]], b_re[,index[4]], e_re[,index[4]],
            what=c(0,1,0,0),
            cex=0.5, beanlinewd=3, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[3],medcol,NA, medcol), overalline="median", beanlines="quantiles",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        # if(pres==TRUE){
        # segments(x0=0.6, x1=1.4, y0=median(msy_re[,index[4]]), y1=median(msy_re[,index[4]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(K_re[,index[4]]), y1=median(K_re[,index[4]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(b_re[,index[4]]), y1=median(b_re[,index[4]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(e_re[,index[4]]), y1=median(e_re[,index[4]]), lwd=6)
        # }
        # if(pres==FALSE){
        # segments(x0=0.6, x1=1.4, y0=median(b_re[,index[4]]), y1=median(b_re[,index[4]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(msy_re[,index[4]]), y1=median(msy_re[,index[4]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(e_re[,index[4]]), y1=median(e_re[,index[4]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(K_re[,index[4]]), y1=median(K_re[,index[4]]), lwd=6)
        # segments(x0=4.6, x1=5.4, y0=median(r_re[,index[4]]), y1=median(r_re[,index[4]]), lwd=6)
        # segments(x0=5.6, x1=6.4, y0=median(bbmsy_re[,index[4]]), y1=median(bbmsy_re[,index[4]]), lwd=6)
        # segments(x0=6.6, x1=7.4, y0=median(eemsy_re[,index[4]]), y1=median(eemsy_re[,index[4]]), lwd=6)
        # }
        
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        if(pres==FALSE) axis(1, at=1:7, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)
        if(pres==TRUE) axis(1, at=1:4, labels=c(expression(italic("MSY")), expression(italic("K")), expression(italic("B")), expression(italic("u"))), cex.axis=3)
        if(print_letter==TRUE) print.letter(label="(n)", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
			   # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
       # index <- which(rmodel_vec=="repdec") + index_add
        if(pres==FALSE) beanplot(b_re[,index[5]], msy_re[,index[5]], e_re[,index[5]], K_re[,index[5]], r_re[,index[5]], bbmsy_re[,index[5]], eemsy_re[,index[5]], 
            what=c(0,1,1,0),
            cex=0.5, beanlinewd=5, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[4],medcol,NA, medcol), overalline="median", beanlines="median",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(pres==TRUE) beanplot(msy_re[,index[5]], K_re[,index[5]], b_re[,index[5]], e_re[,index[5]],
            what=c(0,1,0,0),
            cex=0.5, beanlinewd=3, maxstripline=0.5, maxwidth=0.75, 
            ylim=ylim, log="", col=c(cols[4],medcol,NA, medcol), overalline="median", beanlines="quantiles",
            border=NA, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        # if(pres==TRUE){
        # segments(x0=0.6, x1=1.4, y0=median(msy_re[,index[5]]), y1=median(msy_re[,index[5]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(K_re[,index[5]]), y1=median(K_re[,index[5]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(b_re[,index[5]]), y1=median(b_re[,index[5]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(e_re[,index[5]]), y1=median(e_re[,index[5]]), lwd=6)
        # }
        # if(pres==FALSE){
        # segments(x0=0.6, x1=1.4, y0=median(b_re[,index[5]]), y1=median(b_re[,index[5]]), lwd=6)
        # segments(x0=1.6, x1=2.4, y0=median(msy_re[,index[5]]), y1=median(msy_re[,index[5]]), lwd=6)
        # segments(x0=2.6, x1=3.4, y0=median(e_re[,index[5]]), y1=median(e_re[,index[5]]), lwd=6)
        # segments(x0=3.6, x1=4.4, y0=median(K_re[,index[5]]), y1=median(K_re[,index[5]]), lwd=6)
        # segments(x0=4.6, x1=5.4, y0=median(r_re[,index[5]]), y1=median(r_re[,index[5]]), lwd=6)
        # segments(x0=5.6, x1=6.4, y0=median(bbmsy_re[,index[5]]), y1=median(bbmsy_re[,index[5]]), lwd=6)
        # segments(x0=6.6, x1=7.4, y0=median(eemsy_re[,index[5]]), y1=median(eemsy_re[,index[5]]), lwd=6)
        # }
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        if(pres==FALSE) axis(1, at=1:7, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)
        if(pres==TRUE) axis(1, at=1:4, labels=c(expression(italic("MSY")), expression(italic("K")), expression(italic("B")), expression(italic("u"))), cex.axis=3)
        if(print_letter==TRUE) print.letter(label="(o)", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

    mtext("Parameter",  side=1, line=9, cex=1.6, outer=TRUE)
    }

    if(deterministic==TRUE){
    if(relerr==1) ylim <- c(-1, 3.5)
    if(relerr==2) ylim <- c(0, 3.5)
    medcol <- NA
      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
               # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allrep") + index_add
        plot(x=1:8, y=c(b_re[1,1], msy_re[1,1], e_re[1,1], emsy_re[1,1], K_re[1,1], r_re[1,1], bbmsy_re[1,1], eemsy_re[1,1]), ylim=ylim, xlim=c(0,8), col=gray(0.6), pch=19, cex=3, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(2, at=pretty(ylim), las=2, cex.axis=2)
        axis(1, at=1:8, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic(u[{MSY}])), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)  
        if(relerr==1) mtext("Relative Error",  side=2, line=4, cex=1.6) 
        if(relerr==2) mtext("Estimated / True",  side=2, line=4, cex=1.6)
        if(print_letter==TRUE) print.letter(label="k", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
               # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allunder") + index_add
        plot(x=1:8, y=c(b_re[1,2], msy_re[1,2], e_re[1,2], emsy_re[1,2], K_re[1,2], r_re[1,2], bbmsy_re[1,2], eemsy_re[1,2]), ylim=ylim, xlim=c(0,8), col=cols[1], pch=19, cex=3, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(1, at=1:8, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic(u[{MSY}])), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)  
        if(print_letter==TRUE) print.letter(label="l", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)


      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
               # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="allover") + index_add
        plot(x=1:8, y=c(b_re[1,3], msy_re[1,3], e_re[1,3], emsy_re[1,3], K_re[1,3], r_re[1,3], bbmsy_re[1,3], eemsy_re[1,3]), ylim=ylim, xlim=c(0,8), col=cols[2], pch=19, cex=3, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(1, at=1:8, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic(u[{MSY}])), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)  
        if(print_letter==TRUE) print.letter(label="m", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
               # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="repinc") + index_add
        plot(x=1:8, y=c(b_re[1,4], msy_re[1,4], e_re[1,4], emsy_re[1,4], K_re[1,4], r_re[1,4], bbmsy_re[1,4], eemsy_re[1,4]), ylim=ylim, xlim=c(0,8), col=cols[3], pch=19, cex=3, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(1, at=1:8, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic(u[{MSY}])), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)  
        if(print_letter==TRUE) print.letter(label="n", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)

      #   plot(x=1, y=1, ylim=ylim, type="n", xlim=c(0,8),
               # xaxt="n", yaxt="n", xaxs="i", yaxs="i")
        #index <- which(rmodel_vec=="repdec") + index_add
        plot(x=1:8,  y=c(b_re[1,5], msy_re[1,5], e_re[1,5], emsy_re[1,5], K_re[1,5], r_re[1,5], bbmsy_re[1,5], eemsy_re[1,5]), ylim=ylim, xlim=c(0,8), col=cols[4], pch=19, cex=3, xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
        if(relerr==1) abline(h=0, lwd=1, lty=2)
        if(relerr==2) abline(h=1, lwd=1, lty=2)
        axis(1, at=1:8, labels=c(expression(italic("B")), expression(italic("MSY")), expression(italic("u")), expression(italic(u[{MSY}])), expression(italic("K")), expression(italic("r")), expression(italic(B/B[{MSY}])), expression(italic(u/u[{MSY}]))), las=2, cex.axis=2)  
        if(print_letter==TRUE) print.letter(label="o", xy=c(0.05,0.95), cex=2,  font=2, col="black", xpd=NA)
        mtext("Parameter",  side=1, line=9, cex=1.6, outer=TRUE)
    }
  }

}