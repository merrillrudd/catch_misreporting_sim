Paper_Fig_Project <- function(results, run_project=FALSE, start_sim, nproject=10, pres=FALSE, 
    print_letters=TRUE, constantTAC=FALSE, msy=FALSE){


	if(run_project==TRUE){
		unlink(file.path(res_dir, paste0("b_proj_", start_sim, ".rds")), TRUE)
		unlink(file.path(res_dir, paste0("b_proj_t_", start_sim, ".rds")), TRUE)
		unlink(file.path(res_dir, paste0("c_proj_", start_sim, ".rds")), TRUE)
		unlink(file.path(res_dir, paste0("c_proj_t_", start_sim, ".rds")), TRUE)

		b_est <- results$b_est ## iter by dir by year
	    b_true <- results$b_true ## iter by dir by year
    	bmsy_est <- results$bmsy_est ## iter by dir
    	c_rep <- results$c_rep ## iter by dir by year
    	c_true <- results$c_true ## iter by dir by year
    	msy_est <- results$msy_est ## iter by dir
    	K_est <- results$K_est ## iter by dir    

        b_proj <- b_proj_t <- c_proj_t <- TAC <- array(NA, dim=c(dim(b_est)[2],nproject)) # 

        b_est_start <- b_est[,,datyrs]
        b_true_start <- b_true[,,datyrs]
        c_est_start <- c_rep[,,datyrs]
        c_true_start <- c_true[,,datyrs]    

        rep <- c(1,1,0.5,0.5,1.5,1.5,0.8,0.8,0.2,0.2,0.9,0.9,0.4,0.4) 

        umsy_est <- msy_est/bmsy_est
        umsy_true <- msy_true/bmsy_true

        ## managers set catch = Fmsy*B(last year)
        for(mod in 1:ncol(b_est)){

        	b_proj[mod,1] <- max(1, b_est_start[1,mod] + ((z_true^(z_true/(z_true-1)))/(z_true-1))*msy_est[1,mod]*((b_est_start[1,mod]/K_est[1,mod])-(b_est_start[1,mod]/K_est[1,mod])^z_true) - c_est_start[1,mod])
            if(msy==FALSE) TAC[mod,1] <- umsy_est[1,mod]*b_est_start[1,mod]
            if(constantTAC==TRUE & msy==TRUE) TAC[mod,1] <- msy_est[1,mod]
        	b_proj_t[mod,1] <- max(1, b_true_start[1,mod] + ((z_true^(z_true/(z_true-1)))/(z_true-1))*msy_true*((b_true_start[1,mod]/K_true)-(b_true_start[1,mod]/K_true)^z_true) - c_true_start[1,mod])
            c_proj_t[mod,1] <- min(b_proj_t[mod,1], TAC[mod,1]/rep[mod])

       		for(yr in 2:nproject){
                b_proj[mod,yr] <- max(1, b_proj[mod,yr-1] + ((z_true^(z_true/(z_true-1)))/(z_true-1))*msy_est[1,mod]*((b_proj[mod,yr-1]/K_est[1,mod])-(b_proj[mod,yr-1]/K_est[1,mod])^z_true) - TAC[mod,yr-1])
                if(constantTAC==FALSE) TAC[mod,yr] <- umsy_est[1,mod]*b_proj[mod,yr]
                if(constantTAC==TRUE) TAC[mod,yr] <- TAC[mod,1] 
                b_proj_t[mod,yr] <- max(1, b_proj_t[mod,yr-1] + ((z_true^(z_true/(z_true-1)))/(z_true-1))*msy_true*((b_proj_t[mod,yr-1]/K_true)-(b_proj_t[mod,yr-1]/K_true)^z_true) - c_proj_t[mod,yr-1])
                c_proj_t[mod,yr] <- min(b_proj_t[mod,yr], TAC[mod,yr]/rep[mod])

            }
           
        }    

        
        saveRDS(b_proj, file.path(res_dir, paste0("b_proj_", start_sim, ".rds")))
        saveRDS(b_proj_t, file.path(res_dir, paste0("b_proj_t_", start_sim,".rds")))
        # saveRDS(c_proj, file.path(res_dir, paste0("c_proj_", start_sim,".rds")))
        saveRDS(c_proj_t, file.path(res_dir, paste0("c_proj_t_", start_sim,".rds")))
        saveRDS(TAC, file.path(res_dir, paste0("TAC_",start_sim, ".rds")))
	}

	if(run_project==FALSE){

        if(file.exists(file.path(res_dir, paste0("b_proj_", start_sim, ".rds")))==FALSE){
            stop("Need to project simulation forward in time")
        }

		b_proj <- readRDS(file.path(res_dir, paste0("b_proj_", start_sim,".rds")))
		b_proj_t <- readRDS(file.path(res_dir, paste0("b_proj_t_", start_sim,".rds")))
		# c_proj <- readRDS(file.path(res_dir, paste0("c_proj_", start_sim,".rds")))
		c_proj_t <- readRDS(file.path(res_dir, paste0("c_proj_t_", start_sim,".rds")))
        TAC <- readRDS(file.path(res_dir, paste0("TAC_", start_sim, ".rds")))

		b_est <- results$b_est ## iter by dir by year
	    b_true <- results$b_true ## iter by dir by year
    	bmsy_est <- results$bmsy_est ## iter by dir
    	c_rep <- results$c_rep ## iter by dir by year
    	c_true <- results$c_true ## iter by dir by year
    	msy_est <- results$msy_est ## iter by dir
    	K_est <- results$K_est ## iter by dir    

        rep <- c(1,1,0.5,0.5,1.5,1.5,0.8,0.8,0.2,0.2,0.9,0.9,0.4,0.4) 

		umsy_est <- msy_est/bmsy_est
        umsy_true <- msy_true/bmsy_true
	}

    # Beq <- exp(log((K_true^z_true)/K_true - ((K_true^z_true)*umsy_est*rep)/(((z_true^(z_true/(z_true-1)))/(z_true-1))*msy_true))/(z_true-1))


    lmat <- matrix(c(1,2,3,4,5,
	        		6,7,8,9,10,
                  11,12,13,14,15,
                  16,17,18,19,20,
                  16,17,18,19,20), nrow=5, ncol=5, byrow=TRUE)

    if(pres==TRUE){
        axis_lab <- 2.5
        lab_line <- 6
    }
    if(pres==FALSE){
        axis_lab <- 1.7
        lab_line <- 4
    }

    index <- seq(1,14, by=2)[-c(4,5)]
    cols <- brewer.pal(4, "Set1")
    if(pres==FALSE){
    	par(mfrow=c(4,5), mar=c(0,0,0,0), omi=c(1,1,1,0.2))
        for(i in 1:length(index)){
            plot(x=1, y=1, xlim=c(1,35+nproject), ylim=c(min(catch1),max(catch1)*1.6), type="n", yaxt="n", xaxs="i", yaxs="i", cex.axis=axis_lab, xaxt="n")
            mtext(rmodel_vec_names[i],  side=3, line=1.5, cex=1.6)
            polygon(x=c(1:36, 36:1), y=c(rep(-1,36), rep(2000,36)), col="#AAAAAA50", border=NA)
      #     polygon(x=c(1:35, 35:1), y=c(q_c_true[1,,i]/q_b_true[1,,i], rev(q_c_true[3,,i]/q_b_true[3,,i])), col="#0000FF50", border=NA)
            # polygon(x=c(1:35, 35:1), y=c(q_c_rep[1,,i]/q_b_est[1,,i], rev(q_c_rep[3,,i]/q_b_est[3,,i])), col="#FF000050", border=NA)
            # polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_c_rep[1,35,i]/q_b_est[1,35,i], q_c_proj[1,,i]/q_b_proj[1,,i], rev(q_c_proj[3,,i]/q_b_proj[3,,i]), q_c_rep[3,35,i]/q_b_est[3,35,i]), col="#FF000050", border=NA)
            # polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_c_true[1,35,i]/q_b_true[1,35,i], q_c_proj_t[1,,i]/q_b_proj_t[1,,i], rev(q_c_proj_t[3,,i]/q_b_proj_t[3,,i]), q_c_true[3,35,i]/q_b_true[3,35,i]), col="#0000FF50", border=NA)
            abline(h=msy_true, lty=2)
            # abline(v=35, lty=2)
            if(i==1){
                axis(2, at=pretty(c(min(catch1), max(catch1)*1.6))[-length(pretty(c(min(catch1), max(catch1)*1.6)))], las=2,cex.axis=axis_lab)
               mtext("Catch",  side=2, line=lab_line, cex=1.6)
            }
            plotcol <- ifelse(i==1, gray(0.6), cols[i-1])
            lines(x=1:35, y=c_rep[1,index[i],], lwd=5, col=plotcol)
            lines(x=35:(35+nproject), y=c(c_rep[1,index[i],35], TAC[index[i],]), lwd=5, col=plotcol)
            lines(x=1:35, y=c_true[1,index[i],], lwd=2, col=gray(0.2))
            lines(x=35:(35+nproject), y=c(c_true[1,index[i],35], c_proj_t[index[i],]), col=gray(0.2), lwd=2)
            if(print_letters==TRUE) print.letter(label=paste0("(", letters[i],")"), xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
            if(i==3){
                text(x=41, y=140, "Estimated", col=plotcol, font=2, cex=2)
                text(x=39, y=39, "True", col=gray(0.2), font=2, cex=2)
            }
        } 
    }
    if(pres==TRUE) par(mfrow=c(3,5), mar=c(0,0,0,0), omi=c(1,1,1,0.2))
        for(i in 1:length(index)){
    		plot(x=1, y=1, xlim=c(1,35+nproject), ylim=c(0, 1500), type="n", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
    		polygon(x=c(1:36, 36:1), y=c(rep(-1,36), rep(2000,36)), col="#AAAAAA50", border=NA)
    		# polygon(x=c(1:35, 35:1), y=c(q_b_true[1,,i], rev(q_b_true[3,,i])), col="#0000FF50", border=NA)
    		# polygon(x=c(1:35, 35:1), y=c(q_b_est[1,,i], rev(q_b_est[3,,i])), col="#FF000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_b_est[1,35,i], q_b_proj[1,,i], rev(q_b_proj[3,,i]), q_b_est[3,35,i]), col="#AA000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_b_true[1,35,i], q_b_proj_t[1,,i], rev(q_b_proj_t[3,,i]), q_b_true[3,35,i]), col="#0000AA50", border=NA)
    		abline(h=bmsy_true, col="black", lty=2)
    		# abline(v=36, col="black", lty=2)
    		if(i==1){
    			axis(2, at=pretty(c(0,1500)), las=2, cex.axis=axis_lab)
                mtext("Biomass",  side=2, line=lab_line, cex=1.6)
    		}
            plotcol <- ifelse(i==1, gray(0.6), cols[i-1])
            lines(x=1:35, y=b_est[1,index[i],], lwd=5, col=plotcol)
            lines(x=35:(35+nproject), y=c(b_est[1,index[i],35], b_proj[index[i],]), lwd=5, col=plotcol) 
    		lines(x=1:35, y=b_true[1,index[i],], lwd=2, col=gray(0.2))
    	    lines(x=35:(35+nproject), y=c(b_true[1,index[i],35], b_proj_t[index[i],]), lwd=2, col=gray(0.2))
            if(print_letters==TRUE) print.letter(label=paste0("(", letters[i+5],")"), xy=c(0.05,0.92), cex=2,  font=2, col="black", xpd=NA)
        }    

        for(i in 1:length(index)){
    		plot(x=1, y=1, xlim=c(1,35+nproject), ylim=c(0, 3), type="n", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
    		polygon(x=c(1:36, 36:1), y=c(rep(-1,36), rep(2000,36)), col="#AAAAAA50", border=NA)
    		# polygon(x=c(1:35, 35:1), y=c(q_bbmsy_true[1,,i], rev(q_bbmsy_true[3,,i])), col="#0000FF50", border=NA)
    		# polygon(x=c(1:35, 35:1), y=c(q_bbmsy_est[1,,i], rev(q_bbmsy_est[3,,i])), col="#FF000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_bbmsy_est[1,35,i], q_bbmsy_proj[1,,i], rev(q_bbmsy_proj[3,,i]), q_bbmsy_est[3,35,i]), col="#AA000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_bbmsy_true[1,35,i], q_bbmsy_proj_t[1,,i], rev(q_bbmsy_proj_t[3,,i]), q_bbmsy_true[3,35,i]), col="#0000AA50", border=NA)
    		abline(h=1, col="black", lty=2)
    		# abline(v=36, col="black", lty=2)
    		if(i==1){
    			axis(2, at=pretty(c(0,3))[-length(pretty(c(0,3)))], las=2, cex.axis=axis_lab)
    			mtext(expression(italic(B/B[{MSY}])),  side=2, line=lab_line, cex=1.6)
    		}
            plotcol <- ifelse(i==1, gray(0.6), cols[i-1])    		
            lines(x=1:35, y=b_est[1,index[i],]/bmsy_est[1,index[i]], lwd=5, col=plotcol)
            lines(x=35:(35+nproject), y=c(b_est[1,index[i],35]/bmsy_est[1,index[i]], b_proj[index[i],]/bmsy_est[1,index[i]]), lwd=5, col=plotcol)   
            lines(x=1:35, y=b_true[1,index[i],]/bmsy_true, lwd=2, col=gray(0.2))
    	    lines(x=35:(35+nproject), y=c(b_true[1,index[i],35]/bmsy_true, b_proj_t[index[i],]/bmsy_true), lwd=2, col=gray(0.2))
            if(print_letters==TRUE) print.letter(label=paste0("(", letters[i+10], ")"), xy=c(0.05,0.92), cex=2, font=2, col="black", xpd=NA)
        }   

        for(i in 1:length(index)){
        	plot(x=1, y=1, xlim=c(1,35+nproject), ylim=c(0, 7), type="n", yaxt="n", xaxs="i", yaxs="i", cex.axis=axis_lab)
    		polygon(x=c(1:36, 36:1), y=c(rep(-1,36), rep(2000,36)), col="#AAAAAA50", border=NA)
      #   	polygon(x=c(1:35, 35:1), y=c(q_c_true[1,,i]/q_b_true[1,,i], rev(q_c_true[3,,i]/q_b_true[3,,i])), col="#0000FF50", border=NA)
    		# polygon(x=c(1:35, 35:1), y=c(q_c_rep[1,,i]/q_b_est[1,,i], rev(q_c_rep[3,,i]/q_b_est[3,,i])), col="#FF000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_c_rep[1,35,i]/q_b_est[1,35,i], q_c_proj[1,,i]/q_b_proj[1,,i], rev(q_c_proj[3,,i]/q_b_proj[3,,i]), q_c_rep[3,35,i]/q_b_est[3,35,i]), col="#FF000050", border=NA)
    		# polygon(x=c(35:(35+nproject), (35+nproject):35), y=c(q_c_true[1,35,i]/q_b_true[1,35,i], q_c_proj_t[1,,i]/q_b_proj_t[1,,i], rev(q_c_proj_t[3,,i]/q_b_proj_t[3,,i]), q_c_true[3,35,i]/q_b_true[3,35,i]), col="#0000FF50", border=NA)
        	abline(h=1, lty=2)
        	# abline(v=35, lty=2)
        	if(i==1){
                axis(2, at=pretty(c(0,7))[-length(pretty(c(0,7)))], las=2, cex.axis=axis_lab)
    			mtext(expression(italic(u/u[{MSY}])),  side=2, line=lab_line, cex=1.6)
    		}
            plotcol <- ifelse(i==1, gray(0.6), cols[i-1])
        	lines(x=1:35, y=((c_rep[1,index[i],]/b_est[1,index[i],])/(msy_est[1,index[i]]/bmsy_est[1,index[i]])), lwd=5, col=plotcol)
            # lines(x=35:(35+nproject), y=c((c_rep[1,index[i],35]/b_est[1,index[i],35])/(msy_est[1,index[i]]/bmsy_est[1,index[i]]), (c_proj[index[i],]/b_proj[index[i],])/(msy_est[1,index[i]]/bmsy_est[1,index[i]])), lwd=5, col=plotcol)
            lines(x=35:(35+nproject), y=c((c_rep[1,index[i],35]/b_est[1,index[i],35])/(msy_est[1,index[i]]/bmsy_est[1,index[i]]), (TAC[index[i],]/b_proj[index[i],])/(msy_est[1,index[i]]/bmsy_est[1,index[i]])), lwd=5, col=plotcol)
            lines(x=1:35, y=((c_true[1,index[i],]/b_true[1,index[i],])/(msy_true/bmsy_true)), lwd=2, col=gray(0.2))
        	lines(x=35:(35+nproject), y=c((c_true[1,index[i],35]/b_true[1,index[i],35])/(msy_true/bmsy_true), (c_proj_t[index[i],]/b_proj_t[index[i],])/(msy_true/bmsy_true)), col=gray(0.2), lwd=2)
            if(print_letters==TRUE) print.letter(label=paste0("(", letters[i+15], ")"), xy=c(0.05,0.92), cex=2, font=2, col="black", xpd=NA)
        }

    mtext("Year",  side=1, line=3, cex=1.6, outer=TRUE)
    
   

}