projectResults <- function(dirs, itervec, bmsy_factor, nproject){


    results_table <- matrix(NA, nrow=length(dirs), ncol=6)
              rownames(results_table) <- dirs
              colnames(results_table) <- c("True Crashes", "Projected Greater than True", "Similar and Stable", 
                                            "True Greater than Projected", 
                                            "Projected Crashes", "True and Projected Crash")

        bproj <- bproj_true <- rel_err <- array(NA, dim=c(length(datyrs)+nproject, length(itervec), length(dirs)))
        r_est <- K_est <- msy_deriv <- bmsy_deriv <-  emsy_deriv <- sigma_deriv <- nll <- matrix(0, nrow=length(itervec), ncol=length(dirs))    
        both_crash <- trueb_crash <- projb_crash <- truegexpected <- expectedgtrue <- similarstable <- matrix(0, nrow=length(itervec), ncol=length(dirs))    
        pred_catch <- true_catch <- array(NA, dim=c(length(datyrs)+nproject, length(itervec), length(dirs)))
        report_rate <- matrix(NA, nrow=(length(datyrs)+nproject), ncol=length(dirs))

    for(mod in 1:length(dirs)){
    	if(mod %% 5 ==0){print(mod)}
      bmodel <- ifelse(grepl("Schaefer", dirs[mod]), "s", "pt")
    	res <- readResults(results_dir=dirs[mod], itervec=itervec)
    	
        if(mod %in% grep("sigma0.001", dirs)){sigma <- 0.001}
        if(mod %in% grep("sigma0.1", dirs)){sigma <- 0.1}
        if(mod %in% grep("sigma0.3", dirs)){sigma <- 0.3}
        if(mod %in% grep("sigma0.5", dirs)){sigma <- 0.5}

        if(grepl("allrep", dirs[mod])) {report_rate[,mod] <- rep(1,length=(length(datyrs)+nproject))}
        if(grepl("allunder", dirs[mod])) {report_rate[,mod] <- rep(0.5, length=(length(datyrs)+nproject))}
        if(grepl("allover", dirs[mod])) {report_rate[,mod] <- rep(1.5, length=(length(datyrs)+nproject))}
        if(grepl("repinc", dirs[mod])){
            rep1 <- 0.2
            rep2 <- 0.8
            coef <- seq(rep1, rep2, length=length(datyrs))
            slope <- (rep2 - rep1)/length(datyrs)
            project_coef <- rep(rep2, nproject)

            report_rate[,mod] <- c(coef, project_coef)
        }

        if(mod %in% grep("repdec", dirs)){
            rep1 <- 0.8
            rep2 <- 0.2
            coef <- seq(rep1, rep2, length=length(datyrs))
            slope <- (rep2 - rep1)/length(datyrs)
            project_coef <- rep(rep2, nproject)

            report_rate[,mod] <- c(coef, project_coef)
        }


        for(i in itervec){
        	true <- readRDS(paste0(dirs[mod], "\\", itervec[i], "\\TRUE.rds"))
        	r_est[i,mod] <- res$r[i]
        	K_est[i,mod] <- res$K[i]
        	msy_deriv[i,mod] <- res$msy[i]
            nll[i,mod] <- res$nll[i]
            sigma_deriv[i,mod] <- res$sigma[i]
            bmsy_deriv[i,mod] <- res$bmsy[i]
        	Bpred_est <- res$Bpred[,i]   
            emsy_deriv[i,mod] <- msy_deriv[i,mod]/(bmsy_deriv[i,mod]*bmsy_factor)

            true_catch[1:length(Bpred_est),i,mod] <- true$Catch
            datfile <- paste0(dirs[mod], "\\", itervec[i], "\\", bmodel, "_unreported.dat")
            pred_catch[1:length(Bpred_est),i,mod] <- scan(datfile, skip=11, nlines=7, multi.line=TRUE, comment.char="#")

            eps_project <- rnorm(nrow(bproj), 0, sigma)
            for(y in 1:nrow(bproj)){
              if(y <= length(true$Biomass)){
                bproj[y,i, mod] <- Bpred_est[y]
                bproj_true[y,i, mod] <- true$Biomass[y]
                rel_err[y,i, mod] <- (bproj[y,i,mod]-bproj_true[y,i, mod])/bproj_true[y,i, mod]
              }
              if(bmodel=="s"){
                if(y > length(true$Biomass)){
                  bproj[y,i,mod] <- max(1, bproj[y-1,i,mod] + r_est[i,mod]*bproj[y-1,i,mod]*(1-bproj[y-1,i,mod]/K_est[i,mod]) - pred_catch[y-1,i,mod])
                  bproj_true[y,i,mod] <- max(1, (bproj_true[y-1,i,mod] + r_true*bproj_true[y-1,i,mod]*(1-bproj_true[y-1,i,mod]/K_true) - true_catch[y-1,i,mod])*exp(eps_project[y]))
                  rel_err[y,i,mod] <- (bproj[y,i,mod]-bproj_true[y,i,mod])/bproj_true[y,i,mod]
                    pred_catch[y,i,mod] <- bproj[y,i,mod]*emsy_deriv[i,mod]
                    true_catch[y,i,mod] <- pred_catch[y,i,mod]/report_rate[y,mod]
                }  
              }
              if(bmodel=="pt"){
                if(y > length(true$Biomass)){
                  bproj[y,i,mod] <- max(1, bproj[y-1,i,mod] + ((2^(2/(2-1)))/(2-1))*msy_deriv[i,mod]*((bproj[y-1,i,mod]/K_est[i,mod])-(bproj[y-1,i,mod]/K_est[i,mod])^2) - pred_catch[y-1,i,mod])
                  bproj_true[y,i,mod] <- max(1, (bproj_true[y-1,i,mod] + ((2^(2/(2-1)))/(2-1))*MSY_true*((bproj_true[y-1,i,mod]/K_true)-(bproj_true[y-1,i,mod]/K_true)^2) - true_catch[y-1,i,mod])*exp(eps_project[y]))
                  rel_err[y,i,mod] <- (bproj[y,i,mod]-bproj_true[y,i,mod])/bproj_true[y,i,mod]
                    pred_catch[y,i,mod] <- bproj[y,i,mod]*emsy_deriv[i,mod]
                    true_catch[y,i,mod] <- pred_catch[y,i,mod]/report_rate[y,mod]
                }  
              }
          }

        	
            if(min(bproj[,i,mod])==1 & min(bproj_true[,i,mod])!=1){projb_crash[i,mod] <- 1}
            if(min(bproj[,i,mod])!=1 & min(bproj_true[,i,mod])==1){trueb_crash[i,mod] <- 1}
            if(min(bproj[,i,mod])==1 & min(bproj_true[,i,mod])==1){both_crash[i,mod] <- 1}
            if(min(bproj[,i,mod])!=1 & min(bproj_true[,i,mod])!=1 & mean(rel_err[,i,mod]) < -0.05){truegexpected[i,mod] <- 1}
            if(min(bproj[,i,mod])!=1 & min(bproj_true[,i,mod])!=1 & mean(rel_err[,i,mod]) > 0.05){expectedgtrue[i,mod] <- 1}
            if(min(bproj[,i,mod])!=1 & min(bproj_true[,i,mod])!=1 & mean(rel_err[,i,mod]) < 0.05 & mean(rel_err[,i,mod]) > -0.05){similarstable[i,mod] <- 1}
        
        }

        results_table[mod,1] <- sum(trueb_crash[,mod])/length(itervec)
        results_table[mod,2] <- sum(expectedgtrue[,mod])/length(itervec)
        results_table[mod,3] <- sum(similarstable[,mod])/length(itervec)
        results_table[mod,4] <- sum(truegexpected[,mod])/length(itervec)
        results_table[mod,5] <- sum(projb_crash[,mod])/length(itervec)
        results_table[mod,6] <- sum(both_crash[,mod])/length(itervec)
        
    }

    
    Outs <- NULL
    Outs$Expected <- bproj
    Outs$True <- bproj_true
    Outs$RelError <- rel_err
    Outs$PredCatch <- pred_catch
    Outs$TrueCatch <- true_catch
    Outs$r_estimates <- r_est
    Outs$K_estimates <- K_est
    Outs$msy_deriv <- msy_deriv
    Outs$bmsy_deriv <- bmsy_deriv
    Outs$emsy_deriv <- emsy_deriv
    Outs$sigma_deriv <- sigma_deriv
    Outs$Table <- results_table
    Outs$ReportRate <- report_rate

    return(Outs)
}