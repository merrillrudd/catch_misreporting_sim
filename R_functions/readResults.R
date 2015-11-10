readResults <- function(results_dir, itervec, nyrs=length(datyrs)){
	
	bmodel <- ifelse(grepl("Schaefer", results_dir), "s", "pt")
	rep_files <- paste0(results_dir, "\\", itervec, "\\", bmodel, "_unreported.rep")
		K_est <- r_est <- msy_est <- bmsy_est <- sigma_est <- nll <- vector(length=length(itervec))
		Bpred_est <- matrix(NA, nrow=nyrs, ncol=length(itervec))
		if(bmodel=="s"){
		  for(i in itervec){
			file <- rep_files[i]
			K_est[i] <- as.numeric(read.table(file, skip=1, nrows=1))
            r_est[i] <- as.numeric(read.table(file, skip=3, nrows=1))
            msy_est[i] <- as.numeric(read.table(file, skip=11, nrows=1))
            Bpred_est[,i] <- as.numeric(read.table(file, skip=13, nrows=1))
            nll[i] <- as.numeric(read.table(file, skip=15), nrows=1)
            sigma_est[i] <- as.numeric(read.table(file, skip=7, nrows=1))
            bmsy_est[i] <- as.numeric(read.table(file, skip=9, nrows=1))
			rm(file)
		  }
		}
		if(bmodel=="pt"){
			for(i in itervec){
			file <- rep_files[i]
			K_est[i] <- as.numeric(read.table(file, skip=1, nrows=1))
            r_est[i] <- NA
            msy_est[i] <- as.numeric(read.table(file, skip=9, nrows=1))
            Bpred_est[,i] <- as.numeric(read.table(file, skip=11, nrows=1))
            nll[i] <- as.numeric(read.table(file, skip=13), nrows=1)
            sigma_est[i] <- as.numeric(read.table(file, skip=5, nrows=1))
            bmsy_est[i] <- as.numeric(read.table(file, skip=7, nrows=1))
			rm(file)
		  }
		}
		

	
	Outs <- NULL
	Outs$K <- K_est
	Outs$r <- r_est
	Outs$msy <- msy_est
	Outs$Bpred <- Bpred_est
	Outs$nll <- nll
	Outs$sigma <- sigma_est
	Outs$bmsy <- bmsy_est
	return(Outs)  
}