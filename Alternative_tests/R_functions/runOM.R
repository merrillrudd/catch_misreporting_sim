runOM <- function(bmodel, K, q, r, z, process_err, obs_err, datyrs, 
	example_catch=NULL, Fpattern=NULL){

	totalyrs <- 1:datyrs

	if(is.null(example_catch)){
		Ct <- vector(length=length(totalyrs))
		Ft <- Fpattern
	}
	if(is.null(example_catch)==FALSE){
		Ct <- example_catch
		Ft <- vector(length=length(totalyrs))
	}

	check <- 0
	while(check==0){
		eps_proc <- rnorm(length(totalyrs), 0, process_err)
		Bt <- vector(length=length(totalyrs))
		
		MSY <- (r*K*z)/((z+1)^(1/z+1))	

		if(grepl("startalt", bmodel)) Bt[1] <- 0.75*K
		if(grepl("startalt", bmodel)==FALSE) Bt[1] <- K
		if(is.null(Fpattern)==FALSE) Ct[1] <- Ft[1] * Bt[1]
		if(is.null(example_catch)==FALSE) Ft[1] <- Ct[1]/Bt[1]
		
		for(t in 2:length(totalyrs)){
			if(bmodel=="s"){Bt[t] <- max(1, (Bt[t-1] + r*Bt[t-1]*(1-Bt[t-1]/K) - Ct[t-1])*exp(eps_proc[t]-(process_err^2)/2))}
			if(grepl("pt", bmodel)){Bt[t] <- max(1, (Bt[t-1] + ((z^(z/(z-1)))/(z-1))*MSY*((Bt[t-1]/K)-(Bt[t-1]/K)^z) - Ct[t-1])*exp(eps_proc[t]-(process_err^2)/2))}
			if(is.null(Fpattern)==FALSE) Ct[t] <- Ft[t] * Bt[t]
			if(is.null(example_catch)==FALSE) Ft[t] <- Ct[t]/Bt[t]
		}
		if(min(Bt) > 1){check <- 1}
		if(min(Bt)==1){check <- 0}
	}
	
	# plot(Bt, type="l")
	# par(new=TRUE)
	# plot(Ct, pch=19, col="blue", yaxt="n")

	# plot(Bt, type="l", ylim=c(min(Bt),K*1.5))
	# points(x=0, y=K, cex=2)

	eps_obs <- rnorm(length(totalyrs), 0, obs_err)
	It <- vector(length=length(totalyrs))
	for(t in 1:length(totalyrs)){
		It[t] <- max(0, Bt[t]*q*exp(eps_obs[t]-(obs_err^2)/2))
	}

	if(grepl("pt", bmodel)) BMSY <- 0.4*K

	Outs <- NULL
	Outs$Index <- It
	Outs$Catch <- Ct
	Outs$Biomass <- Bt
	Outs$F <- Ft
	Outs$MSY <- MSY
	Outs$BMSY <- BMSY
	Outs$K <- K
	Outs$r <- r
	return(Outs)
}