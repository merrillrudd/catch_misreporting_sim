createDat <- function(rmodel, bmodel, dir, ndatyrs, index, catch, z, report, start_sim){

		
		if(rmodel %in% c("allrep", "allunder", "allover")){
			catch_dat <- catch*report
		}
		if(rmodel == "repinc"){
			rep1 <- 0.2
			rep2 <- 0.8
			coef <- ((0.8-0.2)/(ndatyrs-1))*1:ndatyrs + 0.2
			catch_dat <- catch*coef
		}
		if(rmodel=="repdec"){
			rep1 <- 0.8
			rep2 <- 0.2
			coef <- ((0.2-0.8)/(ndatyrs-1))*1:ndatyrs + 0.8
			catch_dat <- catch*coef
		}
		if(rmodel=="inc_mean1"){
			rep1 <- 0.6
			rep2 <- 1.4
			coef <- ((rep2 - rep1)/(ndatyrs-1))*1:ndatyrs + rep1
			catch_dat <- catch*coef
		}
		if(rmodel=="dec_mean1"){
			rep1 <- 1.4
			rep2 <- 0.6
			coef <- ((rep2 - rep1)/(ndatyrs-1))*1:ndatyrs + rep1
			catch_dat <- catch*coef
		}
		if(rmodel=="repinc_v2"){
			rep1 <- 0.4
			rep2 <- 0.9
			coef <- ((rep2 - rep1)/(ndatyrs-1))*1:ndatyrs + rep1
			catch_dat <- catch*coef
		}
		if(rmodel=="repdec_v2"){
			rep1 <- 0.9
			rep2 <- 0.4
			coef <- ((rep2 - rep1)/(ndatyrs-1))*1:ndatyrs + rep1
			catch_dat <- catch*coef
		}
		
		startK <- ifelse(start_sim=="K", -1, 1)
		
		datfile <- paste0(dir,"\\", "pt_unreported.dat")
		write("# number of yrs of biomass index", file=datfile)
		write(ndatyrs, file=datfile, append=TRUE)
		write("# index data", file=datfile, append=TRUE)
		write(index, file=datfile, append=TRUE)
		write("# catch data", file=datfile, append=TRUE)
		write(catch_dat, file=datfile, append=TRUE)
		write("# z", file=datfile, append=TRUE)
		write(z, file=datfile, append=TRUE)
		write("# phase for initial depletion", file=datfile, append=TRUE)
		write(startK, file=datfile, append=TRUE)
		write("# eof", file=datfile, append=TRUE)
		write(999, file=datfile, append=TRUE)

}