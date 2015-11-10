writeRetrospectiveResults <- function(iter, datyrs, dirs, res_dir){
	
	assess_seq <- seq(15, datyrs, by=10)
	MSY_true <- bmsy_true <- r_true <- K_true <- res_msy <- res_K <- res_best <- res_bmsy <- res_btrue <- res_ctrue <- res_crep <- array(NA, dim=c(length(iter), length(dirs), length(assess_seq)))



	for(d in 1:length(dirs)){
		for(i in 1:length(iter)){
			for(a in 1:length(assess_seq)){
				xdir <- file.path(dirs[d], i)
			    temp_true <- readRDS(file.path(xdir, "TRUE.rds"))
    			time_btrue <- temp_true$Biomass
    			time_ctrue <- temp_true$Catch
    			res_btrue[i,d,a] <- time_btrue[assess_seq[a]]
    			res_ctrue[i,d,a] <- time_ctrue[assess_seq[a]]
    			MSY_true[i,d,a] <- temp_true$MSY
    			bmsy_true[i,d,a] <- temp_true$BMSY
    			r_true[i,d,a] <- temp_true$r
    			K_true[i,d,a] <- temp_true$K

				adir <- file.path(dirs[d], i, paste0("A", assess_seq[a]))
				res_msy[i,d,a] <- as.numeric(read.table(file.path(adir, "pt_unreported.rep"), skip=9, nrows=1))
				res_K[i,d,a] <- as.numeric(read.table(file.path(adir, "pt_unreported.rep"), skip=1, nrows=1))
				time_best <- as.numeric(read.table(file.path(adir, "pt_unreported.rep"), skip=11, nrows=1))
				res_best[i,d,a] <- time_best[length(time_best)]
				res_bmsy[i,d,a] <- as.numeric(read.table(file.path(adir, "pt_unreported.rep"), skip=7, nrows=1))
				
				temp_nyrs <- as.numeric(read.table(file.path(adir, "pt_unreported.dat"), skip=1, nrows=1))
				temp_crows <- ifelse(temp_nyrs %% 5 == 0, temp_nyrs/5, temp_nyrs/5+1)
				temp_cskip <- 4 + temp_crows
				time_crep <- scan(file.path(adir, "pt_unreported.dat"), skip=temp_cskip, nlines=temp_crows, multi.line=TRUE, comment.char="#")
				res_crep[i,d,a] <- time_crep[length(time_crep)]
			}
		}
	}

	Outs <- NULL
	Outs$msy_est <- res_msy
	Outs$K_est <- res_K
	Outs$b_est <- res_best
	Outs$b_true <- res_btrue
	Outs$bmsy_est <- res_bmsy
	Outs$c_true <- res_ctrue
	Outs$c_rep <- res_crep
	Outs$bmsy_true <- bmsy_true
	Outs$msy_true <- MSY_true
	Outs$r_true <- r_true
	Outs$K_true <- K_true

	if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))) save_file <- "allresults-retrospective.rds"
    if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))==FALSE) save_file <- "1way-results-retrospective.rds"
    if(any(grepl("1way_catch", dirs))==FALSE & any(grepl("2way_catch", dirs))) save_file <- "2way-results-retrospective.rds"

    saveRDS(Outs, file.path(res_dir, save_file))
    return(Outs)

}