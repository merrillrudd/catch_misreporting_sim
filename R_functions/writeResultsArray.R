writeResultsArray <- function(iter, datyrs, dirs, res_dir){

	res_best <- res_btrue <- res_ctrue <- res_crep <- array(NA, dim=c(length(iter), datyrs, length(dirs)))
	res_msy <- res_bmsy <- res_K <- matrix(NA, nrow=length(iter), ncol=length(dirs))

	message("Reading results files")
	for(i in 1:length(iter)){
	    for(d in 1:length(dirs)){
    		xdir <- file.path(dirs[d], i) # find iteration & terminal 35 year dataset within directory
    		temp_true <- readRDS(file.path(xdir, "TRUE.rds"))
    		res_btrue[i,,d] <- temp_true$Biomass
    		res_ctrue[i,,d] <- temp_true$Catch

        term_dir <- file.path(xdir, "A35")
    		temp_nyrs <- as.numeric(read.table(file.path(term_dir, "pt_unreported.dat"), skip=1, nrows=1))
    		temp_crows <- ifelse(temp_nyrs %% 5 == 0, temp_nyrs/5, temp_nyrs/5+1)
    		temp_crep <- scan(file.path(term_dir, "pt_unreported.dat"), skip=11, nlines=temp_crows, multi.line=TRUE, comment.char="#")
    		temp_best <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=11, nrows=1))
    		temp_msy <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=9, nrows=1))
    		temp_bmsy <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=7, nrows=1))
    		temp_K <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=1, nrows=1))    

    		res_crep[i,,d] <- temp_crep
    		res_best[i,,d] <- temp_best
    		res_bmsy[i,d] <- temp_bmsy
    		res_msy[i,d] <- temp_msy
    		res_K[i,d] <- temp_K
    	}
    }

    Outs <- NULL
    Outs$b_est <- res_best
    Outs$b_true <- res_btrue
    Outs$c_rep <- res_crep
    Outs$c_true <- res_ctrue

    Outs$msy_est <- res_msy
    Outs$bmsy_est <- res_bmsy
    Outs$K_est <- res_K

    if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))) save_file <- "allresults.rds"
    if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))==FALSE) save_file <- "1way-results.rds"
    if(any(grepl("1way_catch", dirs))==FALSE & any(grepl("2way_catch", dirs))) save_file <- "2way-results.rds"

    saveRDS(Outs, file.path(res_dir, save_file))
    return(Outs)

}