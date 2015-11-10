writeTerminalResults <- function(iter, datyrs, dirs, res_dir){

	res_best <- btrue <- ctrue <- res_crep <- array(NA, dim=c(length(iter), length(dirs), datyrs))
	res_msy <- res_bmsy <- res_K <- bmsytrue <- msytrue <- Ktrue <- rtrue <- matrix(NA, nrow=length(iter), ncol=length(dirs))
  
	message("Reading results files")
	for(i in 1:length(iter)){
	    for(d in 1:length(dirs)){

            if(i>1 & grepl("0.001", dirs[d])) next
    		xdir <- file.path(dirs[d], i) # find iteration & terminal 35 year dataset within directory
    		setwd(xdir)
    		setwd("../")
    		setwd("../")
    		temp_true <- readRDS(file.path(getwd(), "Truth", i, "TRUE.rds"))
    		setwd(init_dir)
    		btrue[i,d,] <- temp_true$Biomass
    		ctrue[i,d,] <- temp_true$Catch
            msytrue[i,d] <- temp_true$MSY
            bmsytrue[i,d] <- temp_true$BMSY
            Ktrue[i,d] <- temp_true$K
            rtrue[i,d] <- temp_true$r


        term_dir <- xdir
    		temp_nyrs <- as.numeric(read.table(file.path(term_dir, "pt_unreported.dat"), skip=1, nrows=1))
    		temp_crows <- ifelse(temp_nyrs %% 5 == 0, temp_nyrs/5, temp_nyrs/5+1)
    		temp_crep <- scan(file.path(term_dir, "pt_unreported.dat"), skip=11, nlines=temp_crows, multi.line=TRUE, comment.char="#")
    		temp_best <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=11, nrows=1))
    		temp_msy <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=9, nrows=1))
    		temp_bmsy <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=7, nrows=1))
    		temp_K <- as.numeric(read.table(file.path(term_dir, "pt_unreported.rep"), skip=1, nrows=1))    

    		res_crep[i,d,] <- temp_crep
    		res_best[i,d,] <- temp_best
    		res_bmsy[i,d] <- temp_bmsy
    		res_msy[i,d] <- temp_msy
    		res_K[i,d] <- temp_K
    	}
    }

    Outs <- NULL
    Outs$b_est <- res_best
    Outs$b_true <- btrue
    Outs$c_rep <- res_crep
    Outs$c_true <- ctrue

    Outs$msy_est <- res_msy
    Outs$bmsy_est <- res_bmsy
    Outs$K_est <- res_K
    Outs$msy_true <- msytrue
    Outs$bmsy_true <- bmsytrue
    Outs$K_true <- Ktrue
    Outs$r_true <- rtrue

    #if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))) save_file <- "allresults-terminal.rds"
    #if(any(grepl("1way_catch", dirs)) & any(grepl("2way_catch", dirs))==FALSE) save_file <- "1way-results-terminal.rds"
    if(any(grepl("1way_catch", dirs))==FALSE & any(grepl("2way_catch", dirs))){
      if(all(dirs==dirs2_startalt)) save_file <- "2way-results-terminal-startalt.rds"
      if(all(dirs==dirs2_startK)) save_file <- "2way-results-terminal-startK.rds"
    }

    saveRDS(Outs, file.path(res_dir, save_file))
    return(Outs)

}