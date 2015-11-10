retroEM <- function(rmodel, bmodel, itervec, datyrs,
	report, sigma, re, checknll, catch, exe){

	bmodel_full <- ifelse(bmodel=="s", "Schaefer", "Pella-Tomlinson")
	retro_dir <- file.path(admb_dir, "Retrospective")
    if(file.exists(retro_dir)==FALSE) dir.create(retro_dir)
  if(identical(catch, catch1)) catch_full <- "2way_catch"
  if(identical(catch, catch2)) catch_full <- "1way_catch"
  dir_catch <- paste0(retro_dir, "\\", catch_full)
    if(file.exists(dir_catch)==FALSE) dir.create(dir_catch)
	dir_bmod <- file.path(dir_catch, bmodel_full)
    if(file.exists(dir_bmod)==FALSE) dir.create(dir_bmod)
  dir_rmod <- paste0(dir_bmod, "\\", rmodel)
    if(file.exists(dir_rmod)==FALSE) dir.create(dir_rmod)
  dir_sigma <- paste0(dir_rmod, "\\sigma", sigma)
    if(file.exists(dir_sigma)==FALSE) dir.create(dir_sigma)
    	

 	for(iter in itervec){
 		dir_iter <- file.path(dir_sigma, iter)
 		if(file.exists(dir_iter)) unlink(dir_iter, TRUE)
 		if(file.exists(dir_iter)==FALSE) dir.create(dir_iter)

 		    if(bmodel=="s"){
          rOM <- r_init
          zOM <- NULL
          MSYOM <- NULL
        }
        if(bmodel=="pt"){
          rOM <- NULL
          zOM <- z_init
          MSYOM <- MSY_init
        }	
        if(rmodel=="allunder_changeq") changeq <- TRUE
        if(rmodel!="allunder_changeq") changeq <- FALSE
        OM <- runOM(bmodel=bmodel, r=rOM, K=K_init, 
          z=zOM, q=q_init, 
          process_err=sigma, obs_err=sigma, 
          datyrs=datyrs, example_catch=catch)
        saveRDS(OM, paste0(dir_iter, "\\TRUE.rds"))
        assess_seq <- rev(seq(15, datyrs, by=10))
        for(a in 1:length(assess_seq)){
        	a_dir <- file.path(dir_iter, paste0("A",assess_seq[a]))
        	if(file.exists(a_dir)==FALSE) dir.create(a_dir)
        	index_dat <- OM$Index[1:assess_seq[a]]
        	catch_dat <- OM$Catch[1:assess_seq[a]]
        	createDat(bmodel=bmodel, rmodel=rmodel, dir=a_dir,
        		ndatyrs=assess_seq[a], index=index_dat, catch=catch_dat,
        		z=zOM, report=report)

        	nll_vec <- vector(length=checknll)
        	msy_vec <- vector(length=checknll)
          checkstd <- rep(0, checknll)
          checkbound <- rep(0, checknll)
          for(iter2 in 1:checknll){        

              dir_iter2 <- paste0(a_dir, "\\", iter2)
                if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
                if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)

            seed <- round(runif(1, 1, 10000000))
        	    seedfile <- paste0(dir_iter2, "\\seed.txt")
	            write("# random seed", file=seedfile)
	            write(seed, file=seedfile, append=TRUE)
	          createPin(bmodel=bmodel, dir=dir_iter2, re=re, seed=seed, lh=lh)
      			
      		  if(getwd() != dir_iter2){setwd(dir_iter2)}
              file.copy(exe, dir_iter2, over=TRUE)
              file.copy(paste0(a_dir, "\\", bmodel, "_unreported.dat"), dir_iter2, over=TRUE)
              shell(paste0(bmodel, "_unreported"))  

            if(file.exists(paste0(dir_iter2, "\\", bmodel, "_unreported.std"))==FALSE){
          	   checkstd[iter2] <- 1
        		}  

        		if(bmodel=="s") nll_vec[iter2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=15, nrows=1)))
      		  if(bmodel=="pt") nll_vec[iter2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=13, nrows=1)))
      		  if(bmodel=="pt") msy_vec[iter2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=9, nrows=1)))    	
		      } # end dir_iter2

			x <- 0
	    while(sum(checkstd) > 0){
      	rerun_vec <- which(checkstd==1)
      	for(rerun in rerun_vec){
      		dir_iter2 <- paste0(a_dir, "\\", rerun)
            if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
            if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)
  		
  		    seed <- round(runif(1, 1, 10000000))
           	seedfile <- paste0(dir_iter2, "\\seed.txt")
            write("# random seed", file=seedfile)
            write(seed, file=seedfile, append=TRUE)
          createPin(bmodel=bmodel, dir=dir_iter2, re=re, seed=seed, lh=lh)
   				
   				if(getwd() != dir_iter2){setwd(dir_iter2)}
        	file.copy(exe, dir_iter2, over=TRUE)
          file.copy(paste0(a_dir, "\\", bmodel, "_unreported.dat"), dir_iter2, over=TRUE)
          shell(paste0(bmodel, "_unreported"))           

          if(file.exists(paste0(dir_iter2, "\\", bmodel, "_unreported.std"))==FALSE){
     	   		   checkstd[rerun] <- 1
     	   	}   	   		
          if(bmodel=="s") nll_vec[rerun] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=15, nrows=1)))
 		  		if(bmodel=="pt") nll_vec[rerun] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=13, nrows=1)))
          if(bmodel=="pt") msy_vec[rerun] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=9, nrows=1)))    			
        } # end rerun
        x <- x+1
        lmin <- length(which(round(nll_vec,2)==min(round(nll_vec,2))))
   		  if(x>=10 & lmin > 1) checkstd <- 0
   		} # end while std function
   		
      if(bmodel=="pt"){
       y <- 0
   		while(all(round(msy_vec,2)==round(exp(0),2))){
   		   for(rerun2 in 1:checknll){
   		      dir_iter2 <- paste0(a_dir, "\\", rerun2)
            if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
            if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)
  		
  		    seed <- round(runif(1, 1, 10000000))
           	seedfile <- paste0(dir_iter2, "\\seed.txt")
            write("# random seed", file=seedfile)
            write(seed, file=seedfile, append=TRUE)
          createPin(bmodel=bmodel, dir=dir_iter2, re=re, seed=seed, lh=lh)
   				
   				if(getwd() != dir_iter2){setwd(dir_iter2)}
        	file.copy(exe, dir_iter2, over=TRUE)
          file.copy(paste0(a_dir, "\\", bmodel, "_unreported.dat"), dir_iter2, over=TRUE)
          shell(paste0(bmodel, "_unreported"))           

          if(file.exists(paste0(dir_iter2, "\\", bmodel, "_unreported.std"))==FALSE){
     	   		   checkstd[rerun2] <- 1
     	   	}   	   		
          if(bmodel=="s") nll_vec[rerun2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=15, nrows=1)))
 		  		if(bmodel=="pt") nll_vec[rerun2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=13, nrows=1)))
          if(bmodel=="pt") msy_vec[rerun2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=9, nrows=1)))   
        } # end rerun 2
        y <- y+1
        if(y >= 3) msy_vec <- 0
        } # end while checkmsy function
       }
	        	
      findmin <- which(nll_vec==min(nll_vec))[1]
      dir_findmin <- paste0(a_dir, "\\", findmin)
      file.copy(paste0(dir_findmin, "\\", bmodel, "_unreported.std"), a_dir, over=TRUE)
      file.copy(paste0(dir_findmin, "\\", bmodel, "_unreported.rep"), a_dir, over=TRUE)
      file.copy(paste0(dir_findmin, "\\", bmodel, "_unreported.pin"), a_dir, over=TRUE)
      
      setwd(init_dir)
      unlink(paste0(a_dir, "\\", 1:checknll), TRUE) 
            
  	} # end assessment year
  } # end iter

    on.exit(setwd(init_dir))
} # end function

