runEM <- function(rmodel, bmodel, itervec, datyrs, start_sim,
  report, sigma, re, checknll, catch, exe, rinput, Kinput){

  bmodel_full <- ifelse(grepl("startalt", bmodel), bmodel_vec_names[2], bmodel_vec_names[1])
  if(identical(catch, catch1)) catch_full <- "2way_catch"
  if(identical(catch, catch2)) catch_full <- "1way_catch"
  dir_start <- paste0(admb_dir, "\\", bmodel_full)
    if(file.exists(dir_start)==FALSE) dir.create(dir_start)
  dir_catch <- paste0(dir_start, "\\", catch_full)
    if(file.exists(dir_catch)==FALSE) dir.create(dir_catch)
  dir_combo <- paste0(dir_catch, "\\K", Kinput, "_r", rinput)
    if(file.exists(dir_combo)==FALSE) dir.create(dir_combo)
  dir_sigma <- paste0(dir_combo, "\\sigma", sigma)
    if(file.exists(dir_sigma)==FALSE) dir.create(dir_sigma)

  dir_rmod <- paste0(dir_sigma, "\\", rmodel)
    if(file.exists(dir_rmod)==FALSE) dir.create(dir_rmod)

  dir_truth <- paste0(dir_sigma, "\\", "Truth")
  
  ### write data files
  for(iter in itervec){

    dir_runiter <- paste0(dir_rmod, "\\", iter)
     if(file.exists(dir_runiter)) unlink(dir_runiter,TRUE)
     if(file.exists(dir_runiter)==FALSE) dir.create(dir_runiter)

    dir_datiter <- file.path(dir_truth, iter)
    if(file.exists(dir_datiter)==FALSE) stop("True population not generated - run generateData.R")

    OM <- readRDS(file.path(dir_datiter, "TRUE.rds"))
    index_dat <- OM$Index
    catch_dat <- OM$Catch
    createDat(bmodel=bmodel, rmodel=rmodel, dir=dir_runiter, 
      ndatyrs=datyrs, index=index_dat, z=z_true,
      catch=catch_dat, report=report, start_sim=start_sim)

  }

  set.seed(456)
  #invisible <- runif(150)
  for(iter in itervec){
    nll_vec <- vector(length=checknll)
    checkstd <- rep(0, checknll)
    dir_runiter <- paste0(dir_rmod, "\\", iter)
    for(iter2 in 1:checknll){

      dir_iter2 <- paste0(dir_runiter, "\\", iter2)
        if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
        if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)

      createPin(dir=dir_iter2, re=re)
      
      if(getwd() != dir_iter2){setwd(dir_iter2)}
      file.copy(exe, dir_iter2, over=TRUE)
      file.copy(paste0(dir_runiter, "\\", "pt_unreported.dat"), dir_iter2, over=TRUE)
      try(shell("pt_unreported"))

      if(file.exists(paste0(dir_iter2, "\\", "pt_unreported.std"))==FALSE){
        checkstd[iter2] <- 1
      }

      if(grepl("pt", bmodel)) nll_vec[iter2] <- as.numeric(unlist(read.table("pt_unreported.rep", skip=13, nrows=1)))
    } #end dir_iter2

    lmin <- length(which(round(nll_vec,2)==min(round(nll_vec,2))))

    x <- 0
    while(sum(checkstd) != 0){
      rerun_vec <- which(checkstd==1)
      for(rerun in rerun_vec){
        dir_iter2 <- paste0(dir_runiter, "\\", rerun)
        if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
        if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)

        createPin(dir=dir_iter2, re=re)

        if(getwd() != dir_iter2){setwd(dir_iter2)}
        file.copy(exe, dir_iter2, over=TRUE)
        file.copy(paste0(dir_runiter, "\\", "pt_unreported.dat"), dir_iter2, over=TRUE)
        try(shell("pt_unreported"))

        if(file.exists(paste0(dir_iter2, "\\", "pt_unreported.std"))==FALSE){
          checkstd[rerun] <- 1
        }
        if(file.exists(paste0(dir_iter2, "\\", "pt_unreported.std"))==TRUE){
          checkstd[rerun] <- 0
        }

        if(grepl("pt", bmodel)) nll_vec[rerun] <- as.numeric(unlist(read.table("pt_unreported.rep", skip=13, nrows=1)))
      } # end rerun
      x <- x+1
      lmin <- length(which(round(nll_vec,2)==min(round(nll_vec,2), na.rm=TRUE)))
      if(x>=10 & lmin>1) checkstd <- 0
      if(x>=10 & lmin <= 1) invisible <- rnorm(100,0,1)
      if(x>=50 & lmin <= 1){
        stop("Potential non-convergence")
      }
    } # end while function

    findmin <- which(nll_vec==min(nll_vec))[1]
    dir_findmin <- paste0(dir_runiter, "\\", findmin)
    file.copy(paste0(dir_findmin, "\\", "pt_unreported.std"), dir_runiter, over=TRUE)
    file.copy(paste0(dir_findmin, "\\", "pt_unreported.rep"), dir_runiter, over=TRUE)
    file.copy(paste0(dir_findmin, "\\", "pt_unreported.pin"), dir_runiter, over=TRUE)

    setwd(init_dir)
    unlink(paste0(dir_runiter, "\\", 1:checknll), TRUE)
  }
 


on.exit(setwd(init_dir))
}

# reiter <- function(niter, bmodel, rmodel, dir, datyrs, 
#   report, sigma, adjyrs, checknll, re, catch, exe){
  
#     dir_iter <- paste0(dir, "\\", niter)
#      if(file.exists(dir_iter)) unlink(dir_iter,TRUE)
#      if(file.exists(dir_iter)==FALSE) dir.create(dir_iter)

#     if(bmodel=="s"){
#       rOM <- r_true
#       zOM <- NULL
#       MSYOM <- NULL
#     }
#     if(bmodel=="pt"){
#       rOM <- NULL
#       zOM <- z_true
#       MSYOM <- msy_true
#     }
#     OM <- runOM(bmodel=bmodel, r=rOM, K=K_true, 
#       z=zOM, MSY=MSYOM, q=q_true, 
#       process_err=sigma, obs_err=sigma, 
#       datyrs=datyrs, example_catch=catch)
#     saveRDS(OM, paste0(dir_iter, "\\TRUE.rds"))
#     index_dat <- OM$Index
#     catch_dat <- OM$Catch
#     createDat(bmodel=bmodel, rmodel=rmodel, dir=dir_iter, 
#       ndatyrs=length(datyrs), index=index_dat, 
#       catch=catch_dat, report=report, adjyrs=adjyrs)

#     nll_vec <- vector(length=checknll)
#     checkstd <- rep(0, checknll)
#     for(iter2 in 1:checknll){

#       dir_iter2 <- paste0(dir_iter, "\\", iter2)
#         if(file.exists(dir_iter2)) unlink(dir_iter2, TRUE)
#         if(file.exists(dir_iter2)==FALSE) dir.create(dir_iter2)

#       seed <- round(runif(1, 1, 10000000))
#         seedfile <- paste0(dir_iter2, "\\seed.txt")
#         write("# random seed", file=seedfile)
#         write(seed, file=seedfile, append=TRUE)
#       createPin(bmodel=bmodel, dir=dir_iter2, re=re, seed=seed)
      
#       if(getwd() != dir_iter2){setwd(dir_iter2)}
#       file.copy(exe, dir_iter2, over=TRUE)
#       file.copy(paste0(dir_iter, "\\", bmodel, "_unreported.dat"), dir_iter2, over=TRUE)
#       shell(paste0(bmodel, "_unreported"))

#       if(file.exists(paste0(dir_iter2, "\\", bmodel, "_unreported.std"))==FALSE){
#         checkstd[iter2] <- 1
#       }

#       if(bmodel=="s") nll_vec[iter2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=15, nrows=1)))
#       if(bmodel=="pt") nll_vec[iter2] <- as.numeric(unlist(read.table(paste0(bmodel, "_unreported.rep"), skip=13, nrows=1)))
#     }

#     Outs <- NULL
#     Outs$checkstd <- checkstd
#     Outs$nll_vec <- nll_vec
#     return(Outs)  
# }