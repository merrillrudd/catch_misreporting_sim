generateData <- function(bmodel, sigma, catch, itervec, seed){
	
  bmodel_full <- ifelse(grepl("startalt", bmodel), bmodel_vec_names[2], bmodel_vec_names[1])
  if(identical(catch, catch1)) catch_full <- "2way_catch"
  if(identical(catch, catch2)) catch_full <- "1way_catch"
  dir_start <- paste0(admb_dir, "\\", bmodel_full)
    if(file.exists(dir_start)==FALSE) dir.create(dir_start)
  dir_catch <- paste0(dir_start, "\\", catch_full)
    if(file.exists(dir_catch)==FALSE) dir.create(dir_catch)
  dir_sigma <- paste0(dir_catch, "\\sigma", sigma)
    if(file.exists(dir_sigma)==FALSE) dir.create(dir_sigma)
  dir_truth <- paste0(dir_sigma, "\\", "Truth")
    if(file.exists(dir_truth)==FALSE) dir.create(dir_truth)

  set.seed(seed)
  for(iter in itervec){
  	### get seed back on track to match correct iteration
  	if(itervec[1]!=1){
  		for(iter2 in 1:itervec[1]){
  			invisible <- runOM(bmodel=bmodel, r=r_true, K=K_true, z=z_true, q=q_true,
  				process_err=sigma, obs_err=sigma, datyrs=datyrs, example_catch=catch)
  		}
  	}

  	dir_iter <- paste0(dir_truth, "\\", iter)
     if(file.exists(dir_iter)) unlink(dir_iter,TRUE)
     if(file.exists(dir_iter)==FALSE) dir.create(dir_iter)

  	OM <- runOM(bmodel=bmodel, r=r_true, K=K_true, z=z_true, q=q_true,
  		process_err=sigma, obs_err=sigma, datyrs=datyrs, example_catch=catch)
  	saveRDS(OM, file.path(dir_iter, "TRUE.rds"))
  }

}