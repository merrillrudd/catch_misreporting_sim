generateData <- function(bmodel, sigma, catch, fmort, itervec, seed, rinput, Kinput){
	
  bmodel_full <- ifelse(grepl("startalt", bmodel), bmodel_vec_names[2], bmodel_vec_names[1])
  if(is.null(catch)) if(identical(fmort, F_2way)) catch_full <- "2way_catch"
  if(is.null(catch))if(identical(fmort, F_1way)) catch_full <- "1way_catch"
  if(is.null(fmort)) if(identical(catch, catch1)) catch_full <- "1way_catch"
  if(is.null(fmort)) if(identical(catch, catch2)) catch_full <- "2way_catch"
  dir_start <- paste0(admb_dir, "\\", bmodel_full)
    if(file.exists(dir_start)==FALSE) dir.create(dir_start)
  dir_catch <- paste0(dir_start, "\\", catch_full)
    if(file.exists(dir_catch)==FALSE) dir.create(dir_catch)
  dir_combo <- paste0(dir_catch, "\\K", Kinput, "_r", rinput)
    if(file.exists(dir_combo)==FALSE) dir.create(dir_combo)
  dir_sigma <- paste0(dir_combo, "\\sigma", sigma)
    if(file.exists(dir_sigma)==FALSE) dir.create(dir_sigma)
  dir_truth <- paste0(dir_sigma, "\\", "Truth")
    if(file.exists(dir_truth)==FALSE) dir.create(dir_truth)

  set.seed(seed)
  for(iter in itervec){

  	dir_iter <- paste0(dir_truth, "\\", iter)
     if(file.exists(dir_iter)) unlink(dir_iter,TRUE)
     if(file.exists(dir_iter)==FALSE) dir.create(dir_iter)

  	OM <- runOM(bmodel=bmodel, r=rinput, K=Kinput, z=z_true, q=q_true,
  		process_err=sigma, obs_err=sigma, datyrs=datyrs, example_catch=catch, Fpattern=fmort)
  	saveRDS(OM, file.path(dir_iter, "TRUE.rds"))
  }

}