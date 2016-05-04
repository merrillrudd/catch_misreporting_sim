retroResults <- function(dirs){
	assess_seq <- seq(5,length(datyrs), by=5)
    a_dirvec <- paste0("A", 1:length(assess_seq))
    results <- list() 
      for(i in 1:length(dirs)){
        xdir <- dirs[i]
        setwd(xdir)
        rep <- as.vector(unlist(sapply(a_dirvec, 
          function(x) file.path(xdir, dir(), x, "s_unreported.rep"))))
        res <- as.data.frame(sapply(a_dirvec, function(x) (sapply(rep[grep(x, rep)],
          function(y) as.numeric(read.table(y, skip=11, nrow=1))))))
        results[[i]] <- res
      }
    return(results)

    on.exit(setwd(init_dir))
}