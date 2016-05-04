createPin <- function(dir, re){
	
	pinfile <- paste0(dir, "\\", "pt_unreported.pin")

		write("# logK", file=pinfile)
        write(log(runif(1, (-re*K_true + K_true), (re*K_true + K_true))), file=pinfile, append=TRUE)
        write("# logMSY", file=pinfile, append=TRUE)
        write(log(runif(1, (-re*msy_true + msy_true), (re*msy_true + msy_true))), file=pinfile, append=TRUE)
		write("# P0", file=pinfile, append=TRUE)
		write(1, file=pinfile, append=TRUE)
}