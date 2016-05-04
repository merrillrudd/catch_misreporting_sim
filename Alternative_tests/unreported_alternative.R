###############################################
### load packages
###############################################
rm(list=ls())
library(RColorBrewer)
library(plyr)

###############################################
### directories
###############################################
 # init_dir <- "D:\\Projects\\Catch_unreporting\\catch_misreporting_sim-master"
 # admb_dir <- "D:\\Projects\\Catch_unreporting"
init_dir <- admb_dir <- "C:\\Git_Projects\\catch_misreporting_sim\\Alternative_tests"
# admb_dir <- "C:\\Projects\\Catch_unreporting"
# init_dir <- admb_dir <- "F:\\Merrill\\Projects\\Catch_unreporting"
# s_dir <- paste0(admb_dir, "\\Schaefer")
pt_dir <- paste0(admb_dir, "\\Pella-Tomlinson Executable")
res_figs <- file.path(init_dir, "figs")
dir.create(res_figs, showWarnings=FALSE)

###############################################
### compile executables
###############################################
pt_exe <- paste0(pt_dir, "\\pt_unreported.exe")

setwd(init_dir)
source("r_functions\\functions.R")

###############################################
### load true values
###############################################
catch2 <- as.numeric(t(read.table("example_catch.txt")))/2
datyrs <- 35
# catch2 <- (100/datyrs)*(1:datyrs)+rnorm(datyrs, 0, 15)
#   catch2[which(catch2 < 0)] <- 1
# saveRDS(catch2, "example_catch2.txt")
catch1 <- readRDS("example_catch2.txt")/2
r_true <- 0.2
z_true <- 1.188
K_true <- 1000
bmsy_true <- 0.4*K_true
msy_true <- (r_true*K_true*z_true)/((z_true+1)^(1/z_true + 1))
bmey_true <- bmsy_true*1.3
emsy_true <- msy_true/bmsy_true
q_true <- 0.01
sigma_proc <- 0.3
sigma_obs <- 0.3

set.seed(123)
OM1 <- runOM(bmodel="pt_startalt", z=z_true, K=K_true,
	q=q_true, r=r_true, process_err=0, obs_err=0, datyrs=datyrs,
	example_catch=catch1)
OM1$F <- OM1$Catch/OM1$Biomass

F_1way <- OM1$F

set.seed(123)
OM2 <- runOM(bmodel="pt_startalt", z=z_true, K=K_true,
	q=q_true, r=r_true, process_err=0, obs_err=0, datyrs=datyrs,
	example_catch=catch2)
OM2$F <- OM2$Catch/OM2$Biomass

F_2way <- OM2$F

r_vec <- c(0.05, 0.2, 0.7)
K_vec <- c(1000)

###################
### setup models
###################

sigma_vec <- c(0.001, 0.3)
rmodel_vec <- c("allrep", "allunder", "allover", "repinc",   "repdec" ) #c("inc_mean1", "dec_mean1")       
rmodel_vec_names <- c("100% Reporting", "Constant Under-reporting",  "Constant Over-reporting", "Increasing Reporting", "Decreasing Reporting") 
bmodel_vec <- c("pt_startalt" )  #  "pt",   , 
bmodel_vec_names <- c("Pella-Tomlinson Start K", "Pella-Tomlinson Start Alt") 
catch_vec <- c("1way_catch", "2way_catch")

###############################################
### directories to extract results
###############################################
modcombos <- expand.grid(bmodel_vec_names, catch_vec, as.character(sigma_vec), rmodel_vec, stringsAsFactors=FALSE)

### can re-set all dirs to re-write terminal results with directories we'll want to plot
alldirs <- apply(modcombos, 1, function(x) paste0(admb_dir,"\\",
   as.character(x[1]), "\\", x[2], "\\sigma", x[3], "\\", x[4]))

#unlink(alldirs, TRUE)

dirs2 <- alldirs[grep("2way_catch", alldirs)]
dirs2_startK <- dirs2[grepl("K", dirs2)]
dirs2_startalt <- dirs2[grep("Alt", dirs2)]

dirs1 <- alldirs[grep("1way_catch", alldirs)]
dirs1_startK <- dirs1[grepl("K", dirs1)]
dirs1_startalt <- dirs1[grep("Alt", dirs1)]

modcombos_v2 <- expand.grid(bmodel_vec_names, catch_vec, as.character(K_vec), as.character(r_vec), as.character(0.001), rmodel_vec, stringsAsFactors=FALSE)

### can re-set all dirs to re-write terminal results with directories we'll want to plot
alldirs_v2 <- apply(modcombos_v2, 1, function(x) paste0(admb_dir,"\\",
   as.character(x[1]), "\\", x[2], "\\K", x[3], "_r", x[4], "\\sigma", x[5], "\\", x[6]))

dirs2_Klow_rlow <- alldirs_v2[grepl("K1000_r0.05", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs2_Khigh_rlow <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.05", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]

dirs2_Klow_rmed <- alldirs_v2[grepl("K1000_r0.2", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs2_Khigh_rmed <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.2", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]

dirs2_Klow_rhigh <- alldirs_v2[grepl("K1000_r0.7", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs2_Khigh_rhigh <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.7", alldirs_v2) & grepl("2way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]

dirs1_Klow_rlow <- alldirs_v2[grepl("K1000_r0.05", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs1_Khigh_rlow <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.05", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]

dirs1_Klow_rmed <- alldirs_v2[grepl("K1000_r0.2", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs1_Khigh_rmed <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.2", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]

dirs1_Klow_rhigh <- alldirs_v2[grepl("K1000_r0.7", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]
dirs1_Khigh_rhigh <- alldirs_v2[grepl("K1000",alldirs_v2)==FALSE & grepl("r0.7", alldirs_v2) & grepl("1way", alldirs_v2) & grepl("Start Alt", alldirs_v2)]


###############################################
### run model
###############################################

setwd(init_dir)
source("r_functions\\functions.R")

iter_vec <- 1:2

### generate data
start <- Sys.time()
for(c in 1:length(catch_vec)){
	for(rr in 1:length(r_vec)){
	for(kk in 1:length(K_vec)){
	for(s in 1:length(sigma_vec)){
		if(catch_vec[c]=="2way_catch") xcatch <- catch2
		if(catch_vec[c]=="1way_catch") xcatch <- catch1
		if(sigma_vec[s]==0.001) iters <- 1
		if(sigma_vec[s]!=0.001) iters <- iter_vec
		generateData(bmodel="pt_startalt", sigma=sigma_vec[s], catch=xcatch, fmort=NULL, itervec=iters, seed=123, rinput=r_vec[rr], Kinput=K_vec[kk])
	}
	}
	}
}
total <- Sys.time() - start

### run model scenarios
start <- Sys.time()
for(c in 1:length(catch_vec)){
	for(rr in 1:length(r_vec)){
		for(kk in 1:length(K_vec)){
		for(s in 1:length(sigma_vec)){
			if(catch_vec[c]=="2way_catch") xcatch <- as.numeric(catch1)
		    if(catch_vec[c]=="1way_catch") xcatch <- as.numeric(catch2)
		    for(r in 1:length(rmodel_vec)){
		    	xrep <- ifelse(rmodel_vec[r]=="allrep", 1, ifelse(rmodel_vec[r]=="allunder", 0.5,
		    		ifelse(rmodel_vec[r]=="allover", 1.5, NA)))
		    	xstart <- ifelse(grepl("startalt", "pt_startalt"), "Depl", "K")
		    	if(sigma_vec[s]==0.001) iters <- 1
		    	if(sigma_vec[s]!=0.001) iters <- iter_vec
		    	runEM(rmodel=rmodel_vec[r], bmodel="pt_startalt", itervec=iters,
		    		datyrs=datyrs, report=xrep, start_sim=xstart, catch=xcatch,
		    		sigma=sigma_vec[s], re=0.25, checknll=10, exe=pt_exe, rinput=r_vec[rr], Kinput=K_vec[kk])
		    }
		}
		}
	}
	}
}
total <- Sys.time() - start

#################################
## results
################################

setwd(init_dir)
source("r_functions\\functions.R")

if(file.exists(file.path(init_dir, "results"))==FALSE) dir.create(file.path(init_dir, "results"))
res_dir <- file.path(init_dir, "results")

iter_vec <- 1:2

### low K 2way
res1 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Klow_rlow, res_dir=res_dir, name="2way_Klow_rlow")
Paper_Fig_Project(results=res1, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res1, deterministic=TRUE, relerr=2, catch=catch2)


res2 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Klow_rmed, res_dir=res_dir, name="2way_Klow_rmed")
Paper_Fig_Project(results=res2, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res2, deterministic=TRUE, relerr=2, catch=catch2)


res3 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Klow_rhigh, res_dir=res_dir, name="2way_Klow_rhigh")
Paper_Fig_Project(results=res3, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res3, deterministic=TRUE, relerr=2, catch=catch2)



## low K 1way
res7 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs1_Klow_rlow, res_dir=res_dir, name="1way_Klow_rlow")
Paper_Fig_Project(results=res7, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res7, deterministic=TRUE, relerr=2, catch=catch1)


res8 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs1_Klow_rmed, res_dir=res_dir, name="1way_Klow_rmed")
Paper_Fig_Project(results=res8, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res8, deterministic=TRUE, relerr=2, catch=catch1)


res9 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs1_Klow_rhigh, res_dir=res_dir, name="1way_Klow_rhigh")
Paper_Fig_Project(results=res9, run_project=TRUE, start_sim="Alt", nproject=30, constantTAC=FALSE)
Paper_Fig_Terminal(results=res9, deterministic=TRUE, relerr=2, catch=catch1)


## high K 2way
### high K messed up
res4 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Khigh_rlow, res_dir=res_dir, name="2way_Khigh_rlow")
Paper_Fig_Project(results=res4, run_project=TRUE, start_sim="Alt", nproject=50, constantTAC=FALSE)

res5 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Khigh_rmed, res_dir=res_dir, name="2way_Khigh_rmed")
Paper_Fig_Project(results=res5, run_project=TRUE, start_sim="Alt", nproject=50, constantTAC=FALSE)

res6 <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_Khigh_rhigh, res_dir=res_dir, name="2way_Khigh_rhigh")
Paper_Fig_Project(results=res6, run_project=TRUE, start_sim="Alt", nproject=50, constantTAC=FALSE)