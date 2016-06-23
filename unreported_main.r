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
init_dir <- admb_dir <- "C:\\Git_Projects\\catch_misreporting_sim"
# admb_dir <- "C:\\Projects\\Catch_unreporting"
# init_dir <- admb_dir <- "F:\\Merrill\\Projects\\Catch_unreporting"
# s_dir <- paste0(admb_dir, "\\Schaefer")
pt_dir <- paste0(admb_dir, "\\Pella-Tomlinson Executable")
res_figs <- file.path(init_dir, "figs")
dir.create(res_figs, showWarnings=FALSE)

###############################################
### compile executables
###############################################

### Pella-Tomlinson
if(file.exists(pt_dir)==FALSE) dir.create(pt_dir)
setwd(pt_dir)
file.copy(from=paste0(init_dir, "\\tpls\\pt_unreported.tpl"), to=pt_dir, over=TRUE)
system("admb -s pt_unreported")
setwd(init_dir)
pt_exe <- paste0(pt_dir, "\\pt_unreported.exe")
if(file.exists(pt_exe)) message("Pella-Tomlinson executable produced successfully") else message("Failure to produce Pella-Tomlinson executable")


###############################################
### reset working directory to call functions
###############################################
setwd(init_dir)
source("r_functions\\functions.R")

###############################################
### load true values
###############################################
catch1 <- as.numeric(t(read.table("example_catch.txt")))
datyrs <- 35
# catch2 <- (100/datyrs)*(1:datyrs)+rnorm(datyrs, 0, 15)
#   catch2[which(catch2 < 0)] <- 1
# saveRDS(catch2, "example_catch2.txt")
catch2 <- readRDS("example_catch2.txt")
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

###############################################
### test OM
##############################################
set.seed(123)
OM1 <- runOM(bmodel="pt_startalt", z=z_true, K=K_true,
	q=q_true, r=r_true, process_err=0, obs_err=0, datyrs=datyrs,
	example_catch=catch1)
OM2 <- runOM(bmodel="pt_startalt", z=z_true, K=K_true,
	q=q_true, r=r_true, process_err=sigma_proc,
	obs_err=sigma_proc, datyrs=datyrs, example_catch=catch1)
index_test <- OM2$Index
catch_test <- OM2$Catch
B_true_test <- OM2$Biomass
# plot(B_true_test2, main="Pella-Tomlinson", type="l", col="blue", lwd=2, xaxs="i", yaxs="i", ylim=c(0, max(B_true_test2)))
plotTruth(plotOM=OM2)

png(file.path(res_figs, "Catch_allrep.png"), width=400, height=300)
par(mar=c(6,6,1,1))
plot(x=1:datyrs, y=OM1$Catch, col=gray(0.2), lwd=4, xaxs="i", yaxs="i", type="l",
	ylim=c(0, max(OM1$Catch)*1.1), cex.axis=1.5, xlab="", ylab="", yaxt="n")
axis(2, at=seq(0, max(OM1$Catch)*1.1, by=50), cex.axis=1.5)
mtext("Year", side=1, cex=2.5, line=3)
mtext("True catch", side=2, cex=2.5, line=3)
dev.off()

png(file.path(res_figs, "True_Biomass.png"), width=400, height=300)
par(mar=c(6,6,1,1))
plot(x=1:datyrs, y=OM2$Biomass, col=gray(0.2), lwd=4, xaxs="i", yaxs="i", type="l",
	ylim=c(0, max(OM2$Biomass)*1.1), cex.axis=1.5, xlab="", ylab="", cex.lab=1.6,
	yaxt="n")
axis(2, at=seq(0, max(OM2$Biomass)*1.1, by=200), cex.axis=1.5)
mtext("Year", side=1, cex=2.5, line=3)
mtext("Biomass", side=2, cex=2.5, line=3)
dev.off()

png(file.path(res_figs, "Survey_Index.png"), width=400, height=300)
par(mar=c(6,6,1.5,1))
plot(x=1:datyrs, y=OM2$Index, col=gray(0.2), lwd=4, xaxs="i", yaxs="i", pch=19,
	ylim=c(0, max(OM2$Index)*1.1), cex.axis=1.5, xlab="", ylab="", cex.lab=1.6,
	yaxt="n")
axis(2, at=seq(0, max(OM2$Index)*1.1, by=2), cex.axis=1.5)
mtext("Year", side=1, cex=2.5, line=3)
mtext("Abundance index", side=2, cex=2.5, line=3)
dev.off()

png(file.path(res_figs, "Demonstrate_simulation.png"), width=1500, height=600)
res <- readRDS(file.path(init_dir, "results", "2way-results-terminal-startalt.rds"))
Paper_Fig_Simulation(results=res)
dev.off()

png(file.path(res_figs, "Demonstrate_simulation_bw.png"), width=1500, height=600)
res <- readRDS(file.path(init_dir, "results", "2way-results-terminal-startalt.rds"))
Paper_Fig_Simulation(results=res, bw=TRUE)
dev.off()


###############################################
### models to run
###############################################
sigma_vec <- c(0.001, 0.3)   #  
rmodel_vec <- c("allrep", "allunder", "allover", "repinc",   "repdec", "repinc_v2", "repdec_v2") #c("inc_mean1", "dec_mean1")       
rmodel_vec_names <- c("100% Reporting", "Constant Under-reporting",  "Constant Over-reporting", "Increasing Reporting", "Decreasing Reporting") 
bmodel_vec <- c("pt_startalt" )  
bmodel_vec_names <- c("Pella-Tomlinson Start K", "Pella-Tomlinson Start Alt") 
catch_vec <- c("2way_catch") #"1way_catch"

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


###############################################
### run model
###############################################

iter_vec <- 1:1000  #seed=123

### generate data
start <- Sys.time()
for(c in 1:length(catch_vec)){
for(b in 1:length(bmodel_vec)){
	for(s in 1:length(sigma_vec)){
		if(catch_vec[c]=="2way_catch") xcatch <- as.numeric(catch1)
		if(catch_vec[c]=="1way_catch") xcatch <- as.numeric(catch2)
		if(sigma_vec[s]==0.001) iters <- 1
		if(sigma_vec[s]!=0.001) iters <- iter_vec
		generateData(bmodel=bmodel_vec[b], sigma=sigma_vec[s], catch=xcatch, itervec=iters, seed=123)
	}
}
}
total <- Sys.time() - start

### run model scenarios
start <- Sys.time()
for(c in 1:length(catch_vec)){
	for(b in 1:length(bmodel_vec)){
		for(s in 1:length(sigma_vec)){
			if(catch_vec[c]=="2way_catch") xcatch <- as.numeric(catch1)
		    if(catch_vec[c]=="1way_catch") xcatch <- as.numeric(catch2)
		    for(r in 1:length(rmodel_vec)){
		    	xrep <- ifelse(rmodel_vec[r]=="allrep", 1, ifelse(rmodel_vec[r]=="allunder", 0.5,
		    		ifelse(rmodel_vec[r]=="allover", 1.5, NA)))
		    	xstart <- ifelse(grepl("startalt", bmodel_vec[b]), "Depl", "K")
		    	if(sigma_vec[s]==0.001) iters <- 1
		    	if(sigma_vec[s]!=0.001) iters <- iter_vec
		    	runEM(rmodel=rmodel_vec[r], bmodel=bmodel_vec[b], itervec=iters,
		    		datyrs=datyrs, report=xrep, start_sim=xstart, catch=xcatch,
		    		sigma=sigma_vec[s], re=0.25, checknll=10, exe=pt_exe)
		    }
		}
	}
}
total <- Sys.time() - start



###############################################
### write results
###############################################
setwd(init_dir)
source("R_functions\\functions.R")

if(file.exists(file.path(init_dir, "results"))==FALSE) dir.create(file.path(init_dir, "results"))
res_dir <- file.path(init_dir, "results")

iter_vec <- 1:1000
# term_2way_startk <- writeterminalresults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_startk, res_dir=res_dir)
term_2way_startalt <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_startalt, res_dir=res_dir)

###############################################
### read results
###############################################
if(file.exists(file.path(init_dir, "results"))==FALSE) dir.create(file.path(init_dir, "results"))
res_dir <- file.path(init_dir, "results")

term_2way_startalt <- readRDS(file.path(res_dir, "2way-results-terminal-startalt.rds"))

setwd(init_dir)
source("r_functions\\functions.R")

png(file.path(res_figs, "Paper_Fig_Terminal_stochastic_Alt.png"), width=1300, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure2.pdf"), width=18, height=14)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure2_bw.pdf"), width=18, height=14)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2, bw=TRUE)
dev.off()


png(file.path(res_figs, "Paper_Fig_Project_deterministic_Alt.png"), width=1200, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=5, constantTAC=FALSE)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure3.pdf"), width=18, height=14)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=5, constantTAC=FALSE)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure3_bw.pdf"), width=18, height=14)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=5, constantTAC=FALSE, bw=TRUE)
dev.off()