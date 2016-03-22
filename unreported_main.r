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

# r_true2 <- 0.05
# MSY_true2 <- (r_true2*K_true*z_true)/((z_true+1)^(1/z_true + 1))


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

png(file.path(res_figs, "Estimated_Biomass.png"), width=1500, height=600)
res <- readRDS(file.path(init_dir, "results", "2way-results-terminal-startalt.rds"))
par(mfrow=c(2,5), mar=c(3,0,0,0), omi=c(1,2,1,1))
## catch time series
xplot <- 1:datyrs
cols <- brewer.pal(4, "Set1")
up <- (0.8-0.2)/(datyrs-1)*xplot + 0.2
down <- (0.2-0.8)/(datyrs-1)*xplot + 0.8
    plot(x=xplot, y=catch1, type="l", lwd=3, ylim=c(0, max(catch1)*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.2))
    axis(2, at=pretty(c(1,catch1*1.6)), cex.axis=3)
    mtext("Reported\ncatch",  side=2, line=5, cex=3)
    mtext("100% Reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, ylim=c(0, max(catch1)*1.6), 
      xaxs="i", yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=catch1*0.5, lwd=3, col=cols[1])
     mtext("Constant Under-reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=catch1*1.5, lwd=3, col=cols[2])
     mtext("Constant Over-reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=catch1*up, lwd=3, col=cols[3])
     mtext("Increasing Reporting",  side=3, line=1.5, cex=1.9)
    plot(x=xplot, y=catch1, type="l", lwd=3, xaxs="i", ylim=c(0, max(catch1)*1.6), 
      yaxs="i", xaxt="n", yaxt="n", col=gray(0.6))
    lines(x=xplot, y=catch1*down, lwd=3, col=cols[4])
     mtext("Decreasing Reporting",  side=3, line=1.5, cex=1.9)

plot(x=xplot, y=res$b_est[1,2,], col=gray(0.2), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
axis(1, at=seq(10,30, by=10), cex.axis=3)
axis(2, at=seq(0, 1850, by=500), cex.axis=3)
mtext("Estimated\nbiomass",  side=2, line=5, cex=3)
plot(x=xplot, y=res$b_est[1,2,], col=gray(0.6), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
lines(x=xplot, y=res$b_est[1,4,], col=cols[1], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
axis(1, at=seq(10,30, by=10), cex.axis=3)
plot(x=xplot, y=res$b_est[1,2,], col=gray(0.6), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
mtext("Year", side=1, line=5, cex=3)
lines(x=xplot, y=res$b_est[1,6,], col=cols[2], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
axis(1, at=seq(10,30, by=10), cex.axis=3)
plot(x=xplot, y=res$b_est[1,2,], col=gray(0.6), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
lines(x=xplot, y=res$b_est[1,12,], col=cols[3], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
axis(1, at=seq(10,30, by=10), cex.axis=3)
plot(x=xplot, y=res$b_est[1,2,], col=gray(0.6), type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
lines(x=xplot, y=res$b_est[1,14,], col=cols[4], type="l", lwd=3, ylim=c(0,1850), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
axis(1, at=seq(10,30, by=10), cex.axis=3)
dev.off()


###############################################
### models to run
###############################################
sigma_vec <- c(0.3)   #  
rmodel_vec <- c("allrep", "allunder", "allover", "repinc",   "repdec", "repinc_v2", "repdec_v2") #c("inc_mean1", "dec_mean1")       
rmodel_vec_names <- c("100% Reporting", "Constant Under-reporting",  "Constant Over-reporting", "Increasing Reporting", "Decreasing Reporting") 
# rmodel_vec_names <- c("Increasing Reporting\nMean=1", "Decreasing Reporting\nMean=1")#
bmodel_vec <- c("pt_startalt" )  #  "pt",   , 
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
#iter_vec <- 101:200 #seed=4567
# iter_vec <- 201:300 #seed=8910
# iter_vec <- 301:1000 #seed=11121314
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



###########################################
setwd(init_dir)
source("R_functions\\functions.R")

if(file.exists(file.path(init_dir, "results"))==FALSE) dir.create(file.path(init_dir, "results"))
res_dir <- file.path(init_dir, "results")

iter_vec <- 1:1000
# term_2way_startk <- writeterminalresults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_startk, res_dir=res_dir)
term_2way_startalt <- writeTerminalResults(iter=iter_vec, datyrs=datyrs, dirs=dirs2_startalt, res_dir=res_dir)

# term_2way_startK <- readRDS(file.path(res_dir, "2way-results-terminal-startK.rds"))
term_2way_startalt <- readRDS(file.path(res_dir, "2way-results-terminal-startalt.rds"))

##---------------------------------
png(file.path(res_figs, "Paper_Fig_Terminal_stochastic_Alt.png"), width=1300, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2)
dev.off()

png(file.path(res_figs, "Presentation_Fig_Terminal_stochastic_Alt.png"), width=1300, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, show_catch=FALSE, print_letter=FALSE, show_params=TRUE, pres=TRUE)
dev.off()

png(file.path(res_figs, "Presentation_Fig_Reporting_Rates.png"), width=1500, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, show_catch=TRUE, print_letter=FALSE, show_params=FALSE, pres=TRUE)
dev.off()

png(file.path(res_figs, "Paper_Fig_Project_deterministic_Alt.png"), width=1200, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, constantTAC=FALSE)
dev.off()

png(file.path(res_figs, "Paper_Fig_Project_deterministic_Alt_constantTAC.png"), width=1200, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, constantTAC=TRUE)
dev.off()

png(file.path(res_figs, "Paper_Fig_Project_deterministic_Alt_constantTAC_MSY.png"), width=1200, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, constantTAC=TRUE, msy=TRUE)
dev.off()

png(file.path(res_figs, "Presentation_Fig_Project_deterministic_Alt.png"), width=1500, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, pres=TRUE, print_letters=FALSE)
dev.off()

##--------------------------------


png(file.path(res_figs, "Paper_Fig_Terminal_deterministic.png"), width=1200, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=TRUE, relerr=2) #relerr = 2 == estimated/true
dev.off()

png(file.path(res_figs, "Paper_Fig_Terminal_deterministic_K.png"), width=1200, height=800)
Paper_Fig_Terminal(results=term_2way_startK, deterministic=TRUE, relerr=2)
dev.off()



png(file.path(res_figs, "Paper_Fig_Terminal_stochastic_K.png"), width=1200, height=800)
Paper_Fig_Terminal(results=term_2way_startK, deterministic=FALSE, relerr=2)
dev.off()

png(file.path(res_figs, "MSY_MRE.png"), width=1200, height=800)
plotMRE(results=term_2way_startalt, absolute=FALSE, parameter="msy")
dev.off()

plotMRE(results=term_2way_startalt, absolute=FALSE, parameter="B")
plotMRE(results=term_2way_startalt, absolute=FALSE, parameter="K")

plotMRE(results=term_2way_startK, absolute=FALSE, parameter="K")

png(file.path(res_figs, "K_MRE.png"), width=1200, height=800)
plotMRE(results=term_2way_startalt, absolute=FALSE, parameter="K")
dev.off()

png(file.path(res_figs, "MSY_MARE.png"), width=1200, height=800)
plotMRE(results=term_2way_startK, absolute=TRUE)
dev.off()

Paper_Fig_Project(results=term_2way_startalt, uncertainty=FALSE, run_project=TRUE, start_sim="Alt")
Paper_Fig_Project(results=term_2way_startalt, uncertainty=TRUE, run_project=FALSE, start_sim="Alt")



png(file.path(res_figs, "Paper_Fig_Project_deterministic_K_unreporting.png"), width=1000, height=600)
Paper_Fig_Project(results=term_2way_startK, uncertainty=FALSE, run_project=TRUE, start_sim="K")
dev.off()




######## analytical calculations of equilibrium biomass


png(file.path(res_figs, "Paper_Fig_Terminal_beanplot_twocatch.png"), width=1200, height=700)
Paper_Fig_Terminal(results=term_2way, dirs=dirs2[grep("sigma0.1", dirs2)],
	results_extra=term_1way, dirs_extra=dirs1[grep("sigma0.1", dirs1)])
dev.off()

png(file.path(res_figs, "Paper_Fig_Retro.png"), width=1200, height=1400)
Paper_Fig_Retro(results=retro_2way, dirs=dirs2[grep("sigma0.1", dirs2)])
dev.off()

png(file.path(res_figs, "catch_scenarios.png"), width=1200, height=700)
plotScenarios(plot_catch="all")
dev.off()

##==================================================================================================

bottomfish_dat <- read.csv(file.path(init_dir, "hawaiian_bottomfish_reporting_ratios.csv"))
plot(x=bottomfish_dat$Year, y=bottomfish_dat$Reported_1000lbs/bottomfish_dat$TotalCatch_1000lbs, type="l", lwd=2)



png(file.path(res_figs, "MSY_Retro_MRE.png"), width=1200, height=800)
plotMRE(absolute=TRUE, assess_seq=TRUE)
dev.off()

png(file.path(res_figs, "terminal_2way.png"), width=1200, height=800)
plotTerminal(dirs=dirs2, results=term_2way)
dev.off()

png(file.path(res_figs, "terminal_1way.png"), width=1200, height=800)
plotTerminal(dirs=dirs1, results=term_1way)
dev.off()

png(file.path(res_figs, "terminal_det.png"), width=1200, height=800)
plotTerminal(dirs=dirs_det, results=list(term_2way, term_1way), nres=2)
dev.off()

png(file.path(res_figs, "terminal_stoch.png"), width=1200, height=800)
plotTerminal(dirs=dirs_stoch, results=list(term_2way, term_1way), nres=2)
dev.off()

plotTerminal(dirs=dirs_changeq[grep("2way", dirs_changeq)], results=term_2way_changeq)
plotTerminal(dirs=dirs_changeq[grep("1way", dirs_changeq)], results=term_1way_changeq)

png(file.path(res_figs, "terminal_allunder_changeq.png"), width=1200, height=800)
plotTerminal(dirs=dirs_changeq, results=list(term_2way_changeq, term_1way_changeq), nres=2)
dev.off()

png(file.path(res_figs, "MSYcompare_retro_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot="MSY", scenario="all")
dev.off()

png(file.path(res_figs, "MSYcompare_retro_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot="MSY", scenario="all")
dev.off()

png(file.path(res_figs, "MSYcompare_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot="MSY", scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "MSYcompare_stoch.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot="MSY", scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_allrep_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("MSY","K","r"), scenario="allrep")
dev.off()

png(file.path(res_figs, "ParamCompare_allrep_det"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="allrep", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_allrep_stoch"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="allrep", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_allunder_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("MSY","K","r"), scenario="allunder")
dev.off()

png(file.path(res_figs, "ParamCompare_allunder_det"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="allunder", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_allunder_stoch"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="allunder", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_repinc_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("MSY","K","r"), scenario="repinc")
dev.off()

png(file.path(res_figs, "ParamCompare_repinc_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="repinc", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_repinc_stoch"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="repinc", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_repdec_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("MSY","K","r"), scenario="repdec")
dev.off()

png(file.path(res_figs, "ParamCompare_repdec_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="repdec", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_repdec_stoch.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("MSY", "K", "r"), scenario="repdec", nres=2)
dev.off()

png(file.path(res_figs, "ParamCompare_allover_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("MSY","K","r"), scenario="allover")
dev.off()

png(file.path(res_figs, "ParamCompare_allrep_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("MSY","K","r"), scenario="allrep")
dev.off()

png(file.path(res_figs, "ParamCompare_allunder_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("MSY","K","r"), scenario="allunder")
dev.off()

png(file.path(res_figs, "ParamCompare_repinc_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("MSY","K","r"), scenario="repinc")
dev.off()

png(file.path(res_figs, "ParamCompare_repdec_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("MSY","K","r"), scenario="repdec")
dev.off()

png(file.path(res_figs, "ParamCompare_allover_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("MSY","K","r"), scenario="allover")
dev.off()

png(file.path(res_figs, "BBmsyCompare_retro_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot="BBmsy", scenario="all")
dev.off()

png(file.path(res_figs, "EEmsyCompare_retro_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot="EEmsy", scenario="all")
dev.off()

png(file.path(res_figs, "BBmsyCompare_retro_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot="BBmsy", scenario="all")
dev.off()

png(file.path(res_figs, "EEmsyCompare_retro_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot="EEmsy", scenario="all")
dev.off()

png(file.path(res_figs, "BBmsyCompare_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("BBmsy"), scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "BBmsyCompare_stoch.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("BBmsy"), scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "EEmsyCompare_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot=c("EEmsy"), scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "EEmsyCompare_stoch.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot=c("EEmsy"), scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "StatusCompare_allrep_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("BBmsy", "EEmsy"), scenario="allrep")
dev.off()

png(file.path(res_figs, "StatusCompare_allunder_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("BBmsy", "EEmsy"), scenario="allunder")
dev.off()

png(file.path(res_figs, "StatusCompare_repinc_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("BBmsy", "EEmsy"), scenario="repinc")
dev.off()

png(file.path(res_figs, "StatusCompare_repdec_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("BBmsy", "EEmsy"), scenario="repdec")
dev.off()

png(file.path(res_figs, "StatusCompare_allover_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot=c("BBmsy", "EEmsy"), scenario="allover")
dev.off()

png(file.path(res_figs, "StatusCompare_allrep_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("BBmsy", "EEmsy"), scenario="allrep")
dev.off()

png(file.path(res_figs, "StatusCompare_allunder_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("BBmsy", "EEmsy"), scenario="allunder")
dev.off()

png(file.path(res_figs, "StatusCompare_repinc_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("BBmsy", "EEmsy"), scenario="repinc")
dev.off()

png(file.path(res_figs, "StatusCompare_repdec_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("BBmsy", "EEmsy"), scenario="repdec")
dev.off()

png(file.path(res_figs, "StatusCompare_allover_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot=c("BBmsy", "EEmsy"), scenario="allover")
dev.off()

png(file.path(res_figs, "BiomassCompare_2way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs2, results=retro_2way, to.plot="biomass", scenario="all")
dev.off()

png(file.path(res_figs, "BiomassCompare_1way.png"), width=1200, height=800)
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot="biomass", scenario="all")
dev.off()

png(file.path(res_figs, "Biomasscompare_det.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_det, results=list(retro_2way, retro_1way), to.plot="biomass", scenario="all", nres=2)
dev.off()

png(file.path(res_figs, "Biomasscompare_stoch.png"), width=1200, height=800)
plotRetrospective(dirs=dirs_stoch, results=list(retro_2way, retro_1way), to.plot="biomass", scenario="all", nres=2)
dev.off()


plotRetrospective(dirs=dirs2, results=retro_2way, to.plot="exploit", scenario="all")
plotRetrospective(dirs=dirs1, results=retro_1way, to.plot="exploit", scenario="all")

png(file.path(res_figs, "timeseries_det_2way.png"), width=1200, height=1600)
plotTime(dirs=dirs2, results=retro_2way, datyrs=datyrs, trend="deterministic")
dev.off()


png(file.path(res_figs, "timeseries_det_2way.png"), width=1200, height=1600)
plotTime(dirs=dirs2, results=retro_2way, datyrs=datyrs, trend="deterministic")
dev.off()


png(file.path(res_figs, "timeseries_det_1way.png"), width=1200, height=1600)
plotTime(dirs=dirs1, results=retro_1way, datyrs=datyrs, trend="deterministic")
dev.off()

png(file.path(res_figs, "timeseries_stoch_2way.png"), width=1200, height=1600)
plotTime(dirs=dirs2, results=retro_2way, datyrs=datyrs, trend="stochastic")
dev.off()

png(file.path(res_figs, "reporting_scenarios.png"), width=1200, height=600)
plotScenarios(plot_catch=0)
dev.off()

png(file.path(res_figs, "catch_2way.png"), width=1200, height=800)
plotScenarios(plot_catch=2)
dev.off()

png(file.path(res_figs, "catch_1way.png"), width=1200, height=800)
plotScenarios(plot_catch=1)
dev.off()


OM1 <- runOM(bmodel="pt", r=NULL, K=K_true, MSY=MSY_true,
		q=q_true, z=z_true, process_err=0, obs_err=0,
		datyrs=datyrs, example_catch=catch1)
OM2 <- runOM(bmodel="pt", r=NULL, K=K_true, MSY=MSY_true,
		q=q_true, z=z_true, process_err=0.1, obs_err=0.1,
		datyrs=datyrs, example_catch=catch1)

png(file.path(res_figs, "true_biomass_det.png"), width=800, height=800)
plotTruth(plot_index=FALSE, plotOM=OM1)
dev.off()

png(file.path(res_figs, "true_biomass1.png"), width=800, height=800)
plotTruth(plot_index=FALSE, plotOM=OM2)
dev.off()

png(file.path(res_figs, "truebiomass_v_index.png"), width=800, height=800)
plotTruth(plot_index=TRUE, plotOM=OM2)
dev.off()

png(file.path(res_figs, "b_bre_2way_det.png"), width=1200, height=600)
plotResults(to.plot=c("biomass","biomassRE"),
	dirs=dirs2, results=term_2way, process_err=0.001)
dev.off()

png(file.path(res_figs, "b_bre_1way_det.png"), width=1200, height=600)
plotResults(to.plot=c("biomass","biomassRE"),
	dirs=dirs1, results=term_1way, process_err=0.001)
dev.off()

png(file.path(res_figs, "b_bre_2way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("biomass","biomassRE"),
	dirs=dirs2, results=term_2way, process_err=0.1)
dev.off()

png(file.path(res_figs, "b_bre_1way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("biomass","biomassRE"),
	dirs=dirs1, results=term_1way, process_err=0.1)
dev.off()

png(file.path(res_figs, "bbmsy_bbmsyre_2way_det.png"), width=1200, height=600)
plotResults(to.plot=c("BBmsy","BBmsyRE"),
	dirs=dirs2, results=term_2way, process_err=0.001)
dev.off()

png(file.path(res_figs, "bbmsy_bbmsyre_1way_det.png"), width=1200, height=600)
plotResults(to.plot=c("BBmsy","BBmsyRE"),
	dirs=dirs1, results=term_1way, process_err=0.001)
dev.off()

png(file.path(res_figs, "bbmsy_bbmsyre_2way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("BBmsy","BBmsyRE"),
	dirs=dirs2, results=term_2way, process_err=0.1)
dev.off()

png(file.path(res_figs, "bbmsy_bbmsyre_1way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("BBmsy","BBmsyRE"),
	dirs=dirs1, results=term_1way, process_err=0.1)
dev.off()


png(file.path(res_figs, "eemsy_eemsyre_2way_det.png"), width=1200, height=600)
plotResults(to.plot=c("EEmsy","EEmsyRE"),
	dirs=dirs2, results=term_2way, process_err=0.001)
dev.off()

png(file.path(res_figs, "eemsy_eemsyre_1way_det.png"), width=1200, height=600)
plotResults(to.plot=c("EEmsy","EEmsyRE"),
	dirs=dirs1, results=term_1way, process_err=0.001)
dev.off()

png(file.path(res_figs, "eemsy_eemsyre_2way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("EEmsy","EEmsyRE"),
	dirs=dirs2, results=term_2way, process_err=0.1)
dev.off()

png(file.path(res_figs, "eemsy_eemsyre_1way_stoch.png"), width=1200, height=600)
plotResults(to.plot=c("EEmsy","EEmsyRE"),
	dirs=dirs1, results=term_1way, process_err=0.1)
dev.off()





####################################################################

mod <- "allrep"
if(mod=="allrep") label <- "100% Reporting"
if(mod=="allunder") label <- "Constant Underreporting"
if(mod=="repinc") label <- "Reporting Rate Increasing"
if(mod=="repdec") label <- "Reporting Rate Decreasing"
if(mod=="allover") label <- "Constant Overreporting"
dir <- file.path(init_dir, "results", "1way_catch", "Pella-Tomlinson", mod, "sigma0.001")
dev.new()

png(file.path(admb_dir, "figs", "iter_compare_b_allover.png"), width=1100, height=700)
# nll_mat <- matrix(0, nrow=16, ncol=15)
iter <- 1:100
restrue <- matrix(0, nrow=length(iter), ncol=datyrs)
resest <- matrix(0, nrow=length(iter), ncol=datyrs)
par(mfrow=c(2,2), omi=c(0,0,0.3,0))
plot(x=1, y=1, type="n", xlim=c(1,datyrs), ylim=c(0,3000), xaxs="i", yaxs="i", xlab="Year", ylab="Biomass")
mtext(paste(label, "sigma=0.1", sep=" "), side=3, outer=TRUE, font=2)
for(i in iter){
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1))
	restrue[i,] <- btrue
	resest[i,] <- best
	lines(x=1:35, y=as.numeric(btrue), col="black")
	lines(x=1:35, y=as.numeric(best), col="red")
}
plot(x=1, y=1, type="n", xlim=c(1,35), ylim=c(0,3000), xaxs="i", yaxs="i", xlab="Year", ylab="Biomass")
truequants <- apply(restrue, 2, function(x) quantile(x, probs=c(0.05, 0.5, 0.95)))
estquants <- apply(resest, 2, function(x) quantile(x, probs=c(0.05, 0.5, 0.95)))
polygon(x=c(1:35, 35:1), y=c(truequants[1,], rev(truequants[3,])), col="#88888870", border=NA)
polygon(x=c(1:35, 35:1), y=c(estquants[1,], rev(estquants[3,])), col="#88000050", border=NA)
lines(truequants[2,], lwd=3)
lines(estquants[2,], lwd=3, col="#880000")


bbtrue <- matrix(0, nrow=length(iter), ncol=35)
bbest <- matrix(0, nrow=length(iter), ncol=35)
plot(x=1, y=1, type="n", xlim=c(1,35), ylim=c(0,4), xaxs="i", yaxs="i", xlab="Year", ylab="B/Bmsy")
for(i in iter){
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1))
	bmsy_true <- true$BMSY
    bmsyest <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"),
		skip=7, nrows=1))
	bbtrue[i,] <- btrue/bmsy_true
	bbest[i,] <- best/bmsyest
	lines(x=1:35, y=bbtrue[i,], col="black")
	lines(x=1:35, y=bbest[i,], col="blue")
}
plot(x=1, y=1, type="n", xlim=c(1,35), ylim=c(0,4), xaxs="i", yaxs="i", xlab="Year", ylab="B/Bmsy")
bbtruequants <- apply(bbtrue, 2, function(x) quantile(x, probs=c(0.05, 0.5, 0.95)))
bbestquants <- apply(bbest, 2, function(x) quantile(x, probs=c(0.05, 0.5, 0.95)))
polygon(x=c(1:35, 35:1), y=c(bbtruequants[1,], rev(bbtruequants[3,])), col="#88888870", border=NA)
polygon(x=c(1:35, 35:1), y=c(bbestquants[1,], rev(bbestquants[3,])), col="#00008850", border=NA)
lines(bbtruequants[2,], lwd=3)
lines(bbestquants[2,], lwd=3, col="#000088")

dev.off()

par(mfrow=c(5,5))
for(i in iter){
	plot(x=1,y=1, type="n", xlim=c(1,35), ylim=c(-1,1))
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1)
	re <- (best-btrue)/btrue
	# lines(x=1:length(btrue), y=btrue, col="black")
	# lines(x=1:length(best), y=best, col="blue", lwd=2)
	points(x=1:length(btrue), y=re, lwd=2)
	abline(h=0, col="red")
	mtext(mean(as.numeric(re)), font=2, side=3)

	# for(j in 1:10){
	# 	ydir <- file.path(dir,i,j)
	# 	nll_mat[i,j] <- as.numeric(read.table(file.path(ydir, "pt_unreported.rep"),
	# 		skip=13, nrows=1))
	# }

}

for(i in iter){
	plot(x=1,y=1, type="n", xlim=c(1,35), ylim=c(0,1500))
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1)
	re <- (best-btrue)/btrue

	lines(x=1:length(btrue), y=btrue, col="black")
	lines(x=1:length(best), y=best, col="blue", lwd=2)
	# points(x=1:length(btrue), y=re, lwd=2)
	# abline(h=0, col="red")
	mtext(mean(as.numeric(re)), font=2, side=3)

	# for(j in 1:10){
	# 	ydir <- file.path(dir,i,j)
	# 	nll_mat[i,j] <- as.numeric(read.table(file.path(ydir, "pt_unreported.rep"),
	# 		skip=13, nrows=1))
	# }

}

for(i in iter){
	plot(x=1,y=1, type="n", xlim=c(1,35), ylim=c(0,4))
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1))
	bmsy_true <- true$BMSY
    bmsyest <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"),
		skip=7, nrows=1))
    bbmsyest <- best/bmsyest
    bbmsytrue <- btrue/bmsy_true

	lines(x=1:length(btrue), y=bbmsytrue, col="black")
	lines(x=1:length(best), y=bbmsyest, col="blue", lwd=2)
	# points(x=1:length(btrue), y=re, lwd=2)
	# abline(h=0, col="red")

	# for(j in 1:10){
	# 	ydir <- file.path(dir,i,j)
	# 	nll_mat[i,j] <- as.numeric(read.table(file.path(ydir, "pt_unreported.rep"),
	# 		skip=13, nrows=1))
	# }

}

for(i in iter){
	plot(x=1,y=1, type="n", xlim=c(1,35), ylim=c(-1,1))
	xdir <- file.path(dir, i)
	true <- readRDS(file.path(dir,i,"TRUE.rds"))
	btrue <- true$Biomass
	best <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"), 
		skip=11, nrows=1))
	bmsy_true <- true$BMSY
    bmsyest <- as.numeric(read.table(file.path(xdir, "pt_unreported.rep"),
		skip=7, nrows=1))
    bbmsyest <- best/bmsyest
    bbmsytrue <- btrue/bmsy_true
    relerr <- (bbmsyest-bbmsytrue)/bbmsytrue

	lines(x=1:length(btrue), y=relerr, col="black", lwd=2)
	# points(x=1:length(btrue), y=re, lwd=2)
	abline(h=0, col="red")

	# for(j in 1:10){
	# 	ydir <- file.path(dir,i,j)
	# 	nll_mat[i,j] <- as.numeric(read.table(file.path(ydir, "pt_unreported.rep"),
	# 		skip=13, nrows=1))
	# }

}


