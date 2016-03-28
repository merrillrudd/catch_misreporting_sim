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
### models to run
###############################################
rmodel_vec_names <- c("100% Reporting", "Constant Under-reporting",  "Constant Over-reporting", "Increasing Reporting", "Decreasing Reporting") 

###############################################
### read results
###############################################
if(file.exists(file.path(init_dir, "results"))==FALSE) dir.create(file.path(init_dir, "results"))
res_dir <- file.path(init_dir, "results")

term_2way_startalt <- readRDS(file.path(res_dir, "2way-results-terminal-startalt.rds"))

png(file.path(res_figs, "Paper_Fig_Terminal_stochastic_Alt.png"), width=1300, height=800)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure2.pdf"), width=18, height=14)
Paper_Fig_Terminal(results=term_2way_startalt, deterministic=FALSE, relerr=2)
dev.off()

png(file.path(res_figs, "Paper_Fig_Project_deterministic_Alt.png"), width=1200, height=800)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, constantTAC=FALSE)
dev.off()

pdf(file.path(res_figs, "Rudd_and_Branch_Figure3.pdf"), width=18, height=14)
Paper_Fig_Project(results=term_2way_startalt, run_project=TRUE, start_sim="Alt", nproject=15, constantTAC=FALSE)
dev.off()

