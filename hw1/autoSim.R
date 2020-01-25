# autoSim.R

# sample sizes/ distributions/ seed/ rep
nVals <- seq(100, 500, by=100)
distTypes <- c("gaussian", "t1", "t5")
seed <- 203
rep <-50

# run simulations and write output to files
for (dist in distTypes) {
  for (n in nVals){
    arg <- paste("n=", n, " dist=", shQuote(shQuote(dist)), " seed=", seed, " rep=", rep, sep="")
    oFile <- paste("n_", n, "_dist_", dist, ".txt", sep="")
    sysCall <- paste("nohup Rscript runSim.R ", arg, " > ", oFile, sep="")
    system(sysCall, wait = FALSE)
    print(paste("sysCall=", sysCall, sep=""))
  }
}
