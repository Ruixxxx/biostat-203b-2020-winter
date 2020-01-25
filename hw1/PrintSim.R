## collect simulation results from output files and print average MSEs in a table

datalist <- c()
info <- c()
N <- c()

# sample sizes/ distributions
nVals <- seq(100, 500, by=100)
distTypes <- c("gaussian", "t1", "t5")

for (dist in distTypes){
  for (n in nVals){
    filename <- paste("n_", n, "_dist_", dist, ".txt", sep="")
    datalist <- read.delim(filename)
    datalist <- datalist[ , 2]
    info <- c(info, datalist)
    # the first column of the table
    N <- c(N, rep(n, 2))
  }
}
Num <- N[1: 10]
# the second column of the table
Method <- c("PrimeAvg", "SampleAvg")
Method <- rep(Method, 5)
# the table
table <- data.frame(n = Num, Method = Method, Gaussian = info[1: 10], t_5 = info[21: 30], t_1 = info[11: 20])

# print the table
print(table)

