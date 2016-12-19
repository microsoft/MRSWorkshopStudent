




#############################
# Summary Statistics
#############################

# rxSummary is the primary function for summarizing numeric
# variables, and it works like summary functions in many other
# languages.
# One twist: rxSummary uses R's formula syntax to identify which
# variables should be summarized. Put all the variables you'd
# like to summarize on the right-hand side of the formula, as if
# they were predictors.
rxSummary( ~ arr_delay + dayOfWeek_F, data = flightsXdf)





###################
# Exercise 7: Summarize some variables
###################

# 1. Try summarizing a few different variables in flightsXdf.
# 2. Summarize arr_delay for each level of one of the factors.
















# You can get quantiles using rxQuantile - but only one variable
# at a time. Median flight was four minutes early!
rxQuantile(varName = "arr_delay", data = flightsXdf)







#############################
# Categorical Variables
#############################




# To summarize quantitive values, use rxCrossTabs or rxCube.
# Same syntax and functionality, different outputs. Compare:
rxCrossTabs( ~ origin_F : dest_F, data = flightsXdf)

rxCube( ~ origin_F : dest_F, data = flightsXdf)


# Note that the right-hand variables are separated by colons
# instead of plus signs! This is to highlight that crosstabs are
# about the relationship between the two variables, not their
# relationship to some dependent variable.






###################
# Exercise 8: Crosstabs
###################

# Use rxDataStep and either rxCrossTabs or rxCube to answer: 
# How many flights
#  - left Newark (origin == "EWR")
#  - arrived in Tulsa (dest == "TUL")
#  - with an arrival delay of more than five minutes?











#######################
# Predicting Arrival Delay
#######################

# Model syntax in MRS is usually very similar to open-source R.
# Here's a simple linear regression predicting arrival delay:
mod1 <- rxLinMod(arr_delay ~ origin_F, data = flightsXdf)

summary(mod1)


# This model object can be saved for later use
save(mod1, file = "model.Rdata")


# And used for prediction
newData <- data.frame(origin_F = factor(c("EWR", "LGA", "JFK"),
                                        levels = c("EWR", "LGA", "JFK")))

rxPredict(mod1, data = newData)





###################
# Exercise 9: Build a Model
###################


# 1. Expand the linear regression above into a more robust model.
# 2. Try one of the other algorithms for predicting arrival delay
# (see help("RevoScaleR") and scroll down for a list)
# 3. Load "flights_q234.csv" into an XDF, and test your model











#######################
# Intro to Parallel Computing
#######################


set.seed(42)
A <- matrix(rnorm(1000000), 1000)

## Check CPU time for unparallel matrix multiplication:

system.time(A %*% A) # not parallel

library(snow)
cl <- makeSOCKcluster(names = 3L)
system.time(parMM(cl, A, A))
stopCluster(cl)







# let's compare the performance of HPA functions with their open-source R couterparts:
# rxLinMod vs lm
# rxLogit vs glm
# rxGlm vs glm
# rxSummary vs dplyr

# we will read the airline.csv data set and load it into R as a data.frame
# since open-source R functions do not work on XDF files, we won't be using XDF for these tests

sample.data.dir <- rxGetOption("sampleDataDir")

airline.csv <- file.path(sample.data.dir, "AirlineDemoSmall.csv")

colInfo <- list(
  DayOfWeek = list(
    type = "factor",
    levels = c("Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday", "Sunday"))
)

airline.df <- rxImport(inData = airline.csv, # no outFile means we get a data.frame
                       colInfo = colInfo,
                       missingValueString = "M")

dim(airline.df)



# rxLinMod vs lm

model <- lm(ArrDelay ~ DayOfWeek, data = airline.df)
summary(model)

model <- rxLinMod(ArrDelay ~ DayOfWeek, data = airline.df, dropFirst = TRUE)
summary(model)

library(microbenchmark)
microbenchmark(
  lm(ArrDelay ~ DayOfWeek, data = airline.df),
  rxLinMod(ArrDelay ~ DayOfWeek, data = airline.df, dropFirst = TRUE, reportProgress = 0),
  times = 30
)



# rxLogit vs glm

parallel <- function() rxLogit(ArrDelay > 10 ~ DayOfWeek, data = airline.df, dropFirst = TRUE, reportProgress = 0)
sequentl <- function() glm(ArrDelay > 10 ~ DayOfWeek, data = airline.df, family = binomial(link = 'logit'))

microbenchmark(
  parallel(),
  sequentl(),
  times = 10
)



# rxSummary vs dplyr

revo_sum <- function() rxSummary(ArrDelay ~ F(DayOfWeek), data = airline.df, reportProgress = 0)

library(dplyr)

dplyrsum <- function() {
  airline.df %>%
    group_by(DayOfWeek) %>%
    summarise(
      Means = mean(ArrDelay, na.rm = TRUE),
      StdDev = sd(ArrDelay, na.rm = TRUE),
      Min = min(ArrDelay, na.rm = TRUE),
      Max = max(ArrDelay, na.rm = TRUE),
      ValidObs = sum(!is.na(ArrDelay))
    )
}

microbenchmark(
  revo_sum(),
  dplyrsum(),
  times = 10
)



# rxGlm vs glm on small data set: glm wins

library(robust)
data(breslow.dat, package = "robust")
dim(breslow.dat)

parallel <- function() rxGlm(sumY ~ Base + Age + Trt, dropFirst = TRUE, data = breslow.dat, family = poisson(), reportProgress = 0)
sequentl <- function() glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = poisson())

# smaller data set means parallel algorithm is not necessarily faster
microbenchmark(
  parallel(),
  sequentl(),
  times = 10
)



# rxGlm vs glm on lard data set: rxGlm wins

# what if we blow up the size of the data 5000 fold
breslow.big <- do.call(rbind, lapply(1:5000, function(i) breslow.dat))
dim(breslow.big)

parallel <- function() rxGlm(sumY ~ Base + Age + Trt, dropFirst = TRUE, data = breslow.big, family = poisson(), reportProgress = 0)
sequentl <- function() glm(sumY ~ Base + Age + Trt, data = breslow.big, family = poisson())

# smaller data set means parallel algorithm is not necessarily faster
microbenchmark(
  parallel(),
  sequentl(),
  times = 10
)

rm(breslow.big)




# comparing RxLocalParallel with RxLocalSeq

rxSetComputeContext(RxLocalParallel())
rxOptions(numCoresToUse = 12)

rxExec(sqrt, rxElemArg(1:4))
# rxElemArg allows you to pass different arguments to each worker
rxExec(sqrt, 1:4, timesToRun = 4)
# timesToRun calculates the square roots of the entire sequence 1:4 four times

system('nproc')
system('cat /proc/cpuinfo')


nsize <- 10^5
system.time(rnorm(nsize))

rxSetComputeContext(RxLocalSeq())
system.time(rxExec(function(i) rnorm(nsize), rxElemArg(1:4)))

rxSetComputeContext(RxLocalParallel())
system.time(rxExec(function(i) rnorm(nsize), rxElemArg(1:4), execObjects = "nsize"))


compare.runtimes <- function(nsize, nproc) {
  cat(sprintf("size = %s \n", formatC(nsize, format = "d", big.mark = ",")))
  st1 <- system.time(rnorm(nsize))
  
  rxSetComputeContext(RxLocalSeq())
  st2 <- system.time(rxExec(function(i) rnorm(nsize), rxElemArg(1:nproc)))
  
  rxSetComputeContext(RxLocalParallel())
  st3 <- system.time(rxExec(function(i) rnorm(nsize), rxElemArg(1:nproc), execObjects = c("nsize", "nproc")))
  
  dd <- do.call(rbind, list(st1, st2, st3))
  dd <- as.data.frame(dd)
  dd$test <- c('single test', 'four sequential', 'four parallel')
  dd$factor <- dd$elapsed / lag(dd$elapsed)
  dd[ , c('test', 'elapsed', 'factor')]
}

compare.runtimes(10^4, 16)
compare.runtimes(10^5, 16)
compare.runtimes(10^6, 16)
compare.runtimes(10^7, 16)



# the mandelbrot example:

# z_{n+1} = z^2_n + c

mandelbrot <- function(x0, y0, lim) {
  x <- x0; y <- y0
  iter <- 0
  while (x^2 + y^2 < 4 && iter < lim) {
    xtemp <- x^2 - y^2 + x0
    y <- 2 * x * y + y0
    x <- xtemp
    iter <- iter + 1
  }
  iter
}

mandelbrot(0, 0, 50)
mandelbrot(2, 5, 50)

vmandelbrot <- function(xvec, y0, lim) {
  sapply(xvec, mandelbrot, y0 = y0, lim = lim)
}

vmandelbrot(0:10, 0, 50)

size <- 150
x.in <- seq(-2.0, 0.6, length.out = size)
y.in <- seq(-1.3, 1.3, length.out = size)
m <- 100
z <- rxExec(vmandelbrot, x.in, y0 = rxElemArg(y.in), m, execObjects = "mandelbrot")
z <- matrix(unlist(z), ncol = size) # order the data for the image

image(x.in, y.in, z, col = c(rainbow(m), '#000000'))

## ----, rxexec-mandelbrot, eval = TRUE--------------------------------------
z <- rxExec(vmandelbrot, x.in, y0 = rxElemArg(y.in), m, taskChunkSize = 48, execObjects = "mandelbrot")
z <- matrix(unlist(z), ncol = size) # order the data for the image
image(x.in, y.in, z, col = c(rainbow(m), '#000000'))




