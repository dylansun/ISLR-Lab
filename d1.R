source('~/Documents/elementsStasLearn/ESL-ch2/f1.R')
dat <- runif(n=1000, min=-1, max = 1)
fdat <- lapply(dat, f1)
plot(dat,fdat)
