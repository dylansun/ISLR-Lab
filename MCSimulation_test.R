# Here’s a simple example to show how it can be used.

n <- 1000  # number of simulations to run

# List described the distribution of each variable
vals <- list(list(var="Uniform",
                  dist="unif",
                  params=c(0,1)),
             list(var="Normal",
                  dist="norm",
                  params=c(0,1)),
             list(var="Weibull",
                  dist="weibull",
                  params=c(2,1)))

# Generate the sample
samp <- generateMCSample(n,vals)

# Plot with ggplot2
library(ggplot2)
samp.mt <- melt(samp,id="n")
gg <- ggplot(samp.mt,aes(x=value)) +
  geom_histogram(binwidth=0.1) +
  theme_bw() +
  facet_wrap(~variable, ncol=3,scale="free")

