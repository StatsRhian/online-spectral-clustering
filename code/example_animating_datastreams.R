#View Andy's data as an animation

rm(list = ls())
invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
nMicro <- 150

source("code/global_settings.R")

############################## Parameter settings #############################################
nDim = 3            # Dimensionality of the data
rate = 1/8000      # 1/rate = approximately the number of points needed for a class mean to complete 1 rotation
nClust = 2 # ALWAYS for Andy's Gaussians
N= 2000    # Number of data points
radius = 1         # Radius of hypersphere
ProbClassOne = 0.5 # Probaility of class twp = 1 - ProbClassOne
covar = diag(nDim)*0.005 # Noise covariance matrix

out <- generateData(dim = nDim, numPoints=N,rate=rate,radius=radius, ProbClassOne=ProbClassOne,covar=covar)
data <- out$data
trueClusters <- out$labels
modelCentres <- out$centres

plot(data, col = trueClusters)

library("animation")
saveHTML({
  myseq = seq(1, 2000, by = 50)
  for (i in 1:(length(myseq)-1)){
    plot(data[myseq[i]:myseq[i+1],], ylim = c(-1,1), xlim = c(-1,1), col = trueClusters[myseq[i]:myseq[i+1]])
    ani.pause()
  }
}, img.name = "hypersphere_data", imgdir = "animations", htmlfile = "hypersphere_data.html",
autobrowse = FALSE, title = "Hypersphere dataset")
