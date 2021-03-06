% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bioenergmod.mc.R
\name{bioenergmod.mc}
\alias{bioenergmod.mc}
\title{Bioenergetics model with Monte Carlo simulation}
\usage{
bioenergmod.mc(nsim, energyneed, tothabitat, energysim, floodsim, accessiblesim,
  wetsplit = TRUE, minday = NULL)
}
\arguments{
\item{nsim}{Number of simulations to perform; can be any number up to the maximum number of resamples provided in \code{energysim}, \code{floodsim}, and \code{accessiblesim}}

\item{energyneed}{Daily energy requirements of the population}

\item{tothabitat}{Data frame with two columns: "habitat" giving the names of each land cover type of interest and "area" giving the total area potentially available}

\item{energysim}{Resampled energy density estimates for each land cover type in columns; see Details}

\item{floodsim}{List of resampled proportion open water estimates for each land cover type; see Details}

\item{accessiblesim}{Optional; list of resampled proportion accessible open water estimates for each land cover type; see Details}

\item{wetsplit}{Optional; if \code{TRUE}, split total area of open water labeled "wetlands" into seasonal vs. permanent; see Details}

\item{minday}{Optional; use to specify the earliest time step at which land cover types other than wetlands should be considered accessible}
}
\value{
Returns a named list of arrays providing model outputs for each time step (rows), land cover type (columns), and simulation (dim3)

1) \code{energy}: summary results, including for each time step: daily energy required, total energy supply, total energy accessible, total energy consumed, and total energy shortfall

2) \code{energy.supply}: energy supply provided by each land cover type at each time step

3) \code{energy.accessible}: energy accessible provided by each land cover type at each time step

4) \code{energy.consumed}: energy consumed in each land cover type at each time step (assumed to be proportional to energy accessible)
}
\description{
Run bioenergetics model repeatedly with resampled parameters to quantify uncertainty or sensitivity.
}
\details{
This function takes many resamples of energy density, proportion open water, and optionally proportion accessible as inputs to the bioenergetics model, allowing quantification of the uncertainty in the model outputs or their sensitivity to changes in individual parameters.

All resampled parameters should contain estimates for each simulation in columns. \code{energysim} contains an extra column called "habitat" giving land cover type names matching those in \code{tothabitat}. \code{floodsim} and \code{accessiblesim} are named lists with elements for each land cover type; each list contains estimates for each time step (rows) and simulation (columns).

The \code{wetsplit} option is useful if proportion accessible varies by wetland type (seasonal vs. permanent) but proportion flooded is only available for all wetlands. If \code{wetsplit=TRUE}, \code{accessiblesim} must contain resampled estimates of proportion accessible for "seas" and "perm" land cover types. Also, "prop.perm" will be estimated for each simulation of wetlands flooding curves, reflecting the current pattern of permanent wetlands flooding in the Central Valley. NOTE: This section of code should be customized for other applications!

Units for \code{tothabitat}, \code{energysim}, and \code{energyneed} are assumed to be internally consistent, i.e. if energy density estimates in \code{energysim} are provided in kJ/ha, \code{tothabitat} should be in ha and \code{energyneed} should be in kJ.
}
\examples{
## Total habitat and energy need do not vary:
tothabitat = data.frame(habitat=c('A','B','C'), area=c(10000,15000,30000))
energyneed = calculate_energy_demand(n=c(100,150,200), bodymass=c(0.5,0.6,0.7),
   metabolism='FMR', assimilation=0.73)

## Resample energy density only (use mean values for all other parameters):
energydens = data.frame(habitat=c('A','B','C'), mean=c(10,8,12), sd=c(0.1,0.2,0.3))
energysim = lapply(c(1:nrow(energydens)), function(x) {
   rnorm(10, mean=energydens$mean[x], sd=energydens$sd[x])})
energysim = as.data.frame(do.call(rbind, energysim))
energysim$habitat = energydens$habitat
energysim = energysim[,c(11,1:10)]

flood = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), time=rep(c(1:3),3),
   value=c(0.9,0.8,0.7, 0.1,0.15,0.2, 0.5,0.5,0.5), sd=c(rep(0.1,3), rep(0.2,3), rep(0.05,3)))
floodsim = lapply(c(1:nrow(flood)), function(x) {
   rnorm(10, mean=flood$value[x], sd=flood$sd[x])})
floodsim = as.data.frame(do.call(rbind, floodsim))
floodsim = cbind(flood[,c('habitat','time')], floodsim)
floodsim = split(floodsim, f=floodsim$habitat) #put in list form
floodsim = lapply(floodsim, function(x) {x=x[,3:ncol(x)]})

accessible = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), time=rep(c(1:3),3),
   value=rep(0.9,9), sd=c(rep(0.1,3), rep(0.2,3), rep(0.05,3)))
accessiblesim = lapply(c(1:nrow(accessible)), function(x) {
   rnorm(10, mean=accessible$value[x], sd=accessible$sd[x])})
accessiblesim = as.data.frame(do.call(rbind, accessiblesim))
accessiblesim = cbind(accessible[c('habitat','time')], accessiblesim)
accessiblesim = split(accessiblesim, f=accessiblesim$habitat)
accessiblesim = lapply(accessiblesim, function(x) {x=x[,3:ncol(x)]})

results = bioenergmod.mc(nsim=10, energyneed=energyneed, tothabitat=tothabitat,
   energysim=energysim, floodsim=floodsim, accessiblesim=accessiblesim, wetsplit=FALSE)

## find median daily energy accessible across all iterations and plot result
a = as.data.frame(apply(results$energy.accessible[,2:ncol(results$energy.accessible),],
     MARGIN=c(1,2), median))
a = cbind(time=results$energy.accessible[,'time',1], a)
plot_bioenergmod(a, ylab='kJ (thousands)', scale=1000, der=energyneed)
}
\author{
Kristen Dybala, kdybala@pointblue.org
}
\seealso{
\code{\link{run_bioenergmod_loop}}, \code{\link{plot_bioenergmod}}
}

