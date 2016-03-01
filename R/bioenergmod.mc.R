#' Bioenergetics model with Monte Carlo simulation
#'
#' Run bioenergetics model repeatedly with resampled parameters to quantify uncertainty or sensitivity.
#'
#' @param tothabitat Data frame with two columns: "habitat" giving the names of each land cover type of interest and "area" giving the total area potentially available
#' @param energysim Resampled energy density estimates for each land cover type in columns; see Details
#' @param floodsim List of resampled proportion open water estimates for each land cover type; see Details
#' @param accessiblesim Optional; list of resampled proportion accessible open water estimates for each land cover type; see Details
#' @param wetsplit Optional; if \code{TRUE}, split total area of open water labeled "wetlands" into seasonal vs. permanent; see Details
#' @param minday Optional; use to specify the earliest time step at which land cover types other than wetlands should be considered accessible
#' @param energyneed Daily energy requirements of the population
#' @param nsim Number of simulations to perform; can be any number up to the maximum number of resamples provided in \code{energysim}, \code{floodsim}, and \code{accessiblesim}
#'
#' @details This function takes many resamples of energy density, proportion open water, and optionally proportion accessible as inputs to the bioenergetics model, allowing quantification of the uncertainty in the model outputs or their sensitivity to changes in individual parameters.
#'
#' All resampled parameters should contain estimates for each simulation in columns. \code{energysim} contains an extra column called "habitat" giving land cover type names matching those in \code{tothabitat}. \code{floodsim} and \code{accessiblesim} are named lists with elements for each land cover type; each list contains estimates for each time step (rows) and simulation (columns).
#'
#' The \code{wetsplit} option is useful if proportion accessible varies by wetland type (seasonal vs. permanent) but proportion flooded is only available for all wetlands. If \code{wetsplit=TRUE}, \code{accessiblesim} must contain resampled estimates of proportion accessible for "seas" and "perm" land cover types. Also, "prop.perm" will be estimated for each simulation of wetlands flooding curves, reflecting the current pattern of permanent wetlands flooding in the Central Valley. NOTE: This section of code should be customized for other applications!
#'
#' Units for \code{tothabitat}, \code{energysim}, and \code{energyneed} are assumed to be internally consistent, i.e. if energy density estimates in \code{energysim} are provided in kJ/ha, \code{tothabitat} should be in ha and \code{energyneed} should be in kJ.
#'
#' @return Returns a named list of arrays providing model outputs for each time step (rows), land cover type (columns), and simulation (dim3)
#'
#' 1) \code{energy}: summary results, including for each time step: daily energy required, total energy supply, total energy accessible, total energy consumed, and total energy shortfall
#'
#' 2) \code{energy.supply}: energy supply provided by each land cover type at each time step
#'
#' 3) \code{energy.accessible}: energy accessible provided by each land cover type at each time step
#'
#' 4) \code{energy.consumed}: energy consumed in each land cover type at each time step (assumed to be proportional to energy accessible)
#'
#' @author Kristen Dybala, kdybala@@pointblue.org
#'
#' @seealso \code{\link{run_bioenergmod_loop}}, \code{\link{plot_bioenergmod}}
#'
#' @examples
#' ## Total habitat and energy need do not vary:
#' tothabitat = data.frame(habitat=c('A','B','C'), area=c(10000,15000,30000))
#' energyneed = calculate_energy_demand(n=c(100,150,200), bodymass=c(0.5,0.6,0.7),
#'    metabolism='FMR', assimilation=0.73)
#'
#' ## Resample energy density only (use mean values for all other parameters):
#' energydens = data.frame(habitat=c('A','B','C'), mean=c(10,8,12), sd=c(0.1,0.2,0.3))
#' energysim = lapply(c(1:nrow(energydens)), function(x) {
#'    rnorm(10, mean=energydens$mean[x], sd=energydens$sd[x])})
#' energysim = as.data.frame(do.call(rbind, energysim))
#' energysim$habitat = energydens$habitat
#' energysim = energysim[,c(11,1:10)]
#'
#' flood = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), time=rep(c(1:3),3),
#'    value=c(0.9,0.8,0.7, 0.1,0.15,0.2, 0.5,0.5,0.5), sd=c(rep(0.1,3), rep(0.2,3), rep(0.05,3)))
#' floodsim = lapply(c(1:nrow(flood)), function(x) {
#'    rnorm(10, mean=flood$value[x], sd=flood$sd[x])})
#' floodsim = as.data.frame(do.call(rbind, floodsim))
#' floodsim = cbind(flood[,c('habitat','time')], floodsim)
#' floodsim = split(floodsim, f=floodsim$habitat) #put in list form
#' floodsim = lapply(floodsim, function(x) {x=x[,3:ncol(x)]})
#'
#' accessible = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), time=rep(c(1:3),3),
#'    value=rep(0.9,9), sd=c(rep(0.1,3), rep(0.2,3), rep(0.05,3)))
#' accessiblesim = lapply(c(1:nrow(accessible)), function(x) {
#'    rnorm(10, mean=accessible$value[x], sd=accessible$sd[x])})
#' accessiblesim = as.data.frame(do.call(rbind, accessiblesim))
#' accessiblesim = cbind(accessible[c('habitat','time')], accessiblesim)
#' accessiblesim = split(accessiblesim, f=accessiblesim$habitat)
#' accessiblesim = lapply(accessiblesim, function(x) {x=x[,3:ncol(x)]})
#'
#' results = bioenergmod.mc(nsim=10, energyneed=energyneed, tothabitat=tothabitat,
#'    energysim=energysim, floodsim=floodsim, accessiblesim=accessiblesim, wetsplit=FALSE)
#'
#' ## find median daily energy accessible across all iterations and plot result
#' a = as.data.frame(apply(results$energy.accessible[,2:ncol(results$energy.accessible),],
#'      MARGIN=c(1,2), median))
#' a = cbind(time=results$energy.accessible[,'time',1], a)
#' plot_bioenergmod(a, ylab='kJ (thousands)', scale=1000, der=energyneed)
#' @export
#'

bioenergmod.mc = function(nsim, energyneed, tothabitat, energysim, floodsim, accessiblesim, wetsplit=TRUE, minday=NULL)

{ results = pbapply::pblapply(c(1:nsim), function(i) {
    ## for each iteration of Monte Carlo simulation:
    ## 1) extract resampled parameters for the iteration
    if (ncol(energysim)==2) { #no resamples of energysim
      energy = energysim[,c('habitat','value')]
    } else {
      energy = data.frame(habitat=energysim$habitat, value=energysim[,(i+1)])
    }

    depth = lapply(c(1:length(accessiblesim)), function(x,i) {
      if (is.null(dim(accessiblesim[[x]]))) {accessiblesim[[x]][i]} else {accessiblesim[[x]][,i]}
      }, i=i)
    depth = as.data.frame(do.call(cbind, depth))
    names(depth) = names(accessiblesim)
    depth$time = c(1:dim(depth)[1])
    depth = reshape2::melt(depth, id.vars='time', variable.name='habitat', value.name='value')

    flood = lapply(c(1:length(floodsim)), function(x,i) {
      if (is.null(dim(floodsim[[x]]))) {floodsim[[x]][i]} else {floodsim[[x]][,i]}
      }, i=i)
    flood = as.data.frame(do.call(cbind, flood))
    names(flood) = names(floodsim)
    flood$time = c(1:dim(flood)[1])

    if (wetsplit==TRUE) { ## estimate prop.perm from resampled wetlands flooding curve
      ## Note: this section of code is specific to the Central Valley pattern of permanent wetlands flooding -- change/customize for other applications!
      flood$prop.perm = NA
      flood$prop.perm[1:34] = 1
      flood$prop.perm[124:nrow(flood)] = tothabitat$area[tothabitat$habitat=='perm']/(flood$wetlands[124:nrow(flood)]*sum(tothabitat$area[tothabitat$habitat %in% c('seas','perm')]))
      flood$prop.perm[35:107] = (flood$wetlands[34]*sum(tothabitat$area[tothabitat$habitat %in% c('seas','perm')]))/(flood$wetlands[35:107]*sum(tothabitat$area[tothabitat$habitat %in% c('seas','perm')]))
      flood$prop.perm[108:123] = spline(x=flood$time, y=flood$prop.perm, xout=c(108:123), method='natural')$y
      flood$prop.perm[flood$prop.perm>1]=1
      flood = reshape2::melt(flood, id.vars=c('time','prop.perm'), variable.name='habitat', value.name='value')
      flood$prop.perm[flood$habitat != 'wetlands']=NA
    } else {
      flood = reshape2::melt(flood, id.vars=c('time'), variable.name='habitat', value.name='value')
    }

    ## 2) calculate habitat change
    change = calculate_habitat_change(tothabitat = tothabitat[tothabitat$habitat %in% flood$habitat,],
                                      flood = flood, time='time', value='value', accessible = depth, wetsplit=wetsplit)
    if (!is.null(minday)) { #earliest time step at which land cover types other than wetlands considered accessible
      list = names(change$accessible)
      list = list[-which(list %in% c('time','wetlands','seas','perm'))]
      change$prop.accessible[1:(minday-1),which(colnames(change$prop.accessible) %in% list)] = 0
      change$accessible[1:(minday-1),which(colnames(change$accessible) %in% list)] = 0
      change$openwater[1:(minday-1),which(colnames(change$openwater) %in% list)] = 0
      change$added[1:(minday-1),which(colnames(change$added) %in% list)] = 0
      change$returned[1:(minday-1),which(colnames(change$returned) %in% list)] = 0
    }

    ## step 3: run bioenergetics loop
    model = run_bioenergmod_loop(energyneed = energyneed, energydens = energy, habitat.available = change$openwater,
                                 habitat.accessible = change$accessible, habitat.added = change$added,
                                 habitat.returned = change$returned, prop.accessible = change$prop.accessible)
    return(model)
  })
  final = list()
  final$energy = do.call(abind::abind, list(lapply(results, function(x) x[["energy"]]), along=3))
  final$energy.supply = do.call(abind::abind, list(lapply(results, function(x) x[["energy.supply"]]), along=3))
  final$energy.accessible = do.call(abind::abind, list(lapply(results, function(x) x[["energy.accessible"]]), along=3))
  final$energy.consumed = do.call(abind::abind, list(lapply(results, function(x) x[["energy.consumed"]]), along=3))
  final$energy.lost = do.call(abind::abind, list(lapply(results, function(x) x[["energy.lost"]]), along=3))
  return(final)
}
