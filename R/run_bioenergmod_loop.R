#' Bioenergetics model
#'
#' Calculate daily energy supply, accounting for dynamic habitat availability and consumption in previous time steps
#'
#' @param energyneed Daily energy required by the population
#' @param energydens Energy density estimates for each land cover type
#' @param habitat.available Total area of each land cover type available during each time step
#' @param habitat.accessible Total area of each land cover type available and accessible during each time step
#' @param habitat.added Total area of each land cover type newly added since the last time step
#' @param habitat.returned Total area of each land cover type lost before the start of the next time step
#' @param prop.accessible Optional; proportion of open water in each land cover type that is accessible during each time step
#'
#' @details The bioenergetics model proceeds iteratively through each time step, calculating total energy supply available and accessible, energy consumed to meet the daily energy requirements, energy shortfalls (if any), and energy leftover (if any) and carried forward to the next time step. See Dybala et al. (2016) for details of the model structure.
#'
#' The 4 habitat input parameters (and optionally \code{prop.accessible}) are intended to be calculated first through a call to the \code{calculate_habitat_change} function. Units for \code{energyneed}, \code{energydens}, and the 4 habitat inputs are assumed to be internally consistent, i.e. if \code{energydens} is provided in kJ/ha, \code{energyneed} should be in kJ and the habitat inputs should be in ha.
#'
#' @return Returns a named list of data frames providing model outputs for each time step (rows) and land cover type (columns):
#'
#' 1) \code{energy}: summary results, including for each time step: daily energy required, total energy supply, total energy accessible, total energy consumed, and total energy shortfall
#'
#' 2) \code{energy.supply}: energy supply provided by each land cover type at each time step
#'
#' 3) \code{energy.accessible}: energy accessible provided by each land cover type at each time step
#'
#' 4) \code{energy.consumed}: energy consumed in each land cover type at each time step (assumed to be proportional to energy accessible)
#'
#' 5) \code{energy.lost}: energy lost at the end of each time step due to habitat becoming unavailable before the start of the next time step; energy is not consumed and effectively wasted
#'
#' @examples
#' energyneed = calculate_energy_demand(n=c(100,150,200), bodymass=c(0.5,0.6,0.7),
#'    metabolism='FMR', assimilation=0.73)
#' energydens = data.frame(habitat=c('A','B','C'), value=c(1,2,3))
#' tot = data.frame(habitat=c('A','B','C'), area=c(100,200,300))
#' flood = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), yday=rep(c(1:3),3),
#'    value=c(0.9,0.8,0.7, 0.1,0.15,0.2, 0.5,0.5,0.5))
#' depth = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), yday=rep(c(1:3),3),
#'    value=rep(0.9,9))
#' change = calculate_habitat_change(tothabitat=tot, flood=flood, time='yday',
#'    value='value', accessible=depth, wetsplit=FALSE)
#' results = run_bioenergmod_loop(energyneed=energyneed, energydens=energydens,
#'    habitat.available=change$openwater, habitat.accessible=change$accessible,
#'    habitat.added=change$added, habitat.returned=change$returned,
#'    prop.accessible=change$prop.accessible)
#'
#' @seealso \code{\link{bioenergmod.mc}} for Monte Carlo simulation, \code{\link{calculate_habitat_change}} for calculating habitat input parameters, and \code{\link{calculate_energy_demand}} for estimating daily energy required by a population
#'
#' @export
#'
run_bioenergmod_loop = function(energyneed, energydens, habitat.available, habitat.accessible, habitat.added, habitat.returned, prop.accessible=NULL) {
  ## set up empty data frames to be filled in by function, with landcover types in columns and time steps in rows
  energy.supply = habitat.added
  energy.supply[,2:ncol(energy.supply)] = 0
  energy.density = energy.supply
  energy.accessible = energy.supply
  energy.consumed = energy.supply
  energy.density.remaining = energy.supply
  energy.lost = energy.supply
  energy.remaining = energy.supply

  ## master DF of total energy need vs. total supply, total accessible, and any shortfalls
  energy = data.frame(time=habitat.added$time, DER=energyneed, supply=0, accessible=0, shortfall=0)

  ## [EQUATION 9 - partial] energy.added = habitat.added * energy.density
  energy.added = reshape2::melt(habitat.added, id.vars='time', variable.name='habitat')
  energy.added = merge(energy.added, energydens, by='habitat')
  energy.added$value = energy.added$value.x*energy.added$value.y
  energy.added = reshape2::dcast(energy.added[,c('habitat','time','value')], time~habitat, value.var='value')

  ## run loop through each time step to carry forward any leftover energy at the end of each time step
  for (t in c(1:nrow(energy.supply))) {
    ## for each land cover type (in columns) during each time step (rows):

    ## [EQUATION 9 - remainder] energy.supply = energy.remaining (from previous time step, if any) + energy.added (from any newly flooded areas)
    if (t==1) { ## if first time step, none leftover from previous time step (assume anything flooded on first time step is newly flooded)
      energy.supply[t,2:ncol(energy.supply)] = energy.added[t,2:ncol(energy.added)]
    } else {
      energy.supply[t,2:ncol(energy.supply)] = energy.added[t,2:ncol(energy.added)] + energy.remaining[t-1,2:ncol(energy.remaining)]
    }
    ## [EQUATION 10] energy.density = energy.supply/habitat.available
    energy.density[t,2:ncol(energy.density)] = energy.supply[t,2:ncol(energy.supply)]/habitat.available[t,2:ncol(habitat.available)]

    ## total energy supply during each time step = sum of energy supply from each habitat type
    energy$supply[t] = sum(energy.supply[t,2:ncol(energy.supply)])

    ## [EQUATION 11] energy.accessible = energy.supply * proportion accessible (e.g. proportion suitable depth)
    if (!is.null(prop.accessible)) {
      energy.accessible[t,2:ncol(energy.accessible)] = energy.supply[t,2:ncol(energy.supply)] * prop.accessible[t,2:ncol(prop.accessible)]
    } else { #if no accessible info provided, just assume all is accessible
      energy.accessible[t,2:ncol(energy.accessible)] = energy.supply[t,2:ncol(energy.supply)]
    }
    # total energy supply accessible during each time step = sum of energy supply accessible from each habitat type
    energy$accessible[t] = sum(energy.accessible[t,2:ncol(energy.accessible)])

    ## [EQUATION 12] energy.consumed = daily energy required * proportion of accessible energy provided by each land cover type (assume ideal free distribution)
    if (energy$accessible[t]>=energy$DER[t]) { #enough energy accessible to meet demand
      energy.consumed[t,2:ncol(energy.consumed)] = energy$DER[t]*(energy.accessible[t,2:ncol(energy.accessible)]/energy$accessible[t])
    } else { #consume everything accessible + calculate any shortfall
      energy.consumed[t,2:ncol(energy.consumed)] = energy.accessible[t,2:ncol(energy.accessible)]
      ##[EQUATION 13] energy shortfall:
      energy$shortfall[t] = energy$DER[t]-energy$accessible[t]
    }

    ## [EQUATION 14] energy.density.remaining (after consumption) = (energy.accessible - energy.consumed)/habitat.accessible
    energy.density.remaining[t,2:ncol(energy.density.remaining)] = (energy.accessible[t,2:ncol(energy.accessible)] - energy.consumed[t,2:ncol(energy.consumed)])/habitat.accessible[t,2:ncol(habitat.accessible)]
    energy.density.remaining[t,2:ncol(energy.density.remaining)][habitat.accessible[t,2:ncol(habitat.accessible)]==0] = 0 ## in case dividing by zero

    ## [EQUATION 15] energy.lost (additional energy lost due to loss of open water area) = energy.density.remaining * habitat.returned
    if (any(habitat.returned[t,2:ncol(habitat.returned)] < habitat.accessible[t,2:ncol(habitat.accessible)])) {
      v = which(habitat.returned[t,2:ncol(habitat.returned)] < habitat.accessible[t,2:ncol(habitat.accessible)])+1
      energy.lost[t,v] = energy.density.remaining[t,v] * habitat.returned[t,v]
    }
    if (any(habitat.returned[t,2:ncol(habitat.returned)] > habitat.accessible[t,2:ncol(habitat.accessible)])) {
      ## lose additional (unconsumed) energy from the inaccessible areas
      v = which(habitat.returned[t,2:ncol(habitat.returned)] > habitat.accessible[t,2:ncol(habitat.accessible)])+1
      energy.lost[t,v] = (energy.density.remaining[t,v]*habitat.accessible[t,v]) + energy.density[t,v]*(habitat.returned[t,v]-habitat.accessible[t,v])
    }

    ## [EQUATION 16] final energy.remaining = energy.supply - energy.consumed - energy.lost (could multiply by a growth rate here, if desired)
    energy.remaining[t,2:ncol(energy.remaining)] = energy.supply[t,2:ncol(energy.supply)]-energy.consumed[t,2:ncol(energy.consumed)]-energy.lost[t,2:ncol(energy.lost)]
    ## --> reset to zero when it goes negative -- assume no deficit can be carried forward
    energy.remaining[t,2:ncol(energy.remaining)][energy.remaining[t,2:ncol(energy.remaining)]<0] = 0
  }
  return(list(energy=energy, energy.supply=energy.supply, energy.accessible=energy.accessible, energy.consumed=energy.consumed, energy.lost=energy.lost))
}
