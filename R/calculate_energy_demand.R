#' Calculate daily energy requirements
#'
#' Calculates total daily energy requirement (DER) for a population based on an allometric scaling equation developed for shorebirds (Kersten and Piersma 1987).
#'
#' @param n Number of individuals in the population; vector of values for each time step
#' @param bodymass Average body mass (kg) of individuals in the population; vector of values for each time step
#' @param metabolism Type of metabolic rate to use: \code{"FMR"} for field metabolic rate or \code{"RMR"} for resting metabolic rate; see Details
#' @param assimilation Estimated assimilation efficiency of energy consumed; defaults to 0.73 estimated for shorebirds (Castro et al. 1989)
#' @param adjust Optional; allows manual adjustment of DER between time steps \code{t1} and \code{t2} by adding a specified percentage of the calculated DER; see Details
#' @param t1 Optional; starting time step for adjustment
#' @param t2 Optional; ending time step for adjustment
#' @param plot Logical; if \code{TRUE}, also returns a plot of daily energy requirements for convenience
#'
#' @details Daily energy requirements (DER) for a population are calculated based on allometric scaling equations, requiring only an estimate of average body mass (kg) and the number of individuals in the population. The function includes four equations:
#'
#' (1) RMR = 437*m^0.729, where RMR is resting metabolic rate and m is body mass (kg), derived for shorebirds (Kersten and Piersma 1987)
#'
#' (2) FMR = 3 * RMR, where FMR is field metabolic rate, based on comparisons of lab and field animals, and taking into account additional energy expended due to daily activities and thermoregulation (Kersten and Piersma 1987)
#'
#' (3) DEI = FMR/assimilation efficiency, where DEI is daily energy intake, because not all energy consumed is assimilated; if RMR is selected, equation is instead DEI = RMR/assimilation efficiency
#'
#' (4) DER = DEI * n, where n is the number of individuals in the population
#'
#' To manually adjust the calculated DER up or down during certain time steps, e.g. increasing by a percentage prior to a migration window, use the adjust parameter to specify the percentage change (values between 0 and 1) and specify the time steps.
#'
#' @return Returns vector of daily energy requirements (kJ) for the population at each time step.
#'
#' @author Kristen Dybala, \email{kdybala@@pointblue.org}
#'
#' @references
#' Brand LA, Takekawa JY, Shinn J, Graham T, Buffington K, Spring S, Miles K. 2013. Effects of wetland management on carrying capacity of duck and shorebird benthivores in a coastal estuary. Vallejo, CA: U.S. Geological Survey, Western Ecological Research Center.
#'
#' Castro G, Stoyan N, Myers JP. 1989. Assimilation efficiency in birds: A function of taxon or food type? Comp. Biochem. Physiol. 92A:271-278.
#'
#' Kersten M, Piersma T. 1987. High levels of energy expenditure in shorebirds; Metabolic adaptations to an energetically expensive way of life. Ardea 75:175-187.
#'
#' @examples
#' calculate_energy_demand(n=c(100,150,200), bodymass=c(0.5,0.6,0.7),
#'    metabolism='FMR', assimilation=0.73)
#' calculate_energy_demand(n=c(100,150,200), bodymass=c(0.5,0.6,0.7),
#'    metabolism='FMR', assimilation=0.73, adjust=0.25, t1=3, t2=3)
#'
#' @export
#'
calculate_energy_demand = function(n, bodymass, metabolism='FMR', assimilation=0.73, adjust=NULL, t1, t2, plot=TRUE) {
  ## resting metabolic rate depends on body mass (kg)
  RMR = 437*bodymass^.729 # Equation 1 (from Kersten and Piersma 1987, Brand et al. 2013)

  ## field metabolic rate is higher than RMR
  FMR = RMR*3 # Equation 2 (from Kersten and Piersma 1987, Brand et al. 2013)

  ## daily energy intake is higher than RMR or FMR to account for assimilation efficiency
  if (metabolism=='RMR') {
    DEI = RMR/assimilation
  } else if (metabolism=='FMR') {
    DEI = FMR/assimilation # Equation 3 (Castro et al. 1989, Brand et al. 2013)
  } else (stop('metabolism should be FMR or RMR'))

  ## daily energy requirement of entire population
  DER = DEI*n # Equation 4

  ## adjustments
  if (!is.null(adjust)) {
    DER2 = DER
    if (t2>t1) {
      DER2[t1:t2] = DER[t1:t2] + adjust*DER[t1:t2]
    } else if (t2==t1) {
      DER2[t1] = DER[t1] + adjust*DER[t1]
    }

    if (plot==TRUE) {
      tmp = data.frame(time=c(1:length(n)), DER=DER, DER2=DER2)
      ggplot(tmp, aes(x=time, y=DER2/1000000)) + geom_line() + ylab('kj (millions)') +
        geom_line(aes(y=DER), linetype='dashed')
    }
    return(DER2)
  } else {
    if (plot==TRUE) {
      tmp = data.frame(time=c(1:length(n)), DER=DER)
      ggplot(tmp, aes(x=time, y=DER/1000000)) + geom_line() + ylab('kj (millions)')
      return(DER)
    }
  }
}
