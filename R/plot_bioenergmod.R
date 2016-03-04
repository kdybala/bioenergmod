#' Plot bioenergetics model results
#'
#' Plot temporal variation in parameters or model outputs by land cover type
#'
#' @param x One of the list elements resulting from \code{run_bioenergmod_loop}
#' @param ylab Y axis label
#' @param scale Value by which to divide the results, for scaling
#' @param xaxislab Defaults to \code{TRUE}; if \code{FALSE}, do not show x axis labels; useful for multi-panel plots
#' @param xmax Optional value to over-ride automatic x-axis range
#' @param ymax Optional value to over-ride automatic y-axis range
#' @param der Optionally provide daily energy requirements to plot a line on top of model results for comparison
#' @param palette Optionally provide a set of colors to over-ride the defaults
#' @param levels Optionally provide a list of factor levels to control the order in which they're stacked
#' @param labels Optionally provide a list of labels to replace defaults
#'
#' @details Convenience plotting function for plotting the contribution of each land cover type to habitat availability or energy supply over all time steps as a stacked area plot. Can be used directly with the results from \code{calculate_habitat_change} or \code{run_bioenergmod_loop}, or with any data frame with time steps in rows and types in columns. To use this plot with the results of \code{bioenergmod.mc}, first summarize across all iterations of the Monte Carlo simulation (see examples).
#'
#' Note: This function is customized with parameters specific to the Central Valley Joint Venture non-breeding shorebirds project (Dybala et al. 2016), such as the land cover type and time step labels. Edit function for other applications.
#'
#' @return Prints the plot and returns a ggplot2 object
#'
#' @author Kristen Dybala, kdybala@pointblue.org
#'
#' @references Dybala KE, Reiter ME, Hickey CM, Shuford WD, Strum KM, and Yarris GS. 2016. A bioenergetics modeling approach to setting conservation objectives for non-breeding shorebirds in California's Central Valley wetlands and flooded agriculture.
#'
#' @examples
#' energyneed = calculate_energy_demand(n=c(100,150,200), bodymass=c(0.1,0.08,0.05),
#'    metabolism='FMR', assimilation=0.73)
#' energydens = data.frame(habitat=c('A','B','C'), value=c(5,10,15))
#' tot = data.frame(habitat=c('A','B','C'), area=c(10000,4000,6000))
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
#' plot_bioenergmod(results$energy.accessible, scale=1, ylab='kJ', der=energyneed)
#'
#' \dontrun{
#' ## To work with the results of bioenergmod.mc, first summarize across all
#' ## iterations of the Monte Carlo simulation:
#' results = bioenergmod.mc()
#' a = as.data.frame(apply(results$energy.accessible[,2:6,], MARGIN=c(1,2), median))
#' plot_bioenergmod(a, ylab='kJ (billions)', scale=1000000000, der=energyneed)
#' }
#'
#' @seealso \code{\link{run_bioenergmod_loop}}, \code{\link{bioenergmod.mc}}
#' @import ggplot2
#'
#' @export

plot_bioenergmod = function(x, ylab, scale=1, xaxislab=TRUE, xmax=NULL, ymax=NULL, der=NULL, palette=NULL,
                            levels=NULL, labels=NULL) {
  value=NULL
  habitat=NULL

  ## transform model results into long-form data frame
  dat = reshape2::melt(x, id.vars=c('time'), variable.name='habitat')

  if (!is.null(levels)) {
    dat$habitat = factor(dat$habitat, levels=levels)
    dat = dat[order(dat$habitat),]
  }

  p = ggplot(dat, aes(x=time, y=value/scale), environment=environment()) + geom_area(aes(fill=habitat)) + ylab(ylab) + xlab(NULL) +
    scale_y_continuous(expand=c(0,0)) + theme_classic() +
    theme(legend.title=element_blank(), legend.position=c(0,1), legend.justification=c(0,1), legend.text=element_text(size=9),
          legend.key.height=grid::unit(0.5,'cm'), legend.key.width=grid::unit(0.5,'cm'),
          axis.text=element_text(size=9), axis.title=element_text(size=10, vjust=1, face='plain'),
          plot.title=element_text(hjust=0, size=10), plot.margin=grid::unit(c(0,0,0.5,0),'lines'))
  if (!is.null(palette)) {
    if (!is.null(labels)) {
      p = p + scale_fill_manual(values=palette, labels=labels)
    } else {p = p + scale_fill_manual(values=palette)}
  } else {
    if (!is.null(labels)) {
      p = p + scale_fill_manual(labels=labels)
    }
  }
  if (!is.null(xmax)) {
    if (xaxislab==TRUE) {
      p = p + scale_x_continuous(limits=c(1,xmax), expand=c(0,0), breaks=c(1,32,62,93,124,154,185,216,244,275,305),
                                 labels=c('Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May'))
    } else {
      p = p + scale_x_continuous(limits=c(1,xmax), expand=c(0,0), breaks=c(1,32,62,93,124,154,185,216,244,275,305), labels=NULL)
    }
  } else {
    if (xaxislab==FALSE) {p = p + scale_x_continuous(labels=NULL)}
  }

  if (!is.null(ymax)) {
    p = p + coord_cartesian(ylim=c(0,ymax))
  }
  if (!is.null(der)) {
    tmp = data.frame(time=c(1:length(der)), der=der)
    p = p + geom_line(data=tmp, aes(y=der/scale))
  }
  return(p)
}
