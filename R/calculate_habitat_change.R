#' Calculate habitat change
#'
#' Calculates habitat added or lost between time steps.
#'
#' @param tothabitat Data frame with two columns: "habitat" giving the names of each land cover type of interest and "area" giving the total area potentially available
#' @param flood Long-format data frame with proportion flooded values for each time step and land cover type of interest; land cover names should be in a column named "habitat"
#' @param time Name of column in \code{flood} containing timesteps
#' @param value Name of column in \code{flood} containing the percent flooded values for each time step
#' @param accessible Optional; data frame with same structure as \code{flood} containing proportion of flooded area that is accessible for each time step and land cover of interest (e.g. suitable depth)
#' @param alt Optional; value to be used for land cover types with missing accessibility data, e.g. assume 1 or 0
#' @param wetsplit Optional; if \code{TRUE}, split total area of open water labeled "wetlands" into seasonal vs. permanent; see Details
#'
#' @details This function calculates the total area of open water (and optionally accessible open water) for each land cover type of interest during each time step from the total area of each land cover type and estimates for each time step of the proportion open water (and optionally the proportion of the open water area that is accessible). The function then calculates the daily change in the total area of open water as area added since the last time step and area lost before the start of the next time step. Area added and lost is only calculated for total open water area, not accessible open water area.
#'
#' The \code{wetsplit} option is useful if proportion accessible varies by wetland type (seasonal vs. permanent) but proportion flooded is only available for all wetlands. If \code{wetsplit=TRUE}, the \code{flood} data frame requires a column labeled "prop.perm" that reflects the proportion of open water wetlands estimated to be permanent wetlands during each time step, and the \code{accessible} data frame requires land cover types (in the "habitat" column) called "seas" and "perm".
#'
#' @return Returns a list with 3-5 elements, formatted for input into the bioenergetics modeling function and for plotting, including:
#'
#'  (1) \code{openwater}: total area of open water for each time step (rows) and land cover type (columns)
#'
#'  (2) \code{added}: total area of open water added since the previous time step
#'
#'  (3) \code{returned}: total area of open water lost before the start of the next time step
#'
#'  (4) (optionally) \code{accessible}: total area of accessible open water for each time step and land cover type
#'
#'  (5) (optionally) \code{prop.accessible}: proportion accessible for each time step and land cover type, including \code{alt} values filled in where previously missing
#'
#' @author Kristen Dybala, \email{kdybala@@pointblue.org}
#'
#' @examples
#' tot = data.frame(habitat=c('A','B','C'), area=c(100,200,300))
#' flood = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), yday=rep(c(1:3),3),
#'    value=c(0.9,0.8,0.7, 0.1,0.15,0.2, 0.5,0.5,0.5))
#' depth = data.frame(habitat=c(rep('A',3),rep('B',3),rep('C',3)), yday=rep(c(1:3),3),
#'    value=rep(0.9,9))
#' calculate_habitat_change(tothabitat=tot, flood=flood, time='yday',
#'    value='value', accessible=NULL, alt=0, wetsplit=FALSE)
#' calculate_habitat_change(tothabitat=tot, flood=flood, time='yday',
#'    value='value', accessible=depth, wetsplit=FALSE)
#'
#' @export
#'
calculate_habitat_change = function(tothabitat, flood, time, value, accessible=NULL, alt=1, wetsplit=TRUE) {
  ## calculate total open water available in each habitat type during each time step:
  dat = merge(flood, tothabitat, by='habitat', all.x=TRUE, all.y=TRUE, sort=FALSE)
  dat$openwater = dat$area * dat[,colnames(dat)==value]
  colnames(dat)[colnames(dat)==time]='time'

  if (wetsplit==TRUE) { ## partition wetlands into perm and seas
    wetsub = dat[dat$habitat=='wetlands',]
    wetsub$habitat = plyr::revalue(wetsub$habitat, c('wetlands'='perm'))
    wetsub$openwater = wetsub$openwater * wetsub$prop.perm
    wetsub2 = dat[dat$habitat=='wetlands',]
    wetsub2$habitat = plyr::revalue(wetsub2$habitat, c('wetlands'='seas'))
    wetsub2$openwater = wetsub2$openwater * (1-wetsub2$prop.perm)

    dat = dat[-which(dat$habitat=='wetlands'),]
    if (nrow(dat)>0) {
      dat = rbind(dat, wetsub, wetsub2)
    } else { #only wetlands included in habitat types
      dat = rbind(wetsub, wetsub2)
    }
    dat$prop.perm = NULL
  }

  ## calculate additions and subtractions between time steps within each habitat type
  dat = dat[order(dat$habitat, dat$time),]
  dat$added = ave(dat$openwater, factor(dat$habitat), FUN = function(x) {c(NA, diff(x))}) #calculate difference from previous row for each group
  dat$added[is.na(dat$added)] = dat$openwater[is.na(dat$added)] #keep value from first time step
  dat$returned = dat$added
  dat$returned[dat$added>0] = 0
  dat$returned = abs(dat$returned) #negative numbers indicate ha returned at end of previous time interval
  dat$returned = c(dat$returned[2:nrow(dat)],0) #shift up one
  dat$added[dat$added<0] = 0 #remove negative values from habitat added

  ## if applicable, also calculate total accessible open water in each habitat type during each time step:
  if (!is.null(accessible)) {
    colnames(accessible)[colnames(accessible)==time]='time'
    colnames(accessible)[colnames(accessible)==value]='prop.accessible'
    dat = merge(dat, accessible[,c('habitat','time','prop.accessible')], by=c('habitat','time'), all.x=TRUE, sort=FALSE)
    dat$prop.accessible[is.na(dat$prop.accessible)] = alt #for those missing depth values, provide assumed alternative
    dat$accessible = dat$openwater*dat$prop.accessible
    dat = dat[,c('habitat','time','openwater','added','returned','accessible','prop.accessible')]
  } else {
    dat = dat[,c('habitat','time','openwater','added','returned')]
  }

  dat = reshape2::melt(dat, id.vars=c('habitat','time'))
  dat = reshape2::dcast(dat, variable+time~habitat, value.var='value')
  dat = split(dat, dat$variable, drop=TRUE)
  dat = lapply(dat, function(x) {x=x[,2:ncol(x)]})
  return(dat)
}
