#' Calculate lifetime sparseness according to Olsen et al. 2008
#'
#' minodours is interpreted as fraction when <=1. If >1 it is assumed then
#' there must be >= that many odour responses. Any cells that has been tested 
#' for fewer than than minodours will have a sparseness value of NA
#' Definition:
#' S = 1/(1-1/N)*( 1 - (sum(r/N)^2)/(sum(r^2/N)) )
#' 
#' where N is the number of odours and r is the analogue response intensity of
#' the neuron to odour j minus baseline firing rate. Analogue response intensity
#' was the mean spike rate (averaged across five sweeps) during the entire 500-ms
#' odour stimulus period. Any values of rj < 0 were set to zero before computing
#' lifetime sparseness (this was the only analysis in this study in which
#' negative responses were zeroed). Responses to paraffin oil (solvent control)
#' were not considered in the sparseness analysis. We computed the lifetime
#' sparseness for each individual cell in our study for which we tested at least
#' 12 of the odours in our set. We used the Mann–Whitney U-test to assess the
#' significance of sparseness differences.
#' @param x matrix of odour data with one column per odour, 1 row per cell
#' @param minodours Minimum number of odours required to compute valid score (see details)
#' @return named numeric vector, one entry for each row (neuron) in x
#' @export
#' @references Olsen S.R. and Wilson R.I. (2008). Lateral presynaptic 
#' inhibition mediates gain control in an olfactory circuit.
#' Nature 452 (7190), 956–960. doi:10.1038/nature06864
#' @references B. Willmore, D. J. Tolhurst, Network Comput. Neural
#' Syst. 12, 255 (2001)
#' @examples
#' dst=subset(fixedUseful,shortGenotype%in%c("JK56","JK1029","fru"))
#' dst$S=lifetime_sparseness(dst[,JonnyOdors[-1]]) # -1 drops blank
#' if(require(ggplot2))
#'   qplot(sex,S,data=dst,facets=.~cluster, binwidth=0.1,main='Lifetime Sparseness by cell type')+geom_boxplot()
lifetime_sparseness<-function(x,minodours=0.75){
  if(is.data.frame(x)) x=as.matrix(x)
  if(!is.matrix(x)){
    if(is.vector(x)) x<-matrix(x,nrow=1)
    else stop("Unknown data format. I like matrices, dataframes and vectors.")
  }
  # remove any terms <0
  if(any(x<0)){
    nxlt0=sum(x<0,na.rm=T)
    x[x<0]=0
    warning("Zeroing ",nxlt0,' responses < 0')
  }
  # main calculation
  N<-ncol(x) # number of odours
  S=1/(1-1/N)*( 1 - (rowSums(x/N,na.rm=T)^2)/(rowSums(x^2/N,na.rm=T)) )
  
  # but replace caclulated values with NA if too few odours are available
  nodours_percell=rowSums(is.finite(x))
  badcell=nodours_percell<minodours
  if(any(badcell)){
    warning("Returning NA for ",sum(badcell,na.rm=T),' cells that have too few odours')
    S[badcell]=NA
  }
  S
}
