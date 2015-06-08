#' Calculate lifetime sparseness according to Olsen et al. 2008
#' 
#' @details minodours is interpreted as fraction when <=1. If >1 it is assumed 
#'   then there must be >= that many odour responses. Any cells that have been 
#'   tested for fewer than than minodours will have a sparseness value of NA. 
#'   Definition:
#'   
#'   S = 1/(1-1/N)*( 1 - (sum(r/N)^2)/(sum(r^2/N)) )
#'   
#'   where N is the number of odours and r is the analogue response intensity of
#'   the neuron to odour j minus baseline firing rate. Analogue response 
#'   intensity was the mean spike rate (averaged across five sweeps) during the 
#'   entire 500-ms odour stimulus period. Any values of rj < 0 were set to zero 
#'   before computing lifetime sparseness (this was the only analysis in this 
#'   study in which negative responses were zeroed). Responses to paraffin oil 
#'   (solvent control) were not considered in the sparseness analysis. We 
#'   computed the lifetime sparseness for each individual cell in our study for 
#'   which we tested at least 12 of the odours in our set. We used the 
#'   Mann--Whitney U test to assess the significance of sparseness differences.
#' @param x matrix of odour data with one column per odour, 1 row per cell
#' @param minodours Minimum number of odours required to compute valid score 
#'   (see details)
#' @return named numeric vector, one entry for each row (neuron) in x
#' @export
#' @references Olsen S.R. and Wilson R.I. (2008). Lateral presynaptic inhibition
#'   mediates gain control in an olfactory circuit. Nature 452 (7190), 956--960.
#'   \href{doi:10.1038/nature06864}{http://dx.doi.org/10.1038/nature06864}
#'   
#'   Willmore B. and  Tolhurst D.J.(2001). Characterizing the sparseness of 
#'   neural codes. Network Comput. Neural Syst. 12, 255 (2001) 
#'   \href{doi:10.1080/net.12.3.255.270}{http://dx.doi.org/10.1080/net.12.3.255.270}
#'   
#' @section Missing Values (NAs): When there are missing values in \code{x}, 
#'   they are omitted silently. NAs do not count towards the value of N, the 
#'   number of responses /cell in the equation above.
#' @examples
#' spikes=CollectSpikesFromSweeps(
#'   system.file('igor','spikes','nm20110914c4',package='gphys'),
#'   subdir='BLOCKI',sweeps=0:4)
#' od=OdourResponseFromSpikes(spikes,response=c(2200,2700),baseline=c(0,2000))
#' S=lifetime_sparseness(od)
#' 
#' # check what happens with NAs
#' od2=od
#' od2[5,'cVA']=NA
#' S2=lifetime_sparseness(od2)
lifetime_sparseness<-function(x,minodours=0.75){
  if(is.data.frame(x)) x=as.matrix(x)
  if(!is.matrix(x)){
    if(is.vector(x)) x<-matrix(x,nrow=1)
    else stop("Unknown data format. I like matrices, dataframes and vectors.")
  }
  # remove any terms <0
  if(any(x<0, na.rm=T)){
    nxlt0=sum(x<0,na.rm=T)
    x[x<0]=0
    warning("Zeroing ",nxlt0,' responses < 0')
  }
  # main calculation
  N=rowSums(is.finite(x)) # number of odours
  S=1/(1-1/N)*( 1 - (rowSums(x/N,na.rm=T)^2)/(rowSums(x^2/N,na.rm=T)) )
  
  # but replace caclulated values with NA if too few odours are available
  if(minodours<=1) 
    minodours=minodours*ncol(x)
  badcell=N<minodours
  if(any(badcell)){
    warning("Returning NA for ",sum(badcell,na.rm=T),' cells that have too few odours')
    S[badcell]=NA
  }
  S
}
