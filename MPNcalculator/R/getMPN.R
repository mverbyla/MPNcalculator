#' Get MPN Function
#'
#' This function uses Maximum Likelihood Estimation with the Broyden–Fletcher–Goldfarb–Shanno (BFGS) quasi-Newton non-linear optimization method based on the FORTRAN PORT library (http://www.netlib.org/port/) to calculate the most probable number (MPN) for data produced by one or more Quanti-Tray 2000 experiments run for the same sample (at potentially different dilutions).
#' @param dataList A list containing the data from one or more IDEXX Quanti-Tray 2000 plates analyzed for the same sample (at potentially different dilutions). Note that the data list must include an integer sample ID, the name of the parameter being tested (e.g., E.coli, Enterococci, etc.), an array of the number of positive wells, an array of the number of total wells, and an array of the volume (mL) for each well.
#' @keywords MPN
#' @export
#' @examples
#' my_data <- list(
#'    sample=1,
#'    param="E.coli",
#'    x=c(48,22,34,6),
#'    n=c(48,48,48,48),
#'    v=c(1.86e-03,1.86e-04,1.86e-04,1.86e-05)
#'    )
#' getMPN(my_data)
#' # Note that the default starting point for the maximum likelihood value for MPN is 1 (this works for most cases)

getMPN <- function(dataList){
  num<-dataList$n
  xVal<-dataList$x
  volu<-dataList$v
  MPN<-100*signif(
    (bbmle::mle2(function(mu){sum(suppressWarnings((num-xVal)*mu*volu-xVal*log(1-exp(-mu*volu))))},
                 start=list(mu=1),
                 method="SANN",
                 optimizer="nlminb")@details$par)
    ,digits=3)
  return(MPN)
  }
