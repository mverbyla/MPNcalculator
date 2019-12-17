#' Get MPN Confidence Intervals Function
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
#' myMu<-getMPN(my_data)
#' getMPN_CI(mu=myMu,dataList=my_data,alpha=0.05)
#' # Note that the default starting point for the maximum likelihood value for MPN is 1 (this works for most cases)

getMPN_CI <- function(mu,dataList,alpha){
  num<-dataList$n
  xVal<-dataList$x
  volu<-dataList$v
  CI <- 100*signif(confint(bbmle::mle2(function(mu){sum(suppressWarnings((num-xVal)*mu*volu-xVal*log(1-exp(-mu*volu))))},start=list(mu=1),method="BFGS",optimizer="nlminb",skip.hessian=F),method="quad",level=1-alpha),digits=3) #calculates the confidence interval based on root-finding to find the exact point where the profile crosses the critical level; see pg. 4 of documentation for bbmle package https://cran.r-project.org/web/packages/bbmle/vignettes/mle2.pdf
  if(isS4(CI)){return(NA)}else{return(CI)}
}
