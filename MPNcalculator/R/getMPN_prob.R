#' Get MPN Probability Score Function
#'
#' This function uses the binomial distribution to calculate the relative probability of obtaining the observed results of a set of Quanti-Tray 2000 experiments for the same sample at one or more dilution levels (under the assumption that the true concentration is equal to the estimated MPN)
#' @param dataList The data frame where your data is stored.
#' @param mu The MPN value estimated using the getMPN function
#' @keywords MPN
#' @export
#' @examples
#' probScore(dataList)
#'
probScore <- function(mu,dataList){
  num<-dataList$n
  xVal<-dataList$x
  volu<-dataList$v
  prob=signif(mean(dbinom(xVal,num,1-exp(-(mu/100)*volu))),digits=3)
  return(prob)
}
