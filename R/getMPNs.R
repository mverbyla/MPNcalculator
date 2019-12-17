#' Get MPNs Function
#'
#' This function allows you to calculate the most probable numbers (MPNs) and their confidence intervals using raw data from an experiment with the IDEXX Quanti-Tray 2000
#' @param allData The data frame where your data is stored.
#' @keywords MPN
#' @export
#' @examples
#' getMPNs(allData)
#'
getMPNs <- function(allData){
  myData <- dplyr::mutate(allData,vol=NA)
  myData[myData$Type == "Large",]$vol <- 1.86
  myData[myData$Type == "Small",]$vol <- 0.186
  myData$uniqueSamples <- as.numeric(factor(paste0(myData$Sample,myData$Parameter)))
  myData <- myData[order(myData$uniqueSamples),]

  results<-data.frame(Sample=vector(),Parameter=vector(),MPN=vector(),CI.Lower=vector(),CI.Upper=vector(),Probability=vector())

  for(i in 1:length(unique(myData$uniqueSamples))){
    dataList <- list(
      sample=unique(myData[myData$uniqueSamples==i,]$Sample),
      param=unique(myData[myData$uniqueSamples==i,]$Parameter),
      x=myData[myData$uniqueSamples==i,]$Positive,
      n=myData[myData$uniqueSamples==i,]$Total,
      v=myData[myData$uniqueSamples==i,]$vol*myData[myData$uniqueSamples==i,]$Dilution
    )

    ########### CALCULATE THE MPNs HERE #################
    if(sum(dataList$n-dataList$x)==0){MPN<-">LOD";CI<-c(NA,NA);prob<-NA}else{
      MPN<-getMPN(dataList)
      CI<-getMPN_CI(MPN,dataList,alpha=0.05)
      prob<-probScore(MPN,dataList)
    }
    results[i,]<-c(as.character(dataList$sample),as.character(dataList$param),MPN,CI[1],CI[2],prob)
  }
  #write.csv(results,"myresults.csv")
  results <- as.data.frame(results)
  results[,c("MPN","CI.Lower","CI.Upper","Probability")] <- suppressWarnings(sapply(results[,c("MPN","CI.Lower","CI.Upper","Probability")],as.numeric))
  return(results)
}
