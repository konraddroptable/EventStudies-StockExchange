################################### Build framework function
setModule<-function(moduleName,moduleDefinition){
  mf<-match.call()
  moduleEnv<-new.env(parent=parent.frame())
  module<-eval(mf$moduleDefinition,envir=moduleEnv)
  
  if(is.null(module) | class(module)!="list")
    stop("no list of exported names")
  
  class(module)<-"Namespace"
  attr(module,"ModuleName")<-mf$moduleName 
  
  return(module)
}

######################################################
##############      Build module        ##############
######################################################
earf<-setModule("splitAnalysis",{
  ################################### Basic functions
  createModelDataFrame<-function(stocksFrame, marketFrame){
    return(list(stocks=stocksFrame, indexes=marketFrame))
  }
  
  logReturn<-function(x){
    if(dim(as.matrix(x))[2] > 1)
      stop("wrong dimension of x!")
    
    numerator<-x[2:length(x)]
    denumerator<-x[1:(length(x)-1)]
    
    return(log(numerator/denumerator))
  }
  
  abnormalReturn<-function(stockReturns, marketReturns, daysColumn){
    if(dim(stockReturns)[1] != dim(marketReturns)[1] ||
         dim(stockReturns)[2] != dim(marketReturns)[2])
      stop("wrong frames dimensions!")
    
    ret<-list()
    stocks<-as.data.frame(stockReturns[, -daysColumn])
    market<-as.data.frame(marketReturns[, -daysColumn])
    
    for(i in 1:(ncol(stocks)))
      ret[[i]]<-stocks[,i]-market[,i]
    
    ret<-as.data.frame(ret)
    colnames(ret)<-colnames(stocks)
    output<-data.frame(days=stockReturns[[daysColumn]], ret)
    
    return(output)  
  }
  
  cumulatedAbnormalReturn<-function(abnormalReturns, daysColumn){
    if(missing(daysColumn))
      daysColumn<-1
    ret<-matrix(NA,nrow(abnormalReturns), ncol(abnormalReturns)-1)
    days<-abnormalReturns[[1]]
    
    beforeDayZero<-match(0, abnormalReturns[[1]])-1
    dayZero<-match(0, abnormalReturns[[1]])
    
    for(i in 1:(ncol(abnormalReturns)-1)){
      for(j in 1:nrow(abnormalReturns)){
        if(j < dayZero){
          ret[j,i]<-sum(abnormalReturns[j:beforeDayZero, i+1])
        } else{
          ret[j,i]<-sum(abnormalReturns[dayZero:j, i+1])
        }
      }
    }
    
    output<-data.frame(days=days, as.data.frame(ret))
    colnames(output)<-names(abnormalReturns)
    
    return(output)
  }
  
  partitionedCAR<-function(ar, span){
    obsVector<-seq(from=1, to=length(ar), by=1)
    
    splitList<-split(obsVector, ceiling(seq_along(obsVector)/10))
    spanList<-list()
    
    # omit parts smaller than span
    for(i in 1:length(splitList)){
      if(length(splitList[[i]]) == span)
        spanList[[i]]<-splitList[[i]]
    }
    ret<-rep(NA,length(spanList)) #output
    
    for(i in 1:length(spanList)){
      ret[i]<-sum(ar[unlist(spanList[[i]])])
    }
    
    return(as.numeric(ret))
  }
  
  partitionedCAR.Labels<-function(days, span){
    splitList<-split(days, ceiling(seq_along(days)/10))
    spanList<-list()
    
    # omit parts smaller than span
    for(i in 1:length(splitList)){
      if(length(splitList[[i]]) == span)
        spanList[[i]]<-splitList[[i]]
    }
    
    ret<-rep(NA,length(spanList)) #output
    
    for(i in 1:length(spanList)){
      ret[i]<-paste("[",
                    unlist(spanList[[i]])[1],
                    ", ",
                    unlist(spanList[[i]])[span],
                    "]",sep="")
    }
    
    return(ret)
  }
  
  shapiroWilkTest<-function(x){
    test<-shapiro.test(x)
    return(data.frame(stat=test$statistic,prob=test$p.value))
  }
  
  getPosition<-function(row){
    row.old<-as.numeric((row))
    row.sort<-sort(row.old, decreasing = FALSE)
    return(match(row.old, row.sort))
  }
  
  getRanks<-function(positionsColumn, const){
    return(positionsColumn-const)
  }
  
  getCorradoStat<-function(modifiedRankColumn, modifiedRankDataFrame){
    nominator<-mean(modifiedRankColumn, na.rm=TRUE)
    colLength<-ncol(modifiedRankDataFrame)
    
    denominator<-sqrt(sum(sapply(as.data.frame(modifiedRankDataFrame^2), sum)/
                            sapply(modifiedRankDataFrame, length)^2)/colLength)
    return(as.numeric(nominator/denominator))
  }
  
  ################################## Framework exported function
  doCalculations<-function(input.data, input.params){
    # create model data.frame
    modelDataFrame<-createModelDataFrame(input.data$stocksFrame, input.data$marketIndexesFrame)
    days<-modelDataFrame[[1]][[input.params$daysColumn]][-1]
    
    # calculate stocks & market log returns
    stocksLogReturns<-data.frame(days=days, sapply(modelDataFrame[[1]][, -input.params$daysColumn], logReturn))
    marketLogReturns<-data.frame(days=days, sapply(modelDataFrame[[2]][, -input.params$daysColumn], logReturn))
    
    # calculate abnormal returns
    abnormalReturns<-abnormalReturn(stockReturns = stocksLogReturns,
                                    marketReturns = marketLogReturns, 
                                    daysColumn=input.params$daysColumn)
    
    # calculate cumulated abnormal returns with specified span
    p.CAR<-as.data.frame(t(data.frame(sapply(abnormalReturns[,-1], partitionedCAR, span=input.params$span))))
    colnames(p.CAR)<-partitionedCAR.Labels(days, span=input.params$span)
    
    # perform shapiro-wilk normality test
    swTests<-as.data.frame(t(sapply(p.CAR, shapiroWilkTest)))
    
    # calculations for getting corrado statistic
    ranks<-as.data.frame(t(sapply(as.data.frame(t(p.CAR)), getPosition)))
    colnames(ranks)<-names(p.CAR)
    ranksModified<-as.data.frame(sapply(ranks, getRanks, const=(0.5+0.5*length(ranks[1,]))))
    corradoStats<-data.frame(stat=sapply(ranksModified, getCorradoStat, modifiedRankDataFrame=ranksModified))
    corradoProbs<-data.frame(prob=2*sapply(abs(corradoStats), pnorm, mean=0, sd=1, lower.tail=FALSE))
    
    # summary of corrado statistics
    corrado<-data.frame(corradoStats,corradoProbs)
    colnames(corrado)<-c("stat","prob")
    
    output.data<-list(modelDataFrame=modelDataFrame,
                      stocksLogReturns=stocksLogReturns,
                      marketLogReturns=marketLogReturns,
                      abnormalReturns=abnormalReturns,
                      cumulatedAbnormalReturns=p.CAR,
                      shapiroWilkTest=swTests,
                      corradoTest=corrado)
    
    return(output.data)
  }
  
  # Exported functions
  list(performCalculation=doCalculations)
})