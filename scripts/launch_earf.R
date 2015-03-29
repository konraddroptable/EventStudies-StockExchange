############################
# Author: Konrad Mrozewski #
# Date: 20-03-2015         #
############################

################################### Packages import
# install.packages("ggplot2")
# install.packages("reshape2")
library(ggplot2)
library(reshape2)

################################### Framework import
source("https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/scripts/earf.R")

################################### Data imports
dataLinks<-list(akcje="https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/data/akcje.txt",
     indeksy="https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/data/indeksy.txt")

################################### Starting parameters
input.data<-list()
input.params<-list()

input.data$marketIndexesFrame<-read.delim(dataLinks$indeksy, dec=",", head=TRUE)
input.data$stocksFrame<-read.delim(dataLinks$akcje, dec=",", head=TRUE)
input.params$daysColumn<-1
input.params$span<-10

################################### Visualisation
calculations<-earf$performCalculation(input.data, input.params)

View(calculations$shapiroWilkTest)
View(calculations$corradoTest)

################################## Charts
ggplot(data=data.frame(value=as.numeric(sapply(data.frame(value=t(calculations$cumulatedAbnormalReturnsByDay[,-1]),
                                                          row.names=NULL),mean)),
                       variable=as.factor(calculations$cumulatedAbnormalReturnsByDay[[1]]), row.names=NULL),
       aes(x=variable,y=value))+
  geom_bar(stat="identity", fill="#33B5E5")+
  ggtitle("Skumulowane średnie zwyżkowe stopy zwrotu")+
  xlab("Dni traksakcyjne")+ylab("Średnia skumulowana zwyżkowa stopa zwrotu")+
  theme(plot.title = element_text(size = rel(2)),
        axis.text.x = element_text(angle = 90, hjust = 1))
  
  


