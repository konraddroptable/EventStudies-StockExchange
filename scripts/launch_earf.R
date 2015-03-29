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
car.melt<-melt(sapply(calculations$cumulatedAbnormalReturns,sum))
car.melt$variable<-as.factor(rownames(car.melt))

ggplot(data=car.melt, aes(x=variable,y=value))+geom_bar(stat="identity")


