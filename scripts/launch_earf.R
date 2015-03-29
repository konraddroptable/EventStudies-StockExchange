############################
# Author: Konrad Mrozewski #
# Date: 20-03-2015         #
############################

################################### Packages import
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("dplyr")
library(ggplot2, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(dplyr, quietly = TRUE)

################################### Framework import
source("https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/scripts/earf.R")

################################### Data imports
dataLinks<-list(akcje="https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/data/akcje.txt",
     indeksy="https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/data/indeksy.txt",
     info="https://raw.githubusercontent.com/konraddroptable/RStudioFiles/master/data/informacje.txt")

################################### Starting parameters
input.data<-list()
input.params<-list()

input.data$marketIndexesFrame<-read.delim(dataLinks$indeksy, dec=",", head=TRUE)
input.data$stocksFrame<-read.delim(dataLinks$akcje, dec=",", head=TRUE)
input.data$info<-read.delim(dataLinks$info, head=TRUE, stringsAsFactors=TRUE)
input.params$daysColumn<-1
input.params$span<-10

################################### Visualisation
calculations<-earf$performCalculation(input.data, input.params)

View(calculations$shapiroWilkTest) #Shapiro-Wilk test
View(calculations$corradoTest) #Corrado test

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


#### Data transformations
gg.data<-inner_join(melt(calculations$cumulatedAbnormalReturnsByDay, id.vars="days"),
                    input.data$info, 
                    by=c("variable" = "spolka"))

#### Partition by trade
ggplot(data = gg.data %>%
         select(-variable,-rok) %>%
         group_by(days,branza) %>%
         summarise(mean.car=mean(value)), aes(x=days, y=mean.car, fill=branza))+
  geom_bar(stat="identity",position="stack")+
  ggtitle("Średnia skumulowana zwyżkowa stopa zwrotu w podziale na branżę spólki")+
  xlab("Dni traksakcyjne")+ylab("Średnia skumulowana zwyżkowa stopa zwrotu")+
  theme(plot.title = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_colour_hue(l=90, c=30)


#### Partition by year
ggplot(data = gg.data %>%
         select(-variable,-branza) %>%
         group_by(days,rok) %>%
         summarise(mean.car=mean(value)), aes(x=days, y=mean.car, fill=as.factor(rok)))+
  geom_bar(stat="identity",position="stack")+
  ggtitle("Średnia skumulowana zwyżkowa stopa zwrotu w podziale na rok dokonania splitu")+
  xlab("Dni traksakcyjne")+ylab("Średnia skumulowana zwyżkowa stopa zwrotu")+
  theme(plot.title = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_colour_hue(l=90, c=30)
