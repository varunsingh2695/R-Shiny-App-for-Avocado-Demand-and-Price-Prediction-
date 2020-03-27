multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

###############################################################
#Tittaa
#data <- read.csv(file="avocado.csv", header=TRUE, sep=",")
#data['Date']<-as.Date(data[,'Date'])
#for ( i in 1:nrow(data)){
#    data[i,'Month']<- format(data[i,'Date'],"%B")
#}
#data['Date']<- NULL
#data['Total.Volume'] <- NULL
#data['X'] <- NULL
#data['year'] <- as.factor(data['year'])
#str(data)
###############################################################
library(ggplot2)
library(dplyr)
setwd("/Users/zaidahmed/Desktop")
data <- read.csv("avocado.csv")
library(lubridate)
str(data)
data$Date <- as.Date(data$Date)
data$month <- strip
month(as.POSIXlt(data$Date, format="%d/%m/%Y"))
data$Month <- as.factor(month(data$Date))
data$year <- as.factor(data$year)
str(data)
###############################################################
#1 Trend of Average Monthly Sales Over the years!!!

gapc<-data%>%filter(type=='conventional')%>%select(Total.Bags,Month)%>%group_by(Month)%>%summarise(mean(Total.Bags))
colnames(gapc)<-c("Month","AVG_SALES")
gapc<- data.frame(gapc)
p1 <- ggplot(gapc,aes(x=Month,y=AVG_SALES)) + geom_point(colour='blue') + ggtitle("Conventional Avocado")

gapo<-data%>%filter(type=='organic')%>%select(Total.Bags,Month)%>%group_by(Month)%>%summarise(mean(Total.Bags))
colnames(gapo)<-c("Month","AVG_SALES")
gapo<- data.frame(gapo)
p2 <- ggplot(gapo,aes(x=Month,y=AVG_SALES)) + geom_point(colour='black') + ggtitle("Organic Avocado")


multiplot(p1,p2, cols=1)
###############################################################
#2 Trend of Average Monthly Sales Over the years for a specific County!!!
gapc<-data%>%filter(type=='conventional' & region == 'Albany')%>%select(Total.Bags,Month)%>%group_by(Month)%>%summarise(mean(Total.Bags))

colnames(gapc)<-c("Month","AVG_SALES")
gapc<- data.frame(gapc)
p1 <- ggplot(gapc,aes(x=Month,y=AVG_SALES)) + geom_point(colour='blue') + ggtitle("Conventional Avocado")

gapo<-data%>%filter(type=='organic' & region== 'Albany')%>%select(Total.Bags,Month)%>%group_by(Month)%>%summarise(mean(Total.Bags))

colnames(gapo)<-c("Month","AVG_SALES")
gapo<- data.frame(gapo)
p2 <- ggplot(gapo,aes(x=Month,y=AVG_SALES)) + geom_point(colour='black') + ggtitle("Organic Avocado")

multiplot(p1,p2, cols=1)
###############################################################
#3 GreenSkin
datn <- data
datn$Green <- datn$Total.Volume-(datn$X4046+datn$X4225+datn$X4770)
head(datn)
gre <- datn%>%filter(type=='organic')%>%select(Month,Green)
head(gre)
###############################################################
#Avg Price trends of Avocado over diff months
gapc<-data%>%select(AveragePrice,Month,year,type)%>%group_by(Month,type)%>%summarise(mean(AveragePrice))
gapc <- data.frame(gapc)
colnames(gapc) <- c('Month','Type','Avg_P')
N <- ggplot(gapc,aes(x=Month,y=Avg_P)) + geom_point(aes(fill=Type,colour=Type))
N
###############################################################




