
# Titender and co.

# Function takes region and variable as arguments and outputs the appropriate plots

edaPlot <- function(reg, var){

    ###############################################################
    # Tittaa ka Day-ta
    ###############################################################
    
    library(ggplot2)
    library(dplyr)
    # setwd("/Users/saura/Desktop")
    data <- read.csv("avocado.csv")
    library(lubridate)
    data$Date <- as.Date(data$Date)
    data$Month <- month.abb[month(data$Date)]
    data$Month <-factor(data$Month, 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

    #############################################################################
    # Defining multiplot to avoid the error (could not find function "multiplot")
    #############################################################################
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
         library(grid)
         plots <- c(list(...), plotlist)
         numPlots = length(plots)
         if (is.null(layout)) {
             layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                              ncol = cols, nrow = ceiling(numPlots/cols))
         }
    
         if (numPlots==1) {
             print(plots[[1]])
         }
         else {
             grid.newpage()
             pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
             for (i in 1:numPlots) {
                 matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                 print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                 layout.pos.col = matchidx$col))
             }
         }
     }
    
    ###########################################################################
    #1 Trend of Average Monthly Sales Over the years for the selected County!!!
    ###########################################################################
    
    if (var=='Sales'){
        
        gapc<-data%>%filter(type=='conventional' & region==reg)%>%select(Total.Volume,Month)%>%group_by(Month)%>%summarise(mean(Total.Volume))
        colnames(gapc)<-c("Month","AVG_SALES")
        gapc<- data.frame(gapc)
        p1 <- ggplot(gapc,aes(x=Month,y=AVG_SALES)) + geom_point(colour='blue') + ggtitle("Conventional Avocado", reg)
        
        gapo<-data%>%filter(type=='organic' & region==reg)%>%select(Total.Volume,Month)%>%group_by(Month)%>%summarise(mean(Total.Volume))
        colnames(gapo)<-c("Month","AVG_SALES")
        gapo<- data.frame(gapo)
        p2 <- ggplot(gapo,aes(x=Month,y=AVG_SALES)) + geom_point(colour='red') + ggtitle("Organic Avocado", reg)
        
        multiplot(p1,p2, cols=1)
    }
    
    ####################################################################
    #2 Trend of Average prices over the years for the selected County!!!
    ####################################################################
    
    else if (var == 'Price'){
            
        gapc<-data%>%filter(region==reg)%>%select(AveragePrice,Month,year,type)%>%group_by(Month,type)%>%summarise(mean(AveragePrice))
        gapc <- data.frame(gapc)
        colnames(gapc) <- c('Month','Type','Avg_Price')
        N1 <- ggplot(gapc,aes(x=Month,y=Avg_Price)) + geom_point(aes(fill=Type,colour=Type)) + ggtitle("Conventinal vs Organic: Price", reg)
        N1
    }
}

edaPlot("California","Price")

