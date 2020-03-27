library(shiny)
library(shinythemes)
library(h2o)
library(lubridate)
library(dplyr)
h2o.init(nthreads = 12)

data <- read.csv(file="avocado.csv", header=TRUE, sep=",")

data4<-data

data['Date']<-as.Date(data[,'Date'])

data[,'year'] <- factor(data[,'year'])

data['Total.Volume'] <- NULL

data['X'] <- NULL

data2<- data

data2['Date']<-as.POSIXct(data[,'Date'])
a = unique(data[,'region'])


predict <- function(dt,avgprice,ty,reg,target){
  data1 <- data %>% select(AveragePrice,target,year,type,region,Date)
  
  train <- data1 %>% filter(year=='2015'| year=='2017' | year=='2016')
  valid <- data1 %>% filter(year=='2018')
  
  
  train <- as.h2o(train)
  valid <- as.h2o(valid)
  
  
  
  y<- target
  
  x <- setdiff(names(train), y)
  
  
  set.seed(666)
  model <- h2o.gbm(
    x = x, 
    y = y, 
    training_frame = train, 
    validation_frame = valid, 
    nfolds=0,
    ntrees = 115,
    max_depth = 7,
    max_runtime_secs = 10)
  
  
  
  dt <- as.Date(dt)
  y <- as.integer(format(dt,'%Y'))
  
  
  pred <- data.frame("AveragePrice" = avgprice, "type" = ty, 
                     "region" = reg, "Date"= dt, "year" = y)
  
  pred[,"year"]<- factor(pred[,"year"])
  
  
  pred<- as.h2o(pred)
  
  
  prediction <- h2o.predict(model,pred)
  
  
  pred2<-floor(prediction[1,1])
  
  return(pred2)
}

plot_time_series <- function(target){

  
  library(ggplot2)
  library(scales)
  
  data3 <- data2 %>% select(AveragePrice,target,year,type,region,Date)
  
  ggplot(data3, aes(Date, data3[,target])) + geom_line() +
    scale_x_datetime(breaks = date_breaks("6 month"),labels=date_format("%Y-%m")) + xlab("Days") + ylab(paste("Units of ", target))+
    ggtitle("Fluctation of units over the past years")
}

edaPlot <- function(reg, var){
  
  
  
  
  
  data4$Date <- as.Date(data4$Date)
  data4$Month <- month.abb[month(data4$Date)]
  data4$Month <-factor(data4$Month, 
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  
  
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
  
  
  
  if (var=='Sales'){
    
    gapc<-data4%>%filter(type=='conventional' & region==reg)%>%select(Total.Volume,Month)%>%group_by(Month)%>%summarise(mean(Total.Volume))
    colnames(gapc)<-c("Month","AVG_SALES")
    gapc<- data.frame(gapc)
    p1 <- ggplot(gapc,aes(x=Month,y=AVG_SALES)) + geom_point(colour='blue', size=5) + ggtitle("Conventional Avocado", reg)
    
    gapo<-data4%>%filter(type=='organic' & region==reg)%>%select(Total.Volume,Month)%>%group_by(Month)%>%summarise(mean(Total.Volume))
    colnames(gapo)<-c("Month","AVG_SALES")
    gapo<- data.frame(gapo)
    p2 <- ggplot(gapo,aes(x=Month,y=AVG_SALES)) + geom_point(colour='red',size=5) + ggtitle("Organic Avocado", reg)
    
    multiplot(p1,p2, cols=1)
  }
  
  
  
  else if (var == 'Price'){
    
    gapc<-data4%>%filter(region==reg)%>%select(AveragePrice,Month,year,type)%>%group_by(Month,type)%>%summarise(mean(AveragePrice))
    gapc <- data.frame(gapc)
    colnames(gapc) <- c('Month','Type','Avg_Price')
    N1 <- ggplot(gapc,aes(x=Month,y=Avg_Price)) + geom_point(aes(fill=Type,colour=Type),size=5) + ggtitle("Conventinal vs Organic: Price", reg)
    N1
  }
}


ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Avocado Stock Prediction"),
                conditionalPanel(
                  condition = "input.password != 'password'",
                  textInput("username", "Username:", placeholder = "Type the username here"),
                  textInput("password", "Password:", placeholder = "Type the password here")
                ), 
                conditionalPanel(
                  condition = c("input.password == 'password'","input.username == 'username"), 
                tabsetPanel(type = "tabs",
                            tabPanel("Predict",
                            sidebarLayout(
                              sidebarPanel(h3('Input Parameters'),
                                selectInput(inputId = "location", label="Select a region:", choice=a),
                                textInput(inputId = "avgpr",
                                          label ="Input the average price of one avocado:", value=0.5),
                                dateInput(inputId="date", label="Select a date for prediction",
                                          value = Sys.Date()+10, min = Sys.Date()+1, max = NULL,
                                          format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                language = "en", width = NULL),
                                radioButtons(inputId = 'type', label = "Organic or Conventional?",inline = TRUE, choices = c('Organic'='organic','Conventional'='conventional')),
                                radioButtons(inputId = 'choice',label = "What do you want to predict?", choices = c("X4046"="X4046","X4225"="X4225","X4770"="X4770","Small Bags"="Small.Bags","Large Bags"="Large.Bags")),
                                actionButton(inputId = 'predict', label = "Predict!")
                              )
                            ,
                            mainPanel(
                              h1("Predicted units You Should have"),
                              verbatimTextOutput(outputId = "tar"),
                              plotOutput(outputId = "tsgraph")
                              
                            ))),tabPanel("Check Historical Data",
                                      sidebarLayout(
                                        sidebarPanel(h3('Input Parameters'),
                                                     selectInput(inputId = "loc", label="Enter a region:", choices = a),
                                                     radioButtons(inputId = 'ty', label = "What do you want to plot?",inline = TRUE, choices = c('Sales'='Sales','Price'='Price')),
                                                     actionButton(inputId = 'plot', label = "Plot!!")
                                        ),
                                        
                                        mainPanel(
                                          plotOutput(outputId = "graph")
                                        ))
                            
                            
                            
                )
                
    )            )         
)


server <- function(input,output){

  
 
  
  
  
  
  
  observeEvent(input$predict,{
    if (input$password=="password"){
    
      dt <- input$date
      
      avgprice =as.numeric(input$avgpr)
      
      ty = input$type
      
      reg = input$location
      
      target = input$choice
  
      
      
      
      pred2 <- abs(predict(dt,avgprice,ty,reg,target))
      
      output$tar<- renderPrint({pred2})
      
      output$tsgraph <- renderPlot({plot_time_series(target)})
      

      
      
  }})
  
  observeEvent(input$plot, {
    if (input$password=="password"){
    loc <- input$loc
    type <- input$ty
    
    output$graph <- renderPlot({edaPlot(loc,type)})
  }})
  
                 
  }
  

shinyApp(ui=ui, server=server)


