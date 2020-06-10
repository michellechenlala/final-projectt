library(shiny)
library(e1071)
library(forecast)
library(dplyr)
shinyServer(function(input, output) {
  
  source("API_DATA.R")
  
  data_select_Input <- reactive({
    temp <- subset(stock_data, as.Date(date,"%Y-%m-%d") >= as.Date(input$date[1]) & as.Date(date,"%Y-%m-%d") <= as.Date(input$date[2]))
    subset(temp, Name == input$TichkeInput)
    })
  c_temp <- reactive({
    paste0(input$showfiger)
  })
  cords <- reactiveValues(xy=NULL)
  observeEvent(  
    input$pl_click,
    {
      if(!is.null(input$pl_click)){
        cords$xy <- input$pl_click[c('x', 'y')]
      }
    })
  observeEvent(input$go, {
    data_select<-data_select_Input()
    ymax<-max(data_select$high)
    ymin<-min(data_select$low)
    c<-c_temp()
    output$plot <- renderPlot({
      switch(c[1],
       open = plot(data_select$open,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=1,col="black", axes = F, lwd = 2),
       high = plot(data_select$high,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=2,col="red", axes = F, lwd = 2),
       low = plot(data_select$low,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=3,col="blue", axes = F, lwd = 2),
       close = plot(data_select$close,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=1,col="green", axes = F, lwd = 2))
      
      Date_show <- format(as.Date(data_select$date,"%Y-%m-%d"), "%Y-%m-%d")
      
      
      axis(1,labels=Date_show,at=1:length(data_select$date),las=1, tick = F)
      title(main='',ylab='Price',xlab='')
      
      for(n in 1:length(c))
        if(n==1){}
        else{
          switch(c[n],
                 open = lines(data_select$open,type="l",xaxt="n",ylab='',xlab='',lty=1,col="black", lwd = 2),
                 high = lines(data_select$high,type="l",xaxt="n",ylab='',xlab='',lty=2,col="red", lwd = 2),
                 low = lines(data_select$low,type="l",xaxt="n",ylab='',xlab='',lty=3,col="blue", lwd = 2),
                 close = lines(data_select$close,type="l",xaxt="n",ylab='',xlab='',lty=1,col="green", lwd = 2))
        }
      c_col<-c("")
      for(n in 1:length(c))
        c_col<-switch(c[n],
               open = c(c_col,c("black")),
               high = c(c_col,c("red")),
               low = c(c_col,c("blue")),
               close = c(c_col,c("green")))
      c_col<-c_col[-1]
      c_lty<-c(1)
      for(n in 1:length(c))
        c_lty<-switch(c[n],
                      open = c(c_lty,c(1)),
                      high = c(c_lty,c(2)),
                      low = c(c_lty,c(3)),
                      close = c(c_lty,c(1)))
      c_lty<-c_lty[-1]
      legend("topright",c,lty=c_lty,col=c_col)
      
      xy <- cords$xy
      if(!is.null(xy)){
        temp <- round(xy[['x']])
        xy_x <- as.character(Date_show[temp])
        xy_y <- round(xy[['y']],3)
        xy_final <- c(xy_x,xy_y)
        
        legend(xy,paste(as.list(xy_final), collapse=', '))
      } 
       
    })
  })
  

})