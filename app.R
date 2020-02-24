library(shiny)
library(quantmod)
library(fpp)
ui=fluidPage(
  titlePanel("StockPrediction"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select Ticker Yahoo Finance"),
      textInput("symb","Symbol","GOOG"),
      dateRangeInput("dates",
                     "Date Range",
                     start="2007-01-01",
                     end=as.character(Sys.Date())),
      br(),
      br()
    ),
    mainPanel(plotOutput("technical"),plotOutput("summary"))
  )
)

server=function(input,output){
  dataInput=reactive({
    getSymbols(input$symb,from=input$dates[1],
               to=input$dates[2],auto.assign = F)
    
  })
  output$technical=renderPlot({
    data=dataInput()
    chartSeries(data,type="candlesticks")
    addBBands()
    addRSI()
  })
  output$summary=renderPlot({
    datadf=as.data.frame(dataInput())
    dataclose=datadf[,4]
    dataclose=na.omit(dataclose)
    fit=auto.arima(ts(dataclose,frequency = 7),D=1)
    summary(fit)
    plot(forecast(fit,h=50))
    
  })
  
}

shinyApp(ui=ui,server=server)