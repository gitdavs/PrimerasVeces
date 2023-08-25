#Javier de Vicente. UC3M.
#install.packages("shiny")

library(shiny)

# Define UI for application that draws a histogram
ui<-fluidPage(  
  # Application title
  titlePanel("Consistencia"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("size",
                  "Sample Size:",
                  min = 30,
                  max = 50000,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


server<-function(input, output) {
  Population<-rnorm(1000000,4950,2500)+runif(1000000,0,1)
  Parametro<-quantile(Population,0.025)
  
  
  
  
  output$distPlot <- renderPlot({
    VaR97.5_1<-matrix(0,input$size/10,1)
    VaR97.5_2<-matrix(0,input$size/10,1)
    VaR97.5_3<-matrix(0,input$size/10,1)
    
    a=0
    for (i in seq(1,input$size,10)) {
      a=a+1
      Sample<-sample(Population,size=i,replace=FALSE)
      VaR97.5_1[a]<-mean(Sample)-2*sd(Sample)-Parametro
      VaR97.5_2[a]<-quantile(Sample,0.025)-Parametro
      VaR97.5_3[a]<-qnorm(0.025,mean(Sample),sd(Sample))-Parametro
      
    }
    ts.plot(VaR97.5_1,ylim=c(-500,500),xlab="Sample Size")
    lines(VaR97.5_2,col="blue")
    lines(VaR97.5_3,col="green")
    abline(0.666,0,col="red")
    abline(100,0,col="tomato2")
    abline(-100,0,col="tomato2")
  })
}


shinyApp(ui, server)
#Vemos como a partir de 10000 ya convergen el azul y verde. El negro tiene
#sesgo negativo. Pero vemos como el verde tiene menos varianza. 
#Con este grafico ya puedes tomar una decision justificada. 