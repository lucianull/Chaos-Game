library(shiny)
library(shape)
library(shinythemes)

# UI

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Chaos Game"),
  
  sidebarLayout(
    sidebarPanel(
           wellPanel(
             selectizeInput('shape', h5(tags$b('Shape')), choices = list(
               "Two Dimensions" = c('Triangle' = 'triangle',
                                    'Square' = 'square',
                                    'Pentagon' = 'pentagon',
                                    'Hexagon' = 'hexagon',
                                    'Two Triangles' ='twoTriangles')
             ), selected = 'triangle'),
             conditionalPanel(
               condition = "input.shape =='triangle'",
               sliderInput("dist.triangle",
                           label = h5(tags$b("Distance ratio to endpoint:")),
                           min = 0.01, max = 0.99, value = 0.50, step = 0.01),
               div("For", tags$b("Triangle"), "default value is 0.50",
                   style = "font-size: 9.5pt; color: teal", align= "left")
             ),
             conditionalPanel(
               condition = "input.shape=='square'",
               sliderInput("dist.square",
                           label = h5(tags$b("Distance ratio to endpoint:")),
                           min = 0.01, max = 0.99, value = 0.67, step = 0.01),
               div("For", tags$b("Square"), "default value is 2/3 (0.67)",
                   style = "font-size: 9.5pt; color: teal", align = "left")
             ),
             conditionalPanel(
               condition = "input.shape=='pentagon'",
               sliderInput("dist.pentagon",
                           label = h5(tags$b("Distance ratio to endpoint:")),
                           min = 0.01, max = 0.99, value = 0.63, step = 0.01),
               div("For", tags$b("Pentagon"), "default value is 0.63",
                   style = "font-size: 9.5pt; color: teal", align = "left")
             ),
             conditionalPanel(
               condition = "input.shape=='hexagon'",
               sliderInput("dist.hexagon",
                           label = h5(tags$b("Distance ratio to endpoint:")),
                           min = 0.01, max = 0.99, value = 0.67, step = 0.01),
               div("For", tags$b("Hexagon"), "default value is 2/3 (0.67)",
                   style = "font-size: 9.5pt; color: teal", align = "left")
             ),
             conditionalPanel(
               condition = "input.shape =='twoTriangles'",
               sliderInput("dist.twoTriangles",
                           label = h5(tags$b("Distance ratio to endpoint:")),
                           min = 0.01, max = 0.99, value = 0.50, step = 0.01),
               div("For", tags$b("Two Triangles"), "default value is 0.50",
                   style = "font-size: 9.5pt; color: teal", align= "left")
             ),
             
             radioButtons("nrPointsRadiobutton", "Size", c("1-100" = "1", "100-1000" = "2", "1001-100000" = "3")),
             
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 1",
               sliderInput("initialNrPoints",
                           label = h5(tags$b("Number of points:")),
                           min = 1, max = 100, value = 1, step = 1)
             ),
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 2",
               sliderInput("extendedNrPoints",
                           label = h5(tags$b("Number of points:")), 
                           min = 101, max = 1000, value = 101, step = 20)
             ),
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 3",
               sliderInput("completeNrPoints",
                           label = h5(tags$b("Number of points:")),
                           min = 1001, max = 10000, value = 1001, step = 100)
             )
           )
    
  ),
  mainPanel(
    fluidRow(
      column(4,
             plotOutput("distPlot")),
      column(4,
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 1",
               plotOutput("initialPlot", width = "600px", height = "600px")
             ),
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 2",
               plotOutput("extendedPlot", width = "600px", height = "600px")
             ),
             conditionalPanel(
               condition = "input.nrPointsRadiobutton == 3",
               plotOutput("completePlot", width = "600px", height = "600px")
             )
            )
    )
  )
))

# functions for server

Triangle <- function(distanceRatio)
{
  NMAX <- 10000
  aux <- sqrt(3)/2
  TriangleVertices <- matrix(NA, ncol = 3, nrow = 3)
  TriangleVertices[1,] <- c(1, 0, 0)
  TriangleVertices[2,] <- c(2, 1, 0)
  TriangleVertices[3,] <- c(3, 0.5, aux)
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  for(i in 1:(NMAX - 1))
  {
    RandomVertex <- sample(1:3, 1)
    NextPointX <- RandomPointX + (TriangleVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
    NextPointY <- RandomPointY + (TriangleVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
    Points[i + 1, ] <- c(NextPointX, NextPointY)
    RandomPointX <- NextPointX
    RandomPointY <- NextPointY
  }
  return (list(TriangleVertices, Points))
  
}

# Square <- function(distanceRatio)

server <- function(input, output)
{

  figure <-reactive({
    if(input$shape == "triangle")
    {
      return(Triangle(input$dist.triangle))
    }
  })
  output$initialPlot <- renderPlot({
    Vertices <- figure()[[1]]
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$initialNrPoints - 1, 1], Points[1 : input$initialNrPoints - 1, 2], pch = 20, col="black")
    points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 2, col="blue")
  })
  output$extendedPlot <- renderPlot({
    Vertices <- figure()[[1]]
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$extendedNrPoints - 1, 1], Points[1 : input$extendedNrPoints - 1, 2], pch = 20, col="black")
    points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 2, col="blue")
  })
  output$completePlot <- renderPlot({
    Vertices <- figure()[[1]]
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$completeNrPoints - 1, 1], Points[1 : input$completeNrPoints - 1, 2], pch = 20, col="black")
    points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 2, col="blue")
  })
}

shinyApp(ui = ui, server = server)