library(shiny)
library(shape)
library(shinythemes)

# UI

NMAX <- 300000

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
                               'Barnsley Fern' ='barnsleyFern')
        ), selected = 'triangle'),
        
        conditionalPanel(
          condition = "input.shape =='pentagon'",
          selectizeInput('ruleset', h5(tags$b('Rules')), choices = list(
            "Rules" = c('Rule 1' = 'rule1',
                        'Rule 2' = 'rule2')
          ), selected = 'rule1')),
        
        conditionalPanel(
          condition = "input.shape =='square'",
          selectizeInput('rulesetSquare', h5(tags$b('Rules')), choices = list(
            "Rules" = c('Rule 1' = 'rule1',
                        'Rule 2' = 'rule2',
                        'Rule 3' = 'rule3'
            )
          ), selected = 'rule1')),
        
        conditionalPanel(
          condition = "input.shape =='barnsleyFern'",
          selectizeInput('rulesetFern', h5(tags$b('Rules')), choices = list(
            "Rules" = c('Rule 1' = 'rule1',
                        'Rule 2' = 'rule2'
            )
          ), selected = 'rule1')),
        
        conditionalPanel(
          condition = "input.shape =='triangle'",
          sliderInput("dist.triangle",
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.50, step = 0.01),
          div("For", tags$b("Triangle"), "default value is 0.50",
              style = "font-size: 9.5pt; color: teal", align= "left")
        ),
        conditionalPanel(
          condition = "input.shape=='square'&& input.rulesetSquare=='rule1' ",
          sliderInput("dist.square",
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.67, step = 0.01),
          div("For", tags$b("Square"), "default value is 2/3 (0.67)",
              style = "font-size: 9.5pt; color: teal", align = "left")
        ),
        conditionalPanel(
          condition = "input.shape=='square'&& input.rulesetSquare=='rule2' ",
          div("A point inside a square repeatedly jumps half of the distance towards a randomly chosen vertex, but the currently chosen vertex cannot neighbor the previously chosen vertex if the two previously chosen vertices are the same."),
          sliderInput("dist.squareRule2",
                      
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.5, step = 0.01),
          div("For", tags$b("Square"), " ruleset 2 default value is 0.50",
              style = "font-size: 9.5pt; color: teal", align = "left")
        ),
        
        conditionalPanel(
          condition = "input.shape=='square'&& input.rulesetSquare=='rule3' ",
          div("The current vertex cannot be chosen in the next iteration."),
          sliderInput("dist.squareRule3",
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.5, step = 0.01),
          div("For", tags$b("Square"), "ruleset 3 default value is 0.5",
              style = "font-size: 9.5pt; color: teal", align = "left")
        ),
        
        conditionalPanel(
          condition = "input.shape=='pentagon'&& input.ruleset=='rule1' ",
          sliderInput("dist.pentagon",
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.63, step = 0.01),
          div("For", tags$b("Pentagon"), "default value is 0.63",
              style = "font-size: 9.5pt; color: teal", align = "left")
        ),
        conditionalPanel(
          condition = "input.shape=='pentagon' && input.ruleset=='rule2'",
          div("A point inside a pentagon repeatedly jumps half of the distance towards a randomly chosen vertex, but the currently chosen vertex cannot neighbor the previously chosen vertex if the two previously chosen vertices are the same."),
          sliderInput("dist.pentagonRule2",
                      label = h5(tags$b("Distance ratio to endpoint:")),
                      min = 0.01, max = 0.99, value = 0.56, step = 0.01),
          div("For", tags$b("Pentagon"), "ruleset 2 default value is 0.56",
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
        
        radioButtons("nrPointsRadiobutton", "Size", c("1-100" = "1", "100-1000" = "2", "1001-300000" = "3")),
        
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
                      min = 1001, max = 300000, value = 1001, step = 100)
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
                 plotOutput("initialPlot", width = "550px", height = "550px")
               ),
               conditionalPanel(
                 condition = "input.nrPointsRadiobutton == 2",
                 plotOutput("extendedPlot", width = "550px", height = "550px")
               ),
               conditionalPanel(
                 condition = "input.nrPointsRadiobutton == 3",
                 plotOutput("completePlot", width = "550px", height = "550px")
               )
        )
      )
    )
  ))

# functions for server

Triangle <- function(distanceRatio)
{
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

Square <- function(distanceRatio)
{
  SquareVertices <- matrix(NA, ncol = 3, nrow = 4)
  SquareVertices[1,] <- c(1, 0, 0)
  SquareVertices[2,] <- c(2, 1, 0)
  SquareVertices[3,] <- c(3, 1, 1)
  SquareVertices[4,] <- c(3, 0, 1)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  for(i in 1:(NMAX - 1))
  {
    RandomVertex <- sample(1:4, 1)
    NextPointX <- RandomPointX + (SquareVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
    NextPointY <- RandomPointY + (SquareVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
    Points[i + 1, ] <- c(NextPointX, NextPointY)
    RandomPointX <- NextPointX
    RandomPointY <- NextPointY
  }
  return (list(SquareVertices, Points))
}

SquareRule2 <- function(distanceRatio)
{
  SquareVertices <- matrix(NA, ncol = 3, nrow = 4)
  SquareVertices[1,] <- c(1, 0, 0)
  SquareVertices[2,] <- c(2, 1, 0)
  SquareVertices[3,] <- c(3, 1, 1)
  SquareVertices[4,] <- c(3, 0, 1)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  RandomVertexP2 <- 1
  RandomVertexP1 <- 1
  for(i in 1:(NMAX - 1))
  {  
    RandomVertex <- sample(1:4, 1)
    if(
      ((RandomVertexP2 == RandomVertexP1) 
       &&
       (
         (RandomVertex == 1 && RandomVertexP1 != 2 && RandomVertexP1 != 4)
         ||
         (RandomVertex == 4  && RandomVertexP1 != 1 && RandomVertexP1 != 3) 
         ||
         (RandomVertex != 1  && RandomVertex!= 4 && RandomVertex != RandomVertexP1 +1 &&
          RandomVertex != RandomVertexP1 -1)
       )
      ) || (RandomVertexP2 != RandomVertexP1))
    {
      NextPointX <- RandomPointX + (SquareVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
      NextPointY <- RandomPointY + (SquareVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
      Points[i + 1, ] <- c(NextPointX, NextPointY)
      RandomPointX <- NextPointX
      RandomPointY <- NextPointY
      RandomVertexP2 <- RandomVertexP1
      RandomVertexP1 <- RandomVertex
    }
  }
  return (list(SquareVertices, Points))
}

SquareRule3 <- function(distanceRatio)
{
  SquareVertices <- matrix(NA, ncol = 3, nrow = 4)
  SquareVertices[1,] <- c(1, 0, 0)
  SquareVertices[2,] <- c(2, 1, 0)
  SquareVertices[3,] <- c(3, 1, 1)
  SquareVertices[4,] <- c(3, 0, 1)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  PreviousVertex <- 0
  for(i in 1:(NMAX - 1))
  {
    RandomVertex <- sample(1:4, 1)
    if(RandomVertex!=PreviousVertex)
    {
      NextPointX <- RandomPointX + (SquareVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
      NextPointY <- RandomPointY + (SquareVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
      Points[i + 1, ] <- c(NextPointX, NextPointY)
      RandomPointX <- NextPointX
      RandomPointY <- NextPointY
      PreviousVertex <- RandomVertex
    }
  }
  return (list(SquareVertices, Points))
}

Pentagon <- function(distanceRatio)
{
  PentagonVertices <- matrix(NA, ncol = 3, nrow = 5)
  PentagonVertices[1,] <- c(1, 0.2, 0)
  PentagonVertices[2,] <- c(2, 0.81, 0)
  PentagonVertices[3,] <- c(3, 1, 0.61)
  PentagonVertices[4,] <- c(4, 0.5, 1)
  PentagonVertices[5,] <- c(5, 0, 0.61)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  for(i in 1:(NMAX - 1))
  {
    RandomVertex <- sample(1:5, 1)
    NextPointX <- RandomPointX + (PentagonVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
    NextPointY <- RandomPointY + (PentagonVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
    Points[i + 1, ] <- c(NextPointX, NextPointY)
    RandomPointX <- NextPointX
    RandomPointY <- NextPointY
  }
  return (list(PentagonVertices, Points))
}



PentagonRule2 <- function(distanceRatio)
{
  PentagonVertices <- matrix(NA, ncol = 3, nrow = 5)
  PentagonVertices[1,] <- c(1, 0.2, 0)
  PentagonVertices[2,] <- c(2, 0.81, 0)
  PentagonVertices[3,] <- c(3, 1, 0.61)
  PentagonVertices[4,] <- c(4, 0.5, 1)
  PentagonVertices[5,] <- c(5, 0, 0.61)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  
  RandomVertexP2 <- 1
  RandomVertexP1 <- 1
  iterations <-0
  
  for(i in 2:(NMAX - 1))
  {
    RandomVertex <- sample(1:5, 1)
    if(
      ((RandomVertexP2 == RandomVertexP1) 
       &&
       (
         (RandomVertex == 1 && RandomVertexP1 != 2 && RandomVertexP1 != 5)
         ||
         (RandomVertex == 5  && RandomVertexP1 != 4 && RandomVertexP1 != 1) 
         ||
         (RandomVertex != 1  && RandomVertex!= 5 && RandomVertex != RandomVertexP1 +1 &&
          RandomVertex != RandomVertexP1 -1)
       )
      ) || (RandomVertexP2 != RandomVertexP1))
    {
      NextPointX <- RandomPointX + (PentagonVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
      NextPointY <- RandomPointY + (PentagonVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
      Points[i + 1, ] <- c(NextPointX, NextPointY)
      RandomPointX <- NextPointX
      RandomPointY <- NextPointY
      RandomVertexP2 <- RandomVertexP1
      RandomVertexP1 <- RandomVertex
    }
  }
  
  return (list(PentagonVertices, Points))
}


Hexagon <- function(distanceRatio)
{
  HexagonVertices <- matrix(NA, ncol = 3, nrow = 6)
  HexagonVertices[1,] <- c(1, 0.25, 0)
  HexagonVertices[2,] <- c(2, 0.75, 0)
  HexagonVertices[3,] <- c(3, 1, 0.5)
  HexagonVertices[4,] <- c(4, 0.75, 1)
  HexagonVertices[5,] <- c(5, 0.25, 1)
  HexagonVertices[6,] <- c(6, 0, 0.5)
  
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  RandomPointX <- runif(1, 0, 1)
  RandomPointY <- runif(1, 0, 1)
  Points[1, ] <- c(RandomPointX, RandomPointY)
  for(i in 1:(NMAX - 1))
  {
    RandomVertex <- sample(1:6, 1)
    NextPointX <- RandomPointX + (HexagonVertices[RandomVertex, 2] - RandomPointX) * distanceRatio
    NextPointY <- RandomPointY + (HexagonVertices[RandomVertex, 3] - RandomPointY) * distanceRatio
    Points[i + 1, ] <- c(NextPointX, NextPointY)
    RandomPointX <- NextPointX
    RandomPointY <- NextPointY
  }
  return (list(HexagonVertices, Points))
}

TwoTriangles <-function()
{
  TrianglesVertices <- matrix(NA, ncol=3, nrow = 6)
  TrianglesVertices[1,] <- c(1, 0, 0.5)
  TrianglesVertices[2,] <- c(2, 0.44, 0.21)
  TrianglesVertices[3,] <- c(3, 0.34, 0.38)
  TrianglesVertices[4,] <- c(4, 0.52, 0)
  TrianglesVertices[5,] <- c(5, 0.9, 0.28)
  TrianglesVertices[6,] <- c(6, 0.97, 0.91)
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  NextPointX <- runif(1, 0, 1)
  NextPointY <- runif(1, 0, 1)
  Points[1, ] <- c(0, 0)
  
  for(i in 1:(NMAX - 1))
  {
    chanceChangeTriangle = runif(1, 0, 1)
    if(chanceChangeTriangle <= 0.01)
    {
      NextPointX <- 0
      NextPointY <- 0.16 * NextPointY
    }
    else if(chanceChangeTriangle <= 0.86)
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- 0.85 * NextPointX + 0.04 * NextPointY
      NextPointY <- -0.04 * OldPointX + 0.85 * OldPointY + 1.6
    }
    else if(chanceChangeTriangle <= 0.93)
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- 0.2 * NextPointX - 0.26 * NextPointY
      NextPointY <- 0.23 * OldPointX + 0.22 * OldPointY + 1.6
    }
    else
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- -0.15 * OldPointX + 0.28 * OldPointY
      NextPointY <- 0.26 * OldPointX + 0.24 * OldPointY + 0.44
    }
    Points[i + 1, ] <- c(NextPointX, NextPointY)
  }
  for(i in 1:(NMAX - 1))
  {
    Points[i,1] <- Points[i,1] /5 + 0.5
    Points[i,2] <- Points[i,2] /10
  }
  return (list(TrianglesVertices, Points))
}

TwoTrianglesMutated <-function()
{
  TrianglesVertices <- matrix(NA, ncol=3, nrow = 6)
  TrianglesVertices[1,] <- c(1, 0, 0.5)
  TrianglesVertices[2,] <- c(2, 0.44, 0.21)
  TrianglesVertices[3,] <- c(3, 0.34, 0.38)
  TrianglesVertices[4,] <- c(4, 0.52, 0)
  TrianglesVertices[5,] <- c(5, 0.9, 0.28)
  TrianglesVertices[6,] <- c(6, 0.97, 0.91)
  Points <- matrix(NA, ncol=2, nrow = NMAX)
  NextPointX <- runif(1, 0, 1)
  NextPointY <- runif(1, 0, 1)
  Points[1, ] <- c(0, 0)
  
  for(i in 1:(NMAX - 1))
  {
    chanceChangeTriangle = runif(1, 0, 1)
    if(chanceChangeTriangle <= 0.02)
    {
      NextPointX <- 0
      NextPointY <- 0.25 * NextPointY - 0.4
    }
    else if(chanceChangeTriangle <= 0.86)
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- 0.95 * NextPointX + 0.005 * NextPointY - 0.002
      NextPointY <- -0.005 * OldPointX + 0.93 * OldPointY + 0.5
    }
    else if(chanceChangeTriangle <= 0.93)
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- 0.035 * NextPointX - 0.2 * NextPointY -0.09
      NextPointY <- 0.16 * OldPointX + 0.04 * OldPointY + 0.02
    }
    else
    {
      OldPointX <- NextPointX
      OldPointY <- NextPointY
      NextPointX <- -0.04 * OldPointX + 0.2 * OldPointY + 0.083
      NextPointY <- 0.16 * OldPointX + 0.04 * OldPointY + 0.12
    }
    Points[i + 1, ] <- c(NextPointX, NextPointY)
  }
  for(i in 1:(NMAX - 1))
  {
    Points[i,1] <- Points[i,1] /4 + 0.5
    Points[i,2] <- Points[i,2] /7
  }
  return (list(TrianglesVertices, Points))
}

server <- function(input, output)
{
  figure <-reactive({
    if(input$shape == "triangle")
    {
      return(Triangle(input$dist.triangle))
    }
    
    if(input$shape == "square")
    {
      if(input$rulesetSquare == "rule1")
        return(Square(input$dist.square))
      if(input$rulesetSquare == "rule2")
        return(SquareRule2(input$dist.squareRule2))
      if(input$rulesetSquare == "rule3")
        return(SquareRule3(input$dist.squareRule3))
    }
    if(input$shape == "pentagon")
    {
      if(input$ruleset == "rule1")
        return(Pentagon(input$dist.pentagon))
      else
        return(PentagonRule2(input$dist.pentagonRule2))
    }
    if(input$shape == "hexagon")
    {
      return(Hexagon(input$dist.hexagon))
    }
    if(input$shape == "barnsleyFern")
    {
      if(input$rulesetFern == "rule1")
        return(TwoTriangles(input$dist.barnsleyFern))
      else
        return(TwoTrianglesMutated(input$dist.barnsleyFern))
    }
  })
  output$initialPlot <- renderPlot({
    Vertices <- figure()[[1]]
    par(bg="#616161")
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$initialNrPoints - 1, 1], Points[1 : input$initialNrPoints - 1, 2], pch = 20, col="white")
    if(input$shape != "barnsleyFern")
    {
      points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 3, col="#d1d1d1")
    }
  })
  output$extendedPlot <- renderPlot({
    Vertices <- figure()[[1]]
    par(bg="#616161")
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$extendedNrPoints - 1, 1], Points[1 : input$extendedNrPoints - 1, 2], pch = 20, col="white")
    if(input$shape != "barnsleyFern")
    {
      points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 3, col="#d1d1d1")
    }
  })
  output$completePlot <- renderPlot({
    Vertices <- figure()[[1]]
    par(bg="#616161")
    Points <- figure()[[2]]
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(Points[1 : input$completeNrPoints - 1, 1], Points[1 : input$completeNrPoints - 1, 2], pch = 20, col="white")
    if(input$shape != "barnsleyFern")
    {
      points(Vertices[, 2], Vertices[, 3], pch = 20, cex = 3, col="#d1d1d1")
    }
  })
}

shinyApp(ui = ui, server = server)