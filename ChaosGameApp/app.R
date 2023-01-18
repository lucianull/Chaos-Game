library(shiny)
library(shape)
library(shinythemes)

# UI

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Chaos Game"),
  
  fluidRow(
    column(4,
           wellPanel(
             selectizeInput('shape', h5(tags$b('Shape')), choices = list(
               "Two Dimensions" = c('Triangle' = 'triangle',
                                    'Square' = 'square',
                                    'Pentagon' = 'pentagon',
                                    'Hexagon' = 'hexagon')
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
                           min = 1001, max = 100000, value = 1001, step = 500)
             )
           ))
    
  )
)

server <- function(input, output)
{
  #Afisarea figurii
  
  #  output$plot_initial <- renderPlot({
  #    tips <- figure
  #  })
  
}

shinyApp(ui = ui, server = server)