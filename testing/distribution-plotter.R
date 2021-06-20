library(shiny)

# Define UI for application that draws different distributions
ui <- fluidPage(
    
    # Application title
    titlePanel("Distribution plotter"),
    
    # Sidebar with different distr. / params 
    sidebarLayout(
        sidebarPanel(
            selectInput("distr",
                        label = "Which distribution?",
                        choices = c("Beta", "Normal", "t"), selected="Beta"),
            conditionalPanel(
                condition = "input.distr == 'Beta'",
                sliderInput(
                    "alpha",
                    "Value of Alpha",
                    1,
                    10,
                    1,
                    step = 1)
            ),
            conditionalPanel(
                condition = "input.distr == 'Beta'",
                sliderInput(
                    "beta",
                    "Value of Beta",
                    1,
                    10,
                    1,
                    step = 1)
            ),
            conditionalPanel(
                condition = "input.distr == 'Normal'",
                sliderInput(
                    "mu",
                    "Value of Mu",
                    -10,
                    10,
                    0,
                    step = 1)
            ),
            conditionalPanel(
                condition = "input.distr == 'Normal'",
                sliderInput(
                    "sigma",
                    "Value of Sigma",
                    1,
                    10,
                    1,
                    step = 1)
            ),
            conditionalPanel(
                condition = "input.distr == 't'",
                sliderInput(
                    "t_DF",
                    "Degrees of freedom",
                    1,
                    100,
                    1,
                    step = 1)
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
        
    )
)

# Define server logic required to draw distribution plots
server <- function(input, output) {
    pbeta <- seq(0,1,0.01)
    pnormal <- seq(-10, 10, 0.01)
    pt <- seq(-10, 10, 0.01)
    output$plot <- renderPlot({
        if (input$distr == "Beta"){
            plot(pbeta, dbeta(pbeta, input$alpha, input$beta), 
                 type="l", 
                 ylab="Density",
                 xlab ="X",
                 col = 4)
        } else if (input$distr == "Normal"){
            plot(pnormal, dnorm(pnormal, input$mu, input$sigma), 
                 type="l",
                 ylab="Density",
                 xlab ="X",
                 col = 3)
        } else if (input$distr == "t"){
            plot(pt, dt(pt, input$t_DF), 
                 type = "l",
                 ylab="Density",
                 xlab ="X",
                 col = 2)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
