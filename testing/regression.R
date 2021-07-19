# regression.R
# In this script the Bosten housing data set is used
# to predict the house price in bosten from house
# details. This is the foundation of an interactive
# shiny dashboard where you can compare two types 
# of linear models. A discription of the data can be
# found here:
# https://www.rdocumentation.org/packages/mlbench/versions/2.1-3/topics/BostonHousing


# Include shiny.
if (!require("shiny")) install.packages("shiny")
library(shiny)

# Install the package containing the data.
if (!require("mlbench")) install.packages("mlbench")

# Load the data set.
library(mlbench)
data(BostonHousing)
dim(BostonHousing)
head(BostonHousing)

fitLinearModel <- function(target, variables, dataset, splitRate){
  # Fits a linear model for the provided target and
  # variables of the passed data set. 
  modelFormula <- paste0(target, " ~")
  counter <- 0
  
  # Set up formula object for the linear model.
  for (variable in variables){
    if (counter == 0){
      modelFormula <- paste0(modelFormula, " ", variable)
    } else {
      modelFormula <- paste0(modelFormula, " + ", variable)
    }
    counter <- counter + 1
  }
  
  # Perform train/test split of the provided data set.
  set.seed(420)
  sampleSize <- floor(splitRate * nrow(dataset))
  trainIdx <- sample(seq_len(nrow(dataset)), size = sampleSize)
  trainData <- dataset[trainIdx, ]
  testData <- dataset[-trainIdx, ]
  
  # Fit the linear model and calculate predictions.
  modelFit <- lm(formula = modelFormula, data = trainData)
  modelAIC <- AIC(modelFit)
  modelPredictions <- predict(modelFit, newdata = testData)
  modelPredictionError <- abs(modelPredictions - testData[target])
  
  result <- list(model = modelFit, 
                 AIC = modelAIC, 
                 predictions = modelPredictions, 
                 predictionErrors = modelPredictionError,
                 trueValues = testData[target])
  
  return(result)
}


### Test cases ###
### ---------- ###

# Model 1
target <- colnames(BostonHousing)[length(colnames(BostonHousing))]
variables <- colnames(BostonHousing)[1:(length(colnames(BostonHousing)) - 1)]
fit <- fitLinearModel(target, variables, BostonHousing, 0.85)
summary(fit[[1]])
fit[[2]]
fit[[3]]
fit[[4]]
fit[[5]]

# Model 2
variables2 <- colnames(BostonHousing)[1:(length(colnames(BostonHousing)) - 5)]
fit2 <- fitLinearModel(target, variables2, BostonHousing, 0.85)

# Plot results.
#quartz()
#plot(c(1:length(fit[[5]][, 1])), fit[[5]][, 1], col = c(1:length(fit[[5]][, 1])), pch = 1, xlab = "Index", ylab = "Target")
#points(c(1:length(fit[[3]])), fit[[3]], col = c(1:length(fit[[3]])), pch = 3)
plot(c(1:length(fit[[5]][, 1])), fit[[5]][, 1], col = "blue", pch = 1, xlab = "Index", ylab = "Target")
lines(spline(c(1:length(fit[[5]][, 1])), fit[[5]][, 1]), col = "blue")
points(c(1:length(fit[[3]])), fit[[3]], col = "red", pch = 2)
lines(spline(c(1:length(fit[[3]])), fit[[3]]), col = "red")
points(c(1:length(fit2[[3]])), fit2[[3]], col = "green", pch = 3)
lines(spline(c(1:length(fit2[[3]])), fit2[[3]]), col = "green")

layout()


### Shiny app ###
### --------- ###

# Global scope
target <- colnames(BostonHousing)[length(colnames(BostonHousing))]
variables <- colnames(BostonHousing)[1:(length(colnames(BostonHousing)) - 1)]

# To Do: 
# - Maybe reduce calculations by isolating code
# - clean up app appearance

ui <- fluidPage(
  
  fluidRow(
    column(6,
      checkboxGroupInput("variablesModel1", "Choose variables for model 1:",
                         selected = variables[1],
                         choiceNames =
                           variables,
                         choiceValues =
                           variables
      ),
      
      textOutput("modelAIC_1")
    ),
    
    column(6,
      checkboxGroupInput("variablesModel2", "Choose variables for model 2:",
                         selected = variables[2],
                         choiceNames =
                           variables,
                         choiceValues =
                           variables
      )
    ),
    
    textOutput("modelAIC_2")
  ),
  
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$modelAIC_1 <- renderText({
    # Fit model
    target <- colnames(BostonHousing)[length(colnames(BostonHousing))]
    variables1 <- input$variablesModel1
    fit1 <- fitLinearModel(target, variables1, BostonHousing, 0.85)
    
    print(paste0("AIC of model 1: ", fit1[[2]]))
  })
  
  output$modelAIC_2 <- renderText({
    # Fit model
    target <- colnames(BostonHousing)[length(colnames(BostonHousing))]
    variables2 <- input$variablesModel2
    fit2 <- fitLinearModel(target, variables2, BostonHousing, 0.85)
    
    print(paste0("AIC of model 2: ", fit2[[2]]))
  })
  
  output$plot <- renderPlot({
    # Fit models
    target <- colnames(BostonHousing)[length(colnames(BostonHousing))]
    variables1 <- input$variablesModel1
    fit1 <- fitLinearModel(target, variables1, BostonHousing, 0.85)
    
    variables2 <- input$variablesModel2
    fit2 <- fitLinearModel(target, variables2, BostonHousing, 0.85)
    
    # Set up plot.
    plot(c(1:length(fit1[[5]][, 1])), fit1[[5]][, 1], 
         col = "blue", pch = 1, xlab = "Index", ylab = "Target", 
         main = "True predictions vs. fitted models")
    lines(spline(c(1:length(fit1[[5]][, 1])), fit1[[5]][, 1]), 
          col = "blue")
    points(c(1:length(fit1[[3]])), fit1[[3]], 
           col = "red", pch = 2)
    lines(spline(c(1:length(fit1[[3]])), fit1[[3]]), 
          col = "red")
    points(c(1:length(fit2[[3]])), fit2[[3]], 
           col = "green", pch = 3)
    lines(spline(c(1:length(fit2[[3]])), fit2[[3]]), 
          col = "green")
    legend(0, 50, legend = c("True values", "model 1", "model 2"), col = c("blue", "red", "green"), lty = 1)
  })
}

shinyApp(ui, server)



