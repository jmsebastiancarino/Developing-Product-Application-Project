# Loading all the necessary and relevant packages
library(shiny)
library(ggplot2)
library(datasets)
library(shinythemes)

## Define server logic
shinyServer(function(input, output, session) {
  
## Creating the dataset menu
  
output$first <- renderUI({
                selectInput("Data", "Choose your data:", 
                             choices = c("Airquality",
                                         "Cars",
                                         "Swiss"), selected = "Airquality")
              
          })

## Creating a menu of Variables which will be the basis as the dependent Variable 

output$second <- renderUI({
                if(is.null(input$Data)){
                        return()

                        }
                
                if(input$Data == "Cars"){
                  selectInput("Variable", "Choose your dependent Variable:",
                               choices = as.character(names(mtcars)))
                  
                } else if(input$Data == "Airquality"){
                   selectInput("Variable", "Choose your dependent Variable:",
                                choices = as.character(names(airquality)), selected = as.character(names(airquality))[1])
                  
                } else {
                  selectInput("Variable", "Choose your dependent Variable:",
                               choices = as.character(names(swiss)))
                  
                }
                               

                               
          })


## Creating a menu of Machine Learning method

output$third <- renderUI({
                if(is.null(input$Data)){
                  return()
                  
                }
                               
                selectInput("algorithm", "Choose your method", 
                             choices = c("Regression", "Random Forest", "Trees"))
              
          })
  
## Creating the output result which is the details of the final Model based on the choices made in the setup  

output$summary <- renderPrint({
          
          ## If the choice dataset is "Cars"
  
                if(input$Data == "Cars"){
                        
                        ## Setting up the output based on the chosen Machine Learning method
                  
                            if(input$algorithm == "Regression"){
                                  
                                  ## Create the specification of the model 
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                  ## Create the model
                                  cars_glm <- train(var_formula, method = "glm", data = mtcars, na.action = na.omit)
                                  
                                  ## Final model details
                                  summary(cars_glm)
                            
                            } else if(input$algorithm == "Random Forest"){
                                  
                                  ## Create the specification of the model 
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                  ## Create the model
                                  cars_rf <- train(var_formula, method = "rf", data = mtcars, na.action = na.omit)
                                  
                                  ## Final model details
                                  cars_rf
                              
                            } else {
                              
                                  ## Create the specification of the model 
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                  ## Create model
                                  cars_rpart <- train(var_formula, method = "rpart", data = mtcars, na.action = na.omit)
                                  
                                  ## Final model details
                                  cars_rpart
                                  
                            }
                
         ## If the choice dataset is "Airquality"      
             
               } else if(input$Data == "Airquality"){
                          
                      ## Setting up the output based on the chosen Machine Learning method
                 
                          if(input$algorithm == "Regression"){
                             
                                  ## Create the specification of the model  
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                  ## Create model
                                  airquality_glm <- train(var_formula, method = "glm", data = airquality, na.action = na.omit)
                                  
                                  ## Final model details
                                  summary(airquality_glm)
                                
                                
                          } else if(input$algorithm == "Random Forest"){
                          
                                  ## Create the specification of the model
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                  ## Create model
                                  airquality_rf <- train(var_formula, method = "rf", data = airquality, na.action = na.omit)
                                  
                                  ## Final model details
                                  airquality_rf
                          
                                        
                          } else {
                                
                                  ## Create the specification of the model
                                  var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                  ## Create model
                                  airquality_rpart <- train(var_formula, method = "rpart", data = airquality, na.action = na.omit)
                                  
                                  ## Final model details
                                  airquality_rpart
                                  
                          }
                 
          ## If the choise dataset is "Swiss"      
               } else {
               
                      ## Setting up the output based on the chosen Machine Learning method
                 
                          if(input$algorithm == "Regression"){
                                   
                                   ## Create the specification of the model
                                   var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                   ## Create model
                                   swiss_glm <- train(var_formula, method = "glm", data = swiss, na.action = na.omit)
                                   
                                   ## Final model details
                                   summary(swiss_glm)
                      
                                     
                      } else if(input$algorithm == "Random Forest"){
                                   
                                   ## Create the specification of the model
                                   var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                   ## Create model
                                   swiss_rf <- train(var_formula, method = "rf", data = swiss, na.action = na.omit)
                                   
                                   ## Final model details
                                   swiss_rf
                        
                      } else {
                                   ## Create the specification of the model
                                   var_formula <- as.formula(paste(input$Variable, "~."))
                                  
                                   ## Create model
                                   swiss_rpart <- train(var_formula, method = "rpart", data = swiss, na.action = na.omit)
                                  
                                   ## Final model details
                                   swiss_rpart
                        
                      }
              }
    })


## Creating the plot output 

output$plot <- renderPlot({
  
                    ## If the choice dataset is "Cars"
                    
                    if(input$Data == "Cars"){
                      
                      ## Setting up the output based on the chosen Machine Learning method
                      
                      if(input$algorithm == "Regression"){
                        
                                ## Create the specification of the model 
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create the model
                                cars_glm <- train(var_formula, method = "glm", data = mtcars, na.action = na.omit)
                                
                                ## Final model details
                                par(mfrow = c(2,2))
                                plot(cars_glm$finalModel)
                        
                      } else if(input$algorithm == "Random Forest"){
                        
                                ## Create the specification of the model 
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create the model
                                cars_rf <- train(var_formula, method = "rf", data = mtcars, na.action = na.omit)
                                
                                ## Final model details
                                plot(cars_rf$finalModel)
                                
                      } else {
                        
                                ## Create the specification of the model 
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                cars_rpart <- train(var_formula, method = "rpart", data = mtcars, na.action = na.omit)
                                
                                ## Final model details
                                fancyRpartPlot(cars_rpart$finalModel)
                        
                      }
                      
            ## If the choice dataset is "Airquality"      
                      
            } else if(input$Data == "Airquality"){
                      
                   ## Setting up the output based on the chosen Machine Learning method
                      
                      if(input$algorithm == "Regression"){
                                
                                ## Create the specification of the model  
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                airquality_glm <- train(var_formula, method = "glm", data = airquality, na.action = na.omit)
                                
                                ## Final model details
                                par(mfrow = c(2,2))
                                plot(airquality_glm$finalModel)
                                
                        
                      } else if(input$algorithm == "Random Forest"){
                        
                                ## Create the specification of the model
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                airquality_rf <- train(var_formula, method = "rf", data = airquality, na.action = na.omit)
                                
                                ## Final model details
                                plot(airquality_rf$finalModel)
                                
                      } else {
                        
                                ## Create the specification of the model
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                airquality_rpart <- train(var_formula, method = "rpart", data = airquality, na.action = na.omit)
                                
                                ## Final model details
                                fancyRpartPlot(airquality_rpart$finalModel)
                        
                      }
                      
             ## If the choise dataset is "Swiss"      
                } else {
                      
                   ## Setting up the output based on the chosen Machine Learning method
                      
                      if(input$algorithm == "Regression"){
                        
                                ## Create the specification of the model
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                swiss_glm <- train(var_formula, method = "glm", data = swiss, na.action = na.omit)
                                
                                ## Final model details
                                par(mfrow = c(2,2))
                                plot(swiss_glm$finalModel)
                        
                        
                      } else if(input$algorithm == "Random Forest"){
                        
                                ## Create the specification of the model
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                swiss_rf <- train(var_formula, method = "rf", data = swiss, na.action = na.omit)
                                
                                ## Final model details
                                plot(swiss_rf$finalModel)
                        
                      } else {
                                ## Create the specification of the model
                                var_formula <- as.formula(paste(input$Variable, "~."))
                                
                                ## Create model
                                swiss_rpart <- train(var_formula, method = "rpart", data = swiss, na.action = na.omit)
                                
                                ## Final model details
                                fancyRpartPlot(swiss_rpart$finalModel)
                        
                      }
                    }

  
  
          })

## Create a print output where the data of the dataset is shown
output$Data <- renderPrint({
                
                  ## If the dataset chosen is "Cars"
                  if(input$Data == "Cars"){
                        
                             mtcars
                    
                  } else if(input$Data == "Airquality"){
                    
                             airquality
                    
                  } else {
                    
                             swiss
                      
                  }
  
          })

## Create a clock time
output$Time <- renderText(
          {
                  invalidateLater(1000)
                  paste("Time:", format(Sys.time(), "%X"))        
  
          })

## Create a date
output$Date <- renderText(
          {
                 paste("Date:", format(Sys.time(), "%B %d %Y"))
  
          })

## Create Remarks
output$Remarks <- renderText({
          
                "Author: Sebastian Carino"
          })

## Create Instruction 
output$Introduction <- renderText({
  
                "Welcome to Interactive Data Science. The creation of this shiny web app is inspired by Andrew Ng and professors, Roger Peng, Jeff Leek, and Brian Caffo. 
  Through their lectures about data science especially the machine learning models, I created a simple calculator-like application where visitors can see outputs from models such as regression, random forest, and trees
  using the installed datasets in R such as mtcars, swiss, and airquality."
          })

})