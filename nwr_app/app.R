library(shiny)
library(reticulate)
source_python('nwr_model.py')
nwrf <- phos_func

# NWR app #

# Function takes feedstock and manure volume arguments and computes phosphorus nutrient outflow
# in kg/time step.
# 
# 3.79 constant converts gallon to liter (1L = 1kg)
# 0.9112 nutrient portion remaining post liquid-solid separation after AD output to lagoon
# 2.205 constant converts kilogram to pound
# 0.00378541 constant converts mg/ml to kg/gal
# 1000 constant converts kg to cubic meter
# kWh 1.02264 correction factor, 39.2 calorific value, 3.6 conversion factor

# Sources python script from previous application
# Function arguments (blood, fish_by, paper_pulp, OB, GT, manure, loc, soil_test, acres)
# Output:
## 1 total phos output
## 2 phos output lbs/acre
## 3 phos index categorization
## 4 recommended phos application per index categorization
## 5 application decision
## 6 tipping fee revenue
## 7 renewable energy generated from methane (kWh)

# user interface -----------------------------------------------------------------------------
ui <- fluidPage(
  
  ### HTML build ###
  titlePanel('Nutrient Waste Recycling Model'),
  
  tags$hr(),
  
  tabsetPanel(
    
    tabPanel('Welcome',
             
             tags$p(),
             
             tags$p('This model computes anaerobic digestion (AD) phosphorus output for lagoon
                    application and energy production in kilowatt hours from user feedstock and
                    manure inputs. The model also computes total revenue generated from tipping fees.'),
             
             tags$p('Enter input values below for computing AD phosphorus output (lbs) and recommended
                    lbs/acre crop application. Energy production (kWh) and tipping fee revenue
                    information is provided below phosphorus output and application recommendation.')
             
             ),
  
    tabPanel('User Inputs',
             
             tags$p(),
             
             tags$p('Enter input values below to generate model results.'),
             
             column(2,
                    
                    tags$p(),
                    
                    inputPanel(
                      
                      ### user input build ###
                      # define producer AD inputs
                      numericInput('blood', 'Blood Product (gal)', min = 0, value = 0),
                      numericInput('fish_by', 'Fish Byproduct (gal)', min = 0, value = 0),
                      numericInput('paper_pulp', 'Paper Pulp (gal)', min = 0, value = 0),
                      numericInput('OB', 'Out-of-Date Beverages (gal)', min = 0, value = 0),
                      numericInput('GT', 'Grease Trap Waste (gal)', min = 0, value = 0),
                      numericInput('manure', 'Manure (gallons)', min = 0, value = 0),
                      textInput('loc', 'Location West or East of the Cascades', value = 'e.g. East'),
                      numericInput('soil_test', 'Last Phosphorus Soil Test Value (ppm)', min = 0, value = 0),
                      numericInput('acres', 'Crop Acres for Effluent Application', min = 0, value = 0),
                      
                      # calculation action
                      actionButton('calc', 'Calculate AD Output')
                      
                      )
                    
                    ),
             
             column(6,
                      
                    tags$p(textOutput('ADout1')),
                    tags$p(textOutput('ADout2')),
                    tags$p(textOutput('ADout3')),
                    tags$p(textOutput('ADout4')),
                    tags$p(textOutput('ADout5')),
                    tags$p(textOutput('ADout6')),
                    tags$p(textOutput('ADout7'))
                      
                    )
               
               
             
             )
    
    )
  
)

# input modeling ------------------------------------------------------------------------------
server <- function(input, output){
  
  # reactive input activated only when action button 'clicked'
  fun <- eventReactive(input$calc, {
    
    # sourced python AD output function
    nwrf(input$blood, input$fish_by, input$paper_pulp,
         input$OB, input$GT, input$manure, loc = input$loc,
         soil_test = input$soil_test, acres = input$acres)
    
    })
  
  output$ADout1 <- renderPrint({
    
    cat('Total AD phosporus output:', fun()[[1]], 'lbs')
    
  })
  
  output$ADout2 <- renderPrint({
    
    cat('Phosphorus lbs/acre from total AD output:', fun()[[2]])
    
  })
  
  output$ADout3 <- renderPrint({
    
    cat('Phosphorus index categorization per soil test:', fun()[[3]])
    
  })
  
  output$ADout4 <- renderPrint({
    
    cat('Recommended phosphours application range', fun()[[4]], 'lbs/acre')
    
  })
  
  output$ADout5 <- renderPrint({
    
    cat('Application decision based on lbs/acre output:', fun()[[5]])
    
  })
  
  output$ADout6 <- renderPrint({
    
    cat('Revenue from tipping fees:', '$', fun()[[6]])
    
  })
  
  output$ADout7 <- renderPrint({
    
    cat('Approximate energy produced from total input biogas potential:', fun()[[7]], 'kWh')
    
  })
  
}

# build ----------------------------------------------------------------------------------------
shinyApp(ui, server)
