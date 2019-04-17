library(shiny)
library(DT)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Appartment Prices in Switzerland"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Text describing the shiny app
      h5("Prediction of appartment prices in Switzerland based on the number of rooms, surface and location."),
 
      # Input: Simple integer interval ----
      sliderInput("rooms", "Number of rooms:",
                  min = 1, max = 7,
                  value = 4,step = 0.5),
      
      # Input: Decimal interval with step value ----
      sliderInput("surface", "Surface (m2):",
                  min = 1, max = 250,
                  value = 100),
      p("Predictions are performed using a linear mixed model trained with data from ",a("anibis.ch",href="https://www.anibis.ch/fr/immobilier-immobilier-locations--410/advertlist.aspx")),
      p("Surface and number of rooms are modelled as fixed effects, while a random slope and intercepts are fit for each location and a random intercept for each canton"),
      p("The model achieves a mean absolute error of 225 CHF (root mean square of 340 CHF) on a test set (20% of all listings)"),
      p("Code is available on my",a("github page",href="https://github.com/jbryois")),
      p("Statistical Modeling is availble",a("here",href="https://github.com/jbryois"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      DTOutput('pred')
    )
  )
)