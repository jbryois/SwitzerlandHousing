library(shiny)
library(tidyverse)
library(DT)

# load model

model8 <- readRDS("../Data/processed/mixed_model8.rds")
sd_surface <- readRDS("../Data/processed/mixed_model_sd_surface.rds")
sd_rooms <- readRDS("../Data/processed/mixed_model_sd_rooms.rds")

listings <- readRDS("../Data/processed/location_canton_anibis.rds")

server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    tibble(
      Name = c("rooms",
               "surface"),
      Value = c(input$rooms,input$surface))
  })
  
  #Prediction
  predictValues <- reactive({
    listings <- mutate(listings,rooms=(input$rooms-4)/sd_rooms,surface=(input$surface-100)/sd_surface)
    listings <- mutate(listings,prediction=round(predict(model8,listings,allow.new.level=TRUE))) %>% 
      select(-surface,-rooms) %>%
      arrange(-prediction)
  })
  
  # Show the values in an HTML table ----
  
  output$pred = renderDataTable(isolate(predictValues()))
  
  proxy = dataTableProxy('pred')
  
  observe({
    replaceData(proxy, predictValues(), resetPaging = FALSE)
  })
  
  
}