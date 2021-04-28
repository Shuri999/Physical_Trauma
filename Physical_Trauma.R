#===============================================================================

library(tidyverse)
library(data.table)
library(inspectdf)
library(dplyr)
library(shiny)

# The data ----
injuries <- fread("injuries.csv")
products <- fread("products.csv")
population <- fread("population.csv")

injuries <- injuries %>% rename(Body_Part=body_part,Diagnosis=diag,Location=location)

# EDA ----
injuries %>% inspect_na()
products %>% inspect_na()
population %>% inspect_na()

# Look at the product associated with the most injuries
injuries$prod_code %>% 
  as.factor() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1,1] -> pc

products$title[products$prod_code == pc]

stair_step <- injuries %>% filter(prod_code == 1842)


# App de olacaq 4 dene analiz ----

#1 ci analiz
stair_step %>% count(diag, sort = T) 

#2 ci analiz
stair_step %>% count(body_part, sort = T) 

#3 cu analiz
stair_step %>% count(location, sort = T)

#4 cu analiz

stair_step %>% 
  count(age, sex) %>% 
  merge(population, by = c("age", "sex"), all.x = T) %>% 
  mutate(rate = n / population * 10000) %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line() + 
  labs(y = "Injuries per 10,000 people")

# MyShinyApp

ui <- fluidPage(
  titlePanel("Physical Trauma"),
  theme = shinytheme("superhero"),
  selectInput("code",label = "Choose",products$title),
  plotOutput("cou"),
  h4('------------------------------'),
  fluidRow(
    column(4,tableOutput("body")),
    column(4,tableOutput("diag")),
    column(4,tableOutput("loc")))

  
)

server <- function(input, output, session) {
  data <- reactive(injuries %>% merge(products,by ="prod_code",all.x = T) %>% filter(title==input$code))
  
  output$diag <- renderTable(
    data() %>% count(Diagnosis, sort = T)
  ) 
  output$body <- renderTable(
    data() %>% count(Body_Part, sort = T)
  ) 
  output$loc <- renderTable(
    data() %>% count(Location, sort = T)
  )
  output$cou <- renderPlot({
    data() %>% count(age, sex) %>% 
      merge(population, by = c("age", "sex"), all.x = T) %>% 
      mutate(rate = n / population * 10000) %>% 
      ggplot(aes(age, rate, colour = sex)) + 
      geom_line(size = 1.5) + 
      labs(y = "Injuries per 10,000 people") + theme_light()
  })
}

shinyApp(ui, server)

#===============================================================================
