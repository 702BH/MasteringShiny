# chapter 4 ER Injuries

library(shiny)
library(vroom)
library(tidyverse)
library(neiss)


top_prod <- injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  count(prod1, sort = TRUE) %>%
  filter(n > 5 * 365)

injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  semi_join(top_prod, by = "prod1") %>%
  mutate(age = floor(age), sex = tolower(sex), race = tolower(race)) %>%
  filter(sex != "unknown") %>%
  select(trmt_date, age, sex, race, body_part, diag, location, prod_code = prod1, weight, narrative) %>%
  vroom::vroom_write("neiss/injuries.tsv.gz")

products %>%
  semi_join(top_prod, by = c("code" = "prod1")) %>%
  rename(prod_code = code) %>%
  vroom::vroom_write("neiss/products.tsv")

population %>%
  filter(year == 2017) %>%
  select(-year) %>%
  rename(population = n) %>%
  vroom::vroom_write("neiss/population.tsv")


# Exploration





prod_codes <- setNames(products$code, products$title)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = prod_codes))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
  
)

server <- function(input, output, session){
  selected <- reactive(injuries %>% filter(code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt=weight, sort=TRUE)
  )
  
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population *1e4)
  }) 
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, color = sex)) +
      geom_line() + 
      labs(y = "Estimated number of injuries")
  }, res = 96)
  
}