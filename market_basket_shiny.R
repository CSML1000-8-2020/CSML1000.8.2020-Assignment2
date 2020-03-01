# Load Libraries
library(knitr)
# library(ggplot2)
# library(lubridate)
library(arules)
library(arulesViz)
library('dplyr')
library('data.table')
library(shinydashboard)
library(shiny)

# Load files needed for selecting products
#aisles = read.csv("./input/aisles.csv")
departments = read.csv("./input/departments.csv")
products = read.csv("./input/products.csv")
products_for_dept <- subset(products, department_id %in% 1)
head(products)
head(products_for_dept)
products_in_cart <- data.frame(product_id=integer(), product_name=character(), aisle_id=integer(), department_id=integer()) 
#products_in_cart <- head(products_for_dept)

# Load Transaction data
suppressWarnings(
    tr <- read.transactions('./input/InstaCart_MBA.csv', format = 'basket', sep=',')
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Group 8 Market"),
  dashboardSidebar(
    selectInput('dept_col', 'Department', departments$department)
    #selectInput('aisle_col', 'Aisle', aisles$aisle)
  ),
  dashboardBody(title = "Group 8 Market", 
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        textOutput(outputId = "dept_val"),
        textOutput(outputId = "last_added"),
      ),
      actionButton("add_to_cart", "Add to Cart") 
    ),
    fluidRow(
      box(
        title = "Products:",
        selectInput('prod_col', NULL, products_for_dept$product_name, size = 10, selectize = FALSE),
      ),
      box(
        title = "Cart:",
        selectInput('cart_col', NULL, products_in_cart$product_name, size = 10, selectize = FALSE),
      )
    ),
    fluidRow(
      box(
        title = "Product Recomendations:", width=12,
        textOutput(outputId = "product_added_to_card"),
      )
    )

  )
)

# Define server function
server <- function(input, output, session) {
  
      output$dept_val <- renderText({
        sel_dept_row = which(departments$department == input$dept_col)
        products_for_dept <- subset(products, department_id %in% departments$department_id[sel_dept_row])
        updateSelectInput(session, "prod_col",
                          label = NULL,
                          choices = products_for_dept$product_name,
                          selected = NULL
        )
        paste("Dept:", input$dept_col, sep=" ")
        })
      
      observeEvent(input$add_to_cart, {
        # prod_row = which(products_for_dept$product_name == input$prod_col)
        # products_in_cart <- rbind(products_in_cart, prod_row)

        updateSelectInput(session, "cart_col",
                          label = NULL,
                          choices = rx_cart()$product_name,
                          selected = NULL
        )
      })
      
      rx_cart <- reactive({
        prod_row = which(products_for_dept$product_name == input$prod_col)
        tmp <- rbind(products_in_cart, products_for_dept[prod_row,])
      })
      
      output$product_added_to_card <- renderText({
        grocery_item = input$prod_col#"Garlic"
        rules <- apriori(tr, parameter = list(supp=0.001, conf=0.15),
                 appearance = list(default="rhs", lhs=grocery_item),
                 control = list (verbose=F))
        rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
        #
        result = inspect(head(rules_conf))
        result$rhs
      })
}

# Run Shiny App
shinyApp(ui = ui, server = server)


