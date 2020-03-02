# Load Libraries
library(knitr)
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
products_in_cart <- data.frame(product_id=integer(), product_name=character(), aisle_id=integer(), department_id=integer()) 
# products_in_cart <- products_for_dept[0,]

# Load Transaction data
suppressWarnings(
    tr <- read.transactions('./input/InstaCart_MBA.csv', format = 'basket', sep=',')
)

# rules <- readRDS("./input/rules.rds")

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
      actionButton("add_to_cart", "Add to Cart"),
      actionButton("clear_cart", "Clear Cart") 
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
        tableOutput(outputId = "product_added_to_cart"),
      )
    )

  )
)

# Define server function
server <- function(input, output, session) {
  
      session$userData$products_in_cart <- products_in_cart[0,]
      updateSelectInput(session=session, inputId = "cart_col",
                          label = NULL,
                          choices = session$userData$products_in_cart$product_name,
                          selected = NULL
        )

      output$dept_val <- renderText({
        sel_dept_row = which(departments$department == input$dept_col)
        session$userData$products_for_dept <<- subset(products, department_id %in% departments$department_id[sel_dept_row])
        updateSelectInput(session, "prod_col",
                          label = NULL,
                          choices = session$userData$products_for_dept$product_name,
                          selected = NULL
        )
        paste("Dept:", input$dept_col, sep=" ")
        })
    
      recommend_update <- function()
        {
        output$product_added_to_cart <- renderTable({
          grocery_item <- input$prod_col
          grocery_item
          rules <- apriori(tr, parameter = list(supp=0.00001, conf=0.3),
                   appearance = list(default="rhs", lhs=grocery_item),
                   control = list (verbose=F))
          rules_subset <- subset(rules, subset = lhs %in% grocery_item)#grocery_item)
          rules_conf <- sort (rules_subset, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
          # result <- as.data.frame(rules_conf)
          result <- as(rules_conf,"data.frame")
          
          if(is.null(result$rules)){
            result$rules <- "NA"
          }
          result$rules
          }, rownames=FALSE, colnames=FALSE)
      }
        
      observeEvent(input$clear_cart, {
        session$userData$products_in_cart <<- products_in_cart[0,]
        updateSelectInput(session=session, inputId = "cart_col",
                          label = NULL,
                          choices = session$userData$products_in_cart$product_name,
                          selected = NULL
         )
      })
      
      observeEvent(input$add_to_cart, {
        prod_row = which(session$userData$products_for_dept$product_name == input$prod_col)
        session$userData$products_in_cart <<- rbind(session$userData$products_in_cart, session$userData$products_for_dept[prod_row,])
        updateSelectInput(session, "cart_col",
                          label = NULL,
                          choices = session$userData$products_in_cart$product_name,
                          selected = NULL
        )
        recommend_update()
      })
      

}

# Run Shiny App
shinyApp(ui = ui, server = server)


