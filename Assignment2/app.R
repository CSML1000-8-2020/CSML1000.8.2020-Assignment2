#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Instacart Market Basket Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('analysisType', 'Choose a Analysis Type', c('Exploratory Data Analysis', 'Clustering Anaylysis', 'Association Rules Analysis')),
            
            conditionalPanel(
                condition = "input.analysisType == 'Exploratory Data Analysis'",
                selectInput("expAnalysis", "Exploratory Analysis", c('InstaCart Question Summary', 'Distribution Analysis'))
            ),
            conditionalPanel(
                condition = "input.analysisType == 'Clustering Anaylysis'",
                selectInput("clusterAnalysis", "Clustering Analysis", c('K-Means', 'Hierarchical', 'EM-GMM')),
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'K-Means'",
                    selectInput('x', 'X Variable', c('add_to_cart_order','reordered','order_number','order_dow','order_hour_of_day','days_since_prior_order','aisel_id', 'department_id')),
                    selectInput('y', 'Y Variable', c('add_to_cart_order','reordered','order_number','order_dow','order_hour_of_day','days_since_prior_order','aisel_id', 'department_id')),
                    selectInput('count','Cluster Count',c('3','5','10'))
                )
            ),
            conditionalPanel(
                condition = "input.analysisType == 'Association Rules Analysis'",
                selectInput("arulesAnalysis", "Asscociation Rules Analysis", c('Introduction', 'Rules Identified', 'Result'))
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            conditionalPanel(
                condition = "input.analysisType == 'Exploratory Data Analysis'",
                conditionalPanel(
                    condition = "input.expAnalysis == 'InstaCart Question Summary'",
                    htmlOutput("instacartsummary"),
                    imageOutput("instacart_sidebar")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Distribution Analysis'",
                    textOutput("summary")
                )
            ),
            conditionalPanel(
                condition = "input.analysisType == 'Clustering Anaylysis'",
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'K-Means'",
                    
                ),
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'Hierarchical'",
                    
                ),
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'EM-GMM'",
                    
                ),
            ),
            conditionalPanel(
                condition = "input.analysisType == 'Association Rules Analysis'",
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'Introduction'",
                    
                ),
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'Rules Identified'",
                    
                ),
                conditionalPanel(
                    condition = "input.clusterAnalysis == 'Result'",
                    
                ),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$instacartsummary <- renderUI(HTML(
        "<h3 align='center'>InstaCart Question Summary<h3><br>
        <p>Instacart, a company offering a grocery ordering and delivery app, through which , customers issue orders and personal shoppers review your order and do the in-store shopping and delivery for you.
         Kaggle hosts Instacart open-sourced transactional data with 3 million orders, which we used in this project for a practice with unsupervised clustering study.</p>
        "
    ))
    
    output$summary <- renderText({"This is a placeholder for text materials"})
    
    output$instacart_sidebar <- renderImage({
        filename <- normalizePath((file.path('images/instacart_sidebar.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
