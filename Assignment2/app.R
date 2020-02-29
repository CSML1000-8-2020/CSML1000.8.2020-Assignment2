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
            selectInput('analysisType', 'Choose a Analysis Type', c('Exploratory Data Analysis', 'Association Rules Analysis')),
            
            conditionalPanel(
                condition = "input.analysisType == 'Exploratory Data Analysis'",
                selectInput("expAnalysis", "Exploratory Analysis", c('InstaCart Question Summary', 'Top Sellers', 'Order Size Distribution','First Item in Cart', 'Days Since Prior Order','Best Reordered Items','Sale By Department/Aisles'))
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
                    htmlOutput("desc_instacart"),
                    imageOutput("img_instacart")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Top Sellers'",
                    htmlOutput("desc_topseller"),
                    imageOutput("img_topseller")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Order Size Distribution'",
                    htmlOutput("desc_ordersize"),
                    imageOutput("img_ordersize")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'First Item in Cart'",
                    htmlOutput("desc_firstitem"),
                    imageOutput("img_firstitem")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Days Since Prior Order'",
                    htmlOutput("desc_dayssinceprior"),
                    imageOutput("img_dayssinceprior")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Best Reordered Items'",
                    htmlOutput("desc_bestreordered"),
                    imageOutput("img_bestreordered")
                ),
                conditionalPanel(
                    condition = "input.expAnalysis == 'Sale By Department/Aisles'",
                    htmlOutput("desc_salebyaisle"),
                    imageOutput("img_salebyaisle")
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

    #output$summary <- renderText({"This is a placeholder for text materials"})
    
    output$desc_instacart <- renderUI(HTML(
        "<h3 align='center'>InstaCart Question Summary<h3><br>
        <p>Instacart, a company offering a grocery ordering and delivery app, through which , customers issue orders and personal shoppers review your order and do the in-store shopping and delivery for you.
         Kaggle hosts Instacart open-sourced transactional data with 3 million orders, which we used in this project for a practice with unsupervised clustering study.</p>
        "
    ))
    
    output$img_instacart <- renderImage({
        filename <- normalizePath((file.path('images/instacart_sidebar.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    
    
    output$desc_topseller <- renderUI(HTML(
        "<h3 align='center'>What Products People Like Most?<h3><br>
         <p> A descriptive analysis of the frequency of each product that get ordered will provide some insight about what people are spending money with and tells us which target product should we use in the following association analysis.</p>
        "
    ))
    
    output$img_topseller <- renderImage({
        filename <- normalizePath((file.path('images/top_sellers.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    
    
    output$desc_ordersize <- renderUI(HTML(
        "<h3 align='center'>How Many People Put in Their Basket<h3><br>
         <p> The size of the order is an important descriptive analysis. This give us how big people's order will be, among the thousands of products available, they will buy a few each time anyhow.</p>
        "
    ))
    
    output$img_ordersize <- renderImage({
        filename <- normalizePath((file.path('images/order_size_distribution.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
   

    output$desc_firstitem <- renderUI(HTML(
        "<h3 align='center'>What People Look For Fisrt<h3><br>
         <p> When people come to shop, what is the most important item they do not want to miss on their mind? Is the first one more likely the anchor item that all the following items somehow asscociated with the first one?</p>
        "
    ))
    
    output$img_firstitem <- renderImage({
        filename <- normalizePath((file.path('images/first_item_in_cart.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    

    output$desc_dayssinceprior <- renderUI(HTML(
        "<h3 align='center'>How offen People Shop?<h3><br>
         <p> If a customer buy something today and he need a refill, how many days will he wait for?</p>
        "
    ))
    
    output$img_dayssinceprior <- renderImage({
        filename <- normalizePath((file.path('images/days_since_prior.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    

    output$desc_bestreordered <- renderUI(HTML(
        "<h3 align='center'>What are the product People ReOrder?<h3><br>
         <p> If a customer buy something today and he need a refill, what type of product it will most likely be?</p>
        "
    ))
    
    output$img_bestreordered <- renderImage({
        filename <- normalizePath((file.path('images/best_reordered.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    
    
    output$desc_salebyaisle <- renderUI(HTML(
        "<h3 align='center'>From which Department/aisles People Shop most often?<h3><br>
         <p> With all the different department and aisles, where people buy most?</p>
        "
    ))
    
    output$img_salebyaisle <- renderImage({
        filename <- normalizePath((file.path('images/sale_by_depart_aisle.png')))
        list(src=filename,alt="just a image", style="display: block; margin-left: auto; margin-right: auto;", width=800, height=600)
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
