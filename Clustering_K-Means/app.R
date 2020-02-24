

# ###### Aisles #####################################################
# # Concatenate data file subdir and name.
# aisles_file <- paste("./Dataset/aisles.csv",sep="")
# # Load full data
# aisles = read.csv(aisles_file)
# 
# ###### Departments ################################################
# # Concatenate data file subdir and name.
# departments_file <- paste("./Dataset/departments.csv",sep="")
# # Load full data
# departments = read.csv(departments_file)
# 
# ###### Order Products Train #######################################
# # Concatenate data file subdir and name.
# order_products__train_file <- paste("./Dataset/order_products__train.csv",sep="")
# # Load full data
# order_products__train = read.csv(order_products__train_file, header = TRUE, sep=",", nrows = 100000)
# 
# ###### Orders #####################################################
# # Concatenate data file subdir and name.
# orders_file <- paste("./Dataset/orders.csv",sep="")
# # Load full data
# orders = read.csv(orders_file)#, header = TRUE, sep=",", nrows = 500000)
# 
# ###### Products ###################################################
# # Concatenate data file subdir and name.
# products_file <- paste("./Dataset/products.csv",sep="")
# # Load full data
# products = read.csv(products_file)
# 
# 
# names(aisles)
# names(departments)
# names(order_products__train)
# names(orders)
# names(products)
# 
# mydata <- merge(order_products__train,orders,by="order_id")
# mydata <- merge(mydata,products,by="product_id")
# names(mydata)
# # mydata <- arrange(mydata, order_id)
# # mydata <- arrange(mydata, add_to_cart_order)
# mydata_KMeans <- mydata[,c(3,4,7,8,9,10,12,13)]
#write.csv(mydata_KMeans,'InstaCart_KMeans.csv')
Insta_KMeans_file <- paste("./Dataset/InstaCart_KMeans.csv",sep="")
mydata_KMeans <- read.csv(Insta_KMeans_file)
mydata_KMeans <- mydata_KMeans[,2:9]
names(mydata_KMeans)

ncol(mydata_KMeans)


library(shiny)

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
    headerPanel('Instacart k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', names(mydata_KMeans)),
        selectInput('ycol', 'Y Variable', names(mydata_KMeans),
                    selected=names(mydata_KMeans)[[2]]),
        numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 10)
    ),
    mainPanel(
        plotOutput('plot1')
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        mydata_KMeans[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
