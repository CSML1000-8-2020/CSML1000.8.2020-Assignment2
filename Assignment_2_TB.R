suppressMessages(library(dplyr)) 
suppressMessages(library(arules)) 
suppressMessages(library(arulesViz)) 
suppressMessages(library(stringr))
if (!require("pacman")) install.packages("pacman")

# p_load function installs missing packages and loads all the packages given as input
pacman::p_load("readr", 
               "data.table", 
               "tidyverse", 
               "dplyr", 
               "stringr", 
               "DT", 
               "ggplot2",
               "knitr",
               "magrittr",
               "treemap",
               "arules",
               "arulesViz")
orders<-read.csv("orders.csv")
head(orders)
orders %>% ggplot(aes(x= order_dow, fill = order_dow)) + geom_histogram(stat="count")
orders %>% 
  ggplot(aes(x= order_hour_of_day, fill = order_dow)) + 
  geom_histogram(stat="count") +
  facet_wrap(~ order_dow, ncol = 2)
products<-read.csv("products.csv")
departments<-read.csv("departments.csv")
aisles<-read.csv("aisles.csv")
products %>% 
  group_by(department_id, aisle_id) %>% summarize(count=n()) %>%
  left_join(departments,by="department_id") %>% 
  left_join(aisles,by="aisle_id") %>%
  treemap(index=c("department","aisle"),vSize="count",title="Tree map of Unique products offered in each Department/ aisle",palette="Set3",border.col="#FFFFFF")
order_products_train<-read.csv("order_products__train.csv")
order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(departments,by="department_id") %>% 
  left_join(aisles,by="aisle_id") %>%
  mutate(onesize = 1) %>%
  treemap(index=c("department","aisle"),vSize="sumcount",title="Tree map of most ordered products in Department/Aisle",palette="Set3",border.col="#FFFFFF")
order_products_prior<-read.csv("order_products__prior.csv")
# Split the "Product ID" values into groups based on "Order ID" variable
order_product <- order_products_prior %>% 
  left_join(products, by = "product_id")
transactions <- as(split(order_product$product_name,order_product$order_id),"transactions")

hist(size(transactions), 
     breaks = 0:150, 
     xaxt="n", 
     ylim=c(0,250000), 
     col = "blue",
     main = "Number of Products per Order", 
     xlab = "Order Size:Number of Products")
+ axis(1, 
       at = seq(0,160,by=10)) + 
  mtext(paste("Total:", length(transactions), "Orders,", sum(size(transactions)), "Products"))

