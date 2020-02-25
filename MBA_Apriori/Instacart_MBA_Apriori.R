library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

# ###### Aisles #####################################################
# # Concatenate data file subdir and name.
# aisles_file <- paste("./Dataset/aisles.csv",sep="")
# # Load full data
# aisles = read.csv(aisles_file)
# 
# 
# ###### Departments ################################################
# # Concatenate data file subdir and name.
# departments_file <- paste("./Dataset/departments.csv",sep="")
# # Load full data
# departments = read.csv(departments_file)
# 
# 
# ###### Order Products Train #######################################
# # Concatenate data file subdir and name.
# order_products__train_file <- paste("./Dataset/order_products__train.csv",sep="")
# # Load full data
# order_products__train = read.csv(order_products__train_file, header = TRUE, sep=",", nrows = 400000)
# 
# 
# ###### Orders #####################################################
# # Concatenate data file subdir and name.
# orders_file <- paste("./Dataset/orders.csv",sep="")
# # Load full data
# orders = read.csv(orders_file)#, header = TRUE, sep=",", nrows = 500000)
# 
# 
# ###### Products ###################################################
# # Concatenate data file subdir and name.
# products_file <- paste("./Dataset/products.csv",sep="")
# # Load full data
# products = read.csv(products_file)
# products$product_name <- gsub(',', '', products$product_name)
# 
# names(aisles)
# names(departments)
# names(order_products__train)
# names(orders)
# names(products)
# # 
# mydata <- merge(order_products__train,orders,by="order_id")
# mydata <- merge(mydata,products,by="product_id")
# 
# # Create Transaction Table
# retail <- mydata[complete.cases(mydata),]
# glimpse(retail)
# str(retail)
# retail$Time <- as.factor(retail$order_hour_of_day)
# a <- hms(as.character(retail$order_hour_of_day))
# retail$Time = hour(a)
# retail %>% 
#   ggplot(aes(x=Time)) + 
#   geom_histogram(stat="count",fill="indianred")
# 
# 
# detach("package:plyr", unload=TRUE)
# 
# tmp <- retail %>% 
#   group_by(product_id, product_name) %>% 
#   summarize(count = n()) %>% 
#   arrange(desc(count))
# tmp <- head(tmp, n=10)
# tmp
# tmp %>% 
#   ggplot(aes(x=reorder(product_name,count), y=count))+
#   geom_bar(stat="identity",fill="indian red")+
#   coord_flip()
# 
# retail_sorted <- retail[order(retail$user_id),]
# library(plyr)
# itemList <- ddply(retail,c("user_id","order_id"), 
#                   function(df1)paste(df1$product_name, 
#                                      collapse = ","))
# 
# 
# # Remove user and order id's from the list
# itemList$user_id <- NULL
# itemList$order_id <- NULL
# colnames(itemList) <- c("Items")
# write.csv(itemList,"InstaCart_MBA.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('InstaCart_MBA.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')
# Training Apriori on the dataset
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules)#[1:10])

topRules <- rules[1:10]
# Visualising the results
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")
plot(topRules, method = "two-key plot")
plot(topRules, method = "scatterplot")
plot(topRules, method = "paracoord")
