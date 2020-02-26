# Load the libraries
library(tidyverse)
library(knitr)
library(ggplot2)
library(arules)
library(arulesViz)
library(plyr)

# Read in all required .csv Files
# 1 ##### Aisles ##################################################
# Concatenate data file subdir and name.
aisles_file <- paste("./Dataset/aisles.csv",sep="")
# Load full data
aisles = read.csv(aisles_file)
# 2 ##### Departments #############################################
# Concatenate data file subdir and name.
departments_file <- paste("./Dataset/departments.csv",sep="")
# Load full data
departments = read.csv(departments_file)
# 3 ##### Order Products Train ####################################
# Concatenate data file subdir and name.
order_products__train_file <- paste("./Dataset/order_products__train.csv",sep="")
# Load full data
order_products__train = read.csv(order_products__train_file)#, header = TRUE, sep=",", nrows = 400000)
# 4 ##### Orders ##################################################
# Concatenate data file subdir and name.
orders_train_file <- paste("./Dataset/orders_train.csv",sep="")
# Load full data
orders_train = read.csv(orders_train_file)#, header = TRUE, sep=",", nrows = 500000)
# 5 ##### Products ################################################
# Concatenate data file subdir and name.
products_file <- paste("./Dataset/products.csv",sep="")
# Load full data
products = read.csv(products_file)
# Remove commas from certain product names
products$product_name <- gsub(',', '', products$product_name)

# Merge the order_products__train and orders_train dataframes
mydata <- merge(order_products__train,orders_train,by="order_id")
mydata <- merge(mydata,products,by="product_id")

# Create Transaction Table
str(mydata)
mydata$Time <- as.factor(mydata$order_hour_of_day)
mydata %>%
  ggplot(aes(x=Time)) +
  geom_histogram(stat="count",fill="indianred")

# EDA 
detach("package:plyr", unload=TRUE)
tmp <- mydata %>%
  group_by(product_id, product_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>%
  ggplot(aes(x=reorder(product_name,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

retail_sorted <- mydata[order(mydata$user_id),]
library(plyr)
itemList <- ddply(mydata,c("user_id","order_id"),
                  function(df1)paste(df1$product_name,
                                     collapse = ","))
mydata$order_id <- as.factor(mydata$order_id)
Subcategories <- subset(mydata$product_name, mydata$product_name != 'Other')
Transactions <- subset(mydata$order_id, mydata$order_id != 'Other')
Tr <- split(Subcategories, Transactions)
class(Tr) # Tr should be "list"
tr <- as(Tr, "transactions")
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

# Modeling

###################################################################
# Training Apriori on the dataset
apriori_rules <- apriori(tr, parameter = list(supp=0.001, conf=0.4))
apriori_rules <- sort(apriori_rules, by='count', decreasing = TRUE)
summary(apriori_rules)

inspect(apriori_rules[1:10])
apriori_topRules <- apriori_rules[1:10]

# Visualising the results of the Apriori Analysis
plot(apriori_topRules, method="graph")
plot(apriori_topRules, method = "grouped")
plot(apriori_topRules, method = "two-key plot")
plot(apriori_topRules, method = "scatterplot")
plot(apriori_topRules, method = "paracoord")

###################################################################
# Training Eclat on the dataset
eclat_rules = eclat(tr, parameter = list(support = 0.001, minlen = 2))
eclat_rules <- sort(eclat_rules, by='count', decreasing = TRUE)
summary(eclat_rules)

inspect(sort(eclat_rules, by = 'count')[1:10])
eclat_topRules <- eclat_rules[1:10]

# Visualising the results of the Eclat Analysis
plot(eclat_topRules, method="graph")
plot(eclat_topRules, method = "scatterplot")
plot(eclat_topRules, method = "paracoord")
