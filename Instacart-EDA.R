library(knitr)      
library(tidyverse)  
library(data.table) 
library(caret)      
library(ROCR)       
library(gridExtra)  

aisles <- fread('aisles.csv', stringsAsFactors = TRUE)
departments <- fread('departments.csv', stringsAsFactors = TRUE)
products <- fread('products.csv', stringsAsFactors = TRUE)
orders <- fread('orders.csv', stringsAsFactors = TRUE)
order_products_train <- fread('order_products__train.csv')
order_products_prior <- fread('order_products__prior.csv')

### PRODUCT ANALYSIS ###

# See which product is most commonly ordered

tmp = order_products_train %>%
  left_join(products) %>%
  group_by(product_name) %>%
  summarize(count=n()) %>%
  top_n(n=50, wt=count) %>%  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(product_name,count), y=percentage)) +  
  geom_col() + ggtitle('Top 50 Products') + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank()) 

### DEPARTMENT ANALYSIS ###

# See which department gets the most orders

tmp = order_products_train %>%
  left_join(products) %>%
  left_join(departments) %>%
  group_by(department) %>%
  summarize(count=n()) %>%
  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(department,count), y=percentage)) +  
  geom_col() + ggtitle('Departments') + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank()) 

# To look deeper we can check which department gets the most orders per hour

rbind(order_products_train,order_products_prior) %>%
  left_join(products) %>%
  left_join(departments) %>%
  left_join(orders) %>%
  group_by(order_hour_of_day, department) %>%
  summarize(count = n()) %>%
  ggplot (aes(x=order_hour_of_day, y=count, fill=department)) + 
  geom_col()+ ylab('Orders Quantity') + xlab("Hour of the day") + ggtitle('Department Orders over 24 Hours')

# Same for day of the week

rbind(order_products_train,order_products_prior) %>%
  left_join(products) %>%
  left_join(departments) %>%
  left_join(orders) %>%
  group_by(order_dow, department) %>%
  summarize(count = n()) %>%
  ggplot (aes(x=order_dow, y=count, fill=department)) + 
  geom_col() + ylab('Orders Quantity') + xlab("Day of the week") + ggtitle('Department Orders per day')


### AISLE ANALYSIS ### 

# See which Aisle gets the most orders

tmp = order_products_train %>%
  left_join(products) %>%
  left_join(aisles) %>%
  group_by(aisle) %>%
  summarize(count=n()) %>%
  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(aisle,count), y=percentage)) +  
  geom_col() + ggtitle('Aisles') + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank()) 

# Previous Aisle graph is kind of messy, To make it more clear will just take top 50

tmp = order_products_train %>%
  left_join(products) %>%
  left_join(aisles) %>%
  group_by(aisle) %>%
  summarize(count=n()) %>%
  top_n(n=50, wt=count) %>%  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(aisle,count), y=percentage)) +  
  geom_col() + ggtitle('Top 50 Aisles') + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank()) +  ylab('Percentage of Orders') + xlab('Aisles')


### ORDERS ANALYSIS ###

# Next we want to see what day is the most busy, day 0 is sunday

order_products_prior %>%
  left_join(orders) %>%
  group_by(order_dow) %>%
  summarize(count = n()) %>%
  mutate(percentage=count/sum(count)) %>%
  ggplot (aes(x=as.factor(order_dow), y=percentage)) + 
  geom_col()+ ylab('Percentage of Orders') + xlab("Day of the week") + ggtitle('Daily Orders')

# To get the raw number of orders per day
orders %>%
  ggplot( aes(x=order_dow)) +
  geom_bar() + ylab('Number of Orders') + xlab("Day of the week")

# With order information we can determine the top products for each day

order_products_prior %>% 
  left_join(orders) %>% left_join(products) %>%
  group_by(order_dow, product_name) %>%
  summarize(n=n()) %>%
  mutate(percentage=n/sum(n)) %>%
  top_n(5, wt=n) %>%
  ggplot (aes(x=as.factor(order_dow), y=percentage, fill=product_name)) + 
  geom_col() + ylab('Proprtion of Orders') + xlab("Day of the week") + ggtitle('Daily Top 5 Products Ordered') +
  theme(legend.position="bottom",legend.direction="horizontal")

# Using the time of day provided we can see what the busiest hours are

order_products_prior %>%
  left_join(orders) %>%
  group_by(order_hour_of_day) %>%
  summarize(count = n()) %>%
  mutate(percentage=count/sum(count)) %>%
  ggplot (aes(x=as.factor(order_hour_of_day), y=percentage)) + 
  geom_col()+ ylab('Percentage of Orders') + xlab("Hour of the day") + ggtitle('Hourly Orders')

# To get raw number of orders per hour
orders %>%
  ggplot( aes(x=order_hour_of_day)) +
  geom_bar() + ylab('Number of Orders') + xlab("Hour of the day")

# Same thing as for the days, we can check the top products for each hour

order_products_prior %>% 
  left_join(orders) %>% left_join(products) %>%
  group_by(order_hour_of_day, product_name) %>%
  summarize(n=n()) %>%
  mutate(percentage=n/sum(n)) %>%
  top_n(5, wt=n) %>%
  ggplot (aes(x=as.factor(order_hour_of_day), y=percentage, fill=product_name)) + 
  geom_col() + ylab('Proprtion of Orders In A Hour') + xlab("Hour of the day") + ggtitle('Hourly Top 5 Products Ordered') +
  theme(legend.position="bottom",legend.direction="horizontal")

### Customer data analysis ###

# Looking at the number of orders customers make

orders %>%
  group_by(user_id) %>%
  summarize(n.orders=n()) %>%
  group_by(n.orders) %>%
  summarize (n.users = n()) %>%
  ggplot(aes(x=n.orders, y=n.users))  +
  geom_col() + labs(x='Number of Orders Per Customer', y='Number of Customers')

# How much time passes between customer orders

orders %>%
  group_by(days_since_prior_order) %>%
  summarize ( n.orders = n()) %>% 
  ggplot(aes(x=days_since_prior_order, y=n.orders)) +
  geom_col() + labs(x='Days since last order', y='Number of Customers')

# How many items are in each order, not sure why x axis is up to 150

rbind(order_products_prior, order_products_train) %>%
  group_by(order_id) %>%
  summarise(n_items=n()) %>%
  ggplot(aes(x=n_items)) +
  geom_bar() + labs(x='Items per Order', y='Number of Customers')

# Which department recives the most customer orders

tmp = rbind(order_products_train, order_products_prior) %>%
  left_join(products) %>%
  left_join(departments) %>%
  group_by(department) %>%
  summarize(count=n()) %>%
  mutate(percentage=count/sum(count))
ggplot (tmp, aes(x=reorder(department,-count), y=percentage)) +  
  geom_col() + ggtitle('Departments') + ylab('Percent of orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank()) 

# Same graph but with raw numbers instead of percentage of sales

ggplot (tmp, aes(x=reorder(department,-count), y=count)) +  
  geom_col() + ggtitle('Departments') + ylab('Number of orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank())

# Same thing as above for departments but this time looking at aisles, Because there is way too many we only look at the top few

tmp = rbind(order_products_train, order_products_prior) %>%
  left_join(products) %>%
  left_join(aisles) %>%
  group_by(aisle) %>%
  summarize(count=n()) %>%
  top_n(20, wt=count) %>%
  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(aisle,-count), y=percentage)) +  
  geom_col() + ggtitle('Top 20 Aisles') + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank())

# Aisle orders by raw numbers

ggplot (tmp, aes(x=reorder(aisle,-count), y=count)) +  
  geom_col() + ggtitle('Top 20 Aisles') + ylab('Number of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank())

# We already know what top selling product is from previous graphs but its always nice to double check

tmp = rbind(order_products_train, order_products_prior) %>%
  left_join(products, by='product_id') %>%
  group_by(product_name) %>%
  summarize(count=n()) %>%
  top_n(20, wt=count) %>%
  mutate(percentage=count/sum(count))

ggplot (tmp, aes(x=reorder(product_name,-count), y=percentage)) +  
  geom_col() + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank())

# Just as before another graph with raw numbers
ggplot (tmp, aes(x=reorder(product_name,-count), y=count)) +  
  geom_col() + ylab('Percentage of Orders') +
  theme (
    axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
    axis.title.x = element_blank())

### REORDERING ###

# Instacart has provided us with reordering information
# Using this we can see what percent of orders are reordered by divided by total

tmp = rbind(order_products_train,order_products_prior) %>%
  group_by(reordered) %>%
  summarize(count = n()) %>%
  mutate(probability = count/sum(count))

tmp %>%
  ggplot(aes(x=factor(0), y=probability, fill=factor(0:1))) + 
  geom_col(width=1) + 
  coord_polar(theta='y') + xlab(" ")

