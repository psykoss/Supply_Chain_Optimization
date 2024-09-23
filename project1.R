library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(caTools)
library(leaflet)
library(ggthemes)
library(plotly)

# Load the data
data <- read.csv("dataco/DataCoSupplyChainDataset.csv")

# Take a look at the structure and dimension
head(data)
dim(data)
str(data)
colnames(copy)
any(is.na(data))
which(is.na(data))
sum(is.na(data))
colnames(data)[colSums(is.na(data)) > 0]

#_________________PREPROCESSING_PIPELINE_________________#


# Data preprocessing pipeline
preprocessing_pipeline <- function(data) {
  
  #Drop unnecessary columns
  
  data <- data %>%
    select(!c(Customer.Fname, 
              Customer.Lname, 
              Customer.Password, 
              Category.Id, 
              Customer.Email, 
              Customer.Password, 
              Customer.Street, 
              Customer.Zipcode, 
              Order.Item.Cardprod.Id, 
              Order.Zipcode, 
              Product.Description, 
              Product.Image)) 
  
  #Transform character Dates to posixCt format
  data$order.date..DateOrders. <- as.POSIXct(data$order.date..DateOrders., format = "%m/%d/%Y %H:%M")    
  data$shipping.date..DateOrders. <- as.POSIXct(data$shipping.date..DateOrders., format = "%m/%d/%Y %H:%M")    
  
  # Factorize  variables
  data$Type <- as.factor(data$Type)
  data$Delivery.Status <- as.factor(data$Delivery.Status)
  data$Late_delivery_risk <- as.factor(data$Late_delivery_risk)
  data$Category.Name <- as.factor(data$Category.Name)
  data$Customer.Segment <- as.factor(data$Customer.Segment)
  data$Order.Status <- as.factor(data$Order.Status)
  data$Shipping.Mode <- as.factor(data$Shipping.Mode)
  
  # Renaming some columns
  names(data)[names(data) == 'Days.for.shipment..scheduled.'] <- 'Date.Shipping.Scheduled'
  names(data)[names(data) == 'Type'] <- 'Type.Payment'
  names(data)[names(data) == 'order.date..DateOrders.'] <- 'Date.Order'
  names(data)[names(data) == 'shipping.date..DateOrders.'] <- 'Date.Shipping.Order'
   
  #Imputing NA values: 
  
  
  
  
  return(data)
}

# Outlier detection

# Boxplots

# 3-Sigma-Rule

# Normalization

# DBSCAN (extra)

# Apply preprocessing pipeline 
data.clean <- preprocessing_pipeline(data)

#_________________EXPLORATORY_DATA_ANALYSIS_________________#

# Make a plot of the total orders per country
order_summary_country <- data.clean %>%
  group_by(Order.Country) %>%
  summarise(TotalOrders = n())

order_summary_country <- as.data.frame(order_summary_country)

plt <- ggplot(order_summary_country, aes(x = reorder(Order.Country, -TotalOrders), y = TotalOrders)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 1) + 
  theme_clean() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(x = "Country", y = "Total Orders", title = "Total Orders per Country")  

plt

# Evolution of orders over months 2014-2018
order_data <- data.clean %>%
  mutate(Date.Order = as.Date(Date.Order)) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalOrders = n())        

plt2 <- ggplot(order_data, aes(x = Date.Order, y = TotalOrders)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 2) +           
  theme_clean() +                                
  labs(x = "Date", y = "Total Orders", title = "Evolution of Orders Over Time") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
plt2

