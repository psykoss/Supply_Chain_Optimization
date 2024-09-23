library(tidyverse)
library(ggplot2)
library(caTools)

# Load the data
data <- read.csv("dataco/DataCoSupplyChainDataset.csv")

# Take a look at the structure and dimension
head(data)
dim(data)
str(data)
colnames(copy)
any(is.na(data))
which(is.na(data))

# Data preprocessing pipeline
preprocessing_pipeline <- function(data) {
  
  #1. Drop unnecessary columns
  
  data <- data %>%
    select(!c(Customer.Fname, 
              Customer.Lname, 
              Customer.Password, 
              Customer.Id, 
              Product.Category.Id, 
              Category.Id, 
              Customer.Email, 
              Customer.Password, 
              Customer.Street, 
              Order.Customer.Id, 
              Department.Id, 
              Customer.Zipcode, 
              Order.Id, 
              Order.Item.Id, 
              Order.Item.Cardprod.Id, 
              Order.Zipcode, 
              Product.Description, 
              Product.Image)) 
  
  #2.Transform character Dates to posixCt format
  data$order.date..DateOrders. <- as.POSIXct(data$order.date..DateOrders., format = "%m/%d/%Y %H:%M")    
  data$shipping.date..DateOrders. <- as.POSIXct(data$shipping.date..DateOrders., format = "%m/%d/%Y %H:%M")    
  
  #3. Factorize  variables
  data$Type <- as.factor(data$Type)
  data$Delivery.Status <- as.factor(data$Delivery.Status)
  data$Late_delivery_risk <- as.factor(data$Late_delivery_risk)
  data$Category.Name <- as.factor(data$Category.Name)
  data$Customer.Segment <- as.factor(data$Customer.Segment)
  data$Order.Status <- as.factor(data$Order.Status)
  data$Shipping.Mode <- as.factor(data$Shipping.Mode)
  
  #4. Imputing NA values: 
  
  
  
  return(data)
}

#5. Outlier Detection : 
#6. Feature Creation :


data.clean <- preprocessing_pipeline(data)

