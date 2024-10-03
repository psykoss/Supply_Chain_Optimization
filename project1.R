install.packages("tidyverse",
                 "caTools",
                 "leaflet",
                 "plotly",
                 "ggthemes",
                 "naniar")

library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(caTools)
library(leaflet)
library(ggthemes)
library(plotly)
library(naniar)

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
              Product.Image,
              Latitude,
              Longitude)) 
  
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

  return(data)
}
data.clean <- preprocessing_pipeline(data)
# ============NA imputation====================#
# Plot missing values
na.data.plt <- gg_miss_var(data.clean)
na.data.plt
# Omit NA values since imputation of Qualitative variables is not possible (You cannot create synthetic orders and also the sample size is big n  = 180000)
data.clean <- na.omit(data.clean)
# =============================================#

# Outlier detection

# Boxplots

# 3-Sigma-Rule

# Normalization

# DBSCAN (extra)

# Apply preprocessing pipeline 
data.clean <- preprocessing_pipeline(data)

#=================EDA=============================#

countries_coords <- read.csv("country-coord.csv")

#______________ORDERS_PER_COUNTRY_________________#

# Group data per Country and compute the frequency of orders
order_summary_country <- data.clean %>%
  group_by(Order.Country) %>%
  summarise(TotalOrders = n())
order_summary_country <- as.data.frame(order_summary_country)

# Select the top 20 countries with most orders
top_countries <- order_summary_country %>% 
  arrange(desc(TotalOrders)) %>% 
  head(20)

plt <- ggplot(top_countries, aes(x = reorder(Order.Country, TotalOrders), y = TotalOrders)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + 
  theme_wsj() +  
  labs(x = "Country", y = "Total Orders", title = "Top 20 Countries by Total Orders") + 
  coord_flip()

plt

#_____________ORDERS_PER_MARKET___________________#
library(leaflet)
# Add approximate coordinates for each market
orders_market <- data.clean %>%
  group_by(Market) %>%
  summarise(TotalOrders = n()) %>%
  mutate(
    lat = case_when(
      Market == "Africa" ~ 1.9577,        # Central Africa
      Market == "Europe" ~ 54.5260,       # Central Europe
      Market == "LATAM" ~ -14.2350,       # Central Latin America
      Market == "Pacific Asia" ~ 1.3521,  # Southeast Asia
      Market == "USCA" ~ 37.0902          # Central US
    ),
    lon = case_when(
      Market == "Africa" ~ 17.3592,
      Market == "Europe" ~ 15.2551,
      Market == "LATAM" ~ -51.9253,
      Market == "Pacific Asia" ~ 103.8198,
      Market == "USCA" ~ -95.7129
    )
  )

# Create a color palette for each market
palette <- c("Africa" = "red", "Europe" = "green", "LATAM" = "blue", 
             "Pacific Asia" = "purple", "USCA" = "orange")

library(rnaturalearth)
world_map <- map_data("world")

# Plot the map
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray", color = "black") +  # Base world map
  geom_point(data = orders_market, aes(x = lon, y = lat, color = Market, size = TotalOrders),
             alpha = 0.7, show.legend = FALSE) +  # Plot points for each region
  scale_color_manual(values = palette) +  # Apply custom colors
  scale_size_continuous(range = c(5, 20)) +  # Adjust point sizes based on TotalOrders
  geom_text(data = orders_market, aes(x = lon, y = lat, label = TotalOrders),
            vjust = -1, size = 6, fontface = "bold") +  # Add labels for total orders
  theme_wsj() +
  theme(legend.position = "left") +
  labs(title = "Total Orders by Market", color = "Market", size = "Total Orders")
#____________________ORDER_EVOLUTION______________#


# Group data by order and compute the frequency of orders
order_data <- data.clean %>%
  mutate(Date.Order = as.Date(Date.Order)) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalOrders = n())        

plt2 <- ggplot(order_data, aes(x = Date.Order, y = TotalOrders)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 2) +           
  theme_wsj() +                                
  labs(x = "Date", y = "Total Orders", title = "Evolution of Orders Over Time") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
plt2
