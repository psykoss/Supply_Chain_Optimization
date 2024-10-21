install.packages(c("moments", "tidyverse", "ggplot2", "rnaturalearth", "rnaturalearthdata", "caTools", "leaflet", "ggthemes", "plotly", "DataExplorer", "naniar", "ggpubr"))
library(moments)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(caTools)
library(leaflet)
library(ggthemes)
library(plotly)
library(DataExplorer)
library(naniar)
library(ggpubr)
library(dplyr)

set.seed(1111)

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
              Longitude,
              Category.Name,
              Customer.City,
              Customer.Country,
              Customer.Segment,
              Customer.State,
              Customer.Id,
              Department.Name,
              Market,
              Order.City,
              Order.Customer.Id,
              Order.Id,
              Order.Item.Id,
              Order.Region,
              Order.State,
              Order.Country,
              Product.Name,
              shipping.date..DateOrders.,
              order.date..DateOrders.,
              Product.Card.Id,
              Product.Category.Id,
              Product.Status,
              Department.Id)) 
  
  # Factorize  variables
  data$Type <- as.factor(data$Type)
  data$Delivery.Status <- as.factor(data$Delivery.Status)
  data$Late_delivery_risk <- as.factor(data$Late_delivery_risk)
  data$Order.Status <- as.factor(data$Order.Status)
  data$Shipping.Mode <- as.factor(data$Shipping.Mode)
  
  return(data)
}
# Apply preprocessing pipeline 
data.clean <- preprocessing_pipeline(data)
# ============NA imputation===========================================================================================================================#
# Plot missing values
plot_missing(data)
plot_missing(data)
# ===============Outlier detection====================================================================================================================#
# Plots distributions
calc_stats <- function(column) {
  c(
    min = min(column, na.rm = TRUE),
    Q1 = quantile(column, 0.25, na.rm = TRUE),
    median = median(column, na.rm = TRUE),
    mean = mean(column, na.rm = TRUE),
    Q3 = quantile(column, 0.75, na.rm = TRUE),
    max = max(column, na.rm = TRUE)
  )
}

# Define stats for each variable
sales_stats <- calc_stats(data.clean$Sales)
benefit_stats <- calc_stats(data.clean$Benefit.per.order)
discount_stats <- calc_stats(data.clean$Order.Item.Discount)
price_stats <- calc_stats(data.clean$Order.Item.Product.Price)
total_stats <- calc_stats(data.clean$Order.Item.Total)
profit_stats <- calc_stats(data.clean$Order.Profit.Per.Order)
product_price_stats <- calc_stats(data.clean$Product.Price)
sales_per_customer_stats <- calc_stats(data.clean$Sales.per.customer)
# Create a custom color palette
color_palette <- c("min" = "red", "Q1" = "orange", "median" = "green", "mean" = "blue", "Q3" = "purple", "max" = "black")

calc_stats <- function(column) {
  c(
    min = min(column, na.rm = TRUE),
    Q1 = quantile(column, 0.25, na.rm = TRUE),
    median = median(column, na.rm = TRUE),
    mean = mean(column, na.rm = TRUE),
    Q3 = quantile(column, 0.75, na.rm = TRUE),
    max = max(column, na.rm = TRUE)
  )
}

# Define stats for each variable
sales_stats <- calc_stats(data.clean$Sales)
benefit_stats <- calc_stats(data.clean$Benefit.per.order)
discount_stats <- calc_stats(data.clean$Order.Item.Discount)
price_stats <- calc_stats(data.clean$Order.Item.Product.Price)
total_stats <- calc_stats(data.clean$Order.Item.Total)
profit_stats <- calc_stats(data.clean$Order.Profit.Per.Order)
product_price_stats <- calc_stats(data.clean$Product.Price)
sales_per_customer_stats <- calc_stats(data.clean$Sales.per.customer)

# Create a custom color palette
color_palette <- c("min" = "red", "Q1" = "orange", "median" = "green", "mean" = "blue", "Q3" = "purple", "max" = "black")
create_plot <- function(data, x_var, title) {
  stats <- calc_stats(data[[x_var]])
  
  ggplot(data, aes_string(x = x_var)) + 
    geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
    geom_density(alpha = .4, fill = "#000000", bw = 100) +
    geom_vline(xintercept = stats[1], linetype = "dashed", color = color_palette["min"], size = 1, show.legend = TRUE) + 
    geom_vline(xintercept = stats[2], linetype = "dashed", color = color_palette["Q1"], size = 1, show.legend = TRUE) + 
    geom_vline(xintercept = stats[3], linetype = "dashed", color = color_palette["median"], size = 1, show.legend = TRUE) + 
    geom_vline(xintercept = stats[4], linetype = "dashed", color = color_palette["mean"], size = 1, show.legend = TRUE) + 
    geom_vline(xintercept = stats[5], linetype = "dashed", color = color_palette["Q3"], size = 1, show.legend = TRUE) + 
    geom_vline(xintercept = stats[6], linetype = "dashed", color = color_palette["max"], size = 1, show.legend = TRUE) +
    theme_clean() + 
    ggtitle(title) +
    xlab(title) + 
    ylab("Density")
}
# Create plots for each variable
plt1 <- create_plot(data.clean, "Sales", "Sales")
plt2 <- create_plot(data.clean, "Benefit.per.order", "Benefit Per Order")
plt3 <- create_plot(data.clean, "Order.Item.Discount", "Discount per Order (%)")
plt4 <- create_plot(data.clean, "Order.Item.Product.Price", "Price of item per Order")
plt5 <- create_plot(data.clean, "Order.Item.Total", "Order Item Total")
plt6 <- create_plot(data.clean, "Order.Profit.Per.Order", "Profit per order")
plt7 <- create_plot(data.clean, "Product.Price", "Product Price")
plt8 <- create_plot(data.clean, "Sales.per.customer", "Sales per Customer")

# Create a legend manually
legend_data <- data.frame(
  Statistic = c("min", "Q1", "median", "mean", "Q3", "max"),
  Color = c("red", "orange", "green", "blue", "purple", "black")
)

legend_plot <- ggplot(legend_data, aes(x = Statistic, y = 1, fill = Statistic)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Statistics") +
  guides(fill = guide_legend(title = "Statistics"))

# Arrange all plots including the legend
combined_plot <- ggarrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7, plt8, nrow = 4, ncol = 2, common.legend = FALSE, legend = "right")

# Add the legend plot below the combined plots
final_plot <- ggarrange(combined_plot, legend_plot, ncol = 1, heights = c(7.8, 0.5))

# Display the final plot
print(final_plot)

# Plot discrete features
create_plot_cat <- function(data, x_var, title, xlab) {
  ggplot(data, aes_string(x = x_var, fill = x_var)) + 
    geom_bar(color = "black") +  # Outline bars with black color
    theme_clean() + 
    ggtitle(title) +
    xlab(NULL) +
    scale_fill_discrete(name = x_var)+  # Optional: Legend with x_var name
    theme(
      legend.position = "none",  # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1)  # Optional: Tilt X-axis text
    )  
}

plt.cat1 <- create_plot_cat(data.clean, "Type", "Distribution of payment types", "Types")
plt.cat2 <- create_plot_cat(data.clean, "Order.Status", "Status of Order", "Status")
plt.cat3 <- create_plot_cat(data.clean, "Late_delivery_risk", "Risk of late Delivery", "Risk")
plt.cat4 <- create_plot_cat(data.clean, "Delivery.Status", "Status of Delivery", "Status")
plt.cat5 <- create_plot_cat(data.clean, "Shipping.Mode", "Shipment Types", "Types")


combined_plot_cat <- ggarrange(plt.cat1,plt.cat2,plt.cat3,plt.cat4,plt.cat5, nrow = 3, ncol = 2)

# Skewness table

skew.numeric.cols <- c("Sales",
                  "Sales.per.customer",
                  "Benefit.per.order",
                  "Product.Price",
                  "Order.Item.Discount",
                  "Order.Item.Product.Price",
                  "Order.Item.Profit.Ratio",
                  "Order.Profit.Per.Order",
                  "Order.Item.Total")

sk1 <- skewness(data.clean$Sales)
sk2 <- skewness(data.clean$Sales.per.customer)
sk3 <- skewness(data.clean$Benefit.per.order)
sk4 <- skewness(data.clean$Product.Price)
sk5 <- skewness(data.clean$Order.Item.Discount)
sk6 <- skewness(data.clean$Order.Item.Product.Price)
sk7 <- skewness(data.clean$Order.Item.Profit.Ratio)
sk8 <- skewness(data.clean$Order.Profit.Per.Order)
sk9 <- skewness(data.clean$Order.Item.Total)
skews <- c(sk1,sk2,sk3,sk4,sk5,sk6,sk7,sk8,sk9)

skewness.df <- data.frame(numeric_columns = skew.numeric.cols, skewness = skews)
write.csv(skewness.df, "skewness.csv")

# Boxplots
bxplt.1 <- ggplot(data.clean, aes(x = Sales)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Sales",x = "Number of Sales") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
bxplt.2 <- ggplot(data.clean, aes(x = Sales.per.customer)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Sales per customer",x = "Number of Sales per customer") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )

bxplt.3 <- ggplot(data.clean, aes(x = Product.Price)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Product Price", x = "Price ($)") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )

bxplt.4 <- ggplot(data.clean, aes(x = Benefit.per.order)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Benefit per order", x = "Benefit ($)") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )

bxplt.5 <- ggplot(data.clean, aes(x = Order.Item.Discount)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Order Item Discount", x = "Discount ($)") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
bxplt.6 <- ggplot(data.clean, aes(x = Order.Item.Profit.Ratio)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Profit Ratio",  x = "Profit Ratio (%)") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
bxplt.7 <- ggplot(data.clean, aes(x = Order.Item.Total)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Total Amount per Order",x = "Number of Orders") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
bxplt.8 <- ggplot(data.clean, aes(x = Order.Item.Product.Price)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "BoxPlot of Product Price (Without discount)", x = "Price ($)") + 
  theme_clean() +
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )


ggarrange(bxplt.1,bxplt.2,bxplt.3,bxplt.4,bxplt.5,bxplt.6,bxplt.7,bxplt.8,
          nrow = 4, ncol = 2)

# Number of outliers using IQR

outliers_count_iqr <- data.clean %>%
  select(numeric.cols) %>%  
  summarise(across(everything(), ~ {
    Q1 <- quantile(.x, 0.25)
    Q3 <- quantile(.x, 0.75)
    IQR <- Q3 - Q1
    sum(.x < (Q1 - 1 * IQR) | .x > (Q3 + 1 * IQR))  
  }, .names = "outliers_{col}")) 
write_csv(outliers_count_iqr, "outliers_iqr.csv")


# 3-Sigma-Rule
outliers_count_sigma <- data.clean %>%
  select(numeric.cols) %>%  
  summarise(across(everything(), ~ {
    mu <- mean(.x)
    sigma <- sd(.x)
    sum(.x < mu - 3*sigma | .x > mu + 3*sigma)
      }, .names = "outliers_{col}")) 
write_csv(outliers_count_sigma, "outliers_sigma.csv")

# Get outliers position 
outliers_pos <- function(data, numeric.cols) {
  outliers_list <- data %>%
    select(all_of(numeric.cols)) %>%
    map(~ {
      mu <- mean(.x)
      sigma <- sd(.x)
      which(.x < mu - 3 * sigma | .x > mu + 3 * sigma)  
    }) %>%
    unlist() %>%
    unique()
  
  return(outliers_list)
}
outliers_position <- outliers_pos(data.clean, numeric.cols)
# Remove outliers
# data.clean <- data.clean[-outliers_position,]


# DBSCAN (extra)

library(dbscan)

dbscan_result <- dbscan(data.num, eps = 10, minPts = 8)

labels <- dbscan_result$cluster

dbscan_result

#=================EDA============================================================================================================================#

#________________REPORT___________________________#
#create_report(data.clean)

#______________ORDERS_PER_COUNTRY_________________#

# Group data per Country and compute the frequency of orders
order_summary_country <- data %>%
  group_by(Order.Country) %>%
  summarise(TotalOrders = n())
order_summary_country <- as.data.frame(order_summary_country)

# Select the top 20 countries with most orders
top_countries <- order_summary_country %>% 
  arrange(desc(TotalOrders)) %>% 
  head(20)

ggplot(top_countries, aes(x = reorder(Order.Country, TotalOrders), y = TotalOrders)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + 
  theme_economist_white() +  
  labs(x = "Country", y = "Total Orders", title = "Top 20 Countries by Total Orders") + 
  coord_flip()

#_____________ORDERS_PER_MARKET___________________#

# Add approximate coordinates for each market
orders_market <- data %>%
  group_by(Market) %>%
  summarise(TotalOrders = n()) %>%
  mutate(
    lat = case_when(
      Market == "Africa" ~ 1.9577,        
      Market == "Europe" ~ 54.5260,       
      Market == "LATAM" ~ -14.2350,       
      Market == "Pacific Asia" ~ 1.3521,  
      Market == "USCA" ~ 37.0902         
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

world_map <- map_data("world")

# Plot the map
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +  
  geom_point(data = orders_market, aes(x = lon, y = lat, color = Market, size = TotalOrders),
             alpha = 0.7, show.legend = TRUE) +  
  scale_color_manual(values = palette) +  
  scale_size_continuous(range = c(5, 20)) +  
  geom_text(data = orders_market, aes(x = lon, y = lat, label = TotalOrders),
            vjust = -1, size = 6, fontface = "bold") +  
  theme_economist_white() +
  theme(legend.position = "left") +
  labs(title = "Total Orders by Market", color = "Market", size = "Total Orders")


benefit_order_market <- data %>%
  group_by(Market) %>%
  summarise(TotalBenefit = sum(Benefit.per.order)) %>%
  mutate(
    lat = case_when(
      Market == "Africa" ~ 1.9577,        
      Market == "Europe" ~ 54.5260,       
      Market == "LATAM" ~ -14.2350,       
      Market == "Pacific Asia" ~ 1.3521,  
      Market == "USCA" ~ 37.0902         
    ),
    lon = case_when(
      Market == "Africa" ~ 17.3592,
      Market == "Europe" ~ 15.2551,
      Market == "LATAM" ~ -51.9253,
      Market == "Pacific Asia" ~ 103.8198,
      Market == "USCA" ~ -95.7129
    )
  )
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +  
  geom_point(data = benefit_order_market, aes(x = lon, y = lat, color = Market, size = TotalBenefit),
             alpha = 0.7, show.legend = TRUE) +  
  scale_color_manual(values = palette) +  
  scale_size_continuous(range = c(5, 20)) +  
  geom_text(data = benefit_order_market, aes(x = lon, y = lat, label = scales::scientific(TotalBenefit)),
            vjust = -1, size = 6, fontface = "bold") +  
  theme_economist_white() +
  theme(legend.position = "left") +
  labs(title = "Total Benefit Orders by Market", color = "Market", size = "Total Benefit ($)")

#____________________ORDER_EVOLUTION______________#


# Group data by order and compute the frequency of orders
order_data <- data %>%
  mutate(Date.Order = as.Date(as.POSIXct(order.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalOrders = n())        

orders_time <- ggplot(order_data, aes(x = Date.Order, y = TotalOrders)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 1, alpha = 0.7) +
  geom_smooth(color = "black", size = 1) +
  theme_clean() +                                
  labs(x = "Date Order", y = "Total Orders", title = "Evolution of Orders per month") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#_____________________BENEFIT_OVER_TIME______________________________#
benefit_data <- data %>%
  mutate(Date.Order = as.Date(as.POSIXct(order.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalBenefit = sum(Benefit.per.order))

benefit_time <- ggplot(benefit_data, aes(x = Date.Order, y = TotalBenefit)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 1, alpha = 0.7) +
  geom_smooth(color = "black", size = 1) +
  theme_clean() +                                
  labs(x = "Date Order", y = "Total Benefit", title = "Evolution of Benefit per month") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#_____________________sALES_OVER_TIME______________________________#
sales_time_data <- data %>%
  mutate(Date.Order = as.Date(as.POSIXct(order.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalSales = sum(Sales))

sales_time <- ggplot(sales_time_data, aes(x = Date.Order, y = TotalSales)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 1, alpha = 0.7) +
  geom_smooth(color = "black", size = 1) +
  theme_clean() +                                
  labs(x = "Date Order", y = "Total Sales", title = "Evolution of Sales per month") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#_____________________PRICE_OVER_TIME______________________________#
price_time_data <- data %>%
  mutate(Date.Order = as.Date(as.POSIXct(order.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalPrice = sum(Product.Price))

price_time <- ggplot(price_time_data, aes(x = Date.Order, y = TotalPrice)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 1, alpha = 0.7) +
  geom_smooth(color = "black", size = 1) +
  theme_clean() +                                
  labs(x = "Date Order", y = "Total price", title = "Evolution of price per month") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
price_time
#_____________________QUANTITY_OVER_TIME______________________________#
quantity_time_data <- data %>%
  mutate(Date.Order = as.Date(as.POSIXct(order.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Date.Order) %>%                       
  summarise(TotalQuantity = sum(Order.Item.Quantity))

quantity_time <- ggplot(quantity_time_data, aes(x = Date.Order, y = TotalQuantity)) +
  geom_line(color = "blue", size = 1) +           
  geom_point(color = "red", size = 1, alpha = 0.7) +
  geom_smooth(color = "black", size = 1) +
  theme_clean() +                                
  labs(x = "Date Order", y = "Total Quantity ordered", title = "Evolution of quantity ordered per month") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
quantity_time


#_____________________DAYS_TO_SHIP_OVER_TIME______________________________#
shipping_days_time_data <- data %>%
  mutate(Shipping.Order = as.Date(as.POSIXct(shipping.date..DateOrders., format = "%m/%d/%Y %H:%M"))) %>%  
  group_by(Shipping.Order) %>%                       
  summarise(DaysReal = sum(Days.for.shipping..real.),
            DaysScheduled = sum(Days.for.shipment..scheduled.)
            )

# Plotting both time series with points and smooth line
shippings_time <- ggplot(shipping_days_time_data, aes(x = Shipping.Order)) +
  geom_line(aes(y = DaysReal, color = "DaysReal"), size = 1) +         # Line for DaysReal
  geom_line(aes(y = DaysScheduled, color = "DaysScheduled"), size = 1) + # Line for DaysScheduled
  geom_point(aes(y = DaysReal), color = "black", size = 0.5, alpha = 0.7) +  # Points for DaysReal
  geom_point(aes(y = DaysScheduled), color = "black", size = 0.5, alpha = 0.7) + # Points for DaysScheduled
  geom_smooth(aes(y = DaysReal), color = "black", size = 1, se = FALSE) +  # Smoothed line for DaysReal
  theme_clean() +                                
  labs(x = "Date Shipment", y = "Total Days Expected VS Total Days Real", title = "Evolution of Total Days Expected VS Total Days Real Shipment") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



timeseries_plot <- ggarrange(orders_time,
                             quantity_time,
                             price_time,
                             sales_time,
                             benefit_time,
                             shippings_time, nrow = 3, ncol= 2)

#________________CORRELATION_ANALYSIS_____________#
plot_correlation(data.clean, maxcat = 9L)


# Objective: Correct skewed distributions
# 1. Benefit per order
# 2. Order Item Discount
# 3. Order Item Product Price
# 4. Order item Profit Ratio
# 5. Order item Total
# 6. Order profit per order
# 7. Product price 
# 8. Sales
# 9. Sales per costumer

# Solutions:
# 1. Remove outliers
# 3. Transformations (log, exp, ...)

# Box Cox
#bc <- boxcox(data.clean$Sales ~ 1)  
#lambda <- bc$x[which.max(bc$y)]     
#data.clean$Sales_boxcox <- (data.clean$Sales^lambda - 1) / lambda

# Yeo_Johnson
#library(bestNormalize)
#yj <- yeojohnson(data.num$Order.Item.Total)
#data.num$Order.Item.Total <- yj$x.t  


# QQ-Plot

#=======================UNSUPERVISED_LEARNING=========================#
library(factoextra)
numeric.cols <- c("Sales",
                       "Sales.per.customer",
                       "Benefit.per.order",
                       "Product.Price",
                       "Order.Item.Discount",
                       "Order.Item.Product.Price",
                       "Order.Item.Total",
                       "Order.Profit.Per.Order")
data.num <- data.clean %>%
  select(all_of(numeric.cols))

pca <- prcomp(data.num, scale = T)
fviz_eig(pca, addlabels=TRUE, hjust = -0.3,
         linecolor ="red") + theme_minimal()


biplot <- fviz_pca_biplot(pca, 
                          label = "var",       
                          repel = TRUE,        
                          geom.ind = "point",  
                          alpha.ind = 0.2,     
                          pointsize = 0.9)

fviz_contrib(pca, choice = "ind", axes = 1, top=100)



data.frame(z1=-pca$x[,1],z2=pca$x[,2]) %>% 
  ggplot(aes(z1,z2,label=data$Order.Country,color=data.clean$Product.Price)) + geom_point(size=0.7, alpha =.6) +
  labs(title="PCA", x="PC1", y="PC2") +
  theme_bw() + scale_color_gradient(low="lightblue", high="blue")+theme(legend.position="bottom") + geom_text(size=4, hjust=0.6, vjust=0, check_overlap = TRUE, color = "black") 


#__________________________FA___________________________________________#

# Problem of singular matrix is solved through removing the variables that have high correlations (Avoid multicollinearity)

fa <- factanal(data.num, factors = 3, rotation = "varimax", scores = "Bartlett", lower = 0.01)

#______________________CLUSTERING________________________________________#
library(cluster)
library(mclust)

#___________KMEANS_____________________________________#
k = 5
data.scaled <- scale(data.num)
fit <- kmeans(data.scaled, centers = k, nstart = 1000)

clus_plot <- fviz_cluster(fit, data = data.scaled, geom = "point") +
  theme_clean()

wss <- function(k) {
  kmeans(data.scaled, centers = k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wss_values <- sapply(k_values, wss)
elbow_df <- data.frame(k = k_values, wss = wss_values)

elbow_plot <- ggplot(elbow_df,aes(x = k, y = wss_values)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red", size = 1) +  
  labs(x = "Number of Clusters", y = "Total Within Cluster Sum of Squares", title = "Elbow Plot Kmeans") +
  scale_x_continuous(breaks=seq(0, 10, 1)) + 
  theme_clean()


