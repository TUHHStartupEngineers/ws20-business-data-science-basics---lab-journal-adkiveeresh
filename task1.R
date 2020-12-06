library(readxl)
library(dplyr)
library(stringr)
library(rlang)
library(tidyr)
library(lubridate)
library(ggplot2)
library(writexl)
library(readr)

# 2.0 Importing Files ----
# A good convention is to use the file name and suffix it with tbl for the data structure tibble

bikes_tbl <- read_excel("C:/Users/veere/OneDrive/Desktop/Third/Bussines Data Science/DS_101/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Users/veere/OneDrive/Desktop/Third/Bussines Data Science/DS_101/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("C:/Users/veere/OneDrive/Desktop/Third/Bussines Data Science/DS_101/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# If the data has no common column name, you can provide each column name in the "by" argument. For example, by = c("a" = "b") will match x.a to y.b. The order of the columns has to match the order of the tibbles).

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

# Chaining commands with the pipe and assigning it to 

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()
bike_orderlines_joined_tbl %>% glimpse()

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# 3.0 Wrangling Data ----
# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 3.1 Separate category name
  separate (col= category,into   = c("category.1", "category.2", "category.3"),sep= " - ")%>% 
  # 3.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  # 3.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 3.3.1 by exact column name
  select(-...1, -gender) %>%
  # 3.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  # 3.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  # 3.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  # 3.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 4.0 Business Insights ----
# 4.1 Sales by location ----

# Step 1 - Manipulate
sales_by_state_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  # Select columns
  tidyr::separate(col = location,
           into = c("city","state"),
           sep = ",") %>% 
  # Select columns and add a year
  select(state, total_price) %>%
  
  # Group by and summarize year and main catgegory
  group_by(state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() 

sales_by_state_cat_1_tbl

# Step to Visualize {r plot1, fig.width=10, fig.height=7, echo=FALSE}
sales_by_state_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )








# Challenge task 2

sales_by_year_state_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  tidyr::separate(col = location,
                  into = c("city","state"),
                  sep = ",") %>% 
  
  # Select columns and add a year
  select(state,order_date, total_price) %>%
  mutate(year = year(order_date)) %>% 
  
  # Group by and summarize year and main catgegory
  group_by(year,state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() 

# Step 2 - Visualize
sales_by_year_state_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state )) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  facet_wrap(~ state)+
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

bike_orderlines_wrangled_tbl %>% 
  write_rds("C:/Users/veere/OneDrive/Desktop/Third/Bussines Data Science/DS_101/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
