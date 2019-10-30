# This module contains a code example for creating multiple change variables in R using functions
# by Hana Chang

library(dplyr)

# How to sort the data by ID and time (in this case, it is year_and_mth)
data <- data %>% 
  group_by(customer_id) %>%
  arrange(year_and_mth, .by_group = TRUE)

# Create function for change variable with current month and 1 month lag
customer_1m_change <- function(x){
  ifelse(lag(x,1)!=0, (x - lag(x,1))/lag(x,1), 0) 
}

# Create data frame for 1 month change
customer_1m_change <- data %>% 
  group_by(customer_id) %>%
  arrange(year_and_mth, .by_group = TRUE) %>%
  mutate_each(funs = funs(customer_change_1m), v1:v5)

# How to create 2 month rolling sum variable 
customer_rollsum_2m <- function(x){
  rollsum(x, 2, align = "right", fill = 0)
}

# Create data frame for 2 month rolling sum
customer_rollsum_2m <- data %>% 
  group_by(customer_id) %>%
  arrange(year_and_mth, .by_group = TRUE) %>%
  mutate_each(funs = funs(customer_rollsum_2m), v1:v5)
  
# Change variables for 2 month rolling sums
customer_2m_change <- function(x){
  ifelse(lag(x,2)!=0, (x - lag(x,2))/lag(x,2), 0) 
}

customer_2m_change <- customer_rollsum_2m %>% 
  group_by(customer_id) %>%
  arrange(year_and_mth, .by_group = TRUE) %>%
  mutate_each(funs = funs(customer_2m_change), v1:v5)

# Change varialbe names before merging
names(customer_1m_change)[3:8] <- paste("ch1m", names(customer_1m_change)[3:8],sep = "_")
names(customer_2m_change)[3:8] <- paste("ch1m", names(customer_2m_change)[3:8],sep = "_")

# You can modify these codes to create multiple rolling sum and change variables
# Make sure to merge all data together at the end to the original data set
data2 <- left_join(data, customer_1m_change, by=c("customer_id", "year_and_mth"))

