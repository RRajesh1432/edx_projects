
## WRITTEN BY : RAJESH RAGI
## DATE : JULY 2024



# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(gridExtra)

# Read the data
data <- read.csv("C:/Users/rajes/Desktop/Zidio Internship/data.csv")

# Exploratory Data Analysis (EDA)


head(data)
df <- data
str(df)
summary(df)

# Data Cleaning
# Checking  for duplicates
sum(duplicated(df))
# Check for missing values
colSums(is.na(df))
# Impute missing values in 'Gender' with the most frequent value
df$Gender <- ifelse(is.na(df$Gender), names(which.max(table(df$Gender))), df$Gender)
# Verify there are no remaining missing values
sum(is.na(df))

# Data Visualization
# Gender distribution
ggplot(df, aes(x = Gender)) +
  geom_bar() +
  ggtitle("Gender Distribution")

# Orders distribution
p1 <- ggplot(df, aes(x = Orders)) +
  geom_bar() +
  ggtitle("Overall Orders")

# Gender-wise orders distribution
p2 <- ggplot(df, aes(x = Orders, fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender-wise Orders")

# Combine both plots
grid.arrange(p1, p2, ncol = 2, top = "Overall Orders VS Gender-wise Orders")

# Boxplots for each brand's orders and searches
cols <- names(df)[3:ncol(df)]
plot_list <- lapply(cols, function(col) {
  ggplot(df, aes_string(y = col)) +
    geom_boxplot() +
    theme(axis.text.x = element_blank())
})
do.call(grid.arrange, c(plot_list, ncol = 6))

# Correlation heatmap
cor_matrix <- cor(df[, 4:ncol(df)])
heatmap(cor_matrix, main = "Correlation Heatmap")

# Histograms for selected columns
df[, 1:2] %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

# Calculate total searches and identify top 10 customers based on total searches
new_df <- df %>%
  mutate(Total_Search = rowSums(select(., 4:ncol(.))))

top_10 <- new_df %>%
  arrange(desc(Total_Search)) %>%
  head(10)

ggplot(top_10, aes(x = reorder(Cust_ID, -Total_Search), y = Total_Search, fill = Gender)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cust_ID", title = "Top 10 Cust_ID based on Total Searches")

# Scaling the data
x <- as.matrix(df[, 3:ncol(df)])
features <- scale(x)

# Determine the optimal number of clusters using the Elbow method
set.seed(123)
fviz_nbclust(features, kmeans, method = "wss", k.max = 15)

# Silhouette Score for each K value
fviz_nbclust(features, kmeans, method = "silhouette", k.max = 15)

# Apply K-means clustering with K=3 as per Elbow Method
set.seed(123)
model <- kmeans(features, centers = 3)

# Add cluster assignments to the dataframe
df$Cluster <- as.factor(model$cluster)
write.csv(df, "Cluster_data.csv", row.names = FALSE)

# Cluster distribution
ggplot(df, aes(x = Cluster)) +
  geom_bar() +
  ggtitle("Cluster Distribution")

# Read the clustered data and calculate total searches
c_df <- read.csv("Cluster_data.csv")
c_df$Total_Search <- rowSums(c_df[, 4:ncol(c_df)])

# Visualizations for each cluster
for (i in 0:2) {
  cluster_data <- c_df %>% filter(Cluster == i)
  
  p1 <- ggplot(cluster_data, aes(x = Gender)) +
    geom_bar() +
    ggtitle(paste("Customer count in Cluster", i))
  
  p2 <- ggplot(cluster_data, aes(x = Gender, y = Total_Search)) +
    geom_bar(stat = "summary", fun = "sum") +
    ggtitle(paste("Total Search by Gender in Cluster", i))
  
  print(grid.arrange(p1, p2, ncol = 2, 
                     top = paste("No. of customers and their searches in Cluster", i)))
}

# Final visualizations
ggplot(c_df, aes(x = Cluster, fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Total Customers in each Cluster")

final_df <- c_df %>%
  group_by(Cluster) %>%
  summarise(Total_Search = sum(Total_Search),
            Orders = sum(Orders))

p1 <- ggplot(final_df, aes(x = Cluster, y = Total_Search)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Searches by each Cluster")

p2 <- ggplot(final_df, aes(x = Cluster, y = Orders)) +
  geom_bar(stat = "identity") +
  ggtitle("Past Orders by each Cluster")

grid.arrange(p1, p2, ncol = 2, 
             top = "Customer Searches and Past Orders by Cluster")

# Print the final summarized dataframe
print(final_df)

