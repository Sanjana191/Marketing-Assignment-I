# Task1: Read data
df <- read.csv("retailer.csv")
# Descriptive analysis
head(df)
str(df)
summary(df)

# loading dplyr
library(dplyr)

#Task2: Normalise the data (excluding respondent_id) 
store_attributes <- df %>% select(-respondent_id)
normalized <- scale(store_attributes)
# variable with smallest minimum
min_values <- apply(normalized, 2, min)
min_variable <- names(min_values)[which.min(min_values)]
min_value <- min(min_values)
# variable with largest maximum
max_values <- apply(normalized, 2, max)
max_variable <- names(max_values)[which.max(max_values)]
max_value <- max(max_values)
# 
cat("Smallest minimum is for:", min_variable, "with value:", min_value, "\n")
cat("Largest maximum is for:", max_variable, "with value:", max_value, "\n")

#Task3: Create store attributes data and normalize all the variables
store_data <- df %>% select(variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy)
store_data_scaled <- scale(store_data)

#Task4: Calculating the distance
distance <- dist(store_data_scaled, method = "euclidean")
print(distance) #to check the results 

#Task5: Set seed for reproducibility
set.seed(123)

#Task6: Run hierarchical clustering
hc <- hclust(distance, method = "ward.D2")

#Task7: Plot the dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram - Ward.D2 Method")

#Task8: Cut dendrogram into 3 clusters
clusters3 <- cutree(hc, k = 3)

#Task9: Count observations in 3 clusters
table(clusters3)

#Task10: K-means clustering (3 clusters)
kmeans3 <- kmeans(store_data_scaled, centers = 3, nstart = 100, iter.max = 1000)
table(kmeans3$cluster)

#Task11: Cut dendrogram into 4 clusters
clusters4 <- cutree(hc, k = 4)
table(clusters4)

#Task12: K-means clustering (4 clusters)
kmeans4 <- kmeans(store_data_scaled, centers = 4, nstart = 100, iter.max = 1000)
table(kmeans4$cluster)

#Task13: comparing the cluster solutions
nb <- NbClust(store_data_scaled, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2")

#Task14: Segment profiling using flexclust
kcc <- as.kcca(kmeans3, store_data_scaled)
segment_means <- parameters(kcc)
print(segment_means)

#Task15: Segment profile plot
  barchart(kcc, data = store_data_scaled, main = "Segment Profile Plot")
  # Add cluster labels to original data
  df$cluster <- kmeans3$cluster
  # Demographic profiling by cluster
  aggregate(cbind(income, age) ~ cluster, data = df, FUN = mean)

    