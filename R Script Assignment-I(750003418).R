# Task1: Read the dataset and call it 'retailer' 
retailer <- read.csv("retailer.csv")
glimpse(retailer)
summary(retailer)
#`glimpse` and `summary`functions to inspect the variables in  the dataset


#Task2 & 3: Normalize the dataset
retailer_norm <- scale(retailer) %>% as_tibble()
# normalize the variables by using the `scale` function

#We can now confirm that all variables have a mean of approximately 0. Let's check variety_of_choice and low_prices:
mean(retailer_norm$variety_of_choice)
mean(retailer_norm$low_prices)

#We can also see that each variable has a standard deviation of 1. Let's check quality_of_service and return_policy: 
sd(retailer_norm$quality_of_service)
sd(retailer_norm$return_policy)

# checking the smallest minimum value and the largest maximum value
summary(retailer_norm)
#Based on the summary provided, the variable with the smallest minimum value is variety_of_choice at -1.7723, and the variable with the largest maximum value is age at 2.9858

#Task4: Calculating the distance (using euclidean distance)
distance <- dist(retailer_norm, method = "euclidean")
print(distance) #to check the results 

#Task5: Set seed for reproducibility
set.seed(123)

#Task6: Run hierarchical clustering
hier_clust <- hclust(distance, method = "ward.D2")

#Task7: Plot the dendrogram of the hierarchical clustering process.
plot(hier_clust, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

#Task8: Cut dendrogram into 3 clusters
# Plot the dendogram again
plot(hier_clust, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

# Draw rectangles highlighting the 3 clusters
rect.hclust(hier_clust, k = 3, border = "blue")

# Create a three-cluster solution
hcluster_groups <- cutree(hier_clust, k = 3)

#Task9: Count observations in 3 clusters
table(hcluster_groups)

#Task10: K-means clustering (3 clusters)
set.seed(123) # for reproducibility
kmeans3 <- kmeans(retailer_norm, centers = 3, nstart = 100, iter.max = 1000)
table(kmeans3$cluster)

#Task11: Cut dendrogram into 4 clusters
clusters4 <- cutree(hier_clust, k = 4)
table(clusters4)

#Task12: K-means clustering (4 clusters)
kmeans4 <- kmeans(retailer_norm, centers = 4, nstart = 100, iter.max = 1000)
table(kmeans4$cluster)

# Plot the dendogram again
plot(hier_clust, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

# Draw rectangles highlighting the 4 clusters
rect.hclust(hier_clust, k = 4, border = "red")

#Task13: comparing the cluster solutions
library(NbClust)
nbclust_results <- NbClust(retailer_norm, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index = "all")
nbclust_results$Best.nc

#Task 14: Calculate the means for each store attribute variable per cluster
retailer_norm_with_clusters <- retailer_norm %>%
  as.data.frame() %>% #data frame for dplyr
  mutate(cluster = as.factor(kmeans3$cluster))

# Then, calculate the means for all normalized attributes by cluster
cluster_means_dplyr <- retailer_norm_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) # 'everything()' applies mean to all other columns
# if you have non-attribute columns, you might need to select them
print(cluster_means_dplyr)

#Task 15: Segment profiling using flexclust
retailer <- retailer %>% 
  mutate(hcluster_groups = hcluster_groups)

retailer %>%
  select(-respondent_id, -age, -income) %>% # Exclude non-attribute columns
  group_by(hcluster_groups) %>%             # Group by the new cluster assignments
  summarise_all(~ mean(.x)) %>%             # Calculate the mean for each group
  print(width = Inf)

library(flexclust)
# Note: Converting hclust to kcca for plotting
hier_clust_flex <- as.kcca(hier_clust, retailer_norm, k = 3)

barchart(hier_clust_flex, main = "Segment Profiles") 
