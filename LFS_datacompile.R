#Section 1: Combine LFS into one .csv file

files <- list.files(path="C:/820WF/LFS_2017_2023", pattern='[.]csv')

files

lfs <- as.data.frame(matrix(ncol=60, nrow=1))

for (i in files){
  
  path <- paste("C:/820WF/LFS_2017_2023/",get("i"), sep="")
  temp1 <- read.csv(path)
  
  colnames(lfs) <- colnames(temp1)
  
  lfs <- rbind(lfs, temp1)
  
  print(i)
  
}  

lfs <- lfs[-1,]

write.csv(lfs, file = "C:/820WF/LFS_2017_2023/lfs_17_23.csv")

#Section 2: Observations and bar plot

# Define the file path
full_dataset_path <- "C:/820WF/LFS_2017_2023/lfs_17_23.csv"

# Read the CSV file
data <- read.csv(full_dataset_path)

head(data)
str(data)



summary(data)

#Section 3: Corealation

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create a correlation matrix (example with mtcars dataset)
correlation_matrix <- cor(data)

# Clean the correlation matrix by replacing NAs with zeros
correlation_matrix[is.na(correlation_matrix)] <- 0

# Ensure that all values are finite (replace Inf with a large value)
correlation_matrix[!is.finite(correlation_matrix)] <- 1e-10

# Calculate hierarchical clustering order
order <- hclust(as.dist(1 - correlation_matrix))$order

# Reorder the correlation matrix and create a long format
correlation_reordered <- correlation_matrix[order, order]
correlation_long <- melt(correlation_reordered)

# Create a ggplot2-style correlation matrix plot
ggplot(data = correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()


# Create a heat map-style correlation matrix plot
heatmap(correlation_matrix, col = colorRampPalette(c("blue", "white", "red"))(50), 
        main = "Correlation Heatmap", xlab = "Variables", ylab = "Variables")

#Section 4: Correlation

# Calculate Pearson correlation
correlation_coefficient <- cor(data$IMMIG, data$EDUC, method = "pearson")
print()

# Assuming you have a data frame named 'df'

# Define the names of the attributes you want to calculate correlations for
attributes_of_interest <- c("Immig", "Sex", "Educ")

# Create an empty data frame to store correlation results
correlation_results <- data.frame()

# Loop through the selected attributes and calculate correlations
for (Immig in attributes_of_interest) {
  for (Sex in attributes_of_interest) {
    if (Immig != Sex) {
      Immig <- data[, Immig]
      Sex <- data[, Sex]
      correlation_coefficient <- cor(Immig, Sex, method = "pearson")
      result <- data.frame(Variable1 = Immig, Variable2 = Sex, Correlation = correlation_coefficient)
      correlation_results <- rbind(correlation_results, result)
    }
  }
}

# Print the correlation table
print(correlation_results)

summary(data)


