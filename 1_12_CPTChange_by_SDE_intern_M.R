

#Load
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)


################
#Cleaning


#Read tabs
tab1 <- read_excel("/Users/taylormartin/Desktop/statistics/data2.xlsx", sheet = "2023 w CPT Change")
tab2 <- read_excel("/Users/taylormartin/Desktop/statistics/data2.xlsx", sheet = "2023 wo CPT Change")
interns_tab <- read_excel("/Users/taylormartin/Desktop/statistics/data2.xlsx", sheet = "Interns_")

#create intern list
interns_list <- interns_tab$Interns
#female list
filtered_female_interns_list <- interns_tab$Interns[interns_tab$Female == 1]
#male list
filtered_male_interns_list <- interns_tab$Interns[interns_tab$Female == 0]

# Add an identifier column to each tab
tab1$SourceTab <- 'Tab1'  # Adding identifier for tab1
tab2$SourceTab <- 'Tab2'  # Adding identifier for tab2

# Aligning Structures of tab1 and tab2
# Adding missing columns and aligning the order
# Find missing columns in both datasets
missing_in_tab1 <- setdiff(colnames(tab2), colnames(tab1))
missing_in_tab2 <- setdiff(colnames(tab1), colnames(tab2))
# Add missing columns initialized with NA
for(col in missing_in_tab1) {
  tab1[[col]] <- NA
}
for(col in missing_in_tab2) {
  tab2[[col]] <- NA
}
# Align the column order
tab1 <- tab1[colnames(tab2)]
# Combine the datasets
combined_dataset <- rbind(tab1, tab2)

#Cleaning
#Name column based on the first row
for(i in 1:ncol(combined_dataset)) {
  colnames(combined_dataset)[i] <- as.character(combined_dataset[1, i])
}
#Remove first row
combined_dataset <- combined_dataset[-1, ]


combined_dataset <- combined_dataset[combined_dataset$SERV_PROV_NAME %in% filtered_male_interns_list, ]


# Check the result of the filtering
print(head(combined_dataset))
print(paste("Number of rows after filtering:", nrow(combined_dataset)))

# Assuming your combined dataset is named 'combined_dataset'
combined_dataset$CombinedColumn <- ifelse(combined_dataset$Tab1 == "Tab1", 
                                          combined_dataset$SMRTDTA_ELEM_VALUE, 
                                          combined_dataset$SmartDataElementValue)

## Rename the column
colnames(combined_dataset)[colnames(combined_dataset) == "Tab1"] <- "CPT Change"
# Update values in the column
combined_dataset$`CPT Change` <- ifelse(combined_dataset$`CPT Change` == "Tab1", 1, 0)

# Rename the column
colnames(combined_dataset)[colnames(combined_dataset) == "CombinedColumn"] <- "SDE"









# Assuming combined_dataset is your data frame

# Check the structure and unique values in the SDE and CPT Change columns
print(str(combined_dataset$SDE))
print(unique(combined_dataset$SDE))
print(str(combined_dataset$`CPT Change`))
print(unique(combined_dataset$`CPT Change`))

# Convert SDE and CPT Change to numeric if they are not already
combined_dataset$SDE <- as.numeric(combined_dataset$SDE)
combined_dataset$`CPT Change` <- as.numeric(combined_dataset$`CPT Change`)

# Splitting the data into two groups
group_1 <- combined_dataset[combined_dataset$SDE == 1, ]
group_2 <- combined_dataset[is.na(combined_dataset$SDE) | combined_dataset$SDE != 1, ]

# Count the number of rows where CPT Change is 1 in each group
successes_group_1 <- sum(group_1$`CPT Change` == 1, na.rm = TRUE)
successes_group_2 <- sum(group_2$`CPT Change` == 1, na.rm = TRUE)

# Check the counts
print(paste("Successes in Group 1:", successes_group_1))
print(paste("Successes in Group 2:", successes_group_2))

# Calculate the total number of observations in each group
n_group_1 <- nrow(group_1)
n_group_2 <- nrow(group_2)

# Proceed with Z-test if both groups have observations
if (n_group_1 <= 0 | n_group_2 <= 0) {
  stop("Error: One or both of the groups have no observations. Please check your data and filtering criteria.")
}

# Perform the two-proportional Z-test
z_test_result <- prop.test(x = c(successes_group_1, successes_group_2),
                           n = c(n_group_1, n_group_2),
                           alternative = "two.sided",
                           correct = FALSE)

# Output the result
print(z_test_result)


# Load ggplot2
library(ggplot2)

# Calculate proportions for each group
proportion_group_1 <- successes_group_1 / n_group_1
proportion_group_2 <- successes_group_2 / n_group_2

# Create a data frame for plotting
plot_data <- data.frame(
  Group = c("SDE", "No SDE"),
  Proportion = c(proportion_group_1, proportion_group_2)
)

# Extract Z-test statistic and p-value
z_score <- z_test_result$statistic
p_value <- z_test_result$p.value

# Create the plot
ggplot(plot_data, aes(x = Group, y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Proportion, 3), y = Proportion), vjust = -0.3) +
  annotate("text", x = 1.5, y = max(plot_data$Proportion), 
           label = paste("Z-score:", round(z_score, 2), "\nP-value:", round(p_value, 3)), 
           hjust = 0.5, vjust = 1.5, color = "blue", size = 4) +
  labs(title = "Proportion of 'CPT Change' in Each Group",
       x = "Group",
       y = "Proportion") +
  theme_minimal()
