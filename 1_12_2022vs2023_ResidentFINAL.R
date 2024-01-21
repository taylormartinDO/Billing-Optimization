
#Load
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)


################
#Cleaning


#Read tabs
tab1 <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 w CPT Change")
tab2 <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 wo CPT Change")

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

combined_dataset <- combined_dataset[combined_dataset$RESIDENT == "Y", ]


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


#####################
#Analysis

#investigation
#SDE x CPT change
# Subset the data to include only rows where SDE is 1
sde_cohort <- combined_dataset[combined_dataset$SDE == 1, ]
# Count the number of rows where both SDE and CPT Change are 1
sde_cpt_change_successes <- sum(sde_cohort$`CPT Change` == 1, na.rm = TRUE)
# Calculate the total number of rows in the SDE cohort
total_sde_rows <- nrow(sde_cohort)
# Calculate the proportion
proportion_sde_cpt_change <- sde_cpt_change_successes / total_sde_rows
# Output the result
proportion_sde_cpt_change



# Prepare the data
sde_cohort <- combined_dataset[combined_dataset$SDE == 1, ]
no_sde_cohort <- combined_dataset[is.null(combined_dataset$SDE) | combined_dataset$SDE != 1, ]

# Calculate the number of successes and sample sizes for each cohort
successes_sde <- sum(sde_cohort$`CPT Change` == 1, na.rm = TRUE)
successes_no_sde <- sum(no_sde_cohort$`CPT Change` == 1, na.rm = TRUE)

n_sde <- nrow(sde_cohort)
n_no_sde <- nrow(no_sde_cohort)

# Perform the two-proportional Z-test
z_test_result <- prop.test(x = c(successes_sde, successes_no_sde),
                           n = c(n_sde, n_no_sde),
                           alternative = "two.sided",
                           correct = FALSE) # Correction not applied for a Z-test

# Output the result
print(z_test_result)







# Assuming your tabs are data frames named 'tab_2023_w_change', 'tab_2023_wo_change', 'tab_2022_w_change', 'tab_2022_wo_change'
tab_2022_w_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2022 w CPT Change")
tab_2022_wo_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2022 wo CPT Change")

# Assuming you have already created the data frames as mentioned before
# Assuming you have data frames named 'filtered_tab_2023_w_change', 'filtered_tab_2023_wo_change', 'filtered_tab_2022_w_change', 'filtered_tab_2022_wo_change'
# Function to rename columns and delete the first row
process_data_frame <- function(df) {
  # Rename columns based on the first row
  colnames(df) <- df[1, ]
  
  # Delete the first row
  df <- df[-1, ]
  
  return(df)
}

# Apply the function to all data frames
tab_2022_w_change <- process_data_frame(tab_2022_w_change)
tab_2022_wo_change <- process_data_frame(tab_2022_wo_change)


# Filter data based on RESIDENT column
filtered_tab_2022_w_change <- tab_2022_w_change[tab_2022_w_change$RESIDENT == 'Y', ]
filtered_tab_2022_wo_change <- tab_2022_wo_change[tab_2022_wo_change$RESIDENT == 'Y', ]










# Calculate proportions for the plot
proportion_sde <- successes_sde / n_sde
proportion_no_sde <- successes_no_sde / n_no_sde
proportion_2022 <- nrow(filtered_tab_2022_w_change) / (nrow(filtered_tab_2022_w_change) + nrow(filtered_tab_2022_wo_change))

# Create a data frame for plotting
plot_data <- data.frame(
  Cohort = c("2023", "2022"),
  Proportion = c(proportion_sde, proportion_2022)
)

# Create the plot
ggplot(plot_data, aes(x = Cohort, y = Proportion, fill = Cohort)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Proportion - 1.96 * sqrt(Proportion * (1 - Proportion) / n_sde),
                    ymax = Proportion + 1.96 * sqrt(Proportion * (1 - Proportion) / n_sde)),
                position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(label = round(Proportion, 3), y = Proportion + 0.01), vjust = 0, hjust = -0.1) +
  annotate("text", x = 1.5, y = max(plot_data$Proportion) + 0.05, label = paste("Z-score:", round(z_test_result$statistic, 2)), color = "red") +
  labs(title = "CPT Changes",
       x = "Cohort",
       y = "Proportion") +
  theme_minimal()

