
# Assuming your tabs are data frames named 'tab_2023_w_change', 'tab_2023_wo_change', 'tab_2022_w_change', 'tab_2022_wo_change'
tab_2023_w_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 w CPT Change")
tab_2023_wo_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 wo CPT Change")
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
tab_2023_w_change <- process_data_frame(tab_2023_w_change)
tab_2023_wo_change <- process_data_frame(tab_2023_wo_change)
tab_2022_w_change <- process_data_frame(tab_2022_w_change)
tab_2022_wo_change <- process_data_frame(tab_2022_wo_change)


# Filter data based on RESIDENT column
filtered_tab_2023_w_change <- tab_2023_w_change[tab_2023_w_change$RESIDENT == 'Y', ]
filtered_tab_2023_wo_change <- tab_2023_wo_change[tab_2023_wo_change$RESIDENT == 'Y', ]
filtered_tab_2022_w_change <- tab_2022_w_change[tab_2022_w_change$RESIDENT == 'Y', ]
filtered_tab_2022_wo_change <- tab_2022_wo_change[tab_2022_wo_change$RESIDENT == 'Y', ]




# Assuming you have proportions for 2023 and 2022 (replace with your actual proportions)
proportion_2023 <- nrow(filtered_tab_2023_w_change) / (nrow(filtered_tab_2023_w_change) + nrow(filtered_tab_2023_wo_change))
proportion_2022 <- nrow(filtered_tab_2022_w_change) / (nrow(filtered_tab_2022_w_change) + nrow(filtered_tab_2022_wo_change))

# Combine proportions and names into a data frame
proportions_data <- data.frame(
  Proportion = c(proportion_2023, proportion_2022),
  Category = c("2023", "2022")
)

# Create a bar plot
barplot(
  proportions_data$Proportion,
  names.arg = proportions_data$Category,
  ylim = c(0, 1),
  ylab = "Proportion",
  main = "Proportion of Rows with CPT Change (RESIDENT = Y)"
)

# Add text annotations
text(1, proportion_2023, sprintf("%.2f", proportion_2023), pos = 3, col = "red", cex = 1.2)
text(2, proportion_2022, sprintf("%.2f", proportion_2022), pos = 3, col = "red", cex = 1.2)





# Statistical test - Chi-square test
chi_square_result_filtered <- chisq.test(
  matrix(
    c(
      nrow(filtered_tab_2023_w_change), nrow(filtered_tab_2023_wo_change),
      nrow(filtered_tab_2022_w_change), nrow(filtered_tab_2022_wo_change)
    ),
    ncol = 2,
    byrow = TRUE
  )
)

# Print results
print(chi_square_result_filtered)
