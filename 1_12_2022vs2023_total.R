

#Load
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)



# Assuming your tabs are data frames named 'tab_2023_w_change', 'tab_2023_wo_change', 'tab_2022_w_change', 'tab_2022_wo_change'
tab_2023_w_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 w CPT Change")
tab_2023_wo_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2023 wo CPT Change")
tab_2022_w_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2022 w CPT Change")
tab_2022_wo_change <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = "2022 wo CPT Change")



# Calculate the proportions for 2023
proportion_2023_w_change <- nrow(tab_2023_w_change) / (nrow(tab_2023_w_change) + nrow(tab_2023_wo_change))

# Calculate the proportions for 2022
proportion_2022_w_change <- nrow(tab_2022_w_change) / (nrow(tab_2022_w_change) + nrow(tab_2022_wo_change))

# Print or use the proportions
print(proportion_2023_w_change)
print(proportion_2022_w_change)


# Assuming you have already created the data frames as mentioned before

# Visualization - Bar plot
barplot(
  c(
    nrow(tab_2023_w_change) / (nrow(tab_2023_w_change) + nrow(tab_2023_wo_change)),
    nrow(tab_2022_w_change) / (nrow(tab_2022_w_change) + nrow(tab_2022_wo_change))
  ),
  names.arg = c("2023", "2022"),
  ylim = c(0, 1),
  ylab = "Proportion",
  main = "Proportion of Rows with CPT Change"
)

# Statistical test - Chi-square test
chi_square_result <- chisq.test(
  matrix(
    c(
      nrow(tab_2023_w_change), nrow(tab_2023_wo_change),
      nrow(tab_2022_w_change), nrow(tab_2022_wo_change)
    ),
    ncol = 2,
    byrow = TRUE
  )
)

# Print results
print(chi_square_result)
