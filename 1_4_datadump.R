install.packages("readxl")
library(readxl)

####Cleaning
# input data
w_CPT_Change23 <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx")
wo_CPT_Change23 <- read_excel("/Users/taylormartin/Desktop/statistics/data.xlsx", sheet = 2)

#Name column based on the first row
for(i in 1:ncol(w_CPT_Change23)) {
  colnames(w_CPT_Change23)[i] <- as.character(w_CPT_Change23[1, i])
}
for(i in 1:ncol(wo_CPT_Change23)) {
  colnames(wo_CPT_Change23)[i] <- as.character(wo_CPT_Change23[1, i])
}

#Remove first row
w_CPT_Change23 <- w_CPT_Change23[-1, ]
wo_CPT_Change23 <- w_CPT_Change23[-1, ]

wo_CPT_Change23$new_column <- 1
w_CPT_Change23$new_column <- 1
wo_CPT_Change23$new_column <- NA  # Or another value if applicable
combined_dataset <- rbind(w_CPT_Change23, wo_CPT_Change23)
# Assuming the new column is the last column in the dataframe w_CPT_Change23
colnames(combined_dataset)[ncol(combined_dataset)] <- "CPT Change"

combined_dataset$`CPT Change`[is.na(combined_dataset$`CPT Change`)] <- 0
combined_dataset$`CPT Change`[combined_dataset$`CPT Change` != 1] <- 0


########################################
#Investigation
# Number of rows in the dataset
total_rows <- nrow(w_CPT_Change23)
# Number of rows with a 1 in SMRTDTA_ELEM_VALUE column
rows_with_one <- sum(w_CPT_Change23$SMRTDTA_ELEM_VALUE == 1)
# Displaying the results
cat("Total number of rows:", total_rows, "\n")
cat("Rows with 1 in SMRTDTA_ELEM_VALUE:", rows_with_one)


#First, calculate the number of rows with the value of 1 and the total number of rows for each dataset.
prop1 <- sum(w_CPT_Change23$SMRTDTA_ELEM_VALUE == 1) / nrow(w_CPT_Change23)
count1 <- sum(w_CPT_Change23$SMRTDTA_ELEM_VALUE == 1)

prop2 <- sum(wo_CPT_Change23$SMRTDTA_ELEM_VALUE == 1) / nrow(wo_CPT_Change23)
count2 <- sum(wo_CPT_Change23$SMRTDTA_ELEM_VALUE == 1)


#Perform the Two-Proportion Z-Test:
test_result <- prop.test(x = c(count1, count2),
                         n = c(nrow(w_CPT_Change23), nrow(wo_CPT_Change23)),
                         alternative = "two.sided")
#View the Results:
print(test_result)
  


#Calculate the Necessary Counts and Proportions:
#For the group with a 1 in "CPT Change":
group1 <- subset(combined_dataset, `CPT Change` == 1)
success1 <- sum(group1$SMRTDTA_ELEM_VALUE == 1)
n1 <- nrow(group1)
#For the group without a 1 in "CPT Change":
group2 <- subset(combined_dataset, `CPT Change` != 1)
success2 <- sum(group2$SMRTDTA_ELEM_VALUE == 1)
n2 <- nrow(group2)
#Perform the Two-Proportion Z-Test:
test_result <- prop.test(x = c(success1, success2), n = c(n1, n2), alternative = "two.sided")
#Check the Results:
print(test_result)

