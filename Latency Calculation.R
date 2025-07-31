#没有Step3 & 4
#STEP5
#计算negative情况下的correct latency的平均数
library(dplyr)

# Loop through subject numbers for each group (101-132, 201-231, 301-333)
subject_numbers <- c(101:132, 201:231, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311
# Initialize an empty dataframe to store the results
latency_positive <- data.frame(
  case_number = integer(),
  mean_rt_NN = numeric(),
  mean_rt_NE = numeric(),
  stringsAsFactors = FALSE
)
# Loop over each subject number
for (i in subject_numbers) {
  # Construct the dataset names dynamically for both NN and NE datasets
  dataset_name_NE <- paste0("IAT_", i, "_NE")
  dataset_name_NN <- paste0("IAT_", i, "_NN")
  
  # Initialize variables to store the mean values
  mean_rt_NE <- NA
  mean_rt_NN <- NA
  
  # Check if the NE dataset exists
  if (exists(dataset_name_NE)) {
    # Get the NE dataset
    dataset_NE <- get(dataset_name_NE)
    
    # Filter for correct responses (corr == 1) and calculate the mean of rt
    mean_rt_NE <- dataset_NE %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
  
  # Check if the NN dataset exists
  if (exists(dataset_name_NN)) {
    # Get the NN dataset
    dataset_NN <- get(dataset_name_NN)

    # Filter for correct responses (corr == 1) and calculate the mean of rt
    mean_rt_NN <- dataset_NN %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
    
    # Add the results for the current case to the dataframe
  latency_negative <- latency_negative %>%
    add_row(
      case_number = i,
      mean_rt_NN = mean_rt_NN,
      mean_rt_NE = mean_rt_NE
    )
  }


# View the result
View(latency_negative)
nrow(latency_negative)













#计算positive情况下的correct latency
library(dplyr)

# Initialize an empty dataframe to store the results for Positive datasets
latency_positive <- data.frame(
  case_number = integer(),
  mean_rt_PE = numeric(),
  mean_rt_PN = numeric(),
  stringsAsFactors = FALSE
)

# Loop through subject numbers for each group (101-132, 201-232, 301-333)
subject_numbers <- c(101:132, 201:231, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311

# Loop over each subject number
for (i in subject_numbers) {
  # Construct the dataset names dynamically for both PN and PE datasets
  dataset_name_PE <- paste0("IAT_", i, "_PE")
  dataset_name_PN <- paste0("IAT_", i, "_PN")
  
  # Initialize variables to store the mean values
  mean_rt_PE <- NA
  mean_rt_PN <- NA
  
  # Check if the PE dataset exists
  if (exists(dataset_name_PE)) {
    # Get the PE dataset
    dataset_PE <- get(dataset_name_PE)
    
    # Filter for correct responses (corr == 1) and calculate the mean of rt
    mean_rt_PE <- dataset_PE %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
  
  # Check if the PN dataset exists
  if (exists(dataset_name_PN)) {
    # Get the PN dataset
    dataset_PN <- get(dataset_name_PN)
    
    # Filter for correct responses (corr == 1) and calculate the mean of rt
    mean_rt_PN <- dataset_PN %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
  
  # Add the results for the current case to the latency_positive dataframe
  latency_positive <- latency_positive %>%
    add_row(
      case_number = i,
      mean_rt_PN = mean_rt_PN,
      mean_rt_PE = mean_rt_PE
    )
}

# View the result
View(latency_positive)
