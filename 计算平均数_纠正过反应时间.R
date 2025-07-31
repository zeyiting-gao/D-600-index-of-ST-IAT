#STEP9
#计算NN的调整之后的rt的平均数
latency_negative <- latency_negative %>%
  mutate(mean_adj_NN_re = NA) 

for(i in c(101:132, 201:231, 301:333)) {  # Loop through each subject ID
  
  # Skip certain subjects by case number
  if (i %in% c(119, 210, 311)) next
  
  # Dynamically create the dataset name for the current subject
  dataset_name <- paste0("IAT_", i, "_NN")  # For example, "IAT_101_NN"
  
  # Check if the dataset for the current subject exists
  if (exists(dataset_name)) {
    
    # Calculate the mean of adj_rt for the current subject dataset
    mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE)  # Calculate the mean of adj_rt, ignoring NA values
    
    # Update the latency_negative dataset by assigning mean_adj_rt to the correct row
    latency_negative <- latency_negative %>%
      mutate(mean_adj_NN_re = ifelse(case_number == i, mean_adj_rt, mean_adj_NN_re))
  }
}

# View the updated latency_negative dataset
view(latency_negative)

#计算NE的调整之后的rt的平均数
latency_negative <- latency_negative %>%
  mutate(mean_adj_NE_re = NA) 

for(i in c(101:132, 201:231, 301:333)) {  # Loop through each subject ID
  
  # Skip certain subjects by case number
  if (i %in% c(119, 210, 311)) next
  
  # Dynamically create the dataset name for the current subject
  dataset_name <- paste0("IAT_", i, "_NE")  # For example, "IAT_101_NE"
  
  # Check if the dataset for the current subject exists
  if (exists(dataset_name)) {
    
    # Calculate the mean of adj_rt for the current subject dataset
    mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE)  # Calculate the mean of adj_rt, ignoring NA values
    
    # Update the latency_negative dataset by assigning mean_adj_rt to the correct row
    latency_negative <- latency_negative %>%
      mutate(mean_adj_NE_re = ifelse(case_number == i, mean_adj_rt, mean_adj_NE_re))
  }
}

# View the updated latency_negative dataset
view(latency_negative)

#计算PE的调整过的rt的平均数
latency_positive <- latency_positive %>%
  mutate(mean_adj_PE_re = NA) 

for(i in c(101:132, 201:231, 301:333)) {  # Loop through each subject ID
  
  # Skip certain subjects by case number
  if (i %in% c(119, 210, 311)) next
  
  # Dynamically create the dataset name for the current subject
  dataset_name <- paste0("IAT_", i, "_PE")  # For example, "IAT_101_PE"
  
  # Check if the dataset for the current subject exists
  if (exists(dataset_name)) {
    
    # Calculate the mean of adj_rt for the current subject dataset
    mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE)  # Calculate the mean of adj_rt, ignoring NA values
    
    # Update the latency_positive dataset by assigning mean_adj_rt to the correct row
    latency_positive <- latency_positive %>%
      mutate(mean_adj_PE_re = ifelse(case_number == i, mean_adj_rt, mean_adj_PE_re))
  }
}

# View the updated latency_positive dataset
  view(latency_positive)
  
#计算PN调整之后的rt的平均数
  latency_positive <- latency_positive %>%
    mutate(mean_adj_PN_re = NA) 
  
  for(i in c(101:132, 201:231, 301:333)) {  # Loop through each subject ID
    
    # Skip certain subjects by case number
    if (i %in% c(119, 210, 311)) next
    
    # Dynamically create the dataset name for the current subject
    dataset_name <- paste0("IAT_", i, "_PN")  # For example, "IAT_101_PN"
    
    # Check if the dataset for the current subject exists
    if (exists(dataset_name)) {
      
      # Calculate the mean of adj_rt for the current subject dataset
      mean_adj_rt <- get(dataset_name) %>%
        pull(adj_rt) %>%
        mean(., na.rm = TRUE)  # Calculate the mean of adj_rt, ignoring NA values
      
      # Update the latency_positive dataset by assigning mean_adj_rt to the correct row
      latency_positive <- latency_positive %>%
        mutate(mean_adj_PN_re = ifelse(case_number == i, mean_adj_rt, mean_adj_PN_re))
    }
  }
  
  # View the updated latency_positive dataset
  view(latency_positive)
  
