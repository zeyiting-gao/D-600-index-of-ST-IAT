#STEP7
#计算NN的纠正之后的rt
# Assuming latency_negative dataset contains case_number and mean_rt_NN
# Iterate over each dataset and create the adj_rt column
for(i in c(101:132, 201:231, 301:333)) {
  
  # Skip if case_number is 119 or 210 or 311
  if (i %in% c(119, 210, 311)) next
  
  # Create the dataset name dynamically
  dataset_name <- paste0("IAT_", i, "_NN")
  
  # Check if the dataset exists
  if (exists(dataset_name)) {
    
    # Get the mean_rt_NN for the current subject (i)
    mean_rt <- latency_negative %>%
      filter(case_number == i) %>%
      pull(mean_rt_NN) # Extract the mean_rt_NN for that case
    
    # Update the dataset with the new adj_rt column
    assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}

#计算NE的纠正之后的rt
# Assuming latency_negative dataset contains case_number and mean_rt_NE
# Iterate over each dataset and create the adj_rt column
for(i in c(101:132, 201:231, 301:333)) {
  
  # Skip if case_number is 119 or 210 or 311
  if (i %in% c(119, 210, 311)) next
  
  # Create the dataset name dynamically
  dataset_name <- paste0("IAT_", i, "_NE")
  
  # Check if the dataset exists
  if (exists(dataset_name)) {
    
    # Get the mean_rt_NE for the current subject (i)
    mean_rt <- latency_negative %>%
      filter(case_number == i) %>%
      pull(mean_rt_NE) # Extract the mean_rt_NE for that case
    
    # Update the dataset with the new adj_rt column
    assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}

#计算PE的纠正之后的rt
# Assuming latency_PEgative dataset contains case_number and mean_rt_PE
# Iterate over each dataset and create the adj_rt column
for(i in c(101:132, 201:231, 301:333)) {
  
  # Skip if case_number is 119 or 210 or 311
  if (i %in% c(119, 210, 311)) next
  
  # Create the dataset name dynamically
  dataset_name <- paste0("IAT_", i, "_PE")
  
  # Check if the dataset exists
  if (exists(dataset_name)) {
    
    # Get the mean_rt_PE for the current subject (i)
    mean_rt <- latency_positive %>%
      filter(case_number == i) %>%
      pull(mean_rt_PE) # Extract the mean_rt_PE for that case
    
    # Update the dataset with the PEw adj_rt column
    assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}

#计算PN纠正过的rt
for(i in c(101:132, 201:231, 301:333)) {
  
  # Skip if case_number is 119 or 210 or 311
  if (i %in% c(119, 210, 311)) next
  
  # Create the dataset name dynamically
  dataset_name <- paste0("IAT_", i, "_PN")
  
  # Check if the dataset exists
  if (exists(dataset_name)) {
    
    # Get the mean_rt_PN for the current subject (i)
    mean_rt <- latency_positive %>%
      filter(case_number == i) %>%
      pull(mean_rt_PN) # Extract the mean_rt_PN for that case
    
    # Update the dataset with the PNw adj_rt column
    assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}
view(latency_negative)


