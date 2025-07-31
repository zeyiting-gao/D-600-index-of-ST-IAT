#STEP6
#计算negative的pooled SD

# First, add the pooled_sd column to the latency_negative dataframe
latency_negative$pooled_sd <- NA

# Loop through each subject
for (i in c(101:118, 120:132, 201:209, 211:231, 301:310, 312:333)) {
  
  # Create the names of the datasets for the current subject
  NN_name <- paste0("IAT_", i, "_NN")
  NE_name <- paste0("IAT_", i, "_NE")
  
  # Check if both datasets exist
  if (exists(NN_name) && exists(NE_name)) {
    
    # Get the dataframes for the current subject
    NN_data <- get(NN_name)
    NE_data <- get(NE_name)
    
    # Calculate the sample size (n) and standard deviation (sd) for all responses (not just correct ones)
    n1 <- nrow(NN_data)  # Total number of responses in NN
    n2 <- nrow(NE_data)  # Total number of responses in NE
    
    sd1 <- sd(NN_data$rt)  # Standard deviation for all responses in NN
    sd2 <- sd(NE_data$rt)  # Standard deviation for all responses in NE
    
    # Calculate the pooled standard deviation (using the formula for two independent samples)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    
    # Update the latency_negative dataframe with the pooled_sd for the current case
    latency_negative$pooled_sd[latency_negative$case_number == i] <- pooled_sd
  }
}

# Check the result
# First, add the pooled_sd column to the latency_positive dataframe
latency_positive$pooled_sd <- NA

# Loop through each subject
for (i in c(101:118, 120:132, 201:209, 211:231, 301:310, 312:333)) {
  
  # Create the names of the datasets for the current subject
  PN_name <- paste0("IAT_", i, "_PN")
  PE_name <- paste0("IAT_", i, "_PE")
  
  # Check if both datasets exist
  if (exists(PN_name) && exists(PE_name)) {
    
    # Get the dataframes for the current subject
    PN_data <- get(PN_name)
    PE_data <- get(PE_name)
    
    # Calculate the sample size (n) and standard deviation (sd) for all responses (not just correct ones)
    n1 <- nrow(PN_data)  # Total number of responses in PN
    n2 <- nrow(PE_data)  # Total number of responses in PE
    
    sd1 <- sd(PN_data$rt)  # Standard deviation for all responses in PN
    sd2 <- sd(PE_data$rt)  # Standard deviation for all responses in PE
    
    # Calculate the pooled standard deviation (using the formula for two independent samples)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    
    # Update the latency_positive dataframe with the pooled_sd for the current case
    latency_positive$pooled_sd[latency_positive$case_number == i] <- pooled_sd
  }
}

# Check the result
view(latency_positive)

