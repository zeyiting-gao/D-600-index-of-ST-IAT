#在去除外异常case之后，将每个subject的数据按照规则分成四个独立的datasets
#因为实际上我们在里面做了两次独立的IAT（食物和积极情绪的联想；食物和消极情绪的联想）
#因为每个IAT中分别有两次规则（如食物和积极情绪的联想有规则1:食物和积极一组；规则2：食物和消极一组，所以我把四个规则都单独储存以方便计算）
class(IAT_101$rule)
for (i in 101:132) {
  if (i != 119 && exists(paste0("IAT_", i))) {  # Check if dataset exists and not IAT_119
    # Get the dataset dynamically
    dataset_name <- get(paste0("IAT_", i))
    
    # Split the dataset by the 'rule' column into four separate dataframes based on the rule values
    IAT_PN <- dataset_name %>% filter(rule == "positive_food_neutral_block.xlsx")  # Positive Neutral
    IAT_PE <- dataset_name %>% filter(rule == "positive_food_emotional_block.xlsx")  # Positive Emotional
    IAT_NN <- dataset_name %>% filter(rule == "negative_food_neutral_block.xlsx")  # Negative Neutral
    IAT_NE <- dataset_name %>% filter(rule == "negative_food_emotional_block.xlsx")  # Negative Emotional
    
    # Assign the new dataframes to the global environment with the appropriate names
    assign(paste0("IAT_", i, "_PN"), IAT_PN)
    assign(paste0("IAT_", i, "_PE"), IAT_PE)
    assign(paste0("IAT_", i, "_NN"), IAT_NN)
    assign(paste0("IAT_", i, "_NE"), IAT_NE)
  }
}

for (i in 201:232) {
  if (i != 210 && exists(paste0("IAT_", i))) {  # Check if dataset exists and not IAT_119
    # Get the dataset dynamically
    dataset_name <- get(paste0("IAT_", i))
    
    # Split the dataset by the 'rule' column into four separate dataframes based on the rule values
    IAT_PN <- dataset_name %>% filter(rule == "positive_food_neutral_block.xlsx")  # Positive Neutral
    IAT_PE <- dataset_name %>% filter(rule == "positive_food_emotional_block.xlsx")  # Positive Emotional
    IAT_NN <- dataset_name %>% filter(rule == "negative_food_neutral_block.xlsx")  # Negative Neutral
    IAT_NE <- dataset_name %>% filter(rule == "negative_food_emotional_block.xlsx")  # Negative Emotional
    
    # Assign the new dataframes to the global environment with the appropriate names
    assign(paste0("IAT_", i, "_PN"), IAT_PN)
    assign(paste0("IAT_", i, "_PE"), IAT_PE)
    assign(paste0("IAT_", i, "_NN"), IAT_NN)
    assign(paste0("IAT_", i, "_NE"), IAT_NE)
  }
}

for (i in 301:333) {
  if (i != 311 && exists(paste0("IAT_", i))) {  # Check if dataset exists and not IAT_119
    # Get the dataset dynamically
    dataset_name <- get(paste0("IAT_", i))
    
    # Split the dataset by the 'rule' column into four separate dataframes based on the rule values
    IAT_PN <- dataset_name %>% filter(rule == "positive_food_neutral_block.xlsx")  # Positive Neutral
    IAT_PE <- dataset_name %>% filter(rule == "positive_food_emotional_block.xlsx")  # Positive Emotional
    IAT_NN <- dataset_name %>% filter(rule == "negative_food_neutral_block.xlsx")  # Negative Neutral
    IAT_NE <- dataset_name %>% filter(rule == "negative_food_emotional_block.xlsx")  # Negative Emotional
    
    # Assign the new dataframes to the global environment with the appropriate names
    assign(paste0("IAT_", i, "_PN"), IAT_PN)
    assign(paste0("IAT_", i, "_PE"), IAT_PE)
    assign(paste0("IAT_", i, "_NN"), IAT_NN)
    assign(paste0("IAT_", i, "_NE"), IAT_NE)
  }
}
