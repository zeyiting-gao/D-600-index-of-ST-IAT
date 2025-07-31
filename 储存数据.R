#在数据处理的一开始，我手动把所有被试的IAT结果excel文件手动加载到R中，并命名为X101，X102....
#以下代码是我首先尝试把第一个被试的IAT结果excel文件赋值到IAT_101中，且只保留和数据分析相关的列
library(tidyverse)
IAT_101 <- X101 %>%
  slice(3:n()) %>%  # 前面两行无内容，所以不保留
  select(
    rule = conds_file,   #这三行是重命名原始excel文件中较为冗长的变量名
    corr = key_resp.corr, 
    rt = key_resp.rt      
  )
IAT_101 <- data_101[complete.cases(X_101), ] #把含有NA的行去掉（e.g.,删除规则改变的中间的空白行，以使得dataset更加精简）
IAT_101 <- data_101[!IAT_101$rule %in% c("neutral_emotional_practice.xlsx","None"),] #（把训练和最后一行删除掉）

#按照类似逻辑将中立组都放进去

for (i in 101:132) {
  if (i != 119 && exists(paste0("X", i))) {  # Skip X119 and check if the dataset exists
    dataset_name <- paste0("X", i)  # Generate dataset name (X102, X103, ...)
    IAT_name <- paste0("IAT_", i)   # Generate corresponding IAT name (IAT_102, IAT_103, ...)
    
    # Apply the transformation and create new dataset
    assign(IAT_name, get(dataset_name) %>%
             slice(3:n()) %>%
             select(
               rule = conds_file,   
               corr = key_resp.corr, 
               rt = key_resp.rt      
             ) %>%
             filter(complete.cases(.)) %>%
             filter(!rule %in% c("neutral_emotional_practice.xlsx", "None")))
  }
}

for (i in 201:232) {
  if (i != 210 && exists(paste0("X", i))) {  # Skip X210 and check if the dataset exists
    dataset_name <- paste0("X", i)  # Generate dataset name (X102, X103, ...)
    IAT_name <- paste0("IAT_", i)   # Generate corresponding IAT name (IAT_102, IAT_103, ...)
    
    # Apply the transformation and create new dataset
    assign(IAT_name, get(dataset_name) %>%
             slice(3:n()) %>%
             select(
               rule = conds_file,   
               corr = key_resp.corr, 
               rt = key_resp.rt      
             ) %>%
             filter(complete.cases(.)) %>%
             filter(!rule %in% c("neutral_emotional_practice.xlsx", "None")))
  }
}


for (i in 301:333) {
  if (i != 311 && exists(paste0("X", i))) {  # Skip X210 and check if the dataset exists
    dataset_name <- paste0("X", i)  # Generate dataset name (X102, X103, ...)
    IAT_name <- paste0("IAT_", i)   # Generate corresponding IAT name (IAT_102, IAT_103, ...)
    
    # Apply the transformation and create new dataset
    assign(IAT_name, get(dataset_name) %>%
             slice(3:n()) %>%
             select(
               rule = conds_file,   
               corr = key_resp.corr, 
               rt = key_resp.rt      
             ) %>%
             filter(complete.cases(.)) %>%
             filter(!rule %in% c("neutral_emotional_practice.xlsx", "None")))
  }
}

subject_numbers <- c(101:132, 201:232, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311

#然后我发现IAT232是无效数据，因为里面有一个reaction time是“none”，所有数据中只有它一个是有该问题的，因此排除出后续分析。
# 下面是用于检查其他IAT的数据里面有没有reaction time是非数字”none“的
for (i in subject_numbers) {
  # Construct the dataset name dynamically
  dataset_name <- paste0("IAT_", i)
  
  # Check if the dataset exists
  if (exists(dataset_name)) {
    # Get the dataset
    dataset <- get(dataset_name)
    
    # Check the class of 'rt' column
    if (is.character(dataset$rt)) {
      # Output the name of the dataset if 'rt' is a character
      print(paste("Dataset", dataset_name, "has 'rt' as character."))
    }
  }
}
