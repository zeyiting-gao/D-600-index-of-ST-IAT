####储存数据####
#在数据处理的一开始，我手动把所有被试的IAT结果excel文件手动加载到R中，并命名为X101，X102....
#按照类似逻辑将其他数据组都放进去（里面有几个被试的数据缺失或者无效，如”119“，”210“，所以就不将它们导入）
for (i in 101:132) {
  if (i != 119 && exists(paste0("X", i))) 
    dataset_name <- paste0("X", i)  # Generate dataset name (X102, X103, ...)
    IAT_name <- paste0("IAT_", i)   # Generate corresponding IAT name (IAT_102, IAT_103, ...)
    
# 把X+数字的文件 的关键信息储存到IAT+数字 文件
    assign(IAT_name, get(dataset_name) %>%
             slice(3:n()) %>%  #前两行无信息，所以从第三行开始
             select(
               rule = conds_file,   #重命名变量，让变量名更容易理解
               corr = key_resp.corr, 
               rt = key_resp.rt      
             ) %>%
             filter(complete.cases(.)) %>%
             filter(!rule %in% c("neutral_emotional_practice.xlsx", "None"))) #这个是practice trial的结果，所以去除
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

#创造一个subject_numbers，代表我们需要处理的有效数据的编号
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


###分解数据### 
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
    
    # Assign the new dataframes with the appropriate names
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
    
    IAT_PN <- dataset_name %>% filter(rule == "positive_food_neutral_block.xlsx")  # Positive Neutral
    IAT_PE <- dataset_name %>% filter(rule == "positive_food_emotional_block.xlsx")  # Positive Emotional
    IAT_NN <- dataset_name %>% filter(rule == "negative_food_neutral_block.xlsx")  # Negative Neutral
    IAT_NE <- dataset_name %>% filter(rule == "negative_food_emotional_block.xlsx")  # Negative Emotional
    
    assign(paste0("IAT_", i, "_PN"), IAT_PN)
    assign(paste0("IAT_", i, "_PE"), IAT_PE)
    assign(paste0("IAT_", i, "_NN"), IAT_NN)
    assign(paste0("IAT_", i, "_NE"), IAT_NE)
  }
}

for (i in 301:333) {
  if (i != 311 && exists(paste0("IAT_", i))) {  
    dataset_name <- get(paste0("IAT_", i))
    
    IAT_PN <- dataset_name %>% filter(rule == "positive_food_neutral_block.xlsx")  # Positive Neutral
    IAT_PE <- dataset_name %>% filter(rule == "positive_food_emotional_block.xlsx")  # Positive Emotional
    IAT_NN <- dataset_name %>% filter(rule == "negative_food_neutral_block.xlsx")  # Negative Neutral
    IAT_NE <- dataset_name %>% filter(rule == "negative_food_emotional_block.xlsx")  # Negative Emotional
    
    assign(paste0("IAT_", i, "_PN"), IAT_PN)
    assign(paste0("IAT_", i, "_PE"), IAT_PE)
    assign(paste0("IAT_", i, "_NN"), IAT_NN)
    assign(paste0("IAT_", i, "_NE"), IAT_NE)
  }
}


###STEP2###（根据算法的论文）
#根据算法要求 去除作答过快的被试 （最终无output，说明所有cases都满足）
# Loop through subject numbers for each group (101-132, 201-232, 301-333)
subject_numbers <- c(101:132, 201:231, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 232 & subject_numbers != 311]  # Exclude 119, 210, 311

# Loop over each subject number
for (i in subject_numbers) {
  # Construct the dataset name dynamically
  dataset_name <- paste0("IAT_", i)
  
  if (exists(dataset_name)) {
    # Get the dataset
    dataset <- get(dataset_name)
    # 抽出来每个被试里反应速度小于0.3 的回答
    filtered_data <- dataset %>% filter(rt < 0.3)
    # 看一下每个被试的文件里rt<0.3里有没有超过34个 （34 是标准中规定的1/10的行总数）
    if (nrow(filtered_data) > 34) {
           print(paste("Dataset", dataset_name, "has more than 34 rows with rt < 0.3"))
    }
  }
}
#最好发现没有满足这个条件的，说明所有被试在这个标准下都没有乱答，所以我们不需要去除被试。


#根据算法要求； 在每一个subject的dataset中删除作答时间过长的cases
subject_numbers <- c(101:132, 201:232, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311

# Loop over each subject number
for (i in subject_numbers) {
    dataset_name <- paste0("IAT_", i)
   if (exists(dataset_name)) {  
    dataset <- get(dataset_name)
    rows_before <- nrow(dataset)
    filtered_data <- dataset %>% filter(rt <= 10)
    rows_after <- nrow(filtered_data)
    rows_deleted <- rows_before - rows_after  #比较一下每个被试有多少反应是超过10s的
    
    # Report if any rows were deleted
    if (rows_deleted > 0) {
      print(paste("Dataset", dataset_name, "had", rows_deleted, "rows deleted (rt > 10)."))
      #filtered_data里储存的是rt<10s的反应，所以重新储存相当于rt>10的反应在后续数据分析中剔除
      assign(dataset_name, filtered_data)
    }
  }
}



###算法中无Step3 & 4


####STEP5####（对应算法中的step5)
#计算negative情况下的correct latency的平均数
library(dplyr)
subject_numbers <- c(101:132, 201:231, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311

# Initialize an empty dataframe to store the results
latency_negative <- data.frame(
  case_number = integer(),
  mean_rt_NN = numeric(),
  mean_rt_NE = numeric(),
  stringsAsFactors = FALSE  #这一条是chatgpt生成的，我不太知道什么意思
)

for (i in subject_numbers) {
  # Construct the dataset names dynamically for both NN and NE datasets
  dataset_name_NE <- paste0("IAT_", i, "_NE")
  dataset_name_NN <- paste0("IAT_", i, "_NN")
  
  mean_rt_NE <- NA  #创造一列，后面会将正确作答的反应时间的平均数赋值到这个变量中
  mean_rt_NN <- NA
  
 
  if (exists(dataset_name_NE)) {
    dataset_NE <- get(dataset_name_NE)
    
    # 计算答对题目的平均数（所以先筛选corr=1,再计算反应速度的平均数mean_rt）
    mean_rt_NE <- dataset_NE %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
  
  # 与上面代码同理，对NN的平均rt进行计算
  if (exists(dataset_name_NN)) {
    dataset_NN <- get(dataset_name_NN)
    mean_rt_NN <- dataset_NN %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)  #pull 函数的功能相当于把一个column转化为一个vector，相当于把mean_rt赋值给mean_rt_NN
  }
  
  # Add the results for the current case to the dataframe
  latency_negative <- latency_negative %>%
    add_row(
      case_number = i,
      mean_rt_NN = mean_rt_NN,
      mean_rt_NE = mean_rt_NE  #将上面计算的反应时间平均数一条一条的储存在mean_rt_NN/NE的变量中
    )
}

#同理，计算positive情况下的correct latency
library(dplyr)

# 同理，创造一个代表positive情况的dataset，来储存PE和PN状态下的平均反应速度
latency_positive <- data.frame(
  case_number = integer(),
  mean_rt_PE = numeric(),
  mean_rt_PN = numeric(),
  stringsAsFactors = FALSE
)

subject_numbers <- c(101:132, 201:231, 301:333)
subject_numbers <- subject_numbers[subject_numbers != 119 & subject_numbers != 210 & subject_numbers != 311]  # Exclude 119, 210, 311


for (i in subject_numbers) {
 
  dataset_name_PE <- paste0("IAT_", i, "_PE")
  dataset_name_PN <- paste0("IAT_", i, "_PN")
  
  mean_rt_PE <- NA
  mean_rt_PN <- NA
  
   if (exists(dataset_name_PE)) {
      dataset_PE <- get(dataset_name_PE)
    
    # 同理，计算回答正确的反应速度的平均数
    mean_rt_PE <- dataset_PE %>%
      filter(corr == 1) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      pull(mean_rt)
  }
  
  if (exists(dataset_name_PN)) {
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
      mean_rt_PN = mean_rt_PN,    #将上面计算的反应时间平均数一条一条的储存在mean_rt_PN/PE的变量中
      mean_rt_PE = mean_rt_PE
    )
}



####Step6####（根据算法中的step6）
#计算negative的pooled SD

latency_negative$pooled_sd_negative <- NA #这个变量代表负面情绪下IAT的反应时间的pooled SD

for (i in c(101:118, 120:132, 201:209, 211:231, 301:310, 312:333)) {
    NN_name <- paste0("IAT_", i, "_NN")
  NE_name <- paste0("IAT_", i, "_NE")
 
  if (exists(NN_name) && exists(NE_name)) {
        NN_data <- get(NN_name)
    NE_data <- get(NE_name)
    
    n1 <- nrow(NN_data)  # Total number of responses in NN
    n2 <- nrow(NE_data)  # Total number of responses in NE
    
    sd1 <- sd(NN_data$rt)  # Standard deviation for all responses in NN
    sd2 <- sd(NE_data$rt)  # Standard deviation for all responses in NE
    
    # Calculate the pooled standard deviation (using the formula for two independent samples)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    
    # Update the latency_negative dataframe with the pooled_sd for the current case
    latency_negative$pooled_sd_negative[latency_negative$case_number == i] <- pooled_sd
  }
}

#同理，对positive也进行相同操作
latency_positive$pooled_sd_positive <- NA


for (i in c(101:118, 120:132, 201:209, 211:231, 301:310, 312:333)) {
  
  PN_name <- paste0("IAT_", i, "_PN")
  PE_name <- paste0("IAT_", i, "_PE")
  
  if (exists(PN_name) && exists(PE_name)) {
    
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
    latency_positive$pooled_sd_positive[latency_positive$case_number == i] <- pooled_sd
  }
}

# Check the result
view(latency_positive)



####Step7###
#STEP7
#计算NN的纠正之后的rt （纠正=把回答错误的那一个反应的反应时间+600ms）
for(i in c(101:132, 201:231, 301:333)) {
    if (i %in% c(119, 210, 311)) next #就是对存在的IAT_XXX文件进行操作。跳过不存在的三个我呢见
  
  dataset_name <- paste0("IAT_", i, "_NN")
  
  if (exists(dataset_name)) {

 # Extract the mean_rt_NN for that case #其实这个地方可以直接根据mean_rt_NN进行操作，但是因为统一后面部分的代码，所以把mean_rt_NN先赋值到mean_rt
    mean_rt <- latency_negative %>%
      filter(case_number == i) %>%
      pull(mean_rt_NN)
    
    # Update the dataset with the new adj_rt column
    assign(dataset_name, get(dataset_name) %>%
        mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6))) #创造adj_rt代表调整过后的反应时间，如果回答正确则不修改，回答错误则加0.6s.
  }
}

#和上面同理，计算NE的纠正之后的rt
for(i in c(101:132, 201:231, 301:333)) {
  
  if (i %in% c(119, 210, 311)) next
  
  dataset_name <- paste0("IAT_", i, "_NE")
    if (exists(dataset_name)) {
    
      mean_rt <- latency_negative %>%
      filter(case_number == i) %>%
      pull(mean_rt_NE) 
    
  assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}

#计算PE的纠正之后的rt
for(i in c(101:132, 201:231, 301:333)) {
  
    if (i %in% c(119, 210, 311)) next
  
  dataset_name <- paste0("IAT_", i, "_PE")
  
  if (exists(dataset_name)) {
    
    mean_rt <- latency_positive %>%
      filter(case_number == i) %>%
      pull(mean_rt_PE) 
    
      assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}

#计算PN纠正过的rt
for(i in c(101:132, 201:231, 301:333)) {
    if (i %in% c(119, 210, 311)) next
     dataset_name <- paste0("IAT_", i, "_PN")
    if (exists(dataset_name)) {
    
    mean_rt <- latency_positive %>%
      filter(case_number == i) %>%
      pull(mean_rt_PN) 
    
    assign(dataset_name, get(dataset_name) %>%
             mutate(adj_rt = ifelse(corr == 1, rt, mean_rt + 0.6)))
  }
}


####根据论文 无step8####

#####STEP9#####
#计算NN的调整之后的rt的平均数
latency_negative <- latency_negative %>%
  mutate(mean_adj_NN_re = NA) 

for(i in c(101:132, 201:231, 301:333)) {
    if (i %in% c(119, 210, 311)) next #这几个IAT文件是无效或者缺失的，所以直接跳过
    dataset_name <- paste0("IAT_", i, "_NN") 
  
      if (exists(dataset_name)) {
        # Calculate the mean of adj_rt for the current subject dataset
    mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE)  # 计算adj_rt（上个步骤做出来调整之后的rt）的平均数
    
    latency_negative <- latency_negative %>%
      mutate(mean_adj_NN_re = ifelse(case_number == i, mean_adj_rt, mean_adj_NN_re)) #将当前算出来的mean_adj_rt赋值到所在行中的mean_adj_NN_re
  } #这里chatgpt生成的代码稍微有点冗余，事实上就可以理解为latency_negative$mean_adj_NN_re[latency_negative$case_number=i]<-mean_adj_rt 
}

#按照相同的道路，计算latency
latency_negative <- latency_negative %>%
  mutate(mean_adj_NE_re = NA) 

for(i in c(101:132, 201:231, 301:333)) { 
  if (i %in% c(119, 210, 311)) next
  dataset_name <- paste0("IAT_", i, "_NE") 
      if (exists(dataset_name)) {
      mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE)  
    
    latency_negative <- latency_negative %>%
      mutate(mean_adj_NE_re = ifelse(case_number == i, mean_adj_rt, mean_adj_NE_re))#将当前算出来的mean_adj_rt赋值到所在行中的mean_adj_NE_re 
  }
}


#计算PE的调整过的rt的平均数
latency_positive <- latency_positive %>%
  mutate(mean_adj_PE_re = NA) 

for(i in c(101:132, 201:231, 301:333)) {  
  if (i %in% c(119, 210, 311)) next
    dataset_name <- paste0("IAT_", i, "_PE") 
  if (exists(dataset_name)) {
      mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE) 
        
    latency_positive <- latency_positive %>%
      mutate(mean_adj_PE_re = ifelse(case_number == i, mean_adj_rt, mean_adj_PE_re))#将当前算出来的mean_adj_rt赋值到所在行中的mean_adj_PE_re 
  }
}

#同理，计算PN的调整过的rt的平均数
latency_positive <- latency_positive %>%
  mutate(mean_adj_PN_re = NA) 

for(i in c(101:132, 201:231, 301:333)) { 
  if (i %in% c(119, 210, 311)) next
  
  dataset_name <- paste0("IAT_", i, "_PN") 
  if (exists(dataset_name)) {
      mean_adj_rt <- get(dataset_name) %>%
      pull(adj_rt) %>%
      mean(., na.rm = TRUE) 
      
    latency_positive <- latency_positive %>%
      mutate(mean_adj_PN_re = ifelse(case_number == i, mean_adj_rt, mean_adj_PN_re))#将当前算出来的mean_adj_rt赋值到所在行中的mean_adj_PN_re 
  }
}



####step 10 & 11####(因为我们没有exercise trial，所以我们不需要做step11)

#根据算法的最后一步，把standardized difference（变量名sd_diff）给计算出来，这个指标代表了最终被试对食物和情绪词汇（积极/消极）联想的倾向，在文献中用来代表情绪性进食的倾向）
library(tidyverse)

latency_negative<- latency_negative %>%
  mutate (sd_diff=(mean_adj_NN_re - mean_adj_NE_re)/pooled_sd_negative) 

latency_positive<- latency_positive %>%
  mutate (sd_diff=(mean_adj_PN_re - mean_adj_PE_re)/pooled_sd_positive)

latency_negative <- latency_negative %>%
  rename(ID = case_number)  #这个地方重新命名ID只是为了和另外一个dataset合并数据

latency_positive <- latency_positive %>%
  rename(ID = case_number)


view(latency_negative)
view(latency_positive)


#然后是合并数据，这里代码 略