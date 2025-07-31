#根据算法的最后一步，把standardized difference给计算出来，这个指标代表了最终被试对食物和情绪词汇（积极/消极）联想的倾向，用来代表情绪饮食）
library(tidyverse)
view(latency_negative)
view(latency_positive)

latency_negative<- latency_negative %>%
   mutate (sd_diff=(mean_adj_NN_re - mean_adj_NE_re)/pooled_sd_negative)

latency_positive<- latency_positive %>%
  mutate (sd_diff=(mean_adj_PN_re - mean_adj_PE_re)/pooled_sd_positive)

latency_negative <- latency_negative %>%
  rename(ID = case_number)

latency_positive <- latency_positive %>%
  rename(ID = case_number)
