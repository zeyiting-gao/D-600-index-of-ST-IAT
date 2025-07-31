#合并credamo文件和IAT的最后参数
merged_data_credamo <- data_credamo %>%
  left_join(latency_negative, by = "ID")

merged_data_credamo <- merged_data_credamo %>%
  left_join(latency_positive, by = "ID")


merged_data_credamo$std_diff_negative<-NULL
names(merged_data_credamo)
data<-merged_data_credamo
merged_data_credamo[,c(103:109)]<-NULL
names(data)
merged_data_credamo[]
library(haven)
write_sav(merged_data_credamo, "C:/Users/NHGJ/Desktop/R情绪化进食/merged_data20250211.sav")

#merge the merged data with the eating food data
library(tidyverse)

names(merged_data_credamo)
names(merged_data20250211)
merged_data_credamo1 <- merged_data20250211 %>%
  left_join(Eating_Calories, by = "ID")

merged_data_credamo1$Sex <- ifelse(merged_data_credamo1$Sex == "女", 1, 0)
nrow(merged_data_credamo1)
merged_data_credamo1$Height
class(merged_data_credamo1$Weight)

merged_data_credamo1<-merged_data_credamo1 %>%
  mutate(BMI=(Weight/2)/((Height/100)^2))

merged_data_credamo1$BMI
names(merged_data_credamo1)
library(haven)
write_sav(merged_data_credamo1, "C:/Users/NHGJ/Desktop/R情绪化进食/merged_data20250224.sav")
