library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
survey_df <- read_xlsx(here::here("data/survey-thesis.xlsx"), 
                       sheet = 1) %>% 
  mutate(gender = as.factor(gender), education = as.factor(education),
         age = as.factor(age), eco = as.factor(eco), identifier = as.factor(identifier))

count(survey_df$identifier)

survey_df <- survey_df[!survey_df$name %in% c("test", "Steph", "tan", "li  jing"),]

survey_df <- survey_df %>% group_by(identifier, plot_name) %>% 
  slice(1) %>% ungroup() %>% 
  arrange(identifier, plor_order)

# gender
survey_df %>% group_by(gender) %>% dplyr::summarise(n = n()/12)

# age
survey_df %>% group_by(age) %>% dplyr::summarise(n = n()/12)

# education
survey_df %>% group_by(education) %>% dplyr::summarise(n = n()/12)

# eco
survey_df %>% group_by(eco) %>% dplyr::summarise(n = n()/12)

count(survey_df$identifier)

result <- survey_df %>%  group_by(plot_name) %>%  
  # filter(responses == (correct_response[plot_name]))
  dplyr::summarise(n = n(), correct = sum(responses==as.numeric(correct_response[plot_name])))

# Marginal residuals
## V1
result %>% filter(str_detect(plot_name, '1._1')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._1')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._1')) %>% summarise(sum(n), sum(correct))

# Conditional residuals
## V1
result %>% filter(str_detect(plot_name, '1._2')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._2')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._2')) %>% summarise(sum(n), sum(correct))

# Conditional residuals Normality
## V1
result %>% filter(str_detect(plot_name, '1._3')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._3')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._3')) %>% summarise(sum(n), sum(correct))

# Least confounded residual normality
## V1
result %>% filter(str_detect(plot_name, '1._4')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._4')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._4')) %>% summarise(sum(n), sum(correct))

## Aut
result %>% filter(str_detect(plot_name, 'aut_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._4')) %>% summarise(sum(n), sum(correct))

# Lin
result %>% filter(str_detect(plot_name, 'lin_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._4')) %>% summarise(sum(n), sum(correct))

# slp
result %>% filter(str_detect(plot_name, 'slp_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._4')) %>% summarise(sum(n), sum(correct))


correct_response <- c("aut_v11_1.png" = 7, "aut_v12_1.png" = 7, 
                      "aut_v13_1.png" = 13, "aut_v14_1.png" = 13,
                      "lin_v11_1.png" = 7, "lin_v12_1.png" = 7,
                      "lin_v13_1.png" = 13, "lin_v14_1.png" = 13,
                      "slp_v11_1.png" = 7, "slp_v12_1.png" = 7,
                      "slp_v13_1.png" = 13, "slp_v14_1.png" = 13,
                      
                      "aut_v11_2.png" = 7, "aut_v12_2.png" = 7, 
                      "aut_v13_2.png" = 13, "aut_v14_2.png" = 13,
                      "lin_v11_2.png" = 7, "lin_v12_2.png" = 7,
                      "lin_v13_2.png" = 13, "lin_v14_2.png" = 13,
                      "slp_v11_2.png" = 7, "slp_v12_2.png" = 7,
                      "slp_v13_2.png" = 13, "slp_v14_2.png" = 13,
                      
                      "aut_v11_3.png" = 9, "aut_v12_3.png" = 9, 
                      "aut_v13_3.png" = 4, "aut_v14_3.png" = 4,
                      "lin_v11_3.png" = 9, "lin_v12_3.png" = 9,
                      "lin_v13_3.png" = 4, "lin_v14_3.png" = 4,
                      "slp_v11_3.png" = 9, "slp_v12_3.png" = 9,
                      "slp_v13_3.png" = 4, "slp_v14_3.png" = 4,
                      
                      "aut_v11_4.png" = 9, "aut_v12_4.png" = 9, 
                      "aut_v13_4.png" = 4, "aut_v14_4.png" = 4,
                      "lin_v11_4.png" = 9, "lin_v12_4.png" = 9,
                      "lin_v13_4.png" = 4, "lin_v14_4.png" = 4,
                      "slp_v11_4.png" = 9, "slp_v12_4.png" = 9,
                      "slp_v13_4.png" = 4, "slp_v14_4.png" = 4,
                      
                      "aut_v21_1.png" = 5, "aut_v22_1.png" = 5, 
                      "aut_v23_1.png" = 15, "aut_v24_1.png" = 15,
                      "lin_v21_1.png" = 5, "lin_v22_1.png" = 5,
                      "lin_v23_1.png" = 15, "lin_v24_1.png" = 15,
                      "slp_v21_1.png" = 5, "slp_v22_1.png" = 5,
                      "slp_v23_1.png" = 15, "slp_v24_1.png" = 15,
                      
                      "aut_v21_2.png" = 5, "aut_v22_2.png" = 5, 
                      "aut_v23_2.png" = 15, "aut_v24_2.png" = 15,
                      "lin_v21_2.png" = 5, "lin_v22_2.png" = 5,
                      "lin_v23_2.png" = 15, "lin_v24_2.png" = 15,
                      "slp_v21_2.png" = 5, "slp_v22_2.png" = 5,
                      "slp_v23_2.png" = 15, "slp_v24_2.png" = 15,
                      
                      "aut_v21_3.png" = 8, "aut_v22_3.png" = 8, 
                      "aut_v23_3.png" = 20, "aut_v24_3.png" = 20,
                      "lin_v21_3.png" = 8, "lin_v22_3.png" = 8,
                      "lin_v23_3.png" = 20, "lin_v24_3.png" = 20,
                      "slp_v21_3.png" = 8, "slp_v22_3.png" = 8,
                      "slp_v23_3.png" = 20, "slp_v24_3.png" = 20,
                      
                      "aut_v21_4.png" = 8, "aut_v22_4.png" = 8, 
                      "aut_v23_4.png" = 20, "aut_v24_4.png" = 20,
                      "lin_v21_4.png" = 8, "lin_v22_4.png" = 8,
                      "lin_v23_4.png" = 20, "lin_v24_4.png" = 20,
                      "slp_v21_4.png" = 8, "slp_v22_4.png" = 8,
                      "slp_v23_4.png" = 20, "slp_v24_4.png" = 20,
                      
                      "aut_v31_1.png" = 15, "aut_v32_1.png" = 15, 
                      "aut_v33_1.png" = 2, "aut_v34_1.png" = 2,
                      "lin_v31_1.png" = 15, "lin_v32_1.png" = 15,
                      "lin_v33_1.png" = 2, "lin_v34_1.png" = 2,
                      "slp_v31_1.png" = 15, "slp_v32_1.png" = 15,
                      "slp_v33_1.png" = 2, "slp_v34_1.png" = 2,
                      
                      "aut_v31_2.png" = 15, "aut_v32_2.png" = 15, 
                      "aut_v33_2.png" = 2, "aut_v34_2.png" = 2,
                      "lin_v31_2.png" = 15, "lin_v32_2.png" = 15,
                      "lin_v33_2.png" = 2, "lin_v34_2.png" = 2,
                      "slp_v31_2.png" = 15, "slp_v32_2.png" = 15,
                      "slp_v33_2.png" = 2, "slp_v34_2.png" = 2,
                      
                      "aut_v31_3.png" = 11, "aut_v32_3.png" = 11, 
                      "aut_v33_3.png" = 6, "aut_v34_3.png" = 6,
                      "lin_v31_3.png" = 11, "lin_v32_3.png" = 11,
                      "lin_v33_3.png" = 6, "lin_v34_3.png" = 6,
                      "slp_v31_3.png" = 11, "slp_v32_3.png" = 11,
                      "slp_v33_3.png" = 6, "slp_v34_3.png" = 6,
                      
                      "aut_v31_4.png" = 11, "aut_v32_4.png" = 11, 
                      "aut_v33_4.png" = 6, "aut_v34_4.png" = 6,
                      "lin_v31_4.png" = 11, "lin_v32_4.png" = 11,
                      "lin_v33_4.png" = 6, "lin_v34_4.png" = 6,
                      "slp_v31_4.png" = 11, "slp_v32_4.png" = 11,
                      "slp_v33_4.png" = 6, "slp_v34_4.png" = 6)

#V11
survey_df[survey_df$plot_name == "aut_v11_1.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v12_1.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v13_1.png",]$responses # 5
survey_df[survey_df$plot_name == "aut_v14_1.png",]$responses # 2 // 8

survey_df[survey_df$plot_name == "lin_v11_1.png",]$responses # 2 responses
survey_df[survey_df$plot_name == "lin_v12_1.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v13_1.png",]$responses # 3 
survey_df[survey_df$plot_name == "lin_v14_1.png",]$responses # 1 // 7

survey_df[survey_df$plot_name == "slp_v11_1.png",]$responses # 0
survey_df[survey_df$plot_name == "slp_v12_1.png",]$responses # 2 
survey_df[survey_df$plot_name == "slp_v13_1.png",]$responses # 4
survey_df[survey_df$plot_name == "slp_v14_1.png",]$responses # 3 // 9

#V12
survey_df[survey_df$plot_name == "aut_v11_2.png",]$responses # 2
survey_df[survey_df$plot_name == "aut_v12_2.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v13_2.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v14_2.png",]$responses # 3 // 6

survey_df[survey_df$plot_name == "lin_v11_2.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v12_2.png",]$responses # 3 
survey_df[survey_df$plot_name == "lin_v13_2.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v14_2.png",]$responses # 2 // 5

survey_df[survey_df$plot_name == "slp_v11_2.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v12_2.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v13_2.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v14_2.png",]$responses # 1 // 5

#V13
survey_df[survey_df$plot_name == "aut_v11_3.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v12_3.png",]$responses # 2
survey_df[survey_df$plot_name == "aut_v13_3.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v14_3.png",]$responses # 3 // 5

survey_df[survey_df$plot_name == "lin_v11_3.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v12_3.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v13_3.png",]$responses # 4
survey_df[survey_df$plot_name == "lin_v14_3.png",]$responses # 3 // 10

survey_df[survey_df$plot_name == "slp_v11_3.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v12_3.png",]$responses # 0
survey_df[survey_df$plot_name == "slp_v13_3.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v14_3.png",]$responses # 0 // 4

#V14
survey_df[survey_df$plot_name == "aut_v11_4.png",]$responses # 3
survey_df[survey_df$plot_name == "aut_v12_4.png",]$responses # 2
survey_df[survey_df$plot_name == "aut_v13_4.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v14_4.png",]$responses # 2 // 7

survey_df[survey_df$plot_name == "lin_v11_4.png",]$responses # 5
survey_df[survey_df$plot_name == "lin_v12_4.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v13_4.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v14_4.png",]$responses # 0 // 6

survey_df[survey_df$plot_name == "slp_v11_4.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v12_4.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v13_4.png",]$responses # 2 # correct 1
survey_df[survey_df$plot_name == "slp_v14_4.png",]$responses # 2 // 4

# For Version 1, 1/76

#V21
survey_df[survey_df$plot_name == "aut_v21_1.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v22_1.png",]$responses # 3 # correct 1
survey_df[survey_df$plot_name == "aut_v23_1.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v24_1.png",]$responses # 1 // 5

survey_df[survey_df$plot_name == "lin_v21_1.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v22_1.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v23_1.png",]$responses # 2 
survey_df[survey_df$plot_name == "lin_v24_1.png",]$responses # 0 // 4

survey_df[survey_df$plot_name == "slp_v21_1.png",]$responses # 0
survey_df[survey_df$plot_name == "slp_v22_1.png",]$responses # 2 
survey_df[survey_df$plot_name == "slp_v23_1.png",]$responses # 0
survey_df[survey_df$plot_name == "slp_v24_1.png",]$responses # 1 // 3

#V22
survey_df[survey_df$plot_name == "aut_v21_2.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v22_2.png",]$responses # 2
survey_df[survey_df$plot_name == "aut_v23_2.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v24_2.png",]$responses # 2 // 5

survey_df[survey_df$plot_name == "lin_v21_2.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v22_2.png",]$responses # 2 
survey_df[survey_df$plot_name == "lin_v23_2.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v24_2.png",]$responses # 2 // 7

survey_df[survey_df$plot_name == "slp_v21_2.png",]$responses # 2 # correct 1
survey_df[survey_df$plot_name == "slp_v22_2.png",]$responses # 0
survey_df[survey_df$plot_name == "slp_v23_2.png",]$responses # 1 
survey_df[survey_df$plot_name == "slp_v24_2.png",]$responses # 1 // 4

#V23
survey_df[survey_df$plot_name == "aut_v21_3.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v22_3.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v23_3.png",]$responses # 3
survey_df[survey_df$plot_name == "aut_v24_3.png",]$responses # 0 // 3

survey_df[survey_df$plot_name == "lin_v21_3.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v22_3.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v23_3.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v24_3.png",]$responses # 1 // 6

survey_df[survey_df$plot_name == "slp_v21_3.png",]$responses # 3
survey_df[survey_df$plot_name == "slp_v22_3.png",]$responses # 3
survey_df[survey_df$plot_name == "slp_v23_3.png",]$responses # 2 
survey_df[survey_df$plot_name == "slp_v24_3.png",]$responses # 1 // 9

#V24
survey_df[survey_df$plot_name == "aut_v21_4.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v22_4.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v23_4.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v24_4.png",]$responses # 0 // 2

survey_df[survey_df$plot_name == "lin_v21_4.png",]$responses # 3
survey_df[survey_df$plot_name == "lin_v22_4.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v23_4.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v24_4.png",]$responses # 3 # correct 1 // 7

survey_df[survey_df$plot_name == "slp_v21_4.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v22_4.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v23_4.png",]$responses # 1 
survey_df[survey_df$plot_name == "slp_v24_4.png",]$responses # 1 // 4

# For Version2, 3/59

#V31
survey_df[survey_df$plot_name == "aut_v31_1.png",]$responses # 1
survey_df[survey_df$plot_name == "aut_v32_1.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v33_1.png",]$responses # 3
survey_df[survey_df$plot_name == "aut_v34_1.png",]$responses # 1 // 5

survey_df[survey_df$plot_name == "lin_v31_1.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v32_1.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v33_1.png",]$responses # 1 
survey_df[survey_df$plot_name == "lin_v34_1.png",]$responses # 2 // 4

survey_df[survey_df$plot_name == "slp_v31_1.png",]$responses # 2 # correct 1
survey_df[survey_df$plot_name == "slp_v32_1.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v33_1.png",]$responses # 1
survey_df[survey_df$plot_name == "slp_v34_1.png",]$responses # 2 // 6

#V32
survey_df[survey_df$plot_name == "aut_v31_2.png",]$responses # 1 # correct 1
survey_df[survey_df$plot_name == "aut_v32_2.png",]$responses # 3 # correct 3
survey_df[survey_df$plot_name == "aut_v33_2.png",]$responses # 2 # correct 2
survey_df[survey_df$plot_name == "aut_v34_2.png",]$responses # 1 // 7 # correct 1

survey_df[survey_df$plot_name == "lin_v31_2.png",]$responses # 2
survey_df[survey_df$plot_name == "lin_v32_2.png",]$responses # 1 
survey_df[survey_df$plot_name == "lin_v33_2.png",]$responses # 2 # correct 2
survey_df[survey_df$plot_name == "lin_v34_2.png",]$responses # 1 // 6 # correct 1

survey_df[survey_df$plot_name == "slp_v31_2.png",]$responses # 2 
survey_df[survey_df$plot_name == "slp_v32_2.png",]$responses # 3
survey_df[survey_df$plot_name == "slp_v33_2.png",]$responses # 3 
survey_df[survey_df$plot_name == "slp_v34_2.png",]$responses # 1 // 9

#V33
survey_df[survey_df$plot_name == "aut_v31_3.png",]$responses # 4 # all c
survey_df[survey_df$plot_name == "aut_v32_3.png",]$responses # 4 # all C
survey_df[survey_df$plot_name == "aut_v33_3.png",]$responses # 2
survey_df[survey_df$plot_name == "aut_v34_3.png",]$responses # 0 // 10

survey_df[survey_df$plot_name == "lin_v31_3.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v32_3.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v33_3.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v34_3.png",]$responses # 2 // 2

survey_df[survey_df$plot_name == "slp_v31_3.png",]$responses # 1 # AC
survey_df[survey_df$plot_name == "slp_v32_3.png",]$responses # 1 # AC
survey_df[survey_df$plot_name == "slp_v33_3.png",]$responses # 0 
survey_df[survey_df$plot_name == "slp_v34_3.png",]$responses # 3 // 5 # C2

#V34
survey_df[survey_df$plot_name == "aut_v31_4.png",]$responses # 5
survey_df[survey_df$plot_name == "aut_v32_4.png",]$responses # 0
survey_df[survey_df$plot_name == "aut_v33_4.png",]$responses # 1 #AC
survey_df[survey_df$plot_name == "aut_v34_4.png",]$responses # 3 // 9 #C1

survey_df[survey_df$plot_name == "lin_v31_4.png",]$responses # 0
survey_df[survey_df$plot_name == "lin_v32_4.png",]$responses # 3
survey_df[survey_df$plot_name == "lin_v33_4.png",]$responses # 1
survey_df[survey_df$plot_name == "lin_v34_4.png",]$responses # 1 // 5

survey_df[survey_df$plot_name == "slp_v31_4.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v32_4.png",]$responses # 2
survey_df[survey_df$plot_name == "slp_v33_4.png",]$responses # 1 
survey_df[survey_df$plot_name == "slp_v34_4.png",]$responses # 2 // 7

#For Version 3, 25/74

