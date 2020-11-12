library(readxl)
library(dplyr)
library(tidyverse)
survey_df <- read_xlsx(here::here("data/survey-thesis.xlsx"), 
                       sheet = 1
                       ) %>% 
  na.omit() %>% 
  mutate(name = factor(name), plor_order = as.numeric(plor_order))

count(survey_df$name)

survey_df <- survey_df %>% group_by(name, plot_name) %>% 
  slice(1) %>% ungroup() %>% 
  arrange(name, plor_order)

count(survey_df$name)

survey_df <- survey_df[!survey_df$name %in% c("test", "Steph"),]

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
survey_df[survey_df$plot_name == "aut_v34_3.png",]$responses # 0 // 9

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

For Version 3, 25/74

