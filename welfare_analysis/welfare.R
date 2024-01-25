library(Hmisc)

rm(list=ls())
gc()

setwd("/home/jafar/methods/welfare_calc/")

weights96 <- rbind(mdb.get("Data96.mdb", tables = c("R96Data")),
                 mdb.get("Data96.mdb", tables = c("U96Data")))[,c("Address", "weight")] 


data96 <- rbind(mdb.get("Data96.mdb", tables = c("R96P3S01")), # rural consumption good 
                mdb.get("Data96.mdb", tables = c("U96P3S01"))) # urban consumption good 

library(dplyr)
data96 <- left_join(data96, weights96, by="Address")


treated_goods <- c(11531, # vegetble oil
                   11764, # red bin
                   11231, 11232, # automatic chicken, non-automatic chicken
                   11117, 11118, # imported rice 1 and 2
                   11211, 11212, # sheep meat, bull meat 
                   11441, 11442, # automatic egg, non-automatic egg 
                   11521, 11522, 11523, # butter variants
                   11812, #sugar
                   12112) # imported tea



data96 <- data96 %>%    # renaming, removing extra rows and columns, treating NAs as zeros, transforming all quantities to gram
  select(Address, DYCOL01, DYCOL03, DYCOL04, weight) %>% 
  rename(good = DYCOL01, gram = DYCOL03, Kgram = DYCOL04) %>% 
  filter(good %in% treated_goods) %>% 
  rowwise() %>% 
  mutate(gram = if_else(is.na(gram), 0, as.double(gram)),
         Kgram = if_else(is.na(as.double(Kgram)), 0, as.double(Kgram))) %>% 
  mutate(g = 1000*Kgram + gram) %>% 
  ungroup() # NA in row 56091 is ignored


data96sum <- data96 %>% # Transforming Codes to Names
  mutate(good_name = case_when(
    good == 11531 ~ "vegetble oil",
    good == 11764 ~ "Red bean",
    good %in% c(11231, 11232) ~ "Chicken",
    good %in% c(11117, 11118) ~ "Imported rice",
    good %in% c(11211, 11212) ~ "Meat",
    good %in% c(11441, 11442) ~ "Egg",
    good %in% c(11521, 11522, 11523) ~ "Butter",
    good == 11812 ~ "Sugar",
    good == 12112 ~ "Imported tea"
  )) %>% 
  group_by(good_name) %>% 
  summarise(c = as.integer(sum(g*weight)/sum(weight)))




#### Doing the same exercise for the year 97

weights97 <- rbind(mdb.get("Data97.mdb", tables = c("R97Data")),
                 mdb.get("Data97.mdb", tables = c("U97Data")))[,c("Address", "weight")]

data97 <- rbind(mdb.get("Data97.mdb", tables = c("R97P3S01")), # rural consumption good 
                mdb.get("Data97.mdb", tables = c("U97P3S01"))) # urban consumption good 

data97 <- left_join(data97, weights97, by="Address")


data97 <- data97 %>% 
  select(Address, DYCOL01, DYCOL03, DYCOL04, weight) %>% 
  rename(good = DYCOL01, gram = DYCOL03, Kgram = DYCOL04) %>% 
  filter(good %in% treated_goods) %>% 
  rowwise() %>% 
  mutate(gram = if_else(is.na(gram), 0, as.double(gram)),
         Kgram = if_else(is.na(as.double(Kgram)), 0, as.double(Kgram))) %>% 
  mutate(g = 1000*Kgram + gram) %>% 
  ungroup()


data97sum <- data97 %>%
  mutate(good_name = case_when(
    good == 11531 ~ "vegetble oil",
    good == 11764 ~ "Red bean",
    good %in% c(11231, 11232) ~ "Chicken",
    good %in% c(11117, 11118) ~ "Imported rice",
    good %in% c(11211, 11212) ~ "Meat",
    good %in% c(11441, 11442) ~ "Egg",
    good %in% c(11521, 11522, 11523) ~ "Butter",
    good == 11812 ~ "Sugar",
    good == 12112 ~ "Imported tea"
  )) %>% 
  group_by(good_name) %>% 
  summarise(c = as.integer(sum(g*weight)/sum(weight)))


# joining consumption data of the year 96 with 97

consumption_data <- left_join(data96sum, data97sum, by="good_name", suffix = c("_96", "_97"))

welfare_data <- consumption_data %>%   # welfare analysis of counterfactual policies
    mutate(c_change = c_97 - c_96) %>% 
    mutate(WC_actual_r_0.4 = -2 * (1.6 * c_97 - c_96)) %>% 
  mutate(r_0.1 = -2 * (1.9 * c_97 - c_96)) %>%
  mutate(r_0.2 = -2 * (1.8 * c_97 - c_96)) %>%
  mutate(r_0.3 = -2 * (1.7 * c_97 - c_96)) %>%
  mutate(r_0.4 = -2 * (1.6 * c_97 - c_96)) %>%
  mutate(r_0.5 = -2 * (1.5 * c_97 - c_96)) %>%
  mutate(r_0.6 = -2 * (1.4 * c_97 - c_96)) %>%
  mutate(r_0.7 = -2 * (1.3 * c_97 - c_96)) %>%
  mutate(r_0.8 = -2 * (1.2 * c_97 - c_96)) %>%
  mutate(r_0.9 = -2 * (1.1 * c_97 - c_96)) %>%
  mutate(r_1.0 = -2 * (1.0 * c_97 - c_96)) %>%
  mutate(r_1.1 = 2 * (-0.9 * c_97 - c_96)) %>%
  mutate(r_1.2 = 2 * (-0.8 * c_97 - c_96)) %>%
  mutate(r_1.3 = 2 * (-0.7 * c_97 - c_96)) %>%
  mutate(r_1.4 = 2 * (-0.6 * c_97 - c_96)) %>%
  mutate(r_1.5 = 2 * (-0.5 * c_97 - c_96)) %>%
  mutate(r_1.6 = 2 * (-0.4 * c_97 - c_96)) %>% 
  mutate(r_1.7 = 2 * (-0.3 * c_97 - c_96))
  

library(tidyr)



welfare_data_print <- consumption_data %>% 
  mutate(c_change = c_97 - c_96) %>% 
  mutate(welfare_cost = -2 * (1.6 * c_97 - c_96)) 


library(stargazer)

stargazer(welfare_data_print[-3,], summary=FALSE, rownames=FALSE, out="welfare_result.tex") # Egg is removed to keep consistency



avg_welfare <- welfare_data[-3, c(1, 6:15)] %>%   # Egg is removed to keep consistency
  pivot_longer(!good_name, 
               names_to = "ratio", values_to = "welfare cost",
               names_prefix = "r_",
               names_transform = list(ratio = as.numeric, "welfare cost"=as.numeric)) %>% 
  group_by(ratio) %>% 
  summarise(across(!good_name, ~ mean(.x)))
  

png("counter_factual_welfare.png", width = 640, height = 480) # ploting counterfactual welfare costs
welfare_plot <- plot(x=avg_welfare$ratio, y=avg_welfare$"welfare cost",
       type = "o",
       xlab = "ratio",
       ylab = "welfare cost",
       col = "red")
dev.off()

