# ---
# title: "Young lives Peru data file creation"
# author: "CG Tredoux and ARL Dawes"
# date: "September 14, 2016"
# output: html_document


#  YOUNGER COHORT DATA FILE


## Load some packages -----------------------------------------------------------------------------------------------

library(pacman) #install this if not already present
p_load(tidyverse, haven, magrittr, psych, glmnet, car, lubridate)

# define a function for adding over NA values
add_over_na <- function (anydf, anyvec)
  {
   anyvec <- anydf %>% 
                rowSums(na.rm = T) 
   anydf$anyvec = anyvec
   anyvec <- ifelse(!is.na(anydf[,1:length(anydf)]),anyvec,NA)
   
   # anydf %>% 
   #    mutate(anyvec = ifelse(!any(is.na(.[,1])), 
   #                          anyvec, NA)) 
}



# xz <- select(peru_long.dat,contains("food"))
# xz <- add_over_na(xz,a)



## Data file creation -----------------------------------------------------------------------------------------------

# Load data and do some selection and summing
# load constructed file and make new file for cumulation
peru_recon.dat <- read_spss("data/peru_reconstructed/peru_constructed.sav")
peru_long.dat<- peru_recon.dat

# Get child id and round records and remove OC
peru_long.dat %<>% 
  select(childid, placeid, round, yc, starts_with("inr")) %>% 
  filter(yc == 1)

peru_recon.dat %<>% 
  filter(yc == 1) 


## Select Child descriptors, and trim existing file --------------------------------------------------------------------

# Get age in months 
temp <- peru_recon.dat %>% 
  select(childid, agemon, round, yc)
peru_long.dat <- inner_join(peru_long.dat, temp) %>% 
  mutate(age_months = as.numeric(agemon)) %>% 
  select(-agemon)
rm(temp)



## Select Child social status variables, and trim existing file -------------------------------------------------------------------

#  Get type of research site (rural vs urban), and child's first language
temp <- peru_recon.dat %>% 
  select(childid, round, sex, typesite, chlang)
peru_long.dat <- left_join(peru_long.dat, temp) 

# Get variables concerning parental languages and make indigenous status variable
# with three levels
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pechildlevel5yrold.sav")
# temp %<>% 
#   select(childid = CHILDID, DADLANG, MUMLANG, CARELANG) %>% 
#   mutate(indigenous1 = ifelse(MUMLANG %in% 32:34 | DADLANG%in% 32:34 | CARELANG%in% 32:34,
#                               2,0)) %>%
#   mutate(indigenous2 = ifelse(MUMLANG == 37 | DADLANG== 37 | CARELANG== 37,
#                               2,0)) %>% 
#   mutate(indigenous3 = ifelse(MUMLANG %in% 35:36 | DADLANG%in% 35:36 | CARELANG%in% 35:36,
#                               2,0)) %>%
#   mutate(indigenous4 = ifelse(MUMLANG == 31 | DADLANG== 31 | CARELANG== 31,
#                               1,0)) 
# temp$indigenous <-  rowSums(temp[,5:8],na.rm = T) 
# temp %<>% 
#   mutate(indigenous = ifelse(indigenous == 3, 5, indigenous)) %>% 
#   mutate(indigenous = ifelse(indigenous == 2, 3, indigenous)) %>% 
#   mutate(indigenous = ifelse(indigenous == 5, 2, indigenous)) %>% 
#   mutate(indigenous = ifelse(indigenous == 4, 3, indigenous)) %>% 
#   mutate(indigenous = ifelse(indigenous == 0, NA, indigenous)) %>% 
#   select(childid, indigenous)

temp2 <- peru_recon.dat %>% 
  select(childid, chlang, round)
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pechildlevel5yrold.sav")
temp %<>% 
  select(childid = CHILDID, DADLANG, MUMLANG) %>% 
  mutate(round = 2)
temp <- left_join(temp,temp2) 
temp %<>% 
  filter(round == 2) %>% 
  mutate(indig_spanish = ifelse(chlang == 31 & MUMLANG == 31 & (DADLANG == 31 | is.na(DADLANG)),TRUE, FALSE),
         indig_mixed = ifelse(chlang == 31 & (MUMLANG != 31 | DADLANG != 31),TRUE, FALSE),
         indig_notmixed = ifelse(chlang != 31 & (MUMLANG != 31 | DADLANG != 31),TRUE, FALSE))
temp %<>% 
  mutate(indigenous = case_when(
    .$indig_spanish == TRUE  ~  0,
    .$indig_mixed  == TRUE ~ 1,
    .$indig_notmixed== TRUE  ~ 2
  )
  )
temp %<>% 
  select(childid, indigenous)

peru_long.dat <- left_join(peru_long.dat, temp)

# Note that the coding above is a bit complex... need to look at code
# carefully to see what is going on.It creates a 3 point scale

# We attempt to deduce birthorder from other data collected by YL
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_householdrosterr4.sav")
temp %<>% 
  select(childid = CHILDCODE, RELATER4, MEMAGER4) %>% 
  mutate(childid2 = as.character(childid)) %>% 
  select(-childid)

temp2 <- peru_long.dat %>% 
  filter(round == 4) %>% 
  mutate(childid2 = substr(childid,3,8)) %>% 
  mutate(childid2 = ifelse(substr(childid2, 1, 1) == "0",
                           substr(childid2,2,8),
                           childid2)) 

temp3 <- left_join(temp,temp2) %>% 
  select(childid2, RELATER4, MEMAGER4, age_months) %>% 
  mutate(has_sibs = ifelse(RELATER4 %in% (7:12), 1, 0)) %>% 
  mutate(older_sib = ifelse(has_sibs & MEMAGER4 > age_months%/%12, 1, 0)) %>% 
  group_by(childid2) %>% 
  mutate(bocum = cumsum(older_sib)) %>% 
  summarise(birth_order = max(bocum) + 1) %>% 
  mutate(birth_order = ifelse(birth_order > 6, 6, birth_order)) %>% 
  mutate(childid = childid2) %>% 
  mutate(childid = ifelse(nchar(childid) == 5, paste("PE0",childid, sep = ""),
                          paste("PE",childid, sep = ""))) %>% 
  select(-childid2)

peru_long.dat <- left_join(peru_long.dat, temp3)
rm(temp, temp2, temp3)



## Select caregiver influences---------------------------------------------------------------------------------------------------

# We try to compute caregiveR age, but typically age of both caregivers is reported
# We take minimum, and also only when caregiver is female
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r1/survey_data_r1/peru_r1/pesubsec2householdroster1.sav")
temp %<>% 
  select(childid = CHILDID, RELATE, SEX, care_age = AGE) %>% 
  filter(RELATE == 1 & SEX ==2) %>% 
  group_by(childid) %>% 
  summarise(care_age = min(care_age))
peru_long.dat <- left_join(peru_long.dat, temp)


# Work out age of mother at birth of index child
temp <- peru_long.dat %>%
  filter(round ==1) %>% 
  mutate(mother_age_birth = care_age - age_months%/%12) %>% 
  select(childid, round, mother_age_birth)
peru_long.dat <- left_join(peru_long.dat, temp)


# Get data on education and literacy of caregiver, parents
temp <- peru_recon.dat %>% 
  select(childid, round, caredu, momedu, dadedu, dadlit, momlit) %>% 
  na_if(., 28) %>% na_if(., 99) %>% 
  mutate(momedu = case_when(
    .$momedu == 0  ~  0,
    .$momedu > 0 & .$momedu < 8  ~  1,
    .$momedu > 8 & .$momedu < 13  ~  2,
    .$momedu > 12  ~  3
  )
  )
  
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid","round"))

# Get data on partner status; only seems available at Round 1
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r1/SPSS/survey_data_r1/peru_r1/pechildlevel1yrold.sav")
temp %<>% 
  select(childid = CHILDID, partner = PARTNER) %>% 
  mutate(round = 1, partner = ifelse(partner ==1, 1, 
                    ifelse(!is.na(partner),0,NA))) %>% 
  na_if(., 99)
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# Compute caregiver child burden - number of children below 18 years in household
# Round 1
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r1/SPSS/survey_data_r1/peru_r1/pesubsec2householdroster1.sav")
temp1 <- temp%>% 
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n = n()) %>% 
  mutate(round = 1)  
temp2 <-  temp %>% 
  filter(RELATE %in% c(0, 5, 6, 10, 12)) %>%
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n_children_1 = n()) 
temp3 <- full_join(temp1,temp2)
temp3$n_children_1[is.na(temp3$n_children_1)] <- 0  
temp3 %<>% 
  select(childid,round,n_children_1)
peru_long.dat <- left_join(peru_long.dat, temp3)

# Round 2
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pesubhouseholdmember5.sav" )
temp1 <- temp%>% 
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n = n()) %>% 
  mutate(round = 2)  
temp2 <-  temp %>% 
  filter(RELATE %in% c(0, 5, 6, 10, 12)) %>%
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n_children_2 = n()) 
temp3 <- full_join(temp1,temp2)
temp3$n_children_2[is.na(temp3$n_children_2)] <- 0  
temp3 %<>% 
  select(childid,round,n_children_2)
peru_long.dat <- left_join(peru_long.dat, temp3)

# Round 3
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_householdmemberlevel.sav" )
temp1 <- temp%>% 
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n = n()) %>% 
  mutate(round = 3)  
temp2 <-  temp %>% 
  filter(RELATE %in% c(0, 5, 6, 10, 12)) %>%
  select(childid = CHILDID, RELATE) %>% 
  group_by(childid) %>% 
  summarise(n_children_3 = n()) 
temp3 <- full_join(temp1,temp2)
temp3$n_children_3[is.na(temp3$n_children_3)] <- 0  
temp3 %<>% 
  select(childid,round,n_children_3)
peru_long.dat <- left_join(peru_long.dat, temp3)

# Round 4
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_householdrosterr4.sav" )
temp1 <- temp%>% 
  select(childid = CHILDCODE, RELATER4) %>% 
  group_by(childid) %>% 
  summarise(n = n()) %>% 
  mutate(round = 4)  
temp2 <-  temp %>% 
  filter(RELATER4 %in% c(0, 5, 6, 10, 12)) %>%
  select(childid = CHILDCODE, RELATER4) %>% 
  group_by(childid) %>% 
  summarise(n_children_4 = n()) 
temp3 <- full_join(temp1,temp2)
temp3$n_children_4[is.na(temp3$n_children_4)] <- 0  
temp3 %<>% 
  select(childid,round,n_children_4) %>% 
  mutate(childid = ifelse(nchar(childid) == 5, paste("PE0",childid, sep = ""),
                          paste("PE",childid, sep = "")))

peru_long.dat <- left_join(peru_long.dat, temp3)

peru_long.dat %<>% 
  mutate(numchild_home = ifelse(!is.na(n_children_1),n_children_1,
                                ifelse(!is.na(n_children_2),n_children_2,
                                       ifelse(!is.na(n_children_3),n_children_3,
                                              n_children_4)))) %>% 
select(-contains("n_children"))

peru_long.dat %<>% 
  mutate(numchild_home = ifelse(numchild_home > 6, 6, numchild_home))




## Select Poverty Inequality and Shocks --------------------------------------------------------------------------------------------


# Get information about shocks, and make summed variable

# temp <- peru_recon.dat %>% 
#   select(childid, round, wi, totalexp, starts_with("shecon"), starts_with("shfam"),
#          starts_with("shenv"), shhouse3)
# 
# temp %<>% select (contains("sh")) 
# temp <- temp[apply(temp, 1, function(y) !all(is.na(y))),]

temp <- peru_recon.dat %>% 
  select(childid, round, wi, totalexp, starts_with("shecon"), starts_with("shfam"),
         starts_with("shenv"), shhouse3)
temp$shock <-rowSums(temp[,5:18], na.rm=T)
temp %<>% 
    mutate(shock = ifelse((is.na(shecon1) & is.na(shecon2) & 
                           is.na(shecon3) & is.na(shecon4) & is.na(shecon5) & 
                             is.na(shecon6) & is.na(shecon7) & is.na(shecon8) & 
                             is.na(shecon9) & is.na(shecon10) & is.na(shecon11) & 
                             is.na(shecon12) & is.na(shecon13) & is.na(shecon14) & 
                             is.na(shfam1) & is.na(shfam2) & is.na(shfam3) & is.na(shfam4) & 
                            is.na(shfam5) & is.na(shfam6) & is.na(shfam7) & is.na(shfam8) &
                            is.na(shfam9) & is.na(shfam10) & is.na(shfam11) & is.na(shfam12) &
                            is.na(shfam13) & is.na(shfam14) & is.na(shfam15) & is.na(shfam16) &
                            is.na(shfam17) & is.na(shfam21) & is.na(shenv1) & is.na(shenv2) & 
                            is.na(shenv3) & is.na(shenv4) & is.na(shenv5) & is.na(shenv6) &
                            is.na(shenv7) & is.na(shenv8) & is.na(shenv9) & is.na(shenv10) & 
                            is.na(shenv11) & is.na(shenv12) & is.na(shhouse3)), NA, shock))

peru_long.dat <- inner_join(peru_long.dat, temp)

#  Average wi across rounds1 and 2
temp <- peru_long.dat %>% 
  filter(round == 1 | round == 2) %>% 
  group_by(childid) %>% 
  summarise(wi_early_yrs = mean(wi, na.rm = T)) 

peru_long.dat <- left_join(peru_long.dat, temp, by=("childid"))


# Get information about food shortages, and make summed variable
# NOTE THAT WE HAVE TO DO SOMETHING ABOUT THE ROUND 2 data

# Round 2
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pechildlevel5yrold.sav")
temp %<>% 
  select(childid = CHILDID, fs1 = PE4C26, fs2 = PE4C27, fs3 = PE4C28, fs4 = PE4C29) %>% 
  mutate(round = 2)
temp2 <- temp %>% 
  dplyr::select(starts_with("fs"))
temp$food_insecurity_r2 <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid","round"))


# Round 3
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_householdlevel.sav")
temp %<>% 
  select(childid = CHILDID, WRRYFDR3,NOPREFR3, LIMTVRR3, NOTWNTR3, SMLLMLR3, FEWMLR3, NOFOODR3,
         SLPHNGR3, DAYNGTR3, FRQWRYR3, FRQPRFR3, FRQLMTR3, FRQNWNR3, FRQSMLR3, FRQFEWR3, FRQNOFR3, 
         FRQHNGR3, FRQDAYR3) %>% 
  mutate(fs1 = WRRYFDR3*FRQWRYR3, fs2 = NOPREFR3*FRQPRFR3, fs3 = LIMTVRR3*FRQLMTR3, fs4 = NOTWNTR3*FRQNWNR3,
         fs5 = SMLLMLR3*FRQSMLR3, fs6 = FEWMLR3*FRQFEWR3, fs7 = NOFOODR3*FRQNOFR3, fs8 = SLPHNGR3*FRQHNGR3,
         fs9 = DAYNGTR3*FRQDAYR3, round = 3) %>% 
  na_if(., 77)

temp2 <- temp %>% 
  dplyr::select(starts_with("fs"))

temp$food_insecurity <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))

temp_R3 <- temp %>%  
  select(childid, food_insecurity, round, starts_with("fs"))

# Round 4
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_youngerhousehold.sav")
temp %<>% 
  select(CHILDCODE, WRRYFDR4,NOPREFR4, LIMTVRR4, NOTWNTR4, SMLLMLR4, FEWMLR4, NOFOODR4,
         SLPHNGR4, DAYNGTR4, FRQWRYR4, FRQPRFR4, FRQLMTR4, FRQNWNR4, FRQSMLR4, FRQFEWR4, FRQNOFR4, 
         FRQHNGR4, FRQDAYR4) %>% 
  mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  mutate(fs1 = WRRYFDR4*FRQWRYR4, fs2 = NOPREFR4*FRQPRFR4, fs3 = LIMTVRR4*FRQLMTR4, fs4 = NOTWNTR4*FRQNWNR4,
         fs5 = SMLLMLR4*FRQSMLR4, fs6 = FEWMLR4*FRQFEWR4, fs7 = NOFOODR4*FRQNOFR4, fs8 = SLPHNGR4*FRQHNGR4,
         fs9 = DAYNGTR4*FRQDAYR4, round = 4) %>% 
  na_if(., 77) 

temp2 <- temp %>% 
  dplyr::select(starts_with("fs"))

temp$food_insecurity <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))

temp_R4 <-  temp %>% 
  select(childid, food_insecurity, round, starts_with("fs"))

temp <- rbind(temp_R3,temp_R4)

peru_long.dat <- left_join(peru_long.dat, temp)
rm(temp,temp1,temp2,temp3,temp_R3,temp_R4)

# Compute correlation to see if relationship, there is
# peru_long.dat %>% 
#   group_by(childid) %>% 
#   summarise(food_security = mean(food_security, na.rm = T),
#             food_shortage = mean(food_shortage, na.rm = T)) %$% 
#   cor(food_shortage, food_security, use = "pairwise.complete.obs")

# We need to combine food shortage and food security.  Need to think about perhaps
# summing food security and then 27*food shortage, and averaging down

#  THE FOLLOWING ATTEMPT TO COMPUTE MONTHLY INCOME FAILED
# # This computes/estimates a monthly salary for household
# # Round 2
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pesubhouseholdmember5.sav")
# temp %<>% 
#   select(childid = CHILDID, income_amount = INCAMNT, income_time = as.numeric(INCTIME)) 
# temp$income_time_trans <- (891.91666667 -1306.61091270*temp$income_time +
#                              815.97916667*temp$income_time^2 -279.53483796*temp$income_time^3 +
#                              56.57986111*temp$income_time^4 -6.75879630*temp$income_time^5 +
#                              0.44097222*temp$income_time^6 -0.01211971*temp$income_time^7)
# temp$income <- temp$income_amount*temp$income_time_trans
# temp$round <- 2
# temp %<>% select(round, childid, income) %>% 
#   group_by(childid, round) %>% 
#   summarise(income_monthly_r2 = sum(income, na.rm = T))
# 
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))
# 
# # Round 3
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_stblhhsec3percmonetaryincomes.sav")
# temp %<>% 
#   select(childid = CHILDID, income_amount = INCERNR3, income_time = as.numeric(INCTMER3)) 
# temp$income_time_trans <- (891.91666667 -1306.61091270*temp$income_time +
#                              815.97916667*temp$income_time^2 -279.53483796*temp$income_time^3 +
#                              56.57986111*temp$income_time^4 -6.75879630*temp$income_time^5 +
#                              0.44097222*temp$income_time^6 -0.01211971*temp$income_time^7)
# temp$income <- temp$income_amount*temp$income_time_trans
# temp$round <- 3
# temp %<>% select(round, childid, income) %>% 
#   group_by(childid, round) %>% 
#   summarise(income_monthly_r3 = sum(income, na.rm = T))
# 
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))
# 
# # NOTE: R4 SEEMS LIKE IT HAS VERY SPARSE DATA ON INCOME, BRACKET FOR NOW
# 
# # Round 4
# # temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_householdrosterr4.sav")
# # temp %<>% 
# # select(childid = CHILDCODE, income_amount = INDAMTR4, income_time = as.numeric(INDTIMR4), MNWAGER4) %>% 
# # mutate(childid = ifelse(nchar(childid) == 5, paste("PE0",childid, sep = ""),
# # paste("PE",childid, sep = ""))) %>% 
# # mutate(income_amount = ifelse (income_amount <0, NA, income_amount)) %>% 
# # mutate(income_amount = as.numeric(income_amount)) %>% 
# # mutate(income_time = ifelse (income_time %in% 1:8, income_time, NA))
# # 
# # 
# # 
# # temp$income_time_trans <- (891.91666667 -1306.61091270*temp$income_time +
# # 815.97916667*temp$income_time^2 -279.53483796*temp$income_time^3 +
# # 56.57986111*temp$income_time^4 -6.75879630*temp$income_time^5 +
# # 0.44097222*temp$income_time^6 -0.01211971*temp$income_time^7)
# # temp$income <- temp$income_amount*temp$income_time_trans
# # temp$round <- 4
# # temp %<>% select(round, childid, income) %>% 
# # group_by(childid, round) %>% 
# # summarise(income_monthly_r4 = sum(income, na.rm = T))
# # 
# # peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))
# 
# # Merge rows 
# peru_long.dat %<>% 
#   mutate(income_monthly = ifelse(is.na(income_monthly_r2) & is.na(income_monthly_r3),
#                                  NA, ifelse(!is.na(income_monthly_r2), income_monthly_r2,
#                                             income_monthly_r3))) %>% 
#   select(-income_monthly_r2, -income_monthly_r3)
# rm(temp, temp2)



# Now find and sum mental health of mother / caregiver
# Note that although we seemed to find data for R1 and R2 and R3 in questionnaires etc,
# they are not in the data files 
# THEY SEEM NOT TO HAVE BEEN ARCHIVED
# BUT files obtained from PERU on 28/11/2016

# R1 
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r1/SPSS/survey_data_r1/peru_r1/Caregiver Depression R1.sav")
temp %<>% 
  dplyr::select(childid = CHILDID, HEADACHE:TIRED) %>% 
  mutate(round = 1) %>%
  na_if(., 9)

temp2 <- temp%>% 
      select(HEADACHE:TIRED)  
temp2 <- (2-temp2)

temp$mother_mhealth <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
temp_R1 <- temp
temp_R1 %<>% 
  rename(sdq1 = HEADACHE, sdq2 = POORAPP, sdq3 = SLEEP, sdq4 = FRIGHT, sdq5 = HNDSHAKE, 
         sdq6 = TENSE, sdq7 = DIGESTIN, sdq8 = THINK, sdq9 = UNHAPPY, sdq10 = CRY,
         sdq11 = ENJOY, sdq12 = DECISION, sdq13 = WORK, sdq14 = USEFUL, sdq15 = LOST,
         sdq16 = WORTH, sdq17 = ENDING, sdq18 = ALLTIRED, sdq19 = STOMACH, sdq20 = TIRED) %>% 
  na_if(., 79) 


# R2 
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/Caregiver Depression R2.sav")
temp %<>% 
  dplyr::select(childid = CHILDID, HEADACHE:TIRED) %>% 
  mutate(round = 2) %>% 
  na_if(., 99) %>% 
  na_if(., 88) 

temp2 <- temp%>% 
  select(HEADACHE:TIRED) 
temp$mother_mhealth <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
temp_R2 <- temp
temp_R2 %<>% 
  rename(sdq1 = HEADACHE, sdq2 = POORAPP, sdq3 = SLEEP, sdq4 = FRIGHT, sdq5 = HNDSHAKE, 
         sdq6 = TENSE, sdq7 = DIGESTIN, sdq8 = THINK, sdq9 = UNHAPPY, sdq10 = CRY,
         sdq11 = ENJOY, sdq12 = DECISION, sdq13 = WORK, sdq14 = USEFUL, sdq15 = LOST,
         sdq16 = WORTH, sdq17 = ENDING, sdq18 = ALLTIRED, sdq19 = STOMACH, sdq20 = TIRED) %>% 
  na_if(., 79) 


# R3 
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/survey_data_r3/Caregiver Depression R3.sav")
temp %<>% 
  dplyr::select(childid = CHILDID, MHDACHR3:MESYTRR3) %>% 
  mutate(round = 3) %>% 
  na_if(., 99)%>% 
  na_if(., 88) %>% 
  na_if(., 79) 

temp2 <- temp%>% 
select(MHDACHR3:MESYTRR3) 
temp$mother_mhealth <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
temp_R3 <- temp
temp_R3 %<>% 
rename(sdq1 =MHDACHR3,  sdq2 = MAPPPRR3,  sdq3 = MSLPBDR3,  sdq4 = MFRGHTR3,
       sdq5 = MHDSHKR3,  sdq6 = MFNRVSR3,  sdq7 = MDIGPRR3,  sdq8 = MTRBCRR3,  
       sdq9 = MUNHPYR3,  sdq10 = MCRYMRR3,  sdq11 = MDFENJR3,  sdq12 = MDFDECR3,  
       sdq13 = MWRKSFR3,  sdq14 = MNOPRTR3,  sdq15 = MLSINTR3,  sdq16 = MWRTHLR3,  
       sdq17 = MNOGOR3,  sdq18 = MTIREDR3,  sdq19 = MUNSTMR3,  sdq20 = MESYTRR3) %>% 
  na_if(., 79) 


# R4 
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/Caregiver Depression R4.sav")
temp %<>% 
  dplyr::select(childid = CHILDID, MHDACHR4:MESYTRR4) %>% 
  mutate(round = 4) %>% 
  na_if(., 99)%>% 
  na_if(., 88) %>% 
  na_if(., 79) 


temp2 <- temp%>% 
  select(MHDACHR4:MESYTRR4) 
temp$mother_mhealth <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
temp_R4 <- temp
temp_R4 %<>% 
  rename(sdq1 =MHDACHR4,  sdq2 = MAPPPRR4,  sdq3 = MSLPBDR4,  sdq4 = MFRGHTR4,
         sdq5 = MHDSHKR4,  sdq6 = MFNRVSR4,  sdq7 = MDIGPRR4,  sdq8 = MTRBCRR4,  
         sdq9 = MUNHPYR4,  sdq10 = MCRYMRR4,  sdq11 = MDFENJR4,  sdq12 = MDFDECR4,  
         sdq13 = MWRKSFR4,  sdq14 = MNOPRTR4,  sdq15 = MLSINTR4,  sdq16 = MWRTHLR4,  
         sdq17 = MNOGOR4,  sdq18 = MTIREDR4,  sdq19 = MUNSTMR4,  sdq20 = MESYTRR4)%>% 
  na_if(., 79) 


tempsdq <- rbind(temp_R1,temp_R2,temp_R3,temp_R4)

peru_long.dat <- left_join(peru_long.dat, tempsdq, by = c("childid", "round")) 

rm(list = ls(pattern = "temp")) 

#  Now we want to pull in some data about problem drinking and aggression in the home from 
#  round 1  (all that seems available)

temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r1/SPSS/survey_data_r1/peru_r1/Domestic Violence_R1_YC_Peru.sav")
temp %<>% 
    select(childid = CHILDID, alc_1perwk = TOMA, 
           drink_to_drunk = EMBORR, drunk_hit = EMBPEG) %>% 
    mutate(round = 1, alc_1perwk = ifelse(alc_1perwk==2, 0, ifelse(alc_1perwk==4,1, 
                          ifelse(alc_1perwk==3,2, ifelse(alc_1perwk==1,3,NA))))) %>%
    mutate(round = 1, drink_to_drunk = ifelse(drink_to_drunk==2, 0, ifelse(drink_to_drunk==3,1, 
                                                                 ifelse(drink_to_drunk==1,2,NA)))) %>%
    mutate(round = 1, drunk_hit = ifelse(drunk_hit==2, 0, ifelse(drunk_hit==3,1, 
                                                                         ifelse(drunk_hit==1,2,NA)))) %>%
  
    na_if(., 9) %>% 
    na_if(., 8) 


# Combine these as a sum

temp2 <- temp %>%
                  select(alc_1perwk:drunk_hit)  
temp$home_alcprob <- ifelse(apply(is.na(temp2),1,all),NA,rowSums(temp2,na.rm=TRUE))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round")) 

temp <- peru_long.dat %>% 
    mutate(home_alcprob = ifelse(round != 1, NA, home_alcprob)) %>% 
    mutate(home_alcprob = ntile(home_alcprob, 5)) %>% 
    select(childid, round, home_alcprob)
peru_long.dat <- peru_long.dat %>% 
  select(-home_alcprob)  
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


# #  We pull in the only data we have on early infant discpline from R1
##   DECIDED TO REJECT THIS AFTER LOOKING AT DISTRIBUTION, VERY FEW CASES
# 
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r1/SPSS/survey_data_r1/peru_r1/pechildlevel1yrold.sav")
# temp %<>% 
#   select(childid = CHILDID, DPALM, LSAC, LPELL, LAMEN) %>% 
#   mutate(round = 1)
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))
# 


## Select caregiver aspirations ---------------------------------------------------------------------------------------------------

# Get data on expected earnings for child, and degree of education

# Round 2
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pechildlevel5yrold.sav")
temp %<>% 
  select(childid = CHILDID, exp_age_earn_r2 = EXPEARN) %>% 
  mutate(round = 2, exp_age_earn_r2 = cut(exp_age_earn_r2, breaks = c(1,16, 18,22, 99),
         labels = c("Age_16", "Age_18", "Age_22", "Age_gtr_22")))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# Round 3
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_householdlevel.sav")
temp %<>% 
  select(childid = CHILDID, edu_level_like = GRDLKER3) %>% 
  mutate(round = 3, edu_level_like = cut(edu_level_like, breaks = c(0,11, 14,16, 99),
                                        labels = c("school", "tech", "uni_ug", "uni_pg")))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# Round 4
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_youngerhousehold.sav")
temp %<>% 
  mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  select(childid, exp_age_earn_r4 = ERNMNYR4) %>% 
  na_if(., 88) %>%
  na_if(., 77) %>%
  na_if(., 79) %>% 
  mutate(round = 4, exp_age_earn_r4 = cut(exp_age_earn_r4, breaks = c(1,16, 18,22, 99),
                                          labels = c("Age_16", "Age_18", "Age_22", "Age_gtr_22")))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


#  Now we import the support for learning vars (mostly R3)

# R3
temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_householdlevel.sav")
temp %<>% 
  select(childid = CHILDID, extra_tuition = YCEXTUR3, care_part_school1 = WRKDAYR3, 
         care_part_school2 = MTPRASR3, care_part_school3 = GRPMTGR3, 
         care_part_school4 = INDMTGR3, care_part_school5 = SCHPRTR3, 
         care_part_school6 = FNDRSER3, read_encourage1 = ENREADR3, 
         read_encourage2 = BOOKHMR3, read_encourage3 = READTXR3, 
         read_encourage4 = USEDCTR3, help_homework_r3 = HLPHMWR3) %>% 
         mutate(round = 3) %>%
         mutate(read_encourage1 = 4 - read_encourage1) %>% 
         na_if(., 79) %>% 
         na_if(., 88) %>%  
         mutate(help_homework_r3 = ifelse(help_homework_r3 == 1,0,1)) %>% 
         mutate(care_part_school1 = ifelse(care_part_school1 == 1,1,
                          ifelse(care_part_school1 == 2,NA,0)),
                care_part_school2 = ifelse(care_part_school2 == 1,1,
                          ifelse(care_part_school2 == 2,NA,0)),
                care_part_school3 = ifelse(care_part_school3 == 1,1,
                          ifelse(care_part_school3 == 2,NA,0)),
                care_part_school4 = ifelse(care_part_school4 == 1,1,
                          ifelse(care_part_school4 == 2,NA,0)),
                care_part_school5 = ifelse(care_part_school5 == 1,1,
                          ifelse(care_part_school5 == 2,NA,0)),
                care_part_school6 = ifelse(care_part_school6 == 1,1,
                          ifelse(care_part_school6 == 2,NA,0))) 
temp2 <- temp %>%
  select(care_part_school1:care_part_school6) 
temp$care_part_school <- ifelse(apply(is.na(temp2),1,all),NA,
                                     rowSums(temp2,na.rm=TRUE))
temp2 <- temp %>%
  select(read_encourage1:read_encourage4) 
temp$read_encourage <- ifelse(apply(is.na(temp2),1,all),NA,
                                rowSums(temp2,na.rm=TRUE))
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


# R4
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_householdrosterr4.sav")
# temp %<>%
#   mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
#                           paste("PE",CHILDCODE, sep = ""))) %>% 
#   select(childid, NUMBOKR4, TXTFUNR4) %>% 
#   group_by(childid) %>% 
#   summarise(numbooksr4 = sum(NUMBOKR4, na.rm = T), txtfunr4 = sum(TXTFUNR4, na.rm = T)) %>% 
#   mutate(round = 4)
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_youngerhousehold.sav")
temp %<>%
  mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  select(childid, help_homework_r4 = HLPHMWR4) %>% 
  mutate(round = 4)
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/ch_yc/pe_r4_ycch_youngerchild.sav" )
temp %<>%
  mutate(round = 4, childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  select(childid, round, child_ed_aspiration = CLDSTDR4)
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

#  HOW DO WE COMBINE THE VARIABLES ABOVE?

## Select education environment ------------------------------------------------------------------------------------------

# Type of school (public, private, etc)
temp <- peru_recon.dat %>% 
  select(childid, round, school_type = schtype) %>% 
  mutate(school_type = ifelse(school_type == 1, 1, 0)) %>% 
  na_if(., 99)
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# Caregiver view of school quality
# Round 3
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/survey_data_r3/pe_yc_householdlevel.sav")
temp %<>% 
  mutate(round = 3) %>% 
  select(childid = CHILDID, round, care_school_qual1 = TCHMSSR3, care_school_qual2 = TCHLTER3, 
         care_school_qual3 = TCHINFR3, care_school_qual4 = QLTSCHR3, care_school_qual5 = HPYEDCR3) %>% 
  mutate(care_school_qual3 = 4 - care_school_qual3, care_school_qual4 = 4 - care_school_qual4,
         care_school_qual5 = ifelse(care_school_qual5 == 2, 1, ifelse(care_school_qual5 != 0, 2,care_school_qual5)))

# Round 4
temp2 <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/hh_yc/pe_r4_ychh_householdrosterr4.sav" )
temp2 %<>% 
  mutate(round = 4, childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                                     paste("PE",CHILDCODE, sep = ""))) %>% 
  filter(RELATER4 == 0) %>% 
  select(childid, round, care_school_qual1 = TCMISSR4, care_school_qual2 = TCLEVER4, 
         care_school_qual3 = TCINFDR4, care_school_qual4 = QTYTCHR4,care_school_qual5 = HPYEDCR4) %>%
  mutate(care_school_qual3 = 4 - care_school_qual3, care_school_qual4 = 4 - care_school_qual4,
         care_school_qual5 = ifelse(care_school_qual5 == 2, 1, ifelse(care_school_qual5 != 0, 2,care_school_qual5))) %>% 
  na_if(., 77)
temp3 <- rbind(temp,temp2)

peru_long.dat <- left_join(peru_long.dat, temp3, by = c("childid", "round"))
rm(list = ls(pattern = "temp")) 

#  Language of instruction - copy to all rounds, even though only avail in R3
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/survey_data_r3/pe_yc_householdlevel.sav")
temp %<>% 
  mutate(round = 3) %>% 
  select(childid = CHILDID, round, lang_instruct = LNGINSR3)


temp2 <- left_join(peru_long.dat, temp)
temp2 %<>% 
  mutate(lang_instruct_matches = ifelse(lang_instruct == chlang, 1, ifelse(!is.na(lang_instruct), 0, NA))) %>% 
  select(childid, round, lang_instruct, lang_instruct_matches)
peru_long.dat <- left_join(peru_long.dat, temp2, by = c("childid", "round"))

temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/survey_data_r3/pe_yc_childlevel.sav")
temp %<>% 
  mutate(round = 3) %>% 
  select(childid = CHILDID, round, school_punish1 = TCPHOTHR3,
         school_punish2 = TCPHYUR3) %>% 
  na_if(., 79)
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


## Select child health and well-being --------------------------------------------------

#  Get many vars from overall data file and sum health problems
temp <- peru_recon.dat %>% 
          select(childid, round, health_probs = chhprob, stunted = stunt, 
                 severely_stunted = sstunt, child_ladder = cladder, 
                 house_care = hcare, house_chore = hchore, house_task = htask, 
                 probs_vision = prvision, probs_hear = prhear, probs_head = prhead, 
                 probs_resp = prresp, work_money = hwork)  


temp2 <- temp %>% 
       select(probs_vision:probs_resp)
temp$child_healthprobs <- ifelse(apply(is.na(temp2),1,all),NA,
                                                 rowSums(temp2,na.rm=TRUE))

peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round")) 

# Need to create a stunting var that reflects over time
temp1 <- peru_long.dat %>% 
  filter(round == 1) %>% 
  select(childid, stunt_r1 = stunted, sstunt_r1 = severely_stunted) %>% 
  mutate(sstunt_r1 = sstunt_r1)
temp2 <- peru_long.dat %>% 
  filter(round == 2) %>% 
  select(childid, stunt_r2 = stunted, sstunt_r2 = severely_stunted) %>% 
  mutate(sstunt_r2 = sstunt_r2) 
temp3 <- peru_long.dat %>% 
  filter(round == 3) %>% 
  select(childid, stunt_r3 = stunted, sstunt_r3 = severely_stunted) %>% 
  mutate(sstunt_r3 = sstunt_r3) 
temp4 <- left_join(temp1, temp2)
temp5 <- left_join(temp4, temp3)
temp6 <- temp5 %>% 
  select(-childid)
temp5$stunting_to_r3 <- ifelse(apply(is.na(temp6),1,all),NA,
                                 rowSums(temp6,na.rm=TRUE))
temp5 %<>% 
  select(childid, stunting_to_r3)

peru_long.dat <- left_join(peru_long.dat, temp5, by = c("childid")) 
rm(list = ls(pattern = "temp")) 


# Compute cladder over rounds (average)
temp <- peru_long.dat %>% 
  group_by(childid) %>% 
  summarise(ave_child_ladder = mean(child_ladder, na.rm = T)) 
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid"))

# Hours spent on household chores etc. and then paid hwork
temp<- peru_long.dat %>% 
  select(childid, round, house_care:house_task)
temp2 <- temp %>% 
  select(house_care:house_task)
temp$chore_hours <- ifelse(apply(is.na(temp2),1,all),NA,
                           rowSums(temp2,na.rm=TRUE))
temp %<>% 
  mutate(chore_hours = ifelse(chore_hours > 9, 9, chore_hours))
temp %<>% 
  select(childid, round, chore_hours)
peru_long.dat <- left_join(peru_long.dat, temp)

temp <- peru_long.dat %>% 
  group_by(childid) %>% 
  summarise(ave_work_money = mean(work_money, na.rm = T)) 
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid"))


## Select child education --------------------------------------------------------------

# Preschool programme attendance
# [only in round 2, but we copy to all rounds]
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r2/SPSS/survey_data_r2/peru_r2/pechildlevel5yrold.sav" )
temp %<>%
  select(childid = CHILDID, attended_creche = CRECH) 
peru_long.dat <- left_join(peru_long.dat, temp)

# Age first school enrolment
# [only in round 4, but we extrapolate and copy to all rounds]
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/hh_yc/pe_r4_ychh_householdrosterr4.sav" )
temp %<>%
  mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  select(childid, AGESTR4, RELATER4) %>% 
  filter(RELATER4 == 0) %>% 
  mutate(age_school_enrol = AGESTR4) %>% 
  select(-AGESTR4, -RELATER4)
peru_long.dat <- left_join(peru_long.dat, temp)


# ATTEMPTED TO READ ATTPRER4, BUT NOT ENOUGH DATA TO WARRANT IT
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/hh_yc/pe_r4_ychh_householdrosterr4.sav")
# temp %<>%
#   mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
#                           paste("PE",CHILDCODE, sep = ""))) %>% 
#   filter(RELATER4 == 0) %>% 
#   mutate(round = 4) %>% 
#   select(childid, ATTPRER4, round) 
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# LOOKS LIKE FOR R3 THEY ONLY ASKED THIS Q OF ALL BUT INDEX CHILD
# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_householdmemberlevel.sav")
# temp %<>%
#   filter(RELATE == 0) %>% 
#   select(childid = CHILDID,AGESTR3) %>% 
#   mutate(round = 3)
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


## PPVT
#  Rounds 2 and 3
temp <- read_spss("data/peru_reconstructed/peru_constructed.sav") %>% 
  filter(yc == 1) %>% 
  filter(round != 4) %>% 
  select(childid, round, ppvt_raw = ppvtraw)


# Round 4
temp2 <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/cog_yc/pe_r4_yccog_youngerchild.sav") %>% 
  select(childid = CHILDCODE, ppvt_raw) %>% 
  mutate(round = 4, childid = ifelse(nchar(childid) == 5, paste("PE0",childid, sep = ""),
                          paste("PE",childid, sep = ""))) 
temp3 <- rbind(temp,temp2)

peru_long.dat <- left_join(peru_long.dat, temp3) 

## Maths
temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_childlevel.sav")
temp %<>%
  select(childid = CHILDID, math_co) %>% 
  mutate(round = 3, maths_perco = (math_co/29)*100) %>% 
  select(-math_co)

temp2 <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/cog_yc/pe_r4_yccog_youngerchild.sav" )
temp2 %<>%
  mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
                          paste("PE",CHILDCODE, sep = ""))) %>% 
  select(childid, maths_perco) %>% 
  mutate(round = 4)

temp3 <- rbind(temp, temp2)
peru_long.dat <- left_join(peru_long.dat, temp3, by = c("childid", "round"))
rm(list = ls(pattern = "temp")) 

temp <- peru_recon.dat %>% 
  select(childid, round, read_prob = readprob, write_prob = writeprob, 
         literate, hours_study = hstudy) %>% 
  mutate(hours_study = ifelse(hours_study > 4, 4, hours_study))
# remember to reverse these
peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_childlevel.sav")
# temp %<>%
#   select(childid = CHILDID,TMONINR3, FTRWRKR3) %>% 
#   mutate(round = 3)
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# temp <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r4/SPSS/ch_yc/pe_r4_ycch_youngerchild.sav" )
# temp %<>%
#   mutate(childid = ifelse(nchar(CHILDCODE) == 5, paste("PE0",CHILDCODE, sep = ""),
#                           paste("PE",CHILDCODE, sep = ""))) %>% 
#   select(childid, JOBLKER4, CLDSTDR4) %>% 
#   mutate(round = 4)
# peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/SPSS/survey_data_r3/peru_r3/youngerchild/pe_yc_childlevel.sav")
temp %<>% 
  select(childid = CHILDID, egra_raw = egra_co) %>% 
  mutate(round = 3)

peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))


## Select outcome variables (not yet selected)-----------------------------------------------------------------------------

# Get child enrolment and grade
temp <- peru_recon.dat %>% 
  select(childid, round, enrol, dint) 

peru_long.dat <- left_join(peru_long.dat, temp, by = c("childid", "round"))

# But we have discovered that the R3 chgrade data is wrong, so we
# read all the chgrade data again from the constructed and raw data files

temp1 <- read_spss("E:/Dropbox/contract/Young Lives/data/peru_r3/survey_data_r3/pe_yc_householdlevel.sav")
temp1 %<>% 
  select(childid = CHILDID,chgrade = GRDER309) %>% 
  mutate(round = 3)
temp2 <- read_spss("data/peru_reconstructed/peru_constructed.sav")
temp2 %<>% 
  filter(round == 4) %>% 
  select(childid,chgrade) %>% 
  mutate(round = 4)
temp3 <- rbind(temp1,temp2)

peru_long.dat <- left_join(peru_long.dat, temp3, by = c("childid", "round"))
rm(temp1, temp2, temp3)

# Compute to see if age-appropriate grade enrolment

x <- 1-month(peru_long.dat$dint)/12  # fraction of year from months
y <- peru_long.dat$age_months + x*12  # projected age of child at end of the year in months
y <- round(y/12,0)
peru_long.dat %<>% 
  mutate(yrs_grtr_grade = (y - 6 - chgrade)) %>% 
  mutate(yrs_grtr_grade = ifelse(yrs_grtr_grade < -2 | yrs_grtr_grade > 4, NA, yrs_grtr_grade))


# ## SCHOOL QUALITY --------------------------------------------------------------------------------------
# #  We use the community survey to try and estimate school quality
# #  See Excel sheet for variables
# 
 temp <- read_spss("e:/Dropbox/contract/Young Lives/data/peru_r3/community_data_r3/pe_r3_community_comm_level_r3.sav" )
# temp %<>% 
#   select(placeid = PLACEID, PRITCHSP, PRITCHQU, PRITCHAM,  
#          PRISPKSP, PRISPKQU, PRISPKAM, PRISPKOT, dropout_rate = PRIDROP, 
#          repet_rate = PRIMREP, library = DISBIBPS, pclab = DISLCMPS, 
#          science_lab = DISLCSPS, pc_kidprogram = COMLAPPS, clean_safe_school = PRIESCSG,
#          one_teacher_forall = ONE4ALL, two_teachers = `@2ORMORE`, one_teacher_pergrade = ONEPERGR,
#          psychologist = PSICOLPS, nurse = ENFERMPS) %>% 
#   mutate(school_qual1 = ifelse(dropout_rate < median(dropout_rate, na.rm = T), 1,0),
#          school_qual2 = ifelse(repet_rate < median(repet_rate, na.rm = T), 1,0),
#          school_qual3 = ifelse(library == 1, 1,ifelse(!is.na(library), 0, NA)),
#          school_qual4 = ifelse(science_lab == 1, 1,ifelse(!is.na(science_lab), 0, NA)),
#          school_qual5 = ifelse(pc_kidprogram == 1, 1,ifelse(!is.na(pc_kidprogram), 0, NA)),
#          school_qual6 = ifelse(clean_safe_school == 1, 1,ifelse(!is.na(clean_safe_school), 0, NA)),
#          school_qual7 = ifelse(one_teacher_forall == 1, 1/3,ifelse(!is.na(one_teacher_forall), 0, NA)),  
#          school_qual8 = ifelse(two_teachers == 1, 2/3,ifelse(!is.na(one_teacher_forall), 0, NA)), 
#          school_qual9 = ifelse(one_teacher_pergrade == 1, 3/3,ifelse(!is.na(one_teacher_pergrade), 0, NA))
#          )
# temp2 <- temp %>% 
#   select(school_qual1:school_qual9)
# temp$school_quality <- ifelse(apply(is.na(temp2),1,all),NA,
#                            rowSums(temp2,na.rm=TRUE))
# temp %<>% 
#   select(placeid, contains("school_qual"))
#   
# peru_long.dat <- left_join(peru_long.dat,temp)

#  Create index of structural deprivation
peru_long.dat %<>% 
  mutate(deprived_grouping = ifelse(typesite==2 & indigenous > 0, 
                                    1, 0))



## SAVE FILE, younger cohort only, for easier re-use---------------------------------------------------------------------------------------------
save(peru_long.dat, file = "peru_long_yc.Rdata")

