# libraries
library(stringr)
library(tidyverse)
library(readstata13)
library(caret)

print_08 <- read.dta13("stata/amps-2008-newspaper-magazine-readership-v1.1.dta")
electr_08 <- read.dta13("stata/amps-2008-electronic-media-v1.1.dta")
internet_08 <- read.dta13("stata/amps-2008-cellphone-and-internet-v1.1.dta")
demogrs_08 <- read.dta13("stata/amps-2008-demographics-v1.1.dta")
lsm_08 <- read.dta13("stata/amps-2008-lsm-and-saarf-segmentations-v1.1.dta")
attitudes_08 <- read.dta13("stata/amps-2008-attitudes-v1.1.dta")
lifestage_08 <- read.dta13("stata/amps-2008-lifestages-v1.1.dta")
personal_08 <- read.dta13("stata/amps-2008-personal-v1.1.dta")
# 
save(print_08, electr_08, internet_08, demogrs_08, personal_08, lsm_08, lifestage_08, attitudes_08, file = "input_08.RData")

load("input_08.RData")
# 
print_08_labels <- readLines("stata/amps_2008_print_variable_labels.txt")
electr_08_labels <- readLines("stata/amps_2008_electronic_variable_labels.txt")

## 1st Print (newspapers and magazines) Media Set

## ISSUES
names_issues_print_08 <- str_subset(print_08_labels, 'Number of different issues usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_08 <- str_subset(print_08_labels, 'Number of different issues usually read') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# # fix names and get rid of some and save
# names_newspapers_08_issues <- names_issues_print_08[c(1:48)]
# fix(names_newspapers_08_issues)
# saveRDS(names_newspapers_08_issues, "names_newspapers_08_issues.rds")
names_newspapers_08_issues <- readRDS("names_newspapers_08_issues.rds")

# vector of variables
vars_newspapers_08_issues <- vars_issues_print_08[c(1:48)]
issues_newspapers_08 <- print_08[,vars_newspapers_08_issues]

# Magazines
# # fix names and get rid of some (including MNet guides and save
# names_magazines_08_issues <- names_issues_print_08[c(50:62,64:74,76:79,82:103,105:109,112:145,147:148,150:161, 163:166,168:169)]
# fix(names_magazines_08_issues)
# saveRDS(names_magazines_08_issues, "names_magazines_08_issues.rds")
names_magazines_08_issues <- readRDS("names_magazines_08_issues.rds")

# vector of variables
vars_magazines_08_issues <- vars_issues_print_08[c(50:62,64:74,76:79,82:103,105:109,112:145,147:148,150:161, 163:166,168:169)]
issues_magazines_08 <- print_08[,vars_magazines_08_issues]

# create datasets ...for newspapers and magazines: NB simple the same
newspapers_engagement_08_all <- issues_newspapers_08
names(newspapers_engagement_08_all) <- names_newspapers_08_issues
magazines_engagement_08_all <- issues_magazines_08
names(magazines_engagement_08_all) <- names_magazines_08_issues

newspapers_engagement_08_simple_all <- issues_newspapers_08
names(newspapers_engagement_08_simple_all) <- names_newspapers_08_issues
magazines_engagement_08_simple_all <- issues_magazines_08
names(magazines_engagement_08_simple_all) <- names_magazines_08_issues

# # # replace NAs with zeros
newspapers_engagement_08_all[is.na(newspapers_engagement_08_all)] <- 0
magazines_engagement_08_all[is.na(magazines_engagement_08_all)] <- 0

newspapers_engagement_08_simple_all[is.na(newspapers_engagement_08_simple_all)] <- 0
magazines_engagement_08_simple_all[is.na(magazines_engagement_08_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_08_all, "newspapers_engagement_08_all.rds")
saveRDS(magazines_engagement_08_all, "magazines_engagement_08_all.rds")
saveRDS(newspapers_engagement_08_simple_all, "newspapers_engagement_08_simple_all.rds")
saveRDS(magazines_engagement_08_simple_all, "magazines_engagement_08_simple_all.rds")

# CLEAN UP
# for newspapers: – Included in “Other”: The Weekender, Sondag
# Son Ooskaap to be coded as “Son” in this dataset
names(newspapers_engagement_08_all)[29] <- "Son"
other.news <- as.vector(apply(newspapers_engagement_08_all[,c(35,45)], 1, mean))
newspapers_engagement_08 <- newspapers_engagement_08_all %>%
        mutate(other.news = other.news)
newspapers_engagement_08 <- newspapers_engagement_08[,-c(35,45)]

newspapers_engagement_08_simple <- newspapers_engagement_08 # note in this case they would be the same

# for magazines - dealt with it in vehicle_cleaning project
magazines_engagement_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_08.rds")
magazines_engagement_08_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_08_simple.rds")

# save them in this project
saveRDS(newspapers_engagement_08, "newspapers_engagement_08.rds")
saveRDS(magazines_engagement_08, "magazines_engagement_08.rds")
saveRDS(newspapers_engagement_08_simple, "newspapers_engagement_08_simple.rds")
saveRDS(magazines_engagement_08_simple, "magazines_engagement_08_simple.rds")

magazines_engagement_08 <- readRDS("magazines_engagement_08.rds")
newspapers_engagement_08 <- readRDS("newspapers_engagement_08.rds")
magazines_engagement_08_simple <- readRDS("magazines_engagement_08_simple.rds")
newspapers_engagement_08_simple <- readRDS("newspapers_engagement_08_simple.rds")

## 2nd Electronic Media Set
# RADIO

# use 4 weeks for names vector
names_radio_08_4w <- electr_08_labels %>%
        str_subset('Radio station listened to in the past 4 weeks') %>%
        str_replace('.+\\s-\\s','') %>%
        str_trim()

# # # get rid of tail lines
# # names_radio_08_4w <- names_radio_08_4w[1:100] # get rid of summaries & "unsure" &"none"
# 
# names_radio_08 <- names_radio_08_4w
# # 
# check_radio_10 <- readRDS("names_radio_10.rds")
# # 
# fix(names_radio_08)
# 
# saveRDS(names_radio_08, "names_radio_08.rds")
names_radio_08 <- readRDS('names_radio_08.rds')


# get data...
radio4weeks_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[012]')]
radio4weeks_08 <- radio4weeks_08[,c(1:68,73)] # get rid of tails...

radio7days_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[234]')]
radio7days_08 <- radio7days_08[,c(24:88,92)]   # get rid of tails...

radioYesterday_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[456]')]
radioYesterday_08 <- radioYesterday_08[,c(22:79, 84)]  # get rid of "unsure" and "none" etc..

# creating engagement set:
radio_engagement_08_all <- radio4weeks_08
radio_engagement_08_all[,-c(57,58,67)] <- radio4weeks_08[,-c(57,58,67)] +  radio7days_08
radio_engagement_08_all[,-c(34,39,43,50,57,58,61,66,67,68)] <- radio_engagement_08_all[,-c(34,39,43,50,57,58,61,66,67,68)] +  radioYesterday_08

names(radio_engagement_08_all) <- names_radio_08

saveRDS(radio_engagement_08_all, "radio_engagement_08_all.rds")
radio_engagement_08_all <- readRDS("radio_engagement_08_all.rds")

# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_08 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_08.rds")
# and save in this workspace
saveRDS(radio_engagement_08, "radio_engagement_08.rds")

radio_engagement_08 <- readRDS("radio_engagement_08.rds")

## TV (this year, included specific dstv and toptv channels (will include them))

# check_tv_names_12 <- readRDS("names_tv_12_copy.rds")

names_tv_08 <- c("e tv",
                 "SABC 1",
                 "SABC 2",
                 "SABC 3",
                 "Soweto TV",
                 "DSTV",
                 "Other TV")

saveRDS(names_tv_08, "names_tv_08.rds")
names_tv_08 <- readRDS("names_tv_08.rds")

# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV") 
# also NB for 2008 topTV changed name to Starsat. Will stick to topTV here.


tv4weeks_08 <- electr_08[,c('ca39co14_4', #e TV
                            'ca39co14_7', #SABC 1
                            'ca39co14_8', #SABC 2
                            'ca39co14_9', #SABC 3
                            'ca39co15_1', #Soweto TV
                            'ca39co16_8', # DSTV
                            'ca39co16_7')] #Other TV
                        

# want to isolate only past 7 days...
tv7days_08 <- electr_08[,c('ca39co24_4', #e TV
                           'ca39co24_7', #SABC 1
                           'ca39co24_8', #SABC 2
                           'ca39co24_9', #SABC 3
                           'ca39co25_1', #Soweto TV
                           'ca39co26_8', # DSTV
                           'ca39co26_7')] #Other TV

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_08 <- electr_08[,c('ca39co34_4', #e TV
                               'ca39co34_7', #SABC 1
                               'ca39co34_8', #SABC 2
                               'ca39co34_9', #SABC 3
                               'ca39co35_1', #Soweto TV
                               'ca39co36_8', # DSTV
                               'ca39co36_7')] #Other TV

# combining into a tv engagement dataset (using tv4weeks_08 as basis):

tv_engagement_08 <- tv4weeks_08 + tv7days_08 +tvYesterday_08
names(tv_engagement_08) <- names_tv_08

# #NB could consider adding tv viewing intensity... ie light medium or heavy...
# 
# tv_intense_08 <- electr_08[,c('ca45co60_1',
#                           'ca45co60_2','ca45co60_3')]
# tv_intense_08$ca45co60_2 <- ifelse(tv_intense_08$ca45co60_2 == 1,2,tv_intense_08$ca45co60_2)
# tv_intense_08$ca45co60_3 <- ifelse(tv_intense_08$ca45co60_3 == 1,3,tv_intense_08$ca45co60_3)
# 
# tv_intensity <- rowSums(tv_intense_08)

saveRDS(tv_engagement_08, "tv_engagement_08.rds")

tv_engagement_08 <- readRDS("tv_engagement_08.rds")


## 3rd Internet Media Set

## accessed: sum of 12 months, 4weeks, 7days and yesterday
internet_level1 <- internet_08[,str_detect(names(internet_08), 'ca41co(11)|(12)|(13)|(14)')]

internet_level1 <- data.frame(ifelse(internet_level1 == 2 | is.na(internet_level1), 0, 1))

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

int_search <- internet_08[,c('ca41co18_1',
                             'ca41co18_2',
                             'ca41co19_4')] %>%
        mutate(sum = ca41co18_1 + ca41co18_2 + ca41co19_4)
        
int_search <- ifelse(int_search$sum %in% c(2,3), 1, int_search$sum)

int_social <- internet_08[,c('ca41co18_7',
                             'ca41co18_8')]

int_social <- rowSums(int_social)
int_social <- as.vector(ifelse(int_social != 0, 1, int_social))

int_print <- internet_08[,c('ca41co19_7')]

int_news <- internet_08[,c('ca41co19_8')]

int_tv <- internet_08[,c('ca41co19_6')]

int_radio <- internet_08[,c('ca41co19_5')]

internet_level2 <- data.frame(int_search,
                              int_social,
                              int_print,
                              int_news,
                              int_tv,
                              int_radio)

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_08 <- internet_level2  * internet_level1
internet_engagement_08_simple <- internet_level1

saveRDS(internet_engagement_08, "internet_engagement_08.rds")
saveRDS(internet_engagement_08_simple, "internet_engagement_08_simple.rds")

internet_engagement_08 <- readRDS("internet_engagement_08.rds")
internet_engagement_08_simple <- readRDS("internet_engagement_08_simple.rds")

## create single dataframe for media08, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_08 <- data.frame(cbind(qn = demogrs_08$qn,
                                  rowSums(newspapers_engagement_08),
                                  rowSums(magazines_engagement_08),
                                  rowSums(radio_engagement_08),
                                  rowSums(tv_engagement_08),
                                  rowSums(internet_engagement_08)))
names(media_type_08) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_08 <- media_type_08 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet)) 


media_type_08_simple <- data.frame(cbind(qn = demogrs_08$qn,
                                  rowSums(newspapers_engagement_08_simple),
                                  rowSums(magazines_engagement_08_simple),
                                  rowSums(radio_engagement_08),
                                  rowSums(tv_engagement_08),
                                  internet_engagement_08_simple))
names(media_type_08_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_08_simple <- media_type_08_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))



media_type_08_simple_print <- data.frame(cbind(qn = demogrs_08$qn,
                                         rowSums(newspapers_engagement_08_simple),
                                         rowSums(magazines_engagement_08_simple),
                                         rowSums(radio_engagement_08),
                                         rowSums(tv_engagement_08),
                                         rowSums(internet_engagement_08)))
names(media_type_08_simple_print) <- c("qn",
                                 "newspapers",
                                 "magazines",
                                 "radio",
                                 "tv",
                                 "internet")
media_type_08_simple_print <- media_type_08_simple_print %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))

# Level 2: Vehicles
media_vehicles_08 <- data.frame(cbind(qn = demogrs_08$qn,
                                      newspapers_engagement_08,
                                      magazines_engagement_08,
                                      radio_engagement_08,
                                      tv_engagement_08,
                                      internet_engagement_08))

media_vehicles_08_simple <- data.frame(cbind(qn = demogrs_08$qn,
                                      newspapers_engagement_08_simple,
                                      magazines_engagement_08_simple,
                                      radio_engagement_08,
                                      tv_engagement_08,
                                      internet_eng = internet_engagement_08_simple))

media_vehicles_08_simple_print <- data.frame(cbind(qn = demogrs_08$qn,
                                             newspapers_engagement_08_simple,
                                             magazines_engagement_08_simple,
                                             radio_engagement_08,
                                             tv_engagement_08,
                                             internet_engagement_08))



saveRDS(media_type_08, 'media_type_08.rds')
saveRDS(media_vehicles_08, 'media_vehicles_08.rds')

saveRDS(media_type_08_simple, 'media_type_08_simple.rds')
saveRDS(media_type_08_simple_print, 'media_type_08_simple_print.rds')

saveRDS(media_vehicles_08_simple, 'media_vehicles_08_simple.rds')
saveRDS(media_vehicles_08_simple_print, 'media_vehicles_08_simple_print.rds')

media_type_08.rds <- readRDS('media_type_08.rds')
media_vehicles_08 <- readRDS('media_vehicles_08.rds')
media_type_08_simple <- readRDS('media_type_08_simple.rds')
media_vehicles_08_simple <- readRDS('media_vehicles_08_simple.rds')
media_type_08_simple_print <- readRDS('media_type_08_simple_print.rds')
media_vehicles_08_simple_print <- readRDS('media_vehicles_08_simple_print.rds')

## 4th Demographics Set (see notes for descriptions)

age <- personal_08[,'ca48co34']
age_actual <- personal_08[,'ca48co35']# actual age..note some 999 = refusal or dont know
age_actual[age_actual == 999] <- NA

sex <- demogrs_08[,'ca91co51a']

edu <- demogrs_08[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_08[,'ca91co50']

# # more levels for numeric treatment later on
# hh_inc1 <- personal_08[,'ca49co32'] # 1 - 9
# hh_inc2 <- personal_08[,'ca49co33'] # 0 - 9 == 10 - 19
# hh_inc3 <- personal_08[,'ca49co34'] # 0 - 9 == 20 - 29
# hh_inc4 <- personal_08[,'ca49co35'] # 0 - 1 == 30 - 31
# 
# hh_bind <- cbind.data.frame(hh_inc1,(10 + hh_inc2), (20 + hh_inc3),(30 + hh_inc4) )
# hh_bind[is.na(hh_bind)] <- 0
# hh_inc_alt <- rowSums(hh_bind)

# #ca49co35
# 0	R 30 000 - 39 999	531	 43.7%
# 1	R 40 0000+	683	 56.3%
# 2	No personal income	0	 0.0%
# 3	Refused
# #ca49co34
# 0	R 7 000 - 7 999	890	 9.7%
# 1	R 8 000 - 8 999	1037	 11.3%
# 2	R 9 000 - 9 999	738	 8.0%
# 3	R 10 000 - 10 999	1246	 13.6%
# 4	R 11 000 - 11 999	486	 5.3%
# 5	R 12 000 - 13 999	866	 9.4%
# 6	R 14 000 - 15 999	1023	 11.1%
# 7	R 16 000 - 19 999	1128	 12.3%
# 8	R 20 000 - 24 999	1110	 12.1%
# 9	R 25 000 - 29 999
#
# #ca49co33
# 0	R 1100 - 1199	159	 1.9%
# 1	R 1 200 - 1 299	333	 3.9%
# 2	R 1 400 - 1 599	433	 5.0%
# 3	R 1 600 - 1 999	788	 9.2%
# 4	R 2 000 - 2 499	1012	 11.8%
# 5	R 2 500 - 2 999	851	 9.9%
# 6	R 3 000 - 3 999	1324	 15.4%
# 7	R 4 000 - 4 999	1264	 14.7%
# 8	R 5 000 - 5 999	1317	 15.3%
# 9	R 6 000 - 6 999
#
# #ca49co32
# 1	R 1 - 299	111	 5.3%
# 2	R 300 - 399	88	 4.2%
# 3	R 400 - 499	97	 4.6%
# 4	R 500 - 599	151	 7.2%
# 5	R 600 - 699	136	 6.5%
# 6	R 700 - 799	112	 5.3%
# 7	R 800 - 899	579	 27.5%
# 8	R 900 - 999	429	 20.4%
# 9	R 1 000 - 1 099

race <- demogrs_08[,'ca91co51b']
province <- demogrs_08[,'ca91co56']
metro1 <- demogrs_08[,'ca91co57']
metro2 <- demogrs_08[,'ca91co58'] + 9
metro <- rowSums(cbind(metro1,
                       metro2), na.rm = TRUE)

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal
##13 Welkom/Witbank??

metro <- ifelse(metro == 19, 7, metro) # add soweto back to greater jhb
metro <- ifelse(metro == 13, 12, metro) # vaal
metro <- ifelse(metro == 14, 13, metro) # welkom/witbank
table(metro)

lang <- as.numeric(demogrs_08[,'ca91co75']) # here no need to change 0 to 1...?either?
lifestages <- demogrs_08[,'ca91co77']
mar_status <- personal_08[,'ca48co09']
        
lsm <- lsm_08[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)
lsm_full <- lsm

# lifestyle groups total groups lsm groups 1:10
#Value	Category (changed by 1)
lifestyle <- lsm_08[,'ca52co45'] + 1 # to get rid of zero

# attitudes::
# want:
#1: None 
#2: Now Generation
#3: Nation Builders
#4: Distants_Survivors
#5: Distants_Established
#6: Rooted
#7: Global Citizens
attitudesA <- lsm_08[,'ca56co21'] + 1 # to get rid of zeros
attitudesB <- lsm_08[,'ca56co21_lsm']

attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)

attitudesB <- ifelse(attitudesB == 8, 4, attitudesB)
attitudesB <- ifelse(attitudesB == 9, 5, attitudesB)

attitudesA <- ifelse(attitudesA == 4, 0, attitudesA)
attitudesA <- ifelse(attitudesA == 5 | attitudesA == 6, attitudesA + 1, attitudesA)

attitudes <- attitudesA + attitudesB
table(attitudes) # check


demographics_08 <- data.frame(qn = demogrs_08$qn,
                              pwgt = demogrs_08$pwgt,
                              age,
                              age_actual,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm,
                              lsm_full,
                              lifestyle,
                              attitudes)



#reducing levels of categorical variables and setting factor types for demographics:
# age:
demographics_08$age <- ifelse(demographics_08$age %in% c(1,2), 1, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(3,4), 2, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(5,6), 3, demographics_08$age)
demographics_08$age <- ifelse(demographics_08$age %in% c(7,8), 4, demographics_08$age)
demographics_08$age <- factor(demographics_08$age, ordered = TRUE)

# sex:
demographics_08$sex <- factor(demographics_08$sex, ordered = FALSE)

#edu:
demographics_08$edu <- ifelse(demographics_08$edu %in% c(1,2,3,4), 1, demographics_08$edu)
demographics_08$edu <- ifelse(demographics_08$edu %in% c(5), 2, demographics_08$edu)
demographics_08$edu <- ifelse(demographics_08$edu %in% c(6,7,8), 3, demographics_08$edu)
demographics_08$edu <- factor(demographics_08$edu, ordered = TRUE)

#hh_inc
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(1,2,3,4), 1, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(5,6), 2, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(7), 3, demographics_08$hh_inc)
demographics_08$hh_inc <- ifelse(demographics_08$hh_inc %in% c(8), 4, demographics_08$hh_inc)
demographics_08$hh_inc <- factor(demographics_08$hh_inc, ordered = TRUE)

demographics_08$race <- factor(demographics_08$race, ordered = FALSE)
demographics_08$province <- factor(demographics_08$province, ordered = FALSE)
demographics_08$metro <- factor(demographics_08$metro, ordered = FALSE)
demographics_08$lang <- factor(demographics_08$lang, ordered = FALSE)
demographics_08$lifestages <- factor(demographics_08$lifestages, ordered = FALSE)
demographics_08$mar_status <- factor(demographics_08$mar_status, ordered = FALSE)

# lsm
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(1,2), 1, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(3,4), 2, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(5,6), 3, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(7,8), 4, demographics_08$lsm)
demographics_08$lsm <- ifelse(demographics_08$lsm %in% c(9,10), 5, demographics_08$lsm)
demographics_08$lsm <- factor(demographics_08$lsm, ordered = TRUE)

demographics_08$lifestyle <- factor(demographics_08$lifestyle, ordered = FALSE)
demographics_08$attitudes <- factor(demographics_08$attitudes, ordered = FALSE)

demographics_08$lsm_full <- factor(demographics_08$lsm_full, ordered = TRUE)

saveRDS(demographics_08, "demographics_08.rds")
demographics_08 <- readRDS("demographics_08.rds")

# # read datafiles again (if necessary)
# magazines_engagement_08 <- readRDS("magazines_engagement_08.rds")
# magazines_engagement_08_simple <- readRDS("magazines_engagement_08_simple.rds")
# 
# newspapers_engagement_08 <- readRDS("newspapers_engagement_08.rds")
# newspapers_engagement_08_simple <- readRDS("newspapers_engagement_08_simple.rds")
# 
# radio_engagement_08 <- readRDS("radio_engagement_08.rds")
# tv_engagement_08 <- readRDS("tv_engagement_08.rds")
# internet_engagement_08 <- readRDS("internet_engagement_08.rds")
# internet_engagement_08_simple <- readRDS("internet_engagement_08_simple.rds")
# 
# media_type_08 <- readRDS("media_type_08.rds")
# media_type_08_simple <- readRDS("media_type_08_simple.rds")
# media_vehicles_08 <- readRDS("media_vehicles_08.rds")
# media_vehicles_08_simple <- readRDS("media_vehicles_08_simple.rds")
# 
# demographics_08 <- readRDS("demographics_08.rds")

# #create single datasets
set08 <- demographics_08 %>%
        left_join(media_type_08) %>%
        left_join(media_vehicles_08)

set08_simple <- demographics_08 %>%
        left_join(media_type_08_simple) %>%
        left_join(media_vehicles_08_simple)

set08_simple_print <- demographics_08 %>%
        left_join(media_type_08_simple_print) %>%
        left_join(media_vehicles_08_simple_print)

# get rid of zero variances:
ind_08 <- nearZeroVar(set08[,18:ncol(set08)], saveMetrics = TRUE)
good_set <- set08[,18:ncol(set08)][,!ind_08$zeroVar]
set08 <- data.frame(cbind(set08[,1:17], good_set))

ind_08_simple <- nearZeroVar(set08_simple[,18:ncol(set08_simple)], saveMetrics = TRUE)
good_set_simple <- set08_simple[,18:ncol(set08_simple)][,!ind_08_simple$zeroVar]
set08_simple <- data.frame(cbind(set08_simple[,1:17], good_set_simple))

ind_08_simple_print <- nearZeroVar(set08_simple_print[,18:ncol(set08_simple_print)], saveMetrics = TRUE)
good_set_simple_print <- set08_simple_print[,18:ncol(set08_simple_print)][,!ind_08_simple_print$zeroVar]
set08_simple_print <- data.frame(cbind(set08_simple_print[,1:17], good_set_simple_print))

# save them:
saveRDS(set08, "set08.rds")
saveRDS(set08_simple, "set08_simple.rds")
saveRDS(set08_simple_print, "set08_simple_print.rds")
