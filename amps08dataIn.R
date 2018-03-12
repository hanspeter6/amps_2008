# libraries
library(stringr)
library(tidyverse)
library(readstata13)

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
# internet_08_labels <- readLines("stata/amps_2008_internet_variable_labels.txt")
# demogrs_08_labels <- readLines("stata/amps_2008_demographics_variable_labels.txt")
# personal_08_labels <- readLines("stata/amps_2008_personal_variable_labels.txt")
# lsm_08_labels <- readLines("stata/amps_2008_lsm_variable_labels.txt")
# lifestage_08_labels <- readLines("stata/amps_2008_lifestages_variable_labels.txt")
# attitudes_08_labels <- readLines("stata/amps_2008_attitudes_variable_labels.txt")
# 
# 
# # 
# save(print_08_labels, electr_08_labels, internet_08_labels, demogrs_08_labels, personal_08_labels, lsm_08_labels, lifestage_08_labels, attitudes_08_labels, file = "labels_08.RData")
# # 
# load("labels_08.RData")

## 1st Print (newspapers and magazines) Media Set

names_print_08 <- str_subset(print_08_labels, 'Number of different issues usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()

check_2012 <- readRDS("names_print_12_copy.rds")
# 
# ind_correct_12 <- which(check_2012 %in% names_print_08)
# 
# diff_12 <- check_2012[-ind_correct_12]
# 
# ind_correct_08 <- which(names_print_08 %in% check_2012)
# 
# diff_08 <- names_print_08[-ind_correct_08]
# 
# fix(diff_08)
# 
# names_print_08[-ind_correct_08] <- diff_08


# fix(names_print_08)

saveRDS(names_print_08, "names_print_08.rds")

names_print_08 <- readRDS("names_print_08.rds")

# names_dailies_08 <- names_print_08[1:22]
# names_biweeklies_08 <- names_print_08[23]
# names_weeklies_08 <- names_print_08[24:52]

# # NBNB: Not community papers in 2008...
# names_community_cape_town <- names_print[40:51]
# names_community_restCape <- names_print[52:61]
# names_community_FreeState <- names_print[62:66]
# names_community_NWest <- names_print[67:68]
# names_community_Jhb <- names_print[69]
# names_community_ERand <- names_print[70:71]
# names_community_KZn <- names_print[72:74]
# 
# names_mags_weekly_08 <- names_print_08[53:65]
# names_fortnightly_mags_08 <- names_print_08[66:67]
# names_monthly_news_08 <- names_print_08[68:69]
# names_monthly_mags_08 <- names_print_08[70:087]
# 
# names_alt_monthly_08 <- names_print_08[150:161]
# names_quarterly_mags_08 <- names_print_08[164:168]
# 
# names_monthly_store_mags_08 <- names_print_08[088:089]
# names_alt_month_store_mags_08 <- names_print_08[162:163]
# names_quarterly_store_mags_08 <- names_print_08[169:172]

# create print dataset:
issues_08 <- print_08[,str_detect(names(print_08), 'ca[345678]co\\d{2}')]

saveRDS(issues_08, "issues_08.rds")
# 
# thorough_08 <- print_08[,str_detect(names(print_08), 'ca((34)|(35)|(36)|(37)|(38)|(39))co\\d{2}')]
# 
# # one extra in thrrough.. Elle Decorations
# 
# thorough_08 <- thorough_08[,-which(names(thorough_08) == 'ca38co50')] # get rid of elle decorations
# 
# names(thorough_08) <- names_print_08
# 
# # # need to reverse numbering to serve as weights (see value_lables text file):
# thorough_08 <- 7 - thorough_08
# 
# saveRDS(thorough_08, "thorough_08.rds")
# # create single print dataset:

print_engagement_08 <- issues_08

# replace nas with zero's:
print_engagement_08[is.na(print_engagement_08)] <- 0
names(print_engagement_08) <- names_print_08

saveRDS(print_engagement_08, "print_engagement_08.rds")

print_engagement_08 <- readRDS("print_engagement_08.rds")

newspapers_engagement_08 <- print_engagement_08[,c(1:48)]
magazines_engagement_08 <- print_engagement_08[,c(49:170)]

saveRDS(newspapers_engagement_08, "newspapers_engagement_08.rds")
saveRDS(magazines_engagement_08, "magazines_engagement_08.rds")

magazines_engagement_08 <- readRDS("magazines_engagement_08.rds")
newspapers_engagement_08 <- readRDS("newspapers_engagement_08.rds")



#radio, tv... other.
# etc...


## 2nd Electronic Media Set
# RADIO

# use 4 weeks for names vector
names_radio_08_4w <- electr_08_labels %>%
        str_subset('Radio station listened to in the past 4 weeks') %>%
        str_replace('.+\\s-\\s','') %>%
        str_trim()
# # get rid of tail lines
# names_radio_08_4w <- names_radio_08_4w[1:100] # get rid of summaries & "unsure" &"none"

names_radio_08 <- names_radio_08_4w
# 
check_radio_10 <- readRDS("names_radio_10.rds")
# 
fix(names_radio_08)

saveRDS(names_radio_08, "names_radio_08.rds")
names_radio_08 <- readRDS('names_radio_08.rds')


# 
# names_radio_08_7 <- electr_08_labels %>%
#         str_subset('ca65co\\d{2}_\\d') %>%
#         str_replace('.+s\\s-\\s','') %>%
#         str_trim()
# 
# names_radio_08_7 <- names_radio_08_7[1:91] # get rid of "unsure" and "none" etc
# # # 
# names_radio_08_y <- electr_08_labels %>%
#         str_subset('ca66co\\d{2}_\\d') %>%
#         str_replace('.+listened\\sto\\syesterday\\s-\\s','')
# names_radio_08_y <- names_radio_08_y[-c(64,65)] # get rid of "unsure" and "none"
# 
# 
# 

# get data...
radio4weeks_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[012]')]
radio4weeks_08 <- radio4weeks_08[,c(1:68,73)] # get rid of tails...

radio7days_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[234]')]
radio7days_08 <- radio7days_08[,c(24:88,92)]   # get rid of tails...

radioYesterday_08 <- electr_08[,str_detect(names(electr_08), 'ca40co[456]')]
radioYesterday_08 <- radioYesterday_08[,c(22:79, 84)]  # get rid of "unsure" and "none" etc..

# # identifying missing stations by changing all to "64"
# a <- names(radio4weeks_08)
# b <- names(radio7days_08)
# c <- names(radioYesterday_08)
# b_adj <- b %>%
#         str_replace("65", "64")
# c_adj <- c %>%
#         str_replace("66", "64")
# 
# names(radio7days_08) <- b_adj
# names(radioYesterday_08) <- c_adj
# 
# ind_7 <- which(names(radio4weeks_08) %in% names(radio7days_08))
# ind_y <- which(names(radio4weeks_08) %in% names(radioYesterday_08))
# 
# # adding up
# radio4weeks_08[,ind_7] <- radio4weeks_08[,ind_7] + radio7days_08
# radio4weeks_08[,ind_y] <- radio4weeks_08[,ind_y] + radioYesterday_08



# creating engagement set:
radio_engagement_08 <- radio4weeks_08
radio_engagement_08[,-c(57,58,67)] <- radio4weeks_08[,-c(57,58,67)] +  radio7days_08
radio_engagement_08[,-c(34,39,43,50,57,58,61,66,67,68)] <- radio_engagement_08[,-c(34,39,43,50,57,58,61,66,67,68)] +  radioYesterday_08

names(radio_engagement_08) <- names_radio_08

saveRDS(radio_engagement_08, "radio_engagement_08.rds")
radio_engagement_08 <- readRDS("radio_engagement_08.rds")

## TV (this year, included specific dstv and toptv channels (will include them))

check_tv_names_12 <- readRDS("names_tv_12_copy.rds")

names_tv_08 <- c("e TV",
                 "MNet Main",
                 # "MNet CSN",
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
                            'ca39co14_5', #MNet Main
                            # 'ca39co14_6', # MNet CSN
                            'ca39co14_7', #SABC 1
                            'ca39co14_8', #SABC 2
                            'ca39co14_9', #SABC 3
                            'ca39co15_1', #Soweto TV
                            'ca39co16_8', # DSTV
                            'ca39co16_7')] #Other TV
                        

# want to isolate only past 7 days...
tv7days_08 <- electr_08[,c('ca39co24_4', #e TV
                           'ca39co24_5', #MNet Main
                           # 'ca39co24_6', # MNet CSN
                           'ca39co24_7', #SABC 1
                           'ca39co24_8', #SABC 2
                           'ca39co24_9', #SABC 3
                           'ca39co25_1', #Soweto TV
                           'ca39co26_8', # DSTV
                           'ca39co26_7')] #Other TV

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_08 <- electr_08[,c('ca39co34_4', #e TV
                               'ca39co34_5', #MNet Main
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
                                  scale(rowSums(newspapers_engagement_08)),
                                  scale(rowSums(magazines_engagement_08)),
                                  scale(rowSums(radio_engagement_08)),
                                  scale(rowSums(tv_engagement_08)),
                                  scale(rowSums(internet_engagement_08))))
names(media_type_08) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")

media_type_08_simple <- data.frame(cbind(qn = demogrs_08$qn,
                                  scale(rowSums(newspapers_engagement_08)),
                                  scale(rowSums(magazines_engagement_08)),
                                  scale(rowSums(radio_engagement_08)),
                                  scale(rowSums(tv_engagement_08)),
                                  scale(internet_engagement_08_simple)))
names(media_type_08) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
# Level 2: Vehicles
media_vehicles_08 <- data.frame(cbind(qn = demogrs_08$qn,
                                      newspapers_engagement_08,
                                      magazines_engagement_08,
                                      radio_engagement_08,
                                      tv_engagement_08,
                                      internet_engagement_08))

media_vehicles_08_simple <- data.frame(cbind(qn = demogrs_08$qn,
                                      newspapers_engagement_08,
                                      magazines_engagement_08,
                                      radio_engagement_08,
                                      tv_engagement_08,
                                      internet_engagement_08_simple))

saveRDS(media_type_08, 'media_type_08.rds')
saveRDS(media_vehicles_08, 'media_vehicles_08.rds')
saveRDS(media_type_08_simple, 'media_type_08_simple.rds')
saveRDS(media_vehicles_08_simple, 'media_vehicles_08_simple.rds')

media_type_08.rds <- readRDS('media_type_08.rds')
media_vehicles_08 <- readRDS('media_vehicles_08.rds')
media_type_08_simple.rds <- readRDS('media_type_08_simple.rds')
media_vehicles_08_simple <- readRDS('media_vehicles_08_simple.rds')

## 4th Demographics Set (see notes for descriptions)

age <- personal_08[,'ca48co34']
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
# pers_inc1 <- personal_08[,'ca57co61']
# pers_inc2 <- personal_08[,'ca57co62'] + 10
# pers_inc3 <- personal_08[,'ca57co63'] + 20
# pers_inc4 <- personal_08[,'ca57co64'] + 30
# for(i in 1: length(pers_inc4)) {
#         if(!is.na(pers_inc4[i])) {
#                 if(pers_inc4[i] == 31) {
#                         pers_inc4[i] <- 0
#                 }
#                 if(pers_inc4[i] == 32) {
#                         pers_inc4[i] <- 60
#                 }
#         }
# }
# pers_inc <- rowSums(cbind(pers_inc1,
#                           pers_inc2,
#                           pers_inc3,
#                           pers_inc4), na.rm = TRUE)
lsm <- lsm_08[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)


# lifestyle groups total groups lsm groups 1:10
#Value	Category (changed by 1)
#0	None
#1	Cell Sophisticates
#2	Outdoors
#3	Avid Readers
#4	Sports
#5	Traditionals
#6	Gamers
#7	Studious
#8	Showgoers
# lifestyle <- lsm_08[,'ca52co45']# + 1 # to get rid of zero

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
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              # pers_inc,
                              lsm,
                              # lifestyle,
                              attitudes)


# save as

saveRDS(demographics_08, "demographics_08.rds")
demographics_08 <- readRDS("demographics_08.rds")
