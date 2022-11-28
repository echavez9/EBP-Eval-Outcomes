## Erica Chavez Santos
## October 3, 2022, edited 11/16
## HPRC EBP Data Cleaning
## Participant outcomes survey
## outcomes demographics

# clear work space of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
  
}


# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

# Load the other packages, installing as needed.
pacman::p_load(readr, dplyr, tidyr, lubridate, table1)

#set working directory 
# setwd("~/Documents/Git/HPRC EBP")

# read in data
df_outcomes <- read_csv("RemoteEBPParticipant_DATA_2022-11-13_1525.csv") ## 1521 obs. 206 var. (test data 11/13) 

#3 Create a copy of data for testing
df_test <- df_outcomes

table(df_outcomes$redcap_event_name, useNA = "always") 
## N = 496 admin arm 
## N = 277 outreach arm 1
## N = 252 post arm 1
## N = 496 pre arm 1


##filter out pre arm for descriptives
df_outcomes <- df_outcomes %>%
  filter(redcap_event_name == 'pre_arm_1') ## N = 496 ## ADD POST ARM 


##### DESCRIPTIVES #######
## Org ID
table(df_outcomes$org_id, useNA = "always") ## 22 orgs represented, NAs = 202

## Age (q1) # MEAN (STD. DEV)
table(df_outcomes$q1, useNA = "always") # NA, N = 17

## Do you live alone? (q2)
table(df_outcomes$q2, useNA = "always") # NA, N = 9 

## Are you a caregiver? (q3)
table(df_outcomes$q3, useNA = "always") # NA, N = 12

## What is your gender? (q4)
table(df_outcomes$q4, useNA = "always") # NA, N = 8
## no one answered "other gender identity"

## Q5 - Ethnicity
## What is your ethnicity? (q5)
table(df_outcomes$q5, useNA = "always") # NA, N = 48

df_outcomes <- df_outcomes %>%
  mutate(Ethnicity =
           ifelse(q5 == 1, "Hispanic", "Not Hispanic"))

## check variable
table(df_outcomes$Ethnicity, useNA = "always")

## What is your race? (q6)
table(df_outcomes$q6___1, useNA = "always") # n = 344 White/Caucasion
table(df_outcomes$q6___2, useNA = "always") # n = 97 Black/African American
table(df_outcomes$q6___3, useNA = "always") # n = 29 Asian
table(df_outcomes$q6___4, useNA = "always") # n = 2 Native Hawaiian/Pacific Islander
table(df_outcomes$q6___5, useNA = "always") # n = 5 American Indian/Native American
table(df_outcomes$q6___6, useNA = "always") # n = 1 Other
table(df_outcomes$q6_other, useNA = "always") # n = 12 

# create recoded race variable
df_outcomes <- df_outcomes %>%
  mutate(Race =
           case_when(q6___1 == 1 ~ "White/Caucasian",
                     q6___2 == 1 ~ "Black/African American",
                     q6___3 == 1 ~ "Asian",
                     q6___4 == 1 ~ "Native Hawaiian/Pacific Islander",
                     q6___5 == 1 ~ "American Indian/Native American",
                     q6___6 == 1 ~ "Other"))

# test to see if numbers are correct 
table(df_outcomes$Race, useNA = "always")

## Q7 What health chronic conditions are you living with? 
table(df_outcomes$q7___0, useNA = "always") # none, N = 30
table(df_outcomes$q7___1, useNA = "always") # N = 93, Asthma, emphysema, COPD, or chronic bronchitis, N = 16
table(df_outcomes$q7___2, useNA = "always") # N = 51, Arthritis--Rheumatoid (RA)
table(df_outcomes$q7___3, useNA = "always") # N = 136, Arthritis--Osteoarthritis (OA)
table(df_outcomes$q7___4, useNA = "always") # N = 52, Arthritis--Other Arthritis diagnosis
table(df_outcomes$q7___5, useNA = "always") # N = 34, Cancer
table(df_outcomes$q7___6, useNA = "always") # N = 156, Depression, anxiety, PTSD, bipolar or other mental health condition
table(df_outcomes$q7___7, useNA = "always") # N = 214, Diabetes
table(df_outcomes$q7___8, useNA = "always") # N = 88, Heart trouble (e.g. angina, congestive heart failure, or coronary artery disease)
table(df_outcomes$q7___9, useNA = "always") # N = 250, Hypertension or high blood pressure
table(df_outcomes$q7___10, useNA = "always") # N = 2, HIV or AIDS
table(df_outcomes$q7___11, useNA = "always") # N = 43, Kidney problems
table(df_outcomes$q7___12, useNA = "always") # N = 0, Liver Problems (such as cirrhosis)
table(df_outcomes$q7___13, useNA = "always") # N = 5, Stroke or other cerebrovascular disease
table(df_outcomes$q7___14, useNA = "always") # N = 5, Digestive Problems--Crohn's disease
table(df_outcomes$q7___15, useNA = "always") # N = 36, Digestive Problems--Irritable bowel syndrome (IBD)
table(df_outcomes$q7___16, useNA = "always") # N = 3, Digestive Problems--Ulcerative colitis
table(df_outcomes$q7___17, useNA = "always") # N = 67, Digestive Problems--Other

## create a count variable for chronic conditions
df_outcomes <- df_outcomes %>%
  mutate(cc_num = q7___1 + q7___2 + q7___3 + q7___4 +
           q7___5 + q7___6 + q7___7 + q7___8 + q7___9 +
           q7___10 + q7___11 + q7___12 + q7___13 + q7___14 +
           q7___15 + q7___16 + q7___17)

# test to see if numbers are correct 
table(df_outcomes$cc_num, useNA = "always")

## How hard to pay for basic needs? (q8_pay)
table(df_outcomes$q8_pay, useNA = "always")

#table 1 # adding labels next to table1 code otherwise, having issues displaying labels correctly for q2, q3, and q4
label(df_outcomes$q2) <- "Do you live alone?"
label(df_outcomes$q3) <- "Are you a caregver?"
label(df_outcomes$q4) <- "Gender"
label(df_outcomes$q8_pay.factor) <- "How hard to pay for basics"
label(df_outcomes$q9_school.factor) <- "Highest level of education"


table1(~ q2 + q3 + q4 + Ethnicity + Race + cc_num + cc_binary + Rurality + q7_pay.factor + q8_school.factor + q10.factor + q10a + q11.factor, data = df_outcomes)


#table1(~ q_4___1 + q_4___2 + q_4___2 + q_4___3 + q_4___4 + q_4___5 + q_4___6 | remote_mode + survey_type, data = df_all)

table1(~ q1 + q2 + q3 + q4)

###### Summary scales #######

### Loneliness (sum of items: sc1- sc3) ###
# check one of the variables
table(df_outcomes$sc1, useNA = "always") # range 1-3

## create loneliness measure (range is 3-9)
df_outcomes <- df_outcomes %>%
  mutate(loneliness = sc1 + sc2 + sc3)

#check variable
table(df_outcomes$loneliness, useNA = "always")

### Social isolation (sum of items: sc4- sc8) ###
# check one of the variables
table(df_outcomes$sc4, useNA = "always") # range 1-5

## create social isolation measure (range is 3-25)
df_outcomes <- df_outcomes %>%
  mutate(soc_isolation = sc4 + sc5 + sc6 + sc7 + sc8)

#check variable
table(df_outcomes$soc_isolation, useNA = "always")

### Anxiety (sum of items: emo1- emo2)
# check one of the variables
table(df_outcomes$emo1, useNA = "always") # range 0-3

## create anxiety measure (range is 0-6)
df_outcomes <- df_outcomes %>%
  mutate(anxiety = emo1 + emo2)

#check variable
table(df_outcomes$anxiety, useNA = "always")

### Depression (sum of items: emo3- emo10
# check one of the variables
table(df_outcomes$emo3, useNA = "always") # range 0-3

## create depression measure (range is 0-24)
df_outcomes <- df_outcomes %>%
  mutate(depression = emo3 + emo4 + emo5 + emo6 + emo7 + emo8 + emo9 + emo10)

#check variable
table(df_outcomes$depression, useNA = "always")

### Physical activity (Multiple # of days by # of minutes: pa1 and pa2)
# check one of the variables
table(df_outcomes$pa1, useNA = "always") # range 0-7
table(df_outcomes$pa2, useNA = "always") # range 0-7

## recode pa2 to minutes - using the lower range number to be conservative 
## for 1 = less than 15 min, using 1
df_outcomes <- df_outcomes %>%
  mutate(pa2_recoded = 
           case_when(pa2 == 1 ~ "1",
                     pa2 == 2 ~ "15",
                     pa2 == 3 ~ "30",
                     pa2 == 4 ~ "45",
                     pa2 == 5 ~ "60",
                     pa2 == 6 ~ "75",
                     pa2 == 7 ~ "90"))

#check variable
table(df_outcomes$pa2_recoded, useNA = "always")

## make new pa2_recoded variable numeric
df_outcomes$pa2_recoded <- as.numeric(df_outcomes$pa2_recoded)

## create physical activity measure (range is 1-630, so we get days * minutes)
df_outcomes <- df_outcomes %>%
  mutate(phys_activity =
           pa1*pa2_recoded)


#check variable
table(df_outcomes$phys_activity, useNA = "always")


### Social needs (Add up number of responses- including other: resources & resources_other
# check one of the variables
table(df_outcomes$resources___1, useNA = "always") # range 0/1
table(df_outcomes$resources___0, useNA = "always") # range 0/1, not including _0 (I do not need help meeting any basic needs)

## create social needs measure (range is 0-12)
df_outcomes <- df_outcomes %>%
  mutate(social_needs = resources___1 + resources___2 + resources___3 + resources___4 
         + resources___5 + resources___6 + resources___7 + resources___8 + resources___9 
         + resources___10 + resources___11 + resources___12)

## check resources___other
table(df_outcomes$resources_other)

#check variable
table(df_outcomes$social_needs, useNA = "always")

### Self-efficacy (sum of items: cc1-cc6)
# check one of the variables
table(df_outcomes$cc1, useNA = "always") # range 1-10
table(df_outcomes$cc2, useNA = "always") # range 1-10

## create self efficacy measure (range is 6-60)
df_outcomes <- df_outcomes %>%
  mutate(self_efficacy = cc1 + cc2 + cc3 + cc4 + cc5 + cc6 )

#check variable
table(df_outcomes$self_efficacy, useNA = "always")

### Pain interference (sum of items: pain1- pain6)
# check one of the variables
table(df_outcomes$pain1, useNA = "always") # range 1-5
table(df_outcomes$pain2, useNA = "always") # range 1-5

## create pain interference measure (range is 6-30)
df_outcomes <- df_outcomes %>%
  mutate(pain_interference = pain1 + pain2 + pain3 + pain4 + pain5 + pain6)

#check variable
table(df_outcomes$pain_interference, useNA = "always")

### Diabetes symptoms (sum of items: dia1- dia7)
# check one of the variables
table(df_outcomes$dia1, useNA = "always") # range 0-2
table(df_outcomes$dia2, useNA = "always") # range 0-2

## create diabetes measure (range is 0-14)
df_outcomes <- df_outcomes %>%
  mutate(diabetes = dia1 + dia2 + dia3 + dia4 + dia5 + dia6 + dia7)

#check variable
table(df_outcomes$diabetes, useNA = "always")






#### PARKING LOT
## Age - checking #s, n = 5 NAs

table(df_outcomes$q1, useNA = "always")

## Q4 - Sex # 1 = male n =7, 2 = female n = 113, n =3 NA
table(df_outcomes$q4, useNA = "always")

#check other sex/gender as well, NA =123
table(df_outcomes$q4_other, useNA = "always")


## Q5 - Ethnicity

# table(df_outcomes$q5, useNA = "always") # n = 14 Hispanic, n = 106 not Hispanic, & n = 3 NAs

df_outcomes <- df_outcomes %>%
  mutate(Ethnicity =
           ifelse(q5 == 1, "Hispanic", "Not Hispanic"))

# use table(df_outcomes$q5_ethn, useNA = "always") to check #s

## Q6 - Race 
table(df_outcomes$q6___1, useNA = "always") # n = 96 White/Caucasion
table(df_outcomes$q6___2, useNA = "always") # n = 15 Black/African American
table(df_outcomes$q6___3, useNA = "always") # n = 6 Asian
table(df_outcomes$q6___4, useNA = "always") # n = 0 Native Hawaiian/Pacific Islander
table(df_outcomes$q6___5, useNA = "always") # n = 0 American Indian/Native American
table(df_outcomes$q6___6, useNA = "always") # n = 1 Other
table(df_outcomes$q6_other, useNA = "always") # n = 1 Hispanic, n=122 NA/missing, but this person also put Hispanic under ethnicity so it's covered. 

## 1 person answered yes to White/Caucasian and Asian 

# create recoded race variable

df_outcomes <- df_outcomes %>%
  mutate(Race =
           case_when(q6___1 == 1 & q6___3 == 1 ~ "White/Caucasian & Asian",
                     q6___1 == 1 ~ "White/Caucasian",
                     q6___2 == 1 ~ "Black/African American",
                     q6___3 == 1 ~ "Asian",
                     q6___4 == 1 ~ "Native Hawaiian/Pacific Islander",
                     q6___5 == 1 ~ "American Indian/Native American",
                     q6___6 == 1 ~ "Other"))

# test to see if numbers are correct 
table(df_outcomes$Race, useNA = "always")


## Q7 What health chronic conditions are you living with? 

table(df_outcomes$q9___0, useNA = "always") # none, N = 30
table(df_outcomes$q9___1, useNA = "always") # N = 16, Asthma, emphysema, COPD, or chronic bronchitis, N = 16
table(df_outcomes$q9___2, useNA = "always") # N = 6, Arthritis--Rheumatoid (RA)
table(df_outcomes$q9___3, useNA = "always") # N = 26, Arthritis--Osteoarthritis (OA)
table(df_outcomes$q9___4, useNA = "always") # N = 9, Arthritis--Other Arthritis diagnosis
table(df_outcomes$q9___5, useNA = "always") # N = 4, Cancer
table(df_outcomes$q9___6, useNA = "always") # N = 18, Depression, anxiety, PTSD, bipolar or other mental health condition
table(df_outcomes$q9___7, useNA = "always") # N = 13, Diabetes
table(df_outcomes$q9___8, useNA = "always") # N = 2, Heart trouble (e.g. angina, congestive heart failure, or coronary artery disease)
table(df_outcomes$q9___9, useNA = "always") # N = 24, Hypertension or high blood pressure
table(df_outcomes$q9___10, useNA = "always") # N = 0, HIV or AIDS
table(df_outcomes$q9___11, useNA = "always") # N = 3, Kidney problems
table(df_outcomes$q9___12, useNA = "always") # N = 0, Liver Problems (such as cirrhosis)
table(df_outcomes$q9___13, useNA = "always") # N = 2, Stroke or other cerebrovascular disease
table(df_outcomes$q9___14, useNA = "always") # N = 1, Digestive Problems--Crohn's disease
table(df_outcomes$q9___15, useNA = "always") # N = 9, Digestive Problems--Irritable bowel syndrome (IBD)
table(df_outcomes$q9___16, useNA = "always") # N = 1, Digestive Problems--Ulcerative colitis
table(df_outcomes$q9___17, useNA = "always") # N = 10, Digestive Problems--Other

## create a count variable for chronic conditions
df_outcomes <- df_outcomes %>%
  mutate(cc_num = q9___1 + q9___2 + q9___3 + q9___4 +
           q9___5 + q9___6 + q9___7 + q9___8 + q9___9 +
           q9___10 + q9___11 + q9___12 + q9___13 + q9___14 +
           q9___15 + q9___16 + q9___17)

# test to see if numbers are correct 
table(df_outcomes$cc_num, useNA = "always")

## create a 2 or more chronic conditions variable

df_outcomes <- df_outcomes %>%
  mutate(cc_binary =
           ifelse(cc_num >= 2, 1, 0))

# test to see if numbers are correct 
table(df_outcomes$cc_binary, useNA = "always")


## Preliminary table 1

## Q2 - Do you live alone? 

df_outcomes$q2 <- 
  factor(df_outcomes$q2, levels=c(0,1),
         labels=c("No", 
                  "Yes"))


table(df_outcomes$q2, useNA = "always")

## Q3 - Are you a caregiver

df_outcomes$q3 <- 
  factor(df_outcomes$q3, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

# table(df_outcomes$q3, useNA = "always")

# labels

df_outcomes$q4 <- 
  factor(df_outcomes$q4, levels=c(1,2),
         labels=c("Male", 
                  "Female"))


# label chronic condition variables
# cc_num
label(df_outcomes$cc_num) <- "Number of chronic conditions reported"

#cc_binary
df_outcomes$cc_binary <- 
  factor(df_outcomes$cc_binary, levels=c(0, 1),
         labels=c("No", 
                  "Yes"))
label(df_outcomes$cc_binary) <- "Two or more chronic conditions reported"

#table 1 # adding labels next to table1 code otherwise, having issues displaying labels correctly for q2, q3, and q4
label(df_outcomes$q2) <- "Do you live alone?"
label(df_outcomes$q3) <- "Are you a caregver?"
label(df_outcomes$q4) <- "Sex"
label(df_outcomes$q7_pay.factor) <- "How hard to pay for basics"
label(df_outcomes$q8_school.factor) <- "Highest level of education"
label(df_outcomes$q10.factor) <- "Certified health professional"
label(df_outcomes$q10a) <- "Certified health professional- other"
label(df_outcomes$q11.factor) <- "Community health worker, promotora, or other lay health provider"


table1(~ q2 + q3 + q4 + Ethnicity + Race + cc_num + cc_binary + Rurality + q7_pay.factor + q8_school.factor + q10.factor + q10a + q11.factor, data = df_outcomes)
