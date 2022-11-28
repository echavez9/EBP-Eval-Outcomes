## Erica Chavez Santos
## October 3, 2022
## HPRC EBP Data Cleaning
## Participant outcomes survey
## Leader demographics

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
df_outcomes <- read_csv("RemoteEBPParticipant_DATA_2022-10-03_1539.csv")

#3 Create a copy of data for testing
df_test <- df_outcomes

## Zip codes ##

##INSERT THIS CODE FROM OTHER FILES


## Age - checking #s, n = 5 NAs

table(df_leader$q1, useNA = "always")

## Q4 - Sex # 1 = male n =7, 2 = female n = 113, n =3 NA
table(df_leader$q4, useNA = "always")

#check other sex/gender as well, NA =123
table(df_leader$q4_other, useNA = "always")


## Q5 - Ethnicity

# table(df_leader$q5, useNA = "always") # n = 14 Hispanic, n = 106 not Hispanic, & n = 3 NAs

df_leader <- df_leader %>%
  mutate(Ethnicity =
           ifelse(q5 == 1, "Hispanic", "Not Hispanic"))

# use table(df_leader$q5_ethn, useNA = "always") to check #s

## Q6 - Race 
table(df_leader$q6___1, useNA = "always") # n = 96 White/Caucasion
table(df_leader$q6___2, useNA = "always") # n = 15 Black/African American
table(df_leader$q6___3, useNA = "always") # n = 6 Asian
table(df_leader$q6___4, useNA = "always") # n = 0 Native Hawaiian/Pacific Islander
table(df_leader$q6___5, useNA = "always") # n = 0 American Indian/Native American
table(df_leader$q6___6, useNA = "always") # n = 1 Other
table(df_leader$q6_other, useNA = "always") # n = 1 Hispanic, n=122 NA/missing, but this person also put Hispanic under ethnicity so it's covered. 

## 1 person answered yes to White/Caucasian and Asian 

# create recoded race variable

df_leader <- df_leader %>%
  mutate(Race =
           case_when(q6___1 == 1 & q6___3 == 1 ~ "White/Caucasian & Asian",
                     q6___1 == 1 ~ "White/Caucasian",
                     q6___2 == 1 ~ "Black/African American",
                     q6___3 == 1 ~ "Asian",
                     q6___4 == 1 ~ "Native Hawaiian/Pacific Islander",
                     q6___5 == 1 ~ "American Indian/Native American",
                     q6___6 == 1 ~ "Other"))

# test to see if numbers are correct 
table(df_leader$Race, useNA = "always")


## Q9 What health chronic conditions are you living with? 

table(df_leader$q9___0, useNA = "always") # none, N = 30
table(df_leader$q9___1, useNA = "always") # N = 16, Asthma, emphysema, COPD, or chronic bronchitis, N = 16
table(df_leader$q9___2, useNA = "always") # N = 6, Arthritis--Rheumatoid (RA)
table(df_leader$q9___3, useNA = "always") # N = 26, Arthritis--Osteoarthritis (OA)
table(df_leader$q9___4, useNA = "always") # N = 9, Arthritis--Other Arthritis diagnosis
table(df_leader$q9___5, useNA = "always") # N = 4, Cancer
table(df_leader$q9___6, useNA = "always") # N = 18, Depression, anxiety, PTSD, bipolar or other mental health condition
table(df_leader$q9___7, useNA = "always") # N = 13, Diabetes
table(df_leader$q9___8, useNA = "always") # N = 2, Heart trouble (e.g. angina, congestive heart failure, or coronary artery disease)
table(df_leader$q9___9, useNA = "always") # N = 24, Hypertension or high blood pressure
table(df_leader$q9___10, useNA = "always") # N = 0, HIV or AIDS
table(df_leader$q9___11, useNA = "always") # N = 3, Kidney problems
table(df_leader$q9___12, useNA = "always") # N = 0, Liver Problems (such as cirrhosis)
table(df_leader$q9___13, useNA = "always") # N = 2, Stroke or other cerebrovascular disease
table(df_leader$q9___14, useNA = "always") # N = 1, Digestive Problems--Crohn's disease
table(df_leader$q9___15, useNA = "always") # N = 9, Digestive Problems--Irritable bowel syndrome (IBD)
table(df_leader$q9___16, useNA = "always") # N = 1, Digestive Problems--Ulcerative colitis
table(df_leader$q9___17, useNA = "always") # N = 10, Digestive Problems--Other

## create a count variable for chronic conditions
df_leader <- df_leader %>%
  mutate(cc_num = q9___1 + q9___2 + q9___3 + q9___4 +
           q9___5 + q9___6 + q9___7 + q9___8 + q9___9 +
           q9___10 + q9___11 + q9___12 + q9___13 + q9___14 +
           q9___15 + q9___16 + q9___17)

# test to see if numbers are correct 
table(df_leader$cc_num, useNA = "always")

## create a 2 or more chronic conditions variable

df_leader <- df_leader %>%
  mutate(cc_binary =
           ifelse(cc_num >= 2, 1, 0))

# test to see if numbers are correct 
table(df_leader$cc_binary, useNA = "always")


## Preliminary table 1

## Q2 - Do you live alone? 

df_leader$q2 <- 
  factor(df_leader$q2, levels=c(0,1),
         labels=c("No", 
                  "Yes"))


table(df_leader$q2, useNA = "always")

## Q3 - Are you a caregiver

df_leader$q3 <- 
  factor(df_leader$q3, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

# table(df_leader$q3, useNA = "always")

# labels

df_leader$q4 <- 
  factor(df_leader$q4, levels=c(1,2),
         labels=c("Male", 
                  "Female"))


# label chronic condition variables
# cc_num
label(df_leader$cc_num) <- "Number of chronic conditions reported"

#cc_binary
df_leader$cc_binary <- 
  factor(df_leader$cc_binary, levels=c(0, 1),
         labels=c("No", 
                  "Yes"))
label(df_leader$cc_binary) <- "Two or more chronic conditions reported"

#table 1 # adding labels next to table1 code otherwise, having issues displaying labels correctly for q2, q3, and q4
label(df_leader$q2) <- "Do you live alone?"
label(df_leader$q3) <- "Are you a caregver?"
label(df_leader$q4) <- "Sex"
label(df_leader$q7_pay.factor) <- "How hard to pay for basics"
label(df_leader$q8_school.factor) <- "Highest level of education"
label(df_leader$q10.factor) <- "Certified health professional"
label(df_leader$q10a) <- "Certified health professional- other"
label(df_leader$q11.factor) <- "Community health worker, promotora, or other lay health provider"


table1(~ q2 + q3 + q4 + Ethnicity + Race + cc_num + cc_binary + Rurality + q7_pay.factor + q8_school.factor + q10.factor + q10a + q11.factor, data = df_leader)

# Q7. How hard is it for you to pay for the very basics like food, housing, heating, medical care, and medications?
table(df_leader$q7_pay.factor, useNA= "always")

# Q8. What is the highest level of education that you have completed?
table(df_leader$q8_school.factor, useNA = "always")

# Q10. Are you a certified health professional?
table(df_leader$q10a, useNA = "always")

# Q11. Are you a community health worker, promotora, or other lay health provider?
table(df_leader$q11.factor, useNA = "always")

# Program 1 - Active Living Every Day (ALED), No (N = 123)
table(df_leader$prog1___1, useNA = "always")

# Program 2 -	AEA Arthritis Foundation Exercise Program, No (N = 119), Yes (N = 4)
table(df_leader$prog1___2, useNA = "always")

# Program 3 - Bingocize, No (N = 116), Yes (N = 7)
table(df_leader$prog1___3, useNA = "always")

# Program 4 - Cancer: Thriving and Surviving, No (N = 115), Yes (N = 8)
table(df_leader$prog1___4, useNA = "always")

# Program 5 - CAPABLE, No (N = 123)
table(df_leader$prog1___5, useNA = "always")

# Program 6 - Chronic Disease Self-Management Program (CDSMP), No (N = 67), Yes (N = 56)
table(df_leader$prog1___6, useNA = "always")

# Program 7 -	Chronic Pain Self-Management Program (CPSMP), No (N = 86), Yes (N = 37)
table(df_leader$prog1___7, useNA = "always")

# Program 8 - Diabetes Self-Management Program (DSMP), No (N = 83), Yes (N = 40)
table(df_leader$prog1___8, useNA = "always")

# Program 9 - EnhanceWellness, No (N = 119), Yes (N = 4)
table(df_leader$prog1___9, useNA = "always")

# Program 10 - FallScape & FallsTall, No (N = 123)
table(df_leader$prog1___10, useNA = "always")

# Program 11 - GeriFit, No (N = 122), Yes (N = 1)
table(df_leader$prog1___11, useNA = "always")

# Program 12 - Healthy IDEAS, No (N = 123)
table(df_leader$prog1___12, useNA = "always")

# Program 13 - HomeMeds, No (N = 117), Yes (N = 6)
table(df_leader$prog1___13, useNA = "always")

# Program 14 - National Diabetes Prevention Program (DPP), No (N = 111), Yes (N = 12)
table(df_leader$prog1___14, useNA = "always")

# Program 15 - On The Move, No (N = 123)
table(df_leader$prog1___15, useNA = "always")

# Program 16 - Online Better Choices, Better Health given by Canary Health, No (N = 123)
table(df_leader$prog1___16, useNA = "always")

# Program 17 - Otago, No (N = 123)
table(df_leader$prog1___17, useNA = "always")

# Program 18 - PEARLS (Program to Encourage Active, Rewarding Lives), No (N = 107), Yes (N = 16)
table(df_leader$prog1___18, useNA = "always")

# Program 19 - Positive Self-Management for HIV (PSMP), No (N = 122), Yes (N = 1)
table(df_leader$prog1___19, useNA = "always")

# Program 20 - Powerful Tools for Caregivers, No (N = 115), Yes (N = 8)
table(df_leader$prog1___20, useNA = "always")

# Program 21 - Programa de Manejo Personal de la Diabetes (Spanish Diabetes Self- Management Program), No (N = 121), Yes (N = 2)
table(df_leader$prog1___21, useNA = "always")

# Program 22 - Stay Active and Independent for Life (SAIL), No (N = 121), Yes (N = 2)
table(df_leader$prog1___22, useNA = "always")

# Program 23 - Stress Busting for Family Caregivers, No (N = 123)
table(df_leader$prog1___23, useNA = "always")

# Program 24 - Tai Chi for Arthritis and Falls Prevention, No (N = 113), Yes (N = 10)
table(df_leader$prog1___24, useNA = "always")

# Program 25 - Tai Ji Quan: Moving for Better Balance, No (N = 119), Yes (N = 4)
table(df_leader$prog1___25, useNA = "always")

# Program 26 - 	Tomando Control de su Salud (Spanish Chronic Disease Self-Management Program), No (N = 118), Yes (N = 5)
table(df_leader$prog1___26, useNA = "always")

# Program 27 - Walk With Ease, No (N = 108), Yes (N = 15)
table(df_leader$prog1___27, useNA = "always")

# Program 28 - Wellness Recovery Action Plan, No (N = 123)
table(df_leader$prog1___28, useNA = "always")

# Program 29 - Workplace Chronic Disease Self-Management Program (WCDSMP), No (N = 122), Yes (N = 1)
table(df_leader$prog1___29, useNA = "always")

# Program 30 - YMCA Moving for Better Balance, No (N = 123)
table(df_leader$prog1___30, useNA = "always")

# Program 31 - 	A Matter of Balance, No (N = 86), Yes (N = 37)
table(df_leader$prog1___31, useNA = "always")

# Program 32 - Care Transitions, No (N = 123)
table(df_leader$prog1___32, useNA = "always")

# Program 33 - Eat Smart, Move More, Weigh Less, No (N = 123)
table(df_leader$prog1___33, useNA = "always")

# Program 34 - EnhanceFitness, No (N = 113), Yes (N = 10)
table(df_leader$prog1___34, useNA = "always")

# Program 35 - Fit and Strong, No (N = 122), Yes (N = 1)
table(df_leader$prog1___35, useNA = "always")

# Program 36 - Health Coaches for Hypertension Control, No (N = 117), Yes (N = 6)
table(df_leader$prog1___36, useNA = "always")

# Program 37 - Health Matters, No (N = 123)
table(df_leader$prog1___37, useNA = "always")

# Program 38 - Mind over Matter, No (N = 23
table(df_leader$prog1___38, useNA = "always")

# Program 39 - ¡Sí, Yo Puedo Controlar Mí Diabetes!, No (N = 123)
table(df_leader$prog1___39, useNA = "always")

# Program 40 - REACH, No (N = 123)
table(df_leader$prog1___40, useNA = "always")

# Program 41 - Respecting Choices, No (N = 123)
table(df_leader$prog1___41, useNA = "always")

# Program 42 - Stepping On, No (N = 121, Yes (N = 2)
table(df_leader$prog1___42, useNA = "always")

# Program 0 - Other , No (N = 112), Yes (N = 11)
table(df_leader$prog1___0, useNA = "always")


label(df_leader$prog1___0.factor) <- "Other"
label(df_leader$prog1___1.factor) <- "Active Living Every Day (ALED)"
label(df_leader$prog1___2.factor) <- "AEA Arthritis Foundation Exercise Program"
label(df_leader$prog1___3.factor) <- "Bingocize"
label(df_leader$prog1___4.factor) <- "Cancer: Thriving and Surviving"
label(df_leader$prog1___5.factor) <- "CAPABLE"
label(df_leader$prog1___6.factor) <- "Chronic Disease Self-Management Program (CDSMP)"
label(df_leader$prog1___7.factor) <- "Chronic Pain Self-Management Program (CPSMP)"
label(df_leader$prog1___8.factor) <- "Diabetes Self-Management Program (DSMP)"
label(df_leader$prog1___9.factor) <- "EnhanceWellness"
label(df_leader$prog1___10.factor) <- "FallScape & FallsTall"
label(df_leader$prog1___11.factor) <- "GeriFit"
label(df_leader$prog1___12.factor) <- "Healthy IDEAS"
label(df_leader$prog1___13.factor) <- "HomeMeds"
label(df_leader$prog1___14.factor) <- "National Diabetes Prevention Program (DPP)"
label(df_leader$prog1___15.factor) <- "On The Move"
label(df_leader$prog1___16.factor) <- "Online Better Choices, Better Health given by Canary Health"
label(df_leader$prog1___17.factor) <- "Otago"
label(df_leader$prog1___18.factor) <- "PEARLS (Program to Encourage Active, Rewarding Lives)"
label(df_leader$prog1___19.factor) <- "Positive Self-Management for HIV (PSMP)"
label(df_leader$prog1___20.factor) <- "Program 20 - Powerful Tools for Caregivers"
label(df_leader$prog1___21.factor) <- "Programa de Manejo Personal de la Diabetes (Spanish Diabetes Self- Management Program)"
label(df_leader$prog1___22.factor) <- "Stay Active and Independent for Life (SAIL)"
label(df_leader$prog1___23.factor) <- "Stress Busting for Family Caregivers"
label(df_leader$prog1___24.factor) <- "Tai Chi for Arthritis and Falls Prevention"
label(df_leader$prog1___25.factor) <- "Tai Ji Quan: Moving for Better Balance"
label(df_leader$prog1___26.factor) <- "Tomando Control de su Salud (Spanish Chronic Disease Self-Management Program)"
label(df_leader$prog1___27.factor) <- "Walk With Ease"
label(df_leader$prog1___28.factor) <- "Wellness Recovery Action Plan"
label(df_leader$prog1___29.factor) <- "Workplace Chronic Disease Self-Management Program (WCDSMP)"
label(df_leader$prog1___30.factor) <- "YMCA Moving for Better Balance"
label(df_leader$prog1___31.factor) <- "A Matter of Balance"
label(df_leader$prog1___32.factor) <- "Care Transitions"
label(df_leader$prog1___33.factor) <- "Eat Smart, Move More, Weigh Less"
label(df_leader$prog1___34.factor) <- "EnhanceFitness"
label(df_leader$prog1___35.factor) <- "Fit and Strong"
label(df_leader$prog1___36.factor) <- "Health Coaches for Hypertension Control"
label(df_leader$prog1___37.factor) <- "Health Matters"
label(df_leader$prog1___38.factor) <- "Mind over Matter"
label(df_leader$prog1___39.factor) <- "¡Sí, Yo Puedo Controlar Mí Diabetes!"
label(df_leader$prog1___40.factor) <- "REACH"
label(df_leader$prog1___41.factor) <- "Respecting Choices"
label(df_leader$prog1___42.factor) <- "Stepping On"

## Table with all program counts 
table1(~ prog1___1.factor + prog1___2.factor + prog1___3.factor + prog1___4.factor +
         prog1___5.factor + prog1___6.factor + prog1___7.factor + prog1___8.factor + prog1___9.factor +
         prog1___10.factor + prog1___11.factor + prog1___12.factor + prog1___13.factor + prog1___14.factor +
         prog1___15.factor + prog1___16.factor + prog1___17.factor + prog1___18.factor + prog1___19.factor +
         prog1___20.factor + prog1___21.factor + prog1___22.factor + prog1___23.factor + prog1___24.factor +
         prog1___25.factor + prog1___26.factor + prog1___27.factor + prog1___28.factor + prog1___29.factor +
         prog1___30.factor + prog1___31.factor + prog1___32.factor + prog1___33.factor + prog1___34.factor +
         prog1___35.factor + prog1___36.factor + prog1___37.factor + prog1___38.factor + prog1___39.factor +
         prog1___40.factor + prog1___41.factor + prog1___42.factor + prog1___0.factor, data = df_leader)

## Table w/ programs that have >1 count
table1(~ prog1___2.factor + prog1___3.factor + prog1___4.factor + prog1___6.factor + 
         prog1___7.factor + prog1___8.factor + prog1___9.factor + prog1___11.factor + 
         prog1___13.factor + prog1___14.factor + prog1___18.factor + prog1___19.factor +
         prog1___20.factor + prog1___21.factor + prog1___22.factor + prog1___24.factor +
         prog1___25.factor + prog1___26.factor + prog1___27.factor + prog1___29.factor +
         prog1___30.factor + prog1___31.factor + prog1___34.factor + prog1___35.factor + 
         prog1___36.factor + prog1___42.factor + prog1___0.factor, data = df_leader)


## Count of how many programs each org did

df_leader <- df_leader %>% 
  mutate(program_count= 
           prog1___2 + prog1___3 + prog1___4 + prog1___6 + 
           prog1___7 + prog1___8 + prog1___9 + prog1___11 + 
           prog1___13 + prog1___14+ prog1___18 + prog1___19 +
           prog1___20 + prog1___21 + prog1___22 + prog1___24 +
           prog1___25 + prog1___26 + prog1___27 + prog1___29 +
           prog1___30 + prog1___31 + prog1___34 + prog1___35 + 
           prog1___36 + prog1___42 + prog1___0)
table(df_leader$program_count)

table1(~ program_count, data = df_leader)

# copy of data set w/ labels
write.csv(df_leader,"step 2. recoded_data.csv", row.names = FALSE)


