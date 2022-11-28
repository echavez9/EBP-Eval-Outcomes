#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('RemoteEBPParticipant_DATA_2022-11-13_1525.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_event_name)="Event Name"
label(data$redcap_data_access_group)="Data Access Group"
label(data$demographics_timestamp)="Survey Timestamp"
label(data$screen1)="Who is completing this survey?"
label(data$name_staff)="Staff name"
label(data$name_part)="Participant name"
label(data$phone_part)="Participant phone number"
label(data$email)="Email address"
label(data$date)="Todays date"
label(data$zip)="Participant ZIP code"
label(data$org_id)="Organization ID:"
label(data$org_notes)="Notes about linking participants to the org providing their program: (optional)"
label(data$q1)="1.  What is your age?"
label(data$q2)="2.  Do you live alone?"
label(data$q3)="3. Are you a caregiver?"
label(data$q4)="4. What is your gender?"
label(data$q4_other)="Please describe your other gender identity"
label(data$q5)="5.  What is your ethnicity?"
label(data$q6___1)="6.  What is your race? (choice=White/Caucasian)"
label(data$q6___2)="6.  What is your race? (choice=Black/African American)"
label(data$q6___3)="6.  What is your race? (choice=Asian)"
label(data$q6___4)="6.  What is your race? (choice=Native Hawaiian/Pacific Islander)"
label(data$q6___5)="6.  What is your race? (choice=American Indian/Native American)"
label(data$q6___6)="6.  What is your race? (choice=Other {q6_other})"
label(data$q6_other)="Please describe your other race"
label(data$q7___0)="7.  What health (chronic) conditions are you living with? (choice=I have no chronic conditions)"
label(data$q7___1)="7.  What health (chronic) conditions are you living with? (choice=Asthma, emphysema, COPD, or chronic bronchitis)"
label(data$q7___2)="7.  What health (chronic) conditions are you living with? (choice=Arthritis--Rheumatoid (RA))"
label(data$q7___3)="7.  What health (chronic) conditions are you living with? (choice=Arthritis--Osteoarthritis (OA))"
label(data$q7___4)="7.  What health (chronic) conditions are you living with? (choice=Arthritis--Other Arthritis diagnosis)"
label(data$q7___5)="7.  What health (chronic) conditions are you living with? (choice=Cancer)"
label(data$q7___6)="7.  What health (chronic) conditions are you living with? (choice=Depression, anxiety, PTSD, bipolar or other mental health condition)"
label(data$q7___7)="7.  What health (chronic) conditions are you living with? (choice=Diabetes)"
label(data$q7___8)="7.  What health (chronic) conditions are you living with? (choice=Heart trouble (e.g. angina, congestive heart failure, or coronary artery disease))"
label(data$q7___9)="7.  What health (chronic) conditions are you living with? (choice=Hypertension or high blood pressure)"
label(data$q7___10)="7.  What health (chronic) conditions are you living with? (choice=HIV or AIDS)"
label(data$q7___11)="7.  What health (chronic) conditions are you living with? (choice=Kidney problems)"
label(data$q7___12)="7.  What health (chronic) conditions are you living with? (choice=Liver Problems (such as cirrhosis))"
label(data$q7___13)="7.  What health (chronic) conditions are you living with? (choice=Stroke or other cerebrovascular disease)"
label(data$q7___14)="7.  What health (chronic) conditions are you living with? (choice=Crohns disease)"
label(data$q7___15)="7.  What health (chronic) conditions are you living with? (choice=Irritable bowel syndrome (IBD))"
label(data$q7___16)="7.  What health (chronic) conditions are you living with? (choice=Ulcerative colitis)"
label(data$q7___17)="7.  What health (chronic) conditions are you living with? (choice=Other digestive problems)"
label(data$q8_pay)="8. How hard is it for you to pay for the very basics like food, housing, heating, medical care, and medications? "
label(data$q9_school)="9. What is the highest level of education that you have completed?"
label(data$q10)="10.  Which program are you currently taking?"
label(data$q10_other)="10a. What other program are you taking?"
label(data$q11)="11. How is [q10] being delivered?"
label(data$q11_other)="What other way is your program being delivered?"
label(data$demographics_complete)="Complete?"
label(data$program_survey_timestamp)="Survey Timestamp"
label(data$completed_prog)="Did you complete the program ([pre_arm_1][q10])?"
label(data$health)="HEALTH In general, would you say your health is:"
label(data$fatigue)="On a scale from 1 to 10 please select the number that describes your fatigue or tiredness in the past week: (Where 1=No Fatigue and 10=Severe Fatigue)"
label(data$pain)="On a scale from 1 - 10 please select the number that describes your level of pain in the past week: (Where 1=No Pain and 10=Severe Pain)"
label(data$sleep)="On a scale from 1 - 10 please select the number that describes your SLEEP QUALITY in the past week: (Where 1=No Trouble Sleeping and 10=Extreme Trouble Sleeping)"
label(data$sc1)="1.	That you lack companionship?"
label(data$sc2)="2.	Left out?"
label(data$sc3)="3.	Isolated from others?"
label(data$sc4)="4. See family, friends or neighbors in-person?  This can include in-person visits with social distancing."
label(data$sc5)="5. Talk with family, friends or neighbors by phone or video chat?"
label(data$sc6)="6. Use email, text messaging, or internet to communicate with family, friends or neighbors?"
label(data$sc7)="7. Attend religious services (e.g. church, temple, mosque) online, by phone, or in-person?"
label(data$sc8)="8. Attend classes or other groups you belong to online, by phone, or in-person?"
label(data$emo1)="1.	Feeling nervous, anxious or on edge"
label(data$emo2)="2.	Not being able to stop or control worrying"
label(data$emo3)="3. Little interest or pleasure in doing things"
label(data$emo4)="4. Feeling down, depressed or hopeless?"
label(data$emo5)="5. Trouble falling asleep, or staying asleep, or sleeping too much?"
label(data$emo6)="6. Feeling tired or having little energy?"
label(data$emo7)="7.  Poor appetite or overeating"
label(data$emo8)="8. Feeling bad about yourself, or that you are a failure or have let your family down"
label(data$emo9)="9. Trouble concentrating on things, such as reading the newspaper or watching television"
label(data$emo10)="10. Moving or speaking so slowly that other people could have noticed. Or the opposite - being so fidgety or restless that you have been moving around a lot more than usual."
label(data$emo11)="How difficult have these problems made it for you to do your work, take care of things at home, or get along with other people? "
label(data$pa1)="1. In the past week, how many days did you do aerobic activity?"
label(data$pa2)="2. In the past week, how many minutes per day did you do these aerobic activities?"
label(data$resources___1)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Caregiving help)"
label(data$resources___2)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=COVID-19 resources (e.g., info, testing, vaccines))"
label(data$resources___3)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Financial)"
label(data$resources___4)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Food (e.g. groceries, meals))"
label(data$resources___5)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Health care)"
label(data$resources___6)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Housing)"
label(data$resources___7)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Medications)"
label(data$resources___8)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Safety)"
label(data$resources___9)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Services in my language)"
label(data$resources___10)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Social support)"
label(data$resources___11)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Transportation)"
label(data$resources___12)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=Other: {resources_other})"
label(data$resources___0)="We understand it can be difficult to access resources during this time. Which basic needs are hard for you to meet at this time? Check all that apply. (choice=I do not need help meeting any basic needs)"
label(data$resources_other)="Please describe the other needs that are hard for you to meet."
label(data$tech1a)="a. A computer (desktop or laptop)?"
label(data$tech1b)="b. A tablet (i.e., iPad)?"
label(data$tech1c)="c.	A smart phone (i.e., phone with a video screen and access to internet)"
label(data$tech1d)="d.	Other type of phone (e.g. landline or cell phone without internet or screen)"
label(data$tech1e)="e.	Speaker and microphone (audio) that allows you to talk to and hear people?"
label(data$tech1f)="f.	A camera (video) that allows you to see people and they see you"
label(data$tech1g)="g.	A camera (video) that allows you to see people and they see you"
label(data$tech1h)="h. Other: {tech1h_other}"
label(data$tech1h_other)="Other type of device:"
label(data$tech2)="2. Does family, a friend or a neighbor help you with your technology to participate in this program?  This could include setting up, logging on, or using your technology."
label(data$tech3)="3. On a scale from 1 - 10, please rate how nervous or worried you feel about using whatever technology you used to participate in the program (such as phone, computer, or tablet): (Where 1= Never nervous or worried and 10= Always nervous or worried)"
label(data$tech4)="4. On a scale from 1 - 10, please ratewhether you have the skills to use whatever technology you used to participate in the program (such as phone, computer, or tablet): (Where 1= Never have the skills to use, and 10= Always have the skills to use)"
label(data$program_survey_complete)="Complete?"
label(data$managing_your_chronic_condition_cdsmp_timestamp)="Survey Timestamp"
label(data$cc1)="1. How confident do you feel that you can keep the fatigue caused by your disease from interfering with the things you want to do?"
label(data$cc2)="2. How confident do you feel that you can keep the physical discomfort or pain of your disease from interfering with the things you want to do?"
label(data$cc3)="3. How confident do you feel that you can keep the emotional distress caused by your disease from interfering with the things you want to do?"
label(data$cc4)="4. How confident do you feel that you can keep any other symptoms or health problems you have from interfering with the things you want to do?"
label(data$cc5)="5. How confident do you feel that you can the different tasks and activities needed to manage your health condition so as to reduce your need to see a doctor?"
label(data$cc6)="6. How confident do you feel that you can do things other than just taking medication to reduce how much your illness affects your everyday life?"
label(data$managing_your_chronic_condition_cdsmp_complete)="Complete?"
label(data$living_with_pain_cpsmp_timestamp)="Survey Timestamp"
label(data$pain1)="1.	Your day to day activities?"
label(data$pain2)="2.	Work around the home?"
label(data$pain3)="3.	Your ability to participate in social activities?"
label(data$pain4)="4. Your household chores?"
label(data$pain5)="5. The things you usually do for fun?"
label(data$pain6)="6. Your enjoyment of social activities?"
label(data$opioid1)="1.	Do you have any opioid medications in your house?"
label(data$opioid2)="2.	Are you currently taking any opioid medications?"
label(data$opioid3)="3.	In the past six months have you reduced the amount (dose) of opioid medications you are taking?"
label(data$living_with_pain_cpsmp_complete)="Complete?"
label(data$living_with_diabetes_dsmp_timestamp)="Survey Timestamp"
label(data$dia1)="1. Morning headaches?"
label(data$dia2)="2. Nightmares?"
label(data$dia3)="3. Night sweats?"
label(data$dia4)="4. Lightheadedness?"
label(data$dia5)="5. Shakiness or weakness?"
label(data$dia6)="6. Intense hunger?"
label(data$dia7)="7. Times when you passed out, fainted, or lost consciousness, even for a short time?"
label(data$living_with_diabetes_dsmp_complete)="Complete?"
label(data$medication_use_homemeds_timestamp)="Survey Timestamp"
label(data$med1)="1.	Do you have a better understanding of your medications due to your HomeMeds assessment?"
label(data$med2)="2.	Have there been changes to your medications based on the HomeMeds assessment?"
label(data$medication_use_homemeds_complete)="Complete?"
label(data$satisfaction_timestamp)="Survey Timestamp"
label(data$sat1)="1. Did [pre_arm_1][q10] provide you with useful strategies to use in your everyday life?"
label(data$sat2)="2. On a scale of 1 - 10, how much did you benefit from participating in [pre_arm_1][q10]? (Where 1=Not Beneficial and 10=Very Beneficial)"
label(data$sat3)="3. What did you like about [pre_arm_1][q10]?"
label(data$sat4)="4. What could be improved about [pre_arm_1][q10]?"
label(data$sat5a)="5a. On a scale from 1-10, how likely is it that you would recommend [pre_arm_1][q10] to a friend or family member? (Where 1=Not Likely At All and 10=Extremely Likely)"
label(data$sat5b)="5b. Why did you select that number? "
label(data$sat6)="6. Please use the space below to share anything else about [pre_arm_1][q10]."
label(data$satisfaction_complete)="Complete?"
label(data$qual1)="1) What are the barriers you faced to completing the survey? Can be related to doing the survey specifically (e.g. tech issues) and other life challenges (e.g. feeling low energy after recent hospitalization)."
label(data$qual2)="2) What got in the way of participating in the remote program?  Participants may offer barriers without you prompting, if not, ask them this question before the call ends Make note of whether they completed the program and why dropped out (e.g. didnt feel comfortable with others seeing them on Zoom in their home and were required to keep camera on) "
label(data$qual_qs_for_phone_complete)="Complete?"
label(data$assigned_to)="This participant is assigned to team member:"
label(data$remind_due)="Follow-up survey due:"
label(data$remind_start)="Eligible to take follow-up survey starting on:"
label(data$remind_end)="Follow-up survey expires on:"
label(data$call_notes)="Call Notes:"
label(data$script_hide)="Click here to hide/show the call scripts:"
label(data$date_call1)="Date of call attempt #1:"
label(data$caller_1)="Team Member:"
label(data$ph_num_1)="Team Member Phone:"
label(data$status_call1)="Status of call attempt #1:"
label(data$callback_1)="Better time to call back (following first call): "
label(data$date_call2)="Date of call attempt #2:"
label(data$caller_2)="Team Member:"
label(data$ph_num_2)="Team Member Phone:"
label(data$status_call2)="Status of call attempt #2:"
label(data$callback_2)="Better time to call back (following second call): "
label(data$date_call3)="Date of call attempt #3:"
label(data$caller_3)="Team Member:"
label(data$ph_num_3)="Team Member Phone:"
label(data$status_call3)="Status of call attempt #3:"
label(data$callback_3)="Better time to call back (following third call): "
label(data$survey_complete)="Survey successfully completed:"
label(data$method)="Method:"
label(data$need_link)="Does this participant need us to send another survey link?"
label(data$survey_ph_date)="Date of phone survey:"
label(data$total_time)="Total time of survey (in minutes):"
label(data$survey_none)="Reason survey not completed:"
label(data$reason_none)="Reason (if given):"
label(data$notes_status)="Status Notes:"
label(data$mo_reminder_calls_complete)="Complete?"
label(data$incent_type)="The default incentive is a Tango Card Reward link sent via email ($10 for the pre survey and $20 for the post survey).  If this participant should NOT receive an email Tango link, please indicate here:"
label(data$incent_other)="Other reason not to send incentive:"
label(data$mail_name)="Confirm that we have participants name correct: "
label(data$mail_add1)="Address Line 1"
label(data$mail_add2)="Address Line 2"
label(data$mail_city)="City"
label(data$mail_state)="State"
label(data$mail_zip)="Mailing Zip Code"
label(data$incent_sent_date)="Incentive sent on: "
label(data$incent_sent_by)="Incentive sent by:"
label(data$incentive_sent_complete)="Complete?"
label(data$contact_status)="Study contact status This participant:"
label(data$opt_out___1)="Please select why this organization should not be contacted: (choice=Declined to enroll/participate in the study)"
label(data$opt_out___2)="Please select why this organization should not be contacted: (choice=No longer wishes to be contacted)"
label(data$opt_out___3)="Please select why this organization should not be contacted: (choice=Dropped out of the study)"
label(data$opt_out___0)="Please select why this organization should not be contacted: (choice=Other explanation: {opt_out_other})"
label(data$opt_out_other)="Other opt-out reason:"
label(data$opt_out_date)="Date of change/status confirmed:"
label(data$future_contact_date)="Date requested for future contact:"
label(data$opt_out_notes)="Contact opt-out notes:"
label(data$do_not_contact_complete)="Complete?"
label(data$calc_pre)="Pre Survey Date:"
label(data$calc_post)="Post Survey Date"
label(data$clean_status)="Actions for this record:"
label(data$clean_date)="Record was copied to the CLEAN database on:"
label(data$clean_movedby)="By team member: "
label(data$moved_to_clean_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("pre_arm_1","outreach_arm_1","post_arm_1","admin_arm_1"))
data$redcap_data_access_group.factor = factor(data$redcap_data_access_group,levels=c("bassett"))
data$screen1.factor = factor(data$screen1,levels=c("1","2"))
data$org_id.factor = factor(data$org_id,levels=c("100","101","102","103","104","105","106","107","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","134","135","136","137","138","139","140","141","142","143","144","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","173","174","175","176","177","178","179","180","182","183","184","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230"))
data$q2.factor = factor(data$q2,levels=c("1","0"))
data$q3.factor = factor(data$q3,levels=c("1","0"))
data$q4.factor = factor(data$q4,levels=c("1","2","3","4"))
data$q5.factor = factor(data$q5,levels=c("1","0"))
data$q6___1.factor = factor(data$q6___1,levels=c("0","1"))
data$q6___2.factor = factor(data$q6___2,levels=c("0","1"))
data$q6___3.factor = factor(data$q6___3,levels=c("0","1"))
data$q6___4.factor = factor(data$q6___4,levels=c("0","1"))
data$q6___5.factor = factor(data$q6___5,levels=c("0","1"))
data$q6___6.factor = factor(data$q6___6,levels=c("0","1"))
data$q7___0.factor = factor(data$q7___0,levels=c("0","1"))
data$q7___1.factor = factor(data$q7___1,levels=c("0","1"))
data$q7___2.factor = factor(data$q7___2,levels=c("0","1"))
data$q7___3.factor = factor(data$q7___3,levels=c("0","1"))
data$q7___4.factor = factor(data$q7___4,levels=c("0","1"))
data$q7___5.factor = factor(data$q7___5,levels=c("0","1"))
data$q7___6.factor = factor(data$q7___6,levels=c("0","1"))
data$q7___7.factor = factor(data$q7___7,levels=c("0","1"))
data$q7___8.factor = factor(data$q7___8,levels=c("0","1"))
data$q7___9.factor = factor(data$q7___9,levels=c("0","1"))
data$q7___10.factor = factor(data$q7___10,levels=c("0","1"))
data$q7___11.factor = factor(data$q7___11,levels=c("0","1"))
data$q7___12.factor = factor(data$q7___12,levels=c("0","1"))
data$q7___13.factor = factor(data$q7___13,levels=c("0","1"))
data$q7___14.factor = factor(data$q7___14,levels=c("0","1"))
data$q7___15.factor = factor(data$q7___15,levels=c("0","1"))
data$q7___16.factor = factor(data$q7___16,levels=c("0","1"))
data$q7___17.factor = factor(data$q7___17,levels=c("0","1"))
data$q8_pay.factor = factor(data$q8_pay,levels=c("1","2","3","77","99"))
data$q9_school.factor = factor(data$q9_school,levels=c("1","2","3","4","5","6"))
data$q10.factor = factor(data$q10,levels=c("1","2","3","4","5","6","7","8"))
data$q11.factor = factor(data$q11,levels=c("1","2","3","4"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))
data$completed_prog.factor = factor(data$completed_prog,levels=c("1","0","99"))
data$health.factor = factor(data$health,levels=c("1","2","3","4","5"))
data$fatigue.factor = factor(data$fatigue,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$pain.factor = factor(data$pain,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$sleep.factor = factor(data$sleep,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$sc1.factor = factor(data$sc1,levels=c("1","2","3"))
data$sc2.factor = factor(data$sc2,levels=c("1","2","3"))
data$sc3.factor = factor(data$sc3,levels=c("1","2","3"))
data$sc4.factor = factor(data$sc4,levels=c("1","2","3","4","5"))
data$sc5.factor = factor(data$sc5,levels=c("1","2","3","4","5"))
data$sc6.factor = factor(data$sc6,levels=c("1","2","3","4","5"))
data$sc7.factor = factor(data$sc7,levels=c("1","2","3","4","5"))
data$sc8.factor = factor(data$sc8,levels=c("1","2","3","4","5"))
data$emo1.factor = factor(data$emo1,levels=c("0","1","2","3"))
data$emo2.factor = factor(data$emo2,levels=c("0","1","2","3"))
data$emo3.factor = factor(data$emo3,levels=c("0","1","2","3"))
data$emo4.factor = factor(data$emo4,levels=c("0","1","2","3"))
data$emo5.factor = factor(data$emo5,levels=c("0","1","2","3"))
data$emo6.factor = factor(data$emo6,levels=c("0","1","2","3"))
data$emo7.factor = factor(data$emo7,levels=c("0","1","2","3"))
data$emo8.factor = factor(data$emo8,levels=c("0","1","2","3"))
data$emo9.factor = factor(data$emo9,levels=c("0","1","2","3"))
data$emo10.factor = factor(data$emo10,levels=c("0","1","2","3"))
data$emo11.factor = factor(data$emo11,levels=c("1","2","3","4"))
data$pa1.factor = factor(data$pa1,levels=c("0","1","2","3","4","5","6","7"))
data$pa2.factor = factor(data$pa2,levels=c("1","2","3","4","5","6","7"))
data$resources___1.factor = factor(data$resources___1,levels=c("0","1"))
data$resources___2.factor = factor(data$resources___2,levels=c("0","1"))
data$resources___3.factor = factor(data$resources___3,levels=c("0","1"))
data$resources___4.factor = factor(data$resources___4,levels=c("0","1"))
data$resources___5.factor = factor(data$resources___5,levels=c("0","1"))
data$resources___6.factor = factor(data$resources___6,levels=c("0","1"))
data$resources___7.factor = factor(data$resources___7,levels=c("0","1"))
data$resources___8.factor = factor(data$resources___8,levels=c("0","1"))
data$resources___9.factor = factor(data$resources___9,levels=c("0","1"))
data$resources___10.factor = factor(data$resources___10,levels=c("0","1"))
data$resources___11.factor = factor(data$resources___11,levels=c("0","1"))
data$resources___12.factor = factor(data$resources___12,levels=c("0","1"))
data$resources___0.factor = factor(data$resources___0,levels=c("0","1"))
data$tech1a.factor = factor(data$tech1a,levels=c("1","2","3","4"))
data$tech1b.factor = factor(data$tech1b,levels=c("1","2","3","4"))
data$tech1c.factor = factor(data$tech1c,levels=c("1","2","3","4"))
data$tech1d.factor = factor(data$tech1d,levels=c("1","2","3","4"))
data$tech1e.factor = factor(data$tech1e,levels=c("1","2","3","4"))
data$tech1f.factor = factor(data$tech1f,levels=c("1","2","3","4"))
data$tech1g.factor = factor(data$tech1g,levels=c("1","2","3","4"))
data$tech1h.factor = factor(data$tech1h,levels=c("1","2","3","4"))
data$tech2.factor = factor(data$tech2,levels=c("1","0"))
data$tech3.factor = factor(data$tech3,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$tech4.factor = factor(data$tech4,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$program_survey_complete.factor = factor(data$program_survey_complete,levels=c("0","1","2"))
data$cc1.factor = factor(data$cc1,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$cc2.factor = factor(data$cc2,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$cc3.factor = factor(data$cc3,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$cc4.factor = factor(data$cc4,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$cc5.factor = factor(data$cc5,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$cc6.factor = factor(data$cc6,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$managing_your_chronic_condition_cdsmp_complete.factor = factor(data$managing_your_chronic_condition_cdsmp_complete,levels=c("0","1","2"))
data$pain1.factor = factor(data$pain1,levels=c("1","2","3","4","5"))
data$pain2.factor = factor(data$pain2,levels=c("1","2","3","4","5"))
data$pain3.factor = factor(data$pain3,levels=c("1","2","3","4","5"))
data$pain4.factor = factor(data$pain4,levels=c("1","2","3","4","5"))
data$pain5.factor = factor(data$pain5,levels=c("1","2","3","4","5"))
data$pain6.factor = factor(data$pain6,levels=c("1","2","3","4","5"))
data$opioid1.factor = factor(data$opioid1,levels=c("1","2","3"))
data$opioid2.factor = factor(data$opioid2,levels=c("1","2","3"))
data$opioid3.factor = factor(data$opioid3,levels=c("1","2","3"))
data$living_with_pain_cpsmp_complete.factor = factor(data$living_with_pain_cpsmp_complete,levels=c("0","1","2"))
data$dia1.factor = factor(data$dia1,levels=c("0","1","2"))
data$dia2.factor = factor(data$dia2,levels=c("0","1","2"))
data$dia3.factor = factor(data$dia3,levels=c("0","1","2"))
data$dia4.factor = factor(data$dia4,levels=c("0","1","2"))
data$dia5.factor = factor(data$dia5,levels=c("0","1","2"))
data$dia6.factor = factor(data$dia6,levels=c("0","1","2"))
data$dia7.factor = factor(data$dia7,levels=c("0","1","2"))
data$living_with_diabetes_dsmp_complete.factor = factor(data$living_with_diabetes_dsmp_complete,levels=c("0","1","2"))
data$med1.factor = factor(data$med1,levels=c("1","2","3"))
data$med2.factor = factor(data$med2,levels=c("1","2","3"))
data$medication_use_homemeds_complete.factor = factor(data$medication_use_homemeds_complete,levels=c("0","1","2"))
data$sat1.factor = factor(data$sat1,levels=c("1","0"))
data$sat2.factor = factor(data$sat2,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$sat5a.factor = factor(data$sat5a,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$satisfaction_complete.factor = factor(data$satisfaction_complete,levels=c("0","1","2"))
data$qual_qs_for_phone_complete.factor = factor(data$qual_qs_for_phone_complete,levels=c("0","1","2"))
data$assigned_to.factor = factor(data$assigned_to,levels=c("1","2","3","4","5"))
data$script_hide.factor = factor(data$script_hide,levels=c("0","1"))
data$caller_1.factor = factor(data$caller_1,levels=c("1","2","3","4","5"))
data$ph_num_1.factor = factor(data$ph_num_1,levels=c("JK","KC","SJ","LS","EC"))
data$status_call1.factor = factor(data$status_call1,levels=c("0","1","2","3"))
data$caller_2.factor = factor(data$caller_2,levels=c("1","2","3","4","5"))
data$ph_num_2.factor = factor(data$ph_num_2,levels=c("JK","KC","SJ","LS","EC"))
data$status_call2.factor = factor(data$status_call2,levels=c("0","1","2","3"))
data$caller_3.factor = factor(data$caller_3,levels=c("1","2","3","4","5"))
data$ph_num_3.factor = factor(data$ph_num_3,levels=c("JK","KC","SJ","LS","EC"))
data$status_call3.factor = factor(data$status_call3,levels=c("0","1","2","3"))
data$survey_complete.factor = factor(data$survey_complete,levels=c("1","2","3","0"))
data$method.factor = factor(data$method,levels=c("1","2"))
data$need_link.factor = factor(data$need_link,levels=c("1","0"))
data$survey_none.factor = factor(data$survey_none,levels=c("1","2","3","0","5","4"))
data$mo_reminder_calls_complete.factor = factor(data$mo_reminder_calls_complete,levels=c("0","1","2"))
data$incent_type.factor = factor(data$incent_type,levels=c("1","2","0","3"))
data$mail_state.factor = factor(data$mail_state,levels=c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
data$incent_sent_by.factor = factor(data$incent_sent_by,levels=c("6","1","2","3","4","5"))
data$incentive_sent_complete.factor = factor(data$incentive_sent_complete,levels=c("0","1","2"))
data$contact_status.factor = factor(data$contact_status,levels=c("1","2","3"))
data$opt_out___1.factor = factor(data$opt_out___1,levels=c("0","1"))
data$opt_out___2.factor = factor(data$opt_out___2,levels=c("0","1"))
data$opt_out___3.factor = factor(data$opt_out___3,levels=c("0","1"))
data$opt_out___0.factor = factor(data$opt_out___0,levels=c("0","1"))
data$do_not_contact_complete.factor = factor(data$do_not_contact_complete,levels=c("0","1","2"))
data$clean_status.factor = factor(data$clean_status,levels=c("1","0"))
data$clean_movedby.factor = factor(data$clean_movedby,levels=c("6","5","2","7","4"))
data$moved_to_clean_complete.factor = factor(data$moved_to_clean_complete,levels=c("0","1","2"))

levels(data$redcap_event_name.factor)=c("Pre","Outreach","Post","Admin")
levels(data$redcap_data_access_group.factor)=c("Bassett")
levels(data$screen1.factor)=c("I am a program participant completing my own survey","I am staff completing the survey on behalf of a program participant")
levels(data$org_id.factor)=c("100 - AAASWFL","101 - AARTH Ministry","102 - AgeOptions","103 - Ardent Solutions, Inc.","104 - Area Agency on Aging of Broward County","105 - Area Agency on Aging of East Texas","106 - Area Agency on Aging, Region One","107 - Bernardi Multipurpose Senior Center","109 - Camarillo Health Care District","110 - Caring Health Center","111 - Centralina Area Agency on Aging","112 - Cox Health Systems","113 - Davis County Health Department","114 - dba/Poudre Valley Health Care","115 - Deschutes County Health Services","116 - Dignity Health","117 - Elder Care Services of DeKalb County","118 - Elder Services of the Merrimack Valley","119 - ElderLink","120 - Enhance Asian Community on Health","121 - Ethos","122 - Fairhill Partners","123 - Feet First","124 - Five County Area Agency on Aging","125 - Franklin County YMCA","126 - Greater Lynn Senior Services","127 - Health Initiative","128 - Healthy Living for ME","129 - Holy Cross Health","130 - Journey to Better Health (J2BH), Howard County General Hospital","131 - Kansas City Regional Arthritis Center at Saint Lukes Hospital","132 - Kaweah Delta Health Care District","134 - Lake County Tribal Health","135 - Lane Council of Governments, Senior & Disability Services","136 - LifePath and Family Health Center of Worcester","137 - MAC, Inc./AICS","138 - Mercy Health System","139 - MetroNorth YMCA","140 - Michigan State University Extension","141 - Mississippi County Health Department","142 - Missouri Association of Area Agencies on Aging","143 - Montgomery County Office of Senior Services","144 - Multnomah County","146 - National Kidney Foundation of Michigan","147 - New Mexico Department of Health","148 - Northeast Florida AHEC","149 - Norwescap/RSVP","150 - Osteoarthritis Action Alliance","151 - Partners in Care Foundation","152 - Pennsylvania Department of Aging","153 - Philadelphia Corporation for Aging","154 - Pike County Area Agency on Aging","155 - Rappahannock Rapidan Community Services -","156 - Region 2 Area Agency on Aging","157 - AAA9","158 - Research Institute, Center for Rural Community Health, Bassett Healthcare Network","159 - Rush University Medical Center","160 - Senior Olympics","161 - Senior Resources Area Agency on Aging","162 - Senior Solutions","163 - South Western Vermont Council on aging","164 - Springfield College","165 - SWPA Area Agency on Aging, Inc.","166 - The Institute for Human Services, Inc.","167 - Upstate Medical University","168 - Sherman Oaks/East Valley Adult Center","169 - Alicia Broadous-Duncan MPSC","170 - Valley Intercommunity Council","171 - VIC/Wilkinson Senior MPC","173 - Watts Labor Community Action Committee (WLCAC)","174 - Wayne County Department of Aging","175 - Wellness INX LLC","176 - West Florida Area Health Education Center","177 - Westchester County Dept of Senior Programs","178 - YMCA of Greater Boston","179 - Wellness at Work","180 - University of North Carolina","182 - St. Louis Area Agency on Aging","183 - Chinese Culture and Community Service Center Inc. (CCACC)","184 - Tampa General Hospital","186 - Samaritan Health Services Research & Education","187 - The Area Agency on Aging","188 - Community Health Partnership","189 - University of Tennessee","190 - Parma VA Community Outpatient Clinic","191 - Mid-East Commission Area Agency on Aging","192 - Seattle Parks & Rec","193 - Northshore YMCA","194 - New Mexico Special Olympics","195 - NYU Langone","196 - Lee Health","197 - WACOG","198 - Baker Ripley","199 - Maryland Living Well Center of Excellence - MAC, Inc.","200 - EnhanceFitness Maui","201 - South Dakota State UniversitySDSU Extension","202 - Jewish Family Service of Atlantic & Cape May Counties","203 - Aljoya Mercer island","204 - Malden YMCA","205 - Noyes Health","206 - City of Oakland Park - Active Adults","207 - The Center in Oak Harbor","208 - Utah State University Extension","209 - Davis County Senior Services","210 - MAC, Inc. MD Living Well Center of Excellence","211 - Partners in Care Foundation","212 - Innovations Ministries, Director","213 - TN Dept of Mnetla Health and Substance Abuse Services,","214 - Senior Support Program of the Tri-Valley","215 - Elderbridge Area on Aging","216 - NDSU","217 - Harris County Areas Agency on Aging","218 - Capital Area Agency on Aging","219 - Baltimore County Department of Aging","220 - Oregon Wellness Network and the NWSDS AAA","221 - North Carolina Center for Health Wellness at UNC Asheville","222 - Salt Lake City County Government","223 - Anne Arundel County Maryland","224 - Aging and Disability Resource Center of Dodge County","225 - St. PETER MBC","226 - National Church Residences","227 - Rutgers, The State University of New Jersey","228 - Hawaii Healthy Aging Partnership","229 - Williams YMCA of Avery County - Mitchell County Healthy Living Center","230 - Aging and Disability Resource Center of Brown County")
levels(data$q2.factor)=c("Yes","No")
levels(data$q3.factor)=c("Yes","No")
levels(data$q4.factor)=c("Male","Female","Transgender","Other gender identity {q4_other}")
levels(data$q5.factor)=c("Hispanic/Latino","Not Hispanic/Latino")
levels(data$q6___1.factor)=c("Unchecked","Checked")
levels(data$q6___2.factor)=c("Unchecked","Checked")
levels(data$q6___3.factor)=c("Unchecked","Checked")
levels(data$q6___4.factor)=c("Unchecked","Checked")
levels(data$q6___5.factor)=c("Unchecked","Checked")
levels(data$q6___6.factor)=c("Unchecked","Checked")
levels(data$q7___0.factor)=c("Unchecked","Checked")
levels(data$q7___1.factor)=c("Unchecked","Checked")
levels(data$q7___2.factor)=c("Unchecked","Checked")
levels(data$q7___3.factor)=c("Unchecked","Checked")
levels(data$q7___4.factor)=c("Unchecked","Checked")
levels(data$q7___5.factor)=c("Unchecked","Checked")
levels(data$q7___6.factor)=c("Unchecked","Checked")
levels(data$q7___7.factor)=c("Unchecked","Checked")
levels(data$q7___8.factor)=c("Unchecked","Checked")
levels(data$q7___9.factor)=c("Unchecked","Checked")
levels(data$q7___10.factor)=c("Unchecked","Checked")
levels(data$q7___11.factor)=c("Unchecked","Checked")
levels(data$q7___12.factor)=c("Unchecked","Checked")
levels(data$q7___13.factor)=c("Unchecked","Checked")
levels(data$q7___14.factor)=c("Unchecked","Checked")
levels(data$q7___15.factor)=c("Unchecked","Checked")
levels(data$q7___16.factor)=c("Unchecked","Checked")
levels(data$q7___17.factor)=c("Unchecked","Checked")
levels(data$q8_pay.factor)=c("Not hard at all","Somewhat hard","Very hard","Prefer not to answer","Not applicable")
levels(data$q9_school.factor)=c("Less than high school/GED","Some high school","High school graduate/GED","Some college","College graduate","Post- college")
levels(data$q10.factor)=c("Chronic Disease Self-Management Program (CDSMP)","Chronic Pain Self-Management Program (CPSMP)","Diabetes Self-Management Program (DSMP)","EnhanceFitness","Healthy IDEAS","HomeMeds","Walk with Ease","Other: {q10_other}")
levels(data$q11.factor)=c("Live group online using Zoom or other video-conferencing","Mailed materials + phone calls","Self-directed toolkit","Other {q11_other}")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$completed_prog.factor)=c("Yes","No","Im not sure")
levels(data$health.factor)=c("Excellent","Very Good","Good","Fair","Poor")
levels(data$fatigue.factor)=c("1 No Fatigue","2","3","4","5","6","7","8","9","10 Severe Fatigue")
levels(data$pain.factor)=c("1 No Pain","2","3","4","5","6","7","8","9","10 Severe Pain")
levels(data$sleep.factor)=c("1 No Trouble Sleeping","2","3","4","5","6","7","8","9","10 Extreme Trouble Sleeping")
levels(data$sc1.factor)=c("Hardly ever","Some of the time","Often")
levels(data$sc2.factor)=c("Hardly ever","Some of the time","Often")
levels(data$sc3.factor)=c("Hardly ever","Some of the time","Often")
levels(data$sc4.factor)=c("Never","Once a week","2 days a week","3-5 days a week","Nearly every day")
levels(data$sc5.factor)=c("Never","Once a week","2 days a week","3-5 days a week","Nearly every day")
levels(data$sc6.factor)=c("Never","Once a week","2 days a week","3-5 days a week","Nearly every day")
levels(data$sc7.factor)=c("Never","Once a week","2 days a week","3-5 days a week","Nearly every day")
levels(data$sc8.factor)=c("Never","Once a week","2 days a week","3-5 days a week","Nearly every day")
levels(data$emo1.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo2.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo3.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo4.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo5.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo6.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo7.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo8.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo9.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo10.factor)=c("None at all","Several days","More than half the days","Nearly every day")
levels(data$emo11.factor)=c("Not difficult at all","Somewhat difficult","Very difficult","Extremely difficult")
levels(data$pa1.factor)=c("0 days","1 day","2 days","3 days","4 days","5 days","6 days","7 days")
levels(data$pa2.factor)=c("Less than 15 minutes","15-29 minutes","30-44 minutes","45-59 minutes","60-74 minutes","75-90 minutes","More than 90 minutes")
levels(data$resources___1.factor)=c("Unchecked","Checked")
levels(data$resources___2.factor)=c("Unchecked","Checked")
levels(data$resources___3.factor)=c("Unchecked","Checked")
levels(data$resources___4.factor)=c("Unchecked","Checked")
levels(data$resources___5.factor)=c("Unchecked","Checked")
levels(data$resources___6.factor)=c("Unchecked","Checked")
levels(data$resources___7.factor)=c("Unchecked","Checked")
levels(data$resources___8.factor)=c("Unchecked","Checked")
levels(data$resources___9.factor)=c("Unchecked","Checked")
levels(data$resources___10.factor)=c("Unchecked","Checked")
levels(data$resources___11.factor)=c("Unchecked","Checked")
levels(data$resources___12.factor)=c("Unchecked","Checked")
levels(data$resources___0.factor)=c("Unchecked","Checked")
levels(data$tech1a.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1b.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1c.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1d.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1e.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1f.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1g.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech1h.factor)=c("I own this","I borrow this from someone in my household","I borrow this from a community resource (senior center, public library)","I do not have this technology")
levels(data$tech2.factor)=c("Yes","No")
levels(data$tech3.factor)=c("1 - Never nervous or worried","2","3","4","5","6","7","8","9","10 - Always nervous or worried")
levels(data$tech4.factor)=c("1 - I never have the skills I need to use technology","2","3","4","5","6","7","8","9","10 - I always have the skills I need to use technology")
levels(data$program_survey_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$cc1.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$cc2.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$cc3.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$cc4.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$cc5.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$cc6.factor)=c("1 - Not at all confident","2","3","4","5","6","7","8","9","10 - Totally confident")
levels(data$managing_your_chronic_condition_cdsmp_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$pain1.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$pain2.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$pain3.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$pain4.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$pain5.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$pain6.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$opioid1.factor)=c("Yes","No","Not Sure")
levels(data$opioid2.factor)=c("Yes","No","Not Sure")
levels(data$opioid3.factor)=c("Yes","No","Not Sure")
levels(data$living_with_pain_cpsmp_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$dia1.factor)=c("No","Yes","Dont know")
levels(data$dia2.factor)=c("No","Yes","Dont know")
levels(data$dia3.factor)=c("No","Yes","Dont know")
levels(data$dia4.factor)=c("No","Yes","Dont know")
levels(data$dia5.factor)=c("No","Yes","Dont know")
levels(data$dia6.factor)=c("No","Yes","Dont know")
levels(data$dia7.factor)=c("No","Yes","Dont know")
levels(data$living_with_diabetes_dsmp_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$med1.factor)=c("Yes","No","Not Sure")
levels(data$med2.factor)=c("Yes","No","Not Sure")
levels(data$medication_use_homemeds_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$sat1.factor)=c("Yes","No")
levels(data$sat2.factor)=c("1 - Not Beneficial","2","3","4","5 - Somewhat Beneficial","6","7","8","9","10 - Very Beneficial")
levels(data$sat5a.factor)=c("1 - Not Likely At All","2","3","4","5 - Somewhat Likely","6","7","8","9","10 - Extremely Likely")
levels(data$satisfaction_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$qual_qs_for_phone_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$assigned_to.factor)=c("James Kelley","Kelly Chadwick","Snowy Johnson","Lesley Steinman","Erica Chavez")
levels(data$script_hide.factor)=c("Hide","Show")
levels(data$caller_1.factor)=c("James Kelley","Kelly Chadwick","Snowy Johnson","Lesley Steinman","Erica Chavez")
levels(data$ph_num_1.factor)=c("J: (206) 685-2868","K: (541) 270-8648","S: (406) 620-2374","L: (206) 543-9837","E: (509) 449-1651")
levels(data$status_call1.factor)=c("No voicemail / message option","Left a voicemail / left message with someone else","Call Rescheduled: {callback_1}","Spoke with Participant - Status Indicated Below")
levels(data$caller_2.factor)=c("James Kelley","Kelly Chadwick","Snowy Johnson","Lesley Steinman","Erica Chavez")
levels(data$ph_num_2.factor)=c("J: (206) 685-2868","K: (541) 270-8648","S: (406) 620-2374","L: (206) 543-9837","E: (509) 449-1651")
levels(data$status_call2.factor)=c("No voicemail / message option","Left a voicemail / left message with someone else","Call Rescheduled: {callback_2}","Spoke with Participant - Status Indicated Below")
levels(data$caller_3.factor)=c("James Kelley","Kelly Chadwick","Snowy Johnson","Lesley Steinman","Erica Chavez")
levels(data$ph_num_3.factor)=c("J: (206) 685-2868","K: (541) 270-8648","S: (406) 620-2374","L: (206) 543-9837","E: (509) 449-1651")
levels(data$status_call3.factor)=c("No voicemail / message option","Left a voicemail / left message with someone else","Call Rescheduled: {callback_3}","Spoke with Participant - Status Indicated Below")
levels(data$survey_complete.factor)=c("Yes - Participant has already / plans to complete survey","Unknown - Made 3 call attempts and could not reach individual","Unknown - Survey window closed / not eligible","No")
levels(data$method.factor)=c("Online (email link)","Over-the-phone")
levels(data$need_link.factor)=c("Yes","No")
levels(data$survey_none.factor)=c("Declined participation","Had to end survey early, DO NOT call back","Had to end survey early, OK to call back","Incorrect contact information - not able to reach individual","Duplicate record","Other Reason")
levels(data$mo_reminder_calls_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$incent_type.factor)=c("Send this person a Visa Cash Card INSTEAD of a Tango Link","This participant is from Mt. Sinai and should not receive an incentive","This participant declined the incentive","Other Reason")
levels(data$mail_state.factor)=c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
levels(data$incent_sent_by.factor)=c("Caitlin Mayotte","James Kelley","Kelly Chadwick","Snowy Johnson","Lesley Steinman","Erica Chavez")
levels(data$incentive_sent_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$contact_status.factor)=c("Should NOT be contacted again for this study","Should be contacted at a later date:","Has re-engaged in the study")
levels(data$opt_out___1.factor)=c("Unchecked","Checked")
levels(data$opt_out___2.factor)=c("Unchecked","Checked")
levels(data$opt_out___3.factor)=c("Unchecked","Checked")
levels(data$opt_out___0.factor)=c("Unchecked","Checked")
levels(data$do_not_contact_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$clean_status.factor)=c("Eligible - moved to CLEAN database","NOT eligible; mark form as Incomplete")
levels(data$clean_movedby.factor)=c("Caitlin Mayotte","Erica Chavez","Kelly Chadwick","Sruthi Sravanam","Lesley Steinman")
levels(data$moved_to_clean_complete.factor)=c("Incomplete","Unverified","Complete")
