library(foreign)
library(flexsurv)
library(survminer)
library(ggplot2)
library(bbmle)
library(aod)
library(dotwhisker)
library(interplot)
#---------#
cgss2010 <- read_sav("data source/cgss2010.sav")
data10 <- cgss2010

data10$age <- 2010 - data10$a3a
table(data10$age)

#age categorical
data10$ageCAT <- NA
data10$ageCAT[data10$age < 30] <- 0
data10$ageCAT[data10$age >= 30 & data10$age < 40] <- 1
data10$ageCAT[data10$age >= 40 & data10$age < 50] <- 2
data10$ageCAT[data10$age >= 50 & data10$age < 80] <- 3
data10$ageCAT[data10$age >= 80] <- 4
table(data10$ageCAT)
data10$ageCAT <- factor(data10$ageCAT)

data10$ageCATa <- NA
data10$ageCATa[data10$age < 30] <- 0
data10$ageCATa[data10$age >= 30 & data10$age < 40] <- 1
data10$ageCATa[data10$age >= 40 & data10$age < 50] <- 2
data10$ageCATa[data10$age >= 50 & data10$age < 80] <- 3
data10$ageCATa[data10$age >= 80] <- 4
table(data10$ageCATa)
data10$ageCAT <- factor(data10$ageCAT)

data10$ageD <- ifelse(data10$ageCATa == 0, 1, 0)
data10$ageD <- factor(data10$ageD)

# ethnicity - a4 = ethnicity
# 1 = Han - 0 = all other ethnicity
data10$a4 <- ifelse(data10$a4 < 1, NA, data10$a4)
data10$ethn <- ifelse(data10$a4 > 1, 0, data10$a4)
table(data10$ethn)

# a18: 1= agricultural; 2: non-agricultural; 3: blueprint; 4: residential(prev. agric); 5: residential (prev. non.ag);
# 6: military; 7: no hukou
table(data10$a18)
data10$hukou <- NA
data10$hukou[data10$a18 == 1] <- 0
data10$hukou[data10$a18 == 4] <- 1
data10$hukou[data10$a18 == 2] <- 2
data10$hukou[data10$a18 == 5] <- 2
table(data10$hukou)
data10$hukou <- factor(data10$hukou)

#hukoudummy
data10$hukouD <- NA
data10$hukouD[data10$hukou == 0] <- 0
data10$hukouD[data10$hukou == 1] <- 1
data10$hukouD[data10$hukou == 2] <- 1
table(data10$hukouD)
data10$hukouD<-factor(data10$hukouD)

data10$hukouDM <- NA
data10$hukouDM[data10$hukou == 0] <- 0
data10$hukouDM[data10$hukou == 1] <- 1
data10$hukouDM[data10$hukou == 2] <- 1
hukouDM <- mean(data10$hukouDM, na.rm = T)

table(data10$a3a) #Birth
data10$birth <- ifelse(data10$a3a > 0, data10$a3a, NA)
table(data10$birth)

data10$birth2 <- (data10$birth)^2
table(data10$birth2)

data10$u30 <- ifelse(data10$birth >= 1980, 1, 0)
table(data10$u30)

#---------#
table(data10$a2) #gender - 1 = male
data10$gender <- NA
data10$gender[data10$a2 == 1] <- 1
data10$gender[data10$a2 == 2] <- 0
table(data10$gender)
data10$gender <- factor(data10$gender)

data10$genderM <- NA
data10$genderM[data10$a2 == 1] <- 1
data10$genderM[data10$a2 == 2] <- 0
genderM <- mean(data10$genderM, na.rm = T)
#---------#
table(data10$s5) # area you live in - 1 = city; 2 = rural

# geography: 1 = urban; 0 = urban
data10$geography <- NA
data10$geography[data10$s5 == 1] <- 1
data10$geography[data10$s5 == 2] <- 0
table(data10$geography)
data10$geography <- factor(data10$geography)

data10$geographyM <- NA
data10$geographyM[data10$s5 == 1] <- 1
data10$geographyM[data10$s5 == 2] <- 0
geographyM <- mean(data10$geographyM, na.rm = T)

# political geogrphy
data10$redgeography.cont <- NA
data10$redgeography.cont[data10$s41 == 1] <- 30
data10$redgeography.cont[data10$s41 == 4] <- 29
data10$redgeography.cont[data10$s41 == 7] <- 28
data10$redgeography.cont[data10$s41 == 10] <-27
data10$redgeography.cont[data10$s41 == 11] <- 26
data10$redgeography.cont[data10$s41 == 17] <- 25
data10$redgeography.cont[data10$s41 == 21] <- 24
data10$redgeography.cont[data10$s41 == 25] <- 23
data10$redgeography.cont[data10$s41 == 27] <- 22
data10$redgeography.cont[data10$s41 == 29] <- 21
data10$redgeography.cont[data10$s41 == 3] <- 20
data10$redgeography.cont[data10$s41 == 5] <- 19
data10$redgeography.cont[data10$s41 == 8] <- 18
data10$redgeography.cont[data10$s41 == 14] <- 17
data10$redgeography.cont[data10$s41 == 15] <- 16
data10$redgeography.cont[data10$s41 == 19] <- 15
data10$redgeography.cont[data10$s41 == 22] <- 14
data10$redgeography.cont[data10$s41 == 23] <- 13
data10$redgeography.cont[data10$s41 == 28] <- 12
data10$redgeography.cont[data10$s41 == 30] <- 11
data10$redgeography.cont[data10$s41 == 31] <- 10
data10$redgeography.cont[data10$s41 == 12] <- 9
data10$redgeography.cont[data10$s41 == 2] <- 8
data10$redgeography.cont[data10$s41 == 6] <- 7
data10$redgeography.cont[data10$s41 == 9] <- 6
data10$redgeography.cont[data10$s41 == 13] <- 5
data10$redgeography.cont[data10$s41 == 16] <- 4
data10$redgeography.cont[data10$s41 == 18] <- 3
data10$redgeography.cont[data10$s41 == 20] <-2
data10$redgeography.cont[data10$s41 == 24] <-1
data10$redgeography.cont[data10$s41 == 26] <-0

#Red/pink provinces: 
#RED provinces= 2 -> BEI, TIA, SHA, LIAONING, TIBET, SHANDONG, SHAANXI, SHANXI, HEBEI, HUBEI
#Light-red = 1 -> Zhejiang, Xinjiang, Qinghai, Jiangsu, Ningxia, Gansu, Inner Mong, Jilin, Hilong, Hunan, CHQ
#PINK provinces= 0 -> Sichuan, Anhui, Hainan, Henan, Fujian, Yunnan, Guangxi, Jiangxi, Guizhou, Guangdong

table(data10$s41)
data10$redgeography <- NA

#red provinces
data10$redgeography[data10$s41 == 1] <- 2 
data10$redgeography[data10$s41 == 4] <- 2
data10$redgeography[data10$s41 == 7] <- 2
data10$redgeography[data10$s41 == 10] <-2
data10$redgeography[data10$s41 == 11] <- 2
data10$redgeography[data10$s41 == 17] <- 2
data10$redgeography[data10$s41 == 21] <- 2
data10$redgeography[data10$s41 == 25] <- 2
data10$redgeography[data10$s41 == 27] <- 2
data10$redgeography[data10$s41 == 29] <- 2

#light-red provinces
data10$redgeography[data10$s41 == 3] <- 1 
data10$redgeography[data10$s41 == 5] <- 1
data10$redgeography[data10$s41 == 8] <- 1
data10$redgeography[data10$s41 == 14] <- 1
data10$redgeography[data10$s41 == 15] <- 1
data10$redgeography[data10$s41 == 19] <- 1
data10$redgeography[data10$s41 == 22] <- 1
data10$redgeography[data10$s41 == 23] <- 1
data10$redgeography[data10$s41 == 28] <- 1
data10$redgeography[data10$s41 == 30] <- 1
data10$redgeography[data10$s41 == 31] <- 1

#pink provinces
data10$redgeography[data10$s41 == 12] <- 0
data10$redgeography[data10$s41 == 2] <- 0
data10$redgeography[data10$s41 == 6] <- 0
data10$redgeography[data10$s41 == 9] <- 0
data10$redgeography[data10$s41 == 13] <- 0
data10$redgeography[data10$s41 == 16] <- 0
data10$redgeography[data10$s41 == 18] <- 0
data10$redgeography[data10$s41 == 20] <-0
data10$redgeography[data10$s41 == 24] <-0
data10$redgeography[data10$s41 == 26] <-0

table(data10$redgeography)

data10$redgeography <- factor(data10$redgeography)
redgeographyM <- mean(data10$redgeographyM, na.rm = T)

#light red province is reference category
data10$LREDprov <- NA
data10$LREDprov[data10$redgeography == 0] <- 2
data10$LREDprov[data10$redgeography == 1] <- 0
data10$LREDprov[data10$redgeography == 2] <- 1
data10$LREDprov <- factor(data10$LREDprov)

## per capita Gross regional product
# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data10$GRP_pc <- NA
data10$GRP_pc[data10$s41 == 1] <- 2
data10$GRP_pc[data10$s41 == 4] <- 2
data10$GRP_pc[data10$s41 == 7] <- 2
data10$GRP_pc[data10$s41 == 15] <- 2
data10$GRP_pc[data10$s41 == 19] <- 2
data10$GRP_pc[data10$s41 == 3] <- 2
data10$GRP_pc[data10$s41 == 27] <- 2
data10$GRP_pc[data10$s41 == 24] <- 2
data10$GRP_pc[data10$s41 == 12] <- 2
data10$GRP_pc[data10$s41 == 10] <- 2

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Henan , Hunan, Hebei,Shanxi , Heilongjiang
data10$GRP_pc[data10$s41 == 5] <- 1
data10$GRP_pc[data10$s41 == 28] <- 1
data10$GRP_pc[data10$s41 == 21] <- 1
data10$GRP_pc[data10$s41 == 29] <- 1
data10$GRP_pc[data10$s41 == 8] <- 1
data10$GRP_pc[data10$s41 == 18] <- 1
data10$GRP_pc[data10$s41 == 22] <- 1
data10$GRP_pc[data10$s41 == 17] <- 1
data10$GRP_pc[data10$s41 == 11] <- 1
data10$GRP_pc[data10$s41 == 31] <- 1

#poorest 10 provinces
#Hainan, Xinjiang, Sichuan, Qinghai , Jiangxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, 
#Gangsu
data10$GRP_pc[data10$s41 == 20] <- 0
data10$GRP_pc[data10$s41 == 14] <- 0
data10$GRP_pc[data10$s41 == 6] <- 0
data10$GRP_pc[data10$s41 == 30] <- 0
data10$GRP_pc[data10$s41 == 16] <- 0
data10$GRP_pc[data10$s41 == 9] <- 0
data10$GRP_pc[data10$s41 == 13] <- 0
data10$GRP_pc[data10$s41 == 25] <- 0
data10$GRP_pc[data10$s41 == 2] <- 0
data10$GRP_pc[data10$s41 == 26] <- 0
data10$GRP_pc[data10$s41 == 23] <- 0

table(data10$GRP_pc)
data10$GRP_pc <- factor(data10$GRP_pc)
#---------#
table(data10$a7a) # 1 = no edu; 2-8 = primary/secondary edu; 10, 12-13 = higher edu

data10$edua <- NA
data10$edua[data10$a7a == 3] <- 0
data10$edua[data10$a7a == 4] <- 1
data10$edua[data10$a7a == 6] <- 1
data10$edua[data10$a7a == 7] <- 2
data10$edua[data10$a7a == 10] <- 3
data10$edua[data10$a7a == 12] <- 3
data10$edua[data10$a7a == 13] <- 3
table(data10$edua)

# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data10$edu <- NA
data10$edu[data10$a7a == 1] <- 0
data10$edu[data10$a7a >= 2 & data10$a7a <= 8] <- 1
data10$edu[data10$a7a == 10] <- 2
data10$edu[data10$a7a == 12] <- 2
data10$edu[data10$a7a == 13] <- 2
table(data10$edu)
data10$edu <- factor(data10$edu)

eduM <- mean(data10$eduM, na.rm = T)
# two levels education: 0 = primary/secondary edu; 1 = higher edu
data10$HEdu <- NA
data10$HEdu[data10$edu == 1] <- 0
data10$HEdu[data10$edu == 2] <- 1
table(data10$HEdu)
data10$HEdu <- factor(data10$HEdu)

data10$education1 <- NA
#data10$education1[data10$a7a == 1] <- 0
#data10$education1[data10$a7a == 2] <- 1
data10$education1[data10$a7a == 3] <- 1
data10$education1[data10$a7a == 4] <- 2
data10$education1[data10$a7a == 5] <- 3
data10$education1[data10$a7a == 6] <- 4
data10$education1[data10$a7a == 7] <- 5
data10$education1[data10$a7a == 8] <- 6
data10$education1[data10$a7a == 9] <- 7
data10$education1[data10$a7a == 10] <-8
data10$education1[data10$a7a == 11] <- 9
data10$education1[data10$a7a == 12] <- 10
data10$education1[data10$a7a == 13] <- 11
table(data10$education1)

data10$educationM <- mean(data10$education1, na.rm= T)

#---------#
table(data10$a9a)
data10$CCPapp <- ifelse(data10$a9a > 0, data10$a9a, NA)
data10$CCPapp[is.na(data10$CCPapp)] <- 0
table(data10$CCPapp)

#---------#
table(data10$a10a)
data10$CCPmemb <- ifelse(data10$a10a > 0, data10$a10a, NA)
data10$CCPmemb[is.na(data10$CCPmemb)] <- 0
table(data10$CCPmemb)
#---------#
table(data10$a89b) #father education
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data10$FAedu <- NA
data10$FAedu[data10$a89b == 1] <- 0
data10$FAedu[data10$a89b >= 2 & data10$a89b <= 8] <- 1
data10$FAedu[data10$a89b == 10] <- 2
data10$FAedu[data10$a89b == 12] <- 2
data10$FAedu[data10$a89b == 13] <- 2
table(data10$FAedu)
data10$FAedu <- factor(data10$FAedu)


data10$FAHighEdu <- NA
data10$FAHighEdu[data10$FAedu == 0] <- 0
data10$FAHighEdu[data10$FAedu == 1] <- 0
data10$FAHighEdu[data10$FAedu == 2] <- 1
data10$FAHighEdu <- factor(data10$FAHighEdu)

data10$FAHighEduM <- NA
data10$FAHighEduM[data10$FAedu == 0] <- 0
data10$FAHighEduM[data10$FAedu == 1] <- 0
data10$FAHighEduM[data10$FAedu == 2] <- 1
FAHighEduM <- mean(data10$FAHighEduM, na.rm = T)


#---------#
table(data10$a89c) # father politics -> 1: CCP; 2: Demo parties; 3 = CYL; 4: masses
# 1= CCP; 0 = no CCP
data10$FACCP <- NA
data10$FACCP[data10$a89c == 1] <- 1
data10$FACCP[data10$a89c > 1] <- 0
table(data10$FACCP)
data10$FACCP <- factor(data10$FACCP)

data10$FACCPM <- NA
data10$FACCPM[data10$a89c == 1] <- 1
data10$FACCPM[data10$a89c > 1] <- 0
FACCPM <- mean(data10$FACCPM, na.rm = T)


#---------#
table(data10$a90b) # mother edu
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data10$MOedu <- NA
data10$MOedu[data10$a90b == 1] <- 0
data10$MOedu[data10$a90b >= 2 & data10$a90b <= 8] <- 1
data10$MOedu[data10$a90b == 10] <- 2
data10$MOedu[data10$a90b == 12] <- 2
data10$MOedu[data10$a90b == 13] <- 2
table(data10$MOedu)
data10$MOedu <- factor(data10$MOedu)
MOeduM <- mean(data10$MOeduM, na.rm = T)


data10$MOHighEdu <- NA
data10$MOHighEdu[data10$MOedu == 0] <- 0
data10$MOHighEdu[data10$MOedu == 1] <- 0
data10$MOHighEdu[data10$MOedu == 2] <- 1
data10$MOHighEdu <- factor(data10$MOHighEdu)


data10$MOHighEduM <- NA
data10$MOHighEduM[data10$MOedu == 0] <- 0
data10$MOHighEduM[data10$MOedu == 1] <- 0
data10$MOHighEduM[data10$MOedu == 2] <- 1
MOHighEduM <- mean(data10$MOHighEduM,  na.rm=T)
MOHighEduM

data10$ParentsHEdu <- NA
data10$ParentsHEdu[data10$MOHighEdu == 0 & data10$FAHighEdu == 0] <- 0
data10$ParentsHEdu[data10$MOHighEdu == 1 & data10$FAHighEdu == 0] <- 1
data10$ParentsHEdu[data10$MOHighEdu == 0 & data10$FAHighEdu == 1] <- 1
data10$ParentsHEdu[data10$MOHighEdu == 1 & data10$FAHighEdu == 1] <- 1
table(data10$ParentsHEdu)
data10$ParentsHEdu <- factor(data10$ParentsHEdu)
#---------#
table(data10$a90c) # mother poltics 
# 1= CCP; 0 = no CCP
data10$MOCCP <- NA
data10$MOCCP[data10$a90c == 1] <- 1
data10$MOCCP[data10$a90c > 1] <- 0
table(data10$MOCCP)

data10$MOCCPM <- NA
data10$MOCCPM[data10$a90c == 1] <- 1
data10$MOCCPM[data10$a90c > 1] <- 0
MOCCPM <- mean(data10$MOCCPM, na.rm = T)

#---------#
# 0 = no parents CCP; 1 = at least 1 parent CCP; 2 = both parents CCP
data10$ParentsCCP <- NA
data10$ParentsCCP[data10$FACCP == 0 & data10$MOCCP == 0] <- 0
data10$ParentsCCP[data10$FACCP == 1 & data10$MOCCP == 0] <- 1
data10$ParentsCCP[data10$FACCP == 0 & data10$MOCCP == 1] <- 1
data10$ParentsCCP[data10$FACCP == 1 & data10$MOCCP == 1] <- 2
table(data10$ParentsCCP)
data10$ParentsCCP <- factor(data10$ParentsCCP)

ParentsCCPM <- mean(data10$ParentsCCPM, na.rm = T)

data10$familyPoli <- NA
data10$familyPoli[data10$ParentsCCP == 0] <- 0
data10$familyPoli[data10$ParentsCCP == 1] <- 1
data10$familyPoli[data10$ParentsCCP == 2] <- 1
data10$familyPoli <- factor(data10$familyPoli)
table(data10$familyPoli)
data10$familyPoli <- factor(data10$familyPoli)
#---------#
#Work experience and current job
table(data10$a58)
#1: Currently engaged in non-agricultural work; 
#2: Currently working in agriculture, once had non-agricultural work;
#3: Currently working in farming, no non-agricultural work ; 
#4: No job at the moment, and only farming;
#5: No job at the moment, have had a non-agricultural job;
#6: Never worked

head(data10$a59a) 
- # breakdown data15$a59a)
  #1 I am the boss (or partner)
  #2 Individual industrial and commercial households
  #3 Employed by others (with regular employer)
  #4 Labor workers/labor dispatch personnel
  #5 Part-time workers, casual workers (employees without regular employers)
  #6 Work/help in my own business/enterprise and receive salary
  #7 Working/helping in my own business/enterprise without receiving salary
  #8 Freelancers
  #9 other
  
  

#Job --> 2 = casual work; 1 = rural work; 3 = freelance; 4 = self-emplyed;
# 5 = labor worker; 0 = clerk; 6 = private business; 7 = boss
data10$Job <- NA
data10$Job[data10$a58 == 1] <- 0
data10$Job[data10$a58 == 2] <- 1
data10$Job[data10$a58 == 3] <- 1
data10$Job[data10$a58 == 4] <- 1
data10$Job[data10$a58 == 5] <- 2
data10$Job[data10$a58 == 6] <-  2
data10$Job[data10$a59a == 1] <- 7
data10$Job[data10$a59a == 2] <- 6
data10$Job[data10$a59a == 3] <- 5
data10$Job[data10$a59a == 4] <- 0
data10$Job[data10$a59a == 5] <- 3
data10$Job[data10$a59a == 6] <- 4
data10$Job[data10$a59a == 7] <- 2
data10$Job[data10$a59a == 8] <- 3
table(data10$Job)

#Jobnew --> 0 = white collar; 1 = labor; 2 = rural work
data10$Jobnew <- NA
data10$Jobnew[data10$Job == 0] <- 1
data10$Jobnew[data10$Job == 3] <- 1
data10$Jobnew[data10$Job == 2] <- 1
data10$Jobnew[data10$Job == 4] <- 1
data10$Jobnew[data10$Job == 5] <- 0
data10$Jobnew[data10$Job == 6] <- 0
#data10$Jobnew[data15$Job == 1] <- 2
data10$Jobnew[data10$Job == 7] <- 0
table(data10$Jobnew)
#Jobnew1 --> 0 = white collar; 1 = labor; 2 = rural work
data10$Jobnew1 <- NA
data10$Jobnew1[data10$Job == 0] <- 1
data10$Jobnew1[data10$Job == 3] <- 1
data10$Jobnew1[data10$Job == 2] <- 1
data10$Jobnew1[data10$Job == 4] <- 0
data10$Jobnew1[data10$Job == 5] <- 0
data10$Jobnew1[data10$Job == 6] <- 0
#data10$Jobnew[data10$Job == 1] <- 2
data10$Jobnew1[data10$Job == 7] <- 0
table(data10$Jobnew1)


#Jobnew.rural --> 0 = white collar; 1 = labor; 2 = rural work
data10$Jobnew.rural <- NA
data10$Jobnew.rural[data10$Job == 0] <- 1
data10$Jobnew.rural[data10$Job == 3] <- 1
data10$Jobnew.rural[data10$Job == 2] <- 1
data10$Jobnew.rural[data10$Job == 4] <- 1
data10$Jobnew.rural[data10$Job == 5] <- 0
data10$Jobnew.rural[data10$Job == 6] <- 0
data10$Jobnew.rural[data10$Job == 1] <- 2
data10$Jobnew.rural[data10$Job == 7] <- 0
table(data10$Jobnew.rural)

# Job.labor <-  0 = laboers & casual work; rest is same as Job

data10$Job.labor <- ifelse(data10$Job == 2, 0, data10$Job)
#JobClerk
# 2 = rural work & unemployed; 1 = Wokers; 0 = white collar ; 3 = Managment & private business
data10$JobKey <- NA
data10$JobKey[data10$Job == 1] <- 2
data10$JobKey[data10$Job == 2] <- 1
data10$JobKey[data10$Job == 3] <- 1
data10$JobKey[data10$Job == 4] <- 1
data10$JobKey[data10$Job == 5] <- 0
data10$JobKey[data10$Job == 0] <- 1
data10$JobKey[data10$Job == 6] <- 3
data10$JobKey[data10$Job == 7] <- 3
table(data10$JobKey)
data10$JobKey <- factor(data10$JobKey)

#Job urban
# 0= clerk; 1 = labor worker; 2 = business
data10$UrbanJob <- NA
data10$UrbanJob[data10$Job == 2] <- 1
data10$UrbanJob[data10$Job == 3] <- 1
data10$UrbanJob[data10$Job == 4] <- 1
data10$UrbanJob[data10$Job == 5] <- 0
data10$UrbanJob[data10$Job == 0] <- 1
data10$UrbanJob[data10$Job == 6] <- 2
data10$UrbanJob[data10$Job == 7] <- 2
table(data10$UrbanJob)
data10$UrbanJob <- factor(data10$UrbanJob)

#Job merged NO RURAL
# 0 = labor worker; 1 = others
data10$JobD <- ifelse(data10$UrbanJob == 1, 0, 1)
table(data10$JobD)
data10$JobD <- factor(data10$JobD)

# year FE #
data10$year <- ifelse(data10$CCPapp >= 2005, data10$CCPapp, NA)
table(data10$year)
#--------------------------------------------------------------------------------------
# 1st: cox hazard analysis on the age citizens' application to the CCP #

ageApp10 <- data10$CCPapp - data10$birth
table(ageApp10)

data10$age18App <- ifelse(ageApp10 >= 18, ageApp10, NA)
table(data10$age18App)

data10$age18App[is.na(data10$age18App)] <- 0

time10 <- data10$age18App - 18
table(time10)
#------------------------------------------------------------------------------------
# event
# Creating a dummy for those who applied 2005-2009 and those who did not applied ever

data10$time0509q <- NA
data10$time0509q[data10$CCPapp >= 2005] <- 1
data10$time0509q[data10$CCPapp == 0] <- 0
table(data10$time0509q)

event10q <- data10$time0509q 
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------ //

#1.a CCP acceptance age likelihood

Acc0509 <- ifelse(data10$CCPmemb >= 2005, 1, 0)
table(Acc0509)

event10.a <- Acc0509
table(event10.a)

#------------------------------------------------------------------------------------ 

ageAcc10 <- data10$CCPmemb - data10$birth
table(ageAcc10)

age18Acc10 <- ifelse(ageApp10 >= 18, ageApp10, NA)
table(age18Acc10)


time10.a <- age18Acc10 - 17
table(time10.a)

#------------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------------ //
# Set of dependent variables
data10$Ref_Acc_u30 <- NA
data10$Ref_Acc_u30[data10$CCPapp >= 2005 & data10$birth >= 1980 & data10$CCPmemb >= 2005] <- 1
data10$Ref_Acc_u30[data10$CCPapp < 2005 & data10$birth >= 1980 & data10$CCPmemb >= 2005] <- 2
data10$Ref_Acc_u30[data10$CCPapp >= 2005 & data10$birth < 1980 & data10$CCPmemb >= 2005] <- 3
data10$Ref_Acc_u30[data10$CCPapp < 2005 & data10$birth < 1980 & data10$CCPmemb >= 2005] <- 4
data10$Ref_Acc_u30[data10$CCPapp < 2005 & data10$birth >= 1980 & data10$CCPmemb < 2005] <- 5
data10$Ref_Acc_u30[data10$CCPapp == 0 & data10$birth >=  1980 & data10$CCPmemb == 0] <- 6
data10$Ref_Acc_u30[data10$CCPapp == 0 & data10$birth <  1980 & data10$CCPmemb == 0] <- 7
data10$Ref_Acc_u30[data10$CCPapp < 2005 & data10$CCPapp > 0 & data10$birth < 1980 & data10$CCPmemb == 0] <- 8
data10$Ref_Acc_u30[data10$CCPapp >= 2005 & data10$birth >= 1980 & data10$CCPmemb == 0] <- 9
data10$Ref_Acc_u30[data10$CCPapp >= 2005 & data10$birth <  1980 & data10$CCPmemb == 0] <- 10

data10$Ref_Acc_u30[is.na(data10$Ref_Acc_u30)] <- 0

table(data10$Ref_Acc_u30)
data10$Ref_Acc_u30 <- factor(data10$Ref_Acc_u30)


#new DV - Accepted refused
data10$RefAcc <- NA
data10$RefAcc[data10$CCPapp >= 2005 & data10$CCPmemb >= 0] <- 0
data10$RefAcc[data10$CCPapp >= 2005 & data10$CCPmemb >= 2005] <- 1
table(data10$RefAcc)

#new DV - Accepted refused + 6
data10$RefAcc6 <- NA
data10$RefAcc6[data10$CCPapp >= 2004 & data10$CCPmemb >= 0] <- 0
data10$RefAcc6[data10$CCPapp >= 2004 & data10$CCPmemb >= 2004] <- 1
table(data10$RefAcc6)

#new DV - Accepted refused + 4
data10$RefAcc4 <- NA
data10$RefAcc4[data10$CCPapp >= 2006 & data10$CCPmemb >= 0] <- 0
data10$RefAcc4[data10$CCPapp >= 2006 & data10$CCPmemb >= 2006] <- 1
table(data10$RefAcc4)

#new DV - Accepted refused +10
data10$RefAcc10 <- NA
data10$RefAcc10[data10$CCPapp >= 2000 & data10$CCPmemb >= 0] <- 0
data10$RefAcc10[data10$CCPapp >= 2000 & data10$CCPmemb >= 2000] <- 1
table(data10$RefAcc10)

#new DV - Accepted refused +15
data10$RefAcc15 <- NA
data10$RefAcc15[data10$CCPapp >= 1995 & data10$CCPmemb >= 0] <- 0
data10$RefAcc15[data10$CCPapp >= 1995 & data10$CCPmemb >= 1995] <- 1
table(data10$RefAcc15)


#---------- APPLICANTS DV

#new DV - Applicants or not
data10$AppNo <- NA
data10$AppNo[data10$CCPapp == 0] <- 0
data10$AppNo[data10$CCPapp >= 2005] <- 1
table(data10$AppNo)

#new DV - Applicants or not + 6
data10$AppNo6 <- NA
data10$AppNo6[data10$CCPapp == 0] <- 0
data10$AppNo6[data10$CCPapp >= 2004] <- 1
table(data10$AppNo6)

#new DV - Applicants or not+ 4
data10$AppNo4 <- NA
data10$AppNo4[data10$CCPapp == 0] <- 0
data10$AppNo4[data10$CCPapp >= 2006] <- 1
table(data10$AppNo4)

#new DV - Applicants or not +10
data10$AppNo10 <- NA
data10$AppNo10[data10$CCPapp == 0] <- 0
data10$AppNo10[data10$CCPapp >= 2000] <- 1
table(data10$AppNo10)

#new DV - Applicants or not +15
data10$AppNo15 <- NA
data10$AppNo15[data10$CCPapp == 0] <- 0
data10$AppNo15[data10$CCPapp >= 1995] <- 1
table(data10$AppNo15)


# creating dummies for the tests and the probit models

NoApp10 <- ifelse(data10$Ref_Acc_u30 == 7, 1, 0)
table(NoApp10)
NoAppu30.10 <- ifelse(data10$Ref_Acc_u30 == 6, 1, 0)
table(NoAppu30.10)
Ref0509 <- ifelse(data10$Ref_Acc_u30 == 9 |
                    data10$Ref_Acc_u30 == 10, 1, 0)
table(Ref0509)
Acc0509 <- ifelse(data10$Ref_Acc_u30 == 1 |
                    data10$Ref_Acc_u30 == 3, 1, 0)
table(Acc0509)

# No applicants u30 and those refused 2005-2009
NoAppu30.Ref0509 <- NA
NoAppu30.Ref0509[NoAppu30.10 == 1 & Ref0509 == 0] <- 0
NoAppu30.Ref0509[NoAppu30.10 == 0 & Ref0509 == 1] <- 1
table(NoAppu30.Ref0509)

# No applicants u30 and those accepted 2005-2009
NoAppu30.Acc0509 <- NA
NoAppu30.Acc0509[NoAppu30.10 == 1 & Acc0509 == 0] <- 0
NoAppu30.Acc0509[NoAppu30.10 == 0 & Acc0509 == 1] <- 1
table(NoAppu30.Acc0509)

# No applicants u30 and applicants
NoAppu30.App0509 <- NA
NoAppu30.App0509[NoAppu30.10 == 1 & Acc0509 == 0 & Ref0509 == 0] <- 0
NoAppu30.App0509[NoAppu30.10 == 0 & Acc0509 == 1 & Ref0509 == 0] <- 1
NoAppu30.App0509[NoAppu30.10 == 0 & Acc0509 == 0 & Ref0509 == 1] <- 1
table(NoAppu30.App0509)

# No applicants over 30 and applicants
NoAppov30.App0509 <- NA
NoAppov30.App0509[NoAppu30.10 == 0 & Acc0509 == 0 & Ref0509 == 0] <- 0
NoAppov30.App0509[NoAppu30.10 == 0 & Acc0509 == 1 & Ref0509 == 0] <- 1
NoAppov30.App0509[NoAppu30.10 == 0 & Acc0509 == 0 & Ref0509 == 1] <- 1
table(NoAppov30.App0509)

# Accepted & refused 2005-2009 -> 0: refused; 1: accepted
Acc0509.Ref0509 <- NA
Acc0509.Ref0509[Ref0509 == 1 & Acc0509 == 0] <- 0
Acc0509.Ref0509[Ref0509 == 0 & Acc0509 == 1] <- 1
table(Acc0509.Ref0509)

#------------------------------------------- E N D -----------------------------------------#
