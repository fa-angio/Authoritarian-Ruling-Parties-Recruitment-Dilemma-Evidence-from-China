library(foreign)
library(flexsurv)
library(survminer)
library(survival)
library(ggplot2)
library(bbmle)
library(aod)
library(interplot)
library(devtools)
library(DAMisc)
library(rstan)
library(hrbrthemes)
library(lmtest)
library(sandwich)
library(car)
library(broom)
library(tidyverse)
library(gplots)
library(dotwhisker)
library(hrbrthemes)
#---------#
cgss2015_14 <- read_sav("data source/cgss2015_14.sav")

data15 <- cgss2015_14

summary(data15$a301)
data15$age <- 2015-data15$a301
table(data15$age)
table(data15$b2) #Soc Eco level 3 years ago

data15$ageCAT <- NA
data15$ageCAT[data15$age < 30] <- 0
data15$ageCAT[data15$age >= 30 & data15$age < 40] <- 1
data15$ageCAT[data15$age >= 40 & data15$age < 50] <- 2
data15$ageCAT[data15$age >= 50 & data15$age < 80] <- 3
data15$ageCAT[data15$age >= 80] <- 4
table(data15$ageCAT)


data15$ageD <- ifelse(data15$ageCATa == 0, 1, 0)
data15$ageD <- factor(data15$ageD)

# ethnicity - a4 = ethnicity
# 1 = Han - 0 = all other ethnicity
data15$a4 <- ifelse(data15$a4 < 1, NA, data15$a4)
data15$ethn <- ifelse(data15$a4 > 1, 0, data15$a4)
table(data15$ethn)



# SocEco: 0 = decreased; 1 = same level; 2 = increased
data15$SocEco3YRago <- NA
data15$SocEco3YRago[data15$b2 == 1] <- 2
data15$SocEco3YRago[data15$b2 == 2 & 9] <- 1
data15$SocEco3YRago[data15$b2 == 3] <- 0
table(data15$SocEco3YRago)
SocEco3YRagoM <- mean(data15$SocEco3YRago, na.rm = T)
summary(data15$SocEco3YRagoM)

data15$SocEco3YRago <- factor(data15$SocEco3YRago)

# 0 = decreased, 1 increased
data15$SocEcoD <- ifelse(data15$SocEco3YRago == 0, 0, 1)
table(data15$SocEcoD)

data15$SocEco3YRagoM <- NA
data15$SocEco3YRagoM[data15$b2 == 1] <- 2
data15$SocEco3YRagoM[data15$b2 == 2] <- 1
data15$SocEco3YRagoM[data15$b2 == 9] <- 1
data15$SocEco3YRagoM[data15$b2 == 3] <- 0
table(data15$SocEco3YRagoM)

#---------#
table(data15$a301) #Birth
data15$birth <- ifelse(data15$a301 > 0, data15$a301, NA)
table(data15$birth)

data15$u30 <- ifelse(data15$birth >= 1983, 1, 0)
table(data15$u30)

#---------#
table(data15$a2) #gender - 1 = male
data15$gender <- NA
data15$gender[data15$a2 == 1] <- 1
data15$gender[data15$a2 == 2] <- 0
table(data15$gender)
data15$genderM <- mean(data15$gender, na.rm=TRUE)

data15$gender <- factor(data15$gender)

data15$genderM <- mean(data15$genderM, na.rm=TRUE)
summary(data15$genderM)
#---------#
table(data15$s1) # area you live in - 2 = rural, 1 = city 

# geography: 0 = rural; 1 = urban
data15$geography <- NA
data15$geography[data15$s1 == 1] <- 1
data15$geography[data15$s1 == 2] <- 0
table(data15$geography)
data15$geography <- factor(data15$geography)

data15$geographyM <- NA
data15$geographyM[data15$s1 == 1] <- 1
data15$geographyM[data15$s1 == 2] <- 0
data15$geographyM <- mean(data15$geographyM, na.rm = T)
summary(data15$geographyM)

#Red/pink provinces: 
#RED provinces= 2 -> BEI, TIA, SHA, LIAONING, TIBET, SHANDONG, SHAANXI, SHANXI, HEBEI, HUBEI
#Light-red = 1 -> Zhejiang, Xinjiang, Qinghai, Jiangsu, Ningxia, Gansu, Inner Mong, Jilin, Hilong, Hunan, CHQ
#PINK provinces= 0 -> Sichuan, Anhui, Hainan, Henan, Fujian, Yunnan, Guangxi, Jiangxi, Guizhou, Guangdong

table(data15$s41)
head(data15$s41)
#Megacities
#Megacities = 1 <- 1 (SHA), 4 (BEI), 12 (Guangdong), 28 (CHQ), 7 (TIA)
# Megacities = 0 <- rest  of provinces
data15$megacities <- 0
data15$megacities[data15$s41 == 1] <- 1
data15$megacities[data15$s41 == 4] <- 1
data15$megacities[data15$s41 == 12] <- 1
data15$megacities[data15$s41 == 28] <- 1
data15$megacities[data15$s41 == 7] <- 1
table(data15$megacities)
data15$megacities <- factor(data15$megacities)


## political geography
data15$redgeography.cont <- NA
data15$redgeography.cont[data15$s41 == 1] <- 30
data15$redgeography.cont[data15$s41 == 4] <- 29
data15$redgeography.cont[data15$s41 == 7] <- 28
data15$redgeography.cont[data15$s41 == 10] <-27
data15$redgeography.cont[data15$s41 == 11] <- 26
data15$redgeography.cont[data15$s41 == 17] <- 25
data15$redgeography.cont[data15$s41 == 21] <- 24
data15$redgeography.cont[data15$s41 == 25] <- 23
data15$redgeography.cont[data15$s41 == 27] <- 22
data15$redgeography.cont[data15$s41 == 29] <- 21
data15$redgeography.cont[data15$s41 == 3] <- 20
data15$redgeography.cont[data15$s41 == 5] <- 19
data15$redgeography.cont[data15$s41 == 8] <- 18
data15$redgeography.cont[data15$s41 == 14] <- 17
data15$redgeography.cont[data15$s41 == 15] <- 16
data15$redgeography.cont[data15$s41 == 19] <- 15
data15$redgeography.cont[data15$s41 == 22] <- 14
data15$redgeography.cont[data15$s41 == 23] <- 13
data15$redgeography.cont[data15$s41 == 28] <- 12
data15$redgeography.cont[data15$s41 == 30] <- 11
data15$redgeography.cont[data15$s41 == 31] <- 10
data15$redgeography.cont[data15$s41 == 12] <- 9
data15$redgeography.cont[data15$s41 == 2] <- 8
data15$redgeography.cont[data15$s41 == 6] <- 7
data15$redgeography.cont[data15$s41 == 9] <- 6
data15$redgeography.cont[data15$s41 == 13] <- 5
data15$redgeography.cont[data15$s41 == 16] <- 4
data15$redgeography.cont[data15$s41 == 18] <- 3
data15$redgeography.cont[data15$s41 == 20] <-2
data15$redgeography.cont[data15$s41 == 24] <-1
data15$redgeography.cont[data15$s41 == 26] <-0

#red provinces
data15$redgeography <- NA
data15$redgeography[data15$s41 == 1] <- 2 
data15$redgeography[data15$s41 == 4] <- 2
data15$redgeography[data15$s41 == 7] <- 2
data15$redgeography[data15$s41 == 10] <-2
data15$redgeography[data15$s41 == 11] <- 2
data15$redgeography[data15$s41 == 17] <- 2
data15$redgeography[data15$s41 == 21] <- 2
data15$redgeography[data15$s41 == 25] <- 2
data15$redgeography[data15$s41 == 27] <- 2
data15$redgeography[data15$s41 == 29] <- 2

#light-red provinces
data15$redgeography[data15$s41 == 3] <- 1 
data15$redgeography[data15$s41 == 5] <- 1
data15$redgeography[data15$s41 == 8] <- 1
data15$redgeography[data15$s41 == 14] <- 1
data15$redgeography[data15$s41 == 15] <- 1
data15$redgeography[data15$s41 == 19] <- 1
data15$redgeography[data15$s41 == 22] <- 1
data15$redgeography[data15$s41 == 23] <- 1
data15$redgeography[data15$s41 == 28] <- 1
data15$redgeography[data15$s41 == 30] <- 1
data15$redgeography[data15$s41 == 31] <- 1

#pink provinces
data15$redgeography[data15$s41 == 12] <- 0
data15$redgeography[data15$s41 == 2] <- 0
data15$redgeography[data15$s41 == 6] <- 0
data15$redgeography[data15$s41 == 9] <- 0
data15$redgeography[data15$s41 == 13] <- 0
data15$redgeography[data15$s41 == 16] <- 0
data15$redgeography[data15$s41 == 18] <- 0
data15$redgeography[data15$s41 == 20] <-0
data15$redgeography[data15$s41 == 24] <-0
data15$redgeography[data15$s41 == 26] <-0

table(data15$redgeography)

data15$redgeography <- factor(data15$redgeography)
data15$redgeographyM <- mean(data15$redgeography, na.rm=TRUE)
summary(data15$redgeographyM)

#Red province is reference category
data15$REDprov <- NA
data15$REDprov[data15$redgeography == 0] <- 2
data15$REDprov[data15$redgeography == 1] <- 1
data15$REDprov[data15$redgeography == 2] <- 0
data15$REDprov <- factor(data15$REDprov)

#light red province is reference category
data15$LREDprov <- NA
data15$LREDprov[data15$redgeography == 0] <- 2
data15$LREDprov[data15$redgeography == 1] <- 0
data15$LREDprov[data15$redgeography == 2] <- 1
data15$LREDprov <- factor(data15$LREDprov)


## per capita Gross regional product
# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data15$GRP_pc <- NA
data15$GRP_pc[data15$s41 == 1] <- 2
data15$GRP_pc[data15$s41 == 4] <- 2
data15$GRP_pc[data15$s41 == 7] <- 2
data15$GRP_pc[data15$s41 == 15] <- 2
data15$GRP_pc[data15$s41 == 19] <- 2
data15$GRP_pc[data15$s41 == 3] <- 2
data15$GRP_pc[data15$s41 == 27] <- 2
data15$GRP_pc[data15$s41 == 24] <- 2
data15$GRP_pc[data15$s41 == 12] <- 2
data15$GRP_pc[data15$s41 == 10] <- 2

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Xinjiang, Hunan, Hebei, Qinghai, Heilongjiang
data15$GRP_pc[data15$s41 == 5] <- 1
data15$GRP_pc[data15$s41 == 28] <- 1
data15$GRP_pc[data15$s41 == 21] <- 1
data15$GRP_pc[data15$s41 == 29] <- 1
data15$GRP_pc[data15$s41 == 8] <- 1
data15$GRP_pc[data15$s41 == 14] <- 1
data15$GRP_pc[data15$s41 == 22] <- 1
data15$GRP_pc[data15$s41 == 17] <- 1
data15$GRP_pc[data15$s41 == 30] <- 1
data15$GRP_pc[data15$s41 == 31] <- 1

#poorest 10 provinces
#Hainan, Henan, Sichuan, Shanxi, Jiangxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, 
#Gangsu
data15$GRP_pc[data15$s41 == 20] <- 0
data15$GRP_pc[data15$s41 == 18] <- 0
data15$GRP_pc[data15$s41 == 6] <- 0
data15$GRP_pc[data15$s41 == 11] <- 0
data15$GRP_pc[data15$s41 == 16] <- 0
data15$GRP_pc[data15$s41 == 9] <- 0
data15$GRP_pc[data15$s41 == 13] <- 0
data15$GRP_pc[data15$s41 == 25] <- 0
data15$GRP_pc[data15$s41 == 2] <- 0
data15$GRP_pc[data15$s41 == 26] <- 0
data15$GRP_pc[data15$s41 == 23] <- 0
table(data15$GRP_pc)
data15$GRP_pc <- factor(data15$GRP_pc)

data15$GRP_pcM <- mean(data15$GRP_pc, na.rm = T)
summary(data15$GRP_pcM)

data15$GRP_pcRich <- NA
data15$GRP_pcRich[data15$GRP_pc == 0] <- 2
data15$GRP_pcRich[data15$GRP_pc == 1] <- 1
data15$GRP_pcRich[data15$GRP_pc == 2] <- 0
table(data15$GRP_pcRich)
data15$GRP_pcRich <- factor(data15$GRP_pcRich)

data15$GRP_pcRichM <- NA
data15$GRP_pcRichM[data15$GRP_pc == 0] <- 2
data15$GRP_pcRichM[data15$GRP_pc == 1] <- 1
data15$GRP_pcRichM[data15$GRP_pc == 2] <- 0
table(data15$GRP_pcRichM)
data15$GRP_pcRichM <- mean(data15$GRP_pcRichM)
summary(data15$GRP_pcRichM)

# a18: 1= agricultural; 2: non-agricultural; 3: blueprint; 4: residential(prev. agric); 5: residential (prev. non.ag);
# 6: military; 7: no hukou
# Hukou -> 0: agricultural; 1: migrant succesful; 2: urban
table(data15$a18)

data15$hukou <- NA
data15$hukou[data15$a18 == 1] <- 0
data15$hukou[data15$a18 == 4] <- 1
data15$hukou[data15$a18 == 2] <- 2
data15$hukou[data15$a18 == 5] <- 2
table(data15$hukou)
data15$hukou <- factor(data15$hukou)

#hukoudummy (1 = urban; 0 = rural)
data15$hukouD <- NA
data15$hukouD[data15$hukou == 0] <- 0
data15$hukouD[data15$hukou == 1] <- 1
data15$hukouD[data15$hukou == 2] <- 1
data15$hukouD<-factor(data15$hukouD)

data15$hukouDM <- NA
data15$hukouDM[data15$hukou == 0] <- 0
data15$hukouDM[data15$hukou == 1] <- 1
data15$hukouDM[data15$hukou == 2] <- 1
hukouDM <- mean(data15$hukouDM, na.rm = T)

#---------#
head(data15$a7a) # 1 = no edu; 2-8 = primary/secondary edu; 10, 12-13 = higher edu

data15$education1 <- NA
#data15$education1[data15$a7a == 1] <- 0
#data15$education1[data15$a7a == 2] <- 1
data15$education1[data15$a7a == 3] <- 1
data15$education1[data15$a7a == 4] <- 2
data15$education1[data15$a7a == 5] <- 3
data15$education1[data15$a7a == 6] <- 4
data15$education1[data15$a7a == 7] <- 5
data15$education1[data15$a7a == 8] <- 6
data15$education1[data15$a7a == 9] <- 7
data15$education1[data15$a7a == 10] <-8
data15$education1[data15$a7a == 11] <- 9
data15$education1[data15$a7a == 12] <- 10
data15$education1[data15$a7a == 13] <- 11
table(data15$education1)

data15$educationM <- mean(data15$education1, na.rm= T)
table(data15$educationM)

# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data15$edua <- NA
data15$edua[data15$a7a == 3] <- 0
data15$edua[data15$a7a == 4] <- 1
data15$edua[data15$a7a == 6] <- 1
data15$edua[data15$a7a == 7] <- 2
data15$edua[data15$a7a == 10] <- 3
data15$edua[data15$a7a == 12] <- 3
data15$edua[data15$a7a == 13] <- 3
table(data15$edua)

# two levels education: 0 = primary/secondary edu; 1 = higher edu
data15$HEdu <- NA
data15$HEdu[data15$edua < 3] <- 0
data15$HEdu[data15$edua == 2] <- 1
table(data15$HEdu)
data15$HEdu <- factor(data15$HEdu)

data15$HEduM <- NA
data15$HEduM[data15$edu == 1] <- 0
data15$HEduM[data15$edu == 2] <- 1
HEduM <- mean(data15$HEduM, na.rm = T)
#---------#
table(data15$a9a)
data15$CCPapp <- ifelse(data15$a9a > 0, data15$a9a, NA)
data15$CCPapp[is.na(data15$CCPapp)] <- 0
table(data15$CCPapp)
#---------#
table(data15$a10a)
data15$CCPmemb <- ifelse(data15$a10a > 0, data15$a10a, NA)
data15$CCPmemb[is.na(data15$CCPmemb)] <- 0
table(data15$CCPmemb)
#---------#
table(data15$a89b) #father education
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data15$FAedu <- NA
data15$FAedu[data15$a89b == 1] <- 0
data15$FAedu[data15$a89b >= 2 & data15$a89b <= 8] <- 1
data15$FAedu[data15$a89b == 10] <- 2
data15$FAedu[data15$a89b == 12] <- 2
data15$FAedu[data15$a89b == 13] <- 2
table(data15$FAedu)
data15$FAedu <- factor(data15$FAedu)

data15$FAeduM <- NA
data15$FAeduM[data15$a89b == 1] <- 0
data15$FAeduM[data15$a89b >= 2 & data15$a89b <= 8] <- 1
data15$FAeduM[data15$a89b == 10] <- 2
data15$FAeduM[data15$a89b == 12] <- 2
data15$FAeduM[data15$a89b == 13] <- 2
FAeduM <- mean(data15$FAeduM, na.rm=TRUE)
FAeduM

data15$FAHighEdu <- NA
data15$FAHighEdu[data15$FAedu == 0] <- 0
data15$FAHighEdu[data15$FAedu == 1] <- 0
data15$FAHighEdu[data15$FAedu == 2] <- 1
data15$FAHighEdu <- factor(data15$FAHighEdu)

data15$FAHighEduM <- NA
data15$FAHighEduM[data15$FAedu == 0] <- 0
data15$FAHighEduM[data15$FAedu == 1] <- 0
data15$FAHighEduM[data15$FAedu == 2] <- 1
FAHighEduM <- mean(data15$FAHighEduM, na.rm = T)
#---------#
table(data15$a89c) # father politics -> 1: masses; 2: Demo parties; 3 = CYL; 4: CCP
# 1= CCP; 0 = no CCP
data15$FACCP <- NA
data15$FACCP[data15$a89c == 4] <- 1
data15$FACCP[data15$a89c == 1 ] <- 0
data15$FACCP[data15$a89c == 2] <- 0
data15$FACCP[data15$a89c == 3 ] <- 0
data15$FACCP <- factor(data15$FACCP)
table(data15$FACCP)

data15$FACCPM <- NA
data15$FACCPM[data15$a89c == 4] <- 1
data15$FACCPM[data15$a89c == 1 ] <- 0
data15$FACCPM[data15$a89c == 2] <- 0
data15$FACCPM[data15$a89c == 3 ] <- 0
data15$FACCPM <- mean(data15$FACCPM, na.rm = T)
table(data15$FACCPM)
#---------#
table(data15$a90b) # mother edu
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data15$MOedu <- NA
data15$MOedu[data15$a90b == 1] <- 0
data15$MOedu[data15$a90b >= 2 & data15$a90b <= 8] <- 1
data15$MOedu[data15$a90b == 10] <- 2
data15$MOedu[data15$a90b == 12] <- 2
data15$MOedu[data15$a90b == 13] <- 2
table(data15$MOedu)
data15$MOedu <- factor(data15$MOedu)

data15$MOeduM <- NA
data15$MOeduM[data15$a90b == 1] <- 0
data15$MOeduM[data15$a90b >= 2 & data15$a90b <= 8] <- 1
data15$MOeduM[data15$a90b == 10] <- 2
data15$MOeduM[data15$a90b == 12] <- 2
data15$MOeduM[data15$a90b == 13] <- 2
MOeduM <- mean(data15$MOeduM, na.rm = T)
MOeduM

data15$MOHighEdu <- NA
data15$MOHighEdu[data15$MOedu == 0] <- 0
data15$MOHighEdu[data15$MOedu == 1] <- 0
data15$MOHighEdu[data15$MOedu == 2] <- 1
data15$MOHighEdu <- factor(data15$MOHighEdu)

data15$MOHighEduM <- NA
data15$MOHighEduM[data15$MOedu == 0] <- 0
data15$MOHighEduM[data15$MOedu == 1] <- 0
data15$MOHighEduM[data15$MOedu == 2] <- 1
MOHighEduM <- mean(data15$MOHighEduM,  na.rm=T)
MOHighEduM

#---------#
table(data15$a90c) # mother poltics 
# 1= CCP; 0 = no CCP
data15$MOCCP <- NA
data15$MOCCP[data15$a90c == 4] <- 1
data15$MOCCP[data15$a90c == 1 ] <- 0
data15$MOCCP[data15$a90c == 2] <- 0
data15$MOCCP[data15$a90c == 3 ] <- 0

table(data15$MOCCP)
data15$MOCCPM <- mean(data15$MOCCP, na.rm = T)
table(data15$MOCCPM)
#---------#
# 0 = no parents CCP; 1 = at least 1 parent CCP; 2 = both parents CCP
data15$ParentsCCP <- NA
data15$ParentsCCP[data15$FACCP == 0 & data15$MOCCP == 0] <- 0
data15$ParentsCCP[data15$FACCP == 1 & data15$MOCCP == 0] <- 1
data15$ParentsCCP[data15$FACCP == 0 & data15$MOCCP == 1] <- 1
data15$ParentsCCP[data15$FACCP == 1 & data15$MOCCP == 1] <- 2
table(data15$ParentsCCP)
data15$ParentsCCP <- factor(data15$ParentsCCP)

data15$ParentsCCPM <- NA
data15$ParentsCCPM[data15$FACCP == 0 & data15$MOCCP == 0] <- 0
data15$ParentsCCPM[data15$FACCP == 1 & data15$MOCCP == 0] <- 1
data15$ParentsCCPM[data15$FACCP == 0 & data15$MOCCP == 1] <- 1
data15$ParentsCCPM[data15$FACCP == 1 & data15$MOCCP == 1] <- 2
ParentsCCPM <- mean(data15$ParentsCCPM, na.rm = T)
ParentsCCPM

# parents either or
data15$familyPoli <- NA
data15$familyPoli[data15$ParentsCCP == 0] <- 0
data15$familyPoli[data15$ParentsCCP == 1] <- 1
data15$familyPoli[data15$ParentsCCP == 2] <- 1
data15$familyPoli <- factor(data15$familyPoli)

#---------#
# 0 = no edu; 1 = primary/secondary; 2 = at least one with higher edu; 3 = both higher edu
data15$ParentsEDU <- NA
data15$ParentsEDU[data15$MOedu == 0 & data15$FAedu == 0] <- 0
data15$ParentsEDU[data15$MOedu == 0 & data15$FAedu == 1] <- 1
data15$ParentsEDU[data15$MOedu == 1 & data15$FAedu == 0] <- 1
data15$ParentsEDU[data15$MOedu == 1 & data15$FAedu == 1] <- 1
data15$ParentsEDU[data15$MOedu == 2 & data15$FAedu == 0] <- 2
data15$ParentsEDU[data15$MOedu == 2 & data15$FAedu == 1] <- 2
data15$ParentsEDU[data15$MOedu == 2 & data15$FAedu == 2] <- 3
data15$ParentsEDU[data15$MOedu == 0 & data15$FAedu == 2] <- 2
data15$ParentsEDU[data15$MOedu == 1 & data15$FAedu == 2] <- 2
table(data15$ParentsEDU)
data15$ParentsEDU <- factor(data15$ParentsEDU)


data15$ParentsHEdu <- NA
data15$ParentsHEdu[data15$MOHighEdu == 0 & data15$FAHighEdu == 0] <- 0
data15$ParentsHEdu[data15$MOHighEdu == 1 & data15$FAHighEdu == 0] <- 1
data15$ParentsHEdu[data15$MOHighEdu == 0 & data15$FAHighEdu == 1] <- 1
data15$ParentsHEdu[data15$MOHighEdu == 1 & data15$FAHighEdu == 1] <- 1
table(data15$ParentsHEdu)
data15$ParentsHEdu <- factor(data15$ParentsHEdu)

#Work experience and current job
head(data15$a58)
#1: Currently engaged in non-agricultural work; 
#2: Currently working in agriculture, once had non-agricultural work;
#3: Currently working in farming, no non-agricultural work ; 
#4: No job at the moment, and only farming;
#5: No job at the moment, have had a non-agricultural job;
#6: Never worked

head(data15$a59a) 
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
# 0 = labor worker; 5 = clerk; 6 = private business; 7 = boss
data15$Job <- NA
data15$Job[data15$a58 == 1] <- 0
data15$Job[data15$a58 == 2] <- 1
data15$Job[data15$a58 == 3] <- 1
data15$Job[data15$a58 == 4] <- 1
data15$Job[data15$a58 == 5] <- 2
data15$Job[data15$a58 == 6] <-  2
data15$Job[data15$a59a == 1] <- 7
data15$Job[data15$a59a == 2] <- 6
data15$Job[data15$a59a == 3] <- 5
data15$Job[data15$a59a == 4] <- 0
data15$Job[data15$a59a == 5] <- 3
data15$Job[data15$a59a == 6] <- 4
data15$Job[data15$a59a == 7] <- 2
data15$Job[data15$a59a == 8] <- 3
table(data15$Job)

#Jobnew --> 0 = white collar; 1 = labor; 2 = rural work
data15$Jobnew <- NA
data15$Jobnew[data15$Job == 0] <- 1
data15$Jobnew[data15$Job == 3] <- 1
data15$Jobnew[data15$Job == 2] <- 1
data15$Jobnew[data15$Job == 4] <- 1
data15$Jobnew[data15$Job == 5] <- 0
data15$Jobnew[data15$Job == 6] <- 0
#data15$Jobnew[data15$Job == 1] <- 2
data15$Jobnew[data15$Job == 7] <- 0
table(data15$Jobnew)

#Jobnew1 --> 0 = white collar; 1 = labor; 2 = rural work
data15$Jobnew1 <- NA
data15$Jobnew1[data15$Job == 0] <- 1
data15$Jobnew1[data15$Job == 3] <- 1
data15$Jobnew1[data15$Job == 2] <- 1
data15$Jobnew1[data15$Job == 4] <- 0
data15$Jobnew1[data15$Job == 5] <- 0
data15$Jobnew1[data15$Job == 6] <- 0
#data15$Jobnew[data15$Job == 1] <- 2
data15$Jobnew1[data15$Job == 7] <- 0
table(data15$Jobnew1)


#Jobnew.rural --> 0 = white collar; 1 = labor; 2 = rural work
data15$Jobnew.rural <- NA
data15$Jobnew.rural[data15$Job == 0] <- 1
data15$Jobnew.rural[data15$Job == 3] <- 1
data15$Jobnew.rural[data15$Job == 2] <- 1
data15$Jobnew.rural[data15$Job == 4] <- 1
data15$Jobnew.rural[data15$Job == 5] <- 0
data15$Jobnew.rural[data15$Job == 6] <- 0
data15$Jobnew.rural[data15$Job == 1] <- 2
data15$Jobnew.rural[data15$Job == 7] <- 0
table(data15$Jobnew.rural)

# Job.labor <-  0 = laboers & casual work; rest is same as Job

data15$Job.labor <- ifelse(data15$Job == 2, 0, data15$Job)

#JobClerk
# 2 = rural work & unemployed; 1 = Wokers; 0 = white collar ; 3 = Managment & private business
data15$JobKey <- NA
data15$JobKey[data15$Job == 1] <- 2
data15$JobKey[data15$Job == 2] <- 1
data15$JobKey[data15$Job == 3] <- 1
data15$JobKey[data15$Job == 4] <- 1
data15$JobKey[data15$Job == 5] <- 0
data15$JobKey[data15$Job == 0] <- 1
data15$JobKey[data15$Job == 6] <- 3
data15$JobKey[data15$Job == 7] <- 3
table(data15$JobKey)
data15$JobKey <- factor(data15$JobKey)

#Job urban
# 0= clerk; 1 = labor worker; 2 = business
data15$UrbanJob <- NA
data15$UrbanJob[data15$Job == 2] <- 1
data15$UrbanJob[data15$Job == 3] <- 1
data15$UrbanJob[data15$Job == 4] <- 1
data15$UrbanJob[data15$Job == 5] <- 0
data15$UrbanJob[data15$Job == 0] <- 1
data15$UrbanJob[data15$Job == 6] <- 2
data15$UrbanJob[data15$Job == 7] <- 2
table(data15$UrbanJob)
data15$UrbanJob <- factor(data15$UrbanJob)

#Job merged NO RURAL
# 0 = labor worker; 1 = others
data15$JobD <- ifelse(data15$UrbanJob == 1, 0, 1)
table(data15$JobD)
data15$JobD <- factor(data15$JobD)

# wealth
summary(data15$a8a)

summary(data15$LNwealth)

# self-reported class
summary(data15$a431)
table(data15$a431)

data15$self.class <- ifelse(data15$a431 > 0, data15$a431, NA)

# JOB SEARCH
summary(data15$a60f)
data15$work.body <- ifelse(data15$a60f >0, data15$a60f, NA)

# wnterpise ownership
summary(data15$a60k)
# 1 State-owned or state-controlled
# 2 Collectively owned or collectively held
# 3 Private/private or private/private holding 
# 4 Hong Kong, Macao and Taiwan capital or Hong Kong, Macao and Taiwan capital holding 
# 5 Foreign ownership or foreign holding

data15$owner <- ifelse(data15$a60k > 0, data15$a60k, NA)

data15$owner <- NA
data15$owner[data15$a60k == 1] <- 0
data15$owner[data15$a60k == 2] <- 0
data15$owner[data15$a60k == 3] <- 1
data15$owner[data15$a60k == 4] <- 1
data15$owner[data15$a60k == 5] <- 1
data15$owner[data15$a60k == 6] <- 1
table(data15$owner)

# WEALTH MATERIAL 
# CAR OWNER
table(data15$a66)

data15$car <- ifelse(data15$a66 > 0, data15$a66, NA)

# INVESTMENTS
table(data15$a6701)

data15$car <- ifelse(data15$a66 > 0, data15$a66, NA)


#--------------------------------------------------------------------------------------
#analysis

# 1st: cox hazard analysis on the age citizens' application to the CCP #

ageApp15 <- data15$CCPapp - data15$birth
table(ageApp15)

data15$age18App15 <- ifelse(ageApp15 >= 18, ageApp15, NA)
table(data15$age18App15)

data15$age18App <- ifelse(ageApp15 >= 18, ageApp15, NA)
table(data15$age18App)

data15$age18App[is.na(data15$age18App)] <- 0

time15 <- data15$age18App - 18
table(time15)

time15a <- data15$age18App
table(time15a)
#------------------------------------------------------------------------------------
# event

# Creating a dummy for those who applied 2010-2015 and those who did not applied ever
table(data15$CCPapp)

data15$time1014q <- NA
data15$time1014q[data15$CCPapp >= 2010] <- 1
data15$time1014q[data15$CCPapp == 0] <- 0
table(data15$time1014q)
event15q <- data15$time1014q
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------ //
#1.a CCP acceptance age likelihood

Acc1014 <- ifelse(data15$CCPmemb >= 2010, 1, 0)
table(Acc1014)

event15.a <- Acc1014
table(event15.a)
#------------------------------------------------------------------------------------ 

ageAcc15 <- data15$CCPmemb - data15$birth
table(ageAcc15)

age18Acc15 <- ifelse(ageApp15 >= 18 , ageApp15, NA)
table(age18Acc15)

mean(age18Acc15, na.rm = T)

time15.a <- age18Acc15 - 18
table(time15.a)

time15.b <- ifelse(time15.a < 15, time15.a, NA)
#------------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------------ 

# Set of dependent variables
data15$Ref_Acc_u30 <- NA
data15$Ref_Acc_u30[data15$CCPapp >= 2010 & data15$birth >= 1985 & data15$CCPmemb >= 2010] <- 1
data15$Ref_Acc_u30[data15$CCPapp < 2010 & data15$birth >= 1985 & data15$CCPmemb >= 2010] <- 2
data15$Ref_Acc_u30[data15$CCPapp >= 2010 & data15$birth < 1985 & data15$CCPmemb >= 2010] <- 3
data15$Ref_Acc_u30[data15$CCPapp < 2010 & data15$birth < 1985 & data15$CCPmemb >= 2010] <- 4
data15$Ref_Acc_u30[data15$CCPapp < 2010 & data15$birth >= 1985 & data15$CCPmemb < 2010] <- 5
data15$Ref_Acc_u30[data15$CCPapp == 0 & data15$birth >=  1985 & data15$CCPmemb == 0] <- 6
data15$Ref_Acc_u30[data15$CCPapp == 0 & data15$birth <  1985 & data15$CCPmemb == 0] <- 7
data15$Ref_Acc_u30[data15$CCPapp < 2010 & data15$CCPapp > 0 & data15$birth < 1983 & data15$CCPmemb == 0] <- 8
data15$Ref_Acc_u30[data15$CCPapp >= 2010 & data15$birth >= 1985 & data15$CCPmemb == 0] <- 9
data15$Ref_Acc_u30[data15$CCPapp >= 2010 & data15$birth <  1985 & data15$CCPmemb == 0] <- 10

data15$Ref_Acc_u30[is.na(data15$Ref_Acc_u30)] <- 0

table(data15$Ref_Acc_u30)
data15$Ref_Acc_u30 <- factor(data15$Ref_Acc_u30)


#new DV - Accepted refused
data15$RefAcc <- NA
data15$RefAcc[data15$CCPapp >= 2010 & data15$CCPmemb >= 0] <- 0
data15$RefAcc[data15$CCPapp >= 2010 & data15$CCPmemb >= 2010] <- 1
table(data15$RefAcc)


#new DV - Accepted refused ROBUSTNESS +6 YEARS
data15$RefAcc6 <- NA
data15$RefAcc6[data15$CCPapp >= 2000 & data15$CCPmemb >= 0] <- 0
data15$RefAcc6[data15$CCPapp >= 2009 & data15$CCPmemb >= 2009] <- 1
table(data15$RefAcc6)

#new DV - Accepted refused ROBUSTNESS +4 YEARS
data15$RefAcc4 <- NA
data15$RefAcc4[data15$CCPapp >= 2011 & data15$CCPmemb >= 0] <- 0
data15$RefAcc4[data15$CCPapp >= 2011 & data15$CCPmemb >= 2011] <- 1
table(data15$RefAcc4)

#new DV - Accepted refused ROBUSTNESS +10 YEARS
data15$RefAcc10 <- NA
data15$RefAcc10[data15$CCPapp >= 2005 & data15$CCPmemb >= 0] <- 0
data15$RefAcc10[data15$CCPapp >= 2005 & data15$CCPmemb >= 2005] <- 1
table(data15$RefAcc10)

#new DV - Accepted refused ROBUSTNESS +15 YEARS
data15$RefAcc15 <- NA
data15$RefAcc15[data15$CCPapp >= 2000 & data15$CCPmemb >= 0] <- 0
data15$RefAcc15[data15$CCPapp >= 2000 & data15$CCPmemb >= 2000] <- 1
table(data15$RefAcc15)


# ---------- applicants DV
#new DV - Applicants or not
data15$AppNo <- NA
data15$AppNo[data15$CCPapp == 0] <- 0
data15$AppNo[data15$CCPapp >= 2010] <- 1
table(data15$AppNo)


#new DV - Applicants or not ROBUSTNESS +6 YEARS
data15$AppNo6 <- NA
data15$AppNo6[data15$CCPapp == 0] <- 0
data15$AppNo6[data15$CCPapp >= 2009] <- 1
table(data15$AppNo6)

#new DV - Applicants or not ROBUSTNESS +4YEARS
data15$AppNo4 <- NA
data15$AppNo4[data15$CCPapp == 0] <- 0
data15$AppNo4[data15$CCPapp >= 2011] <- 1
table(data15$AppNo4)

#new DV - Applicants or not ROBUSTNESS +10YEARS
data15$AppNo10 <- NA
data15$AppNo10[data15$CCPapp == 0] <- 0
data15$AppNo10[data15$CCPapp >= 2005] <- 1
table(data15$AppNo10)

#new DV - Applicants or not ROBUSTNESS +15YEARS
data15$AppNo15 <- NA
data15$AppNo15[data15$CCPapp == 0] <- 0
data15$AppNo15[data15$CCPapp >= 2000] <- 1
table(data15$AppNo15)


# creating dummies for the tests and the probit models

NoApp15 <- ifelse(data15$Ref_Acc_u30 == 7, 1, 0)
table(NoApp15)
NoAppu30.15 <- ifelse(data15$Ref_Acc_u30 == 6, 1, 0)
table(NoAppu30.15)
Ref1014 <- ifelse(data15$Ref_Acc_u30 == 9 |
                    data15$Ref_Acc_u30 == 10, 1, 0)
table(Ref1014)
Acc1014 <- ifelse(data15$Ref_Acc_u30 == 1 |
                    data15$Ref_Acc_u30 == 3, 1, 0)
table(Acc1014)

# No applicants u30 and those refused 2008-2012
NoAppu30.Ref1014 <- NA
NoAppu30.Ref1014[NoAppu30.15 == 1 & Ref1014 == 0] <- 0
NoAppu30.Ref1014[NoAppu30.15 == 0 & Ref1014 == 1] <- 1
table(NoAppu30.Ref1014)

# No applicants u30 and those accepted 2008-2012
NoAppu30.Acc1014 <- NA
NoAppu30.Acc1014[NoAppu30.15 == 1 & Acc1014 == 0] <- 0
NoAppu30.Acc1014[NoAppu30.15 == 0 & Acc1014 == 1] <- 1
table(NoAppu30.Acc1014)

# No applicants u30 and applicants
NoAppu30.App1014 <- NA
NoAppu30.App1014[NoAppu30.15 == 1 & Acc1014 == 0 & Ref1014 == 0] <- 0
NoAppu30.App1014[NoAppu30.15 == 0 & Acc1014 == 1 & Ref1014 == 0] <- 1
NoAppu30.App1014[NoAppu30.15 == 0 & Acc1014 == 0 & Ref1014 == 1] <- 1
table(NoAppu30.App1014)

# No applicants over 30 and applicants
NoAppov30.App1014 <- NA
NoAppov30.App1014[NoAppu30.15 == 0 & Acc1014 == 0 & Ref1014 == 0] <- 0
NoAppov30.App1014[NoAppu30.15 == 0 & Acc1014 == 1 & Ref1014 == 0] <- 1
NoAppov30.App1014[NoAppu30.15 == 0 & Acc1014 == 0 & Ref1014 == 1] <- 1
table(NoAppov30.App1014)

# Accepted & refused 2008-2012 -> 0: refused; 1: accepted
Acc1014.Ref1014 <- NA
Acc1014.Ref1014[Ref1014 == 1 & Acc1014 == 0] <- 0
Acc1014.Ref1014[Ref1014 == 0 & Acc1014 == 1] <- 1
table(Acc1014.Ref1014)


ggplot(data15, aes(RefAcc, fill = factor(ethn))) +
  geom_histogram()
# ---------------------------------- E N D ---------------------------------------------------#
