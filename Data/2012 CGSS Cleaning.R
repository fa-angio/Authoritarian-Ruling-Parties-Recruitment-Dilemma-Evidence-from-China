library(foreign)
library(flexsurv)
library(survminer)
library(ggplot2)
library(bbmle)
library(aod)
library(haven)
#---------#

CGSS12Modified <- read_sav("data source/CGSS12Modified.sav")

data12 <- CGSS12Modified

data12$age <- 2012 - data12$a3a
table(data12$age)

#age categorical
data12$ageCAT <- NA
data12$ageCAT[data12$age < 30] <- 0
data12$ageCAT[data12$age >= 30 & data12$age < 40] <- 1
data12$ageCAT[data12$age >= 40 & data12$age < 50] <- 2
data12$ageCAT[data12$age >= 50 & data12$age < 80] <- 3
data12$ageCAT[data12$age >= 80] <- 4
table(data12$ageCAT)
data12$ageCAT <- factor(data12$ageCAT)

data12$ageD <- ifelse(data12$ageCAT == 0, 1, 0)
data12$ageD <- factor(data12$ageD)

# ethnicity - a4 = ethnicity
# 1 = Han - 0 = all other ethnicity
data12$a4 <- ifelse(data12$a4 < 1, NA, data12$a4)
data12$ethn <- ifelse(data12$a4 > 1, 0, data12$a4)
table(data12$ethn)

# a18: 1= agricultural; 2: non-agricultural; 3: blueprint; 4: residential(prev. agric); 5: residential (prev. non.ag);
# 6: military; 7: no hukou
table(data12$a18)
data12$hukou <- NA
data12$hukou[data12$a18 == 1] <- 0
data12$hukou[data12$a18 == 4] <- 1
data12$hukou[data12$a18 == 2] <- 2
data12$hukou[data12$a18 == 5] <- 2
table(data12$hukou)
data12$hukou <- factor(data12$hukou)

#hukoudummy
data12$hukouD <- NA
data12$hukouD[data12$hukou == 0] <- 0
data12$hukouD[data12$hukou == 1] <- 1
data12$hukouD[data12$hukou == 2] <- 1
table(data12$hukouD)
data12$hukouD<-factor(data12$hukouD)

data12$hukouDM <- NA
data12$hukouDM[data12$hukou == 0] <- 0
data12$hukouDM[data12$hukou == 1] <- 1
data12$hukouDM[data12$hukou == 2] <- 1
hukouDM <- mean(data12$hukouDM, na.rm = T)


table(data12$a3a) #Birth
data12$birth <- ifelse(data12$a3a > 0, data12$a3a, NA)
table(data12$birth)

data12$u30 <- ifelse(data12$birth >= 1982, 1, 0)
table(data12$u30)

#---------#
table(data12$a2) #gender - 1 = male
data12$gender <- NA
data12$gender[data12$a2 == 1] <- 1
data12$gender[data12$a2 == 2] <- 0
table(data12$gender)
data12$gender <- factor(data12$gender)

data12$genderM <- NA
data12$genderM[data12$a2 == 1] <- 1
data12$genderM[data12$a2 == 2] <- 0
genderM <-  mean(data12$genderM, na.rm = T)
#---------#
table(data12$s5a) # area you live in - 1 = central area; 2 = fringe city
# 3 = urban-rural fringe; 4 = towns outside city; 5 = rural

# geography: 0 = rural; 1 = semi-urban; 2 = urban
data12$geography1 <- NA
data12$geography1[data12$s5a == 1] <- 2
data12$geography1[data12$s5a >= 2 & data12$s5a < 5] <- 1
data12$geography1[data12$s5a == 5] <- 0
table(data12$geography1)
data12$geography1 <- factor(data12$geography1)

data12$geography <- NA
data12$geography[data12$geography1 == 0] <- 0
data12$geography[data12$geography1 == 1] <- 1
data12$geography[data12$geography1 == 2] <- 1
table(data12$geography)
data12$geography <-  factor(data12$geography)


data12$geographyM <- NA
data12$geographyM[data12$s5a == 1] <- 2
data12$geographyM[data12$s5a >= 2 & data12$s5a < 5] <- 1
data12$geographyM[data12$s5a == 5] <- 0
geographyM <- mean(data12$geographyM, na.rm=T)

#Red/pink provinces: 
#RED provinces= 2 -> BEI, TIA, SHA, LIAONING, TIBET, SHANDONG, SHAANXI, SHANXI, HEBEI, HUBEI
#Light-red = 1 -> Zhejiang, Xinjiang, Qinghai, Jiangsu, Ningxia, Gansu, Inner Mong, Jilin, Hilong, Hunan, CHQ
#PINK provinces= 0 -> Sichuan, Anhui, Hainan, Henan, Fujian, Yunnan, Guangxi, Jiangxi, Guizhou, Guangdong
table(data12$s41)
data12$redgeography <- NA

#red provinces
data12$redgeography[data12$s41 == 1] <- 2 
data12$redgeography[data12$s41 == 4] <- 2
data12$redgeography[data12$s41 == 7] <- 2
data12$redgeography[data12$s41 == 10] <-2
data12$redgeography[data12$s41 == 11] <- 2
data12$redgeography[data12$s41 == 17] <- 2
data12$redgeography[data12$s41 == 21] <- 2
data12$redgeography[data12$s41 == 25] <- 2
data12$redgeography[data12$s41 == 27] <- 2
data12$redgeography[data12$s41 == 29] <- 2

#light-red provinces
data12$redgeography[data12$s41 == 3] <- 1 
data12$redgeography[data12$s41 == 5] <- 1
data12$redgeography[data12$s41 == 8] <- 1
data12$redgeography[data12$s41 == 14] <- 1
data12$redgeography[data12$s41 == 15] <- 1
data12$redgeography[data12$s41 == 19] <- 1
data12$redgeography[data12$s41 == 22] <- 1
data12$redgeography[data12$s41 == 23] <- 1
data12$redgeography[data12$s41 == 28] <- 1
data12$redgeography[data12$s41 == 30] <- 1
data12$redgeography[data12$s41 == 31] <- 1

#pink provinces
data12$redgeography[data12$s41 == 12] <- 0
data12$redgeography[data12$s41 == 2] <- 0
data12$redgeography[data12$s41 == 6] <- 0
data12$redgeography[data12$s41 == 9] <- 0
data12$redgeography[data12$s41 == 13] <- 0
data12$redgeography[data12$s41 == 16] <- 0
data12$redgeography[data12$s41 == 18] <- 0
data12$redgeography[data12$s41 == 20] <-0
data12$redgeography[data12$s41 == 24] <-0
data12$redgeography[data12$s41 == 26] <-0

table(data12$redgeography)

data12$redgeography <- factor(data12$redgeography)

redgeographyM <- mean(data12$redgeographyM, na.rm = T)

## per capita Gross regional product
# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data12$GRP_pc <- NA
data12$GRP_pc[data12$s41 == 1] <- 2
data12$GRP_pc[data12$s41 == 4] <- 2
data12$GRP_pc[data12$s41 == 7] <- 2
data12$GRP_pc[data12$s41 == 15] <- 2
data12$GRP_pc[data12$s41 == 19] <- 2
data12$GRP_pc[data12$s41 == 3] <- 2
data12$GRP_pc[data12$s41 == 27] <- 2
data12$GRP_pc[data12$s41 == 24] <- 2
data12$GRP_pc[data12$s41 == 12] <- 2
data12$GRP_pc[data12$s41 == 10] <- 2

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Xinjiang, Hunan, Hebei,Shanxi , Heilongjiang
data12$GRP_pc[data12$s41 == 5] <- 1
data12$GRP_pc[data12$s41 == 28] <- 1
data12$GRP_pc[data12$s41 == 21] <- 1
data12$GRP_pc[data12$s41 == 29] <- 1
data12$GRP_pc[data12$s41 == 8] <- 1
data12$GRP_pc[data12$s41 == 14] <- 1
data12$GRP_pc[data12$s41 == 22] <- 1
data12$GRP_pc[data12$s41 == 17] <- 1
data12$GRP_pc[data12$s41 == 11] <- 1
data12$GRP_pc[data12$s41 == 31] <- 1

#poorest 10 provinces
#Hainan, Henan, Sichuan, Qinghai , Jiangxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, 
#Gangsu
data12$GRP_pc[data12$s41 == 20] <- 0
data12$GRP_pc[data12$s41 == 18] <- 0
data12$GRP_pc[data12$s41 == 6] <- 0
data12$GRP_pc[data12$s41 == 30] <- 0
data12$GRP_pc[data12$s41 == 16] <- 0
data12$GRP_pc[data12$s41 == 9] <- 0
data12$GRP_pc[data12$s41 == 13] <- 0
data12$GRP_pc[data12$s41 == 25] <- 0
data12$GRP_pc[data12$s41 == 2] <- 0
data12$GRP_pc[data12$s41 == 26] <- 0
data12$GRP_pc[data12$s41 == 23] <- 0

table(data12$GRP_pc)
data12$GRP_pc <- factor(data12$GRP_pc)
#---------#
table(data12$a7a) # 1 = no edu; 2-8 = primary/secondary edu; 10, 12-13 = higher edu

# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data12$education1 <- NA
#data12$education1[data12$a7a == 1] <- 0
#data12$education1[data12$a7a == 2] <- 1
data12$education1[data12$a7a == 3] <- 1
data12$education1[data12$a7a == 4] <- 2
data12$education1[data12$a7a == 5] <- 3
data12$education1[data12$a7a == 6] <- 4
data12$education1[data12$a7a == 7] <- 5
data12$education1[data12$a7a == 8] <- 6
data12$education1[data12$a7a == 9] <- 7
data12$education1[data12$a7a == 10] <- 8
data12$education1[data12$a7a == 11] <- 9
data12$education1[data12$a7a == 12] <- 10
data12$education1[data12$a7a == 13] <- 121
table(data12$education1)

data12$educationM <- mean(data12$education1, na.rm= T)


data12$edu <- NA
data12$edu[data12$a7a == 1] <- 0
data12$edu[data12$a7a >= 2 & data12$a7a <= 8] <- 1
data12$edu[data12$a7a == 10] <- 2
data12$edu[data12$a7a == 12] <- 2
data12$edu[data12$a7a == 13] <- 2
table(data12$edu)
data12$edu <- factor(data12$edu)

data12$edua <- NA
data12$edua[data12$a7a == 3] <- 0
data12$edua[data12$a7a == 4] <- 1
data12$edua[data12$a7a == 6] <- 1
data12$edua[data12$a7a == 7] <- 2
data12$edua[data12$a7a == 10] <- 3
data12$edua[data12$a7a == 12] <- 3
data12$edua[data12$a7a == 13] <- 3
table(data12$edua)


data12$eduM <- NA
data12$eduM[data12$a7a == 1] <- 0
data12$eduM[data12$a7a >= 2 & data12$a7a <= 8] <- 1
data12$eduM[data12$a7a == 10] <- 2
data12$eduM[data12$a7a == 12] <- 2
data12$eduM[data12$a7a == 13] <- 2
eduM <- mean(data12$eduM, na.rm = T)
# two levels education: 0 = primary/secondary edu; 1 = higher edu
data12$HEdu <- NA
data12$HEdu[data12$edu == 1] <- 0
data12$HEdu[data12$edu == 2] <- 1
table(data12$HEdu)
data12$HEdu <- factor(data12$HEdu)
#---------#
table(data12$a9a)
data12$CCPapp <- ifelse(data12$a9a > 0, data12$a9a, NA)
data12$CCPapp[is.na(data12$CCPapp)] <- 0
table(data12$CCPapp)

#---------#
table(data12$a10a)
data12$CCPmemb <- ifelse(data12$a10a > 0, data12$a10a, NA)
data12$CCPmemb[is.na(data12$CCPmemb)] <- 0
table(data12$CCPmemb)
#---------#
table(data12$a89b) #father education
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data12$FAedu <- NA
data12$FAedu[data12$a89b == 1] <- 0
data12$FAedu[data12$a89b >= 2 & data12$a89b <= 8] <- 1
data12$FAedu[data12$a89b == 10] <- 2
data12$FAedu[data12$a89b == 12] <- 2
data12$FAedu[data12$a89b == 13] <- 2
table(data12$FAedu)
data12$FAedu <- factor(data12$FAedu)

data12$FAeduM <- NA
data12$FAeduM[data12$a89b == 1] <- 0
data12$FAeduM[data12$a89b >= 2 & data12$a89b <= 8] <- 1
data12$FAeduM[data12$a89b == 10] <- 2
data12$FAeduM[data12$a89b == 12] <- 2
data12$FAeduM[data12$a89b == 13] <- 2
FAeduM <- mean(data12$FAeduM, na.rm = T)

data12$FAHighEdu <- NA
data12$FAHighEdu[data12$FAedu == 0] <- 0
data12$FAHighEdu[data12$FAedu == 1] <- 0
data12$FAHighEdu[data12$FAedu == 2] <- 1
data12$FAHighEdu <- factor(data12$FAHighEdu)

data12$FAHighEduM <- NA
data12$FAHighEduM[data12$FAedu == 0] <- 0
data12$FAHighEduM[data12$FAedu == 1] <- 0
data12$FAHighEduM[data12$FAedu == 2] <- 1
FAHighEduM <- mean(data12$FAHighEduM, na.rm = T)


#---------#
table(data12$a89c) # father politics -> 1: CCP; 2: Demo parties; 3 = CYL; 4: masses
# 1= CCP; 0 = no CCP
data12$FACCP <- NA
data12$FACCP[data12$a89c == 1] <- 1
data12$FACCP[data12$a89c > 1] <- 0
table(data12$FACCP)
data12$FACCP <- factor(data12$FACCP)

data12$FACCPM <- NA
data12$FACCPM[data12$a89c == 1] <- 1
data12$FACCPM[data12$a89c > 1] <- 0
FACCPM <- mean(data12$FACCPM, na.rm = T)


#---------#
table(data12$a90b) # mother edu
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data12$MOedu <- NA
data12$MOedu[data12$a90b == 1] <- 0
data12$MOedu[data12$a90b >= 2 & data12$a90b <= 8] <- 1
data12$MOedu[data12$a90b == 10] <- 2
data12$MOedu[data12$a90b == 12] <- 2
data12$MOedu[data12$a90b == 13] <- 2
table(data12$MOedu)
data12$MOedu <- factor(data12$MOedu)

data12$MOeduM <- NA
data12$MOeduM[data12$a90b == 1] <- 0
data12$MOeduM[data12$a90b >= 2 & data12$a90b <= 8] <- 1
data12$MOeduM[data12$a90b == 10] <- 2
data12$MOeduM[data12$a90b == 12] <- 2
data12$MOeduM[data12$a90b == 13] <- 2
MOeduM <- mean(data12$MOeduM, na.rm = T)


data12$MOHighEdu <- NA
data12$MOHighEdu[data12$MOedu == 0] <- 0
data12$MOHighEdu[data12$MOedu == 1] <- 0
data12$MOHighEdu[data12$MOedu == 2] <- 1
data12$MOHighEdu <- factor(data12$MOHighEdu)

data12$MOHighEduM <- NA
data12$MOHighEduM[data12$MOedu == 0] <- 0
data12$MOHighEduM[data12$MOedu == 1] <- 0
data12$MOHighEduM[data12$MOedu == 2] <- 1
MOHighEduM <- mean(data12$MOHighEduM,  na.rm=T)
MOHighEduM

#---------#
table(data12$a90c) # mother poltics 
# 1= CCP; 0 = no CCP
data12$MOCCP <- NA
data12$MOCCP[data12$a90c == 1] <- 1
data12$MOCCP[data12$a90c > 1] <- 0
table(data12$MOCCP)

data12$MOCCPM <- NA
data12$MOCCPM[data12$a90c == 1] <- 1
data12$MOCCPM[data12$a90c > 1] <- 0
MOCCPM <- mean(data12$MOCCPM, na.rm = T)

data12$ParentsHEdu <- NA
data12$ParentsHEdu[data12$MOHighEdu == 0 & data12$FAHighEdu == 0] <- 0
data12$ParentsHEdu[data12$MOHighEdu == 1 & data12$FAHighEdu == 0] <- 1
data12$ParentsHEdu[data12$MOHighEdu == 0 & data12$FAHighEdu == 1] <- 1
data12$ParentsHEdu[data12$MOHighEdu == 1 & data12$FAHighEdu == 1] <- 1
table(data12$ParentsHEdu)
data12$ParentsHEdu <- factor(data12$ParentsHEdu)
#---------#
# 0 = no parents CCP; 1 = at least 1 parent CCP; 2 = both parents CCP
data12$ParentsCCP <- NA
data12$ParentsCCP[data12$FACCP == 0 & data12$MOCCP == 0] <- 0
data12$ParentsCCP[data12$FACCP == 1 & data12$MOCCP == 0] <- 1
data12$ParentsCCP[data12$FACCP == 0 & data12$MOCCP == 1] <- 1
data12$ParentsCCP[data12$FACCP == 1 & data12$MOCCP == 1] <- 2
table(data12$ParentsCCP)
data12$ParentsCCP <- factor(data12$ParentsCCP)

data12$ParentsCCPM <- NA
data12$ParentsCCPM[data12$FACCP == 0 & data12$MOCCP == 0] <- 0
data12$ParentsCCPM[data12$FACCP == 1 & data12$MOCCP == 0] <- 1
data12$ParentsCCPM[data12$FACCP == 0 & data12$MOCCP == 1] <- 1
data12$ParentsCCPM[data12$FACCP == 1 & data12$MOCCP == 1] <- 2
ParentsCCPM <- mean(data12$ParentsCCPM, na.rm = T)

data12$familyPoli <- NA
data12$familyPoli[data12$ParentsCCP == 0] <- 0
data12$familyPoli[data12$ParentsCCP == 1] <- 1
data12$familyPoli[data12$ParentsCCP == 2] <- 1
data12$familyPoli <- factor(data12$familyPoli)
table(data12$familyPoli)
data12$familyPoli <- factor(data12$familyPoli)
#---------#
# GDP per capita 2009 = 5618$ * 6.46 (mean exchange rate US-RMB 2011) = 36292 RMB
summary(data12$a8a)

data12$wealth <- ifelse(data12$a8a >= 1000 & data12$a8a < 9999990, data12$a8a, NA)
summary(data12$wealth)

data12$LNwealth <- log(data12$wealth)
summary(data12$LNwealth)

#increase every 20k which is 1/2 of the gdp per capita ~40k
data12$GDPpc_wealth <- NA
data12$GDPpc_wealth[data12$wealth < 17000] <- 0
data12$GDPpc_wealth[data12$wealth >= 17000 & data12$wealth < 27000] <- 1
data12$GDPpc_wealth[data12$wealth >= 27000 & data12$wealth < 50000] <- 2
data12$GDPpc_wealth[data12$wealth >= 50000] <- 3
table(data12$GDPpc_wealth)
data12$GDPpc_wealth <- factor(data12$GDPpc_wealth)

#NO FACTOR
data12$GDPpc_wealthA <- NA
data12$GDPpc_wealthA[data12$wealth < 20500] <- 0
data12$GDPpc_wealthA[data12$wealth >= 20500 & data12$wealth < 27000] <- 1
data12$GDPpc_wealthA[data12$wealth >= 27000 & data12$wealth < 50000] <- 2
data12$GDPpc_wealthA[data12$wealth >= 50000] <- 3
table(data12$GDPpc_wealthA)


data12$GDPpc_wealthA1 <- NA
data12$GDPpc_wealthA1[data12$wealth > 4500 & data12$wealth < 17000] <- 0
data12$GDPpc_wealthA1[data12$wealth >= 17000 & data12$wealth < 27000] <- 1
data12$GDPpc_wealthA1[data12$wealth >= 27000 & data12$wealth < 50000] <- 2
data12$GDPpc_wealthA1[data12$wealth >= 50000] <- 3
table(data12$GDPpc_wealthA1)
data12$GDPpc_wealthA1 <- factor(data12$GDPpc_wealthA1)

data12$GDPpc_wealthA2 <- NA
data12$GDPpc_wealthA2[data12$wealth < 20500] <- 0
data12$GDPpc_wealthA2[data12$wealth >= 20500 & data12$wealth < 27000] <- 1
data12$GDPpc_wealthA2[data12$wealth >= 27000 & data12$wealth < 50000] <- 2
data12$GDPpc_wealthA2[data12$wealth >= 50000] <- 3
table(data12$GDPpc_wealthA2)
data12$GDPpc_wealthA2 <- factor(data12$GDPpc_wealthA2)


gghistogram(data12$LNwealth)
gghistogram(data12$GDPpc_wealthA)

quantile(data12$LNwealth, c(.50, .60, .70, .80, .90, .92, .999), na.rm = T)
quantile(data12$GDPpc_wealthA, c(.30, .35, .40, .41, .42, .43, .45, .46, .47, .48, .49,
                                 .50, .51, .52, .53, .55, .60, .61, .62, .63, .64, .65,  .70, .75, .80, .88,.89,
                                 .90, .92, .999), na.rm = T)

#Work experience and current job
head(data12$a58)
#1: Currently engaged in non-agricultural work; 
#2: Currently working in agriculture, once had non-agricultural work;
#3: Currently working in farming, no non-agricultural work ; 
#4: No job at the moment, and only farming;
#5: No job at the moment, have had a non-agricultural job;
#6: Never worked

head(data12$a59a) - # breakdown data12$a58)
  #1 I am the boss (or partner)
  #2 Individual industrial and commercial households
  #3 Employed by others (with regular employer)
  #4 Labor workers/labor dispatch personnel
  #5 Part-time workers, casual workers (employees without regular employers)
  #6 Work/help in my own business/enterprise and receive salary
  #7 Working/helping in my own business/enterprise without receiving salary
  #8 Freelancers
  #9 other
  
  #Job --> 2 = unemployed; 1 = rural work; 3 = freelance/casual work; 4 = self-emplyed;
# 0 = labor worker; 5 = clerk; 6 = private business; 7 = boss
data12$Job <- NA
data12$Job[data12$a58 == 2] <- 1
data12$Job[data12$a58 == 3] <- 1
data12$Job[data12$a58 == 4] <- 1
data12$Job[data12$a58 == 5] <- 2
data12$Job[data12$a58 == 6] <-  2
data12$Job[data12$a59a == 1] <- 7
data12$Job[data12$a59a == 2] <- 6
data12$Job[data12$a59a == 3] <- 5
data12$Job[data12$a59a == 4] <- 0
data12$Job[data12$a59a == 5] <- 3
data12$Job[data12$a59a == 6] <- 4
data12$Job[data12$a59a == 7] <- 2
data12$Job[data12$a59a == 8] <- 3
table(data12$Job)
data12$JobF <- factor(data12$Job)

#Jobnew --> 0 = white collar; 1 = labor; 2 = rural work
data12$Jobnew <- NA
data12$Jobnew[data12$Job == 0] <- 1
data12$Jobnew[data12$Job == 3] <- 1
data12$Jobnew[data12$Job == 2] <- 1
data12$Jobnew[data12$Job == 4] <- 1
data12$Jobnew[data12$Job == 5] <- 0
data12$Jobnew[data12$Job == 6] <- 0
#data15$Jobnew[data15$Job == 1] <- 2
data12$Jobnew[data12$Job == 7] <- 0
table(data12$Jobnew)

#JobClerk
# 2 = rural work & unemployed; 1 = Wokers; 0 = clerk ; 3 = Managment & private business
data12$JobKey <- NA
data12$JobKey[data12$Job == 1] <- 2
data12$JobKey[data12$Job == 2] <- 1
data12$JobKey[data12$Job == 3] <- 1
data12$JobKey[data12$Job == 4] <- 1
data12$JobKey[data12$Job == 5] <- 0
data12$JobKey[data12$Job == 0] <- 1
data12$JobKey[data12$Job == 6] <- 3
data12$JobKey[data12$Job == 7] <- 3
table(data12$JobKey)
data12$JobKey <- factor(data12$JobKey)

#Job urban
# 0= clerk; 1 = labor worker; 2 = business
data12$UrbanJob <- NA
data12$UrbanJob[data12$Job == 2] <- 1
data12$UrbanJob[data12$Job == 3] <- 1
data12$UrbanJob[data12$Job == 4] <- 1
data12$UrbanJob[data12$Job == 5] <- 0
data12$UrbanJob[data12$Job == 0] <- 1
data12$UrbanJob[data12$Job == 6] <- 2
data12$UrbanJob[data12$Job == 7] <- 2
table(data12$UrbanJob)
data12$UrbanJob <- factor(data12$UrbanJob)


data12$JobD <- NA
data12$JobD[data12$Job == 0] <- 0
data12$JobD[data12$Job == 1] <- 1
data12$JobD[data12$Job > 1] <- 2
data12$JobD <- factor(data12$JobD)

JobM1 <- mean(data12$Job, na.rm = T)
summary(JobM)
#--------------------------------------------------------------------------------------
# 1st: cox hazard analysis on the age citizens' application to the CCP #

ageApp12 <- data12$CCPapp - data12$birth
table(ageApp12)

data12$age18App <- ifelse(ageApp12 >= 18, ageApp12, NA)
table(data12$age18App)

data12$age18App[is.na(data12$age18App)] <- 0

time12 <- data12$age18App - 18
table(time12)
#------------------------------------------------------------------------------------
# event
# Creating a dummy for those who applied 2005-2009 and those who did not applied ever

data12$time0711q <- NA
data12$time0711q[data12$CCPapp >= 2007] <- 1
data12$time0711q[data12$CCPapp == 0] <- 0
table(data12$time0711q)

event12q <- data12$time0711q
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------ //
#1.a CCP acceptance age likelihood

Acc0711 <- ifelse(data12$CCPmemb >= 2007, 1, 0)
table(Acc0711)

event12.a <- Acc0711
table(event12.a)
#------------------------------------------------------------------------------------ 
ageAcc12 <- data12$CCPmemb - data12$birth
table(ageAcc12)

age18Acc12 <- ifelse(ageApp12 >= 18, ageApp12, NA)
table(age18Acc12)

time12.a <- age18Acc12 - 17
table(time12.a)
#------------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------------ 
# Set of dependent variables
data12$Ref_Acc_u30 <- NA
data12$Ref_Acc_u30[data12$CCPapp >= 2007 & data12$birth >= 1982 & data12$CCPmemb >= 2007] <- 1
data12$Ref_Acc_u30[data12$CCPapp < 2007 & data12$birth >= 1982 & data12$CCPmemb >= 2007] <- 2
data12$Ref_Acc_u30[data12$CCPapp >= 2007 & data12$birth < 1982 & data12$CCPmemb >= 2007] <- 3
data12$Ref_Acc_u30[data12$CCPapp < 2007 & data12$birth < 1982 & data12$CCPmemb >= 2007] <- 4
data12$Ref_Acc_u30[data12$CCPapp < 2007 & data12$birth >= 1982 & data12$CCPmemb < 2007] <- 5
data12$Ref_Acc_u30[data12$CCPapp == 0 & data12$birth >=  1982 & data12$CCPmemb == 0] <- 6
data12$Ref_Acc_u30[data12$CCPapp == 0 & data12$birth <  1982 & data12$CCPmemb == 0] <- 7
data12$Ref_Acc_u30[data12$CCPapp < 2007 & data12$CCPapp > 0 & data12$birth < 1982 & data12$CCPmemb == 0] <- 8
data12$Ref_Acc_u30[data12$CCPapp >= 2007 & data12$birth >= 1982 & data12$CCPmemb == 0] <- 9
data12$Ref_Acc_u30[data12$CCPapp >= 2007 & data12$birth <  1982 & data12$CCPmemb == 0] <- 10

data12$Ref_Acc_u30[is.na(data12$Ref_Acc_u30)] <- 0

table(data12$Ref_Acc_u30)
data12$Ref_Acc_u30 <- factor(data12$Ref_Acc_u30)

#new DV - Accepted refused
data12$RefAcc <- NA
data12$RefAcc[data12$CCPapp >= 2007 & data12$CCPmemb >= 0] <- 0
data12$RefAcc[data12$CCPapp >= 2007 & data12$CCPmemb >= 2007] <- 1
table(data12$RefAcc)

#new DV - Applicants or not
data12$AppNo <- NA
data12$AppNo[data12$CCPapp == 0] <- 0
data12$AppNo[data12$CCPapp >= 2007] <- 1
table(data12$AppNo)

# creating dummies for the tests and the probit models

NoApp12 <- ifelse(data12$Ref_Acc_u30 == 7, 1, 0)
table(NoApp12)
NoAppu30.12 <- ifelse(data12$Ref_Acc_u30 == 6, 1, 0)
table(NoAppu30.12)
Ref0711 <- ifelse(data12$Ref_Acc_u30 == 9 |
                    data12$Ref_Acc_u30 == 10, 1, 0)
table(Ref0711)
Acc0711 <- ifelse(data12$Ref_Acc_u30 == 1 |
                    data12$Ref_Acc_u30 == 3, 1, 0)
table(Acc0711)

# No applicants u30 and those refused 2005-2009
NoAppu30.Ref0711 <- NA
NoAppu30.Ref0711[NoAppu30.12 == 1 & Ref0711 == 0] <- 0
NoAppu30.Ref0711[NoAppu30.12 == 0 & Ref0711 == 1] <- 1
table(NoAppu30.Ref0711)

# No applicants u30 and those accepted 2005-2009
NoAppu30.Acc0711 <- NA
NoAppu30.Acc0711[NoAppu30.12 == 1 & Acc0711 == 0] <- 0
NoAppu30.Acc0711[NoAppu30.12 == 0 & Acc0711 == 1] <- 1
table(NoAppu30.Acc0711)

# No applicants u30 and applicants
NoAppu30.App0711 <- NA
NoAppu30.App0711[NoAppu30.12 == 1 & Acc0711 == 0 & Ref0711 == 0] <- 0
NoAppu30.App0711[NoAppu30.12 == 0 & Acc0711 == 1 & Ref0711 == 0] <- 1
NoAppu30.App0711[NoAppu30.12 == 0 & Acc0711 == 0 & Ref0711 == 1] <- 1
table(NoAppu30.App0711)

# Accepted & refused 2005-2009 -> 0: refused; 1: accepted
Acc0711.Ref0711 <- NA
Acc0711.Ref0711[Ref0711 == 1 & Acc0711 == 0] <- 0
Acc0711.Ref0711[Ref0711 == 0 & Acc0711 == 1] <- 1
table(Acc0711.Ref0711)

#----------------------------------------- E N D --------------------------------------#
