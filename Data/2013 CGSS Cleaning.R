library(foreign)
library(flexsurv)
library(survminer)
library(ggplot2)
library(bbmle)
library(aod)
#---------#

CGSS13_modified <- read_sav("CGSS13_modified.sav")

data13 <- CGSS13_modified

data13$age <- 2013 - data13$a3a
table(data13$age)

#age categorical
data13$ageCAT <- NA
data13$ageCAT[data13$age < 30] <- 0
data13$ageCAT[data13$age >= 30 & data13$age < 40] <- 1
data13$ageCAT[data13$age >= 40 & data13$age < 50] <- 2
data13$ageCAT[data13$age >= 50 & data13$age < 80] <- 3
data13$ageCAT[data13$age >= 80] <- 4
table(data13$ageCAT)
data13$ageCAT <- factor(data13$ageCAT)

data13$ageD <- ifelse(data13$ageCAT == 0, 1, 0)
data13$ageD <- factor(data13$ageD)

# ethnicity - a4 = ethnicity
# 1 = Han - 0 = all other ethnicity
data13$a4 <- ifelse(data13$a4 < 1, NA, data13$a4)
data13$ethn <- ifelse(data13$a4 > 1, 0, data13$a4)
table(data13$ethn)

# a18: 1= agricultural; 2: non-agricultural; 3: blueprint; 4: residential(prev. agric); 5: residential (prev. non.ag);
# 6: military; 7: no hukou
table(data13$a18)
data13$hukou <- NA
data13$hukou[data13$a18 == 1] <- 0
data13$hukou[data13$a18 == 4] <- 1
data13$hukou[data13$a18 == 2] <- 2
data13$hukou[data13$a18 == 5] <- 2
table(data13$hukou)
data13$hukou <- factor(data13$hukou)

#hukoudummy
data13$hukouD <- NA
data13$hukouD[data13$hukou == 0] <- 0
data13$hukouD[data13$hukou == 1] <- 1
data13$hukouD[data13$hukou == 2] <- 1
table(data13$hukouD)
data13$hukouD<-factor(data13$hukouD)

data13$hukouDM <- NA
data13$hukouDM[data13$hukou == 0] <- 0
data13$hukouDM[data13$hukou == 1] <- 1
data13$hukouDM[data13$hukou == 2] <- 1
hukouDM <- mean(data13$hukouDM, na.rm = T)

table(data13$b2) #Soc Eco level 3 years ago

# SocEco: 0 = decreased; 1 = same level; 2 = increased
data13$SocEco3YRago <- NA
data13$SocEco3YRago[data13$b2 == 1] <- 2
data13$SocEco3YRago[data13$b2 == 2 & 9] <- 1
data13$SocEco3YRago[data13$b2 == 3] <- 0
table(data13$SocEco3YRago)
data13$SocEco3YRago <- factor(data13$SocEco3YRago)

data13$SocEco3YRagoM <- NA
data13$SocEco3YRagoM[data13$b2 == 1] <- 2
data13$SocEco3YRagoM[data13$b2 == 2 & 9] <- 1
data13$SocEco3YRagoM[data13$b2 == 3] <- 0
data13$SocEco3YRagoM <- mean(data13$SocEco3YRagoM, na.rm =T)

#---------#

table(data13$a3a) #Birth
data13$birth <- ifelse(data13$a3a > 0, data13$a3a, NA)
table(data13$birth)

data13$u30 <- ifelse(data13$birth >= 1983, 1, 0)
table(data13$u30)

#---------#
table(data13$a2) #gender - 1 = male
data13$gender <- NA
data13$gender[data13$a2 == 1] <- 1
data13$gender[data13$a2 == 2] <- 0
table(data13$gender)
data13$gender <- factor(data13$gender)

data13$genderM <- NA
data13$genderM[data13$a2 == 1] <- 1
data13$genderM[data13$a2 == 2] <- 0
genderM <- mean(data13$genderM, na.rm = T)
summary(genderM)
#---------#
table(data13$s5a) # area you live in - 1 = central area; 2 = fringe city
# 3 = urban-rural fringe; 4 = towns outside city; 5 = rural

# geography: 0 = rural; 1 = semi-urban; 2 = urban
data13$geography1 <- NA
data13$geography1[data13$s5a == 1] <- 2
data13$geography1[data13$s5a >= 2 & data13$s5a < 5] <- 1
data13$geography1[data13$s5a == 5] <- 0
table(data13$geography1)
data13$geography1 <- factor(data13$geography1)

# geography: 0 = rural; 1 = urban
data13$geography <- NA
data13$geography[data13$geography1 == 0] <- 0
data13$geography[data13$geography1 == 1] <- 1
data13$geography[data13$geography1 == 2] <- 1
table(data13$geography)
data13$geography <-  factor(data13$geography)

data13$geographyM <- NA
data13$geographyM[data13$geography1 == 0] <- 0
data13$geographyM[data13$geography1 == 1] <- 1
data13$geographyM[data13$geography1 == 2] <- 1
table(data13$geographyM)
data13$geographyM <-  mean(data13$geographyM, na.rm = T)
table(data13$geographyM)
#Red/pink provinces: 
#RED provinces= 2 -> BEI, TIA, SHA, LIAONING, TIBET, SHANDONG, SHAANXI, SHANXI, HEBEI, HUBEI
#Light-red = 1 -> Zhejiang, Xinjiang, Qinghai, Jiangsu, Ningxia, Gansu, Inner Mong, Jilin, Hilong, Hunan, CHQ
#PINK provinces= 0 -> Sichuan, Anhui, Hainan, Henan, Fujian, Yunnan, Guangxi, Jiangxi, Guizhou, Guangdong

table(data13$s41)
data13$redgeography <- NA

#red provinces
data13$redgeography[data13$s41 == 1] <- 2 
data13$redgeography[data13$s41 == 4] <- 2
data13$redgeography[data13$s41 == 7] <- 2
data13$redgeography[data13$s41 == 10] <-2
data13$redgeography[data13$s41 == 11] <- 2
data13$redgeography[data13$s41 == 17] <- 2
data13$redgeography[data13$s41 == 21] <- 2
data13$redgeography[data13$s41 == 25] <- 2
data13$redgeography[data13$s41 == 27] <- 2
data13$redgeography[data13$s41 == 29] <- 2

#light-red provinces
data13$redgeography[data13$s41 == 3] <- 1 
data13$redgeography[data13$s41 == 5] <- 1
data13$redgeography[data13$s41 == 8] <- 1
data13$redgeography[data13$s41 == 14] <- 1
data13$redgeography[data13$s41 == 15] <- 1
data13$redgeography[data13$s41 == 19] <- 1
data13$redgeography[data13$s41 == 22] <- 1
data13$redgeography[data13$s41 == 23] <- 1
data13$redgeography[data13$s41 == 28] <- 1
data13$redgeography[data13$s41 == 30] <- 1
data13$redgeography[data13$s41 == 31] <- 1

#pink provinces
data13$redgeography[data13$s41 == 12] <- 0
data13$redgeography[data13$s41 == 2] <- 0
data13$redgeography[data13$s41 == 6] <- 0
data13$redgeography[data13$s41 == 9] <- 0
data13$redgeography[data13$s41 == 13] <- 0
data13$redgeography[data13$s41 == 16] <- 0
data13$redgeography[data13$s41 == 18] <- 0
data13$redgeography[data13$s41 == 20] <-0
data13$redgeography[data13$s41 == 24] <-0
data13$redgeography[data13$s41 == 26] <-0

table(data13$redgeography)

data13$redgeography <- factor(data13$redgeography)

redgeographyM <- mean(data13$redgeography, na.rm = T)

## per capita Gross regional product
# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data13$GRP_pc <- NA
data13$GRP_pc[data13$s41 == 1] <- 2
data13$GRP_pc[data13$s41 == 4] <- 2
data13$GRP_pc[data13$s41 == 7] <- 2
data13$GRP_pc[data13$s41 == 15] <- 2
data13$GRP_pc[data13$s41 == 19] <- 2
data13$GRP_pc[data13$s41 == 3] <- 2
data13$GRP_pc[data13$s41 == 27] <- 2
data13$GRP_pc[data13$s41 == 24] <- 2
data13$GRP_pc[data13$s41 == 12] <- 2
data13$GRP_pc[data13$s41 == 10] <- 2

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Xinjiang, Hunan, Hebei, Qinghai, Heilongjiang
data13$GRP_pc[data13$s41 == 5] <- 1
data13$GRP_pc[data13$s41 == 28] <- 1
data13$GRP_pc[data13$s41 == 21] <- 1
data13$GRP_pc[data13$s41 == 29] <- 1
data13$GRP_pc[data13$s41 == 8] <- 1
data13$GRP_pc[data13$s41 == 14] <- 1
data13$GRP_pc[data13$s41 == 22] <- 1
data13$GRP_pc[data13$s41 == 17] <- 1
data13$GRP_pc[data13$s41 == 11] <- 1
data13$GRP_pc[data13$s41 == 31] <- 1

#poorest 10 provinces
#Hainan, Henan, Sichuan, Shanxi, Jiangxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, 
#Gangsu
data13$GRP_pc[data13$s41 == 20] <- 0
data13$GRP_pc[data13$s41 == 18] <- 0
data13$GRP_pc[data13$s41 == 6] <- 0
data13$GRP_pc[data13$s41 == 30] <- 0
data13$GRP_pc[data13$s41 == 16] <- 0
data13$GRP_pc[data13$s41 == 9] <- 0
data13$GRP_pc[data13$s41 == 13] <- 0
data13$GRP_pc[data13$s41 == 25] <- 0
data13$GRP_pc[data13$s41 == 2] <- 0
data13$GRP_pc[data13$s41 == 26] <- 0
data13$GRP_pc[data13$s41 == 23] <- 0

table(data13$GRP_pc)
data13$GRP_pcM <- mean(data13$GRP_pc, na.rm = T)
table(data13$GRP_pcM)
data13$GRP_pc <- factor(data13$GRP_pc)

#---------#
table(data13$a7a) # 1 = no edu; 2-8 = primary/secondary edu; 10, 12-13 = higher edu

# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data13$edu <- NA
data13$edu[data13$a7a == 1] <- 0
data13$edu[data13$a7a >= 2 & data13$a7a <= 8] <- 1
data13$edu[data13$a7a == 10] <- 2
data13$edu[data13$a7a == 12] <- 2
data13$edu[data13$a7a == 13] <- 2
table(data13$edu)
data13$edu <- factor(data13$edu)

data13$eduM <- NA
data13$eduM[data13$a7a == 1] <- 0
data13$eduM[data13$a7a >= 2 & data13$a7a <= 8] <- 1
data13$eduM[data13$a7a == 10] <- 2
data13$eduM[data13$a7a == 12] <- 2
data13$eduM[data13$a7a == 13] <- 2
eduM <- mean(data13$eduM, na.rm = T)

data13$education1 <- NA
#data13$education1[data13$a7a == 1] <- 0
#data13$education1[data13$a7a == 2] <- 1
data13$education1[data13$a7a == 3] <- 1
data13$education1[data13$a7a == 4] <- 2
data13$education1[data13$a7a == 5] <- 3
data13$education1[data13$a7a == 6] <- 4
data13$education1[data13$a7a == 7] <- 5
data13$education1[data13$a7a == 8] <- 6
data13$education1[data13$a7a == 9] <- 7
data13$education1[data13$a7a == 10] <- 8
data13$education1[data13$a7a == 11] <- 9
data13$education1[data13$a7a == 12] <- 10
data13$education1[data13$a7a == 13] <- 11
table(data13$education1)

data13$educationM <- mean(data13$education1, na.rm= T)

data13$edua <- NA
data13$edua[data13$a7a == 3] <- 0
data13$edua[data13$a7a == 4] <- 1
data13$edua[data13$a7a == 6] <- 1
data13$edua[data13$a7a == 7] <- 2
data13$edua[data13$a7a == 10] <- 3
data13$edua[data13$a7a == 12] <- 3
data13$edua[data13$a7a == 13] <- 3
table(data13$edua)
# two levels education: 0 = primary/secondary edu; 1 = higher edu

data13$HEdu <- NA
data13$HEdu[data13$edu == 1] <- 0
data13$HEdu[data13$edu == 2] <- 1
table(data13$HEdu)
data13$HEdu <- factor(data13$HEdu)

data13$HEduM <- NA
data13$HEduM[data13$edu == 1] <- 0
data13$HEduM[data13$edu == 2] <- 1
HEduM <- mean(data13$HEduM, na.rm = T)

#---------#
table(data13$CCPapp)
data13$CCPapp <- ifelse(data13$CCPapp > 0, data13$CCPapp, NA)
data13$CCPapp[is.na(data13$CCPapp)] <- 0

#---------#
table(data13$CCPmemb)
data13$CCPmemb <- ifelse(data13$CCPmemb > 0, data13$CCPmemb, NA)
data13$CCPmemb[is.na(data13$CCPmemb)] <- 0

#---------#
table(data13$a89b) #father education
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data13$FAedu <- NA
data13$FAedu[data13$a89b == 1] <- 0
data13$FAedu[data13$a89b >= 2 & data13$a89b <= 8] <- 1
data13$FAedu[data13$a89b == 10] <- 2
data13$FAedu[data13$a89b == 12] <- 2
data13$FAedu[data13$a89b == 13] <- 2
table(data13$FAedu)
data13$FAedu <- factor(data13$FAedu)

data13$FAeduM <- NA
data13$FAeduM[data13$a89b == 1] <- 0
data13$FAeduM[data13$a89b >= 2 & data13$a89b <= 8] <- 1
data13$FAeduM[data13$a89b == 10] <- 2
data13$FAeduM[data13$a89b == 12] <- 2
data13$FAeduM[data13$a89b == 13] <- 2
FAeduM <- mean(data13$FAeduM, na.rm = T)


data13$FAHighEdu <- NA
data13$FAHighEdu[data13$FAedu == 0] <- 0
data13$FAHighEdu[data13$FAedu == 1] <- 0
data13$FAHighEdu[data13$FAedu == 2] <- 1
data13$FAHighEdu <- factor(data13$FAHighEdu)

data13$FAHighEduM <- NA
data13$FAHighEduM[data13$FAedu == 0] <- 0
data13$FAHighEduM[data13$FAedu == 1] <- 0
data13$FAHighEduM[data13$FAedu == 2] <- 1
FAHighEduM <- mean(data13$FAHighEduM, na.rm = T)

#---------#
table(data13$a89c) # father politics -> 1: CCP; 2: Demo parties; 3 = CYL; 4: masses
# 1= CCP; 0 = no CCP
data13$FACCP <- NA
data13$FACCP[data13$a89c == 1] <- 1
data13$FACCP[data13$a89c > 1] <- 0
table(data13$FACCP)
data13$FACCP <- factor(data13$FACCP)

data13$FACCPM <- NA
data13$FACCPM[data13$a90c == 1] <- 1
data13$FACCPM[data13$a90c > 1] <- 0
FACCPM <- mean(data13$FACCPM, na.rm = T)

#---------#
table(data13$a90b) # mother edu
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data13$MOedu <- NA
data13$MOedu[data13$a90b == 1] <- 0
data13$MOedu[data13$a90b >= 2 & data13$a90b <= 8] <- 1
data13$MOedu[data13$a90b == 10] <- 2
data13$MOedu[data13$a90b == 12] <- 2
data13$MOedu[data13$a90b == 13] <- 2
table(data13$MOedu)
data13$MOedu <- factor(data13$MOedu)

data13$MOeduM <- NA
data13$MOeduM[data13$a90b == 1] <- 0
data13$MOeduM[data13$a90b >= 2 & data13$a90b <= 8] <- 1
data13$MOeduM[data13$a90b == 10] <- 2
data13$MOeduM[data13$a90b == 12] <- 2
data13$MOeduM[data13$a90b == 13] <- 2
MOeduM <- mean(data13$MOeduM, na.rm = T)

data13$MOHighEdu <- NA
data13$MOHighEdu[data13$MOedu == 0] <- 0
data13$MOHighEdu[data13$MOedu == 1] <- 0
data13$MOHighEdu[data13$MOedu == 2] <- 1
data13$MOHighEdu <- factor(data13$MOHighEdu)

data13$MOHighEduM <- NA
data13$MOHighEduM[data13$MOedu == 0] <- 0
data13$MOHighEduM[data13$MOedu == 1] <- 0
data13$MOHighEduM[data13$MOedu == 2] <- 1
MOHighEduM <- mean(data13$MOHighEduM,  na.rm=T)
MOHighEduM
#---------#
table(data13$a90c) # mother poltics 
# 1= CCP; 0 = no CCP
data13$MOCCP <- NA
data13$MOCCP[data13$a90c == 1] <- 1
data13$MOCCP[data13$a90c > 1] <- 0
table(data13$MOCCP)


data13$MOCCPM <- NA
data13$MOCCPM[data13$a90c == 1] <- 1
data13$MOCCPM[data13$a90c > 1] <- 0
MOCCPM <- mean(data13$MOCCPM, na.rm = T)

#---------#
# 0 = no parents CCP; 1 = at least 1 parent CCP; 2 = both parents CCP
data13$ParentsCCP <- NA
data13$ParentsCCP[data13$FACCP == 0 & data13$MOCCP == 0] <- 0
data13$ParentsCCP[data13$FACCP == 1 & data13$MOCCP == 0] <- 1
data13$ParentsCCP[data13$FACCP == 0 & data13$MOCCP == 1] <- 1
data13$ParentsCCP[data13$FACCP == 1 & data13$MOCCP == 1] <- 2
table(data13$ParentsCCP)
data13$ParentsCCP <- factor(data13$ParentsCCP)

data13$ParentsCCPM <- NA
data13$ParentsCCPM[data13$FACCP == 0 & data13$MOCCP == 0] <- 0
data13$ParentsCCPM[data13$FACCP == 1 & data13$MOCCP == 0] <- 1
data13$ParentsCCPM[data13$FACCP == 0 & data13$MOCCP == 1] <- 1
data13$ParentsCCPM[data13$FACCP == 1 & data13$MOCCP == 1] <- 2
ParentsCCPM <- mean(data13$ParentsCCPM, na.rm = T)

data13$familyPoli <- NA
data13$familyPoli[data13$ParentsCCP == 0] <- 0
data13$familyPoli[data13$ParentsCCP == 1] <- 1
data13$familyPoli[data13$ParentsCCP == 2] <- 1
data13$familyPoli <- factor(data13$familyPoli)
table(data13$familyPoli)

data13$ParentsEDU <- NA
data13$ParentsEDU[data13$MOedu == 0 & data13$FAedu == 0] <- 0
data13$ParentsEDU[data13$MOedu == 0 & data13$FAedu == 1] <- 1
data13$ParentsEDU[data13$MOedu == 1 & data13$FAedu == 0] <- 1
data13$ParentsEDU[data13$MOedu == 1 & data13$FAedu == 1] <- 1
data13$ParentsEDU[data13$MOedu == 2 & data13$FAedu == 0] <- 2
data13$ParentsEDU[data13$MOedu == 2 & data13$FAedu == 1] <- 2
data13$ParentsEDU[data13$MOedu == 2 & data13$FAedu == 2] <- 3
data13$ParentsEDU[data13$MOedu == 0 & data13$FAedu == 2] <- 2
data13$ParentsEDU[data13$MOedu == 1 & data13$FAedu == 2] <- 2
table(data13$ParentsEDU)
data13$ParentsEDU <- factor(data13$ParentsEDU)


data13$ParentsHEdu <- NA
data13$ParentsHEdu[data13$MOHighEdu == 0 & data13$FAHighEdu == 0] <- 0
data13$ParentsHEdu[data13$MOHighEdu == 1 & data13$FAHighEdu == 0] <- 1
data13$ParentsHEdu[data13$MOHighEdu == 0 & data13$FAHighEdu == 1] <- 1
data13$ParentsHEdu[data13$MOHighEdu == 1 & data13$FAHighEdu == 1] <- 1
table(data13$ParentsHEdu)
data13$ParentsHEdu <- factor(data13$ParentsHEdu)

#---------#
# GDP per capita 2012 = 6317$ * 6.31 (mean exchange rate US-RMB 2012) = 39860 RMB
summary(data13$wealth12mod)


data13$LNwealth <- log(data13$wealth12mod)  
summary(data13$LNwealth)


#increase every 20k which is 1/2 of the gdp per capita ~40k
data13$GDPpc_wealth <- NA
data13$GDPpc_wealth[data13$wealth12mod < 20000] <- 0
data13$GDPpc_wealth[data13$wealth12mod >= 20000 & data13$wealth12mod < 39860] <- 1
data13$GDPpc_wealth[data13$wealth12mod >= 39860 & data13$wealth12mod < 60000] <- 2
data13$GDPpc_wealth[data13$wealth12mod >= 60000] <- 3
table(data13$GDPpc_wealth)
data13$GDPpc_wealth <- factor(data13$GDPpc_wealth)

#NO FACTOR
data13$GDPpc_wealthA <- NA
data13$GDPpc_wealthA[data13$wealth12mod < 25000] <- 0
data13$GDPpc_wealthA[data13$wealth12mod >= 25000 & data13$wealth12mod < 39860] <- 1
data13$GDPpc_wealthA[data13$wealth12mod >= 39860 & data13$wealth12mod < 60000] <- 2
data13$GDPpc_wealthA[data13$wealth12mod >= 60000] <- 3
table(data13$GDPpc_wealthA)


data13$GDPpc_wealthA1 <- NA
data13$GDPpc_wealthA1[data13$wealth12mod < 16000] <- 0
data13$GDPpc_wealthA1[data13$wealth12mod >= 16000 & data13$wealth12mod < 39860] <- 1
data13$GDPpc_wealthA1[data13$wealth12mod >= 39860 & data13$wealth12mod < 60000] <- 2
data13$GDPpc_wealthA1[data13$wealth12mod >= 60000] <- 3
table(data13$GDPpc_wealthA1)
data13$GDPpc_wealthA1 <- factor(data13$GDPpc_wealthA1)

data13$GDPpc_wealthA2 <- NA
data13$GDPpc_wealthA2[data13$wealth12mod < 25000] <- 0
data13$GDPpc_wealthA2[data13$wealth12mod >= 25000 & data13$wealth12mod < 39860] <- 1
data13$GDPpc_wealthA2[data13$wealth12mod >= 39860 & data13$wealth12mod < 60000] <- 2
data13$GDPpc_wealthA2[data13$wealth12mod >= 60000] <- 3
table(data13$GDPpc_wealthA2)
data13$GDPpc_wealthA2 <- factor(data13$GDPpc_wealthA2)

gghistogram(data13$LNwealth)
gghistogram(data13$GDPpc_wealthA)

quantile(data13$LNwealth, c(.50, .60, .70, .80, .90, .92, .999), na.rm = T)
quantile(data13$GDPpc_wealthA, c(.35, .39, .40, .41, .42, .43, .44, .45, .46,.47,.48,.49,
                                 .50, .51, .52, .53, .54, .55, .58, .59, .60, .61, .62, .65, .70, .80, .91, .999), na.rm = T)

#Work experience and current job
head(data13$a58)
#1: Currently engaged in non-agricultural work; 
#2: Currently working in agriculture, once had non-agricultural work;
#3: Currently working in farming, no non-agricultural work ; 
#4: No job at the moment, and only farming;
#5: No job at the moment, have had a non-agricultural job;
#6: Never worked

head(data13$a59a) - # breakdown data13$a58)
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
data13$Job <- NA
data13$Job[data13$a58 == 2] <- 1
data13$Job[data13$a58 == 3] <- 1
data13$Job[data13$a58 == 4] <- 1
data13$Job[data13$a58 == 5] <- 2
data13$Job[data13$a58 == 6] <-  2
data13$Job[data13$a59a == 1] <- 7
data13$Job[data13$a59a == 2] <- 6
data13$Job[data13$a59a == 3] <- 5
data13$Job[data13$a59a == 4] <- 0
data13$Job[data13$a59a == 5] <- 3
data13$Job[data13$a59a == 6] <- 4
data13$Job[data13$a59a == 7] <- 2
data13$Job[data13$a59a == 8] <- 3
table(data13$Job)
data13$JobF <- factor(data13$Job)

#Jobnew --> 0 = white collar; 1 = labor; 2 = rural work
data13$Jobnew <- NA
data13$Jobnew[data13$Job == 0] <- 1
data13$Jobnew[data13$Job == 3] <- 1
data13$Jobnew[data13$Job == 2] <- 1
data13$Jobnew[data13$Job == 4] <- 1
data13$Jobnew[data13$Job == 5] <- 0
data13$Jobnew[data13$Job == 6] <- 0
#data15$Jobnew[data15$Job == 1] <- 2
data13$Jobnew[data13$Job == 7] <- 0
table(data13$Jobnew)


#JobClerk
# 2 = rural work & unemployed; 1 = Wokers; 0 = clerk ; 3 = Managment & private business
data13$JobKey <- NA
data13$JobKey[data13$Job == 1] <- 2
data13$JobKey[data13$Job == 2] <- 1
data13$JobKey[data13$Job == 3] <- 1
data13$JobKey[data13$Job == 4] <- 1
data13$JobKey[data13$Job == 5] <- 0
data13$JobKey[data13$Job == 0] <- 1
data13$JobKey[data13$Job == 6] <- 3
data13$JobKey[data13$Job == 7] <- 3
table(data13$JobKey)
data13$JobKey <- factor(data13$JobKey)

#Job urban
# 0= clerk; 1 = labor worker; 2 = business
data13$UrbanJob <- NA
data13$UrbanJob[data13$Job == 2] <- 1
data13$UrbanJob[data13$Job == 3] <- 1
data13$UrbanJob[data13$Job == 4] <- 1
data13$UrbanJob[data13$Job == 5] <- 0
data13$UrbanJob[data13$Job == 0] <- 1
data13$UrbanJob[data13$Job == 6] <- 2
data13$UrbanJob[data13$Job == 7] <- 2
table(data13$UrbanJob)
data13$UrbanJob <- factor(data13$UrbanJob)



data13$JobD <- NA
data13$JobD[data13$Job == 0] <- 0
data13$JobD[data13$Job == 1] <- 1
data13$JobD[data13$Job > 1] <- 2
data13$JobD <- factor(data13$JobD)

JobM1 <- mean(data13$Job, na.rm = T)
summary(JobM)
#--------------------------------------------------------------------------------------
# 1st: cox hazard analysis on the age citizens' application to the CCP #
ageApp13 <- data13$CCPapp - data13$birth
table(ageApp13)

data13$age18App <- ifelse(ageApp13 >= 18, ageApp13, NA)
table(data13$age18App)

data13$age18App[is.na(data13$age18App)] <- 0

time13 <- data13$age18App - 18
table(time13)
#------------------------------------------------------------------------------------
# event

# Creating a dummy for those who applied 2008-2012 and those who did not applied ever
data13$time0812q <- NA
data13$time0812q[data13$CCPapp >= 2008] <- 1
data13$time0812q[data13$CCPapp == 0] <- 0
table(data13$time0812q)

event13q <- data13$time0812q
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------ //
#1.a CCP acceptance age likelihood

Acc0813 <- ifelse(data13$CCPmemb >= 2008, 1, 0)
table(Acc0813)
event13.a <- Acc0813
table(event13.a)

#------------------------------------------------------------------------------------ 

ageAcc13 <- data13$CCPmemb - data13$birth
table(ageAcc13)

age18Acc13 <- ifelse(ageApp13 >= 18, ageApp13, NA)
table(age18Acc13)

time13.a <- age18Acc13 - 17
table(time13.a)

#------------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------------ 

# Set of dependent variables
data13$Ref_Acc_u30 <- NA
data13$Ref_Acc_u30[data13$CCPapp >= 2008 & data13$birth >= 1983 & data13$CCPmemb >= 2008] <- 1
data13$Ref_Acc_u30[data13$CCPapp < 2008 & data13$birth >= 1983 & data13$CCPmemb >= 2008] <- 2
data13$Ref_Acc_u30[data13$CCPapp >= 2008 & data13$birth < 1983 & data13$CCPmemb >= 2008] <- 3
data13$Ref_Acc_u30[data13$CCPapp < 2008 & data13$birth < 1983 & data13$CCPmemb >= 2008] <- 4
data13$Ref_Acc_u30[data13$CCPapp < 2008 & data13$birth >= 1983 & data13$CCPmemb < 2008] <- 5
data13$Ref_Acc_u30[data13$CCPapp == 0 & data13$birth >=  1983 & data13$CCPmemb == 0] <- 6
data13$Ref_Acc_u30[data13$CCPapp == 0 & data13$birth <  1983 & data13$CCPmemb == 0] <- 7
data13$Ref_Acc_u30[data13$CCPapp < 2008 & data13$CCPapp > 0 & data13$birth < 1983 & data13$CCPmemb == 0] <- 8
data13$Ref_Acc_u30[data13$CCPapp >= 2008 & data13$birth >= 1983 & data13$CCPmemb == 0] <- 9
data13$Ref_Acc_u30[data13$CCPapp >= 2008 & data13$birth <  1983 & data13$CCPmemb == 0] <- 10

data13$Ref_Acc_u30[is.na(data13$Ref_Acc_u30)] <- 0

table(data13$Ref_Acc_u30)
data13$Ref_Acc_u30 <- factor(data13$Ref_Acc_u30)

#new DV - Accepted refused
data13$RefAcc <- NA
data13$RefAcc[data13$CCPapp >= 2008 & data13$CCPmemb >= 0] <- 0
data13$RefAcc[data13$CCPapp >= 2008 & data13$CCPmemb >= 2008] <- 1
table(data13$RefAcc)

#new DV - Applicants or not
data13$AppNo <- NA
data13$AppNo[data13$CCPapp == 0] <- 0
data13$AppNo[data13$CCPapp >= 2008] <- 1
table(data13$AppNo)

# creating dummies for the tests and the probit models

NoApp13 <- ifelse(data13$Ref_Acc_u30 == 7, 1, 0)
table(NoApp13)
NoAppu30.13 <- ifelse(data13$Ref_Acc_u30 == 6, 1, 0)
table(NoAppu30.13)
Ref0812 <- ifelse(data13$Ref_Acc_u30 == 9 |
                    data13$Ref_Acc_u30 == 10, 1, 0)
table(Ref0812)
Acc0812 <- ifelse(data13$Ref_Acc_u30 == 1 |
                    data13$Ref_Acc_u30 == 3, 1, 0)
table(Acc0812)

# No applicants u30 and those refused 2008-2012
NoAppu30.Ref0812 <- NA
NoAppu30.Ref0812[NoAppu30.13 == 1 & Ref0812 == 0] <- 0
NoAppu30.Ref0812[NoAppu30.13 == 0 & Ref0812 == 1] <- 1
table(NoAppu30.Ref0812)

# No applicants u30 and those accepted 2008-2012
NoAppu30.Acc0812 <- NA
NoAppu30.Acc0812[NoAppu30.13 == 1 & Acc0812 == 0] <- 0
NoAppu30.Acc0812[NoAppu30.13 == 0 & Acc0812 == 1] <- 1
table(NoAppu30.Acc0812)

# No applicants u30 and applicants
NoAppu30.App0812 <- NA
NoAppu30.App0812[NoAppu30.13 == 1 & Acc0812 == 0 & Ref0812 == 0] <- 0
NoAppu30.App0812[NoAppu30.13 == 0 & Acc0812 == 1 & Ref0812 == 0] <- 1
NoAppu30.App0812[NoAppu30.13 == 0 & Acc0812 == 0 & Ref0812 == 1] <- 1
table(NoAppu30.App0812)

# Accepted & refused 2008-2012 -> 0: refused; 1: accepted
Acc0812.Ref0812 <- NA
Acc0812.Ref0812[Ref0812 == 1 & Acc0812 == 0] <- 0
Acc0812.Ref0812[Ref0812 == 0 & Acc0812 == 1] <- 1
table(Acc0812.Ref0812)

#------------------------------------------ E N D ---------------------------------------- #
