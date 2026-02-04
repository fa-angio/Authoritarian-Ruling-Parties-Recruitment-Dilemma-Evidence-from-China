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
cgss2017 <- read_dta("data source/cgss2017.dta")

data17 <- cgss2017

table(data17$a31)
data17$age <- 2017-data17$a31
table(data17$age)
table(data17$b2) #Soc Eco level 3 years ago

data17$ageCATa <- NA
data17$ageCATa[data17$age < 30] <- 0
data17$ageCATa[data17$age >= 30 & data17$age < 40] <- 1
data17$ageCATa[data17$age >= 40 & data17$age < 50] <- 2
data17$ageCATa[data17$age >= 50 & data17$age < 80] <- 3
data17$ageCATa[data17$age >= 80] <- 4
table(data17$ageCATa)


data17$ageD <- ifelse(data17$ageCATa == 0, 1, 0)
data17$ageD <- factor(data17$ageD)

# ethnicity - a4 = ethnicity
# 1 = Han - 0 = all other ethnicity
data17$a4 <- ifelse(data17$a4 < 1, NA, data17$a4)
data17$ethn <- ifelse(data17$a4 > 1, 0, data17$a4)
table(data17$ethn)

ggplot(data17, aes(RefAcc, fill = factor(ethn))) +
  geom_histogram()

# SocEco: 0 = decreased; 1 = same level; 2 = increased
data17$SocEco3YRago <- NA
data17$SocEco3YRago[data17$b2 == 1] <- 2
data17$SocEco3YRago[data17$b2 == 2 & 9] <- 1
data17$SocEco3YRago[data17$b2 == 3] <- 0
table(data17$SocEco3YRago)
SocEco3YRagoM <- mean(data17$SocEco3YRago, na.rm = T)
summary(data17$SocEco3YRagoM)

data17$SocEco3YRago <- factor(data17$SocEco3YRago)

# 0 = decreased, 1 increased
data17$SocEcoD <- ifelse(data17$SocEco3YRago == 0, 0, 1)
table(data17$SocEcoD)

data17$SocEco3YRagoM <- NA
data17$SocEco3YRagoM[data17$b2 == 1] <- 2
data17$SocEco3YRagoM[data17$b2 == 2] <- 1
data17$SocEco3YRagoM[data17$b2 == 9] <- 1
data17$SocEco3YRagoM[data17$b2 == 3] <- 0
table(data17$SocEco3YRagoM)

#---------#
table(data17$a31) #Birth
data17$birth <- ifelse(data17$a31 > 0, data17$a31, NA)
table(data17$birth)

data17$u30 <- ifelse(data17$birth >= 1985, 1, 0)
table(data17$u30)

#---------#
table(data17$a2) #gender - 1 = male
data17$gender <- NA
data17$gender[data17$a2 == 1] <- 1
data17$gender[data17$a2 == 2] <- 0
table(data17$gender)
data17$genderM <- mean(data17$gender, na.rm=TRUE)

data17$gender <- factor(data17$gender)

data17$genderM <- mean(data17$genderM, na.rm=TRUE)
summary(data17$genderM)
#---------#
table(data17$s1) # area you live in - 2 = rural, 1 = city 

# geography: 0 = rural; 1 = urban
data17$geography <- NA
data17$geography[data17$s1 == 1] <- 1
data17$geography[data17$s1 == 2] <- 0
table(data17$geography)
data17$geography <- factor(data17$geography)

data17$geographyM <- NA
data17$geographyM[data17$s1 == 1] <- 1
data17$geographyM[data17$s1 == 2] <- 0
data17$geographyM <- mean(data17$geographyM, na.rm = T)
summary(data17$geographyM)

# mapping applicants & accepted
# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data17$name.prov <- NA
data17$name.prov[data17$s41 == 1] <- "天津"
data17$name.prov[data17$s41 == 4] <- "北京"
data17$name.prov[data17$s41 == 7] <- "上海"
data17$name.prov[data17$s41 == 15] <- "江苏"
data17$name.prov[data17$s41 == 19] <- "浙江"
data17$name.prov[data17$s41 == 3] <- "内蒙古自治区"
data17$name.prov[data17$s41 == 27] <- "辽宁"
data17$name.prov[data17$s41 == 24] <- "福建"
data17$name.prov[data17$s41 == 12] <- "广东"
data17$name.prov[data17$s41 == 10] <- "山东"

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Xinjiang, Hunan, Hebei, Qinghai, Heilongjiang
data17$name.prov[data17$s41 == 5] <- "吉林"
data17$name.prov[data17$s41 == 28] <- "长青"
data17$name.prov[data17$s41 == 21] <- "湖北"
data17$name.prov[data17$s41 == 29] <- "陕西"
data17$name.prov[data17$s41 == 8] <- "酿下"
data17$name.prov[data17$s41 == 14] <- "湖南"
data17$name.prov[data17$s41 == 22] <- "湖南" 
data17$name.prov[data17$s41 == 17] <- "河北" 
data17$name.prov[data17$s41 == 30] <- "青海" 
data17$name.prov[data17$s41 == 31] <- "黑龙江"

#poorest 10 provinces
#Hainan, Henan, Sichuan, Shanxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, Gangsu
data17$name.prov[data17$s41 == 20] <- "海南"
data17$name.prov[data17$s41 == 18] <- "河南"
data17$name.prov[data17$s41 == 6] <- "四川"
data17$name.prov[data17$s41 == 11] <- "山西"
data17$name.prov[data17$s41 == 16] <- "江西"
data17$name.prov[data17$s41 == 9] <- "安徽"
data17$name.prov[data17$s41 == 13] <- "广西"
data17$name.prov[data17$s41 == 25] <- "西藏自治区"
data17$name.prov[data17$s41 == 2] <- "云南"
data17$name.prov[data17$s41 == 26] <- "贵州"
data17$name.prov[data17$s41 == 23] <- "钢塑"

#Red/pink provinces: 
#RED provinces= 2 -> BEI, TIA, SHA, LIAONING, TIBET, SHANDONG, SHAANXI, SHANXI, HEBEI, HUBEI
#Light-red = 1 -> Zhejiang, Xinjiang, Qinghai, Jiangsu, Ningxia, Gansu, Inner Mong, Jilin, Hilong, Hunan, CHQ
#PINK provinces= 0 -> Sichuan, Anhui, Hainan, Henan, Fujian, Yunnan, Guangxi, Jiangxi, Guizhou, Guangdong

table(data17$s41)
head(data17$s41)
#Megacities
#Megacities = 1 <- 1 (SHA), 4 (BEI), 12 (Guangdong), 28 (CHQ), 7 (TIA)
# Megacities = 0 <- rest  of provinces
data17$megacities <- 0
data17$megacities[data17$s41 == 1] <- 1
data17$megacities[data17$s41 == 4] <- 1
data17$megacities[data17$s41 == 12] <- 1
data17$megacities[data17$s41 == 28] <- 1
data17$megacities[data17$s41 == 7] <- 1
table(data17$megacities)
data17$megacities <- factor(data17$megacities)


## political geography

#red provinces
data17$redgeography.cont <- NA
data17$redgeography.cont[data17$s41 == 1] <- 30
data17$redgeography.cont[data17$s41 == 4] <- 29
data17$redgeography.cont[data17$s41 == 7] <- 28
data17$redgeography.cont[data17$s41 == 10] <-27
data17$redgeography.cont[data17$s41 == 11] <- 26
data17$redgeography.cont[data17$s41 == 17] <- 25
data17$redgeography.cont[data17$s41 == 21] <- 24
data17$redgeography.cont[data17$s41 == 25] <- 23
data17$redgeography.cont[data17$s41 == 27] <- 22
data17$redgeography.cont[data17$s41 == 29] <- 21
data17$redgeography.cont[data17$s41 == 3] <- 20
data17$redgeography.cont[data17$s41 == 5] <- 19
data17$redgeography.cont[data17$s41 == 8] <- 18
data17$redgeography.cont[data17$s41 == 14] <- 17
data17$redgeography.cont[data17$s41 == 15] <- 16
data17$redgeography.cont[data17$s41 == 19] <- 15
data17$redgeography.cont[data17$s41 == 22] <- 14
data17$redgeography.cont[data17$s41 == 23] <- 13
data17$redgeography.cont[data17$s41 == 28] <- 12
data17$redgeography.cont[data17$s41 == 30] <- 11
data17$redgeography.cont[data17$s41 == 31] <- 10
data17$redgeography.cont[data17$s41 == 12] <- 9
data17$redgeography.cont[data17$s41 == 2] <- 8
data17$redgeography.cont[data17$s41 == 6] <- 7
data17$redgeography.cont[data17$s41 == 9] <- 6
data17$redgeography.cont[data17$s41 == 13] <- 5
data17$redgeography.cont[data17$s41 == 16] <- 4
data17$redgeography.cont[data17$s41 == 18] <- 3
data17$redgeography.cont[data17$s41 == 20] <-2
data17$redgeography.cont[data17$s41 == 24] <-1
data17$redgeography.cont[data17$s41 == 26] <-0


#red provinces
data17$redgeography <- NA
data17$redgeography[data17$s41 == 1] <- 2 
data17$redgeography[data17$s41 == 4] <- 2
data17$redgeography[data17$s41 == 7] <- 2
data17$redgeography[data17$s41 == 10] <-2
data17$redgeography[data17$s41 == 11] <- 2
data17$redgeography[data17$s41 == 17] <- 2
data17$redgeography[data17$s41 == 21] <- 2
data17$redgeography[data17$s41 == 25] <- 2
data17$redgeography[data17$s41 == 27] <- 2
data17$redgeography[data17$s41 == 29] <- 2

#light-red provinces
data17$redgeography[data17$s41 == 3] <- 1 
data17$redgeography[data17$s41 == 5] <- 1
data17$redgeography[data17$s41 == 8] <- 1
data17$redgeography[data17$s41 == 14] <- 1
data17$redgeography[data17$s41 == 15] <- 1
data17$redgeography[data17$s41 == 19] <- 1
data17$redgeography[data17$s41 == 22] <- 1
data17$redgeography[data17$s41 == 23] <- 1
data17$redgeography[data17$s41 == 28] <- 1
data17$redgeography[data17$s41 == 30] <- 1
data17$redgeography[data17$s41 == 31] <- 1

#pink provinces
data17$redgeography[data17$s41 == 12] <- 0
data17$redgeography[data17$s41 == 2] <- 0
data17$redgeography[data17$s41 == 6] <- 0
data17$redgeography[data17$s41 == 9] <- 0
data17$redgeography[data17$s41 == 13] <- 0
data17$redgeography[data17$s41 == 16] <- 0
data17$redgeography[data17$s41 == 18] <- 0
data17$redgeography[data17$s41 == 20] <-0
data17$redgeography[data17$s41 == 24] <-0
data17$redgeography[data17$s41 == 26] <-0

table(data17$redgeography)

data17$redgeography <- factor(data17$redgeography)
data17$redgeographyM <- mean(data17$redgeography, na.rm=TRUE)
summary(data17$redgeographyM)

#Red province is reference category
data17$REDprov <- NA
data17$REDprov[data17$redgeography == 0] <- 2
data17$REDprov[data17$redgeography == 1] <- 1
data17$REDprov[data17$redgeography == 2] <- 0
data17$REDprov <- factor(data17$REDprov)

#light red province is reference category
data17$LREDprov <- NA
data17$LREDprov[data17$redgeography == 0] <- 2
data17$LREDprov[data17$redgeography == 1] <- 0
data17$LREDprov[data17$redgeography == 2] <- 1
data17$LREDprov <- factor(data17$LREDprov)

## per capita Gross regional product (highest to lowest)
data17$GRP_pc.cont <- NA
data17$GRP_pc.cont[data17$s41 == 1] <- 30
data17$GRP_pc.cont[data17$s41 == 4] <- 29
data17$GRP_pc.cont[data17$s41 == 7] <- 28
data17$GRP_pc.cont[data17$s41 == 15] <- 27
data17$GRP_pc.cont[data17$s41 == 19] <- 26
data17$GRP_pc.cont[data17$s41 == 3] <- 25
data17$GRP_pc.cont[data17$s41 == 27] <- 24
data17$GRP_pc.cont[data17$s41 == 24] <- 23
data17$GRP_pc.cont[data17$s41 == 12] <- 22
data17$GRP_pc.cont[data17$s41 == 10] <- 21
data17$GRP_pc.cont[data17$s41 == 5] <- 20
data17$GRP_pc.cont[data17$s41 == 28] <- 19
data17$GRP_pc.cont[data17$s41 == 21] <- 18
data17$GRP_pc.cont[data17$s41 == 29] <- 17
data17$GRP_pc.cont[data17$s41 == 8] <- 16
data17$GRP_pc.cont[data17$s41 == 14] <- 15
data17$GRP_pc.cont[data17$s41 == 22] <- 14
data17$GRP_pc.cont[data17$s41 == 17] <- 13
data17$GRP_pc.cont[data17$s41 == 30] <- 12
data17$GRP_pc.cont[data17$s41 == 31] <- 11
data17$GRP_pc.cont[data17$s41 == 20] <- 10
data17$GRP_pc.cont[data17$s41 == 18] <- 9
data17$GRP_pc.cont[data17$s41 == 6] <- 8
data17$GRP_pc.cont[data17$s41 == 11] <- 7
data17$GRP_pc.cont[data17$s41 == 16] <- 6
data17$GRP_pc.cont[data17$s41 == 9] <- 5
data17$GRP_pc.cont[data17$s41 == 13] <- 4
data17$GRP_pc.cont[data17$s41 == 25] <- 3
data17$GRP_pc.cont[data17$s41 == 2] <- 2
data17$GRP_pc.cont[data17$s41 == 26] <- 1
data17$GRP_pc.cont[data17$s41 == 23] <- 0
table(data17$GRP_pc.cont)

# high per capita income (first 10 provinces)
# TIA, BEI, SHA, Jiangsu, Zhejiang, Inn Mong, Liaoning, Fujian, Guangdong, Shandong
data17$GRP_pc <- NA
data17$GRP_pc[data17$s41 == 1] <- 2
data17$GRP_pc[data17$s41 == 4] <- 2
data17$GRP_pc[data17$s41 == 7] <- 2
data17$GRP_pc[data17$s41 == 15] <- 2
data17$GRP_pc[data17$s41 == 19] <- 2
data17$GRP_pc[data17$s41 == 3] <- 2
data17$GRP_pc[data17$s41 == 27] <- 2
data17$GRP_pc[data17$s41 == 24] <- 2
data17$GRP_pc[data17$s41 == 12] <- 2
data17$GRP_pc[data17$s41 == 10] <- 2

#medium per capita income (second 10 provinces)
# Jilin, Chongqing, Hubei, Shaanxi, Ningxia, Xinjiang, Hunan, Hebei, Qinghai, Heilongjiang
data17$GRP_pc[data17$s41 == 5] <- 1
data17$GRP_pc[data17$s41 == 28] <- 1
data17$GRP_pc[data17$s41 == 21] <- 1
data17$GRP_pc[data17$s41 == 29] <- 1
data17$GRP_pc[data17$s41 == 8] <- 1
data17$GRP_pc[data17$s41 == 14] <- 1
data17$GRP_pc[data17$s41 == 22] <- 1
data17$GRP_pc[data17$s41 == 17] <- 1
data17$GRP_pc[data17$s41 == 30] <- 1
data17$GRP_pc[data17$s41 == 31] <- 1

#poorest 10 provinces
#Hainan, Henan, Sichuan, Shanxi, Jiangxi, Jiangxi, Anuhui, Guangxi, Tibet, Yunnan, Guizhou, 
#Gangsu
data17$GRP_pc[data17$s41 == 20] <- 0
data17$GRP_pc[data17$s41 == 18] <- 0
data17$GRP_pc[data17$s41 == 6] <- 0
data17$GRP_pc[data17$s41 == 11] <- 0
data17$GRP_pc[data17$s41 == 16] <- 0
data17$GRP_pc[data17$s41 == 9] <- 0
data17$GRP_pc[data17$s41 == 13] <- 0
data17$GRP_pc[data17$s41 == 25] <- 0
data17$GRP_pc[data17$s41 == 2] <- 0
data17$GRP_pc[data17$s41 == 26] <- 0
data17$GRP_pc[data17$s41 == 23] <- 0
table(data17$GRP_pc)
data17$GRP_pc <- factor(data17$GRP_pc)

data17$GRP_pcM <- mean(data17$GRP_pc, na.rm = T)
summary(data17$GRP_pcM)

data17$GRP_pcRich <- NA
data17$GRP_pcRich[data17$GRP_pc == 0] <- 2
data17$GRP_pcRich[data17$GRP_pc == 1] <- 1
data17$GRP_pcRich[data17$GRP_pc == 2] <- 0
table(data17$GRP_pcRich)
data17$GRP_pcRich <- factor(data17$GRP_pcRich)

data17$GRP_pcRichM <- NA
data17$GRP_pcRichM[data17$GRP_pc == 0] <- 2
data17$GRP_pcRichM[data17$GRP_pc == 1] <- 1
data17$GRP_pcRichM[data17$GRP_pc == 2] <- 0
table(data17$GRP_pcRichM)
data17$GRP_pcRichM <- mean(data17$GRP_pcRichM)
summary(data17$GRP_pcRichM)

# a18: 1= agricultural; 2: non-agricultural; 3: blueprint; 4: residential(prev. agric); 5: residential (prev. non.ag);
# 6: military; 7: no hukou
# Hukou -> 0: agricultural; 1: migrant succesful; 2: urban
table(data17$a18)

data17$hukou <- NA
data17$hukou[data17$a18 == 1] <- 0
data17$hukou[data17$a18 == 4] <- 1
data17$hukou[data17$a18 == 2] <- 2
data17$hukou[data17$a18 == 5] <- 2
table(data17$hukou)
data17$hukou <- factor(data17$hukou)

#hukoudummy (1 = urban; 0 = rural)
data17$hukouD <- NA
data17$hukouD[data17$hukou == 0] <- 0
data17$hukouD[data17$hukou == 1] <- 1
data17$hukouD[data17$hukou == 2] <- 1
data17$hukouD<-factor(data17$hukouD)

data17$hukouDM <- NA
data17$hukouDM[data17$hukou == 0] <- 0
data17$hukouDM[data17$hukou == 1] <- 1
data17$hukouDM[data17$hukou == 2] <- 1
hukouDM <- mean(data17$hukouDM, na.rm = T)

#---------#
head(data17$a7a) # 1 = no edu; 2-8 = primary/secondary edu; 10, 12-13 = higher edu

data17$education1 <- NA
#data17$education1[data17$a7a == 1] <- 0
#data17$education1[data17$a7a == 2] <- 1
data17$education1[data17$a7a == 3] <- 1
data17$education1[data17$a7a == 4] <- 2
data17$education1[data17$a7a == 5] <- 3
data17$education1[data17$a7a == 6] <- 4
data17$education1[data17$a7a == 7] <- 5
data17$education1[data17$a7a == 8] <- 6
data17$education1[data17$a7a == 9] <- 7
data17$education1[data17$a7a == 10] <-8
data17$education1[data17$a7a == 11] <- 9
data17$education1[data17$a7a == 12] <- 10
data17$education1[data17$a7a == 13] <- 11
table(data17$education1)

data17$educationM <- mean(data17$education1, na.rm= T)
table(data17$educationM)

# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data17$edua <- NA
data17$edua[data17$a7a == 3] <- 0
data17$edua[data17$a7a == 4] <- 1
data17$edua[data17$a7a == 6] <- 1
data17$edua[data17$a7a == 7] <- 2
data17$edua[data17$a7a == 10] <- 3
data17$edua[data17$a7a == 12] <- 3
data17$edua[data17$a7a == 13] <- 3
table(data17$edua)
data17$eduF <- factor(data17$edu)

edu2 <- (data17$education1)^2

data17$eduM <- NA
data17$eduM[data17$a7a == 1] <- 0
data17$eduM[data17$a7a >= 2 & data17$a7a <= 8] <- 1
data17$eduM[data17$a7a == 10] <- 2
data17$eduM[data17$a7a == 12] <- 2
data17$eduM[data17$a7a == 13] <- 2
eduM <- mean(data17$eduM, na.rm=TRUE)
eduM

# two levels education: 0 = primary/secondary edu; 1 = higher edu
data17$HEdu <- NA
data17$HEdu[data17$edua < 3] <- 0
data17$HEdu[data17$edua == 2] <- 1
table(data17$HEdu)
data17$HEdu <- factor(data17$HEdu)

data17$HEduM <- NA
data17$HEduM[data17$edu == 1] <- 0
data17$HEduM[data17$edu == 2] <- 1
HEduM <- mean(data17$HEduM, na.rm = T)
#---------#
table(data17$a9a)
data17$CCPapp <- ifelse(data17$a9a > 0 & data17$a9a< 2018, data17$a9a, NA)
data17$CCPapp[is.na(data17$CCPapp)] <- 0
table(data17$CCPapp)
#---------#
table(data17$a10a)
data17$CCPmemb <- ifelse(data17$a10a > 0 & data17$a9a < 2018, data17$a10a, NA)
data17$CCPmemb[is.na(data17$CCPmemb)] <- 0
table(data17$CCPmemb)
#---------#
table(data17$a89b) #father education
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data17$FAedu <- NA
data17$FAedu[data17$a89b == 1] <- 0
data17$FAedu[data17$a89b >= 2 & data17$a89b <= 8] <- 1
data17$FAedu[data17$a89b == 10] <- 2
data17$FAedu[data17$a89b == 12] <- 2
data17$FAedu[data17$a89b == 13] <- 2
table(data17$FAedu)
data17$FAedu <- factor(data17$FAedu)

data17$FAeduM <- NA
data17$FAeduM[data17$a89b == 1] <- 0
data17$FAeduM[data17$a89b >= 2 & data17$a89b <= 8] <- 1
data17$FAeduM[data17$a89b == 10] <- 2
data17$FAeduM[data17$a89b == 12] <- 2
data17$FAeduM[data17$a89b == 13] <- 2

data17$FAHighEdu <- NA
data17$FAHighEdu[data17$FAedu == 0] <- 0
data17$FAHighEdu[data17$FAedu == 1] <- 0
data17$FAHighEdu[data17$FAedu == 2] <- 1
data17$FAHighEdu <- factor(data17$FAHighEdu)

data17$FAHighEduM <- NA
data17$FAHighEduM[data17$FAedu == 0] <- 0
data17$FAHighEduM[data17$FAedu == 1] <- 0
data17$FAHighEduM[data17$FAedu == 2] <- 1
#---------#
table(data17$a89c) # father politics -> 1: masses; 2: Demo parties; 3 = CYL; 4: CCP
# 1= CCP; 0 = no CCP
data17$FACCP <- NA
data17$FACCP[data17$a89c == 4] <- 1
data17$FACCP[data17$a89c == 1 ] <- 0
data17$FACCP[data17$a89c == 2] <- 0
data17$FACCP[data17$a89c == 3 ] <- 0
data17$FACCP <- factor(data17$FACCP)
table(data17$FACCP)

data17$FACCPM <- NA
data17$FACCPM[data17$a89c == 4] <- 1
data17$FACCPM[data17$a89c == 1 ] <- 0
data17$FACCPM[data17$a89c == 2] <- 0
data17$FACCPM[data17$a89c == 3 ] <- 0
data17$FACCPM <- mean(data17$FACCPM, na.rm = T)
table(data17$FACCPM)
#---------#
table(data17$a90b) # mother edu
# Three levels education: 0 = no edu; 1 = primary/secondary edu; 2= higher edu
data17$MOedu <- NA
data17$MOedu[data17$a90b == 1] <- 0
data17$MOedu[data17$a90b >= 2 & data17$a90b <= 8] <- 1
data17$MOedu[data17$a90b == 10] <- 2
data17$MOedu[data17$a90b == 12] <- 2
data17$MOedu[data17$a90b == 13] <- 2
table(data17$MOedu)
data17$MOedu <- factor(data17$MOedu)

data17$MOeduM <- NA
data17$MOeduM[data17$a90b == 1] <- 0
data17$MOeduM[data17$a90b >= 2 & data17$a90b <= 8] <- 1
data17$MOeduM[data17$a90b == 10] <- 2
data17$MOeduM[data17$a90b == 12] <- 2
data17$MOeduM[data17$a90b == 13] <- 2

data17$MOHighEdu <- NA
data17$MOHighEdu[data17$MOedu == 0] <- 0
data17$MOHighEdu[data17$MOedu == 1] <- 0
data17$MOHighEdu[data17$MOedu == 2] <- 1
data17$MOHighEdu <- factor(data17$MOHighEdu)

data17$MOHighEduM <- NA
data17$MOHighEduM[data17$MOedu == 0] <- 0
data17$MOHighEduM[data17$MOedu == 1] <- 0
data17$MOHighEduM[data17$MOedu == 2] <- 1

#---------#
table(data17$a90c) # mother poltics 
# 1= CCP; 0 = no CCP
data17$MOCCP <- NA
data17$MOCCP[data17$a90c == 4] <- 1
data17$MOCCP[data17$a90c == 1 ] <- 0
data17$MOCCP[data17$a90c == 2] <- 0
data17$MOCCP[data17$a90c == 3 ] <- 0

table(data17$MOCCP)
data17$MOCCPM <- mean(data17$MOCCP, na.rm = T)
table(data17$MOCCPM)
#---------#
# 0 = no parents CCP; 1 = at least 1 parent CCP; 2 = both parents CCP
data17$ParentsCCP <- NA
data17$ParentsCCP[data17$FACCP == 0 & data17$MOCCP == 0] <- 0
data17$ParentsCCP[data17$FACCP == 1 & data17$MOCCP == 0] <- 1
data17$ParentsCCP[data17$FACCP == 0 & data17$MOCCP == 1] <- 1
data17$ParentsCCP[data17$FACCP == 1 & data17$MOCCP == 1] <- 2
table(data17$ParentsCCP)
data17$ParentsCCP <- factor(data17$ParentsCCP)

data17$ParentsCCPM <- NA
data17$ParentsCCPM[data17$FACCP == 0 & data17$MOCCP == 0] <- 0
data17$ParentsCCPM[data17$FACCP == 1 & data17$MOCCP == 0] <- 1
data17$ParentsCCPM[data17$FACCP == 0 & data17$MOCCP == 1] <- 1
data17$ParentsCCPM[data17$FACCP == 1 & data17$MOCCP == 1] <- 2
ParentsCCPM <- mean(data17$ParentsCCPM, na.rm = T)
ParentsCCPM

# parents either or
data17$familyPoli <- NA
data17$familyPoli[data17$ParentsCCP == 0] <- 0
data17$familyPoli[data17$ParentsCCP == 1] <- 1
data17$familyPoli[data17$ParentsCCP == 2] <- 1
data17$familyPoli <- factor(data17$familyPoli)


#---------#
# 0 = no edu; 1 = primary/secondary; 2 = at least one with higher edu; 3 = both higher edu
data17$ParentsEDU <- NA
data17$ParentsEDU[data17$MOedu == 0 & data17$FAedu == 0] <- 0
data17$ParentsEDU[data17$MOedu == 0 & data17$FAedu == 1] <- 1
data17$ParentsEDU[data17$MOedu == 1 & data17$FAedu == 0] <- 1
data17$ParentsEDU[data17$MOedu == 1 & data17$FAedu == 1] <- 1
data17$ParentsEDU[data17$MOedu == 2 & data17$FAedu == 0] <- 2
data17$ParentsEDU[data17$MOedu == 2 & data17$FAedu == 1] <- 2
data17$ParentsEDU[data17$MOedu == 2 & data17$FAedu == 2] <- 3
data17$ParentsEDU[data17$MOedu == 0 & data17$FAedu == 2] <- 2
data17$ParentsEDU[data17$MOedu == 1 & data17$FAedu == 2] <- 2
table(data17$ParentsEDU)
data17$ParentsEDU <- factor(data17$ParentsEDU)


data17$ParentsHEdu <- NA
data17$ParentsHEdu[data17$MOHighEdu == 0 & data17$FAHighEdu == 0] <- 0
data17$ParentsHEdu[data17$MOHighEdu == 1 & data17$FAHighEdu == 0] <- 1
data17$ParentsHEdu[data17$MOHighEdu == 0 & data17$FAHighEdu == 1] <- 1
data17$ParentsHEdu[data17$MOHighEdu == 1 & data17$FAHighEdu == 1] <- 1
table(data17$ParentsHEdu)
data17$ParentsHEdu <- factor(data17$ParentsHEdu)

#Work experience and current job
table(data17$a58)
#1: Currently engaged in non-agricultural work; 
#2: Currently working in agriculture, once had non-agricultural work;
#3: Currently working in farming, no non-agricultural work ; 
#4: No job at the moment, and only farming;
#5: No job at the moment, have had a non-agricultural job;
#6: Never worked

head(data17$a59a) 
- # breakdown data17$a59a)
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
data17$Job <- NA
data17$Job[data17$a58 == 1] <- 0
data17$Job[data17$a58 == 2] <- 1
data17$Job[data17$a58 == 3] <- 1
data17$Job[data17$a58 == 4] <- 1
data17$Job[data17$a58 == 5] <- 2
data17$Job[data17$a58 == 6] <-  2
data17$Job[data17$a59a == 1] <- 7
data17$Job[data17$a59a == 2] <- 6
data17$Job[data17$a59a == 3] <- 5
data17$Job[data17$a59a == 4] <- 0
data17$Job[data17$a59a == 5] <- 3
data17$Job[data17$a59a == 6] <- 4
data17$Job[data17$a59a == 7] <- 2
data17$Job[data17$a59a == 8] <- 3
table(data17$Job)

#Jobnew --> 0 = white collar; 1 = labor; 2 = rural work
data17$Jobnew <- NA
data17$Jobnew[data17$Job == 0] <- 1
data17$Jobnew[data17$Job == 3] <- 1
data17$Jobnew[data17$Job == 2] <- 1
data17$Jobnew[data17$Job == 4] <- 1
data17$Jobnew[data17$Job == 5] <- 0
data17$Jobnew[data17$Job == 6] <- 0
#data17$Jobnew[data17$Job == 1] <- 2
data17$Jobnew[data17$Job == 7] <- 0
table(data17$Jobnew)

#Jobnew1 --> 0 = white collar; 1 = labor; 2 = rural work
data17$Jobnew1 <- NA
data17$Jobnew1[data17$Job == 0] <- 1
data17$Jobnew1[data17$Job == 3] <- 1
data17$Jobnew1[data17$Job == 2] <- 1
data17$Jobnew1[data17$Job == 4] <- 0
data17$Jobnew1[data17$Job == 5] <- 0
data17$Jobnew1[data17$Job == 6] <- 0
#data17$Jobnew[data17$Job == 1] <- 2
data17$Jobnew1[data17$Job == 7] <- 0
table(data17$Jobnew1)


#Jobnew.rural --> 0 = white collar; 1 = labor; 2 = rural work
data17$Jobnew.rural <- NA
data17$Jobnew.rural[data17$Job == 0] <- 1
data17$Jobnew.rural[data17$Job == 3] <- 1
data17$Jobnew.rural[data17$Job == 2] <- 1
data17$Jobnew.rural[data17$Job == 4] <- 1
data17$Jobnew.rural[data17$Job == 5] <- 0
data17$Jobnew.rural[data17$Job == 6] <- 0
data17$Jobnew.rural[data17$Job == 1] <- 2
data17$Jobnew.rural[data17$Job == 7] <- 0
table(data17$Jobnew.rural)

# Job.labor <-  0 = laboers & casual work; rest is same as Job

data17$Job.labor <- ifelse(data17$Job == 2, 0, data17$Job)
#JobClerk
# 2 = rural work & unemployed; 1 = Wokers; 0 = white collar ; 3 = Managment & private business
data17$JobKey <- NA
data17$JobKey[data17$Job == 1] <- 2
data17$JobKey[data17$Job == 2] <- 1
data17$JobKey[data17$Job == 3] <- 1
data17$JobKey[data17$Job == 4] <- 1
data17$JobKey[data17$Job == 5] <- 0
data17$JobKey[data17$Job == 0] <- 1
data17$JobKey[data17$Job == 6] <- 3
data17$JobKey[data17$Job == 7] <- 3
table(data17$JobKey)
data17$JobKey <- factor(data17$JobKey)

#Job urban
# 0= clerk; 1 = labor worker; 2 = business
data17$UrbanJob <- NA
data17$UrbanJob[data17$Job == 2] <- 1
data17$UrbanJob[data17$Job == 3] <- 1
data17$UrbanJob[data17$Job == 4] <- 0
data17$UrbanJob[data17$Job == 5] <- 0
data17$UrbanJob[data17$Job == 0] <- 1
data17$UrbanJob[data17$Job == 6] <- 2
data17$UrbanJob[data17$Job == 7] <- 2
table(data17$UrbanJob)
data17$UrbanJob <- factor(data17$UrbanJob)

#Job merged NO RURAL
# 0 = labor worker; 1 = others
data17$JobD <- ifelse(data17$UrbanJob == 1, 0, 1)
table(data17$JobD)
data17$JobD <- factor(data17$JobD)

JobM <- mean(data17$Job, na.rm = T)
summary(JobM)

data17$a46
#--------------------------------------------------------------------------------------

# 1st: cox hazard analysis on the age citizens' application to the CCP #

ageApp17 <- data17$CCPapp - data17$birth
table(ageApp17)

data17$age18App17 <- ifelse(ageApp17 >= 18, ageApp17, NA)
table(data17$age18App17)

data17$age18App <- ifelse(ageApp17 >= 18, ageApp17, NA)
table(data17$age18App)

data17$age18App[is.na(data17$age18App)] <- 0

time15 <- data17$age18App - 18
table(time15)

time15a <- data17$age18App
table(time15a)
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------ //
#1.a CCP acceptance age likelihood

Acc1217 <- ifelse(data17$CCPmemb >= 2012 & data17$CCPmemb < 2018, 1, 0)
table(Acc1217)

#------------------------------------------------------------------------------------ 

ageAcc17 <- data17$CCPmemb - data17$birth
table(ageAcc17)

age18Acc17 <- ifelse(ageApp17 >= 18 , ageApp17, NA)
table(age18Acc17)

mean(age18Acc17, na.rm = T)


#------------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------------ 

# Set of dependent variables
data17$Ref_Acc_u30 <- NA
data17$Ref_Acc_u30[data17$CCPapp >= 2012 & data17$birth >= 1987 & data17$CCPmemb >= 2012] <- 1
data17$Ref_Acc_u30[data17$CCPapp < 2012 & data17$birth >= 1987 & data17$CCPmemb >= 2012] <- 2
data17$Ref_Acc_u30[data17$CCPapp >= 2012 & data17$birth < 1987 & data17$CCPmemb >= 2012] <- 3
data17$Ref_Acc_u30[data17$CCPapp < 2012 & data17$birth < 1987 & data17$CCPmemb >= 2012] <- 4
data17$Ref_Acc_u30[data17$CCPapp < 2012 & data17$birth >= 1987 & data17$CCPmemb < 2012] <- 5
data17$Ref_Acc_u30[data17$CCPapp == 0 & data17$birth >=  1987 & data17$CCPmemb == 0] <- 6
data17$Ref_Acc_u30[data17$CCPapp == 0 & data17$birth <  1987 & data17$CCPmemb == 0] <- 7
data17$Ref_Acc_u30[data17$CCPapp < 2012 & data17$CCPapp > 0 & data17$birth < 1985 & data17$CCPmemb == 0] <- 8
data17$Ref_Acc_u30[data17$CCPapp >= 2012 & data17$birth >= 1987 & data17$CCPmemb == 0] <- 9
data17$Ref_Acc_u30[data17$CCPapp >= 2012 & data17$birth <  1987 & data17$CCPmemb == 0] <- 10

data17$Ref_Acc_u30[is.na(data17$Ref_Acc_u30)] <- 0

table(data17$Ref_Acc_u30)
data17$Ref_Acc_u30 <- factor(data17$Ref_Acc_u30)


#new DV - Accepted refused
data17$RefAcc <- NA
data17$RefAcc[data17$CCPapp >= 2012 & data17$CCPmemb >= 0] <- 0
data17$RefAcc[data17$CCPapp >= 2012 & data17$CCPmemb >= 2012] <- 1
table(data17$RefAcc)

#new DV - Accepted refused + 6
data17$RefAcc6 <- NA
data17$RefAcc6[data17$CCPapp >= 2011 & data17$CCPmemb >= 0] <- 0
data17$RefAcc6[data17$CCPapp >= 2011 & data17$CCPmemb >= 2011] <- 1
table(data17$RefAcc6)


#new DV - Accepted refused + 4
data17$RefAcc4 <- NA
data17$RefAcc4[data17$CCPapp >= 2013 & data17$CCPmemb >= 0] <- 0
data17$RefAcc4[data17$CCPapp >= 2013 & data17$CCPmemb >= 2013] <- 1
table(data17$RefAcc4)


#new DV - Accepted refused + 10
data17$RefAcc10 <- NA
data17$RefAcc10[data17$CCPapp >= 2007 & data17$CCPmemb >= 0] <- 0
data17$RefAcc10[data17$CCPapp >= 2007 & data17$CCPmemb >= 2007] <- 1
table(data17$RefAcc10)

#new DV - Accepted refused + 15
data17$RefAcc15 <- NA
data17$RefAcc15[data17$CCPapp >= 2002 & data17$CCPmemb >= 0] <- 0
data17$RefAcc15[data17$CCPapp >= 2002 & data17$CCPmemb >= 2002] <- 1
table(data17$RefAcc15)

# --------- DV APPLICANTS

#new DV - Applicants or not
data17$AppNo <- 0
data17$AppNo[data17$CCPapp == 0] <- 0
data17$AppNo[data17$CCPapp >= 2012] <- 1
table(data17$AppNo)

#new DV - Applicants or not + 6
data17$AppNo6 <- 0
data17$AppNo6[data17$CCPapp == 0] <- 0
data17$AppNo6[data17$CCPapp >= 2011] <- 1
table(data17$AppNo6)

#new DV - Applicants or not + 4
data17$AppNo4 <- 0
data17$AppNo4[data17$CCPapp == 0] <- 0
data17$AppNo4[data17$CCPapp >= 2013] <- 1
table(data17$AppNo4)

#new DV - Applicants or not + 10
data17$AppNo10 <- 0
data17$AppNo10[data17$CCPapp == 0] <- 0
data17$AppNo10[data17$CCPapp >= 2007] <- 1
table(data17$AppNo10)

#new DV - Applicants or not + 12
data17$AppNo15 <- 0
data17$AppNo15[data17$CCPapp == 0] <- 0
data17$AppNo15[data17$CCPapp >= 2002] <- 1
table(data17$AppNo15)


# creating dummies for the tests and the probit models

NoApp17 <- ifelse(data17$Ref_Acc_u30 == 7, 1, 0)
table(NoApp15)
NoAppu30.17 <- ifelse(data17$Ref_Acc_u30 == 6, 1, 0)
table(NoAppu30.17)
Ref1217 <- ifelse(data17$Ref_Acc_u30 == 9 |
                    data17$Ref_Acc_u30 == 10, 1, 0)
table(Ref1217)
Acc1216 <- ifelse(data17$Ref_Acc_u30 == 1 |
                    data17$Ref_Acc_u30 == 3, 1, 0)
table(Acc1216)

# No applicants u30 and those refused 2008-2012
NoAppu30.Ref1217 <- NA
NoAppu30.Ref1217[NoAppu30.17 == 1 & Ref1217 == 0] <- 0
NoAppu30.Ref1217[NoAppu30.17 == 0 & Ref1217 == 1] <- 1
table(NoAppu30.Ref1217)

# No applicants u30 and those accepted 2008-2012
NoAppu30.Acc1216 <- NA
NoAppu30.Acc1216[NoAppu30.17 == 1 & Acc1216 == 0] <- 0
NoAppu30.Acc1216[NoAppu30.17 == 0 & Acc1216 == 1] <- 1
table(NoAppu30.Acc1216)

# No applicants u30 and applicants
NoAppu30.Acc1216 <- NA
NoAppu30.Acc1216[NoAppu30.17 == 1 & Acc1216 == 0 & Ref1217 == 0] <- 0
NoAppu30.Acc1216[NoAppu30.17 == 0 & Acc1216 == 1 & Ref1217 == 0] <- 1
NoAppu30.Acc1216[NoAppu30.17 == 0 & Acc1216 == 0 & Ref1217 == 1] <- 1
table(NoAppu30.Acc1216)

# No applicants over 30 and applicants
NoAppov30.App1216 <- NA
NoAppov30.App1216[NoAppu30.17 == 0 & Acc1216 == 0 & Ref1217 == 0] <- 0
NoAppov30.App1216[NoAppu30.17 == 0 & Acc1216 == 1 & Ref1217 == 0] <- 1
NoAppov30.App1216[NoAppu30.17 == 0 & Acc1216 == 0 & Ref1217 == 1] <- 1
table(NoAppov30.App1216)

# Accepted & refused 2008-2012 -> 0: refused; 1: accepted
Acc1216.Ref1216 <- NA
Acc1216.Ref1216[Ref1217 == 1 & Acc1216 == 0] <- 0
Acc1216.Ref1216[Ref1217 == 0 & Acc1216 == 1] <- 1
table(Acc1216.Ref1216)