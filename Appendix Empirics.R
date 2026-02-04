library(modelsummary)
library(patchwork)
library(wesanderson)

# MAIN MODELS WITH FULL EDUCATION AND AGE VARIABLES APPENDIX 
# A1
modelsummary(list.app, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")

# A2
modelsummary(list.accp, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")

# INTERACTION PLOTS MAIN FINDINGS  - JUST FOR THE PLOT#
app17.jobnew.plot <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                           factor(ageCATa) +Jobnew+ ethn +
                           hukouD + 
                           redgeography.cont + Jobnew*redgeography.cont,
                         family = binomial(link = "probit"),
                         data = data17)


acc17.jobnew.plot <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                           factor(ageCATa) +Jobnew+ ethn +
                           hukouD + 
                           redgeography + Jobnew*redgeography,
                         family = binomial(link = "probit"),
                         data = data17)

interplot(m = acc17.jobnew.plot, 
          var1 = "Jobnew", var2 = "redgeography", hist = TRUE)

# •••••••••••• Heckman selection model ••••••••••••••••.#

acc10.jobnew.rob0 <- glm(formula = RefAcc ~ gender + ParentsHEdu + 
                           ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data10)

acc10.jobnew.rob1 <- glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + familyPoli +
                           ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data10)


acc15.jobnew.rob0 <- glm(formula = RefAcc ~ gender + ParentsHEdu + 
                           ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data15)

acc15.jobnew.rob1 <- glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + familyPoli +
                           ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data15)

acc17.jobnew.rob0 <- glm(formula = RefAcc ~ gender + ParentsHEdu + 
                           factor(ageCATa) + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data17)

acc17.jobnew.rob1 <- glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + familyPoli +
                           factor(ageCATa) + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                           factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                         data = data17)

list.heck <- list(acc10.jobnew.rob0, acc10.jobnew.rob1, 
                  acc15.jobnew.rob0, acc15.jobnew.rob1, 
                  acc17.jobnew.rob0, acc17.jobnew.rob1)

# THIS IS TABLE A3 APPENDIX
modelsummary(list.heck, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")
# Heckman selection test results supports the fixed effect model from main results,
# the CGSS2010 provides a relationship of equality (laborers apply slighly less = accepted
# slightly less), while CGSS2015 has a systematic refuse of laborers vs. white-collar
# (laborers do not apply differently than white-collar, YET they are accepted systematically less)
# defense of CCP family not influential on CCP recruitment assumption
coefs <- mvrnorm(n = 10000, mu = coefficients(acc15.jobnew.rob0), Sigma = vcov(acc15.jobnew.rob0))
coefficients(acc15.jobnew.rob0)
sqrt(diag(vcov(acc15.jobnew.rob0)))

# Coefficients model & simulated model
coefficients(acc15.jobnew.rob0)
colMeans(coefs)

# Standard Errors model & simulated model
sqrt(diag(vcov(acc15.jobnew.rob0)))
apply(coefs, 2, sd) # standard deviation of simulated coefficients

summary(coefs)

# HECKMAN MODELS 2010,2015,2017
#• 2010
heckman.10 <- selection(selection = AppNo ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD + familyPoli + ethn + education1 +
                          redgeography.cont + Jobnew*redgeography.cont, 
                        outcome = RefAcc ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD +ethn +
                          redgeography.cont + Jobnew*redgeography.cont,
                        data = data10, method = "2step")
stargazer(heckman.10)

#• 2015
heckman.15 <- selection(selection = AppNo ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD + familyPoli + ethn + education1 +
                          redgeography.cont + Jobnew*redgeography.cont, 
                        outcome = RefAcc ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD +ethn +
                          redgeography.cont + Jobnew*redgeography.cont,
                        data = data15, method = "2step")
stargazer(heckman.15)


#• 2017
heckman.17 <- selection(selection = AppNo ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD + familyPoli + ethn + education1 +
                          redgeography.cont + Jobnew*redgeography.cont, 
                        outcome = RefAcc ~ Jobnew + gender + ParentsHEdu +
                          ageCATa + hukouD +ethn  +
                          redgeography.cont + Jobnew*redgeography.cont,
                        data = data17, method = "2step")
stargazer(heckman.17)


# summarizes HECKMAN
# TABLE A4
rob.heck.10 <- list(acc10.jobnew,heckman.10)
stargazer(rob.heck.10)

# TABLE A5
rob.heck.15 <- list(acc15.jobnew, heckman.15)
stargazer(rob.heck.15)

#TABLE A6
rob.heck.17 <- list(acc17.jobnew, heckman.17)
stargazer(rob.heck.17)

#••• ROBUSTNESS CHECKS •••#

# MAIN MODELS WITH OLS #
# 2010
app10.ols <- lm(AppNo ~ gender + ParentsHEdu + factor(education1) + ageCAT +factor(Jobnew) + 
                  ethn + hukouD + factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                data = data10)

acc10.ols <- lm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                  ageCAT +factor(Jobnew) + ethn +
                  hukouD + 
                  factor(redgeography) + factor(Jobnew)*factor(redgeography),
                data = data10)

# 2015
app15.ols <- lm(AppNo ~ gender + ParentsHEdu + factor(education1) + ageCAT +factor(Jobnew) + 
                  ethn + hukouD + factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                data = data15)

acc15.ols <- lm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                  ageCAT +factor(Jobnew) + ethn +
                  hukouD + 
                  factor(redgeography) + factor(Jobnew)*factor(redgeography),
                data = data15)

# 2017
app17.ols <- lm(AppNo ~ gender + ParentsHEdu + factor(education1) + factor(ageCATa) +factor(Jobnew) + 
                  ethn + hukouD + factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                data = data17)

acc17.ols <- lm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                  factor(ageCATa) +factor(Jobnew) + ethn +
                  hukouD + 
                  factor(redgeography) + factor(Jobnew)*factor(redgeography),
                data = data17)

# summary OLS
# TABLE A7
app.ols <- list(app10.ols, app15.ols,app17.ols)
msummary(app.ols, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")

# TABLE A8
acc.ols <- list(acc10.ols, acc15.ols,acc17.ols)
msummary(acc.ols, estimate = "{estimate}{stars}", vcov = ~redgeography)

#---- CHANGING IV MODEL

# • moving "self-employed" to labor workers 
# Reason: it is a very big part of the sample so test if that ticks off the relationship
# 2010
app10.jobnew1 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data10)

acc10.jobnew1 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data10)

# 2015
app15.jobnew1 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data15)

acc15.jobnew1 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data15)

# 2017
app17.jobnew1 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data17)

acc17.jobnew1 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew1) + ethn +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew1)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data17)

# TABLE A9
list.app.jobnew1 <- list(app10.jobnew1,app15.jobnew1, app17.jobnew1)
modelsummary(list.app.jobnew1, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")

# TABLE A10
list.acc.jobnew1 <- list(acc10.jobnew1,acc15.jobnew1, acc17.jobnew1)
modelsummary(list.acc.jobnew1, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")


# • adding rural jobs
# 2010
app10.jobrural <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data10)

acc10.jobrural <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data10)

# 2015
app15.jobrural <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data15)

acc15.jobrural <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data15)

# 2017
app17.jobrural <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data17)

acc17.jobrural <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew.rural) + ethn +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew.rural)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data17)

# TABLE A11
list.app.jobrural <- list(app10.jobrural,app15.jobrural, app17.jobrural)
modelsummary(list.app.jobrural, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")

# TABLE A12
list.acc.jobrural <- list(acc10.jobrural,acc15.jobrural, acc17.jobrural)
modelsummary(list.acc.jobrural, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")


# •Changing DV•

# main results 2010 but DV is +6
app10.jobnew6 <- glm(AppNo6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data10)

acc10.jobnew6 <- glm(RefAcc6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data10)

# main results 2015 but DV is +6
app15.jobnew6 <- glm(AppNo6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data15)

acc15.jobnew6 <- glm(RefAcc6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data15)

# main results 2017 but DV is +6
app17.jobnew6 <- glm(AppNo6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew) + 
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data17)

acc17.jobnew6 <- glm(RefAcc6 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data17)

# TABLE A13
list.app6 <- list(app10.jobnew6,app15.jobnew6,app17.jobnew6)
modelsummary(list.app6, estimate = "{estimate}{stars}", vcov = ~redgeography)

# TABLE A14
list.accp6 <- list(acc10.jobnew6,acc15.jobnew6,acc17.jobnew6)
modelsummary(list.accp6, estimate = "{estimate}{stars}", vcov = ~redgeography)

#  • ADDING DV +4

# main results 2010 but DV is +4
app10.jobnew4 <- glm(AppNo4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data10)

acc10.jobnew4 <- glm(RefAcc4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data10)

# main results 2015 but DV is +4
app15.jobnew4 <- glm(AppNo4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data15)

acc15.jobnew4 <- glm(RefAcc4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCAT +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data15)

# main results 2017 but DV is +4
app17.jobnew4 <- glm(AppNo4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew) + 
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                     family = binomial(link = "probit"),
                     data = data17)

acc17.jobnew4 <- glm(RefAcc4 ~ gender + ParentsHEdu + factor(education1) + 
                       ageCATa +factor(Jobnew) +
                       hukouD + 
                       factor(redgeography) + factor(Jobnew)*factor(redgeography),
                     family = binomial(link = "probit"),
                     data = data17)

# TABLE A15
list.app4 <- list(app10.jobnew4,app15.jobnew4,app17.jobnew4)
modelsummary(list.app4, estimate = "{estimate}{stars}", vcov = ~redgeography)

# TABLE A16
list.accp4 <- list(acc10.jobnew4,acc15.jobnew4,acc17.jobnew4)
modelsummary(list.accp4, estimate = "{estimate}{stars}", vcov = ~redgeography)


#  • ADDING DV +10
# main results 2010 but DV is +10
app10.jobnew10 <- glm(AppNo10 ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data10)

acc10.jobnew10 <- glm(RefAcc10 ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data10)

# main results 2015 but DV is +10
app15.jobnew10 <- glm(AppNo10 ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data15)

acc15.jobnew10 <- glm(RefAcc10 ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data15)

# main results 2017 but DV is +10
app17.jobnew10 <- glm(AppNo10 ~ gender + ParentsHEdu + factor(education1) + 
                        factor(ageCATa) +factor(Jobnew) +
                        hukouD + 
                        redgeography.cont + factor(Jobnew)*redgeography.cont, 
                      family = binomial(link = "probit"),
                      data = data17)

acc17.jobnew10 <- glm(RefAcc10 ~ gender + ParentsHEdu + factor(education1) + 
                        ageCATa +factor(Jobnew) +
                        hukouD + 
                        factor(redgeography) + factor(Jobnew)*factor(redgeography),
                      family = binomial(link = "probit"),
                      data = data17)

# TABLE A17
list.app10 <- list(app10.jobnew10,app15.jobnew10,app17.jobnew10)
modelsummary(list.app10, estimate = "{estimate}{stars}", vcov = ~redgeography)

# TABLE A18
list.accp10 <- list(acc10.jobnew10,acc15.jobnew10,acc17.jobnew10)
modelsummary(list.accp10, estimate = "{estimate}{stars}", vcov = ~redgeography)

## PLOTS!

# Party - State convergence

stateparty <- database_state_party
value <- as.factor(stateparty$value)
year <- as.factor(stateparty$year)
year1 <- stateparty$year

urban <- stateparty %>%
  filter(value %in% c( "labor workers", "white collars"))


urb <- urban %>%
  ggplot(., aes(x = partypercent, y = statepercent,  color = value, shape = value)) +
  geom_point() +
  geom_smooth(aes(color = value, fill = value), method = "lm") +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  geom_label_repel(aes(label = year,
                       fill = (value)), color = 'white',
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_segment(aes(x = 15, y = 60, xend = 8, yend = 52),
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "#F8766D") +
  geom_segment(aes(x = 25, y = 25, xend = 34, yend = 35),
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "#00BFC4") +
  labs(y = "% Chinese Workforce", x = "% CCP membership") +
  theme_ipsum(axis_title_size = 25) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        legend.position="bottom")

urb + geom_segment(aes(x = 18, y = 60, xend = 8, yend = 40, color = "darkgrey"),
                   arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = 20, y = 30, xend = 34, yend = 40),
               arrow = arrow(length = unit(0.5, "cm")))


# DENSITY CCP BY PROVINCE & GRP pc

ggplot(data17, 
       aes(redgeography.con, GRP_pc.cont)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  xlab("Provinces by CCP membership density (from lowest to most populated)") + 
  ylab("Provinces by Gross Regional Product per capita (from poorest to wealthier)") +
  labs(color = "Provinces by CCP density") +
  theme_light() +
  theme(legend.position="bottom")

# APPLICATION AND ACCEPTANCE DEPENDING ON AGE
apply.plot <- ggplot(data17, aes(age18App17, factor(AppNo), fill = factor(AppNo)))+
  geom_violin()+
  xlab("Age") + ylab("Citizens Application Decision") +
  scale_fill_manual(values = wes_palette("BottleRocket2", n = 2),
                    labels = c("Not Applied", "Applied")) +
  scale_x_continuous(breaks=seq(18, 80, by = 3)) +
  labs(fill = "Response") +
  theme_light() +
  theme(legend.position="bottom") 

acc.plot <- ggplot(data17, aes(age18Acc17, RefAcc, fill = factor(RefAcc))) +
  geom_violin() +
  xlab("Age") + ylab("CCP Response") +
  scale_fill_manual(values = wes_palette("BottleRocket2", n = 2),
                    labels = c("Refused", "Accepted")) +
  scale_x_continuous(breaks=seq(18, 100, by = 3)) +
  labs(fill = "Response") +
  theme_light() +
  theme(legend.position="bottom") 

apply.plot / acc.plot

# ADDING NEW DATA - CGSS 2012 & 2013 #


# Replication main results with CGSS 2012
app12.jobnew0 <- glm(AppNo ~  factor(Jobnew) , 
                     family = binomial(link = "probit"),
                     data = data12)

app12.jobnew01 <- glm(AppNo ~ gender + ParentsHEdu + HEdu +
                        ageCAT +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data12)

acc12.jobnew0 <- glm(RefAcc ~  factor(Jobnew), 
                     family = binomial(link = "probit"),
                     data = data12)

acc12.jobnew01 <- glm(RefAcc ~ gender + ParentsHEdu + education1 + 
                        factor(ageCAT) +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data12)


# interplot(probit.acc15.jobnew, var1 = 'Jobnew', var2 = 'redgeography')

list.new12 <- list(app12.jobnew0,app12.jobnew01, 
                   acc12.jobnew0,acc12.jobnew01)
modelsummary(list.new12, estimate = "{estimate}{stars}", vcov = ~redgeography)

# Replication main results with CGSS 2013

app13.jobnew0 <- glm(AppNo ~  factor(Jobnew) , 
                     family = binomial(link = "probit"),
                     data = data13)

app13.jobnew01 <- glm(AppNo ~ gender + ParentsHEdu + HEdu + 
                        ageCAT +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data13)

acc13.jobnew0 <- glm(RefAcc ~  factor(Jobnew), 
                     family = binomial(link = "probit"),
                     data = data13)

acc13.jobnew01 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        factor(ageCAT) +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data13)

# interplot(probit.acc15.jobnew, var1 = 'Jobnew', var2 = 'redgeography')

list.new13 <- list(app13.jobnew0,app13.jobnew01, 
                   acc13.jobnew0,acc13.jobnew01)
modelsummary(list.new13, estimate = "{estimate}{stars}", vcov = ~redgeography)


# PLOTS FOR APPENDIX

# PLOT 1 APPENDIX
# density plots CGSSs and national

density_membership_cgss <- read_excel("~/Desktop/density_membership_cgss.xlsx")

density.data <- density_membership_cgss

density.acc <- ggplot(data=density.data, aes(x=dens.acc, group=value, fill=value)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlab("Accepted on Total Population") + 
  theme_ipsum()

# plot 2 with rural areas
minidataset_rural <- read_excel("~/Desktop/minidataset rural.xlsx")
View(minidataset_rural)

rural <- minidataset_rural

value <- as.factor(rural$value)
year <- as.factor(rural$year)


rur <- ggplot(rural, aes(x = partypercent, y = statepercent, 
                         color = year)) +
  scale_color_gradientn(colors=met.brewer("OKeeffe2",type="continuous"))+
  geom_point(size = 6) +
  geom_segment(aes(
    xend=c(tail(partypercent, n=-2018), NA), 
    yend=c(tail(statepercent, n=-2018), NA)
  )) +
  facet_wrap(~rural$value) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  labs(y = "Groups' Share of Chinese Workforce (National)", 
       x = "Groups' Share of CCP membership") +
  theme_ipsum(axis_title_size = 10) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        legend.position="right")
