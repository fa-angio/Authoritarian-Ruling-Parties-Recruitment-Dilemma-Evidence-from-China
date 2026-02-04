library(modelsummary)
library(patchwork)
library(wesanderson)
library(here)
# New script on CCP membership recruitment strategies

# main results 2010
app10.jobnew0 <- glm(AppNo ~  factor(Jobnew) , 
                     family = binomial(link = "probit"),
                     data = data10)

app10.jobnew01 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data10)

app10.jobnew <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                      ageCAT +factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                    family = binomial(link = "probit"),
                    data = data10)

acc10.jobnew0 <- glm(RefAcc ~  factor(Jobnew), 
                     family = binomial(link = "probit"),
                     data = data10)

acc10.jobnew01 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        ageCAT +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data10)

acc10.jobnew <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                      ageCAT +factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography),
                    family = binomial(link = "probit"),
                    data = data10)

# interplot(probit.acc15.jobnew, var1 = 'Jobnew', var2 = 'redgeography')

list.2010 <- list(app10.jobnew0,app10.jobnew01,app10.jobnew, 
                  acc10.jobnew0,acc10.jobnew01, acc10.jobnew)
modelsummary(list.2010, estimate = "{estimate}{stars}", vcov = ~redgeography)


# main results 2015
app15.jobnew0 <- glm(AppNo ~  factor(Jobnew) , 
                     family = binomial(link = "probit"),
                     data = data15)

app15.jobnew01 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        u30+factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data15)

app15.jobnew <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                      u30+factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                    family = binomial(link = "probit"),
                    data = data15)

acc15.jobnew0 <- glm(RefAcc ~  factor(Jobnew), 
                     family = binomial(link = "probit"),
                     data = data15)

acc15.jobnew01 <- glm(RefAcc ~ gender + ParentsHEdu + education1 + 
                        u30+  factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data15)


acc15.jobnew <- glm(RefAcc ~ gender + ParentsHEdu +factor(education1)+
                      u30 + factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography),
                    family = binomial(link = "probit"),
                    data = data15)


list.new <- list(app15.jobnew0,app15.jobnew01, app15.jobnew, 
                 acc15.jobnew0,acc15.jobnew01, acc15.jobnew)
modelsummary(list.new, estimate = "{estimate}{stars}", vcov = ~redgeography)


# main results 2017
app17.jobnew0 <- glm(AppNo ~  factor(Jobnew) , 
                     family = binomial(link = "probit"),
                     data = data17)

app17.jobnew01 <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                        factor(ageCATa) +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data17)

app17.jobnew <- glm(AppNo ~ gender + ParentsHEdu + factor(education1) + 
                      factor(ageCATa) +factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography), 
                    family = binomial(link = "probit"),
                    data = data17)

acc17.jobnew0 <- glm(RefAcc ~  factor(Jobnew), 
                     family = binomial(link = "probit"),
                     data = data17)

acc17.jobnew01 <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                        factor(ageCATa) +factor(Jobnew) + ethn +
                        hukouD + 
                        factor(redgeography), 
                      family = binomial(link = "probit"),
                      data = data17)


acc17.jobnew <- glm(RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                      factor(ageCATa) +factor(Jobnew) + ethn +
                      hukouD + 
                      factor(redgeography) + factor(Jobnew)*factor(redgeography),
                    family = binomial(link = "probit"),
                    data = data17)



list.2017 <- list(app17.jobnew0,app17.jobnew01,app17.jobnew, 
                  acc17.jobnew0,acc17.jobnew01,acc17.jobnew)
modelsummary(list.2017, estimate = "{estimate}{stars}", vcov = ~redgeography)

## • SUMMARY TO MAIN MODELS! ###
# list applicants 2010,2015,2017
list.app <- list(app10.jobnew01,app10.jobnew,app15.jobnew01,app15.jobnew,app17.jobnew01,app17.jobnew)
modelsummary(list.app, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")


# list accepted 2010,2015,2017
list.accp <- list(acc10.jobnew01,acc10.jobnew,acc15.jobnew01,acc15.jobnew,acc17.jobnew01,acc17.jobnew)
modelsummary(list.accp, estimate = "{estimate}{stars}", vcov = ~redgeography, output = "latex")



# list APP & ACC 2010-2017
list.tot <- list(app10.jobnew,app15.jobnew,app17.jobnew,
                 acc10.jobnew,acc15.jobnew,acc17.jobnew)

modelsummary(list.tot, estimate = "{estimate}{stars}", vcov = ~redgeography)

# visualization
list.app.fig <- list(
  "App 2010" = glm(formula = AppNo ~ gender + ParentsHEdu + factor(education1) + 
                     ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                     factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                   data = data10),
  "App 2015" = glm(formula = AppNo ~ gender + ParentsHEdu + factor(education1) + 
                     u30 + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                     factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                   data = data15),
  "App 2017" = glm(formula = AppNo ~ gender + ParentsHEdu + factor(education1) + 
                     factor(ageCATa) + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                     factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
                   data = data17))

list.acc.fig <- list(
  "2010" = glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                 ageCAT + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                 factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
               data = data10),
  "2015" = glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                 u30 + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                 factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
               data = data15),
  "2017" = glm(formula = RefAcc ~ gender + ParentsHEdu + factor(education1) + 
                 factor(ageCATa) + factor(Jobnew) + ethn + hukouD + factor(redgeography) + 
                 factor(Jobnew) * factor(redgeography), family = binomial(link = "probit"), 
               data = data17))

cm <- c('factor(Jobnew)1' = 'Workers')

app.model <- modelplot(list.app.fig, coef_map = cm, vcov = ~redgeography)+
  geom_vline(xintercept = 0, color = 'orange')+
  theme_light() +
  labs(title = 'Applicants Likelihood compared to White-Collars') +
  scale_color_manual(values = wes_palette('Darjeeling1')) + theme(legend.position="none")

acc.model <- modelplot(list.acc.fig, coef_map = cm, vcov = ~redgeography)+
  geom_vline(xintercept = 0, color = 'orange')+
  theme_light() +
  labs(title = 'Acceptance Likelihood compared to White-Collars') +
  scale_color_manual(values = wes_palette('Darjeeling1'))


model.tot <-  app.model + acc.model
model.tot

ggsave(filename = here("Figures/Fig3.pdf"),
       dpi = "retina",
       width = 11,
       height = 6)


