rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(effects)
library(DHARMa)
library(car)
library(visreg)
library(emmeans)
library(multcomp)
library(sjPlot)
library(MuMIn)


# Load data
raw <- read.csv2(here("data", "Verifieringsstr�ckor_2021_2022_2023.csv"))

# 2021 ----
v.2021 <- raw %>% 
  filter(�r == 2021) %>% 
  dplyr::select(c(�r, Omr�de, Shape__Len, Bredd, F�rekomst, T�thet, Varierande, Best�ndet, Missades.med.bilen)) %>% 
  mutate(Bredd = as.factor(Bredd)) %>% 
  mutate(F�rekomst = as.factor(F�rekomst)) %>% 
  mutate(T�thet = as.factor(T�thet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Best�ndet  = as.factor(Best�ndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen))
v.2021

# Beskrivning av vilka typer av objekt som hittades l�ngs verifieringsstr�ckan
# Byt til procent
v.2021.tot <- v.2021 %>% 
  group_by(T�thet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  group_by(Missades.med.bilen) %>% 
  mutate(per = ifelse(Missades.med.bilen == "Nej", sum(per), per)) %>%
  ungroup()
v.2021.tot

pie1 <- read.csv2(here("data", "summary2021.csv"))


pie(pie1$procent, labels=paste(pie1$T�thet, sep = "   ", round(pie1$procent, 2), "%"), 
    col = c("#9CCC65", "#FFD54F", "#FFB300", "#FF6F00"), main = "Verifiering 2021: Uppsala och Heby")

legend("bottomleft", c("Nej", "Ja"), fill = c("#9CCC65", c("#FFD54F", "#FFB300", "#FF6F00")), 
       title = "Missades med bilen", box.col = "white")


# 2022 ----
v.2022 <- raw %>% 
  filter(�r == 2022) %>% 
  dplyr::select(c(�r, Omr�de, Shape__Len, Bredd, F�rekomst, T�thet, Varierande, Best�ndet, Missades.med.bilen)) %>% 
  mutate(Bredd = as.factor(Bredd)) %>% 
  mutate(F�rekomst = as.factor(F�rekomst)) %>% 
  mutate(T�thet = as.factor(T�thet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Best�ndet  = as.factor(Best�ndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen))
v.2022

# Beskrivning av vilka typer av objekt som hittades l�ngs verifieringsstr�ckan
# Byt til procent
v.2022.tot <- v.2022 %>% 
  filter(Missades.med.bilen != "") %>% 
  group_by(T�thet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  group_by(Missades.med.bilen) %>% 
  mutate(per = ifelse(Missades.med.bilen == "Nej", sum(per), per)) %>%
  ungroup()
v.2022.tot

pie2 <- read.csv2(here("data", "summary2022.csv"))


pie(pie2$procent, labels=paste(pie2$T�thet, sep = "   ", round(pie2$procent, 2), "%"), 
    col = c("#9CCC65", "#FFD54F", "#FFB300", "#FF6F00"), main = "Verifiering 2022: Uppsala, Heby och Sveg ")

legend("bottomleft", c("Nej", "Ja"), fill = c("#9CCC65", c("#FFD54F", "#FFB300", "#FF6F00")), 
       title = "Missades med bilen", box.col = "white")


# Model ----
db <- raw %>% 
  filter(Missades.med.bilen != "") %>% 
  dplyr::select(c(�r, Omr�de, Shape__Len, Observer, Bredd, F�rekomst, T�thet, Varierande, Best�ndet, Missades.med.bilen)) %>% 
  mutate(�r = as.factor(�r)) %>% 
  mutate(Omr�de = as.factor(Omr�de)) %>% 
  mutate(Observer = as.factor(Observer)) %>% 
  mutate(Bredd = as.integer(Bredd)) %>% 
  mutate(F�rekomst = as.factor(F�rekomst)) %>% 
  mutate(T�thet = as.factor(T�thet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Best�ndet  = as.factor(Best�ndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen)) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= 1, "Nej"=0)) %>%   
  filter(T�thet != "Dominerande >90%") # Bara 3 obs. G�r inte att ha i modellen
db 
summary(db)

mod.all <- glmmTMB(Missades.med.bilen ~ Best�ndet + T�thet + Omr�de + F�rekomst + Bredd + Varierande + Shape__Len +
                     Best�ndet:T�thet + Best�ndet:Shape__Len + F�rekomst:T�thet + Best�ndet:F�rekomst, family= "binomial", data=db) 


mod.all1 <- glmmTMB(Missades.med.bilen ~ Best�ndet + T�thet +  F�rekomst + Bredd + Varierande + Shape__Len +
                   + (1 | Omr�de), family= binomial, data=db) 

mod.all2 <- glm(Missades.med.bilen ~ Best�ndet + T�thet +  F�rekomst + Bredd + Varierande + Shape__Len + Omr�de,
                  family= "binomial", data=db) 

mod.all3 <- glmmTMB(Missades.med.bilen ~ Best�ndet + T�thet +  F�rekomst + Bredd + Varierande + Shape__Len,
                    family= "binomial", data=db) 

# Mumin ----
options(na.action=na.fail)
dredge <- dredge(mod.all)
top_model <- get.models(dredge, subset = 1)[[1]]
summary(top_model)
summary(model.avg(dredge, subset = delta <= 2))
options(na.action = "na.omit") 
# Model averaging (full) showed that Best�ndets l�ge, Omr�de, and length were in the "best model" (weight= 0.36).
# T�thet was included in the second best, but I chose to include length as I think t�thet and length are
# similar variables. 
# ----
summary(mod.all)
mod_dharma1 <- mod.all2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
# Bara Best�ndets placering, omr�de, och best�ndets l�ngd var av signifikans.K�r egen analys f�r enskilda omr�den d�.

# Alla omr�den:
mod1 <- glm(Missades.med.bilen ~ Best�ndet + Omr�de + Shape__Len, family= "binomial", data=db) #AICc= 246.4581
AICc(mod1)
vif(mod1)
mod2 <- glmmTMB(Missades.med.bilen ~ Best�ndet * Shape__Len, family= "binomial", data=db) #AICc= 250.5709
AICc(mod2)
anova(mod1, mod2, test = "Chi") #simpler model not significantly worse. Using without interaction

summary(mod1) #estimates are log-odds
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
car::Anova(mod1, type= "III")
tab_model(mod1)

visreg(mod1, scale = "response") # log-odds

# Post-hoc: t�thet
t�t.em <- emmeans(mod1, ~ T�thet , type="response") # probabilities
t�t.emms <- emmeans(mod1, "T�thet") # estimated marginal means
# Effect size:
print(eff_size(t�t.em, sigma=sigma(mod1), edf=df.residual(mod1))) #
eff_size(pairs(t�t.em), sigma(mod1), df.residual(mod1), method = "identity") # another way of writing the above
# ----
pairs(t�t.emms)
plot(t�t.emms, comparisons=TRUE)

# Adam Fl�hr:
em.t�t <- as.data.frame(t�t.em)
ggplot(em.t�t, aes(T�thet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(t�t.em, Letters = letters)
em <- as.data.frame(em)
gg.t�thet <- ggplot(em, aes(T�thet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_text(aes(y = 1, label = .group)) +
  xlab("Best�ndets t�thet") +
  ylab("Sannolikheten att missa ett best�nd (1= helt sannolikt)")+
  theme_bw() 
gg.t�thet

# Post-hoc: Best�ndets placering
best.em <- emmeans(mod1, ~ Best�ndet , type="response")
best.emms <- emmeans(mod1, "Best�ndet")
# Effect size:
print(eff_size(best.em, sigma=sigma(mod1), edf=df.residual(mod1)))
eff_size(pairs(best.em), sigma(mod1), df.residual(mod1), method = "identity") # another way of writing the above
# ----
pairs(best.emms)
plot(best.emms, comparisons=TRUE)

# Adam Fl�hr:
em.best <- as.data.frame(best.em)
ggplot(em.best, aes(Best�ndet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(best.em, Letters = letters)
em <- as.data.frame(em)
gg.t�thet <- ggplot(em, aes(Best�ndet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_text(aes(y = 1, label = .group)) +
  xlab("Best�ndets placering") +
  ylab("Sannolikheten att missa ett best�nd (1= helt sannolikt)")+
  theme_bw() 
gg.t�thet


# Add observer to the analysis ----
# Izabel is no. 3

# all years
mod.obs1 <- glmmTMB(Missades.med.bilen ~ Omr�de + Best�ndet + T�thet +  F�rekomst + Bredd + Varierande + Shape__Len + Observer, family= binomial, data=db) 
summary(mod.obs1)
mod_dharma1 <- mod.obs1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mod.obs1, scale= "response")
# Mumin ----
options(na.action=na.fail)
dredge <- dredge(mod.obs1)
top_model <- get.models(dredge, subset = 1)[[1]]
summary(top_model)
summary(model.avg(dredge, subset = delta <= 2))
options(na.action = "na.omit") 

# remove 2023 (cause I dont know if it was Iza or Andreas) and observer 6 (only has 3 obs)
db.fixobs <- db %>% filter(�r != "2023") %>% filter(Observer != "3")

mod.obs2 <- glmmTMB(Missades.med.bilen ~ Omr�de + Best�ndet + T�thet +  F�rekomst + Bredd + Varierande + Shape__Len + Observer, 
                    family= binomial, data=db.fixobs) 
summary(mod.obs2)
mod_dharma1 <- mod.obs2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mod.obs2, scale= "response")
# Mumin ----
options(na.action=na.fail)
dredge <- dredge(mod.obs2)
top_model <- get.models(dredge, subset = 1)[[1]]
summary(top_model)
summary(model.avg(dredge, subset = delta <= 2))
options(na.action = "na.omit") 
