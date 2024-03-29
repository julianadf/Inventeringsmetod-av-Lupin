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


# Load data
raw <- read.csv2(here("data", "Verifieringsstr�ckor_2021_2022.csv"))

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


# 2022
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
  dplyr::select(c(�r, Omr�de, Shape__Len, Bredd, F�rekomst, T�thet, Varierande, Best�ndet, Missades.med.bilen)) %>% 
  mutate(�r = as.factor(�r)) %>% 
  mutate(Omr�de = as.factor(Omr�de)) %>% 
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

mod.all <- glmmTMB(Missades.med.bilen ~ Best�ndet + T�thet + Omr�de + F�rekomst + Bredd + Varierande + Shape__Len, family= "binomial", data=db) 
summary(mod.all)
mod_dharma1 <- mod.all %>% simulateResiduals(n=1000)
plot(mod_dharma1)
# Bara Best�ndets placering, T�thet, och omr�de var av signifikans.K�r egen analys f�r enskilda omr�den d�.

# Alla omr�den:
mod1 <- glm(Missades.med.bilen ~ T�thet + Best�ndet, family= "binomial", data=db) #AICc= 130.4459
mod2 <- glmmTMB(Missades.med.bilen ~ T�thet * Best�ndet, family= "binomial", data=db) #AICc= 136.4032
anova(mod1, mod2, test = "Chi") #simpler model not significantly worse. Using without interaction

summary(mod1) #estimates are log-odds
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
car::Anova(mod1, type= "III")
tab_model(mod1)

visreg(mod1) # log-odds

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
