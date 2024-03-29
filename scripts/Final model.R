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
  mutate(T�thet = dplyr::recode(T�thet, "Enstaka"= "Single")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "Gles <50%"= "Sparse <50%")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "T�t 50-90%"= "Dense 50-90%")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "Dominerande >90%"= "Dominating >90%")) %>%
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Best�ndet  = as.factor(Best�ndet)) %>% 
  mutate(Best�ndet = dplyr::recode(Best�ndet, "B�de inner och yttersl�nt"= "Both inner and outer ditch slopes")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Innersl�nt"= "Inner ditch slope")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Yttersl�nt"= "Outer ditch slope")) %>%
  rename(Detected.with.car = Missades.med.bilen) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car)) %>% 
  #So it is more straightforward, renamed "missed with car" to "Detected", so ja= detected, nej = missed
  mutate(Detected.with.car = dplyr::recode(Detected.with.car, "Ja"= 0, "Nej"=1)) %>% 
  # Before was 1 for "missed with car", 0 for "found with car". Now its 1 for "detected with car", 0 for "missed". 
  filter(T�thet != "Dominating >90%") # Bara 3 obs. G�r inte att ha i modellen
db 
summary(db)

mod.final <- glm(Detected.with.car ~ Best�ndet + T�thet + Omr�de + F�rekomst, family= "binomial", data=db) 
summary(mod.final) #estimates are log-odds
mod_dharma1 <- mod.final %>% simulateResiduals(n=1000)
plot(mod_dharma1)
vif(mod.final)

car::Anova(mod.final, type= "III")
tab_model(mod.final)

visreg(mod.final, scale = "response")

# Figures ----

# Post-hoc: t�thet
t�t.em <- emmeans(mod.final, ~ T�thet , type="response") # probabilities
t�t.emms <- emmeans(mod.final, "T�thet") # estimated marginal means
# Effect size:
print(eff_size(t�t.em, sigma=sigma(mod.final), edf=df.residual(mod.final))) #
eff_size(pairs(t�t.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
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
  coord_cartesian(ylim = c(0,1.1)) +
  geom_text(aes(y = 1.1, label = .group)) +
  xlab("Stand density") +
  ylab("P(detecting a stand)")+
  theme_bw() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y= element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)
        )
gg.t�thet

# Post-hoc: Best�ndets placering
best.em <- emmeans(mod.final, ~ Best�ndet , type="response")
best.emms <- emmeans(mod.final, "Best�ndet")
# Effect size:
print(eff_size(best.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(best.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
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
gg.best <- ggplot(em, aes(Best�ndet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1.1)) +
  geom_text(aes(y = 1.1, label = .group)) +
  xlab("Stand location") +
  ylab("P(detecting a stand)")+
  theme_bw() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y= element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)
  )
gg.best

# Post-hoc: Omr�de
omr.em <- emmeans(mod.final, ~ Omr�de , type="response")
omr.emms <- emmeans(mod.final, "Omr�de")
# Effect size:
print(eff_size(omr.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(omr.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(omr.emms)
plot(omr.emms, comparisons=TRUE)

# Adam Fl�hr:
em.omr <- as.data.frame(omr.em)
ggplot(em.omr, aes(Omr�de, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(omr.em, Letters = letters)
em <- as.data.frame(em)
gg.omr <- ggplot(em, aes(Omr�de, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1.1)) +
  geom_text(aes(y = 1.1, label = .group)) +
  xlab("Survey area") +
  ylab("P(detecting a stand)")+
  theme_bw() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y= element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)
  )
gg.omr


# Post-hoc: F�rekomst utanf�r v�gomr�det
utanf.em <- emmeans(mod.final, ~ F�rekomst , type="response")
utanf.emms <- emmeans(mod.final, "F�rekomst")
# Effect size:
print(eff_size(untanf.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(utanf.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(utanf.emms)
plot(utanf.emms, comparisons=TRUE)

# Adam Fl�hr:
em.utanf <- as.data.frame(utanf.em)
ggplot(em.utanf, aes(F�rekomst, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(utanf.em, Letters = letters)
em <- as.data.frame(em)
gg.utanf <- ggplot(em, aes(F�rekomst, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1.1)) +
  geom_text(aes(y = 1.1, label = .group)) +
  xlab("Stand continues outside the road verge") +
  ylab("P(detecting a stand)") +
  scale_x_discrete(labels=c("Yes", "No")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y= element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)
  )
gg.utanf  
 
# Null model ----
mod.null <- glm(Detected.with.car ~ 1, family= "binomial", data=db) 
AICc(mod.final) # 240.036
AICc(mod.null) # 319.0523

