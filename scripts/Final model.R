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
raw <- read.csv2(here("data", "Verifieringssträckor_2021_2022_2023.csv"))


# Model ----
db <- raw %>% 
  filter(Missades.med.bilen != "") %>% 
  dplyr::select(c(År, Område, Shape__Len, Observer, Bredd, Förekomst, Täthet, Varierande, Beståndet, Missades.med.bilen)) %>% 
  mutate(År = as.factor(År)) %>% 
  mutate(Område = as.factor(Område)) %>% 
  mutate(Observer = as.factor(Observer)) %>% 
  mutate(Bredd = as.integer(Bredd)) %>% 
  mutate(Förekomst = as.factor(Förekomst)) %>% 
  mutate(Täthet = as.factor(Täthet)) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Enstaka"= "Single")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Gles <50%"= "Sparse <50%")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Tät 50-90%"= "Dense 50-90%")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Dominerande >90%"= "Dominating >90%")) %>%
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Beståndet  = as.factor(Beståndet)) %>% 
  mutate(Beståndet = dplyr::recode(Beståndet, "Både inner och ytterslänt"= "Both inner and outer ditch slopes")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Innerslänt"= "Inner ditch slope")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Ytterslänt"= "Outer ditch slope")) %>%
  rename(Detected.with.car = Missades.med.bilen) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car)) %>% 
  #So it is more straightforward, renamed "missed with car" to "Detected", so ja= detected, nej = missed
  mutate(Detected.with.car = dplyr::recode(Detected.with.car, "Ja"= 0, "Nej"=1)) %>% 
  # Before was 1 for "missed with car", 0 for "found with car". Now its 1 for "detected with car", 0 for "missed". 
  filter(Täthet != "Dominating >90%") # Bara 3 obs. Går inte att ha i modellen
db 
summary(db)

mod.final <- glm(Detected.with.car ~ Beståndet + Täthet + Område + Förekomst, family= "binomial", data=db) 
summary(mod.final) #estimates are log-odds
mod_dharma1 <- mod.final %>% simulateResiduals(n=1000)
plot(mod_dharma1)
vif(mod.final)

car::Anova(mod.final, type= "III")
tab_model(mod.final)

visreg(mod.final, scale = "response")

# Figures ----

# Post-hoc: täthet
tät.em <- emmeans(mod.final, ~ Täthet , type="response") # probabilities
tät.emms <- emmeans(mod.final, "Täthet") # estimated marginal means
# Effect size:
print(eff_size(tät.em, sigma=sigma(mod.final), edf=df.residual(mod.final))) #
eff_size(pairs(tät.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(tät.emms)
plot(tät.emms, comparisons=TRUE)

# Adam Flöhr:
em.tät <- as.data.frame(tät.em)
ggplot(em.tät, aes(Täthet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(tät.em, Letters = letters)
em <- as.data.frame(em)
gg.täthet <- ggplot(em, aes(Täthet, prob)) +
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
gg.täthet

# Post-hoc: Beståndets placering
best.em <- emmeans(mod.final, ~ Beståndet , type="response")
best.emms <- emmeans(mod.final, "Beståndet")
# Effect size:
print(eff_size(best.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(best.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(best.emms)
plot(best.emms, comparisons=TRUE)

# Adam Flöhr:
em.best <- as.data.frame(best.em)
ggplot(em.best, aes(Beståndet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(best.em, Letters = letters)
em <- as.data.frame(em)
gg.best <- ggplot(em, aes(Beståndet, prob)) +
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

# Post-hoc: Område
omr.em <- emmeans(mod.final, ~ Område , type="response")
omr.emms <- emmeans(mod.final, "Område")
# Effect size:
print(eff_size(omr.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(omr.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(omr.emms)
plot(omr.emms, comparisons=TRUE)

# Adam Flöhr:
em.omr <- as.data.frame(omr.em)
ggplot(em.omr, aes(Område, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(omr.em, Letters = letters)
em <- as.data.frame(em)
gg.omr <- ggplot(em, aes(Område, prob)) +
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


# Post-hoc: Förekomst utanför vägområdet
utanf.em <- emmeans(mod.final, ~ Förekomst , type="response")
utanf.emms <- emmeans(mod.final, "Förekomst")
# Effect size:
print(eff_size(untanf.em, sigma=sigma(mod.final), edf=df.residual(mod.final)))
eff_size(pairs(utanf.em), sigma(mod.final), df.residual(mod.final), method = "identity") # another way of writing the above
# 
pairs(utanf.emms)
plot(utanf.emms, comparisons=TRUE)

# Adam Flöhr:
em.utanf <- as.data.frame(utanf.em)
ggplot(em.utanf, aes(Förekomst, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  ylab("") +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

em <- multcomp::cld(utanf.em, Letters = letters)
em <- as.data.frame(em)
gg.utanf <- ggplot(em, aes(Förekomst, prob)) +
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

