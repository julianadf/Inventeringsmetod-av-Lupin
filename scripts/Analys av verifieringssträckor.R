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
raw <- read.csv2(here("data", "Verifieringssträckor_2021_2022.csv"))

# 2021 ----
v.2021 <- raw %>% 
  filter(År == 2021) %>% 
  dplyr::select(c(År, Område, Shape__Len, Bredd, Förekomst, Täthet, Varierande, Beståndet, Missades.med.bilen)) %>% 
  mutate(Bredd = as.factor(Bredd)) %>% 
  mutate(Förekomst = as.factor(Förekomst)) %>% 
  mutate(Täthet = as.factor(Täthet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Beståndet  = as.factor(Beståndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen))
v.2021

# Beskrivning av vilka typer av objekt som hittades längs verifieringssträckan
# Byt til procent
v.2021.tot <- v.2021 %>% 
  group_by(Täthet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  group_by(Missades.med.bilen) %>% 
  mutate(per = ifelse(Missades.med.bilen == "Nej", sum(per), per)) %>%
  ungroup()
v.2021.tot

pie1 <- read.csv2(here("data", "summary2021.csv"))
  

pie(pie1$procent, labels=paste(pie1$Täthet, sep = "   ", round(pie1$procent, 2), "%"), 
    col = c("#9CCC65", "#FFD54F", "#FFB300", "#FF6F00"), main = "Verifiering 2021: Uppsala och Heby")

legend("bottomleft", c("Nej", "Ja"), fill = c("#9CCC65", c("#FFD54F", "#FFB300", "#FF6F00")), 
       title = "Missades med bilen", box.col = "white")


# 2022
v.2022 <- raw %>% 
  filter(År == 2022) %>% 
  dplyr::select(c(År, Område, Shape__Len, Bredd, Förekomst, Täthet, Varierande, Beståndet, Missades.med.bilen)) %>% 
  mutate(Bredd = as.factor(Bredd)) %>% 
  mutate(Förekomst = as.factor(Förekomst)) %>% 
  mutate(Täthet = as.factor(Täthet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Beståndet  = as.factor(Beståndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen))
v.2022

# Beskrivning av vilka typer av objekt som hittades längs verifieringssträckan
# Byt til procent
v.2022.tot <- v.2022 %>% 
  filter(Missades.med.bilen != "") %>% 
  group_by(Täthet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  group_by(Missades.med.bilen) %>% 
  mutate(per = ifelse(Missades.med.bilen == "Nej", sum(per), per)) %>%
  ungroup()
v.2022.tot

pie2 <- read.csv2(here("data", "summary2022.csv"))


pie(pie2$procent, labels=paste(pie2$Täthet, sep = "   ", round(pie2$procent, 2), "%"), 
    col = c("#9CCC65", "#FFD54F", "#FFB300", "#FF6F00"), main = "Verifiering 2022: Uppsala, Heby och Sveg ")

legend("bottomleft", c("Nej", "Ja"), fill = c("#9CCC65", c("#FFD54F", "#FFB300", "#FF6F00")), 
       title = "Missades med bilen", box.col = "white")


# Model ----
db <- raw %>% 
  filter(Missades.med.bilen != "") %>% 
  dplyr::select(c(År, Område, Shape__Len, Bredd, Förekomst, Täthet, Varierande, Beståndet, Missades.med.bilen)) %>% 
  mutate(År = as.factor(År)) %>% 
  mutate(Område = as.factor(Område)) %>% 
  mutate(Bredd = as.integer(Bredd)) %>% 
  mutate(Förekomst = as.factor(Förekomst)) %>% 
  mutate(Täthet = as.factor(Täthet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Beståndet  = as.factor(Beståndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen)) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= 1, "Nej"=0)) %>%   
  filter(Täthet != "Dominerande >90%") # Bara 3 obs. Går inte att ha i modellen
db 
summary(db)

mod.all <- glmmTMB(Missades.med.bilen ~ Beståndet + Täthet + Område + Förekomst + Bredd + Varierande + Shape__Len, family= "binomial", data=db) 
summary(mod.all)
mod_dharma1 <- mod.all %>% simulateResiduals(n=1000)
plot(mod_dharma1)
# Bara Beståndets placering, Täthet, och område var av signifikans.Kör egen analys för enskilda områden då.

# Alla områden:
mod1 <- glm(Missades.med.bilen ~ Täthet + Beståndet, family= "binomial", data=db) #AICc= 130.4459
mod2 <- glmmTMB(Missades.med.bilen ~ Täthet * Beståndet, family= "binomial", data=db) #AICc= 136.4032
anova(mod1, mod2, test = "Chi") #simpler model not significantly worse. Using without interaction

summary(mod1) #estimates are log-odds
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
car::Anova(mod1, type= "III")
tab_model(mod1)

visreg(mod1) # log-odds

# Post-hoc: täthet
tät.em <- emmeans(mod1, ~ Täthet , type="response") # probabilities
tät.emms <- emmeans(mod1, "Täthet") # estimated marginal means
# Effect size:
print(eff_size(tät.em, sigma=sigma(mod1), edf=df.residual(mod1))) #
eff_size(pairs(tät.em), sigma(mod1), df.residual(mod1), method = "identity") # another way of writing the above
# ----
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
  coord_cartesian(ylim = c(0,1)) +
  geom_text(aes(y = 1, label = .group)) +
  xlab("Beståndets täthet") +
  ylab("Sannolikheten att missa ett bestånd (1= helt sannolikt)")+
  theme_bw() 
gg.täthet

# Post-hoc: Beståndets placering
best.em <- emmeans(mod1, ~ Beståndet , type="response")
best.emms <- emmeans(mod1, "Beståndet")
# Effect size:
print(eff_size(best.em, sigma=sigma(mod1), edf=df.residual(mod1)))
eff_size(pairs(best.em), sigma(mod1), df.residual(mod1), method = "identity") # another way of writing the above
# ----
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
gg.täthet <- ggplot(em, aes(Beståndet, prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +#, width = 0.1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_text(aes(y = 1, label = .group)) +
  xlab("Beståndets placering") +
  ylab("Sannolikheten att missa ett bestånd (1= helt sannolikt)")+
  theme_bw() 
gg.täthet
