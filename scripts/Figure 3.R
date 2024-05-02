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
library(ggpattern)


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
  mutate(Beståndet = dplyr::recode(Beståndet, "Både inner och ytterslänt"= "Both side and back slopes")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Innerslänt"= "Side slope")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Ytterslänt"= "Back slope")) %>%
  rename(Detected.with.car = Missades.med.bilen) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car)) %>% 
  mutate(Område = dplyr::recode(Område, "Sveg"= "Funäsdalen")) %>% 
  #So it is more straightforward, renamed "missed with car" to "Detected", so ja= detected, nej = missed
  mutate(Detected.with.car = dplyr::recode(Detected.with.car, "Ja"= 0, "Nej"=1))
  # Before was 1 for "missed with car", 0 for "found with car". Now its 1 for "detected with car", 0 for "missed". 
  #filter(Täthet != "Dominating >90%") # Bara 3 obs. Går inte att ha i modellen
db 
summary(db)


count.stands <- db %>% 
  group_by(Täthet, Beståndet, Detected.with.car) %>% 
  summarise(count= n()) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car))
count.stands


explore1 <- ggplot(count.stands, aes(x=Täthet, y=count, fill=Beståndet, alpha = Detected.with.car)) +
  geom_col() +
  scale_alpha_discrete(range = c(1, .7)) +
  scale_fill_manual(values=alpha(c("#762a83", "#1b7837", "#2166ac")))+
  theme_bw() +
  ylab("Number of stands") +
  xlab("") +
  geom_label(aes(label = count, size=10), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme(legend.title = element_blank())
explore1

order.xaxis = c("Single", "Sparse <50%", "Dense 50-90%", "Dominating >90%")

count.stands$interact <- interaction(count.stands$Beståndet, count.stands$Detected.with.car)
fig3 <- count.stands %>% arrange(factor(Täthet,levels= order.xaxis))

explore2 <-  ggplot(fig3, aes(x=fct_inorder(Täthet), y= count, fill = interact)) +
  geom_col() +
  ylab("Number of stands") +
  xlab("") +
  geom_label(aes(label = count, size=6), position = position_stack(vjust = 0.7), show.legend = FALSE) +
  theme_bw() +
  theme(axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 14), 
        legend.text = element_text(size= 14)) +
  scale_fill_manual(name = element_blank(), 
                    values = c(
                      "Both side and back slopes.0" = alpha("#762a83", 3/5),
                      "Both side and back slopes.1" = alpha("#762a83", 1),
                      "Side slope.0" = alpha("#1b7837", 3/5),
                      "Side slope.1" = alpha("#1b7837", 1),
                      "Back slope.0" = alpha("#2166ac", 3/5),
                      "Back slope.1" = alpha("#2166ac", 1)
                    ),
                     labels = c("Both side and back slopes, Detected",
                                "Side slope, Detected",
                                "Back slope, Detected",
                                "Both side and back slopes, Missed",
                                "Side slope, Missed",
                                "Back slope, Missed"
                                ))
explore2
