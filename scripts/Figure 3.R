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
  mutate(Best�ndet = dplyr::recode(Best�ndet, "B�de inner och yttersl�nt"= "Both side and back slopes")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Innersl�nt"= "Side slope")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Yttersl�nt"= "Back slope")) %>%
  rename(Detected.with.car = Missades.med.bilen) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car)) %>% 
  mutate(Omr�de = dplyr::recode(Omr�de, "Sveg"= "Fun�sdalen")) %>% 
  #So it is more straightforward, renamed "missed with car" to "Detected", so ja= detected, nej = missed
  mutate(Detected.with.car = dplyr::recode(Detected.with.car, "Ja"= 0, "Nej"=1))
  # Before was 1 for "missed with car", 0 for "found with car". Now its 1 for "detected with car", 0 for "missed". 
  #filter(T�thet != "Dominating >90%") # Bara 3 obs. G�r inte att ha i modellen
db 
summary(db)


count.stands <- db %>% 
  group_by(T�thet, Best�ndet, Detected.with.car) %>% 
  summarise(count= n()) %>% 
  mutate(Detected.with.car = as.factor(Detected.with.car))
count.stands


explore1 <- ggplot(count.stands, aes(x=T�thet, y=count, fill=Best�ndet, alpha = Detected.with.car)) +
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

count.stands$interact <- interaction(count.stands$Best�ndet, count.stands$Detected.with.car)
fig3 <- count.stands %>% arrange(factor(T�thet,levels= order.xaxis))

explore2 <-  ggplot(fig3, aes(x=fct_inorder(T�thet), y= count, fill = interact)) +
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
