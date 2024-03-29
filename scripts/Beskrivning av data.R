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
library(ggnewscale)

# Load data
raw <- read.csv2(here("data", "Verifieringsstr�ckor_2021_2022_2023.csv"))

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
  mutate(T�thet=fct_relevel(T�thet, "Enstaka", "Gles <50%", "T�t 50-90%", "Dominerande >90%")) %>% 
  mutate(Omr�de = dplyr::recode(Omr�de, "Uppsala"= "Heby-�st")) %>% 
  mutate(Omr�de = dplyr::recode(Omr�de, "Heby"= "Heby-v�st"))
db 
summary(db)

# Figurer
gg.stack <-ggplot(db, aes(x=fct_infreq(Omr�de), fill=T�thet)) + geom_bar(stat="count") + labs(x=NULL, y=NULL) + 
  theme_classic() +
  theme(axis.text.x = element_text(), legend.position = c(0.8, 0.7), 
        axis.text.y.left = element_text(size=15), axis.text.x.bottom = element_text(size=17, family="serif"),
        legend.text = element_text(size=14)) +
  theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) 
  #ylab("Antal best�nd") #+ font("ylab", size=18)
gg.stack + labs(y = "Antal f�rekomster")+ theme(axis.title.y = element_text(size=17, family="serif"))

gg.stack2 <-ggplot(db, aes(x=fct_infreq(Omr�de), fill=Best�ndet)) + geom_bar(stat="count") + labs(x=NULL, y=NULL) + 
  theme_classic() +
  theme(axis.text.x = element_text(), legend.position = c(0.8, 0.7), 
        axis.text.y.left = element_text(size=15), axis.text.x.bottom = element_text(size=17, family="serif"),
        legend.text = element_text(size=14)) +
  theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) 
#ylab("Antal best�nd") #+ font("ylab", size=18)
gg.stack2 + labs(y = "Antal f�rekomster")+ theme(axis.title.y = element_text(size=17, family="serif"))

# T�thet
pie.data <- db %>% 
  group_by(T�thet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  mutate(T�thet=fct_relevel(T�thet, "Enstaka", "Gles <50%", "T�t 50-90%", "Dominerande >90%"))
pie.data

missades.ja <- pie.data %>% filter(Missades.med.bilen =="Ja")

pie.ja <- ggplot(missades.ja, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16), 
        axis.ticks.x=element_blank())
pie.ja + ggtitle("F�rekomster som missades vid bilinventeringen (n=47)")


missades.nej <- pie.data %>% filter(Missades.med.bilen =="Nej")

pie.nej <- ggplot(missades.nej, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.nej + ggtitle("F�rekomster som uppt�cktes vid bilinventeringen (n=82)")  


# Best�ndet
pie.data2 <- db %>% 
  group_by(Best�ndet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count))
pie.data2

miss.ja <- pie.data2 %>% filter(Missades.med.bilen =="Ja")

pie.ja <- ggplot(miss.ja, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16), 
        axis.ticks.x=element_blank())
pie.ja + ggtitle("F�rekomst som missades vid bilinventeringen (n=47)")


miss.nej <- pie.data2 %>% filter(Missades.med.bilen =="Nej")

pie.nej <- ggplot(miss.nej, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.nej + ggtitle("F�rekomst som uppt�cktes vid bilinventeringen (n=82)")  

# per omr�de - t�thet ----
omr�de.t�thet <- db %>% 
  group_by(Omr�de, T�thet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(Omr�de, Missades.med.bilen) %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  mutate(T�thet=fct_relevel(T�thet, "Enstaka", "Gles <50%", "T�t 50-90%", "Dominerande >90%"))
omr�de.t�thet 


# Heby-v�st
hebyv <- omr�de.t�thet %>% filter(Omr�de == "Heby-v�st")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

Hebyv.ja <- hebyv %>% filter(Missades.med.bilen == "Ja")

pie.hebyv <- ggplot(hebyv.ja, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Missades med bilen: Heby-v�st (n=38)")  

hebyv.nej <- hebyv %>% filter(Missades.med.bilen == "Nej")

pie.hebyv <- ggplot(hebyv.nej, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Uppt�cktes med bilen: Heby-v�st (n=47)") 

# Heby-�st
heby� <- omr�de.t�thet %>% filter(Omr�de == "Heby-�st")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

heby�.ja <- heby� %>% filter(Missades.med.bilen == "Ja")

pie.heby� <- ggplot(heby�.ja, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.heby� + ggtitle("Missades med bilen: Heby-�st (n=6)")  

heby�.nej <- heby� %>% filter(Missades.med.bilen == "Nej")

pie.heby�2 <- ggplot(heby�.nej, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.heby�2 + ggtitle("Uppt�cktes med bilen: Heby-�st (n=22)") 

# Sveg
sveg <- omr�de.t�thet %>% filter(Omr�de == "Sveg")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

sveg.ja <- sveg %>% filter(Missades.med.bilen == "Ja")

pie.sveg<- ggplot(sveg.ja, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg + ggtitle("Missades med bilen: Sveg (n=3)")  

sveg.nej <- sveg %>% filter(Missades.med.bilen == "Nej")

pie.sveg2 <- ggplot(sveg.nej, aes(x = "", y= per, fill=T�thet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg2 + ggtitle("Uppt�cktes med bilen: Sveg (n=13)") 


# per omr�de - best�ndets l�ge ----
omr�de.best�nd <- db %>% 
  group_by(Omr�de, Best�ndet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(Omr�de, Missades.med.bilen) %>% 
  mutate(per=(count*100)/sum(count)) 
omr�de.best�nd 


# Heby-v�st
hebyv <- omr�de.best�nd %>% filter(Omr�de == "Heby-v�st")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

hebyv.ja <- hebyv %>% filter(Missades.med.bilen == "Ja")

pie.hebyv <- ggplot(hebyv.ja, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Missades med bilen: Heby-v�st (n=38)")  

hebyv.nej <- hebyv %>% filter(Missades.med.bilen == "Nej")

pie.hebyv <- ggplot(hebyv.nej, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Uppt�cktes med bilen: Heby-v�st (n=47)") 

# Heby-�st
heby� <- omr�de.best�nd %>% filter(Omr�de == "Heby-�st")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

heby�.ja <- heby� %>% filter(Missades.med.bilen == "Ja")

pie.heby� <- ggplot(heby�.ja, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.heby� + ggtitle("Missades med bilen: Heby-�st (n=6)")  

heby�.nej <- heby� %>% filter(Missades.med.bilen == "Nej")

pie.heby�2 <- ggplot(heby�.nej, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.heby�2 + ggtitle("Uppt�cktes med bilen: Heby-�st (n=22)") 

# Sveg
sveg <- omr�de.best�nd %>% filter(Omr�de == "Sveg")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

sveg.ja <- sveg %>% filter(Missades.med.bilen == "Ja")

pie.sveg<- ggplot(sveg.ja, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg + ggtitle("Missades med bilen: Sveg (n=3)")  

sveg.nej <- sveg %>% filter(Missades.med.bilen == "Nej")

pie.sveg2 <- ggplot(sveg.nej, aes(x = "", y= per, fill=Best�ndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg2 + ggtitle("Uppt�cktes med bilen: Sveg (n=13)") 


# L�ngd
db %>% 
  tidyr::gather(T�thet) %>% 
  ggplot(aes(x="Shape__Len", fill="Missades.med.bilen")) +
  geom_histogram(alpha = 0.5, color = "black", position = "identity") 
  #scale_fill_manual(values = c("aquamarine1", "greenyellow"))
gg.len
