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
raw <- read.csv2(here("data", "Verifieringssträckor_2021_2022_2023.csv"))

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
  mutate(Täthet=fct_relevel(Täthet, "Enstaka", "Gles <50%", "Tät 50-90%", "Dominerande >90%")) %>% 
  mutate(Område = dplyr::recode(Område, "Uppsala"= "Heby-öst")) %>% 
  mutate(Område = dplyr::recode(Område, "Heby"= "Heby-väst"))
db 
summary(db)

# Figurer
gg.stack <-ggplot(db, aes(x=fct_infreq(Område), fill=Täthet)) + geom_bar(stat="count") + labs(x=NULL, y=NULL) + 
  theme_classic() +
  theme(axis.text.x = element_text(), legend.position = c(0.8, 0.7), 
        axis.text.y.left = element_text(size=15), axis.text.x.bottom = element_text(size=17, family="serif"),
        legend.text = element_text(size=14)) +
  theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) 
  #ylab("Antal bestånd") #+ font("ylab", size=18)
gg.stack + labs(y = "Antal förekomster")+ theme(axis.title.y = element_text(size=17, family="serif"))

gg.stack2 <-ggplot(db, aes(x=fct_infreq(Område), fill=Beståndet)) + geom_bar(stat="count") + labs(x=NULL, y=NULL) + 
  theme_classic() +
  theme(axis.text.x = element_text(), legend.position = c(0.8, 0.7), 
        axis.text.y.left = element_text(size=15), axis.text.x.bottom = element_text(size=17, family="serif"),
        legend.text = element_text(size=14)) +
  theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) 
#ylab("Antal bestånd") #+ font("ylab", size=18)
gg.stack2 + labs(y = "Antal förekomster")+ theme(axis.title.y = element_text(size=17, family="serif"))

# Täthet
pie.data <- db %>% 
  group_by(Täthet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  mutate(Täthet=fct_relevel(Täthet, "Enstaka", "Gles <50%", "Tät 50-90%", "Dominerande >90%"))
pie.data

missades.ja <- pie.data %>% filter(Missades.med.bilen =="Ja")

pie.ja <- ggplot(missades.ja, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16), 
        axis.ticks.x=element_blank())
pie.ja + ggtitle("Förekomster som missades vid bilinventeringen (n=47)")


missades.nej <- pie.data %>% filter(Missades.med.bilen =="Nej")

pie.nej <- ggplot(missades.nej, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.nej + ggtitle("Förekomster som upptäcktes vid bilinventeringen (n=82)")  


# Beståndet
pie.data2 <- db %>% 
  group_by(Beståndet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate(per=(count*100)/sum(count))
pie.data2

miss.ja <- pie.data2 %>% filter(Missades.med.bilen =="Ja")

pie.ja <- ggplot(miss.ja, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16), 
        axis.ticks.x=element_blank())
pie.ja + ggtitle("Förekomst som missades vid bilinventeringen (n=47)")


miss.nej <- pie.data2 %>% filter(Missades.med.bilen =="Nej")

pie.nej <- ggplot(miss.nej, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.nej + ggtitle("Förekomst som upptäcktes vid bilinventeringen (n=82)")  

# per område - täthet ----
område.täthet <- db %>% 
  group_by(Område, Täthet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(Område, Missades.med.bilen) %>% 
  mutate(per=(count*100)/sum(count)) %>% 
  mutate(Täthet=fct_relevel(Täthet, "Enstaka", "Gles <50%", "Tät 50-90%", "Dominerande >90%"))
område.täthet 


# Heby-väst
hebyv <- område.täthet %>% filter(Område == "Heby-väst")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

Hebyv.ja <- hebyv %>% filter(Missades.med.bilen == "Ja")

pie.hebyv <- ggplot(hebyv.ja, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Missades med bilen: Heby-väst (n=38)")  

hebyv.nej <- hebyv %>% filter(Missades.med.bilen == "Nej")

pie.hebyv <- ggplot(hebyv.nej, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Upptäcktes med bilen: Heby-väst (n=47)") 

# Heby-öst
hebyö <- område.täthet %>% filter(Område == "Heby-öst")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

hebyö.ja <- hebyö %>% filter(Missades.med.bilen == "Ja")

pie.hebyö <- ggplot(hebyö.ja, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyö + ggtitle("Missades med bilen: Heby-öst (n=6)")  

hebyö.nej <- hebyö %>% filter(Missades.med.bilen == "Nej")

pie.hebyö2 <- ggplot(hebyö.nej, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyö2 + ggtitle("Upptäcktes med bilen: Heby-öst (n=22)") 

# Sveg
sveg <- område.täthet %>% filter(Område == "Sveg")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

sveg.ja <- sveg %>% filter(Missades.med.bilen == "Ja")

pie.sveg<- ggplot(sveg.ja, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
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

pie.sveg2 <- ggplot(sveg.nej, aes(x = "", y= per, fill=Täthet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#C5CAE9", "#5C6BC0", "#3949AB", "#1A237E")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg2 + ggtitle("Upptäcktes med bilen: Sveg (n=13)") 


# per område - beståndets läge ----
område.bestånd <- db %>% 
  group_by(Område, Beståndet, Missades.med.bilen) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(Område, Missades.med.bilen) %>% 
  mutate(per=(count*100)/sum(count)) 
område.bestånd 


# Heby-väst
hebyv <- område.bestånd %>% filter(Område == "Heby-väst")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

hebyv.ja <- hebyv %>% filter(Missades.med.bilen == "Ja")

pie.hebyv <- ggplot(hebyv.ja, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Missades med bilen: Heby-väst (n=38)")  

hebyv.nej <- hebyv %>% filter(Missades.med.bilen == "Nej")

pie.hebyv <- ggplot(hebyv.nej, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyv + ggtitle("Upptäcktes med bilen: Heby-väst (n=47)") 

# Heby-öst
hebyö <- område.bestånd %>% filter(Område == "Heby-öst")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

hebyö.ja <- hebyö %>% filter(Missades.med.bilen == "Ja")

pie.hebyö <- ggplot(hebyö.ja, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyö + ggtitle("Missades med bilen: Heby-öst (n=6)")  

hebyö.nej <- hebyö %>% filter(Missades.med.bilen == "Nej")

pie.hebyö2 <- ggplot(hebyö.nej, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.hebyö2 + ggtitle("Upptäcktes med bilen: Heby-öst (n=22)") 

# Sveg
sveg <- område.bestånd %>% filter(Område == "Sveg")
# hebyv$percentJa <- 44.71
# hebyv$percentNej <- 55.29

sveg.ja <- sveg %>% filter(Missades.med.bilen == "Ja")

pie.sveg<- ggplot(sveg.ja, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
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

pie.sveg2 <- ggplot(sveg.nej, aes(x = "", y= per, fill=Beståndet)) + geom_col(color="black") + coord_polar(theta = "y") +
  facet_wrap(vars(Missades.med.bilen)) +
  geom_label(aes(label = count), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(values=c("#388E3C", "#26A69A", "#9CCC65")) +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=16))
pie.sveg2 + ggtitle("Upptäcktes med bilen: Sveg (n=13)") 


# Längd
db %>% 
  tidyr::gather(Täthet) %>% 
  ggplot(aes(x="Shape__Len", fill="Missades.med.bilen")) +
  geom_histogram(alpha = 0.5, color = "black", position = "identity") 
  #scale_fill_manual(values = c("aquamarine1", "greenyellow"))
gg.len
