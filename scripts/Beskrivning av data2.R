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
library(ggrepel)

# Load data
raw <- read.csv2(here("data", "Verifieringssträckor_2021_2022_2023.csv"))

db <- raw %>% 
  filter(Missades.med.bilen != "") %>% 
  dplyr::select(c(År, Område, Antal.sträckor,Sträckor.med.förekomster, Shape__Len, Bredd, Förekomst, Täthet, 
                  Varierande, Beståndet, Missades.med.bilen)) %>% 
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
  mutate(Område = dplyr::recode(Område, "Heby"= "Heby-väst")) %>% 
  mutate(Område = dplyr::recode(Område, "Enköping"= "Heby-syd")) 
db 
summary(db)

# Region ----
region <- db %>% 
  mutate(total = n()) %>% 
  group_by(Område, Antal.sträckor, Sträckor.med.förekomster) %>% 
  summarise() %>% 
  mutate(Sträckor.utan = Antal.sträckor-Sträckor.med.förekomster) %>% 
  pivot_longer(cols = c("Antal.sträckor", "Sträckor.med.förekomster", "Sträckor.utan"),
               names_to = "Sträckor", 
               values_to = "Antal") %>% 
  mutate(procent = round((Antal*100)/131, 1)) %>% 
  filter(Sträckor != "Antal.sträckor") %>% 
  mutate(grupp = paste(Område, Sträckor, sep = ".")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby-syd.Sträckor.med.förekomster"= "Heby-syd: med förekomster")) %>%  
  mutate(grupp = dplyr::recode(grupp, "Heby-syd.Sträckor.utan"= "Heby-syd: utan förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby-väst.Sträckor.med.förekomster"= "Heby-väst: med förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby-väst.Sträckor.utan"= "Heby-väst: utan förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Sträckor.med.förekomster"= "Sveg: med förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Sträckor.utan"= "Sveg: utan förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby-öst.Sträckor.med.förekomster"= "Heby-öst: med förekomster")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby-öst.Sträckor.utan"= "Heby-öst: utan förekomster")) 
region

region$pos <- c(90, 75, 60, 40, 5, 0.7, 25, 15)

gg.region <- ggplot(region, aes(x="", y= procent, fill=grupp)) + 
  geom_col(color="black") + coord_polar(theta = "y") +
  geom_label_repel(aes(y = pos, label = paste0(procent, "%")),size = 4.5, nudge_x = 0.7, show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#4a1486", "#6a51a3", "#e31a1c", "#fc4e2a", "#41ab5d", "#78c679", "#2171b5", "#4292c6"),0.7)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    legend.title=element_blank()
    )
gg.region

# Täthet ----
täthet <- db %>% 
  mutate(total = n()) %>% 
  group_by(Täthet, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= "Missade")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Nej"= "Hittade")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Täthet, sep = ": ")) %>% 
  mutate(csum = rev(cumsum(rev(Antal.objekt))), 
         pos = Antal.objekt/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Antal.objekt/2, pos)) 
täthet

täthet$bla= c(53, 240, 10, 170, 0.5, 90, 276)

gg.täthet <- ggplot(täthet, aes(x="", y= Antal.objekt, fill=grupp)) + 
  geom_col(color="black") + coord_polar(theta = "y") +
  geom_label_repel(aes(y = bla, label = paste0(Antal.objekt)),size = 4.5, nudge_x = 0.7, show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#313695", "#4575b4", "#74add1", "#abd9e9",
                                   "#a50026", "#d73027", "#f46d43"),1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    legend.title=element_blank()
  )
gg.täthet

# Beståndets läge ----
läge <- db %>% 
  mutate(total = n()) %>% 
  group_by(Beståndet, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= "Missade")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Nej"= "Hittade")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Beståndet, sep = ": "))  
läge  

läge$bla= c(68, 220, 40, 117, 6, 80)

gg.läge <- ggplot(läge, aes(x="", y= Antal.objekt, fill=grupp)) + 
  geom_col(color="black") + coord_polar(theta = "y") +
  geom_label_repel(aes(y = bla, label = paste0(Antal.objekt)),size = 4.5, nudge_x = 0.7, show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#313695", "#4575b4", "#74add1", 
                                   "#a50026", "#d73027", "#f46d43"),1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    legend.title=element_blank()
  )
gg.läge

# Förekomst utanför ----
förekomst <- db %>% 
  mutate(total = n()) %>% 
  group_by(Förekomst, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= "Missade")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Nej"= "Hittade")) %>% 
  mutate(Förekomst = dplyr::recode(Förekomst, "Ja"= "Förekommer utanför")) %>% 
  mutate(Förekomst = dplyr::recode(Förekomst, "Nej"= "Förekommer inte utanför")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Förekomst, sep = ": "))  
förekomst  

förekomst$bla= c(6.5, 120, 45, 220)

gg.förekomst <- ggplot(förekomst, aes(x="", y= Antal.objekt, fill=grupp)) + 
  geom_col(color="black") + coord_polar(theta = "y") +
  geom_label_repel(aes(y = bla, label = paste0(Antal.objekt)),size = 4.5, nudge_x = 0.7, show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#313695", "#4575b4", 
                                   "#a50026", "#d73027"),1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    legend.title=element_blank()
  )
gg.förekomst

# Varierande täthet ----
varierande <- db %>% 
  mutate(total = n()) %>% 
  group_by(Varierande, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= "Missade")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Nej"= "Hittade")) %>% 
  mutate(Varierande = dplyr::recode(Varierande, "Ja"= "Varierande täthet")) %>% 
  mutate(Varierande = dplyr::recode(Varierande, "Nej"= "Täthet varierar inte")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Varierande, sep = ": "))  
varierande  

varierande$bla= c(2.5, 120, 40, 220)

gg.varierande <- ggplot(varierande, aes(x="", y= Antal.objekt, fill=grupp)) + 
  geom_col(color="black") + coord_polar(theta = "y") +
  geom_label_repel(aes(y = bla, label = paste0(Antal.objekt)),size = 4.5, nudge_x = 0.7, show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#313695", "#4575b4", 
                                   "#a50026", "#d73027"),1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.text.x = element_blank(),
    legend.title=element_blank()
  )
gg.varierande

# Bredd ----
gg.bredd <- ggplot(db, aes(x=Bredd, group=Missades.med.bilen, fill= Missades.med.bilen)) + geom_histogram(stat="count", binwidth = 15) +
  #stat_bin(binwidth=1, geom='text', color='black', size=4, aes(label=after_stat(count),
   #                                                            group=Missades.med.bilen), position=position_stack(vjust=0.5)) +
  scale_fill_manual(labels=c("Missade", "Hittade"), values=alpha(c("#a50026", "#313695"),1)) +
  theme_classic() +
  xlab("Bredd (m)") +
  ylab("Antal objekt") +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.bredd
  
# Längd ----
gg.längd <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen)) + geom_histogram(binwidth = 30, position="dodge") +
  #stat_bin(binwidth=1, geom='text', color='black', size=4, aes(label=after_stat(count),
  #                                                             group=Missades.med.bilen), position=position_stack(vjust=0.5)) +
  scale_fill_manual(labels=c("Missade", "Hittade"), values=alpha(c("#a50026", "#313695"),1)) +
  scale_alpha_manual(values=c(0.3, 0.5, 0.8, 1)) +
  theme_classic() +
  xlab("Längd (m)") +
  ylab("Antal objekt") +
  scale_x_continuous(breaks = round(seq(0, max(db$Shape__Len), by = 30),1)) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.längd

# Längd och täthet
gg.längdtät <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen, alpha=Täthet)) + geom_histogram(binwidth = 30, position="dodge") +
  #stat_bin(binwidth=1, geom='text', color='black', size=4, aes(label=after_stat(count),
  #                                                             group=Missades.med.bilen), position=position_stack(vjust=0.5)) +
  scale_fill_manual(labels=c("Missade", "Hittade"), values=alpha(c("#a50026", "#313695"),1)) +
  scale_alpha_manual(values=c(0.3, 0.5, 0.8, 1)) +
  theme_classic() +
  xlab("Längd (m)") +
  ylab("Antal objekt") +
  scale_x_continuous(breaks = round(seq(0, max(db$Shape__Len), by = 30),1)) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.längdtät


