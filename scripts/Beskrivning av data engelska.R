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
raw <- read.csv2(here("data", "Verifieringsstr�ckor_2021_2022_2023.csv"))

db <- raw %>% 
  filter(Missades.med.bilen != "") %>% 
  dplyr::select(c(�r, Omr�de, Antal.str�ckor,Str�ckor.med.f�rekomster, Shape__Len, Bredd, F�rekomst, T�thet, 
                  Varierande, Best�ndet, Missades.med.bilen)) %>% 
  mutate(�r = as.factor(�r)) %>% 
  mutate(Omr�de = as.factor(Omr�de)) %>% 
  mutate(Bredd = as.integer(Bredd)) %>% 
  mutate(F�rekomst = as.factor(F�rekomst)) %>% 
  mutate(T�thet = as.factor(T�thet)) %>% 
  mutate(Varierande = as.factor(Varierande)) %>% 
  mutate(Best�ndet  = as.factor(Best�ndet)) %>% 
  mutate(Missades.med.bilen = as.factor(Missades.med.bilen)) %>% 
  mutate(T�thet=fct_relevel(T�thet, "Enstaka", "Gles <50%", "T�t 50-90%", "Dominerande >90%")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "Enstaka"= "Single")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "Gles <50%"= "Sparse <50%")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "T�t 50-90%"= "Dense 50-90%")) %>% 
  mutate(T�thet = dplyr::recode(T�thet, "Dominerande >90%"= "Dominating >90%")) %>%
  mutate(F�rekomst = dplyr::recode(F�rekomst, "Ja"= "Yes")) %>%
  mutate(F�rekomst = dplyr::recode(F�rekomst, "Nej"= "No")) %>%
  mutate(Varierande = dplyr::recode(Varierande, "Ja"= "Yes")) %>%
  mutate(Varierande = dplyr::recode(Varierande, "Nej"= "No")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "B�de inner och yttersl�nt"= "Both inner and outer ditch slopes")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Innersl�nt"= "Inner ditch slope")) %>%
  mutate(Best�ndet = dplyr::recode(Best�ndet, "Yttersl�nt"= "Outer ditch slope")) %>% 
  mutate(Omr�de = dplyr::recode(Omr�de, "Sveg"= "Fun�sdalen"))
db 
summary(db)

# Region ----
region <- db %>% 
  mutate(total = n()) %>% 
  group_by(Omr�de, Antal.str�ckor, Str�ckor.med.f�rekomster) %>% 
  summarise() %>% 
  mutate(Str�ckor.utan = Antal.str�ckor-Str�ckor.med.f�rekomster) %>% 
  pivot_longer(cols = c("Antal.str�ckor", "Str�ckor.med.f�rekomster", "Str�ckor.utan"),
               names_to = "Str�ckor", 
               values_to = "Antal") %>% 
  mutate(procent = round((Antal*100)/131, 1)) %>% 
  filter(Str�ckor != "Antal.str�ckor") %>% 
  mutate(grupp = paste(Omr�de, Str�ckor, sep = ".")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Enk�ping.Str�ckor.med.f�rekomster"= "Enk�ping: non-empty stretch")) %>%  
  mutate(grupp = dplyr::recode(grupp, "Enk�ping.Str�ckor.utan"= "Enk�ping: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby.Str�ckor.med.f�rekomster"= "Heby: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby.Str�ckor.utan"= "Heby: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Str�ckor.med.f�rekomster"= "Sveg: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Str�ckor.utan"= "Sveg: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Uppsala.Str�ckor.med.f�rekomster"= "Uppsala: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Uppsala.Str�ckor.utan"= "Uppsala: empty stretch")) 
region

region$pos <- c(80, 93, 40, 62, 26, 29.5, 5, 17)

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

# Histograms per region
fig2 <- db %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Ja"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Nej"= "Detected"))
fig2

gg.fig2 <- ggplot(fig2, aes(x =T�thet, fill=Missades.med.bilen)) + geom_bar(stat="count") +
  facet_wrap(vars(Omr�de), nrow =2) + 
  #geom_label(aes(label = abund), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#a50026", "#313695"),0.9)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12),
      legend.position = "top", legend.title=element_blank())
gg.fig2

gg.heby <- ggplot(Heby, aes(x=T�thet, fill= Missades.med.bilen)) + geom_bar(stat="count")
gg.heby
# add sifras pa varie bicho etc





# T�thet ----
t�thet <- db %>% 
  mutate(total = n()) %>% 
  group_by(T�thet, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(grupp = paste(Missades.med.bilen, T�thet, sep = ": ")) %>% 
  mutate(csum = rev(cumsum(rev(Antal.objekt))), 
         pos = Antal.objekt/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Antal.objekt/2, pos)) 
t�thet

t�thet$bla= c(49, 205, 10, 120, 72.5, 255, 229.5)

gg.t�thet <- ggplot(t�thet, aes(x="", y= Antal.objekt, fill=grupp)) + 
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
gg.t�thet

# Best�ndets l�ge ----
l�ge <- db %>% 
  mutate(total = n()) %>% 
  group_by(Best�ndet, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Best�ndet, sep = ": "))  
l�ge  

l�ge$bla= c(68, 220, 40, 117, 6, 80)

gg.l�ge <- ggplot(l�ge, aes(x="", y= Antal.objekt, fill=grupp)) + 
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
gg.l�ge

# F�rekomst utanf�r ----
f�rekomst <- db %>% 
  mutate(total = n()) %>% 
  group_by(F�rekomst, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(F�rekomst = dplyr::recode(F�rekomst, "Yes"= "Occurs outside the road verge")) %>% 
  mutate(F�rekomst = dplyr::recode(F�rekomst, "No"= "Does not occur outside the road verge")) %>% 
  mutate(grupp = paste(Missades.med.bilen, F�rekomst, sep = ": "))  
f�rekomst  

f�rekomst$bla= c(6.5, 120, 45, 220)

gg.f�rekomst <- ggplot(f�rekomst, aes(x="", y= Antal.objekt, fill=grupp)) + 
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
gg.f�rekomst

# Varierande t�thet ----
varierande <- db %>% 
  mutate(total = n()) %>% 
  group_by(Varierande, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(Varierande = dplyr::recode(Varierande, "Yes"= "Varying density")) %>% 
  mutate(Varierande = dplyr::recode(Varierande, "No"= "Density does not vary")) %>% 
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
  scale_fill_manual(labels=c("Missed", "Detected"), values=alpha(c("#a50026", "#313695"),1)) +
  theme_classic() +
  xlab("Width (m)") +
  ylab("Number of stands") +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.bredd

# L�ngd ----
gg.l�ngd <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen)) + geom_histogram(binwidth = 30, position="dodge") +
  #stat_bin(binwidth=1, geom='text', color='black', size=4, aes(label=after_stat(count),
  #                                                             group=Missades.med.bilen), position=position_stack(vjust=0.5)) +
  scale_fill_manual(labels=c("Missed", "Detected"), values=alpha(c("#a50026", "#313695"),1)) +
  scale_alpha_manual(values=c(0.3, 0.5, 0.8, 1)) +
  theme_classic() +
  xlab("Length (m)") +
  ylab("Number of stands") +
  scale_x_continuous(breaks = round(seq(0, max(db$Shape__Len), by = 30),1)) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.l�ngd

# L�ngd och t�thet
gg.l�ngdt�t <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen, alpha=T�thet)) + geom_histogram(binwidth = 30, position="dodge") +
  #stat_bin(binwidth=1, geom='text', color='black', size=4, aes(label=after_stat(count),
  #                                                             group=Missades.med.bilen), position=position_stack(vjust=0.5)) +
  scale_fill_manual(labels=c("Missed", "Detected"), values=alpha(c("#a50026", "#313695"),1)) +
  scale_alpha_manual(values=c(0.3, 0.5, 0.8, 1)) +
  theme_classic() +
  xlab("Length (m)") +
  ylab("Number of stands") +
  scale_x_continuous(breaks = round(seq(0, max(db$Shape__Len), by = 30),1)) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16),
        
        legend.position = c(0.8, 0.8), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size= 14), 
        axis.text.y = element_text(size= 14)
  )
gg.l�ngdt�t

# Figure 2
fig2 <- read.csv2(here("data", "Car survey summary density area.csv"))

fig2.perc <- fig2 %>% 
  group_by(Area) %>% 
  mutate(percent= prop.table(Count) * 100) %>% 
fig2.perc

fig2.perc$label <- paste0(round(fig2.perc$Count, 2), "\n(", round(fig2.perc$percent, 1), "%)")

fig2 <- fig2 %>% 
  mutate(Plant.density=fct_relevel(Plant.density, "Single", "Sparse", "Dense", "Dominated")) %>% 
  mutate(Area=fct_relevel(Area, "Heby", "Enk�ping", "Uppsala", "Sveg"))

gg.fig2 <- ggplot(fig2.perc, aes(x=Plant.density, y=Count, fill=Plant.density)) + geom_col(color="black") +
  facet_wrap(vars(Area), nrow =2) +
  #geom_text(aes(label=Count), position=position_stack(vjust=1.)) +
  geom_text(aes(label=label), position=position_stack(vjust=0.99999)) +
  scale_fill_manual(values =c("#a6bddb", "#a6bddb", "#a6bddb", "#a6bddb")) +
  theme_bw() +
  ylim(0,470) +
  ylab("Number of stands") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size= 14, angle = 45, vjust = 0.6), 
        axis.text.y = element_text(size= 14), 
        strip.text = element_text(size=12)
        )

gg.fig2  

