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
  mutate(Täthet = dplyr::recode(Täthet, "Enstaka"= "Single")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Gles <50%"= "Sparse <50%")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Tät 50-90%"= "Dense 50-90%")) %>% 
  mutate(Täthet = dplyr::recode(Täthet, "Dominerande >90%"= "Dominating >90%")) %>%
  mutate(Förekomst = dplyr::recode(Förekomst, "Ja"= "Yes")) %>%
  mutate(Förekomst = dplyr::recode(Förekomst, "Nej"= "No")) %>%
  mutate(Varierande = dplyr::recode(Varierande, "Ja"= "Yes")) %>%
  mutate(Varierande = dplyr::recode(Varierande, "Nej"= "No")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Både inner och ytterslänt"= "Both inner and outer ditch slopes")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Innerslänt"= "Inner ditch slope")) %>%
  mutate(Beståndet = dplyr::recode(Beståndet, "Ytterslänt"= "Outer ditch slope")) 
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
  mutate(grupp = dplyr::recode(grupp, "Enköping.Sträckor.med.förekomster"= "Enköping: non-empty stretch")) %>%  
  mutate(grupp = dplyr::recode(grupp, "Enköping.Sträckor.utan"= "Enköping: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby.Sträckor.med.förekomster"= "Heby: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Heby.Sträckor.utan"= "Heby: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Sträckor.med.förekomster"= "Sveg: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Sveg.Sträckor.utan"= "Sveg: empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Uppsala.Sträckor.med.förekomster"= "Uppsala: non-empty stretch")) %>% 
  mutate(grupp = dplyr::recode(grupp, "Uppsala.Sträckor.utan"= "Uppsala: empty stretch")) 
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

gg.fig2 <- ggplot(fig2, aes(x =Täthet, fill=Missades.med.bilen)) + geom_bar(stat="count") +
  facet_wrap(vars(Område), nrow =2) + 
  #geom_label(aes(label = abund), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_fill_manual(values=alpha(c("#a50026", "#313695"),0.9)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12),
      legend.position = "top", legend.title=element_blank())
gg.fig2

gg.heby <- ggplot(Heby, aes(x=Täthet, fill= Missades.med.bilen)) + geom_bar(stat="count")
gg.heby
# add sifras pa varie bicho etc





# Täthet ----
täthet <- db %>% 
  mutate(total = n()) %>% 
  group_by(Täthet, Missades.med.bilen) %>% 
  summarise(Antal.objekt = n()) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(grupp = paste(Missades.med.bilen, Täthet, sep = ": ")) %>% 
  mutate(csum = rev(cumsum(rev(Antal.objekt))), 
         pos = Antal.objekt/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Antal.objekt/2, pos)) 
täthet

täthet$bla= c(49, 205, 10, 120, 72.5, 255, 229.5)

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
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
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
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "Yes"= "Missed")) %>% 
  mutate(Missades.med.bilen = dplyr::recode(Missades.med.bilen, "No"= "Detected")) %>% 
  mutate(Förekomst = dplyr::recode(Förekomst, "Yes"= "Occurs outside the road verge")) %>% 
  mutate(Förekomst = dplyr::recode(Förekomst, "No"= "Does not occur outside the road verge")) %>% 
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

# Längd ----
gg.längd <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen)) + geom_histogram(binwidth = 30, position="dodge") +
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
gg.längd

# Längd och täthet
gg.längdtät <- ggplot(db, aes(x=Shape__Len, fill= Missades.med.bilen, alpha=Täthet)) + geom_histogram(binwidth = 30, position="dodge") +
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
gg.längdtät

# Figure 2
fig2 <- read.csv2(here("data", "Car survey summary density area.csv"))

fig2 <- fig2 %>% 
  mutate(Plant.density=fct_relevel(Plant.density, "Single", "Sparse", "Dense", "Dominated")) %>% 
  mutate(Area=fct_relevel(Area, "Heby", "Enköping", "Uppsala", "Sveg"))

gg.fig2 <- ggplot(fig2, aes(x=Plant.density, y=Count, fill=Plant.density)) + geom_col(color="black") +
  facet_wrap(vars(Area), nrow =2) +
  #geom_text(aes(label=Count), position=position_dodge(width = 0.00001)) +
  scale_fill_manual(values =c("#a6bddb", "#a6bddb", "#a6bddb", "#a6bddb")) +
  theme_bw() +
  ylab("Number of stands") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size= 14, angle = 45, vjust = 0.6), 
        axis.text.y = element_text(size= 14), 
        strip.text = element_text(size=12)
        )

gg.fig2  
