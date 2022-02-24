x <- c(1,2,3)
y <- c("a", "b", "c")
df <- data.frame(x, y)
print(df)
library(readr)
library(dplyr)
library(lubridate) # pakki med follum fyrir vinnslu med dagsetningar (date eda datetime breytur)
library(ggplot2)
foo <- read_csv("./data/vedurgogn.csv")

mean(data$stöð, na.rm = TRUE)

foo %>%
  summarise(meanSolstundir = mean(sun, na.rm = TRUE),
            sdSolstundir = sd(sun, na.rm = TRUE)
            )

foo %>%
  mutate(litur = sample(c("gulur","raudur","graen"), size = nrow(data), replace = TRUE)) %>%
    group_by(litur) %>%
      summarise(meant = mean(t, na.rm = TRUE))

library(ggplot2)
    
foo %>%
  ggplot(aes(x = sun, y = r)) + 
  geom_point()


---
  title: "Skilaverkefni 1"
author: "Atli Snaer Kristjansson, Valdimar Orn Sverrisson"
date: "1/17/2022"
output: html_document
---
  
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate) # pakki med follum fyrir vinnslu med dagsetningar (date eda datetime breytur)
library(ggplot2)

#a
litir <- c("#f24236","#00a6fb")
AV <- read_csv("./data/vedurgogn.csv")
AV <- rename(AV
             , stod = stöð 
             , ar = ár
             , man = mán
             , medalHiti = t
             , medalMaxHitiPerAr= tx
             , maxHitiPerMan = txx
             , dagsettningMaxHita = txxD1
             , medalMinHitiPerMan = tn
             , minHitiPerMan = tnn
             , dagsettningMinHita = tnnD1
             , medalRakastig = rh
             , heildarUrkomaPerMan = r
             , maxDagUrkomaPerMan = rx
             , dagsettningMaxUrkoma = rxD1
             , medalThristingur = p
             , medalSkyjahula = n
             , solskynsStundir = sun
             , medalVindhradi = f )
print(AV)

##c)
# Solskynsstundir voru með 21 NA's sem að við fjarlaegðum
# MedalRakastig, medalMaxHitiPerAr,maxHitiPerMan og dagsettningMaxHita voru hver með 
# 1 NA's í sér sem að við fjarlægðum

AV %>%
  na.omit() %>%
  summary()

glimpse(AV)


library(ggplot2)
    

#C
#Bæta við dálk með árstíðinni þar sem mælingin átti stað
AV <-  mutate(AV,
              arstid = recode_factor(
                man,
               `12`= "Vetur", `1` = "Vetur",`2` = "Vetur",
               `3` = "Vor",`4` = "Vor",`5` = "Vor",
               `6` = "Sumar",`7` = "Sumar",`8` = "Sumar",
               `9` = "Haust",`10` = "Haust",`11` = "Haust"
                )
              ) 
#D
#Bæta við sér dálk með borg þar sem mælingin átti stað
AV <- mutate(AV, 
             stadur = recode_factor(
               stod,
               `1` = "Reykjavik",
               `422` = "Akureyri"
             )
)


#D1
#Heildarfjödli sólskynsstunda eftir borg
ggplot(AV, aes(x = stadur, y = solskynsStundir, fill = stadur)) +
  geom_bar(stat="Identity") +
  scale_fill_manual(values=litir) +
  labs(title="Heildarfjöldi sólskynsstunda", 
       subtitle="Á Akureyri og í Reykjavík", 
       y="sólskynsstundir", 
       x="staður", 
       caption = "Source: Veðurstofan")


#D2

ggplot(AV, aes(x = man, y = solskynsStundir, fill = stadur)) +
  geom_boxplot() +
  xlim(c(1, 12)) +
  ylim(c(0,200)) +
  scale_fill_manual(values=litir) +
  labs(title="Dreifing fjölda sólskynsstunda", 
       subtitle="Á Akureyri og í Reykjavík", 
       y="sólskynsstundir", 
       x="Mánuður", 
       caption = "Source: Veðurstofan")

#D3
# Heildarsólskynsstundir eftir árstíðum Á Akureyri og í Reykjavík
ggplot(data = AV) +
  geom_bar(mapping = aes(x = arstid, y = solskynsStundir, fill = stadur)
           , stat ="identity"
           , position = "dodge") +
  scale_fill_manual(values=litir) +
  labs(subtitle="Á Akureyri og í Reykjavík", 
       y="sólskynsstundir", 
       x="Árstíðir", 
       title="Heildarsólskynsstundir eftir árstíðum", 
       caption = "Source: Veðurstofa")

##e)
#E1
#Meðalhiti

ggplot(AV, aes(x = arstid, y = medalHiti, fill = stadur)) +
  geom_boxplot(shape=21, position = "dodge",
             aes(fill = factor(stadur))) + 
  scale_fill_manual(values=litir) +
  labs(title="Dreifing meðalhita á mánuði eftir árstíðum", 
       subtitle="Á Akureyri og í Reykjavík", 
       y="sólskynsstundir", 
       x="Mánuður", 
       caption = "Source: Veðurstofan")

#E2
#whattheheck

#E3

ggplot(AV, aes(x = arstid, y = heildarUrkomaPerMan, fill = stadur)) +
  geom_boxplot(shape=21, position = "dodge",
               aes(fill = factor(stadur))) + 
  scale_fill_manual(values=litir) +
  labs(title="Dreifing mánaðarlegrarúrkomu eftir árstíðum", 
       subtitle="Á Akureyri og í Reykjavík", 
       y="sólskynsstundir", 
       x="Mánuður", 
       caption = "Source: Veðurstofan")

##f)
ggplot(AV, aes(x = medalThristingur, y = heildarUrkomaPerMan)) +
  geom_point() + 
  geom_smooth(method = 'lm',se=F) +
  labs(title="", 
       subtitle="", 
       y="meðalloftþrýstingur", 
       x="mánaðarlegúrkoma", 
       caption = "Source: Veðurstofan")
# scale_colour_gradient(
#   low = "#00a6fb",
#   high = "#ffee88",
#   space = "Lab",
#   na.value = "grey50",
#   guide = "colourbar",
#   aesthetics = "fill"
# ) +

#Sjáum að gögnin benda til þess að þegar meðal þrýstingur er hærri er úrkoma
#lág og öfugt þegar úrkoma er mikil

##g)
G1 <- select(AV, man,ar)
G1 <- AV %>% 
  select(man,arstid, solskynsStundir, heildarUrkomaPerMan, medalVindhradi)
glimpse(G1)

#Þetta eru gögn á bilinu 1949 til 2019
G1 %>% 
  group_by(arstid) %>% 
  summarise(avgVindhradi = mean(medalVindhradi),
            sfVindhrada = sd(medalVindhradi),
            avgUrkomaPerMan = mean(heildarUrkomaPerMan),
            sfUrkomu = sd(heildarUrkomaPerMan),
            avgSolskynsstundir = mean(solskynsStundir, na.rm = TRUE),
            sfSolskynsstundir = sd(solskynsStundir, na.rm = TRUE)
            )
min(AV$ar)
max(AV$ar)
##h)
H <- AV %>% 
  select(man, solskynsStundir, stadur)
sameiginlegtMedaltal <- mean(H$solskynsStundir, na.rm = TRUE)
H %>%   
  group_by(stadur) %>% 
  summarise(fjöldiManYfir = nrow(H[H$solskynsStundir > sameiginlegtMedaltal,]))

##i)
AV %>% 
  group_by(stadur) %>% 
  summarise(maxUrkoma = max(maxDagUrkomaPerMan),
            artal = AV[which.max(AV$maxDagUrkomaPerMan),"ar"],
            manudur = AV[which.max(AV$maxDagUrkomaPerMan),"man"],
            dagur = AV[which.max(AV$maxDagUrkomaPerMan),"dagsettningMaxUrkoma"]
            )







