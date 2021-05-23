# Öngyilkossági adatok vizualizációja

setwd("C:/Users/Szabó Eszter/Documents/KRE/R_Stat/Beadandó")

# master.csv (leírás itt: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/)

dataset <- read.csv ("master.csv") 

dataset
str(dataset)

library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Globális átlag
globalis<-aggregate(suicides.100k.pop ~ year, dataset, mean, na.rm=T)
str(globalis)

png(file="E01_glob.png", width = 1024, height = 576, units = "px")
ggplot(data = globalis, mapping = aes(x=year, y=suicides.100k.pop)) + 
  geom_line(color="darkblue") + 
  geom_point(color="darkblue") +
  coord_cartesian(xlim =c(1985, 2015), ylim = c(0, 20)) +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "100 ezer főre jutó évenkénti öngyilkosságok, globális átlag") +
  theme_bw()
dev.off()

#Ábrák

# a. ábra: Idősor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok (átlag - v. total-ra átszámítva)
# Lásd: Skandináv és Mediterrán országok


# b. ábra: Idősor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok CSAK FÉRFIAK (átlag - v. FÉRFIAK-ra átszámítva)
# c. ábra: Idősor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok CSAK NŐK     (átlag - v. NŐK-ra átszámítva)
nem<-aggregate(suicides.100k.pop ~ year + sex, dataset, mean)

str(nem)

png(file = "E02_bc.png", width = 1024, height = 576, units = "px")
ggplot(data = nem, mapping = aes(x=year, y=suicides.100k.pop, col=sex, group=sex)) + 
  geom_line() + 
  geom_point() +
  coord_cartesian(xlim =c(1985, 2015), ylim = c(0, 25)) +
  scale_colour_hue(name="Nem") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "100 ezer főre jutó évenkénti öngyilkosságok átlaga nemenként") +
  theme_bw()
dev.off()

# d. ábra: Idősor (x tengely: évek és y tengely: suicides/100k pop) vonalak generációk (átlag - v. Generációkra-ra átszámítva)
gen<-aggregate (suicides.100k.pop ~ year + generation, dataset, mean)
str(gen)

png(file="E03_gen.png", width = 1024, height = 576, units = "px")
ggplot(data = gen, mapping = aes(x=year, y=suicides.100k.pop, col=generation, group=generation)) + 
  geom_line() + 
  geom_point() +
  coord_cartesian(xlim =c(1985, 2015)) +
  scale_colour_hue(name="Generáció") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "100 ezer főre jutó évenkénti öngyilkosságok átlaga generációnként") +
  theme_bw()
dev.off()

# OSZLOP DIAGRAMMOK EHHEZ HASONLÓ: https://observablehq.com/@d3/grouped-bar-chart

# e. ábra: (x tengely: országok (csak a 6 legnagyobb öngyilk. arányú) - y tengely: suicides/100k pop - csoportok: évek) (átlag - v. átszámítva)

#melyik a 6 legmagasabb öngyi ország:

sorba<-aggregate(suicides.100k.pop ~ country, dataset, mean, na.rm=T)
str(sorba)
sorba[order(sorba$suicides.100k.pop),]

# index                     ország        átlag

# 48                    Kazakhstan        30.5112821
# 12                       Belarus        31.0759127
# 41                       Hungary        32.7615161
# 76            Russian Federation        34.8923765
# 88                     Sri Lanka        35.2951515
# 53                     Lithuania        40.4155725


# melyik a 6 legmagasabb
which(dataset == "Lithuania") # 15045 - 15306
which(dataset == "Sri Lanka") # 23901 - 24032
which(dataset == "Russian Federation") # 20937 - 21260
which(dataset == "Hungary") # 11365 - 11674
which(dataset == "Belarus") # 3189 - 3440
which(dataset == "Kazakhstan") # 13737 - 14048

# 6 legmagasabb kiválasztása (külön-külön)
Lithuania<-aggregate(suicides.100k.pop ~ year + country, dataset[15045:15306, ], mean)
Sri_Lanka<-aggregate(suicides.100k.pop ~ year + country, dataset[23901:24032, ], mean)
Russian_Federation<-aggregate(suicides.100k.pop ~ year + country, dataset[20937:21260, ], mean)
Hungary<-aggregate(suicides.100k.pop ~ year + country, dataset[11365:11674, ], mean)
Belarus<-aggregate(suicides.100k.pop ~ year + country, dataset[3189:3440, ], mean)
Kazakhstan<-aggregate(suicides.100k.pop ~ year + country, dataset[13737:14048, ], mean)

str(Lithuania)
str(Sri_Lanka)
str(Russian_Federation)
str(Hungary)
str(Belarus)
str(Kazakhstan)


elso6<-rbind(Lithuania, Sri_Lanka, Russian_Federation, Hungary, Belarus, Kazakhstan)
str(elso6)

png(file="E04_elso_6_line.png", width = 1024, height = 576, units = "px")
ggplot(data = elso6, mapping = aes(x=year, y=suicides.100k.pop, col=country, group=country)) + 
  geom_line() + 
  geom_point() +
  coord_cartesian(xlim =c(1985, 2015), ylim = c(0,60)) +
  scale_colour_hue(name="Országok") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "Top 6 legmagasabb átlag - 100 ezer főre jutó öngyilkosságok átlaga országonként") +
  theme_bw()
dev.off()

png(file="E05_elso_6_bar.png", width = 1024, height = 576, units = "px")
ggplot(data = elso6, mapping = aes(x=country, y=suicides.100k.pop, group = year, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  #coord_cartesian(xlim =c(1985, 2015)) +
  #scale_colour_hue(name="Országok") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Top 6 legmagasabb átlag - 100 ezer főre jutó öngyilkosságok átlaga országonként") +
  theme_bw()
dev.off()


# f. ábra:  (x tengely: országok (csak a 6 legnagyobb öngyilk. arányú) - y tengely: suicides/100k pop - csoportok: generációk) (átlag - v. átszámítva)

Lit_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[15045:15306, ], mean)
Sri_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[23901:24032, ], mean)
Rus_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[20937:21260, ], mean)
Hun_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[11365:11674, ], mean)
Bela_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[3189:3440, ], mean)
Kaz_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[13737:14048, ], mean)

str(Lit_gen)
str(Sri_gen)
str(Rus_gen)
str(Hun_gen)
str(Bela_gen)
str(Kaz_gen)

elso6_gen<-rbind(Lit_gen, Sri_gen, Rus_gen, Hun_gen, Bela_gen, Kaz_gen)
str(elso6_gen)

png(file="E06_elso_6_gen.png", width = 1024, height = 576, units = "px")
ggplot(data = elso6_gen, mapping = aes(x=generation, y=suicides.100k.pop, group = country, fill=country)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  #coord_cartesian(xlim =c(1985, 2015)) +
  #scale_colour_hue(name="Országok") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Top 6 legmagasabb átlag - 100 ezer főre jutó öngyilkosságok átlaga országonként, generációs bontásban") +
  theme_bw()
dev.off()

# Skandináv országok

which(dataset == "Iceland") # 11675:12056
which(dataset == "Norway") # 17869:18228
which(dataset == "Sweden") # 24369:24726
which(dataset == "Finland") # 8739:9086
which(dataset == "Denmark") # 7419:7682

Iceland<-aggregate(suicides.100k.pop ~ year + country, dataset[11675:12056, ], mean)
Norway<-aggregate(suicides.100k.pop ~ year + country, dataset[17869:18228, ], mean)
Sweden<-aggregate(suicides.100k.pop ~ year + country, dataset[24369:24726, ], mean)
Finland<-aggregate(suicides.100k.pop ~ year + country, dataset[8739:9086, ], mean)
Denmark<-aggregate(suicides.100k.pop ~ year + country, dataset[7419:7682, ], mean)

str(Iceland)
str(Norway)
str(Sweden)
str(Finland)
str(Denmark)

Skandinavia<-rbind(Iceland, Norway, Sweden, Finland, Denmark)
str(Skandinavia)

png(file="E07_sk_orsz.png", width = 1024, height = 576, units = "px")
ggplot(data = Skandinavia, mapping = aes(x=year, y=suicides.100k.pop, col=country, group=country)) + 
  geom_line() + 
  geom_point() +
  coord_cartesian(xlim =c(1985, 2015), ylim = c(0,35)) +
  scale_color_brewer(palette="Set1") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "Skandináv országok - 100 ezer főre jutó öngyilkosságok átlaga évenként") +
  theme_bw()
dev.off()


# Skandináv országok - nemek
Ice_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[11675:12056, ], mean)
Nor_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[17869:18228, ], mean)
Swed_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[24369:24726, ], mean)
Fin_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[8739:9086, ], mean)
Den_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[7419:7682, ], mean)

srt(Ice_nem)
str(Nor_nem)
str(Swed_nem)
str(Fin_nem)
str(Den_nem)

Sk_nem<-rbind(Ice_nem, Nor_nem, Swed_nem, Fin_nem, Den_nem)
str(Sk_nem)

png(file="E08_sk_nem.png", width = 1024, height = 576, units = "px")
ggplot(data = Sk_nem, mapping = aes(x=sex, y=suicides.100k.pop, group = country, fill=country)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Set1") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Skandináv országok - 100 ezer főre jutó öngyilkosságok átlaga nemenként") +
  theme_bw()
dev.off()

# Skandináv országok - generációk
Ice_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[11675:12056, ], mean)
Nor_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[17869:18228, ], mean)
Swed_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[24369:24726, ], mean)
Fin_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[8739:9086, ], mean)
Den_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[7419:7682, ], mean)

srt(Ice_gen)
str(Nor_gen)
str(Swed_gen)
str(Fin_gen)
str(Den_gen)

Sk_gen<-rbind(Ice_gen, Nor_gen, Swed_gen, Fin_gen, Den_gen)
str(Sk_gen)

png(file="E09_sk_gen.png", width = 1024, height = 576, units = "px")
ggplot(data = Sk_gen, mapping = aes(x=generation, y=suicides.100k.pop, group = country, fill=country)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Set1") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Skandináv országok - 100 ezer főre jutó öngyilkosságok átlaga generációnként") +
  theme_bw()
dev.off()

# 5 medirerrán ország: Portugália, Spanyolország, Franciaország, Olaszország, Görögország

which(dataset == "Portugal") # 19357:19680
which(dataset == "Spain") # 23529:23900
which(dataset == "France") # 9087:9446
which(dataset == "Italy") # 12789:13160
which(dataset == "Greece") # 10023:10394

Portugal<-aggregate(suicides.100k.pop ~ year + country, dataset[19357:19680, ], mean)
Spain<-aggregate(suicides.100k.pop ~ year + country, dataset[23529:23900, ], mean)
France<-aggregate(suicides.100k.pop ~ year + country, dataset[9087:9446, ], mean)
Italy<-aggregate(suicides.100k.pop ~ year + country, dataset[12789:13160, ], mean)
Greece<-aggregate(suicides.100k.pop ~ year + country, dataset[10023:10394, ], mean)

str(Portugal)
str(Spain)
str(France)
str(Italy)
str(Greece)

Med<-rbind(Portugal, Spain, France, Italy, Greece)
str(Med)

png(file="E10_med_orsz.png", width = 1024, height = 576, units = "px")
ggplot(data = Med, mapping = aes(x=year, y=suicides.100k.pop, col=country, group=country)) + 
  geom_line() + 
  geom_point() +
  coord_cartesian(xlim =c(1985, 2015), ylim = c(0,35)) +
  scale_color_brewer(palette="Dark2") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="Év", title = "Mediterrán országok - 100 ezer főre jutó öngyilkosságok átlaga évenként") +
  theme_bw()
dev.off()

# Mediterrán országok - nemek

Port_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[19357:19680, ], mean)
Sp_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[23529:23900, ], mean)
Fr_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[9087:9446, ], mean)
It_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[12789:13160, ], mean)
Gr_nem<-aggregate(suicides.100k.pop ~ sex + country, dataset[10023:10394, ], mean)

str(Port_nem)
str(Sp_nem)
str(Fr_nem)
str(It_nem)
str(Gr_nem)

Med_nem<-rbind(Port_nem, Sp_nem, Fr_nem, It_nem, Gr_nem)
str(Med_nem)

png(file="E11_med_nem.png", width = 1024, height = 576, units = "px")
ggplot(data = Med_nem, mapping = aes(x=sex, y=suicides.100k.pop, group = country, fill=country)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Dark2") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Mediterrán országok - 100 ezer főre jutó öngyilkosságok átlaga nemenként") +
  theme_bw()
dev.off()

# Mediterrán országok - generációk

Port_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[19357:19680, ], mean)
Sp_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[23529:23900, ], mean)
Fr_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[9087:9446, ], mean)
It_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[12789:13160, ], mean)
Gr_gen<-aggregate(suicides.100k.pop ~ generation + country, dataset[10023:10394, ], mean)

str(Port_gen)
str(Sp_gen)
str(Fr_gen)
str(It_gen)
str(Gr_gen)

Med_gen<-rbind(Port_gen, Sp_gen, Fr_gen, It_gen, Gr_gen)
str(Med_gen)

png(file="E12_med_gen.png", width = 1024, height = 576, units = "px")
ggplot(data = Med_gen, mapping = aes(x=generation, y=suicides.100k.pop, group = country, fill=country)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Dark2") +
  labs(y="100 ezer főre jutó öngyilkosságok átlaga", x="", title = "Mediterrán országok - 100 ezer főre jutó öngyilkosságok átlaga generációnként") +
  theme_bw()
dev.off()


