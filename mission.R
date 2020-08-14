
library(ncrmp.benthics.analysis)
library(devtools)
library(plotly)
library(knitr)
library(reshape2)
library(tidyr)
library(gridExtra)
library(echarts4r.assets)
library(waffle)
library(extrafont)
library(tidyverse)
library(echarts4r)
library(fontawesome)
library(ggplot2)
library(dplyr)
library(xlsx)
library(readxl)
library(RColorBrewer)

#master lists, sop
#BENTHIC COVER
cover<-read_xlsx("benthic_covers_downloads.xlsx", sheet = "CoverCategories")
benthic.cd<-read_xlsx("NCRMP_FLORIDA2018_Benthic_DataDictionary06_Codes.xlsx", sheet = "SPECIES")
cover.all<-cover%>%
  replace(is.na(.),0)%>%
  group_by(COVER_CAT_CD)%>%
  summarise(h_num=sum(HARDBOTTOM_NUM), s_num=sum(SOFTBOTTOM_NUM), r_num=sum(RUBBLE_NUM)) %>%
   mutate(.,all= h_num+s_num+r_num) %>%
   rename(CODE = COVER_CAT_CD)
 join.cvr<-right_join(cover.all,benthic.cd,by = "CODE") %>%
   drop_na() 
 cover.type<- join.cvr %>%
   group_by(Type) %>%
   summarise(a=sum(all))
 
#pie chart
pie(cover.type$a,labels = cover.type$a,col=rainbow(length(cover.type$a)),
    main = "Type of Cover Observed")
lbls<-as.vector(cover.type$Type)
legend(0.8,0.9,lbls, cex = 0.5, fill= rainbow(length(lbls)))

#barplot
g<-ggplot(cover.type,aes(x=Type,y=a))+geom_bar(stat="identity",aes(fill=Type))+coord_flip()
  theme_classic()+ggtitle("Type of Cover Observed")+xlab("Cover Types")+
  ylab("Count")
g

#Hard corals
hard<-join.cvr %>%
  filter(Type == "HARD CORALS")

h<-ggplot(hard,aes(x=Scientific_Name, y=all))+geom_bar(stat="identity",aes(fill=CODE))+coord_flip()+
  theme_classic()+labs(title = "Count of Hard Corals Observed",x="Species",y="Count")
h


#CORAL DISEASE OBSERVED
demo<-read_xlsx("coral_demographics_downloads.xlsx", sheet = "DemoCorals")
demo.dis<-demo %>%
  filter(DISEASE != "absent")
#56 of 1519 had evidence of disease
total<-demo.dis %>%
  summarise(n())
#10 of 56 is present
demo.pres<-demo.dis %>%
  filter(DISEASE =="present") %>%
  count(DISEASE)
pres.perc<-(demo.pres$n/total)*100
#33 of 56 is fast
demo.fast<-demo.dis %>%
  filter(DISEASE =="fast") %>%
  count(DISEASE)
fast.perc<-(demo.fast$n/total)*100
#13 of 56 is slow
demo.slow<-demo.dis %>%
  filter(DISEASE =="slow") %>%
  count(DISEASE)
slow.perc<-(demo.slow$n/total)*100
x=paste("Fast=",fast.perc$`n()`,"%",sep="")
#research how to input string, name from a variable
#other instead of present, does not say percentage in legend
library(waffle)
waffle(
  c('Fast' = fast.perc$`n()`, 'Slow' = slow.perc$`n()`,
    'Other' = pres.perc$`n()`), 
  rows = 10, colors = c("#FD6F6F", "darkgoldenrod1","cadetblue3"),
  title = 'Coral Disease Observed', legend_pos="bottom"
)

#ESA CORALS
benthic<-read_xlsx("benthic_covers_downloads.xlsx", sheet = "BenthicCoverSample")
benthic.esa<- benthic %>% 
  filter(A_PALMATA == "1" | A_CERVICORNIS == "1" | D_CYLINDRUS == "1" | M_FEROX == "1" |
           M_ANNULARIS == "1" | M_FRANKSI == "1" | M_FAVEOLATA == "1") %>%  
  summarise(apal=sum(A_PALMATA),acer=sum(A_CERVICORNIS),dcyl=sum(D_CYLINDRUS),
            mfer=sum(M_FEROX),mann=sum(M_ANNULARIS),mfra=sum(M_FRANKSI),mfav=sum(M_FAVEOLATA)) %>%
 pivot_longer(cols=apal:mfav,names_to="Corals",values_to="Occurence") %>%
  mutate(.,percentage=(Occurence/total.samp)*100)

total.samp<-n_distinct(benthic$MASTER_SAMPLE_CD)
#percent esa corals present
#0=absent, 1=present in transect, 2=presnt on dive, 3=did not look. ONLY look at 1

#barplot
esa.bar<-ggplot(benthic.esa,aes(x=reorder(Corals,-percentage),y=percentage))+geom_bar(stat="identity",
          aes(fill=Corals))+theme_classic()+
  ggtitle("Percent Occurence of ESA Corals")+xlab("ESA Listed Corals")+
  ylab("Percentage Present")
esa.bar


#REEF FISH
#PIE OF SPECIES
fish<-read_xlsx("fish_downloads.xlsx", sheet = "species")
master<-read_xlsx("NCRMP_Fish_SppList_Export.xlsx", sheet = "NCRMP_Fish_MasterList")
fish.spp<-fish %>%
  group_by(SPECIES_CD)%>%
  summarise(count=sum(NUMBER_OF_INDIVIDUALS))
join.fish<-right_join(fish.spp,master,by = "SPECIES_CD") %>%
  select(SPECIES_CD,count,SCIENTIFIC_NAME,COMMON_NAME,Groups) %>%
  drop_na() 
fish.groups<-join.fish %>%
  group_by(Groups) %>%
  summarise(num=sum(count))

pie(fish.groups$num,labels = fish.groups$num, col=rainbow(length(fish.groups$Groups)),
    main="Number of Fish Observed")
legend("topright", fish.groups$Groups, cex = 0.5, fill= rainbow(length(fish.groups$Groups)))
#barplot
fish.bar<-ggplot(fish.groups,aes(x=reorder(Groups,-num),y=num))+geom_bar(stat="identity",aes(fill=Groups))+
  theme_classic()+coord_flip()+labs(title="Number of Fish Observed",x="Fish Groups",
                                    y="Total Counted")
fish.bar

#PIE OF COMMERCIAL, grouper-snapper complex
#try to remove yellowtail and maybe gray to see if scale is fixed
fish.spp
com.fish<-join.fish %>%
  filter(SPECIES_CD %in% c("CEP CRUE","CEP FULV","EPI ADSC","EPI GUTT",
                                           "EPI MORI","EPI STRI","LUT ANAL","LUT APOD",
                                           "LUT BUCC","LUT CYAN","LUT GRIS","LUT JOCU",
                                           "LUT MAHO","LUT SYNA","MYC BONA","MYC INTE",
                                           "MYC TIGR","MYC VENE","MYC PHEN","OCY CHRY",
                                           "LAC MAXI","PTE VOLI"))
#similar 
labels2<-c("Graysby","Coney","Rock Hind","Red Hind","Red Grouper", "Nassau Grouper",
           "Mutton Snapper","Schoolmaster","Blackfin Snapper","Cubera Snapper",
           "Gray Snapper","Dog Snapper","Mahogany Snapper","Lane Snapper","Black Grouper",
           "Yellowmouth Grouper","Tiger Grouper","Yellowfin Grouper","Scamp","Yellowtail
           Snapper","Hogfish","Red Lionfish")
pie(com.fish$count,labels = com.fish$count, col=rainbow(length(labels2)),
    main="Number of Commercial Fish Observed")
legend("topright", c("Graysby","Coney","Rock Hind","Red Hind","Red Grouper", "Nassau Grouper",
                     "Mutton Snapper","Schoolmaster","Blackfin Snapper","Cubera Snapper",
                     "Gray Snapper","Dog Snapper","Mahogany Snapper","Lane Snapper","Black Grouper",
                     "Yellowmouth Grouper","Tiger Grouper","Yellowfin Grouper","Scamp","Yellowtail
           Snapper","Hogfish","Red Lionfish"), cex = 0.7, fill= rainbow(length(labels2)))

#barplot
com.bar<-ggplot(com.fish,aes(x=SPECIES_CD,y=count))+geom_bar(stat="identity",
aes(fill=SPECIES_CD))+theme_classic()+coord_flip()+
  labs(title="Commercial and Recreational Fish Observed",x="Species",
                                    y="Total Counted")
com.bar
#different graph? stacked bar?try to remove yellowtail and maybe gray to see if scale is fixed
com.stack<-ggplot(com.fish,aes(x=group,y=count))+geom_bar(stat="identity",
                                                        aes(fill=SPECIES_CD))+theme_classic()+coord_flip()+
  labs(title="Commercial and Recreational Fish Observed",x="Species",
       y="Total Counted")
com.stack



