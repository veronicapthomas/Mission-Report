
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
  summarise(h_num=sum(HARDBOTTOM_NUM), s_num=sum(SOFTBOTTOM_NUM), r_num=sum(RUBBLE_NUM))
 cover.sum<-cover.all %>%
   mutate(.,all= h_num+s_num+r_num) %>%
   rename(CODE = COVER_CAT_CD)
 join.cvr<-right_join(cover.sum,benthic.cd,by = "CODE") %>%
   drop_na() %>%
   group_by(Type) %>%
   summarise(a=sum(all))
 
#categories: hard coral, soft coral, sand, algae and turf, sponge, other. pie?
#macro and turf together or separate?
 #in depth look of hard corals. pie? bar?

pie(join.cvr$a,labels = join.cvr$a,col=rainbow(length(join.cvr$a)),
    main = "Type of Cover Observed")
lbls<-as.vector(join.cvr$Type)
legend(0.8,0.9,lbls, cex = 0.5, fill= rainbow(length(lbls)))

#barplot
g<-ggplot(join.cvr,aes(x=Type,y=a))+geom_bar(stat="identity",aes(fill=Type))+coord_flip()
  theme_classic()+ggtitle("Type of Cover Observed")+xlab("Cover Types")+
  ylab("Count")
g

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
#other instead of present
library(waffle)
waffle(
  c(x = fast.perc$`n()`, 'Slow=23%' = slow.perc$`n()`,
    'Present=18%' = pres.perc$`n()`), 
  rows = 10, colors = c("#FD6F6F", "darkgoldenrod1","cadetblue3"),
  title = 'Coral Disease Observed', legend_pos="bottom"
)

#ESA CORALS
benthic<-read_xlsx("benthic_covers_downloads.xlsx", sheet = "BenthicCoverSample")
benthic.esa<- benthic %>% 
  summarise(apal=sum(A_PALMATA),acer=sum(A_CERVICORNIS),
                 dcyl=sum(D_CYLINDRUS),mfer=sum(M_FEROX),mann=sum(M_ANNULARIS),
                 mfra=sum(M_FRANKSI),mfav=sum(M_FAVEOLATA))
total.samp<-n_distinct(benthic$MASTER_SAMPLE_CD)
#filter for 1, sum 1. after filter, calc percentages (new column)
#percent esa corals present
#0 absent 1 present in transect 2 presnt on dive 3 did not look. ONLY look at 1
A.pal<-benthic.esa$apal/total.samp
A.cer<-benthic.esa$acer/total.samp
D.cyl<-benthic.esa$dcyl/total.samp   
M.fer<-benthic.esa$mfer/total.samp    
M.ann<-benthic.esa$mann/total.samp   
M.fra<-benthic.esa$mfra/total.samp   
M.fav<-benthic.esa$mfav/total.samp  
#dataframe use piping to get this
esa.corals<-data.frame(corals=c("A.pal","A.cer","D.cyl","M.fer","M.ann","M.fra","M.fav"),
                       perc=c(A.pal,A.cer,D.cyl,M.fer,M.ann,M.fra,M.fav)*100)
#barplot
esa.bar<-ggplot(esa.corals,aes(x=corals,y=perc))+geom_bar(stat="identity",
          aes(fill=corals))+theme_classic()+
  ggtitle("Percent Occurence of ESA Corals")+xlab("ESA Listed Corals")+
  ylab("Percentage Present")
esa.bar
#order by decreasing order

#REEF FISH
#PIE OF SPECIES
fish<-read_xlsx("fish_downloads.xlsx", sheet = "species")
fish.spp<-fish %>%
  group_by(SPECIES_CD)%>%
  summarise(count=sum(NUMBER_OF_INDIVIDUALS))
# group into categories
#remove and add column in species list, read in species list and join tables


groups<-fish.spp %>%
  group_by(group) %>%
  summarise(total=sum(count))

#come up with better grouping, is barplot better? gobi and blenni. rm silvery, surgeon, butterfly
labels1<-c("Angelfish", "Blenny","Damselfish","Goby","Grunt","Hogfish","Jack","Other","Parrotfish",
           "Porgy","Sea bass and groupers","Snapper","Wrasses")
pie(groups$total,labels = groups$total, col=rainbow(length(labels1)),
    main="Number of Fish Observed")
legend(1.45,1.4, c("Angelfish", "Blenny","Damselfish","Goby","Grunt","Hogfish","Jack","Other",
                     "Parrotfish","Porgy","Sea bass and groupers","Snapper","Wrasses"), 
       cex = 0.5, fill= rainbow(length(labels1)))
#barplot
fish.bar<-ggplot(groups,aes(x=group,y=total))+geom_bar(stat="identity",aes(fill=group))+
  theme_classic()+coord_flip()+labs(title="Number of Fish Observed",x="Fish Groups",
                                    y="Total Counted")
#wrasse (hogfish), snappers, groupers, parrot, grunt,goby and blenny, other, damselfish
#how to make total counted axis look nicer?
fish.bar

#PIE OF COMMERCIAL, grouper-snapper complex
#try to remove yellowtail and maybe gray to see if scale is fixed
fish.spp
com.fish<-fish.spp %>%
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



