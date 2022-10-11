#Visualize households on the map
library(rgdal) #
library(sf)
library(rgeos) #
library(tmap)
library(leaflet)

#transforming data to spartial objects
df3<- st_as_sf(df2, coords = c("q8_2", "q8_1"),  crs = 4326)
library(mapview)
mapview(df3, map.types = "OpenStreetMap.Mapnik")

phubinh_map <- read_sf("../GIS/Phu-Binh/phubinh-border.shp")
phubinh_households <- read_sf("../GIS/Phu-Binh/phubinh-households.shp")

mapview(df3, map.types = "OpenStreetMap.Mapnik")+
  mapview(phubinh_map)

mapview(phubinh_map)+
  mapview(phubinh_households,legend = FALSE)

House.Points <-SpatialPointsDataFrame(df2[,9:8], df2, proj4string = CRS("+init=EPSG:4326"))
library(tmap)

tm_shape(phubinh_map) + 
  tm_borders(alpha=.4)+
  tm_shape(House.Points)+ tm_dots(col = "a2",palette = "Reds", style = "quantile")


#Conventional Graph and EDA

##town distribution
#########number of interviews in each town
ggplot(df2%>%
               filter(!is.na(q5))%>%
               count(q5),aes(x=reorder(q5,-n),y=n, color = factor(q5))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = 0,hjust=-0.5)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    x="Town",
    y = "Number",
    colour = "Labels"
  )+
  coord_flip()

# Age distribution
df2$a2 <- as.integer(df2$a2) # concerning to integer value

df2 %>%
  ggplot( aes(a2))+
  geom_boxplot()+  
  labs(
    x="Age")

##Gender distribution

#plot with both value and percentage.

a3<-df2%>%
  filter(!is.na(a3))%>%
  count(a3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
ggplot(a3,aes(x=reorder(a3,-n),y=n, fill = factor(a3))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    x="Gender",
    y = "Number")


#combine two variables in the same plots using bar and stacked bar plot.

ggplot(df2%>%
         filter(!is.na(a4))%>%
         count(a4,a3), aes(x = reorder(a3,-n),y=n, fill = a4, label=n)) +
  geom_bar(stat = "identity")+
  geom_text(
    size = 3, position = position_stack(vjust = 0.5))+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Education")+
  labs(
    x="Gender",
    y = "Number"
  )

#group bar
library(titanic)
# over education


#distribution of education - overall
summary(df2$a4)


a4<-df2%>%
  filter(!is.na(a4))%>%
  count(a4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
ggplot(a4, aes(x = reorder(a4,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Education",
    y = "Number"
  )

# education vs gender
a4a3<-df2%>%
  filter(!is.na(a4))%>%
  count(a4,a3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
ggplot(a4a3, aes(x = reorder(a4,-n),y=n, fill = a3, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
#  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Education",
    y = "Number"
  )

# education vs town

a4q5<-df2%>%
  filter(!is.na(a4))%>%
  count(a4,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

ggplot(a4q5, aes(x = reorder(a4,-n),y=n, fill = q5, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
#  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
 # geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1,angle = 90)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Education",
    y = "Number"
  )

#example with 3 variables
a3a4a5<-df2%>%
  drop_na(a3, a4,a5)%>%
  count(a4,a3,a5)
ggplot(a3a4a5, aes(x = a3, y = n, fill = a5)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ a4)+
  scale_fill_discrete(name = "Districts")+
  labs(
    x="Gender",
    y = "Number"
  )

###
summary(a5)
a5<-df2%>%
  filter(!is.na(a5))%>%
  count(a5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
ggplot(a5, aes(x = reorder(a5,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Occupation",
    y = "Number"
  )+
  coord_flip()


#### Relationship with the owner
summary(a6)
a6<-df2%>%
  filter(!is.na(a6))%>%
  count(a6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
ggplot(a6, aes(x = reorder(a6,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Status in the house",
    y = "Number"
  )+
  coord_flip()


### Total in the family

summary(df2$a7)

df2 %>%
  ggplot( aes(a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    x="Family Members")

ggplot(df2, aes(x=q5,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    x="Town",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()

ggplot(df2, aes(x=a4,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    x="Town",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()






