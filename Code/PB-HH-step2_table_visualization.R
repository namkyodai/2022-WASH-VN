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
graph_q5 <- ggplot(df2%>%
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

ggsave("../visuals/PB/graph_q5.png", plot = graph_q5)



# Age distribution
df2$a2 <- as.integer(df2$a2) # concerning to integer value

graph_a2<-df2 %>%
  ggplot(aes(a2))+
  geom_boxplot()  +  
  labs(
    x="Age")
graph_a2
ggsave("../visuals/PB/graph_a2.png", plot = graph_a2)


##Gender distribution

#plot with both value and percentage.

a3<-df2%>%
  filter(!is.na(a3))%>%
  count(a3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a3<-ggplot(a3,aes(x=reorder(a3,-n),y=n, fill = factor(a3))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    x="Gender",
    y = "Number")

graph_a3
ggsave("../visuals/PB/graph_a3.png", plot = graph_a3)


#combine two variables in the same plots using bar and stacked bar plot.

graph_a3a4<-ggplot(df2%>%
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
graph_a3a4
ggsave("../visuals/PB/graph_a3a4.png", plot = graph_a3a4)



#group bar
library(titanic)
# over education


#distribution of education - overall
summary(df2$a4)


a4<-df2%>%
  filter(!is.na(a4))%>%
  count(a4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a4<-ggplot(a4, aes(x = reorder(a4,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Education",
    y = "Number"
  )
graph_a4
ggsave("../visuals/PB/graph_a4.png", plot = graph_a4)


# education vs gender
a4a3<-df2%>%
  filter(!is.na(a4))%>%
  count(a4,a3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))


graph_a4a3<-ggplot(a4a3, aes(x = reorder(a4,-n),y=n, fill = a3, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
#  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Education",
    y = "Number"
  )

graph_a4a3
ggsave("../visuals/PB/graph_a4a3.png", plot = graph_a4a3)


# education vs town

a4q5<-df2%>%
  filter(!is.na(a4))%>%
  count(a4,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a4a5<-ggplot(a4q5, aes(x = reorder(a4,-n),y=n, fill = q5, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
#  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
 # geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1,angle = 90)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Education",
    y = "Number"
  )
graph_a4a5
ggsave("../visuals/PB/graph_a4a5.png", plot = graph_a4a5)



#example with 3 variables
a3a4a5<-df2%>%
  drop_na(a3, a4,a5)%>%
  count(a4,a3,a5)
graph_a3a4a5<-ggplot(a3a4a5, aes(x = a3, y = n, fill = a5)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ a4)+
  scale_fill_discrete(name = "Districts")+
  labs(
    x="Gender",
    y = "Number"
  )
graph_a3a4a5
ggsave("../visuals/PB/graph_a3a4a5.png", plot = graph_a3a4a5)


###
summary(df2$a5)
a5<-df2%>%
  filter(!is.na(a5))%>%
  count(a5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a5<-ggplot(a5, aes(x = reorder(a5,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    title = "Occupation - Number",
    x="Occupation",
    y = "Number"
  )+
  coord_flip()
graph_a5
ggsave("../visuals/PB/graph_a5.png", plot = graph_a5)



#### Relationship with the owner
summary(df2$a6)
a6<-df2%>%
  filter(!is.na(a6))%>%
  count(a6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a6<-ggplot(a6, aes(x = reorder(a6,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    title = "Status in the house - Number",
    x="Status in the house",
    y = "Number"
  )+
  coord_flip()
graph_a6
ggsave("../visuals/PB/graph_a6.png", plot = graph_a6)


### Total in the family

summary(df2$a7)

graph_a7_box<-df2 %>%
  ggplot( aes(a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    x="Family Members")
graph_a7_box
ggsave("../visuals/PB/graph_a7_box.png", plot = graph_a7_box)


graph_a7q5<-ggplot(df2, aes(x=q5,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Family Members vs Town",
    x="Town",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()
graph_a7q5
ggsave("../visuals/PB/graph_a7q5.png", plot = graph_a7q5)


graph_a7a4<-ggplot(df2, aes(x=a4,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Family Members vs Occupation",
    x="Occupation",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()

graph_a7a4
ggsave("../visuals/PB/graph_a7a4.png", plot = graph_a7a4)


df2 %>%
  ggplot( aes(x=a7)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.4)

a712<-df2%>%
  select(a1,a7,a7_1,a7_2)
melt_data_a712<-data.frame(melt(a712,id = c("a1")))

melt_data_a712$variable<-recode(melt_data_a712$variable, a7="Total", a7_1="Male", a7_2="Female")

graph_a712 <- ggplot(data=melt_data_a712, aes(x=value, group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(
    title = "Distribution of Members and Gender",
    x = "Numbers of Members",
    y="Density",
  )+
  theme_ipsum()

graph_a712
ggsave("../visuals/PB/graph_a712.png", plot = graph_a712)

### Select asset types that households own (e.g. car, tv, ect)
a8_yes <- df2%>%
  select(q1,a8_1,a8_2,a8_3,a8_4,a8_5,a8_6,a8_7,a8_8,a8_9,a8_10,a8_11,a8_12)

#transform into frame for ggplot ready
a8_yes <- melt(a8_yes,id=c("q1"))

#recode for name
a8_yes$variable<-recode(a8_yes$variable, a8_1="Scooter", a8_2="Car",a8_3="TV",a8_4="Fridge",a8_5="AC",a8_6="Washing Machine", a8_7="Water heater", a8_8="PC/Laptop", a8_9="Landline/mobile", a8_10="ADSL", a8_11="Expensive Furnistrure", a8_12="Water Pump")

#calculate the percentage
a8_yes_perc <- a8_yes%>%
  filter(value=="Yes")%>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$a8_1)*100,2),nsmall=2))

#draw the graph
graph_a8_yes_perc<-ggplot(a8_yes_perc, aes(x = reorder(variable,n),y=n, label=n,fill="variable")) +
  geom_bar(stat="identity")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "none")+
 # scale_fill_discrete(name = "Gender")+
  labs(
    title = "Percentage of households having Assets",
    x="Type of asset",
    y = "Percentage"
  )+
  coord_flip()
graph_a8_yes_perc

ggsave("../visuals/PB/graph_a8_yes_perc.png", plot = graph_a8_yes_perc)

###################################################
##---understand the distribution of each assets with number.


###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################

a8_no <- df2%>%
  select(q1,a8_1_1,a8_2_1,a8_3_1,a8_4_1,a8_5_1,a8_6_1,a8_7_1,a8_8_1,a8_9_1,a8_10,a8_11_1,a8_12_1)

a8_no <- melt(a8_no,id=c("q1"))

a8_no$variable<-recode(a8_no$variable, a8_1_1="Scooter", a8_2_1="Car",a8_3_1="TV",a8_4_1="Fridge",a8_5_1="AC",a8_6_1="Washing Machine", a8_7_1="Water heater", a8_8_1="PC/Laptop", a8_9_1="Landline/mobile", a8_10="ADSL", a8_11_1="Expensive Furnistrure", a8_12_1="Water Pump")

a8_no$value <- recode(a8_no$value, Yes = "1", No ="")

a8_no<- a8_no%>%
  filter(!is.na(value), value>=0)

a8_no

graph_a8_no <- ggplot(data=a8_no, aes(x=value, group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.4) +  
  labs(
    title = "Distribution of Number of Assets",
    x="Number of Asset",
    y = "Density"
  )+
  theme_ipsum()

ggsave("../visuals/PB/graph_a8_no.png", plot = graph_a8_no)


#### House Type
summary(df2$a9)
a9<-df2%>%
  filter(!is.na(a9))%>%
  count(a9)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a9<-ggplot(a9, aes(x = reorder(a9,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of House Types",
    x="House Types",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_a9

ggsave("../visuals/PB/graph_a9.png", plot = graph_a9)



#### House Condition States
summary(df2$a9plus)
a9plus<-df2%>%
  filter(!is.na(a9plus))%>%
  count(a9plus)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a9plus<-ggplot(a9plus, aes(x = reorder(a9plus,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "House Condition States",
    x="Condition States",
    y = "Number/Percentage"
  )#+
#  coord_flip()

ggsave("../visuals/PB/graph_a9plus.png", plot = graph_a9plus)

# combining a9 and a9plus
a9a9plus<-df2%>%
  filter(!is.na(a9))%>%
  count(a9,a9plus)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))


graph_a9a9plus<-ggplot(a9a9plus, aes(x = reorder(a9,n),y=n, fill = a9plus, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Condition States")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "House types and their condition state",
    x="House Type",
    y = "Number/Percentage"
  )+
  coord_flip()
ggsave("../visuals/PB/graph_a9a9plus.png", plot = graph_a9a9plus)


#### Ownership of the house
summary(df2$a10)
a10<-df2%>%
  filter(!is.na(a10))%>%
  count(a10)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a10<-ggplot(a10, aes(x = reorder(a10,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of House Owership",
    x="House Ownership",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_a10
ggsave("../visuals/PB/graph_a10.png", plot = graph_a10)

## 

##########################################
#### Monthly Expenses


##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################


df2$a11_1 <- as.numeric(df2$a11_1) # concerning to integer value

#box plot
graph_a11_1_box<-df2 %>%
  ggplot(aes(a11_1))+
  geom_boxplot()  +  
  labs(
    x="Monthly expenses")
ggsave("../visuals/PB/graph_a11_1_box.png", plot = graph_a11_1_box)

# density
graph_a11_1_den<-df2 %>%
  ggplot(aes(a11_1))+
  geom_density()  +  
  labs(
    x="Monthly expenses")

ggsave("../visuals/PB/graph_a11_1_den.png", plot = graph_a11_1_den)

#box plot with town
graph_a11_1_q5<-ggplot(df2, aes(x=q5,y=a11_1))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Expenses vs Town",
    x="Town",
    y="Millions VND")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()
ggsave("../visuals/PB/graph_a11_1_q5.png", plot = graph_a11_1_q5)


##---understand the distribution of expenses.

a11_v <- df2%>%
  select(q1,a11_1,a11_2,a11_3,a11_4,a11_5,a11_6,a11_7,a11_8,a11_9,a11_10,a11_11,a11_12,a11_13,a11_14,a11_15,a11_16,a11_17)

a11_v <- melt(a11_v,id=c("q1"),na.rm = TRUE, value.name = "value")

glimpse(a11_v)


a11_v$variable<-recode(a11_v$variable, a11_1="Total", a11_2="House Rental",a11_3="Food/Groccery",a11_4="Education",a11_5="Health Care",a11_6="Gas/Coal", a11_7="Electricity", a11_8="Communication", a11_9="Mineral Water", a11_10="Clean Water", a11_11="Cleaning Services", a11_12="House Repair",a11_13="Investment in Farming",a11_14="Other business Invesment",a11_15="Transportation",a11_16="Entertainment",a11_17="Others")
a11_v

a11_v<- a11_v%>%
  filter(!is.na(value), value>=0)





graph_a11_v <- ggplot(data=a11_v, aes(x=value, group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()

graph_a11_v
ggsave("../visuals/PB/graph_a8_no.png", plot = graph_a11_v)

##

graph_a11_v_1 <- ggplot(data=a11_v%>%
                          filter(variable %in% c("Total")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()

ggsave("../visuals/PB/graph_a11_v_1.png", plot = graph_a11_v_1)

graph_a11_v_1_box <- ggplot(data=a11_v%>%
                          filter(variable %in% c("Total")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()

graph_a11_v_1_box
ggsave("../visuals/PB/graph_a11_v_1_box.png", plot = graph_a11_v_1_box)



## combine  2,3,4
graph_a11_v_234 <- ggplot(data=a11_v%>%
                          filter(variable %in% c("House Rental","Food/Groccery","Education")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_234

ggsave("../visuals/PB/graph_a11_v_234.png", plot = graph_a11_v_234)

graph_a11_v_234_box <- ggplot(data=a11_v%>%
                            filter(variable %in% c("House Rental","Food/Groccery","Education")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_234_box

ggsave("../visuals/PB/graph_a11_v_234_box.png", plot = graph_a11_v_234_box)


## combine  5
graph_a11_v_5 <- ggplot(data=a11_v%>%
                            filter(variable %in% c("Health Care")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_5

ggsave("../visuals/PB/graph_a11_v_5.png", plot = graph_a11_v_5)
graph_a11_v_5_box <- ggplot(data=a11_v%>%
                          filter(variable %in% c("Health Care")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_5_box

ggsave("../visuals/PB/graph_a11_v_5_box.png", plot = graph_a11_v_5_box)

## combine  6,7
graph_a11_v_67 <- ggplot(data=a11_v%>%
                            filter(variable %in% c("Gas/Coal","Electricity")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_67

ggsave("../visuals/PB/graph_a11_v_67.png", plot = graph_a11_v_67)

graph_a11_v_67_box <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Gas/Coal","Electricity")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_67_box

ggsave("../visuals/PB/graph_a11_v_67_box.png", plot = graph_a11_v_67_box)


## combine  8
graph_a11_v_8 <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Communication")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_8

ggsave("../visuals/PB/graph_a11_v_8.png", plot = graph_a11_v_8)

graph_a11_v_8_box <- ggplot(data=a11_v%>%
                          filter(variable %in% c("Communication")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_8_box

ggsave("../visuals/PB/graph_a11_v_8_box.png", plot = graph_a11_v_8_box)

## combine  9,10
graph_a11_v_910 <- ggplot(data=a11_v%>%
                          filter(variable %in% c("Mineral Water","Clean Water")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_910

ggsave("../visuals/PB/graph_a11_v_910.png", plot = graph_a11_v_910)

graph_a11_v_910_box <- ggplot(data=a11_v%>%
                            filter(variable %in% c("Mineral Water","Clean Water")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_910_box

ggsave("../visuals/PB/graph_a11_v_910_box.png", plot = graph_a11_v_910_box)

## combine  11
graph_a11_v_11 <- ggplot(data=a11_v%>%
                            filter(variable %in% c("Cleaning Services")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_11

ggsave("../visuals/PB/graph_a11_v_11.png", plot = graph_a11_v_11)

graph_a11_v_11_box <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Cleaning Services")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_11_box

ggsave("../visuals/PB/graph_a11_v_11_box.png", plot = graph_a11_v_11_box)


## combine  12
graph_a11_v_12 <- ggplot(data=a11_v%>%
                             filter(variable %in% c("House Repair")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_12

ggsave("../visuals/PB/graph_a11_v_12.png", plot = graph_a11_v_12)

graph_a11_v_12_box <- ggplot(data=a11_v%>%
                           filter(variable %in% c("House Repair")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_12_box

ggsave("../visuals/PB/graph_a11_v_12_box.png", plot = graph_a11_v_12_box)



## combine  13
graph_a11_v_13 <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Investment in Farming")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_13

ggsave("../visuals/PB/graph_a11_v_13.png", plot = graph_a11_v_13)


graph_a11_v_13_box <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Investment in Farming")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_13_box

ggsave("../visuals/PB/graph_a11_v_13_box.png", plot = graph_a11_v_13_box)



## combine  14
graph_a11_v_14 <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Other business Invesment")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_14

ggsave("../visuals/PB/graph_a11_v_14.png", plot = graph_a11_v_14)


graph_a11_v_14_box <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Other business Invesment")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_14_box

ggsave("../visuals/PB/graph_a11_v_14_box.png", plot = graph_a11_v_14_box)


## combine  15 16
graph_a11_v_1516 <- ggplot(data=a11_v%>%
                           filter(variable %in% c("Transportation","Entertainment")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_1516

graph_a11_v_1516_box <- ggplot(data=a11_v%>%
                             filter(variable %in% c("Transportation","Entertainment")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Expense",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a11_v_1516_box

ggsave("../visuals/PB/graph_a11_v_1516_box.png", plot = graph_a11_v_1516_box)


### 

##########################################
#### Income - A12


##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################


a12 <- df2%>%
  select(q1,a12_1,a12_2,a12_3,a12_4,a12_5,a12_6,a12_7,a12_8,a12_9)

a12 <- melt(a12,id=c("q1"),na.rm = TRUE, value.name = "value")

glimpse(a12)


a12$variable<-recode(a12$variable, a12_1="Total", a12_2="Farming",a12_3="Fixed wage",a12_4="Pension",a12_5="Self-business",a12_6="Saving", a12_7="Gifted by relatives", a12_8="Off-season part-time", a12_9="Others")

a12

graph_a12_box <- ggplot(data=a12%>%
                                 filter(variable %in% c("Total","Farming","Fixed wage","Pension","Self-business","Saving","Gifted by relatives","Off-season part-time","Others")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Income",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a12_box

ggsave("../visuals/PB/graph_a12_box.png", plot = graph_a12_box)



# combine 
graph_a12_v_box <- ggplot(data=a12%>%
                          filter(variable %in% c("Farming","Fixed wage","Pension","Saving","Gifted by relatives","Off-season part-time","Others")), aes(x=value, group=variable, fill=variable)) +
  geom_boxplot() +  
  labs(
    title = "Distribution of Income",
    x="Million VND"
  )+
  theme_ipsum()
graph_a12_v_box

ggsave("../visuals/PB/graph_a12_box.png", plot = graph_a12_box)



graph_a12 <- ggplot(data=a12%>%
                           filter(variable %in% c("Total")), aes(x=value, group=variable, fill=variable)) +
  geom_density(alpha=.4) +  
  labs(
    title = "Distribution of Income",
    x="Million VND",
    y = "Density"
  )+
  theme_ipsum()
graph_a12

ggsave("../visuals/PB/graph_a12.png", plot = graph_a12)


#### Benchmarking Neighbor - Economics
summary(df2$a13_1_1)


a13_1_1<-df2%>%
  filter(!is.na(a13_1_1))%>%
  count(a13_1_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
a13_1_1


graph_a13_1_1<-ggplot(a13_1_1, aes(x = reorder(a13_1_1,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Self-Ranking")+
  labs(
    x="Self-Ranking - Economic",
    y = "Number"
  )
graph_a13_1_1
ggsave("../visuals/PB/graph_a13_1_1.png", plot = graph_a13_1_1)


summary(df2$a13_1_1)


a13_1_2<-df2%>%
  filter(!is.na(a13_1_2))%>%
  count(a13_1_2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
a13_1_2


graph_a13_1_2<-ggplot(a13_1_2, aes(x = reorder(a13_1_2,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Investigator-Ranking")+
  labs(
    x="Investigator-Ranking - Economic",
    y = "Number"
  )
graph_a13_1_2
ggsave("../visuals/PB/graph_a13_1_2.png", plot = graph_a13_1_2)




a13_2_1<-df2%>%
  filter(!is.na(a13_2_1))%>%
  count(a13_2_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_a13_2_1<-ggplot(a13_2_1,aes(x=reorder(a13_2_1,-n),y=n, fill = factor(a13_2_1))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    x="Ability to pay",
    y = "Number")

graph_a13_2_1

ggsave("../visuals/PB/graph_a13_2_1.png", plot = graph_a13_2_1)




##########################################
#### Existing Water Sources


##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################


b1 <- df2%>%
  select(q1,q5,b1_1,b1_2,b1_3,b1_4,b1_5,b1_6,b1_7,b1_8)

b1 <- melt(b1,id=c("q1","q5"),na.rm = TRUE, value.name = "value")

b1$variable<-recode(b1$variable, b1_1="Bottled Water",b1_2="Tap Water",b1_3="Rain Water",b1_4="Well Water",b1_5="Dug Well Water",b1_6="Surface Water",b1_7="Bulk tank water",b1_8="Others")

b1_a<-b1%>%
  count(value,variable)%>%
  filter(value=="Yes")%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))


graph_b1_a<-ggplot(b1_a, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
 # scale_fill_discrete(name = "Gender")+
  labs(
    title = "Existing Water Source - Number",
    x="Exising Water Sources",
    y = "Number - Percentage"
  )+
  coord_flip()
graph_b1_a
ggsave("../visuals/PB/graph_b1_a.png", plot = graph_b1_a)


b1_b<-b1%>%
  count(value,variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_b1_b<-ggplot(b1_b, aes(x = q5, y = n, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ variable)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  scale_fill_discrete(name = "Water Sources")+
  labs(
    x="Town",
    y = "Number"
  )+
  coord_flip()

graph_b1_b
ggsave("../visuals/PB/graph_b1_b.png", plot = graph_b1_b)

### checking if they are using the filters.
## Well Water

b1_4 <-df2%>%
  select(q1,q5,b1_4,b1_4_ex)%>%
  filter(b1_4=="Yes")%>%
  count(b1_4_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b1_4


graph_b1_4<-ggplot(b1_4, aes(x = reorder(b1_4_ex,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
 # scale_fill_discrete(name = "Gender")+
  labs(
    x="Well Water Using Filters",
    y = "Number"
  )
graph_b1_4
ggsave("../visuals/PB/graph_b1_4.png", plot = graph_b1_4)

## Tap Water

b1_2 <-df2%>%
  select(q1,q5,b1_2,b1_2_ex)%>%
  filter(b1_2=="Yes")%>%
  count(b1_2_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b1_2


graph_b1_2<-ggplot(b1_2, aes(x = reorder(b1_2_ex,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  # scale_fill_discrete(name = "Gender")+
  labs(
    x="Tap Water Using Filters",
    y = "Number"
  )
graph_b1_2
ggsave("../visuals/PB/graph_b1_2.png", plot = graph_b1_2)

## Dug Well Water

b1_5 <-df2%>%
  select(q1,q5,b1_5,b1_5_ex)%>%
  filter(b1_5=="Yes")%>%
  count(b1_5_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b1_5


graph_b1_5<-ggplot(b1_5, aes(x = reorder(b1_5_ex,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  # scale_fill_discrete(name = "Gender")+
  labs(
    x="Dug Well Water Using Filters",
    y = "Number"
  )
graph_b1_5
ggsave("../visuals/PB/graph_b1_5.png", plot = graph_b1_5)


### Purpose of using existing Water sources

##Bottled Water

b2_1<-df2%>%
  select(q5,b2_1.1,b2_1.2)
b2_1 <- melt(b2_1,id=c("q5"),na.rm = TRUE)

b2_1$variable<-recode(b2_1$variable,b2_1.1="Cooking",b2_1.2="Drinking")
b2_1
b2_1 <-b2_1%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

b2_1

graph_b2_1<-ggplot(b2_1, aes(x = reorder(variable,-n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1.5, hjust =2) +
 # geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
   scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Bottled Water",
    y = "Number/Percentage"
  )
graph_b2_1
ggsave("../visuals/PB/graph_b2_1.png", plot = graph_b2_1)


##Tap Water

b2_2<-df2%>%
  select(q5,b2_2.1,b2_2.2,b2_2.3,b2_2.4)
b2_2 <- melt(b2_2,id=c("q5"),na.rm = TRUE)

b2_2$variable<-recode(b2_2$variable,b2_2.1="Cooking",b2_2.2="Drinking",b2_2.3="Bathing",b2_2.4="Washing")
b2_2
b2_2 <-b2_2%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_2

graph_b2_2<-ggplot(b2_2, aes(x = reorder(variable,-n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1.5, hjust =2) +
#  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Tap Water",
    y = "Number/Percentage"
  )
graph_b2_2
ggsave("../visuals/PB/graph_b2_2.png", plot = graph_b2_2)



##Well Water

b2_4<-df2%>%
  select(q5,b2_4.1,b2_4.2,b2_4.3,b2_4.4)
b2_4 <- melt(b2_4,id=c("q5"),na.rm = TRUE)

b2_4$variable<-recode(b2_4$variable,b2_4.1="Cooking",b2_4.2="Drinking",b2_4.3="Bathing",b2_4.4="Washing")
b2_4
b2_4a <-b2_4%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

b2_4a

graph_b2_4a<-ggplot(b2_4a, aes(x = reorder(variable,-n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
#  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
#  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Well Water",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_b2_4a
ggsave("../visuals/PB/graph_b2_4a.png", plot = graph_b2_4a)


b2_4b <-b2_4%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_4b

graph_b2_4b<-ggplot(b2_4b, aes(x = reorder(variable,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
    geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Well Water",
    y = "Number/Percentage"
  )
graph_b2_4b
ggsave("../visuals/PB/graph_b2_4b.png", plot = graph_b2_4b)



b2_4c <-b2_4%>%
  filter(!is.na(value),value>0) %>%
  count(q5)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_4c


graph_b2_4c<-ggplot(b2_4c, aes(x = reorder(q5,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Well Water",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b2_4c
ggsave("../visuals/PB/graph_b2_4c.png", plot = graph_b2_4c)



### Dug Well Water

b2_5<-df2%>%
  select(q5,b2_5.1,b2_5.2,b2_5.3,b2_5.4)
b2_5 <- melt(b2_5,id=c("q5"),na.rm = TRUE)

b2_5$variable<-recode(b2_5$variable,b2_5.1="Cooking",b2_5.2="Drinking",b2_5.3="Bathing",b2_5.4="Washing")
b2_5
b2_5a <-b2_5%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_5a

graph_b2_5a<-ggplot(b2_5a, aes(x = reorder(variable,-n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Dug Well Water",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_b2_5a
ggsave("../visuals/PB/graph_b2_5a.png", plot = graph_b2_5a)


b2_5b <-b2_5%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_5b

graph_b2_5b<-ggplot(b2_5b, aes(x = reorder(variable,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Dug Well Water",
    y = "Number/Percentage"
  )
graph_b2_5b
ggsave("../visuals/PB/graph_b2_5b.png", plot = graph_b2_5b)



b2_5c <-b2_5%>%
  filter(!is.na(value),value>0) %>%
  count(q5)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b2_5c


graph_b2_5c<-ggplot(b2_5c, aes(x = reorder(q5,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = 1, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Purpose of Using - Dug Well Water",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b2_5c
ggsave("../visuals/PB/graph_b2_5c.png", plot = graph_b2_5c)


##How much (in Cubic use)
###
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################


#df2$b3 <- as.numeric(df2$b3) # concerning to integer value

#box plot
graph_b3_box<-df2 %>%
  ggplot(aes(b3))+
  geom_boxplot()  +  
  labs(
    x="Monthly Consumption (m3)")
graph_b3_box
ggsave("../visuals/PB/graph_b3_box.png", plot = graph_b3_box)

# density
graph_b3_den<-df2 %>%
  ggplot(aes(b3))+
  geom_density()  +  
  labs(
    x="Monthly consumption (m3")
graph_b3_den
ggsave("../visuals/PB/graph_b3_den.png", plot = graph_b3_den)


### consumption in combination with others variable - town.


graph_b3_q5<-ggplot(df2, aes(x=q5,y=b3))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Consumption (m3) vs Town",
    x="Town",
    y="m3")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()
graph_b3_q5
ggsave("../visuals/PB/graph_b3_q5.png", plot = graph_b3_q5)



### is that volumn enought

b4<-df2%>%
  filter(!is.na(b4))%>%
  count(b4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b4
graph_b4<-ggplot(b4,aes(x=reorder(b4,-n),y=n, fill = factor(b4))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    x="Enought or Not Enough",
    y = "Number")
graph_b4

ggsave("../visuals/PB/graph_b4.png", plot = graph_b4)

### which type of storage does your family has

summary(df2$b6)
b6<-df2%>%
  filter(!is.na(b6))%>%
  count(b6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b6

graph_b6<-ggplot(b6, aes(x = reorder(b6,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of Storage Facility",
    x="Type of storage facilities",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_b6

ggsave("../visuals/PB/graph_b6.png", plot = graph_b6)

### estimate future of water tank

graph_b6_1_box<-df2 %>%
  ggplot(aes(b6_1))+
  geom_boxplot()  +  
  labs(
    x="Estimated Tank Volume (m3)")
graph_b6_1_box
ggsave("../visuals/PB/graph_b6_1_box.png", plot = graph_b6_1_box)


b6_1<-df2%>%
  filter(!is.na(b6_1))%>%
  count(b6_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_b6_1<-ggplot(b6_1, aes(x = reorder(b6_1,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of Storage Facility",
    x="volume of water tank (m3)",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_b6_1
ggsave("../visuals/PB/graph_b6_1.png", plot = graph_b6_1)



#### Is there any risk/harm to the existing water source

b8<-df2%>%
  filter(!is.na(b8))%>%
  count(b8)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_b8<-ggplot(b8, aes(x = reorder(b8,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
#  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of Risk to Water Source",
    x="Risks of Existing Water Sources",
    y = "Number/Percentage"
  )#+
#  coord_flip()
graph_b8
ggsave("../visuals/PB/graph_b8.png", plot = graph_b8)


b8_ex<-df2%>%
  filter(!is.na(b8_ex))%>%
  count(b8_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_b8_ex<-ggplot(b8_ex, aes(x = reorder(b8_ex,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  #  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    title = "Number - Percentage of Risk to Water Source",
    x="Risk",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b8_ex
ggsave("../visuals/PB/graph_b8_ex.png", plot = graph_b8_ex)


#### Investment in water facilities in the last 10 years

summary(df2$b9)
b9<-df2%>%
  filter(!is.na(b9))%>%
  count(b9)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b9
graph_b9<-ggplot(b9, aes(x = reorder(b9,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13))+
  labs(
    title = "Investment on Water Facility over the last 10 years",
    x="In VND",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b9
ggsave("../visuals/PB/graph_b9.png", plot = graph_b9)


#### Water Availability


b10<-df2%>%
  select(q5,b10_1,b10_2,b10_3,b10_4,b10_5,b10_6,b10_7,b10_8)
b10 <- melt(b10,id=c("q5"),na.rm = TRUE)

b10$variable<-recode(b10$variable,b10_1="Bottled Water",b10_2="Tap Water",b10_3="Rain Water",b10_4="Well Water",b10_5="Dug Well Water", b10_6="Surface Water", b10_7="Bulk Water-Truck",b10_8="Others")
b10

b10a <-b10%>%
  filter(!is.na(value),value>0, value=="Yes") %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

b10a

graph_b10a<-ggplot(b10a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Availability",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b10a
ggsave("../visuals/PB/graph_b10a.png", plot = graph_b10a)



b10b <-b10%>%
  filter(!is.na(value),value>0, value=="Yes") %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b10b

graph_b10b<-ggplot(b10b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
    geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
    geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Availability",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_b10b
ggsave("../visuals/PB/graph_b10b.png", plot = graph_b10b)


##Time of the day when water is available
summary(df2$b11_1)
b11_1<-df2%>%
  filter(!is.na(b11_1))%>%
  count(b11_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b11_1
graph_b11_1<-ggplot(b11_1, aes(x = reorder(b11_1,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    title = "Time of the day when water is available",
    x="Time Window",
    y = "Number/Percentage"
  )
graph_b11_1
ggsave("../visuals/PB/graph_b11_1.png", plot = graph_b11_1)

##Duration of daily water service
summary(df2$b12_1)
b12_1<-df2%>%
  filter(!is.na(b12_1))%>%
  count(b12_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
b12_1
graph_b12_1<-ggplot(b12_1, aes(x = reorder(b12_1,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    title = "Duration of daily water service",
    x="Time Window",
    y = "Number/Percentage"
  )
graph_b12_1
ggsave("../visuals/PB/graph_b12_1.png", plot = graph_b12_1)


### B13 Availabiity of Water Source

b13<-df2%>%
  select(q5,b13_1,b13_2,b13_3,b13_4,b13_5,b13_6,b13_7,b13_8)
b13 <- melt(b13,id=c("q5"),na.rm = TRUE)

b13$variable<-recode(b13$variable,b13_1="Bottled Water",b13_2="Tap Water",b13_3="Rain Water",b13_4="Well Water",b13_5="Dug Well Water", b13_6="Surface Water", b13_7="Bulk Water-Truck",b13_8="Others")
b13

b13a <-b13%>%
  filter(!is.na(value),value>0, value=="Yes") %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

b13a

graph_b13a<-ggplot(b13a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Annual Availability",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_b13a
ggsave("../visuals/PB/graph_b13a.png", plot = graph_b13a)



b13b <-b13%>%
  filter(!is.na(value),value>0, value=="Yes") %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

b13b

graph_b13b<-ggplot(b13b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Annual Availability",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_b13b
ggsave("../visuals/PB/graph_b13b.png", plot = graph_b13b)


#####
#Do you know where the domestic wastewater of households in the community is drained?
#############################
#############################
#############################
#############################
#############################
c1<-df2%>%
  filter(!is.na(c1))%>%
  count(c1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
c1
graph_c1<-ggplot(c1,aes(x=reorder(c1,-n),y=n, fill = factor(c1))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Do you know where wastewater go?",
    x="Yes or No",
    y = "Number")
graph_c1

ggsave("../visuals/PB/graph_c1.png", plot = graph_c1)


##Where is the wastewater of households in your group/family area being drained? 

summary(df2$c2)
c2<-df2%>%
  filter(!is.na(c2))%>%
  count(c2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
c2
graph_c2<-ggplot(c2, aes(x = reorder(c2,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(
    title = "Where wastewater going to?",
    x="Wastewater discharging to",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_c2
ggsave("../visuals/PB/graph_c2.png", plot = graph_c2)

#####
# Does your family has its own toilet

d1<-df2%>%
  filter(!is.na(d1))%>%
  count(d1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d1
graph_d1<-ggplot(d1,aes(x=reorder(d1,-n),y=n, fill = factor(d1))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Does your family has its own toilet?",
    x="Yes or No",
    y = "Number")
graph_d1

ggsave("../visuals/PB/graph_d1.png", plot = graph_d1)


###
summary(df2$d3)
d3<-df2%>%
  filter(!is.na(d3))%>%
  count(d3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d3
graph_d3<-ggplot(d3, aes(x = reorder(d3,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(
    title = "Types of toilet?",
    x="Types of Toilet",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_d3
ggsave("../visuals/PB/graph_d3.png", plot = graph_d3)

#how old is your toilet
summary(df2$d4)
d4<-df2%>%
  filter(!is.na(d4))%>%
  count(d4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d4
graph_d4<-ggplot(d4, aes(x = reorder(d4,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =1) +
  geom_text(aes(label = paste0(perc, " %")), vjust = 1,hjust=0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(
    title = "How old is your toilet?",
    x="Toilet Age",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_d4
ggsave("../visuals/PB/graph_d4.png", plot = graph_d4)


### 

d5<-df2%>%
  filter(!is.na(d5))%>%
  count(d5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d5
graph_d5<-ggplot(d5,aes(x=reorder(d5,-n),y=n, fill = factor(d5))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Compost used for farming?",
    x="Yes or No",
    y = "Number")
graph_d5

ggsave("../visuals/PB/graph_d5.png", plot = graph_d5)

#If Yes, how long do you usually compost before bringing it out to fertilize? 


d6<-df2%>%
  filter(!is.na(d6))%>%
  count(d6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d6
graph_d6<-ggplot(d6,aes(x=reorder(d6,-n),y=n, fill = factor(d6))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 1)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Number of days - compost?",
    x="Numbers of days",
    y = "Number")
graph_d6

ggsave("../visuals/PB/graph_d6.png", plot = graph_d6)


##
d7<-df2%>%
  filter(!is.na(d7))%>%
  count(d7)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
d7
graph_d7<-ggplot(d7,aes(x=reorder(d7,-n),y=n, fill = factor(d7))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Using un-composted fertilizer?",
    x="Frequency",
    y = "Number")
graph_d7

ggsave("../visuals/PB/graph_d7.png", plot = graph_d7)


########################################
#How is your household's household garbage collected and treated? 
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################



e1<-df2%>%
  select(q5,e1_1,e1_2,e1_3,e1_4,e1_5)
e1 <- melt(e1,id=c("q5"),na.rm = TRUE)

e1$variable<-recode(e1$variable,e1_1="Burn/bury in your garden",e1_2="Brought to public landfill",e1_3="Pour into barns",e14_4="Field, bush, crown, pond, canal",e1_5="The cleaning team")
e1


e1a <-e1%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e1a



graph_e1a<-ggplot(e1a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Dispose garbage to",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_e1a
ggsave("../visuals/PB/graph_e1a.png", plot = graph_e1a)


##

e1b <-e1%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

e1b

graph_e1b<-ggplot(e1b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Dispose garbage to",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_e1b
ggsave("../visuals/PB/graph_e1b.png", plot = graph_e1b)


####

#Does your family separate solid waste and general waste before taking it out for dumping/burning/burial? (Choose 1 option)

summary(df2$e2)

e2<-df2%>%
  filter(!is.na(e2))%>%
  count(e2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_e2<-ggplot(e2, aes(x = reorder(e2,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Solid Waste Seperation prior to discharge",
    y = "Number"
  )
graph_e2

ggsave("../visuals/PB/graph_e2.png", plot = graph_e2)

#######################

#Does your family have to pay for garbage collection?

summary(df2$e3_1)

e3_1<-df2%>%
  filter(!is.na(e3_1))%>%
  count(e3_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

graph_e3_1<-ggplot(e3_1, aes(x = reorder(e3_1,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
 # scale_fill_discrete(name = "Gender")+
  labs(
    x="Pay for solid waste collection/disposal",
    y = "Number"
  )
graph_e3_1
ggsave("../visuals/PB/graph_e3_1.png", plot = graph_e3_1)


###############
#How much do you have to pay per month (VND)


graph_e3_2_box<-df2 %>%
  ggplot(aes(e3_2))+
  geom_boxplot()  +  
  labs(
    x="Payment for solid waste disposal (VND)")
graph_e3_2_box
ggsave("../visuals/PB/graph_e3_2_box.png", plot = graph_e3_2_box)

# density
graph_e3_2_den<-df2 %>%
  ggplot(aes(e3_2))+
  geom_density()  +  
  labs(
    x="Payment for solid waste disposal (VND)")
graph_e3_2_den
ggsave("../visuals/PB/graph_e3_2_den.png", plot = graph_e3_2_den)


###

e3_2<-df2%>%
  filter(!is.na(e3_2))%>%
  count(e3_2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e3_2
graph_e3_2<-ggplot(e3_2, aes(x = reorder(e3_2,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Payment for solid waste disposal (VND)",
    y = "Number"
  )
graph_e3_2

ggsave("../visuals/PB/graph_e3_2.png", plot = graph_e3_2)

#######################
#####################
e4<-df2%>%
  filter(!is.na(e4))%>%
  count(e4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e4
graph_e4<-ggplot(e4, aes(x = reorder(e4,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Frequency of Waste Collection",
    y = "Number"
  )
graph_e4

ggsave("../visuals/PB/graph_e4.png", plot = graph_e4)


######
#Approximately how far is the distance from your family to the garbage collection point ________km?

e5<-df2%>%
  filter(!is.na(e5))%>%
  count(e5)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e5
graph_e5<-ggplot(e5, aes(x = reorder(e5,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Distance to disposal site (km)",
    y = "Number"
  )+
  coord_flip()
graph_e5

ggsave("../visuals/PB/graph_e5.png", plot = graph_e5)

df2$e5<-as.numeric(df2$e5)
graph_e5_box<-df2 %>%
  ggplot(aes(e5))+
  geom_boxplot()  +  
  labs(
    x="Payment for solid waste disposal (VND)")
graph_e5_box
ggsave("../visuals/PB/graph_e5_box.png", plot = graph_e5_box)

#In your opinion, does the garbage collection site meet hygienic standards in the community?

e6<-df2%>%
  filter(!is.na(e6))%>%
  count(e6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e6
graph_e6<-ggplot(e6, aes(x = reorder(e6,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(
    x="Hygienic of Garbage Collection Site",
    y = "Number"
  )+
  coord_flip()
graph_e6

ggsave("../visuals/PB/graph_e6.png", plot = graph_e6)


######
#In your opinion, are there any outstanding environmental problems in the community?
e7<-df2%>%
  filter(!is.na(e7))%>%
  count(e7)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e7
graph_e7<-ggplot(e7, aes(x = reorder(e7,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Outstanding environmental problems?",
    y = "Number"
  )
graph_e7

ggsave("../visuals/PB/graph_e7.png", plot = graph_e7)

###If yes, what are the most prominent environmental issues in your opinion? (select up to 5 options only)




e8<-df2%>%
  select(q5,e8_1,e8_2,e8_3,e8_4,e8_5,e8_6,e8_7,e8_8,e8_9,e8_10,e8_11)
e8 <- melt(e8,id=c("q5"),na.rm = TRUE)

e8$variable<-recode(e8$variable,e8_1="Flooding in the rainy season",e8_2="Garbage is not collected and thrown indiscriminately",e8_3="Don't provide clean water",e8_4="Pollution of domestic water from underground wells/river water",e8_5="There is no proper, hygienic latrine",e8_6="Pollution from wastewater",e8_7="Lack of drainage system",e8_8="Air pollution /smog",e8_9="Noise pollution",e8_10="Vegetables contaminated with drugs",e8_11="Others")
e8


e8a <-e8%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$d1),2),nsmall=2))

e8a



graph_e8a<-ggplot(e8a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Prominent Environmental Issues",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_e8a
ggsave("../visuals/PB/graph_e8a.png", plot = graph_e8a)


##

e8b <-e8%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

e8b

graph_e8b<-ggplot(e8b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Prominent Environmental Issues",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_e8b
ggsave("../visuals/PB/graph_e8b.png", plot = graph_e8b)


######################


#In your opinion, is the water source used by the household for eating/drinking contaminated/poisonous?

e7<-df2%>%
  filter(!is.na(e7))%>%
  count(e7)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

e7
graph_e7<-ggplot(e7, aes(x = reorder(e7,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Outstanding environmental problems?",
    y = "Number"
  )
graph_e7

ggsave("../visuals/PB/graph_e7.png", plot = graph_e7)


###

f3<-df2%>%
  filter(!is.na(f3))%>%
  count(f3)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))

f3
graph_f3<-ggplot(f3, aes(x = reorder(f3,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
#  scale_fill_discrete(name = "Gender")+
  labs(
    x="Eating/drinking is contaminated/poisonous",
    y = "Number"
  )
graph_f3

ggsave("../visuals/PB/graph_f3.png", plot = graph_f3)

#In your opinion, what kind of water is called good/clean/safe water, please name those standards? 
#####
######################



f4<-df2%>%
  select(q5,f4.2,f4.3,f4.4,f4.5,f4.6)
f4 <- melt(f4,id=c("q5"),na.rm = TRUE)



f4$variable<-recode(f4$variable,f4.2="Clear and colorless",f4.3="Odorless",f4.4="No unusual taste",f4.5="No toxins and pathogenic bacteria",f4.6="Has been inspected by MoH/Stage agency")
f4


f4a <-f4%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$d1),2),nsmall=2))

f4a



graph_f4a<-ggplot(f4a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Knowledge on water quality",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_f4a
ggsave("../visuals/PB/graph_f4a.png", plot = graph_f4a)


##

f4b <-f4%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

f4b

graph_f4b<-ggplot(f4b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Knowledge on water quality",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_f4b
ggsave("../visuals/PB/graph_f4b.png", plot = graph_f4b)



####################


f5<-df2%>%
  select(q5,f5.1,f5.2,f5.3,f5.4,f5.5,f5.6,f5.7,f5.8,f5.9,f5.10)
f5 <- melt(f5,id=c("q5"),na.rm = TRUE)



f5$variable<-recode(f5$variable,f5.1="Do not know",f5.2="Health protection",f5.3="Convenient",f5.4="Quality of water can be guaranteed without harmful contaminants",f5.5="Clean environment",f5.6="Reduce cost for water use",f5.7="Better living conditions",f5.8="Sufficient amount of water can be delivered continuously",f5.9="Have a business/service opportunity",f5.10="No water borne disease")
f5


f5a <-f5%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$d1),2),nsmall=2))

f5a



graph_f5a<-ggplot(f5a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Knowledge on Benefits of WTP",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_f5a
ggsave("../visuals/PB/graph_f5a.png", plot = graph_f5a)


##

f5b <-f5%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

f5b

graph_f5b<-ggplot(f5b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 2, hjust =0, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Knowledge on Benefits of WTP",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_f5b
ggsave("../visuals/PB/graph_f5b.png", plot = graph_f5b)


##################



f1<-df2%>%
  select(q5,f1_1,f1_2,f1_3,f1_4,f1_5,f1_6,f1_7,f1_8,f1_9,f1_10,f1_11,f1_12,f1_13,f1_14,f1_15,f1_16,f1_17)
f1 <- melt(f1,id=c("q5"),na.rm = TRUE)



f1$variable<-recode(f1$variable,f1_1="Flu",f1_2="Headache",f1_3="Diarrhea",f1_4="Dysentery",f1_5="Malaria",f1_6="Dengue",f1_7="Cholera",f1_8="Eye diseases",f1_9="Respiratory diseases",f1_10="Gynecological",f1_11="Helminths",f1_12="Hepatitis A",f1_13="Dermatology",f1_14="Stomach diseases",f1_15="Cancer",f1_16="Food poisoning",f1_17="Amoebiasis")
f1


f1a <-f1%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$d1),2),nsmall=2))

f1a



graph_f1a<-ggplot(f1a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Experiencing diseases in the Community",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_f1a
ggsave("../visuals/PB/graph_f1a.png", plot = graph_f1a)


##

f1b <-f1%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

f1b

graph_f1b<-ggplot(f1b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "red",position = position_dodge(1.5),vjust = 0.5, hjust =3, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Experiencing diseases in the Community",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_f1b
ggsave("../visuals/PB/graph_f1b.png", plot = graph_f1b)


#####################



##In the past month, has anyone in your family suffered from the diseases mentioned above? 


f2<-df2%>%
  select(q5,f2.1,f2.2,f2.3,f2.4,f2.5,f2.6,f2.8,f2.9,f2.10,f2.11,f2.12,f2.13,f2.14,f2.18)
f2 <- melt(f2,id=c("q5"),na.rm = TRUE)



f2$variable<-recode(f2$variable,f2.1="Flu",f2.2="Headache",f2.3="Diarrhea",f2.4="Dysentery",f2.5="Malaria",f2.6="Dengue",f2.7="Cholera",f2.8="Eye diseases",f2.9="Respiratory diseases",f2.10="Gynecological",f2.11="Helminths",f2.12="Hepatitis A",f2.13="Dermatology",f2.14="Stomach diseases",f2.15="Cancer",f2.16="Food poisoning",f2.17="Amoebiasis",f2.18="Others")
f2


f2a <-f2%>%
  filter(!is.na(value),value>0) %>%
  count(variable,q5)%>%
  mutate(perc = format(round(n/length(df2$d1),2),nsmall=2))

f2a



graph_f2a<-ggplot(f2a, aes(x = reorder(variable,n),y=n, label=n, fill = q5)) +
  geom_bar(stat="identity", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(1.5),vjust = 0, hjust =0, angle=90) +
  #  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Experiencing diseases in the last 3 months",
    y = "Number/Percentage"
  )+
  coord_flip()
graph_f2a
ggsave("../visuals/PB/graph_f2a.png", plot = graph_f2a)


##

f2b <-f2%>%
  filter(!is.na(value),value>0) %>%
  count(variable)%>%
  mutate(perc = format(round(n/length(df2$q1)*100,2),nsmall=2))

f2b

graph_f2b<-ggplot(f2b, aes(x = reorder(variable,n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(),fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "red",position = position_dodge(1.5),vjust = 0.5, hjust =3, angle=0) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -0.2)+
  theme(legend.position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  scale_fill_discrete(name = "Town")+
  labs(
    x="Experiencing diseases in the last 3 months",
    y = "Number/Percentage"
  )+
  coord_flip()

graph_f2b
ggsave("../visuals/PB/graph_f2b.png", plot = graph_f2b)

### Willingness to Pay

#Are you willing to connect to the network if the charge is affordable? Or you still prefer to use the existing water source.

g1<-df2%>%
  filter(!is.na(g1))%>%
  count(q5,g1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g1

graph_g1<-ggplot(g1, aes(x = reorder(q5,n),y=n, fill = g1, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Willingness")+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g1
ggsave("../visuals/PB/graph_g1.png", plot = graph_g1)


##


summary(df2$g1)


g1b<-df2%>%
  filter(!is.na(g1))%>%
  count(g1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g1b
graph_g1b<-ggplot(g1b, aes(x = reorder(g1,-n),y=n, label=n)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue3")+
  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -0.5, hjust =2) +
  geom_text(aes(label = paste0(perc, " %")), vjust = -0.5,hjust=0)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Gender")+
  labs(
    x="Willingess to Connect",
    y = "Number - Percentage"
  )
graph_g1b
ggsave("../visuals/PB/graph_g1b.png", plot = graph_g1b)

###For having more reliable water supply and better quality of water, are you willing to pay additional expenses on top of existing expense for water consumption?



g2<-df2%>%
  filter(!is.na(g2))%>%
  count(g2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g2
graph_g2<-ggplot(g2,aes(x=reorder(g2,-n),y=n, fill = factor(g2))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Willingness to pay additional expenses",
    x="Willingness",
    y = "Number - Percentage")
graph_g2

ggsave("../visuals/PB/graph_g2.png", plot = graph_g2)

####

g2a<-df2%>%
  filter(!is.na(g2))%>%
  count(q5,g2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g2a

graph_g2a<-ggplot(g2a, aes(x = reorder(q5,n),y=n, fill = g2, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Willingness")+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g2a
ggsave("../visuals/PB/graph_g2a.png", plot = graph_g2a)


############


g4<-df2%>%
  filter(!is.na(g4))%>%
  count(g4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g4
graph_g4<-ggplot(g4,aes(x=reorder(g4,n),y=n, fill = factor(g4))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
 #   title = "Willingness to pay additional expenses",
    x="Percentage of increase willing to pay",
    y = "Number - Percentage")+
  coord_flip()
graph_g4

ggsave("../visuals/PB/graph_g4.png", plot = graph_g4)

####

g4a<-df2%>%
  filter(!is.na(g4))%>%
  count(q5,g4)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g4a

graph_g4a<-ggplot(g4a, aes(x = reorder(q5,n),y=n, fill = g4, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Willingness")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g4a
ggsave("../visuals/PB/graph_g4a.png", plot = graph_g4a)

###########
#Specifically, what will be the affordable service charge for you to pay for 1 cubic of treated water? Or per month


g5_1<-df2%>%
  filter(!is.na(g5_1))%>%
  count(g5_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_1
graph_g5_1<-ggplot(g5_1,aes(x=reorder(g5_1,n),y=n, fill = factor(g5_1))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    #   title = "Willingness to pay additional expenses",
    x="Affordable amount per m3 (VND)",
    y = "Number - Percentage")+
  coord_flip()
graph_g5_1

ggsave("../visuals/PB/graph_g5_1.png", plot = graph_g5_1)

####

g5_1a<-df2%>%
  filter(!is.na(g5_1))%>%
  count(q5,g5_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_1a

graph_g5_1a<-ggplot(g5_1a, aes(x = reorder(q5,n),y=n, fill = g5_1, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = 0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Affordable amount per m3 (VND)")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g5_1a
ggsave("../visuals/PB/graph_g5_1a.png", plot = graph_g5_1a)

##Value of …... In VND/m3

g5_1_ex<-df2%>%
  filter(!is.na(g5_1_ex))%>%
  count(g5_1_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_1_ex
graph_g5_1_ex<-ggplot(g5_1_ex,aes(x=reorder(g5_1_ex,n),y=n, fill = factor(g5_1_ex))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    #   title = "Willingness to pay additional expenses",
    x="Affordable amount per m3 (thousand VND)",
    y = "Number - Percentage")+
  coord_flip()
graph_g5_1_ex

ggsave("../visuals/PB/graph_g5_1_ex.png", plot = graph_g5_1_ex)

####

g5_1_exa<-df2%>%
  filter(!is.na(g5_1_ex))%>%
  count(q5,g5_1_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_1_exa

g5_1_exa$g5_1_ex<-as.factor(g5_1_exa$g5_1_ex)

graph_g5_1_exa<-ggplot(g5_1_exa, aes(x = reorder(q5,n),y=n, fill = g5_1_ex, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = 0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Thousand VND/m3")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g5_1_exa


ggsave("../visuals/PB/graph_g5_1_exa.png", plot = graph_g5_1_exa)


###Specifically, what will be the affordable service charge for you to pay for 1 cubic of treated water? Or per month

g5_2<-df2%>%
  filter(!is.na(g5_2))%>%
  count(g5_2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_2
graph_g5_2<-ggplot(g5_2,aes(x=reorder(g5_2,n),y=n, fill = factor(g5_2))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(
    #   title = "Willingness to pay additional expenses",
    x="Affordable amount per month (VND)",
    y = "Number - Percentage")+
  coord_flip()
graph_g5_2

ggsave("../visuals/PB/graph_g5_2.png", plot = graph_g5_2)

####

g5_2a<-df2%>%
  filter(!is.na(g5_2))%>%
  count(q5,g5_2)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_2a

graph_g5_2a<-ggplot(g5_2a, aes(x = reorder(q5,n),y=n, fill = g5_2, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = 0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Affordable amount per month (VND)")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g5_2a
ggsave("../visuals/PB/graph_g5_2a.png", plot = graph_g5_2a)


###########
#Value of …..thousands/household/month


g5_2_ex<-df2%>%
  filter(!is.na(g5_2_ex))%>%
  count(g5_2_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))


g5_2_ex$g5_2_ex<-as.factor(g5_2_ex$g5_2_ex)



graph_g5_2_ex<-ggplot(g5_2_ex,aes(x=reorder(g5_2_ex,n),y=n, fill = factor(g5_2_ex))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    #   title = "Willingness to pay additional expenses",
    x="Affordable amount per month (VND)",
    y = "Number - Percentage")+
  coord_flip()
graph_g5_2_ex

ggsave("../visuals/PB/graph_g5_2_ex.png", plot = graph_g5_2_ex)

####

g5_2_exa<-df2%>%
  filter(!is.na(g5_2_ex))%>%
  count(q5,g5_2_ex)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g5_2_exa

g5_2_exa$g5_2_ex<-as.factor(g5_2_exa$g5_2_ex)

graph_g5_2_exa<-ggplot(g5_2_exa, aes(x = reorder(q5,n),y=n, fill = g5_2_ex, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = 0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Thousand VND/m3")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g5_2_exa


ggsave("../visuals/PB/graph_g5_2_exa.png", plot = graph_g5_2_exa)

####

graph_g5_2_ex_box<-df2 %>%
  ggplot(aes(g5_2_ex))+
  geom_boxplot()  +  
  labs(
    x="Thousand VND/month")
graph_g5_2_ex_box
ggsave("../visuals/PB/graph_g5_2_ex_box.png", plot = graph_g5_2_ex_box)

# density
graph_g5_2_ex_den<-df2 %>%
  ggplot(aes(g5_2_ex))+
  geom_density()  +  
  labs(
    x="Thousand VND/month")
graph_g5_2_ex_den
ggsave("../visuals/PB/graph_g5_2_ex_den.png", plot = graph_g5_2_ex_den)


############
#Aside from paying for monthly water fee, are you willing to pay for initial investment cost for connection pipes, pumps, and new storage tank (if required) to the nearest new water distribution pipe, which will be likely located in nearby road or pathway?



g6<-df2%>%
  filter(!is.na(g6))%>%
  count(g6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g6
graph_g6<-ggplot(g6,aes(x=reorder(g6,-n),y=n, fill = factor(g6))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  labs(
    title = "Willingness to pay for connect",
    x="Willingness",
    y = "Number - Percentage")
graph_g6

ggsave("../visuals/PB/graph_g6.png", plot = graph_g6)

####

g6a<-df2%>%
  filter(!is.na(g6))%>%
  count(q5,g6)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g6a

graph_g6a<-ggplot(g6a, aes(x = reorder(q5,n),y=n, fill = g6, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = -1)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "Willingness")+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g6a
ggsave("../visuals/PB/graph_g6a.png", plot = graph_g6a)

##

g7_1<-df2%>%
  filter(!is.na(g7_1))%>%
  count(g7_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g7_1
graph_g7_1<-ggplot(g7_1,aes(x=reorder(g7_1,n),y=n, fill = factor(g7_1))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=1,hjust=1),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    #   title = "Willingness to pay additional expenses",
    x="maximum amount of investment",
    y = "Number - Percentage")+
  coord_flip()
graph_g7_1

ggsave("../visuals/PB/graph_g7_1.png", plot = graph_g7_1)

####

g7_1a<-df2%>%
  filter(!is.na(g7_1))%>%
  count(q5,g7_1)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g7_1a

graph_g7_1a<-ggplot(g7_1a, aes(x = reorder(q5,n),y=n, fill = g7_1, label=n)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  #  geom_text(aes(label = paste0(n)),colour = "black",position = position_dodge(0.9),vjust = -1) +
  geom_text(aes(label = paste0(perc, " %")), position = position_dodge(0.9),vjust = 0.5)+
  theme(legend.position = "right")+
  scale_fill_discrete(name = "maximum amount of investment")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    x="Town",
    y = "Number - Percentage"
  )+
  coord_flip()

graph_g7_1a
ggsave("../visuals/PB/graph_g7_1a.png", plot = graph_g7_1a)


#First investment amount?

graph_g7_2_box<-df2 %>%
  ggplot(aes(g7_2))+
  geom_boxplot()  +  
  labs(
    x="VND")
graph_g7_2_box
ggsave("../visuals/PB/graph_g7_2_box.png", plot = graph_g7_2_box)

# density
graph_g7_2_den<-df2 %>%
  ggplot(aes(g7_2))+
  geom_density()  +  
  labs(
    x="VND")
graph_g7_2_den
ggsave("../visuals/PB/graph_g7_2_den.png", plot = graph_g7_2_den)


#If the WTP is selected to be in your farm/agriculture land, are you willing to collaborate with the developer and local authority for site compensation and clearance?


g8<-df2%>%
  filter(!is.na(g8))%>%
  count(g8)%>%
  mutate(perc = format(round(n/sum(n)*100,2),nsmall=2))
g8
graph_g8<-ggplot(g8,aes(x=reorder(g8,-n),y=n, fill = factor(g8))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label = prettyNum(n, big.mark = ",", scientific = FALSE)), vjust = -0.5)+
  geom_text(aes(label = paste0(perc, " %")), vjust = 2)+
  theme(axis.text.x = element_text(angle=0, vjust=0,hjust=0.5),
        legend.position = "none")+
  labs(
    title = "Site Clearance Collaboration",
    x="Site Clearance Collaboration",
    y = "Number - Percentage")
graph_g8

ggsave("../visuals/PB/graph_g8.png", plot = graph_g8)

























































