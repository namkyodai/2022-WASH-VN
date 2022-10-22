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

ggsave("../visuals/PB/graph_a2.png", plot = graph_a2)


##Gender distribution

#plot with both value and percentage.

df2%>%
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

ggsave("../visuals/PB/graph_a6.png", plot = graph_a6)


### Total in the family

summary(df2$a7)

graph_a7_box<-df2 %>%
  ggplot( aes(a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    x="Family Members")

ggsave("../visuals/PB/graph_a7_box.png", plot = graph_a7_box)


graph_a7q5<-ggplot(df2, aes(x=q5,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Family Members vs Town",
    x="Town",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()

ggsave("../visuals/PB/graph_a7q5.png", plot = graph_a7q5)


graph_a7a4<-ggplot(df2, aes(x=a4,y=a7))+
  geom_boxplot(fill="cadetblue3", alpha=0.7)+  
  labs(
    title = "Family Members vs Occupation",
    x="Occupation",
    y="Family members")+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="red")+
  coord_flip()


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

b2_1 










### Willingness to Pay

