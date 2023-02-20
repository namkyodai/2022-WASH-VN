#setting the directory to working file
library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path))  

#loading necessary packages/libraries
library(dplyr)
library(ggplot2)
library(lubridate) #for dealing with dates and times
library(tidyverse)
library(janitor) #for cleaning names
library(scales) #extending for ggplot2
library(plotly) #for interactive graphs
library(readxl) #for reading excel file   
library(data.table)
#load Data into R 

df1 <- data.frame(read_excel("../Data/WASH-data-TN-VN-Industry.xlsx", sheet = "database",skip = 6))
df1<-df1 %>%
  remove_empty(c("rows")) #removing empty rows and empty columns it they exist
glimpse(head(df1))

#checking overall missing/NULL values
x1 <- map_df(df1, function(x){sum(is.na(x))}) 
missing <- x1 %>% gather(key = "Variable") %>% filter(value > 0) %>% mutate(value = value/nrow(df1))
missing01<-ggplot(missing, aes(x = reorder(Variable, -value),y = value)) + 
  geom_bar(stat = "identity", fill = "salmon") + 
  coord_flip()

missing01

#checking which variables are with missing value completely
missing %>%
  filter(value >0.96)%>%
  arrange(desc(value))

df2<-df1 %>%
      clean_names() 
glimpse(df2)


# Age distribution
df2$q1_10 <- as.numeric(df2$q1_10) # 

graph_q1_10_box<-df2 %>%
  ggplot(aes(q1_10/1000000))+
  geom_boxplot()  +  
  labs(
    x="Million VND")
graph_q1_10_box

summary(df2$q1_10/1000000)

graph_q1_10_den<-df2 %>%
  ggplot(aes(q1_10/1000000))+
  geom_density()  +  
  labs(
    x="Million VND")
graph_q1_10_den


graph_q1_13<-df2 %>%
  ggplot(aes(q1_13))+
  geom_histogram()  +  
  labs(
    x="Million VND")
graph_q1_13


summary(df2$q2)
summary(df2$q11)

graph_q1_12_box<-df2 %>%
  ggplot(aes(q1_12/1000000))+
  geom_boxplot()  +  
  labs(
    x="Million VND")
graph_q1_12_box

summary(df2$q1_12/1000000)

graph_q1_12_den<-df2 %>%
  ggplot(aes(q1_12/1000000))+
  geom_density()  +  
  labs(
    x="Million VND")
graph_q1_12_den


graph_q12_box<-df2 %>%
  ggplot(aes(q12/1000000))+
  geom_boxplot()  +  
  labs(
    x="Million VND")
graph_q12_box

summary(df2$q12/1000000)

graph_q12_den<-df2 %>%
  ggplot(aes(q12/1000000))+
  geom_density()  +  
  labs(
    x="Million VND")
graph_q12_den
