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

df1 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "database",skip = 5))
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


##Start data Cleaning process
source("PB-HH-step1_cleaning.R")

## Start data tables and visualization

source("PB-HH-step2_table_visualization.R")