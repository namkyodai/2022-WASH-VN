#Define types of data for variables/columns
library(purrr)
df2<-df2%>%
  purrr::modify_at(c(4,12:15,19,21,23,25,27,29,31,33,35,37,38,40,42:44,73:75,77:108,110,112,114,116,118,120,122,124,126,128,130,132,133,135,137,139,141,143,145,147,149:155,157,159,161,163,165,167,169,171:182,184:192,194,196,197,198:208,209:226,227:232,234,235,237,239,242), factor)

df2$q8_1 <-as.numeric(df2$q8_1)
df2$q8_2 <-as.numeric(df2$q8_2)

glimpse(head(df2))

# for binary answer
generic <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "generic",skip = 1))
yesno <- setNames(generic$c, generic$a) #Yes or No
suf <- setNames(generic$e, generic$a) #Enough or not enough
#---town------
town <- data.frame(read_excel("../Data/WASH-data-TN-VN-Household.xlsx", sheet = "town",skip = 1))
df2$q5<-recode(df2$q5, !!!setNames(town$c, town$b))
#---gender------
gender <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "gender",skip = 1))
gender<-gender[1:2,]
df2$a3<-recode(df2$a3, !!!setNames(gender$c, gender$a))
df2$a3 <- factor(df2$a3,levels=unlist(gender$c)) #order the list

#------Education
A4 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A4",skip = 1))
df2$a4<-recode(df2$a4, !!!setNames(A4$c, A4$a))
df2$a4 <- factor(df2$a4,levels=unlist(A4$c)) #order the list

## Occupation
A5 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A5",skip = 1))
df2$a5<-recode(df2$a5, !!!setNames(A5$c, A5$a))
df2$a5 <- factor(df2$a5,levels=unlist(A5$c)) #order the list


## Relationship with household
A6 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A6",skip = 1))
df2$a6<-recode(df2$a6, !!!setNames(A6$c, A6$a))
df2$a6 <- factor(df2$a6,levels=unlist(A6$c)) #order the list

#---Owned Assets
df2$a8_1[is.na(df2$a8_1)] = 2
df2$a8_1<-recode(df2$a8_1, !!!yesno)
df2$a8_2[is.na(df2$a8_2)] = 2
df2$a8_2<-recode(df2$a8_2, !!!yesno)
df2$a8_3[is.na(df2$a8_3)] = 2
df2$a8_3<-recode(df2$a8_3, !!!yesno)
df2$a8_4[is.na(df2$a8_4)] = 2
df2$a8_4<-recode(df2$a8_4, !!!yesno)
df2$a8_5[is.na(df2$a8_5)] = 2
df2$a8_5<-recode(df2$a8_5, !!!yesno)
df2$a8_6[is.na(df2$a8_6)] = 2
df2$a8_6<-recode(df2$a8_6, !!!yesno)
df2$a8_7[is.na(df2$a8_7)] = 2
df2$a8_7<-recode(df2$a8_7, !!!yesno)
df2$a8_8[is.na(df2$a8_8)] = 2
df2$a8_8<-recode(df2$a8_8, !!!yesno)
df2$a8_9[is.na(df2$a8_9)] = 2
df2$a8_9<-recode(df2$a8_9, !!!yesno)
df2$a8_10[is.na(df2$a8_10)] = 2
df2$a8_10<-recode(df2$a8_10, !!!yesno)
df2$a8_11[is.na(df2$a8_11)] = 2
df2$a8_11<-recode(df2$a8_11, !!!yesno)
df2$a8_12[is.na(df2$a8_12)] = 2
df2$a8_12<-recode(df2$a8_12, !!!yesno)

## House Types
A9 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A9",skip = 1))
df2$a9<-recode(df2$a9, !!!setNames(A9$c, A9$a))
df2$a9 <- factor(df2$a9,levels=unlist(A9$c)) #order the list

## House Conditions
#A9plus <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A9plus",skip = 1))
#df2$a9plus<-recode(df2$a9plus, !!!setNames(A9plus$c, A9plus$a))
#df2$a9plus <- factor(df2$a9plus,levels=unlist(A9plus$c)) #order the list

## House ownership
A10 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A10",skip = 1))
df2$a10<-recode(df2$a10, !!!setNames(A10$c, A10$a))
df2$a10 <- factor(df2$a10,levels=unlist(A10$c)) #order the list


i <-c(45:72)
df2[ , i] <- apply(df2[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

glimpse(df2)

## Owned Rank of Economic compared to neighbors
A13 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "A13",skip = 1))
df2$a13_1_1<-recode(df2$a13_1_1, !!!setNames(A13$c, A13$a))
df2$a13_1_1 <- factor(df2$a13_1_1,levels=unlist(A13$c)) #order the list

df2$a13_1_2<-recode(df2$a13_1_2, !!!setNames(A13$c, A13$a))
df2$a13_1_2 <- factor(df2$a13_1_2,levels=unlist(A13$c)) #order the list

# Does your family live affordable with the current income?
df2$a13_2_1<-recode(df2$a13_2_1, !!!yesno)

df2$b1_1[is.na(df2$b1_1)] = 2
df2$b1_1<-recode(df2$b1_1, !!!yesno)
df2$b1_1_ex[is.na(df2$b1_1_ex)] = 2
df2$b1_1_ex<-recode(df2$b1_1_ex, !!!yesno)
df2$b1_2[is.na(df2$b1_2)] = 2
df2$b1_2<-recode(df2$b1_2, !!!yesno)
df2$b1_2_ex[is.na(df2$b1_2_ex)] = 2
df2$b1_2_ex<-recode(df2$b1_2_ex, !!!yesno)
df2$b1_3[is.na(df2$b1_3)] = 2
df2$b1_3<-recode(df2$b1_3, !!!yesno)
df2$b1_3_ex[is.na(df2$b1_3_ex)] = 2
df2$b1_3_ex<-recode(df2$b1_3_ex, !!!yesno)
df2$b1_4[is.na(df2$b1_4)] = 2
df2$b1_4<-recode(df2$b1_4, !!!yesno)
df2$b1_4_ex[is.na(df2$b1_4_ex)] = 2
df2$b1_4_ex<-recode(df2$b1_4_ex, !!!yesno)
df2$b1_5[is.na(df2$b1_5)] = 2
df2$b1_5<-recode(df2$b1_5, !!!yesno)
df2$b1_5_ex[is.na(df2$b1_5_ex)] = 2
df2$b1_5_ex<-recode(df2$b1_5_ex, !!!yesno)
df2$b1_6[is.na(df2$b1_6)] = 2
df2$b1_6<-recode(df2$b1_6, !!!yesno)
df2$b1_6_ex[is.na(df2$b1_6_ex)] = 2
df2$b1_6_ex<-recode(df2$b1_6_ex, !!!yesno)
df2$b1_7[is.na(df2$b1_7)] = 2
df2$b1_7<-recode(df2$b1_7, !!!yesno)
df2$b1_7_ex[is.na(df2$b1_7_ex)] = 2
df2$b1_7_ex<-recode(df2$b1_7_ex, !!!yesno)
df2$b1_8[is.na(df2$b1_8)] = 2
df2$b1_8<-recode(df2$b1_8, !!!yesno)
df2$b1_8_ex[is.na(df2$b1_8_ex)] = 2
df2$b1_8_ex<-recode(df2$b1_8_ex, !!!yesno)
#note for the best replacement --> use separate file and use replace with (e.g. with Atom)

#dealing with multiple choices

# --> we need to transform the data for each multiple choices in to separate variables.
# --> selecting a sample example from the database, in this case, it is q5


## Water Use purpose
#*********SEPARATING MULTIPLE CHOICE************
# ++++++++++++++++++++++++++++++++++++++

lev <- levels(factor(df2$b2_1))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("b2_1", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$b2_1), ncol = length(lev))
char.var <- as.character(df2$b2_1)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "b2_1")
#+++++++++++++++++++++++++++++++++++++
lev <- levels(factor(df2$b2_2))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("b2_2", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$b2_2), ncol = length(lev))
char.var <- as.character(df2$b2_2)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "b2_2")
#+++++++++++++++++++++++++++++++++++++
lev <- levels(factor(df2$b2_4))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("b2_4", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$b2_4), ncol = length(lev))
char.var <- as.character(df2$b2_4)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "b2_4")
#+++++++++++++++++++++++++++++++++++++
lev <- levels(factor(df2$b2_5))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("b2_5", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$b2_5), ncol = length(lev))
char.var <- as.character(df2$b2_5)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "b2_5")
#***************************************
B2 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B2",skip = 1))
B2_ex <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B2_ex",skip = 1))
df2$b2_1_ex<-recode(df2$b2_1_ex, !!!setNames(B2_ex$c, B2_ex$a))
df2$b2_1_ex <- factor(df2$b2_1_ex,levels=unlist(B2_ex$c)) #order the list

df2$b2_2_ex<-recode(df2$b2_2_ex, !!!setNames(B2_ex$c, B2_ex$a))
df2$b2_2_ex <- factor(df2$b2_2_ex,levels=unlist(B2_ex$c)) #order the list

df2$b2_4_ex<-recode(df2$b2_4_ex, !!!setNames(B2_ex$c, B2_ex$a))
df2$b2_4_ex <- factor(df2$b2_4_ex,levels=unlist(B2_ex$c)) #order the list

df2$b2_5_ex<-recode(df2$b2_5_ex, !!!setNames(B2_ex$c, B2_ex$a))
df2$b2_5_ex <- factor(df2$b2_5_ex,levels=unlist(B2_ex$c)) #order the list


df2$b4[is.na(df2$b4)] = 2
df2$b4<-recode(df2$b4, !!!suf)


## Water Storage
B6 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B6",skip = 1))
df2$b6<-recode(df2$b6, !!!setNames(B6$c, B6$a))
df2$b6 <- factor(df2$b6,levels=unlist(B6$c)) #order the list

###Did you experience any containminant of your water sources

df2$b7_1[is.na(df2$b7_1)] = 2
df2$b7_1<-recode(df2$b7_1, !!!yesno)

df2$b7_2[is.na(df2$b7_2)] = 2
df2$b7_2<-recode(df2$b7_2, !!!yesno)

df2$b7_3[is.na(df2$b7_3)] = 2
df2$b7_3<-recode(df2$b7_3, !!!yesno)

df2$b7_4[is.na(df2$b7_4)] = 2
df2$b7_4<-recode(df2$b7_4, !!!yesno)

df2$b7_5[is.na(df2$b7_5)] = 2
df2$b7_5<-recode(df2$b7_5, !!!yesno)


df2$b7_6[is.na(df2$b7_6)] = 2
df2$b7_6<-recode(df2$b7_6, !!!yesno)

df2$b8[is.na(df2$b8)] = 2
df2$b8<-recode(df2$b8, !!!yesno)

## Over the last 10 years, how much did you spend on constructing/developing your existing water sources, connection, equipment and storage tanks?
B9 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B9",skip = 1))
df2$b9<-recode(df2$b9, !!!setNames(B9$c, B9$a))
df2$b9 <- factor(df2$b9,levels=unlist(B9$c)) #order the list


#Availability and Reliability of water sources

df2$b10_1[is.na(df2$b10_1)] = 2
df2$b10_1<-recode(df2$b10_1, !!!yesno)


df2$b10_2[is.na(df2$b10_2)] = 2
df2$b10_2<-recode(df2$b10_2, !!!yesno)

df2$b10_3[is.na(df2$b10_3)] = 2
df2$b10_3<-recode(df2$b10_3, !!!yesno)


df2$b10_4[is.na(df2$b10_4)] = 2
df2$b10_4<-recode(df2$b10_4, !!!yesno)

df2$b10_5[is.na(df2$b10_5)] = 2
df2$b10_5<-recode(df2$b10_5, !!!yesno)

df2$b10_6[is.na(df2$b10_6)] = 2
df2$b10_6<-recode(df2$b10_6, !!!yesno)

df2$b10_7[is.na(df2$b10_7)] = 2
df2$b10_7<-recode(df2$b10_7, !!!yesno)

df2$b10_8[is.na(df2$b10_8)] = 2
df2$b10_8<-recode(df2$b10_8, !!!yesno)


B11 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B11",skip = 1))
df2$b11_1<-recode(df2$b11_1, !!!setNames(B11$c, B11$a))
df2$b11_1 <- factor(df2$b11_1,levels=unlist(B11$c)) #order the list


B12 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "B12",skip = 1))
df2$b12_1<-recode(df2$b12_1, !!!setNames(B12$c, B12$a))
df2$b12_1 <- factor(df2$b12_1,levels=unlist(B12$c)) #order the list


df2$b13_1<-recode(df2$b13_1, !!!yesno)
df2$b13_2<-recode(df2$b13_2, !!!yesno)
df2$b13_4<-recode(df2$b13_4, !!!yesno)
df2$b13_5<-recode(df2$b13_5, !!!yesno)


#Do you know where the domestic wastewater of households in the community is drained?

df2$c1[is.na(df2$c1)] = 2
df2$c1<-recode(df2$c1, !!!yesno)


#Where is the wastewater of households in your group/family area being drained? 

C2 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "C2",skip = 1))
df2$c2<-recode(df2$c2, !!!setNames(C2$c, C2$a))
df2$c2 <- factor(df2$c2,levels=unlist(C2$c)) #order the list

# flood Frequency
C4 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "C4",skip = 1))
df2$c4<-recode(df2$c4, !!!setNames(C4$c, C4$a))
df2$c4 <- factor(df2$c4,levels=unlist(C4$c)) 

#flood draining time
C5 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "C5",skip = 1))
df2$c5<-recode(df2$c5, !!!setNames(C5$c, C5$a))
df2$c5 <- factor(df2$c5,levels=unlist(C5$c)) 

#Cause of Flooding
C6 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "C6",skip = 1))
df2$c6<-recode(df2$c6, !!!setNames(C6$c, C6$a))
df2$c6 <- factor(df2$c6,levels=unlist(C6$c)) 

#Which of the following options do you think the best solution for short to medium development of community drainage system?

C7 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "C7",skip = 1))
df2$c7<-recode(df2$c7, !!!setNames(C7$c, C7$a))
df2$c7 <- factor(df2$c7,levels=unlist(C7$c)) 

#Does your family have its own toilet?
df2$d1<-recode(df2$d1, !!!yesno)

#What is the reason for not having owned toilet in your family?


#What is the type of toilet in your house?
D3 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "D3",skip = 1))
 df2$d3<-recode(df2$d3, !!!setNames(D3$c, D3$a))
 df2$d3 <- factor(df2$d3,levels=unlist(D3$c)) 

#How old is your toilet?
D4 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "D4",skip = 1))
df2$d4<-recode(df2$d4, !!!setNames(D4$c, D4$a))
df2$d4 <- factor(df2$d4,levels=unlist(D4$c)) 

#Does your family make compost of fertilizer for crops?
df2$d5[is.na(df2$d5)] = 2
df2$d5<-recode(df2$d5, !!!yesno)


#Have you ever applied fresh (un-composted) fertilizer directly to plants? (Choose 1 option)
D7 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "D7",skip = 1))
df2$d7<-recode(df2$d7, !!!setNames(D7$c, D7$a))
df2$d7 <- factor(df2$d7,levels=unlist(D7$c)) 

#Surveyor evaluates the level of hygiene as he observes the house.
D8 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "D8",skip = 1))
df2$d8<-recode(df2$d8, !!!setNames(D8$c, D8$a))
df2$d8 <- factor(df2$d8,levels=unlist(D8$c)) 

#Does your family separate solid waste and general waste before taking it out for dumping/burning/burial? (Choose 1 option)

E2 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "E2",skip = 1))
df2$e2<-recode(df2$e2, !!!setNames(E2$c, E2$a))
df2$e2 <- factor(df2$e2,levels=unlist(E2$c)) 

#Does your family have to pay for garbage collection?
df2$e3_1[is.na(df2$e3_1)] = 2
df2$e3_1<-recode(df2$e3_1, !!!yesno)

df2$e3_2 <-as.numeric(df2$e3_2)

#How often is the waste collected?
E4 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "E4",skip = 1))
df2$e4<-recode(df2$e4, !!!setNames(E4$c, E4$a))
df2$e4 <- factor(df2$e4,levels=unlist(E4$c)) 

#In your opinion, does the garbage collection site meet hygienic standards in the community?
E6 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "E6",skip = 1))
 df2$e6<-recode(df2$e6, !!!setNames(E6$c, E6$a))
 df2$e6 <- factor(df2$e6,levels=unlist(E6$c)) 


#In your opinion, are there any outstanding environmental problems in the community?

df2$e7[is.na(df2$e7)] = 2
df2$e7<-recode(df2$e7, !!!yesno)

#In the past month, has anyone in your family suffered from the diseases mentioned above? 

#+++++++++++++++++++++++++++++++++++++
lev <- levels(factor(df2$f2))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("f2", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$f2), ncol = length(lev))
char.var <- as.character(df2$f2)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "f2")
#------------------------


#In your opinion, is the water source used by the household for eating/drinking contaminated/poisonous?
df2$f3[is.na(df2$f3)] = 3
F3 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "F3",skip = 1))
df2$f3<-recode(df2$f3, !!!setNames(F3$c, F3$a))
df2$f3 <- factor(df2$f3,levels=unlist(F3$c)) 

#In your opinion, what kind of water is called good/clean/safe water, please name those standards? 
#+++++++++++++++++++++++++++++++++++++
lev <- levels(factor(df2$f4))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("f4", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$f4), ncol = length(lev))
char.var <- as.character(df2$f4)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "f4")
#------------------------

#Do you think about the benefits if the locality is invested in building a centralized portable water treatment plant and a distribution system?
lev <- levels(factor(df2$f5))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("f5", lev, sep = ".")) #rename it
result <- matrix(data = "", nrow = length(df2$f5), ncol = length(lev))
char.var <- as.character(df2$f5)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
df2<-add_column(df2, result, .after = "f5")


#Are you willing to connect to the network if the charge is affordable? Or you still prefer to use the existing water source.

G1 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G1",skip = 1))
#df2$g1<-recode(df2$g1, !!!setNames(G1$c, G1$a))
#df2$g1 <- factor(df2$g1,levels=unlist(G1$c)) 


#For having more reliable water supply and better quality of water, are you willing to pay additional expenses on top of existing expense for water consumption?
df2$g2[is.na(df2$g2)] = 2
df2$g2<-recode(df2$g2, !!!yesno)

#If No, what is the reason leading to your decision?

#If Yes, how many percentage of increase that you can afford and willing to pay for?
G4 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G4",skip = 1))
df2$g4<-recode(df2$g4, !!!setNames(G4$c, G4$a))
df2$g4 <- factor(df2$g4,levels=unlist(G4$c)) 


#Specifically, what will be the affordable service charge for you to pay for 1 cubic of treated water? Or per month
G5_1 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G5.1",skip = 1))
df2$g5_1<-recode(df2$g5_1, !!!setNames(G5_1$c, G5_1$a))
df2$g5_1 <- factor(df2$g5_1,levels=unlist(G5_1$c)) 


G5_2 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G5.2",skip = 1))
df2$g5_2<-recode(df2$g5_2, !!!setNames(G5_2$c, G5_2$a))
df2$g5_2 <- factor(df2$g5_2,levels=unlist(G5_2$c)) 


#Aside from paying for monthly water fee, are you willing to pay for initial investment cost for connection pipes, pumps, and new storage tank (if required) to the nearest new water distribution pipe, which will be likely located in nearby road or pathway?
df2$g6[is.na(df2$g6)] = 2
df2$g6<-recode(df2$g6, !!!yesno)


#If Yes, what will be the maximum amount of investment that you think you can invest?
G7 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G7",skip = 1))
df2$g7_1<-recode(df2$g7_1, !!!setNames(G7$c, G7$a))
df2$g7_1 <- factor(df2$g7_1,levels=unlist(G7$c)) 

summary(df2$g7_2)
df2$g7_2<-as.numeric(df2$g7_2)


#If the WTP is selected to be in your farm/agriculture land, are you willing to collaborate with the developer and local authority for site compensation and clearance?
G8 <- data.frame(read_excel("../Data/WASH-data-PB-VN-Household.xlsx", sheet = "G8",skip = 1))
df2$g8<-recode(df2$g8, !!!setNames(G8$c, G8$a))
df2$g8 <- factor(df2$g8,levels=unlist(G8$c)) 

glimpse(head(df2))

#df2<-df2 %>%
#  remove_empty(c("rows", "cols"))

#write down df2 to csv file
write.csv(df2,'../Data/df2.csv')

