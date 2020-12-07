#TOXIC CHEMICALS IN COSMETICS
#Final project by Munju Kam, Deanna Kwon, Ruiqi Xie


# List of Questions to investigate:
## Q1. what are top 5 most reported type of chemical in cosmetic?
## Q2. What is the health effect of commonly reported chemicals?
## Q3.Which primary/sub category of cosmetics contain most chemical content reports?
## Q4.Which Company is responsive to health concern?


#Load library

library(readr)
library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)
library(xlsx)
library(tidyverse)

#Load column/Remove column
my_col_types <- cols(
  ProductName= col_character(),
  CompanyName= col_character(),
  BrandName= col_character(),
  PrimaryCategory= col_character(),
  SubCategory= col_character(),
  CasId = col_double(),
  CasNumber = col_character(),
  ChemicalName=col_character(),
  ChemicalCount = col_double(),
  CDPHId = col_skip(),
  CSFId = col_skip(),
  CSF= col_skip(),
  CompanyId = col_skip(),
  PrimaryCategoryId = col_skip(),
  SubCategoryId = col_skip(), 
  ChemicalId = col_skip()
)

#Read the data as df
df <- read_csv("cscpopendata.csv", col_type=my_col_types,na=c("", "NA"))


df <- df%>%
  #Filter case number with space
filter(CasNumber != "" & BrandName !="")
InitialDateReported<-as.Date(df$InitialDateReported,format= "%m/%d/%y")
MostRecentDateReported<-as.Date(df$MostRecentDateReported,format= "%m/%d/%y")
DiscontinuedDate<-as.Date(df$DiscontinuedDate,format= "%m/%d/%y")
ChemicalDateRemoved<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")
ChemicalCreatedAt<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")
ChemicalUpdatedAt<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")

##########################################################################################################################################################################
# Q1. what are top 5 most reported type of chemical in cosmetic?

#Create summary table for top 5 common Chemicals in cosmetics
top5chemical <- group_by(df, ChemicalName)
top5chemical <- summarize(top5chemical, ChemicalCount = n())
top5chemical <- arrange(top5chemical, desc(ChemicalCount))
top5chemical <-head(top5chemical, n = 5)
#Test any percentage of appearance frequency for chemical name in the report
chemicalsearch<- function(ChemlName = "") {
  rows <- subset(df, ChemicalName == ChemlName)
  Percentage <- round((nrow(rows)/nrow(df)*100),2)
  Percentage <- paste(Percentage, "%", sep="")
  return(Percentage)
}

#Create the percentage column with calculation using for loop
top5chemical$Percentage<-0
for(i in 1:5){
  top5chemical$Percentage[i] <- chemicalsearch(top5chemical$ChemicalName[i])
}
 

#Create bar chart for Top 5 common Chemicals in cosmetics
p<-barplot(top5chemical$ChemicalCount,names.arg=top5chemical$ChemicalName,xlab="ChemicalName",ylab="Reported times",col="gold",
           main="Top 5 common Chemicals in cosmetics",border="red")
p


##########################################################################################################################################################################


# Q2. What is the toxicity of commonly reported chemicals?

#Read in reference pdf
#Note: Refer to "pdf cleaning process.R" for extraction method of pdf to excel
pdf <- read_excel("pdf.xlsx", sheet = 1)

#Select only needed columns
pdf <- pdf %>% 
  select(`CAS No.`, Cancer, Developmental, 'Female Reproductive', 'Male Reproductive')

names(pdf)[1]<-"CasNumber" #Change name of "CAS NO." column to "CasNumber" for later use

ungroup(df) #Ungroup df from previous question

#Making exact same copy of summary table in previous question, but adding
#CasNumber column
df<- group_by(df, ChemicalName, CasNumber)
summ <- summarize(df, num_types = n()) 
pivot<- arrange(summ, desc(num_types)) 
top5chemical1<-head(pivot, n = 5) 

#Then add the CasNumber to top5chemical as its order
top5chemical$CasNumber <- top5chemical1$CasNumber 

#Check whether two CasNumber columns are identical
identical(top5chemical$CasNumber, top5chemical1$CasNumber) 
top5chemical <- as.data.frame(top5chemical) #Coercing top5chemical as dataframe
#Leftjoin top5chemical table and pdf by "CasNumber" to look into health effects
top5toxic <- left_join(top5chemical, pdf, by = "CasNumber") 
top5toxic<-top5toxic[c(-5),]

#Please refer to ppt for visualization

##########################################################################################################################################################################

#Q3.Which primary category of cosmetics contain most chemical content reports?

#Create Summary table for chem count on primary category
Primary <- df %>% 
  select(PrimaryCategory, ChemicalCount) %>% 
  group_by(PrimaryCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))


#Create Summary table for chem count on Subcategory
Sub <- df %>% 
  select(SubCategory, ChemicalCount) %>% 
  group_by(SubCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))

##########################################################################################################################################################################

#Q4.Which Company is responsive to health concern?

#Create the data frame that contains the company that respond to the health concern 
df <- ungroup(df)
ethics <- df %>% 
  select(CompanyName,DiscontinuedDate,ChemicalCreatedAt,ChemicalUpdatedAt,ChemicalDateRemoved) %>% 
  filter(ChemicalCreatedAt!=ChemicalUpdatedAt)
ethics <- ethics[complete.cases(ethics), ]

#Create table that summarize the count of company named mentioned
ethics5 <- group_by(ethics, CompanyName)
ethics5 <- summarize(ethics5, Frequency = n())
ethics5 <- arrange(ethics5, desc(Frequency))
ethics5 <-head(ethics5, n = 5) 

#Q4.Visualization with bar graph

p<-barplot(ethics5$Frequency,names.arg=ethics5$CompanyName,xlab="CompanyName",ylab="Frequency",col="seagreen3",
           main="Top 5 Company responsive to health concern",border="seagreen4")
p 


 

#Data referece: https://data.chhs.ca.gov/dataset/chemicals-in-cosmetics/resource/57da6c9a-41a7-44b0-ab8d-815ff2cd5913
#https://www.cdph.ca.gov/Programs/CCDPHP/DEODC/OHB/CSCP/CDPH%20Document%20Library/chemlist.pdf

