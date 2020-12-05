# install.packages("readxl")

#Load library

library(readr)
library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)

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
df <- read_csv("cscpopendata.csv", col_type=my_col_types,na=c(""))
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
top5chemical$Percentage <- round((top5chemical$ChemicalCount)/sum(top5chemical$ChemicalCount)*100,2)
top5chemical <-head(top5chemical, n = 5) 

#Create bar chart for Top 5 common Chemicals in cosmetics
p<-barplot(top5chemical$ChemicalCount,names.arg=top5chemical$ChemicalName,xlab="ChemicalName",ylab="Reported times",col="gold",
           main="Top 5 common Chemicals in cosmetics",border="red")
p

##########################################################################################################################################################################

# Q2. What is the toxicity of commonly reported chemicals?

pdf <- read_excel("pdf.xlsx")
pdf[,c(1)]<-NULL
pdf[,c(1)]<-NULL

pdf <- pdf %>% 
  select(Chemical, Cancer, Developmental, 'Female Reproductive', 'Male Reproductive')

TiO2 <- pdf[grep("Titanium dioxide", x=pdf$Chemical),]
ButylHy <- pdf[grep('Butylated hydroxyanisole', x=pdf$Chemical),]
CarbonB <- pdf[grep('Carbon black', x=pdf$Chemical),]
Talc <- pdf[grep('Talc', x=pdf$Chemical),]
Retinol <- pdf[grep('Retinol', x=pdf$Chemical),]
Cocamide <- pdf[grep('cocamide', x=pdf$Chemical),]
Silica <- pdf[grep('Silica', x=pdf$Chemical),]
Mica <- pdf[grep('mica', x=pdf$Chemical),]# lung scarring which leads to symptoms such as coughing, shortness of breath, weakness, and weight loss.
Vitamin<- pdf[grep('vitamin', x=pdf$Chemical),]#Too much intake-altered bone metabolism and altered metabolism of other fat-soluble vitamins
Retinylpalmitate<- pdf[grep('palmitate', x=pdf$Chemical),]#(Combination of pure vitamin A and fatty acid palmitic acid)-Generally safe

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


 

