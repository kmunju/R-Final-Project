#Load library
library(readr)
library(dplyr)

#Load column/Remove column
my_col_types <- cols(
  ProductName= col_character(),
  CompanyName= col_character(),
  BrandName= col_character(),
  PrimaryCategory= col_factor(),
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
  filter(CasNumber !=""&BrandName!="")
InitialDateReported<-as.Date(df$InitialDateReported,format= "%m/%d/%y")
MostRecentDateReported<-as.Date(df$MostRecentDateReported,format= "%m/%d/%y")
DiscontinuedDate<-as.Date(df$DiscontinuedDate,format= "%m/%d/%y")
ChemicalDateRemoved<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")
ChemicalCreatedAt<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")
ChemicalUpdatedAt<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")

##########################################################################################################################################################################
# Q1. what are top 10 most reported type of chemical in cosmetic?
library(reshape2)

top10chemical<- df %>% 
  select(ChemicalName, ChemicalCount) %>% 
  group_by(ChemicalName) %>% 
  summarise(sumChemcnt=sum(ChemicalCount))%>%
  arrange(desc(sumChemcnt))%>%
  top_n(10, ChemicalName)

df <- group_by(df, ChemicalName)
summ <- summarize(df, num_types = n())
pivot <- dcast(summ, ChemicalName~ ., value.var = "num_types")
pivot<- arrange(pivot, desc(.))
top10chemical<-head(pivot, n = 10) 

# Q2. What is the toxicity of commonly reported chemicals?
# install.packages("readxl")
library(readxl)
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


#Q3.Which primary category of cosmetics contain most chemical content reports?
library(dplyr)
library(reshape2)

#Summary result for chem count(primary category)
Primary <- df %>% 
  select(PrimaryCategory, ChemicalCount) %>% 
  group_by(PrimaryCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))

#Summary result for chem count(subcategory)
Sub <- df %>% 
  select(SubCategory, ChemicalCount) %>% 
  group_by(SubCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))


#Q4.Which companies' cosmetics contain most chemical reports and have not yet fixed, removed, or discontinued the product? Which companies are responsive to public health concern?

#Company responsive to health concern
ethics <- df %>% 
  select(CompanyName,ProductName,ChemicalCount,DiscontinuedDate,ChemicalCreatedAt,ChemicalUpdatedAt,ChemicalDateRemoved) %>% 
  group_by(CompanyName,ProductName,ChemicalCreatedAt,ChemicalUpdatedAt,DiscontinuedDate,ChemicalDateRemoved) %>% 
  summarise(chemcnt=sum(ChemicalCount))%>%
  filter(ChemicalCreatedAt!=ChemicalUpdatedAt) %>% 
  arrange(desc(chemcnt))
ethics <- ethics[complete.cases(ethics), ]