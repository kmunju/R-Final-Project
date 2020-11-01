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

#top_n(10, Avgchemcntt) 


#Q4.Which companies' cosmetics contain most chemical reports and have not yet fixed, removed, or discontinued the product? Which companies are responsive to public health concern?

#Company responsive to health concern
ethics <- df %>% 
  select(CompanyName,ProductName,ChemicalCount,DiscontinuedDate,ChemicalCreatedAt,ChemicalUpdatedAt,ChemicalDateRemoved) %>% 
  group_by(CompanyName,ProductName,ChemicalCreatedAt,ChemicalUpdatedAt,DiscontinuedDate,ChemicalDateRemoved) %>% 
  summarise(chemcnt=sum(ChemicalCount))%>%
  filter(ChemicalCreatedAt!=ChemicalUpdatedAt) %>% 
  arrange(desc(chemcnt))
ethics <- ethics[complete.cases(ethics), ]