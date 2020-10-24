
#Load library
library(readr)
library(dplyr)

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
  ChemicalCreatedAt= col_skip(),
  ChemicalUpdatedAt= col_skip(),
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

top10chemical<- ungroup(df) %>%
  top_n(10, ChmemicalName) 
