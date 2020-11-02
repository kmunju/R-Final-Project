
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

  
#Filter case number with space
filter(CasNumber !=""&BrandName!="")
InitialDateReported<-as.Date(df$InitialDateReported,format= "%m/%d/%y")
MostRecentDateReported<-as.Date(df$MostRecentDateReported,format= "%m/%d/%y")
DiscontinuedDate<-as.Date(df$DiscontinuedDate,format= "%m/%d/%y")
ChemicalDateRemoved<-as.Date(df$ChemicalDateRemoved,format= "%m/%d/%y")

# what are top 10 most reported type of chemical in cosmetic?
top10chemical<- ungroup(df) %>%
  group_by(df,ChemicalName) %>%
  top_n(10, ChemicalName) %>%
unique(df$ChemicalName)

top10chemical<- df %>% 
  select(ChemicalName, ChemicalCount) %>% 
  group_by(ChemicalName) %>% 
  summarise(sumChemcnt=sum(ChemicalCount))%>%
  arrange(desc(sumChemcnt))%>%
  top_n(10, ChemicalName)

df <- group_by(df, ChemicalName)
summ <- summarize(df, num_types = n())
pivot <- dcast(summ, sale_price_binned ~ city, value.var = "num_types")
