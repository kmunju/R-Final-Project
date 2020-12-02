
top10<-top10chemical
p <- qplot(ChemicalName, data = top10, geom = "bar",      # Basic bar chart
           fill = I("lightblue"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))   
p


#Load library
library(readr)
library(dplyr)

#Load column/Remove column


#Read the data as df
df <- read_csv("cscpopendata_cleaned.csv",na=c(""))
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
suppressPackageStartupMessages(library(ggplot2))
df <- group_by(df, ChemicalName)
summ <- summarize(df, num_types = n())
pivot <- dcast(summ, ChemicalName~ ., value.var = "num_types")
pivot<- arrange(pivot, desc(.))
top10chemical<-head(pivot, n = 10) 

top10chemical$reportedtimes<-top10chemical$.
top10chemical$.<-NULL
top10<-top10chemical
top10chemical$ChemicalName
top10chemical$ChemicalName[which(top10chemical$ChemicalName==
                            "Retinol/retinyl esters, when in daily dosages in excess of 10,000 IU, or 3,000 retinol equivalents.")]<-"Retinol/retinyl esters"
top10chemical$ChemicalName[which(top10chemical$ChemicalName==
                                   "Cocamide diethanolamine")]<-"Cocamide"
top10chemical$ChemicalName[which(top10chemical$ChemicalName==
                                   "Vitamin A palmitate")]<-"VitaminA palmitate"

p<-barplot(top10chemical$.,names.arg=top10chemical$ChemicalName,xlab="ChemicalName",ylab="Reported times",col="gold",
        main="Top 10 Chemical chart",border="red")
p

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
# 1 visualization 
