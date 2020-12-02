#Load library
library(readr)
library(dplyr)
library(readxl)

df <- read_csv("cscpopendata_cleaned.csv", na=c(""))

# Q2. What is the toxicity of commonly reported chemicals?
pdf <- read_excel("pdf.xlsx")
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

Chemical <- c("Titanium Dioxide", "Butylated Hydroxyanisole", "Carbon Black"," Talc", "Retinol", "Cocamide Diethanolamine", "Silica", "Pirimicarb", "Vitamin A", "Retinyl Palmitate")
Toxicity <- c("Cancer", "Cancer", "Cancer", "Cancer", "Developmental", "Cancer" ,"Cancer", "Other", "Other", "Other")
# Cancer <- c(1, 1, 1, 1, 0, 1, 1, 0, 0, 0)
# Developmental <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
# `Female Reproductive` <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# `Male Reproductive` <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# Other <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1 )
q2table <- data.frame(Chemical, Toxicity)

levels(Toxicity) = c("Cancer","Developmental","Female Reproductive","Male Reproductive", "Other")
levels(Toxicity)

# Selection of different plots 
library(ggplot2)
q2plot <- qplot(Chemical, data = q2table, geom = "bar", fill = Toxicity)
q2plot

q2plot2 <- ggplot(q2table, aes(Toxicity, Chemical))
q2plot2 <- q2plot2 + geom_label(aes(label=Toxicity))#nudge_x=1,nudge_y=1)
q2plot2

q2plot3 <- ggplot(q2table, aes(Toxicity, fill = Chemical))
q2plot3 <- q2plot3 + geom_bar(position="fill")
q2plot3


#Q3.Which primary category of cosmetics contain most chemical content reports?
library(dplyr)
library(reshape2)

#Summary result for chem count(primary category)
Primary <- df %>% 
  select(PrimaryCategory, ChemicalCount) %>% 
  group_by(PrimaryCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))

Primary <- as.data.frame(Primary)
#Summary result for chem count(subcategory)
Sub <- df %>% 
  select(SubCategory, ChemicalCount) %>% 
  group_by(SubCategory) %>% 
  summarise(Avgchemcnt=mean(ChemicalCount))%>%
  arrange(desc(Avgchemcnt))

