# loading libraries
library(tabulizer)
library(pdftools)
library(tidyverse)
library(dplyr)
library(janitor)
library(xlsx)

# setting url of the pdf and extracting tables
pdfurl <- 'https://www.cdph.ca.gov/Programs/CCDPHP/DEODC/OHB/CSCP/CDPH%20Document%20Library/chemlist.pdf'
pdf <- extract_tables(pdfurl)

# process of cleaning begins here:
# removing the first row for first list, and first two rows for rest of the lists
# containing column titles of tables in rows
pdf[[1]]<-pdf[[1]][-c(1),]
pdf[[2]]<-pdf[[2]][-c(1,2),]
pdf[[3]]<-pdf[[3]][-c(1,2),]
pdf[[4]]<-pdf[[4]][-c(1,2),]
pdf[[5]]<-pdf[[5]][-c(1,2),]
pdf[[6]]<-pdf[[6]][-c(1,2),]
pdf[[7]]<-pdf[[7]][-c(1,2),]
pdf[[8]]<-pdf[[8]][-c(1,2),]
pdf[[9]]<-pdf[[9]][-c(1,2),]
pdf[[10]]<-pdf[[10]][-c(1,2),]
pdf[[11]]<-pdf[[11]][-c(1,2),]
pdf[[12]]<-pdf[[12]][-c(1,2),]
pdf[[13]]<-pdf[[13]][-c(1,2),]
pdf[[14]]<-pdf[[14]][-c(1,2),]
pdf[[15]]<-pdf[[15]][-c(1,2),]
pdf[[16]]<-pdf[[16]][-c(1,2),]
pdf[[17]]<-pdf[[17]][-c(1,2),]
pdf[[18]]<-pdf[[18]][-c(1,2),]
pdf[[19]]<-pdf[[19]][-c(1,2),]
pdf[[20]]<-pdf[[20]][-c(1,2),]
pdf[[21]]<-pdf[[21]][-c(1,2),]
pdf[[22]]<-pdf[[22]][-c(1,2),]
pdf[[23]]<-pdf[[23]][-c(1,2),]
pdf[[24]]<-pdf[[24]][-c(1,2),]
pdf[[25]]<-pdf[[25]][-c(1,2),]
pdf[[26]]<-pdf[[26]][-c(1,2),]
pdf[[27]]<-pdf[[27]][-c(1,2),]
pdf[[28]]<-pdf[[28]][-c(1,2),]
pdf[[29]]<-pdf[[29]][-c(1,2),]
pdf[[30]]<-pdf[[30]][-c(1,2),]
pdf[[31]]<-pdf[[31]][-c(1,2),]
pdf[[32]]<-pdf[[32]][-c(1,2),]
pdf[[33]]<-pdf[[33]][-c(1,2),]
pdf[[34]]<-pdf[[34]][-c(1,2),]

# re-writing column titles as the original pdf file
pdf[[1]][1,] <- c("_","Chemical","Synonyms","CAS No.","Cancer","Developmental",
                  "Female Reproductive","Male Reproductive","CalEPA - Prop 65","IARC",
                  "EPA - IRIS","NTP - RoC","NTP - OHAT")

# setting the first row of list 1 of pdf to column names
pdf[[1]] <- pdf[[1]] %>%
  row_to_names(row_number = 1)

# binding lists together
pdfbind <- rbind(pdf[[1]],pdf[[2]],pdf[[3]],pdf[[4]],pdf[[5]],pdf[[6]],pdf[[7]],
                 pdf[[8]],pdf[[9]],pdf[[10]],pdf[[11]],pdf[[12]],pdf[[13]],pdf[[14]],
                 pdf[[15]],pdf[[16]],pdf[[17]],pdf[[18]],pdf[[19]],pdf[[20]],pdf[[21]],
                 pdf[[22]],pdf[[23]],pdf[[24]],pdf[[25]],pdf[[26]],pdf[[27]],pdf[[28]],
                 pdf[[29]],pdf[[30]],pdf[[31]],pdf[[32]],pdf[[33]],pdf[[34]])

# changing the whole binded pdf to data frame
pdfbind <- as.data.frame(pdfbind)

# finding which rows contain information other than "x" or "blank" in Developmental column
# View(pdfbind[c(which(pdfbind$`Developmental` !="x" & pdfbind$`Developmental` !="")),])

# changing rows that contain wrong information to "blank" by comparing to pdf
pdfbind[c(26, 28, 30, 96, 125, 150, 151, 152, 187, 223, 257,
          325, 354, 385, 453, 517, 619, 876, 877, 915, 916,
          934, 939, 971, 1005),c("Developmental")] <- ""

# changing rows that contain wrong information to "x" by comparing to pdf
pdfbind[c(31, 291, 420, 487, 584, 746, 776, 780),c("Developmental")] <- "x"

# finding which rows contain information other than "x" or "blank" in `Female Reproductive` column
# View(pdfbind[c(which(pdfbind$`Female Reproductive` !="x" & pdfbind$`Female Reproductive` !="")),])

# changing rows that contain wrong information to "blank" by comparing to pdf
pdfbind[c(26, 28, 30, 96, 125, 150, 151, 152, 187, 223, 257,
          291, 325, 354, 385, 420, 453, 517, 584, 619, 746, 776,
          780, 876, 877, 915, 916, 934, 939, 971, 1005), 7] <- ""

# changing rows that contain wrong information to "x" by comparing to pdf
pdfbind[c(31, 487), 7] <- "x"

# writing the cleaned pdf as excel file for future convenience
write.xlsx(pdfbind, file="pdf.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


