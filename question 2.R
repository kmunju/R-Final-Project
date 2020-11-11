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
# Lung scarring which leads to symptoms such as coughing, shortness of breath, weakness, and weight loss
Mica <- pdf[grep('mica', x=pdf$Chemical),]
#Too much intake-altered bone metabolism and altered metabolism of other fat-soluble vitamins
Vitamin<- pdf[grep('vitamin', x=pdf$Chemical),]
#(Combination of pure vitamin A and fatty acid palmitic acid)-Generally safe
Retinylpalmitate<- pdf[grep('palmitate', x=pdf$Chemical),]
