library("data.table")
xls <- read.xlsx(xlsxFile = "data/Base_Endereco0219.xlsx",sheet = 1)
names(xls) <- c("id","end","compl","bairro","cidade","cep")

df <- read.csv("data/newXLS.csv",header = F,sep = ";",stringsAsFactors = F)
names(df) <- c("id","end","compl","bairro","cidade","cep")

erro <- read.csv("data/newXLSErr.csv",header = F,sep = ";",stringsAsFactors = F)
names(erro) <- c("id","end","compl","bairro","cidade","cep")

df$flag <- "ok"
erro$flag <- "cep"


newdf <- merge.data.frame(x = xls,y = df,by = id)

library("dplyr")
library("sqldf")
newdf <- left_join(xls,df,by = "id")
