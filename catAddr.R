library("data.table")
library("openxlsx")

orig <- read.xlsx(xlsxFile = "data/Base_Endereco0219.xlsx",sheet = 1)
names(orig) <- c("id","end","compl","bairro","cidade","cep")

gooddf <- read.csv("data/newXLS.csv",header = F,sep = ";",stringsAsFactors = F)
names(gooddf) <- c("id","end","compl","bairro","cidade","cep")
gooddf$id <- as.character(gooddf$id)

errdf <- read.csv("data/newXLSErr.csv",header = F,sep = ";",stringsAsFactors = F)
names(errdf) <- c("id","end","compl","bairro","cidade","cep")

gooddf$flag <- "ok"
errdf$flag <- "cep"


newdf <- rbind(gooddf,errdf)

norig <- merge.data.frame(x = orig,y = newdf,by = "id")
norig$id <- as.character(norig$id)
orig$id <- as.character(orig$id)

library("dplyr")
library("sqldf")
norig <- left_join(orig,newdf,by = c("id","cep"))

dfERR <- unique(df)
