library(data.table)
library("tidyr")
library("stringr")
require("sqldf")
library("openxlsx")
library("tm")
library("data.table")

files <- list.files(path = "correios",pattern = "LOG_LOGRADOURO*")
bairro <- read.delim("correios/LOG_BAIRRO.TXT", header=F, stringsAsFactors=FALSE,sep = "@")
names(bairro) <- c("BAI_NU","UFE_SG","LOC_NU","BAI_NO","BAI_NO_ABREV")
ceps <- data.frame()

for(file in files) {
        cep <- read.delim(paste0("correios/",file), header=FALSE, stringsAsFactors=FALSE,sep = "@")
        ceps <- rbind(ceps,as.data.frame(cep))
}
names(ceps) <- c("LOG_NU","UFE_SG","LOC_NU","BAI_NU_INI","BAI_NU_FIM","LOG_NO","LOG_COMPLEMENTO",
                "CEP","TLO_TX","LOG_STA_TLO","LOG_NO_ABREV")
head(ceps)
remove(cep,file,files)

bairro <- data.table(bairro,key = "BAI_NU",stringsAsFactors = F)
conBairro <- function(myBairro) {
        bairro[which(bairro$BAI_NU == myBairro),]
}

ceps <- data.table(ceps,key = "CEP",stringsAsFactors = F)
findByCEP <- function(myQuery) {
        ceps[which(ceps$CEP == myQuery),]
}

localidade <- data.table(localidade,key = "LOC_NU",stringsAsFactors = F)
findByLOC <- function(myQuery) {
        localidade[which(localidade$LOC_NU == myQuery),]
}

newXLS <- data.frame()
xls <- read.xlsx(xlsxFile = "data/Base_Endereco0219.xlsx",sheet = 1)

localidade <- read.csv(file = "correios/LOG_LOCALIDADE.txt",header = F,sep = "@",stringsAsFactors = F)
names(localidade) <- c("LOC_NU","UFE_SG","LOC_NO","CEP",
                       "LOC_IN_SIT","LOC_IN_TIPO_LOC","LOC_NU_SUB",
                       "LOC_NO_ABREV","MUN_NU")
erros <- 0
file.remove("data/newXLS.csv")
for(i in 1:dim(xls)[1]) {
        df <- xls[i,]
        if(sapply(strsplit(df$Endereco, " "), length) < 3) {
                write.table(x = df, file = "data/newXLSErr.csv", sep = ",", append = TRUE, quote = FALSE,
                            col.names = FALSE, row.names = FALSE)
                erros <- erros +1
                next
        }
        df$Endereco <- str_trim(string = df$Endereco,side = "both")
        adr <- word(df$Endereco,1)
        adr <- str_remove(df$Endereco,adr)
        documents <- Corpus(VectorSource(adr))
        adr = tm_map(documents, removePunctuation)$content
        adr <- str_trim(string = adr,side = "both")
        if(sapply(strsplit(adr, " "), length) > 2) {
                x <- word(adr,-1)
        }
        adr <- str_remove(adr,x)
        adr <- str_trim(string = adr,side = "both")
        newCEP <- findByCEP(df$CEP)
        if(length(newCEP$LOG_NU) == 0) {
                write.table(x = df, file = "data/newXLSErr.csv", sep = ";", append = TRUE, quote = FALSE,
                            col.names = FALSE, row.names = FALSE)
                erros <- erros +1
                next                
        }
        df$Endereco <- paste(newCEP$TLO_TX,newCEP$LOG_NO,", ",x)
        df$Bairro <- as.character(conBairro(myBairro = newCEP$BAI_NU_INI)[,4])
        df$Descricao_Cidade <- as.character(findByLOC(newCEP$LOC_NU)[,3])
        write.table(x = df, file = "data/newXLS.csv", sep = ";", append = TRUE, quote = FALSE,
                    col.names = FALSE, row.names = FALSE)
}
