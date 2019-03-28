library(data.table)
library("tidyr")
library("stringr")
require("sqldf")
library("openxlsx")
library("tm")

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

conEnd <- function(myQuery) {
        myQuery <- paste0("select * from ceps where LOG_NO LIKE ","'",myQuery,"'")
        ender <- sqldf(myQuery)
        bairr <- conBairro(ender$BAI_NU_INI)
        response <- data.frame(endereco=paste(ender$TLO_TX, ender$LOG_NO), bairro=bairr$BAI_NO, uf=ender$UFE_SG, cep=ender$CEP)
        return(response)
}

conBairro <- function(myBairro) {
        myBairro <- paste0("select * from bairro where BAI_NU LIKE ","'",myBairro,"'")
        response <- sqldf(myBairro)
        return(response)
}

findByCEP <- function(myQuery) {
        myQuery <- paste0("select * from ceps where CEP LIKE ","'",myQuery,"'")
        sqldf(myQuery)
}

findByLOC <- function(myQuery) {
        myQuery <- paste0("select * from localidade where LOC_NU LIKE ","'",myQuery,"'")
        return(sqldf(myQuery))
}

newXLS <- data.frame()
xls <- read.xlsx(xlsxFile = "data/Base_Endereco0219.xlsx",sheet = 1)

localidade <- read.csv(file = "correios/LOG_LOCALIDADE.txt",header = F,sep = "@",stringsAsFactors = F)
names(localidade) <- c("LOC_NU","UFE_SG","LOC_NO","CEP",
                       "LOC_IN_SIT","LOC_IN_TIPO_LOC","LOC_NU_SUB",
                       "LOC_NO_ABREV","MUN_NU")

for(i in 1:dim(xls)[1]) {
        df <- xls[i,]
        adr <- word(df$Endereco,1)
        adr <- str_remove(df$Endereco,adr)
        x <- word(adr,-1)
        adr <- str_remove(adr,x)
        documents <- Corpus(VectorSource(adr))
        adr = tm_map(documents, removePunctuation)$content
        adr <- str_trim(string = adr,side = "both")
        newCEP <- findByCEP(df$CEP)
        new_adr <- conEnd(adr)
        df$Endereco <- paste(newCEP$TLO_TX,newCEP$LOG_NO,x)
        df$Bairro <- as.character(conBairro(myBairro = newCEP$BAI_NU_INI)[4])
        df$Descricao_Cidade <- as.character(findByLOC(newCEP$LOC_NU)[3])
        newXLS <- rbind(newXLS,df)
}


write.csv(x = newXLS,file = "data/newXLS.csv")


