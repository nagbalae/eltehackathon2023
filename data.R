library(readxl)
library(data.table)
befejezett <- list.files(pattern = "mbvk_ingatlan_befejezett_*")
elo <- list.files(pattern = "mbvk_ingatlan_elo*")
elo
listdf <- lapply(befejezett, read_excel)
dfb <- do.call(rbind,listdf) |> as.data.table()
listdfe <- lapply(elo, read_excel)
dfe <- do.call(rbind,listdfe) |> as.data.table()
write.csv(x = dfb, "gyorsdata.csv")

dfb$telep <- dfb$cim
sub(',.*',"",dfb$cim)
test <- strsplit(dfb$cim,' ') [994]
test
grepl(pattern = "utca|u.|utca.",test)
patt<- "(utca)|(u[.])|(utca)|(út)|(Lakótelep)|(Krt[.])|U|(Sor)|(Hrsz)|(Hrsz[.])|(tér)|(Tér)|(Park)|(Lakókert)|(Dűlő)|(Belterület)|(Telep)|(Belt.)|(Köz)|(köz)|(Major)|(major)|(Sétány)|(Kapu)|(Liget)|(Oldal)"
dfb[!sapply(dfb$cim,function(x){grepl(pattern = patt,x)}),]$cim

