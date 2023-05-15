library(readxl)
library(data.table)
library(jsonlite)
library(purrr)



get_coords <- function(cim){
	url<-URLencode(paste0("http://localhost:8080/search.php?q=",cim,"&limit=1"))
	data <- read_json(url, simplifyVector = T)
	if(length(data) == 0){
		NA
	} else {
		c(data$lat,data$lon)
	}
}

befejezett <- list.files(pattern = "mbvk_ingatlan_befejezett_*")
elo <- list.files(pattern = "mbvk_ingatlan_elo*")
elo
listdf <- lapply(befejezett, read_excel)
dfb <- do.call(rbind,listdf) |> as.data.table()
listdfe <- lapply(elo, read_excel)
dfe <- do.call(rbind,listdfe) |> as.data.table()
dfe
lapply(listdfe,ncol)
dfb
write.csv(x = dfb, "gyorsdata.csv")
df <- read.csv("gyorsdata.csv") |> as.data.table()


df$point <- sapply(df$cim, get_coords)
df$point2 <- df$point
df[is.na(df$point2),]$point2 <- sapply(sub(",.*","",df[is.na(df$point2),]$cim),get_coords)
df2 <- df[!is.na(df$point2),]
df2$lat <- as.double(unlist(map(df2$point2,1)))
df2$lon <- as.double(unlist(map(df2[!is.na(df2$point2),]$point2,2)))
df[]
plot(df2$lon,df2$lat)

colnames(df2)
write.csv(x = df2[,-c("point","point2")],file = "lonlattal.csv")

df2
df2$azonosito <- paste(strsplit(df2$ugyszam,"/") |> map(1),strsplit(df2$ugyszam,"/") |> map(2),sep = "/")
df2$iktszam <- 	strsplit(df2$ugyszam,"/") |> map(3) |> unlist()
df2$iktszam
df2

duplicated(df2$iktszam)
########## duplikált törlése nem jó jelenleg

uniqlist <- aggregate(iktszam~azonosito, df2, max) |> as.data.frame()
uniqlist <- paste(uniqlist$azonosito,uniqlist$iktszam,sep = "/")
df2_szurt <- df2[df2$ugyszam %in% uniqlist,]

plot(df2_szurt$lon,df2_szurt$lat)


df2_szurt$ev <- strsplit(df2_szurt$ugyszam,"/") |> map(2) |> unlist() |> as.numeric()
df2_szurt$ev
hist(df2_szurt$ev)
df3 <- df2_szurt|> as.data.frame()
df3 <- df3[,-c(12,13)]
as.data.frame(df3) |> typeof()
df3
df3 |> colnames()
typeof(df2_szurt)
write.csv(x=df3,file = "szurt.csv")
colnames(df3)
typeof(df3)
