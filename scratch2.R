lakas <- grepl("lakás|Lakás|lakas|Lakas",df2$leiras)
summary(lakas)
lakoh <- grepl("lakóház|Lakóház|lakohaz|Lakohaz",df2$leiras)
udul <- grepl("üdülő|Üdülő|udulo|Udulo|Hétvégi|hetvegi|hétvégi|Hetvegi",df$leiras)
summary(lakoh)
summary(udul)
summary(lakas&lakoh)
par(mfrow=c(1,2))
plot(df2[lakoh,]$lon,df2[lakoh,]$lat)
plot(df2[lakas,]$lon,df2[lakas,]$lat)
dev.off()

df$ugyszam
strsplit(df$ugyszam,"/") |> map(1) |> unlist() |> unique()
strsplit(df$ugyszam,"/") |> map(2) |> unlist() |> unique()

paste(strsplit(df$ugyszam,"/") |> map(1),strsplit(df$ugyszam,"/") |> map(2),sep = "/")
