library(data.table)
library(ggplot2)
library(magrittr)

options(datatable.print.class = TRUE)
head(fb)

fb <- fread("data/dataset_Facebook.csv")
summary(fb)

colnames(fb)
#A szókozök gondot okoznak a datatableban, ezeket _-vel helyettesítem
colnames(fb) <- gsub(" ", "_", colnames(fb))
#Néhány változót nagybetűvel írtak
colnames(fb) <- tolower(colnames(fb))


ggplot(fb, aes(Page_total_likes)) + geom_histogram()
#nincs kiugró érték

ggplot(fb, aes(like)) + geom_histogram()
fb[like > 1000][order(-like)]
#5172 nagyon kiugró

clearOutlierLike <- function(fb) {
  fb[like > 2000, like := NA]
}


ggplot(fb, aes(share)) + geom_histogram()
fb[share > 100][order(-share)]
#790 kiugró

clearOutlierShare <- function(fb) {
  fb[share > 700, share := NA]
}


ggplot(fb, aes(comment)) + geom_histogram()
fb[comment > 60][order(-comment)]
#372 kiugró, 3 db >100

clearOutlierComment <- function(fb) {
  fb[comment > 300, comment := NA]
}

#A hisztogramokról látható, hogy nagyon nagy a szórás.

clearOutlierLike(fb)
clearOutlierShare(fb)
clearOutlierComment(fb)


#total_interactions a "comment", a "like", és a "share" összege. mivel ez nem
#függvényként van megadva (az új NA-kat nem kezeli), ezt is módosítani kell

fb[, total_interactions := comment + like + share]

fb[, lapply(.SD, uniqueN)] #nincsenek duplikátumok
