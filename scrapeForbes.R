library("XML")
setwd("/Dropbox/GitRepository/Celebs/")
url <- "http://star-currency.forbes.com/celebrity-list/top-celebrities?page="
pages <- 0:57
urls <- paste(url, pages, sep="")
n <- length(urls)
out <- vector("list", n)

for(i in 1:n){
	out[[i]]<- data.frame(readHTMLTable(urls[i]))
	names(out[[i]]) <- c("rank", "name", "score", "profession", "age")
}
x <- out[[1]]

for(i in 2:56){
	x <- rbind(x, out[[i]])
}

write.csv(x, "forbes1400.csv", row.names=F)
