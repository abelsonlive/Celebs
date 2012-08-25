rm(list=ls())
options(digits=1)
library("stringr")
library("plyr")
setwd("~/Dropbox/GitRepository/Celebs/")
#read in raw data from google refine
d <- read.csv("celebs-csv(1).csv", stringsAsFactors=F)
names(d) <- c("rank", "name", "spouse", "breakup", "dated", "canoodled", "score", "profession", "age")
# all.ppl <- gsub(" ", "_", unique(c(d$name, d$spouse, d$dated, d$canoodled)))
# all.ppl <- all.ppl[which(all.ppl!="")]
# n = length(all.ppl)
# get just the forbes top 1400
vecs <- grep("[0-9]", d$rank)
n.seeds <- length(vecs)

#get scores:
Score <- as.numeric(d$score[vecs])
Rank <- as.numeric(d$rank[vecs])
Label <- gsub(" ", "_", d$name[vecs])
# get age
Age <- as.numeric(d$age[vecs])
# get profession
Profession <- d$profession[vecs]
i = numeric(n.seeds)
# add id
attr = data.frame(Label,
				  Score,				  			  
				  Rank,
				  Age,
				  Profession,
				  Spouses = i,
				  Dated = i,
				  Total = i,
				  Spouses1400 = i,
				  Dated1400 = i,
				  Total1400 = i,
				  stringsAsFactors=F)
# get names of top 1400
top1400 = as.character(d$name[which(d$name!="")])
top1400 = gsub(" ", "_", top1400)

for (s in 1:n.seeds){

	rows <- (vecs[s]:(vecs[(s+1)]-1))
	actor <- try(d[rows, ])
	name = actor$name[1]
	
	# clean spouses
	spouses <- actor$spouse[which(actor$spouse!=name)]
	spouses <- gsub(" ", "_",unique(spouses[which(spouses!="")]))
	# match top 1400	
	arg1 <- spouses %in% top1400
	spouses1400  = spouses[arg1]
	
	# clean dated
	dated <- c(actor$dated, actor$canoodled, actor$breakup)
	dated <- gsub(" ", "_", unique(dated[which(dated != "" & dated != name)]))
	
	# remove matches with spouses
	arg2   <- !dated %in% spouses
	dated =  dated[arg2]

	# match top 1400
	arg3 <- dated %in% top1400
	dated1400  = dated[arg3]

	#compute variables
	ms = length(spouses) 
	dts = length(dated)
	ms1400 = length(spouses1400)
	dts1400 = length(dated1400)
	attr$Spouses[s] = ms
	attr$Dated[s] = dts
	attr$Total[s] = ms + dts
    attr$Spouses1400[s] = ms1400 
    attr$Dated1400[s] = dts1400
    attr$Total1400[s] = ms1400 + dts1400
    name = gsub(" ", "_", name)
# start edge list
	if(s == 1){
		edges  = data.frame(Source="", 
							Target="",
							Weight=0, 
							stringsAsFactors=F)
		for(m in 1:ms1400){
			df = data.frame(Source = name,
					  	    Target = spouses1400[m],
					        Weight = 2, 
					        stringsAsFactors=F)
			edges = rbind(edges, df)
		}
		for(dt in 1:dts1400){
			df = data.frame(Source = name,
					  	    Target = dated1400[dt],
					        Weight = 1, 
					        stringsAsFactors=F)
			edges = rbind(edges, df)
		}
	}
	if(s > 1){

		if (ms1400!=0){
			for(m in 1:ms1400){
			df = data.frame(Source = name,
					  	    Target = spouses1400[m],
					        Weight = 2, 
					        stringsAsFactors=F)
			edges = rbind(edges, df)
			}
		}
		if (dts1400!=0){
			for(dt in 1:dts1400){
			df = data.frame(Source = name,
					  	    Target = dated1400[dt],
					        Weight = 1,
					        stringsAsFactors=F)
			edges = rbind(edges, df)

			}
		}
	}
}

# remove duplicates and other cleanup
edges <- edges[-1,]
edges$Weight[which(edges$Weight==4)] = 2

# create ids for edge list
names = unique(c(as.character(edges[,1]), as.character(edges[,2])))
n = length(names)
id = paste("p", 1:n, sep="")
cross = data.frame(Source=names, id_source=id, stringsAsFactors=F)
cross2 = data.frame(Target=names, id_target=id, stringsAsFactors=F)
edges1 = join(cross, edges, by="Source", type="right")
edges2 = join(cross2, edges1, by="Target", type="right")
edges = edges2[,-c(1:2)]
names(edges) = c("Weight", "Source", "Target")
# add direction type
edges$Type = "Undirected"


#create node table
node = data.frame(Id = id, Label=names, stringsAsFactors=F)
nodes = join(attr, node, by="Label", type="right", "all")
nodes$Label = gsub("_", " ", nodes$Label)
nodes$Score = as.numeric(nodes$Score)
nodes$Age = as.integer(nodes$Age)
per = (nodes$Total1400/ nodes$Total) * 100
per[is.na(per)] = 0
nodes$p1400 =  round(per, 1)

# subset node and edges by people who have at least one connection
nodes = nodes[which(nodes$Total>1),]
final_ids = nodes$Id
arg4 = final_ids %in% edges$Target
edges = edges[arg4,]
# write to file
write.csv(edges, "edges.csv", row.names=F)
write.csv(nodes, "nodes.csv", row.names=F)
