rm(list=ls())
options(digits=3, stringsAsFactors=F)
library("stringr")
library("plyr")
setwd("~/Dropbox/GitRepository/Celebs/")
#read in raw data from google refine
d <- read.csv("refined.csv", stringsAsFactors=F)
names(d) <- c("rank", "name", "spouse", "breakup", "dated", "canoodled", "score", "profession", "age")
# all.ppl <- unique(c(d$name, d$spouse, d$dated, d$canoodled))
# all.ppl <- all.ppl[which(all.ppl!="")]
# all.ppl <- gsub(" ", "_", unique(all.ppl))

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

#create attribute df
attr = data.frame(
				Id = paste("p", 1:n.seeds, sep=""),
				Label,
				Score,				  			  
				Rank,
				Age,
				Profession,
				Spouses = i,
				Dated = i,
				Total = i,
				Spouses1400 = i,
				Dated1400 = i,
				Total1400 = i
				)

# get lookup names of top 1400
top1400 = gsub(" ", "_", attr$Label)


for (s in 1:n.seeds){
	s = 6
	rows <- (vecs[s]:(vecs[(s+1)]-1))
	actor <- try(d[rows, ])
	name = actor$name[1]
    name = gsub(" ", "_", name)

	# clean spouses
	spouses <- gsub(" ", "_",unique(actor$spouse[which(actor$spouse!="")]))
	spouses <- spouses[which(spouses!=name)]
	# match top 1400	
	arg1 <- spouses %in% top1400
	spouses1400  = spouses[arg1]
	
	# clean dated
	dated <- c(actor$dated, actor$canoodled, actor$breakup)
	dated <- gsub(" ", "_",unique(dated[which(dated!="")]))
	dated <- dated[which(dated!=name)]

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



# start edge list
	if(s==1){
		edges = data.frame(Source="", Target="",Weight=0)
			for(m in 1:ms){
				mId = attr$Id[which(top == spouses[m])]
				df = data.frame(Source = name,Target = mId,Weight = 2)
				edges = rbind(edges, df)
			}
			for(dt in 1:dts){
				dtId = attr$Id[which(attr$Label == dated[dt])]
				df = data.frame(Source = name,Target = dtId,Weight = 1)
				edges = rbind(edges, df)
			}
		# cleanup	
		edges <- edges[-1,]
	}
# populate
	if (ms + dts !=  0){
		if(ms != 0){
			for(m in 1:ms){
				dtId = attr$Id[which(attr$Label == dated[m])]
				df = data.frame(Source = name,Target = dtId,Weight = 1)
				edges = rbind(edges, df)
			}
		}
		if(dts != 0){
			for(dt in 1:dts){
				dtId = attr$Id[which(attr$Label == dated[dt])]
				df = data.frame(Source = name,Target = dtId,Weight = 1)
				edges = rbind(edges, df)

			}
		}
	}
	print(s)
}

# create ids for edge list
names = unique(c(as.character(edges[,1]), as.character(edges[,2])))

length(names)
cross = data.frame(Source=names, id_source=Id, stringsAsFactors=F)
cross2 = data.frame(Target=names, id_target=Id, stringsAsFactors=F)
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
per[is.na(per)] = 0.00
nodes$p1400 =  round(per, 3)

# subset node and edges by people who have at least one connection
nodes = nodes[which(nodes$Total>1),]
final_ids = nodes$Id

arg4 = !final_ids %in% edges$Target | !final_ids %in% edges$Source 
edges = edges[!arg4,]


library("foreign")
missing = data.frame(
			id = read.delim("missing.txt", sep="	")[,1],
			stringsAsFactors=F
			)

missing = join(attr, missing, type='right', by='Id')
# arg5 = edges$Target in% final_ids
# nodes = nodes[arg5,] 
# write to file
write.csv(edges, "edges.csv", row.names=F)
write.csv(nodes, "nodes.csv", row.names=F)
