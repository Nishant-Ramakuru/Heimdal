library("SocialMediaLab")
library("igraph")
anuser<-function(uname,numtw = 500){
#anunm<-paste("from:",uname,sep="")  
ts<-CollectDataTwitter(uname,numTweets = numtw)
ats<<-ts
}
#Actor network , no extra paramaters
actor<-function(remv = 6){
tsp<-Create(ats,type="Actor")
bad.vs<-V(tsp)[degree(tsp)<remv]
tsp<-delete.vertices(tsp, bad.vs)
V(tsp)$color<-ifelse(V(tsp)$name=='CA', 'blue', 'red')
E(tsp)$color<-ifelse(E(tsp)$grade==9, "red", "grey")
E(tsp)$color<-ifelse(E(tsp)$spec=='X', "red", ifelse(E(tsp)$spec=='Y', "blue", "grey"))
par(mai=c(0,0,1,0)) 
plot(tsp,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Actor Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=0.5,			#the font of the name labels
     vertex.label=V(tsp)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.7,			#specifies the size of the font of the labels. can also be made to vary
     edge.arrow.size =0.4   #to make arrow size smaller
)
} 
#Bimodal network,
bimodal<-function(remv=6){
tsp<-Create(ats,type="Bimodal")
#V(ts)
#E(ts)
#degree(ts)
bad.vs<-V(tsp)[degree(tsp)<remv]
#tsp<-delete.vertices(tsp, bad.vs)
#for solo analysis
V(tsp)$color<-ifelse(V(tsp)$name=='CA', 'blue', 'red')
E(tsp)$color<-ifelse(E(tsp)$grade==9, "red", "grey")
E(tsp)$color<-ifelse(E(tsp)$spec=='X', "red", ifelse(E(tsp)$spec=='Y', "blue", "grey"))
par(mai=c(0,0,1,0)) 
plot(tsp,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Bimodal Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(tsp)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)
} 
 
# Semantic network, add always termFreq & hashtagFreq , can remove terms or hashtags
semantic<-function(tf=20,htf=80,remv=6){
tsp<-Create(ats,type="Semantic",termFreq = tf, hashtagFreq = htf)
V(tsp)
E(tsp)
#degree(ts)
bad.vs<-V(tsp)[degree(tsp)<remv]
tsp<-delete.vertices(tsp, bad.vs)
 #for solo analysis
V(tsp)$color<-ifelse(V(tsp)$name=='CA', 'blue', 'red')
E(tsp)$color<-ifelse(E(tsp)$grade==9, "red", "grey")
E(tsp)$color<-ifelse(E(tsp)$spec=='X', "red", ifelse(E(tsp)$spec=='Y', "blue", "grey"))
par(mai=c(0,0,1,0)) 
plot(tsp,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	#see the igraph documentation for details
     main='Semantic Network',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(tsp)$name,		#species the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)
}
