library(XML)
library(ggplot2)
library(reshape2)
library(animint) 
library(rCharts)
library(NbClust)
#library(gdata) #commented for the ggplots at the very end.. needed for cbindX in the middle of the code

URL="http://www.usatoday.com/sports/college/Schools/finances/"
tablesfromURL = readHTMLTable(URL)  
findata=tablesfromURL[[1]]
save(findata,file="findataraw.rda")

load("findataraw.rda")

findata=findata[,-6] # removing column on % subsidy (of total revenue)
names(findata)=c("Rank","School","Total.Revenue","Total.Expenses", "Total.Subsidy")
for (i in 3:5){findata[,i] = gsub("[[:punct:]]", "",findata[,i])}
for (i in 3:5){findata[,i] = as.numeric(findata[,i])}
findata$Rank = as.numeric(levels(findata$Rank))[findata$Rank]
findata$Revenue.Less.Expenses= (findata$Total.Revenue)-(findata$Total.Expenses)

# let's generate the table
tab1=dTable(findata, sPaginationType = "full_numbers")
tab1$publish('NCAA College Finance Data',host='gist')
#tab1 published at: http://www.pagist.info/7023937

# let's generate the heatmap after dropping the column on Rank for this purpose
findatamelt=ddply(melt(findata[,-1]),.(variable),transform,rescale=rescale(value))
names(findatamelt)=c("School","Variable","value","rescale")
hmap <- rPlot(Variable ~ School, color = 'rescale', data = findatamelt, type = 'tile')
hmap$addParams(height = 400, width=1000)
hmap$guides(reduceXTicks = FALSE)
hmap$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
hmap$guides(y = list(numticks = length(unique(findatamelt$School))))
hmap$guides(x = list(numticks = 5))
hmap$publish("Heatmap of College Finance Data",host="gist")
# published at: http://www.pagist.info/7023956

# Correlation matrix of variables (we remove the School Column this time from findata)
corrmatrix<-cor(findata[,-2]) #store corr matrix
corrdata=as.data.frame(corrmatrix)
corrdata$Variable1=names(corrdata)
corrdatamelt=melt(corrdata,id="Variable1")
names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
corrmatplot$addParams(height = 400, width=800)
corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
corrmatplot$guides(x = list(numticks = 3))
corrmatplot$addParams(staggerLabels=TRUE)
corrmatplot$publish('Correlation Matrix of College Finance Data',host="gist")
# Published at: http://www.pagist.info/7023971

################ Clustering (Quick reference: Quick-R, Kabacoff, http://www.statmethods.net/advstats/cluster.html)
# Determining number of clusters - Tal Galili's post based on the Kabacoff book - http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within Groups sum of squares")}

wssplot(findata[-c(1:2)])
########
set.seed(123)
nc <- NbClust(findata[-c(1:2)], min.nc=2, max.nc=15, method="kmeans") # 4 factors based on Hubert index and the D index
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,])) 

########## 4 clusters#########

set.seed(123)
kmeansdata=kmeans(findata[-c(1:2)],4)  # Decided on 4 clusters
# get cluster means 
meanvars=aggregate(findata[-c(1:2)],by=list(kmeansdata$cluster),FUN=mean)
# append cluster assignment

findata <- data.frame(findata, kmeansdata$cluster)

names(findata)=c("Rank","School","Total.Revenue","Total.Expenses","Total.Subsidy","Revenue.Less.Expenses", "Group") 

# Display table of Group membership

gp1=findata[which(findata$Group==1), ]
gp2=findata[which(findata$Group==2), ]
gp3=findata[which(findata$Group==3), ]
gp4=findata[which(findata$Group==4), ]

gp1$School=factor(gp1$School)
gp2$School=factor(gp2$School)
gp3$School=factor(gp3$School)
gp4$School=factor(gp4$School)

gp1=gp1[c(2)]
gp2=gp2[c(2)]
gp3=gp3[c(2)]
gp4=gp4[c(2)]
SchoolsGroup=cbindX(gp1,gp2,gp3,gp4)
names(SchoolsGroup)=c("Group 1","Group 2", "Group 3", "Group 4")

tab2=dTable(SchoolsGroup, sPaginationType = "full_numbers")
tab2$templates$script ="C:/Users/patil/Dropbox/Public/ncaa/chart_customsort.html"
tab2$params$table$aoColumns =
  list(
    list(sType = "string_ignore_null", sTitle = "Group 1"),
    list(sType = "string_ignore_null", sTitle = "Group 2"),
    list(sType = "string_ignore_null", sTitle = "Group 3"),
    list(sType = "string_ignore_null", sTitle = "Group 4")
  )
tab2
tab2$publish('NCAA Colleges Grouped',host='gist')
# published at: http://www.pagist.info/7028781

#Group 1: Factor w/ 19 levels "Alabama","Arkansas",..: 17 11 9 1 4 18 8 15 12 3 ...
#$ Group 2: Factor w/ 61 levels "Air Force","Akron",..: 12 29 1 45 6 35 57 10 54 43 ...
#$ Group 3: Factor w/ 33 levels "Arizona","Arizona State",..: 13 14 27 16 32 22 20 30 33 8 ...
#$ Group 4: Factor w/ 115 levels "Alabama A&M",..: 11 110 57 70 84 50 81 49 5 34 ...

# Properties of the 4 Groups

############## Parallel Plot#############
names(meanvars)=c("Group","Total.Revenue","Total.Expenses","Total.Subsidy","Revenue.Less.Expenses")
parrSchool <- rCharts$new()
parrSchool$field('lib', 'parcoords')
parrSchool$set(padding = list(top = 25, left = 5, bottom = 10, right = 0), width=800, height=400)
parrSchool$set(data = toJSONArray(meanvars, json = F), 
                colorby = 'Total.Revenue', 
                range = range(meanvars$Total.Revenue),
                colors = c('red','blue')
)
parrSchool$setLib("parcoords")
parrSchool
# used html code of generated page and modify dependencies on the JS files-  iframe link: https://dl.dropboxusercontent.com/u/56796392/ncaa/parallelplot.html


# Create a variable - Details - which will provide the tooltip information in the next set of plots

findata$Details=paste("School:",findata$School," Group:",findata$Group," Total.Revenue:",findata$Total.Revenue," Total.Expenses:",findata$Total.Expenses,
                                                          " Total.Subsidy:",findata$Total.Subsidy,
                                              " Revenue.Less.Expenses:",findata$Revenue.Less.Expenses," Rank:",
                            findata$Rank)

findata$Group=as.factor(findata$Group) # To color code plots based on group membership


# Stitching multiple ggplots together using animint... amazing stuff ### Note that gdata messes with the reordering... 
# so I had to restart the R session and avoid launching that (http://stackoverflow.com/questions/3744178/ggplot2-sorting-a-plot).

ncaaplots=list(
rev=ggplot()+geom_bar(aes(y=Total.Revenue,x=reorder(School,Total.Revenue),clickSelects=Details),alpha=.6,data=findata,stat="identity", position="identity")+coord_flip()+ggtitle("Total Revenues")+ xlab("Schools Ordered by Total Revenue")+theme(axis.line.y=element_blank(), axis.text.y=element_blank(), 
                                                                                                                                                                                                                axis.ticks.y=element_blank(), axis.title.y=element_blank()),
exp=ggplot()+geom_bar(aes(y=Total.Expenses,x=reorder(School,Total.Expenses),clickSelects=Details), alpha=.6,data=findata,stat="identity", position="identity")+theme(axis.text.x = element_blank())+coord_flip()+ggtitle("Total Expenses")+  xlab("Schools Ordered by Total Expenses") + theme(axis.line.y=element_blank(), axis.text.y=element_blank(), 
                                                                                                                                                                                                                                                        axis.ticks.y=element_blank(), axis.title.y=element_blank()),
sub=ggplot()+geom_bar(aes(y=Total.Subsidy,x=reorder(School,Total.Subsidy),clickSelects=Details), alpha=.6,data=findata,stat="identity", position="identity")+theme(axis.text.x = element_blank())+coord_flip()+ggtitle("Total Subsidy")+xlab("Schools Ordered by Total Subsidy") + theme(axis.line.y=element_blank(), axis.text.y=element_blank(), 
                                                                                                                                                                                                                                              axis.ticks.y=element_blank(), axis.title.y=element_blank()),
revsub=ggplot()+geom_point(aes(y=Total.Revenue,x=Total.Subsidy,clickSelects=Details,size=Total.Expenses, fill=Group ),data=findata)+ggtitle("Revenue Versus Subsidy")+guides(fill="none"),
expsub=ggplot()+geom_point(aes(x=Total.Subsidy,y=Total.Expenses,clickSelects=Details, size=Total.Revenue, fill=Group),data=findata)+ggtitle("Expenses Versus Subsidy")+guides(fill="none"),
profit=ggplot()+geom_point(aes(x=Total.Subsidy,y=Revenue.Less.Expenses,clickSelects=Details, size=Total.Revenue, fill=Group),data=findata)+ggtitle("Subsidy Versus Revenue less Expenses)")+guides(fill="none")
)
gg2animint(ncaaplots,"gginteractive")


################################################
