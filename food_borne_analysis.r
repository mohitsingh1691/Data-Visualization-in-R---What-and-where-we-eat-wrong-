library(alluvial)library(sqldf)
library(RColorBrewer)
library(ggplot2)
library(fmsb)
library(reshape2)
fd <-read.csv(“FoodData.csv”)
str(fd)
fdo <-read.csv(“fd_outbreak.csv”)

#grid bubble
colnames(fdo)[10] <- “cases1”
colnames(fdo)[2] <- “Vehicle”
fdo_bpp <- sqldf(“SELECT *, SUM(cases1) as cases FROM fdo GROUP BY Microorganism”)
unique(fdo_bpp$Vehicle)
fdo_bpp <-fdo_bpp[!fdo_bpp$Vehicle == “Unknown”, ]
fdo_bpp <-fdo_bpp[!fdo_bpp$Vehicle == “Multiple foods”, ]
fdo_bpp <-fdo_bpp[!fdo_bpp$Microorganism == “Unknown”, ]
ll <-c(“Salad,chicken”, “Custard”,”Fish”,”Meat”,”Pastry”,”Pizza”,”Rasberries”,”Rice”,”Salad”,”Sandwich”,”Shrimp”,”Turkey”)
# ss<- subset(fdo_bpp, Vehicle==ll)
#fdo_bpp<-subset(fdo_bpp, Vehicle %in% ll)
#View(ss)
fdo_bpp <-head(fdo_bpp[order(fdo_bpp$cases, decreasing=TRUE), ], 20)
fdo_bpp$radius <- sqrt(sqrt( fdo_bpp$cases/ pi ))

ggplot(fdo_bpp,aes(fdo_bpp$Microorganism,fdo_bpp$Vehicle))+
  geom_point(aes(size=radius*7.5),shape=21,fill=”#e3a75d”)+
             scale_size_identity()+
               theme(panel.grid.major=element_line(linetype=5,color=”#787794"),
                                                   panel.background = element_rect(fill = “#19153a”),axis.text.x=element_text(angle=90,hjust=1,vjust=0))
                                                                                   #which country it occurred? where it occured? what did they eat there?
                                                                                   colnames(fdo)[10] <- “cases”
                                                                                   colnames(fdo)[2] <- “Vehicle”
                                                                                   fdo1 <- sqldf(“SELECT *, SUM(cases) AS cases FROM fdo GROUP BY Country, Vehicle”)
                                                                                   fdo1 <-fdo1[!fdo1$Vehicle == “Multiple foods”, ]
                                                                                   fdo1 <-fdo1[!fdo1$Vehicle == “Unknown”, ]
                                                                                   fdo1 <-fdo1[!fdo1$Setting == “Unknown”, ]
                                                                                   #fdo[‘cases’] <-sqrt(fdoo[‘cases’])
                                                                                   #fdo <-fdo[which(fdo$Country==’USA’ 
                                                                                   # | fdo$Country==’Canada’ |fdo$Country==’Brazil’), ]
                                                                                   
                                                                                   cutoff <-2
                                                                                   fdo2 <-subset(fdo1, ave(ID., Country, FUN = length) > cutoff)
                                                                                   
                                                                                   cutoff2 <-3
                                                                                   fdo2 <-subset(fdo2, ave(ID., Setting, FUN = length) > cutoff2)
                                                                                   cutoff1 <-1
                                                                                   fdo2 <-subset(fdo2, ave(ID., Vehicle, FUN = length) > cutoff1)
                                                                                   fdo21 <-fdo2[,’Vehicle’]
                                                                                   unique(fdo21)
                                                                                   vl <-unique(fdo2$Vehicle)
                                                                                   vl
                                                                                   k = ifelse(fdo2$Setting==”Airplane”, “#ab5796”, 
                                                                                              ifelse(fdo2$Setting==”Bakery”, “#513082”, 
                                                                                                     ifelse(fdo2$Setting==”Camp”, “#de826b”,
                                                                                                            ifelse(fdo2$Setting==”Catered function”, “#30ab7c”,
                                                                                                                   ifelse(fdo2$Setting==”Home”, “#b8cc64”,
                                                                                                                          ifelse(fdo2$Setting==”Hotel”, “#5ea381”,
                                                                                                                                 ifelse(fdo2$Setting==”Restaurant”, “#db5072”,
                                                                                                                                        ifelse(fdo2$Setting==”School”, “#bfd7b5”,”#f9f8f7")
                                                                                                                                               
                                                                                                                                        )))))))
                                                                                              unique(fdo2$Vehicle)
                                                                                              alluvial(fdo2[,c(‘Country’,’Setting’)], freq=fdo2$cases, col = k, hide=fdo2$cases < 20, alpha=.6, border=FALSE)
                                                                                              alluvial(fdo2[,c(‘Setting’,’Vehicle’)], freq=fdo2$cases, col = k, hide=fdo2$cases < 20, alpha=.6)
                                                                                              #Radar Plot
                                                                                              # Library
                                                                                              library(fmsb)
                                                                                              library(reshape2)
                                                                                              # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
                                                                                              # The default radar chart proposed by the library:
                                                                                              #California
                                                                                              colnames(fd)[7] <- “loc”
                                                                                              fd1 <-subset(fd, State==’California’)
                                                                                              #fd1 <- sqldf(“SELECT * sum(Illnesses) as Illnesses FROM fd1 GROUP BY State”)
                                                                                              fd1 <- sqldf(“SELECT * FROM fd1 GROUP BY loc”)
                                                                                              fd1 <- fd1[order(-fd1$Illnesses), ]
                                                                                              #fd1 <-fd1[row.names(unique(fd1[,c(“loc”)])),]
                                                                                              
                                                                                              fd1 <-fd1[1:5, ]
                                                                                              rownames(fd1) <- fd1$loc
                                                                                              fd2 <-t(fd1)
                                                                                              View(fd2)
                                                                                              fd2 <-as.data.frame(fd2)
                                                                                              include_list <-c(“Illnesses”,”Hospitalizations”, “Deaths”)
                                                                                              df3 <-subset(fd2, rownames(fd2) %in% include_list)
                                                                                              df3 <-as.data.frame(df3)
                                                                                              colnames(df3) <-c(“a”,”b”,”c”,”d”,”e”) 
                                                                                              #par(mar=c(1, 2, 2, 1)) #decrease default margin
                                                                                              #layout(matrix(1:4, ncol=2)) #draw 4 plots to device
                                                                                              #max(df3)
                                                                                              #df3<- rbind(rep(20,6), rep(0,6),df3)
                                                                                              lapply(df3, as.numeric)
                                                                                              View(df3)
                                                                                              df3$a <- as.numeric(as.character(df3$a)) 
                                                                                              df3$b <- as.numeric(as.character(df3$b)) 
                                                                                              df3$c <- as.numeric(as.character(df3$c)) 
                                                                                              df3$d <- as.numeric(as.character(df3$d)) 
                                                                                              df3$e <- as.numeric(as.character(df3$e))
                                                                                              radarchart(df3, maxmin = F)
                                                                                              #California
                                                                                              colnames(fd)[7] <- “loc”
                                                                                              fd1 <-subset(fd, State==’Texas’)
                                                                                              #fd1 <- sqldf(“SELECT * sum(Illnesses) as Illnesses FROM fd1 GROUP BY State”)
                                                                                              fd1 <- sqldf(“SELECT * FROM fd1 GROUP BY loc”)
                                                                                              fd1 <- fd1[order(-fd1$Illnesses), ]
                                                                                              #fd1 <-fd1[row.names(unique(fd1[,c(“loc”)])),]
                                                                                              
                                                                                              fd1 <-fd1[1:6, ]
                                                                                              rownames(fd1) <- fd1$loc
                                                                                              fd2 <-t(fd1)
                                                                                              fd2 <-as.data.frame(fd2)
                                                                                              include_list <-c(“Illnesses”,”Hospitalizations”, “Deaths”)
                                                                                              df3 <-subset(fd2, rownames(fd2) %in% include_list)
                                                                                              df3 <-as.data.frame(df3)
                                                                                              colnames(df3) <-c(“a”,”b”,”c”,”d”,”e”,”f”) 
                                                                                              #par(mar=c(1, 2, 2, 1)) #decrease default margin
                                                                                              #layout(matrix(1:4, ncol=2)) #draw 4 plots to device
                                                                                              #max(df3)
                                                                                              #df3<- rbind(rep(20,6), rep(0,6),df3)
                                                                                              lapply(df3, as.numeric)
                                                                                              df3[is.na(df3)] <- 0
                                                                                              
                                                                                              df3$a <- as.numeric(as.character(df3$a)) 
                                                                                              df3$b <- as.numeric(as.character(df3$b)) 
                                                                                              df3$c <- as.numeric(as.character(df3$c)) 
                                                                                              df3$d <- as.numeric(as.character(df3$d)) 
                                                                                              df3$e <- as.numeric(as.character(df3$e)) 
                                                                                              df3$f <- as.numeric(as.character(df3$f))
                                                                                              radarchart(df3, maxmin = F)
                                                                                              dd <-dcast(fd, Illnesses ~ loc, value.var=”Illnesses”)
                                                                                              # Custom the radarChart 
                                                                                              colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
                                                                                              df3=rbind(rep(200,20) , rep(0,5) , df3)
                                                                                              radarchart(df3 , axistype=1 , 
                                                                                                         #custom the grid
                                                                                                         cglcol=”#787794", cglty=1, axislabcol=”#787794", caxislabels=seq(0,40,10), cglwd=0.4,
                                                                                                         
                                                                                                         #custom polygon
                                                                                                         pcol=colors_in,
                                                                                                         pfcol=colors_in, 
                                                                                                         plwd=3, 
                                                                                                         
                                                                                                         #custom labels
                                                                                                         vlcex=0.8
                                                                                              )