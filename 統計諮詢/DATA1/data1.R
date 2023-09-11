
###### Read data ######
table1=read.csv("/Users/lvdaxia/Desktop/碩二上/統計諮詢/data1/data1.csv",header = TRUE)
table1=replace(table1,is.na(table1),0)
table1=as.data.frame(table1)

###### Rename columns ######
dates=c("2009-02","2009-04","2009-06","2009-08","2009-10","2009-12","2010-02","2010-04","2010-06","2010-08","2010-10","2010-12","2011-02","2011-04","2011-06","2011-08","2011-10","2011-12","2012-02","2012-04","2012-06","2012-08","2012-10","2012-12")
names(table1)=c("Order","family",dates)

###### Table2 (sum of order time-step=2 month) ######
orders=unique(table1[,1]) 
table2=data.frame()
for (i in 1:length(orders)) {
  colsums=colSums(table1[table1[,"Order"]==orders[i],c(-1,-2)], na.rm = TRUE)
  table2=rbind(table2,colsums)
}
rownames(table2)=orders
colnames(table2)=dates
###### Table3 (sum of order time-step=4 month) ######
table3=data.frame()
for (o in orders) {
  newrow=c()
  for (i in 1:(length(table2)/2)) {
    newrow[i]=rowSums(table2[o,i:(i+1)])
  }
  table3=rbind(table3,newrow)
}
rownames(table3)=orders
###### Table4 (sum of order time-step=6 month) ######
table4=data.frame()
for (o in orders) {
  newrow=c()
  for (i in 1:(length(table2)/3)) {
    newrow[i]=rowSums(table2[o,i:(i+2)])
  }
  table4=rbind(table4,newrow)
}
rownames(table4)=orders
###### Plot different orders ######
table_plot=function(table){
  plot(y=table[1,],x=c(1:length(table)),type = "l",col=1,xlab = "dates",ylab = "count", ylim = c(0,max(table)))
  for (i in c(2:length(table))) {
    lines(y=table[i,],x=c(1:length(table)),type = "l",col=i)
  }
  order_in_English=c("Hemiptera", "Hymenoptera", "Diptera", "Odonata", "Coleoptera", 
                     "Orthoptera", "Thysanoptera", "Neuroptera", "Lepidoptera", 
                     "Isoptera", "Blattodea", "Trichoptera", "Psocoptera", 
                     "Phasmatodea", "Embioptera", "Thysanura")
  legend("topright", legend = order_in_English, col = 1:length(orders), lty = 1:length(table), title = "Orders", cex = 0.5)
}
table_plot(table2)
table_plot(table3)
table_plot(table4)

###### Correlations within order ######
heatmap_within_order=function(o,orders=orders,table1=table1){
  species=which(table1[,"Order"]==orders[o]) 
  cormatrix=matrix(nrow = length(species),ncol=length(species))
  for (i in 1:length(species)) {
    for (j in 1:length(species)) {
      cormatrix[i,j]=cor(as.numeric(table1[i,c(-1,-2)]),as.numeric(table1[j,c(-1,-2)]))
    }
  }
  heatmap(replace(cormatrix,is.na(cormatrix),0),
          main = "Heatmap",
          xlab = "Species",
          ylab = "Species",
          Rowv = NA,
          Colv = NA,
          scale = "none",
          symm = TRUE
  )
}
heatmap_within_order(1,orders,table1)

###### Correlations with orders ######
cormatrix=matrix(nrow = length(orders),ncol=length(orders))
for (i in 1:length(orders)) {
  for (j in 1:length(orders)) {
    cormatrix[i,j]=cor(as.numeric(table2[i,]),as.numeric(table2[j,]))
  }
}
heatmap(replace(cormatrix,is.na(cormatrix),0),
        main = "Heatmap",
        xlab = "order",
        ylab = "order",
        Rowv = NA,
        Colv = NA,
        scale = "none",
        symm = TRUE
)

###### Rain and Temperature data ######
rainandtemp=read.csv("/Users/lvdaxia/Desktop/碩二上/統計諮詢/data1/rainandtemp.csv",header = FALSE)
temps=as.numeric(rainandtemp[c(-1,-2),2])
temp=c()
for (i in 1:24) {
  temp[i]=(temps[2*i-1]+temps[2*i])/2
}
rains=as.numeric(rainandtemp[c(-1,-2),3])
rain=c()
for (i in 1:24) {
  rain[i]=rains[2*i-1]+rains[2*i]
}

sum_of_insect=colSums(table2)

par(mfrow=c(2,1))
plot(rain,sum_of_insect)
plot(temp,sum_of_insect)

mat <- matrix(c(1,2,3), 3,1,byrow = T)
layout(mat, widths = 1, heights = c(1,1,5))
plot(y=rain,x=c(1:length(rain)),type = "l",col="skyblue",xaxt = "n",lwd=5)
plot(y=temp,x=c(1:length(temp)),type = "l",col="pink",xaxt = "n",lwd=5)
table_plot(table2)
