 45## IEMS 462-1 project

sales=read.csv("Documents/IEMS462/Project/catalog sales data.csv")
## deleting inconsistent data e.g.falord + sprord is not equal to ordhist (From hit 1)
newsales<-subset(sales, sales$falord+sales$sprord==sales$ordhist)

## deleting inconsistent data e.g."the number of orders are not recorded but there are sales amounts" (From hit 2)
newsales1<- subset(newsales, (newsales$ordhist != 0 & newsales$slshist!=0)| (newsales$ordhist == 0 & newsales$slshist==0))
save(newsales,file="Documents/IEMS462/Project/catalog sales data.Rdata")
load("/Users/huazhenzhao/Documents/IEMS462/Project/catalog sales data.Rdata")  