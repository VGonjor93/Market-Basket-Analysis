#Installing packages ------
install.packages("arules")
install.packages("arulesViz") 
install.packages("caTools")
install.packages("colorspace")
install.packages("tabulizer")
install.packages("dplyr")
install.packages("RColorBrewer")

library(RColorBrewer)
library(caret)
library(readr)
library(DataExplorer)
library(arules)
library(arulesViz)
library(caTools)
library(colorspace)
library(reshape2)

#Importing data -----

trans <- read.transactions("C:/Users/poni6/Desktop/Data Analysis/Modulo 2/Task 4/Trans.csv", format = "basket", sep = ",", rm.duplicates=TRUE)
productType <- read_csv("C:/Users/poni6/Downloads/ElectronidexItems2017.csv")
View(productType)
summary(trans)

#Data exploration -----

itemFrequencyPlot(trans, type="absolute", col='steelblue3', topN = 10, xlab = "Product", ylab = "Sales frequency",  main="Top 10 most frequently bought items")

transSize <- size(trans)
histogram(transSize, col='steelblue3', breaks = 30, xlab = "Number of items per transaction", ylab = "Percentage of transactions", main = "Frequency of transactions based on the amount of items")

#Fuck this
#par(mar=c(2.5,16,0,1), mfrow=c(1,1))
#(sort(table(unlist(LIST(trans))))[1:10],
#        horiz=TRUE,
#        las=1,
#        col='steelblue3',
#        xlab='', )

image(sample(trans, 100))
ct <- crossTable(trans)
ct["Logitech Wireless Keyboard", "Logitech Wireless Keyboard"]
ct["Panasonic On-Ear Stereo Headphones", "Panasonic On-Ear Stereo Headphones"]
ct["Canon Ink", "Canon Ink"]
ct["Logitech Stereo Headset","Logitech Stereo Headset"]
ct["Ethernet Cable","Ethernet Cable"]
ct["Canon Office Printer","Canon Office Printer"]
ct["Audio Cable", "Audio Cable"]

trans_1item <- trans[which(size(trans)==1),]
ct_1tem <- crossTable(trans_1item)
itemFrequencyPlot(trans_1item, type="absolute", col='steelblue3', topN = 5, xlab = "Product", ylab = "Times sold",  main="Top 5 most frequently bought alone items")
ct_1tem["Apple MacBook Air", "Apple MacBook Air"]
ct_1tem["Apple Earpods", "Apple Earpods"]
ct_1tem["iMac","iMac"]
ct_1tem["CYBERPOWER Gamer Desktop", "CYBERPOWER Gamer Desktop"]
ct_1tem["Apple MacBook Pro", "Apple MacBook Pro"]


#Calculating volume of sales
trans_df <- as.data.frame(itemFrequency(trans) * nrow(trans))
colnames(trans_df) <- "Volume"
trans_df$Product_name <- row.names(trans_df)
trans_df <- trans_df[c(2, 1)]
rownames(trans_df) <- NULL

#Creating rules ----- 

aprioriRules <- apriori (trans, parameter = list(supp = 0.01, conf = 0.1, minlen = 2))
inspect(sort(aprioriRules, by = "lift")[1:10])
inspect(sort(aprioriRules, by = "support")[1:10])
inspect(sort(aprioriRules, by = "confidence")[1:10])
summary(aprioriRules)

bussinessRules <- apriori (trans, parameter = list(supp = 0.001, conf = 0.1, minlen = 5))
inspect(sort(bussinessRules, by = "lift")[1:10])
inspect(sort(bussinessRules, by = "support")[1:10])
inspect(sort(bussinessRules, by = "confidence")[1:10])
summary(bussinessRules)
#Plotting rules -----
plot(aprioriRules)
plot(aprioriRules, measure=c("support", "lift"), shading="confidence")
plot(aprioriRules, measure=c("lift", "confidence"), shading="order", col = c("red","blue", "green", "orange"), control=list(main = "Two-key plot"))
plot(aprioriRules[1:10], method="matrix", measure="lift")
plot(aprioriRules, method="grouped", measure="support", shading ="confidence")
plot(aprioriRules, method="grouped", measure="confidence", control=list(col=sequential_hcl(100)))
plot(aprioriRules[1:10], method="graph", control=list(type="items"))
plot(aprioriRules, method="paracoord", control=list(reorder=TRUE))
plot(aprioriRules[1:10], method="paracoord", control=list(reorder=TRUE))
plot(aprioriRules,control=list(col=brewer.pal(11,"Spectral")),main="")

plot(aprioriRules@info$ntransactions)

iMacRules = subset(aprioriRules, items %in% c("iMac"))
inspect(sort(iMacRules, by = "lift")[1:10])
inspect(sort(iMacRules, by = "support")[1:10])
inspect(sort(iMacRules, by = "confidence")[1:10])

plot(iMacRules, measure=c("lift", "confidence"), shading="order", col = c("red","blue"), control=list(main = "Two-key plot"))


ViewSonicRules = subset(aprioriRules, items %in% c("ViewSonic Monitor"))
inspect(sort(ViewSonicRules, by = "lift")[1:10])
inspect(sort(ViewSonicRules, by = "support")[1:10])
inspect(sort(ViewSonicRules, by = "confidence")[1:10])

plot(ViewSonicRules, measure=c("lift", "confidence"), shading="order", col = c("red","blue"), control=list(main = "Two-key plot"))



sansMacHPrules<- apriori(trans, parameter = list(supp = 0.01, conf = 0.1, minlen = 2),
                       appearance = list(none = c("iMac", "HP Laptop")))
inspect(sort(sansMacHPrules, by = "lift")[1:10])
inspect(sort(sansMacHPrules, by = "support")[1:10])
inspect(sort(sansMacHPrules, by = "confidence")[1:10])
summary(sansMacHPrules)


#Checking for redundant rules -----

is.redundant(aprioriRules)



#Adding a "category" level to the transaction data -----

productType$ID <- seq.int(nrow(productType))
pT.m <- melt(productType, id.vars='ID')
na.omit(pT.m)
pT.m <- pT.m [order(pT.m$value),]
pT.m$ID <- NULL


category = pT.m$variable

category <- as.data.frame(category)
category$ID <- seq.int(nrow(category))
category <- subset(category, ID < 126)
category$ID <- NULL

trans@itemInfo$category <- category$category

categoryLevel <- arules::aggregate(trans, trans@itemInfo$category)


#Exploring data based on category -----
itemFrequencyPlot(categoryLevel, type="absolute", col='steelblue3', topN = 17, xlab = "Products", ylab = "Amount of times sold", main = "Product types based on times sold")

categoryCT <- crossTable(categoryLevel)
categoryCT["Desktop", "Desktop"]

#Creating rules based on category -----
categoryRules <- apriori (categoryLevel, parameter = list(supp = 0.05, conf = 0.5, minlen = 2))
inspect(sort(categoryRules, by = "lift")[1:10])
inspect(sort(categoryRules, by = "support")[1:10])
inspect(sort(categoryRules, by = "confidence")[1:10])
summary(categoryRules)


desktopRules<- apriori(categoryLevel, parameter = list(supp = 0.01, conf = 0.1, minlen = 2),
                   appearance = list(none = c("Desktop")))
inspect(sort(desktopRules, by = "lift")[1:10])
inspect(sort(desktopRules, by = "support")[1:10])
inspect(sort(desktopRules, by = "confidence")[1:10])
summary(desktopRules)

accessoryRules <- subset(categoryRules, items %in% c("Accessories", "Computer Mice", "Speakers", "Keyboard", "Active Headphones", "Mouse and Keyboard Combo", "Computer Headphones", "Computer Cords", "Computer Stands", "External Hardrives", "Smart Home Devices"))
inspect(sort(accessoryRules, by = "lift")[1:10])
inspect(sort(accessoryRules, by = "support")[1:10])
inspect(sort(accessoryRules, by = "confidence")[1:10])
summary(accessoryRules)

plot(desktopRules, measure=c("lift", "confidence"), shading="support", control=list(main = "Two-key plot"))
plot(desktopRules, measure=c("support", "confidence"), shading="lift", control=list(main = "Two-key plot"))

#Ploting rules based on category -----

arulesViz::plotly_arules(categoryRules)

plot(categoryRules, measure=c("support", "confidence"), shading="lift")
plot(categoryRules, measure=c("lift", "confidence"), shading="order", col = c("yellow","green", "lightblue", "darkblue", "purple", "pink", "red", "orange"), control=list(main = "Two-key plot"))
plot(categoryRules, method="grouped", measure="support")
plot(categoryRules, method="grouped", measure="confidence", control=list(col=sequential_hcl(100)))
plot(categoryRules[1:10], method="graph", control=list(type="items"))
plot(categoryRules, method="paracoord", control=list(reorder=TRUE))
plot(categoryRules[1:10], method="paracoord", control=list(reorder=TRUE))

#Checking for redundant rules based on category -----

is.redundant(categoryRules)
