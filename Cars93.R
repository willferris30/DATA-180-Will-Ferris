library("MASS")
?Cars93
head(Cars93)
ls(Cars93)
table(Cars93$AirBags)
Cars93$Type
Cars93$Manufacturer
colors()
colors()[1:20]
barplot(table(Cars93$AirBags),ylab="Frequency",cex.lab=1.3,col=c(209,32,42))
palette()
palette(c("red2", "orchid1"))
stripchart(Cars93$MPG.city, method = "stack", pch=16, + cex.axis = 1.2, cex.lab=1.2, xlab = "Miles per Gallon")
hist(Particulate,cex.lab=1.2,cex.axis=1.2,col="lightgray",+xlab="Particulate (g/gal)")
box()
