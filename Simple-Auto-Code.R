Mydata1 <- read.csv(file.choose(), header=T, sep=",") # read data.all
Mydata1
names(Mydata1)
G = c(min(Mydata1$I2), 8 , 15, 30 ,max(Mydata1$I2))
labs <- c("0-8", "8-15" , "15-30" , "30+")


Data.Titr <- cut(Mydata1$I2, breaks = G, labels=labs,include.lowest = TRUE)
Data.Titr
table(Data.Titr)


Data.Kit1 <- cut(Mydata1$Kit1, breaks = G, labels=labs,include.lowest = TRUE)
Data.Kit1
table(Data.Kit1)

table(Data.Kit1, Data.Titr)

library(epiR)
# (test+ Gold+, test+ Gold-, test- Gold+, test- Gold-)  #Guide

Min_Val = 0
Max_Val = 8
GOLD = Mydata1$I2 >=Min_Val & Mydata1$I2 <=Max_Val
S = Mydata1$Kit1 >=Min_Val & Mydata1$Kit1<=Max_Val
sum(GOLD)
sum(S)
X = table(S,GOLD)
X
# (test+ Gold+:4, test+ Gold-:2, test- Gold+:3, test- Gold-:1)  #Guide
# (X[4], X[2], x[3], X[1])
dat <- as.table(matrix(c(X[4], X[2], X[3], X[1]), nrow = 2, byrow = TRUE))
colnames(dat) <- c("Titr+","Titr-")
rownames(dat) <- c("Kit+","Kit-")
dat
rval <- epi.tests(dat, conf.level = 0.95)
rval
A = rval
OUT = A$tab
colnames(OUT) <- c("Titr+","Titr-","Total")
rownames(OUT) <- c("Kit+","Kit-","Total")
OUT
print(rval) ; summary(rval)
 
