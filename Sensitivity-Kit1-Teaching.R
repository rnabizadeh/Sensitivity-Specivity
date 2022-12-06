
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
 
# 0-8 
table(Data.Kit1, Data.Titr)
dat <- as.table(matrix(c(19,106,2,310), nrow = 2, byrow = TRUE))
colnames(dat) <- c("Titr+","Titr-")
rownames(dat) <- c("Kit+","Kit-")
dat
rval <- epi.tests(dat, conf.level = 0.95)
A = rval
OUT = A$tab
colnames(OUT) <- c("Titr+","Titr-","Total")
rownames(OUT) <- c("Kit+","Kit-","Total")
OUT
print(rval) ; summary(rval)




