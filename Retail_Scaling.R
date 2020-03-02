library(readxl)
retail <- read_excel("Retail.xlsx")
View(retail)
retail=retail[,-1]
rownames(retail) = c("Forever 21", "Zara", "H & M", "ROSS", "Marshall's", "T.J. Max", "Macy's", "JC Penny", "GUESS",
                   "Old Navy", "Levis")
View(retail)

options(digits=4)

library("smacof")

###############################################
# Transform similarities to dissimilarities
###############################################

# Just simply take the reverse

diss.ret <- sim2diss(retail,method="reverse",to.dist=TRUE)
diss.ret
max(diss.ret)
min(diss.ret)

##############################################
# MDS with the function cmdscale
##############################################

# Obtain the principal coordinates for k=n-1=10

MDS.ret <- cmdscale(diss.ret,k=10,eig=TRUE)
MDS.ret

# As it can be seen, only 7 eigenvalues are positive

# Obtain the precision measure for the positive eigenvalues

MDS.m <- cumsum(MDS.ret$eig[1:7]/sum(abs(MDS.ret$eig)))
MDS.m

# The first 2 principal coordinates give an accurate explanation of the data set 

# Obtain the perceptual map

plot(MDS.ret$points[,1],MDS.ret$points[,2],xlab="Principal coordinate 1",ylab="Principal coordinate 2",pch=20,col="deepskyblue2")
text(MDS.ret$points[,1],MDS.ret$points[,2],labels=rownames(MDS.ret$points),col="firebrick2",pos=1)
abline(v=0,h=0)

