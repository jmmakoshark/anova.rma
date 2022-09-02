library(data.table)
library(metafor)

dat=fread("anova_rma_ex.csv",header=TRUE,sep=",")

MILK=dat[dat$Abbr=="MILK",]

yi_MILK=as.matrix(MILK$H2)

vi_MILK=as.matrix(MILK$Std_Error)

author_MILK=as.matrix(MILK$Author)

est_MILK3 <- rma.mv(yi=yi_MILK, V=vi_MILK, method="REML", random = ~ 1 | author_MILK/yi_MILK)
est_MILK2 <- rma.mv(yi=yi_MILK, V=vi_MILK, method="REML", random = ~ 1 | author_MILK/yi_MILK, sigma2=c(0,NA))
anova(est_MILK2,est_MILK3)

as.data.frame(anova(est_MILK2,est_MILK3))





