library(ccgarch)
library(fGarch)
# estimate univariate GARCH models to get starting values

f1 = garchFit(~garch(1,1), data=data_2[,6],include.mean=FALSE)
f1<-f1@fit$coef
f2 = garchFit(~garch(1,1), data=data_2[,8],include.mean=FALSE)
f2<-f2@fit$coef
data_dcc=data_2[,c(6,8)]

# create vectors and matrices of starting values

a=c(f1[1],f2[1])
a
A=diag(c(f1[2],f2[2]))
A
B=diag(c(f1[3],f2[3]))
dccpara=c(0.2,0.6)

dccresults=dcc.estimation(inia=a,iniA=A,iniB=B,ini.dcc=dccpara,dvar=data_dcc,model="diagonal")
dcc_brent_wti<-dccresults$DCC
plot(dcc_brent_wti[,2],type="l")




library(rugarch)
library(rmgarch)
library(xts)

spec = ugarchspec()
mspec = multispec( replicate(2, spec) )
mspec
# note that replicate(spec, 2) does not

# or simply combine disparate objects
spec1 = ugarchspec(distribution = "norm")
spec2 = ugarchspec(distribution = "std")
mspec = multispec( c( spec1, spec2 ) )

dccspec(mspec)

data_dcc<-as.xts(data_3)
dccfit(dccspec, data_2[,9])

spec = ugarchspec(data_2[,6])
spec
