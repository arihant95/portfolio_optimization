setwd("C:/Users/Arihant Binaykia/Desktop/FALAB")
cprices <- read.csv("C:/Users/Arihant Binaykia/Desktop/FALAB/cprices.csv")
cprices <- read.csv("C:/Users/Arihant Binaykia/Desktop/FALAB/cprices.csv",header=TRUE,sep=",")
head(cprices)
nr=nrow(cprices)
nr
return=(cprices[2:nr,]/cprices[1:(nr-1),])-1
return
avret=colMeans(return)
avret=c(0.0004593778,0.0005580266,0.0037295463,0.0005553326,-0.0009444179,0.0014527095)
avret
dmat=cov(return)
dmat
dvec=rep(0,nrow(dmat))
dvec
amat=matrix(1,nrow=nrow(dmat))
amat
amat=cbind(amat,avret,diag(1,nrow(dmat)))
amat
rp=0.0001
bvec=c(1,rp,rep(0,nrow(dmat)))
bvec
library(quadprog)
pf=solve.QP(dmat,dvec,amat,bvec,2)
pf
risk=sqrt(pf$value)
risk
rseq=seq(from=0,to=max(avret),by=0.0001)
rseq
bvec
res=matrix(0,ncol = nrow(dmat)+1,nrow = length(rseq))
rf=0.08/249
myvector=matrix(0,ncol=1,nrow=length(rseq))
for(i in 1:length(rseq))
{
  bvec=c(1,rseq[i],rep(0,nrow(dmat)))
  pf= solve.QP(dmat,dvec,amat,bvec,2)
  res[i,]=c(sqrt(pf$value),pf$solution)
  
  myvector[i,]=c((rseq[i]-rf)/sqrt(pf$value))
  
}

myvector
x=max(myvector)
x
bvec
res
library(ggplot2)
plot(rseq~res[,1],col="red")





