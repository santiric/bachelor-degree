
#Capitolo 2, sezione 2.3

#age age in years
#sex
#bmi body mass index

#bp average blood pressure

#s1 tc, total serum cholesterol

#s2 ldl, low-density lipoproteins

#s3 hdl, high-density lipoproteins

#s4 tch, total cholesterol / HDL

#s5 ltg, possibly log of serum triglycerides level

#s6 glu, blood sugar level


dataset=diabetes
Y=dataset[,c(1,2)]
X=dataset[,-c(1,2,11)]
library(CCA)



X.sd=scale(X)
Y.sd=scale(Y)
first.cca=cc(X.sd,Y.sd)
B=as.matrix(cca.sd$xcoef)
U=as.matrix(X.sd)%*%as.matrix(cca.sd$xcoef)
U[1:10,]
first.cca$scores$xscores[1:10,]
library(MASS)
Null(B) 
round(t(B) %*% Null(B),5) #left nullspace

#invarianza rispetto a trasformaizioni lineari:
lin.tr.cca=cc(first.cca$scores$xscores,first.cca$scores$yscores)
lin.tr.cca$cor
first.cca$cor
lin.tr.cca$xcoef#U stesso

U.tilde=as.matrix(X.sd)%*%as.matrix(cbind(cca.sd$xcoef,Null(B)))
U.tilde #ha le prime colonne di U


tilde.cca=cc(U.tilde,first.cca$scores$yscores)
tilde.cca$cor
round(tilde.cca$xcoef,5) #non cambiamo i risultati usando U tilde

#prova

