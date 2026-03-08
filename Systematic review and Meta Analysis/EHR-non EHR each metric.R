install.packages("metafor")
install.packages("forestplot")
install.packages("dplyr")
library("metafor")
library("forestplot")
library("dplyr")


#Analyse SD only


dtSDonly <- SDonly
inf <- metagen (TE = logHR, seTE = selogHR, data = SDonly, studlab = Author, sm = "HR")
inf
subEHR <- update(inf, subgroup = SDonly$Source, tau.common = FALSE)
subEHR

length(dtSDonly$Source)
length(inf$yi)

sum(is.na(SDonly$Source))  # This must be 0


#Analyse on EHR SD only
dtSDonly <- SDonly
dtSDehronly <- subset (dtSDonly, dsource == "EHR")


resSDehr <- rma(yi=logHR, sei=selogHR, data = dtSDehronly)
resSDehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resSDehr, transf=exp, digits=2)

#forest plot
forest(resSDehr)
forest(resSDehr, addpred=TRUE, header=TRUE)
print(forest(resSDehr, addpred=TRUE, header=TRUE))
forest(resSDehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resSDehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resSDehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtSDehronly$Author, sep = ", "))

#Analyse on non EHR SD only
dtSDonly <- SDonly
dtSDnonehronly <- subset (dtSDonly, dsource == "non-EHR")


resSDnonehr <- rma(yi=logHR, sei=selogHR, data = dtSDnonehronly)
resSDnonehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resSDnonehr, transf=exp, digits=2)

#forest plot
forest(resSDnonehr)
forest(resSDnonehr, addpred=TRUE, header=TRUE)
print(forest(resSDnonehr, addpred=TRUE, header=TRUE))
forest(resSDnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resSDnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resSDnonehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtSDnonehronly$Author, sep = ", "))





#Analyse VIM only


dtVIMonly <- VIMonly
inf <- metagen (dtVIMonly$logHR, dtVIMonly$selogHR, studlab = paste(dtVIMonly$Author), sm = "HR")
inf
subEHR <- update(inf, subgroup = dtVIMonly$dsource, tau.common = FALSE)
subEHR

#Analyse on EHR VIM only
dtVIMonly <- VIMonly
dtVIMehronly <- subset (dtVIMonly, dsource == "EHR")


resVIMehr <- rma(yi=logHR, sei=selogHR, data = dtVIMehronly)
resVIMehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIMehr, transf=exp, digits=2)

#forest plot
forest(resVIMehr)
forest(resVIMehr, addpred=TRUE, header=TRUE)
print(forest(resVIMehr, addpred=TRUE, header=TRUE))
forest(resVIMehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resVIMehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resVIMehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtVIMehronly$Author, sep = ", "))

#Analyse on non EHR VIM only
dtVIMonly <- VIMonly
dtVIMnonehronly <- subset (dtVIMonly, dsource == "non-EHR")


resVIMnonehr <- rma(yi=logHR, sei=selogHR, data = dtVIMnonehronly)
resVIMnonehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIMnonehr, transf=exp, digits=2)

#forest plot
forest(resVIMnonehr)
forest(resVIMnonehr, addpred=TRUE, header=TRUE)
print(forest(resVIMnonehr, addpred=TRUE, header=TRUE))
forest(resVIMnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resVIMnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resVIMnonehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtVIMnonehronly$Author, sep = ", "))



#Analyse CV only


dtCVonly <- CVonly
inf <- metagen (dtCVonly$logHR, dtCVonly$selogHR, studlab = paste(dtCVonly$Author), sm = "HR")
inf
subEHR <- update(inf, subgroup = dtCVonly$Source, tau.common = FALSE)
subEHR

#Analyse on EHR CV only
dtCVonly <- CVonly
dtCVehronly <- subset (dtCVonly, dsource == "EHR")


resCVehr <- rma(yi=logHR, sei=selogHR, data = dtCVehronly)
resCVehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resCVehr, transf=exp, digits=2)

#forest plot
forest(resCVehr)
forest(resCVehr, addpred=TRUE, header=TRUE)
print(forest(resCVehr, addpred=TRUE, header=TRUE))
forest(resCVehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resCVehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resCVehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtCVehronly$Author, sep = ", "))

#Analyse on non EHR CV only
dtCVonly <- CVonly
dtCVnonehronly <- subset (dtCVonly, dsource == "non-EHR")


resCVnonehr <- rma(yi=logHR, sei=selogHR, data = dtCVnonehronly)
resCVnonehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resCVnonehr, transf=exp, digits=2)

#forest plot
forest(resCVnonehr)
forest(resCVnonehr, addpred=TRUE, header=TRUE)
print(forest(resCVnonehr, addpred=TRUE, header=TRUE))
forest(resCVnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resCVnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resCVnonehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtCVnonehronly$Author, sep = ", "))

#Analyse ARV only


dtARVonly <- ARVonly
inf <- metagen (dtARVonly$logHR, dtARVonly$selogHR, studlab = paste(dtARVonly$Author), sm = "HR")
inf
subEHR <- update(inf, subgroup = dtARVonly$Source, tau.common = FALSE)
subEHR

#Analyse on EHR ARV only
dtARVonly <- ARVonly
dtARVehronly <- subset (dtARVonly, dsource == "EHR")


resARVehr <- rma(yi=logHR, sei=selogHR, data = dtARVehronly)
resARVehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resARVehr, transf=exp, digits=2)

#forest plot
forest(resARVehr)
forest(resARVehr, addpred=TRUE, header=TRUE)
print(forest(resARVehr, addpred=TRUE, header=TRUE))
forest(resARVehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resARVehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resARVehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtARVehronly$Author, sep = ", "))

#Analyse on non EHR ARV only
dtARVonly <- ARVonly
dtARVnonehronly <- subset (dtARVonly, dsource == "non-EHR")


resARVnonehr <- rma(yi=logHR, sei=selogHR, data = dtARVnonehronly)
resARVnonehr
#predicted pooled risk ratio and corresponding CI/PI
predict(resARVnonehr, transf=exp, digits=2)

#forest plot
forest(resARVnonehr)
forest(resARVnonehr, addpred=TRUE, header=TRUE)
print(forest(resARVnonehr, addpred=TRUE, header=TRUE))
forest(resARVnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resARVnonehr, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resARVnonehr, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtARVnonehronly$Author, sep = ", "))
