dt <- all_SBP_V2

allEHR <- subset (dt, Source == "EHR")
alleGFR <- subset (dt, eGFR == "No")

### SUB GROUP EHR ###
resall_EHR <- rma(yi=logHR, sei=selogHR, data = allEHR)
resall_EHR
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_EHR, transf=exp, digits=2)

### SUB GROUP DESIGN == PROSPECTIVE ###

alldesign <- subset (dt, Design == "prospective")

resall_design <- rma(yi=logHR, sei=selogHR, data = alldesign)
resall_design
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_design, transf=exp, digits=2)

### SUB GROUP eGFR > 60 ###

alleGFR <- subset (dt, eGFR == "No")

resall_eGFR <- rma(yi=logHR, sei=selogHR, data = alleGFR)
resall_eGFR
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_eGFR, transf=exp, digits=2)

### SUB GROUP PREVIOUS CVD == NO ###

allPrevCVD <- subset (dt, PrevCVD == "No")

resall_PrevCVD <- rma(yi=logHR, sei=selogHR, data = allPrevCVD)
resall_PrevCVD
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_PrevCVD, transf=exp, digits=2)

### SUB GROUP PREVIOUS FU <=5 YEARS ###

allFU5 <- subset (dt, FU5 == "<5")

resall_FU5 <- rma(yi=logHR, sei=selogHR, data = allFU5)
resall_FU5
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_FU5, transf=exp, digits=2)

### SUB GROUP PREVIOUS OBS <=3 YEARS ###

allObs3 <- subset (dt, Obs3 == "<3")

resall_Obs3 <- rma(yi=logHR, sei=selogHR, data = allObs3)
resall_Obs3
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_Obs3, transf=exp, digits=2)

### SUB GROUP HYPERTENSION ###

allHT <- subset (dt, population == "HT")

resall_HT <- rma(yi=logHR, sei=selogHR, data = allHT)
resall_HT
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_HT, transf=exp, digits=2)


### SUB GROUP DIABETES MELLITUS ###

allDM <- subset (dt, population == "DM")

resall_DM <- rma(yi=logHR, sei=selogHR, data = allDM)
resall_DM
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_DM, transf=exp, digits=2)

### SUB GROUP ELDERLY ###

allELD <- subset (dt, population == "Elderly")

resall_ELD <- rma(yi=logHR, sei=selogHR, data = allELD)
resall_ELD
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_ELD, transf=exp, digits=2)


### SUB GROUP POST MENOPAUSAL ###

allPM <- subset (dt, population == "Postmenopausal women")

resall_PM <- rma(yi=logHR, sei=selogHR, data = allPM)
resall_PM
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_PM, transf=exp, digits=2)


### SUB GROUP GENERAL POPULATION ###

allGP <- subset (dt, population == "General population")

resall_GP <- rma(yi=logHR, sei=selogHR, data = allGP)
resall_GP
#predicted pooled risk ratio and corresponding CI/PI
predict(resall_GP, transf=exp, digits=2)


#### BPV Metric == SD ####
dtSD <- subset (dt, Metric == "SD")

#Sub analysis studies with SD by excluding non-EHR
SD_EHR <- subset (dtSD, Source == "EHR")

resSD_EHR <- rma(yi=logHR, sei=selogHR, data = SD_EHR)
resSD_EHR
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_EHR, transf=exp, digits=2)

### SUB GROUP DESIGN == PROSPECTIVE ###

SDdesign <- subset (dtSD, Design == "prospective")

res_SDdesign <- rma(yi=logHR, sei=selogHR, data = SDdesign)
res_SDdesign
#predicted pooled risk ratio and corresponding CI/PI
predict(res_SDdesign, transf=exp, digits=2)

### SUB GROUP eGFR > 60 ###

SDeGFR <- subset (dtSD, eGFR == "No")

resSD_eGFR <- rma(yi=logHR, sei=selogHR, data = SDeGFR)
resSD_eGFR
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_eGFR, transf=exp, digits=2)

### SUB GROUP PREVIOUS CVD == NO ###

SDPrevCVD <- subset (dtSD, PrevCVD == "No")

resSD_PrevCVD <- rma(yi=logHR, sei=selogHR, data = SDPrevCVD)
resSD_PrevCVD
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_PrevCVD, transf=exp, digits=2)

### SUB GROUP PREVIOUS FU <=5 YEARS ###

SDFU5 <- subset (dtSD, FU5 == "<5")

resSD_FU5 <- rma(yi=logHR, sei=selogHR, data = SDFU5)
resSD_FU5
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_FU5, transf=exp, digits=2)

### SUB GROUP PREVIOUS OBS <=3 YEARS ###

SDObs3 <- subset (dtSD, Obs3 == "<3")

SDall_Obs3 <- rma(yi=logHR, sei=selogHR, data = SDObs3)
SDall_Obs3
#predicted pooled risk ratio and corresponding CI/PI
predict(SDall_Obs3, transf=exp, digits=2)

### SUB GROUP HYPERTENSION ###

SDHT <- subset (dtSD, population == "HT")

resSD_HT <- rma(yi=logHR, sei=selogHR, data = SDHT)
resSD_HT
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_HT, transf=exp, digits=2)

### SUB GROUP DIABETES MELLITUS ###

SDDM <- subset (dtSD, population == "DM")

resSD_DM <- rma(yi=logHR, sei=selogHR, data = SDDM)
resSD_DM
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_DM, transf=exp, digits=2)

### SUB GROUP ELDERLY ###

SDELD <- subset (dtSD, population == "Elderly")

resSD_ELD <- rma(yi=logHR, sei=selogHR, data = SDELD)
resSD_ELD
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_ELD, transf=exp, digits=2)


### SUB GROUP POST MENOPAUSAL ###

SDPM <- subset (dtSD, population == "Postmenopausal women")

resSD_PM <- rma(yi=logHR, sei=selogHR, data = SDPM)
resSD_PM
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_PM, transf=exp, digits=2)


### SUB GROUP GENERAL POPULATION ###

SDGP <- subset (dtSD, population == "General population")

resSD_GP <- rma(yi=logHR, sei=selogHR, data = SDGP)
resSD_GP
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_GP, transf=exp, digits=2)


##### SUB GROUP >100.000 #######

SDN <- subset (dtSD, NG == ">100.000")

resSD_NG <- rma(yi=logHR, sei=selogHR, data = SDN)
resSD_NG
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_NG, transf=exp, digits=2)

library(stringr)
dtSD <- subset (dt, BPVMet == "SD-SBP")

resSD <- rma(yi=logHR, sei=selogHR, data = dtSD)
resSD
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD, transf=exp, digits=2)


#### BPV Metric == cv ####
dtCV <- subset (dt, Metric == "CV")

#Sub analysis studies with SD by excluding non-EHR
CV_EHR <- subset (dtCV, Source == "EHR")

resCV_EHR <- rma(yi=logHR, sei=selogHR, data = CV_EHR)
resCV_EHR
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_EHR, transf=exp, digits=2)

### SUB GROUP DESIGN == PROSPECTIVE ###

CVdesign <- subset (dtCV, Design == "prospective")

res_CVdesign <- rma(yi=logHR, sei=selogHR, data = CVdesign)
res_CVdesign
#predicted pooled risk ratio and corresponding CI/PI
predict(res_CVdesign, transf=exp, digits=2)

### SUB GROUP eGFR > 60 ###

CVeGFR <- subset (dtCV, eGFR == "No")

resCV_eGFR <- rma(yi=logHR, sei=selogHR, data = CVeGFR)
resCV_eGFR
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_eGFR, transf=exp, digits=2)

### SUB GROUP PREVIOUS CVD == NO ###

CVPrevCVD <- subset (dtCV, PrevCVD == "No")

resCV_PrevCVD <- rma(yi=logHR, sei=selogHR, data = CVPrevCVD)
resCV_PrevCVD
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_PrevCVD, transf=exp, digits=2)

### SUB GROUP PREVIOUS FU <=5 YEARS ###

CVFU5 <- subset (dtCV, FU5 == "<5")

resCV_FU5 <- rma(yi=logHR, sei=selogHR, data = CVFU5)
resCV_FU5
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_FU5, transf=exp, digits=2)

### SUB GROUP PREVIOUS OBS <=3 YEARS ###

CVObs3 <- subset (dtCV, Obs3 == "<3")

CV_Obs3 <- rma(yi=logHR, sei=selogHR, data = CVObs3)
CV_Obs3
#predicted pooled risk ratio and corresponding CI/PI
predict(CV_Obs3, transf=exp, digits=2)

### SUB GROUP HYPERTENSION ###

CVHT <- subset (dtCV, population == "HT")

resCV_HT <- rma(yi=logHR, sei=selogHR, data = CVHT)
resCV_HT
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_HT, transf=exp, digits=2)

### SUB GROUP DIABETES MELLITUS ###

CVDM <- subset (dtCV, population == "DM")

resCV_DM <- rma(yi=logHR, sei=selogHR, data = CVDM)
resCV_DM
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_DM, transf=exp, digits=2)

### SUB GROUP ELDERLY ###

CVELD <- subset (dtCV, population == "Elderly")

resCV_ELD <- rma(yi=logHR, sei=selogHR, data = CVELD)
resCV_ELD
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_ELD, transf=exp, digits=2)


### SUB GROUP POST MENOPAUSAL ###

SDPM <- subset (dtSD, population == "Postmenopausal women")

resSD_PM <- rma(yi=logHR, sei=selogHR, data = SDPM)
resSD_PM
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_PM, transf=exp, digits=2)


### SUB GROUP GENERAL POPULATION ###

CVGP <- subset (dtCV, population == "General population")

resCV_GP <- rma(yi=logHR, sei=selogHR, data = CVGP)
resCV_GP
#predicted pooled risk ratio and corresponding CI/PI
predict(resCV_GP, transf=exp, digits=2)

#### BPV Metric == ARV ####
dtARV <- subset (dt, Metric == "ARV")

#Sub analysis studies with SD by excluding non-EHR
ARV_EHR <- subset (dtARV, Source == "EHR")

resARV_EHR <- rma(yi=logHR, sei=selogHR, data = ARV_EHR)
resARV_EHR
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_EHR, transf=exp, digits=2)

### SUB GROUP DESIGN == PROSPECTIVE ###

ARVdesign <- subset (dtARV, Design == "prospective")

res_ARVdesign <- rma(yi=logHR, sei=selogHR, data = ARVdesign)
res_ARVdesign
#predicted pooled risk ratio and corresponding CI/PI
predict(res_ARVdesign, transf=exp, digits=2)

### SUB GROUP eGFR > 60 ###

ARVeGFR <- subset (dtARV, eGFR == "No")

resARV_eGFR <- rma(yi=logHR, sei=selogHR, data = ARVeGFR)
resARV_eGFR
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_eGFR, transf=exp, digits=2)

### SUB GROUP PREVIOUS CVD == NO ###

ARVPrevCVD <- subset (dtARV, PrevCVD == "No")

resARV_PrevCVD <- rma(yi=logHR, sei=selogHR, data = ARVPrevCVD)
resARV_PrevCVD
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_PrevCVD, transf=exp, digits=2)

### SUB GROUP PREVIOUS FU <=5 YEARS ###

ARVFU5 <- subset (dtARV, FU5 == "<5")

resARV_FU5 <- rma(yi=logHR, sei=selogHR, data = ARVFU5)
resARV_FU5
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_FU5, transf=exp, digits=2)

### SUB GROUP PREVIOUS OBS <=3 YEARS ###

ARVObs3 <- subset (dtARV, Obs3 == "<3")

ARV_Obs3 <- rma(yi=logHR, sei=selogHR, data = ARVObs3)
ARV_Obs3
#predicted pooled risk ratio and corresponding CI/PI
predict(ARV_Obs3, transf=exp, digits=2)

### SUB GROUP HYPERTENSION ###

ARVHT <- subset (dtARV, population == "HT")

resARV_HT <- rma(yi=logHR, sei=selogHR, data = ARVHT)
resARV_HT
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_HT, transf=exp, digits=2)

### SUB GROUP DIABETES MELLITUS ###

ARVDM <- subset (dtARV, population == "DM")

resARV_DM <- rma(yi=logHR, sei=selogHR, data = ARVDM)
resARV_DM
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_DM, transf=exp, digits=2)

### SUB GROUP ELDERLY ###

ARVELD <- subset (dtARV, population == "Elderly")

resARV_ELD <- rma(yi=logHR, sei=selogHR, data = ARVELD)
resARV_ELD
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_ELD, transf=exp, digits=2)


### SUB GROUP POST MENOPAUSAL ###

SDPM <- subset (dtSD, population == "Postmenopausal women")

resSD_PM <- rma(yi=logHR, sei=selogHR, data = SDPM)
resSD_PM
#predicted pooled risk ratio and corresponding CI/PI
predict(resSD_PM, transf=exp, digits=2)


### SUB GROUP GENERAL POPULATION ###

ARVGP <- subset (dtARV, population == "General population")

resARV_GP <- rma(yi=logHR, sei=selogHR, data = ARVGP)
resARV_GP
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_GP, transf=exp, digits=2)



#### BPV Metric == VIM  ####
dtVIM <- subset (dt, Metric == "VIM")

#Sub analysis studies with SD by excluding non-EHR
VIM_EHR <- subset (dtVIM, Source == "EHR")

resVIM_EHR <- rma(yi=logHR, sei=selogHR, data = VIM_EHR)
resVIM_EHR
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_EHR, transf=exp, digits=2)

### SUB GROUP DESIGN == PROSPECTIVE ###

VIMdesign <- subset (dtVIM, Design == "prospective")

res_VIMdesign <- rma(yi=logHR, sei=selogHR, data = VIMdesign)
res_VIMdesign
#predicted pooled risk ratio and corresponding CI/PI
predict(res_VIMdesign, transf=exp, digits=2)

### SUB GROUP eGFR > 60 ###

VIMeGFR <- subset (dtVIM, eGFR == "No")

resVIM_eGFR <- rma(yi=logHR, sei=selogHR, data = VIMeGFR)
resVIM_eGFR
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_eGFR, transf=exp, digits=2)

### SUB GROUP PREVIOUS CVD == NO ###

VIMPrevCVD <- subset (dtVIM, PrevCVD == "No")

resVIM_PrevCVD <- rma(yi=logHR, sei=selogHR, data = VIMPrevCVD)
resVIM_PrevCVD
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_PrevCVD, transf=exp, digits=2)

### SUB GROUP PREVIOUS FU <=5 YEARS ###

VIMFU5 <- subset (dtVIM, FU5 == "<5")

resVIM_FU5 <- rma(yi=logHR, sei=selogHR, data = VIMFU5)
resVIM_FU5
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_FU5, transf=exp, digits=2)

### SUB GROUP PREVIOUS OBS <=3 YEARS ###

VIMObs3 <- subset (dtVIM, Obs3 == "<3")

VIM_Obs3 <- rma(yi=logHR, sei=selogHR, data = VIMObs3)
VIM_Obs3
#predicted pooled risk ratio and corresponding CI/PI
predict(VIM_Obs3, transf=exp, digits=2)

### SUB GROUP HYPERTENSION ###

VIMHT <- subset (dtVIM, population == "HT")

resVIM_HT <- rma(yi=logHR, sei=selogHR, data = VIMHT)
resVIM_HT
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_HT, transf=exp, digits=2)

### SUB GROUP DIABETES MELLITUS ###

VIMDM <- subset (dtVIM, population == "DM")

resVIM_DM <- rma(yi=logHR, sei=selogHR, data = VIMDM)
resVIM_DM
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_DM, transf=exp, digits=2)

### SUB GROUP ELDERLY ###

ARVELD <- subset (dtARV, population == "Elderly")

resARV_ELD <- rma(yi=logHR, sei=selogHR, data = ARVELD)
resARV_ELD
#predicted pooled risk ratio and corresponding CI/PI
predict(resARV_ELD, transf=exp, digits=2)


### SUB GROUP POST MENOPAUSAL ###

VIMPM <- subset (dtVIM, population == "Postmenopausal women")

resVIM_PM <- rma(yi=logHR, sei=selogHR, data = VIMPM)
resVIM_PM
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_PM, transf=exp, digits=2)


### SUB GROUP GENERAL POPULATION ###

VIMGP <- subset (dtVIM, population == "General population")

resVIM_GP <- rma(yi=logHR, sei=selogHR, data = VIMGP)
resVIM_GP
#predicted pooled risk ratio and corresponding CI/PI
predict(resVIM_GP, transf=exp, digits=2)

