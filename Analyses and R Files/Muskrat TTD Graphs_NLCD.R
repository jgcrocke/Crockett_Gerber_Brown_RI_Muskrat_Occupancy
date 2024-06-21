library(rjags)
library(runjags)
library(mcmcplots)
library(MCMCvis)
rm(list=ls())
#Load in the model and data
setwd("C:/Users/John Crockett/Documents/GitHub/Crockett_Gerber_Brown_RI_Muskrat_Occupancy")
load("MuskratTTD_For_Graphs.Rdata")
load("MuskratTTD_nlcd.Rdata")
#
#Plotting coefficients
jpeg(filename = "Figures/Ch1Fig3BetasAlphas.jpg",width =16,height =6 ,
    units = "in",res = 200)
par(mfrow=c(1,2))
MCMCplot(Muskrat.TTD$mcmc,
         params =c("beta1","beta2","beta3",
                   "beta4","beta5","beta6","betaw"),
         labels = c("Urban areas","Forested wetland",
                    "Open wetland","Salt water","Water",
                    "Second order stream","Block Island Sound",
                    "Blackstone","Narragansett Bay",
                    "Pawcatuck","Pawtuxet","Quinebaug", "Woonasquatucket-Moshassuck"),
         mar = c(4.1,4.1,3.1,1.1),
         sz_ax_txt = 1,sz_labels = 1,sz_main_txt = 1,sz_ax = 1,guide_axis = F,
         sz_tick_txt = 1,sz_med =1.25,sz_thick = 3,sz_thin = 1.5,ref = NULL,
         xlim = c(-3,2),xlab="Coefficient estimate")
text(-5,8,"Covariates affecting muskrat occupancy",srt=90,xpd=T)
text(2,14,"(A)",xpd=T)
MCMCplot(Muskrat.TTD$mcmc,
         params =Muskrat.TTD$monitor[3:15],
         labels = c("Observer 1","Observer 2",
           "Observer 3", "Observer 4","Observer 5","Observer 6",
           "Time spent in water","Time spent in built",
           "Time spent in wetland","Temperature at start of survey",
           "Cloud cover at start of survey",
           "Precipitation in previous 24 hours",
           "Surveying by kayak"),
         mar = c(4.1,5.1,3.1,1.1),
         sz_ax_txt = 1,sz_labels = 1,sz_main_txt = 1,sz_ax = 1,guide_axis = F,
         sz_tick_txt = 1,sz_med =1.25,sz_thick = 3,sz_thin = 1.5,ref = NULL,
         xlim = c(-1.25,1.5),xlab="Coefficient estimate")
text(-2.5,8,expression(paste("Covariates affecting  ",lambda)),srt=90,xpd=T)
text(1.5,14,"(B)",xpd=T)
dev.off()
#####
# Making it easier to work with the results of the model
mcmc=data.frame(combine.mcmc(Muskrat.TTD))
colnames(mcmc)
HPDinterval(combine.mcmc(Muskrat.TTD),prob=0.95)
Muskrat.TTD$monitor[9:15]
apply(mcmc,2,median)

#####
#Mapping
#####
# Load in the available sites
library(sf)
Qkm=read_sf("Site Shapefiles/Qkm_NLCD.shp")
# Applying the same transformations used in the model to the available sites
Qkm$Bimp=(Qkm$N19_Urb-meanimp)/sdimp
Qkm$Bforest=(Qkm$N19_FWt-meanforest)/sdforest
Qkm$Bswamp=(Qkm$N19_OWt-meanswamp)/sdswamp
Qkm$Bh2o=(Qkm$N19_Wtr-meanh2o)/sdh2o
Qkm$BStrm=ifelse(Qkm$StrmOr_>1,1,0)
Qkm$BStrm[is.na(Qkm$BStrm)]=0
Qkm$Salt=0
for (i in 1:17101){
  Qkm$Salt[i]=ifelse(sum(Qkm$`Salt Wat_1`[i]+Qkm$`Salt Mar_1`[i]+
    Qkm$Intertid_1[i]+Qkm$`Tidal Cr_1`[i]+Qkm$`Tidal Ri_1`[i])>0,
    1,0)
}

#Adding predicted occupancy to the map
logit.psi=vector(length=length(Qkm$Fldpl_1))
betaw=apply(mcmc[,22:28],2,FUN = median)
Qkm$Watrshd[Qkm$Watrshd=="Block Island Sound"]="BIS"
Wshed=as.factor(Qkm$Watrshd)
WSHED=as.numeric(Wshed)
for (m in 1:length(Qkm$Fldpl_1)){
  logit.psi[m]=median(mcmc$psi.int)+median(mcmc$beta1)*Qkm$Bimp[m]+
    median(mcmc$beta2)*Qkm$Bforest[m]+
    median(mcmc$beta3)*Qkm$Bswamp[m]+
    median(mcmc$beta4)*Qkm$Salt[m]+
    median(mcmc$beta5)*Qkm$Bh2o[m]+median(mcmc$beta6)*Qkm$BStrm[m]+
    betaw[WSHED[m]]
}
expit=function(x){(exp(x))/(1+exp(x))}
Qkm$Psi.median19=expit(logit.psi)

# re-doing the above for 2001 landcover
Qkm$Bimp1=(Qkm$N01_Urb-meanimp)/sdimp
Qkm$Bforest1=(Qkm$N01_FWt-meanforest)/sdforest
Qkm$Bswamp1=(Qkm$N01_OWt-meanswamp)/sdswamp
Qkm$Bh2o1=(Qkm$N01_Wtr-meanh2o)/sdh2o

# 2001 predicted occupancy 
logit.psi=vector(length=length(Qkm$Fldpl_1))

for (m in 1:length(Qkm$Fldpl_1)){
  logit.psi[m]=median(mcmc$psi.int)+median(mcmc$beta1)*Qkm$Bimp1[m]+
    median(mcmc$beta2)*Qkm$Bforest1[m]+
    median(mcmc$beta3)*Qkm$Bswamp1[m]+
    median(mcmc$beta4)*Qkm$Salt[m]+
    median(mcmc$beta5)*Qkm$Bh2o1[m]+median(mcmc$beta6)*Qkm$BStrm[m]+
    betaw[WSHED[m]]
}
Qkm$Psi.median01=expit(logit.psi)
#Calculating change in predicted occupancy 2001 to 2019
Qkm$Pred_Occ_Change=Qkm$Psi.median19-Qkm$Psi.median01
#plots in the paper were made using QGIS for easy changes to legends and symbology
plot(Qkm[c("Psi.median01","Psi.median19","Pred_Occ_Change")],border=NA)
plot(Qkm["Pred_Occ_Change"],border=NA)
mean(Qkm$Pred_Occ_Change)
hist(Qkm$Pred_Occ_Change)

st_write(obj = Qkm,
         dsn =  "C:/Users/John Crockett/Documents/URI/GIS Data/QKm_withNLCD_Preds.shp",
         append = F)
Qkm=st_read("C:/Users/John Crockett/Documents/URI/GIS Data/QKm_withNLCD_Preds.shp")
range(surveyed$N19_Wtr)

#####
## Working with the weibull parameters
#From JAGS, the parameters are v and lambda
#The pdf is v*lambda^(v-1)exp(-lambda*y^v)
lambda=mcmc$lambda.int
v=mcmc$shape
# Cumulative probability of detection is:
# 1-exp(-bx^(k)) where k is the shape parameter,
# b is the scale, x is the duration

#For a site with no kayak, in the reference watershed, surveyed by
#  other observer, and averaging across all the other covs,
# log(b)=log(lambda.int) therefore b=lambda.int
# If same but with kayaks, log(b)=log(lambda.int)+alpha13
#   therefore b=exp(log(lambda.int)+alpha13)
#So cum.prob=1-exp(-(exp(log(lambda.int)+alpha13))*duration^shape)
# Graphing against observer:
#####
library(RColorBrewer)
pal=brewer.pal(n=6,name = "Dark2")

duration=1:80
colnames(OBS)
prob.other=1-exp(-exp(log(median(mcmc$lambda.int)))*
                     duration^median(mcmc$shape))
prob.Amy=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha1))*
                   duration^median(mcmc$shape))
prob.Chris=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha2))*
                              duration^median(mcmc$shape))
prob.Jess=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha3))*
                   duration^median(mcmc$shape))
prob.John=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha4))*
                   duration^median(mcmc$shape))
prob.Justin=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha5))*
                   duration^median(mcmc$shape))
prob.Kylie=1-exp(-exp(log(median(mcmc$lambda.int))+median(mcmc$alpha6))*
                   duration^median(mcmc$shape))
jpeg(filename = "Figures/Ch1Fig6cumdetection.jpg",width =6.5,height =6.5,
    units = "in",res = 300)
par(mar=c(4.5,4.1,4.3,1.5))
plot(duration,prob.other,type="l",lwd=3,ylim=c(0,0.7),
     xlab="Survey duration",ylab="Probability of detection",bty="l")
lines(duration,prob.Amy,lwd=3,col=pal[1])
lines(duration,prob.Chris,lwd=3,col=pal[2])
lines(duration,prob.Jess,lwd=3,col=pal[3])
lines(duration,prob.John,lwd=3,col=pal[4])
lines(duration,prob.Justin,lwd=3,col=pal[5])
lines(duration,prob.Kylie,lwd=3,col=pal[6])
legend("top",bty="n",xpd=NA,cex=1,lwd=3,ncol=3,inset=-0.17,
       seg.len = 1,x.intersp = 0.6,
       text.width = c(25,20,20),
       legend = c("Other observers","Observer 1",
                  "Observer 2","Observer 3",
                  "Observer 4","Observer 5",
                  "Observer 6"),col = c("black",pal))
dev.off()
######
# Graph of muskrat harvest and number of trappers
######
setwd("C:/Users/John Crockett/Documents/GitHub/Crockett_Gerber_Brown_RI_Muskrat_Occupancy")
muskdata=readxl::read_xlsx("Data from Field Work/Muskrat Harvest Data.xlsx")

jpeg(filename = "Figures/Ch1Fig1MuskratHarvest.jpg",width =8.5,height = 9,
    units = "in",res = 100)
par(mfrow=c(2,2),mar=c(4,4,0.75,0.5))
plot(muskdata$Year,muskdata$`Number of Muskrats Harvested`,
     pch=16,xlab="Year",ylab="Number of muskrats harvested",
     ylim=c(0,12000),yaxp=c(0,12000,6),bty="l")
text(x=2023,y=12000,labels="(A)")
plot(muskdata$Year,muskdata$`Number of Muskrats Harvested`,
     pch=16,xlab="Year",ylab="Number of muskrats harvested",
     ylim=c(0,300),yaxp=c(0,300,6),bty="l",xlim=c(2000,2023))
text(x=2023,y=300,labels="(C)")
par(mar=c(4.6,4,1,0.5))
plot(muskdata$Year,muskdata$`Number of Trappers`,
     pch=16,xlab="Year",ylab="Number of trappers",
     ylim=c(0,410),yaxp=c(0,400,4),bty="l")
text(x=2023,y=410,labels="(B)")
dev.off()


