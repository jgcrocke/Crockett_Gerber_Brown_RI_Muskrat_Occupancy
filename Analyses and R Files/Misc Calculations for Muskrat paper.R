rm(list=ls())
setwd("C:/Users/John Crockett/Documents/GitHub/Crockett_Gerber_Brown_RI_Muskrat_Occupancy")
regions=read_sf("Other Shapefiles/ri_eco_l4.shp")
plot(regions["US_L4NAME"])
allwatershedsused=read_sf("Other Shapefiles/AllWatershedsUsed.shp")
plot(allwatershedsused["Watershed"])
sum(st_area(allwatershedsused))/10000

precip=read.fwf(file = "C:/Users/John Crockett/Downloads/v2.prcp_adj/v2.prcp_adj",
         widths =c(3,5,3,1,4,5,5,5,5,5,5,5,5,5,5,5,5),
         col.names =c("Country","Station","Modifier",
                      "Duplicated","Year","Jan","Feb","Mar","Apr",
                      "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")  )
precip=precip[precip$Country==425,]
precip=precip[precip$Station==72507,]
for(i in 1:length(precip$Country)){
  for(j in 6:17){
    precip[i,j]=ifelse(-9000>precip[i,j],NA,
                       ifelse(precip[i,j]<0,0,precip[i,j]/100))
  }
}
apply(precip[6:17],2,mean,na.rm=T)
