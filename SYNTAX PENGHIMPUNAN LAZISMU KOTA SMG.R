###Step 1: Package ###
library(tmap)
library(raster)
library(knitr)
library(kableExtra)
library(dplyr)
library(RSQLite)
library(DBI)
library(ggplot2)
library(tigris)
library(sp)
library(spdep)
library(sf)
library(readxl)
library(spgwr)
library(openxlsx)
library(corrplot)
library(DescTools)
library(nortest)
library(car)
library(spatialreg)
library(tidyverse)
###Step 2: Set Data ###
data<-read_excel("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/datapenghimpunan.xlsx", sheet="penghimpunan2122")
kbl(data, caption = "Jumlah Penghimpunan Kantor Layanan Kota Semarang menurut Kecamatan Tahun 2021 dan 2022") %>%  kable_styling()
###Step 3: Analisis Deskriptif ####
summary(data)
### Step 4: Memasukkan Data SHP ###
Admin3Kecamatan <- st_read("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/Adm_Kecamatan_Semarang-polygon.shp")
names(Admin3Kecamatan)
### Step 5: Menampilkan Peta Kota Semarang ###
tm_shape(Admin3Kecamatan) + tm_polygons()
### step 6: Memasukkan Data Excel Penghimpunan 2021 dan 2022###
data21<-read_excel("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/datapenghimpunan.xlsx", sheet="penghimpunan21")
data22<-read_excel("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/datapenghimpunan.xlsx", sheet="penghimpunan22")
head(data21,5)
head(data22,5)
### Step 7: Menambahkan variabel dalam file shp ###
Admin3Kecamatan$JumlahPenghimpunan21=data21$JumlahPenghimpunan21
Admin3Kecamatan$JumlahPenghimpunan22=data22$JumlahPenghimpunan22
### Step 8: Menampilkan Layout Peta Jumlah Penghimpunan 2021 dan 2022###
tm_shape(Admin3Kecamatan) + 
  tm_borders(col="red", lwd=2)+
  tm_layout(main.title = "Peta Sebaran Jumlah Penghimpunan dari KL Tahun 2021", title.size = 0.2, 
            title.position = c("right", "top"), 
            legend.outside=TRUE, legend.position= c("right", "bottom"))+
  tm_text("name", size=0.3)+
  tm_polygons(col="JumlahPenghimpunan21", style="quantile")
win.graph()

tm_shape(Admin3Kecamatan) + 
  tm_borders(col="red", lwd=2)+
  tm_layout(main.title = "Peta Sebaran Jumlah Penghimpunan dari KL Tahun 2022", title.size = 0.5, 
            title.position = c("right", "top"), 
            legend.outside=TRUE, legend.position= c("right", "bottom"))+
  tm_text("name", size=0.3)+
  tm_polygons(col="JumlahPenghimpunan22", style="quantile")
win.graph()
### Step 9: Matriks Pembobot ###
##Pembobot 21
queen.nb21=poly2nb(Admin3Kecamatan) #Pembobot queen
queen.nb21
queen.listw=nb2listw(queen.nb21) #convert nb to listw type
queen.smg21= queen.listw
queen.smg21
##Pembobot 22
queen.nb22=poly2nb(Admin3Kecamatan) #Pembobot queen
queen.nb22
queen.listw=nb2listw(queen.nb22) #convert nb to listw type
queen.smg22= queen.listw
queen.smg22
#Menyimpan Matriks Pembobot
bobot.queen21 = listw2mat(queen.listw) #convert listw to matrix
write.csv(bobot.queen21, "Matriks Bobot Queen.csv")
bobot.queen22 = listw2mat(queen.listw) #convert listw to matrix
write.csv(bobot.queen21, "Matriks Bobot Queen.csv")
### Step 10: Uji Dependensi Spasial dengan Moran's I
#Moran Test Penghimpunan 21: Pembobot Queen
moran.test(Admin3Kecamatan$JumlahPenghimpunan21,queen.smg21,randomisation=FALSE)
a<-moran.plot(Admin3Kecamatan$JumlahPenghimpunan21,queen.smg21, labels = Admin3Kecamatan$name, pch = 19, main="Moran Scatterplot Jumlah Penghimpunan 2021")
text(a,data21$Kecamatan,cex=0.65, col="red")
#Moran Test Penghimpunan 22: Pembobot Queen
moran.test(Admin3Kecamatan$JumlahPenghimpunan22,queen.smg22,randomisation=FALSE)
a<-moran.plot(Admin3Kecamatan$JumlahPenghimpunan22,queen.smg22, labels = Admin3Kecamatan$name, pch = 19, main="Moran Scatterplot Jumlah Penghimpunan 2022")
text(a,data22$Kecamatan,cex=0.65, col="red")

data.2021=data$JumlahPenghimpunan21
data.2022=data$JumlahPenghimpunan22
##LOCAL MORAN
lmoran1.21<-localmoran(data.2021,queen.smg21) %>% as.data.frame()
lmoran1.22<-localmoran(data.2022,queen.smg22) %>% as.data.frame()

data$scale.data.2021<-scale(data.2021) %>% as.vector()
data$scale.data.2022<-scale(data.2022) %>% as.vector()
data$lag.data.2021<-lag.listw(queen.smg21, data$scale.data.2021)
data$quad_sig.2021<-NA
data[(data$scale.data.2021 >= 0 &
        data$lag.data.2021 >= 0), "quad_sig.2021"] <- "High-High"
data[(data$scale.data.2021 <= 0 &
        data$lag.data.2021 <= 0), "quad_sig.2021"] <- "Low-Low"
data[(data$scale.data.2021 >= 0 &
        data$lag.data.2021 <= 0), "quad_sig.2021"] <- "High-Low"
data[(data$scale.data.2021 <= 0 &
        data$lag.data.2021 >= 0), "quad_sig.2021"] <- "Low-High"
lmoran1.21$quad_sig.2021=data$quad_sig.2021
data$sig.2021<-NA
data[(lmoran1.21$`Pr(z != E(Ii))` <= 0.05), "sig.2021"] <- "Signifikan"
data[(lmoran1.21$`Pr(z != E(Ii))` > 0.05), "sig.2021"] <- "tidak Signifikan"
lmoran1.21$sig.2021=data$sig.2021
write.table(lmoran1.21,"local moran 2021.csv")
shp_negara=read_sf("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/Adm_Kecamatan_Semarang-polygon.shp")
library(dplyr)
data2121<-read_excel("E:/Choirunnisa Hasna_Statistika/semester 7/LAPORAN MAGANG/datapenghimpunan.xlsx", sheet="penghimpunan21")

gabung_negara=left_join(Admin3Kecamatan,data21,by="ID")
gabung_negara
a<-localmoran.plot(Admin3Kecamatan$JumlahPenghimpunan22,queen.smg22, labels = Admin3Kecamatan$name, pch = 19, main="Moran Scatterplot Jumlah Penghimpunan 2022")


tm_shape(Admin3Kecamatan) + 
  tm_borders(col="red", lwd=2)+
  tm_layout(main.title = "Peta Sebaran Jumlah Penghimpunan dari KL Tahun 2022", title.size = 0.5, 
            title.position = c("right", "top"), 
            legend.outside=TRUE, legend.position= c("right", "bottom"))+
  tm_text("name", size=0.3)+
  tm_polygons(col="JumlahPenghimpunan22", style="quantile")

#Pemetaan
plot.negara.2021 = ggplot(data = data21) +
  geom_sf(mapping = aes(fill=quad_sig.2021)) +
  geom_sf_text(aes(label=Admin3Kecamatan$name),size=3) +
  scale_fill_manual(values=c("red","blue","green","yellow")) +
  labs(fill="category")
view(plot.negara.2021)
plot.negara.2021




13.54






















local <- localmoran(Admin3Kecamatan$JumlahPenghimpunan22, listw = queen.smg21)
head(local)
local

#Pemetaan
plot.negara.2010 = ggplot(data = gabung.negara) +
  geom_sf(mapping = aes(fill=quad_sig.10)) +
  geom_sf_text(aes(label=KabKot),size=3) +
  scale_fill_manual(values=c("red","blue","green","yellow")) +
  labs(fill="category")
plot.negara.2010

local <- localmoran(Admin3Kecamatan$JumlahPenghimpunan22, listw = queen.smg21)
# process results
shape$cluster <- as.factor(local$GetClusterIndicators())
levels(shape$cluster) <- lisa$GetLabels()




# calculate LISA as per GEODA
lisa <- local_moran(queen.smg21, shape["SID79"]) # or any other variable :)

# process results
shape$cluster <- as.factor(lisa$GetClusterIndicators())
levels(shape$cluster) <- lisa$GetLabels()

# A visual overview
ggplot(data = shape) +
  geom_sf(aes(fill = cluster))