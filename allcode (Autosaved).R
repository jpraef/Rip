#check for needed packages, install if needed, require if not
ndpkg <- function(p){
 ifelse(is.element(p,installed.packages()[,1]),require(p,character.only = T), install.packages(p, dep = T))
}
sapply(c("ggplot2", "reshape", "mgcv", "dplyr", "maps", "directlabels"), ndpkg)
mpp <- map_data("state")

#Load needed data: md0, md00, lon, lat, year, month, prec, tavg
load("~/Desktop/Project 1/Data/General/lat.jdata.rda")
load("~/Desktop/Project 1/Data/General/lon.jdata.rda")
load("~/Desktop/Project 1/Data/General/md0.jdata.rda")
load("~/Desktop/Project 1/Data/General/md00.jdata.rda")
load("~/Desktop/Project 1/Data/General/year.jdata.rda")
load("~/Desktop/Project 1/Data/General/month.jdata.rda")
load("~/Desktop/Project 1/Data/General/prec.jdata.rda")
load("~/Desktop/Project 1/Data/General/tavg.jdata.rda")

#LDAS Veg
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.water.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.urban.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.bare.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.grassland.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.vegfrac.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.cropland.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.woodland.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.pixels.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.forest.jdata.rda")
load("/Users/joe/Desktop/Project 1/Data/General/Veg/lv.shrubland.jdata.rda")

#Create a gridded annual md0 index
md00y <- md00[month==7] #Only need one month of the annual md0
md00yg <- by(md00y,list(lon[month==7],lat[month==7],year[month==7]),median)
md00ygs <- aperm(apply(md00yg,1:2,scale), c(2,3,1)) #scaling and reordering dimensions (z-scores)
md00mgs <- aperm(apply(md00ygs, 1:2, function(x) rep(x, each = 12)), c(2,3,1)) #each year multiplied by 12 to fit the dataframe

 

#scale monthly variables by month
###test
md0s <- aperm(apply(md0,1:2, scale), c(2,3,1))
###
mm = rep(1:12, 97)
md0s <- md0
for (i in 1:12){
	md0s[,,mm==i]=scale(md0[,,mm==i][,,1:97])
	}

tavgs <- tavg
for (i in 1:12){
	tavgs[,,mm==i]=scale(tavg[,,mm==i][,,1:97])
	}

precs <- prec
for (i in 1:12){
	precs[,,mm==i]=scale(prec[,,mm==i][,,1:97])
	}

#data frame of all variables
all_var <- as.data.frame(cbind(year, month, lat, lon, md00, md00mgs, md0, prec, tavg))
#add a water year variable
all_var$water_year <- NA
all_var[1:234234, "water_year"] <- 1918
all_var[234235: 30216186, "water_year"] <- rep(1919:2014, each = 312312)
all_var[30216186:30294264, "water_year"] <- 2015


#2 year drought, z-score thresholds == 0, 1, 2 

mi20 <- ifelse(((md00ygs[,,1:96]>=0)+(md00ygs[,,2:97]>=0))==2,1,NA)
mi21 <- ifelse(((md00ygs[,,1:96]>=1)+(md00ygs[,,2:97]>=1))==2,1,NA)
mi22 <- ifelse(((md00ygs[,,1:96]>=2)+(md00ygs[,,2:97]>=2))==2,1,NA)
         
#3 year drought, z-score threshold == 0, 1, 2
mi30 <- ifelse(((md00ygs[,,1:95]>=0)+(md00ygs[,,2:96]>=0)+(md00ygs[,,3:97]>=0))==3,1,NA)  
mi31 <- ifelse(((md00ygs[,,1:95]>=1)+(md00ygs[,,2:96]>=1)+(md00ygs[,,3:97]>=1))==3,1,NA)  
mi32 <- ifelse(((md00ygs[,,1:95]>=2)+(md00ygs[,,2:96]>=2)+(md00ygs[,,3:97]>=2))==3,1,NA)  
         
#5 year drought, z-score threshold == 0, 1, 2
mi50 <- ifelse(((md00ygs[,,1:93]>=0)+(md00ygs[,,2:94]>=0)+(md00ygs[,,3:95]>=0)+(md00ygs[,,4:96]>=0)+(md00ygs[,,5:97]>=0))==5,1,NA)
mi51 <- ifelse(((md00ygs[,,1:93]>=1)+(md00ygs[,,2:94]>=1)+(md00ygs[,,3:95]>=1)+(md00ygs[,,4:96]>=1)+(md00ygs[,,5:97]>=1))==5,1,NA)         
mi52 <- ifelse(((md00ygs[,,1:93]>=2)+(md00ygs[,,2:94]>=2)+(md00ygs[,,3:95]>=2)+(md00ygs[,,4:96]>=2)+(md00ygs[,,5:97]>=2))==5,1,NA)         
         

#compare criteria visually
rnk_per <- data.frame(Year = 1918:2014, percent_20 = NA, percent_21 = NA, percent_22 = NA, percent_30 = NA, percent_31 = NA, percent_32 = NA, percent_50 = NA, percent_51 = NA, percent_52 = NA)

#total core cells/ total land cells
for (i in 1:96){
	rnk_per[i+1,2]<- 100 * (sum(!is.na(mi20[ , , i])) / sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+1,3]<- 100 * (sum(!is.na(ifelse(mi21[ , ,i] >= 1, 1, NA))) / sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+1,4]<- 100 * (sum(!is.na(ifelse(mi22[ , ,i] >= 2, 1, NA))) / sum(!is.na(md00ygs[ , , i])))
}

for (i in 1:95){
	rnk_per[i+2,5]<- 100 * (sum(!is.na(mi30[ , , i]))/
	sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+2,6]<- 100 * (sum(!is.na(ifelse(mi31[ , ,i] >= 1, 1, NA)))/sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+2,7]<- 100 * (sum(!is.na(ifelse(mi32[ , ,i] >= 2, 1, NA)))/sum(!is.na(md00ygs[ , , i])))
}

for (i in 1:93){
	rnk_per[i+4,8]<- 100 * (sum(!is.na(mi50[ , , i]))/
	sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+4,9]<- 100 * (sum(!is.na(ifelse(mi51[ , ,i] >= 1, 1, NA)))/sum(!is.na(md00ygs[ , , i])))
	rnk_per[i+4,10]<- 100 * (sum(!is.na(ifelse(mi52[ , ,i] >= 2, 1, NA)))/sum(!is.na(md00ygs[ , , i])))
}

rnk_per_m <- melt(rnk_per[,2:10])
colnames(rnk_per_m) <- c("Threshold", "Percentage")
rnk_per_m$Year <- rep(1918:2014, 9)
rnk_per_m$Window <- substr(rnk_per_m$Threshold, 1, 9)
rnk_per_m$Window <- rep(c("2 Year", "3 Year", "5 Year"), each = 291)

rnk_graph <- ggplot(data = rnk_per_m, aes(x = Year, y = Percentage, color = Threshold)) + geom_ribbon(aes(ymin = 0, ymax = Percentage, fill = Threshold), color = "black") + scale_fill_manual(name = "SD > ... ",values = c("darkgoldenrod1", "chocolate1","brown2","darkgoldenrod2", "chocolate2","brown3","darkgoldenrod3", "chocolate3","brown4"), labels = c("0", "1", "2","0", "1", "2","0", "1", "2"))+ facet_grid(Window~., scale = "free") + theme_minimal() + labs(title = "Drought Window Lengths and Threshold Values for Moisture Deficit Anomalies",x = "Year", y = "Percentage Drought in Western States") 


#Intensity
mi20_sum<-(md00ygs[,,1:96]>=0)+(md00ygs[,,2:97]>=0)
for (i in 1:96){
mi20_sum[,,i]<-ifelse(mi20_sum[,,i]==2,((md00ygs[,,i])+(md00ygs[,,i+1])), NA)
}
mi21_sum<-(md00ygs[,,1:96]>=1)+(md00ygs[,,2:97]>=1)
for (i in 1:96){
mi21_sum[,,i]<-ifelse(mi21_sum[,,i]==2,((md00ygs[,,i])+(md00ygs[,,i+1])), NA)
}
mi22_sum<-(md00ygs[,,1:96]>=2)+(md00ygs[,,2:97]>=2)
for (i in 1:96){
mi22_sum[,,i]<-ifelse(mi22_sum[,,i]==2,((md00ygs[,,i])+(md00ygs[,,i+1])), NA)
}

mi30_sum<-(md00ygs[,,1:95]>=0)+(md00ygs[,,2:96]>=0)+(md00ygs[,,3:97]>=0)
for (i in 1:95){
mi30_sum[,,i]<-ifelse(mi30_sum[,,i]==3,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])), NA)
}
mi31_sum<-(md00ygs[,,1:95]>=1)+(md00ygs[,,2:96]>=1)+(md00ygs[,,3:97]>=1)
for (i in 1:95){
mi31_sum[,,i]<-ifelse(mi31_sum[,,i]==3,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])), NA)
}
mi32_sum<-(md00ygs[,,1:95]>=2)+(md00ygs[,,2:96]>=2)+(md00ygs[,,3:97]>=2)
for (i in 1:95){
mi32_sum[,,i]<-ifelse(mi32_sum[,,i]==3,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])), NA)
} 

mi50_sum <- (md00ygs[,,1:93]>=0)+(md00ygs[,,2:94]>=0)+(md00ygs[,,3:95]>=0)+(md00ygs[,,4:96]>=0)+(md00ygs[,,5:97]>=0)
for (i in 1:93){
mi50_sum[,,i]<-ifelse(mi50_sum[,,i]==5,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])+(md00ygs[,,i+3]) + (md00ygs[,,i+4])), NA)
}
mi51_sum <- (md00ygs[,,1:93]>=1)+(md00ygs[,,2:94]>=1)+(md00ygs[,,3:95]>=1)+(md00ygs[,,4:96]>=1)+(md00ygs[,,5:97]>=1) 
for (i in 1:93){
mi51_sum[,,i]<-ifelse(mi51_sum[,,i]==5,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])+(md00ygs[,,i+3]) + (md00ygs[,,i+4])), NA)
}        
mi52_sum <- (md00ygs[,,1:93]>=2)+(md00ygs[,,2:94]>=2)+(md00ygs[,,3:95]>=2)+(md00ygs[,,4:96]>=2)+(md00ygs[,,5:97]>=2)  
for (i in 1:93){
mi52_sum[,,i]<-ifelse(mi52_sum[,,i]==5,((md00ygs[,,i])+(md00ygs[,,i+1])+(md00ygs[,,i+2])+(md00ygs[,,i+3]) + (md00ygs[,,i+4])), NA)
}

#replace the 3 year drought areas with their summed anomalies, then sum all the anomalies for the drought

rnk_int <- data.frame(Year = 1918:2014, Intensity = NA)
for (i in 1:96){
	rnk_int[i+1,2]<- sum(mi20_sum[ , , i], na.rm = T)
	rnk_int[i+1,3]<- sum(mi21_sum[ , , i], na.rm = T)
	rnk_int[i+1,4]<- sum(mi22_sum[ , , i], na.rm = T)
}

for (i in 1:95){
	rnk_int[i+2,5]<- sum(mi30_sum[ , , i], na.rm = T)
	rnk_int[i+2,6]<- sum(mi31_sum[ , , i], na.rm = T)
	rnk_int[i+2,7]<- sum(mi32_sum[ , , i], na.rm = T)
}

for (i in 1:93){
	rnk_int[i+4,8]<- sum(mi50_sum[ , , i], na.rm = T)
	rnk_int[i+4,9]<- sum(mi51_sum[ , , i], na.rm = T)
	rnk_int[i+4,10]<- sum(mi52_sum[ , , i], na.rm = T)
}

rnk_int_m <- melt(rnk_int[,2:10])
colnames(rnk_int_m) <- c("Threshold", "Intensity")
rnk_int_m$Year <- rep(1918:2014, 9)
rnk_int_m$Window <- substr(rnk_int_m$Threshold, 1, 9)
rnk_int_m$Window <- rep(c("2 Year", "3 Year", "5 Year"), each = 291)

#int_lab<- as.data.frame(cbind(rnk_int$Year[which(rnk_int$Intensity >= 2000)], rnk_int$Intensity[which(rnk_int$Intensity >= 2000)]))

#int_grph <- ggplot(data = rnk_int_m, aes(x = Year, y = Intensity, color = Threshold)) + geom_line() + labs(title = "Sum of SD in drought areas, years -3 to 0", x = "Year", y = "Sum S.D.") + geom_label(data = int_lab, aes(x = V1, y = V2, label = V1), position = "jitter") +theme_minimal()


int_graph <- ggplot(data = rnk_int_m, aes(x = Year, y = Intensity, color = Threshold)) + geom_ribbon(aes(ymin = 0, ymax = Intensity, fill = Threshold), color = "black") + scale_fill_manual(name = "SD > ... ",values = c("darkgoldenrod1", "chocolate1","brown2","darkgoldenrod2", "chocolate2","brown3","darkgoldenrod3", "chocolate3","brown4"), labels = c("0", "1", "2","0", "1", "2","0", "1", "2"))+ facet_grid(Window~., scale = "free") + theme_minimal() + labs(title = "Drought Intensity by year, by theshold and window lengths",x = "Year", y = "Drought Intensity in Western States") 


mdfrq<-apply(mi31,c(1,2),sum,na.rm=T)
mdfrq[mdfrq == 0] <- NA
All = melt(mdfrq)
frac_drght <- sum(!is.na(mdfrq))/26026

mdfrq1_94<-apply(mi31[,,1:94],c(1,2),sum,na.rm=T)
mdfrq1_94[mdfrq1_94 == 0] <- NA
All_less = melt(mdfrq1_94)

mdfrq1_48<-apply(mi31[,,1:48],c(1,2),sum,na.rm=T)
mdfrq1_48[mdfrq1_48 == 0] <- NA
Half1 = melt(mdfrq1_48)

mdfrq48_95<-apply(mi31[,,48:95],c(1,2),sum,na.rm=T)
mdfrq48_95[mdfrq48_95 == 0] <- NA
Half2 = melt(mdfrq48_95)

mdfrq48_94<-apply(mi31[,,48:94],c(1,2),sum,na.rm=T)
mdfrq48_94[mdfrq48_94 == 0] <- NA
Half_less = melt(mdfrq48_94)

mdfrq95<-apply(mi31[,,95],c(1,2),sum,na.rm=T)
mdfrq95[mdfrq95 == 0] <- NA
Last1 = melt(mdfrq95)


frq2 <- melt(data.frame(ac_All = All[,3], ad_Half1 = Half1[,3], ae_Half2 = Half2[,3], bc_All_less = All_less[,3], bd_Half_less = Half_less[,3], be_Last = Last1[,3]))

frq2$variable <- rep(c("1920:2014","1920:1968","1968:2014","1920:2013","1968:2013","2014"), each = 26026)
frq2$Lat <- rep(All[,1], 6)
frq2$Lon <- rep(All[,2], 6)
colnames(frq2) <- c("variable", "Counts", "Lat", "Lon")

cnt_graph <- ggplot(frq2, aes(x = Lat, y = Lon)) + geom_tile(aes(fill = Counts)) + coord_equal() +  scale_fill_distiller(palette="Spectral", na.value="white") + xlim(c(-124.6875, -102.0625)) + ylim(c(31.1875, 48.9375)) + geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50") + facet_wrap(~ variable, ncol = 2, nrow = 3) + theme_minimal() + theme(strip.background = element_blank()) + ggtitle("Drought Core Occurrences")


#Decadal count graphs
mdfrq<-apply(mi31,c(1,2),sum,na.rm=T)
mdfrq[mdfrq == 0] <- NA
All1 = melt(mdfrq)
frac_drght <- sum(!is.na(mdfrq))/26026

mdfrq1_10<-apply(mi31[,,1:10],c(1,2),sum,na.rm=T)
mdfrq1_10[mdfrq1_10 == 0] <- NA
first2 = melt(mdfrq1_10)

mdfrq11_20<-apply(mi31[,,11:20],c(1,2),sum,na.rm=T)
mdfrq11_20[mdfrq11_20 == 0] <- NA
second3 = melt(mdfrq11_20)

mdfrq21_30<-apply(mi31[,,21:30],c(1,2),sum,na.rm=T)
mdfrq21_30[mdfrq21_30 == 0] <- NA
third4 = melt(mdfrq21_30)

mdfrq31_40<-apply(mi31[,,31:40],c(1,2),sum,na.rm=T)
mdfrq31_40[mdfrq31_40 == 0] <- NA
fourth5 = melt(mdfrq31_40)

mdfrq41_50<-apply(mi31[,,41:50],c(1,2),sum,na.rm=T)
mdfrq41_50[mdfrq41_50 == 0] <- NA
fifth6 = melt(mdfrq41_50)

mdfrq51_60<-apply(mi31[,,51:60],c(1,2),sum,na.rm=T)
mdfrq51_60[mdfrq51_60 == 0] <- NA
sixth7 = melt(mdfrq51_60)

mdfrq61_70<-apply(mi31[,,61:70],c(1,2),sum,na.rm=T)
mdfrq61_70[mdfrq61_70 == 0] <- NA
seventh8 = melt(mdfrq61_70)

mdfrq71_80<-apply(mi31[,,71:80],c(1,2),sum,na.rm=T)
mdfrq71_80[mdfrq71_80 == 0] <- NA
eighth9 = melt(mdfrq71_80)

mdfrq81_90<-apply(mi31[,,81:90],c(1,2),sum,na.rm=T)
mdfrq81_90[mdfrq81_90 == 0] <- NA
ninth0 = melt(mdfrq81_90)

mdfrq91_95<-apply(mi31[,,91:95],c(1,2),sum,na.rm=T)
mdfrq91_95[mdfrq91_95 == 0] <- NA
tenth11 = melt(mdfrq91_95)


frq3 <- melt(data.frame(All = All1[,3], first = first2[,3], second = second3[,3], third = third4[,3], fourth = fourth5[,3], fifth = fifth6[,3], sixth = sixth7[,3], seventh = seventh8[,3], eighth = eighth9[,3], ninth = ninth0[,3], tenth = tenth11[,3]))

frq3$variable <- rep(c("1920:2014","1920:1929","1930:1939","1940:1949","1950:1959","1960:1969","1970:1979","1980:1989","1990:1999","2000:2009","2010:2015"), each = 26026)
frq3$Lat <- rep(All[,1], 11)
frq3$Lon <- rep(All[,2], 11)
colnames(frq3) <- c("variable", "Occurrence", "Lat", "Lon")
frq3$variable <- factor(frq3$variable, levels = c("1920:2014","1920:1929","1930:1939","1940:1949","1950:1959","1960:1969","1970:1979","1980:1989","1990:1999","2000:2009","2010:2015"))

cnt_graph2 <- ggplot(frq3, aes(x = Lat, y = Lon)) + geom_tile(aes(fill = Occurrence)) + coord_equal() +  scale_fill_distiller(palette="Spectral", na.value="white") + xlim(c(-124.6875, -102.0625)) + ylim(c(31.1875, 48.9375)) + geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50") + facet_wrap(~ variable, ncol = 4, nrow = 3) + theme_minimal() + theme(strip.background = element_blank()) + ggtitle("Drought core occurrence by decade") + theme(legend.position = c(.85, .2))

#We have identified 2014, 1931, 1936, 1956, 2002, and 1961 as the years that deserve a closer look.  The plot from this code will be a 3 x 6 plot, with the year in question as the rightmost plot.
#1929:31, 34:36, 54:56, 59:61, 00:02, 12:14
mi1 <- ifelse((md00ygs[,,1:97]>=1)==1,1,NA)
mi1r <- md00ygs * mi1
mi21r <- md00ygs[,,2:97] * mi21
mi31r <- md00ygs[,,3:97] * mi31
dimnames(mi1r)[[3]] <- 1918:2014
dimnames(mi21r)[[3]] <- 1919:2014
dimnames(mi31r)[[3]] <- 1920:2014

nn <- melt(data.frame("1929" = melt(mi1r[,,"1929"])[,3],"1930" = melt(mi21r[,,"1930"])[,3],"1931" = melt(mi31r[,,"1931"])[,3],"1934" = melt(mi1r[,,"1934"])[,3],"1935" = melt(mi21r[,,"1935"])[,3],"1936" = melt(mi31r[,,"1936"])[,3],"1954" = melt(mi1r[,,"1954"])[,3],"1955" = melt(mi21r[,,"1955"])[,3],"1956" = melt(mi31r[,,"1956"])[,3],"1959" = melt(mi1r[,,"1959"])[,3],"1960" = melt(mi21r[,,"1960"])[,3],"1961" = melt(mi31r[,,"1961"])[,3],"2000" = melt(mi1r[,,"2000"])[,3],"2001" = melt(mi21r[,,"2001"])[,3],"2002" = melt(mi31r[,,"2002"])[,3],"2012" = melt(mi1r[,,"2012"])[,3],"2013" = melt(mi21r[,,"2013"])[,3],"2014" = melt(mi31r[,,"2014"])[,3]))

nn$Lat <- rep(melt(mi31r[,,"2014"])[,2], 18)
nn$Lon <- rep(melt(mi31r[,,"2014"])[,1], 18)
nn$Year <- rep(c(1931,1936,1956,1961,2002,2014), each = 26026*3 )
nn$Period <- rep(c("a","b","c"), each = 26026)

map_plt <- ggplot(nn) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + coord_equal() + xlim(c(-124.6875, -102.0625)) + ylim(c(31.1875, 48.9375)) + geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50") + scale_fill_distiller(palette="Spectral", na.value="white") + facet_grid(Period~Year) + theme_minimal() 
#This graph shows the progression of drought cores, with values equal to that years md00 zscores.  c is the final year in the 3 year window, a is two years before


#Line plots of md0, md00, tavg, and prec, for five years leading to the drought year, maybe one additional year, by water year.
dimnames(mi31)[[3]] <- 1920:2014

#each years core is expanded over the entire dataset.
mi3114 <- melt(aperm(apply(mi31[,,"2014"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))
mi3102 <- melt(aperm(apply(mi31[,,"2002"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))
mi3161 <- melt(aperm(apply(mi31[,,"1961"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))
mi3156 <- melt(aperm(apply(mi31[,,"1956"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))
mi3136 <- melt(aperm(apply(mi31[,,"1936"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))
mi3131 <- melt(aperm(apply(mi31[,,"1931"], 1:2, function(x) rep(x, each = 1164)), c(2,3,1)))

#note: veg_dr_lv works pretty well, so in the interest of keeping this code together (for now) i'm going to renames veg_dr_lv as all_var


all_var <- veg_dr_lv

#add to all_var before masking
all_var$mi3114 <- mi3114$value
all_var$mi3102 <- mi3102$value
all_var$mi3161 <- mi3161$value
all_var$mi3156 <- mi3156$value
all_var$mi3136 <- mi3136$value
all_var$mi3131 <- mi3131$value

#subset by year
all_var14<- filter(all_var, water_year >= 2009)
all_var02 <- filter(all_var, water_year >= 1997 & water_year <=2007)
all_var61 <- filter(all_var, water_year >= 1956 & water_year <=1966)
all_var56 <- filter(all_var, water_year >= 1951 & water_year <= 1961)
all_var36 <- filter(all_var, water_year >= 1931 & water_year <= 1941)
all_var31 <- filter(all_var, water_year >= 1926 & water_year <= 1936)

#Mask prec, tavg, and md00mgs
#2014
all_var14$mmd <- all_var14$mi3114 * all_var14$md00mgs
all_var14$pmc <- all_var14$mi3114 * all_var14$prec
all_var14$tmv <- all_var14$mi3114 * all_var14$tavg
#2002
all_var02$mmd <- all_var02$mi3102 * all_var02$md00mgs
all_var02$pmc <- all_var02$mi3102 * all_var02$prec
all_var02$tmv <- all_var02$mi3102 * all_var02$tavg
#1961
all_var61$mmd <- all_var61$mi3161 * all_var61$md00mgs
all_var61$pmc <- all_var61$mi3161 * all_var61$prec
all_var61$tmv <- all_var61$mi3161 * all_var61$tavg
#1956
all_var56$mmd <- all_var56$mi3156 * all_var56$md00mgs
all_var56$pmc <- all_var56$mi3156 * all_var56$prec
all_var56$tmv <- all_var56$mi3156 * all_var56$tavg
#1936
all_var36$mmd <- all_var36$mi3136 * all_var36$md00mgs
all_var36$pmc <- all_var36$mi3136 * all_var36$prec
all_var36$tmv <- all_var36$mi3136 * all_var36$tavg
#1931
all_var31$mmd <- all_var31$mi3131 * all_var31$md00mgs
all_var31$pmc <- all_var31$mi3131 * all_var31$prec
all_var31$tmv <- all_var31$mi3131 * all_var31$tavg

sub_var <- data.frame(mmd31 = all_var31$mmd, mmd36 = all_var36$mmd, mmd56 = all_var56$mmd, mmd61 = all_var61$mmd,mmd02 = all_var02$mmd, mmd14 = NA, pmc31 = all_var31$pmc, pmc36 = all_var36$pmc, pmc56 = all_var56$pmc, pmc61 = all_var61$pmc,pmc02 = all_var02$pmc, pmc14 = NA, tmv31 = all_var31$tmv, tmv36 = all_var36$tmv, tmv56 = all_var56$tmv, tmv61 = all_var61$tmv,tmv02 = all_var02$tmv, tmv14 = NA)
sub_var[1:1951950,  "mmd14"] <- all_var14$mmd
sub_var[1:1951950,  "pmc14"] <- all_var14$pmc
sub_var[1:1951950,  "tmv14"] <- all_var14$tmv
sub_var[,"year"] <- rep(-5:5, each = (12*143*182))


sub_var_m <- melt(sub_var)
sub_var_m$year <- as.factor(rep(rep(-5:5, each = (12*143*182)), 18))
sub_var_m$rel <- substr(sub_var_m$variable, 1,3)


sub_var$month <- all_var$month
sub_var$year <- all_var$year
sub_var$wateryear <- all_var$water_year
sub_var$lon <- all_var$lon
sub_var$lat <- all_var$lat




#md00 data.frame
aa<- data.frame(aggregate(mi3114 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "a")
colnames(aa) <-  c("year","value","letter")
aa <- aa[92:97,]
bb<- data.frame(aggregate(mi3102 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "b")
colnames(bb) <- c("year","value","letter")
bb <- bb[80:85,]
cc<-data.frame(aggregate(mi3161 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "c")
colnames(cc) <- c("year","value","letter")
cc <- cc[39:44,]
dd<-data.frame(aggregate(mi3156 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "d")
colnames(dd) <- c("year","value","letter")
dd <- dd[34:39,]
ee<-data.frame(aggregate(mi3136 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "e")
colnames(ee) <- c("year","value","letter")
ee <- ee[14:19,]
ff<-data.frame(aggregate(mi3131 ~ wateryear, data= sub_var, mean, na.rm = T, na.action = NULL), "f")
colnames(ff) <- c("year","value","letter")
ff <- ff[9:14,]

men_var<- rbind(ff,ee,dd,cc,bb,aa)
men_var$rel <- rep(-5:0,6)

aaa<- data.frame(
  sub_var %>%
  select(md0s14, wateryear) %>%
  filter(wateryear >= 2009 & wateryear <= 2014),
  "a")
colnames(aaa) <- c("md0s","year","letter")
bbb<- data.frame(
  sub_var %>%
  select(md0s02, wateryear) %>%
  filter(wateryear >= 1997 & wateryear <= 2002),
  "b")
colnames(bbb) <- c("md0s","year","letter")
ccc<- data.frame(
  sub_var %>%
  select(md0s61, wateryear) %>%
  filter(wateryear >= 1956 & wateryear <= 1961),
  "c")
colnames(ccc) <- c("md0s","year","letter")
ddd<- data.frame(
  sub_var %>%
  select(md0s56, wateryear) %>%
  filter(wateryear >= 1951 & wateryear <= 1956),
  "d")
colnames(ddd) <- c("md0s","year","letter")
eee<- data.frame(
  sub_var %>%
  select(md0s36, wateryear) %>%
  filter(wateryear >= 1931 & wateryear <= 1936),
  "e")
colnames(eee) <- c("md0s","year","letter")
fff<- data.frame(
  sub_var %>%
  select(md0s31, wateryear) %>%
  filter(wateryear >= 1926 & wateryear <= 1931),
  "f")
colnames(fff) <- c("md0s","year","letter")
##### This section is messed up somehow... Come back to it later
aaa<- data.frame(aggregate(md0s14 ~  wateryear + lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "a")
colnames(aaa) <- c("year","month","md0s","letter")
aaa <- aaa[1093:1164,]
bbb<- data.frame(aggregate(md0s02 ~ wateryear+ lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "b")
colnames(bbb) <- c("year","month","md0s","letter")
bbb <- bbb[949:1020,]
ccc<-data.frame(aggregate(md0s61 ~ wateryear+ lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "c")
colnames(ccc) <- c("year","month","md0s","letter")
ccc <- ccc[457:528,]
ddd<-data.frame(aggregate(md0s56 ~ wateryear+ lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "d")
colnames(ddd) <- c("year","month","md0s","letter")
ddd <- ddd[397:468,]
eee<-data.frame(aggregate(md0s36 ~ wateryear+ lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "e")
colnames(eee) <- c("year","month","md0s","letter")
eee <- eee[157:228,]
fff<-data.frame(aggregate(md0s31 ~ wateryear+ lon + lat, data= sub_var, mean, na.rm = T, na.action = NULL), "f")
colnames(fff) <- c("year","month","md0s","letter")
fff <- fff[97:168,]

md0tt <- rbind(fff,eee,ddd,ccc,bbb,aaa)

#precs
aaa1<- data.frame(aggregate(precs14 ~ month + year, data= sub_var, mean, na.rm = T, na.action = NULL), "a")
colnames(aaa1) <- c("year","month","precs","letter")
aaa1 <- aaa1[1093:1164,]
bbb1<- data.frame(aggregate(precs02 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "b")
colnames(bbb1) <- c("year","month","precs","letter")
bbb1 <- bbb1[949:1020,]
ccc1<-data.frame(aggregate(precs61 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "c")
colnames(ccc1) <- c("year","month","precs","letter")
ccc1 <- ccc1[457:528,]
ddd1<-data.frame(aggregate(precs56 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "d")
colnames(ddd1) <- c("year","month","precs","letter")
ddd1 <- ddd1[397:468,]
eee1<-data.frame(aggregate(precs36 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "e")
colnames(eee1) <- c("year","month","precs","letter")
eee1 <- eee1[157:228,]
fff1<-data.frame(aggregate(precs31 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "f")
colnames(fff1) <- c("year","month","precs","letter")
fff1 <- fff1[97:168,]


prectt <- rbind(fff1,eee1,ddd1,ccc1,bbb1,aaa1)

#tavgs
aaa2<- data.frame(aggregate(tavgs14 ~ month + year, data= sub_var, mean, na.rm = T, na.action = NULL), "a")
colnames(aaa2) <- c("year","month","tavgs","letter")
aaa2 <- aaa2[1093:1164,]
bbb2<- data.frame(aggregate(tavgs02 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "b")
colnames(bbb2) <- c("year","month","tavgs","letter")
bbb2 <- bbb2[949:1020,]
ccc2<-data.frame(aggregate(tavgs61 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "c")
colnames(ccc2) <- c("year","month","tavgs","letter")
ccc2 <- ccc2[457:528,]
ddd2<-data.frame(aggregate(tavgs56 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "d")
colnames(ddd2) <- c("year","month","tavgs","letter")
ddd2 <- ddd2[397:468,]
eee2<-data.frame(aggregate(tavgs36 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "e")
colnames(eee2) <- c("year","month","tavgs","letter")
eee2 <- eee2[157:228,]
fff2<-data.frame(aggregate(tavgs31 ~ month +year, data= sub_var, mean, na.rm = T, na.action = NULL), "f")
colnames(fff2) <- c("year","month","tavgs","letter")
fff2 <- fff2[97:168,]

tavgtt <- rbind(fff2,eee2,ddd2,ccc2,bbb2,aaa2)

men_var_s <- melt(data.frame(md0s = md0tt[,3], precs = prectt[,3], tavgs = tavgtt[,3]))
men_var_s$letter <- rep(rep(c("1931", "1936", "1956", "1961", "2002", "2014"), each = 72), 3)

men_var_s$rel <- as.factor(rep(rep(-5:0, 6, each = 12), 3))

bx_graph <- ggplot(men_var_s, aes(x = rel, y = value, fill = letter)) + geom_boxplot() + facet_grid(variable~., scales = "free") + theme_minimal() + labs(title = "Variables at Cores", x = "Years Previous", y = "Z-score")
ln_graph <- ggplot(men_var, aes(x = rel, y = value, color = letter)) + geom_line() + theme_minimal()

#note: Something is up with these scaled values.  It might be that they've been scaled across the entire dataset and instead must be scaled by individual months

#Bam model between md0, prec, and tavg z-scores over the entire data-set.  Should it be by water-year? That shouldn't matter, I think.  

#yprec <- aggregate(prec ~ lon+lat+water_year, all_var, mean, na.rm = T) 
#ytavg <- aggregate(tavg ~ lon+lat+water_year, all_var, mean, na.rm = T)
#ymd0 <- aggregate(md0 ~ lon+lat+water_year, all_var, mean, na.rm = T)
#yprecs <- aggregate(precs ~ lon+lat+water_year, all_var, mean, na.rm = T) 
#ytavgs <- aggregate(tavgs ~ lon+lat+water_year, all_var, mean, na.rm = T)
#ymd0s <- aggregate(md0s ~ lon+lat+water_year, all_var, mean, na.rm = T)

#dmmbb <- data.frame(lon = yprec[,1], lat = yprec[,2], year = yprec[,3], prec = yprec[,4],tavg = ytavg[,4],md0 = ymd0[,4], precs = yprecs[,4],tavgs = ytavgs[,4],md0s = ymd0s[,4])
#dmmbb$precs <- as.numeric(dmmbb$precs )
#dmmbb$tavgs <- as.numeric(dmmbb$tavgs )
#dmmbb$md0s <- as.numeric(dmmbb$md0s )
#mi3rm <- cbind(mi31 )
#dmmbb$mi31 <- mi3rm
#dmmbb[,"mi31"] <- matrix(mi31, ncol = 1)

#Bam model of md0 zscores ~ prec + tavg z scores, in water years 1919:2014 ( oct 1918 to sept 2014
#dmmbb_bam <- bam(md0s ~ s(precs,tavgs), data = filter(dmmbb, year <= 2014))
#summary(dmmbb_bam)
#gm_graph <- vis.gam(dmmbb_bam, view= c("precs", "tavgs"), plot.type = "contour", too.far = 0.1)
#dmmbb_bam_ts <- bam(md0s ~ ts(precs,tavgs), data = filter(dmmbb, year <=2014))
#summary(dmmbb_bam)
#gm_chk_graph<-gam.check(dmmbb_bam)



#points(x = precs, y = tavgs, data = dmmbb)

#to do: scale md0, tavg, and prec
md000 <- aggregate(md0 ~ lat + lon + water_year, data = all_var, FUN = sum, na.rm = T, na.action = NULL)
mdf000 <- filter(md000, water_year >= 1919, water_year <= 2014)
mdf000$m <- scale(mdf000$md0)

#Does the aggregated md0 match md00?
md0000 <- aggregate(md00 ~ lat + lon + year, data = all_var, FUN = mean, na.rm = T, na.action = NULL)
md000f <- aggregate(md0 ~ water_year, data = mdf000, FUN = mean)
plot(md000f$water_year, md000f$md0)
lines(md0000$year, md0000$md00)

#back to scaling things
prc0 <- aggregate(prec ~ lat + lon + water_year, data = all_var, FUN = sum, na.rm = T,na.action = NULL)
prcf0 <- filter(prc0, water_year >= 1919, water_year <= 2014)
prcf0$p <- scale(prcf0$prec)

tavg0 <- aggregate(tavg ~ lat + lon + water_year, data = all_var, FUN = mean, na.rm = T,na.action = NULL)
tavgf0 <- filter(tavg0, water_year >= 1919, water_year <= 2014)
tavgf0$tt <- scale(tavgf0$tavg)




dn <- data.frame(md0 = mdf000$md0,prec = prcf0$prec,tavg = tavgf0$tavg, md0s = mdf000$m,precs = prcf0$p,tavgs = tavgf0$t, year= mdf000$water_year, lat = tavgf0$lat, lon = tavgf0$lon)

rasted <- ggplot(data = dn, aes(x = precs, y = tavgs)) + geom_contour(aes(z = md0s))


dn_bam <- bam(md0stan_an ~ s(precstan_an, tavgstan_an), data = veg_dr_lv)
vis.gam(dn_bam, view = c("prec","tavg"), plot.type = "contour", too.far = .1)

newdata <- data.frame(precs = rep(seq(-1.04724, 14.92760, length.out = 200), 200), tavgs = rep(seq(-3.5408, 3.4682, length.out = 2000), each = 200))
prednd <- predict.gam(dn_bam, newdata)
newdata$md0s <- prednd


bi14 <- filter(mi3114, value == 1)
lat14 <- unique(bi14$X2)
lon14 <- unique(bi14$X1)
pnts <- filter(dn, year == 2014 & lat %in% lat14 & lon %in% lon14)

bi31 <- filter(mi3131, value == 1)
lat31 <- unique(bi31$X2)
lon31 <- unique(bi31$X1)
pnts2 <- filter(dn, year == 1931 & lat %in% lat31 & lon %in% lon31)

bi36 <- filter(mi3136, value == 1)
lat36 <- unique(bi36$X2)
lon36 <- unique(bi36$X1)
pnts4 <- filter(dn, year == 1936 & lat %in% lat36 & lon %in% lon36)

bi02 <- filter(mi3102, value == 1)
lat02 <- unique(bi02$X2)
lon02 <- unique(bi02$X1)
pnts3 <- filter(dn, year == 2002 & lat %in% lat02 & lon %in% lon02)


prtvmd_plt1 <- ggplot(newdata, aes(x = precs, y = tavgs)) + geom_raster(aes(fill = md0s)) + stat_contour(aes(z = md0s), color = "black") + geom_point(data = pnts, aes(x = precs, y = tavgs), shape = 4, size = .5, alpha = 1/10, color = "black")  + scale_fill_distiller(palette = "YlOrBr") + theme_minimal() + labs(title = "Moisture Deficit as function of Precipitation, Temperature, 2014 points", x = "Normalized Cumulative Water-Year Precipitation", y = "Normalized Average Water-Year Temperature")

prtvmd_plt2 <- ggplot(newdata, aes(x = precs, y = tavgs)) + geom_raster(aes(fill = md0s)) + stat_contour(aes(z = md0s), color = "black") + geom_point(data = pnts2, aes(x = precs, y = tavgs), shape = 4, size = .5, alpha = 1/10, color = "black")  + scale_fill_distiller(palette = "YlOrBr") + theme_minimal() + labs(title = "Moisture Deficit as function of Precipitation, Temperature, 1931 points", x = "Normalized Cumulative Water-Year Precipitation", y = "Normalized Average Water-Year Temperature")

prtvmd_plt3 <- ggplot(newdata, aes(x = precs, y = tavgs)) + geom_raster(aes(fill = md0s)) + stat_contour(aes(z = md0s), color = "black") + geom_point(data = pnts3, aes(x = precs, y = tavgs), shape = 4, size = .5, alpha = 1/10, color = "black")  + scale_fill_distiller(palette = "YlOrBr") + theme_minimal() + labs(title = "Moisture Deficit as function of Precipitation, Temperature, 2002 points", x = "Normalized Cumulative Water-Year Precipitation", y = "Normalized Average Water-Year Temperature")

prtvmd_plt4 <- ggplot(newdata, aes(x = precs, y = tavgs)) + geom_raster(aes(fill = md0s)) + stat_contour(aes(z = md0s), color = "black") + geom_point(data = pnts4, aes(x = precs, y = tavgs), shape = 4, size = .5, alpha = 1/10, color = "black")  + scale_fill_distiller(palette = "YlOrBr") + theme_minimal() + labs(title = "Moisture Deficit as function of Precipitation, Temperature, 1936 points", x = "Normalized Cumulative Water-Year Precipitation", y = "Normalized Average Water-Year Temperature")




#Kruskal _ wallis test to compare yearly distributions of md0

all_var$mi31 <- NA
mi31mon <- aperm(apply(mi31, 1:2, function(x) rep(x, each = 12)), c(2,3,1))
mil <- melt(mi31mon)
all_var[624625:30294264,"mi31"]<-mil$value

mi0 <- aggregate(mi31 ~ lat + lon + year, data = all_var, FUN = mean)
mif0 <- filter(tavg0, water_year >= 1919, water_year <= 2014)
tavgf0$tt <- scale(tavgf0$tavg)

all_var$md0_msk <- all_var$md0 * all_var$mi31
all_var$prec_msk <- all_var$prec * all_var$mi31
all_var$tavg_msk <- all_var$tavg * all_var$mi31

kruskal.test(md0_msk ~ water_year, data = all_var)
#Kruskal-Wallis rank sum test

#data:  md0_msk by water_year
#Kruskal-Wallis chi-squared = 41284, df = 86, p-value < 2.2e-16

kruskal.test(prec_msk ~ water_year, data = all_var)
#Kruskal-Wallis rank sum test

#data:  prec_msk by water_year
#Kruskal-Wallis chi-squared = 33672, df = 86, p-value < 2.2e-16


kruskal.test(tavg_msk ~ water_year, data = all_var)
#Kruskal-Wallis rank sum test

#data:  tavg_msk by water_year
#Kruskal-Wallis chi-squared = 27351, df = 86, p-value < 2.2e-16

sapply(1920:2013, function(i){
	wilcox.test(md0_msk ~ water_year, data = all_var, subset = water_year %in% c(i, 2014))$p.value
	})
sapply(1920:2013, function(i){
	wilcox.test(prec_msk ~ water_year, data = all_var, subset = water_year %in% c(i, 2014))$p.value
	})
sapply(1920:2013, function(i){
	wilcox.test(tavg_msk ~ water_year, data = all_var, subset = water_year %in% c(i, 2014))$p.value
	})
	
#not quite working	
wil_p_val <- data.frame(1:95,1:95,1:95)
a1 <- filter(all_var, water_year == 2014)

for (i in 1920:2014){
	a2 <- filter(all_var, water_year == i)
	if(sum(!is.na(a2$md0_msk)) == 0){
		a2[is.na(a2)] <- 0
		}
	wil_p_val[i-1919, 1] <- wilcox.test(as.numeric(a1$md0_msk), as.numeric(a2$md0_msk))$p.value
	wil_p_val[i-1919, 2] <- wilcox.test(as.numeric(a1$prec_msk), as.numeric(a2$prec_msk))$p.value
	wil_p_val[i-1919, 3] <- wilcox.test(as.numeric(a1$tavg_msk), as.numeric(a2$tavg_msk))$p.value
	}

colnames(wil_p_val) <- c("md0","prec","tavg")
wil_m <- melt(wil_p_val)
wil_m$rel <- rep(1920:2014, 3)
ggplot(data = wil_m,aes(rel, fill = value)) + geom_bar(position = "dodge")


# standarizing data again, by grid cell, by month

tessd <- aggregate(md0 ~ month + lat + lon, data = all_var, sd, na.rm = T, na.action = NULL)
tesmean <- aggregate(md0 ~ month + lat + lon, data = all_var, mean, na.rm = T, na.action = NULL)

all_var_test <- data.frame(all_var[,1:4], md0 = all_var$md0, water_year = all_var$water_year, tesmean_all, tessd_all)

all_var_test$md0s <- (all_var_test$md0 - all_var_test$tesmean_all) / (all_var_test$tessd_all)  