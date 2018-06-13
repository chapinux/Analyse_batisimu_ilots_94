
library(dplyr)
library(readr)
library(ggplot2)
library(ggExtra)
setwd("/home/paulchapron/dev/Analyse_batisimu_ilots_94/")


df <- read_delim("results_94/output_clean.csv", delim = ";", trim_ws = T,col_types = c("ccnn"), na = "NA" )
names(df) <- c("dir", "idpar", "nbo", "sdp")
head(df)

plot(df)

# case -1:
#   status = "NO RULE FOUND";
# break;
# case -2:
#   status = "TRIANGULATION ERROR";
# break;
# case -42:
#   status = "FILTERED";
# break;
# case -88:
#   status = "MINIMUM PARCEL AREA TOO BIG";

df %>%
  filter(nbo==-2 | nbo == -1 | nbo == -88 | is.na(nbo)) %>%
  group_by(nbo ) %>%
  summarize(count=n())

#on arrondit au metre la surface de plancher

df$sdp <-  floor(df$sdp)



#plot surface / nb objets
df2 <- filter(df, nbo > 0 & !is.na(nbo))
df2 %>%
  group_by(nbo) %>%
  summarise(count=n())

p<- ggplot(df2, aes(nbo, sdp))+
  geom_count(alpha=0.2, color="#7A67EE")+
   scale_size_area()+
  theme_bw()+
  labs(x= "nombre d'objets", y = "surface de plancher", size="nombre\nde simus")
p
ggMarginal(p,type="histogram")


library(sf)
sim <-  st_read("./17887/simul_17887_true_no_demo_sampler.shp")
plot(sim)

setwd("./results_94/")
originwd <- getwd()

dfilots <- tibble(Longueur=numeric(), Largeur = numeric(), Hauteur=numeric(),Rotation=numeric(),ID_PARC=integer(), Aire=numeric(), Volume=numeric(), imu=integer() ,idpar=character(), geometry=st_multipolygon() )

df2

df3 <- head(df2, 50)


setwd(originwd)

getwd()

for (didi in unique(df3$dir)){
  mypath <-  paste(getwd(),"/",didi,"/",sep = "")
  cat("path construit", mypath,"\n")
  setwd(file.path(mypath))
  cat("path courant", getwd(),"\n")
  cat((list.files(file.path(mypath), pattern = "*.shp")),"\n")
    
  setwd(originwd)
}


for (f in list.dirs(originwd)){
  setwd(file.path(f))
  if(length(list.files(file.path(f), pattern = "*.shp")) == 1 ){
    #cat("un SHP trouvé ! ")
  }
  else{
    cat("pas de SHP ! ")
    cat(file.path(f), "\n")
    cat(list.files(file.path(f), pattern = "*.shp"))
  }
  }

setwd(originwd)
yy <-  rbind(dfilots, sim)








