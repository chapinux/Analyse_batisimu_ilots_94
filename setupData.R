
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

# cas particuliers 
df %>%
  filter(nbo==-2 | nbo == -1 | nbo == -88 | is.na(nbo)) %>%
  group_by(nbo ) %>%
  summarize(count=n())

#on arrondit au metre la surface de plancher

df$sdp <-  floor(df$sdp)


#on ne garde que les parcelles effectivement construites
df2 <- filter(df, nbo > 0 & !is.na(nbo))


#plot surface / nb objets
df2 %>%
  group_by(nbo) %>%
  summarise(count=n())

p<- ggplot(df2, aes(nbo, sdp))+
  geom_count(alpha=0.2, color="#7A67EE")+
   scale_size_area()+
  theme_bw()+
  labs(x= "nombre d'objets", y = "surface de plancher", size="nombre\nde simus")
p
ggMarginal(p,type="histogram", color="#6945BB", fill="#7A67EE")


#lecture des shp
library(sf)


#on lit un shp à part pour avoir un objet de la structure des autres résultats pour concaténer
sim <-  st_read("./17887/simul_17887_true_no_demo_sampler.shp")
plot(sim)

setwd("./results_94/")
originwd <- getwd()


ll <- length(unique(df2$dir))
dirs <- unique(df2$dir) 
# on boucle sur les repertoires listés dans df2 , donc contenant des parcelles construites,
#on concactène tout sans un gros dataframe 
# boucle très lente

for (didi in dirs){
  cat(match(didi, dirs), "/", ll, "\n")
  mypath <-  paste(getwd(),"/",didi,"/",sep = "")
  setwd(file.path(mypath))
  lili <- list.files(file.path(mypath), pattern = "*.shp")
  if(length(lili)>1){cat("Plus d'un SHP dans le repertoire , refaire la boucle !!!")}
  currentshp <- st_read(file.path(lili[1]), quiet = T)
  sim <- rbind(sim,currentshp )      
  setwd(originwd)
}


getwd()

# tous les shp
lili <- list.files(pattern = "*.shp", recursive = T)

library(parallel)

Sys.time()
res <- foreach(f=head(lili, 1000),
        .combine=rbind) %dopar% st_read(f)
Sys.time()



nbcores <-  detectCores() -1 
cl <-makeCluster(nbcores)


Sys.time()
parLapply( cl , lili, st_read)
Sys.time()




concaten <- function(fifi){
    myshp <- st_read(fifi, quiet = T)
    if(length(myshp) > 0){
    sim <- rbind(sim,myshp )
    }      
}



sim$centro <- st_centroid(sim$geometry)

#lili <- list.files(file.path(getwd()), pattern = "*.shp", recursive = T)

#dataframe des attributs
lightdf <- sim  %>% select(Longueur, Largeur, Hauteur, Rotation, Aire , Volume,imu_dir, idpar, centro)


plot(lightdf$centro, col= lightdf$Aire , cex=0.0000001)

qplot(lightdf$Aire, xlab="Aire construite", ylab="nombre d'individus", bins=80, colour=I("black"), fill=I("pink"))

getwd()
setwd("/home/paulchapron/dev/Analyse_batisimu_ilots_94")
#sauvegarde du df nettoyé 
write_csv(lightdf, "lightdfAttribEtCentroides.csv")
lightdf <- read_csv("lightdfAttribEtCentroides.csv")



#plot d'îlot

library(ggplot2)






plot(lightdf %>% filter(imu_dir == 17887) )

pp<- ggplot(lightdf)+
    geom_sf()    

pp
  





#header du fichier test.cv :
testcsv <- read_csv("test.csv")
names(testcsv) <- c("gid","pid","rid","idpar","idblock","libelle_zo","date_dul","libelle_de","libelle__1","fonctions","top_zac","top_zac2","bande1","typ_bande2","bande2","typ_bande1","art_51","art_61","art_711","art_721","art_731","art_741","art_81","art_91","art_10_top","art_101","art_121","art_131","art_141","art_52","art_62","art_712","art_722","art_732","art_742","art_82","art_92","art_10_t_1","art_102","art_122","art_132","art_142","insee","object_id","dep","annee","statut_dul","b1_haut_m","b2_haut_m","b1_haut_mt","b1_art_9_t","correction","fonctions2","b2_zon_cor","zonage_coh","simul","aire_inter","mos2012","geom","imu","status","nb_objects","floor_area","aire","airesurperim","convexite","densite")
summary(testcsv)  





