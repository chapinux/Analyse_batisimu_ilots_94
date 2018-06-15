
library(dplyr)
library(readr)
library(ggplot2)
library(ggExtra)
library(sf)
library(parallel)
library(factoextra)
library(ade4)


setwd("/home/paulchapron/dev/Analyse_batisimu_ilots_94")


df <- read_csv("test.csv")
names(df) <- c("gid","pid","rid","idpar","idblock","libelle_zo","date_dul","libelle_de","libelle__1","fonctions","top_zac","top_zac2","bande1","typ_bande2","bande2","typ_bande1","art_51","art_61","art_711","art_721","art_731","art_741","art_81","art_91","art_10_top","art_101","art_121","art_131","art_141","art_52","art_62","art_712","art_722","art_732","art_742","art_82","art_92","art_10_t_1","art_102","art_122","art_132","art_142","insee","object_id","dep","annee","statut_dul","b1_haut_m","b2_haut_m","b1_haut_mt","b1_art_9_1","correction","fonctions2","b2_zon_cor","zonage_coh","simul","aire_inter","mos2012","geom","imu","status","nb_objects","floor_area","aire","airesurperim","convexite","densite")


# on supprime certaines colonnes mais on garde idpar 
dflight <- subset(df,select =-c(
  gid,pid,rid,idblock,libelle_zo,date_dul,libelle_de,libelle__1,fonctions,top_zac,top_zac2, bande2,typ_bande1,art_91,art_10_top,art_141,art_92,art_10_t_1,art_101,art_102,art_142,insee,object_id,dep,annee,statut_dul,b1_art_9_1, b1_haut_mt,correction,fonctions2,b2_zon_cor,zonage_coh,simul,aire_inter,mos2012,imu, status)
)


# on ne garde pas la geom
dflight <- dflight%>%select(-geom) 

# on nettoie les colonnes articles à part
cleanArt <- dflight %>% select(matches("art|idpar"))
cleanArt[cleanArt == 99] <- NA
cleanArt[cleanArt == 88] <- NA

#on vire les colonnes articles du df 
dflight <- dflight%>%select(-matches("art"))


#on joint avec les articles netooyes
dflight <- inner_join(dflight,cleanArt, by="idpar")

# quand c'est non  renseigné -> NA 
dflight$bande1[dflight$bande1 == 99] <- NA
dflight$bande1[dflight$bande1 == 88] <- NA


# hauteur par défaut 25 
dflight$b1_haut_m[dflight$b1_haut_m == 99] <- 25
dflight$b1_haut_m[dflight$b1_haut_m == 88] <- 25

dflight$b2_haut_m[dflight$b2_haut_m == 99] <- 25
dflight$b2_haut_m[dflight$b2_haut_m == 88] <- 25


# pour ces deux articles quand c pas renseigné c'est 0
dflight$art_131[is.na(dflight$art_131)] <- 0 
dflight$art_132[is.na(dflight$art_132)] <- 0 


#nb objets code -42
dflight$nb_objects[dflight$nb_objects==-42] <- NA


dflight$typ_bande2

names(dflight)

dd1 <-  dflight %>% filter(typ_bande2 == 2  )
dd0 <-  dflight %>% filter(typ_bande2 == 0  )


dfsanstrous <- na.omit(dd0)

mypca <- dudi.pca(dfsanstrous[,-1], center=T, scannf = F, nf= 29 )


dflight$bande1

#dessin
fviz_eig(mypca)
fviz_pca_var(mypca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
