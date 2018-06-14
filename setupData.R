
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


# on boucle sur les repertoires listés dans df2 , donc contenant des parcelles construites,
#on concactène tout sans un gros dataframe 
for (didi in head(unique(df2$dir)),500){
  mypath <-  paste(getwd(),"/",didi,"/",sep = "")
  setwd(file.path(mypath))
  lili <- list.files(file.path(mypath), pattern = "*.shp")
  if(length(lili)>1){cat("Plus d'un SHP dans le repertoire , refaire la boucle !!!")}
  currentshp <- st_read(file.path(lili[1]))
  sim <- rbind(sim,currentshp )      
  setwd(originwd)
}

#lili <- list.files(file.path(getwd()), pattern = "*.shp", recursive = T)

#dataframe des attributs
lightdf








