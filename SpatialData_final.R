#Modelling Coeini species
#Spatial data definida

setwd("~/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué")
# Load libraries
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(viridis)
library(maps)
library(biomapME)
#install.packages("biomapME")
library(usdm)
library(elevatr)
library(RColorBrewer)
library(stringr)
library(dismo)
library(sdm)
library(spThin)
library(dplyr)
library(adehabitatHR)
library(ENMeval)
library(ggplot2)
library(sdm)

#Neotropical map
Neotropico<-readOGR("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/Neotropico_modelling.shp")

spp_coords <- read.table("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/data.txt", h=T)
colnames(spp_coords)=c("species",'lat', 'long')
occs.xy <- spp_coords[c('long', 'lat')]
sp::coordinates(occs.xy) <- ~ long + lat

#plot(Neotropico)
#plot(occs.xy,add=T)
#plot(occs.xy)
#plot(Neotropico,add=T)
bgExt <- adehabitatHR::mcp(occs.xy, percent = 100) # Create a polygon surrounding points (MCP)
#plot(bgExt, add=T) # Plot the MCP
bgExt_buff <- rgeos::gBuffer(bgExt, width = 3) # Adding a buffer around the polygon
#plot(bgExt_buff,border="red", add=T) # Plot the buffer
#plot(occs.xy, add=TRUE, pch=16,cex=.5, col="red") # Plot species points

#Current layers (bio15, bio18, bio2, bio8, modis 2010 e roughness)
#comando de ler os rasters - para nao precisar rodar os layers de novo
Current_land2 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/Current_land2.grd")
#Current_land2<-subset(Current_land2, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#Cenários futuros
#IP_2050_RCP26
IP_2050_RCP26 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/IP_2050_RCP26.grd")
#IP_2050_RCP26<-subset(IP_2050_RCP26, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#IP_2050_RCP85
IP_2050_RCP85 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/IP_2050_RCP85.grd")
#IP_2050_RCP85<-subset(IP_2050_RCP85, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#IP_2100_RCP26
IP_2100_RCP26 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/IP_2100_RCP26.grd")
#IP_2100_RCP26<-subset(IP_2100_RCP26, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#IP_2100_RCP85
IP_2100_RCP85 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/IP_2100_RCP85.grd")
#IP_2100_RCP85<-subset(IP_2100_RCP85, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#UK_2050_RCP26
UK_2050_RCP26 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/UK_2050_RCP26.grd")
#UK_2050_RCP26<-subset(UK_2050_RCP26, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#UK_2050_RCP85
UK_2050_RCP85 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/UK_2050_RCP85.grd")
#UK_2050_RCP85<-subset(UK_2050_RCP85, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#UK_2100_RCP26
UK_2100_RCP26 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/UK_2100_RCP26.grd")
#UK_2100_RCP26<-subset(UK_2100_RCP26, c("bio_15","bio_18","bio_2","bio_8","roughness"))

#UK_2100_RCP85
UK_2100_RCP85 = raster::stack("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/spatialData/UK_2100_RCP85.grd")
#UK_2100_RCP85<-subset(UK_2100_RCP85, c("bio_15","bio_18","bio_2","bio_8","roughness"))

# Extracting points only in the Amazon basin (tira isso aqui de dentro do loop!)
amazonia_shp = readOGR("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/amazon_basin/amazlm_poly_1608.shp")

#coordenadas
spp_coords <- read.table("/Users/isabelafreitasoliveira/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué/data.txt", h=T) # list of species occurrences - use the same format if you add more occurrences
head(spp_coords) 
(targetSpecies <- unique(spp_coords$Species))
colnames(spp_coords) = c("species","lat","long")

species_list = unique(spp_coords$species)#[1:3]
species_list

#
thinning_data = function(species_list){
  spp_coords_for_thin = spp_coords[spp_coords$species == species_list,]
  dat <-thin( loc.data = spp_coords_for_thin,
              lat.col = "lat", long.col = "long",
              spec.col = "species",
              thin.par = 13, reps = 1,
              locs.thinned.list.return = TRUE,
              write.files = FALSE,
              write.log.file = FALSE)
  dat = as.data.frame(dat)
  dat$species = species_list
  dat = dat[,c(3,1,2)]
  colnames(dat) = c("species", "long", "lat")
  return(dat)
}

spp_coords_th_f = lapply(species_list,thinning_data)

spp_coords_thinned = bind_rows(spp_coords_th_f) # here is our thinned data

#Saber quantas ocorrencias foram perdidas de cada espécie
#nrow(spp_coords_thinned[spp_coords_thinned$species=="Colobura_dirce",])

#testt = spp_coords_thinned[spp_coords_thinned$species=="Colobura_dirce",]
#coordinates(testt) = testt[,2:3]
#nas= raster::extract(Current_land2[[1]],testt)
#testt$nas = nas

#plot(Neotropico2)
#plot(testt[is.na(testt$nas),],add=T,col="red")
#locator() # saber a coordenada no mapa - dar esc depois que clicar no mapa


nrow(spp_coords) # original data
nrow(spp_coords_thinned) # thinned data (way less data, see?) - retira as ocorrencias da mesma espécie em uma mesma celula? 

head(spp_coords_thinned)
tail(spp_coords_thinned)

# Se não quiser rodar novamente, salvar em csv
write.csv(spp_coords_thinned, "spp_coords_thinned.csv")
spp_coords_thinned = read.csv("spp_coords_thinned.csv")
spp_coords_thinned = spp_coords_thinned[,c("species","long","lat")]

inc <- table(spp_coords_thinned$species)
inc # Table with number of records per species for selection
inc_full_model <- names(inc[inc > 11]) # more then 11 records
inc_full_model

species_to_model <- names(inc[inc > 20]) # spp with more then 20 records
species_to_model

# This line subsets the points to contain only the species in the above selection for species with more than 20 records (the minimal number of records depends also in the number of environmental variables you have selected. I like to keep a balance of at least 5 species records after thinned per variable). You can either use less variables or try an ensemble of small models. Or you could risk a bit and model species with 6 or more records, though the models might be a bit less accurate
full_presence_data_clean <- spp_coords_thinned[spp_coords_thinned$species %in% species_to_model,]
head(full_presence_data_clean)
tail(full_presence_data_clean)

# Set a species list for modelling
species_list = unique(full_presence_data_clean$species)
species_list

# Create a table for adding the model evaluation results
evaluation_SDMs <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("Species", "AUC", "TSS", "Kappa", "Threshold")
colnames(evaluation_SDMs) <- x  
write.table(evaluation_SDMs,"evaluation_SDMs") # Take care that if you have run some models before, this line will erase the evaluations

as.data.frame(species_list)
i=3 # número de cada espécie.

# Create a directory (if already do not exists called output_SDM)
#dir.create("output_SDM")

###Aqui começa o loop

# Create a directory (if already do not exists called output_SDM)
#dir.create("output_SDM")

for (i in 1: length(species_list)) {
  
  species = species_list[i]
  print(paste0("Begin modelling for ","sp", i,": ", species_list[i]))
  genero <- word(species, 1, sep="_")
  especie <- word(species, 2, sep="_")
  
  # Move back to the working directory
  setwd("~/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué")
  # Create a MCP and use it as the background for each species
  occs.xy.z <- full_presence_data_clean[full_presence_data_clean$species == species_list[i],]
  
  occs.xy <- occs.xy.z[c('long', 'lat')]
  sp::coordinates(occs.xy) <- ~ long + lat
  bgExt <- mcp(occs.xy, percent = 100) 
  bgExt <- rgeos::gBuffer(bgExt, width = 100) # 100 graus de buffer para cortar
  
  # Crop env raster to the species background
  envsBgCrop <- raster::crop(Current_land2, bgExt)
  envsBgMsk <- raster::mask(envsBgCrop, bgExt)
  
  #plot(envsBgMsk)
  
  fun <- function() {
    plot(bgExt, add=TRUE, lty=2, lwd=0.5)
    points(occs.xy@coords, bg = "red", pch = 21)
  }
  plot(envsBgMsk[[1]], addfun=fun) 
  
  # Below I created an empty raster to add the species points to it
  bck.na<-envsBgMsk[[1]]
  bck.na[]<-NA
  
  # This is just to make sure we have only one record per grid cell (necessary only when not using spthin as we did before, but I will maintain it here, as it wont change anything but will keep the script the same regardless of using spthin)
  r<-rasterize(coordinates(occs.xy.z[,c('long', 'lat')]),bck.na,fun='count')
  
  occ_data_clean_ok.pa<-rasterToPoints(r,fun=function(x){x>0}, spatial=T) 
  ###
  points(occ_data_clean_ok.pa@coords, bg = "blue", pch = 21)
  
  # Here I will create the pseudo-absences. For modelling, we want to have not only the presence records, but also the places where we do not have the species. As absence records are rare for most species, we generate a random distribution of points to call them pseudo-absences and we use it for modelling. For Maxent, a large number of pseudo absences is generally required and we have the loop below indicating to produce more or less random points according to the extent of the climate raster of each species cropped above.
  
  # if (ncell(envsBgMsk)>200000){
  #    bg <- randomPoints(envsBgMsk,20000)
  #  } else {
  #    bg <- randomPoints(envsBgMsk,ncell(envsBgMsk)*0.1)
  #  }
  
  bg <- randomPoints(envsBgMsk,1000)
  
  # Just save as another object
  envsBgMsk_scale = envsBgMsk
  
  # To set k-fold number. When modelling, we can separated our data in test and training. In the case of the evaluation method randomkfold, depending on the number of species records, we can use 5 divisions of the data or ten.
  #mmm = 10
  #mmm = ifelse(nrow(occ_data_clean_ok.pa@data)<25, 10 ,5)
  
  # And here is to select the evaluation method. Generally, when you have too few points, it is better to use jackknife or bootstrap
  #metodo = "randomkfold" # Se eu já tiver escolhido anteriomente
  #metodo = ifelse(nrow(occ_data_clean_ok.pa@data)<21, "jackknife" ,'checkerboard2')
  
  # If you have enough data (25 or more records), I would recommend to use the block method instead of randomkfold as in the example above.
  
  # Finally, let's model the species distribution/niche. Take a look at the help of the function ENMevaluate, see all options. This function helps to run all tunning sets that we could test for fitting a model.
  
  #Modelagem
  #install.packages("rJava", dependencies = TRUE)
  #library(rJava)
  
  mx3 <- ENMevaluate(occs = coordinates(occ_data_clean_ok.pa), envs = envsBgMsk_scale, bg = bg, partitions = 'checkerboard2', algorithm = "maxent.jar",
                     RMvalues=seq(0.5,5,0.5),
                     fc=c("L","LQ","LQP"),
                     parallel = TRUE,numCores=4,
                     categoricals = "lc") 
  
  # Save results by best AICc
  dir.create(paste("output_SDM_com_lc_2050_2100_F3",sep="/"))
  setwd(paste("output_SDM_com_lc_2050_2100_F3",sep="/"))
  
  # Order results by best AICc
  ENMresults <- mx3@results[order(mx3@results$delta.AICc),]
  
  # Write species results
  write.table(ENMresults, paste(genero,especie,'evaluation_SDMs.csv', sep="_"), sep="\t", row.names=FALSE) 
  
  # Passo para corrigir um bug da modelagem
  nn = ifelse(sum(mx3@results$delta.AICc < 2 &  mx3@results$train.AUC>0.5,na.rm = TRUE)==0,10,2)
  
  # Selecting models with lowers AUC and train higher than 0.5
  ENMv_dAICc <- subset(ENMresults,delta.AICc <=nn)# 
  ENMv_dAICc <- subset(ENMv_dAICc,auc.train > 0.5)# ## Mudei aquiiii
  
  head(ENMv_dAICc)
  ###########
  # Predict raster
  ###########
  
  # Pegando as predições do resultado da modelagem em formato raster
  mx_pred_AIC2= eval.predictions(mx3)[[mx3@results[which (mx3@results$delta.AICc<=nn & mx3@results$auc.train>0.5),]$tune.args]]
  
  # Pegando modelos selecionados para projetar em outros tempos
  mx_model_AIC2= eval.models(mx3)[mx3@results[which (mx3@results$delta.AICc<=nn & mx3@results$auc.train>0.5),]$tune.args]
  
  # Write species results
  write.table(mx3@variable.importance[mx3@results[which (mx3@results$delta.AICc==0 & mx3@results$auc.train>0.5),]$tune.args], 
              paste(genero,especie,'Variable_imp.csv', sep="_"), 
              sep="\t", row.names=FALSE)   
  
  weights_AUC = mx3@results[which (mx3@results$delta.AICc<=nn & mx3@results$auc.train>0.5),]$auc.train
  
  #verifica se tem mais de um modelo e faz a média ponderada dos modelos
  if (length(weights_AUC)>1) {
    print("more than one model selected")
    log_pred = weighted.mean(x=mx_pred_AIC2, w=weights_AUC)
  } else {
    print("just one model selected")
    log_pred = mx_pred_AIC2
  }
  
  # Crop to buffer buffer
  log_pred <- raster::crop(log_pred, extent(envsBgMsk_scale))
  #log_pred <- raster::mask(log_pred, extent(envsBgMsk_scale))
  plot(log_pred, legend=FALSE)
  points(mx3@occs, pch=21, bg=mx3@occs.grp)
  title( bquote(italic(.(species))~'(Current)'))
  
  # Save logistic output
  outputname = paste(paste(genero,"_" ,especie,"_Current", '.tif', sep=""))
  writeRaster(log_pred,outputname, bylayer = TRUE, options = c("COMPRESS=DEFLATE"), format="GTiff", overwrite=TRUE)
  
  # Plot logistic predictions
  plot_log <- raster::plot(log_pred, main=paste(genero, especie,'logistic',"\n", paste(ENMv_dAICc$tune.args,collapse=' '),  sep=" "))
  points(mx3@occs, pch=21, bg=mx3@occs)
  
  # Plot as PDF
  pdf(paste(genero, especie, paste(ENMv_dAICc$settings,collapse='&'), 'logistic.pdf', sep="_"))
  plot_log <- raster::plot(log_pred, main=paste(genero, especie,'logistic',"\n", paste(ENMv_dAICc$tune.args,collapse=' '),  sep=" "))
  points(mx3@occs, pch=21, bg=mx3@occs)
  dev.off()
  

  # Show best models and AICcs
  
  col_int<- c('rm',  'fc', 'AICc', 'delta.AICc', 'w.AIC','auc.train')
  best_models = ENMv_dAICc[,col_int]
  write.table(cbind.data.frame(ENMv_dAICc[,c('fc','rm')],round(best_models[c('AICc', 'delta.AICc', 'w.AIC','auc.train')],2)),paste(genero,especie,'evaluation_selectedSDMs.csv', sep="_"), sep="\t", row.names=FALSE) 
  
  ## Here I will create a binary map (presence absence) using a threshold
  ## In this case I chose  maximum specificy=sensitivity 
  
  sp1 <- as.data.frame(occ_data_clean_ok.pa@coords) # we only kept the coordinates columns
  sp1$Occurrence <- 1 # add Occurrence column
  coordinates(sp1) <- c('x','y')
  proj4string(sp1) = "+proj=longlat +datum=WGS84 +no_defs"
  
  #if (ncell(envsBgMsk)>200000){
  pseudo <- sampleRandom(envsBgMsk[[1]],na.rm=TRUE,1000,xy=TRUE,sp=TRUE)
  #  } else {
  #  pseudo <- sampleRandom(envsBgMsk[[1]],na.rm=TRUE,ncell(envsBgMsk)*0.1,xy=T,sp=T)
  # }
  
  names(pseudo) = c("x","y","var")
  pseudo@data$Occurrence <- 0
  pseudo@data <- pseudo@data[,"Occurrence",drop=F] # we only keep the column Occurrence
  #proj4string(pseudo)
  speciess <- rbind(sp1,pseudo)
  obs <- speciess$Occurrence
  pred <- raster::extract(log_pred,speciess)
  ev <- evaluates(obs,pred)
  ev1 <- ev@threshold_based$threshold
  ev2 <- ev1[2] #  max(specificity+sensitivity). 
  # Getting some statistcs to report
  AUC <- ev@statistics$AUC
  TSS <- ev@threshold_based$TSS[2]
  Kappa <- ev@threshold_based$Kappa[2]
  max_se_sp <- ev2
  
  cbind.data.frame(species_list[i],AUC,TSS,Kappa,max_se_sp)
  
  write.table(cbind.data.frame(species_list[i],AUC,TSS,Kappa,max_se_sp),"evaluation_SDMs",append = TRUE,col.names = TRUE,row.names = FALSE)
  
  # Write a table to keep the AUC and TSS values
  
  # using ifelse to convert predicted probabilities into presence-absence (threshold=ev2)
  pa.pa <- raster(log_pred) # creating an empty raster
  pa.pa[] <- ifelse(log_pred[] >= ev2,1,NA)
  plot(pa.pa)
  #plot(sp1, add=TRUE)
  points(mx3@occs, pch=16)
  maps::map('world',add=TRUE)
  title(paste0("Thresholded_map"))
  
  # Bellow, I will only kepp the predictions that are close to the points (for example, when you want to remove overpredictions when you want to create a species distribution map)
  #Detect continous clamps in the rasters
  clumped <- clump(pa.pa, directions=8)
  sp_buf <- raster::buffer(sp1,width=100000) ## here we use a buffer remove suitable areas that are not continuous with the species records
  inter <- raster::extract(clumped, sp_buf,na.rm=TRUE) 
  inters <- na.exclude(inter[[1]])
  raster1 <- match(clumped,inters)
  #raster binário sem extrapolação
  current_bin <- raster(raster1) # creating an empty raster
  current_bin[] <- ifelse(raster1[] >= 1,1,NA)
  plot(current_bin)
  points(mx3@occs, pch=16)
  maps::map('world',add=TRUE)
  title(paste0("Thresholded_map"))  
  
  # To create species polygons from model predictions
  #Binary occurrences
  #final_species_polygons = rasterToPolygons(pr.pa2,dissolve=TRUE)
  #plot(final_species_polygons, col="red")
  #points(mx3@occ.pts, pch=16)
  #maps::map('world',add=TRUE)
  #title(paste0("Thresholded_map"))
  
  outputname = paste(paste(genero,"_", especie, "_threshold" , '.tif', sep=""))
  writeRaster(current_bin,outputname, bylayer = TRUE, options = c("COMPRESS=DEFLATE"), format="GTiff", overwrite=TRUE)
  
  #writeOGR(final_species_polygons,paste0(genero,"_", especie, "_threshold", ".shp"), driver="ESRI Shapefile",layer=final_species_polygons@data$layer, overwrite_layer = TRUE)
  
  # Finally, if you want to project the map into future/past environmental conditions, you just need to project the map (do not run the model -ENMval again) and repeat the same steps above
  
  #Rasters do Futuros
  
  #Função para predizer o modelo pro futuro- usa o modelo do presente para predizer a distribuição futura
  future_pred = function(layers_fut){
    # You just have to change the current layers to the future one
    preds_LGMCrop <- raster::crop(layers_fut, bgExt)
    # Calculating LGM  
    norm_fun <- function(mx_model_AIC2) {
      p <- predict(mx_model_AIC2, preds_LGMCrop, args='outputformat=cloglog') # Add the preds_LGMMsk in this line as I did
      return(p)
    }
    LGM_norm_list= lapply(mx_model_AIC2,norm_fun)
    LGM_norm=stack(LGM_norm_list)
    LGM_log_pred = weighted.mean(x=LGM_norm, w=weights_AUC)
    LGM_log_pred <- raster::crop(LGM_log_pred, extent(envsBgMsk_scale))
    return(LGM_log_pred)
  }
  
  # RCP26 2050
  IP_2050_RCP26_pred = future_pred(layers_fut = IP_2050_RCP26) 
  UK_2050_RCP26_pred = future_pred(layers_fut = UK_2050_RCP26)
  #média dos dois gcm's
  RCP26_2050_pred = mean(IP_2050_RCP26_pred,UK_2050_RCP26_pred)
  plot(RCP26_2050_pred)
  
  # RCP85 2050
  IP_2050_RCP85_pred = future_pred(layers_fut = IP_2050_RCP85) 
  UK_2050_RCP85_pred = future_pred(layers_fut = UK_2050_RCP85)
  #média dos dois gcm's
  RCP85_2050_pred = mean(IP_2050_RCP85_pred,UK_2050_RCP85_pred)
  plot(RCP85_2050_pred)
  
  # RCP26 2100
  IP_2100_RCP26_pred = future_pred(layers_fut = IP_2100_RCP26) 
  UK_2100_RCP26_pred = future_pred(layers_fut = UK_2100_RCP26)
  #média dos dois gcm's
  RCP26_2100_pred = mean(IP_2100_RCP26_pred,UK_2100_RCP26_pred)
  plot(RCP26_2100_pred)
  
  # RCP85 2100
  IP_2100_RCP85_pred = future_pred(layers_fut = IP_2100_RCP85) 
  UK_2100_RCP85_pred = future_pred(layers_fut = UK_2100_RCP85)
  #média dos dois gcm's
  RCP85_2100_pred = mean(IP_2100_RCP85_pred,UK_2100_RCP85_pred)
  
  # And if you want to have a presence absence map:  
  binary_fut = function(future_pred){
    pr.pa_fut <- raster(future_pred) # creating an empty raster
    pr.pa_fut[] <- ifelse(future_pred[] >= ev2,1,NA)
    
    clumped <- clump(pr.pa_fut, directions=8)
    sp_buf <- raster::buffer(sp1,width=100000) ## here we use a buffer remove suitable areas that are not continuous with the species records
    inter <- raster::extract(clumped, sp_buf,na.rm=TRUE) 
    inters <- na.exclude(inter[[1]])
    raster1 <- match(clumped,inters)
    #raster binário sem extrapolação
    current_bin_fut <- raster(raster1) # creating an empty raster
    current_bin_fut[] <- ifelse(raster1[] >= 1,1,NA)
    #plot(current_bin_fut)
    points(mx3@occs, pch=16)
    maps::map('world',add=TRUE)
    title(paste0("Thresholded_map"))  
    
    return(current_bin_fut) #cortar mapa futuro longe dos pontos atuais
    #return(pr.pa_fut)
  }
  
  #Layers futuro binario
  RCP26_2050_bin = binary_fut(future_pred = RCP26_2050_pred)
  RCP85_2050_bin = binary_fut(future_pred = RCP85_2050_pred)
  RCP26_2100_bin = binary_fut(future_pred = RCP26_2100_pred)
  RCP85_2100_bin = binary_fut(future_pred = RCP85_2100_pred)
  #plot(RCP85_2050_bin,col="blue", main="2050")
  #plot(RCP85_2100_bin,col="red",main="2100", add=TRUE)
  #plot(RCP85_2050_bin,col="yellow",main="2100", add=TRUE)
  
  
  # Calcular area 1
  current_area = sum(na.exclude(getValues(raster::area(current_bin,na.rm=TRUE))))
  RCP26_2050_area = sum(na.exclude(getValues(raster::area(RCP26_2050_bin,na.rm=TRUE))))
  RCP85_2050_area = sum(na.exclude(getValues(raster::area(RCP85_2050_bin,na.rm=TRUE))))
  RCP26_2100_area = sum(na.exclude(getValues(raster::area(RCP26_2100_bin,na.rm=TRUE))))
  RCP85_2100_area = sum(na.exclude(getValues(raster::area(RCP85_2100_bin,na.rm=TRUE))))
  
  write.table(cbind.data.frame(species_list[i],current_area, RCP26_2050_area,RCP85_2050_area,RCP26_2100_area,RCP85_2100_area),"areas_distr.txt",append = TRUE,col.names = TRUE,row.names = FALSE)
  
  
  #plot(sp1, add=TRUE)
  points(mx3@occs, pch=16)
  maps::map('world',add=TRUE)
  title(paste0("Thresholded_map_Future"))  
  
  ###############################################
  # Plotizinhos menos zoadinhos #################
  ###############################################
  
  #plot(log_pred)
  #plot(sp1, add=TRUE)
  #points(mx3@occs, pch=16)
  #maps::map('world',add=TRUE)
  
  # Testing extent for map
  #extent_plot = drawExtent()
  extent_plot = extent(log_pred)
  
  # Writing a function to create plots for each time slice
  # Some test data for the funtion
  #raster_data=LIG_log_pred
  #period='(LIG)'
  # Function
  nice_plot_fun = function(raster_data,period){
    # Raster to points for ggplot
    TAB_RDA <- as.data.frame(rasterToPoints(raster_data))
    # Junto os valores dos 4 rasters
    colnames(TAB_RDA)[3] <- "value"
    # Dou um nome para os títulos das figuras    
    TAB_RDA$variable <- factor(rep("Current", nrow(TAB_RDA)), 
                               levels = c("Current"))
    head(TAB_RDA)
    tail(TAB_RDA)        
    # Reverse values to fit the legend
    TAB_RDA$value = 1-TAB_RDA$value
    # Plot 
    my_ggplot = ggplot() +
      geom_polygon(data=Neotropico, aes(long, lat, group=group), fill="#FCFFA4FF",col=NA,size = .2)+
      # geom_tile vai plotar a tabela como um raster
      geom_tile(data = TAB_RDA, aes(x = x, y = y, fill = cut(value, breaks=seq(0, 1, length.out=10), include.lowest = T)),linejoin="round") + 
      #scale_fill_viridis_d vai definir as cores do raster. O argumento option define o gradiente de cores e vai de A até H. Testa outras cores se preferir. Direction pode ser 1 ou -1. Testa a diferença para entender. Alpha é a transparência. 
      scale_fill_viridis_d(alpha = 1, direction = 1, option = "B", labels = c(">0.9","","","","0.5","","","","<0.1")) +
      #scale_fill_brewer(palette = "RdYlBu", labels = c(">0.9","","","","0.5","","","","<0.1")) +
      # Adiciona o shape da América do Sul
      geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
      #geom_polygon(data=brasila_simpl, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
      # Adiciona os pontos da espécie. Mexe no size para aumentar ou diminuir o tamanho dos pontos
      geom_point(data=occs.xy.z,aes(x=long ,y=lat), col="black", fill="white", size=2, shape=21)+
      # Aqui utilizamos o extent que vc definiiu acima
      coord_cartesian(xlim=extent_plot[1:2],ylim = extent_plot[3:4])+
      ggsn::scalebar(x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4],dist = 500,dist_unit = "km", model = 'WGS84',st.size = 2.5,location="bottomright",transform=TRUE, border.size=.5)+
      ggsn::north(anchor = c("x"= -39,"y"=-43),x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4])+
      # Título dos eixos
      xlab(NULL) + ylab(NULL) +
      #ggtitle(expression(paste(italic("Chironius gouveai")," ","(LIG)")))+
      ggtitle(bquote(italic(.(genero)~.(especie))~.(period)))+
      guides(fill=guide_legend(title="Suitability")) +
      #facet_wrap(~ variable,nrow = 2) +
      theme_bw(base_size = 14, base_family = "Times") +
      theme(panel.grid = element_blank(), plot.background = element_blank(), panel.background = element_blank(), strip.text = element_text(size=14))
    return(my_ggplot)
  }
  nice_plot_bin_fun = function(raster_data,period){
    # Raster to points for ggplot
    TAB_RDA <- as.data.frame(rasterToPoints(raster_data))
    # Junto os valores dos 4 rasters
    colnames(TAB_RDA)[3] <- "value"
    # Dou um nome para os títulos das figuras    
    TAB_RDA$variable <- factor(rep("Current", nrow(TAB_RDA)), 
                               levels = c("Current"))
    head(TAB_RDA)
    tail(TAB_RDA)        
    # Reverse values to fit the legend
    TAB_RDA$value = 1-TAB_RDA$value
    # Plot 
    my_ggplot = ggplot() +
      geom_polygon(data=Neotropico, aes(long, lat, group=group), fill="grey90",col=NA,size = .2)+
      # geom_tile vai plotar a tabela como um raster
      geom_tile(data = TAB_RDA, aes(x = x, y = y, fill = cut(value, breaks=seq(0, 1, length.out=10), include.lowest = T)),linejoin="round") + 
      #scale_fill_viridis_d vai definir as cores do raster. O argumento option define o gradiente de cores e vai de A até H. Testa outras cores se preferir. Direction pode ser 1 ou -1. Testa a diferença para entender. Alpha é a transparência. 
      scale_fill_viridis_d(alpha = 1, direction = 1, option = "D", labels = c("")) +
      #scale_fill_brewer(palette = "RdYlBu", labels = c(">0.9","","","","0.5","","","","<0.1")) +
      # Adiciona o shape da América do Sul
      geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
      #geom_polygon(data=brasila_simpl, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
      # Adiciona os pontos da espécie. Mexe no size para aumentar ou diminuir o tamanho dos pontos
      geom_point(data=occs.xy.z,aes(x=long ,y=lat), col="black", fill="orange", size=2, shape=21)+
      # Aqui utilizamos o extent que vc definiiu acima
      coord_cartesian(xlim=extent_plot[1:2],ylim = extent_plot[3:4])+
      ggsn::scalebar(x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4],dist = 500,dist_unit = "km", model = 'WGS84',st.size = 2.5,location="bottomright",transform=TRUE, border.size=.5)+
      ggsn::north(anchor = c("x"= -39,"y"=-43),x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4])+
      # Título dos eixos
      xlab(NULL) + ylab(NULL) +
      #ggtitle(expression(paste(italic("Chironius gouveai")," ","(LIG)")))+
      ggtitle(bquote(italic(.(genero)~.(especie))~.(period)))+
      guides(fill=guide_legend(title="Distribution")) +
      #facet_wrap(~ variable,nrow = 2) +
      theme_bw(base_size = 14, base_family = "Times") +
      theme(panel.grid = element_blank(), plot.background = element_blank(), panel.background = element_blank(), strip.text = element_text(size=14))
    return(my_ggplot)
  }
  # Current plot
  current_plot = nice_plot_fun(raster_data=log_pred,period='(Current)')
  pdf(paste(paste(genero,"_", especie, "_current" , '.pdf', sep="")))
  print(current_plot)
  dev.off()
  
  #Future
  #RCP26_2050
  RCP26_2050_plot = nice_plot_fun(raster_data=RCP26_2050_pred,period='(Rcp2.6 2050)')
  pdf(paste(paste(genero,"_", especie, "_RCP2.6_2050" , '.pdf', sep="")))
  print(RCP26_2050_plot)
  dev.off()
  
  #RCP85_2050
  RCP85_2050_plot = nice_plot_fun(raster_data=RCP85_2050_pred,period='(Rcp8.5 2050)')
  pdf(paste(paste(genero,"_", especie, "_RCP8.5_2050" , '.pdf', sep="")))
  print(RCP85_2050_plot)
  dev.off()
  
  #RCP26_2100
  RCP26_2100_plot = nice_plot_fun(raster_data=RCP26_2100_pred,period='(Rcp2.6 2100)')
  pdf(paste(paste(genero,"_", especie, "_RCP2.6_2100" , '.pdf', sep="")))
  print(RCP26_2100_plot)
  dev.off()
  
  #RCP85_2100
  RCP85_2100_plot = nice_plot_fun(raster_data=RCP85_2100_pred,period='(Rcp8.5 2100)')
  pdf(paste(paste(genero,"_", especie, "_RCP8.5_2100" , '.pdf', sep="")))
  print(RCP85_2100_plot)
  dev.off()
  
  # Extract suitability values
  pdf(paste(paste(genero,"_", especie, "_boxplot", '.pdf', sep="")))
  boxplot(raster::extract(log_pred,occ_data_clean_ok.pa),
          raster::extract(RCP26_2050_pred,occ_data_clean_ok.pa),
          raster::extract(RCP85_2050_pred,occ_data_clean_ok.pa),
          raster::extract(RCP26_2100_pred,occ_data_clean_ok.pa),
          raster::extract(RCP85_2100_pred,occ_data_clean_ok.pa),
          names = c("Current","Rcp26 2050","Rcp85 2050","Rcp26 2100","Rcp85 2100"))
  dev.off()


 write.csv(cbind.data.frame(raster::extract(log_pred,occ_data_clean_ok.pa),
  raster::extract(RCP26_2050_pred,occ_data_clean_ok.pa),
  raster::extract(RCP85_2050_pred,occ_data_clean_ok.pa),
  raster::extract(RCP26_2100_pred,occ_data_clean_ok.pa),
  raster::extract(RCP85_2100_pred,occ_data_clean_ok.pa)),
  paste(paste(genero,"_", especie, "_boxplot", '.csv', sep="")))
 
#pontos somente na regiao amazonica
 pts_amazonia = raster::intersect(occ_data_clean_ok.pa,amazonia_shp)
 
 # Extract suitability values for amazon basin
 pdf(paste(paste(genero,"_", especie, "_boxplot_amazon", '.pdf', sep="")))
 boxplot(raster::extract(log_pred,occ_data_clean_ok.pa),
         raster::extract(RCP26_2050_pred,pts_amazonia),
         raster::extract(RCP85_2050_pred,pts_amazonia),
         raster::extract(RCP26_2100_pred,pts_amazonia),
         raster::extract(RCP85_2100_pred,pts_amazonia),
         names = c("Current","Rcp26 2050","Rcp85 2050","Rcp26 2100","Rcp85 2100"))
 dev.off()
 
 write.csv(cbind.data.frame(raster::extract(log_pred,pts_amazonia),
                            raster::extract(RCP26_2050_pred,pts_amazonia),
                            raster::extract(RCP85_2050_pred,pts_amazonia),
                            raster::extract(RCP26_2100_pred,pts_amazonia),
                            raster::extract(RCP85_2100_pred,pts_amazonia)),
           paste(paste(genero,"_", especie, "_boxplot_amazon_basin", '.csv', sep="")))
 
 write.csv(cbind.data.frame(median(raster::extract(RCP26_2050_pred,pts_amazonia)),
                            median(raster::extract(RCP85_2050_pred,pts_amazonia)),
                            median(raster::extract(RCP26_2100_pred,pts_amazonia)),
                            median(raster::extract(RCP85_2100_pred,pts_amazonia))),
          paste(paste(genero,"_", especie, "median_boxplot_amazon_basin", '.csv', sep="")))
                            
 
  ## Plots binários
  #Current
  Current_plot_bin = nice_plot_bin_fun(raster_data=current_bin,period='(Current)')
  pdf(paste(paste(genero,"_", especie, "_Current_Binary " , '.pdf', sep="")))
  print(Current_plot_bin)
  dev.off()
  
  #RCP26_2050
  RCP26_2050_plot_bin = nice_plot_bin_fun(raster_data=RCP26_2050_bin,period='(Rcp2.6 2050)')
  pdf(paste(paste(genero,"_", especie, "_RCP2.6_2050_Binary " , '.pdf', sep="")))
  print(RCP26_2050_plot_bin)
  dev.off()
  
  #RCP85_2050
  RCP85_2050_plot_bin = nice_plot_bin_fun(raster_data=RCP85_2050_bin,period='(Rcp8.5 2050)')
  pdf(paste(paste(genero,"_", especie, "_RCP8.5_2050_Binary" , '.pdf', sep="")))
  print(RCP85_2050_plot_bin)
  dev.off()
  
  #RCP26_2100
  RCP26_2100_plot_bin = nice_plot_bin_fun(raster_data=RCP26_2100_bin,period='(Rcp2.6 2100)')
  pdf(paste(paste(genero,"_", especie, "_RCP2.6_2100_Binary" , '.pdf', sep="")))
  print(RCP26_2100_plot_bin)
  dev.off()
  
  #RCP85_2100
  RCP85_2100_plot_bin = nice_plot_bin_fun(raster_data=RCP85_2100_bin,period='(Rcp8.5 2100)')
  pdf(paste(paste(genero,"_", especie, "_RCP8.5_2100_Binary" , '.pdf', sep="")))
  print(RCP85_2100_plot_bin)
  dev.off()

  ### Mapas no ggplot
  raster_data_1 = current_bin
  raster_data_2 = RCP85_2050_bin
  raster_data_3 = RCP85_2100_bin
  raster_data_4 = raster::mask(raster_data_3,raster_data_1, maskvalue=1)
  period=''
  
  # Raster to points for ggplot
  TAB_RDA1 <- as.data.frame(rasterToPoints(raster_data_1))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA1)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA1$variable <- factor(rep("Current", nrow(TAB_RDA1)), 
                              levels = c("Current"))
  head(TAB_RDA1)
  tail(TAB_RDA1)        
  # Reverse values to fit the legend
  TAB_RDA1$value = 1-TAB_RDA1$value
  
  ##
  # Raster to points for ggplot
  TAB_RDA2 <- as.data.frame(rasterToPoints(raster_data_2))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA2)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA2$variable <- factor(rep("Current", nrow(TAB_RDA2)), 
                              levels = c("Current"))
  head(TAB_RDA2)
  tail(TAB_RDA2)        
  # Reverse values to fit the legend
  TAB_RDA2$value = 1-TAB_RDA2$value
  
  TAB_RDA3 <- as.data.frame(rasterToPoints(raster_data_3))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA3)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA3$variable <- factor(rep("Current", nrow(TAB_RDA3)), 
                              levels = c("Current"))
  head(TAB_RDA3)
  tail(TAB_RDA3)        
  # Reverse values to fit the legend
  TAB_RDA3$value = 1-TAB_RDA3$value
  
  TAB_RDA4 <- as.data.frame(rasterToPoints(raster_data_4))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA4)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA4$variable <- factor(rep("Current", nrow(TAB_RDA4)), 
                              levels = c("Current"))
  head(TAB_RDA4)
  tail(TAB_RDA4)        
  # Reverse values to fit the legend
  TAB_RDA4$value = 1-TAB_RDA4$value
  
  # Plot 
  my_ggplot_2 = ggplot() +
    geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col=NA,size = .2)+
    # geom_tile vai plotar a tabela como um raster
    #geom_tile(data = TAB_RDA1, aes(x = x, y = y, fill = cut(value, breaks=seq(0, 1, length.out=10), include.lowest = T)),linejoin="round") + 
    #scale_fill_viridis_d vai definir as cores do raster. O argumento option define o gradiente de cores e vai de A até H. Testa outras cores se preferir. Direction pode ser 1 ou -1. Testa a diferença para entender. Alpha é a transparência. 
    # scale_fill_viridis_d(alpha = 1, direction = 1, option = "B", labels = c(">0.9","","","","0.5","","","","<0.1")) +
    #scale_fill_brewer(palette = "RdYlBu", labels = c(">0.9","","","","0.5","","","","<0.1")) +
    annotate(geom="raster", x=TAB_RDA1$x, y=TAB_RDA1$y, fill = "#d73027")+
    annotate(geom="raster", x=TAB_RDA2$x, y=TAB_RDA2$y, fill = "#fee090")+
    annotate(geom="raster", x=TAB_RDA3$x, y=TAB_RDA3$y, fill = "#4575b4")+
    annotate(geom="raster", x=TAB_RDA4$x, y=TAB_RDA4$y, fill = "#984ea3")+
    # Adiciona o shape da América do Sul
    geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
    #geom_polygon(data=brasila_simpl, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
    # Adiciona os pontos da espécie. Mexe no size para aumentar ou diminuir o tamanho dos pontos
    
    #geom_point(data=occs.xy.z,aes(x=long ,y=lat), col="black", fill="white", size=2, shape=21)+
    # Aqui utilizamos o extent que vc definiiu acima
    coord_cartesian(xlim=extent_plot[1:2],ylim = extent_plot[3:4])+
    ggsn::scalebar(x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4],dist = 500,dist_unit = "km", model = 'WGS84',st.size = 2.5,location="bottomright",transform=TRUE, border.size=.5)+
    ggsn::north(anchor = c("x"= -39,"y"=-43),x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4])+
    # Título dos eixos
    xlab(NULL) + ylab(NULL) +
    #ggtitle(expression(paste(italic("Chironius gouveai")," ","(LIG)")))+
    ggtitle(bquote(italic(.(genero)~.(especie))~.(period)))+
    guides(fill=guide_legend(title="Suitability")) +
    #facet_wrap(~ variable,nrow = 2) +
    theme_bw(base_size = 14, base_family = "Times") +
    theme(panel.grid = element_blank(), plot.background = element_blank(), panel.background = element_blank(), strip.text = element_text(size=14))
  pdf(paste(paste(genero,"_", especie, "Range_Shift_RCP8.5" , '.pdf', sep="")))
  print(my_ggplot_2)
  dev.off()
  
  #MAPA RCP2.6
  ### Mapas no ggplot
  raster_data_1.1 = current_bin
  raster_data_2.1 = RCP26_2050_bin
  raster_data_3.1 = RCP26_2100_bin
  raster_data_4.1 = raster::mask(raster_data_3.1,raster_data_1.1, maskvalue=1)
  period=''
  
  # Raster to points for ggplot
  TAB_RDA1.1 <- as.data.frame(rasterToPoints(raster_data_1.1))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA1.1)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA1.1$variable <- factor(rep("Current", nrow(TAB_RDA1.1)), 
                              levels = c("Current"))
  head(TAB_RDA1.1)
  tail(TAB_RDA1.1)        
  # Reverse values to fit the legend
  TAB_RDA1.1$value = 1-TAB_RDA1.1$value
  
  ##
  # Raster to points for ggplot
  TAB_RDA2.1 <- as.data.frame(rasterToPoints(raster_data_2.1))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA2.1)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA2.1$variable <- factor(rep("Current", nrow(TAB_RDA2.1)), 
                              levels = c("Current"))
  head(TAB_RDA2.1)
  tail(TAB_RDA2.1)        
  # Reverse values to fit the legend
  TAB_RDA2.1$value = 1-TAB_RDA2.1$value
  
  TAB_RDA3.1 <- as.data.frame(rasterToPoints(raster_data_3.1))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA3.1)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA3.1$variable <- factor(rep("Current", nrow(TAB_RDA3.1)), 
                              levels = c("Current"))
  head(TAB_RDA3.1)
  tail(TAB_RDA3.1)        
  # Reverse values to fit the legend
  TAB_RDA3.1$value = 1-TAB_RDA3.1$value
  
  TAB_RDA4.1 <- as.data.frame(rasterToPoints(raster_data_4.1))
  # Junto os valores dos 4 rasters
  colnames(TAB_RDA4.1)[3] <- "value"
  # Dou um nome para os títulos das figuras    
  TAB_RDA4.1$variable <- factor(rep("Current", nrow(TAB_RDA4.1)), 
                              levels = c("Current"))
  head(TAB_RDA4.1)
  tail(TAB_RDA4.1)        
  # Reverse values to fit the legend
  TAB_RDA4.1$value = 1-TAB_RDA4.1$value
  
  # Plot 
  my_ggplot_2.1 = ggplot() +
    geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col=NA,size = .2)+
    # geom_tile vai plotar a tabela como um raster
    #geom_tile(data = TAB_RDA1, aes(x = x, y = y, fill = cut(value, breaks=seq(0, 1, length.out=10), include.lowest = T)),linejoin="round") + 
    #scale_fill_viridis_d vai definir as cores do raster. O argumento option define o gradiente de cores e vai de A até H. Testa outras cores se preferir. Direction pode ser 1 ou -1. Testa a diferença para entender. Alpha é a transparência. 
    # scale_fill_viridis_d(alpha = 1, direction = 1, option = "B", labels = c(">0.9","","","","0.5","","","","<0.1")) +
    #scale_fill_brewer(palette = "RdYlBu", labels = c(">0.9","","","","0.5","","","","<0.1")) +
    annotate(geom="raster", x=TAB_RDA1.1$x, y=TAB_RDA1.1$y, fill = "#d73027")+
    annotate(geom="raster", x=TAB_RDA2.1$x, y=TAB_RDA2.1$y, fill = "#fee090")+
    annotate(geom="raster", x=TAB_RDA3.1$x, y=TAB_RDA3.1$y, fill = "#4575b4")+
    annotate(geom="raster", x=TAB_RDA4.1$x, y=TAB_RDA4.1$y, fill = "#984ea3")+
    # Adiciona o shape da América do Sul
    geom_polygon(data=Neotropico, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
    #geom_polygon(data=brasila_simpl, aes(long, lat, group=group), fill=NA,col="grey20",size = .2)+
    # Adiciona os pontos da espécie. Mexe no size para aumentar ou diminuir o tamanho dos pontos
    
    #geom_point(data=occs.xy.z,aes(x=long ,y=lat), col="black", fill="white", size=2, shape=21)+
    # Aqui utilizamos o extent que vc definiiu acima
    coord_cartesian(xlim=extent_plot[1:2],ylim = extent_plot[3:4])+
    ggsn::scalebar(x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4],dist = 500,dist_unit = "km", model = 'WGS84',st.size = 2.5,location="bottomright",transform=TRUE, border.size=.5)+
    ggsn::north(anchor = c("x"= -39,"y"=-43),x.min=extent_plot[1], x.max=extent_plot[2], y.min=extent_plot[3], y.max=extent_plot[4])+
    # Título dos eixos
    xlab(NULL) + ylab(NULL) +
    #ggtitle(expression(paste(italic("Chironius gouveai")," ","(LIG)")))+
    ggtitle(bquote(italic(.(genero)~.(especie))~.(period)))+
    guides(fill=guide_legend(title="Suitability")) +
    #facet_wrap(~ variable,nrow = 2) +
    theme_bw(base_size = 14, base_family = "Times") +
    theme(panel.grid = element_blank(), plot.background = element_blank(), panel.background = element_blank(), strip.text = element_text(size=14))
  pdf(paste(paste(genero,"_", especie, "Range_Shift_RCP2.6" , '.pdf', sep="")))
  print(my_ggplot_2.1)
  dev.off()
  
  # Move back to the working directory
  setwd("~/Documents/Biologia/Doutorado INPA/Tese/Manuscritos/Manuscrito cap 3/Análise_Josué")
  print(paste0("Modelling complete for ","sp", i,": ", species_list[i]))
}

# If you want to test with another species, just change the "i" in the beggining of the modelling to another number
