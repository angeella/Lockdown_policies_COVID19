###################################################################################
##############################SOME PLOTS###################################
###################################################################################

rm(list=ls())
#Set your path
path <- "~/GitHub/Lockdown_policies_COVID19/"

#load packages
source(paste0(path,"Code/Angela/packages.R"))
#load compute R0 function
source(paste0(path, "Code/Angela/compute_R0_ML.R"))
#load utils function
source(paste0(path,"Code/Angela/utils.R"))
#load variables names
load(paste0(path,"Code/Angela/Data/var.RData"))
#load clusters
load(paste0(path,"Code/Angela/Data/cluster20paesi.Rdata"))
#load fitted model
load(paste0(path,"Code/Angela/Data/out.RData"))

###Map of the effects of the clusters to the number of predicted actives after 14 days

#load map data
clust$id <- countrycode(clust$id, 'iso3c', 'country.name')
clust$id[20] <- "USA"
clust$id[10] <- "UK"

#Compute effects
pr<-ggeffect(mod1, "Clusters")

clust$predicted <- ifelse(clust$cluster == "1", pr$predicted[1]/pr$predicted[4],
                          ifelse(clust$cluster == "2", pr$predicted[2]/pr$predicted[4],
                                 ifelse(clust$cluster == "3", pr$predicted[3]/pr$predicted[4],
                                        ifelse(clust$cluster == "4", pr$predicted[4]/pr$predicted[4], pr$predicted[5]/pr$predicted[4]))))
clust$predicted <- round(clust$predicted,2)
clust$predicted <- as.factor(clust$predicted)

#Adjust map
world <- map_data("world")
world$id <- world$region
mapbig <- left_join(world, clust, by="id")

worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "lightcyan1",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


#Europe
europe <- worldmap + coord_fixed(xlim = c(-15, 35), ylim = c(35, 70), ratio = 1.5)

europe2 <- europe + geom_polygon(data = mapbig,
                                 aes(fill = predicted,
                                     x = long,
                                     y = lat,
                                     group = group),
                                 color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma", begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

europe2

#America

na <- worldmap + coord_fixed(xlim = c(-170, -50), ylim = c(-12, 100), ratio = 1.5)
na2=na + geom_polygon(data = mapbig,
                      aes(fill = predicted,
                          x = long,
                          y = lat,
                          group = group),
                      color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma",begin=0.2, end=0.9,
                       direction = -1,
                       name = "",
                       na.value = "grey80",
                       na.translate=FALSE
                       #guide = guide_colorbar(
                       #barheight = unit(140, units = "mm"),
                       #barwidth = unit(6, units = "mm"))
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 6, face = "bold"),
        legend.title=element_text(size=6), 
        legend.text=element_text(size=6)) + guides(fill=guide_legend(nrow=5, byrow=TRUE,ncol = 2))

na2
#Asia

kor <- worldmap + coord_fixed(xlim = c(122, 132), ylim = c(32, 39), ratio = 1.5)
kor2=kor+ geom_polygon(data = mapbig,
                       aes(fill = predicted,
                           x = long,
                           y = lat,
                           group = group),
                       color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma",begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

sin <- worldmap + coord_fixed(xlim = c(102, 105), ylim = c(0.5, 2.5), ratio = 1.5)
sin2=sin+ geom_polygon(data = mapbig,
                       aes(fill = predicted,
                           x = long,
                           y = lat,
                           group = group),
                       color = "grey70") +
  theme(text = element_text(size = 30),
        plot.title = element_text(face = "bold")) +
  scale_fill_viridis_d(option = "magma",begin=0.2, end=0.9,
                     direction = -1,
                     name = "",
                     na.value = "grey80",
                     guide = guide_colorbar(
                       barheight = unit(140, units = "mm"),
                       barwidth = unit(6, units = "mm")))+ theme(legend.position = "none") 

#save
save(na2,europe2,kor2,sin2, file = paste0(path, "Code/Angela/Data/plot_mapAngela.RData"))


## Plot effects interaction Clusters and lockdown policies

#For the gatherings restrictions:
eff <- ggeffect(mod1, terms = c("Clusters", "gatherings_restrictionsF"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Gatherings Restrictions"
levels(eff$group) <-  c("No measures", "> 1000", "100-1000 people", "10-100 people", "< 10 people")

pGath <- plot(eff) + ylab("")+ 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

#For the testing policy
eff <- ggeffect(mod1, terms = c("Clusters", "testing_policyF"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Testing"
levels(eff$group) <-  c("No measures", "Specific criteria", "Symptoms", "Open")

pTest <- plot(eff)+ ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

#For the contact tracing
eff <- ggeffect(mod1, terms = c("Clusters", "contact_tracingF"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Tracing"
levels(eff$group) <-  c("No measures", "Limited", "Comprehensive")
a <- plot(eff)
pCont <- plot(eff) + ylab("")+ 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

#For the transport closing:
eff <- ggeffect(mod1, terms = c("Clusters", "transport_closingF [0, 2]"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Transport Closing"
levels(eff$group) <-  c("No measures", "Require closing")

pTr <- plot(eff) + ylab("")+ 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

#For the workplace closing
eff <- ggeffect(mod1, terms = c("Clusters", "workplace_closingF"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Workplace Closing"
levels(eff$group) <-  c("No measures", "Recommend closing", "Require closing for some sectors", "Require closing all-but-essential workplaces")

pW <- plot(eff)+ ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

#For the stay_home_restrictions
eff <- ggeffect(mod1, terms = c("Clusters", "stay_home_restrictionsF [0, 3]"))
attr(eff, "legend.title") <- ""
attr(eff,"title") <- "Stay Home"
levels(eff$group) <- c("No measures", "Recommend not leaving house", "Only  daily exercise and essential trips", "Minimal exceptions")

pSH <- plot(eff)+ ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")

save(pGath,pTest,pTr, 
     pW, pSH,pCont, 
     file = paste0(path, "Code/Angela/Data/plot_mod.RData"))

## Plot random effects id

#Define the formula for the model
f <- as.formula(paste("active_lag", "~", 
                      paste(c(var_EC[c(3)]), collapse=" + "), 
                      "+", paste(c(var_FIX[c(3)]), collapse=" + "),
                      "+", paste(c(var_HS[1]), collapse=" + "),
                      "+", paste(c(var_LD[c(2,4:6,8:9)]),collapse=" + "),
                      "+ offset(log(active + 1)) + (pca_LD|id) + (1|date2)"))

#Run the negative binomial mixed model with offeset defined as the logarithmic of the number of active at time t
mod1 <- glmmTMB(f, dat, family="nbinom2")

#No lockdown policies
predictRE <- ggeffects::ggpredict(mod1, terms = c("id"), condition = "active [100 1000]",
                       type = "re")

pRE <- plot(predictRE) + ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")


predRE <- data.frame(predicted = pRE$data$predicted, 
                     id = unlist(lapply(sort(states_to_sel), function(x) rep(x,2))),
                     active = rep(c(100,1000), length(states_to_sel)))

pop <- unique(dat %>% dplyr::select(id, pop))
pop$id <- as.factor(pop$id)

predRE <- full_join(predRE,pop, by = "id")
predRE$active <- as.factor(predRE$active)

predRE$Clusters <- ifelse(predRE$id %in% Cl1, "Cl1", 
                       ifelse(predRE$id %in% Cl2, "Cl2", 
                              ifelse(predRE$id %in% Cl3, "Cl3", 
                                     ifelse(predRE$id %in% Cl4, "Cl4","Cl5"))))

predRE$Clusters <- as.factor(predRE$Clusters)
predRE$id <- as.factor(predRE$id)
predRE$id = factor(predRE$id, levels=states_to_sel)

predRE <- predRE %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(id = fct_relevel(id,states_to_sel))

predRE$ref <- rep(predRE$predicted[predRE$id == "ITA"],length(states_to_sel))

pNL <-  ggplot(predRE) + 
  geom_point(aes(x = id, y = predicted, shape =active, color = Clusters)) + 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  ylab("Predicted Number of Actives after 14 days") + theme_minimal() +
  geom_hline(yintercept = 1)

#Combination si testing and tracing max no lockdown

predictRE <- ggpredict(mod1, terms = c("id", "active [3000]"), condition = c( 
                                                            "testing_policyF [3]",
                                                            "contact_tracingF [2]"),
                       type = "re")

pRE <- plot(predictRE) + ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")


predRE <- data.frame(predicted = pRE$data$predicted, 
                     id = unlist(lapply(sort(states_to_sel), function(x) rep(x,2))),
                     active = rep(c(100,1000), length(states_to_sel)))

predRE$active <- as.factor(predRE$active)

predRE$Clusters <- ifelse(predRE$id %in% Cl1, "Cl1", 
                          ifelse(predRE$id %in% Cl2, "Cl2", 
                                 ifelse(predRE$id %in% Cl3, "Cl3", 
                                        ifelse(predRE$id %in% Cl4, "Cl4","Cl5"))))

predRE$Clusters <- as.factor(predRE$Clusters)
predRE$id <- as.factor(predRE$id)
predRE$id = factor(predRE$id, levels=states_to_sel)

pTT <- predRE %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(id = fct_relevel(id,states_to_sel)) %>% ggplot() + 
  geom_point(aes(x = id, y = predicted, shape =active, color = Clusters)) + 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  ylab("Predicted Number of Actives after 14 days") + theme_minimal()

#Testing Tracing Stay Home and Gathering

predictRE <- ggpredict(mod1, terms = c("id", 
                                       "active [100, 1000]",
                                       "workplace_closingF [3]",
                                       "gatherings_restrictionsF [4]"), type = "re")


pRE <- plot(predictRE) + ylab("")+
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  theme_minimal(base_size = 12)  + theme(plot.title =element_text(),
                                         axis.title.x = element_blank(),
                                         legend.position = "None")


predRE <- data.frame(predicted = pRE$data$predicted, 
                     id = unlist(lapply(sort(states_to_sel), function(x) rep(x,2))),
                     active = rep(c(100,1000), length(states_to_sel)))

predRE$active <- as.factor(predRE$active)

predRE$Clusters <- ifelse(predRE$id %in% Cl1, "Cl1", 
                          ifelse(predRE$id %in% Cl2, "Cl2", 
                                 ifelse(predRE$id %in% Cl3, "Cl3", 
                                        ifelse(predRE$id %in% Cl4, "Cl4","Cl5"))))

predRE$Clusters <- as.factor(predRE$Clusters)
predRE$id <- as.factor(predRE$id)
predRE$id = factor(predRE$id, levels=states_to_sel)

pTT <- predRE %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(id = fct_relevel(id,states_to_sel)) %>% ggplot() + 
  geom_point(aes(x = id, y = predicted, shape =active, color = Clusters)) + 
  scale_color_viridis(discrete=TRUE,  option="A", direction=-1, end=0.9) +
  ylab("Predicted Number of Actives after 14 days") + theme_minimal()
