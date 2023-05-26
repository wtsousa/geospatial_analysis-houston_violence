#Carregando pacotes sf e tidyverse
library(sf)
library(tidyverse)

#Lendo o shapefile de Houston
Houston <- read_sf("Houston_City_Limit.shp")
#Sistema de Coordenadas de Houston é WGS 84 (CRS 4326)
st_crs(Houston)

#Plotando o shapefile com o ggplot no sistema de 
#referencia de coordenadas do objeto
ggplot(Houston) +
  geom_sf(fill = "White") +
  coord_sf(datum = st_crs(4326))


#Carregando dados criminalísticos de Houston
crimeHouston <- read_csv("Base Houston.csv")
summary(crimeHouston)


# Plotando as localizacoes dos delitos
ggplot(data = Houston) +
  geom_sf(fill = "White") +
  geom_point(data = crimeHouston,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Red',
             size = 0.75) +
  theme_light()


# Carregando o pacote rmapshaper
library(rmapshaper)
# Simplificando o shape de Houston
HoustonShp = ms_simplify(input = Houston,
                    keep = 0.02,
                    keep_shapes = TRUE)

# Plotando as localizacoes dos delitos com shape simplificado
ggplot(data = HoustonShp) +
  geom_sf(fill = "White") +
  geom_point(data = crimeHouston,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Red',
             size = 0.75) +
  theme_light()


# Plotando as localizacoes pelo tipo do delitos em mapas separados
ggplot(data = HoustonShp) +
  geom_sf(fill = "White") +
  geom_point(data = crimeHouston,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  facet_wrap(~offense,
             labeller = labeller(offense = c("aggravated assault"="LESÃO CORPORAL",
                                          "rape" = "ESTUPRO",
                                          "robbery" = "ROUBO",
                                          "burglary" = "ARROMBAMENTO",
                                          "murder" = "ASSASSINATO",
                                          "auto theft" = "FURTO DE VEÍCULO")))


# Refazendo o mapa anterior com qmplot (sem shapefile)
library(ggmap)
qmplot(x = lon, 
       y = lat, 
       data = crimeHouston,
       colour = I('red'), 
       size = I(0.3), 
       darken = 0.1) +
  facet_wrap(~offense,
             labeller = labeller(offense = c("aggravated assault"="LESÃO CORPORAL",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "burglary" = "ARROMBAMENTO",
                                             "murder" = "ASSASSINATO",
                                             "auto theft" = "FURTO DE VEÍCULO")))

#-------------------------------------------------------------------------------------#
# -------------- Estimando a intensidade de um padrao de pontos  ---------------------#
#-------------------------------------------------------------------------------------#

#Carregando pacotes
library(spatstat)
library(maptools)
#Definindo o shapefile como uma janela onde os 
#pontos serao plotados owin - observation window
HoustonShp2 <- readShapePoly("Houston_City_Limit.shp")
HoustonOWin <- as.owin(HoustonShp2)

#Criando o Padrao de Pontos no Plano 
#(Planar Point Pattern)
Houstonppp = ppp( x = crimeHouston$lon, 
             y = crimeHouston$lat, 
             window = HoustonOWin)


#-------------------------------------------------------------------------------------#
# ------------------------------ Efeito de 1a ordem  ---------------------------------#
#-------------------------------------------------------------------------------------#

raio.est = bw.diggle(Houstonppp)
raio.est
#Sigma otimizado = 0.003

#Estimando o efeito de primeira ordem (intensidade) usando 
#diferentes kernels
Houstonkde.g1 = density.ppp(x = Houstonppp, 
                       sigma = 0.003, 
                       kernel="quartic")
Houstonkde.g2 = density.ppp(x = Houstonppp, 
                       sigma = 0.003, 
                       kernel="gaussian")
Houstonkde.g3 = density.ppp(x = Houstonppp, 
                       sigma = 0.003, 
                       kernel="epanechnikov")

#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(2,2))
par(mar=c(0.5,0.5,1.5,0.5))
plot(Houstonppp, pch=21, cex=0.3, bg="blue", main="Crimes", cex.main=0.5)
plot(Houstonkde.g1, main="Kernel Quartico", cex.main=0.5)
plot(Houstonkde.g2, main="Kernel Normal")
plot(Houstonkde.g3, main="Kernel Epanechnikov")
par(mfrow=c(1,1))


#Avaliando o impacto de diferentes raios (tau)
Houstonkde.tau1 = density.ppp(x = Houstonppp, 
                            sigma = 0.003, 
                            kernel="gaussian")
Houstonkde.tau2 = density.ppp(x = Houstonppp, 
                            sigma = 0.01, 
                            kernel="gaussian")
Houstonkde.tau3 = density.ppp(x = Houstonppp, 
                            sigma = 0.05, 
                            kernel="gaussian")

par(mfrow=c(2,2))
par(mar=c(0.5,0.5,1.5,0.5))
plot(Houstonppp, pch=21, cex=0.3, bg="blue", main="Crimes", cex.main=0.5)
plot(Houstonkde.tau1, main="sigma = 0.003", cex.main=0.5)
plot(Houstonkde.tau2, main="sigma = 0.01")
plot(Houstonkde.tau3, main="sigma = 0.05")
par(mfrow=c(1,1))


#Estimando Densidade com GGMAP
density_ggmap <- qmplot(x = lon, 
                       y = lat, 
                       data = crimeHouston,
                       colour = I('red'), 
                       size = I(0.3), 
                       darken = 0.3) +
  stat_density2d(data = crimeHouston,
                 aes(x = lon, y = lat, fill = ..level..),
                 alpha = 0.4,
                 h = 0.025,
                 n = 400,
                 geom = "polygon") +
  scale_fill_gradient(low = "black", 
                      high= "red")

#Visualizando o objeto
density_ggmap

# Estimando a intensidade para cada tipo de delito, usando o grafico realizado para todos os delitos
density_ggmap + 
  facet_wrap(~offense,
             labeller = labeller(offense = c("aggravated assault"="LESÃO CORPORAL",
                                             "rape" = "ESTUPRO",
                                             "robbery" = "ROUBO",
                                             "burglary" = "ARROMBAMENTO",
                                             "murder" = "ASSASSINATO",
                                             "auto theft" = "FURTO DE VEÍCULO")))

#-------------------------------------------------------------------------------------#
# ------------------------------ Efeito de 2a ordem  ---------------------------------#
#-------------------------------------------------------------------------------------#

#Estimando a funcao G
Houston.G = Gest(Houstonppp)

#Plotando a funcao G
par(mar=c(2.5,2.5,1.5,0.5))
plot(Houston.G, main="Funcao G")

#Funcoes para estimar os envelopes da funcao G
Genv = envelope(Houstonppp, fun = Gest, nsim = 20)
plot(Genv)

#Realizando o teste de Clark-Evans para testar agregacao espacial
clarkevans.test(Houstonppp, alternative = "less")


#### Dividindo o conjunto de dados por tipo de crime

#ASSASSINATOS
murderHouston <- subset(crimeHouston,offense=="murder")
murderppp = ppp( x = murderHouston$lon,
                  y = murderHouston$lat, 
                  window = HoustonOWin)
Genv_murder <- envelope(murderppp, fun = Gest, nsim = 20)

#LESAO CORPORAL
assaultHouston <- subset(crimeHouston,offense=="aggravated assault")
assaultppp = ppp(x = assaultHouston$lon,
                 y = assaultHouston$lat, 
                 window = HoustonOWin)
Genv_assault <- envelope(assaultppp, fun = Gest, nsim = 20)

#ESTUPRO
rapeHouston <- subset(crimeHouston,offense=="rape")
rapeppp <- ppp(x = rapeHouston$lon,
                 y = rapeHouston$lat, 
                 window = HoustonOWin)
Genv_rape <- envelope(rapeppp, fun = Gest, nsim = 20)

#ROUBO
robberyHouston <- subset(crimeHouston,offense=="robbery")
robberyppp <- ppp(x = robberyHouston$lon,
              y = robberyHouston$lat, 
              window = HoustonOWin)
Genv_robbery <- envelope(robberyppp, fun = Gest, nsim = 20)

#ARROMBAMENTO
burglaryHouston <- subset(crimeHouston,offense=="burglary")
burglaryppp <- ppp(x = burglaryHouston$lon,
                 y = burglaryHouston$lat, 
                 window = HoustonOWin)
Genv_burglary <- envelope(burglaryppp, fun = Gest, nsim = 20)

#FURTO DE VEICULO
autotheftHouston <- subset(crimeHouston,offense=="auto theft")
autotheftppp <- ppp(x = autotheftHouston$lon,
                  y = autotheftHouston$lat, 
                  window = HoustonOWin)
Genv_autotheft <- envelope(autotheftppp, fun = Gest, nsim = 20)

#Comparando as curvas G
par(mfrow=c(2,3))
plot(Genv_murder, main="ASSASSINATO")
plot(Genv_assault, main="LESAO CORPORAL")
plot(Genv_rape, main="ESTUPRO")
plot(Genv_robbery, main="ROUBO")
plot(Genv_burglary, main="ARROMBAMENTO")
plot(Genv_autotheft, main="FURTO DE VEICULO")

#Realizando o teste de Clark-Evans para testar agregacao espacial
clarkevans.test(murderppp, alternative = "less")
clarkevans.test(assaultppp, alternative = "less")
clarkevans.test(rapeppp, alternative = "less")
clarkevans.test(robberyppp, alternative = "less")
clarkevans.test(burglaryppp, alternative = "less")
clarkevans.test(autotheftppp, alternative = "less")
