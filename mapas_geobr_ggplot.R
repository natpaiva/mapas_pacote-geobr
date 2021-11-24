##############################################
### CRIANDO MAPAS NO R
### PACOTE 'GEOBR'
### USANDO SHP DISPONIVEL NO PACOTE 'GEOBR'
### TAXA DE MORTALIDADE MATERNA por UF, 2019
### NATALIA PAIVA IESC - UFRJ
#############################################



library(geobr)
library(ggplot2) 
library(sf)
library(dplyr)
library(rio)
library(RColorBrewer)
library(ggsn) # escala e rosa dos ventos

# exemplos de rosas dos ventos
northSymbols()

# TAXA DE MORTALIDADE MATERNA (por 100mil nascidos vivos), 2019
# Fonte dos dados: SIM e SINASC
dados <- import("RazaoMortMaterna2019-mapas.xlsx") 
head(dados)

summary(dados$Razao)

# shapefile do Mapa do Brasil por UF (ano de 2019)
mapa.br <- read_state(year=2019, showProgress = FALSE) 

# shapefile do Mapa do estado do RJ (ano de 2010)
mapa.rj <- read_state(code_state="RJ", year=2010, showProgress = FALSE) 

# shapefile do Mapa do estado do RJ por municipios (ano de 2018)
mapa.rj.munic <- read_municipality(code_muni= "RJ", year=2018)


# Plotando todos os estados do Brasil
ggplot() +
  geom_sf(data= mapa.br, fill="gray80", color="white", size=.15, show.legend = FALSE) +
  labs(subtitle="Brasil por UF (shapefile de 2019)", size=8) +
  theme_void()



# Plotando estado do Rio de Janeiro 
ggplot() +
  geom_sf(data= mapa.rj, fill="white", color="black", size=0.20, show.legend = FALSE) +
  labs(title="Estado do Rio de Janeiro, 2010 ", size=8) +
  theme_void() 

# Plotando estado do Rio de Janeiro por Municípios
ggplot() +
  geom_sf(data= mapa.rj, fill="darkgreen", color="black", size=.10, show.legend = FALSE) +
  labs(title=" Estado do Rio de Janeiro por Municípios, 2018 ", size=8) +
  theme_void() 


#######################
## JUNTAR BASE DE DADOS de RAZÃO DE MORT MATERNA COM SHAPEFILE BRASIL POR UF
######################

# Fazendo left_join das informações do geobr com os dados
# precisamos que as UFs estejam escritas em ambas as bases de MESMA maneira

mapa.br <- dplyr::left_join(mapa.br, dados, by = "abbrev_state")
mapa.br


# Código do MAPA
mapa.br %>%
  ggplot() +
  geom_sf(aes(fill = Razao)) +
  north(mapa.br, symbol = 1) + # rosa dos ventos
  scalebar(mapa.br, dist = 500, dist_unit = "km", st.size = 2.75,
           transform = TRUE, model = "WGS84") + # barra de escala
  labs(title = "Razão mortalidade materna por 100 mil nascidos vivos, 2019",
       fill = "Razão",
       caption= "SIM e SINASC, 2019")+
  theme_void()


#Criando os Intervalos para a Legenda
#poderia colocar quartis, por exemplo
classes <- findInterval(mapa.br$Razao, c(-Inf, 40, 60, 80, Inf))

mapa.br <- mapa.br %>% 
  mutate(classes = factor(classes))

# Criar legenda
legenda =  c('Menos de 40',
             '40 |-- 60 ',
             '60 |-- 80 ',
             'Mais de 80')

cores = brewer.pal(n= 4, name = "Reds") # exemplos de name = Blues , Greens


# Código do MAPA
mapa.br %>%
  ggplot() +
  geom_sf(aes(fill = classes), color = "black") +
  north(mapa.br, symbol = 10) + # rosa dos ventos
  scalebar(mapa.br, dist = 500, dist_unit = "km", st.size = 2.75,
           location = "bottomleft", transform = TRUE, model = "WGS84") + # barra de escala
  scale_fill_manual(labels = legenda,
                    values = cores)+ 
  labs(title = "Razão mortalidade materna por 100 mil nascidos vivos, 2019",
       fill = "Razão",
       caption= "SIM e SINASC, 2019")+
  theme_void()


# Código do MAPA
ggplot() +
  geom_sf(data=mapa.br, aes(fill=Razao), color= NA, size=.15) +
  scalebar(mapa.br, dist = 500, location = "bottomright", transform = TRUE, #Adicione uma barra de escala
                 dist_unit = "km", st.size = 2.5, model = 'WGS84') +
  north(mapa.br, symbol = 3, scale = 0.10)+ # rosa dos ventos
  labs(title = "Razão mortalidade materna por 100 mil nascidos vivos, 2019",
       fill = "Razão",
       caption= "SIM e SINASC, 2019",
       size= 8) +
  scale_fill_distiller(palette = "Greens", name="Razão", # paletas = Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
                       limits = c(0,100), direction = 1) + # fique atento para limites
  theme_void() 

# Código do MAPA - separando por regioes
mapa.br %>%
  ggplot() +
  geom_sf(aes(fill = classes), color = "black") +
  scale_fill_manual(labels = legenda,
                    values = cores)+ # lembrar de rodar legenda, cores e classes antes;
  labs(title = "Razão mortalidade materna por 100 mil nascidos vivos, 2019",
       fill = "Razão",
       caption= "SIM e SINASC, 2019")+
  theme_bw()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  facet_wrap(~ name_region) # quebrando mapa por regioes


# vejam mais exemplos em https://jodavid.github.io/post/gerando-um-mapa-com-geobr-no-r/