folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
library(openair)
library(ggplot2)
library(scales)

# Obtenemos los tweets de los hashtags
q <- paste('SELECT *  
           FROM hashtag',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- unique(unlist(tweets$account, use.names = FALSE))

# Reordenamos
colnames(tweets) <- c("id", "hashtag", "user", "tweet", "date")
tweets$id <- NULL

# Listado de instituciones
orgs <- c("jjcc_chile", "Rdemocratica", "ChileaLaMoneda", "feuc", "Estafados_CORFO", "Izquierda_Tuit", "naupuc", "feusach", "la_fech", "confech", "mguc", "izqautonoma", "u_informado", "mesup_Chile", "difusion_ACES", "infestudiantes", "feuv", "ucscconcepcion", "creceruc", "SolidaridadUC", "FECUdeC", "CONESCHILE", "UNE_CHILE", "feutfsm", "FEUSAM", "FEVUNAB", "secundarios", "FEUAI_stgo", "DeudaEducativa", "FEUANDES", "FEL_Stgo", "Feutal2013", "FEDEPUDP", "_FEUCEN", "FEUDD_stgo", "FEUBO_OFICIAL", "informanteUDA", "FeustSantiago", "FEPUCV", "FEUTEM", "FEUDMVina", "FepPedagogico", "FELUCHILE", "FEUFRO", "privmovilizadas", "feubb", "FEUAntofagasta", "Feupla", "DelegConfechUC", "feusmjmc", "FEUCN", "FeuachUACH", "Feuls", "MACEUAI", "feuvsantiago", "FEUCM2011", "FEDEUNAP", "feucncqbo", "la_Fech", "soydederecha", "Bienestar_FECh", "brigadachacon", "alvaroramis", "CREASUAH", "marxismo_cl", "Une_Santiago", "CiudadaniaDenun", "juvnacionalista", "jjcc_uchile", "la_confesh", "CeapUsach2013", "MovilizateChile", "mesup_chile", "RDValdivia", "profes_UCENTRAL", "centroGAM", "SomosLasF3", "Chilejusto2012", "Cacerolachilena", "AccionChileVerd", "Feuc", "feuahurtado", "NOaBachelet", "CACoUC", "oindependiente", "GNoViolenta", "MedicosPorChile", "canal_chile", "Rev_ecoamerica", "ElChilenero", "FOACHcl", "PropeUsach", "difamadores", "juventupolitica", "Humanidades_Uch", "Difusion_ACES", "Infestudiantes", "DerechaTuitera", "CENDA_chile", "ProAccesoChile", "PAIEPUSACH", "Feudem_Talca", "InformanteUDA", "Feuv", "fel_chile", "Feuls13", "Feudem_Iqq", "Privmovilizadas", "FEUAI_vina", "Corfo", "feul_osorno", "EstafadosCorfo9", "PPDbiobio", "JuventudGuzman", "feudlaconce", "FeUpla", "Juventudes_PRI", "IgualesChile", "FedeumCurico", "FEUDLA_", "FundJaimeGuzman", "JuventudRebelde", "fedeumsanfer", "Juvenil_Iguales", "feufro", "FEUVStgo", "FundacionFEDES", "Feul_PtoMontt", "juventudCCS", "PPD_Chile", "FundacionPuente", "feummagallanes", "FundacionPortas", "PPDStgoCentro", "PDC_Chile", "Feubbchillan", "fundacionchile", "JuventudClaude", "FeudlaSomostods", "Feudla_VL", "JuventudPSUV", "EstafadosUDM", "EndeudadosCorfo", "fundacionLJ", "FEJ_Chile", "JuventudVolante", "FundTemplanza", "FedPalestina", "JvenesXBachelet", "FEUDD_STGO", "JuvProvidencia", "EstafadosporBan", "Juventud_PPD", "SecundariosJS", "SecundariosJJCC", "FeutfsmConceRbb", "Juventudhuastek", "FundacionAVINA", "FELsecundarios", "FundLasRosas", "JuventudenMarc", "JuventudLibrehn", "fundacionRAYUNd", "SecundariosJota", "JuventuDurango", "Fundacion_Tacal", "Juventud_UDI_30", "juventududivina", "FederacionUA", "Cores_PPD", "FEC_Chillan", "juventudecut", "FundacionDecide", "FEPUC", "JUVINDxMICHELLE", "FundacionFilba", "AC_Chile", "INJUVCHILE", "pdcprovidencia", "izqautonomauc")

# Listado de líderes
leaders <- c("camila_vallejo", "GiorgioJackson", "Sr_Ballesteros", "panchofigueroa", "velagrau", "AFielbaum", "gabrielboric", "NoamTitelman", "sebavielmas", "ANKALAO", "elperrotracio", "feliperasa", "RebecaGaeteS", "SDonoso_", "_marioalbertod", "danirslz", "PabloReyes_F", "sebafarfans", "marjoriecuello", "MoisesParedesR", "pepoglatz", "recarex", "PMillalen", "Pato_Contreras_", "jorbritoh", "SebaGodoyElguet", "goyarzun", "unavico", "j_miranda_s", "sebagamboa_", "vlatorre", "felipevargasr", "_isabelsalgado", "florespineda", "raecheco", "IriarteCORE", "acouble", "camiloriffo", "palta_mayo", "Godoy_JC", "pjgonzalez_", "leoiec", "NatalyEspin0za", "aalegred", "elRodrigoDuran", "Felipesalga", "guillermo_peter", "Javier_Fano", "intialavia", "IsmaelSoruco", "geo_sur", "FranciscoSainz", "FdaSandoval", "DanielaPobleteP", "Patricio_Indo", "krivera_uls", "ConstanzaLeyton", "BorisNegrete", "InzulzaAlberto", "Fco_Acunac", "OrleansRomero", "alexis_gonzlz", "pablo_fepucv", "d_cid", "Chriistopher_xD", "andresdouglasr", "Pipovaldebenito", "vargassasmay", "Carolina_Jarap", "DanaeDiazJeria", "Valeilic", "Javijadue", "EliasLonconado", "carlos_ruminott", "alvarobeckdorf", "Cris_Sarabia", "bernardo_barria", "jmiguelprieto", "YoxcyCampos", "Paonessa", "AngelitaJaviera", "carocatomas", "henry_varas", "Valessoncilla", "CrisLagos", "Mapa_Ruiz_", "germainquintana", "Franciscolabrav", "Coni_nogues", "pipe_higueras", "josefinaprivas", "Karolcariola", "_EloisaGonzalez")

# Dejamos solo los que nos interesan
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  
  # Generamos y ordenamos el arreglo de las fechas
  dates <- unique(unlist(tweets.hashtag$date, use.names = FALSE))
  dates <- dates[order(format(as.Date(dates),"%m%d"))]
  
  table.hashtag <- c()
  
  # Generamos la separacion de fechas
  for(j in 1:length(dates))
  {
    tweets.hashtag.date <- tweets.hashtag[(tweets.hashtag$date == dates[j]) , ]
    
    # Obtenemos los lideres
    tweets.leaders <- tweets.hashtag.date[(tweets.hashtag.date$user %in% leaders) , ]
    users.leaders <- unique(unlist(tweets.leaders$user, use.names = FALSE))
    
    # Obtenemos las organizaciones
    tweets.orgs <-  tweets.hashtag.date[(tweets.hashtag.date$user %in% orgs) , ]
    users.orgs <- unique(unlist(tweets.orgs$user, use.names = FALSE))
    
    # Obtenemos el resto
    tweets.people <- tweets.hashtag.date[(!(tweets.hashtag.date$user %in% orgs) && !(tweets.hashtag.date$user %in% leaders)), ]
    users.people <- unique(unlist(tweets.people$user, use.names = FALSE))
    
    # Armamos la fila
    row <- c()
    row$date <- dates[j]
    row$leaders <- if (length(users.leaders) > 0) nrow(tweets.leaders)/length(users.leaders) else 0
    row$orgs <- if (length(users.orgs) > 0) nrow(tweets.orgs)/length(users.orgs) else 0
    row$people <- if (length(users.people) > 0) nrow(tweets.people)/length(users.people) else 0
    row <- as.data.frame(row, stringsAsFactors = FALSE)
    
    # Generamos la tabla
    table.hashtag <- rbind(table.hashtag, row)
    
    # Removemos
    rm(tweets.hashtag.date, tweets.people, tweets.orgs, tweets.leaders, row)
  }
  
  # Plot
  p1 <- ggplot() + 
    geom_line(data = table.hashtag, aes(x = date, y = people, color = "People")) +
    geom_line(data = table.hashtag, aes(x = date, y = leaders, color = "Leaders")) +
    geom_line(data = table.hashtag, aes(x = date, y = orgs, color = "Organizations")) +
    #scale_x_date(breaks = date_breaks(width = "10 day")) + 
    xlab("") + ylab("Dialy tweets per user") +
    #scale_y_log10() +
    theme(legend.position="bottom",legend.direction="vertical") +
    ggtitle(i)
  
  ggsave(paste0("../data/plots/",i,".pdf",sep = ""), plot = p1)
  
  # Guardamos la tabla
  assign(paste("table.hashtag", i, sep = ""), table.hashtag)
  
  # Removemos las variables
  rm(tweets.hashtag, dates, table.hashtag)  
}