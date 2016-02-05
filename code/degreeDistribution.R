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
           FROM hashtag_user_network',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Reordenamos
tweets$id <- NULL

# Listado de instituciones
orgs <- c("feuc", "feusach", "la_fech", "confech", "mesup_Chile", "difusion_ACES", "feuv", "ucscconcepcion", "FECUdeC", "CONESCHILE", "UNE_CHILE", "com_fech", "feutfsm", "FEUSAM", "FEVUNAB", "secundarios", "FEUAI_stgo", "FEUANDES", "FEL_Stgo", "Feutal2013", "FEDEPUDP", "_FEUCEN", "FEUDD_stgo", "FEUBO_OFICIAL", "FeustSantiago", "FEPUCV", "FEUTEM", "FEUDMVina", "FepPedagogico", "FELUCHILE", "FEUFRO", "feubb", "FEUAntofagasta", "Feupla", "DelegConfechUC", "feusmjmc", "FEUCN", "FeuachUACH", "Feuls", "feuvsantiago", "FEUCM2011", "FEDEUNAP", "feucncqbo", "la_Fech", "Bienestar_FECh", "Une_Santiago", "CeapUsach2013", "Feuc", "feuahurtado", "CACoUC", "FOACHcl", "PropeUsach", "Difusion_ACES", "Feudem_Talca", "Feuv", "Feuls13", "FEUAI_vina", "feul_osorno", "feudlaconce", "FeUpla", "FedeumCurico", "FEUDLA_", "fedeumsanfer", "feufro", "FEUVStgo", "FundacionFEDES", "Feul_PtoMontt", "feummagallanes", "Feubbchillan", "Feudla_VL", "FEJ_Chile", "FEUDD_STGO", "SecundariosJS", "SecundariosJJCC", "FeutfsmConceRbb", "Juventudhuastek", "FELsecundarios", "FederacionUA", "FEC_Chillan", "FEPUC")

# Listado de movimientos
movs <- c("ccp_movilizada", "manecolombia", "yodebocl", "1800horas", "soyvalparaiso", "ciclopatagonia", "twittsejero", "mfc_oficial", "bicicultura", "heavyciclistas", "vidaoutdoor", "ciclistaseduc", "maqmaravillosa", "vivelabici", "comunistamexico", "indignateco", "prensaestudiant", "confidencialcol", "radicalyfeliz", "colpoderpopular", "link_anarquista", "anonchileno", "fdialoga", "jjcc_chile", "amnistiachile", "jsusach", "caipuc", "twittsejera", "tomalafacultad", "trabajo_chile", "soychilecl", "anefchile", "oposicionpinera", "happyciclistas", "cicilistaseduc", "educacion2020", "superarpobreza", "universitecl", "ciclismourbano", "yomesumo", "yonovotexel", "nolesvoteschile", "aministiachile", "aqui_leo", "Rdemocratica", "coneschile", "marcoporchile", "lafundacionsol", "ChileaLaMoneda", "Estafados_CORFO", "Izquierda_Tuit", "naupuc", "mguc", "izqautonoma", "u_informado", "infestudiantes", "creceruc", "SolidaridadUC", "DeudaEducativa", "valdivianos", "informanteUDA", "privmovilizadas", "MACEUAI", "soydederecha", "brigadachacon", "CREASUAH", "marxismo_cl", "CiudadaniaDenun", "la_confesh", "CeapUsach2013", "MovilizateChile", "mesup_chile", "RDValdivia", "profes_UCENTRAL", "centroGAM", "SomosLasF3", "Chilejusto2012", "Cacerolachilena", "AccionChileVerd", "NOaBachelet", "CACoUC", "oindependiente", "GNoViolenta", "MedicosPorChile", "canal_chile", "acciongay", "Rev_ecoamerica", "ElChilenero", "difamadores", "juventupolitica", "Humanidades_Uch", "Infestudiantes", "DerechaTuitera", "CENDA_chile", "ProAccesoChile", "PAIEPUSACH", "InformanteUDA", "fel_chile", "Feudem_Iqq", "Privmovilizadas", "EstafadosCorfo9", "PPDbiobio", "JuventudGuzman", "Juventudes_PRI", "movimientosurda", "democraciareal", "IgualesChile", "igualeschile", "FundJaimeGuzman", "JuventudRebelde", "Juvenil_Iguales", "FundacionFEDES", "juventudCCS", "PPD_Chile", "FundacionPuente", "FundacionPortas", "PPDStgoCentro", "PDC_Chile", "fundacionchile", "JuventudClaude", "FeudlaSomostods", "JuventudPSUV", "EstafadosUDM", "EndeudadosCorfo", "fundacionLJ", "JuventudVolante", "FundTemplanza", "FedPalestina", "JvenesXBachelet", "JuvProvidencia", "EstafadosporBan", "Juventud_PPD", "SecundariosJS", "SecundariosJJCC", "Juventudhuastek", "FundacionAVINA", "FELsecundarios", "FundLasRosas", "JuventudenMarc", "JuventudLibrehn", "fundacionRAYUNd", "SecundariosJota", "JuventuDurango", "Fundacion_Tacal", "Juventud_UDI_30", "juventududivina", "Cores_PPD", "juventudecut", "FundacionDecide", "JUVINDxMICHELLE", "FundacionFilba", "AC_Chile", "INJUVCHILE", "pdcprovidencia", "izqautonomauc")

# Listado de líderes
leaders <- c("camila_vallejo", "valenzuelalevi", "srcrispin", "GiorgioJackson", "Sr_Ballesteros", "panchofigueroa", "velagrau", "AFielbaum", "gabrielboric", "NoamTitelman", "sebavielmas", "ANKALAO", "elperrotracio", "feliperasa", "RebecaGaeteS", "SDonoso_", "_marioalbertod", "danirslz", "PabloReyes_F", "sebafarfans", "marjoriecuello", "MoisesParedesR", "pepoglatz", "recarex", "PMillalen", "Pato_Contreras_", "jorbritoh", "SebaGodoyElguet", "goyarzun", "unavico", "j_miranda_s", "sebagamboa_", "vlatorre", "felipevargasr", "_isabelsalgado", "florespineda", "raecheco", "IriarteCORE", "acouble", "camiloriffo", "palta_mayo", "Godoy_JC", "pjgonzalez_", "leoiec", "NatalyEspin0za", "aalegred", "elRodrigoDuran", "Felipesalga", "guillermo_peter", "Javier_Fano", "intialavia", "IsmaelSoruco", "geo_sur", "FranciscoSainz", "FdaSandoval", "DanielaPobleteP", "Patricio_Indo", "krivera_uls", "ConstanzaLeyton", "BorisNegrete", "InzulzaAlberto", "Fco_Acunac", "OrleansRomero", "alexis_gonzlz", "pablo_fepucv", "d_cid", "Chriistopher_xD", "andresdouglasr", "Pipovaldebenito", "vargassasmay", "Carolina_Jarap", "DanaeDiazJeria", "Valeilic", "Javijadue", "EliasLonconado", "carlos_ruminott", "alvarobeckdorf", "Cris_Sarabia", "bernardo_barria", "jmiguelprieto", "YoxcyCampos", "Paonessa", "AngelitaJaviera", "carocatomas", "henry_varas", "Valessoncilla", "CrisLagos", "Mapa_Ruiz_", "germainquintana", "Franciscolabrav", "Coni_nogues", "pipe_higueras", "josefinaprivas", "Karolcariola", "_EloisaGonzalez")
 
# Listado de medios
medios <- c("el_dinamo", "sentidoscomunes", "elquintopoder", "elespectador", "austral_losrios", "austral_osorno", "lidersanantonio", "elcomerciocom",  "opanoticias", "phumano", "eltiempo", "thecliniccl", "nacioncl", "larepublica_pe", "latercera", "cooperativa", "votainteligente", "cnnchile", "canal_chile", "rvfradiopopular", "tvparachile", "radiosantamaria", "elmostrador", "tolerancia0", "elpost_cl", "mercuriovalpo", "mercurioafta", "diarioafta", "laotravoz", "soyarica")

# Listado de personalidades (periodistas, políticos, columnistas, etc)
famosos <- c("illapu", "intihistorico", "inti_historico", "tvn_mauricio", "copano", "soledadalvear", "lostres", "marcelodiazd", "fernandopaulsen", "inti_illimani", "mwaissbluth", "biobio", "mercuriovalpo", "alfonsoconcha", "sboric", "flaitechileno", "pcayuqueo", "renenaranjo", "toledo_campos", "mariseka", "hectorcaldera", "elterribledios", "danieljadue", "sobras", "aalaluf", "jaimegajardo", "ingridcruztoro", "sesnaola", "ignaciowalker", "pablosimonetti", "adribarrientos")

# Dejamos solo los que nos interesan
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  tweets.hashtag.network <- tweets.hashtag[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag.network, directed=TRUE)
  
  # Obtenemos el resto
  tweets.people <- V(network)[!((V(network)$name %in% orgs) | (V(network)$name %in% leaders) | (V(network)$name %in% movs) | (V(network)$name %in% medios) | (V(network)$name %in% famosos))]$name
  people <- unique(unlist(tweets.people, use.names = FALSE))
  
  # Obtenemos la distribución
  degree.in <- degree(network, V(network), mode="in")
  
  # Obtenemos las distribuciones separadas
  degree.in.leaders <- na.omit(degree.in[leaders])
  degree.in.orgs <- na.omit(degree.in[orgs])
  degree.in.people <- na.omit(degree.in[people])
  degree.in.movs <- na.omit(degree.in[movs])
  degree.in.medios <- na.omit(degree.in[medios])
  degree.in.famosos <- na.omit(degree.in[famosos])
  
  degree.in.leaders[degree.in.leaders==0] <- NA
  degree.in.orgs[degree.in.orgs==0] <- NA
  degree.in.people[degree.in.people==0] <- NA
  degree.in.movs[degree.in.movs==0] <- NA
  degree.in.medios[degree.in.medios==0] <- NA
  degree.in.famosos[degree.in.famosos==0] <- NA
  
  degree.in.leaders <- na.omit(degree.in.leaders)
  degree.in.orgs <- na.omit(degree.in.orgs)
  degree.in.people <- na.omit(degree.in.people)
  degree.in.movs <- na.omit(degree.in.movs)
  degree.in.medios <- na.omit(degree.in.medios)
  degree.in.famosos <- na.omit(degree.in.famosos)
  
  # Creamos el grafico
  p1 <- ggplot()
    
  # Degree de los líderes
  if(length(degree.in.leaders) > 0){
    degree.in.leaders.df <- data.frame(table(degree=factor(degree.in.leaders, levels=seq_len(max(degree.in.leaders)))))
    degree.in.leaders.df$degree <- as.numeric(as.character(degree.in.leaders.df$degree))
    degree.in.leaders.df <- degree.in.leaders.df[(degree.in.leaders.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.leaders.df, aes(x=degree, y=Freq, color = "Leaders", shape="Leaders")) 
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Leaders", shape="Leaders")) 
  }
  
  # Degree de las organizaciones
  if(length(degree.in.orgs) > 0){
    degree.in.orgs.df <- data.frame(table(degree=factor(degree.in.orgs, levels=seq_len(max(degree.in.orgs)))))
    degree.in.orgs.df$degree <- as.numeric(as.character(degree.in.orgs.df$degree))
    degree.in.orgs.df <- degree.in.orgs.df[(degree.in.orgs.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.orgs.df, aes(x=degree, y=Freq, color = "Organizations", shape="Organizations"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Organizations", shape="Organizations")) 
  }
  
  # Degree de los movimientos
  if(length(degree.in.movs) > 0){
    degree.in.movs.df <- data.frame(table(degree=factor(degree.in.movs, levels=seq_len(max(degree.in.movs)))))
    degree.in.movs.df$degree <- as.numeric(as.character(degree.in.movs.df$degree))
    degree.in.movs.df <- degree.in.movs.df[(degree.in.movs.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.movs.df, aes(x=degree, y=Freq, color = "Movements", shape="Movements"))
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Movements", shape="Movements")) 
  }
  
  # Degree de los medios
  if(length(degree.in.medios) > 0){
    degree.in.medios.df <- data.frame(table(degree=factor(degree.in.medios, levels=seq_len(max(degree.in.medios)))))
    degree.in.medios.df$degree <- as.numeric(as.character(degree.in.medios.df$degree))
    degree.in.medios.df <- degree.in.medios.df[(degree.in.medios.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.medios.df, aes(x=degree, y=Freq, color = "Media", shape="Media"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Media", shape="Media")) 
  }
  
  # Degree de los famosos
  if(length(degree.in.famosos) > 0){
    degree.in.famosos.df <- data.frame(table(degree=factor(degree.in.famosos, levels=seq_len(max(degree.in.famosos)))))
    degree.in.famosos.df$degree <- as.numeric(as.character(degree.in.famosos.df$degree))
    degree.in.famosos.df <- degree.in.famosos.df[(degree.in.famosos.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.famosos.df, aes(x=degree, y=Freq, color = "Celebrities", shape="Celebrities"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Celebrities", shape="Celebrities")) 
  }
  
  # Degree de los normales
  if(length(degree.in.people) > 0){
    degree.in.people.df <- data.frame(table(degree=factor(degree.in.people, levels=seq_len(max(degree.in.people)))))
    degree.in.people.df$degree <- as.numeric(as.character(degree.in.people.df$degree))  
    degree.in.people.df <- degree.in.people.df[(degree.in.people.df$Freq != 0),]
    p1 <- p1 + geom_point(data = degree.in.people.df, aes(x=degree, y=Freq, color = "People", shape="People"))
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "People", shape="People")) 
  }
  
  # Plot
  p1 <- p1 +
  xlab("Indegree") + ylab("Freq") + labs(colour = "Types", shape = "Types") +
  scale_y_log10() +
  theme(legend.position="bottom", legend.direction="horizontal") +
  ggtitle(i) 
  
  # Guardamos la tabla
  data.degree <- data.frame(user=names(degree.in), degree=degree.in, row.names=NULL)
  data.degree <- data.degree[order(-data.degree$degree),]
  #assign(paste("degree.", i, sep = ""), data.degree) 
  write.csv(format(data.degree, digits=5), paste("../data/enero2016/degree.", i, ".csv", sep = ""), row.names=TRUE)
  
  ggsave(paste0("../data/plots/hashtags/",i,".pdf",sep = ""), plot = p1)
  
  # Removemos las variables
  rm(tweets.hashtag, tweets.hashtag.network, tweets.people, network, degree.in.people, degree.in.orgs, degree.in.leaders, degree.in.famosos, degree.in.medios, degree.in.movs, degree.in.people.df, degree.in.orgs.df, degree.in.leaders.df, degree.in.famosos.df, degree.in.medios.df, degree.in.movs.df, degree.in, data.degree)  
}