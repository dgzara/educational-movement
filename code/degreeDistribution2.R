folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
library(openair)
library(ggplot2)
library(scales)

# Obtenemos los tweets dirigidos
q <- paste('SELECT *  
           FROM tweets',sep="")
tweets <- dbGetQuery(con, q)

# Formato
tweets$account <- substring(tweets$account, 2) # Eliminamos el @ del principio
tweets <- tweets[c("id", "tweetuser", "account", "tweet", "tweetdate")]
colnames(tweets) <- c("id", "source", "target", "tweet", "date")

# Reordenamos
tweets$id <- NULL

# Listado de instituciones
orgs <- c("jjcc_chile", "Rdemocratica", "ChileaLaMoneda", "feuc", "Estafados_CORFO", "Izquierda_Tuit", "naupuc", "feusach", "la_fech", "confech", "mguc", "izqautonoma", "u_informado", "mesup_Chile", "difusion_ACES", "infestudiantes", "feuv", "ucscconcepcion", "creceruc", "SolidaridadUC", "FECUdeC", "CONESCHILE", "UNE_CHILE", "feutfsm", "FEUSAM", "FEVUNAB", "secundarios", "FEUAI_stgo", "DeudaEducativa", "FEUANDES", "FEL_Stgo", "Feutal2013", "FEDEPUDP", "_FEUCEN", "FEUDD_stgo", "FEUBO_OFICIAL", "informanteUDA", "FeustSantiago", "FEPUCV", "FEUTEM", "FEUDMVina", "FepPedagogico", "FELUCHILE", "FEUFRO", "privmovilizadas", "feubb", "FEUAntofagasta", "Feupla", "DelegConfechUC", "feusmjmc", "FEUCN", "FeuachUACH", "Feuls", "MACEUAI", "feuvsantiago", "FEUCM2011", "FEDEUNAP", "feucncqbo", "la_Fech", "soydederecha", "Bienestar_FECh", "brigadachacon", "alvaroramis", "CREASUAH", "marxismo_cl", "Une_Santiago", "CiudadaniaDenun", "juvnacionalista", "jjcc_uchile", "la_confesh", "CeapUsach2013", "MovilizateChile", "mesup_chile", "RDValdivia", "profes_UCENTRAL", "centroGAM", "SomosLasF3", "Chilejusto2012", "Cacerolachilena", "AccionChileVerd", "Feuc", "feuahurtado", "NOaBachelet", "CACoUC", "oindependiente", "GNoViolenta", "MedicosPorChile", "canal_chile", "Rev_ecoamerica", "ElChilenero", "FOACHcl", "PropeUsach", "difamadores", "juventupolitica", "Humanidades_Uch", "Difusion_ACES", "Infestudiantes", "DerechaTuitera", "CENDA_chile", "ProAccesoChile", "PAIEPUSACH", "Feudem_Talca", "InformanteUDA", "Feuv", "fel_chile", "Feuls13", "Feudem_Iqq", "Privmovilizadas", "FEUAI_vina", "Corfo", "feul_osorno", "EstafadosCorfo9", "PPDbiobio", "JuventudGuzman", "feudlaconce", "FeUpla", "Juventudes_PRI", "IgualesChile", "FedeumCurico", "FEUDLA_", "FundJaimeGuzman", "JuventudRebelde", "fedeumsanfer", "Juvenil_Iguales", "feufro", "FEUVStgo", "FundacionFEDES", "Feul_PtoMontt", "juventudCCS", "PPD_Chile", "FundacionPuente", "feummagallanes", "FundacionPortas", "PPDStgoCentro", "PDC_Chile", "Feubbchillan", "fundacionchile", "JuventudClaude", "FeudlaSomostods", "Feudla_VL", "JuventudPSUV", "EstafadosUDM", "EndeudadosCorfo", "fundacionLJ", "FEJ_Chile", "JuventudVolante", "FundTemplanza", "FedPalestina", "JvenesXBachelet", "FEUDD_STGO", "JuvProvidencia", "EstafadosporBan", "Juventud_PPD", "SecundariosJS", "SecundariosJJCC", "FeutfsmConceRbb", "Juventudhuastek", "FundacionAVINA", "FELsecundarios", "FundLasRosas", "JuventudenMarc", "JuventudLibrehn", "fundacionRAYUNd", "SecundariosJota", "JuventuDurango", "Fundacion_Tacal", "Juventud_UDI_30", "juventududivina", "FederacionUA", "Cores_PPD", "FEC_Chillan", "juventudecut", "FundacionDecide", "FEPUC", "JUVINDxMICHELLE", "FundacionFilba", "AC_Chile", "INJUVCHILE", "pdcprovidencia", "izqautonomauc")

# Listado de líderes
leaders <- c("camila_vallejo", "GiorgioJackson", "Sr_Ballesteros", "panchofigueroa", "velagrau", "AFielbaum", "gabrielboric", "NoamTitelman", "sebavielmas", "ANKALAO", "elperrotracio", "feliperasa", "RebecaGaeteS", "SDonoso_", "_marioalbertod", "danirslz", "PabloReyes_F", "sebafarfans", "marjoriecuello", "MoisesParedesR", "pepoglatz", "recarex", "PMillalen", "Pato_Contreras_", "jorbritoh", "SebaGodoyElguet", "goyarzun", "unavico", "j_miranda_s", "sebagamboa_", "vlatorre", "felipevargasr", "_isabelsalgado", "florespineda", "raecheco", "IriarteCORE", "acouble", "camiloriffo", "palta_mayo", "Godoy_JC", "pjgonzalez_", "leoiec", "NatalyEspin0za", "aalegred", "elRodrigoDuran", "Felipesalga", "guillermo_peter", "Javier_Fano", "intialavia", "IsmaelSoruco", "geo_sur", "FranciscoSainz", "FdaSandoval", "DanielaPobleteP", "Patricio_Indo", "krivera_uls", "ConstanzaLeyton", "BorisNegrete", "InzulzaAlberto", "Fco_Acunac", "OrleansRomero", "alexis_gonzlz", "pablo_fepucv", "d_cid", "Chriistopher_xD", "andresdouglasr", "Pipovaldebenito", "vargassasmay", "Carolina_Jarap", "DanaeDiazJeria", "Valeilic", "Javijadue", "EliasLonconado", "carlos_ruminott", "alvarobeckdorf", "Cris_Sarabia", "bernardo_barria", "jmiguelprieto", "YoxcyCampos", "Paonessa", "AngelitaJaviera", "carocatomas", "henry_varas", "Valessoncilla", "CrisLagos", "Mapa_Ruiz_", "germainquintana", "Franciscolabrav", "Coni_nogues", "pipe_higueras", "josefinaprivas", "Karolcariola", "_EloisaGonzalez")

# Dejamos solo los que nos interesan
for(i in 2011:2013){
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Obtenemos el resto
  tweets.people <- tweetsYear[(!(tweetsYear$target %in% orgs) && !(tweetsYear$target %in% leaders)), ]
  people <- unique(unlist(tweets.people$target, use.names = FALSE))
  print(length(people))
  
  # Obtenemos la distribución
  degree.in <- degree(network, V(network), mode="in")
  
  # Obtenemos las distribuciones separadas
  degree.in.leaders <- na.omit(degree.in[leaders])
  degree.in.orgs <- na.omit(degree.in[orgs])
  degree.in.people <- na.omit(degree.in[people])
  
  degree.in.leaders[degree.in.leaders==0] <- NA
  degree.in.orgs[degree.in.orgs==0] <- NA
  #degree.in.people[degree.in.people==0] <- NA
  
  degree.in.leaders <- na.omit(degree.in.leaders)
  degree.in.orgs <- na.omit(degree.in.orgs)
  degree.in.people <- na.omit(degree.in.people)
  
  # Creamos el grafico
  p1 <- ggplot()
  
  # Degree de los líderes
  if(length(degree.in.leaders) > 0){
    degree.in.leaders.df <- data.frame(table(degree=factor(degree.in.leaders, levels=seq_len(max(degree.in.leaders)))))
    degree.in.leaders.df$degree <- as.numeric(as.character(degree.in.leaders.df$degree))
    p1 <- p1 + geom_line(data = degree.in.leaders.df, aes(x=degree, y=Freq, color = "Leaders")) 
  } 
  
  # Degree de las organizaciones
  if(length(degree.in.orgs) > 0){
    degree.in.orgs.df <- data.frame(table(degree=factor(degree.in.orgs, levels=seq_len(max(degree.in.orgs)))))
    degree.in.orgs.df$degree <- as.numeric(as.character(degree.in.orgs.df$degree))
    p1 <- p1 + geom_line(data = degree.in.orgs.df, aes(x=degree, y=Freq, color = "Organizations"))
  } 
  
  # Degree de los normales
  if(length(degree.in.people) > 0){
    degree.in.people.df <- data.frame(table(degree=factor(degree.in.people, levels=seq_len(max(degree.in.people)))))
    degree.in.people.df$degree <- as.numeric(as.character(degree.in.people.df$degree))  
    p1 <- p1 + geom_line(data = degree.in.people.df, aes(x=degree, y=Freq, color = "People"))
  } 
  
  # Plot
  p1 <- p1 +
    xlab("Indegree") + ylab("Freq") +
    scale_x_log10() +
    scale_y_log10() +
    theme(legend.position="bottom", legend.direction="vertical") +
    ggtitle(i)
  
  ggsave(paste0("../data/plots/distributions/",i,".pdf",sep = ""), plot = p1)
  
  # Removemos las variables
  rm(tweetsYear, tweets.people, network, degree.in, degree.in.people, degree.in.orgs, degree.in.leaders, degree.in.people.df, degree.in.orgs.df, degree.in.leaders.df)  
}