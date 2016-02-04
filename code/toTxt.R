folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
library(openair)

# Obtenemos los tweets de los hashtags
q <- paste('SELECT *  
           FROM tweets',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)
tweets <- tweets[c("id", "tweetuser", "account", "tweet", "tweetdate")]
colnames(tweets) <- c("id", "tweetuser", "account", "tweet", "date")
tweets$id <- NULL
tweets$account <- NULL

# Listado de instituciones
orgs <- c("jjcc_chile", "Rdemocratica", "ChileaLaMoneda", "feuc", "Estafados_CORFO", "Izquierda_Tuit", "naupuc", "feusach", "la_fech", "confech", "mguc", "izqautonoma", "u_informado", "mesup_Chile", "difusion_ACES", "infestudiantes", "feuv", "ucscconcepcion", "creceruc", "SolidaridadUC", "FECUdeC", "CONESCHILE", "UNE_CHILE", "feutfsm", "FEUSAM", "FEVUNAB", "secundarios", "FEUAI_stgo", "DeudaEducativa", "FEUANDES", "FEL_Stgo", "Feutal2013", "FEDEPUDP", "_FEUCEN", "FEUDD_stgo", "FEUBO_OFICIAL", "informanteUDA", "FeustSantiago", "FEPUCV", "FEUTEM", "FEUDMVina", "FepPedagogico", "FELUCHILE", "FEUFRO", "privmovilizadas", "feubb", "FEUAntofagasta", "Feupla", "DelegConfechUC", "feusmjmc", "FEUCN", "FeuachUACH", "Feuls", "MACEUAI", "feuvsantiago", "FEUCM2011", "FEDEUNAP", "feucncqbo", "la_Fech", "soydederecha", "Bienestar_FECh", "brigadachacon", "alvaroramis", "CREASUAH", "marxismo_cl", "Une_Santiago", "CiudadaniaDenun", "juvnacionalista", "jjcc_uchile", "la_confesh", "CeapUsach2013", "MovilizateChile", "mesup_chile", "RDValdivia", "profes_UCENTRAL", "centroGAM", "SomosLasF3", "Chilejusto2012", "Cacerolachilena", "AccionChileVerd", "Feuc", "feuahurtado", "NOaBachelet", "CACoUC", "oindependiente", "GNoViolenta", "MedicosPorChile", "canal_chile", "Rev_ecoamerica", "ElChilenero", "FOACHcl", "PropeUsach", "difamadores", "juventupolitica", "Humanidades_Uch", "Difusion_ACES", "Infestudiantes", "DerechaTuitera", "CENDA_chile", "ProAccesoChile", "PAIEPUSACH", "Feudem_Talca", "InformanteUDA", "Feuv", "fel_chile", "Feuls13", "Feudem_Iqq", "Privmovilizadas", "FEUAI_vina", "Corfo", "feul_osorno", "EstafadosCorfo9", "PPDbiobio", "JuventudGuzman", "feudlaconce", "FeUpla", "Juventudes_PRI", "IgualesChile", "FedeumCurico", "FEUDLA_", "FundJaimeGuzman", "JuventudRebelde", "fedeumsanfer", "Juvenil_Iguales", "feufro", "FEUVStgo", "FundacionFEDES", "Feul_PtoMontt", "juventudCCS", "PPD_Chile", "FundacionPuente", "feummagallanes", "FundacionPortas", "PPDStgoCentro", "PDC_Chile", "Feubbchillan", "fundacionchile", "JuventudClaude", "FeudlaSomostods", "Feudla_VL", "JuventudPSUV", "EstafadosUDM", "EndeudadosCorfo", "fundacionLJ", "FEJ_Chile", "JuventudVolante", "FundTemplanza", "FedPalestina", "JvenesXBachelet", "FEUDD_STGO", "JuvProvidencia", "EstafadosporBan", "Juventud_PPD", "SecundariosJS", "SecundariosJJCC", "FeutfsmConceRbb", "Juventudhuastek", "FundacionAVINA", "FELsecundarios", "FundLasRosas", "JuventudenMarc", "JuventudLibrehn", "fundacionRAYUNd", "SecundariosJota", "JuventuDurango", "Fundacion_Tacal", "Juventud_UDI_30", "juventududivina", "FederacionUA", "Cores_PPD", "FEC_Chillan", "juventudecut", "FundacionDecide", "FEPUC", "JUVINDxMICHELLE", "FundacionFilba", "AC_Chile", "INJUVCHILE", "pdcprovidencia", "izqautonomauc")

# Listado de líderes
leaders <- c("camila_vallejo", "GiorgioJackson", "Sr_Ballesteros", "panchofigueroa", "velagrau", "AFielbaum", "gabrielboric", "NoamTitelman", "sebavielmas", "ANKALAO", "elperrotracio", "feliperasa", "RebecaGaeteS", "SDonoso_", "_marioalbertod", "danirslz", "PabloReyes_F", "sebafarfans", "marjoriecuello", "MoisesParedesR", "pepoglatz", "recarex", "PMillalen", "Pato_Contreras_", "jorbritoh", "SebaGodoyElguet", "goyarzun", "unavico", "j_miranda_s", "sebagamboa_", "vlatorre", "felipevargasr", "_isabelsalgado", "florespineda", "raecheco", "IriarteCORE", "acouble", "camiloriffo", "palta_mayo", "Godoy_JC", "pjgonzalez_", "leoiec", "NatalyEspin0za", "aalegred", "elRodrigoDuran", "Felipesalga", "guillermo_peter", "Javier_Fano", "intialavia", "IsmaelSoruco", "geo_sur", "FranciscoSainz", "FdaSandoval", "DanielaPobleteP", "Patricio_Indo", "krivera_uls", "ConstanzaLeyton", "BorisNegrete", "InzulzaAlberto", "Fco_Acunac", "OrleansRomero", "alexis_gonzlz", "pablo_fepucv", "d_cid", "Chriistopher_xD", "andresdouglasr", "Pipovaldebenito", "vargassasmay", "Carolina_Jarap", "DanaeDiazJeria", "Valeilic", "Javijadue", "EliasLonconado", "carlos_ruminott", "alvarobeckdorf", "Cris_Sarabia", "bernardo_barria", "jmiguelprieto", "YoxcyCampos", "Paonessa", "AngelitaJaviera", "carocatomas", "henry_varas", "Valessoncilla", "CrisLagos", "Mapa_Ruiz_", "germainquintana", "Franciscolabrav", "Coni_nogues", "pipe_higueras", "josefinaprivas", "Karolcariola", "_EloisaGonzalez")

# Dejamos solo los que nos interesan
for(i in orgs){
  tweetsUser <- tweets[(tweets$tweetuser == i) , ]
  for(j in 2010:2013)
  {
    tweetsYear <- selectByDate(tweetsUser, year = j)
    if(nrow(tweetsYear) > 0){
      tweetsYear$tweetuser <- NULL
      tweetsYear$date <- NULL
      write.csv(tweetsYear, paste0("../data/carolina/orgs/",i,"_",j,".txt"), row.names=FALSE)
    }
  }
}

# Dejamos solo los que nos interesan
for(i in leaders){
  tweetsUser <- tweets[(tweets$tweetuser == i) , ]
  for(j in 2010:2013)
  {
    tweetsYear <- selectByDate(tweetsUser, year = j)
    if(nrow(tweetsYear) > 0){
      tweetsYear$tweetuser <- NULL
      tweetsYear$date <- NULL
      write.csv(tweetsYear,paste0("../data/carolina/leaders/",i,"_",j,".txt"), row.names=FALSE)
    }
  }
}