rm(list = ls())


tbl_caderneta_coletiva <- readRDS(file = "./database/CADERNETA_COLETIVA_2007.rds")

# User defined Function ---------------------------------------------------

fn_Urbano <- function(UF, ext){
  Urbano <- as.logical(NA)
  
  if(UF == 11){
    if(ext %in% c(7:11)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 12) {
    if(ext %in% c(3,4)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 13) {
    if(ext %in% c(9:13)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 14) {
    if(ext %in% c(3,4)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 15) {
    if(ext %in% c(9:14)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 16) {
    if(ext %in% c(4:6)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 17) {
    if(ext %in% c(6:10)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 21) {
    if(ext %in% c(13:24)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 22) {
    if(ext %in% c(10:19)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 23) {
    if(ext %in% c(24:36)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 24) {
    if(ext %in% c(9:13)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 25) {
    if(ext %in% c(10:16)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 26) {
    if(ext %in% c(16:25)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 27) {
    if(ext %in% c(9:13)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 28) {
    if(ext %in% c(8:9)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 29) {
    if(ext %in% c(22:36)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 31) {
    if(ext %in% c(28:45)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 32) {
    if(ext %in% c(10:14)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 33) {
    if(ext %in% c(31:37)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 35) {
    if(ext %in% c(31:51)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 41) {
    if(ext %in% c(19:29)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 42) {
    if(ext %in% c(14:23)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 43) {
    if(ext %in% c(19:30)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 50) {
    if(ext %in% c(9:13)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 51) {
    if(ext %in% c(11:18)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 52) {
    if(ext %in% c(18:28)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else if(UF == 53) {
    if(ext %in% c(8:9)) {
      Urbano <- FALSE
    } else{
      Urbano <- TRUE
    }
  } else {
    stop("UF nao mapeada")
  }
  
  return(Urbano)  
}

tbl_caderneta_coletiva$Urbano <- NA
for(i in seq_len(nrow(tbl_caderneta_coletiva))){
  if ((i %% 1000) == 0){
    cat(i, "\n")
  }
  
  tbl_caderneta_coletiva$Urbano[i] <- fn_Urbano(tbl_caderneta_coletiva$cod_uf[i], tbl_caderneta_coletiva$num_ext_renda[i])  
}

unique(tbl_caderneta_coletiva$Urbano)


saveRDS(object = tbl_caderneta_coletiva,
        file = "./database/CADERNETA_COLETIVA_2007.rds")


