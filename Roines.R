# =============================================================================
# ROINES.R: Selecciona preguntes errades de examens anterior i avalua.
# Suposem que pregs.Rdat i Resum.Rdat ja estan carregats en memória
# També axuiliars de CHATpros.R
# -----------------------------------------------------------------------------
# Sel·lecciona preguntes errades de examens anteriors
pregs2 <- dplyr::filter(pregs,Resum$encert)
# Claus per als diferents tractaments..........................................
keyA <- "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"
keyB <- "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"
# Nom de la sessió i clau: MODIFICAR ACÍ......................................
examen <- pregs2$examen
numseq <- pregs2$numseq
key <- keyA
# Text Introductori (igual per a totes les sessions)...........................
hello <- "Responde simplemente (a) o (b) sin dar explicaciones detalladas: "
# Número de preguntes per ronda................................................
npregs <- 45
# Crea data.frame inicial sobre el qual acumul·lar.............................
ExpDat <- data.frame(examen=NA,numero=NA,
                   true_reply=NA,GPT_reply=NA,encert=NA,temps=NA)
# Comença bucle................................................................
for ( i in 1:npregs ) {
    resultat <-  make_question(pregs2,  
                               indice=list(examen[i],numseq[i]), 
                               APIkey=key, 
                               pretext=hello)
    if (length(resultat$GPT_reply)==0) {
        resultat$GPT_reply <- NA
        resultat$encert <- NA
    }
    ExpDat <- rbind(ExpDat,resultat)
}
ExpDat <- ExpDat[-1,] 
ExpDat <- cbind(numord=seq(1,npregs),ExpDat)
