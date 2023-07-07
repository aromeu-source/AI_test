# -----------------------------------------------------------------------------
# CHATpros.R: CC (BY-NC-ND 4.0). 
# Andrés Romeu
# Departamento de Fundamentos del Análisis Económico
# -----------------------------------------------------------------------------
require(httr)
require(stringr)
# ask_chatgpt =================================================================
# Descripció: Crida el ChatGPT API amb la pregunta i retorna la resposta
#
# Ús:
#     ask_chatgpt( prompt , APIkey )
#     
# Arguments:
#    prompt: Cadena amb la pregunta.
#
#    APIkey: Cadena amb la clau API del perfil de l'usuari. Visiteu 
#            http://https://platform.openai.com/account/api-keys per obtenir-ne
#            la del perfil de l'usuari.
#
# Retorn: Una cadena amb la resposta.
# -----------------------------------------------------------------------------

ask_chatgpt <- function(prompt,APIkey) {

  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", APIkey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}
# make_question ===============================================================
# Descripció: Llig una pregunta d'una bateria pre-formatada per l'usuari i
#             retorna la resposta de chatGPT, i compara amb la resposta 
#             correcta.
# 
# Ús:
#       make_question(bateria=pregs,tema=0,numero=0,seqnum,APIkey)
#     
# Arguments:
#
#   bateria: Data frame amb la bateria formatada
#    indice: Llista: primer element (char) examen; segon element num.preg.
#    APIkey: Cadena amb la clau API del perfil de l'usuari. Visiteu 
#            http://https://platform.openai.com/account/api-keys per obtenir-ne
#            la del vostre perfil.
#   pretext: Missatge opcional que anteposar a la pregunta. Normalment, demanem
#            a chatPGT que trie una opció sense donar explicacions detallades.
#
# Retorn: Una llista amb el següents elements:
#             tema: Escalar
#           numero: Escalar
#       true_reply: Character, opció correcta ("(a)" o "(b)")
#        GPT_reply: Character, opció donada per chatPGT ("(a)" o "(b)")
#           encert: Boolean.
#            temps: Temps de resposta en segons.
# -----------------------------------------------------------------------------

make_question <- function( bateria, indice=NULL, APIkey, pretext=NULL) {

    opcions = c("(a)","(b)")
    prn <- which(bateria$examen == indice[[1]] & bateria$numseq==indice[[2]])
    pregunta <- paste( pretext,
                       bateria$clause[prn],": ",
                       bateria$opa[prn]," ",
                       bateria$opb[prn],sep="")

    t1 <- Sys.time()
    GPT_reply <- str_extract(ask_chatgpt(pregunta,APIkey),"\\([a-b]\\)")
    t2 <- Sys.time()
    true_reply <- pregs[prn,c("true_a","true_b")]
    true_reply <- opcions[ c(true_reply[[1]],true_reply[[2]]) ]
    encert <- (GPT_reply==true_reply)

    cat("Iteració:",indice[[2]],"...Resp.correcta:",true_reply,
        "...Resp.GPT:",GPT_reply,"...Ok =",encert,"\n")
    return(list( "examen"=indice[[1]],
                 "numero"=indice[[2]],
                 "true_reply"=true_reply,
                 "GPT_reply"=GPT_reply,
                 "encert" = encert,
                 "temps" = t2-t1) )
}


