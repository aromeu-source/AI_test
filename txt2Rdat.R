# Aquest programa per a convertir fitxers en ASCII TXT amb les preguntes de...
# les bateries a un format que R puga llegir de valors separats per la barra..
# vertical. No podem utilitzar comma separated values perquè hi ha variables..
# que són cadenes.............................................................
# (c) A.Romeu 2023............................................................
library(stringr)
pregs <- read.csv(file="examenes1923.txt",header=TRUE,sep="|")
pregs$true_a <- str_detect(pregs$opa,regex("\\(a\\)[*]"))
pregs$true_b <- str_detect(pregs$opb,regex("\\(b\\)[*]"))
pregs$numseq <- rep(1:30,9)
save(pregs,file="examenes1923.Rdat")                    
