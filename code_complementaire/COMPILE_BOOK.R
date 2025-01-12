# Ce script peut être utilisé pour automatiser une partie importante de la mise
# en production du livre sous quarto. Il réalise notamment les étapes suivantes : 

# 1) recompresser les fichiers .rda pour assurer une taille minimale de ces derniers
# 2) vérifier la taille de tous les fichiers du livre pour respecter la contraintes 
# de github de 50 Mib
# 3) générer l'ensemble des fichiers pour la publication html en compilant le livre


rm(list = ls())

# Ne oublier de changer l'auteur avant de lancer le program "PA" ou "JG"
#author <- "PA"
author <- "JG"

#________________________________________________________________________________
# DEFINITION DES CHEMINS VERS LES DEUX DOSSIERS PRINCIPAUX
if(author == "PA"){
  email <- "philippe.apparicio@usherbrooke.ca"
  username <- "appariciop"
  book_dir <- 'C:/_____QUATRO2/QUARTO/AnalysesSpatialeDansR'
}else{
  email <- "gelbjeremy22@gmail.com"
  username <- "JeremyGelb"
  book_dir <- 'E:/Livres/AnalyseSpatiale_Quarto/MethodesAnalyseSpatiale'
}

setwd(book_dir)


#________________________________________________________________________________
# Etape 0 : vérifier l'installation des packages nécessaires

library(renv)
deps <- dependencies()
all_pkgs <- unique(deps$Package)
missing_pkg <- setdiff(all_pkgs, rownames(installed.packages()))

print("INSTALLING THE FOLLOWING MISSING PACKAGES : ")
print(missing_pkg)

install.packages(missing_pkg)  

#________________________________________________________________________________
# Etape 1 : compresser chacun des fichiers .rda
paths <- list.files('data', pattern = '*.rda', full.names = TRUE, recursive = TRUE)
tools::resaveRdaFiles(paths = paths, compress = "auto")

#________________________________________________________________________________
# Etape 2 Vérifier si aucun fichier ne fait plus que 50MO dans tout le livre
library(fs)
max_github_size <- fs::as_fs_bytes('50Mib')
all_files <- list.files(getwd(), full.names = TRUE, recursive = TRUE)
sizes <- sapply(all_files, file_size)

pb_files <- sizes[sizes >= max_github_size]

if(length(pb_files) > 0){
  stop('You have files that are too big for github!')
  print(names(pb_files))
}else{
  print('all the files have a good size for github!')
}


#________________________________________________________________________________
# Etape 3 Effectuer le rendering du quarto
# library(rstudioapi)
# rstudioapi::terminalExecute("quarto preview --render html --no-watch-inputs --no-browse")
library(quarto)
quarto_render()

#________________________________________________________________________________
# Etape 4 REVérifier si aucun fichier ne fait plus que 50MO dans tout le livre après compilation
library(fs)
max_github_size <- fs::as_fs_bytes('50Mib')
all_files <- list.files(getwd(), full.names = TRUE, recursive = TRUE)
sizes <- sapply(all_files, file_size)

pb_files <- sizes[sizes >= max_github_size]

if(length(pb_files) > 0){
  stop('You have files that are too big for github!')
  print(names(pb_files))
}else{
  print('all the files have a good size for github!')
}

#________________________________________________________________________________
# Etape 5 git commit et push

# attention, je recommande de faire cet étape manuellement avec le logiciel desktop github!

#------------------------------------------
# STAGE AND COMMIT ALL CHANGE IN GIT
#------------------------------------------
message <- paste0("Upload of the book the: ",Sys.Date(),", by: ",author)
shell(paste0('cd "',book_dir,'" & git add -A & git commit -m "',message,'"'))

#------------------------------------------
# PUSH IT ONLINE
#------------------------------------------
shell(paste0('cd "',book_dir,'" & D: & git push origin'))

