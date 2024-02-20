# Méthodes d’analyse spatiale&nbsp;: un grand bol d’R

Apparicio Philippe et Jérémy Gelb (2024). _Méthodes d’analyses spatiales&nbsp;: un grand bol d’R. Université de Sherbrooke_. fabriqueREL. Licence CC BY-SA.

Résumé&nbsp;: Ce livre vise à décrire une panoplie de méthodes d'analyse spatiale avec le logiciel ouvert R. Le contenu est pensé pour être accessible à tous et toutes, même à ceux et celles n'ayant presque aucune base en analyse de données spatiales ou en statistiques. La philosophie de ce livre est de donner toutes les clefs de compréhension et de mise en œuvre des méthodes abordées dans R. La présentation des méthodes est basée sur une approche compréhensive et intuitive plutôt que mathématique, sans pour autant que la rigueur statistique ne soit négligée. Bonne lecture!

Le livre a d'ailleurs été écrit intégralement dans R avec [Quatro](https://quarto.org/).

## Structure du livre

Le livre est organisé autour de cinq grandes parties.

**Partie 1. Données spatiales et R** 

Dans cette première partie, nous voyons comment importer, manipuler, cartographier et exporter des données spatiales dans R, principalement avec les *packages* `sf` pour les données vectorielles, `terra` pour les données matricielles (images) et `tmap` pour la cartographie (chapitre&nbsp;1). Maîtriser les notions abordées dans ce chapitre constitue une étape préalable et indispensable à tout projet d'analyse spatiale. D'une part, avant d'analyser des données spatiales, il convient de les structurer (importation et manipulation) et de les explorer (cartographie). D'autre part, une fois la ou les méthodes d'analyse spatiale mises en œuvre, il convient de cartographier les résultats finaux et de les exporter au besoin dans un format de données géographiques (shapefile (`shp`), GeoPackage (`GPKG`), GeoJSON (`geojson`), sqlite (`sqlite`), GeoTiff, etc).

**Partie 2. Autocorrélation spatiale** 

L'autocorrélation spatiale est une notion fondamentale en analyse spatiale permettant de vérifier si les entités proches ou voisines ont tendance à être (dis)semblables en fonction d'un phénomène donné. Dans le chapitre 2, nous décrivons une panoplie de méthodes permettant de définir des matrices de pondération spatiale qui sont utilisées pour évaluer la dépendance spatiale d'une ou plusieurs variables, soit les mesures d'autocorrélation spatiale globale et locale. La compréhension de la notion de dépendance spatiale et les différentes mesures d'autocorrélation spatiale est primordiale puisqu'elles sont largement mobilisées dans d'autres méthodes d'analyses spatiales décrites dans les chapitres suivants, notamment les régressions spatiales (chapitre&nbsp;6), les méthodes de classification spatiale (chapitre&nbsp;7) et d'interpolation spatiale (chapitre 8).

**Partie 3. Méthodes de répartition ponctuelle et de détections d'agrégats spatiaux** 

Cette troisième partie comprend deux chapitres. Dans le chapitre&nbsp;3, nous abordons les principales méthodes qui permettent de décrire un semis de points dans un espace géographique donnée&nbsp;: fréquence et densité des points, analyse centrographique, arrangement spatial du semis de points (méthode du plus proche voisin, méthode des quadrats), cartographie de la densité (notamment l'estimation de la densité par noyau). Dans le chapitre&nbsp;4, nous abordons deux familles de méthodes de détection d'agrégats spatiaux et spatio-temporels qui s'appliquent à des géométries différentes&nbsp;: les méthodes de classification basées sur la densité des points (couche de points), principalement les algorithmes DBSCAN et ST-DBSCAN, et les méthodes de balayage de Kulldorff (couche de polygones).

**Partie 4. Analyses de données avec des réseaux de transport** 

Cette quatrième partie comprend aussi deux chapitres. Dans le chapitre&nbsp;5, nous voyons comment construire un réseau multimode (voiture, marche, vélo, transport en commun) afin de calculer différentes mesures d'accessibilité spatiale dans R avec le *package* `R5R`. Dans le chapitre&nbsp;6, nous décrivons plusieurs méthodes permettant de décrire la distribution spatiale d'évènements localisés sur un réseau de rues avec le *package* `spnetwork`.

**Partie 5. Méthodes de régression spatiale et de classification spatiale**

Cette cinquième et dernière partie comprend deux chapitres. Le chapitre&nbsp;7 est consacré à plusieurs méthodes de régression intégrant l'espace&nbsp;: modèles d'économétrie spatiale, modèles généralisés additifs (GAM) avec une spline bivariée sur les coordonnées géographiques, régressions géographiquement pondérées. Dans le chapitre 8, nous décrivons des méthodes permettant de regrouper des entités spatiales d'une région en plusieurs classes en fonction de leurs caractéristiques évaluées à partir de plusieurs variables. Nous distinguons deux types de méthodes de classification non supervisée spatiale&nbsp;: les méthodes de classification non supervisée avec contrainte spatiale visant à regrouper des entités spatiales en plusieurs régions avec une absence de mitage (algorithmes AZP, SKATER, REDCAP, etc.); les méthodes de classification non supervisée avec une dimension spatiale (méthode *ClustGeo* et classification floue c-means spatiale).
