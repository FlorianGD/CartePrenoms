# Carte des prénoms
Florian Gaudin-Delrieu  

# Récupération des données

Les données sont en accès libre sur la plateforme [open data du gouvernement français](https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/).
Les données sont issues d'OpenStreetMap, et sont donc "© les contributeurs d'OpenStreetMap sous licence ODbL".

Les données sur les prénoms sont aussi en open source, [fournies par l'INSEE ](http://www.data.gouv.fr/fr/datasets/fichier-des-prenoms-edition-2016/).


```r
library(sp)
library(rgdal, warn.conflicts = FALSE)
```

```
## rgdal: version: 1.2-5, (SVN revision 648)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 2.1.2, released 2016/10/24
##  Path to GDAL shared files: 
##  Loaded PROJ.4 runtime: Rel. 4.9.1, 04 March 2015, [PJ_VERSION: 491]
##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/rgdal/proj
## WARNING: no proj_defs.dat in PROJ.4 shared files
##  Linking to sp version: 1.2-3
```

```r
library(tmap, warn.conflicts = FALSE)
library(raster)
library(stringr)
library(tidyverse, warn.conflicts = FALSE)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## extract(): tidyr, raster
## filter():  dplyr, stats
## lag():     dplyr, stats
## select():  dplyr, raster
```
 
 J'ai choisi de récupérer les données de 2014 simplifiées à 100m, qui sont moins lourdes à traiter que les données de 2017 non simplifiées.


```r
france <- readOGR("departements", "departements-20140306-100m", stringsAsFactors = FALSE, use_iconv = TRUE, encoding = "iso-8859-1")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "departements", layer: "departements-20140306-100m"
## with 101 features
## It has 4 fields
```

```
## NOTE: rgdal::checkCRSArgs: no proj_defs.dat in PROJ.4 shared files
```

```r
france
```

```
## class       : SpatialPolygonsDataFrame 
## features    : 101 
## extent      : -61.80976, 55.83665, -21.38973, 51.08984  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## variables   : 4
## names       : code_insee,      nom, nuts3,            wikipedia 
## min values  :         01,      Ain, FR101, fr:Ain (département) 
## max values  :        976, Yvelines, FR940,          fr:Yvelines
```

```r
france@data
```

```
##     code_insee                     nom nuts3                  wikipedia
## 0           01                     Ain FR711       fr:Ain (département)
## 1           02                   Aisne FR221     fr:Aisne (département)
## 2           03                  Allier FR721    fr:Allier (département)
## 3           04 Alpes-de-Haute-Provence FR821 fr:Alpes-de-Haute-Provence
## 4           05            Hautes-Alpes FR822            fr:Hautes-Alpes
## 5           06         Alpes-Maritimes FR823         fr:Alpes-Maritimes
## 6           07                 Ardèche FR712   fr:Ardèche (département)
## 7           08                Ardennes FR211  fr:Ardennes (département)
## 8           09                  Ariège FR621    fr:Ariège (département)
## 9           10                    Aube FR212      fr:Aube (département)
## 10          11                    Aude FR811      fr:Aude (département)
## 11          12                 Aveyron FR622   fr:Aveyron (département)
## 12          13        Bouches-du-Rhône FR823        fr:Bouches-du-Rhône
## 13          14                Calvados FR251  fr:Calvados (département)
## 14          15                  Cantal FR722    fr:Cantal (département)
## 15          16                Charente FR531  fr:Charente (département)
## 16          17       Charente-Maritime FR532       fr:Charente-Maritime
## 17          18                    Cher FR241      fr:Cher (département)
## 18          19                 Corrèze FR631   fr:Corrèze (département)
## 19          21               Côte-d'Or FR261               fr:Côte-d'Or
## 20          22           Côtes-d'Armor FR521           fr:Côtes-d'Armor
## 21          23                  Creuse FR632    fr:Creuse (département)
## 22          24                Dordogne FR611  fr:Dordogne (département)
## 23          25                   Doubs FR431     fr:Doubs (département)
## 24          26                   Drôme FR713     fr:Drôme (département)
## 25          27                    Eure FR231      fr:Eure (département)
## 26          28            Eure-et-Loir FR242            fr:Eure-et-Loir
## 27          29               Finistère FR522               fr:Finistère
## 28          2A            Corse-du-Sud FR831            fr:Corse-du-Sud
## 29          2B             Haute-Corse FR832             fr:Haute-Corse
## 30          30                    Gard FR812                    fr:Gard
## 31          31           Haute-Garonne FR623           fr:Haute-Garonne
## 32          32                    Gers FR624      fr:Gers (département)
## 33          33                 Gironde FR612   fr:Gironde (département)
## 34          34                 Hérault FR813   fr:Hérault (département)
## 35          35         Ille-et-Vilaine FR523         fr:Ille-et-Vilaine
## 36          36                   Indre FR243     fr:Indre (département)
## 37          37          Indre-et-Loire FR244          fr:Indre-et-Loire
## 38          38                   Isère FR714     fr:Isère (département)
## 39          39                    Jura FR432      fr:Jura (département)
## 40          40                  Landes FR613    fr:Landes (département)
## 41          41            Loir-et-Cher FR245            fr:Loir-et-Cher
## 42          42                   Loire FR715     fr:Loire (département)
## 43          43             Haute-Loire FR723             fr:Haute-Loire
## 44          44        Loire-Atlantique FR511        fr:Loire-Atlantique
## 45          45                  Loiret FR246    fr:Loiret (département)
## 46          46                     Lot FR625       fr:Lot (département)
## 47          47          Lot-et-Garonne FR614          fr:Lot-et-Garonne
## 48          48                  Lozère FR814    fr:Lozère (département)
## 49          49          Maine-et-Loire FR512          fr:Maine-et-Loire
## 50          50                  Manche FR252    fr:Manche (département)
## 51          51                   Marne FR213     fr:Marne (département)
## 52          52             Haute-Marne FR214             fr:Haute-Marne
## 53          53                 Mayenne FR513   fr:Mayenne (département)
## 54          54      Meurthe-et-Moselle FR411      fr:Meurthe-et-Moselle
## 55          55                   Meuse FR412     fr:Meuse (département)
## 56          56                Morbihan FR524                fr:Morbihan
## 57          57                 Moselle FR413   fr:Moselle (département)
## 58          58                  Nièvre FR262    fr:Nièvre (département)
## 59          59                    Nord FR301      fr:Nord (département)
## 60          60                    Oise FR222      fr:Oise (département)
## 61          61                    Orne FR253      fr:Orne (département)
## 62          62           Pas-de-Calais FR302           fr:Pas-de-Calais
## 63          63             Puy-de-Dôme FR724             fr:Puy-de-Dôme
## 64          64    Pyrénées-Atlantiques FR615    fr:Pyrénées-Atlantiques
## 65          65         Hautes-Pyrénées FR626         fr:Hautes-Pyrénées
## 66          66     Pyrénées-Orientales FR815     fr:Pyrénées-Orientales
## 67          67                Bas-Rhin FR421                fr:Bas-Rhin
## 68          68               Haut-Rhin FR422               fr:Haut-Rhin
## 69          69                   Rhône FR717     fr:Rhône (département)
## 70          70             Haute-Saône FR433             fr:Haute-Saône
## 71          71          Saône-et-Loire FR263          fr:Saône-et-Loire
## 72          72                  Sarthe FR514    fr:Sarthe (département)
## 73          73                  Savoie FR718    fr:Savoie (département)
## 74          74            Haute-Savoie FR716            fr:Haute-Savoie
## 75          75                   Paris FR101                   fr:Paris
## 76          76          Seine-Maritime FR232          fr:Seine-Maritime
## 77          77          Seine-et-Marne FR102          fr:Seine-et-Marne
## 78          78                Yvelines FR103                fr:Yvelines
## 79          79             Deux-Sèvres FR533             fr:Deux-Sèvres
## 80          80                   Somme FR223     fr:Somme (département)
## 81          81                    Tarn FR627      fr:Tarn (département)
## 82          82         Tarn-et-Garonne FR628         fr:Tarn-et-Garonne
## 83          83                     Var FR825       fr:Var (département)
## 84          84                Vaucluse FR826  fr:Vaucluse (département)
## 85          85                  Vendée FR515    fr:Vendée (département)
## 86          86                  Vienne FR534    fr:Vienne (département)
## 87          87            Haute-Vienne FR633            fr:Haute-Vienne
## 88          88                  Vosges FR414    fr:Vosges (département)
## 89          89                   Yonne FR264     fr:Yonne (département)
## 90          90   Territoire-de-Belfort FR434   fr:Territoire de Belfort
## 91          91                 Essonne FR104   fr:Essonne (département)
## 92          92          Hauts-de-Seine FR105          fr:Hauts-de-Seine
## 93          93       Seine-Saint-Denis FR106       fr:Seine-Saint-Denis
## 94          94            Val-de-Marne FR107            fr:Val-de-Marne
## 95          95              Val-d'Oise FR108              fr:Val-d'Oise
## 96         971              Guadeloupe FR910              fr:Guadeloupe
## 97         972              Martinique FR920              fr:Martinique
## 98         973                  Guyane FR930                  fr:Guyane
## 99         974              La Réunion FR940              fr:La Réunion
## 100        976                 Mayotte  <NA>                 fr:Mayotte
```
