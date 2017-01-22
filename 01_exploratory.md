# Carte de France des prénoms
Florian Gaudin-Delrieu  
20 janvier 2017  

## Récupération des données

Les données sont en accès libre sur la plateforme [open data du gouvernement français](https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/).
Les données sont issues d'OpenStreetMap, et sont donc "© les contributeurs d'OpenStreetMap sous licence ODbL".

Les données sur les prénoms sont aussi en open source, [fournies par l'INSEE ](http://www.data.gouv.fr/fr/datasets/fichier-des-prenoms-edition-2016/).


```r
library(sp)
library(rgdal)
```

```
## rgdal: version: 1.2-5, (SVN revision 648)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 2.1.2, released 2016/10/24
##  Path to GDAL shared files: /usr/local/share/epsg_csv
##  Loaded PROJ.4 runtime: Rel. 4.9.1, 04 March 2015, [PJ_VERSION: 491]
##  Path to PROJ.4 shared files: (autodetected)
##  Linking to sp version: 1.2-3
```

```r
library(tmap)
library(raster)
library(stringr)
library(tidyverse)
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
head(france@data)
```

```
##   code_insee                     nom nuts3                  wikipedia
## 0         01                     Ain FR711       fr:Ain (département)
## 1         02                   Aisne FR221     fr:Aisne (département)
## 2         03                  Allier FR721    fr:Allier (département)
## 3         04 Alpes-de-Haute-Provence FR821 fr:Alpes-de-Haute-Provence
## 4         05            Hautes-Alpes FR822            fr:Hautes-Alpes
## 5         06         Alpes-Maritimes FR823         fr:Alpes-Maritimes
```

```r
str(france@data)
```

```
## 'data.frame':	101 obs. of  4 variables:
##  $ code_insee: chr  "01" "02" "03" "04" ...
##  $ nom       : chr  "Ain" "Aisne" "Allier" "Alpes-de-Haute-Provence" ...
##  $ nuts3     : chr  "FR711" "FR221" "FR721" "FR821" ...
##  $ wikipedia : chr  "fr:Ain (département)" "fr:Aisne (département)" "fr:Allier (département)" "fr:Alpes-de-Haute-Provence" ...
```
*Note :* Pour ne plus avoir le message d'erreur `NOTE: rgdal::checkCRSArgs: no proj_defs.dat in PROJ.4 shared files`, j'ai fait comme indiqué à [cette adresse](https://gis.stackexchange.com/questions/224467/raster-error-note-rgdalcheckcrsargs-no-proj-defs-dat-in-proj-4-shared-file)'

Essayons un premier graphique à l'aide de `tmap`


```r
tm_shape(france) +
  tm_borders()
```

![](01_exploratory_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Avec, les départements d'outre-mer (DOM) la carte est illisible. En première approche, nous allons ignorer ces DOM. Pour les exclure, nous allons utiliser le fait que les départements de France métropolitaine sont codés sur 2 chiffres alors que les DOM sont codés sur 3 chiffres.

```r
france <- france[str_length(france$code_insee) == 2, ]
france
```

```
## class       : SpatialPolygonsDataFrame 
## features    : 96 
## extent      : -5.142238, 9.560364, 41.33323, 51.08984  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## variables   : 4
## names       : code_insee,      nom, nuts3,            wikipedia 
## min values  :         01,      Ain, FR101, fr:Ain (département) 
## max values  :         95, Yvelines, FR832,          fr:Yvelines
```

```r
tm_shape(france) + tm_borders()
```

![](01_exploratory_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Lisons maintenant les données des prénoms


```r
prenoms <- read_tsv("dpt2015.txt")
```

```
## Parsed with column specification:
## cols(
##   sexe = col_integer(),
##   preusuel = col_character(),
##   annais = col_character(),
##   dpt = col_character(),
##   nombre = col_double()
## )
```

```r
head(prenoms)
```

```
## # A tibble: 6 × 5
##    sexe preusuel annais   dpt nombre
##   <int>    <chr>  <chr> <chr>  <dbl>
## 1     1        A   XXXX    XX     27
## 2     1    AADEL   XXXX    XX     53
## 3     1    AADIL   1983    84      3
## 4     1    AADIL   1992    92      3
## 5     1    AADIL   XXXX    XX    162
## 6     1   AAKASH   XXXX    XX     24
```

Les données manquantes apparaissent comme `"XXXX"` pour les années de naissance, et `"XX"` pour les départements.


```r
table(prenoms$dpt == "XX") # Moins de 1% des départements sont manquants
```

```
## 
##   FALSE    TRUE 
## 3372275   33036
```

```r
table(prenoms$annais == "XXXX") # Il y a exactement le même nombre d'années de naissance manquantes
```

```
## 
##   FALSE    TRUE 
## 3372275   33036
```

```r
filter(prenoms, dpt == "XX", annais != "XXXX") # Toutes les données manquantes sont sur les mêmes lignes
```

```
## # A tibble: 0 × 5
## # ... with 5 variables: sexe <int>, preusuel <chr>, annais <chr>,
## #   dpt <chr>, nombre <dbl>
```
Les données manquantes étant sur les mêmes lignes, nous allons les supprimer, puisque nous ne pouvons pas les placer par département. Nous allons renommer les variables pour des noms moins obscurs, et nous changeons le département en code_insee, pour pouvoir joindre les données avec la carte plus facilement.


```r
prenoms <- prenoms %>% 
  rename(prenom = preusuel, annee = annais, code_insee = dpt) %>% 
  mutate(sexe = factor(sexe, levels = c(1, 2), labels = c("M", "F"))) %>% 
  filter(annee != "XXXX")
head(prenoms)
```

```
## # A tibble: 6 × 5
##     sexe prenom annee code_insee nombre
##   <fctr>  <chr> <chr>      <chr>  <dbl>
## 1      M  AADIL  1983         84      3
## 2      M  AADIL  1992         92      3
## 3      M  AARON  1962         75      3
## 4      M  AARON  1982         75      3
## 5      M  AARON  1984         75      3
## 6      M  AARON  1985         75      4
```

## Carte pour un prénom, toutes années confondues

Essayons de résumer les données pour un prénom (par exemple `"Florian"`), toutes années confondues

```r
florian <- prenoms %>% 
  filter(prenom == "FLORIAN") %>% 
  group_by(code_insee) %>% 
  summarise(total = sum(nombre))
```


```r
tm_shape(sp::merge(france, florian)) +
  tm_borders() +
  tm_fill(col = "total")
```

![](01_exploratory_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Il y a de nombreux Florian dans le Nord, intéressant ! La Corse est manquante, cela doit venir du fait que les départements sont 2A et 2B, et doivent être marqués comme 20 dans un des 2 jeux de données, ce qui fait que la jointure ne marche pas.

```r
setdiff(unique(france$code_insee), unique(prenoms$code_insee)) # Il n'y a bien que la Corse qui est dans france
```

```
## [1] "2A" "2B"
```

