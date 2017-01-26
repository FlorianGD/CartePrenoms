# Modifier les jeux de données

source("02_fonctions.R")

#### Données prénoms ####

prenoms_recalcules <- read_tsv("data/original/dpt2015.txt", locale = locale(encoding = "iso-8859-1")) %>%
  rename(prenom = preusuel, annee = annais, code_insee = dpt) %>% 
  mutate(sexe = factor(sexe, levels = c(1, 2), labels = c("M", "F")),
         annee = as.integer(annee)) %>% 
  filter(annee != "XXXX",
         code_insee != "97") %>%  
  modifier_historique_tous

write_csv(prenoms_recalcules, "data/recalc/prenoms_recalcules.csv")

naissances <- prenoms_recalcules %>%
  group_by(code_insee, annee) %>% 
  summarise(naissances = sum(nombre)) %>% 
  ungroup()

write_csv(naissances, "data/recalc/naissances.csv")

#### Données carte ####

france <- readOGR("data/original/departements", "departements-20140306-100m", 
                  stringsAsFactors = FALSE, 
                  use_iconv = TRUE, encoding = "iso-8859-1")

# Retirer les DOMs
france <- france[str_length(france$code_insee) == 2, ]

# Créer un champ département qui concatène le nom et le code
france$nom_dept <- str_c(france$nom, france$code_insee, sep = ", ")

# écriture des données
writeOGR(france, "data/recalc/departements", "departements-20140306-100m",
         "ESRI Shapefile")

