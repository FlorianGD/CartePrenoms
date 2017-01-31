# Modifier les jeux de données

source("02_fonctions.R")

#### Données prénoms ####

### Fonctions pour créer le jeu de données modifié
modifier_historique_prenoms <- function(data, code_avant, annee_limite, props){
  # Rajoute des valeurs pour des départements non présents avant une certaine annee
  # Renvoie le jeu de données data modifié
  # data doit contenir des colonnes `annee` (numeric), `code_insee` (chr),
  # `prenom` (chr), `sexe` (factor M/F)  
  # code_avant  est un vecteur de character de taille 1
  # props est un tibble `code_insee`, `prop`   
  
  codes_apres <- props$code_insee
  
  avant <- data %>% 
    filter(code_insee == code_avant,
           annee < annee_limite) %>% 
    complete(nesting(annee, prenom, sexe), code_insee = codes_apres) %>% 
    fill(nombre) %>% 
    inner_join(props, by = "code_insee") %>%
    mutate(nombre = prop * nombre) %>%
    select(-prop)
  
  apres <- data%>% 
    filter(annee >= annee_limite,
           code_insee %in% codes_apres)
  
  data %>% 
    anti_join(props, by = "code_insee") %>% 
    bind_rows(avant, apres) %>% 
    arrange(annee, code_insee, sexe, prenom)
}

modifier_historique_oise <- function(data){
  # modifer historique appliqué aux départements 78, 91 et 95
  # Les proportions viennent de la population entre 1968 et 1998 (simplifiées)  
  
  prop <-tibble("code_insee" = c("78", "91", "95"),
                "prop" = c(0.4, 0.3, 0.3))
  
  modifier_historique_prenoms(data, code_avant = "78",
                              annee_limite = 1968,
                              props = prop)
}  

modifier_historique_seine <- function(data){
  # modifer historique appliqué aux départements 75, 92, 93 et 94
  # Les proportions viennent de la population après 1968 (simplifiées)   
  
  prop <- tibble("code_insee" = c("75", "92", "93", "94"),
                 "prop" = c(0.4, 0.2, 0.2, 0.2))
  
  modifier_historique_prenoms(data, code_avant = "75",
                              annee_limite = 1968,
                              props = prop)
}

modifier_historique_corse <- function(data){
  # modifer historique appliqué aux départements 2A et 2B
  # Il faut remplacer le département 20 par 2A et 2B (moitié-moitié)
  
  prop <- tibble("code_insee" = c("20", "2A", "2B"),
                 "prop" = c(0, 0.5, 0.5))
  
  modifier_historique_prenoms(data, code_avant = "20",
                              annee_limite = 9999,
                              props = prop) %>% 
    filter(nombre != 0)
}

modifier_historique_tous <- function(data) {
  data %>% 
    modifier_historique_corse() %>% 
    modifier_historique_oise() %>% 
    modifier_historique_seine()
}

### Modifier le jeu de données
prenoms_recalcules <- read_tsv("data/original/dpt2015.txt", locale = locale(encoding = "iso-8859-1")) %>%
  rename(prenom = preusuel, annee = annais, code_insee = dpt) %>% 
  mutate(sexe = factor(sexe, levels = c(1, 2), labels = c("M", "F")),
         annee = as.integer(annee)) %>% 
  filter(annee != "XXXX",
         code_insee != "97") %>%  
  modifier_historique_tous

write_csv(prenoms_recalcules, "data/recalc/prenoms_recalcules.csv")
saveRDS(prenoms_recalcules, "data/recalc/prenoms.rds")

naissances <- prenoms_recalcules %>%
  group_by(code_insee, annee) %>% 
  summarise(naissances = sum(nombre)) %>% 
  ungroup()

write_csv(naissances, "data/recalc/naissances.csv")
saveRDS(naissances, "data/recalc/naissances.rds")

#### Données carte ####

france <- readOGR("data/original/departements", "departements-20140306-100m", 
                  stringsAsFactors = FALSE, 
                  use_iconv = TRUE, encoding = "iso-8859-1")

# Retirer les DOMs
france <- france[str_length(france$code_insee) == 2, ]

# Créer un champ département qui concatène le nom et le code
france$nom_dept <- str_c(france$nom, " (", france$code_insee, ")")

# écriture des données
writeOGR(france, "data/recalc/departements", "departements-20140306-100m",
         "ESRI Shapefile")

