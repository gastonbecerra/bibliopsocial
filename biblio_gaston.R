library(ojsr)
library(tidyverse)
library(googlesheets4)
library(gt)
library(gtExtras)
library(stringr)
library(widyr)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(viridisLite)

gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1Vn0MYaZwBshZLEeZCj67Jo4uWvZawI0emsd-26ru_0U/edit?usp=sharing"
data <- googlesheets4::read_sheet(sheet_url)

names(data)

articles <- data %>%
  select(input_url = `input_url`, 
         YEAR = `DC.Date.created`, 
         lang = `DC.Language`, 
         tipo = `DC.Type.articleType`, 
         author = `DC.Creator.PersonalName`, 
         description = `DC.Description`, 
         title = `citation_title`, 
         keywords = `keywords`,
         institution = `citation_author_institution`) %>%
  mutate(YEAR = lubridate::year(YEAR) ) %>%
  filter(tipo != "Introduction")

table(articles$institution) %>% length()
glimpse(articles)

source('equivalencias.R')

articles <- articles %>%
  separate_rows(institution, sep = "[,;]") %>%
  mutate(institution = str_trim(institution)) %>%
  mutate(institution = ifelse(institution %in% names(equivalencias), 
                              equivalencias[institution], institution)) %>%
  group_by(input_url, YEAR, lang, tipo, author, description, title, keywords) %>%
  summarise(institution = paste(unique(institution), collapse = "; ")) %>%
  ungroup()

table(articles$institution) %>% length()
glimpse(articles)


# tabla sintetica -------------------


articles %>%
  group_by(YEAR) %>%
  summarise(
    articulos_publicados = n(),
    autores_unicos = n_distinct(unlist(strsplit(author, ", "))),
    instituciones_unicas = n_distinct(unlist(strsplit(institution, ", "))),
    idiomas_es = sum(lang == "es") / n() * 100,
    idiomas_en = sum(lang == "en") / n() * 100,
    idiomas_pt = sum(lang %in% c("pt", "Portugues")) / n() * 100,
    tipo_articles = sum(tipo == "Articles") / n() * 100,
    tipo_dossier = sum(tipo == "Dossier") / n() * 100,
    idioma_proporciones = list(c(idiomas_es, idiomas_en, idiomas_pt)),
    tipo_proporciones = list(c(tipo_articles, tipo_dossier)),
    .groups = 'drop'
  ) %>%
  gt() %>%
  gt_plt_bar_stack(idioma_proporciones, width = 65,
                   labels = c("es", "en", "pt"),
                   palette = viridis(3, option = "viridis")) %>%
  gt_plt_bar_stack(tipo_proporciones, width = 65,
                   labels = c("Articles", "Dossier"),
                   palette = viridis(2, option = "viridis")) %>%
  cols_hide(columns = c(idiomas_es, idiomas_en, idiomas_pt, 
                        tipo_articles, tipo_dossier)) %>%
  cols_label(
    YEAR = "Año",
    articulos_publicados = "Artículos",
    autores_unicos = "Autores",
    instituciones_unicas = "Afiliaciones"
  )


# keywords -----------------------


keywords <- articles %>%
  separate_rows(keywords, sep = "[,;]") %>%
  mutate(keywords = str_trim(keywords)) %>%
  select(input_url, keywords) %>%
  pivot_longer(cols = -input_url, names_to = "xxx", values_to = "key") %>%
  select(-xxx) %>%
  mutate(key = tolower(key) %>% trimws()) %>%
  filter(!is.na(key)) %>%
  mutate(key = str_replace_all(key, "[^[:alnum:][:space:]]", "")) %>%
  mutate(key = chartr("áéíóú", "aeiou", key)) %>%
  group_by(key) %>% mutate(k = n()) %>% ungroup()

keywords %>%
  # filter(k>1) %>%
  widyr::pairwise_count(item = key, feature = input_url, sort = TRUE) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(keywords %>% count(key), by = c("name" = "key")) %>%
  create_layout(layout = "kk") %>%
  ggraph() +
  theme_graph() +
  geom_edge_link(aes(edge_alpha = n)) +
  geom_node_text(aes(label = str_to_title(name), size = n), repel = TRUE, max.overlaps = 50) +
  # geom_node_text(aes(label = str_to_title(name)), repel = TRUE, max.overlaps = 50) + 
  geom_node_point(aes(size = n)) +
  theme(legend.position = "none")


# autores ----------------------


authors <- articles %>%
  separate_rows(author, sep = "[,;]") %>%
  mutate(author = str_trim(author)) %>%
  select(input_url, author) %>%
  pivot_longer(cols = -input_url, names_to = "xxx", values_to = "author") %>%
  select(-xxx) %>%
  mutate(author = tolower(author) %>% trimws()) %>%
  filter(!is.na(author)) %>%
  mutate(author = str_replace_all(author, "[^[:alnum:][:space:]]", "")) %>%
  mutate(author = chartr("áéíóú", "aeiou", author)) %>%
  mutate(author = tools::toTitleCase(author)) %>%  # Capitalización adecuada
  group_by(author) %>% mutate(k = n()) %>% ungroup()

authors %>%
  widyr::pairwise_count(item = author, feature = input_url, sort = TRUE) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(authors %>% count(author), by = c("name" = "author")) %>%
  create_layout(layout = "kk") %>%
  ggraph() +
  theme_graph() +
  # geom_edge_link(aes(edge_alpha = n)) +
  # geom_node_text(aes(label = name, size = n), repel = TRUE) +
  # geom_node_point(aes(size = n)) +
  # theme(legend.position = "none")
  geom_edge_link(aes(edge_alpha = n, edge_width = n, color = n)) +  # Variar grosor y color de las colaboraciones
  geom_node_text(aes(label = name, size = n), repel = TRUE) +  # Tamaño de texto basado en el número de colaboraciones
  geom_node_point(aes(size = n), color = "darkblue") +  # Tamaño de los nodos basado en número de colaboraciones
  scale_edge_width(range = c(0.5, 3)) +  # Controlar el rango del grosor de los enlaces
  scale_edge_color_continuous(low = "lightblue", high = "darkblue") +  # Colorear los enlaces por intensidad
  theme(legend.position = "none")



# instituciones por año ---------------------


instituciones <- articles %>%
  separate_rows(institution, sep = "[,;]") %>%
  mutate(institution = str_trim(institution)) %>%
  select(input_url, YEAR, institution) %>%
  filter(!is.na(institution), institution != "NA", institution != "") %>%
  mutate(count=n(), .by = institution) %>%
  mutate(institution = ifelse(count > 2, institution, "Otros")) %>%
  pivot_longer(cols = c(-input_url,-YEAR,-count), names_to = "xxx", values_to = "institution") %>%
  select(-xxx)

instituciones %>%
  count(YEAR, institution) %>% 
  left_join(instituciones) %>%
  ggplot(aes(x=fct_reorder(institution, count, .desc = FALSE), y=as.factor(YEAR), fill = n)) + 
  geom_tile() +
  geom_text(aes(label=n), color="white", show.legend = FALSE, size=3) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

  