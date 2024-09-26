library(ojsr)
library(tidyverse)
library(googlesheets4)
library(gt)
library(gtExtras)

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
  mutate(YEAR = lubridate::year(YEAR) )
glimpse(articles)


# tabla sintetica -------------------

#2do: falta normalizar instituciones

articles %>%
  filter(tipo != "Introduction") %>%
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
                   palette = c("#ff4343", "#bfbfbf", "#0a1c2b")) %>%
  gt_plt_bar_stack(tipo_proporciones, width = 65,
                   labels = c("Articles", "Dossier"),
                   palette = c("#1f77b4", "#ff7f0e")) %>%
  cols_hide(columns = c(idiomas_es, idiomas_en, idiomas_pt, 
                        tipo_articles, tipo_dossier))


# keywords -----------------------


library(stringr)
library(widyr)
library(tidygraph)
library(ggraph)
library(ggplot2)

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
  geom_edge_link(aes(edge_alpha = n)) +
  geom_node_text(aes(label = name, size = n), repel = TRUE) +
  geom_node_point(aes(size = n)) +
  theme(legend.position = "none")
