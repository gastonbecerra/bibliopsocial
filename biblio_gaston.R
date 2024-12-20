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
library(svglite)

# borrador https://docs.google.com/document/d/1qUtIZv4qiGdOFx3BMQ8iIjKpgbeRL7ZsSfjLxxi3Sig/edit#heading=h.ij3dr2225ayk


gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1Vn0MYaZwBshZLEeZCj67Jo4uWvZawI0emsd-26ru_0U/edit?usp=sharing"
data <- googlesheets4::read_sheet(sheet_url)

names(data)

articles <- data %>%
  select(input_url = `input_url`, 
         year = `citation_date`, 
         lang = `DC.Language`, 
         tipo = `DC.Type.articleType`, 
         author = `DC.Creator.PersonalName`, 
         description = `DC.Description`, 
         title = `citation_title`, 
         keywords = `keywords`,
         institution = `citation_author_institution`) %>%
  # filter(tipo != "Introduction") %>%
  mutate(
    year = if_else(
      str_length(year) == 4, 
      as.numeric(year), # Si es solo el año, lo convertimos directamente
      year(ymd(year))   # Si es una fecha completa, extraemos el año con lubridate
    )
  )

table(articles$institution) %>% length()
glimpse(articles)

source('equivalencias.R')

articles <- articles %>%
  separate_rows(institution, sep = "[,;]") %>%
  mutate(institution = str_trim(institution)) %>%
  mutate(institution = ifelse(institution %in% names(equivalencias), 
                              equivalencias[institution], institution)) %>%
  group_by(input_url, year, lang, tipo, author, description, title, keywords) %>%
  summarise(institution = paste(unique(institution), collapse = "; ")) %>%
  ungroup()

table(articles$institution) %>% length()
glimpse(articles)


# tabla sintetica -------------------


articles %>%
  group_by(year) %>%
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
                   labels = c("es", "en", "otro"),
                   palette = viridis::viridis(3, option = "viridis")) %>%
  gt_plt_bar_stack(tipo_proporciones, width = 65,
                   labels = c("Articles", "Dossier"),
                   palette = viridis::viridis(2, option = "viridis")) %>%
  cols_hide(columns = c(idiomas_es, idiomas_en, idiomas_pt, 
                        tipo_articles, tipo_dossier)) %>%
  cols_label(
    year = "Año",
    articulos_publicados = "Artículos",
    autores_unicos = "Autores",
    instituciones_unicas = "Afiliaciones",
    idioma_proporciones = "Idiomas (% es, en, otro)",
    tipo_proporciones = "Tipos (% articulo, dossier)"
  )


tabla_autores_por_paper <- articles %>%
  mutate(
    num_authors = lengths(strsplit(author, ","))
  ) %>%
  group_by(year, num_authors) %>%
  summarise(papers = n(), .groups = "drop")

tabla_autores_por_paper

indice_colaboracion <- articles %>%
  mutate(
    num_authors = lengths(strsplit(author, ","))
  ) %>%
  group_by(year) %>%
  summarise(
    promedio_autores = mean(num_authors),
    .groups = "drop"
  )

indice_colaboracion

datos_grafico <- articles %>%
  mutate(
    num_authors = lengths(strsplit(author, ","))
  ) %>%
  group_by(year, num_authors) %>%
  summarise(papers = n(), .groups = "drop") %>%
  group_by(year) %>%
  summarise(
    total_papers = sum(papers),
    indice_colaboracion = sum(num_authors * papers) / sum(papers),
    .groups = "drop"
  )
factor_escala <- max(datos_grafico$total_papers) / max(datos_grafico$indice_colaboracion)
articles_ordenados <- articles %>%
  mutate(num_authors = lengths(strsplit(author, ","))) %>%
  group_by(year, num_authors) %>%
  summarise(papers = n(), .groups = "drop") %>%
  mutate(num_authors = factor(num_authors, levels = rev(sort(unique(num_authors)))))
ggplot() +
  geom_col(
    data = articles_ordenados,
    aes(x = year, y = papers, fill = num_authors),
    position = "stack"
  ) +
  geom_line(
    data = datos_grafico,
    aes(x = year, y = indice_colaboracion * factor_escala, group = 1),
    color = "red", size = 1
  ) +
  scale_y_continuous(
    name = "Cantidad de Papers",
    sec.axis = sec_axis(~./factor_escala, name = "Índice de Colaboración")
  ) +
  scale_fill_viridis_d(name = "Número de Autores", option = "viridis", direction = -1) +
  labs(
    x = "Año",
    y = "Cantidad de Papers"
  ) +
  theme_minimal()

#title = "Distribución de Papers por Número de Autores y Año",
#subtitle = "Incluye el Índice de Colaboración (línea roja)"




# autores ----------------------


authors <- articles %>%
  filter(tipo != "Introduction") %>%
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
  geom_node_point(aes(size = n)) +  # Tamaño de los nodos basado en número de colaboraciones
  scale_edge_width(range = c(1, 3)) +  # Controlar el rango del grosor de los enlaces
  # scale_edge_color_continuous(low = "lightblue", high = "darkblue") +  # Colorear los enlaces por intensidad
  theme(legend.position = "none")

instituciones <- articles %>%
  separate_rows(institution, sep = "[,;]") %>%
  mutate(institution = str_trim(institution)) %>%
  select(input_url, year, institution) %>%
  filter(!is.na(institution), institution != "NA", institution != "") %>%
  mutate(count=n(), .by = institution) %>%
  mutate(institution = ifelse(count > 2, institution, "Otros")) %>%
  pivot_longer(cols = c(-input_url,-year,-count), names_to = "xxx", values_to = "institution") %>%
  select(-xxx)

instituciones %>%
  count(year, institution) %>% 
  left_join(instituciones) %>%
  ggplot(aes(x=fct_reorder(institution, count, .desc = FALSE), y=as.factor(year), fill = n)) + 
  geom_tile() +
  geom_text(aes(label=n), color="white", show.legend = FALSE, size=3) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())





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

