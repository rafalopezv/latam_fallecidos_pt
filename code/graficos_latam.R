library(janitor)
library(futurevisions)
library(highcharter)
library(EpiEstim)
library(tidyverse)
library(magrittr)
library(furrr)
library(future)

plan(multiprocess) # activar procesamiento paralelo

source("code/limpieza_bases.R")
source("code/funciones.R")

#----------------------------------------------
# aplanamiento de curvas
#----------------------------------------------
# vector de países a remover
quitar <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Guyana", "Suriname", 
            "Trinidad and Tobago", "Dominica", "Grenada", "Saint Kitts and Nevis", "Saint Lucia",
            "Saint Vincent and the Grenadines", "Jamaica")


codigos <- read_csv("input/codigos_covid_paises.csv") %>% 
  remove_empty() %>% 
  filter(
    region_es == "América Latina y el Caribe",
    !country_region %in% quitar
  ) %>% 
  dplyr::select(
    pais = pais_region,
    pais_region = country_region,
    pais_nombre_corto,
    continente
  ) 

# preparación de base: confirmados
df_mundo %>%
  filter(
    base == "fallecidos",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(., codigos) %>% 
  group_by(pais_region, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_region, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(
    titulo = paste0(pais_nombre_corto, ", ", casos, " casos", "\n", total_semanas,  
                    " semanas desde paciente 0", "\n", " Casos últ. semana:", ult_semana)
  ) -> temp


temp %>% 
  dplyr::select(pais_region, casos) %>% 
  unique() %>% 
  arrange(casos) %>% 
  mutate(num = 1:nrow(.)) -> temp_1

# grafico incidencia confirmados
temp %>% 
  group_by(titulo, pais_region, semana) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  left_join(., temp_1) %>% 
  ggplot(aes(semana, incidencia, fill = continente)) + 
  geom_col(color = NA, alpha = 0.9, fill = "#CB1724") + 
  facet_wrap(vars(fct_reorder(titulo, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Open Sans Medium", strip_text_size = 15) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.background = element_rect(fill = "#F8F8F8", colour = NA),
  )  -> curva_confirmados 

# por millon infectados
colores <- c("#698E7C", "#552F31", "#EE5D36", "#EFCF40", "#DC363B", "#315886", "#552F31", "#EFCF40", "#CBB193",
             "#698E7C", "#DC363B", "#EE5D36", "#315886", "#2B2D2D", "#EE5D36", "#DC363B", "#EFCF40", "#2B2D2D",
             "#552F31", "#CBB193")

# cambio idioma
lang <- getOption("highcharter.lang")
lang$months <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

lang$shortMonths <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                      "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")


lang$weekdays <- c("Domingo", "Segunda-feira", "Terça-feira", "Quarta-feira", "Quinta-feira", "Sexta-feira", "Sábado")

options(highcharter.lang = lang)


temp %>% 
  left_join(., codigos) %>% 
  mutate(
    por_millon = casos_acumulados/poblacion *1000000,
    por_millon = round(por_millon, 0)
  ) %>%
  hchart(
    "line",
    hcaes(
      x = fecha, y = por_millon, group = pais_nombre_corto
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos por milhão de habitantes")) %>%
  hc_chart(style = list(fontFamily = "Open Sans")) %>%
  hc_colors(colors = colores) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> millon_confirmados 


# preparación de base: fallecidos
df_mundo %>%
  filter(
    base == "fallecidos",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(., codigos) %>% 
  group_by(pais_region, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_region, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(
    titulo = paste0(pais_nombre_corto, ", ", casos, " fallecidos", "\n", total_semanas,  
                    " semanas desde fallecido 0", "\n", " Fallecidos últ. semana:", ult_semana)
  ) -> temp

# nillon fallecidos
temp %>%
  left_join(., codigos) %>% 
  mutate(
    por_millon = casos_acumulados/poblacion *1000000,
    por_millon = round(por_millon, 0)
  ) %>%
  hchart(
    "line",
    hcaes(
      x = fecha, y = por_millon, group = pais_nombre_corto
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos por milhão de habitantes")) %>%
  hc_chart(style = list(fontFamily = "Open Sans")) %>%
  hc_colors(colors = colores) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> millon_fallecidos 



temp %>% 
  dplyr::select(pais_region, casos) %>% 
  unique() %>% 
  arrange(casos) %>% 
  mutate(num = 1:nrow(.)) -> temp_1

# grafico incidencia fallecidos
temp %>% 
  group_by(titulo, pais_region, semana) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  left_join(., temp_1) %>% 
  ggplot(aes(semana, incidencia, fill = continente)) + 
  geom_col(color = NA, alpha = 0.9, fill = "#09283C") + 
  facet_wrap(vars(fct_reorder(titulo, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Open Sans Medium", strip_text_size = 15) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.background = element_rect(fill = "#F8F8F8", colour = NA),
  )  -> curva_fallecidos 
  
#------------------------------------
# streamgraph de confirmados
#------------------------------------
df_mundo %>%
  filter(
    base == "confirmados",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(codigos, .) %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_region, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() -> temp

# prevalencia latam confirmados
hchart(temp, "streamgraph", hcaes(fecha, casos_acumulados, group = pais_region),
       label = list(
         enabled = TRUE, minFontSize = 10, maxFontSize = 20,
         style = list(
           fontWeight = 100,
           textOutline = "1px gray",
           color = hex_to_rgba("white", 0.9)
         )
       )
     ) %>% 
  hc_tooltip(shared = T, table = T, sort = T, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    itemMarginBottom = 10,
    x = -1,
    y = 105
  ) %>% 
  hc_size(height = 900) -> rio_confirmados
  
# incidencia latam
hchart(temp, "streamgraph", hcaes(fecha, incidencia, group = pais_region),
       label = list(
         enabled = TRUE, minFontSize = 10, maxFontSize = 20,
         style = list(
           fontWeight = 100,
           textOutline = "1px gray",
           color = hex_to_rgba("white", 0.9)
         )
       )
) %>% 
  hc_tooltip(shared = T, table = T, sort = T, outside = T, borderWidth = 0.01,
             style = list(fontFamily = "Open Sans")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    itemMarginBottom = 10,
    x = -1,
    y = 105
  ) %>% 
  hc_size(height = 900) -> incidencia_confirmados 
  


# streamgraph de fallecidos
df_mundo %>%
  filter(
    base == "fallecidos",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(codigos, .) %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_region, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() -> temp

# cambiar colores para diferenciar de confirmados: paleta 
show_palette("pegasi")[[1]] # orden manuel de esta paleta

colores <- c("#698E7C", "#552F31", "#EE5D36", "#EFCF40", "#DC363B", "#315886", "#552F31", "#EFCF40", "#CBB193",
             "#698E7C", "#DC363B", "#EE5D36", "#315886", "#2B2D2D", "#EE5D36", "#DC363B", "#EFCF40", "#2B2D2D",
             "#552F31", "#CBB193")

# prevalencia latam confirmados
hchart(temp, "streamgraph", hcaes(fecha, casos_acumulados, group = pais_nombre_corto),
       label = list(
         enabled = TRUE, minFontSize = 10, maxFontSize = 20,
         style = list(
           fontWeight = 100,
           textOutline = "1px gray",
           color = hex_to_rgba("white", 0.9)
         )
       )
) %>% 
  hc_tooltip(shared = T, table = T, sort = T, borderWidth = 0.01,
             style = list(fontFamily = "Open Sans")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_colors(colores) %>% 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    itemMarginBottom = 10,
    x = -1,
    y = 105
  ) %>% 
  hc_size(height = 900) -> rio_fallecidos
  
# incidencia latam
hchart(temp, "streamgraph", hcaes(fecha, incidencia, group = pais_nombre_corto),
       label = list(
         enabled = TRUE, minFontSize = 10, maxFontSize = 20,
         style = list(
           fontWeight = 100,
           textOutline = "1px gray",
           color = hex_to_rgba("white", 0.9)
         )
       )
) %>% 
  hc_tooltip(shared = T, table = T, sort = T, outside = T, borderWidth = 0.01,
             style = list(fontFamily = "Open Sans")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_colors(colores) %>% 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    itemMarginBottom = 10,
    x = -1,
    y = 105
  ) %>% 
  hc_size(height = 900) -> incidencia_fallecidos

