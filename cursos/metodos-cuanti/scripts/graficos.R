# Clase graficos
# Cargar datasets

library(tidyverse)
library(scales)

# elecciones presidenciales 2023
generales2023 <- read.csv("cursos/metodos-cuanti/data/generales2023.csv") %>% 
  glimpse()


# asuntos Senadores
senadores <- read.csv("cursos/metodos-cuanti/data/DecadaVotada/asuntos-senadores.csv",
                      sep = ";", 
                      fileEncoding = "UTF-8",
                      stringsAsFactors = FALSE) %>% 
  glimpse()

# asuntos Diputados
diputados <- read.csv("cursos/metodos-cuanti/data/DecadaVotada/asuntos-diputados.csv",
                      sep = ";", 
                      fileEncoding = "UTF-8",
                      stringsAsFactors = FALSE) %>% 
  glimpse()


# MIDS 
mids <- read.csv("cursos/metodos-cuanti/data/MID-5/MIDA 5.0.csv") %>% 
  glimpse()


#Polity5
pol <- readxl::read_xls("cursos/metodos-cuanti/data/polity5/p5v2018.xls")


# Torta ####
## cantidad de votos por agrupacion politica (2023) ####
votos_agrup <- generales2023 %>%
  filter(agrupacion_nombre != "") %>%   # sacar vacíos
  group_by(agrupacion_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE)) %>%
  arrange(desc(votos))
# Grafico sencillo
ggplot(votos_agrup, aes(x = "", y = votos, fill = agrupacion_nombre)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Distribución de votos validos por agrupación (Generales 2023)",
       fill = "Agrupación") +
  theme_void() # limpia ejes y fondos


# Proporciones y estetica
# 1) Diccionario de nombres abreviados
abrevs <- c(
  "UNION POR LA PATRIA" = "UxP",
  "JUNTOS POR EL CAMBIO" = "JxC",
  "LA LIBERTAD AVANZA" = "LLA",
  "HACEMOS POR NUESTRO PAIS" = "HxNP",
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "FIT-U"
)

# 2) Colores de campaña
party_cols <- c(
  "UxP"   = "#1D5BA6",   # azul
  "JxC"   = "#F7C800",   # amarillo
  "LLA"   = "#6F2DBD",   # violeta
  "HxNP"  = "#00AEEF",   # celeste
  "FIT-U" = "#E41A1C",   # rojo
  "Otros" = "#BDBDBD"    # gris
)

# 3) Procesar votos
votos_agrup <- generales2023 %>%
  filter(votos_tipo == "POSITIVO", agrupacion_nombre != "") %>%
  group_by(agrupacion_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    agrupacion_abrev = ifelse(agrupacion_nombre %in% names(abrevs),
                              abrevs[agrupacion_nombre], "Otros")
  ) %>%
  group_by(agrupacion_abrev) %>%
  summarise(votos = sum(votos), .groups = "drop") %>%
  arrange(desc(votos)) %>%
  mutate(prop = votos / sum(votos),
         pct  = percent(prop, accuracy = 0.1))

# 4) Gráfico
ggplot(votos_agrup, aes(x = "", y = votos, fill = agrupacion_abrev)) +
  geom_col(width = 1, color = "white", linewidth = 0.3) +
  coord_polar(theta = "y") +
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white",   # <<< letras blancas
            fontface = "bold") +
  scale_fill_manual(values = party_cols, guide = guide_legend(ncol = 1)) +
  labs(
    title  = "Distribución de votos en procentaje por agrupación 
    politica en Elecciones Presidenciales Generales.
    Argentina Año 2023",
    fill   = "Agrupación",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). "
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  filename = "cursos/metodos-cuanti/img/torta_votos_agrupaciones.png", # carpeta/nombre de salida
  width = 8,   # ancho en pulgadas
  height = 4,  # alto en pulgadas
  dpi = 300    # resolución (300 recomendado para publicaciones)
)



## cantidad de votos por jurisdiccion ####
# Total de votos positivos por distrito
votos_distrito <- generales2023 %>%
  filter(votos_tipo == "POSITIVO") %>%
  group_by(distrito_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(votos)) %>%
  mutate(prop = votos / sum(votos),
         pct  = percent(prop, accuracy = 0.1))

# 2) Gráfico de torta (ilegible adrede)
p_distritos_torta <- ggplot(votos_distrito, aes(x = "", y = votos, fill = distrito_nombre)) +
  geom_col(width = 1, color = "white", linewidth = 0.2) +
  coord_polar(theta = "y") +
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5),
            size = 2.5,
            color = "white") +
  labs(
    title   = "Total de votos por distrito (Generales 2023)",
    fill    = "Distrito",
    caption = "Fuente: Datos oficiales de elecciones 2023"
  ) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )
p_distritos_torta
# 3) Guardar como PNG
ggsave(
  filename = "cursos/metodos-cuanti/img/torta_votos_distritos.png",
  plot = p_distritos_torta,
  width = 8,
  height = 4,
  dpi = 300
  )




# Barras verticales
# cantidad de votos por agrupacion politica



# Barras horizontales
# cantidad de votos a LLA por jurisdiccion



# Barras horizontales apiladas
# cantidad de votos por agrupacion politica por jurisdiccion

# ============================
# 1) Diccionarios (siglas y colores)
# ============================
# Abreviaturas de agrupaciones tipo "medios"
abrevs <- c(
  "UNION POR LA PATRIA" = "UxP",
  "JUNTOS POR EL CAMBIO" = "JxC",
  "LA LIBERTAD AVANZA"   = "LLA",
  "HACEMOS POR NUESTRO PAIS" = "HxNP",
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "FIT-U"
)

# Colores de campaña + "Otros"
party_cols <- c(
  "UxP"   = "#1D5BA6",  # azul
  "JxC"   = "#F7C800",  # amarillo
  "LLA"   = "#6F2DBD",  # violeta
  "HxNP"  = "#00AEEF",  # celeste
  "FIT-U" = "#E41A1C",  # rojo
  "Otros" = "#BDBDBD"   # gris
)

# Abreviaturas de distritos
distritos_abrev <- c(
  "Ciudad Autónoma de Buenos Aires" = "CABA",
  "Buenos Aires" = "Bs.As.",
  "Catamarca" = "Cat.",
  "Córdoba" = "Cba.",
  "Corrientes" = "Ctes.",
  "Chaco" = "Chaco",
  "Chubut" = "Chubut",
  "Entre Ríos" = "E.Ríos",
  "Formosa" = "Form.",
  "Jujuy" = "Jujuy",
  "La Pampa" = "L.Pampa",
  "La Rioja" = "L.Rioja",
  "Mendoza" = "Mza.",
  "Misiones" = "Mis.",
  "Neuquén" = "Neuquén",
  "Río Negro" = "R.Negro",
  "Salta" = "Salta",
  "San Juan" = "S.Juan",
  "San Luis" = "S.Luis",
  "Santa Cruz" = "Sta.Cruz",
  "Santa Fe" = "Sta.Fe",
  "Santiago del Estero" = "Sgo.Est.",
  "Tucumán" = "Tuc.",
  "Tierra del Fuego, Antártida e Islas del Atlántico Sur" = "TDF"
)

# ============================
# 2) Preprocesamiento común
# ============================
datos_pos <- generales2023 %>%
  filter(votos_tipo == "POSITIVO") %>%
  mutate(
    agrup_abrev = ifelse(agrupacion_nombre %in% names(abrevs), abrevs[agrupacion_nombre], "Otros"),
    distrito_abrev = recode(distrito_nombre, !!!distritos_abrev)
  )

# ============================
# 3) BARRAS VERTICALES por agrupación (total país)
# ============================
votos_agrup <- datos_pos %>%
  filter(agrupacion_nombre != "") %>%
  group_by(agrup_abrev) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop") %>%
  mutate(agrup_abrev = fct_reorder(agrup_abrev, votos, .desc = TRUE))

p_barras_verticales <- ggplot(votos_agrup,
                              aes(x = agrup_abrev, y = votos, fill = agrup_abrev)) +
  geom_col() +
  geom_text(aes(label = comma(votos)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = party_cols, guide = "none") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(
    title = "Cantidad de votos por agrupación política en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    x   = "Agrupación",
    y = "Votos",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). "
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p_barras_verticales
ggsave(
  filename = "cursos/metodos-cuanti/img/barras_vert_voto_agrup.png",
  plot = p_barras_verticales,
  width = 9,
  height = 5,
  dpi = 300
)
# ============================
# 4) BARRAS HORIZONTALES de LLA por distrito
# ============================
votos_lla_distrito <- datos_pos %>%
  filter(agrup_abrev == "LLA") %>%
  group_by(distrito_abrev) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(votos)

p_lla_horiz <- ggplot(votos_lla_distrito,
                      aes(x = votos, y = fct_reorder(distrito_abrev, votos))) +
  geom_col(fill = party_cols["LLA"]) +
  geom_text(aes(label = comma(votos)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(
    title = "Cantidad de votos a La Libertad Avanza por distrito en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    x = "Votos", y = "Distrito",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). "
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_lla_horiz

ggsave(
  filename = "cursos/metodos-cuanti/img/barras_horiz_votoLLA_distr.png",
  plot = p_lla_horiz,
  width = 9,
  height = 5,
  dpi = 300
)

# ============================
# 5) BARRAS APILADAS por distrito y agrupación (ABSOLUTAS)
# ============================
votos_dist_agrup <- datos_pos %>%
  filter(agrupacion_nombre != "") %>%
  group_by(distrito_abrev, agrup_abrev) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop")

# Ordenar distritos por total para mejor legibilidad
orden_distritos <- votos_dist_agrup %>%
  group_by(distrito_abrev) %>%
  summarise(total = sum(votos), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(distrito_abrev)

votos_dist_agrup$distrito_abrev <- factor(votos_dist_agrup$distrito_abrev,
                                          levels = orden_distritos)

p_apiladas_abs <- ggplot(votos_dist_agrup,
                         aes(x = distrito_abrev, y = votos, fill = agrup_abrev)) +
  geom_col() +
  scale_fill_manual(values = party_cols, name = "Agrupación") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Cantidad de votos por agrupación política según distrito en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    x = "Distrito", y = "Votos",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral en base a telegramas recibidos 
    (Art. 43 Ley 26571 y Art. 105 Ley 19945)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)
  )

p_apiladas_abs

ggsave(
  filename = "cursos/metodos-cuanti/img/barras_apil_voto_distr.png",
  plot = p_apiladas_abs,
  width = 9,
  height = 5,
  dpi = 300
)
# ============================
# 6) BARRAS APILADAS por distrito (PROPORCION 100%)
# ============================
votos_dist_agrup_prop <- votos_dist_agrup %>%
  group_by(distrito_abrev) %>%
  mutate(prop = votos / sum(votos),
         pct  = percent(prop, accuracy = 0.1))

p_apiladas_prop <- ggplot(votos_dist_agrup_prop,
                          aes(x = distrito_abrev, y = votos, fill = agrup_abrev)) +
  geom_col(position = "fill") +
  # etiquetas en blanco dentro de cada sector
  geom_text(aes(label = pct),
            position = position_fill(vjust = 0.5),
            size = 2.5,
            color = "white",
            fontface = "bold") +
  scale_fill_manual(values = party_cols, name = "Agrupación") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de votos por agrupación política según distrito en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    x = "Distrito", y = "Proporción",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). "
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)
  )
p_apiladas_prop
ggsave(
  filename = "cursos/metodos-cuanti/img/barras_apil_voto_distr_prop.png",
  plot = p_apiladas_prop,
  width = 10,
  height = 5,
  dpi = 300
)
# Linea evolutivo ####

# Asuntos tratados por camara

# Agrupar por año y contar asuntos
senadores_count <- senadores %>%
  group_by(ano) %>%
  summarise(asuntos = n(), .groups = "drop") %>%
  mutate(camara = "Senadores")

diputados_count <- diputados %>%
  group_by(ano) %>%
  summarise(asuntos = n(), .groups = "drop") %>%
  mutate(camara = "Diputados")

# Unir ambos
asuntos_total <- bind_rows(senadores_count, diputados_count)

# Gráfico de líneas
asuntos <- ggplot(asuntos_total, aes(x = ano, y = asuntos, color = camara, group = camara)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Diputados" = "#1D5BA6", "Senadores" = "#E41A1C")) +
  labs(
    title = "Cantidad de asuntos legislativos tratados por año. Argentina 1995 - 2024",
    x = "Año",
    y = "Número de asuntos",
    color = "Cámara",
    caption = "Fuente: Decada Votada - Asuntos Diputados y Senadores"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

asuntos

ggsave(
  filename = "cursos/metodos-cuanti/img/lineas_asuntos_legislativos.png",
  plot = asuntos,
  width = 10,
  height = 5,
  dpi = 300
)

# cantidad de MIDs por año
# 0) Normalizar años de inicio/fin manejando valores faltantes y casos raros
mids_years <- mids %>%
  mutate(
    styear = as.integer(styear),
    endyear = as.integer(endyear),
    # Si falta endyear, asumimos mismo año de inicio (mínima duración para contar en el año)
    endyear = ifelse(is.na(endyear) | endyear < styear, styear, endyear),
    # recortar a un rango razonable por si hay valores espurios
    styear = pmax(styear, 1816L, na.rm = TRUE),
    endyear = pmin(endyear, 2014L, na.rm = TRUE) # MIDA 5 llega a 2014
  ) %>%
  # Descartar filas sin año de inicio válido
  filter(!is.na(styear), !is.na(endyear))

# 1) Expandir a formato disputa-año
mids_long <- mids_years %>%
  rowwise() %>%
  mutate(years = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest(years) %>%
  rename(year = years)

# 2) Codificación de HostLev
hostlev_labels <- c(
  `1` = "Sin Acción militarizada",
  `2` = "Amenaza",
  `3` = "Demostración",
  `4` = "Uso",
  `5` = "Guerra (1000 bajas militares por año)"
)

mids_long <- mids_long %>%
  mutate(
    hostlev_f = factor(hostlev,
                       levels = 1:5,
                       labels = hostlev_labels)
  )

# 3) Serie anual: cantidad de MIDs activas por año
mids_per_year <- mids_long %>%
  group_by(year) %>%
  summarise(n_mids = n_distinct(dispnum), .groups = "drop")

p_mids_total <- ggplot(mids_per_year, aes(x = year, y = n_mids)) +
  geom_line(linewidth = 1, color = "#1D5BA6") +
  geom_point(size = 1.5, color = "#1D5BA6") +
  scale_x_continuous(breaks = pretty) +
  labs(
    title = "Cantidad de Disputas Internacionales Militarizadas (MIDs) activas por año",
    x = "Año", y = "Número de MIDs activas",
    caption = "Fuente: Correlates of War base MID 5.0 (dispute-level) expansión disputa–año"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_mids_total


ggsave(
  filename = "cursos/metodos-cuanti/img/lineas_mids.png",
  plot = p_mids_total,
  width = 10,
  height = 5,
  dpi = 300
)



# 4) Serie anual por HostLev: una línea por tipo
mids_per_year_host <- mids_long %>%
  group_by(year, hostlev_f) %>%
  summarise(n_mids = n_distinct(dispnum), .groups = "drop")

# Paleta simple por nivel (puedes ajustar colores si querés)
cols_host <- c(
  "Sin Acción militarizada" = "#9E9E9E",
  "Amenaza"   = "steelblue",
  "Demostración"      = "blue",
  "Uso"          = "darkorange3",
  "Guerra (1000 bajas militares por año)"                   = "red"
)

p_mids_host <- ggplot(mids_per_year_host, aes(x = year, y = n_mids, color = hostlev_f)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  scale_color_manual(values = cols_host, name = "Niel de Hostilidad") +
  scale_x_continuous(breaks = pretty) +
  labs(
    title = "Cantidad de Disputas Internacionales Militarizadas (MIDs) activas por año, según nivel de hostilidad",
    x = "Año", y = "Número de MIDs activas",
    caption = "Fuente: Correlates of War base MID 5.0 (dispute-level) expansión disputa–año"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
p_mids_host

ggsave(
  filename = "cursos/metodos-cuanti/img/lineas_mids_host.png",
  plot = p_mids_host,
  width = 10,
  height = 6,
  dpi = 300
)

eventos <- data.frame(
  year = c(1870, 1914, 1939, 1989, 2001),
  evento = c("G. Franco-Prusiana", 
             "1era G. Mundial", 
             "2da G. Mundial",
             "Fin Guerra Fría", 
             "Atentados 9/11")
)

# En el gráfico total de MIDs (para una segunda version)
p_total_dashed <- p_total +
  geom_vline(data = eventos, aes(xintercept = year), 
             linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_text(data = eventos, 
            aes(x = year, y = max(mids_per_year$n_mids, na.rm = TRUE), 
                label = evento),
            angle = 90, vjust = -0.5, hjust = 1, size = 3)


p_total_dashed


# Histogramas ####


pol_2018 <- pol %>%
  filter(year == 2018, polity >= -10)

hist_2018 <- ggplot(pol_2018, aes(x = polity)) +
  geom_histogram(binwidth = 1, fill = "#1D5BA6", color = "white") +
  labs(
    title = "Valores de Polity5 en países del mundo, año 2018",
    x = "Puntaje de Polity5",
    y = "Cantidad de países",
    caption = "Fuente: Center for Systemic Peace base Polity5"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
hist_2018
ggsave(
  filename = "cursos/metodos-cuanti/img/lineas_mids_host.png",
  plot = hist_2018,
  width = 10,
  height = 6,
  dpi = 300
)

# Histograma 2018 con binwidth más grande
ggplot(pol_2018, aes(x = polity)) +
  geom_histogram(binwidth = 2, fill = "#4CAF50", color = "white") +
  labs(
    title = "Valores de Polity5 en países del mundo, año 2018",
    x = "Puntaje de Polity5",
    y = "Cantidad de países",
    caption = "Fuente: Center for Systemic Peace base Polity5"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Seleccionar años de interés
anios_interes <- c(1950, 1969, 1980, 1991, 2002, 2018)

pol_select <- pol %>%
  filter(year %in% anios_interes, polity >= -10)

# Paleta de colores manual (6 colores)
colores <- c("1950" = "#1B9E77",
             "1969" = "#D95F02",
             "1980" = "#7570B3",
             "1991" = "#E7298A",
             "2002" = "#66A61E",
             "2018" = "#E6AB02")

ggplot(pol_select, aes(x = polity, fill = factor(year))) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~ year, ncol = 3) +
  scale_fill_manual(values = colores, guide = "none") +
  labs(
    title = "Distribución de Polity5 en países seleccionados",
    x = "Puntaje de Polity5",
    y = "Cantidad de países",
    caption = "Fuente: Center for Systemic Peace base Polity5"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )



# Boxplots


# Polity 5
anios_interes <- c(1950, 1969, 1980, 1991, 2002, 2018)
pol_sel <- pol %>% filter(year %in% anios_interes, polity >= -10)

ggplot(pol_sel, aes(x = factor(year), y = polity, fill = factor(year))) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "Polity5 por año seleccionado",
       x = "Año", y = "Puntaje Polity5") +
  theme_minimal()


# Filtrar rango temporal
pol_filtered <- pol %>%
  filter(year >= 1945, year <= 2018, polity >= -10)

ggplot(pol_filtered, aes(x = factor(year), y = polity)) +
  geom_boxplot(outlier.alpha = 0.25, fill = "darkseagreen", color = "black") +
  labs(
    title = "Distribución anual de Polity5 en el mundo (1945–2018)",
    x = "Año", y = "Puntaje Polity5"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6)
  )



## MIDS ####


# Labels en castellano
hostlev_labels <- c(
  `1` = "Sin Acción militarizada",
  `2` = "Amenaza",
  `3` = "Demostración",
  `4` = "Uso",
  `5` = "Guerra"
)

# Reetiquetar HostLev
mids_h <- mids %>%
  filter(hostlev %in% 1:5) %>%
  mutate(hostlev_f = factor(hostlev, 
                            levels = 1:5,
                            labels = hostlev_labels))

# Colores (puedes modificarlos si querés otra paleta)
cols_host <- c(
  "Sin Acción militarizada"                = "#9E9E9E",
  "Amenaza"                                = "#4CAF50",
  "Demostración"                           = "#2196F3",
  "Uso"                                    = "#FF9800",
  "Guerra"  = "#E41A1C"
)

dura_mids <- ggplot(mids_h, aes(x = hostlev_f, y = maxdur, fill = hostlev_f)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = cols_host, guide = "none") +
  labs(
    title = "Duración de disputas MID por nivel de hostilidad",
    x = "Nivel de hostilidad", y = "Duración (días)",
    caption = "Fuente: Correlates of War base MID 5.0 (dispute-level)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = "cursos/metodos-cuanti/img/box_mids_duracion.png",
  plot = dura_mids,
  width = 8,
  height = 6,
  dpi = 300
)

log_duramids <- ggplot(mids_h, aes(x = hostlev_f, y = maxdur, fill = hostlev_f)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  scale_fill_manual(values = cols_host, guide = "none") +
  labs(
    title = "Duración de disputas MID por nivel de hostilidad (escala log)",
    x = "Nivel de hostilidad", y = "Duración (días, log10)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = "cursos/metodos-cuanti/img/box_mids_duracion_log.png",
  plot = log_duramids,
  width = 8,
  height = 6,
  dpi = 300
)


# Dispersion agregando dimensiones ####


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---------------------------
# 0) Parámetros y helpers
# ---------------------------
# Partidos principales (nombres exactos en la base)
nm_uxp <- "UNION POR LA PATRIA"
nm_jxc <- "JUNTOS POR EL CAMBIO"
nm_lla <- "LA LIBERTAD AVANZA"

# Tipos de voto considerados "no válidos"
tipos_novalidos <- c("NULO","IMPUGNADO","RECURRIDO","COMANDO",
                     "EN BLANCO","VOTO EN BLANCO","BLANCO")


# ---------------------------
# 1) Agregación a distrito_id + seccion_id
# ---------------------------
# Totales POSITIVOS por partido (UxP, JxC, LLA) a nivel sección
sec_pos_partidos <- generales2023 %>%
  filter(votos_tipo == "POSITIVO", !is.na(agrupacion_nombre), agrupacion_nombre != "") %>%
  group_by(distrito_id, seccion_id, agrupacion_nombre) %>%
  summarise(votos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop") %>%
  mutate(agrupacion_nombre = recode(agrupacion_nombre,
                                    !!nm_uxp := "UxP",
                                    !!nm_jxc := "JxC",
                                    !!nm_lla := "LLA",
                                    .default = "Otros")) %>%
  group_by(distrito_id, seccion_id, agrupacion_nombre) %>%
  summarise(votos = sum(votos), .groups = "drop") %>%
  pivot_wider(names_from = agrupacion_nombre, values_from = votos, values_fill = 0) %>%
  # Garantizamos que existan las columnas aun si no aparecen
  mutate(UxP = ifelse(is.na(UxP), 0, UxP),
         JxC = ifelse(is.na(JxC), 0, JxC),
         LLA = ifelse(is.na(LLA), 0, LLA)) %>%
  # Total de positivos (todas las agrupaciones, incl. "Otros")
  mutate(pos_total = UxP + JxC + LLA + coalesce(Otros, 0)) %>%
  select(distrito_id, seccion_id, UxP, JxC, LLA, pos_total)

# Totales NO VÁLIDOS por sección
sec_novalidos <- generales2023 %>%
  filter(votos_tipo %in% tipos_novalidos) %>%
  group_by(distrito_id, seccion_id) %>%
  summarise(no_validos = sum(votos_cantidad, na.rm = TRUE), .groups = "drop")

# Total votos emitidos por sección (positivos + no válidos)
sec_totales <- sec_pos_partidos %>%
  left_join(sec_novalidos, by = c("distrito_id","seccion_id")) %>%
  mutate(no_validos = coalesce(no_validos, 0L),
         total_emitidos = pos_total + no_validos) %>%
  # Proporciones (respecto a total emitidos; cambiar a pos_total si preferís)
  mutate(
    prop_uxp = ifelse(total_emitidos > 0, UxP / total_emitidos, NA_real_),
    prop_jxc = ifelse(total_emitidos > 0, JxC / total_emitidos, NA_real_),
    prop_lla = ifelse(total_emitidos > 0, LLA / total_emitidos, NA_real_),
    prop_no_validos = ifelse(total_emitidos > 0, no_validos / total_emitidos, NA_real_)
  )

# Base final para graficar
sec <- sec_totales %>% filter(total_emitidos > 0)

# ---------------------------
# 2) Scatterplots solicitados
# ---------------------------

# (1) UxP (X) vs JxC (Y), conteos (puntos del mismo tamaño)
p1 <- ggplot(sec, aes(x = UxP, y = JxC)) +
  geom_point(size= 3, alpha = 0.6, color = "#455A64") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Cantidad de votos en secciones electorales a Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP)
    en Elecciones Presidenciales Generales. Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "Votos a UxP", y = "Votos a JxC") +
  theme_minimal(base_size = 12)
p1

ggsave("cursos/metodos-cuanti/img/scatter_seccion_uxp_vs_jxc.png", p1, 
       width = 10, height = 6, dpi = 300)

# (2) UxP vs JxC con tamaño = votos LLA
p2 <- ggplot(sec, aes(x = UxP, y = JxC, size = LLA)) +
  geom_point(alpha = 0.3, color = "#6F2DBD") +
  scale_size_area(max_size = 12, labels = comma, name = "Votos LLA") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Cantidad de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "Votos a UxP", y = "Votos a JxC"
  ) +
  theme_minimal(base_size = 12)
p2
ggsave("cursos/metodos-cuanti/img/scatter_seccion_uxp_vs_jxc_size_lla.png", p2, width = 10, height = 6, dpi = 300)

# (3) UxP vs JxC con tamaño = votos no válidos
p3 <- ggplot(sec, aes(x = UxP, y = JxC, size = no_validos)) +
  geom_point(alpha = 0.6, color = "#E41A1C") +
  scale_size_area(max_size = 12, labels = comma, name = "Votos no válidos") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) y votos no validos en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "Votos a UxP", y = "Votos a JxC"
  ) +
  theme_minimal(base_size = 12)
p3
ggsave("graficos/scatter_seccion_uxp_vs_jxc_size_novalidos.png", p3, width = 8, height = 6, dpi = 300)

# (5) Proporciones: X = % UxP, Y = % JxC, size = % LLA, alpha = % no válidos
p5 <- ggplot(sec, aes(x = prop_uxp, y = prop_jxc, size =prop_lla , alpha = prop_no_validos)) +
  geom_point(color = "#1D5BA6") +
  scale_size_area(max_size = 12, name = "% LLA", labels = percent_format(accuracy = 0.1)) +
  scale_alpha(range = c(0.2, 0.9), name = "% no válidos", labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) y votos no validos en Elecciones Presidenciales Generales. 
    Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "% de votos a UxP (sobre total emitidos)",
    y = "% de votos a JxC (sobre total emitidos)") +
  theme_minimal(base_size = 12) +
  guides(alpha = guide_legend(order = 2), size = guide_legend(order = 1))

p5
ggsave("cursos/metodos-cuanti/img/scatter_seccion_prop_uxp_vs_prop_jxc_size_lla_alpha_novalidos.png",
       p5, width = 12, height = 6.2, dpi = 300)




# Cuantiles reales para rotular
qs <- quantile(sec$prop_no_validos, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)

# Agrupar por cuartiles de % no válidos y rotular
sec_q <- sec %>%
  filter(!is.na(prop_uxp), !is.na(prop_jxc), !is.na(prop_no_validos), !is.na(prop_lla)) %>%
  mutate(
    q_no_validos = dplyr::ntile(prop_no_validos, 4),
    q_label = factor(
      q_no_validos, levels = 1:4,
      labels = c(
        paste0("Q1 (", percent(qs[1]), "–", percent(qs[2]), ")"),
        paste0("Q2 (", percent(qs[2]), "–", percent(qs[3]), ")"),
        paste0("Q3 (", percent(qs[3]), "–", percent(qs[4]), ")"),
        paste0("Q4 (", percent(qs[4]), "–", percent(qs[5]), ")")
      )
    )
  )

# Scatter: %UxP vs %JxC, color = cuartil de % no válidos, size = % LLA
p_quartiles <- ggplot(
  sec_q,
  aes(x = prop_uxp, y = prop_jxc, color = q_label, size = prop_lla)
) +
  geom_point(alpha = 0.4) +
  scale_size_area(max_size = 12, name = "% LLA",
                  labels = percent_format(accuracy = 0.1)) +
  scale_color_brewer(palette = "Set2", name = "% no válidos (cuartiles)") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a
  Union por la Patria (UxP) ajustado por voto a La Libertad Avanza (LLA) 
  y votos no validos (Cuartiles) en Elecciones Presidenciales Generales. Argentina Año 2023",
       caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)",
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 4)),
    size  = guide_legend(order = 2)
  )
p_quartiles
ggsave("cursos/metodos-cuanti/img/pquartiles.png",
       p_quartiles
       , width = 11, height = 6.2, dpi = 300)

p_facets <- ggplot(
  sec_q,
  aes(x = prop_uxp, y = prop_jxc, size = prop_lla, color = q_label)
) +
  geom_point(alpha = 0.25) +
  scale_size_area(max_size = 10, name = "% LLA",
                  labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  facet_wrap(~ q_label) +
  labs(
    title = "%UxP vs %JxC por sección (faceteado por cuartiles de % no válidos)",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)",
    caption = "Cada panel muestra un cuartil distinto de % de votos no válidos • Tamaño = % LLA"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  guides(color = "none")  # ya se distingue por 
p_facets


# ultima exploracion
library(dplyr)

sec_enriched <- sec %>%
  mutate(
    # “inclinación ideológica” de la sección: derecha si >0, izquierda si <0
    tilt = prop_jxc - prop_uxp,
    tilt_bin = case_when(
      tilt <= -0.10 ~ "Más UxP (izq)",
      tilt >=  0.10 ~ "Más JxC (der)",
      TRUE          ~ "Centro"
    )
  )

# si ya tenías sec_q con los cuartiles de no válidos: únelos
sec_plot <- sec_q %>%
  select(distrito_id, seccion_id, q_label) %>%
  right_join(sec_enriched, by = c("distrito_id","seccion_id"))

p_colors <- ggplot(sec_plot, aes(x = prop_uxp, y = prop_jxc, color = prop_lla)) +
  geom_point(size = 5, alpha = 0.6) +
  facet_wrap(~ q_label) +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_viridis_c(name = "% LLA", labels = percent_format(accuracy = 0.1)) +
  labs(title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
  ajustado por voto a La Libertad Avanza (LLA) y votos no validos (Cuartiles) en Elecciones Presidenciales Generales. Argentina Año 2023",
       caption = "Fuente: Resultados Provisorios compilados por Direccion Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945). ",
    x = "% de votos a UxP (sobre emitidos)", y = "% de votos a JxC (sobre emitidos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_colors <- p_colors + guides(color = guide_colorbar(barwidth = 15, barheight = 0.8))
p_colors
ggsave("cursos/metodos-cuanti/img/p_colors.png",
       p_colors
       , width = 11, height = 6.2, dpi = 300)


p2 <- ggplot(sec_enriched, aes(x = prop_no_validos, y = prop_lla, color = tilt_bin)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("Más UxP (izq)"="#1D5BA6", "Centro"="#888888", "Más JxC (der)"="#F7C800"),
                     name = "Inclinación") +
  labs(
    title = "¿Crece LLA donde crecen los no válidos? (por inclinación ideológica)",
    x = "% de votos no válidos", y = "% de votos a LLA"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
p2


p3 <- ggplot(sec_enriched, aes(x = prop_no_validos, y = prop_lla)) +
  stat_bin2d(bins = 30) +
  facet_wrap(~ tilt_bin) +
  scale_fill_viridis_c(name = "Secciones") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Densidad conjunta: % no válidos vs % LLA (por inclinación ideológica)",
    x = "% de votos no válidos", y = "% de votos a LLA"
  ) +
  theme_minimal(base_size = 12)
p3



# Hipotesis final testo 

library(dplyr)

eps <- 1e-6  # para evitar divisiones por cero

sec_plot_h <- sec_plot %>%
  mutate(
    # H1: “LLA le saca votos a JxC”
    # ¿Qué proporción de (LLA + JxC) se la lleva LLA?
    share_lla_en_derecha = prop_lla / pmax(prop_lla + prop_jxc, eps),
    
    # H2: “LLA le saca votos a UxP”
    # ¿Qué proporción de (LLA + UxP) se la lleva LLA?
    share_lla_en_izquierda = prop_lla / pmax(prop_lla + prop_uxp, eps),
    
    # H3: “LLA es opuesta a los no válidos”
    # Para testear visualmente, coloreamos por % no válidos (esperaríamos
    # que las zonas de alto LLA en la ‘real’ tengan BAJO color acá si la hipótesis fuese cierta)
    prop_noval = prop_no_validos
  )

library(ggplot2)
library(scales)

p_vacio_color <- ggplot(sec_plot, aes(x = prop_uxp, y = prop_jxc, color = prop_lla)) +
  geom_blank() +   # no muestra datos, solo ejes
  facet_wrap(~ q_label) +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_viridis_c(name = "% LLA", labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) y votos no validos (Cuartiles) en Elecciones Presidenciales Generales. Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Dirección Nacional Electoral\n
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945).",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Ajustar ancho de la barra de color
p_vacio_color <- p_vacio_color +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.8))

# Guardar
ggsave("cursos/metodos-cuanti/img/p_vacio_color.png",
       p_vacio_color, width = 11, height = 6.2, dpi = 300)



p_h1 <- ggplot(sec_plot_h,
               aes(x = prop_uxp, y = prop_jxc, color = share_lla_en_derecha)) +
  geom_point(size = 5, alpha = 0.6) +
  facet_wrap(~ q_label) +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_viridis_c(name = "Cuota LLA",
                        labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) y votos no validos (Cuartiles) en Elecciones Presidenciales Generales. Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Dirección Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945).",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.8))

p_h1
ggsave("cursos/metodos-cuanti/img/p_h1.png",
       p_h1, width = 11, height = 6.2, dpi = 300)

p_h2 <- ggplot(sec_plot_h,
               aes(x = prop_uxp, y = prop_jxc, color = share_lla_en_izquierda)) +
  geom_point(size = 5, alpha = 0.6) +
  facet_wrap(~ q_label) +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_viridis_c(name = "Cuota LLA",
                        labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proporciones de votos a en secciones electorales Juntos por el Cambio (JxC) por voto a Union por la Patria (UxP) 
    ajustado por voto a La Libertad Avanza (LLA) y votos no validos (Cuartiles) en Elecciones Presidenciales Generales. Argentina Año 2023",
    caption = "Fuente: Resultados Provisorios compilados por Dirección Nacional Electoral
    en base a telegramas recibidos (Art. 43 Ley 26571 y Art. 105 Ley 19945).",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.8))

p_h2
ggsave("cursos/metodos-cuanti/img/p_h2.png",
       p_h2, width = 11, height = 6.2, dpi = 300)


p_h3 <- ggplot(sec_plot_h,
               aes(x = prop_uxp, y = prop_jxc, color = prop_noval)) +
  geom_point(size = 5, alpha = 0.6) +
  facet_wrap(~ q_label) +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_viridis_c(name = "% no válidos",
                        labels = percent_format(accuracy = 0.1),
                        direction = -1) +  # invertimos para que “alto = claro”
  labs(
    title = "Hipótesis H3 (opuesta): LLA baja donde suben no válidos (color = % no válidos)",
    x = "% de votos a UxP (sobre emitidos)",
    y = "% de votos a JxC (sobre emitidos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.8))
p_h3