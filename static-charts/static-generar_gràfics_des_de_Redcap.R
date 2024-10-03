# Establir working directory. Triar segons on s'estigui executant. En el futur haurem de mirar una manera per evitar fer aquest pas previ.
# setwd("C:/Users/joanc/OneDrive/Documents/Joan/Programació i estadística/Visual Studio Code") # Casa
setwd("C:/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code/Redcap-Gantt-Charts") # Clínic
# setwd("C:/Users/motosgalera-a/Downloads/codi") # Anna

# Carregar llibreries d'interès.
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, 
               httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, tibble, openxlsx, gridExtra, reshape2, 
               ggiraph, htmlwidgets, purrr)


# GRÀFICS GANTT ESTÀTICS A PARTIR DE REDCAP

## Importem els formulari Redcap que necessitem i modifiquem les dades per adequar-les al format del codi.

# Carregar formulari Redcap final dels tractaments.
input_gràfics <- import("documents_necessaris/Formulari_tractaments_redcap.csv")

# El modifiquem per adaptar-lo al codi que tenim fet.
input_gràfics_llarg <- input_gràfics %>% 
  select(-t_trat) %>% 
  pivot_longer(
    cols = matches("t_trat_(\\d+)$|t_trat_tipo_(\\d+)$|t_trat_adm_otro_(\\d+)$|t_trat_adm_(\\d+)$|t_trat_fecha_i_(\\d+)$|t_trat_fecha_f_(\\d+)$"),  
    names_to = c(".value", "tratamiento_id"),  
    names_pattern = "(t_trat|t_trat_tipo|t_trat_adm_otro|t_trat_adm|t_trat_fecha_i|t_trat_fecha_f)_(\\d+)") %>% 
  select(-redcap_event_name, -ant_ingreso_fecha_hosp, -out_alta_hosp_fecha, -tratamiento_complete, -tratamiento_id) %>% 
  filter(!is.na(t_trat))

# Carreguem també la codificació dels tractaments per passar de codis Redcap a labels Redcap.
codificacio_tractaments <- import("documents_necessaris/Codificación_tratamientos.csv")

# Transformem els números dels tractaments a labels.
input_gràfics_descodificat <- input_gràfics_llarg %>% 
  rowwise() %>%
  mutate(t_trat = {
    tract = NA_character_
    for (i in seq_along(codificacio_tractaments$`Codificacion tratamientos`)) {
      if (grepl(codificacio_tractaments$`Codificacion tratamientos`[i], t_trat, ignore.case = TRUE)) {
        tract <- codificacio_tractaments$`Tratamiento redcap labels`[i]
        break
      }
    }
    tract
  })

# Transformem els números de les vies d'administració a labels.
input_gràfics_descodificat$t_trat_adm <- factor(input_gràfics_descodificat$t_trat_adm, levels = c(0, 1, 2, 3, 4), labels = c("Oral", "Nebulizado", "Endovenoso", "Tópico", "Otra"))
  
# Carregarem les dates importants dels pacients.
dates_estancia_hosp <- read.csv("documents_necessaris/Dates_importants.csv") # Dates importants estància en raw data.
dates_estancia_hosp$d1_sospecha_fecha_vapvat <- as.Date(dates_estancia_hosp$d1_sospecha_fecha_vapvat)
dates_estancia_hosp$iu_fecha <- as.Date(dates_estancia_hosp$iu_fecha)

# També carregarem informació dels resultats de les principals proves realitzades als pacients (FA D1, SOC D1, FA D3 i SOC D3) i de si a la sospita és una VAT o una VAP.
resultats_proves <- read.csv("documents_necessaris/Resultats_proves.csv", check.names = F) # En labels.


## Acabem de modificar les dades i generem els gràfics.

# Utilitzarem el df previ a la modificació en format Redcap: tractament_noms_columna. Afegim les dates importants en el formulari dels tractaments.
tractament_gantt <- input_gràfics_descodificat %>%
  left_join(dates_estancia_hosp %>% select(nhc, ant_ingreso_fecha_hosp, iu_fecha, d1_sospecha_fecha_vapvat, d1_dm_filma_fecha_r, d1_dm_fecha_mc, d3_dm_filma_fecha_r, d3_dm_fecha_mc, out_alta_uci_fecha_final, out_alta_hosp_fecha), join_by(nhc)) %>% 
  select(-t_trat_adm_otro, -t_trat_tipo, -nhc) %>%
  # Canviem els noms de les variables perquè s'entenguin en els gràfics.
  rename(`Identificación del paciente` = ant_id,
         Tratamiento = t_trat,
         `Administración` = t_trat_adm,
         `Fecha inicial tratamiento` = t_trat_fecha_i,
         `Fecha final tratamiento` = t_trat_fecha_f,
         `Fecha ingreso hospitalario` = ant_ingreso_fecha_hosp,
         `Fecha ingreso UCI` = iu_fecha,
         `Fecha sospecha clínica` = d1_sospecha_fecha_vapvat,
         `Fecha resultado FA D1` = d1_dm_filma_fecha_r,
         `Fecha resultado SOC D1` = d1_dm_fecha_mc,
         `Fecha resultado FA D3` = d3_dm_filma_fecha_r,
         `Fecha resultado SOC D3` = d3_dm_fecha_mc,
         `Fecha alta UCI` = out_alta_uci_fecha_final,
         `Fecha alta hospitalaria` = out_alta_hosp_fecha) %>%
  # Posem la columna d'identificació del pacient com a primera.
  relocate(`Identificación del paciente`, .before = 1) %>% 
  # Creem dues variables, una que serà l'inici de l'eix de les x (3 dies abans de la data de sospita clínica), i una altra que serà el final de l'eix de les x 5 dies després del resultat del SOC D1)
  mutate(fecha_x_inicio = as.Date(as.numeric(`Fecha sospecha clínica`) - 3),
         fecha_x_final = as.POSIXct(`Fecha resultado SOC D1`, format = "%Y-%m-%d %H:%M") + lubridate::days(7)) %>%
  # Partim alguns tractaments en dos perquè no ens ocupin tant d'amplitut en el gràfic.
  mutate(Tratamiento = str_replace_all(Tratamiento, "/", "/ ")) %>% 
  mutate(Tratamiento = str_wrap(Tratamiento, width = 18)) 

# En cas de que no hi hagi resultat de SOC D1, agafem per calcular la data final el resultat de FA D1.
tractament_gantt$fecha_x_final <- ifelse(is.na(tractament_gantt$fecha_x_final),
                                         as.POSIXct(tractament_gantt$`Fecha resultado FA D1`, format = "%Y-%m-%d %H:%M") + lubridate::days(9),
                                         tractament_gantt$fecha_x_final)


# Verifiquem que les dates estan en format data hora.
tractament_gantt$fecha_x_inicio <- as.POSIXct(tractament_gantt$fecha_x_inicio, format = "%Y-%m-%d %H:%M")
tractament_gantt$fecha_x_final <- as.POSIXct(tractament_gantt$fecha_x_final, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha inicial tratamiento` <- as.POSIXct(tractament_gantt$`Fecha inicial tratamiento`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha final tratamiento` <- as.POSIXct(tractament_gantt$`Fecha final tratamiento`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha ingreso UCI` <- as.POSIXct(tractament_gantt$`Fecha ingreso UCI`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha sospecha clínica` <- as.POSIXct(
  paste(tractament_gantt$`Fecha sospecha clínica`, "00:00"), 
  format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha resultado FA D1` <- as.POSIXct(tractament_gantt$`Fecha resultado FA D1`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha resultado SOC D1` <- as.POSIXct(tractament_gantt$`Fecha resultado SOC D1`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha resultado FA D3` <- as.POSIXct(tractament_gantt$`Fecha resultado FA D3`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha resultado SOC D3` <- as.POSIXct(tractament_gantt$`Fecha resultado SOC D3`, format = "%Y-%m-%d %H:%M")

# Ara eliminem els tractaments que queden fora de rang.
dates_VAPVAT <- tractament_gantt %>%
  filter(as.numeric(difftime(`Fecha final tratamiento`, `Fecha sospecha clínica`, units = "days")) > -3) %>%
  filter(as.numeric(difftime(`Fecha inicial tratamiento`, `Fecha resultado SOC D1`, units = "days")) < 5 |
         as.numeric(difftime(`Fecha inicial tratamiento`, `Fecha resultado FA D1`, units = "days")) < 8) %>% 
 # Fem el codi així perquè no es produeixin missings, ja que no tots els pacients tenen SOC D3 i per tant elimina aquests resultats.
  arrange(`Identificación del paciente`)
  
  
# Creem una llista buida per guardar tots els pacients.
pacients_per_gantt <- list()

# Iterem sobre cada identificació única del pacient.
for (i in unique(dates_VAPVAT$`Identificación del paciente`)) {
  # Agrupem els pacients per identificació i guardem els resultats a la llista.
  pacients_per_gantt[[paste0("Gantt_", i)]] <- filter(dates_VAPVAT, `Identificación del paciente` == i)
}

# Funció per generar gràfics ajustant els límits del rang de dates.
generar_grafic_estancia <- function(df) {
  
  # Obtenim el rang que hem triat per fer amplada dels gràfics.
  xlim_range <- range(c(df$`fecha_x_inicio`, df$`fecha_x_final`), na.rm = TRUE)
  
  # Ajustem el límit inferior i superior del rang en funció de dates d'inici i final del tractament.
  df <- df %>%
    mutate(`Fecha inicial tratamiento` = pmax(`Fecha inicial tratamiento`, xlim_range[1]),
           `Fecha final tratamiento` = pmin(`Fecha final tratamiento`, xlim_range[2]))
  
  # Creem un nou df per crear els rectangles que ens serviran per determinar els periodes del gràfic Gantt.
  df2 <- df %>% 
    select(`Identificación del paciente`, `Fecha ingreso UCI`, `Fecha sospecha clínica`, `Fecha resultado FA D1`, `Fecha resultado SOC D1`, `Fecha resultado FA D3`, `Fecha resultado SOC D3`) %>%
    distinct() %>% 
    arrange(`Identificación del paciente`)
  
  # Creem un df amb les dates importants del gràfic, les etiquetes que tenen i els colors associats.
  vertical_lines <- data.frame(
    xintercept = c(as.POSIXct(NA), df2$`Fecha sospecha clínica`, df2$`Fecha resultado FA D1`, df2$`Fecha resultado SOC D1`, df2$`Fecha resultado FA D3`, df2$`Fecha resultado SOC D3`),
    label = c("Pre sospecha", "Sospecha", "FA D1", "SOC D1", "FA D3", "SOC D3"),
    fill = c("blue", "darkgreen", "gold", "orange", "pink", "purple")
  )
  
  # Creem funció per ajustar les etiquetes de dates duplicades. Aquesta informació probablement no la posarem ja que amb l'eix d'abaix ha en tenim suficient.
  ajustar_labels <- function(df) {
    df <- df %>%
      group_by(formatted_date = format(as.Date(xintercept), "%d-%m")) %>%
      mutate(label_noms_verticals = ifelse(row_number() == 1, format(as.Date(xintercept), "%d-%m"), "")) %>%
      ungroup() %>%
      select(-formatted_date)
    return(df)
  }
  
  # Apliquem la funció per ajustar les etiquetes. La podem aplicar però no es veurà reflectit al gràfic.
  vertical_lines <- ajustar_labels(vertical_lines)

  # Creem rectangles que delimiten els periodes temporals per a cada pacient. En creem un per cada periode. Els periodes són Pre sospecha, Sospecha, FA D1, SOC D1, FA D3 i SOC D3.
  rect_data_antes_sospecha <- data.frame(
    label = "Pre sospecha",
    # Fem que l'inici sigui menys infinit perquè es vegi que el rectangle arriba al final.
    xmin = as.POSIXct(-Inf, format = "%Y-%m-%d %H:%M"),
    # Fins la data de sospita clínica que aquesta sempre apareix.
    xmax = vertical_lines$xintercept[2],
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[1]
  )
  
  rect_data_sospecha <- data.frame(
    label = "Sospecha",
    # L'inici és la data de sospita clínica.
    xmin = vertical_lines$xintercept[2],
    # El final és qualsevol de les dates següents o bé l'infinit si no hi ha cap més data (cosa impossible). 
    xmax = as.POSIXct(ifelse(!is.na(vertical_lines$xintercept[3]), vertical_lines$xintercept[3], ifelse(!is.na(vertical_lines$xintercept[4]), vertical_lines$xintercept[4], ifelse(!is.na(vertical_lines$xintercept[5]),vertical_lines$xintercept[5] , ifelse(!is.na(vertical_lines$xintercept[6]), vertical_lines$xintercept[6], as.POSIXct(Inf, format = "%Y-%m-%d %H:%M")))))),
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[2] 
  )
  
  # Ens els següents exemples és el mateix, com a xmin tenim la data del propi event, que si no existeix no hi ha rectangle, i el xmax és qualsevol de les dates dels següents events i sinó l'infinit.
  ###Problema: cas que en algun dels pacients no es segueixi l'ordre d'events habitual. Per exemple que SOC D1 sigui més tard que FA D3. 
  rect_data_FA_D1 <- data.frame(
    label = "FA D1",
    xmin = vertical_lines$xintercept[3],
    xmax = as.POSIXct(ifelse(!is.na(vertical_lines$xintercept[4]), vertical_lines$xintercept[4], ifelse(!is.na(vertical_lines$xintercept[5]),vertical_lines$xintercept[5] , ifelse(!is.na(vertical_lines$xintercept[6]), vertical_lines$xintercept[6], as.POSIXct(Inf, format = "%Y-%m-%d %H:%M"))))),
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[3] 
  )
    
  rect_data_SOC_D1 <- data.frame(
    label = "SOC D1",
    xmin = vertical_lines$xintercept[4],
    xmax = as.POSIXct(ifelse(!is.na(vertical_lines$xintercept[5]),vertical_lines$xintercept[5] , ifelse(!is.na(vertical_lines$xintercept[6]), vertical_lines$xintercept[6], as.POSIXct(Inf, format = "%Y-%m-%d %H:%M")))),
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[4] 
  )
    
  rect_data_FA_D3 <- data.frame(
    label = "FA D3",
    xmin = vertical_lines$xintercept[5],
    xmax = as.POSIXct(ifelse(!is.na(vertical_lines$xintercept[6]), vertical_lines$xintercept[6], as.POSIXct(Inf, format = "%Y-%m-%d %H:%M"))),
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[5] 
  )
    
  rect_data_SOC_D3 <- data.frame(
    label = "SOC D3",
    xmin = vertical_lines$xintercept[6],
    xmax = as.POSIXct(Inf, format = "%Y-%m-%d %H:%M"),
    ymin = -Inf,
    ymax = Inf,
    fill = vertical_lines$fill[6]
  )

  # Creem el gràfic. Seleccionem les x d'inici (data d'inici del tractament) i de final (data final tractament), la distribució de l'eix de les y en funció del tractament ordenat per data d'inici de tractament i definim els colors de les barres creades en funció de la via d'administració.
  p <- ggplot(df, aes(x = `Fecha inicial tratamiento`, xend = `Fecha final tratamiento`, 
                      y = reorder(Tratamiento, desc(`Fecha inicial tratamiento`)), 
                      yend = reorder(Tratamiento, desc(`Fecha inicial tratamiento`)), 
                      color = `Administración`)) +
    xlab(NULL) + ylab(NULL) +
    # Afegim el títol de cada gràfic.
    ggtitle(paste0("Diagrama de tratamientos del paciente ", unique(df$`Identificación del paciente`))) +   
    # Afegim un color per a cada via d'administració.
    scale_color_manual(values = c("Endovenoso" = "black", "Nebulizado" = "#555555", "Oral" = "#C6C6C6", "Tópico" = "#F2F2F2")) + 
    # Creem una llegenda per definir els events.
    scale_fill_identity(name = "Eventos", guide = "legend", labels = c("Pre sospecha", "Sospecha", "FA D1", "SOC D1", "FA D3", "SOC D3")) +
    # Afegim les línies verticals que defineixen les dates dels events.
    geom_vline(data = vertical_lines, aes(xintercept = xintercept), linetype = "solid", size = 1, color = "black", alpha = 0) +
    # Eliminem de moment la línia que definia la data exacta de l'event.
    # geom_text(data = vertical_lines, aes(x = xintercept, y = Inf, label = label_noms_verticals, color = fill), vjust = 1.5, hjust = 0, size = 2.5, inherit.aes = FALSE) +
    # Definim tots els rectangles del fons del gràfic que defineixen l'inici i final dels events.
    geom_rect(data = rect_data_antes_sospecha, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_rect(data = rect_data_sospecha, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_rect(data = rect_data_FA_D1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_rect(data = rect_data_SOC_D1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_rect(data = rect_data_FA_D3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_rect(data = rect_data_SOC_D3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
    geom_segment(size = 6) +
    # Establim el límit de les dates del gràfic, canviem format dates inferior i les fem aparèixer cada dos dies.
    scale_x_datetime(limits = xlim_range, date_breaks = "2 days", date_labels = "%d-%m") +
    # Fem aparèixer la llegenda a la dreta.
    theme(legend.position = "right",
          # Fem el gràfic lo més ample possible.
          panel.spacing = unit(2, "lines")) +
  theme_minimal()
  
  return(p)
}

# Apliquem la funció a tots els pacients.
lista_graficos_estancia <- lapply(pacients_per_gantt, generar_grafic_estancia)
lista_graficos_estancia

# Creem la carpeta per guardar els gràfics dins del WD.
output_directory <- "static-charts/output_gràfics"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Funció per guardar els gràfics.
guardar_grafic_estancia <- function(df, output_directory) {
  p <- generar_grafic_estancia(df)
  patient_id <- unique(df$`Identificación del paciente`)
  file_name <- paste0(output_directory, "/Gantt_Paciente_", patient_id, ".png")
  ggsave(filename = file_name, plot = p, width = 9, height = 5, units = "in", dpi = 300)
}

# Guardem tots els gràfics a la carpeta.
lapply(pacients_per_gantt, guardar_grafic_estancia, output_directory = output_directory)