# Establir working directory. Triar segons on s'estigui executant. En el futur haurem de mirar una manera per evitar fer aquest pas previ.
# setwd("C:/Users/joanc/OneDrive/Documents/Joan/Programació i estadística/Visual Studio Code") # Casa
setwd("C:/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code/Redcap-Gantt-Charts") # Clínic
# setwd("C:/Users/motosgalera-a/Downloads/codi") # Anna

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

# Ara ens centrarem en crear els missatges pel gràfics interactius. Primer separem el document dels resultats en 5 dfs.
VAT_o_VAP <- resultats_proves %>% 
  select(1,3)

`Resultado del FA D1` <- resultats_proves %>%
  select(1, 4:56)

`Resultado del SOC D1` <- resultats_proves %>% 
  select(1, 57:273) %>% 
  mutate(`Número de patógenos detectados` = ifelse(`Número de patógenos detectados` == "4 o más", 4, `Número de patógenos detectados`))

`Resultado del FA D3` <- resultats_proves %>% 
  select(1, 274:326)

`Resultado del SOC D3` <- resultats_proves %>% 
  select(1, 327:543) %>% 
  mutate(`Número de patógenos detectados` = case_when(
    `Número de patógenos detectados` == "0" ~ "0",
    `Número de patógenos detectados` == "1" ~ "1",
    `Número de patógenos detectados` == "2" ~ "2",
    `Número de patógenos detectados` == "3" ~ "3",
    `Número de patógenos detectados` == "4 o más" ~ "4",
    is.na(`Número de patógenos detectados`) ~ ""
  ))

# Ara crearem funcions per resumir la informació de cada apartat. Comencem per la més senzilla que és una funció per determinar si es tracta d'una VAT o d'una VAP.
resumir_informació_vap_o_vat <- function(df) {
  df2 <- df %>% 
    mutate(Informació_vap_o_vat = paste("Sospecha de", `VAT o VAP`)) %>% 
    select(-`VAT o VAP`)
  
  return(df2)
}

  # Apliquem la funció.
  resum_VAT_o_VAP <- resumir_informació_vap_o_vat(VAT_o_VAP)

# Continuem creant la següent funció per resumir les dades del FA i les apliquem al FA del D1.
resumir_informació_FA <- function(df) {
  # Utilitzem rowwise per avaluar cada fila de manera individual.
  df <- df %>%
    rowwise() %>%
    mutate(
      # Crear la columna 'Informació_FA' que serà la columna resum i en cas de que no es detectin patògens generar un missatge.
      Patógenos_FA = ifelse(`Patógenos detectados` == "No",
                             "El resultado del FA es: Negativo.", ""),
      
      # Crear la columna 'Patógenos_FA' per recollir quins patògens s'han recollit.
      Tipo_patógenos_FA = {
        # Primer obtenim els noms de les columnes que tenen l'estructura "Tipo de patógenos detectados (choice=)".
        cols_patogenos <- grep("^Tipo de patógenos detectados \\(choice=", names(df), value = TRUE)
        # Obtenim els noms de les columnes que tenen l'estructura "Carga de".
        cols_carga <- grep("^Carga de ", names(df), value = TRUE)
        
        # Inicialitzem un vector per emmagatzemar els patògens detectats.
        patogenos_detectados <- c()
        
        # Iterem sobre les columnes dels patògens detectats.
        for (col_name in cols_patogenos) {
          patogeno <- str_extract(col_name, "(?<=\\(choice=).+?(?=\\))")  # Per extraure els noms dels patògens.
          if (cur_data()[[col_name]] == "Checked") {
            
            # Afegim el patogen detectat juntament amb el número que li pertoca.
            patogenos_detectados <- c(patogenos_detectados, paste0("Patógeno ", length(patogenos_detectados) + 1, ": ", patogeno))
            
            # Verifiquem si té una columna de càrrega asociada (si té el mateix patogen).
            carga_col <- paste0("Carga de ", patogeno)
            if (carga_col %in% cols_carga && cur_data()[[carga_col]] != "") {
              # Si la té, afegim la càrrega darrere del patogen.
              patogenos_detectados[length(patogenos_detectados)] <- paste0(
                patogenos_detectados[length(patogenos_detectados)],
                "\nCarga: ", cur_data()[[carga_col]], " copias bin/mL"
              )
            }
          }
        }
        
        # Unim tots els patògens detectats en una sola string separada per salts de línia.
        if (length(patogenos_detectados) > 0) {
          paste(patogenos_detectados, collapse = "\n\n")
        } else {
          NA
        }
      },
      
      # Afegim una columna de resistencies per recollir la informació quan no hi ha resistències en el resultat.
      Resistencias_FA = ifelse(`Genes de resistencia detectados` == "No", "No se detectan resistencias.", ""),
      
      # Crear la columna 'Tipo_resistencia_FA' per recollir les resistències detectades.
      Tipo_resistencia_FA = {
        
        # Obtenim els noms de les columnes amb estructura "Tipo de genes de resistencia (choice=)".
        cols_resistencia <- grep("^Tipo de genes de resistencia \\(choice=", names(df), value = TRUE)
        
        # Inicialitzem un vector per emmagatzemar les resistències detectades.
        resistencias_detectadas <- c()
        
        # Iterem sobre les columnes de resistències.
        for (col_name in cols_resistencia) {
          resistencia <- str_extract(col_name, "(?<=\\(choice=).+?(?=\\))")  # Extrau el nom de la resistència.
          
          if (cur_data()[[col_name]] == "Checked") {  
            
            # Afegim la resistència detectada juntament amb el número associat.
            resistencias_detectadas <- c(resistencias_detectadas, paste0("Resistencia ", length(resistencias_detectadas) + 1, ": ", resistencia))
          }
        }
        
        # Unim totes les resistències detectades en un sol string separat per salts de línia.
        if (length(resistencias_detectadas) > 0) {
          paste(resistencias_detectadas, collapse = "\n")
        } else {
          NA
        }
      }
    ) %>%
    ungroup() %>%
    
    # Seleccionem només les columnes d'interés.
    select(`ID del paciente`, Patógenos_FA, Tipo_patógenos_FA, Resistencias_FA, Tipo_resistencia_FA) %>%
    
    # Unifiquem tota la informació en la columna Informació_FA.
    mutate(Informació = ifelse(Patógenos_FA == "", paste0("El resultado del FA es:\n\n", Tipo_patógenos_FA, "\n\n", ifelse(Resistencias_FA == "No se detectan resistencias.", Resistencias_FA, Tipo_resistencia_FA)), Patógenos_FA))
  
  return(df)
}

  # Apliquem la funció.
  resum_FA_D1 <- resumir_informació_FA(`Resultado del FA D1`)

# Continuem amb la funció que resumeix les dades del SOC i les apliquem al SOC D1.
resumir_informació_SOC <- function(df) {
  # Usamos rowwise para evaluar cada fila individualmente
  df <- df %>%
    rowwise() %>%
    mutate(
      # Inicializamos las columnas de información
      Información = ifelse(`Número de patógenos detectados` == 0, 
                           "El resultado del SOC es: Negativo", 
                           NA_character_),
      
      # Para manejar los casos con diferentes números de patógenos
      `Información patógeno 1` = if(`Número de patógenos detectados` >= 1) {
        # Extraemos la fila actual para usar cur_data()
        cur_row <- cur_data()
        # Verificamos la concentración
        conc_p1 <- cur_row$`Concentración de patógeno 1`
        if (str_detect(conc_p1, "\\d")) { # Si contiene un número
          conc_p1 <- paste0(conc_p1, " UFC/mL")
        }
        # Recogemos la información del patógeno 1
        info_p1 <- paste0("Patógeno 1: ", cur_row$`Nombre de patógeno 1`, 
                          "\nConcentración: ", conc_p1)
        
        # Resistencia intermedia patógeno 1
        if (cur_row$`Resistencia intermedia patógeno 1` == "No") {
          info_p1 <- paste0(info_p1, "\nResistencia intermedia: No")
        } else if (cur_row$`Resistencia intermedia patógeno 1` == "No procede") {
          info_p1 <- paste0(info_p1, "\nResistencia intermedia: No procede")
        } else if (cur_row$`Resistencia intermedia patógeno 1` == "Sí") {
          # Obtener los tipos de resistencia intermedia
          tipos_res_intermedia <- grep("^Tipo de resistencia/s intermedia patógeno 1 \\(choice=", names(cur_row), value = TRUE)
          resistencias_intermedia <- tipos_res_intermedia %>%
            keep(~ cur_row[[.x]] == "Checked") %>% # Filtrar solo las "Checked"
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                # Si es "Otros", tomar el texto de la columna "Especificar..."
                return(cur_row$`Especificar el tipo de resistencia/s intermedia del patógeno 1`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>% # Eliminar entradas vacías
            paste(collapse = ", ")
          info_p1 <- paste0(info_p1, "\nResistencia intermedia: ", resistencias_intermedia)
        }
        
        # Resistencia patógeno 1
        if (cur_row$`Resistencia patógeno 1` == "No") {
          info_p1 <- paste0(info_p1, "\nResistencia: No")
        } else if (cur_row$`Resistencia patógeno 1` == "No procede") {
          info_p1 <- paste0(info_p1, "\nResistencia: No procede")
        } else if (cur_row$`Resistencia patógeno 1` == "Sí") {
          # Obtener los tipos de resistencia
          tipos_res <- grep("^Tipo de resistencia/s patógeno 1 \\(choice=", names(cur_row), value = TRUE)
          resistencias <- tipos_res %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s del patógeno 1`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p1 <- paste0(info_p1, "\nResistencia: ", resistencias)
        }
        info_p1
      } else { 
        NA_character_
      },
      
      # Repetir el proceso para los demás patógenos si hay más de uno
      `Información patógeno 2` = if(`Número de patógenos detectados` >= 2) {
        cur_row <- cur_data()
        conc_p2 <- cur_row$`Concentración de patógeno 2`
        if (str_detect(conc_p2, "\\d")) {
          conc_p2 <- paste0(conc_p2, " UFC/mL")
        }
        info_p2 <- paste0("Patógeno 2: ", cur_row$`Nombre de patógeno 2`, 
                          "\nConcentración: ", conc_p2)
        # Resistencia intermedia patógeno 2
        if (cur_row$`Resistencia intermedia patógeno 2` == "No") {
          info_p2 <- paste0(info_p2, "\nResistencia intermedia: No")
        } else if (cur_row$`Resistencia intermedia patógeno 2` == "No procede") {
          info_p2 <- paste0(info_p2, "\nResistencia intermedia: No procede")
        } else if (cur_row$`Resistencia intermedia patógeno 2` == "Sí") {
          tipos_res_intermedia <- grep("^Tipo de resistencia/s intermedia patógeno 2 \\(choice=", names(cur_row), value = TRUE)
          resistencias_intermedia <- tipos_res_intermedia %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s intermedia del patógeno 2`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p2 <- paste0(info_p2, "\nResistencia intermedia: ", resistencias_intermedia)
        }
        
        # Resistencia patógeno 2
        if (cur_row$`Resistencia patógeno 2` == "No") {
          info_p2 <- paste0(info_p2, "\nResistencia: No")
        } else if (cur_row$`Resistencia patógeno 2` == "No procede") {
          info_p2 <- paste0(info_p2, "\nResistencia: No procede")
        } else if (cur_row$`Resistencia patógeno 2` == "Sí") {
          tipos_res <- grep("^Tipo de resistencia/s patógeno 2 \\(choice=", names(cur_row), value = TRUE)
          resistencias <- tipos_res %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s del patógeno 2`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p2 <- paste0(info_p2, "\nResistencia: ", resistencias)
        }
        info_p2
      } else { 
        NA_character_
      },
      
      # Ara fem el mateix pel patogen 3.
      `Información patógeno 3` = if(`Número de patógenos detectados` >= 3) {
        cur_row <- cur_data()
        conc_p3 <- cur_row$`Concentración de patógeno 3`
        if (str_detect(conc_p3, "\\d")) {
          conc_p3 <- paste0(conc_p3, " UFC/mL")
        }
        info_p3 <- paste0("Patógeno 3: ", cur_row$`Nombre de patógeno 3`, 
                          "\nConcentración: ", conc_p3)
        # Resistencia intermedia patógeno 3
        if (cur_row$`Resistencia intermedia patógeno 3` == "No") {
          info_p3 <- paste0(info_p3, "\nResistencia intermedia: No")
        } else if (cur_row$`Resistencia intermedia patógeno 3` == "No procede") {
          info_p3 <- paste0(info_p3, "\nResistencia intermedia: No procede")
        } else if (cur_row$`Resistencia intermedia patógeno 3` == "Sí") {
          tipos_res_intermedia <- grep("^Tipo de resistencia/s intermedia patógeno 3 \\(choice=", names(cur_row), value = TRUE)
          resistencias_intermedia <- tipos_res_intermedia %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s intermedia del patógeno 3`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p3 <- paste0(info_p3, "\nResistencia intermedia: ", resistencias_intermedia)
        }
        
        # Resistencia patógeno 3
        if (cur_row$`Resistencia patógeno 3` == "No") {
          info_p3 <- paste0(info_p3, "\nResistencia: No")
        } else if (cur_row$`Resistencia patógeno 3` == "No procede") {
          info_p3 <- paste0(info_p3, "\nResistencia: No procede")
        } else if (cur_row$`Resistencia patógeno 3` == "Sí") {
          tipos_res <- grep("^Tipo de resistencia/s patógeno 3 \\(choice=", names(cur_row), value = TRUE)
          resistencias <- tipos_res %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s del patógeno 3`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p3 <- paste0(info_p3, "\nResistencia: ", resistencias)
        }
        info_p3
      } else { 
        NA_character_
      },
      
      # Finalment pel patogen 4.
      `Información patógeno 4` = if(`Número de patógenos detectados` == 4) {
        cur_row <- cur_data()
        conc_p4 <- cur_row$`Concentración de patógeno 4`
        if (str_detect(conc_p4, "\\d")) {
          conc_p4 <- paste0(conc_p4, " UFC/mL")
        }
        info_p4 <- paste0("Patógeno 4: ", cur_row$`Nombre de patógeno 4`, 
                          "\nConcentración: ", conc_p4)
        # Resistencia intermedia patógeno 4
        if (cur_row$`Resistencia intermedia patógeno 4` == "No") {
          info_p4 <- paste0(info_p4, "\nResistencia intermedia: No")
        } else if (cur_row$`Resistencia intermedia patógeno 4` == "No procede") {
          info_p4 <- paste0(info_p4, "\nResistencia intermedia: No procede")
        } else if (cur_row$`Resistencia intermedia patógeno 4` == "Sí") {
          tipos_res_intermedia <- grep("^Tipo de resistencia/s intermedia patógeno 4 \\(choice=", names(cur_row), value = TRUE)
          resistencias_intermedia <- tipos_res_intermedia %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s intermedia del patógeno 4`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p4 <- paste0(info_p4, "\nResistencia intermedia: ", resistencias_intermedia)
        }
        
        # Resistencia patógeno 4
        if (cur_row$`Resistencia patógeno 4` == "No") {
          info_p4 <- paste0(info_p4, "\nResistencia: No")
        } else if (cur_row$`Resistencia patógeno 4` == "No procede") {
          info_p4 <- paste0(info_p4, "\nResistencia: No procede")
        } else if (cur_row$`Resistencia patógeno 4` == "Sí") {
          tipos_res <- grep("^Tipo de resistencia/s patógeno 4 \\(choice=", names(cur_row), value = TRUE)
          resistencias <- tipos_res %>%
            keep(~ cur_row[[.x]] == "Checked") %>%
            map_chr(~ {
              if (str_detect(.x, "\\(choice=Otros\\)")) {
                return(cur_row$`Especificar el tipo de resistencia/s del patógeno 4`)
              } else {
                return(str_extract(.x, "(?<=\\(choice=).+?(?=\\))"))
              }
            }) %>%
            discard(~ . == "") %>%
            paste(collapse = ", ")
          info_p4 <- paste0(info_p4, "\nResistencia: ", resistencias)
        }
        info_p4
      } else { 
        NA_character_
      }
    ) %>%
    ungroup() %>% 
    select(`ID del paciente`, `Número de patógenos detectados`, Información, `Información patógeno 1`, `Información patógeno 2`, `Información patógeno 3`, `Información patógeno 4`) %>% 
  
    # Unifiquem tota la informació en la columna Informació_FA.
    mutate(Información = case_when(`Número de patógenos detectados` == 0 ~ Información,
                                   `Número de patógenos detectados` == 1 ~ paste0("El resultado del SOC es:\n\n", `Información patógeno 1`),
                                   `Número de patógenos detectados` == 2 ~ paste0("El resultado del SOC es:\n\n", `Información patógeno 1`, "\n\n", `Información patógeno 2`),
                                   `Número de patógenos detectados` == 3 ~ paste0("El resultado del SOC es:\n\n", `Información patógeno 1`, "\n\n", `Información patógeno 2`, "\n\n", `Información patógeno 3`),
                                   `Número de patógenos detectados` == 4 ~ paste0("El resultado del SOC es:\n\n", `Información patógeno 1`, "\n\n", `Información patógeno 2`, "\n\n", `Información patógeno 3`, "\n\n", `Información patógeno 4`)
                                   )) %>% 
    select(`ID del paciente`, Información)
    
  return(df)
}

  # Apliquem la funció.
  resum_SOC_D1 <- resumir_informació_SOC(`Resultado del SOC D1`)

# Finalment apliquem les funcions que hem creat de FA i SOC pels D3.
  # Apliquem la funció.
  resum_FA_D3 <- resumir_informació_FA(`Resultado del FA D3`)
  
  # Apliquem la funció.
  resum_SOC_D3 <- resumir_informació_SOC(`Resultado del SOC D3`)

# Ara guardem tots els resums en el df que utilitzarem per crear els gràfics. Ho farem mitjançant els identificadors dels pacients.
dates_VAPVAT_interactiu <- dates_VAPVAT %>% 
  
  # Copiem la columna de VAT/VAP.
  left_join(resum_VAT_o_VAP %>% select(`ID del paciente`, Informació_vap_o_vat),
            by = c("Identificación del paciente" = "ID del paciente"), 
            relationship = "many-to-many") %>%
  
  # Copiem la columna de FA D1. 
  left_join(resum_FA_D1 %>% select(`ID del paciente`, Informació), 
            by = c("Identificación del paciente" = "ID del paciente"), 
            relationship = "many-to-many") %>% 
  mutate(`Informació FA D1` = Informació) %>% 
  select(-Informació) %>% 
  
  # Copiem la columna de SOC D1.
  left_join(resum_SOC_D1 %>% select(`ID del paciente`, Información), 
            by = c("Identificación del paciente" = "ID del paciente"), 
            relationship = "many-to-many") %>% 
  mutate(`Informació SOC D1` = Información) %>% 
  select(-Información) %>% 
  
  # Copiem la columna de FA D3.
  left_join(resum_FA_D3 %>% select(`ID del paciente`, Informació), 
            by = c("Identificación del paciente" = "ID del paciente"), 
            relationship = "many-to-many") %>% 
  mutate(`Informació FA D3` = Informació) %>% 
  select(-Informació) %>% 
  
  # Copiem la columna de SOC D3.
  left_join(resum_SOC_D3 %>% select(`ID del paciente`, Información), 
            by = c("Identificación del paciente" = "ID del paciente"), 
            relationship = "many-to-many") %>% 
  mutate(`Informació SOC D3` = Información) %>% 
  select(-Información)
  
# Creem una llista buida per guardar tots els pacients.
pacients_per_gantt <- list()

# Iterem sobre cada identificació única del pacient.
for (i in unique(dates_VAPVAT_interactiu$`Identificación del paciente`)) {
  # Agrupem els pacients per identificació i guardem els resultats a la llista.
  pacients_per_gantt[[paste0("Gantt_", i)]] <- filter(dates_VAPVAT_interactiu, `Identificación del paciente` == i)
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
  
  # Fem el mateix per les descripcions dels gràfics interactius.
  df3 <- df %>% 
    select(`Identificación del paciente`, Informació_vap_o_vat, `Informació FA D1`, `Informació SOC D1`, `Informació FA D3`, `Informació SOC D3`) %>%
    distinct() %>% 
    arrange(`Identificación del paciente`)
  
  # Creem un df amb les dates importants del gràfic, les etiquetes que tenen i els colors associats.
  vertical_lines <- data.frame(
    xintercept = c(as.POSIXct(NA), df2$`Fecha sospecha clínica`, df2$`Fecha resultado FA D1`, df2$`Fecha resultado SOC D1`, df2$`Fecha resultado FA D3`, df2$`Fecha resultado SOC D3`),
    label = c("Pre sospecha", "Sospecha", "FA D1", "SOC D1", "FA D3", "SOC D3"),
    fill = c("blue", "darkgreen", "gold", "orange", "pink", "purple"), # Els colors han d'anar per ordre alfabètic perquè funcioni bé la llegenda.
    info = c(NA, paste(df3$Informació_vap_o_vat), df3$`Informació FA D1`, df3$`Informació SOC D1`, df3$`Informació FA D3`, df3$`Informació SOC D3`)
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
    ggiraph::geom_vline_interactive(data = vertical_lines, aes(xintercept = xintercept, tooltip = info, data_id = label), linetype = "solid", size=1, color = "black", alpha = 0) +
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
  
  # Crear el directori de sortida si no existeix.
  output_directory <- "dynamic-charts/docs"
  if (!dir.exists(output_directory)) {
    dir.create(output_directory)
  }
  
  # Establir el nom del fitxer utilitzant l'identificador del pacient.
  patient_id <- unique(df$`Identificación del paciente`)
  file_name <- paste0("/Gantt_Paciente_", patient_id, ".html")
  
  # Establim el gràfic com a plot_interactiu.
  plot_interactiu <- girafe(ggobj = p)
  
  # Guardem el gràfic interactiu com a HTML.
  saveWidget(plot_interactiu, file.path(output_directory, file_name))
  
  # També retornem el gràfic interactiu per si es vol mostrar a l'instant.
  return(plot_interactiu)
}

# Apliquem la funció a tots els pacients.
lista_graficos_estancia <- lapply(pacients_per_gantt, generar_grafic_estancia)
lista_graficos_estancia