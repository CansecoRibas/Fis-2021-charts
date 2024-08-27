# Establir working directory. Triar segons on s'estigui executant. En el futur haurem de mirar una manera per evitar fer aquest pas previ.
# setwd("C:/Users/joanc/OneDrive/Documents/Joan/Programació i estadística/Visual Studio Code") # Casa
setwd("C:/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code/Redcap-Gantt-Charts") # Clínic
# setwd("C:/Users/motosgalera-a/Downloads/codi") # Anna

# Carregar llibreries d'interès.
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, 
               httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, tibble, openxlsx, gridExtra, reshape2)

# IMPORTACIÓ TRACTAMENT ANTIBIÒTIC I ANTIFÚNGIC

## Importar els documents necessaris: Redcap i tractaments del SAP

# Carregar dades analítiques pacients.
tratamiento_SAP <- import("documents_necessaris/Tratamientos_AMOTOS_31_07.csv")

# Modificar nom de les variables i seleccionar aquelles que ens interessen.
tratamiento_SAP_nombres <- tratamiento_SAP %>% select( # Vigilar format dates!
  nhc = nhc,
  numero_episodio = episodio,
  tratamiento = drug_name,
  fecha_inicio_presc = dia_ini_prescrip,
  hora_inicio_presc = hora_ini_prescrip,
  fecha_final_presc = dia_fin_prescrip,
  hora_final_presc = hora_fin_prescrip,
  fecha_admin = fecha_adm,
  hora_admin = hora_adm,
  via_administracion = via_administracion
)

# Carregar el template del Redcap per posteriorment carregar les dades.
formulari_tractaments_redcap <- import("documents_necessaris/Formulari_tractaments_redcap.csv")

##### Carregar la codificació d'APROU per entendre la via d'administració del raw data.
codificacio_APROU <- import("documents_necessaris/APROUdic.csv")

# Carregar codificació de tractaments per poder posteriorment canviar el nom dels tractaments a la codificació de Redcap numèrica.
codificacio_tractaments <- import("documents_necessaris/Codificación_tratamientos.csv")

# Carregar la classificació de tractaments per poder després assignar a cada tractament si és un antibiòtic, antiviral o antifúngic.
classificacio_tractaments <- import("documents_necessaris/Clasificación_tratamientos.csv")

## Preprocessat

### Selecció dels tractaments d'interès i classificació del tipus de tractament.

# Seleccionar únicament les files de tratamiento_SAP_nombres que continguin els noms que hi ha en classificacio_tractaments.
tratamiento_SAP_atb_atv_atf <- tratamiento_SAP_nombres %>% 
  filter(grepl(paste(classificacio_tractaments$tratamiento, collapse = "|"), tratamiento))

# Creem una nova columna a tratamiento_SAP_atb_atv_atf_abreviado on afegim la classificació del tractament.
tratamiento_SAP_atb_atv_atf_clas <- tratamiento_SAP_atb_atv_atf %>%
  rowwise() %>%
  mutate(`tipo de tratamiento` = {
    tipo = NA_character_
    for (i in seq_along(classificacio_tractaments$tratamiento)) {
      if (grepl(classificacio_tractaments$tratamiento[i], tratamiento, ignore.case = TRUE)) {
        tipo <- classificacio_tractaments$`tipo de tratamiento`[i]
        break
      }
    }
    tipo
  })

### Verificació del número de pacients presents en les dades.
# Mirem els nhcs diferents que hi ha en el document dels tractaments totals.
nhc_amb_tractaments_general <- tratamiento_SAP %>% 
  select(nhc) %>% 
  distinct()
nrow(nhc_amb_tractaments_general)

# Fem el mateix però només pels pacients que tenen tractaments antibiòtics.
nhc_amb_tractaments_atb_atv_atf <- tratamiento_SAP_atb_atv_atf_clas %>% 
  select(nhc) %>% 
  distinct()
nrow(nhc_amb_tractaments_atb_atv_atf)

# Ara mirem els pacients del Redcap en els que coincideixen els nhcs en el tractament total.
ant_id_presents <- nhc_amb_tractaments_general %>% 
  left_join(formulari_tractaments_redcap %>% 
              select(nhc, ant_id), by = c("nhc" = "nhc"))

# El mateix però per tractaments antibiòtics.
ant_id_presents <- nhc_amb_tractaments_atb_atv_atf %>% 
  left_join(formulari_tractaments_redcap %>% 
              select(nhc, ant_id), by = c("nhc" = "nhc"))

# Ara mirem els pacients del Redcap en els que no coincideixen els nhcs en el tractament total.
nhc_ant_id_not_in_dif <- formulari_tractaments_redcap %>%
  anti_join(nhc_amb_tractaments_general, by = "nhc") %>%
  select(nhc, ant_id)

# El mateix però per tractaments antibiòtics.
nhc_ant_id_not_in_dif <- formulari_tractaments_redcap %>%
  anti_join(nhc_amb_tractaments_atb_atv_atf, by = "nhc") %>%
  select(nhc, ant_id)
### Els pacients que es perden en el pas anterior NO tenen tractament antibiotic segons les dades exportades del SAP.

### Verificació dels tractaments presents en la raw data i en la classificació de tractaments.
# Mirem quins tractaments diferents hi ha en la raw data.
#tract_dif_raw_data <- tratamiento_SAP_nombres %>%
#  select(tratamiento) %>%
#  distinct()

# Eliminar espais en blanc al final si ha quedat algun.
#tract_dif_raw_data <-  trimws(tract_dif_raw_data$tratamiento)

# Ara verifiquem quins tractaments diferents hi ha en els tractaments atb, atf i atv.
tract_dif_atb_atf_atv <- tratamiento_SAP_atb_atv_atf_clas %>%
  select(tratamiento) %>%
  distinct()

# Eliminar espais en blanc al final si ha quedat algun.
# tract_dif_atb_atf_atv <-  trimws(tract_dif_atb_atf_atv$tratamiento)

## Recodificació de variables

### Modificació dels valors de via d'administracio, del tractament i del tipus de tractament

# Recodifiquem la via_administracion per tenir valors en format Redcap.
# Creem nou df per emmagatzemar dades.
tratamiento_SAP_recod <- tratamiento_SAP_atb_atv_atf_clas
# Creem nova columna amb els noms recodificats de la via d'administració.
tratamiento_SAP_recod$via_administracion_recod <- codificacio_APROU$DESCR[match(tratamiento_SAP_recod$via_administracion, codificacio_APROU$APROUID)]

# ANNA: Afegeixo la columna de la via d'administració perquè després tornem a utilitzar tratamiento_SAP_atb_atv_atf_clas
tratamiento_SAP_atb_atv_atf_clas$via_administracion_recod <- codificacio_APROU$DESCR[match(tratamiento_SAP_recod$via_administracion, codificacio_APROU$APROUID)]

# Codifiquem la variable tractament amb els codis del Redcap.
# Primer agafem un vector amb els noms dels tractaments amb estructura actuals, un vector de la seva traducció amb les labels del Redcap per comprovar que són coincidents i finalment un vector amb la codificació numèrica. 
nombres_tratamiento_actual <- codificacio_tractaments$`Tratamiento SAP`
nombres_tratamiento_redcap <- codificacio_tractaments$`Tratamiento redcap labels`
nombres_tratamiento_codificado <- codificacio_tractaments$`Codificacion tratamientos`

# Comprovem com queda amb les labels.
tratamiento_nombres_recod <-  tratamiento_SAP_atb_atv_atf_clas %>% #tratamiento_SAP_recod
  rowwise() %>%
  mutate(tratamiento = {
    tract = NA_character_
    for (i in seq_along(codificacio_tractaments$`Tratamiento SAP`)) {
      if (grepl(codificacio_tractaments$`Tratamiento SAP`[i], tratamiento, ignore.case = TRUE)) {
        tract <- codificacio_tractaments$`Tratamiento redcap labels`[i]
        break
      }
    }
    tract
  })


# Ara recodifiquem la via_administracion creant una nova columna que recodifica les vies d'administració en format Redcap.
tratamiento_via_recod <- tratamiento_nombres_recod # Podem posar el tratamiento_nombres_recod.
tratamiento_via_recod$via_administracion_redcap <- codificacio_APROU$REDCAP[match(tratamiento_via_recod$via_administracion_recod, codificacio_APROU$DESCR)]
tratamiento_via_recod$via_administracion_redcap <- factor(tratamiento_via_recod$via_administracion_redcap, levels = c(0, 1, 2, 3, 4), labels = c("Oral", "Nebulizado", "Endovenoso", "Tópico", "Otra"))


# ANNA: Això ja no passa S'ha detectat que hi ha algun tractament sense via d'administració i aquest és sempre la tobramicina neb, que s'entén que la via d'administració és nebulització, per tant: 1.
#tratamiento_via_tob_recod <- tratamiento_via_recod %>% 
#  mutate(via_administracion_redcap = if_else(tratamiento == 140, # 140 és la codificació de la Tobramicina.
#                                       1, # Cofificació nebulització.
#                                       via_administracion_redcap)) # Mantenir el valor tal qual.

############## ANNA: Segur? És important destacar que he considerat una via d'administració que era "Bucal" com a "Oral" per la Nistatina per evitar afegir texto libre.
# Ara afegim una nova columna que en cas que sigui "Otra" (== 4) s'especifiqui la via d'administració en format "texto libre". 
tratamiento_via_otra_recod <- tratamiento_via_recod %>% 
  mutate(especif_via_administracion_recod = ifelse(via_administracion_redcap == 4, via_administracion_recod, NA)) %>% 
  select(-via_administracion_recod)


# Ara codificar el tipus de tractament per passar-lo format Redcap.
tratamiento_tipo_recod <- tratamiento_via_otra_recod
tratamiento_tipo_recod$`tipo de tratamiento` <- factor(tratamiento_tipo_recod$`tipo de tratamiento`, levels=c("Antibiótico", "Antiviral", "Antifúngico"), labels=c(0, 1, 2))


## Selecció de registres vàlids

### Selecció tractaments dins l'episodi, selecció de tractaments únics dins d'epidosi

# Verificar que totes les dates tinguin el mateix format. IMPORTANT: En el document csv les variables estan en format data (%y-%m-%d), si no no funciona!
tratamiento_tipo_recod$fecha_admin <- as.Date(tratamiento_tipo_recod$fecha_admin)
tratamiento_tipo_recod$fecha_final_presc <- as.Date(tratamiento_tipo_recod$fecha_final_presc)
formulari_tractaments_redcap$ant_ingreso_fecha_hosp <- as.Date(formulari_tractaments_redcap$ant_ingreso_fecha_hosp)
formulari_tractaments_redcap$out_alta_hosp_fecha <- as.Date(formulari_tractaments_redcap$out_alta_hosp_fecha)

# Selecció de tractaments que es troben dins del ingrés hospitalari del pacient.
tratamiento_SAP_episodio <- tratamiento_tipo_recod %>%
  left_join(formulari_tractaments_redcap %>% 
              select(nhc, ant_ingreso_fecha_hosp, out_alta_hosp_fecha), 
            by = "nhc", relationship = "many-to-many") %>%
  filter((fecha_admin >= ant_ingreso_fecha_hosp & fecha_admin <= out_alta_hosp_fecha) &
         (fecha_final_presc >= ant_ingreso_fecha_hosp & fecha_final_presc <= out_alta_hosp_fecha)) %>%
  select(-ant_ingreso_fecha_hosp, -out_alta_hosp_fecha)


# Ara en cas que hi hagi varis antibiòtics administrats iguals excepte per les seves dates d'inici i final o iguals totalment, s'eliminaran les files que les seves dates d'inici i final estiguin contingudes dins d'un periode del mateix antibiòtic més llarg o si hi ha files duplicades.
# Primer eliminem els duplicats que és més senzill.
tratamiento_SAP_unico <- distinct(tratamiento_SAP_episodio)

# Ara eliminarem els tractaments iguals continguts dins d'un període més grans.
# Per fer això primer verifiquem que les dates que tenim estan en el format correcte.
tratamiento_SAP_unico <- tratamiento_SAP_unico %>%
  mutate(fecha_admin = as.Date(fecha_admin),
         fecha_final_presc = as.Date(fecha_final_presc))

# Creem una funció per fusionar periodes superposats.
merge_overlaps <- function(df) {
  df <- df[order(df$fecha_admin),]
  results <- data.frame()
  while(nrow(df) > 0) {
    actual <- df[1, ]
    df <- df[-1, ]
    
    # Trobem superposicions, incloent aquelles que comencen el dia després de actual$fecha_final.
    overlaps <- which(df$fecha_admin <= actual$fecha_final_presc + 1)
    while(length(overlaps) > 0) {
      # Extenem el periode actual fins la última fecha_final entre les superposicions.
      actual$fecha_final_presc <- max(actual$fecha_final_presc, max(df[overlaps,]$fecha_final_presc))
      # Actualizem la data d'inici mínima.
      actual$fecha_admin<- min(actual$fecha_admin, min(df[overlaps,]$fecha_admin))
      # Eliminem les files superposades del df.
      df <- df[-overlaps, ]
      # Busquem noves superposicions després d'actualitzar el periode actual.
      overlaps <- which(df$fecha_admin <= actual$fecha_final_presc + 1)
    }
    # Agreguem el periode actual als resultats.
    results <- rbind(results, actual)
  }
  results
}


# Apliquem la funció a les dades.
tratamiento_SAP_periodos <- tratamiento_SAP_unico %>%
  group_by(nhc, tratamiento, via_administracion_redcap, especif_via_administracion_recod) %>%
  group_modify(~merge_overlaps(.x)) %>%
  ungroup() %>% 
  # Finalment seleccionem únicament les columnes que volem.
  select(nhc, tratamiento, via_administracion_redcap, especif_via_administracion_recod, `tipo de tratamiento`, fecha_admin, fecha_final_presc)

# Filtrem també els tractaments que durin un sol dia de manera aïllada i sense continuitat.
tratamiento_bolus_aislado <- tratamiento_SAP_periodos %>%
  group_by(nhc, tratamiento) %>%
  mutate(duracion_dias = as.numeric(difftime(fecha_final_presc, fecha_admin, units = "days"))) %>%
  add_count(name = "total_count") %>% 
  filter(duracion_dias < 2 & total_count == 1) %>%
  ungroup() %>% 
  select(-total_count, -duracion_dias)

# Seleccionem els resultats que no coincideixen amb el df dels bolus aïllats.
tratamiento_SAP_no_bolus <- anti_join(tratamiento_SAP_periodos, tratamiento_bolus_aislado, 
                                      by = c("nhc", "tratamiento", "via_administracion_redcap", "especif_via_administracion_recod", "tipo de tratamiento", "fecha_admin", "fecha_final_presc"))

## Transformació a format Redcap

### Modificació de les columnes en funció del número de registres

# Modificar els noms de les columnes perquè siguin iguals que les del Redcap.
tratamiento_nombres_columna <- tratamiento_SAP_no_bolus %>% 
  rename(t_trat = tratamiento, t_trat_tipo = `tipo de tratamiento`, t_trat_adm = via_administracion_redcap, t_trat_adm_otro = especif_via_administracion_recod, t_trat_fecha_i = fecha_admin, t_trat_fecha_f = fecha_final_presc) %>% 
  # Ordenem els registres per inici de tractament perquè així apareguin primer els tractaments importants per l'estudi.
  arrange(t_trat_fecha_i)

# Fer un group_by per nhc, crear una nova variable per contar el número de tractaments per pacient i modificar els noms de les columnes per poder importar les dades directament.
tratamiento_modificacion_columnas <- tratamiento_nombres_columna %>% 
  group_by(nhc) %>% 
  mutate(n_t_trat = n(),
         index_tratamiento = row_number()) %>% 
  pivot_wider(names_from = index_tratamiento,
              names_glue = "{.value}_{index_tratamiento}",
              values_from = c(t_trat, 
                              t_trat_tipo, 
                              t_trat_adm,
                              t_trat_adm_otro,
                              t_trat_fecha_i, 
                              t_trat_fecha_f)) %>% 
  rename(t_trat = n_t_trat)

# Afegim les variables al formulari redcap per ja tenir preparada la importació.
formulari_tractaments_redcap_curt <- formulari_tractaments_redcap %>% # Retallem el formulari per no duplicar la informació
  select(ant_id, redcap_event_name, nhc)
formulari_tractaments_redcap_junt <- inner_join(formulari_tractaments_redcap_curt, tratamiento_modificacion_columnas, by = "nhc")

# Modificar la columna formulario_complete si hi ha alguna dada de tractaments recollida.
formulari_tractaments_redcap_final <- formulari_tractaments_redcap_junt %>% 
  mutate(tratamiento_complete = ifelse(is.na(t_trat), 0, 2))


## El·laboració gràfics Gantt

### Gràfic periode VAP/VATs
# Carregarem primer les dates importants dels pacients.
dates_estancia_hosp <- read.csv("documents_necessaris/Dates_importants.csv") # Dates importants estància
dates_estancia_hosp$d1_sospecha_fecha_vapvat <- as.Date(dates_estancia_hosp$d1_sospecha_fecha_vapvat)

# Utilitzarem el df previ a la modificació en format Redcap: tractament_noms_columna. Afegim les dates importants en el formulari dels tractaments.
tractament_gantt <- tratamiento_nombres_columna %>%
  left_join(formulari_tractaments_redcap %>% select(nhc, ant_id), join_by(nhc)) %>% 
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
         fecha_x_final = as.POSIXct(`Fecha resultado SOC D1`, format = "%Y-%m-%d %H:%M") + lubridate::days(5)) %>%
  # Partim alguns tractaments en dos perquè no ens ocupin tant d'amplitut en el gràfic.
  mutate(Tratamiento = str_replace_all(Tratamiento, "/", "/ ")) %>% 
  mutate(Tratamiento = str_wrap(Tratamiento, width = 18)) 

# En cas de que no hi hagi resultat de SOC D1, agafem per calcular la data final el resultat de FA D1.
tractament_gantt$fecha_x_final <- ifelse(is.na(tractament_gantt$fecha_x_final),
                                         as.POSIXct(tractament_gantt$`Fecha resultado FA D1`, format = "%Y-%m-%d %H:%M") + lubridate::days(8),
                                         tractament_gantt$fecha_x_final)


# Verifiquem que les dates estan en format data hora.
tractament_gantt$fecha_x_inicio <- as.POSIXct(tractament_gantt$fecha_x_inicio, format = "%Y-%m-%d %H:%M")
tractament_gantt$fecha_x_final <- as.POSIXct(tractament_gantt$fecha_x_final, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha inicial tratamiento` <- as.POSIXct(tractament_gantt$`Fecha inicial tratamiento`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha final tratamiento` <- as.POSIXct(tractament_gantt$`Fecha final tratamiento`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha final tratamiento` <- ifelse(tractament_gantt$`Fecha inicial tratamiento` == tractament_gantt$`Fecha final tratamiento`, tractament_gantt$`Fecha inicial tratamiento` + lubridate::hours(12), tractament_gantt$`Fecha final tratamiento`) ############# Afegim aquesta línia de codi perquè es puguin mostrar els bolus. Quan finalment obtinguem data d'inici i final de tractaments amb hores i minuts ja no caldrà.
tractament_gantt$`Fecha final tratamiento` <- as.POSIXct(tractament_gantt$`Fecha final tratamiento`, format = "%Y-%m-%d %H:%M") # Aquesta tampoc.
tractament_gantt$`Fecha ingreso UCI` <- as.POSIXct(tractament_gantt$`Fecha ingreso UCI`, format = "%Y-%m-%d %H:%M")
tractament_gantt$`Fecha sospecha clínica` <- as.POSIXct(tractament_gantt$`Fecha sospecha clínica`, format = "%Y-%m-%d %H:%M")
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
  xlim_range <- range(c(df$fecha_x_inicio, df$fecha_x_final), na.rm = TRUE)
  
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
    xintercept = c(df2$`Fecha ingreso UCI`, df2$`Fecha sospecha clínica`, df2$`Fecha resultado FA D1`, df2$`Fecha resultado SOC D1`, df2$`Fecha resultado FA D3`, df2$`Fecha resultado SOC D3`),
    label = c("Pre sospecha", "Sospecha", "FA D1", "SOC D1", "FA D3", "SOC D3"),
    fill = c("blue", "darkgreen", "gold", "orange", "pink", "purple") # Els colors han d'anar per ordre alfabètic perquè funcioni bé la llegenda.
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
    geom_vline(data = vertical_lines, aes(xintercept = xintercept), linetype = "dotted", size = 0.5,
               color = vertical_lines$fill) +
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
output_directory <- "output_gràfics"
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
