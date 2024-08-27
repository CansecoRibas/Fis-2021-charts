#!/bin/bash

# Ruta al directori del projecte. Seleccionar la que toqui en cada cas. En un futur caldria unificar-les.
# PROJECT_DIR="C:/Users/joanc/OneDrive/Documents/Joan/Programació i estadística/Visual Studio Code/Redcap_Gantt-Charts" # Casa
PROJECT_DIR="C:/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code/Redcap-Gantt-Charts" # Clínic
# PROJECT_DIR="C:/Users/motosgalera-a/Downloads/codi/Redcap-Gantt_Charts" # Anna

# Canviar al directori del projecte.
cd "$PROJECT_DIR"

# Executar el script de R per generar i guardar els gràfics a la carpeta "output_gràfics".
echo "Executant script de R."
Rscript codi_generar_gràfics_gantt.R
echo "Script de R executat."

# Afegir els canvis.
echo "Afegint canvis a Git."
git add output_gràfics/
echo "Canvis a Git afegits."

# Fer el commit de l'actualització.
echo "Fent commit."
git commit -m "Actualització gràfics dels pacients en data $(date +'%Y-%m-%d %H:%M:%S')"
echo "Commit realitzat."

# Enviar els canvis al repositori remot.
echo "Realitzant push al repositori."
git push origin main
echo "Push realitzat."