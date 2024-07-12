#!/bin/bash

# Ruta al directori del projecte.
PROJECT_DIR="C:/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code"

# Canviar al directori del projecte.
cd "$PROJECT_DIR"

# Executar el script de R per generar i guardar els gràfics a la carpeta "output_gràfics".
Rscript codi_generar_gràfics_gantt.R

# Afegir els canvis.
git add output_grafics/

# Fer el commit de l'actualització.
git commit -m "Actualització gràfics dels pacients en data $(date +'%Y-%m-%d %H:%M:%S')"

# Enviar els canvis al repositori remot.
git push origin main
