# El·laboració de gràfics Gantt a partir dels tractaments de pacients amb VAP/VAT.

En aquest repositori hi ha tot el necessari per poder generar els gràfics Gantt de tots els tractaments rebuts pels pacients del FIS durant el periode crític de desenvolupament de la VAP/VAT. Concretament conté: una carpeta anomenada "documents_necessaris" on hi ha tots els inputs per generar els gràfics, un script de R anomenat "codi_generar_gràfics_gantt.R" que a partir dels inputs genera els gràfics, una carpeta anomenada "output_gràfics" on s'emmagatzemen tots els gràfics generats i que està connectada al Redcap del projecte (https://redcap.clinic.cat/redcap_v13.10.4/ProjectSetup/index.php?pid=1121) i un script incomplert encara per realitzar una actualització automàtica dels gràfics cada dues setmanes (utilitzant el "programador de tareas" de Windows).


### Requeriments

Per poder treballar amb aquest material he requerit:
- Creació de compte de GitHub (https://github.com/signup?ref_cta=Sign+up&ref_loc=header+logged+out&ref_page=%2F&source=header-home).
- instal·lació de Virtual Studio Code (https://code.visualstudio.com/download).
    - Dins de Virtual Studio Code he hagut d'iniciar sessió amb el meu usuari de GitHub.
    - Dins de Virtual Studio Code he hagut d'instal·larme les següents extensions (R, GitHub Pull Requests).
- instal·lació de R (https://cran.r-project.org/bin/windows/base/).
    - Dins de R he hagut d'instal·lar el paquet "languageserver" (install.packages("languageserver")).
- intal·lació de Git (https://git-scm.com/download/win)


### Actualitzar directori remot via directori local (només admin)

A partir d'aquí, es copia el directori remot com a repositori local dins de VS Code, es configuren els paths per adequar-los a la disposició local, i es podria executar el scripts dins de Git Bash seguint els següents passos.
- cd "/directori/on/hi/ha/script" (anar al directori que hem creat en localment a partir de copiar el directori remot, en el meu cas la ruta del Clinic és "/c/Users/Jcanseco/Documents/Joan/Programació i estadística/Visual Studio Code".)
- ./actualitzar_gràfics.sh (executar el script (a vegades es queda una mica travat en el moment de penjar les coses al GitHub però ho acaba fent)).

