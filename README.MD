# 📊 Proyecto Estaturas en Clase

Este proyecto es una aplicación interactiva desarrollada en **R con Shiny** que permite a los estudiantes ingresar sus estaturas, almacenarlas en una hoja de cálculo de Google Sheets y visualizar estadísticas y gráficos en tiempo real.

## 🚀 Funcionalidades
- Formulario para ingresar nombre y estatura.
- Conexión con Google Sheets mediante credenciales de servicio.
- Cálculo automático de estadísticas:
  - Media
  - Desviación estándar
  - Máximo y mínimo
- Visualización:
  - Histograma de frecuencias
  - Curva normal superpuesta con eje secundario
- Tabla con todos los datos enviados por los estudiantes.

## 🔗 Aplicación en línea
Puedes acceder a la aplicación desplegada en shinyapps.io en el siguiente enlace:

👉 [Ingreso de Estaturas en Clase](https://yfquke-sebastian-menares.shinyapps.io/estaturas/)

## ⚙️ Tecnologías utilizadas
- [R](https://www.r-project.org/)
- [Shiny](https://shiny.rstudio.com/)
- [googlesheets4](https://googlesheets4.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [dplyr](https://dplyr.tidyverse.org/)

## 📂 Estructura del proyecto
- `app.R` → Código principal de la aplicación Shiny.
- `credenciales.json` → Archivo de credenciales de Google (no se sube al repositorio, está en `.gitignore`).
- `README.md` → Este documento.

## 🛡️ Notas de seguridad
- El archivo `credenciales.json` contiene información sensible y **no debe subirse a GitHub**.
- Se recomienda mantenerlo en `.gitignore` y configurarlo de forma segura en shinyapps.io.

---
