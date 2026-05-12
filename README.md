# README.md

# PediTreeTor Shiny App
Not intended for clinical diagnosis.
For research, teaching and visualization purposes only.


Aplicación interactiva en **R Shiny** para construir y visualizar árboles genealógicos clínicos utilizando el paquete `kinship2`.

La aplicación está orientada a genética clínica y permite:

- Construcción interactiva de pedigríes.
- Añadir individuos por relación familiar respecto a un caso índice.
- Importar familias desde CSV.
- Visualizar información clínica y genética.
- Exportar figuras en PNG y SVG.
- Gestionar variantes genéticas familiares.

---

# Características

## Construcción de familias

La app permite añadir:

- Caso índice
- Padre / madre
- Hermanos
- Hijos
- Tíos paternos y maternos
- Primos paternos y maternos
- Hijos de primos

Los familiares intermedios necesarios se generan automáticamente.

---

## Información genética

Cada individuo puede incluir:

- Fenotipo clínico
- Estado genético:
  - Portador
  - No portador
  - Desconocido
- Una o varias variantes genéticas

Ejemplo:

```text
WFS1 c.1949A>C (p.Tyr650Ser)
GJB2 c.35delG
```

---

## Importación mediante CSV

Formato esperado:

```text
id,label,sex,father,mother,status,carrier,phenotype,variant
```

Ejemplo:

```text
ADR,ADR NHC 170292,1,,,affected,carrier,Sordera,WFS1 c.1949A>C (p.Tyr650Ser)
```

### Variables admitidas

#### sex

- 1 = varón
- 2 = mujer
- 3 = desconocido

#### status

- affected
- unaffected
- unknown

#### carrier

- carrier
- noncarrier
- unknown

También se aceptan equivalentes en castellano:

- afectado
- sano
- portador
- no portador
- desconocido

---

# Instalación

## Requisitos

R >= 4.1

Paquetes necesarios:

```r
install.packages(c("shiny", "DT", "kinship2"))
```

---

# Ejecución

Desde R:

```r
shiny::runApp("app.R")
```

O desde RStudio:

```r
Run App
```

---

# Exportación

La aplicación permite exportar:

- PNG
- SVG
- CSV

El formato SVG es recomendable para edición posterior en:

- Inkscape
- Adobe Illustrator
- PowerPoint

---

# Licencia

Este proyecto está distribuido bajo licencia:

GNU General Public License v3.0

Ver archivo LICENSE.

---

# Autor

Álvaro del Real

Aplicación desarrollada para visualización de pedigríes clínicos y familiares en genética médica.

---

