# app.R
# Aplicación Shiny para construir árboles genealógicos con kinship2
# Permite introducir individuos manualmente, cargar CSV o añadir familiares por parentesco respecto a un caso índice.

library(shiny)
library(DT)
library(kinship2)

empty_family <- function() {
  data.frame(
    id = character(),
    label = character(),
    sex = integer(),
    father = character(),
    mother = character(),
    status = character(),
    carrier = character(),
    phenotype = character(),
    variant = character(),
    visible = logical(),
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
    titlePanel("Árbol genealógico interactivo"),
    sidebarLayout(
      sidebarPanel(
        h4("Importar datos"),
        fileInput("upload_csv", "Cargar CSV", accept = c(".csv")),
        checkboxInput("header_csv", "El CSV tiene encabezados", value = TRUE),
        selectInput(
          "sep_csv",
          "Separador",
          choices = c("Coma" = ",", "Punto y coma" = ";", "Tabulador" = "\t"),
          selected = ","
        ),
        actionButton("load_csv", "Importar CSV"),
        helpText("Columnas esperadas: id, label, sex, father, mother, status, carrier, phenotype, variant. En variant puedes incluir varias variantes separadas por |."),
        hr(),
        
        h4("Caso índice"),
        textInput("proband_id", "ID del caso índice", value = "APG"),
        actionButton("set_proband", "Fijar caso índice"),
        hr(),
        
        h4("Añadir por parentesco"),
        selectInput(
          "relationship",
          "Parentesco respecto al caso índice",
          choices = c(
            "Caso índice" = "proband",
            "Hermano/a" = "sibling",
            "Hijo/a" = "child",
            "Padre" = "father",
            "Madre" = "mother",
            "Tío/a paterno/a" = "paternal_uncle",
            "Primo/a paterno/a" = "paternal_cousin",
            "Hijo/a de primo/a paterno/a" = "child_paternal_cousin",
            "Tío/a materno/a" = "maternal_uncle",
            "Primo/a materno/a" = "maternal_cousin",
            "Hijo/a de primo/a materno/a" = "child_maternal_cousin"
          )
        ),
        textInput("id", "ID", value = ""),
        textInput("label", "Etiqueta visible", value = ""),
        selectInput("sex", "Sexo", choices = c("Varón" = 1, "Mujer" = 2, "Desconocido" = 3)),
        selectInput(
          "status",
          "Fenotipo",
          choices = c(
            "Sano / no afectado" = "unaffected",
            "Afectado" = "affected",
            "Desconocido" = "unknown"
          )
        ),
        selectInput(
          "carrier",
          "Estudio genético",
          choices = c(
            "No estudiado / desconocido" = "unknown",
            "Portador" = "carrier",
            "No portador" = "noncarrier"
          )
        ),
        textInput("phenotype", "Clínica", value = ""),
        textAreaInput(
          "variant",
          "Variante(s) / genotipo",
          value = "",
          rows = 3,
          placeholder = "Ejemplo:\nWFS1 c.1949A>C (p.Tyr650Ser)\nGJB2 c.35delG"
        ),
        actionButton("add_relative", "Añadir familiar"),
        hr(),
        
        h4("Edición manual avanzada"),
        textInput("father", "Padre explícito (ID)", value = ""),
        textInput("mother", "Madre explícita (ID)", value = ""),
        actionButton("add_manual", "Añadir / actualizar manualmente"),
        actionButton("delete", "Eliminar seleccionado"),
        hr(),
        
        actionButton("example_wfs1", "Cargar ejemplo Familia 1 - WFS1"),
        actionButton("clear", "Limpiar todo"),
        br(), br(),
        downloadButton("download_png", "Descargar PNG"),
        downloadButton("download_svg", "Descargar SVG"),
        downloadButton("download_csv", "Descargar CSV")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Árbol", plotOutput("pedigree_plot", height = "700px")),
          tabPanel("Tabla", DTOutput("family_table")),
          tabPanel(
            "Ayuda",
            h4("Idea del modelo"),
            p("kinship2 necesita conocer padre y madre de cada individuo. La app permite introducir familiares por parentesco respecto al caso índice."),
            p("Los familiares necesarios para dibujar la relación, aunque no estén estudiados, pueden aparecer como individuos de fenotipo/genotipo desconocido."),
            h4("Convención visual"),
            p("Símbolo negro: afectado. Portador: estado genético indicado en la etiqueta. ? = estudio de portador no realizado/desconocido.")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(data = empty_family(), selected = NULL, proband = "APG")
    
    add_or_update <- function(row) {
      existing <- which(rv$data$id == row$id)
      if (length(existing) > 0) {
        rv$data[existing, ] <<- row
      } else {
        rv$data <<- rbind(rv$data, row)
      }
    }
    
    make_row <- function(id, label = id, sex = 3, father = "", mother = "",
                         status = "unknown", carrier = "unknown",
                         phenotype = "", variant = "", visible = TRUE) {
      data.frame(
        id = as.character(id),
        label = ifelse(label == "", id, label),
        sex = as.integer(sex),
        father = as.character(father),
        mother = as.character(mother),
        status = as.character(status),
        carrier = as.character(carrier),
        phenotype = as.character(phenotype),
        variant = as.character(variant),
        visible = visible,
        stringsAsFactors = FALSE
      )
    }
    
    ensure_person <- function(id, label = id, sex = 3, visible = TRUE) {
      if (!id %in% rv$data$id) {
        add_or_update(make_row(id = id, label = label, sex = sex, visible = visible))
      }
    }
    
    ensure_proband_parents <- function() {
      pid <- rv$proband
      father_id <- paste0(pid, "_padre")
      mother_id <- paste0(pid, "_madre")
      ensure_person(father_id, "Padre", 1)
      ensure_person(mother_id, "Madre", 2)
      
      idx <- which(rv$data$id == pid)
      if (length(idx) > 0) {
        rv$data$father[idx] <- father_id
        rv$data$mother[idx] <- mother_id
      }
      list(father = father_id, mother = mother_id)
    }
    
    observeEvent(input$set_proband, {
      rv$proband <- input$proband_id
    })
    
    observeEvent(input$load_csv, {
      req(input$upload_csv)
      
      imported <- read.csv(
        input$upload_csv$datapath,
        header = input$header_csv,
        sep = input$sep_csv,
        stringsAsFactors = FALSE,
        na.strings = c("", "NA")
      )
      
      expected <- c("id", "label", "sex", "father", "mother", "status", "carrier", "phenotype", "variant")
      missing_cols <- setdiff(expected, names(imported))
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Faltan columnas:", paste(missing_cols, collapse = ", ")), type = "error", duration = 8)
        return(NULL)
      }
      
      imported <- imported[, expected]
      imported$id <- as.character(imported$id)
      imported$label <- ifelse(is.na(imported$label) | imported$label == "", imported$id, as.character(imported$label))
      imported$sex <- as.integer(imported$sex)
      imported$father <- ifelse(is.na(imported$father), "", as.character(imported$father))
      imported$mother <- ifelse(is.na(imported$mother), "", as.character(imported$mother))
      imported$status <- ifelse(is.na(imported$status) | imported$status == "", "unknown", as.character(imported$status))
      imported$carrier <- ifelse(is.na(imported$carrier) | imported$carrier == "", "unknown", as.character(imported$carrier))
      imported$phenotype <- ifelse(is.na(imported$phenotype), "", as.character(imported$phenotype))
      imported$variant <- ifelse(is.na(imported$variant), "", as.character(imported$variant))
      imported$variant <- gsub("\\s*\\|\\s*", "\n", imported$variant)
      imported$visible <- TRUE
      
      imported$status <- tolower(imported$status)
      imported$status <- ifelse(imported$status %in% c("afectado", "affected"), "affected", imported$status)
      imported$status <- ifelse(imported$status %in% c("sano", "no afectado", "unaffected"), "unaffected", imported$status)
      imported$status <- ifelse(imported$status %in% c("desconocido", "unknown", ""), "unknown", imported$status)
      
      imported$carrier <- tolower(imported$carrier)
      imported$carrier <- ifelse(imported$carrier %in% c("portador", "carrier"), "carrier", imported$carrier)
      imported$carrier <- ifelse(imported$carrier %in% c("no portador", "noncarrier", "no_portador"), "noncarrier", imported$carrier)
      imported$carrier <- ifelse(imported$carrier %in% c("desconocido", "unknown", "no estudiado", ""), "unknown", imported$carrier)
      
      valid_status <- c("affected", "unaffected", "unknown")
      valid_carrier <- c("carrier", "noncarrier", "unknown")
      
      if (any(!imported$status %in% valid_status)) {
        showNotification("Hay valores no válidos en status.", type = "error", duration = 8)
        return(NULL)
      }
      
      if (any(!imported$carrier %in% valid_carrier)) {
        showNotification("Hay valores no válidos en carrier.", type = "error", duration = 8)
        return(NULL)
      }
      
      rv$data <- imported
      showNotification("CSV importado correctamente", type = "message", duration = 5)
    })
    
    observeEvent(input$add_relative, {
      req(input$id)
      rv$proband <- input$proband_id
      pid <- rv$proband
      
      rel <- input$relationship
      id <- input$id
      label <- ifelse(input$label == "", input$id, input$label)
      father <- ""
      mother <- ""
      
      if (rel == "proband") {
        rv$proband <- id
        updateTextInput(session, "proband_id", value = id)
      }
      
      needs_parents <- c(
        "sibling", "child", "father", "mother",
        "paternal_uncle", "paternal_cousin", "child_paternal_cousin",
        "maternal_uncle", "maternal_cousin", "child_maternal_cousin"
      )
      
      if (rel %in% needs_parents) {
        ensure_person(pid, pid, 3)
        parents <- ensure_proband_parents()
      }
      
      if (rel == "sibling") {
        father <- parents$father
        mother <- parents$mother
      }
      
      if (rel == "child") {
        if (input$sex == 1) {
          father <- pid
          mother <- paste0(id, "_otro_progenitor")
          ensure_person(mother, "Otro progenitor", 2)
        } else {
          mother <- pid
          father <- paste0(id, "_otro_progenitor")
          ensure_person(father, "Otro progenitor", 1)
        }
      }
      
      if (rel == "father") {
        id <- parents$father
        label <- "Padre"
      }
      
      if (rel == "mother") {
        id <- parents$mother
        label <- "Madre"
      }
      
      if (rel == "paternal_uncle") {
        id <- paste0(pid, "_tio_paterno")
        label <- ifelse(input$label == "", "Tío/a paterno/a", input$label)
        father <- parents$father
        mother <- parents$mother
      }
      
      if (rel == "paternal_cousin") {
        uncle <- paste0(pid, "_tio_paterno")
        aunt_partner <- paste0(pid, "_pareja_tio_paterno")
        ensure_person(uncle, "Tío/a paterno/a", 3)
        ensure_person(aunt_partner, "Pareja tío/a paterno/a", 3)
        idx_uncle <- which(rv$data$id == uncle)
        rv$data$father[idx_uncle] <- parents$father
        rv$data$mother[idx_uncle] <- parents$mother
        father <- uncle
        mother <- aunt_partner
      }
      
      if (rel == "child_paternal_cousin") {
        cousin <- paste0(pid, "_primo_paterno")
        cousin_partner <- paste0(id, "_otro_progenitor")
        uncle <- paste0(pid, "_tio_paterno")
        aunt_partner <- paste0(pid, "_pareja_tio_paterno")
        ensure_person(uncle, "Tío/a paterno/a", 3)
        ensure_person(aunt_partner, "Pareja tío/a paterno/a", 3)
        ensure_person(cousin, "Primo/a paterno/a", 3)
        ensure_person(cousin_partner, "Otro progenitor", 3)
        idx_uncle <- which(rv$data$id == uncle)
        rv$data$father[idx_uncle] <- parents$father
        rv$data$mother[idx_uncle] <- parents$mother
        idx_cousin <- which(rv$data$id == cousin)
        rv$data$father[idx_cousin] <- uncle
        rv$data$mother[idx_cousin] <- aunt_partner
        father <- cousin_partner
        mother <- cousin
      }
      
      if (rel == "maternal_uncle") {
        id <- paste0(pid, "_tio_materno")
        label <- ifelse(input$label == "", "Tío/a materno/a", input$label)
        father <- parents$father
        mother <- parents$mother
      }
      
      if (rel == "maternal_cousin") {
        uncle <- paste0(pid, "_tio_materno")
        uncle_partner <- paste0(pid, "_pareja_tio_materno")
        ensure_person(uncle, "Tío/a materno/a", 3)
        ensure_person(uncle_partner, "Pareja tío/a materno/a", 3)
        idx_uncle <- which(rv$data$id == uncle)
        rv$data$father[idx_uncle] <- parents$father
        rv$data$mother[idx_uncle] <- parents$mother
        father <- uncle
        mother <- uncle_partner
      }
      
      if (rel == "child_maternal_cousin") {
        cousin <- paste0(pid, "_primo_materno")
        cousin_partner <- paste0(id, "_otro_progenitor")
        uncle <- paste0(pid, "_tio_materno")
        uncle_partner <- paste0(pid, "_pareja_tio_materno")
        ensure_person(uncle, "Tío/a materno/a", 3)
        ensure_person(uncle_partner, "Pareja tío/a materno/a", 3)
        ensure_person(cousin, "Primo/a materno/a", 3)
        ensure_person(cousin_partner, "Otro progenitor", 3)
        idx_uncle <- which(rv$data$id == uncle)
        rv$data$father[idx_uncle] <- parents$father
        rv$data$mother[idx_uncle] <- parents$mother
        idx_cousin <- which(rv$data$id == cousin)
        rv$data$father[idx_cousin] <- uncle
        rv$data$mother[idx_cousin] <- uncle_partner
        father <- cousin_partner
        mother <- cousin
      }
      
      row <- make_row(
        id = id,
        label = label,
        sex = input$sex,
        father = father,
        mother = mother,
        status = input$status,
        carrier = input$carrier,
        phenotype = input$phenotype,
        variant = input$variant,
        visible = TRUE
      )
      add_or_update(row)
    })
    
    observeEvent(input$add_manual, {
      req(input$id)
      add_or_update(make_row(
        id = input$id,
        label = ifelse(input$label == "", input$id, input$label),
        sex = input$sex,
        father = input$father,
        mother = input$mother,
        status = input$status,
        carrier = input$carrier,
        phenotype = input$phenotype,
        variant = input$variant,
        visible = TRUE
      ))
    })
    
    observeEvent(input$example_wfs1, {
      rv$proband <- "APG"
      updateTextInput(session, "proband_id", value = "APG")
      
      rv$data <- data.frame(
        id = c(
          "APG_padre", "APG_madre", "APG",
          "GPG", "Conyuge_APG", "GPG2", "JPG", "MPG",
          "APG_tio_paterno", "APG_pareja_tio_paterno", "TSP",
          "Conyuge_TSP", "ASS_hijo", "ASS_hija"
        ),
        label = c(
          "Padre", "Madre", "APG\nID1",
          "GPG\nID2", "Pareja APG", "GPG2\nID3", "JPG\nID4", "MPG\nID5",
          "Tío paterno", "Pareja tío", "TSP",
          "Pareja TSP", "ASS", "ASS"
        ),
        sex = c(1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2),
        father = c("", "", "APG_padre", "APG_padre", "", "APG", "APG", "APG", "APG_padre", "", "APG_tio_paterno", "", "Conyuge_TSP", "Conyuge_TSP"),
        mother = c("", "", "APG_madre", "APG_madre", "", "Conyuge_APG", "Conyuge_APG", "Conyuge_APG", "APG_madre", "", "APG_pareja_tio_paterno", "", "TSP", "TSP"),
        status = c("unknown", "unknown", "affected", "affected", "unknown", "unaffected", "unaffected", "unaffected", "unknown", "unknown", "affected", "unknown", "unaffected", "unaffected"),
        carrier = c("unknown", "unknown", "carrier", "carrier", "unknown", "noncarrier", "carrier", "noncarrier", "unknown", "unknown", "noncarrier", "unknown", "noncarrier", "noncarrier"),
        phenotype = c("", "", "Sordera", "Sordera", "", "Sano", "Sano", "Sano", "", "", "Sordera", "", "Sano", "Sano"),
        variant = c("", "", "WFS1 c.1949A>C\np.Tyr650Ser", "WFS1 c.1949A>C\np.Tyr650Ser", "", "No portador", "WFS1 c.1949A>C\np.Tyr650Ser", "No portador", "", "", "WFS1 c.1949A>C\np.Tyr650Ser", "", "No portador", "No portador"),
        visible = TRUE,
        stringsAsFactors = FALSE
      )
    })
    
    observeEvent(input$clear, {
      rv$data <- empty_family()
    })
    
    output$family_table <- renderDT({
      datatable(rv$data, selection = "single", rownames = FALSE, options = list(pageLength = 15))
    })
    
    observeEvent(input$family_table_rows_selected, {
      rv$selected <- input$family_table_rows_selected
      if (length(rv$selected) == 1) {
        row <- rv$data[rv$selected, ]
        updateTextInput(session, "id", value = row$id)
        updateTextInput(session, "label", value = row$label)
        updateSelectInput(session, "sex", selected = row$sex)
        updateTextInput(session, "father", value = row$father)
        updateTextInput(session, "mother", value = row$mother)
        updateSelectInput(session, "status", selected = row$status)
        updateSelectInput(session, "carrier", selected = row$carrier)
        updateTextInput(session, "phenotype", value = row$phenotype)
        updateTextAreaInput(session, "variant", value = row$variant)
      }
    })
    
    observeEvent(input$delete, {
      if (!is.null(rv$selected) && length(rv$selected) == 1) {
        rv$data <- rv$data[-rv$selected, ]
        rv$selected <- NULL
      }
    })
    
    make_pedigree <- reactive({
      df <- rv$data
      validate(need(nrow(df) > 0, "Añade al menos una persona o carga el ejemplo."))
      
      father <- ifelse(df$father == "", NA, df$father)
      mother <- ifelse(df$mother == "", NA, df$mother)
      
      affected <- as.integer(df$status == "affected")
      
      ped <- pedigree(
        id = df$id,
        dadid = father,
        momid = mother,
        sex = df$sex,
        affected = affected
      )
      
      list(ped = ped, df = df)
    })
    
    make_plot_labels <- function(df) {
      carrier_txt <- ifelse(
        df$carrier == "carrier", "",
        ifelse(df$carrier == "noncarrier", "", "?")
      )
      
      phenotype_txt <- ifelse(df$phenotype != "", df$phenotype, "")
      variant_txt <- ifelse(df$variant != "", gsub("\n", " | ", df$variant), "")
      
      paste0(
        df$label,
        #ifelse(phenotype_txt != "", paste0("\n", phenotype_txt), ""),
        ifelse(carrier_txt != "", paste0("\n", carrier_txt), ""),
        ifelse(variant_txt != "", paste0("\n", variant_txt), "")
      )
    }
    
    plot_pedigree <- function(show_title = TRUE) {
      obj <- make_pedigree()
      ped <- obj$ped
      df <- obj$df
      
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      
      par(mar = c(9, 1, 4, 1), xpd = NA)
      
      plot(
        ped,
        id = make_plot_labels(df),
        cex = 0.95,
        symbolsize = 2.8,
        lwd = 3
      )
      
      if (show_title) {
        title("Pedigree", line = 2.2, cex.main = 1.2)
      }
      
      legend(
        "bottomright",
        legend = c("Afectado", "No afectado", "? = no estudiado"),
        fill = c("black", "white", "white"),
        border = c("black", "black", NA),
        bty = "n",
        cex = 0.75,
        inset = c(0, -0.18)
      )
    }
    
    output$pedigree_plot <- renderPlot({
      plot_pedigree()
    }, height = 700)
    
    output$download_png <- downloadHandler(
      filename = function() {
        "pedigri.png"
      },
      content = function(file) {
        png(file, width = 1800, height = 1200, res = 180)
        plot_pedigree()
        dev.off()
      }
    )
    
    output$download_svg <- downloadHandler(
      filename = function() {
        "pedigri.svg"
      },
      content = function(file) {
        svg(file, width = 12, height = 8)
        plot_pedigree()
        dev.off()
      }
    )
    
    output$download_csv <- downloadHandler(
      filename = function() {
        "familia_pedigri.csv"
      },
      content = function(file) {
        write.csv(rv$data, file, row.names = FALSE)
      }
    )
}
  
  

shinyApp(ui = ui, server = server)

