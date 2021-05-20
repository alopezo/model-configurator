#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
    renderDT(data, selection = 'none', server = server, 
             editable = list(target = 'cell', disable = list(columns = c(0))),
             options = list(ordering=F, searching=F,paging=F), ...)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Matrix Configurator"),
    
    fluidRow(
        column(5, offset = 1,
               DTOutput('edades_tbl'),
               actionButton("add_edad_btn", "Agregar")
               ),
        column(5,
               DTOutput('estados_tbl'),
               actionButton("add_estado_btn", "Agregar")
        )
    ),
    hr(),
    fluidRow(
        column(12, align = "center", 
               actionButton("redim", "Redimensionar parámetros", class="btn-primary")
               )
    ),
    hr(),
    fluidRow(
        column(8, offset = 2,
               fluidRow(
                   column(12, align="center",
                          h5("Matriz de contactos efectivos"),
                          DTOutput('matrix_ef_contacts_tbl')
                   )
               )
        )
    ),
    hr(),
    fluidRow(
        column(5, offset = 1,
               fluidRow(
                   column(12, align="center",
                          h5("Porcentaje de Gravedad"),
                          DTOutput('porcg_tbl')
                   )
               ),
               br(),
               fluidRow(
                   column(12, align="center",
                          p('Matriz de modificacion'),
                          DTOutput('matrix_porcg_tbl')
                   )
               )
        ),
        column(5,
               fluidRow(
                   column(12, align="center",
                          h5("Porcentaje de Críticos"),
                          DTOutput('porcc_tbl')
                   )
               ),
               br(),
               fluidRow(
                   column(12, align="center",
                          p('Matriz de modificacion'),
                          DTOutput('matrix_porcc_tbl')
                   )
               )
        ),
    ),
    hr(),
    fluidRow(
        column(5, offset = 1,
               fluidRow(
                   column(12, align="center",
                      h5("Infection Fatality Rate"),
                      DTOutput('ifr_tbl')
                   )
               ),
               br(),
               fluidRow(
                   column(12, align="center",
                      p('Matriz de modificacion'),
                      DTOutput('matrix_ifr_tbl')
                   )
               )
        ),
        column(5,
               fluidRow(
                   column(12, align="center",
                          p('Matriz de pasaje a V'),
                          DTOutput('matrix_pasaje_v_tbl')
                   )
               )
        )
    ),
    hr(),
    fluidRow(
        column(12, align = "center", 
               downloadLink("downloadData", "Download", class="btn btn-success"),
               br(),br(),br()
        )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    values <- reactiveValues();
    values$edades = data.table("Rango de Edad"=c("0-19","20-64","65+"))
    values$estados = data.table("Estado de inmunidad"=c("No inmune","Recuperado","1 dosis","2 dosis"))
    values$matrix_ef_contacts = matrix(1,nrow=nrow(isolate(values$edades)), ncol=nrow(isolate(values$edades)),
                                dimnames = list(isolate(values$edades[[1]]), isolate(values$edades[[1]])))
    values$porc_graves = matrix(1,nrow=1, ncol=nrow(isolate(values$edades)),
                            dimnames = list(c("Porcentaje de graves"), isolate(values$edades[[1]])))
    values$porc_criticos = matrix(1,nrow=1, ncol=nrow(isolate(values$edades)),
                                dimnames = list(c("Porcentaje de críticos"), isolate(values$edades[[1]])))
    values$ifr = matrix(1,nrow=1, ncol=nrow(isolate(values$edades)),
                                dimnames = list(c("IFR"), isolate(values$edades[[1]])))
    values$matrix_porcg = matrix(1,nrow=nrow(isolate(values$estados)), ncol=nrow(isolate(values$edades)),
                            dimnames = list(isolate(values$estados[[1]]), isolate(values$edades[[1]])))
    values$matrix_porcc = matrix(1,nrow=nrow(isolate(values$estados)), ncol=nrow(isolate(values$edades)),
                                 dimnames = list(isolate(values$estados[[1]]), isolate(values$edades[[1]])))
    values$matrix_ifr = matrix(1,nrow=nrow(isolate(values$estados)), ncol=nrow(isolate(values$edades)),
                                 dimnames = list(isolate(values$estados[[1]]), isolate(values$edades[[1]])))
    params_pasaje_v = c('% paso a V', '% proteccion','t hasta efecto', 'duracion en V', 'duracion proteccion')
    values$matrix_pasaje_v = matrix(1,nrow=nrow(isolate(values$estados)), ncol=length(params_pasaje_v),
                               dimnames = list(isolate(values$estados[[1]]), params_pasaje_v))
    
    # Outputs
    output$edades_tbl = renderDT(values$edades, selection = 'none',
                                 editable = list(target = 'cell', disable = list(columns = c(0))),
                                 options = list(ordering=F, searching=F,paging=F,info=F)
                                )
    observeEvent(input$edades_tbl_cell_edit, {
        values$edades <<- editData(values$edades, input$edades_tbl_cell_edit, 'Rango de Edad')
    })
    observeEvent(input$add_edad_btn, {
        values$edades = rbind(values$edades, data.table("Rango de Edad"=c("99")), fill=TRUE)
    })
    
    output$estados_tbl =renderDT(values$estados, selection = 'none', 
                                 editable = list(target = 'cell', disable = list(columns = c(0))),
                                 options = list(ordering=F, searching=F,paging=F,info=F)
                                )
    observeEvent(input$estados_tbl_cell_edit, {
        values$estados <<- editData(values$estados, input$estados_tbl_cell_edit, 'Estado de inmunidad')
    })
    observeEvent(input$add_estado_btn, {
        values$estados = rbind(values$estados, data.table("Estado de inmunidad"=c("Vacuna XX, N dosis")), fill=TRUE)
    })
    
    output$matrix_ef_contacts_tbl = renderDT(values$matrix_ef_contacts, selection = 'none', 
                                       editable = list(target = 'cell', disable = list(columns = c(0))),
                                       options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$porcg_tbl = renderDT(values$porc_graves, selection = 'none', 
                                  editable = list(target = 'cell', disable = list(columns = c(0))),
                                  options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$porcc_tbl = renderDT(values$porc_criticos, selection = 'none', 
                                  editable = list(target = 'cell', disable = list(columns = c(0))),
                                  options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$ifr_tbl = renderDT(values$ifr, selection = 'none', 
                                  editable = list(target = 'cell', disable = list(columns = c(0))),
                                  options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$matrix_porcg_tbl = renderDT(values$matrix_porcg, selection = 'none', 
                                 editable = list(target = 'cell', disable = list(columns = c(0))),
                                 options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$matrix_porcc_tbl = renderDT(values$matrix_porcc, selection = 'none', 
                                       editable = list(target = 'cell', disable = list(columns = c(0))),
                                       options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$matrix_ifr_tbl = renderDT(values$matrix_porcc, selection = 'none', 
                                       editable = list(target = 'cell', disable = list(columns = c(0))),
                                       options = list(ordering=F, searching=F,paging=F,info=F)
    )
    output$matrix_pasaje_v_tbl = renderDT(values$matrix_pasaje_v, selection = 'none', 
                                     editable = list(target = 'cell', disable = list(columns = c(0))),
                                     options = list(ordering=F, searching=F,paging=F,info=F)
    )
    
    observeEvent(input$redim, {
        values$matrix_ef_contacts = matrix(1,nrow=nrow(values$edades), ncol=nrow(values$edades),
                                   dimnames = list(values$edades[[1]], values$edades[[1]]))
        values$porc_graves = matrix(1,nrow=1, ncol=nrow(values$edades),
                                    dimnames = list(c("Porcentaje de graves"), values$edades[[1]]))
        values$porc_criticos = matrix(1,nrow=1, ncol=nrow(values$edades),
                                      dimnames = list(c("Porcentaje de críticos"), values$edades[[1]]))
        values$ifr = matrix(1,nrow=1, ncol=nrow(values$edades),
                            dimnames = list(c("IFR"), values$edades[[1]]))
        values$matrix_porcg = matrix(1,nrow=nrow(values$estados), ncol=nrow(values$edades),
                         dimnames = list(values$estados[[1]], values$edades[[1]]))
        values$matrix_porcc = matrix(1,nrow=nrow(values$estados), ncol=nrow(values$edades),
                                     dimnames = list(values$estados[[1]], values$edades[[1]]))
        values$matrix_ifr = matrix(1,nrow=nrow(values$estados), ncol=nrow(values$edades),
                                     dimnames = list(values$estados[[1]], values$edades[[1]]))
    })
    
    output$downloadData <- downloadHandler(
        
        filename <- function(){
            browser()
            paste("model-params.RData")
        },
        content = function(file) {
            
            edades=values$edades 
            estados=values$estados 
            ifr=values$ifr 
            matrix_ef_contacts=values$matrix_ef_contacts 
            matrix_ifr=values$matrix_ifr 
            matrix_pasaje_v=values$matrix_pasaje_v 
            matrix_porcc=values$matrix_porcc 
            matrix_porcg=values$matrix_porcg 
            porc_criticos=values$porc_criticos 
            porc_graves=values$porc_graves
            
            
            save(edades, 
                 estados, 
                 ifr, 
                 matrix_ef_contacts, 
                 matrix_ifr, 
                 matrix_pasaje_v, 
                 matrix_porcc, 
                 matrix_porcg, 
                 porc_criticos, 
                 porc_graves, file = file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
