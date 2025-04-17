library(glue)
server_parts <-  glue('output$factor <- renderUI({
      selectInput("factor_vname",
                  label = "Additional categorical variables",
                  choices = data.list$factor_adds_list, multiple = T,
                  selected = data.list$factor_adds
      )
    })
    
    output$binary_check <- renderUI({
      checkboxInput("check_binary", "Make binary variables")
    })
    
    output$ref_check <- renderUI({
      checkboxInput("check_ref", "Change reference of categorical variables")
    })
    
    
    output$subset_check <- renderUI({
      checkboxInput("check_subset", "Subset data")
    })
    
    
    observeEvent(input$check_binary, {
      var.conti <- setdiff(names(out), factor_vars)
      output$binary_var <- renderUI({
        req(input$check_binary == T)
        selectInput("var_binary", "Variables to dichotomize",
                    choices = var.conti, multiple = T,
                    selected = var.conti[1])
      })
      
      output$binary_val <- renderUI({
        req(input$check_binary == T)
        req(length(input$var_binary) > 0)
        outUI <- tagList()
        for (v in seq_along(input$var_binary)){
          med <- stats::quantile(out[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
          outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),
                                    selectInput(paste0("con_binary", v), paste0("Define reference:"),
                                                choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
                                    ),
                                    numericInput(paste0("cut_binary", v), input$var_binary[[v]],
                                                 value = med[2], min = med[1], max = med[3]
                                    )
          )
          
        }
        outUI
        
      })
    })
    
    
    observeEvent(input$check_ref, {
      var.factor <- factor_vars
      output$ref_var <- renderUI({
        req(input$check_ref == T)
        selectInput("var_ref", "Variables to change reference",
                    choices = var.factor, multiple = T,
                    selected = var.factor[1])
      })
      
      output$ref_val <- renderUI({
        req(input$check_ref == T)
        req(length(input$var_ref) > 0)
        outUI <- tagList()
        for (v in seq_along(input$var_ref)){
          outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),
                                    choices = levels(factor(out[[input$var_ref[[v]]]])), selected = levels(factor(out[[input$var_ref[[v]]]]))[2])
          
        }
        outUI
        
      })
    })
    
    observeEvent(input$check_subset, {
      output$subset_var <- renderUI({
        req(input$check_subset == T)
        #factor_subset <- c(data.list$factor_original, input$factor_vname)
        
        #validate(
        #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
        #)
        
        tagList(
          selectInput("var_subset", "Subset variables",
                      choices = names(out), multiple = T,
                      selected = "Rheumatic_0_Degenerative_1")
        )
      })
      
      output$subset_val <- renderUI({
        req(input$check_subset == T)
        req(length(input$var_subset) > 0)
        var.factor <- factor_vars
        
        outUI <- tagList()
        
        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% var.factor){
            varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))
            outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
                                      choices = varlevel, multiple = T,
                                      selected = varlevel[2])
          } else{
            val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
                                      min = val[1], max = val[5],
                                      value = c(val[2], val[4]))
          }
          
        }
        outUI
      })
    })
    
    data.info <- reactive({
      out1 <- out[, .SD]
      out1 <- data.table::as.data.table(out1)
      out1[, (conti_vars) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = conti_vars]
      
      out.label1 <- out.label[, .SD]
      out.label1 <- data.table::as.data.table(out.label1)
      #out.label[, var_label := ref[out.label$variable, name.old]]
      
      if (!is.null(input$factor_vname)) {
        out1[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols = input$factor_vname]
        
        out.label1 <- rbind(out.label1[!(variable %in% input$factor_vname)], mk.lev(out1[, .SD, .SDcols = input$factor_vname]))
      }
      
      
      req(!is.null(input$check_binary))
      if (input$check_binary == T){
        validate(
          need(length(input$var_binary) > 0 , "No variables to dichotomize")
        )
        sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
        names(sym.ineq) <- sym.ineq[4:1]
        sym.ineq2 <- c("le", "ge", "l", "g")
        names(sym.ineq2) <- sym.ineq
        for (v in seq_along(input$var_binary)){
          req(input[[paste0("con_binary", v)]])
          req(input[[paste0("cut_binary", v)]])
          if (input[[paste0("con_binary", v)]] == "\u2264"){
            out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]
          } else if (input[[paste0("con_binary", v)]] == "\u2265"){
            out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
          } else if (input[[paste0("con_binary", v)]] == "\u003c"){
            out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
          } else{
            out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
          }
          
          cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
          data.table::setnames(out1, "BinaryGroupRandom", cn.new)
          
          label.binary <- mk.lev(out1[, .SD, .SDcols = cn.new])
          label.binary <- data.table(label.binary)
          label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]
          label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]
          out.label1 <- rbind(out.label1, label.binary)
        }
        
      }
      
      if (!is.null(input$check_ref)){
        if (input$check_ref){
          validate(
            need(length(input$var_ref) > 0 , "No variables to change reference")
          )
          for (v in seq_along(input$var_ref)){
            req(input[[paste0("con_ref", v)]])
            out1[[input$var_ref[[v]]]] <- stats::relevel(out1[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])
            out.label1[variable == input$var_ref[[v]], ':='(level = levels(out1[[input$var_ref[[v]]]]), val_label = levels(out1[[input$var_ref[[v]]]]))]
          }
          
        }
      }
      
      
      if (!is.null(input$check_subset)){
        if (input$check_subset){
          validate(
            need(length(input$var_subset) > 0 , "No variables for subsetting"),
            need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
          )
          var.factor <- factor_vars
          #var.conti <- setdiff(data()$conti_original, input$factor_vname)
          
          for (v in seq_along(input$var_subset)){
            if (input$var_subset[[v]] %in% var.factor){
              out1 <- out1[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
              out1 <- data.table::as.data.table(out1)
              #var.factor <- c(data()$factor_original, input$factor_vname)
              out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
              data.table::setkey(out.label1, "variable", "class", "level")
              data.table::setkey(out.label2, "variable", "class", "level")
              out.label1 <- out.label1[out.label2]
            } else{
              out1 <- out1[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
              #var.factor <- c(data()$factor_original, input$factor_vname)
              out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
              data.table::setkey(out.label1, "variable", "class", "level")
              data.table::setkey(out.label2, "variable", "class", "level")
              out.label1 <- out.label1[out.label2]
            }
          }
          
        }
      }
      #for (vn in ref[["name.new"]]){
      #  w <- which(ref[["name.new"]] == vn)
      #  out.label1[variable == vn, var_label := ref[["name.old"]][w]]
      #}
      
      return(list(data = out1, label = out.label1))
    })
    
    
    data <- reactive({
      data.info()$data
    })
    
    data.label <- reactive(data.info()$label)
    
    varlist_new <- reactive(list(New = setdiff(names(data()), names(out))))
    
    output$data <- renderDT({
      datatable(data(), rownames=F, editable = F, caption = "Data",
                options = c(jstable::opt.data("data"), list(scrollX = TRUE))
      )
    })
    
    
    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(jstable::opt.data("label"), list(scrollX = TRUE))
      )
    })')
