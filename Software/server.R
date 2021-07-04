

function(input, output, session) {
  #################  on start -----------------------------------------------------------------------------------------
  
  
  path <- reactiveVal()
  path_ok <- reactiveVal(0)
  path2_ok <- reactiveVal(0)
  section_first <- reactiveVal(0)
  cd <- reactiveVal(0)
  
  
  report_data <- reactiveValues()
  work_dir <- reactiveValues()
  end_t <- reactiveVal(1)
  end_t_lvl <- reactiveVal()
  miss_input <- reactiveVal()
  fil_data <- reactiveVal()
  save_dir <- reactiveVal()
  path2 <- reactiveVal()
  wel_depth <- reactiveVal()
  wel_loc <- reactiveVal()
  aqua_thick <- reactiveVal()
  
  cbar <- reactiveVal()
  
  
  
  
  observeEvent(input$section_first_xlsx,
               {
                 section_first(input$section_first_xlsx)
                 
                 setupWT_main = data_to_report()
                 setupWT_main$section_first = input$section_first_xlsx
                 data_to_report(setupWT_main)
                 
                 
               })
  
  observeEvent(input$section_first_txt,
               {
                 section_first(input$section_first_txt)
                 
                 setupWT_main = data_to_report()
                 setupWT_main$section_first = input$section_first_txt
                 data_to_report(setupWT_main)
               })
  
  
  observeEvent(input$section_first_proj,
               {
                 section_first(input$section_first_proj)
                 
                 setupWT_main = data_to_report()
                 setupWT_main$section_first = input$section_first_proj
                 data_to_report(setupWT_main)
               })
  
  
  
  
  output$dir_select <- renderUI({
    if (miss_input() == 0)
      return(shinyDirButton(
        "dir",
        "Chose directory",
        title = "",
        class = "btn-primary"
      ))
    
    else
      return(shinyDirButton(
        "dir",
        "Chose directory",
        title = "",
        class = "btn-warning"
      ))
  })
  
  observeEvent(input$dir, {
    updateActionButton(session, "dir", label = "Directory selected")
  })
  
  volumes = getVolumes()()
  
  shinyDirChoose(input, 'dir', roots = volumes)
  
  dir <- reactive(input$dir)
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 c(input$dir, input$name_out_file)
               },
               handlerExpr = {
                 req(is.list(input$dir))
                 path_ok(1)
                 roots = getVolumes()
                 p <- parseDirPath(roots, dir())
                 p <- paste0(p, "/", input$name_out_file)
                 path(p)
               })
  
  data <- reactiveVal()
  
  observeEvent(input$new_project, {
    path2_ok(0)
    new_or_load(0)
    options(shiny.maxRequestSize = 9 * 1024 ^ 2)
    
    output$table.output <- renderTable({
      return(NULL)
    })
    
    output$graph1 <- renderPlot({
      return(NULL)
    })
    
    output$graph2 <- renderPlot({
      return(NULL)
    })
    
    output$table.output2 <- DT::renderDataTable({
      return(NULL)
    })
    
    output$max_data_txt <- renderUI({
      return(NULL)
    })
    
  
    
    output$max_data_proj <- renderUI({
      return(NULL)
    })
    
    output$file1_ui <- renderUI({
      fileInput(
        inputId  = 'file1',
        label = NULL,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        ),
        buttonLabel = "New project",
        placeholder = NULL
      )
    })
  })
  
  
  observeEvent(input$load_project, {
    new_or_load(1)
    path2_ok(0)
    data(NULL)
    data_ok(NULL)
    time_col(NULL)
    drawdown_col(NULL)
    min_val(NULL)
    max_val(NULL)
    format_file1(0)
    
    output$table.output <- renderTable({
      return(NULL)
    })
    
    output$graph1 <- renderPlot({
      return(NULL)
    })
    
    output$graph2 <- renderPlot({
      return(NULL)
    })
    
    output$table.output2 <- DT::renderDataTable({
      return(NULL)
    })
    
    output$max_data_txt <- renderUI({
      return(NULL)
    })

    output$max_data_proj <- renderUI({
      return(NULL)
    })
    
    output$file_project <- renderUI({
      fileInput(
        inputId  = 'file_project',
        label = NULL,
        buttonLabel = "Project",
        placeholder = NULL,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    })
  })
  
  data_to_report <- reactiveVal()
  setupWT_main <- reactiveValues()
  
  observeEvent(input$file1, {
    if (exists("setupWT"))
      remove(setupWT, pos = 1, inherits = TRUE)
    
    data(NULL)
    data_ok(NULL)
    time_col(NULL)
    drawdown_col(NULL)
    min_val(NULL)
    max_val(NULL)
    data_to_report <- reactiveVal()
    setupWT_main <- reactiveValues()
    
    setupWT_main$hydraulic_cond = NULL
    setupWT_main$cone_of_depr = NULL
    setupWT_main$depth = NULL
    setupWT_main$Q = NULL
    setupWT_main$R = NULL
    setupWT_main$Stor = NULL
    setupWT_main$Trans = NULL
    setupWT_main$iter = NULL
    setupWT_main$skin_bot = NULL
    setupWT_main$skin_top = NULL
    setupWT_main$stor_bot = NULL
    setupWT_main$stor_top = NULL
    setupWT_main$cores_to_use = NULL
    setupWT_main$name_out_file = NULL
    setupWT_main$file_t = NULL
    setupWT_main$file2_t = NULL
    setupWT_main$well_location = NULL
    setupWT_main$min = NULL
    setupWT_main$max = NULL
    setupWT_main$data = NULL
    setupWT_main$data_ok = NULL
    setupWT_main$time_col = NULL
    setupWT_main$drawdown_col = NULL
    data_to_report(setupWT_main)
  })
  
  ################## file type ------------------------------------------------------------------------------------------------
  
  file_t <- reactiveVal()
  file_t2 <- reactiveVal()
  report_cond <- reactiveVal()
  new_or_load <- reactiveVal()
  
  #### file 1 ----------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$file1, {
    setupWT_main = data_to_report()
    file1 <- input$file1
    
    if (is.null(file1))
      return()
    
    new_or_load(0)
    data_path <- file1$datapath
    end_string <- nchar(data_path)
    start_string <- end_string
    letters <-
      substring(data_path, 1:end_string , 1:end_string)
    type <- ("")
    
    for (n in 1:6) {
      if (letters[start_string] == ".")
        type <-
          substr(data_path, start_string + 1, end_string)
      
      start_string <- start_string - 1
    }
    
    if (type == "txt" || type == "xlsx")  {
      format_file1(0)
      file_t(type)
      setupWT_main$file_t = type
    }
    
    else {
      format_file1(1)
      file_t(NA)
      setupWT_main$file_t = NA
    }
    data_to_report(setupWT_main)
  })
  
  observeEvent(c(
    input$file1,
    input$sep,
    input$dec,
    input$length_data1x,
    input$length_data1t
  ),
  {
    if (is.null(input$file1) ||
        is.na(data_to_report()$file_t) ||
        is.null(input$sep) ||
        is.null(input$dec) || format_file1() == 1)
      return ()
    
    file1 = input$file1
    
    if (file_t() == "txt") {
      showModal(modalDialog("Loading", footer = NULL))
      dat =  read.csv(
        input$file1$datapath,
        header = TRUE,
        sep = input$sep,
        dec = input$dec
      )
      
      setupWT_main = data_to_report()
      setupWT_main$data = dat
      setupWT_main$time_project = format(Sys.time(), "%d %B %Y")
      
      data_to_report(setupWT_main)
      data(dat)
      
      
      removeModal()
    }
  })
  
  observeEvent(input$load_sheet, {
    if (is.null(input$file1) ||
        is.null(data_to_report()$file_t) ||
        is.null(input$sep) ||
        is.null(input$dec) || is.null(input$sheet))
      return ()
    
    if (file_t() == "xlsx") {
      showModal(modalDialog("Loading", footer = NULL))
      file1 = input$file1
      dat = read.xlsx(file1$datapath, sheetName = input$sheet)
      setupWT_main = data_to_report()
      setupWT_main$data = dat
      
      
      setupWT_main$time_project = format(Sys.time(), "%d %B %Y")
      
      
      
      data_to_report(setupWT_main)
      data(dat)
      
      
      
      
      removeModal()
    }
  })
  
  #### file 2 ---------------------------------------------------------------------------------------------------------------------------------
  
  format_file2 <- reactiveVal(NA)
  
  observeEvent(input$file2, {
    file2 <- input$file2
    
    if (is.null(file2))
      return()
    
    data_path <- file2$datapath
    end_string <- nchar(data_path)
    start_string <- end_string
    letters <-
      substring(data_path, 1:end_string , 1:end_string)
    type <- ("")
    
    for (n in 1:6) {
      if (letters[start_string] == ".")
        type <-  substr(data_path, start_string + 1, end_string)
      
      start_string <- start_string - 1
    }
    
    if (type == "txt")  {
      format_file2(0)
      file_t2(type)
      stage2(223)
    }
    
    else if (type == "xlsx")  {
      format_file2(0)
      file_t2(type)
      stage2(222)
    }
    
    else {
      format_file2(1)
      file_t2(NA)
    }
  })
  
  output$format_file2 <- reactive ({
    format_file2()
  })
  
  outputOptions(output, "format_file2", suspendWhenHidden = FALSE)
  
  #### file project ---------------------------------------------------------------------------------------------------------------------------------
  
  file_t_proj <- reactiveVal (0)
  end_for_report <- reactiveVal()
  length_slider_proj <- reactiveVal()
  
  observeEvent(input$file_project, {
    new_or_load(1)
    file_proj <- input$file_project
    
    
    if (is.null(file_proj))
      return()
    
    data_path = file_proj$datapath
    end_string = nchar(data_path)
    
    if (end_string < 4)
      return ()
    
    start_string = end_string - 1
    type = substr(data_path, start_string , end_string)
    
    if (type == "wt") {
      file_t_proj(0)
      report_cond(1)
      inFile <- isolate({
        input$file_project
      })
      
      
      
      file <- inFile$datapath
      load(file, envir = .GlobalEnv)
      
      cd(setupWT$Cbar)
      setupWT_main = setupWT
      data(setupWT$data)
      data_ok(setupWT$data_ok)
      cone(setupWT_main$cone_of_depr)
      cond(setupWT_main$hydraulic_cond)
      time_col("time")
      drawdown_col("drawdown")
      drawdown <- data_ok()$drawdown
      end_t(which.max(drawdown))
      end_for_report(which.max(drawdown))
      end_t_lvl(as.numeric(drawdown[end_t()]))
      Sys.setlocale("LC_TIME", "English")
      setupWT_main$time_project = format(Sys.time(), "%d %B %Y")
      file_t(setupWT$file_type1)
      data_to_report(setupWT_main)
      length_slider_proj(setupWT$max)
      section_first(setupWT$first_section)
      slider_well_storativity_val(setupWT_main$slider_well_storativity)
      
      
    
      
      
      
      wt_main = data_to_report()
      wt_main$time_project = format(Sys.time(), "%d %B %Y")
      
      data_to_report(wt_main)
    }
    
    else {
      file_t_proj(1)
      return ()
    }
  })
  
  output$file_t_proj <- reactive ({
    file_t_proj()
  })
  
  outputOptions(output, "file_t_proj", suspendWhenHidden = FALSE)
  
  #### out ------------------------------------------------------------------------------------------------------------------------------------
  
  output$file_type <- reactive ({
    file_t()
  })
  
  output$file_type2 <- reactive ({
    file_t2()
  })
  
  outputOptions(output, "file_type", suspendWhenHidden = FALSE)
  outputOptions(output, "file_type2", suspendWhenHidden = FALSE)
  
  #### file 1 data ok ----------------------------------------------------------------------------------------------------------
  
  dat_slope <- reactiveVal()
  data_ok <- reactiveVal()
  
  observeEvent(c(time_col(), drawdown_col(), data()), {
    if (is.null(data_to_report()$time_col) ||
        is.null(data_to_report()$drawdown_col) ||
        is.null(data_to_report()$data) ||
        format_file1() == 1 ||
        is.null(time_col()) ||
        is.null(drawdown_col()) ||
        is.null(data())) {
      return()
      
    }
    
    else {
      if (new_or_load() == 0) {
        dat <-
          data.frame(time = data()[, time_col()], drawdown = data()[, drawdown_col()])
        
        if (sum(is.na(dat[, 1])) > 0 ||
            sum(is.na(dat[, 2])) > 0) {
          dat <- dat[-which(is.na(dat[, c(1, 2)])),]
        }
        data_ok(dat)
      }
      
      else {
        dat <- data.frame(time = data_to_report()$data[, 1],
                          drawdown = data_to_report()$data[, 2])
        
        if (sum(is.na(dat[, 1])) > 0 ||
            sum(is.na(dat[, 2])) > 0) {
          dat <- dat[-which(is.na(dat[, c(1, 2)])),]
        }
      }
      data_ok(dat)
      setupWT_main <- data_to_report()
      setupWT_main$data_ok = dat
      data_to_report(setupWT_main)
    }
  })
  
  ##############################  eval param ------------------------------------------------------------------------------------------------------
  cone_of_d <- reactiveVal ("no")
  conduct <- reactiveVal ("no")
  eval_p <- reactiveVal(0)
  slider_tr <- reactiveVal("no")
  thickness <- reactiveVal("no")
  
  observeEvent(input$previous_eval_p, {
    eval_p(0)
  })
  
  observeEvent(input$eval_param_x, {
    eval_p(1)
  })
  
  observeEvent(input$eval_param_t, {
    eval_p(1)
  })
  
  observeEvent(input$eval_param_p, {
    eval_p(1)
  })
  
  output$eval_par <- reactive({
    eval_p()
  })
  
  output$slider_trans <- reactive({
    slider_tr()
  })
  
  output$thick <- reactive({
    thickness()
  })
  
  outputOptions(output, "eval_par", suspendWhenHidden = FALSE)
  outputOptions(output, "slider_trans", suspendWhenHidden = FALSE)
  outputOptions(output, "thick", suspendWhenHidden = FALSE)
  
  ############################## render ui slider trans  ------------------------------------------------------------------------------------------------------
  
  output$trans_limits <- renderUI({
    delka <- end_t()
    
    sliderInput(
      "trans_lim",
      "Select section for transmissivity evaluation",
      min = 0,
      max = delka,
      if (exists("setupWT") && new_or_load() == 1)
        value =  setupWT$Trans
      
      else
        value = c(round(delka * 0.6, 0), round(delka * 0.95, 0))
      
     
    )
  })
  
  ############################## min max value ------------------------------------------------------------------------------------------------------
  
  min_val <- reactiveVal()
  max_val <- reactiveVal()
  
  observeEvent(file_t(), {
    if (format_file1() == 1)
      return()
    
    if (file_t_proj() == 0 || new_or_load() == 1) {
      min_val(data_to_report()$min)
      max_val(data_to_report()$max)
    }
  })
  
  observeEvent(
    c(
      input$min_data_txt,
      input$max_data_txt,
      input$min_data_xlsx,
      input$max_data_xlsx,
      input$min_data_proj,
      input$max_data_proj,
      data()
    ),
    {
      if (exists("setupWT") && new_or_load() == 1) {
        min_val(as.numeric(input$min_data_proj))
        max_val(input$max_data_proj)
      }
      
      else {
        if (file_t() == "txt") {
          min_val(as.numeric(input$min_data_txt))
          max_val(input$max_data_txt)
        }
        
        if (file_t() == "xlsx") {
          min_val(as.numeric(input$min_data_xlsx))
          max_val(input$max_data_xlsx)
        }
        
        else
          return ()
      }
    }
  )
  
  observeEvent(c(min_val(), max_val()), {
    setupWT_main <- data_to_report()
    setupWT_main$min <- min_val()
    setupWT_main$max <- max_val()
    data_to_report(setupWT_main)
  })
  
  ############################## columns selected ------------------------------------------------------------------------------------------------------
  
  time_col <- reactiveVal()
  drawdown_col <- reactiveVal()
  
  observeEvent(
    c(
      input$time_col_txt,
      input$drawdown_col_txt,
      input$time_col_xlsx,
      input$drawdown_col_xlsx
    ),
    {
      if (new_or_load() == 1)  {
        time_col("time")
        drawdown_col("drawdown")
      }
      
      else {
        if (file_t() == "txt") {
          time_col(input$time_col_txt)
          drawdown_col(input$drawdown_col_txt)
        }
        
        if (file_t() == "xlsx") {
          time_col(input$time_col_xlsx)
          drawdown_col(input$drawdown_col_xlsx)
        }
        setupWT_main <- data_to_report()
        setupWT_main$time_col <- time_col()
        setupWT_main$drawdown_col <- drawdown_col()
        data_to_report(setupWT_main)
      }
    }
  )
  
  ########### data 1 loaded y or no --------------------------------------------------------------------------------------------------------------------------
  
  datt1 <- reactiveVal(0)
  
  observeEvent(c(input$file1, input$file_project), {
    if (format_file1() == 1 || file_t_proj() == 1)
      return()
    
    else
      datt1(1)
  })
  
  observeEvent(c(input$load_project, input$new_project), {
    datt1(0)
  })
  
  output$dat1 <- reactive({
    datt1()
  })
  
  outputOptions(output, "dat1", suspendWhenHidden = FALSE)
  
  ########### data 2 loaded y or no --------------------------------------------------------------------------------------------------------------------------
  
  datt2 <- reactiveVal(0)
  
  observeEvent(input$file2, {
    if (is.null(file_t2()))
      return()
    
    datt2(1)
  })
  
  output$dat2 <- reactive({
    datt2()
  })
  
  outputOptions(output, "dat2", suspendWhenHidden = FALSE)
  
  ############################# file 1 sheets -----------------------------------------------------------------------------------------------------------------
  
  observeEvent(file_t(), {
    if (format_file1() == 1)
      return()
    
    if (file_t() == "xlsx") {
      output$sheet_names <- renderUI({
        infile <- input$file1
        names <- excel_sheets(infile$datapath)
        leng <- length(names)
        selectInput(
          inputId = "sheet",
          choices = names,
          label = NULL,
          multiple = FALSE,
          selected = NULL,
          selectize = FALSE,
          size = leng
        )
      })
    }
    
    else
      return()
  })
  
  
  output$report_con <- renderUI({
    checkboxGroupInput(
      "report_contain",
      label = NULL,
      c(
        "Include",
        "Graph - input data",
        "Graph - data used - Cd"
      ),
      selected = "Include"
    )
  })
  
  observeEvent(input$select_all_graphs_report, {
    updateCheckboxGroupInput(
      session,
      inputId = "report_contain",
      selected = c(
        "Include",
        "Graph - input data",
        "Graph - data used - Cd"
      )
    )
  })
  
  
  ####################### render ui 1 page  colnames  ----------------------------------------------------------------------------------------------
  
  
  observeEvent(data(), {
    if (is.null(file_t()))
      return ()
    
    if (file_t() == "txt") {
      var_names <- variable.names(data())
      rd = c()
      len = length(var_names)
      
      for (i in 1:len)
      {
        if (sum(!is.na(data()[, i])) != 0)
          rd = c(rd, i)
      }
      
      if (is.null(rd))
      {
        var_n = NULL
        var_n2 = NULL
      }
      
      else
      {
        var_n = var_names[rd[1]]
        var_n2 = var_names[rd[2]]
      }
      
      output$time_col_txt <- renderUI({
        selectInput(
          inputId = "time_col_txt",
          choices = var_names[rd],
          label = NULL,
          selected = var_n
        )
      })
      
      output$drawdown_col_txt <- renderUI({
        selectInput(
          inputId = "drawdown_col_txt",
          choices = var_names[rd],
          label = NULL,
          selected = var_n2
        )
      })
      
      
      output$section_first_txt <- renderUI({
        
        if(end_t() <= 1  )
          end_t(5)
        
        
        sliderInput(
          "section_first_txt",
          label = NULL,
          min = 5,
          max = end_t() * 0.6,
          
          if (exists("setupWT") && new_or_load() == 1)
            value = setupWT$section_first
          
          else
            value = round(end_t() * 0.06, 0)
          
        )
      })
    }
    
    if (file_t() == "xlsx") {
      var_names <- variable.names(data())
      rd = c()
      len = length(var_names)
      
      for (i in 1:len)
      {
        if (sum(!is.na(data()[, i])) != 0)
          rd = c(rd, i)
      }
      
      if (is.null(rd))
      {
        var_n = NULL
        var_n2 = NULL
      }
      
      else
      {
        var_n = var_names[rd[1]]
        var_n2 = var_names[rd[2]]
      }
      
      output$time_col_xlsx <- renderUI({
        selectInput(
          inputId = "time_col_xlsx",
          choices = var_names[rd],
          label = NULL,
          selected = var_n
        )
      })
      
      output$drawdown_col_xlsx <- renderUI({
        selectInput(
          inputId = "drawdown_col_xlsx",
          choices = var_names[rd],
          label = NULL,
          selected = var_n2
        )
      })
      
      output$section_first_xlsx <- renderUI({
        
        if(end_t() <= 1  )
          end_t(5)
        
        
        sliderInput(
          "section_first_xlsx",
          label = NULL,
          min = 5,
          max = end_t() * 0.6,
          
          if (exists("setupWT") && new_or_load() == 1)
            value = setupWT$section_first
          
          else
          value = round(end_t() * 0.06, 0)
          
        )
      })
      
    }
  })
  
  ####################### render ui 1 page  min max  ----------------------------------------------------------------------------------------------
  
  observeEvent(input$file_project, {
    if (!exists("setupWT") ||
        file_t_proj() == 1 ||
        format_file1() == 1 ||
        is.null(data_ok()))
      return ()
    delka = length(data_ok()[1:end_t(), 1])
    
    output$min_data_proj <- renderUI({
      textInput(
        "min_data_proj",
        label = NULL,
        width = "200",
        value = data_to_report()$min
      )
    })
    
    output$max_data_projj <- renderUI({
      sliderInput(
        "max_data_proj",
        label = NULL ,
        min = 2,
        max = delka,
        value = length_slider_proj()
      )
    })
    
    
    output$drawdown_col_txt <- renderUI({
      selectInput(
        inputId = "drawdown_col_txt",
        choices = var_names[rd],
        label = NULL,
        selected = var_n2
      )
    })
    
    
    
    output$section_first_proj <- renderUI({
      sliderInput(
        "section_first_proj",
        label = NULL,
        min = 5,
        max = end_t() * 0.3,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$section_first
        
        else
          value = NA
        
        
      )
    })
    
    
  })
  
  
  
  
  observeEvent(data_ok(), {
    if (exists("setupWT") || is.null(file_t()) || is.null(data_ok()))
      return ()
    
    else {
      drawdown <- data_ok()$drawdown
      end_t(which.max(drawdown))
      end_for_report(which.max(drawdown))
      end_t_lvl(as.numeric(drawdown[end_t()]))
      delka = length(data_ok()[, 1])
      
      if (delka > end_t())
        delka = end_t()
      
    }
    
    if (file_t() == "txt") {
      output$min_data_txt <- renderUI({
        textInput(
          "min_data_txt",
          label = NULL,
          width = "200",
          value = 1
        )
      })
      
      output$max_data_txt <- renderUI({
        sliderInput(
          "max_data_txt",
          label = NULL,
          min = 2,
          max = end_t(),
          value = round(delka * 0.85, 0)
        )
      })
      
      
      output$section_first_txt <- renderUI({
        sliderInput(
          "section_first_txt",
          label = NULL,
          min = 5,
          max = (end_t() * 0.3),
          value = round(end_t() * 0.03, 0)
        )
      })
      
    }
    
    if (file_t() == "xlsx") {
      output$min_data_xlsx <- renderUI({
        textInput(
          "min_data_xlsx",
          label = NULL,
          width = "200",
          value = 1
        )
      })
  
      
      output$section_first_xlsx <- renderUI({
        sliderInput(
          "section_first_xlsx",
          label = NULL,
          min = 5,
          max = (end_t() * 0.6),
          value = round(end_t() * 0.06, 0)
        )
      })
      
      
    }
  })
  
  
  
  
  ####################### render ui 2 page  parameters  -----------------------------------------------------------------------------------------------
  
  observeEvent(c(input$file1, input$file_project), {
    output$Q <- renderUI({
      numericInput(
        inputId = "Q",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$Q
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$R <- renderUI({
      numericInput(
        inputId = "R",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$R
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$Storr <- renderUI({
      numericInput(
        inputId = "Stor",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$Stor
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$Stor <- renderUI({
      numericInput(
        "aqua_thick",
        label = NULL,
        min = 0,
        max = 1000,
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$aqua_thick
        
        else
          value = NA
      )
    })
    
    output$aq_thick <- renderUI({
      numericInput(
        "aqua_thick",
        label = NULL,
        min = 0,
        max = 1000,
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$aqua_thick
        
        else
          value = NA
      )
    })
    
    output$well_locat <- renderUI({
      textInput(
        inputId = "well_location",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = data_to_report()$well_location
        
        else
          value = NA
      )
    })
    
    output$well_depthh <- renderUI({
      numericInput(
        inputId = "well_depth",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = data_to_report()$well_depth
        
        else
          value = NULL
      )
    })
  })
  
  observeEvent(c(input$file1, input$file_project, input$param_to_eval), {
    output$Trans <- renderUI({
      numericInput(
        inputId = "Trans",
        label = NULL,
        if (exists("setupWT") &&
            new_or_load() == 1 || slider_tr() == "yes") {
          if (slider_tr() == "yes") {
            if (is.na(input$Q) || is.null(input$Q))
              return ()
            
            tran_eval = (lm(data_ok()[input$trans_lim[1]:input$trans_lim[2], 2] ~ log10(data_ok()[input$trans_lim[1]:input$trans_lim[2], 1])))
            val = round((0.183 * (input$Q) / tran_eval$coefficients[2]), 6)
            
            if (val > 0)
              value = val
            
            else
              value = NA
          }
          
          else
            value = setupWT$Trans
        }
        
        else
          value =  NA,
        min = 0,
        max = 1000
      )
    })
  })
  
  ####################### render ui 2 page  limits  ----------------------------------------------------------------------------------------------
  
  observeEvent(c(input$file1, input$file_project), {
    output$out_name <- renderUI({
      textInput(
        inputId = "name_out_file",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$name
        
        else
          value =  NA
      )
    })
  })





  
  ####################  2.page  - sheets2 ---------------------------------------------------------------------------------------------------------
  
  
  observeEvent(file_t2(), {
    output$sheet_names2 <- renderUI({
      if (file_t2() == "xlsx") {
        infile = input$file2
        names = excel_sheets(infile$datapath)
        leng <- length(names)
        
        selectInput(
          inputId = "sheet2",
          choices = names,
          label = NULL,
          selected = NULL,
          selectize = FALSE,
          size = leng
        )
      }
      
      else
        return()
    })
  })
  
  
  ########################## data load  ------------------------------------------------------------------------------------------------------------
  
  
  #### file 1 --------------------------------------------------------------------------------------------------------------------------------------
  
  data <- reactiveVal()
  
  #### file 2 -----------------------------------------------------------------------------------------------------------------------------------
  
  data2 <- reactiveVal()
  
  observeEvent(c(input$sep2, input$dec2, input$file2), {
    if (is.null(input$file2) || format_file2() == 1 || is.null(file_t2))
    {
      return ()
    }
    
    showModal(modalDialog("Loading", footer = NULL))
    file2 = input$file2
    
    if (file_t2()  == "txt") {
      dat =  read.csv(
        input$file2$datapath,
        header = TRUE,
        sep = input$sep2,
        dec = input$dec2
      )
      data2(dat)
    }
    removeModal()
  })
  
  observeEvent(input$load_sheet2, {
    if (is.null(input$file2))
      return()
    
    showModal(modalDialog("Loading", footer = NULL))
    
    if (file_t2() == "xlsx") {
      file2 = input$file2
      dat = read.xlsx(file2$datapath, sheetName = input$sheet2)
      data2(dat)
    }
    removeModal()
  })
  
  observeEvent(input$no22, {
    data2(data())
  })
  
  
  ######################### table 1 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      data(),
      input$file1,
      input$sep,
      input$dec,
      input$length_data1x,
      input$length_data1t
    ),
    {
      if (format_file1() == 1)
        return()
      
      showModal(modalDialog("Loading", footer = NULL))
      
      if (input$length_data1t == "no" ||
          input$length_data1x == "no")
      {
        output$table.output <- renderTable({
          data()[1:20,]
        })
        
        output$table.output22 <- renderTable({
          data()[1:20,]
        })
        
      }
      
      if (input$length_data1t == "yes" ||
          input$length_data1x == "yes")
      {
        output$table.output <- renderTable({
          data()
        })
        
          output$table.output22 <- renderTable({
            data()
        })
      }
      
      else {
        output$table.output <- renderTable({
          data()[1:20,]
        })
        
        output$table.output22 <- renderTable({
          data()[1:20,]
        })
      }
      removeModal()
    }
  )
  
  ########################## table 2 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(data2(), {
    showModal(modalDialog("Loading", footer = NULL))
    
    if (format_file2() == 1)
      return()
    output$table.output2 <- DT::renderDataTable({
      data2()
    })
    removeModal()
  })
  
  ########################## graph 1 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      min_val(),
      max_val(),
      input$previous_eval_p,
      input$section_first_xlsx,
      input$section_first_txt
    ),
    {
      if (is.null(data_ok()) ||
          is.null(time_col()) &&
          is.null(drawdown_col) &&
          new_or_load() == 0 ||
          format_file1() == 1)
      {
        output$graph1 <- renderPlot({
          return(NULL)
        })
      }
      
      else {
        output$graph1 <- renderPlot({
          dat_slope(first_straight_section(data_ok(), section_first()))
          
          return(
            ggplot(data = data_ok()[1:end_t(),], aes(x = time,
                                                     y = drawdown)) +
              geom_point() + ggtitle("Data range for fit") + scale_x_log10() +
              labs(x = "Time", y = "Drawdown") +
              geom_abline(
                aes(
                  slope = as.numeric(dat_slope()$coefficients[2]),
                  intercept = as.numeric(dat_slope()$coefficients[1]),
                  color = "slope"
                )
              ) +
              labs(color = "")
            
            + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")
            ) +
              theme(plot.title = element_text(size = 18)) +
              theme(text = element_text(size = 16))
          )
          
        })
      }
    }
  )
  
  
  ########################## graph transmissivity  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      input$trans_lim,
      input$eval_param_x,
      input$eval_param_t,
      input$eval_param_p,
      input$go_to_eval
    ),
    {
      
      if (eval_p() == 0 || slider_tr() == "no")
        return ()
      
      output$graph1 <- renderPlot({
        return(
          ggplot(data_ok()[1:end_t(),], aes(x = time,
                                            y = drawdown)) +
            geom_point() + ggtitle("Data range for evaluate transmissivity") + scale_x_log10() +
            labs(x = "Time [s]", y = "Drawdown [m]") +
            geom_point(
              data = data_ok()[input$trans_lim[1]:input$trans_lim[2], ],
              aes(x = time,
                  y = drawdown,
                  color = "Selected - transmissivity")
            ) +
            scale_color_manual(values = "blue") +
            
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")
            ) +
            theme(plot.title = element_text(size = 18)) +
            theme(text = element_text(size = 16))
        )
      },height = 588, width = 1090
      )
    }
  )
  
  ########################## graph 2 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      min_val(),
      max_val(),
      input$previous_eval_p,
      input$section_first_xlsx,
      input$section_first_txt,
      slider_well_storativity_val()
    ),
    {
      if (is.null(data_ok()) ||
          is.null(time_col()) &&
          is.null(drawdown_col) &&
          new_or_load() == 0 ||
          format_file1() == 1 ||
          is.null(slider_well_storativity_val()) )
      {
        output$graph2 <- renderPlot({
          return(NULL)
        })
      }
      
      else {
    
       
    
        output$graph2 <- renderPlot({
      
          return(
            ggplot(data = data_ok()[1:40,], aes(x = time,
                                                     y = drawdown)) +
              geom_point() +
              
              scale_y_log10() +
              scale_x_log10()+
              
              labs(color = "") +
              
              geom_point(
                data = data_ok()[slider_well_storativity_val()[1]:slider_well_storativity_val()[2], ],
                aes(x = time,
                    y = drawdown,
                    color = "Selected - well storativity"
                    )
              )            +
              scale_color_manual(values = "red")
            
            +
              
              theme(legend.position = "bottom")
            
                        +
              
              ggtitle("Data range for fit") + 
            
           
              labs(x = "Time", y = "Drawdown") 
            +
              
              theme(plot.title = element_text(size = 18)) +
              theme(text = element_text(size = 16)) 
        
           +
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")
             )
            
            
          )
          
        })
      }
    }
  )
  
  
  ########################## setup loaded  ------------------------------------------------------------------------------------------------
  
  load_set <- reactiveVal (0)
  
  observeEvent(input$file1, {
    load_set(0)
  })
  
  observeEvent(input$file_project, {
    load_set(1)
  })
  
  output$loaded_setup <- reactive ({
    load_set()
  })
  
  outputOptions(output, "loaded_setup", suspendWhenHidden = FALSE)
  
  
  
  ########################## Main process   ------------------------------------------------------------------------------------------------
  
  well_storativity_result <- reactiveVal(NULL)
  skin_result <- reactiveVal(NULL)

  
  observeEvent(c(input$Q, input$Trans, input$Stor, input$R, cbar(), dat_slope()),
               {
                 if(is.null(input$Q) ||  is.null(input$Trans) || is.null(input$Stor) || is.null(input$R) || is.null(cbar()) || is.null(dat_slope()))
                   return()
                 
                 
                                  skin_by_t = skin_by_time(input$Q, input$Trans, input$Stor, input$R, dat_slope(), cbar())
                                  skin_result(skin_by_t$hw)
                                  
                                  
                                  
                                  
                                  setupWT_main = data_to_report()
                                  setupWT_main$additional_resistance = skin_by_t$hw
                                  setupWT_main$skin_faktor = skin_by_t$w
                                  data_to_report(setupWT_main)
                                   })
  

  
  observeEvent(c(input$Stor, input$R, input$Q, input$slider_well_storativity),
               {
                 if(is.null(input$Q) ||  is.null(input$Stor) || is.null(input$R) || is.null(input$slider_well_storativity) )
                   return()
                 
                 mean_t = mean(data_ok()[input$slider_well_storativity[1]:input$slider_well_storativity[2], 1])
                   mean_s =  mean(data_ok()[input$slider_well_storativity[1]:input$slider_well_storativity[2], 2])
                   
                 c = input$Q*(mean_t/mean_s)
                 vysledek = c/(2*3.14*(input$R^2)* input$Stor)
                 
                 
                 cbar(vysledek)
                 
                 setupWT_main = data_to_report()
                 setupWT_main$well_stor = vysledek
                 data_to_report(setupWT_main)
                 
               })
  
  
  
  
  
  
  
  
  ########################## end of main process ------------------------------------------------------------------------------------------
  
  observeEvent(input$file1, {
    report_cond(0)
  })
  
  output$report_condition <- reactive ({
    report_cond()
  })
  
 
  
  observeEvent(input$go_to_report, {
    updateTabsetPanel(session, "navbar", "Report")
  })
  

  
  outputOptions(output, "report_condition", suspendWhenHidden = FALSE)
  
  ####################### render final report  -----------------------------------------------------------------------------------------------
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Pumping test - report', sep = '.', switch(
        input$format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      showConnections(all = TRUE)
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(),
        HTML = html_document(),
        Word = word_document()
      ))
      file.rename(out, file)
      
    }
  )
  
  
  
  additional_from_slope = reactiveVal()
  
  
  observeEvent(input$show_report, {
    showModal(modalDialog("Loading", footer = NULL))
    
   
    
    
    output$report <- renderUI({
      src2 <- normalizePath('reporrr.Rmd')
      library(knitr)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      knitr::opts_knit$set(root.dir = owd)
      
      tagList(HTML(knitr::knit2html(
        text = readLines(src2), fragment.only = TRUE
      )),
      
      tags$script(HTML(
        'MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'
      )),
      
      tags$script(
        HTML(
          "if (hljs) $('#report pre code').each(function(i, e) {
                       hljs.highlightBlock(e)
                 });"
        )
      ))
    })
    
    
    removeModal()
  })
  
  ############################## btn class ---------------------------------------------------------------------------------------------------------------------------------
  
  output$go_to_evall <- renderUI({
    if (new_proj() == 1)
      cls = "btn-primary"
    
    if (new_proj() == 2)
      cls = "btn-secundary"
    
    actionButton(
      inputId = "input_parameters_from_file",
      label = "Find parameters in file",
      class = "btn-primary",
      width = "180"
    )
  })
  
  ############################## stages ---------------------------------------------------------------------------------------------------------------------------------
  
  ############################### new or project ---------------------------------------------------------------------------------------------------------
  
  new_proj <- reactiveVal (0)
  
  observeEvent(input$new_project, {
    new_proj(1)
    updateTabsetPanel(session, "main1", "panel11")
    updateTabsetPanel(session, "main2", "panel1")
  })
  
  observeEvent(input$load_project, {
    new_proj(2)
    updateTabsetPanel(session, "main1", "panel22")
    updateTabsetPanel(session, "main2", "panel2")
  })
  
  output$new_orproj <- reactive ({
    new_proj()
  })
  
  outputOptions(output, "new_orproj", suspendWhenHidden = FALSE)
  
  ############################### 1.1 ---------------------------------------------------------------------------------------------------------
  
  stage <- reactiveVal(1)
  
  #### previous --------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous1.1, {
    new_proj(1)
  })
  
  observeEvent(input$previous1.1.1, {
    stage(1)
  })
  
  observeEvent(input$previous1.1.1x, {
    stage(1)
  })
  
  observeEvent(input$previous1.1.2x, {
    stage(111)
  })
  
  observeEvent(input$previous1.1.2t, {
    stage(111)
  })
  
  #### next --------------------------------------------------------------------
  
  format_file1 <- reactiveVal(0)
  
  observeEvent(input$file1, {
    if (format_file1() == 1)
      return ()
    
    else
      stage(111)
  })
  
  observeEvent(input$next1.1, {
    if (is.null(input$file1) || is.null(file_t()))
      return()
    
    else
      stage(111)
    
  })
  
  observeEvent(input$next1.1.1t, {
    stage(112)
    updateTabsetPanel(session, "main1", "panel22")
  })
  
  observeEvent(input$next1.1.1x, {
    if (is.null(data()))
      return()
    
    stage(112)
    updateTabsetPanel(session, "main1", "panel22")
  })
  
  output$format_file1 <- reactive ({
    format_file1()
  })
  
  outputOptions(output, "format_file1", suspendWhenHidden = FALSE)
  
  ################################# 1.2 -----------------------------------------------------------------------------------------------------
  
  ####  previous ----------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous1.2, {
    stage(1)
  })
  
  output$stagee <- reactive ({
    stage()
  })
  
  outputOptions(output, "stagee", suspendWhenHidden = FALSE)
  
  #################################   2. page -----------------------------------------------------------------------------------------------
  
  
  observeEvent(c(
    input$next_to_param_xlsx,
    input$next_to_param_txt,
    input$next_proj1t
  ),
  {
    if (input$next_to_param_xlsx == 0 &&
        input$next_to_param_txt == 0 && input$next_proj1t == 0)
      return()
    
    updateTabsetPanel(session, "navbar", "Input parameters")
    
    
    updateTabsetPanel(session, "main2", "panel22")
    stage2(2)
  })
  
  observeEvent(input$prev_to_file,  {
    if (input$prev_to_file == 0 || is.null(time_col()))
      return()
    
    updateTabsetPanel(session, "navbar", "Input file")
    
    if (new_or_load() == 1)
      proj_stag(1)
    
    else
      stage(112)
  })
  
  observeEvent(input$previous_to_file, {
    stage(112)
  })
  
  stage2 <- reactiveVal(2)
  
  #### previous -----------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous21, {
    stage2(2)
  })
  
  observeEvent(input$previous22, {
    stage2(2)
  })
  
  observeEvent(input$previous221, {
    stage2(22)
  })
  
  observeEvent(input$previous23, {
    stage2(2)
  })
  
  observeEvent(input$go_to_eval, {
    updateTabsetPanel(session, "navbar", "Input parameters")
    stage2(23)
  })
  
  observeEvent(c(input$go_to_start, input$go_to_start2), {
    stage(1)
    proj_stag(0)
    updateTabsetPanel(session, "navbar", "Input file")
  })
  
  observeEvent(input$previous222, {
    stage2(221)
  })
  
  observeEvent(input$previous223, {
    stage2(221)
  })
  
  #### next ----------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$next2.1, {
    stage2(21)
  })
  
  observeEvent(input$next21, {
    stage2(23)
  })
  
  observeEvent(input$next_stage2_0, {
    stage2(23)
  })
  
  ##### 22 input file 2 -----------------------------------------------------------------------------------------------------------------------
  
  format_file2 <- reactiveVal(0)
  
  observeEvent(input$input_parameters_from_file, {
    stage2(22)
  })
  
  observeEvent(input$yes22, {
    stage2(221)
  })
  
  observeEvent(input$no22, {
    stage2(2)
  })
  
  observeEvent(input$load_sheet2, {
    stage2(2)
  })
  
  observeEvent(input$next221, {
    if (is.null(input$file2))
      return ()
    
    else if (file_t2() == "xlsx")
      stage2(222)
    
    else
      stage2(223)
  })
  
  observeEvent(input$next223, {
    stage2(2)
  })
  
  observeEvent(input$next222, {
    stage2(2)
  })
  
  ##############
  
  output$stagee2 <- reactive ({
    stage2()
  })
  
  outputOptions(output, "stagee2", suspendWhenHidden = FALSE)
  
  ##############################  set limits yes or no ------------------------------------------------------------------------------------------------------
  
  limits <- reactiveVal(0)
  
  observeEvent(input$limit_set, {
    if (input$limit_set == "yes")
      limits(1)
    
    else
      limits(0)
  })
  
  output$set_limits <- reactive({
    limits()
  })
  
  outputOptions(output, "set_limits", suspendWhenHidden = FALSE)
  
  ########### file project stages --------------------------------------------------------------------------------------------------------------------------
  
  proj_stag <- reactiveVal(0)
  
  observeEvent(input$load_project, {
    proj_stag(0)
  })
  
  observeEvent(input$new_project, {
    proj_stag(0)
  })
  
  observeEvent(input$previous_proj0, {
    stage(1)
    proj_stag()
  })
  
  observeEvent(input$next_proj0, {
    if (is.null(input$file_project))
      return()
    
    else
      proj_stag(1)
  })
  
  observeEvent(input$file_project, {
    if (file_t_proj() == 1)
      return ()
    
    else
      proj_stag(1)
  })
  
  observeEvent(input$previous_proj1t, {
    proj_stag(0)
  })
  
  observeEvent(input$input$next_proj0, {
    proj_stag(1)
  })
  
  output$proj_stage <- reactive({
    proj_stag()
  })
  
  outputOptions(output, "proj_stage", suspendWhenHidden = FALSE)
  
  #### main data to react val -----------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$Q, {
    setupWT_main = data_to_report()
    setupWT_main$Q = input$Q
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$R, {
    setupWT_main = data_to_report()
    setupWT_main$R = input$R
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$Stor, {
    setupWT_main = data_to_report()
    setupWT_main$Stor = input$Stor
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$Trans, {
    setupWT_main = data_to_report()
    setupWT_main$Trans = input$Trans
    data_to_report(setupWT_main)
  })
  
  
  
  

  observeEvent(input$name_out_file, {
    setupWT_main = data_to_report()
    setupWT_main$name = input$name_out_file
    data_to_report(setupWT_main)
  })
  
  observeEvent(file_t(), {
    setupWT_main = data_to_report()
    setupWT_main$file_type1 = file_t()
    data_to_report(setupWT_main)
  })
  
  
  observeEvent(input$well_storativity, {
    setupWT_main = data_to_report()
    setupWT_main$well_storativity = input$well_storativity
    data_to_report(setupWT_main)
  })
  
  
  ####################### report contain  -----------------------------------------------------------------------------------------------
  
  eval_par_report <- reactiveVal ()
  
  observeEvent(input$report_contain, {
    setupWT_main = data_to_report()
    setupWT_main$report_contain = input$report_contain
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$well_location, {
    setupWT_main = data_to_report()
    setupWT_main$well_location = input$well_location
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$well_location <<- input$well_location
      save(setupWT, file = path2())
    }
    wel_loc(input$well_location)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$well_depth, {
    setupWT_main = data_to_report()
    setupWT_main$well_depth = input$well_depth
    
    if (exists("setupWT") && path2_ok() == 1)
      save(setupWT, file = path2())
    
    wel_depth(input$well_depth)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$aqua_thick, {
    setupWT_main = data_to_report()
    setupWT_main$aqua_thick = input$aqua_thick
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$aqua_thick <<- input$aqua_thick
      save(setupWT, file = path2())
    }
    aqua_thick(input$aqua_thick)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$param_to_eval, {
    if (sum(input$param_to_eval == "Transmissivity") == 1)
      slider_tr("yes")
    
    else
      slider_tr("no")
  })
  
  observeEvent(input$param_to_eval, {
    if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 ||
        sum(input$param_to_eval == "Cone of depression") == 1)
      thickness("yes")
    
    else
      thickness("no")
  })
  
  cone <- reactiveVal(0)
  cond <- reactiveVal(0)
  trans <- reactiveVal (0)
  
  observeEvent(c(
    input$param_to_eval,
    input$aqua_thick,
    thickness(),
    input$show_report
  ),
  {
    if (is.null(input$aqua_thick))
      return()
    
    if (!is.null(input$Trans))
      trans(input$Trans)
    
    else if (exists("setupWT"))
      trans(setupWT$Transmiss)
    
    
    if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 &&
        sum(input$param_to_eval == "Cone of depression") == 1)
    {
      cond(trans() / input$aqua_thick)
      cone(3000 * end_t_lvl() * cond() ^ (0.5))
    }
    
    else if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 &&
             sum(input$param_to_eval == "Cone of depression") != 1)
    {
      cond(trans() / input$aqua_thick)
    }
    
    else if (sum(input$param_to_eval == "Hydraulic conductivity") != 1 &&
             sum(input$param_to_eval == "Cone of depression") == 1)
    {
      cond(trans() / input$aqua_thick)
      cone(3000 * end_t_lvl() * cond() ^ (0.5))
      cond(0)
    }
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$hydraulic_cond <<- cond()
      setupWT$cone_of_depr <<- cone()
    }
    
    setupWT_main = data_to_report()
    setupWT_main$hydraulic_cond = cond()
    setupWT_main$cone_of_depr = cone()
    
    data_to_report(setupWT_main)
    eval_par_report(0)
    
    if (exists("setupWT") && path2_ok() == 1)
      save(setupWT, file = path2())
  })
  
  size_text <- reactiveVal()
  size_title <- reactiveVal()
  graph_size <- reactiveVal()
  mark_format <- reactiveVal()
  nav_change <- reactiveVal(0)
  
  observeEvent(input$format, {
  
    if (input$format == "HTML") {
      size_text(12)
      size_title(17)
      mark_format("html")
    }
    
    if (input$format == "Word") {
      size_text(8)
      size_title(11)
      graph_size(11)
      mark_format("docx")
    }
  })
  
  onStop(function() {
    to_del = c("cores",
               "packages",
               "set_cores",
               "ipak",
               "setupWT",
               "obsSnorm",
               "Gwinput")
    
    for (i in 1:5)
    {
      if (exists(to_del[i]))
        remove(list = to_del[i],
               pos = 1,
               inherits = TRUE)
    }
    
  })
  
  
  output$nav_changee <- reactive ({
    nav_change()
  })
  
  outputOptions(output, "nav_changee", suspendWhenHidden = FALSE)
  
  
  
  ############ update plot 1
  
  observeEvent(input$next1.1.1t, {
    updateTabsetPanel(session, "main1", "panel22")
    updateTabsetPanel(session, "main2", "panel22")
  })
  
  
  
  
  
  
  ############### slider for evaluate well storativity
  
  

  
  
  output$slider_well_stor <- renderUI({
    sliderInput(
      "slider_well_storativity",
      label = NULL,
      min = 1,
      max = 40,
      if (exists("setupWT") && new_or_load() == 1)
        value = c(setupWT$slider_well_storativity[1], setupWT$slider_well_storativity[2])
      
      else
      value = c(2,6)
    )
  })

  
  
  ########## well storativity
  

  
  
  
  slider_well_storativity_val <- reactiveVal (NULL)
  
  observeEvent(input$slider_well_storativity,
               {
                 slider_well_storativity_val(input$slider_well_storativity)
                 
                 
                 
                 setupWT_main = data_to_report()
                 setupWT_main$slider_well_storativity = slider_well_storativity_val()
                 data_to_report(setupWT_main)
                 
               })
  

  
  
  
  
  
  observeEvent(c(input$file1, input$file_project, cbar()), {
    
    output$well_stor <- renderUI({
      numericInput(
        inputId = "well_storativity",
        label = NULL,
        if (exists("setupWT") &&
            new_or_load() == 1) 
            value = setupWT$well_stor
        
        else
        {
          if(!is.na(cbar()) && !is.null(cbar()))
            value = round(cbar(),7)
          
          else
            value = NA
        }
        ,
        min = 0,
        max = 1000
      )
    })
  })
  
  
  
  dir_ok <- reactiveVal(0)
  
  observeEvent(input$save_project,
               
               {
                 if(dir_ok() == 1)
                 {
                   path1 <- paste0(path(), "/", input$name_out_file, "-results")
                   pathh2 <- paste0(path(), "/", input$name_out_file, "-project.wt")
                   
                   
                   
                     if (!dir.exists(path()))
                     {
                       dir.create(path())
                     }
                     
                   setupWT = data_to_report()
         
                   path2(pathh2)
                   save(setupWT, file = path2())
                   
                 }
               })
  
  
  observeEvent(input$dirr,
               {
                 if(is.na(input$dirr)|| !is.null(input$dirr))
                 dir_ok(1)
               })
               

  
  
  output$dir_select <- renderUI({
    
      return(shinyDirButton(
        "dirr",
        "Chose directory",
        title = "",
        class = "btn-primary"
      ))
  })

  
  
  
  
  volumes = getVolumes()()
  
  shinyDirChoose(input, 'dirr', roots = volumes)
  
  dir <- reactive(input$dirr)
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 c(input$dirr, input$name_out_file)
               },
               handlerExpr = {
                 req(is.list(input$dirr))
                 path_ok(1)
                 roots = getVolumes()
                 p <- parseDirPath(roots, dir())
                 p <- paste0(p, "/", input$name_out_file)
                 path(p)
               })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  

}
