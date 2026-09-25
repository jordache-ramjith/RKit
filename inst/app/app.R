library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
source('engine.R', local=TRUE)
source('content.R', local=TRUE)
source('estimation.R', local=TRUE)
source('comparisons.R', local=TRUE)
source('estimation_lessons.R', local=TRUE)
source('learner_code.R', local=TRUE)
source('hypothesis.R', local=TRUE)
source('hypothesis_code.R', local=TRUE)
source('hypothesis_lessons.R', local=TRUE)
source('regression.R', local=TRUE)
source('regression_code.R', local=TRUE)
source('regression_lessons.R', local=TRUE)
options(shiny.maxRequestSize = 20 * 1024^2)

card_button <- function(id, title, subtitle, number, disabled=FALSE) {
  tags$button(id=id,type='button',class='action-button topic-card',disabled=if(disabled)'disabled' else NULL,
    tags$span(class='topic-number',number),tags$span(class='topic-title',title),
    tags$span(class='topic-copy',subtitle),if(disabled)tags$span(class='tag later',style='margin-top:14px','Next stage'))
}
help_box <- function(text, class='') div(class=paste('hint',class),text)
scroll_table <- function(id) div(class='table-scroll',tableOutput(id))
scroll_plot <- function(id, height, min_width=540) tagList(
  p(class='small-muted plot-hint','Scroll inside the figure if needed to see all labels and panels.'),
  div(class='plot-scroll',tabindex='0',role='region',`aria-label`='Scrollable figure',
    div(class='plot-canvas',style=paste0('min-width:',min_width,'px;'),
      plotOutput(id,height=paste0(height,'px')))))
code_box <- function(id, copy, download, zipid=NULL) tags$details(tags$summary('Show R code — step by step'),
  p(class='small-muted','A student script with import, preparation and the statistical calculations. Figures use simple styling; tables may have a different layout from the app.'),
  div(class='nav-actions',actionButton(copy,'Copy code'),downloadButton(download,'Download student script'),
    if(!is.null(zipid))downloadButton(zipid,'Script + original data')),
  verbatimTextOutput(id),
  tags$details(tags$summary('Optional: exact app script'),
    p(class='small-muted','This longer script reproduces the app’s full tables, figure styling and checks. It is also included as analysis_exact.R in the ZIP.'),
    downloadButton(paste0(download,'_exact'),'Download exact app script')))
example_path <- normalizePath('../extdata/wellbeing.csv',mustWork=TRUE)
example_meta <- list(kind='csv',name='wellbeing.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'),example=TRUE)
example_raw <- read_source(example_path,example_meta)

ui <- fluidPage(theme=bs_theme(version=5,primary='#087F8C',base_font='system-ui'),
 tags$head(tags$link(rel='stylesheet',href='style.css'),
   tags$script(HTML("$(document).on('shiny:connected',function(){Shiny.setInputValue('viewport_width',window.innerWidth);});let resizeTimer;$(window).on('resize',function(){clearTimeout(resizeTimer);resizeTimer=setTimeout(function(){Shiny.setInputValue('viewport_width',window.innerWidth);},250);});")),tags$title('R Kit — Learn statistics'),
   tags$script(HTML("Shiny.addCustomMessageHandler('copyCode',function(txt){if(navigator.clipboard){navigator.clipboard.writeText(txt).then(function(){Shiny.setInputValue('copy_done',Date.now());}).catch(function(){window.prompt('Copy this code:',txt);});}else{window.prompt('Copy this code:',txt);}});"))),
 div(class='app-shell',div(class='topbar',div(class='brand',span(class='brandmark','R Kit'),span('Learn statistics')),span(class='version','Review version 0.5.1 · Explore your data')),
 uiOutput('page_ui'),div(class='footer',p('Developed by Jordache Ramjith, PhD @ Radboud university medical center, Department of IQ Health, Nijmegen, Netherlands'),a(href='mailto:jordache.ramjith@radboudumc.nl','jordache.ramjith@radboudumc.nl'))))

server <- function(input,output,session) {
 upload_ready <- reactiveVal(TRUE)
 observeEvent(input$upload,upload_ready(TRUE),ignoreNULL=TRUE)
 page <- reactiveVal('home'); data_stage <- reactiveVal('import'); route <- reactiveVal('num')
 rv <- reactiveValues(raw=NULL,meta=NULL,source=NULL,steps=list(),draft=NULL,draftmeta=NULL,draftpath=NULL,
   pending=list(),candidate=NULL,result=NULL,spec=NULL,result_steps=NULL,lesson=1,tab='Analyse')
 session$onSessionEnded(function(){old<-isolate(rv$source);if(!is.null(old)&&file.exists(old))unlink(old)})
 data <- reactive({req(rv$raw);apply_steps(rv$raw,rv$steps)})
 notify_error <- function(e) showNotification(conditionMessage(e),type='error',duration=9)
 clear_result <- function(){rv$result<-NULL;rv$spec<-NULL;rv$result_steps<-NULL;if(exists('clear_estimation'))clear_estimation();if(exists('clear_hypothesis'))clear_hypothesis();if(exists('clear_regression'))clear_regression()}
 use_draft <- function(){
   req(rv$draft); rv$raw<-rv$draft;rv$meta<-rv$draftmeta
   ext<-if(rv$meta$kind=='xlsx')'.xlsx' else '.csv'; target<-tempfile(fileext=ext)
   if(!file.copy(rv$draftpath,target,overwrite=TRUE))stop('The file could not be prepared for this session. Please import it again.')
   old<-rv$source;if(!is.null(old)&&file.exists(old))unlink(old);rv$source<-target
   rv$steps<-list();rv$pending<-list();rv$candidate<-NULL;clear_result()
   data_stage('types');page('data')
 }
 observeEvent(input$go_home,page('home'))
 guide_target <- reactiveVal('start')
 observeEvent(input$go_rguide,{guide_target('start');page('rguide')})
 observeEvent(input$go_rvideo,{guide_target('video');page('rguide')})
 observeEvent(input$go_import,{page('data');data_stage(if(is.null(rv$raw))'import' else 'prepare')})
 observeEvent(input$reset_import,{
   old<-rv$source;if(!is.null(old)&&file.exists(old))unlink(old)
   rv$raw<-NULL;rv$meta<-NULL;rv$source<-NULL;rv$steps<-list()
   rv$draft<-NULL;rv$draftmeta<-NULL;rv$draftpath<-NULL
   rv$pending<-list();rv$candidate<-NULL;rv$lesson<-1;upload_ready(FALSE);clear_result()
   data_stage('import');page('data')
 })
 observeEvent(input$go_describe,page('choices'))
 observeEvent(input$continue_prepare,data_stage('prepare'))
 observeEvent(input$change_question,page('choices'))
 observeEvent(input$example_home,{
   rv$draft<-example_raw;rv$draftmeta<-example_meta;rv$draftpath<-example_path
   if(!is.null(rv$raw))showModal(modalDialog(title='Replace the current dataset?',p('Loading the example clears your current preparation steps and results.'),footer=tagList(modalButton('Keep current data'),actionButton('confirm_use','Use new dataset',class='btn-primary')))) else use_draft()
 })
 observeEvent(input$confirm_use,{removeModal();use_draft()})
 for(key in names(route_names)) local({k<-key;observeEvent(input[[paste0('choose_',k)]],{route(k);rv$lesson<-1;clear_result();page('analysis')})})
 for(st in c('import','types','prepare'))local({stage<-st;observeEvent(input[[paste0('stage_',stage)]],{data_stage(stage);rv$candidate<-NULL})})
 output$page_ui<-renderUI({
   if(page()=='home')return(tagList(
     div(class='hero',div(p(class='eyebrow','A practical introduction to statistics'),h1('Understand your data.\nLearn the R behind it.'),p('Start with a question. Explore a plot. Learn what the numbers mean — and take the complete R code with you.')),
       div(class='data-card',span(class='tag',if(is.null(rv$raw))'START HERE' else 'YOUR DATA'),h3(if(is.null(rv$raw))'Bring a dataset, or try ours' else rv$meta$name),
         if(is.null(rv$raw))p('Upload a CSV or Excel file. Our simulated study is ready if you would like to practise first.') else div(class='metric-row',div(class='metric',tags$b(nrow(data())),span('observations')),div(class='metric',tags$b(ncol(data())),span('variables')),div(class='metric',tags$b(length(rv$steps)),span('preparation steps'))),
         div(class='nav-actions',actionButton('go_import',if(is.null(rv$raw))'Import & prepare data' else 'Review & prepare data',class='btn-primary'),actionButton('example_home','Try example data')),
         if(!is.null(rv$raw))div(class='reset-data',actionButton('reset_import','Clear data & import new',icon=icon('rotate'),title='Clear the current dataset, preparation steps and results, then import another file.')))),
     div(class='section-head',h3('What would you like to do?'),span(class='small-muted','Choose a topic • Learn at your own pace')),
     div(class='topic-grid',card_button('go_describe','Descriptive statistics','Get to know your variables, compare distributions and explore relationships.','01 · AVAILABLE'),
       card_button('go_estimation','Estimation','Estimate a mean or proportion, and explore sampling variation and confidence intervals.','02 · AVAILABLE'),
       card_button('go_hypothesis','Hypothesis testing','Work through a question, inspect assumptions and understand the test result.','03 · AVAILABLE'),
       card_button('go_regression','Linear regression','Understand relationships, adjustment and interactions.','04 · AVAILABLE')),
     div(class='panel-card r-guide-entry',h3('New to R? Start here'),p('Find your way around RStudio, create a project and script, then import data and learn to read R code.'),div(class='nav-actions',actionButton('go_rguide','Open the beginner guide',class='btn-primary'),actionButton('go_rvideo','Watch data visualization')))))
   if(page()=='rguide')return(tagList(
     div(class='section-head',div(p(class='eyebrow','New to R'),h2('Getting started with R')),actionButton('go_home','Home')),
     div(class='nav-actions guide-actions',tags$a(href=paste0('r-guide/data-manipulation.html#',guide_target()),target='_blank',rel='noopener',class='btn btn-default','Open guide in a new tab'),tags$a(href='r-guide/practice.zip',download='practice.zip',class='btn btn-default','Download practice files')),
     tags$iframe(src=paste0('r-guide/data-manipulation.html#',guide_target()),title='Getting started with R and preparing data',class='r-guide-frame',allow='fullscreen',allowfullscreen=NA)))
   if(page()=='reg_choices')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Linear regression'),h2('What would you like to understand?')),actionButton('go_home','Home')),
     p('Choose a starting point. Each has Analyse and Tutorial tabs; the outcome must be a numerical measurement.'),
     div(class='choice-grid',lapply(names(reg_routes),function(k)card_button(paste0('reg_choose_',k),reg_routes[[k]],reg_prompts[[k]],sprintf('%02d',match(k,names(reg_routes)))))),
     help_box('These are ordinary linear models for independent observations. Binary outcomes, clustered observations and repeated visits need other models.')))
   if(page()=='reg_analysis')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Linear regression'),h2(reg_routes[[greg$route]])),div(class='nav-actions',actionButton('go_home','Home'),actionButton('reg_change','Change question'))),
     tabsetPanel(id='reg_mode',selected='Analyse',tabPanel('Analyse',uiOutput('reg_analysis_ui')),tabPanel('Tutorial',uiOutput('reg_tutorial_ui')))))
   if(page()=='h_choices')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Hypothesis testing'),h2('What question would you like to investigate?')),actionButton('go_home','Home')),
     p('Choose the measurements and study structure first. Each route has just two tabs: Analyse and Tutorial.'),
     div(class='choice-grid',lapply(names(h_routes),function(k)card_button(paste0('h_choose_',k),h_routes[[k]],h_prompts[[k]],sprintf('%02d',match(k,names(h_routes)))))),
     help_box('Independent groups contain different people. Paired measurements belong to the same people or matched pairs. This teaching module does not cover clustered studies, survey weights or repeated measurements at three or more times.')))
   if(page()=='h_analysis')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Hypothesis testing'),h2(h_routes[[h$route]])),div(class='nav-actions',actionButton('go_home','Home'),actionButton('h_change_question','Change question'))),
     tabsetPanel(id='h_mode',selected='Analyse',tabPanel('Analyse',uiOutput('h_analysis_ui')),tabPanel('Tutorial',uiOutput('h_tutorial_ui')))))
   if(page()=='est_choices')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Estimation'),h2('What would you like to estimate?')),actionButton('go_home','Home')),
     p('Do you want to describe one population, or compare two groups of different people?'),
     div(class='choice-grid',card_button('est_choose_mean','A mean','Estimate the average of a numerical measurement, such as hours of sleep.','01'),
       card_button('est_choose_proportion','A proportion','Estimate the percentage with one of two possible outcomes, such as Yes or No.','02'),
       card_button('est_choose_mean2','Means in two groups','Estimate each mean and the difference between them.','03'),
       card_button('est_choose_proportion2','Proportions in two groups','Compare percentages, then explore differences, risk ratios and odds ratios.','04')),
     help_box('An estimate describes your sample. A confidence interval expresses uncertainty about the corresponding population value.')))
   if(page()=='est_analysis')return(tagList(
     div(class='section-head',div(p(class='eyebrow','Estimation'),h2(if(isTRUE(est$compare))comparison_title(est$kind)else if(est$kind=='mean')'Estimate a mean'else'Estimate a proportion')),
       div(class='nav-actions',actionButton('go_home','Home'),actionButton('est_change_question','Change question'))),
     tabsetPanel(id='est_mode',selected='Analyse',tabPanel('Analyse',uiOutput('est_analysis_ui')),tabPanel('Tutorial',uiOutput('est_tutorial_ui')))))
   header<-div(class='section-head',div(p(class='eyebrow',if(page()=='data')'Your data workspace' else 'Descriptive statistics'),h2(if(page()=='data')'Import and prepare' else if(page()=='choices')'What would you like to explore?' else route_names[[route()]])),div(class='nav-actions',actionButton('go_home','Home'),if(page()=='analysis')actionButton('change_question','Change question')))
   if(page()=='choices')return(tagList(header,p('Choose the question first. We will help you choose suitable variables, then explain the output.'),
     div(class='choice-grid',lapply(names(route_names),function(k)card_button(paste0('choose_',k),route_names[[k]],paste(route_prompts[[k]],route_examples[[k]]),sprintf('%02d',match(k,names(route_names)))))),
     help_box('Categorical variables name groups. Numerical variables measure amounts. A column of numbers can still be categorical if the numbers are group codes.')))
   if(page()=='data')return(tagList(header,div(class='stepbar',actionButton('stage_import','1  Import'),actionButton('stage_types','2  Understand variables'),actionButton('stage_prepare','3  Prepare')),uiOutput('data_ui')))
   tagList(header,tabsetPanel(id='mode',selected='Analyse',
     tabPanel('Analyse',uiOutput('analysis_ui')),tabPanel('Tutorial',uiOutput('tutorial_ui'))))
 })
 output$data_ui<-renderUI({
   if(data_stage()=='import')return(div(class='two-col',div(class='panel-card',h3('1. Choose a data file'),p(class='small-muted','Each row should be one observation; the first row should contain column names. Files stay in this app session.'),
     fileInput('upload','CSV, text or Excel (.xlsx)',accept=c('.csv','.txt','.tsv','.xlsx')),
     uiOutput('sheet_ui'),selectInput('separator','CSV separator',choices=c('Comma'=',','Semicolon'=';','Tab'='\t')),
     selectInput('decimal','Decimal mark',choices=c('Point'='.', 'Comma'=',')),selectInput('encoding','Text encoding',choices=c('UTF-8','Latin1')),
     textInput('na_values','Missing-value labels (comma-separated)','NA'),p(class='small-muted','Empty cells are also treated as missing. Do not add 0 unless it really means missing.'),
     actionButton('preview_import','Preview file',class='btn-primary'),hr(),actionButton('load_example','Preview example study')),
     div(class='panel-card',h3('2. Check before using it'),uiOutput('import_preview'))))
   if(is.null(rv$raw))return(div(class='panel-card empty',h3('Start by importing a dataset'),p('Use the Import step or load the built-in example.')))
   if(data_stage()=='types')return(tagList(help_box('What does one row represent? Check that each column means what you think it means. Types below are inferred from the file; number-coded categories need your judgement.'),
     div(class='two-col',div(class='panel-card',h3('Set a variable type'),selectInput('type_column','Variable',names(data())),selectInput('type_target','Treat this variable as',choices=c('Categorical — named groups'='categorical','Numerical — a measurement'='numerical','Text / ID — a label'='text')),
       actionButton('apply_type','Apply type',class='btn-primary'),help_box(prep_help$type[2]),actionButton('continue_prepare','Continue to preparation')),
       div(class='panel-card',h3('Your variable guide'),scroll_table('dictionary'))),uiOutput('example_dictionary')))
   tagList(help_box('Preparation is optional. Make a change only when it helps answer your question. Each applied step is recorded in your script, and you can undo the last step.'),
     div(class='two-col',div(class='panel-card sticky-controls',h3('Choose a preparation step'),radioButtons('prep_kind',NULL,choices=c('Filter observations'='filter','Create or recode a variable'='derive'),selected='filter'),uiOutput('prep_controls')),
       div(class='panel-card',h3('Current data'),uiOutput('data_dimensions'),scroll_table('data_preview'),uiOutput('candidate_preview'),h3('Your preparation history'),uiOutput('history'),div(class='nav-actions',actionButton('undo','Undo last step'),actionButton('go_describe','Explore these data',class='btn-primary')))),
       code_box('prep_code','copy_prep','download_prep','zip_prep'))
 })
 output$sheet_ui<-renderUI({req(upload_ready(),input$upload);if(tolower(tools::file_ext(input$upload$name))!='xlsx')return(NULL)
   tryCatch(selectInput('sheet','Excel sheet',readxl::excel_sheets(input$upload$datapath)),error=function(e)help_box(conditionMessage(e)))})
 observeEvent(input$preview_import,{
   tryCatch({req(upload_ready(),input$upload);kind<-if(tolower(tools::file_ext(input$upload$name))=='xlsx')'xlsx' else 'csv'
     meta<-list(kind=kind,name=basename(input$upload$name),delim=input$separator,decimal=input$decimal,encoding=input$encoding,na=unique(c('',trimws(strsplit(input$na_values,',',fixed=TRUE)[[1]]))),sheet=input$sheet,example=FALSE)
     if(kind=='xlsx'&&!nonempty(meta$sheet))stop('Choose an Excel sheet.')
     candidate<-read_source(input$upload$datapath,meta)
     rv$draft<-candidate;rv$draftmeta<-meta;rv$draftpath<-input$upload$datapath
   },error=notify_error)
 })
 observeEvent(input$load_example,{rv$draft<-example_raw;rv$draftmeta<-example_meta;rv$draftpath<-example_path})
 observeEvent(input$use_import,{
   req(rv$draft);if(!is.null(rv$raw))showModal(modalDialog(title='Replace the current dataset?',p('This clears the existing preparation history and results.'),footer=tagList(modalButton('Cancel'),actionButton('confirm_use','Use new dataset',class='btn-primary'))))else use_draft()
 })
 output$import_preview<-renderUI({if(is.null(rv$draft))return(help_box('Preview your file or the example to see its rows and columns here.'))
   tagList(p(paste(nrow(rv$draft),'rows ·',ncol(rv$draft),'columns')),scroll_table('draft_table'),
     help_box('Check: are the column names correct? Are numbers in separate columns? Do missing cells and decimal values look right?'),
     if(isTRUE(rv$draftmeta$example))p(class='small-muted','Simulated wellbeing study. These are teaching data, not findings about a real intervention.'),
     actionButton('use_import','Use this dataset',class='btn-primary'))})
 output$draft_table<-renderTable(head(rv$draft,8),striped=TRUE,rownames=FALSE)
 output$dictionary<-renderTable({d<-data();data.frame(Variable=names(d),Type=vapply(d,function(x)if(is.numeric(x))'Numerical'else if(is.factor(x))'Categorical'else'Text / categorical candidate',character(1)),Missing=vapply(d,function(x)sum(is.na(x)),integer(1)),Distinct=vapply(d,function(x)length(unique(na.omit(x))),integer(1)),check.names=FALSE)},striped=TRUE)
 output$example_dictionary<-renderUI({req(rv$meta);if(!isTRUE(rv$meta$example))return(NULL)
   div(class='panel-card data-dict',style='margin-top:20px',h3('About the example'),p('120 simulated participants; one row per participant. Compare descriptions without making real treatment claims.'),tags$dl(tags$dt('programme / smoking'),tags$dd('Programme group and smoking category.'),tags$dt('age / sleep_hours'),tags$dd('Age in years and sleep per night in hours.'),tags$dt('short_sleep'),tags$dd('Yes if recorded sleep is below 7 hours; No otherwise. Missing sleep stays missing. A teaching category, not a diagnosis.'),tags$dt('wellbeing_score'),tags$dd('A simulated score from 0 to 100; higher means greater reported wellbeing.'),tags$dt('marker'),tags$dd('A fictional positive laboratory measurement in arbitrary units, included to explore right skew and logarithms.'),tags$dt('participant_id'),tags$dd('A unique label, not a measurement to average.')))})
 append_step<-function(s){check_step(data(),s);rv$steps<-c(rv$steps,list(s));rv$candidate<-NULL;clear_result();showNotification('Step applied. The original file is unchanged.',type='message')}
 observeEvent(input$apply_type,{tryCatch(append_step(list(kind='type',column=input$type_column,type=input$type_target)),error=notify_error)})
 output$data_preview<-renderTable(head(data(),10),striped=TRUE,rownames=FALSE,digits=3)
 output$data_dimensions<-renderUI({d<-data();p(class='small-muted',paste(nrow(d),'observations ·',ncol(d),'variables · showing the first 10 rows'))})
 output$prep_controls<-renderUI({d<-data();req(input$prep_kind)
   if(input$prep_kind=='filter')return(tagList(selectInput('filter_col','Which variable?',names(d)),uiOutput('filter_values'),actionButton('add_condition','Add condition'),uiOutput('pending_conditions'),radioButtons('filter_join','Keep rows meeting',choices=c('ALL conditions (AND)'='all','ANY condition (OR)'='any')),div(class='nav-actions',actionButton('preview_filter','Preview filter',class='btn-primary'),actionButton('clear_conditions','Clear conditions')),tags$details(tags$summary('How do filters work?'),p(prep_help$filter[2]))))
   tagList(selectInput('derive_col','Starting variable',names(d)),selectInput('derive_op','What would you like to do?',choices=c('Natural log'='log','Multiply by a constant'='scale','Add a constant'='add','Subtract another variable'='subtract','Divide by another variable'='ratio','Create groups using a threshold'='threshold','Replace a category label'='recode')),textInput('derive_name','New variable name','new_variable'),uiOutput('derive_fields'),actionButton('preview_derive','Preview new variable',class='btn-primary'),tags$details(tags$summary('Why create a new variable?'),p(prep_help$derive[2])))
 })
 output$filter_values<-renderUI({req(input$filter_col);d<-data();v<-d[[input$filter_col]];ops<-c('Equals'='eq','Does not equal'='ne',if(is.numeric(v))c('Greater than'='gt','At least'='ge','Less than'='lt','At most'='le'),'Is missing'='missing','Is not missing'='present')
   tagList(selectInput('filter_op','Condition',ops),if(is.numeric(v))numericInput('filter_value','Value',value=0)else selectizeInput('filter_value','Value',choices=sort(unique(na.omit(as.character(v)))),options=list(create=TRUE)))})
 observeEvent(input$add_condition,{
   tryCatch({req(input$filter_col,input$filter_op);r<-list(column=input$filter_col,operator=input$filter_op,value=as.character(input$filter_value),numeric=is.numeric(data()[[input$filter_col]]));filter_expr(r);rv$pending<-c(rv$pending,list(r));rv$candidate<-NULL},error=notify_error)
 })
 output$pending_conditions<-renderUI({if(!length(rv$pending))return(p(class='small-muted','Add one or more conditions before previewing.'))
   tags$ol(lapply(rv$pending,function(x)tags$li(paste(x$column,switch(x$operator,eq='=',ne='≠',gt='>',ge='≥',lt='<',le='≤',missing='is missing',present='is not missing'),if(!x$operator%in%c('missing','present'))x$value else ''))))})
 observeEvent(input$clear_conditions,{rv$pending<-list();rv$candidate<-NULL})
 output$derive_fields<-renderUI({req(input$derive_op);switch(input$derive_op,
   log=help_box('Natural log is for strictly positive values. It may reduce right skew. The new column is on a log scale.'),
   scale=numericInput('derive_number','Multiply by',value=1),add=numericInput('derive_number','Add',value=0),
   subtract=selectInput('derive_other','Subtract this variable',names(data())),ratio=selectInput('derive_other','Divide by this variable',names(data())),
   threshold=tagList(numericInput('derive_number','Threshold',value=50),textInput('derive_below','Below threshold label','Below'),textInput('derive_above','At or above threshold label','At or above'),help_box('Grouping loses numerical detail. Use a threshold only when it answers a meaningful question.')),
   recode=tagList(selectInput('recode_old','Existing category',sort(unique(na.omit(as.character(data()[[input$derive_col]]))))),textInput('recode_new','Replace with','New label')))
 })
 observeEvent(input$preview_filter,{
   tryCatch({if(!length(rv$pending))stop('Add at least one condition first.');s<-list(kind='filter',rules=rv$pending,join=input$filter_join,column='');check_step(data(),s);rv$candidate<-s},error=notify_error)
 })
 observeEvent(input$preview_derive,{
   tryCatch({s<-list(kind='derive',column=input$derive_col,operation=input$derive_op,name=trimws(input$derive_name),number=input$derive_number,other=input$derive_other,below=input$derive_below,above=input$derive_above,old=input$recode_old,new=input$recode_new);check_step(data(),s);rv$candidate<-s},error=notify_error)
 })
 output$candidate_preview<-renderUI({req(rv$candidate);new<-apply_steps(data(),list(rv$candidate));tagList(hr(),h3('Preview — not applied yet'),help_box(paste(nrow(data()),'rows →',nrow(new),'rows.',ncol(data()),'columns →',ncol(new),'columns.')),scroll_table('candidate_table'),actionButton('apply_candidate','Apply this change',class='btn-primary'))})
 output$candidate_table<-renderTable({req(rv$candidate);new<-apply_steps(data(),list(rv$candidate));if(rv$candidate$kind=='derive')head(new[unique(c(rv$candidate$column,rv$candidate$name))],6)else head(new,6)},digits=3)
 observeEvent(input$apply_candidate,{tryCatch({req(rv$candidate);append_step(rv$candidate);rv$pending<-list()},error=notify_error)})
 output$history<-renderUI({if(!length(rv$steps))return(p(class='small-muted','No preparation steps yet. You can analyse the data as imported.'))
   tags$ol(lapply(rv$steps,function(s)tags$li(if(s$kind=='filter')paste('Keep rows meeting',if(s$join=='all')'all'else'any','of',length(s$rules),'conditions')else if(s$kind=='type')paste('Treat',s$column,'as',s$type)else paste('Create',s$name,'from',s$column,'using',s$operation))))})
 observeEvent(input$undo,{if(length(rv$steps)){rv$steps<-head(rv$steps,-1);rv$candidate<-NULL;clear_result()}})
 output$analysis_ui<-renderUI({
   if(is.null(rv$raw))return(div(class='panel-card empty',h3('Choose your data, or start with the tutorial'),p('You can learn with the built-in example without uploading a file.'),div(class='nav-actions',actionButton('example_analysis','Use example data'),actionButton('open_tutorial','Start tutorial',class='btn-primary'),actionButton('go_import','Import my data'))))
   d<-data();nums<-names(d)[vapply(d,is.numeric,logical(1))];cats<-names(d)[!vapply(d,is.numeric,logical(1))];k<-route()
   good_cats<-cats[vapply(d[cats],function(v)length(unique(na.omit(v)))<=30,logical(1))]
   cat_choices<-unique(c(good_cats,cats,names(d)))
   tagList(div(class='two-col',div(class='panel-card sticky-controls',p(class='eyebrow','1 · Choose your variables'),h3(route_prompts[[k]]),
     if(k%in%c('num','group','num2'))selectInput('var_x',if(k=='num2')'Horizontal axis: numerical variable'else'Numerical variable',nums)else selectInput('var_x',if(k=='cat2')'Row / horizontal-axis category'else'Categorical variable',cat_choices),
     if(k=='group')selectInput('var_y','Group by',cat_choices),if(k=='cat2')selectInput('var_y','Column / colour category',cat_choices,selected=if(length(cat_choices)>1)cat_choices[2]else cat_choices[1]),
     if(k=='num2')selectInput('var_y','Vertical axis: numerical variable',nums,selected=if(length(nums)>1)nums[2]else nums[1]),
     if(k%in%c('group','num2'))selectInput('var_z','Optional third variable: separate panels',c('No extra grouping'='',cat_choices)),
     if(k=='num')sliderInput('hist_bins','Histogram intervals',min=5,max=40,value=15,step=1),
     if(k=='cat')radioButtons('bar_measure','Bar height',c('Percentage'='percent','Count'='count')),
     if(k=='num2')radioButtons('cor_method','Correlation',c('Pearson: straight-line association'='pearson','Spearman: association between ranks'='spearman')),
     help_box(if(k%in%c('cat','cat2','group'))'Category codes may look like numbers. Check what they mean. You can change a variable type in the data workspace.'else'Choose a measurement, not an identifier. Check its units and look for implausible values.'),
     div(class='nav-actions',actionButton('run_desc','Explore these variables',class='btn-primary'),actionButton('open_tutorial','Why? (Tutorial)')),hr(),actionButton('go_import','Review / prepare data')),
     div(class='panel-card',p(class='eyebrow','2 · Look, then describe'),uiOutput('result_ui'))),
     uiOutput('analysis_code_panel'))
 })
 observeEvent(input$example_analysis,{
   rv$draft<-example_raw;rv$draftmeta<-example_meta;rv$draftpath<-example_path
   use_draft();page('analysis')
 })
 observeEvent(input$open_tutorial,{updateTabsetPanel(session,'mode',selected='Tutorial')})
 observeEvent(input$back_analysis,{updateTabsetPanel(session,'mode',selected='Analyse')})
 current_spec<-reactive({k<-route();list(route=k,x=if(nonempty(input$var_x))input$var_x else '',
   y=if(k%in%c('group','cat2','num2')&&nonempty(input$var_y))input$var_y else '',
   z=if(k%in%c('group','num2')&&nonempty(input$var_z))input$var_z else '',
   bins=if(!is.null(input$hist_bins))as.integer(input$hist_bins)else 15L,
   measure=if(nonempty(input$bar_measure))input$bar_measure else 'percent',
   denom='row',cor=if(nonempty(input$cor_method))input$cor_method else 'pearson')})
 observeEvent(input$run_desc,{
   tryCatch({s<-current_spec();res<-run_analysis(data(),s);rv$spec<-s;rv$result<-res;rv$result_steps<-rv$steps},error=notify_error)
 })
 display_spec<-reactive({req(rv$spec);s<-rv$spec;w<-input$viewport_width
   s$facet_columns<-if(is.null(w)||w>=1180)2L else 1L
   if(s$route=='cat2'&&nonempty(input$denominator))s$denom<-input$denominator;s})
 display_result<-reactive({req(rv$result);run_analysis(data(),display_spec())})
 output$result_ui<-renderUI({
   if(is.null(rv$result))return(div(class='empty',h3('Your question comes first'),p('Select the variables on the left, then choose Explore. You will see the observations, a summary table and a guide to reading them.'),help_box('Need a hand? The Tutorial walks through this same question using a small example.')))
   s<-rv$spec;d<-data();selected<-c(s$x,s$y,s$z);selected<-selected[nzchar(selected)]
   complete<-sum(complete.cases(d[selected]));notes<-switch(s$route,
     cat=paste(nrow(d),'rows;',complete,'with a recorded category;',nrow(d)-complete,'missing, excluded from percentage denominators.'),
     num=paste(nrow(d),'rows;',complete,'observed values;',nrow(d)-complete,'missing. Missing values are not zeros.'),
     group=paste(nrow(d),'rows;',rv$result$analysed,'with grouping values;',complete,'complete measurements. Outcome missingness is reported within each group.'),
     paste(nrow(d),'rows;',complete,'complete observations;',nrow(d)-complete,'excluded because a selected value is missing.'))
   tagList(if(!identical(current_spec(),rv$spec))help_box('Selections have changed. These are the previous results. Click Explore again to update the results and script.','warning-note'),
     p(class='small-muted',paste('Result for',paste(selected,collapse=' · '))),help_box(notes),
     if(s$route=='cat2')tagList(h3('1. Read the count table'),
       p(paste0('Rows: ',s$x,'. Columns: ',s$y,'. Sum gives the row and column totals.')),
       scroll_table('cross_table'),h3('2. Choose the percentage denominator'),
       selectInput('denominator','Percentages calculated within',c('Each row group'='row','Each column group'='column','All complete observations'='all')),
       uiOutput('denominator_explanation'),h3('3. Read the percentage table'),scroll_table('analysis_table')),
     scroll_plot('analysis_plot',if(s$route=='cat2')rv$result$height else display_result()$height,if(is.null(input$viewport_width)||input$viewport_width>=1180)640 else 540),if(s$route=='num')scroll_plot('analysis_extra',300),
     if(s$route!='cat2')tagList(h3('The numbers behind the picture'),scroll_table('analysis_table')),
     help_box(switch(s$route,
       num='Read the mean and median together with the shape. SD measures spread around the mean; the IQR covers the middle half. A boxplot point beyond the whiskers is a cue to investigate, not an instruction to delete it.',
       cat='The table uses non-missing observations as its denominator. Describe the category with its count and percentage. The chart is a description of this sample.',
       group='Compare sample sizes, centre and spread. Similar means can hide different distributions. A descriptive difference does not establish statistical significance or causation.',
       cat2='Compare counts and percentages together. A percentage based on very few observations can be unstable. A blank percentage means its denominator is zero.',
       num2=paste(tools::toTitleCase(s$cor),'correlation describes association. Inspect the scatterplot for curves, clusters and unusual observations. NA means fewer than three complete pairs or a constant measurement in that group; correlation is not defined here.'))),
     tags$details(tags$summary('Help me describe this result'),p(switch(s$route,num='“Among … observed values, the median was … (Q1 …, Q3 …). The distribution was … . There were … missing observations.”',cat='“Among … observations with this variable recorded, … were in category … (…%).”',group='“In group …, … observations had a mean of … and SD of … . In group …, … . The distributions appeared … .”',cat2='“Within …, … of … observations were … (…%), compared with … within … .”',num2='“For … complete pairs, the scatterplot showed … . The Pearson/Spearman correlation was … . This describes association, not causation.”'))),
     div(class='nav-actions',if(s$route=='cat2')downloadButton('download_counts','Download counts'),downloadButton('download_table',if(s$route=='cat2')'Download percentages'else'Download table'),downloadButton('download_plot','Download figure'),actionButton('open_tutorial_result','Understand this (Tutorial)')))
 })
 observeEvent(input$open_tutorial_result,updateTabsetPanel(session,'mode',selected='Tutorial'))
 output$analysis_plot<-renderPlot({req(rv$result);display_result()$plot},res=96)
 output$analysis_extra<-renderPlot({req(rv$result$extra);display_result()$extra},res=96)
 output$analysis_table<-renderTable({req(rv$result);if(rv$spec$route=='cat2')as.data.frame.matrix(display_result()$percentages)else as.data.frame(rv$result$table)},digits=2,striped=TRUE,rownames=reactive(isTRUE(rv$spec$route=='cat2')))
 output$denominator_explanation<-renderUI({req(rv$spec$route=='cat2');help_box(switch(display_spec()$denom,row='Each row adds to 100%. Divide each cell by its row total.',column='Each column adds to 100%. Divide each cell by its column total.',all='All cells together add to 100%. Divide each cell by the grand total.'))})
 output$cross_table<-renderTable({req(rv$result,rv$spec$route=='cat2');as.data.frame.matrix(rv$result$counts)},rownames=TRUE)
 output$analysis_code_panel<-renderUI({req(rv$result);code_box('analysis_code','copy_analysis','download_analysis','zip_analysis')})
 output$analysis_code<-renderText({req(rv$spec);learner_script(rv$meta,rv$result_steps,display_spec(),rv$raw)})
 output$prep_code<-renderText({req(rv$meta);learner_script(rv$meta,rv$steps)})

 # Tutorials use separate data and never modify the user's workspace.
 output$tutorial_ui<-renderUI({
   k<-route();i<-rv$lesson;item<-lessons[[k]][[i]]
   tagList(div(class='section-head',div(span(class='progress-label',paste('Step',i,'of',length(lessons[[k]]))),h3(item$title)),actionButton('back_analysis','Back to my analysis')),
     div(class='lesson-layout',div(class='panel-card lesson-copy',lapply(item$text,p),uiOutput('tutorial_description'),help_box(item$question,'lesson-note'),p(tags$b('Take away: '),item$takeaway),
       div(class='nav-actions',actionButton('lesson_prev','Previous',disabled=if(i==1)'disabled'else NULL),actionButton('lesson_next',if(i==5)'Start again'else'Next step',class='btn-primary'))),
       div(class='panel-card',h4('Picture this study'),p(descriptive_context(k,i)),p(class='small-muted','One row per fictional participant. These teaching data are separate from your own data.'),
         if(k=='num'&&i%in%c(2,3))sliderInput('tutorial_outlier','Add an unusually large value to the last observation',min=0,max=20,value=0,step=1),
         if(k=='num'&&i==4)checkboxInput('tutorial_log','Show the natural-log values',FALSE),
         if(k=='cat2')tagList(h4('1. Count the combinations'),p('Rows: programme. Columns: smoking category. Sum gives the totals.'),scroll_table('tutorial_counts'),
           h4('2. Choose the percentage denominator'),selectInput('tutorial_denom','Percentage denominator',c('Each row group'='row','Each column group'='column','All complete observations'='all')),
           uiOutput('tutorial_denominator_explanation'),h4('3. Read the percentages'),scroll_table('tutorial_table')),
         if(k=='cat')radioButtons('tutorial_measure','Bar height',c('Percentage'='percent','Count'='count'),inline=TRUE),
         if(k=='num2')radioButtons('tutorial_cor','Coefficient',c('Pearson'='pearson','Spearman'='spearman'),inline=TRUE),
         uiOutput('tutorial_figures'),if(k!='cat2')scroll_table('tutorial_table'))),
     code_box('tutorial_code','copy_tutorial','download_tutorial','zip_tutorial'))
 })
 observeEvent(input$lesson_prev,{rv$lesson<-max(1,rv$lesson-1)})
 observeEvent(input$lesson_next,{rv$lesson<-if(rv$lesson==5)1 else rv$lesson+1})
 tutorial_state<-reactive({
   d<-example_raw;steps<-list();k<-route();s<-list(route=k,x='sleep_hours',y='',z='',bins=15L,measure='percent',denom='row',cor='pearson')
   if(k=='cat'){s$x<-'smoking';if(nonempty(input$tutorial_measure))s$measure<-input$tutorial_measure}
   if(k=='group'){s$x<-'wellbeing_score';s$y<-'programme'}
   if(k=='cat2'){s$x<-'programme';s$y<-'smoking';if(nonempty(input$tutorial_denom))s$denom<-input$tutorial_denom}
   if(k=='num2'){s$x<-'sleep_hours';s$y<-'wellbeing_score';if(nonempty(input$tutorial_cor))s$cor<-input$tutorial_cor}
   if(k=='num'&&rv$lesson%in%c(2,3)&&!is.null(input$tutorial_outlier))d$sleep_hours[nrow(d)]<-7+input$tutorial_outlier
   if(k=='num'&&rv$lesson==4){s$x<-'marker';if(isTRUE(input$tutorial_log)){steps<-list(list(kind='derive',column='marker',operation='log',name='log_marker'));s$x<-'log_marker'}}
   meta<-example_meta;meta$name<-'tutorial_example.csv'
   list(raw=d,steps=steps,spec=s,meta=meta,result=run_analysis(apply_steps(d,steps),s))
 })
 output$tutorial_description<-renderUI({
   k<-route();i<-rv$lesson
   if(!((k=='cat'&&i==4)||(k=='num'&&i%in%c(3,4,5))||(k=='cat2'&&i%in%c(2,5))||(k%in%c('group','num2')&&i==5)))return(NULL)
   div(class='worked-example',h4('A description of this example'),lapply(tutorial_description(tutorial_state()),p))
 })
 output$tutorial_figures<-renderUI({tagList(scroll_plot('tutorial_plot',tutorial_state()$result$height),if(route()=='num')scroll_plot('tutorial_extra',300))})
 output$tutorial_plot<-renderPlot(tutorial_state()$result$plot,res=96)
 output$tutorial_extra<-renderPlot({req(tutorial_state()$result$extra);tutorial_state()$result$extra},res=96)
 output$tutorial_counts<-renderTable(as.data.frame.matrix(tutorial_state()$result$counts),digits=0,rownames=TRUE,striped=TRUE)
 output$tutorial_denominator_explanation<-renderUI({help_box(switch(tutorial_state()$spec$denom,row='Each row adds to 100%: its row total is the denominator.',column='Each column adds to 100%: its column total is the denominator.',all='All cells together add to 100%: the grand total is the denominator.'))})
 output$tutorial_table<-renderTable({t<-tutorial_state();if(route()=='cat2')as.data.frame.matrix(t$result$percentages)else as.data.frame(t$result$table)},digits=2,striped=TRUE,rownames=reactive(route()=='cat2'))
 output$tutorial_code<-renderText({t<-tutorial_state();learner_script(t$meta,t$steps,t$spec,t$raw)})

 make_zip<-function(file,script,source,filename,exact=NULL){
   dir<-tempfile('analysis_');dir.create(dir);on.exit(unlink(dir,recursive=TRUE),add=TRUE);dir.create(file.path(dir,'data'))
   file.copy(source,file.path(dir,'data',filename));writeLines(script,file.path(dir,'analysis.R'),useBytes=TRUE)
   if(!is.null(exact))writeLines(exact,file.path(dir,'analysis_exact.R'),useBytes=TRUE)
   writeLines(c('Run this analysis','', 'Create an RStudio project in this folder (or set the working directory here).',
     'Open analysis.R. Install missing packages listed at the top, then run the script.',
     'data/ contains the original file. Preparation is recorded in analysis.R.',
     'analysis.R is the student script. analysis_exact.R (when included) reproduces the full app output.',
     'Student figures use simple styling; table layouts may differ. Use the same data for both scripts.',
     'Do not move analysis.R away from data/ without updating its relative import path.'),file.path(dir,'README.txt'))
   zip::zipr(file,c('analysis.R',if(!is.null(exact))'analysis_exact.R','README.txt','data'),root=dir)
 }
 output$download_prep<-downloadHandler(filename=function()'prepare_data.R',content=function(file)writeLines(learner_script(rv$meta,rv$steps),file,useBytes=TRUE))
 output$download_analysis<-downloadHandler(filename=function()'analysis.R',content=function(file){req(rv$spec);writeLines(learner_script(rv$meta,rv$result_steps,display_spec(),rv$raw),file,useBytes=TRUE)})
 output$zip_prep<-downloadHandler(filename=function()'prepared_analysis.zip',content=function(file)make_zip(file,learner_script(rv$meta,rv$steps),rv$source,rv$meta$name,full_script(rv$meta,rv$steps)))
 output$zip_analysis<-downloadHandler(filename=function()'descriptive_analysis.zip',content=function(file){req(rv$spec);make_zip(file,learner_script(rv$meta,rv$result_steps,display_spec(),rv$raw),rv$source,rv$meta$name,full_script(rv$meta,rv$result_steps,display_spec()))})
 output$download_tutorial<-downloadHandler(filename=function()'tutorial_analysis.R',content=function(file){t<-tutorial_state();writeLines(learner_script(t$meta,t$steps,t$spec,t$raw),file,useBytes=TRUE)})
 output$zip_tutorial<-downloadHandler(filename=function()'tutorial_example.zip',content=function(file){t<-tutorial_state();tmp<-tempfile(fileext='.csv');on.exit(unlink(tmp));readr::write_csv(t$raw,tmp,na='');make_zip(file,learner_script(t$meta,t$steps,t$spec,t$raw),tmp,t$meta$name,full_script(t$meta,t$steps,t$spec))})
 output$download_counts<-downloadHandler(filename=function()'contingency_counts.csv',content=function(file){req(rv$result,rv$spec$route=='cat2');utils::write.csv(as.data.frame.matrix(display_result()$counts),file,row.names=TRUE)})
 output$download_table<-downloadHandler(filename=function()'descriptive_table.csv',content=function(file){req(rv$result);if(rv$spec$route=='cat2')utils::write.csv(as.data.frame.matrix(display_result()$percentages),file,row.names=TRUE)else readr::write_csv(rv$result$table,file)})
 output$download_plot<-downloadHandler(filename=function()'descriptive_figure.png',content=function(file){req(rv$result);r<-display_result();ggsave(file,r$plot,width=r$export_width,height=r$export_height,dpi=300,bg='white',limitsize=FALSE)})
 observeEvent(input$copy_prep,session$sendCustomMessage('copyCode',learner_script(rv$meta,rv$steps)))
 observeEvent(input$copy_analysis,{req(rv$spec);session$sendCustomMessage('copyCode',learner_script(rv$meta,rv$result_steps,display_spec(),rv$raw))})
 observeEvent(input$copy_tutorial,{t<-tutorial_state();session$sendCustomMessage('copyCode',learner_script(t$meta,t$steps,t$spec,t$raw))})
 observeEvent(input$copy_done,showNotification('Code copied.',type='message',duration=2))
 output$download_prep_exact<-downloadHandler(filename=function()'prepare_data_exact.R',content=function(file)writeLines(full_script(rv$meta,rv$steps),file,useBytes=TRUE))
 output$download_analysis_exact<-downloadHandler(filename=function()'analysis_exact.R',content=function(file){req(rv$spec);writeLines(full_script(rv$meta,rv$result_steps,display_spec()),file,useBytes=TRUE)})
 output$download_tutorial_exact<-downloadHandler(filename=function()'tutorial_exact.R',content=function(file){t<-tutorial_state();writeLines(full_script(t$meta,t$steps,t$spec),file,useBytes=TRUE)})
 source('estimation_server.R',local=TRUE)
 source('hypothesis_server.R',local=TRUE)
 source('regression_server.R',local=TRUE)
}
shinyApp(ui,server)
