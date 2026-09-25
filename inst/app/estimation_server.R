# Sourced inside server() so data preparation and navigation remain shared.
est <- reactiveValues(kind='mean',compare=FALSE,preview=NULL,draft=NULL,result=NULL,spec=NULL,steps=NULL,lesson=1,draw=0)
clear_estimation <- function(){est$preview<-NULL;est$draft<-NULL;est$result<-NULL;est$spec<-NULL;est$steps<-NULL}
observeEvent(input$go_estimation,page('est_choices'))
observeEvent(input$est_change_question,page('est_choices'))
for(kind in c('mean','proportion'))local({k<-kind;observeEvent(input[[paste0('est_choose_',k)]],{
  est$kind<-k;est$compare<-FALSE;est$lesson<-1;clear_estimation();page('est_analysis')
})})
for(kind in c('mean','proportion'))local({k<-kind;observeEvent(input[[paste0('est_choose_',k,'2')]],{est$kind<-k;est$compare<-TRUE;est$lesson<-1;clear_estimation();page('est_analysis')})})
observeEvent(input$est_example,{
  rv$draft<-example_raw;rv$draftmeta<-example_meta;rv$draftpath<-example_path
  use_draft();page('est_analysis')
})
observeEvent(input$est_to_tutorial,updateTabsetPanel(session,'est_mode',selected='Tutorial'))
observeEvent(input$est_back_analysis,updateTabsetPanel(session,'est_mode',selected='Analyse'))
for(step in c(2,3,4))local({i<-step;observeEvent(input[[paste0('est_why_',i)]],{est$lesson<-if(isTRUE(est$compare)){if(i==2)if(est$kind=='mean')4L else 7L else if(i==4)if(est$kind=='mean')5L else 7L else 3L}else i;updateTabsetPanel(session,'est_mode',selected='Tutorial')})})

output$est_analysis_ui<-renderUI({
  if(is.null(rv$raw))return(div(class='panel-card empty',h3('Choose data or start with the tutorial'),
    p('The tutorial uses a separate simulated example. Your own data can be imported whenever you are ready.'),
    div(class='nav-actions',actionButton('est_example','Use example data'),actionButton('est_to_tutorial','Start tutorial',class='btn-primary'),actionButton('go_import','Import my data'))))
  d<-data();is_mean<-est$kind=='mean'
  choices<-if(is_mean)names(d)[vapply(d,is.numeric,logical(1))]else names(d)[vapply(d,function(x)length(unique(na.omit(x)))%in%1:2,logical(1))]
  tagList(div(class='two-col',div(class='panel-card sticky-controls',p(class='eyebrow','1 · Choose what to estimate'),
    h3(if(isTRUE(est$compare))comparison_title(est$kind)else if(is_mean)'A population mean'else'A population proportion'),
    selectInput('est_variable',if(is_mean)'Numerical measurement'else'Binary outcome variable',choices,
      selected=if(is_mean&&'sleep_hours'%in%choices)'sleep_hours'else if(!is_mean&&'short_sleep'%in%choices)'short_sleep'else choices[1]),
    if(!is_mean)tagList(help_box('Choose a variable with two possible categories. Then identify the event you want to count.'),uiOutput('est_event_ui')),
    if(!length(choices))help_box(if(is_mean)'No numerical measurements are available. Check variable types in Prepare data.'else'No binary variables are available. Create a meaningful two-category variable in Prepare data.'),
    uiOutput('est_group_ui'),
    if(isTRUE(est$compare))tagList(uiOutput('est_reference_ui'),
      if(!is_mean)selectInput('est_design','How were people selected and observed?',c('Choose the study design'='', 'Followed for an outcome over the same period'='cohort','Their status was recorded at one time'='cross_sectional','Selected because they had / did not have the outcome (case-control)'='case_control'))),
    if(is_mean)textInput('est_units','Measurement units (optional)',''),
    help_box('Select a measurement or outcome, rather than an ID. If a person appears on several rows, these simple methods may not be suitable.'),
    actionButton('est_inspect','Inspect these data',class='btn-primary'),hr(),
    div(class='nav-actions',actionButton('est_to_tutorial','Tutorial'),actionButton('go_import','Review / prepare data'))),
    div(class='panel-card',uiOutput('est_flow'))),uiOutput('est_results'),uiOutput('est_code_panel'))
})
output$est_group_ui<-renderUI({
 req(rv$raw,input$est_variable);d<-data()
 groups<-setdiff(names(d)[vapply(d,function(x)length(unique(na.omit(x)))%in%(if(isTRUE(est$compare))2L else 1:30),logical(1))],input$est_variable)
 old<-isolate(input$est_group)
 selected<-if(nonempty(old)&&old%in%groups)old else if(isTRUE(est$compare)&&'programme'%in%groups)'programme'else if(isTRUE(est$compare)&&length(groups))groups[1]else''
 tagList(selectInput('est_group',if(isTRUE(est$compare))'Variable identifying the two groups'else'Optional: estimate separately by group',if(isTRUE(est$compare))groups else c('All observations'='',groups),selected=selected),
  if(isTRUE(est$compare)&&!length(groups))help_box('No other variable currently identifies two observed groups. The outcome cannot also be its own grouping variable. Use Import / prepare data to choose or create a separate grouping variable with two categories.'))
})
output$est_event_ui<-renderUI({req(rv$raw,input$est_variable);v<-data()[[input$est_variable]]
  observed<-sort(unique(as.character(na.omit(v))))
  choices<-if(is.factor(v))unique(c(observed,levels(v)))else observed
  tagList(selectizeInput('est_event','Category counted as the event',choices=choices,selected=tail(observed,1),options=list(create=length(observed)==1)),
    if(isTRUE(rv$meta$example)&&input$est_variable=='short_sleep')help_box('In this fictional study, short_sleep is Yes for recorded sleep below 7 hours and No otherwise. It describes a recorded status, not a diagnosis or a future event.'),
    if(length(observed)==1)help_box('Only one category is observed. If the other possible category is the event of interest, type its label here. Zero observed events still leave uncertainty.'))
})
output$est_reference_ui<-renderUI({req(rv$raw,input$est_group);g<-sort(unique(as.character(na.omit(data()[[input$est_group]]))));tagList(selectInput('est_reference','Reference group: compare the other group with this one',g),help_box('The difference subtracts the reference. A ratio divides by the reference. These are different people in the two groups, not paired measurements.'))})
est_current<-reactive({list(kind=est$kind,x=if(nonempty(input$est_variable))input$est_variable else'',
  group=if(nonempty(input$est_group))input$est_group else'',event=if(est$kind=='proportion'&&nonempty(input$est_event))input$est_event else'',
  unit=if(est$kind=='mean'&&nonempty(input$est_units))input$est_units else'',compare=isTRUE(est$compare),
  reference=if(isTRUE(est$compare)&&nonempty(input$est_reference))input$est_reference else'',
  design=if(isTRUE(est$compare)&&est$kind=='proportion'&&nonempty(input$est_design))input$est_design else'')})
observeEvent(input$est_inspect,{
  tryCatch({s<-est_current();if(isTRUE(s$compare)&&(!nonempty(s$group)||s$group==s$x))stop('Choose a separate grouping variable with two observed categories.');if(isTRUE(s$compare)&&s$kind=='proportion'&&!nonempty(s$design))stop('Choose how people were selected and observed before comparing proportions.');s$conf<-.95;r<-run_estimation(data(),s);est$draft<-est_current();est$preview<-r;est$result<-NULL;est$spec<-NULL;est$steps<-NULL},error=notify_error)
})
output$est_flow<-renderUI({
  if(is.null(est$preview))return(div(class='empty',h3('Begin with your question'),p('Choose the variable and any grouping, then inspect the data. You will choose the confidence level after reviewing the observations.')))
  if(!identical(est$draft,est_current()))return(help_box('Your selections have changed. Choose Inspect these data to review the new selection.','warning-note'))
  tagList(p(class='eyebrow','2 · Review the observations'),
    help_box(paste(est$preview$excluded,'rows excluded because the grouping variable is missing. Missing outcomes are counted separately below.')),
    if(isTRUE(est$compare)&&est$kind=='proportion')tagList(h4('Count each outcome within each group'),p(paste('Event:',est$draft$event)),scroll_table('est_counts')),
    scroll_plot('est_diagnostic',est$preview$height,640),scroll_table('est_preview_table'),
    if(est$kind=='mean')help_box('Inspect each group. A t-based interval is most reliable when observations are independent and the population distribution is roughly normal, especially with small samples. Strong skew or extreme values can make it unreliable. A large sample helps, but does not guarantee validity.')else
      help_box('Confirm which category is counted as the event and which observations form the denominator. The interval uses recorded binary outcomes; missing outcomes are excluded and reported.'),
    actionButton('est_why_2','Why do estimates vary? (Tutorial)'),
    h3('3. Choose the confidence level'),selectInput('est_confidence','Confidence level',c('90%'='.90','95%'='.95','99%'='.99'),selected='.95'),
    p(class='small-muted','Higher confidence gives a wider interval for the same data.'),actionButton('est_why_4','What does confidence mean? (Tutorial)'),
    checkboxInput('est_independent','Each row represents a separate, independent observation.',FALSE),
    if(est$kind=='mean')checkboxInput('est_shape_reviewed','I have reviewed the distributions and unusual values.',FALSE),
    help_box('Confidence intervals describe sampling uncertainty under the method’s assumptions. They do not correct biased sampling, measurement errors or missing-data bias.'),
    actionButton('est_calculate','Estimate with a confidence interval',class='btn-primary'))
})
output$est_counts<-renderTable({req(est$preview$counts);as.data.frame.matrix(est$preview$counts)},rownames=TRUE,striped=TRUE)
output$est_diagnostic<-renderPlot({req(est$preview);est$preview$diagnostic},res=96)
output$est_preview_table<-renderTable({req(est$preview);s<-est$draft;s$conf<-.95;est_display_table(est$preview,s,TRUE)},digits=3,striped=TRUE)
observeEvent(input$est_calculate,{
  tryCatch({
    if(is.null(est$preview)||!identical(est$draft,est_current()))stop('Inspect the current selection first.')
    if(!isTRUE(input$est_independent))stop('Check whether rows are independent before using this method. The tutorial explains the assumption.')
    if(est$kind=='mean'&&!isTRUE(input$est_shape_reviewed))stop('Review the distribution and unusual values before estimating the mean.')
    s<-est_current();s$conf<-as.numeric(input$est_confidence);r<-run_estimation(data(),s)
    est$result<-r;est$spec<-s;est$steps<-rv$steps
  },error=notify_error)
})
output$est_results<-renderUI({req(est$result)
  s<-est$spec;old<-s;old$conf<-NULL
  changed<-!identical(old,est_current())||!isTRUE(all.equal(s$conf,as.numeric(input$est_confidence)))
  div(class='panel-card estimation-results',h3('4. Read the estimate and its uncertainty'),
    if(changed)help_box('Selections have changed. These results and downloads still describe the previous calculation. Inspect and calculate again to update them.','warning-note'),
    p(paste(if(s$kind=='mean')'Method: Student t confidence interval for a mean.'else'Method: Wilson score confidence interval for a binary proportion.',
      if(nonempty(s$group))'Intervals are calculated separately for each group.'else'')),
    if(isTRUE(s$compare)&&s$kind=='proportion'&&s$design=='case_control')help_box('The sample percentages can be calculated, and so can their difference or ratio. But selecting people because they have or do not have the outcome changes these numbers: they do not directly estimate population risks or risk ratios. This app therefore reports only the odds-ratio comparison as a population association for this design, provided cases and controls were selected appropriately. The tutorial explains this using one left-handed and one right-handed person.'),
    scroll_table('est_table'),scroll_plot('est_plot',est$result$height,640),
    h4('A description of these results'),lapply(est_description(est$result,s),p),
    help_box('The dot is the estimate. The line is its confidence interval. This is uncertainty about a population mean or proportion, not the range containing most individual observations.'),
    if(nonempty(s$group)&&!isTRUE(s$compare))help_box('These are individual group intervals, with no adjustment for multiple groups. Overlap or non-overlap is not a formal test of the difference between groups.'),
    if(s$kind=='proportion')help_box('SE is shown in percentage points. With zero or all events, the usual SE estimate is zero; the Wilson interval still shows uncertainty.'),
    if(isTRUE(s$compare))tagList(h3('5. Compare the two groups directly'),scroll_table('est_comparison_table'),
      lapply(comparison_description(est$result,s),p),
      if(s$kind=='proportion')selectInput('est_comparison_measure','Which comparison would you like to see?',setNames(seq_len(nrow(est$result$comparison)),est$result$comparison$measure)),
      p(if(s$kind=='mean')'The dashed line at zero means equal population means. Read the interval for the difference itself.'else'The dashed line marks equal population values: zero for a difference, one for a ratio. Ratio plots use a logarithmic scale, so 0.5 and 2 are equally far from 1.'),
      scroll_plot('est_comparison_plot',330,640),
      div(class='nav-actions',downloadButton('est_download_comparison','Download comparison table'),downloadButton('est_download_comparison_plot','Download comparison figure'))),
    actionButton('est_why_3',if(isTRUE(s$compare))'Understand this comparison (Tutorial)'else'SD or SE: what is the difference? (Tutorial)'),
    div(class='nav-actions',downloadButton('est_download_table','Download table'),downloadButton('est_download_plot','Download figure')))
})
output$est_table<-renderTable({req(est$result);est_display_table(est$result,est$spec)},digits=3,striped=TRUE)
output$est_plot<-renderPlot({req(est$result);est$result$plot},res=96)
output$est_code_panel<-renderUI({req(est$result);code_box('est_code','est_copy','est_download','est_zip')})
output$est_code<-renderText({req(est$spec);learner_estimation_script(rv$meta,est$steps,est$spec,rv$raw)})
output$est_download<-downloadHandler(filename=function()'estimation.R',content=function(file){req(est$spec);writeLines(learner_estimation_script(rv$meta,est$steps,est$spec,rv$raw),file,useBytes=TRUE)})
output$est_zip<-downloadHandler(filename=function()'estimation_with_data.zip',content=function(file){req(est$spec);make_zip(file,learner_estimation_script(rv$meta,est$steps,est$spec,rv$raw),rv$source,rv$meta$name,estimation_script(rv$meta,est$steps,est$spec))})
output$est_download_table<-downloadHandler(filename=function()'estimates.csv',content=function(file){req(est$result);readr::write_csv(est_display_table(est$result,est$spec),file)})
output$est_download_plot<-downloadHandler(filename=function()'estimates.png',content=function(file){req(est$result);ggsave(file,est$result$plot,width=est$result$width,height=est$result$export_height,dpi=300,bg='white',limitsize=FALSE)})
observeEvent(input$est_copy,{req(est$spec);session$sendCustomMessage('copyCode',learner_estimation_script(rv$meta,est$steps,est$spec,rv$raw))})

output$est_comparison_table<-renderTable({req(est$result$comparison);comparison_display(est$result,est$spec)},digits=3,striped=TRUE)
est_selected_comparison<-reactive({req(est$result$comparison);j<-if(est$spec$kind=='mean'||is.null(input$est_comparison_measure))1L else as.integer(input$est_comparison_measure);est$result$comparison_plots[[j]]})
output$est_comparison_plot<-renderPlot(est_selected_comparison(),res=96)
output$est_download_comparison<-downloadHandler(filename=function()'comparison.csv',content=function(file){req(est$result$comparison);readr::write_csv(comparison_display(est$result,est$spec),file)})
output$est_download_comparison_plot<-downloadHandler(filename=function()'comparison.png',content=function(file)ggsave(file,est_selected_comparison(),width=8,height=4,dpi=300,bg='white'))

# Tutorial: six short steps, with one consistent simulation across steps 2–5.
observeEvent(input$est_lesson_prev,{est$lesson<-max(1,est$lesson-1)})
observeEvent(input$est_lesson_next,{total<-if(isTRUE(est$compare))comparison_lesson_count(est$kind)else 6L;est$lesson<-if(est$lesson==total)1L else est$lesson+1L})
observeEvent(input$est_new_samples,{est$draw<-est$draw+1})
est_lesson_titles<-c('What are we trying to estimate?','A different sample gives a different estimate','Separate SD from standard error','What does a confidence interval mean?','Change sample size and confidence','Describe and reproduce an estimate')
output$est_tutorial_ui<-renderUI({
  i<-est$lesson;mean<-est$kind=='mean';simulation<-i%in%2:5
  if(isTRUE(est$compare))return(comparison_tutorial_ui())
  lesson<-single_est_lesson(est$kind,i)
  tagList(div(class='section-head',div(span(class='progress-label',paste('Step',i,'of 6')),h3(lesson$title)),actionButton('est_back_analysis','Back to my analysis')),
    div(class='lesson-layout',div(class='panel-card lesson-copy',lapply(lesson$text,p),
      if(i==1)help_box('A population is the group you want to learn about. A sample is the group you actually observed. How the sample was selected affects how far you can generalise.'),
      if(i==3)tags$details(tags$summary('How is SE calculated?'),p(if(mean)'For a mean, estimated SE is SD divided by the square root of the number observed. Four times as many independent observations gives roughly half the SE if the spread is unchanged.'else'The usual estimated SE uses the observed proportion and sample size. It can be zero when a sample has no events or all events, even though the population proportion remains uncertain.')),
      if(i==4)help_box('A 95% confidence interval does not contain 95% of people. In this interpretation, the population value is fixed; after calculation, this interval either contains it or does not.'),
      if(i==6)help_box(if(mean)'A t interval needs independent observations. With small samples, severe skew and unusual values deserve particular attention. There is no universal sample-size threshold that guarantees validity.'else'Wilson intervals stay between 0% and 100% and retain uncertainty when zero or all events are observed. They assume independent binary outcomes with the same probability within each population/group being estimated.'),
      div(class='nav-actions',actionButton('est_lesson_prev','Previous',disabled=if(i==1)'disabled'else NULL),actionButton('est_lesson_next',if(i==6)'Start again'else'Next step',class='btn-primary'))),
      div(class='panel-card',h4(if(simulation)'Repeat the study on a computer'else'A study to practise with'),uiOutput('est_experiment_context'),
        if(simulation)tagList(sliderInput('est_sim_n','Observations in each sample',min=5,max=200,value=isolate(if(is.null(input$est_sim_n))30 else input$est_sim_n),step=5),
          if(i%in%c(4,5))selectInput('est_sim_conf','Confidence level',c('90%'='.90','95%'='.95','99%'='.99'),selected=isolate(if(is.null(input$est_sim_conf))'.95'else input$est_sim_conf)),
          actionButton('est_new_samples','Draw 100 new samples'),uiOutput('est_sim_summary'),
          scroll_plot('est_sim_plot',if(i%in%c(4,5))650 else if(i==3)500 else 380,640),
          if(i==3&&!mean)scroll_plot('est_sampling_plot',360,640))else tagList(
          checkboxInput('est_tutorial_groups','Show separate estimates by programme',isolate(isTRUE(input$est_tutorial_groups))),
          uiOutput('est_worked_description'),scroll_table('est_tutorial_table'),uiOutput('est_tutorial_plot_ui')))),
    if(simulation)p(class='small-muted','The complete script below generates the simulated samples directly. Your uploaded data are not used.'),
    code_box('est_tutorial_code','est_tutorial_copy','est_tutorial_download',if(!simulation)'est_tutorial_zip'else NULL))
})
est_tutorial_state<-reactive({
  mean<-est$kind=='mean';steps<-if(mean)list()else list(list(kind='derive',column='age',operation='threshold',name='age_40_plus',number=40,above='40 or older',below='Under 40'))
  s<-list(kind=est$kind,x=if(mean)'sleep_hours'else'age_40_plus',group=if(isTRUE(input$est_tutorial_groups))'programme'else'',event=if(mean)''else'40 or older',unit=if(mean)'hours'else'',conf=.95)
  list(spec=s,steps=steps,result=run_estimation(apply_steps(example_raw,steps),s))
})
est_sim_state<-reactive({
  n<-if(is.null(input$est_sim_n))30 else as.integer(input$est_sim_n)
  conf<-if(is.null(input$est_sim_conf)).95 else as.numeric(input$est_sim_conf)
  run_est_simulation(est$kind,n,conf,2026+est$draw)
})
output$est_experiment_context<-renderUI({
 i<-est$lesson;mean<-est$kind=='mean';n<-if(is.null(input$est_sim_n))30 else input$est_sim_n
 if(!i%in%2:5)return(tagList(p(if(mean)'We use the recorded sleep times of fictional adults. The table shows the number with recorded values and their average. Start with those observations before interpreting uncertainty.'else'We use fictional adults and count those aged 40 or older. The table shows the count, the number with recorded ages and the resulting percentage.'),p('The optional grouping control shows each programme separately. Your uploaded data are not used.')))
 tagList(p(paste0('Imagine 100 researchers each running a study with ',n,' different people. The computer creates those 100 samples and calculates ',if(mean)'one mean sleep time'else'one percentage aged 40 or older',' from each. ',if(mean)'We have set the population mean to 7 hours and the spread of individual sleep times (SD) to 1.5 hours.'else'We have set the population proportion aged 40 or older to 40%.',' These population values stay fixed when we draw new samples.')),
 p(switch(as.character(i),'2'='Each bar counts studies whose estimates fell in that range. The dashed line marks the population value. Expect the study results to lie on both sides of that line, rather than all landing exactly on it. Click Draw 100 new samples to repeat the experiment.',
 '3'=if(mean)'The upper panel shows individual people in the latest study; the lower panel shows the means from all 100 studies. Both use the same horizontal scale. Compare their horizontal spread, not the heights of the bars. The collection of means should usually be narrower.'else'The dots show the people in the latest study, with colour indicating their category. The histogram below shows the 100 study percentages. Look at the spread between study results, rather than treating SE as an uncertainty about one person.',
 '4'='Each horizontal line is one study’s interval; its dot is that study’s estimate. A line that reaches the fixed population value contains it. At 95% confidence, expect most lines to reach it and a few to miss. A batch of 100 will not necessarily have exactly 95 hits.',
 '5'='First increase people per study and look for shorter lines. Then keep the sample size fixed and increase the confidence level: the lines widen. The button draws new studies; changing confidence only recalculates intervals for the same sampled values.')))
})
output$est_sim_summary<-renderUI({r<-est_sim_state();tab<-r$table;scale<-if(est$kind=='mean')1 else 100
 unit<-if(est$kind=='mean')' hours'else' percentage points';last<-tail(tab,1);f<-function(x)formatC(x,format='f',digits=3)
 tagList(p(paste0('In these 100 studies, the ',if(est$kind=='mean')'sample means range from 'else'sample percentages range from ',f(min(tab$estimate)*scale),' to ',f(max(tab$estimate)*scale),if(est$kind=='mean')' hours.'else'%.')),
 if(est$lesson>=3)tagList(p(paste0('The SD of these 100 study estimates is ',f(sd(tab$estimate)*scale),unit,'. This shows the sample-to-sample variation we are trying to describe with SE.')),
 p(paste0('Using just the latest study, the estimated SE is ',f(last$se*scale),unit,'. A different batch of studies will give slightly different numbers.')),
 if(est$lesson==3&&est$kind=='mean')p(paste0('Among individual people in that latest study, SD is ',f(sd(r$latest)),' hours. Compare that with the much smaller spread of study means.'))),
 if(est$lesson%in%c(4,5))p(paste0(sum(tab$contains_population),' of these 100 intervals contain the population value; ',sum(!tab$contains_population),' miss it. This is one batch, not a guarantee of exactly ',round((if(is.null(input$est_sim_conf)).95 else as.numeric(input$est_sim_conf))*100),' hits.')))
})
output$est_sim_plot<-renderPlot({r<-est_sim_state();if(est$lesson%in%c(4,5))r$coverage else if(est$lesson==3)r$spread else r$sampling},res=96)
output$est_sampling_plot<-renderPlot(est_sim_state()$sampling,res=96)
output$est_tutorial_plot_ui<-renderUI({scroll_plot('est_tutorial_plot',est_tutorial_state()$result$height,640)})
output$est_tutorial_plot<-renderPlot({r<-est_tutorial_state()$result;if(est$lesson==1)r$diagnostic else r$plot},res=96)
output$est_tutorial_table<-renderTable({t<-est_tutorial_state();est_display_table(t$result,t$spec,preview=est$lesson==1)},digits=3,striped=TRUE)
output$est_worked_description<-renderUI({t<-est_tutorial_state();tagList(lapply(est_description(t$result,t$spec,preview=est$lesson==1),p))})
est_tutorial_exact<-reactive({if(isTRUE(est$compare)){t<-comparison_state();estimation_script(t$meta,list(),t$spec)}else if(est$lesson%in%2:5)est_sim_state()$code else {t<-est_tutorial_state();estimation_script(example_meta,t$steps,t$spec)}})
est_tutorial_script<-reactive({
 if(isTRUE(est$compare)){t<-comparison_state();learner_estimation_script(t$meta,list(),t$spec,t$raw)}
 else if(est$lesson%in%2:5)learner_simulation_script(est$kind,if(is.null(input$est_sim_n))30 else input$est_sim_n,if(is.null(input$est_sim_conf)).95 else as.numeric(input$est_sim_conf),2026+est$draw,est$lesson)
 else {t<-est_tutorial_state();learner_estimation_script(example_meta,t$steps,t$spec,example_raw)}
})
output$est_tutorial_code<-renderText(est_tutorial_script())
output$est_tutorial_download<-downloadHandler(filename=function()if(isTRUE(est$compare))'comparison_example.R'else if(est$lesson%in%2:5)'sampling_simulation.R'else'estimation_example.R',content=function(file)writeLines(est_tutorial_script(),file,useBytes=TRUE))
output$est_tutorial_zip<-downloadHandler(filename=function()'estimation_example.zip',content=function(file){if(isTRUE(est$compare)){t<-comparison_state();csv<-tempfile(fileext='.csv');on.exit(unlink(csv));readr::write_csv(t$raw,csv);make_zip(file,est_tutorial_script(),csv,t$meta$name,est_tutorial_exact())}else make_zip(file,est_tutorial_script(),example_path,example_meta$name,est_tutorial_exact())})
observeEvent(input$est_tutorial_copy,session$sendCustomMessage('copyCode',est_tutorial_script()))

source('comparison_tutorial.R',local=TRUE)

output$est_download_exact<-downloadHandler(filename=function()'estimation_exact.R',content=function(file){req(est$spec);writeLines(estimation_script(rv$meta,est$steps,est$spec),file,useBytes=TRUE)})
output$est_tutorial_download_exact<-downloadHandler(filename=function()'estimation_example_exact.R',content=function(file)writeLines(est_tutorial_exact(),file,useBytes=TRUE))
