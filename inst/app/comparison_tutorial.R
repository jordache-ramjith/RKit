# Sourced inside server; worked examples have their own data and exports.
comparison_state<-reactive({
 mean<-est$kind=='mean';allowed<-if(mean)c(8,80)else c(100,1000)
 n<-if(!is.null(input$cmp_size)&&as.numeric(input$cmp_size)%in%allowed)as.integer(input$cmp_size)else allowed[1]
 if(mean){
   pattern<-rep(c(-2,-1.5,-1,-.5,.5,1,1.5,2),length.out=n)
   raw<-data.frame(programme=rep(c('Programme','Usual routine'),each=n),sleep_hours=c(7.6+pattern,7+pattern))
   s<-list(kind='mean',x='sleep_hours',group='programme',reference='Usual routine',compare=TRUE,unit='hours',event='',conf=.95,design='')
 }else{
   pa<-if(is.null(input$cmp_pa))50 else input$cmp_pa;pb<-if(is.null(input$cmp_pb))40 else input$cmp_pb
   a<-as.integer(round(n*pa/100));b<-as.integer(round(n*pb/100))
   raw<-data.frame(programme=rep(c('Programme','Usual routine'),each=n),symptom_month=c(rep('Yes',a),rep('No',n-a),rep('Yes',b),rep('No',n-b)))
   s<-list(kind='proportion',x='symptom_month',group='programme',reference='Usual routine',compare=TRUE,unit='',event='Yes',conf=.95,design='cohort')
 }
 meta<-example_meta;meta$name<-'comparison_example.csv'
 list(raw=raw,meta=meta,spec=s,result=run_estimation(raw,s))
})
comparison_tutorial_ui<-function(){
 i<-est$lesson;mean<-est$kind=='mean';item<-comparison_lesson(est$kind,i);total<-comparison_lesson_count(est$kind)
 allowed<-if(mean)c(8,80)else c(100,1000);selected<-isolate(if(!is.null(input$cmp_size)&&as.numeric(input$cmp_size)%in%allowed)input$cmp_size else allowed[1])
 show_interval<-if(mean)i>=4 else i>=7
 tagList(div(class='section-head',div(span(class='progress-label',paste('Step',i,'of',total)),h3(item$title)),actionButton('est_back_analysis','Back to my analysis')),
  div(class='lesson-layout',div(class='panel-card lesson-copy',lapply(item$text,p),
    if(show_interval)help_box('An interval is a range of values compatible with the data under a method. Read its width and the values it contains, not only whether it includes the no-difference value.'),
    div(class='nav-actions',actionButton('est_lesson_prev','Previous',disabled=if(i==1)'disabled'else NULL),actionButton('est_lesson_next',if(i==total)'Start again'else'Next step',class='btn-primary'))),
    div(class='panel-card',h4('A two-group study to picture'),p(item$context),
      selectInput('cmp_size','People in each group',setNames(allowed,if(mean)c('8 — smaller fictional study','80 — larger fictional study')else c('100 — smaller fictional study','1,000 — larger fictional study')),selected=selected),
      if(!mean)tagList(sliderInput('cmp_pa','With a symptom: Programme (%)',min=0,max=100,value=isolate(if(is.null(input$cmp_pa))50 else input$cmp_pa),step=5),
       sliderInput('cmp_pb','With a symptom: Usual routine (%)',min=0,max=100,value=isolate(if(is.null(input$cmp_pb))40 else input$cmp_pb),step=5),
       p('Each table row summarises one group of different fictional people. Every person is observed for the same one-month period.'),scroll_table('cmp_counts')),
      scroll_table('cmp_groups'),uiOutput('cmp_arithmetic'),
      if(show_interval)tagList(h4('Estimate the comparison'),scroll_table('cmp_table'),
        if(!mean)selectInput('cmp_metric','Which comparison would you like to see?',c('Risk difference'=1,'Risk ratio'=2,'Odds ratio'=3)),
        p(if(mean)'The dot marks the observed mean difference. The line marks its 95% confidence interval. Zero means equal population means.'else'The dot marks the comparison and the line its 95% confidence interval. The dashed line is 0 for a difference and 1 for a ratio. Ratios use a logarithmic scale: 0.5 and 2 sit equally far from 1.'),
        scroll_plot('cmp_plot',340,640),uiOutput('cmp_interpretation'))else scroll_plot('cmp_people',350,640))),
   code_box('est_tutorial_code','est_tutorial_copy','est_tutorial_download','est_tutorial_zip'))
}
output$cmp_counts<-renderTable({as.data.frame.matrix(comparison_state()$result$counts)},rownames=TRUE,striped=TRUE)
output$cmp_groups<-renderTable({t<-comparison_state();est_display_table(t$result,t$spec,preview=TRUE)},digits=2,striped=TRUE)
output$cmp_people<-renderPlot(comparison_state()$result$diagnostic,res=96)
output$cmp_table<-renderTable({t<-comparison_state();comparison_display(t$result,t$spec)},digits=3,striped=TRUE)
output$cmp_plot<-renderPlot({r<-comparison_state()$result;j<-if(est$kind=='mean'||is.null(input$cmp_metric))1L else as.integer(input$cmp_metric);r$comparison_plots[[j]]},res=96)
output$cmp_interpretation<-renderUI({t<-comparison_state();lapply(comparison_description(t$result,t$spec),p)})
output$cmp_arithmetic<-renderUI({
 t<-comparison_state();r<-t$result;s<-t$spec;i<-est$lesson
 if(i<3)return(NULL)
 f<-function(x)if(is.na(x))'undefined'else if(is.infinite(x))'infinite'else formatC(x,format='f',digits=2)
 a<-r$table[r$table$group=='Programme',];b<-r$table[r$table$group=='Usual routine',]
 if(s$kind=='mean')return(help_box(paste0('Programme minus Usual routine: ',f(a$estimate),' − ',f(b$estimate),' = ',f(a$estimate-b$estimate),' hours. In these fictional data the programme mean is higher.')))
 if(i==3)return(help_box(paste0('Risk difference: ',f(a$estimate*100),'% − ',f(b$estimate*100),'% = ',f(r$comparison$estimate[1]),' percentage points.')))
 if(i==4)return(help_box(paste0('Risk ratio: ',f(a$estimate),' ÷ ',f(b$estimate),' = ',f(r$comparison$estimate[2]),'. This compares risks, not odds.')))
 odds<-function(a)if(a$n==a$events)Inf else a$events/(a$n-a$events)
 lines<-c(paste0('Programme: risk = ',a$events,'/',a$n,' = ',f(a$estimate),'; odds = ',a$events,'/',a$n-a$events,' = ',f(odds(a)),'.'),
 paste0('Usual routine: risk = ',b$events,'/',b$n,' = ',f(b$estimate),'; odds = ',b$events,'/',b$n-b$events,' = ',f(odds(b)),'.'))
 if(i>=6)lines<-c(lines,paste0('Odds ratio: ',f(odds(a)),' ÷ ',f(odds(b)),' = ',f(r$comparison$estimate[3]),'. The risk ratio for these same people is ',f(r$comparison$estimate[2]),'.'))
 tagList(lapply(lines,p))
})
