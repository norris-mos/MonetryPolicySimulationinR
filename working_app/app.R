# Define UI for 3 equation

library(shiny)

ui<-pageWithSidebar(
    
    # App title
    headerPanel('3 Equation Model'),
    
    
    
    # Sidebar panel for inputs
    
    sidebarPanel(
        
        numericInput('shock_time',
                     'Shock to occur in time period:',
                     5,
                     min=0,
                     max=20,
                     step=0.5),
                 
                    
        numericInput(
            
            
            "exogenous",
            "A",
            1.09,
            min = 0,
            max = 10 ,
            step = 0.01,),
        numericInput(
            
            
            "alpha",
            "α/β",
            1,
            min = 0,
            max = 10 ,
            step = 1,),
        
        numericInput('gamma',
                     'γ',
                     2,
                     min=0,
                     max=20,
                     step=1),
        
        numericInput('output_e',
                     'Equilibrium output:',
                     1,
                     min=0,
                     max=20,
                     step=1)

            

            

        
    
    ),
    
    
    
    
    
    
    
    #Main panel for outputs
    
    mainPanel("main panel",
              fluidRow(splitLayout(cellWidths=c("50%","50%","50%"),plotOutput('plot_output'),plotOutput('plot_rate'),plotOutput('plot_inflation')))
        

    )
    
    
    

)
server<-function(input,output) {
    
    output$plot_output<- renderPlot({
        
        
    
        y<-vector(length=20)
        r<-vector(length=20)
        π<-vector(length=20)
        
        a<-1.1
        α<-β<-input$alpha
        γ<-input$gamma
        π_T<-0.02
        yε<-input$output_e
        y[1]<-yε
        π[1]<-π_T
        r[1]<-(a-yε)/γ
        shock<-input$shock_time
        for(t in 2:20){
            if(t<shock){a<-1.1}else{a<-input$exogenous}
            #IS curve
            y[t]<-a-(γ*r[t-1])

            #Philips curve


            δy<-y[t]-yε
            π[t]<-π[t-1]+(α*δy)

            #IR rule
            δπ<-π[t]-π_T
            p2<-(α*β*δπ)/((γ*1)+(γ*α^2*β))
            #print(p1)
            p1<-(a-yε)/γ
            r[t]<-p1+p2
            }
    
    


        

        plot(y,type='l',xlab='Period',ylab='y')
        
        
        
        })
    
    
    
    
    
    output$plot_rate<- renderPlot({
        
        
    
        y<-vector(length=20)
        r<-vector(length=20)
        π<-vector(length=20)
        
        a<-1.1
        α<-β<-input$alpha
        γ<-input$gamma
        π_T<-0.02
        yε<-input$output_e
        y[1]<-yε
        π[1]<-π_T
        r[1]<-(a-yε)/γ
        shock<-input$shock_time
        for(t in 2:20){
            if(t<shock){a<-1.1}else{a<-input$exogenous}
            #IS curve
            y[t]<-a-(γ*r[t-1])

            #Philips curve


            δy<-y[t]-yε
            π[t]<-π[t-1]+(α*δy)

            #IR rule
            δπ<-π[t]-π_T
            p2<-(α*β*δπ)/((γ*1)+(γ*α^2*β))
            #print(p2)
            p1<-(a-yε)/γ
            r[t]<-p1+p2
            }
    
    


        

        plot(r,type='l',xlab='Period',ylab='r')
        
        
        
        })
    
    output$plot_inflation<- renderPlot({
        
        
    
        y<-vector(length=20)
        r<-vector(length=20)
        π<-vector(length=20)
        
        a<-1.1
        α<-β<-input$alpha
        γ<-input$gamma
        π_T<-0.02
        yε<-input$output_e
        y[1]<-yε
        π[1]<-π_T
        r[1]<-(a-yε)/γ
        shock<-input$shock_time
        for(t in 2:20){
            if(t<shock){a<-1.1}else{a<-input$exogenous}
            #IS curve
            y[t]<-a-(γ*r[t-1])

            #Philips curve


            δy<-y[t]-yε
            π[t]<-π[t-1]+(α*δy)

            #IR rule
            δπ<-π[t]-π_T
            p2<-(α*β*δπ)/((γ*1)+(γ*α^2*β))
            #print(p1)
            p1<-(a-yε)/γ
            r[t]<-p1+p2
            }
    
    


        

        plot(π,type='l',xlab='Period',ylab='π')
        
        
        
        })
    
    
    
    


    

    formulaText<-reactive({
        paste('Shock at time ~',input$shock_time)
        })
    
    
    
}
shinyApp(ui,server)