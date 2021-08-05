library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(magick)


# Load necessary packages ----------

library(shiny)
library(LaplacesDemon)
library(ggplot2)
library(logitnorm)
library(actuar)
# library(VGAM)
library(reshape)
library(mvtnorm)
library(ggExtra)
library(grid)
library(gridExtra)
library(DirichletReg)
library(scatterplot3d)
library(markdown)
library(tidyverse)

source("functions.R")
source("formulae.R")
source("CDF.R")
source("PDF.R")
source("code_latex.R")
source("code_r.R")
source("code_python.R")
source("code_stan.R")
source("code_matlab.R")
source("code_mathematica.R")
source("code_julia.R")
source("code_cplusplus.R")
source("plotting.R")
source("example_uses.R")
source("counter_server.R")
source("counter_ui.R")


# js code ---------------
jscode <- "shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
  backgroundColor: 'rgba(255, 255, 255, 0)',
  penColor: 'rgb(0, 0, 0)'
});
var saveButton = document.getElementById('save');
var cancelButton = document.getElementById('clear');

saveButton.addEventListener('click', function (event) {
  var data = signaturePad.toDataURL('image/png');
Shiny.onInputChange('shiny_data',data);
// Send data to server instead...
  //window.open(data);
});

cancelButton.addEventListener('click', function (event) {
        Shiny.onInputChange('shiny_clear','Hallo von clear');
// Send data to server instead...
        signaturePad.clear();

});


}"

# zooo pre conditions start -----

         # prismDependencies <- tags$head(
         # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
         # tags$link(rel = "stylesheet", type = "text/css",
         #          href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
         # )
         # prismLanguageDependencies <- function(languages) {
         # lapply(languages, function(x) {
         #  tags$head(
         #    tags$script(
         #      src = paste0("https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-",
         #                   x, ".min.js")
         #    )
         #  )
         # })
         # }
         # 
         # 
         # # Define UI for random distribution application
         # ben_link <- a("Ben Lambert", href="https://ben-lambert.com/bayesian/", target="_blank")
         # fergus_link <- a("Fergus Cooper", href="https://www.cs.ox.ac.uk/people/fergus.cooper/site/", target="_blank")
         # 
         # ga_30_line <- ""
         # ga_all_line <- ""

         # try({
         #   ga_30 <- rjson::fromJSON(file="https://www.cs.ox.ac.uk/people/fergus.cooper/google_analytics_data_30daysAgo.json")
         #   ga_all <- rjson::fromJSON(file="https://www.cs.ox.ac.uk/people/fergus.cooper/google_analytics_data_2019-01-08.json")
         #   ga_30_line <- h4("Last month: used by ", ga_30['user_count'], " people over ", ga_30['session_count'], "sessions in ", ga_30['country_count'], " countries")
         #   ga_all_line <- h4("Since created: used by ", ga_all['user_count'], " people over ", ga_all['session_count'], "sessions in ", ga_all['country_count'], " countries")
         # })

# zooo pre conditions end -----





#Ui -------
ui <- fluidPage(
   # setInputValue -------------
   tags$script("
    Shiny.addCustomMessageHandler('rhm_click', function(value) {
    //Shiny.setInputValue('dist', 'Normakl');
    Shiny.setInputValue('n', value[0]);
    Shiny.setInputValue('normal_mu', value[1]);
    Shiny.setInputValue('normal_sigma', value[2]);
    Shiny.setInputValue('uniform_a', value[3]);
    Shiny.setInputValue('uniform_b', value[3]);

    });
  "),
   
    includeCSS("custom.css"),
    tags$head(tags$script(src = "signature_pad.js")),
    
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode,functions = "shinyjs.init"),
    
    h1("Draw on plot"),
    div(class="wrapper",
        plotOutput("plot1"),
        HTML("<canvas id='signature-pad' class='signature-pad' width=600 height=400></canvas>"),
        HTML("<div>
           <button id='save'>Save</button>
           <button id='clear'>Clear</button>
           </div>")
    ),
    br(),br(),
    #modul----
    #counterButton(NULL,NULL)
    
    #zoo ui start ----
     #tags$head(includeHTML(("google-analytics.html"))),
     #includeCSS("styles.css"),
     # Application title
     #headerPanel("The distribution zoo"),
     #tagList(h4("by")),
     #fluidRow(h4(ben_link, " and ", fergus_link), ga_30_line, ga_all_line),
     #prismDependencies,
     #prismLanguageDependencies(c("r", "python", "latex",
     #                           "matlab", "mathematica", "c-like",
     #                             "c", "cpp", "julia")),
    
    # Sidebar with controls to select the random distribution type
    # and number of observations to generate. Note the use of the
    # br() element to introduce extra vertical spacing
    sidebarLayout(
      sidebarPanel(
        selectInput("distType", "Category of distribution",
                    c("Continuous Univariate"="Continuous",
                      "Discrete Univariate"="Discrete",
                      "Multivariate"="Multivariate"),
                    selected="Continuous"),
        conditionalPanel("input.distType=='Continuous'",
                         selectInput("dist", "Distribution type:",
                                     c("Beta"="Beta",
                                       "Cauchy"="Cauchy",
                                       "Exponential" = "Exponential",
                                       "Gamma" = "Gamma",
                                       "Half-Cauchy"="HalfCauchy",
                                       "Inverse-Chi-Squared"="InverseChiSquared",
                                       "Inverse-Gamma"="InverseGamma",
                                       "Logit-Normal"="LogitNormal",
                                       "Log-Normal" = "LogNormal",
                                       "Normal" = "Normal",
                                       "Student-t" = "t",
                                       "Uniform" = "Uniform"),
                                     selected="Normal")),
        conditionalPanel("input.distType=='Discrete'",
                         selectInput("dist1", "Distribution type:",
                                     c("Bernoulli" = "Bernoulli",
                                       "Beta-Binomial" = "BetaBinomial",
                                       "Binomial" = "Binomial",
                                       # "Categorical"="Categorical",
                                       "Discrete-Uniform" = "DiscreteUniform",
                                       "Negative-Binomial" = "NegativeBinomial",
                                       "Poisson" = "Poisson"))),
        conditionalPanel("input.distType=='Multivariate'",
                         selectInput("dist2", "Distribution type:",
                                     c("Dirichlet"="Dirichlet",
                                       "Inverse-Wishart"="InverseWishart",
                                       "LKJ"="LKJ",
                                       "Multinomial"="Multinomial",
                                       "Multivariate Normal" = "MultivariateNormal",
                                       "Multivariate Student-t" = "MultivariateT",
                                       "Wishart"="Wishart"))),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist!='Beta'",
                         sliderInput("n", "Range", value = 10,min = 0, max = 1000)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Normal'",
                         sliderInput("normal_mu", "Mean", min=-30, max=30, value=0, step=0.2),
                         sliderInput("normal_sigma", "Standard deviation", min=0.1, max=20, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Uniform'",
                         sliderInput("uniform_a", "Lower bound", min=-30, max=0, value=0, step=0.2),
                         sliderInput("uniform_b", "Upper bound", min=0.1, max=30, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='LogNormal'",
                         sliderInput("lognormal_mu", "Mean of log", min=-10, max=10, value=0, step=0.2),
                         sliderInput("lognormal_sigma", "Standard deviation of log", min=0.1, max=20, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Exponential'",
                         sliderInput("exponential_rate", "Rate parameter", min=0, max=5.0, value=0.5, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Gamma'",
                         sliderInput("gamma_shape", "Shape parameter", min=0.1, max=10, value=1),
                         sliderInput("gamma_rate", "Rate parameter", min=0.1, max=2, value=0.5)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='t'",
                         sliderInput("t_mu", "mode", min=-30, max=30, value=0, step=0.2),
                         sliderInput("t_sigma", "sigma parameter", min=0.1, max=10, value=1, step=0.2),
                         sliderInput("t_nu", "degrees of freedom", min=0, max=20, value=3, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Beta'",
                         sliderInput("beta_a", "Shape parameter 1", min=0.5, max=10, value=1),
                         sliderInput("beta_b", "Shape parameter 2", min=0.5, max=10, value=1)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Cauchy'",
                         sliderInput("cauchy_location", "location parameter", min=-30, max=30, value=0, step=0.2),
                         sliderInput("cauchy_scale", "scale parameter", min=0.5, max=10, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='HalfCauchy'",
                         sliderInput("halfcauchy_location", "location parameter", min=-30, max=30, value=0, step=0.2),
                         sliderInput("halfcauchy_scale", "scale parameter", min=0.5, max=10, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='InverseGamma'",
                         sliderInput("inversegamma_shape", "shape parameter", min=0.5, max=10, value=2),
                         sliderInput("inversegamma_scale", "scale parameter", min=0.5, max=10, value=1)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='InverseChiSquared'",
                         sliderInput("inversechisquared_df", "degrees of freedom", min=0.5, max=10, value=3)),
        conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='LogitNormal'",
                         sliderInput("logitnormal_mu", "mu parameter", min=-10, max=10, value=1, step=0.2),
                         sliderInput("logitnormal_sigma", "sigma parameter", min=0.5, max=10, value=1, step=0.2)),   
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Bernoulli'",
                         sliderInput("bernoulli_prob", "probability", min=0, max=1, value=0.5)),
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='BetaBinomial'",
                         sliderInput("betabinomial_size", "Size", min=0, max=50, value=10),
                         sliderInput("betabinomial_shape1", "Shape parameter 1", min=0, max=50, value=1, step=0.2),
                         sliderInput("betabinomial_shape2", "Shape parameter 2", min=0, max=50, value=1, step=0.2)),
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Binomial'",
                         sliderInput("binomial_size", "size", min=0, max=50, value=10),
                         sliderInput("binomial_prob", "probability", min=0, max=1, value=0.5)),
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='DiscreteUniform'",
                         sliderInput("discreteuniform_lower", "Lower bound", min=-30, max=0, value=0, step=1),
                         sliderInput("discreteuniform_upper", "Upper bound", min=0, max=30, value=1, step=1)),
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Poisson'",
                         sliderInput("poisson_lambda", "Rate", min=0, max=50, value=10, step=0.2),
                         sliderInput("poisson_range", "Range", min=0, max=100, value=40)),
        conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='NegativeBinomial'",
                         sliderInput("negativebinomial_mean", "Mean", min=0, max=50, value=10, step=0.2),
                         sliderInput("negativebinomial_dispersion", "Inverse-dispersion", min=0, max=100, value=3,step=0.2),
                         sliderInput("negativebinomial_range", "Range", min=0, max=100, value=40)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='MultivariateNormal'",
                         sliderInput("multivariatenormal_mux", "Mean of X", min=-10, max=10, value=0, step=0.2),
                         sliderInput("multivariatenormal_muy", "Mean of y", min=-10, max=10, value=0, step=0.2),
                         sliderInput("multivariatenormal_sigmax", "Standard deviation of x", min=0, max=5, value=1, step=0.2),
                         sliderInput("multivariatenormal_sigmay", "Standard deviation of y", min=0, max=5, value=1, step=0.2),
                         sliderInput("multivariatenormal_rho", "Correlation between x and y", min=-1, max=1, value=0, step=0.2),
                         sliderInput("multivariatenormal_range", "Range of plot", min=0, max=100, value=10)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='MultivariateT'",
                         sliderInput("multivariatet_mux", "Mode of x", min=-10, max=10, value=0, step=0.2),
                         sliderInput("multivariatet_muy", "Mode of y", min=-10, max=10, value=0, step=0.2),
                         sliderInput("multivariatet_sigmax", "Sigma x", min=0, max=5, value=1, step=0.2),
                         sliderInput("multivariatet_sigmay", "Sigma y", min=0, max=5, value=1, step=0.2),
                         sliderInput("multivariatet_rho", "Correlation component", min=-1, max=1, value=0,step=0.2),
                         sliderInput("multivariatet_df", "Degrees of freedom", min=0, max=50, value=10,step=0.2),
                         sliderInput("multivariatet_range", "Range of plot", min=0, max=100, value=10)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Wishart'",
                         sliderInput("wishart_dimension", "Dimensions", min=4, max=20, value=4),
                         sliderInput("wishart_df", "Degrees of freedom", min=4, max=100, value=8, step=0.2),
                         sliderInput("wishart_samplesize", "Sample size", min=1000, max=20000, value=5000)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='InverseWishart'",
                         sliderInput("inversewishart_dimension", "Dimensions", min=4, max=20, value=4),
                         sliderInput("inversewishart_df", "Degrees of freedom", min=4, max=100, value=8, step=0.2),
                         sliderInput("inversewishart_samplesize", "Sample size", min=1000, max=20000, value=5000)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Dirichlet'",
                         sliderInput("dirichlet_dimension","Dimensions",min=2,max=4,value=2,step=1),
                         sliderInput("dirichlet_samplesize", "Sample size", min=10, max=20000, value=1000),
                         sliderInput("dirichlet_alpha1","alpha 1",min=0.1,max=10,value=2),
                         sliderInput("dirichlet_alpha2","alpha 2",min=0.1,max=10,value=2),
                         conditionalPanel(condition="input.dirichlet_dimension>'2'",
                                          sliderInput("dirichlet_alpha3","alpha 3",min=0.1,max=10,value=2),
                                          conditionalPanel(condition="input.dirichlet_dimension>'3'",
                                                           sliderInput("dirichlet_alpha4","alpha 4",min=0.1,max=10,value=2)))),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Multinomial'",
                         sliderInput("multinomial_angle","Viewpoint angle",min=0,max=360,value=100),
                         sliderInput("multinomial_size","size",min=2,max=100,value=6),
                         sliderInput("multinomial_prob1","unnormalised probability 1",min=0,max=1,value=0.5),
                         sliderInput("multinomial_prob2","unnormalised probability 2",min=0,max=1,value=0.5),
                         sliderInput("multinomial_prob3","unnormalised probability 3",min=0,max=1,value=0.5)),
        conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='LKJ'",
                         sliderInput("lkj_dimension", "Dimensions", min=4, max=20, value=4),
                         sliderInput("lkj_eta", "Degrees of freedom", min=0, max=40, value=1,step=0.2),
                         sliderInput("lkj_samplesize", "Sample size", min=1000, max=20000, value=2000)),
        br()
      ),
      # Show a tabset that includes a plot, summary, and table view
      # of the generated distribution
      mainPanel(
        uiOutput('mytabs')
      )
    )
)
    #zoo ui end ----<
    
    
    



# Server------

server <- function(input, output, session){

    output$plot1 <- renderPlot({
        
        df <- sample_frac(diamonds, 0.1)
        
        ggplot(df) +

          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, size = 18),
                axis.text = element_text(size=14),
                axis.title = element_text(size=16))
        
        # qplot(1:100,200:299,
        #       xlab="X",ylab="probability density"
        #       )+
        #   geom_line(color='darkblue',size=1) +
        #   geom_vline(xintercept=ifelse(is.na(aMean),-10000,aMean),
        #              colour="orange",
        #              linetype = "longdash",
        #              size=1) +
        #   theme_classic() +
        #   ggtitle(paste0("mean (orange line) = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2))) +
        #   theme(plot.title = element_text(hjust = 0.5, size = 18),
        #         axis.text = element_text(size=14),
        #         axis.title = element_text(size=16)) +
        #   ylim(0, NA)
        
        
    })
   # observe random dist / parameter --------
    observeEvent(input$dist, {
       session$sendCustomMessage("rhm_click", list(100,2,4,8))
       print(" observed!")
    })
    observeEvent(input$shiny_clear, {
       cat(" observed!")
    })
    
   observeEvent(input$shiny_data,{
   shiny_data <-  input$shiny_data
   print(shiny_data)
   plot_src <- gsub("^data.*base64,", "",shiny_data)
    # decode the image code into the image
    plot_image <- image_read(base64enc::base64decode(plot_src))
    # save the image
    image_write(plot_image, paste0("image_data",as.character(as.numeric(format(Sys.time(), "%OS30")) * 1000 + sample(10000:100000,1)),"_",input$dist ,".png") )
     }
     )
    #modul ----
     #counterServer(NULL)
     #zoo start -----
   
   library(shiny)
   library(LaplacesDemon)
   library(ggplot2)
   library(logitnorm)
   library(actuar)
   # library(VGAM)
   library(reshape)
   library(mvtnorm)
   library(ggExtra)
   library(grid)
   library(gridExtra)
   library(DirichletReg)
   library(scatterplot3d)
   library(markdown)
   library(tidyverse)
   
   source("functions.R")
   source("formulae.R")
   source("CDF.R")
   source("PDF.R")
   source("code_latex.R")
   source("code_r.R")
   source("code_python.R")
   source("code_stan.R")
   source("code_matlab.R")
   source("code_mathematica.R")
   source("code_julia.R")
   source("code_cplusplus.R")
   source("plotting.R")
   source("example_uses.R")
   # Define server logic for random distribution application
   #shinyServer(function(input, output) {
   
   data <- reactive({
      #rewrite to random process ----
      
      print(structure(input$n)) 
      
     dist<-#if(input$distType=='Continuous'){
        dnorm
       # switch(input$dist,
       #        Normal = dnorm,
       #        Uniform = dunif,
       #        LogNormal = dlnorm,
       #        Exponential = dexp,
       #        Gamma=dgamma,
       #        t = dst,
       #        Beta=dbeta,
       #        Cauchy=dcauchy,
       #        HalfCauchy=dCustomHalfCauchy,
       #        InverseGamma=dinvgamma,
       #        InverseChiSquared=dCustomInverseChiSquared,
       #        LogitNormal=dlogitnorm,
       #        dnorm)
     #}
     #
     #   else if (input$distType=='Discrete'){
     #   switch(input$dist1,
     #          Bernoulli=dbern,
     #          BetaBinomial=dCustomBetaBinomial,
     #          Binomial=dbinom,
     #          DiscreteUniform=dunifdisc,
     #          Poisson=dpois,
     #          NegativeBinomial=dnbinom,
     #          dbern)
     # } else if (input$distType=='Multivariate'){
     #   switch(input$dist2,
     #          MultivariateNormal=dmvnorm,
     #          MultivarateT=dmvt,
     #          dmvnorm)
     # }
   })
   dataCDF <- reactive({
     dist <- if (input$distType=='Continuous') {
       switch(input$dist,
              Normal = pnorm,
              Uniform = punif,
              LogNormal = plnorm,
              Exponential = pexp,
              Gamma=pgamma,
              t = pst,
              Beta=pbeta,
              Cauchy=pcauchy,
              HalfCauchy=pCustomHalfCauchy,
              InverseGamma=pinvgamma,
              InverseChiSquared=pCustomInverseChiSquared,
              LogitNormal=plogitnorm,
              pnorm)
     } else if (input$distType=='Discrete'){
       switch(input$dist1,
              Bernoulli=pbern,
              Binomial=pbinom,
              DiscreteUniform=punifdisc,
              Poisson=ppois,
              NegativeBinomial=pnbinom,
              BetaBinomial=pCustomBetaBinomial,
              dbern)
     } 
   })
   
   fScale <- reactive({
     lScale <- fScaleFull(input)
   })
   
   fScale1 <- reactive({
     lScale <- fScaleFull1(input)
   })
   
   fExtraFunctionInputs <- reactive({
     lExtra <- fExtraFunctionInputsFull(input)
     return(lExtra)
   })
   
   fExtra1FunctionInputs <- reactive({
     lExtra <- fExtra1FunctionInputsFull(input)
     return(lExtra)
   })
   
   fScaleMVR <- reactive({
     if(input$dist2=="MultivariateNormal")
       lSeq <- seq(-input$multivariatenormal_range, input$multivariatenormal_range,
                   2 * input$multivariatenormal_range / 100)
     else if(input$dist2=="MultivariateT")
       lSeq <- seq(-input$multivariatet_range, input$multivariatet_range,
                   2 * input$multivariatenormal_range / 100)
     return(lSeq)
   })
   
   fCalculateMean <- reactive({
     lExtra <- fCalculateMeanFull(input)
     
   })
   
   fCalculateVariance <- reactive({
     aVar <- fCalculateVarianceFull(input)
     return(aVar)
   })
   output$plot <- renderPlot({
     aDist <- data()
     aMean <- fCalculateMean()
     if (input$distType == 'Continuous'){
       aVar <- fCalculateVariance()
       lExtra <- fExtraFunctionInputs()
       lScale <- fScale()
     }else if (input$distType=='Discrete') {
       aVar <- fCalculateVariance()
       lExtra <- fExtra1FunctionInputs()
       lScale <- fScale1()
     }else if (input$dist2=='MultivariateNormal'){
       aVar <- -99
       lScale <- fScaleMVR()
     }else if (input$dist2=='MultivariateT'){
       aVar <- -99
       lScale <- fScaleMVR()
     }else{
       aVar <- -99
       lScale <- vector()
       lExtra <- vector()
     }
     fPlotPDF(input, aDist, aMean, aVar, lScale, lExtra)
   })
   
   output$plotCDF <- renderPlot({
     aDist <- dataCDF()
     aMean <- fCalculateMean()
     if (input$distType=='Continuous'){
       aVar <- fCalculateVariance()
       lScale <- fScale()
       lExtra <- fExtraFunctionInputs()
     }else if(input$distType=='Discrete'){
       lExtra <- fExtra1FunctionInputs()
       aVar <- fCalculateVariance()
       lScale <- fScale1()
     }else if (input$dist2=='MultivariateNormal'){
       lScale <- fScaleMVR()
       lExtra <- vector()
       aVar <- -99
     }else if (input$dist2=='MultivariateT'){
       lScale <- fScaleMVR()
       lExtra <- vector()
       aVar <- -99
     }
     fPlotCDF(input, aDist, aMean, aVar, lScale, lExtra)
   })
   
   output$formulae <- renderUI({
     fFormulae(input)
   })
   
   output$latex <- renderUI({
     fLatex(input)
   })
   
   output$rcode <- renderUI({
     fRcode(input)
   })
   
   output$pythoncode <- renderUI({
     fPythoncode(input)
   })
   
   output$matlabcode <- renderUI({
     fMatlabcode(input)
   })
   
   output$mathematicacode <- renderUI({
     fMathematicacode(input)
   })
   
   output$juliacode <- renderUI({
     fJuliacode(input)
   })
   
   output$cpluspluscode <- renderUI({
     fCpluspluscode(input)
   })
   
   output$stancode <- renderUI({
     fStanCode(input)
   })
   
   output$BUGScode <- renderUI({
   })
   
   output$JAGScode <- renderUI({
   })
   
   output$ccode <- renderUI({
   })
   
   output$fortrancode <- renderUI({
   })
   
   output$example_uses <- renderUI({
     fExampleUses(input)
   })
   
   output$language <- renderUI({
     selectInput("language", "Language",
                 c("Julia"="Julia",
                   "Mathematica"="Mathematica",
                   "Matlab"="Matlab",
                   "Python"="Python",
                   "R"="R",
                   "Stan"="Stan"
                 ),
                 selected="R")
   })
   
   output$property <- renderUI({
     selectInput("property", "Property",
                 c("PDF"="pdf",
                   "Log PDF"="log_pdf",
                   "random sample of size n"="random"),
                 selected="pdf")
   })
   
   output$code <- renderUI({
     tagList(h4("Note: generates dynamic code for distributions with same properties as in plots"),
             if(is.null(input$language)){
               uiOutput("rcode")
             }else{
               switch(input$language,
                      R=uiOutput("rcode"),
                      Python=uiOutput("pythoncode"),
                      Matlab=uiOutput("matlabcode"),
                      Fortran=uiOutput("fortrancode"),
                      Mathematica=uiOutput("mathematicacode"),
                      Julia=uiOutput("juliacode"),
                      C=uiOutput("ccode"),
                      Cplusplus=uiOutput("cpluspluscode"),
                      Stan=uiOutput("stancode"),
                      BUGS=uiOutput("BUGScode"),
                      JAGS=uiOutput("JAGScode"))
             })
   })
   
   output$mytabs = renderUI({
     # if(input$distType!='Multivariate'){
     #   myTabs = tabsetPanel(type = "tabs", 
     #                        tabPanel("Plot of PDF", plotOutput("plot"),
     #                                 uiOutput("runningQuantities")), 
     #                        tabPanel("Plot of CDF", plotOutput("plotCDF"),
     #                                 uiOutput("runningQuantities1")),
     #                        tabPanel("Formulae", 
     #                                 uiOutput("formulae")),
     #                        tabPanel("LaTeX", 
     #                                 uiOutput("latex")),
     #                        tabPanel("Code", 
     #                                 uiOutput("language"),
     #                                 uiOutput("property"),
     #                                 uiOutput("code")),
     #                        tabPanel("Practical tips",
     #                                 uiOutput("example_uses"))
     #   )
     # }else{
       # myTabs = tabsetPanel(type = "tabs", 
       #                      tabPanel("Plot of PDF", 
                                     plotOutput("plot")
       #,
       #                               uiOutput("runningQuantities")),
       #                      tabPanel("Formulae", 
       #                               uiOutput("formulae")),
       #                      tabPanel("LaTeX", 
       #                               uiOutput("latex")),
       #                      tabPanel("Code", 
       #                               uiOutput("language"),
       #                               uiOutput("property"),
       #                               uiOutput("code")),
       #                      tabPanel("Practical tips",
       #                               uiOutput("example_uses"))
       # )
     #}
   }) # zoo end -----
   
   
    }
shinyApp(ui = ui, server = server)