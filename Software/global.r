

options(java.parameters = "-Xmx3000m")

      if (exists("setupWT"))
        rm(setupWT)

      ipak <- function(pkg) {
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
          install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
      }

      packages <-
        c("rJava",
          "glue",
          "shinydashboard",
          "ggplot2",
          "shinythemes",
          "shinyjs",
          "readxl",
          "data.table",
          "xlsx",
          "GTest",
          "DEoptim",
          "doParallel",
          "parallel",
          "zoo",
          "shinyFiles",
          "rlist",
          "knitr",
          "markdown",
          "kableExtra",
          "xtable",
          "DT",
          "cowplot",
          "shinyBS",
          "shinyWidgets",
          "datasets")

      ipak(packages)

      cores = detectCores()
      set_cores = 1:cores



      first_straight_section = function(data, endd)
      {
        
   
        t=c()
        l=2
        
        if(endd >= 120)
        start = endd - 120
        
        else if(endd >= 80)
          start = endd - 80
        
        else if(endd >= 60)
          start = endd - 60
        
        else if(endd >= 40)
          start = endd - 40
        
        else if(endd >= 30)
          start = endd - 30
        
        else if(endd >= 20)
          start = endd - 20
        
        
        else if(endd >= 10)
          start = endd - 10
        
        else if(endd >= 5)
          start = endd - 5
        
        
        end = endd

    
        
        for(i in start:end)
        {
          min=l
          max=l+6
          r=lm(data[min:max,2] ~ log(data[min:max,1]))
          t[i]=r$coefficients[2]
          l=l+1
        }
        max_slope=which.max(t)
        skin_slope=which(t>t[max_slope]*0.9)
        final_slope = (lm(data[skin_slope, 2] ~ log10(data[skin_slope, 1])))
       
        
        return(final_slope)
      }


      additional_resistances_from_slope = function(T, iz, Q, Cd)
      {
        
        
        W = - 300
      hw = 15
      izd = (2*pi*T*iz)/Q
      vysledek = 5


        while (vysledek > 0.001  && W < 4000 )
        {
          vysledek =  - izd + (1.02946 - (3.5937*10^-3)*W + ((1.24019*10^-4)*W^2) - ((2.0686*10^-6)*W^3)
                               + ((1.32356*10^-6)*W^3)) * log10(Cd) + 0.862 +1.0606*W


          if(vysledek <0)
            vysledek = vysledek * -1
          W = W + 0.001
        }


        hw = (Q/(2*pi*T))*W


      return(hw)

      }



      skin_by_time = function(Q, T, S, rv, izs, cd)
      {
        tt= 10^(-izs$coefficients[1]/izs$coefficients[2])
        
        tdd = (T*tt)/(S*rv^2)
        
        ww = (tdd - 10^(1.075*log10(cd)-0.2562))/(10^(1.0001*log10(cd) - 0.7485))
        
        w = (Q/(2*pi*T)) *ww
        
        vysledek = list(w = ww, hw = w)
        
       
        return(vysledek)
      }
    
        
      
      
      
      
      
