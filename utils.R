get_chi.square <- function(df){
  
  do.call(
  "rbind",
  apply(
    df,
    MARGIN = 1, 
    function(x){
      obs = as.numeric(c(x['no'],x['yes']))
      mychi = chisq.test( obs, p = c(0.5,0.5) )
      
      data.frame(
        p.value   = round(mychi$p.value, digits = 4),
        x.squared = mychi$statistic,
        dfree  = mychi$parameter
        )
      })
  )
}

two_sample_comparison <- function( mydf,  groups = NULL,
                                   at = "Subcategoria", comparison = "greater",
                                   counts = T, jitter = F){

  if(class(mydf)[1] != "data.frame"){
    mydf = as.data.frame(mydf)
  }
  

  varcol      = ifelse(counts, "yes", "sff" )
  firstgroup  = as.numeric(mydf[ mydf[,at] == groups[1], varcol])
  secondgroup = as.numeric(mydf[ mydf[,at] == groups[2], varcol])
  
  if(jitter){
    firstgroup  = jitter(firstgroup)
    secondgroup = jitter(secondgroup)
  }
  
  mywil = wilcox.test( 
    firstgroup, 
    secondgroup, 
    alternative  = comparison,
    correct = F
    )
  
  out = data.frame(
    comparison = paste(groups, collapse = " - "),
    p.val = mywil$p.value,
    H.alternative = comparison
    )
  
  return(out)
}


twoplus_sample_comparison <- function(mydf,  at = "Categoria",
                                      counts = T, jitter = F, 
                                      p.adjustment = "bh"){
  
  if(class(mydf)[1] != "data.frame"){
    mydf = as.data.frame(mydf)
  }
  
  varcol = ifelse(counts, "yes", "sff" )
  myvals = as.numeric(mydf[,varcol])
  
  if(jitter){
    myvals = jitter(  myvals )
  }
  
  FSA::dunnTest( 
    myvals,
    g = mydf[, at],
    method = p.adjustment
    ) -> mytest
  
  return(mytest$res)
}

my_theme <- function(base_size = 15, 
                     rarefaction = T){
  
  q <- theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor = element_blank()
      ,panel.background = element_blank()
      ,panel.grid.major.x = element_blank()
      )
  
  if(!rarefaction){
    q <- q + theme(panel.grid.major.y = element_blank())
  }
  
  return(q)
}

get_ssf.df <- function(df, groupby){

  df %>%
    dplyr::group_by(.dots = groupby) %>%
    dplyr::summarise(
      no  = sum(`Fraud/Mislabeling` == "No"  ),
      yes = sum(`Fraud/Mislabeling` == "Si"  ),
      n   = length(`Fraud/Mislabeling`),
      sff =  yes*100/ length(  `Fraud/Mislabeling`)
    ) %>%
    dplyr::ungroup()-> sff_df
  
  return(sff_df)
}
