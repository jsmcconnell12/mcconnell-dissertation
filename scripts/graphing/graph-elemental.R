
#########################################################
### Declare functions to build elemental graphs
#########################################################

elemental_line <- function(
  d_observed,
  variable_name,
  time_metric, 
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = FALSE,
  main_title     = variable_name,
  x_title        = paste0("Time metric: ", time_metric),
  y_title        = variable_name,
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  g <- ggplot(d_observed, aes_string(x=time_metric, y = variable_name)) 
  if(!smoothed){
    g <- g + geom_line(aes_string(group="id"), size=line_size, color=scales::alpha(color_name,line_alpha), na.rm=T)   
  } else{
    g <- g + geom_smooth(aes_string(group="id"),size=line_size,  method="lm",color=scales::alpha(color_name,line_alpha), na.rm=T, se=F )
    g <- g + geom_smooth(method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T)
    
  }  
  
  g <- g + 
    # scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    # labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  return( g )
}



# ------ black-and-white-version ------------------------
plot_trajectories_bw <- function(
  d,
  sample_size=200
){
  # d <- ds_long_newc
  # d <- ds_long_octo
  if(sample_size=="max"){
    ids <- unique(d$id)
    sample_size_ <- length(ids)
  }else{
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
  }
  dd <- d %>% dplyr::filter(id %in% ids)
  g <-  dd %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_line(aes(group=id), size=.8, alpha=.3, color="black")+
    facet_grid(class~.) +
    # coord_cartesian(xlim=c(-15,2))+
    labs(y="MMSE",x="Time until death")+
    # scale_colour_manual(values=colors_classes)+
    main_theme#+
  
  # theme(text = element_text(size=baseSize+6))
}
# plot_trajectories_bw(ds_long_newc ) %>% print()
# plot_trajectories_bw(ds_long_newc, facet_="vertical") %>% print()

plot_trajectories <- function(d,sample_size=200){
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  g <-  d %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_line(aes(group=id), size=.8, alpha=.7)+
    labs(y="MMSE",x="Time until death",color="Latent class")+
    scale_colour_manual(values=colors_classes)+
    main_theme#+
  # theme(text = element_text(size=baseSize+6))
}

plot_trajectories_v2 <- function(d,sample_size=200){
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  if(!sample_size=="max"){
    dd <- d %>% dplyr::filter(id %in% ids)
  }else{dd <- d}
  g <-  dd %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_point()+
    geom_line(aes(group=id), size=.8, alpha=.7)+
    labs(y="MMSE",x="Time until death",color="Latent class")+
    scale_colour_manual(values=colors_classes)+
    main_theme#+
  # theme(text = element_text(size=baseSize+6))
  g
}
# plot_trajectories_v2(ds_long_newc)


########################
##### Lines ############
#######################

proto_line <- function(
  d, # list object with extracted data (.gh5) and parsed coefficients (.out)
  x, # mapped to x-axis
  outcome_name, # one of the two processes: process_a or process_b
  source_name, # observed or reconstructed from factor scores: observed or fscores
  fill, # mapped to the color of dots: wave or BAGE
  group_label
){
  # d = ds
  # x            ="age"
  # outcome_name ="mmse"
  # source_name  ="observed"
  # fill         ="wave"
  # get data from the list object
  # d <- dto$octo$ds_long
  # d <- ls_model$data
  # d <- ls$data #%>% dplyr::filter(id %in% sample(id,10))
  # browser()
  d1 <- d %>%
    # dplyr::filter(outcome == outcome_name) %>%
    dplyr::mutate(
      wave = factor(wave)
    )
  # d1 <- d1 %>%  dplyr::filter(id %in% sample(id,50))
  # compute min and max to harmonize the graphs
  (ymax <- ceiling(max(d1[,outcome_name], na.rm = T)*1.05))
  (ymin <- floor(min(d1[,outcome_name], na.rm = T)*.95))
  d2 <- d1 %>%
    dplyr::filter(source == source_name)
  
  g <- ggplot2::ggplot(d2,aes_string(x=x, y=outcome_name, fill=fill, group="id")) +
    geom_smooth(method="lm", color=alpha("grey70",.6), na.rm=T, se=F) +
    geom_point(shape=21,size=3, alpha=.4)+
    # the group arguments seems to malfunction, yet produce correct image. investigate
    geom_smooth(aes(group='id'),method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T) +
    scale_y_continuous(limits=c(ymin, ymax))+
    facet_grid(class~.)+
    labs(fill = group_label)+
    # this scale will be added in the complex plot, here now for testing and looking
    # scale_fill_gradient2(low="#7fbf7b", mid="#f7f7f7", high="#af8dc3", space="Lab")+
    main_theme
  g
}