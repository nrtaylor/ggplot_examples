# pan laws plot
library(ggplot2)

gen_dataframe <- function()
{
  pp <- seq(-1, 1 - 1.e-7, length=1000)
  pp_sin <- sapply(pp, function(x) {sqrt(1 - x^2)})
  
  afn <- function(x) { atan2(sqrt(1 - x^2),x) }
  pp_ang <- sapply(pp, afn)
  
  pan_dbfn <- function(x,y) { 20*log10(y(-x)) }
  
  pan_law1L <- function(x) { cos(afn(x)*0.5) }
  pan_law1R <- function(x) { sin(afn(x)*0.5) }
  
  pp_pl1L <- sapply(pp, pan_law1L)
  pp_pl1R <- sapply(pp, pan_law1R)
  
  theta_fn <- function(theta) { theta / (pi - theta) }
  
  pan_law2L <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2)) }
  pan_law2R <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2))*theta_fn(afn(x)) }
  
  pp_pl2L <- sapply(pp, pan_law2L)
  pp_pl2R <- sapply(pp, pan_law2R)  
  
  data.frame(x_axis=pp, ang_ = pp_ang, unit_circle = pp_sin,
             pl1L = pp_pl1L, pl1R = pp_pl1R,
             pl2L = pp_pl2L, pl2R = pp_pl2R)
}

plotPanLaw <- function()
{
  #line1 <- geom_line(aes(x=ang_,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Trig Left"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Trig Right"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Lin Left"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Lin Right"), linetype="dashed")
    
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8))
  y_axis <- scale_y_continuous("dB") #, trans='log10', breaks = c()
  
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe()
  
  ggplot(df_aac) + theme_bw() + ac_labels + line2 + line3 + line4 + line5 +
    scale_color_manual(values=c("#000000", "#000000", "#A0A0A0", "#A0A0A0")) + 
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    coord_polar(theta = "x", start=-pi/2) + x_axis + y_axis
}

gen_dataframeGain <- function()
{
  pp <- seq(-1, 1 - 1.e-7, length=1000)
  pp_sin <- sapply(pp, function(x) {sqrt(1 - x^2)})
  
  pan_dbfn <- function(x,y) { 20*log10(y(-x)) }
  
  afn <- function(x) { atan2(sqrt(1 - x^2),x) }
  pan_law1L <- function(x) { cos(afn(x)*0.5) }
  pan_law1R <- function(x) { sin(afn(x)*0.5) }
  
  pp_pl1L <- sapply(pp, pan_law1L)
  pp_pl1R <- sapply(pp, pan_law1R)
  
  theta_fn <- function(theta) { theta / (pi - theta) }
  
  pan_law2L <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2)) }
  pan_law2R <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2))*theta_fn(afn(x)) }
  
  pp_pl2L <- sapply(pp, pan_law2L)
  pp_pl2R <- sapply(pp, pan_law2R)  
  
  data.frame(x_axis=pp, unit_circle = pp_sin,
             pl1L = pp_pl1L, pl1R = pp_pl1R,
             pl2L = pp_pl2L, pl2R = pp_pl2R)
}

plotPanLawGain <- function()
{
  line1 <- geom_line(aes(x=x_axis,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=x_axis,y=pl1L, color=" Left Trig"))
  line3 <- geom_line(aes(x=x_axis,y=pl1R, color=" Right Trig"))
  line4 <- geom_line(aes(x=x_axis,y=pl2L, color=" Left Lin"))
  line5 <- geom_line(aes(x=x_axis,y=pl2R, color=" Right Lin"))
  
  x_axis <- scale_x_continuous("Angle")
  y_axis <- scale_y_continuous("dB") #, trans='log10', breaks = c()
  
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe()
  
  ggplot(df_aac) + theme_bw() + ac_labels + line1 + line2 + line3 +
    line4 + line5 +
    x_axis + y_axis
}