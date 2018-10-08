# pan laws plot
library(ggplot2)

gen_dataframe <- function(use_db)
{
  pp <- seq(-1, 1 - 1.e-7, length=1000)
  pp_sin <- sapply(pp, function(x) {sqrt(1 - x^2)})
  
  afn <- function(x) { atan2(sqrt(1 - x^2),x) }
  pp_ang <- sapply(pp, function(x,y) { y(-x) }, afn)
  
  if (use_db) {
  pan_dbfn <- function(x,y) { 
      db_val = 20*log10(y(-x)) 
      norm_db = 1 - pmax(0.0, pmin(1.0, db_val/-96.0))
    }
  } else {
    pan_dbfn <- function(x,y) { y(-x) }
  }
  pan_law1L <- function(x) { cos(afn(x)*0.5) }
  pan_law1R <- function(x) { sin(afn(x)*0.5) }
  
  pp_pl1L <- sapply(pp, pan_dbfn, pan_law1L)
  pp_pl1R <- sapply(pp, pan_dbfn, pan_law1R)
  
  theta_fn <- function(theta) { theta / (pi - theta) }
  
  pan_law2L <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2)) }
  pan_law2R <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2))*theta_fn(afn(x)) }
  
  pp_pl2L <- sapply(pp, pan_dbfn, pan_law2L)
  pp_pl2R <- sapply(pp, pan_dbfn, pan_law2R)  
  
  data.frame(x_axis=pp, ang_ = pp_ang, unit_circle = pp_sin,
             pl1L = pp_pl1L, pl1R = pp_pl1R,
             pl2L = pp_pl2L, pl2R = pp_pl2R)
}

plotPanLawDb <- function()
{
  #line1 <- geom_line(aes(x=ang_,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Trig Left"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Trig Right"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Lin Left"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Lin Right"), linetype="dashed")
  
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8),
                               labels = c('0' , '\u03C0/8', '\u03C0/4', '3\u03C0/8', '\u03C0/2'
                                          , '5\u03C0/8', '3\u03C0/4', '7\u03C0/8', '\u03C0'
                                          , '9\u03C0/8', '5\u03C0/4', '11\u03C0/8', '3\u03C0/2'
                                          , '13\u03C0/8', '7\u03C0/4', '15\u03C0/8', '2\u03C0'))
  y_axis <- scale_y_continuous("dB", limits=c(0,1.0),
                               breaks=c(0, 0.125, 0.25, 0.354, 0.5,0.707,0.841,1.0),
                               labels=c("-96 dB", "-18 dB", "-12 dB", "-9 dB", "-6 dB", "-3dB",
                                        "-1.5 dB", "0 dB")) #, trans='log10', breaks = c()
  
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe(TRUE)
  
  ggplot(df_aac) + theme_bw() + ac_labels + line2 + line3 + line4 + line5 +
    scale_color_manual(values=c("#000000", "#000000", "#A0A0A0", "#A0A0A0")) + 
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    coord_polar(theta = "x", start=-pi/2) + x_axis + y_axis
}

plotPanLaw <- function()
{
  #line1 <- geom_line(aes(x=ang_,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Trig Left"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Trig Right"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Lin Left"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Lin Right"), linetype="dashed")
    
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8),
                               labels = c('0' , '\u03C0/8', '\u03C0/4', '3\u03C0/8', '\u03C0/2'
                                              , '5\u03C0/8', '3\u03C0/4', '7\u03C0/8', '\u03C0'
                                              , '9\u03C0/8', '5\u03C0/4', '11\u03C0/8', '3\u03C0/2'
                                              , '13\u03C0/8', '7\u03C0/4', '15\u03C0/8', '2\u03C0'))
  y_axis <- scale_y_continuous("Gain", limits=c(0,1.0)) #, trans='log10', breaks = c()
  
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe(FALSE)
  
  ggplot(df_aac) + theme_bw(base_size=16) + ac_labels + line2 + line3 + line4 + line5 +
    scale_color_manual(values=c("#000000", "#000000", "#B0B0A0", "#B0B0B0")) + 
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