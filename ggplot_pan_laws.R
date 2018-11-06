# pan laws plot
library(ggplot2)

pan_law <- function(law_name, law_left, law_right, use_angle = TRUE)
{
  c(name=law_name, left=law_left, right=law_right, angle_based = use_angle)
}

pan_law_symmetric_root <- function(law_name, law_sq)
{
  pan_law(law_name, function(x){sqrt(1.0 - law_sq)}, function(x){ sqrt(law_sq)})
}

get_pan_laws <- function()
{
  angle_ratio <- function(theta) { theta / (pi - theta) }
  c(pan_law("linear",
            function(x) { (1 + x)/2 }, # span of cos normalized
            function(x) { (1 - x)/2 }, FALSE),
    pan_law("trig_half_angle", # sqrt of linear
            function(x) { cos(x*0.5) },
            function(x) { sin(x*0.5) }),
    pan_law("angle_ratio", # proportionate to
            function(x) { sqrt(1/(1 + angle_ratio(x)^2)) },
            function(x) { sqrt(1/(1 + angle_ratio(x)^2))*angle_ratio(x) }),
    pan_law_symmetric_root("polynomial_1",
                           function(x) { x/pi } ),
    pan_law_symmetric_root("polynomial_3",
                           function(x) { -2*(x/pi)^3 + 3*(x/pi)^2 } )
  )
}

# TODO: Make plot of difference
gen_dataframe <- function(use_db)
{
  pp <- seq(-1, 1 - 1.e-7, length=1000)
  pp_sin <- sapply(pp, function(x) {sqrt(1 - x^2)})
  
  afn <- function(x) { atan2(sqrt(1 - x^2),x) }
  pp_ang <- sapply(pp, function(x,y) { y(-x) }, afn)
  
  dbnormfn <- function(x) {
    db_val = 20*log10(x) 
    norm_db = 1 - pmax(0.0, pmin(1.0, db_val/-96.0))
  }
  
  if (use_db) {
  pan_dbfn <- function(x,y) {
      dbnormfn(y(-x))
    }
  } else {
    pan_dbfn <- function(x,y) { y(-x) }
  }
  pan_law1L <- function(x) { cos(afn(x)*0.5) }
  pan_law1R <- function(x) { sin(afn(x)*0.5) }
  
  # half angle
  #pan_law1L <- function(x) { sqrt((1 + x)/2) }
  #pan_law1R <- function(x) { sqrt((1 - x)/2) }
  
  pp_pl1L <- sapply(pp, pan_dbfn, pan_law1L)
  pp_pl1R <- sapply(pp, pan_dbfn, pan_law1R)

  pan_law1LDer <- function(x) { abs(- 0.5 * sin(afn(x)*0.5)) }
  pan_law1RDer <- function(x) { 0.5 * cos(afn(x)*0.5) }
    
  pp_pl1LDer <- sapply(pp, pan_dbfn, pan_law1LDer)
  pp_pl1RDer <- sapply(pp, pan_dbfn, pan_law1RDer)
  
  theta_fn <- function(theta) { theta / (pi - theta) }
  
  pan_law2L <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2)) }
  pan_law2R <- function(x) { sqrt(1/(1 + theta_fn(afn(x))^2))*theta_fn(afn(x)) }
  
  #pan_law2L <- function(x) { sqrt(1.0 - afn(x)/pi) }
  #pan_law2R <- function(x) { sqrt(afn(x)/pi) }
  
  pp_pl2L <- sapply(pp, pan_dbfn, pan_law2L)
  pp_pl2R <- sapply(pp, pan_dbfn, pan_law2R)

  pan_law2LDer <- function(x) { 
    x_i <- afn(x)
    abs(-0.5 * (2*x_i^2/(pi-x_i)^3 + 2*x_i/(pi - x_i)^2)*(1/(theta_fn(x_i)^2 + 1))^(3/2)) 
  }
  pan_law2RDer <- function(x) { 0.5 * cos(afn(x)*0.5) }
  
  pp_pl2LDer <- sapply(pp, pan_dbfn, pan_law2LDer)
  pp_pl2RDer <- sapply(pp, pan_dbfn, pan_law2RDer)
  
  pan_law3L <- function(x) {(1 + x)/2}#{1 - (1 - x)/2}
  pan_law3R <- function(x) {(1 - x)/2}
  
  pp_pl3L <- sapply(pp, pan_dbfn, pan_law3L)
  pp_pl3R <- sapply(pp, pan_dbfn, pan_law3R)
  
  rms <- function(x,y) {
    if (use_db) {
      return(dbnormfn(sqrt(x*x + y*y)))
    } else {
      return(sqrt(x*x + y*y))
    }
  }
  
  pp_p1RMS <- mapply(rms, sapply(pp, pan_law1L), sapply(pp, pan_law1R))
  pp_p2RMS <- mapply(rms, sapply(pp, pan_law2L), sapply(pp, pan_law2R))
  pp_p3RMS <- mapply(rms, sapply(pp, pan_law3L), sapply(pp, pan_law3R))
  
  data.frame(x_axis=pp, ang_ = pp_ang, unit_circle = pp_sin,
             pl1L = pp_pl1L, pl1R = pp_pl1R,
             pl2L = pp_pl2L, pl2R = pp_pl2R,
             pl3L = pp_pl3L, pl3R = pp_pl3R,
             rms1 = pp_p1RMS, rms2 = pp_p2RMS, rms3 = pp_p3RMS)
}

plotPanLawDb <- function()
{
  #line1 <- geom_line(aes(x=ang_,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Trig Left"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Trig Right"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Sqrt Left"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Sqrt Right"), linetype="dashed")
  
  line6 <- geom_line(aes(x=ang_,y=pl3L, color=" Lin: Left Channel"))
  line7 <- geom_line(aes(x=ang_,y=pl3R, color=" Lin: Right Channel"), linetype="dashed")
  
  line8 <- geom_hline(yintercept=1 - 3/96, linetype="dotted")
  
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8),
                               labels = c('0' , '\u03C0/8', '\u03C0/4', '3\u03C0/8', '\u03C0/2'
                                          , '5\u03C0/8', '3\u03C0/4', '7\u03C0/8', '\u03C0'
                                          , '9\u03C0/8', '5\u03C0/4', '11\u03C0/8', '3\u03C0/2'
                                          , '13\u03C0/8', '7\u03C0/4', '15\u03C0/8', '2\u03C0'))
  y_axis <- scale_y_continuous("dB", limits=c(0,1.0),
                               #breaks=c(0, 0.125, 0.25, 0.354, 0.5,0.707,0.841,1.0),
                               breaks=c(0, 0.5, 0.75, 0.875, 0.9375,1.0),
                               labels=c("-96 dB", "-48 dB", "-24 dB", "-12 dB", "-6 dB",
                                        "0 dB"))
  
  ac_labels <- labs(title = "Comparison of Three Panning Laws", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe(TRUE)
  
  ggplot(df_aac) + theme_bw(base_size=16) + ac_labels + line2 + line3 + line4 + line5 +
    line6 + line7 + line8 +
    scale_color_manual(values=c("#AA8888", "#AA8888", "#000000", "#000000",
                                "#B0B0B0", "#B0B0B0", "#AA8888")) + 
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    x_axis + y_axis + coord_polar(theta = "x", start=-pi/2)
}

plotPanLaw <- function() # TODO: use
{
  #line1 <- geom_line(aes(x=ang_,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Trigonometric: Left Channel"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Trigonometric: Right Channel"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Sqrt Ratio: Left Channel"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Sqrt Ratio: Right Channel"), linetype="dashed")
  
  line6 <- geom_line(aes(x=ang_,y=pl3L, color=" Lin: Left Channel"))
  line7 <- geom_line(aes(x=ang_,y=pl3R, color=" Lin: Right Channel"), linetype="dashed")
    
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8),
                               labels = c('0' , '\u03C0/8', '\u03C0/4', '3\u03C0/8', '\u03C0/2'
                                              , '5\u03C0/8', '3\u03C0/4', '7\u03C0/8', '\u03C0'
                                              , '9\u03C0/8', '5\u03C0/4', '11\u03C0/8', '3\u03C0/2'
                                              , '13\u03C0/8', '7\u03C0/4', '15\u03C0/8', '2\u03C0'))
  y_axis <- scale_y_continuous("Gain", limits=c(0,1.0)) #, trans='log10', breaks = c()
  
  ac_labels <- labs(title = "Comparison of Three Panning Laws", color = "")
  
  #coords <- coord_cartesian(ylim=c(0,-0.3), xlim=c(40,16000))
  
  df_aac <- gen_dataframe(FALSE)
  
  ggplot(df_aac) + theme_bw(base_size=16) + ac_labels + line2 + line3 + line4 + line5 +
    line6 + line7 +
    scale_color_manual(values=c("#AA8888", "#AA8888", "#000000", "#000000",
                                "#B0B0B0", "#B0B0B0")) + 
    scale_linetype_manual(values = c(1, 1, 2, 2)) +
    coord_polar(theta = "x", start=-pi/2) + x_axis + y_axis
}

plotPanLawRMSPolar <- function()
{
  line8 <- geom_line(aes(x=ang_,y=rms1, color=" Trig: RMS"))
  line9 <- geom_line(aes(x=ang_,y=rms2, color=" Sqrt: RMS"))
  line10 <- geom_line(aes(x=ang_,y=rms3, color=" Lin: RMS"))
  
  x_axis <- scale_x_continuous("Angle", limits=c(0,2*pi), breaks=seq(0, 2*pi, by=pi/8),
                               labels = c('0' , '\u03C0/8', '\u03C0/4', '3\u03C0/8', '\u03C0/2'
                                          , '5\u03C0/8', '3\u03C0/4', '7\u03C0/8', '\u03C0'
                                          , '9\u03C0/8', '5\u03C0/4', '11\u03C0/8', '3\u03C0/2'
                                          , '13\u03C0/8', '7\u03C0/4', '15\u03C0/8', '2\u03C0'))
  g_breaks <- seq(0.000015, 1.0005, by=0.25)
  s_labels <- sapply(g_breaks, function(x) {sprintf("%+0.1f dB", 20*log10(x))})
  y_axis <- scale_y_continuous("Gain", limits=c(0,1.1), breaks = g_breaks, labels = s_labels)
  
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  df_aac <- gen_dataframe(FALSE)
  
  ggplot(df_aac) + theme_bw() + ac_labels + 
    line8 + line9 + line10 +
    scale_color_manual(values=c("#AA8888", "#000000", "#000000")) +
    x_axis + y_axis + coord_polar(theta = "x", start=-pi/2)
}

plotPanLawRMS <- function()
{
  line8 <- geom_line(aes(x=ang_,y=rms1, color=" Trig: RMS"))
  line9 <- geom_line(aes(x=ang_,y=rms2, color=" Sqrt: RMS"))
  line10 <- geom_line(aes(x=ang_,y=rms3, color=" Lin: RMS"))
  
  x_axis <- scale_x_continuous("Angle")
  y_axis <- scale_y_continuous("Gain", limits=c(0.0, 1.0),trans='exp', breaks=c(0, 0.125, 0.25, 0.354, 0.5,0.707,0.841,1.0),
                               labels=c("-96 dB", "-18 dB", "-12 dB", "-9 dB", "-6 dB", "-3dB",
                                        "-1.5 dB", "0 dB"))
  
  ac_labels <- labs(title = "Pan Law 1", color = "")

  df_aac <- gen_dataframe(FALSE)
  
  ggplot(df_aac) + theme_bw() + ac_labels + 
    line8 + line9 + line10 +
    scale_color_manual(values=c("#AA8888", "#000000", "#000000")) +
    x_axis + y_axis
}

plotPanLawGain <- function()
{
  #line1 <- geom_line(aes(x=x_axis,y=unit_circle, color=" Angle"))
  line2 <- geom_line(aes(x=ang_,y=pl1L, color=" Left Trig"))
  line3 <- geom_line(aes(x=ang_,y=pl1R, color=" Right Trig"), linetype="dashed")
  line4 <- geom_line(aes(x=ang_,y=pl2L, color=" Left Lin"))
  line5 <- geom_line(aes(x=ang_,y=pl2R, color=" Right Lin"), linetype="dashed")
  
  line6 <- geom_line(aes(x=ang_,y=pl3L, color=" Lin: Left Channel"))
  line7 <- geom_line(aes(x=ang_,y=pl3R, color=" Lin: Right Channel"), linetype="dashed")  

  x_axis <- scale_x_continuous("Angle")
  y_axis <- scale_y_continuous("Gain", limits=c(0.0, 1.0),
                               #trans='exp',
                               breaks=c(0, 0.125, 0.25, 0.354, 0.5,0.707,0.841,1.0),
                               labels=c("-96 dB", "-18 dB", "-12 dB", "-9 dB", "-6 dB", "-3dB",
                                        "-1.5 dB", "0 dB"))
  
  ac_labels <- labs(title = "Pan Law 1", color = "")

  df_aac <- gen_dataframe(FALSE)
  
  ggplot(df_aac) + theme_bw() + ac_labels + 
    line2 + line3 + line6 + line7 + line4 + line5 + 
    scale_color_manual(values=c("#AA8888", "#AA8888", "#000000", "#000000",
                                "#B0B0B0", "#B0B0B0")) + 
    x_axis + y_axis
}

plotPanLawDifferenceDb <- function()
{
  line1 <- geom_line(aes(x=ang_,y=pl1DiffL, color=" Left Trig"))
  line2 <- geom_line(aes(x=ang_,y=pl3DiffL, color=" Left Lin"))
  line3 <- geom_hline(yintercept=0, linetype="dotted")
  line4 <- geom_hline(yintercept=-3/96, linetype="dotted")

  x_axis <- scale_x_continuous("Angle")

  g_labels <- seq(-6.0, 0.5, by=0.5)
  g_breaks <- sapply(g_labels, function(x) {x/96})
  s_labels <- sapply(g_labels, function(x) {sprintf("%+0.1f dB", x)})
  y_axis <- scale_y_continuous("Gain", limits=c(-0.06, 0.005), 
                               breaks=g_breaks, labels=s_labels)
  x_axis <- scale_x_continuous("Angle", limits=c(0,pi), breaks=seq(0, pi, by=pi/4),
                               labels = c('0' , '\u03C0/4', '\u03C0/2', '3\u03C0/4', '\u03C0'))
  ac_labels <- labs(title = "Pan Law 1", color = "")
  
  df_aac <- gen_dataframe(TRUE)
  df_aac$pl1DiffL <- df_aac$pl2L - df_aac$pl1L;
  df_aac$pl3DiffL <- df_aac$pl3L - df_aac$pl1L;
  
  ggplot(df_aac) + theme_bw(base_size=16) + ac_labels + line1 + line2 + line3 + line4 +
    scale_color_manual(values=c("#B0B0B0", "#000000", "#AA8888", "#AA8888")) +
    x_axis + y_axis
}