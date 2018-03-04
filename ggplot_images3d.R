bc_vector <- function(x_, y_, z_)
{
  list(x=x_, y=y_, z=z_)
}

bc_linesegment <- function(start_, end_)
{
  list(start=start_, end=end_)
}

bc_volume_rect <- function(position_, left_, forward_, up_)
{
  list(position = position_, left = left_, forward = forward_, up = up_)
}

bc_volume_capsule <- function(position_, axis_, radius_)
{
  ls <- bc_linesegment(bc_op_sum(position_, axis_),
                       bc_op_sum(position_, bc_op_scalar_mul(-1, axis_)))
  list(position = position_, axis = axis_, radius = radius_, line_segment = ls)
}

bc_op_sum <- function(lhs, rhs)
{
  bc_vector(lhs$x + rhs$x, lhs$y + rhs$y, lhs$z + rhs$z)
}

bc_op_minus <- function(lhs, rhs)
{
  bc_vector(lhs$x - rhs$x, lhs$y - rhs$y, lhs$z - rhs$z)
}

bc_op_dot <- function(lhs, rhs)
{
  lhs$x * rhs$x + lhs$y * rhs$y + lhs$z * rhs$z
}

bc_vector_norm <- function(x, y, z)
{
  v <- bc_vector(x,y,z)
  mag <- sqrt(bc_op_dot(v,v))
  
  bc_op_scalar_mul(1/mag, v)
}

bc_op_scalar_mul <- function(lhs, rhs)
{
  bc_vector(lhs * rhs$x, lhs * rhs$y, lhs * rhs$z)
}

bc_project <- function(segment_, position_)
{
  projected <- bc_op_minus(position_, segment_$start)
  translated <- bc_op_minus(segment_$end, segment_$start)
  proportion <- bc_op_dot(projected, translated)
  
  if (proportion <= 0.0)
  {
    return(segment_$start);
  }
  length_sq = bc_op_dot(translated, translated);
  if (proportion >= length_sq)
  {
    return(segment_$end);
  }
  
  projected <- bc_op_scalar_mul(proportion / length_sq, translated);
  projected <- bc_op_sum(projected, segment_$start)
  projected
}

bc_segment_to_aes <- function(segment_)
{
  aes_(x = segment_$start$x, y = segment_$start$y,
      xend = segment_$end$x, yend = segment_$end$y)
}

plot_projection <- function(segment_, position_, show_angle_)
{
  projection <- bc_project(segment_, position_)
  
  lines <- c(geom_point(aes_(x = projection$x, y = projection$y)),
             geom_segment(bc_segment_to_aes(segment_), size=1),
             geom_point(aes_(x = position_$x, y = position_$y)),
             geom_segment(aes_(x = position_$x, y = position_$y,
                       xend = projection$x, yend = projection$y), linetype="dashed"))
  
  if (show_angle_)
  {
    square_size <- 0.05
    proj_seg <- bc_op_minus(position_, projection)
    proj_length <- sqrt(bc_op_dot(proj_seg,proj_seg))
    
    proj_norm <- bc_op_scalar_mul(square_size/proj_length, proj_seg)
    #return(proj_norm)
    proj_orth <- bc_vector(proj_norm$y * 1.15, -proj_norm$x * 1.15, 0)
    
    angle_square_pos <- bc_op_sum(bc_op_sum(proj_norm, proj_orth), projection)
    lines <- c(lines, 
               geom_segment(bc_segment_to_aes(
                 bc_linesegment(angle_square_pos, bc_op_sum(proj_norm, projection))), size=0.5),
               geom_segment(bc_segment_to_aes(
                 bc_linesegment(angle_square_pos, bc_op_sum(proj_orth, projection))), size=0.5))
  }
  
  # plot_ + lines + coord_fixed()
  lines
}

path_figures <<- "C:/Programming/ntaylor_"

get_figure_path <- function(filename_)
{
  paste(path_figures, filename_, sep="")
}

figure_1_plots <- function(plot_)
{
  point_proj1 <- plot_projection(bc_linesegment(
    bc_vector(0,0,0), bc_vector(0.7,0.6, 0)), bc_vector(-0.3, 0.45, 0), TRUE)

  point_proj2 <- plot_projection(
    bc_linesegment(bc_vector(1,0,0), bc_vector(1.35,0.3, 0)), bc_vector(1.40, 0.55, 0), FALSE)
  
  plot_ + point_proj1 + point_proj2 + get_plain_theme() + coord_fixed()
}

figure_1_1_plots <- function(plot_)
{
  lines <- vector(mode="list", length=5)
  
  solution <- bc_linesegment(
    bc_vector(0,8,0), bc_vector(7, 7, 0))
  
  lines[[1]] <- bc_linesegment(
    solution$end, bc_vector(10, 5, 0))
  lines[[2]] <- bc_linesegment(
    lines[[1]]$end, bc_vector(14, 3, 0))
  lines[[3]] <- bc_linesegment(
    lines[[2]]$end, bc_vector(15.5, 3.5, 0))
  lines[[3]] <- bc_linesegment(
    lines[[2]]$end, bc_vector(20, 3.5, 0))
  lines[[4]] <- bc_linesegment(
    lines[[3]]$end, bc_vector(21.5,6, 0))
  
  point_proj1 <- plot_projection(solution, bc_vector(2.5, 10.45, 0), TRUE)
  
  plot_ + point_proj1 + lines_to_geom(lines) + get_plain_theme() + coord_fixed()
}

rect_to_lines <- function(rect_)
{
  verts <- vector(mode="list", length=8)
  verts[[1]] <- bc_op_sum(bc_op_sum(rect_$left, rect_$forward), rect_$up)
  verts[[2]] <- bc_op_sum(bc_op_minus(rect_$left, rect_$forward), rect_$up)
  verts[[3]] <- bc_op_minus(bc_op_minus(rect_$left, rect_$forward), rect_$up)
  verts[[4]] <- bc_op_minus(bc_op_sum(rect_$left, rect_$forward), rect_$up)
  
  back_face <- bc_op_scalar_mul(-1, rect_$left)
  verts[[5]] <- bc_op_sum(bc_op_sum(back_face, rect_$forward), rect_$up)
  verts[[6]] <- bc_op_sum(bc_op_minus(back_face, rect_$forward), rect_$up)
  verts[[7]] <- bc_op_minus(bc_op_minus(back_face, rect_$forward), rect_$up)
  verts[[8]] <- bc_op_minus(bc_op_sum(back_face, rect_$forward), rect_$up)
  
  for (i in 1:8)
  {
    verts[[i]] <- bc_op_sum(verts[[i]], rect_$position)
  }
  
  lines <- vector(mode = "list", length=12)
  lines[[1]] <- bc_linesegment(verts[[1]], verts[[2]])
  lines[[2]] <- bc_linesegment(verts[[2]], verts[[3]])
  lines[[3]] <- bc_linesegment(verts[[3]], verts[[4]])
  lines[[4]] <- bc_linesegment(verts[[4]], verts[[1]])
  
  #back
  lines[[5]] <- bc_linesegment(verts[[5]], verts[[6]])
  lines[[6]] <- bc_linesegment(verts[[6]], verts[[7]])
  lines[[7]] <- bc_linesegment(verts[[7]], verts[[8]])
  lines[[8]] <- bc_linesegment(verts[[8]], verts[[5]])
  
  # sides
  lines[[9]] <- bc_linesegment(verts[[1]], verts[[5]])
  lines[[10]] <- bc_linesegment(verts[[2]], verts[[6]])
  lines[[11]] <- bc_linesegment(verts[[3]], verts[[7]])
  lines[[12]] <- bc_linesegment(verts[[4]], verts[[8]])
  
  lines
}

rect_to_extent_lines <- function(rect_)
{
  lines <- vector(mode="list", length=3)
  
  lines[[1]] <- bc_linesegment(rect_$position, bc_op_sum(rect_$position, rect_$forward))
  lines[[2]] <- bc_linesegment(rect_$position, bc_op_sum(rect_$position, rect_$left))
  lines[[3]] <- bc_linesegment(rect_$position, bc_op_sum(rect_$position, rect_$up))
  
  lines
}

project_point_to_rect <- function(p_, rect_)
{
  p_rect <- bc_op_minus(p_, rect_$position)
  projected <- bc_project(
    bc_linesegment(rect_$forward, bc_op_scalar_mul(-1, rect_$forward)) , p_rect)
  projected <- bc_op_sum(projected, bc_project(
    bc_linesegment(rect_$left, bc_op_scalar_mul(-1, rect_$left)) , p_rect))
  projected <- bc_op_sum(projected, bc_project(
    bc_linesegment(rect_$up, bc_op_scalar_mul(-1, rect_$up)) , p_rect))
  
  bc_op_sum(projected, rect_$position)
}

# my_rect <- bc_volume_rect(bc_vector(0, 0, 8), bc_vector(2, 0, 0), bc_vector(0,1,0), bc_vector(0,0,1))
# plot_lines <- lines_to_geom_proj(rect_lines, bc_vector_norm(-0.35, 0.45, 0.65))
# plot_lines_e <- lines_to_geom_proj(test_lines_e, 0.5, 0.4, 0.5, "dotted", arrow(type="closed", length=unit(0.015, units="npc")))

project_point_to_plane <- function(p_, normal_)
{
  direction <- bc_op_scalar_mul(bc_op_dot(p_, normal_), normal_)
  bc_op_minus(p_, direction)  
}

project_lines_to_plane <- function(lines_, normal_)
{
  projected_lines <- vector(mode = "list", length=length(lines_))
  for (i in 1:length(lines_))
  {
    projected_lines[[i]]$start <- project_point_to_plane(lines_[[i]]$start, normal_)
    projected_lines[[i]]$end <- project_point_to_plane(lines_[[i]]$end, normal_)
  }
  
  projected_lines
}

lines_to_geom <- function(lines_, linetype_="solid", arrow_=NULL)
{
  geom <- vector(mode = "list", length=length(lines_))
  
  for (i in 1:length(lines_))
  {
    geom[[i]] <- geom_segment(
      bc_segment_to_aes(lines_[[i]]), linetype=linetype_, arrow=arrow_)
  }
  
  geom
}

lines_to_geom_proj <- function(lines_, normal_, linetype_="solid", arrow_=NULL)
{
  projected_lines <- project_lines_to_plane(lines_, normal_)
  
  lines_to_geom(projected_lines, linetype_, arrow_)
  
}

point_to_geom_proj <- function(p_, normal_)
{
  projected <- project_point_to_plane(p_, normal_)
  geom_point(aes_(x = projected$x, y = projected$y))
}

plot_point_to_rect <- function(p_, rect_, view_normal_)
{
  plot_point <- point_to_geom_proj(p_, view_normal_)
  p_to_rect <- project_point_to_rect(p_, rect_)
  plot_point_rect <- point_to_geom_proj(p_to_rect, view_normal_)
  
  rect_lines <- rect_to_lines(rect_)
  rect_extents <- rect_to_extent_lines(rect_)
  
  plot_lines <- lines_to_geom_proj(rect_lines, view_normal_)
  plot_lines_extents <- lines_to_geom_proj(
    rect_extents, view_normal_, "dotted", 
    arrow(type="closed", length=unit(0.015, units="npc")))
  
  project_lines <- vector(mode="list", length=1)
  project_lines[[1]] <- bc_linesegment(p_, p_to_rect)
  plot_project <- lines_to_geom_proj(project_lines, view_normal_, "dotted")
  
  geoms <- c(plot_lines, plot_lines_extents, plot_project, plot_point, plot_point_rect)
  geoms
}

figure_2_plots <- function(plot_)
{
  rect_1 <- bc_volume_rect(
    bc_vector(0, 0, 8), bc_vector(2, 0, 0), bc_vector(0,1.5,0), bc_vector(0,0,1))
  
  rect_1_plots <- plot_point_to_rect(
    bc_vector(1.1, 0.3, -2), rect_1, bc_vector_norm(-0.35, -0.45, 0.65))
  
  rect_2 <- bc_volume_rect(
    bc_vector(6, -2, 8), bc_vector(0,2,0), bc_vector(0,0,2), bc_vector(2, 0, 1))

  rect_2_plots <- plot_point_to_rect(
      bc_vector(6.1, 5.3, 8.5), rect_2, bc_vector_norm(-0.35, -0.45, 0.65))
  
  plot_ + rect_1_plots + rect_2_plots + get_plain_theme() + coord_fixed()
}

bc_circle_lines <- function(center_, radius_, control_points_)
{
  lines <- vector(mode = "list", length=control_points_)
  
  start_point <- bc_op_sum(center_, radius_)
  radius_length <- sqrt(bc_op_dot(radius_, radius_))
  angle_amount <- 2*pi/control_points_;
  for (i in 1:control_points_)
  {
    angle <- angle_amount * i
    end_point <-  bc_vector(cos(angle) * radius_length, sin(angle) * radius_length, 0)
    end_point <- bc_op_sum(center_, end_point)
    lines[[i]] <- bc_linesegment(start_point, end_point)
    start_point <- end_point
  }
  
  lines
}

project_point_to_capsule <- function(p_, capsule_)
{
  along_axis <- bc_project(capsule_$line_segment, p_)
  
  p_relative <- bc_op_minus(p_, along_axis)
  proj_dist_sq <- bc_op_dot(p_relative, p_relative)
  
  if (proj_dist_sq < capsule_$radius * capsule_$radius)
  {
    return(p_)
  }
  
  proj_p <- bc_op_scalar_mul(capsule_$radius/sqrt(proj_dist_sq), p_relative)
  proj_p <- bc_op_sum(proj_p, along_axis)
  proj_p
}

plot_point_to_capsule2 <- function(p_, capsule_, view_normal_)
{
  circle_top <- bc_circle_lines(
    capsule_$line_segment$end, bc_vector(capsule_$radius, 0, 0), 50)
  circle_bottom <- bc_circle_lines(
    capsule_$line_segment$start, bc_vector(capsule_$radius, 0, 0), 50)
  
  circle_top_proj <- project_lines_to_plane(circle_top, view_normal_)
  circle_bottom_proj <- project_lines_to_plane(circle_bottom, view_normal_)
  
  index_top <- 0
  index_bottom <- 0
  top <- bc_vector(0,0,0)
  bottom <- bc_vector(0,0,0)
  
  for (i in 1:length(circle_top_proj))
  {
    if (index_top == 0 ||
        circle_top_proj[[i]]$start$x > top$x)
    {
      index_top <- i
      top <- circle_top_proj[[i]]$start
    }
    if (index_bottom == 0 ||
             circle_top_proj[[i]]$start$x < bottom$x)
    {
      index_bottom <- i
      bottom <- circle_top_proj[[i]]$start
    }
  }
  
  lines <- vector(mode = "list", length=2)
  lines[[1]] <- bc_linesegment(top, circle_bottom_proj[[index_top]]$start)
  lines[[2]] <- bc_linesegment(bottom, circle_bottom_proj[[index_bottom]]$start)
  
  capsule_geom <- lines_to_geom(c(circle_bottom_proj, circle_top_proj, lines))
  
  extents <- vector(mode = "list", length=2)
  extents[[1]] <- bc_linesegment(capsule_$position, capsule_$line_segment$start)
  extents[[2]] <- bc_linesegment(capsule_$position, capsule_$line_segment$end)
  
  extent_geom <- lines_to_geom_proj(
    extents, view_normal_, "dotted", 
    arrow(type="closed", length=unit(0.015, units="npc")))
  
  p_proj <- project_point_to_capsule(p_, capsule_)
  
  project_lines <- vector(mode="list", length=1)
  project_lines[[1]] <- bc_linesegment(p_, p_proj)
  plot_project <- lines_to_geom_proj(project_lines, view_normal_, "dotted")
  
  cap_1 <- geom_curve(aes_(
    x=lines[[1]]$start$x, y=lines[[1]]$start$y, 
    xend=lines[[2]]$start$x, yend=lines[[2]]$start$y),
    curvature = -1, ncp=20)
  
  cap_2 <- geom_curve(aes_(
    x=lines[[1]]$end$x, y=lines[[1]]$end$y, 
    xend=lines[[2]]$end$x, yend=lines[[2]]$end$y),
    curvature = 1, ncp=20)
  
  c (capsule_geom, extent_geom, cap_1, cap_2,
     plot_project,
     point_to_geom_proj(p_, view_normal_),
     point_to_geom_proj(p_proj, view_normal_))
}

figure_3_plots <- function(plot_)
{
  my_cap1 <- bc_volume_capsule(bc_vector(0,-1,1), bc_vector(0,0,1), 0.333)
  all_caps <- plot_point_to_capsule2(bc_vector(-0.75,-1.25,0.75), my_cap1, bc_vector(0.5,-1,1))
  
  my_cap2 <- bc_volume_capsule(bc_vector(2,-1,1.25), bc_vector(0,0,0.75), 0.633)
  all_caps2 <- plot_point_to_capsule2(bc_vector(3.5,0,3.00), my_cap2, bc_vector(0.5,-1,1))
  
  plot_ + all_caps + all_caps2
}

doppler_relative <- function(listener_, source_)
{
  lines <- vector(mode = "list", length=2)
  lines[[1]] <- listener_
  lines[[2]] <- source_
 
  velocities <- lines_to_geom(lines, "solid", arrow(type="closed", length=unit(0.015, units="npc")))

  lines <- vector(mode = "list", length=1)
  lines[[1]] <- bc_linesegment(listener_$start, source_$start)
  
  relative <- lines_to_geom(lines, "dashed")
  
  p1 <- geom_point(aes_(x = listener_$start$x, y = listener_$start$y))
  p2 <- geom_point(aes_(x = source_$start$x, y = source_$start$y))
  
  listener_rel <- bc_project(lines[[1]], listener_$end)
  source_rel <- bc_project(lines[[1]], source_$end)
  
  lines_rel <- vector(mode = "list", length=2)
  lines_rel[[1]] <- bc_linesegment(listener_$start, listener_rel)
  lines_rel[[2]] <- bc_linesegment(source_$start, source_rel)
  
  rel_velocities <- lines_to_geom(lines_rel, "solid", arrow(type="closed", length=unit(0.015, units="npc")))
  
  lines_proj <- vector(mode = "list", length=2)
  lines_proj[[1]] <- bc_linesegment(listener_$end, listener_rel)
  lines_proj[[2]] <- bc_linesegment(source_$end, source_rel)  
  
  proj <- lines_to_geom(lines_proj, "dotted")
  
  c(velocities, rel_velocities, relative, proj, p1, p2)
}

figure_4_plots <- function(plot_)
{
  dop <- doppler_relative(bc_linesegment(bc_vector(-1,-1,0), bc_vector(-1,0.33,0)), bc_linesegment(bc_vector(1,1,0), bc_vector(0.5,0.13,0)))
  dop2 <- doppler_relative(bc_linesegment(bc_vector(1.0,-0.5,0), bc_vector(1.5,-0.633,0)), bc_linesegment(bc_vector(2.5,1,0), bc_vector(2,0.93,0)))
  
  plot_ + dop + dop2
}

# my_cap1 <- bc_volume_capsule(bc_vector(0,-1,1), bc_vector(0,0,1), 0.333)
# all_caps <- plot_point_to_capsule2(bc_vector(-1,0,3.75), my_cap1, bc_vector(0.5,-1,1))

# all_rect_plots <- plot_point_to_rect(bc_vector(1.1, 0.3, -5), my_rect, bc_vector_norm(-0.35, -0.45, 0.65))
# all_rect_plots <- plot_point_to_rect(bc_vector(-1.1, 0.3, -4), my_rect, bc_vector_norm(-0.35, -0.45, 0.65))
# cc_geom <- geom_curve(aes_(x=curve_start$x, y=curve_start$y, xend=curve_end$x, yend=curve_end$y), curvature = 0.5, ncp=20)