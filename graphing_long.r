graphing_long <- function(num_reallocs, int_port_returns,
                     hca_returns, bullet_returns, rand_returns, cap, title) {
  
  
  int_port_returns = ((int_port_returns/cap)-1) * 100
  hca_returns = ((hca_returns/cap)-1) * 100
  bullet_returns = ((bullet_returns/cap)-1)* 100
  rand_returns = ((rand_returns/cap)-1) * 100

  #one dataframe of all relevant plotting data
  dat_df <- data.frame(cbind(int_port_returns, hca_returns, bullet_returns, rand_returns))
  
  #colors for legend
  colors <- c("Interval Portfolio" = "#58508d", 
              "HCA" = "#ffa600",
              "Markowitz Bullet" = "#bc5090",
              "Equal Allocation" = "#EE6352")
  
  return_plt <- ggplot(data = dat_df, aes(x = num_reallocs)) + 
    geom_line(aes(y = int_port_returns, color = "Interval Portfolio")) + 
    geom_line(aes(y = hca_returns,color = "HCA")) +
    geom_line(aes(y = bullet_returns, color = "Markowitz Bullet")) + 
    geom_line(aes(y = rand_returns, color = "Equal Allocation")) +
    #scale_y_continuous(limits = c(70, 150)) +
    labs(y = "Returns (Percentage)", x = "Allocations", 
         col = "Model", title = "Performance by Model",
         subtitle = title) +
    scale_color_manual(values = colors) + theme_light()
  
  
  show(return_plt)
}

graph_longdata <- function(file_dat)
{
  #equal_alloc_returns = rand_returns$X
  num_allocs = 0:10
  n <- length(file_dat)
  
  hca_cl = as.numeric(file_dat[1,])
  mark = as.numeric(file_dat[2,])
  int_prt = as.numeric(file_dat[3,])
  rand_dat = as.numeric(file_dat[4,])
  
  class(num_allocs);class(int_prt);  class(hca_cl); class(mark); class(rand_dat)
  
  title = "Using Different Numbers of Reallocations"
  graphing_long(num_allocs, int_prt, hca_cl, mark, rand_dat, 100000, title)
}
#graph_longdata(long_file)
