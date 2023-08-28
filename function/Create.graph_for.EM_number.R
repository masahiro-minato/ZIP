Create.graph_for.EM_number <- function(
    df.num = Metis_MF3_ADF_number_by_Due_date,
    df.EM = Metis_MF3_EM_ADF_by_Maintenance_date,
    field = "ADF",
    choice = c("`COOK-C`","`SINAI-H`"),
    choice2 = c("COOK_C_EM","SINAI_H_EM"),
    choice3 = c("COOK_C_EM.rate","SINAI_H_EM.rate"),
    X_date = seq(as.Date("2019-01-01"), as.Date("2022-11-30"), by = "day"),
    file.save = FALSE,
    write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_month.tsv",
    graph_title = "Metis-MF3_ADF EM件数",
    legend.position = c(0.85, 0.95),
    x.breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),
    y.breaks = seq(0, 50, 10),
    ylim = c(0, 50),
    graph.save = FALSE,
    save.graph_path = './PDF/Metis-MF3_ADF_EM-dayly.pdf',
    graph.width = 10, 
    graph.height = 6
){
  
  Number.cumsum <- 
    df.num[["df"]] %>%
    mutate_at(.vars = vars(str_sub(choice, start = 2, end = -2)), .funs = ~ cumsum(.)) %>% 
    rename(date = names(df.num[["df"]])[1])
  date.X_date <- tibble(date = X_date)
  df.num <- 
    df.num[["df"]] %>% 
    rename(date = names(df.num[["df"]])[1])
  df.EM <- 
    df.EM %>% 
    rename(date = names(df.EM)[1])
  EM.rate_by_daily <- 
    date.X_date %>%
    left_join(df.EM) %>% 
    left_join(df.num, by=c("date")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(across(.cols = str_sub(choice, start = 2, end = -2), cumsum)) 
  
  column_order <- 
    names(EM.rate_by_daily) %>% 
    sort()
  
  EM.rate_by_daily <- 
    EM.rate_by_daily %>% 
    select(all_of(column_order)) %>% 
    select(date, everything())
  
  for (i in 1:length(choice2)){
    EM.rate_by_daily <- 
      EM.rate_by_daily %>% 
      mutate(
        !!choice3[i] := eval(parse(text = choice2[i]))/eval(parse(text = choice[i]))*100
      )
  }
  
  # ピボット変換
  EM.rate_by_daily_pivot <- 
    pivot_longer(data = EM.rate_by_daily, cols = ends_with("EM")) 
  
  # 列要素
  EM.rate_by_daily_pivot %>% 
    distinct(name)
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # 折れ線グラフ
  Att.labs <- choice
  names(Att.labs) <- choice2
  g_EM <- 
    ggplot(data = EM.rate_by_daily_pivot, 
           mapping = aes(x=date,y=value,colour=name)) +
    geom_line(linewidth = 0.8) +
    theme_bw() + 
    theme(text = element_text(size = 16), title = element_text(size = 14)) +
    labs(x = "年月日",y = "EM件数", title = graph_title) +
    scale_x_date(breaks = x.breaks,labels=date_format("%Y/%m")) +
    scale_y_continuous(breaks = y.breaks) +
    coord_cartesian(ylim = ylim) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 16)) +
    facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs))
  plot(g_EM)
  
  # グラフ保存
  if (graph.save == TRUE){
    ggsave(file=save.graph_path, device=cairo_pdf, plot=g_EM, dpi=300, w=graph.width, h=graph.height)
  }
  return(EM.rate_by_daily)
}  
