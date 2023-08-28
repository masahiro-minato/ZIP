Create.graph_for.number_by.month <- function(
  df = Metis_MIF_2211,
  field = "ADF",
  choice = c("COOK-C","SINAI-H"),
  sel_month = "納入月",
  date.name = "month",
  file.save = FALSE,
  write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_month-2.tsv",
  graph_title = "Metis-MF3 ADF 時系列納品台数",
  legend.position = c(0.85, 0.95),
  breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
  y.breaks = NA,
  graph.save = FALSE,
  save.graph_path='./PDF/Metis-MF3_ADF_時系列台数.pdf',
  graph.width = 10, 
  graph.height = 6
  ){
  # 機器データから納入月別、製造月別、稼働月別などの台数データの抽出とグラフ化を行う
  # df:機器データのデータフレーム
  # field:分野
  # choice:テーマ名をベクトルで表示　ex. c("COOK-C","SINAI-H")
  # sel_month:納入月、製造年月を選択
  # date.name:作成されるファイルのdate列の列名
  # file.save:ファイル保存の有無
  # write.file_path:ファイル保存時のパス
  # graph_title:グラフのタイトル
  # legend.position:凡例の表示位置　ex. c(0.85, 0.95)
  # breaks:グラフ横軸の目盛　ex. seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month")
  # y.breaks:縦軸の目盛り指定　NAだと自動設定　ex. seq(0,7000,1000)
  # graph.save:グラフ保存の有無
  # save.graph_path:グラフ保存時のパス
  # graph.width:グラフの幅
  # graph.height:グラフの高さ
  # 戻り値:Return.value リスト型でデータフレームとグラフ　名前は"df"と"graph"
  
  # リストのベクトル化
  legend.position <- unlist(legend.position)
  y.breaks <- unlist(y.breaks)
  # 台数
  number_pivot <- 
    df %>% 
    filter(eval(parse(text = field)) %in% choice) %>%
    select(all_of(sel_month), all_of(field), 機種機番) %>% 
    group_by(eval(parse(text = sel_month)), eval(parse(text = field))) %>%
    summarise(台数 = n(), .groups = "drop") %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  number_by_month <- 
    number_pivot %>% 
    pivot_wider(
      names_from = `eval(parse(text = field))`,
      values_from = 台数
    )%>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    rename(!!date.name := `eval(parse(text = sel_month))`)
    
  # print(number_by_month)
  
  if (file.save == TRUE){
    write_tsv(number_by_month, write.file_path)
  }
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # library(scales)
  
  # 折れ線グラフ
  g <- 
    ggplot(data = number_pivot, 
           mapping = aes(x=`eval(parse(text = sel_month))`,y=台数,colour=`eval(parse(text = field))`)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.0) +
    theme_bw() + 
    theme(text = element_text(size = 20), title = element_text(size = 16)) +
    theme(legend.position = legend.position, legend.justification = c(0, 1)) +
    labs(x = "年月日",y = "台数", title = graph_title) +
    scale_x_date(breaks = breaks,labels=date_format("%Y/%m")) +
    scale_color_hue(name = "機種")
  if(!anyNA(y.breaks)){
    g <- g + scale_y_continuous(breaks = y.breaks,
                                # 1000桁毎にカンマ
                                labels = label_comma())
  }else{
    g <- g + scale_y_continuous(labels = label_comma())
  }
    # coord_cartesian(ylim = c(0, 7000))
  
  # plot(g)
  # グラフ保存
  if (graph.save == TRUE){
    ggsave(file=save.graph_path, device=cairo_pdf, plot=g, dpi=300, w=graph.width, h=graph.height)
  }
  
  Return.value <- list(number_by_month, g)
  names(Return.value) <- c("df","graph")
  print(Return.value[["df"]])
  plot(Return.value[["graph"]])
  return(Return.value)
}
