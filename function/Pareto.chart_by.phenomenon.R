Pareto.chart_by.phenomenon <- function(
    df = Metis_MF3_2211,            # 保守データ
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",    # 処置部
    P_name = "VOLGA-E",             # 周辺機名
    Choices = "Manufacturing_date", # グルーピング条件 
    num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
    Number_by_Manufacturing_date = Metis_MF3_FIN_number_by_Manufacturing_date, # 製造月別台数
    # E_num = "VOLGA_E_num",      # 製造月別台数の選択列名
    y1.lim = c(0,400),          # グラフ第1軸の範囲
    breaks = seq(0, 400, 100),　# グラフ第1軸の目盛り
    graph_save = FALSE,         # グラフの保存
    graph_title = "Metis-MF3 VOLGA-E EM現象別件数",
    graph_path = "./PDF/Metis-MF3_VOLGA_EM現象別件数パレート図.pdf",
    graph_width = 14, # グラフの横幅
    graph_height = 6, # グラフの高さ
    hjust = 10        # 棒グラフ上部記載の数値高さ
    ){
  # 現象別のEM件数パレート図を描画する
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # リストのベクトル化
  T_location <- unlist(T_location)
  print(T_location)
  
  E_num <- str_c("`",P_name,"`")
  # EM件数
  SCname_Manufacturing_date <- 
    Create_Metis_MF3_SCname(
      df = df,
      T_location = T_location, 
      P_name = P_name,
      Choices = Choices,
      num = num
    )
  
  # EM総数
  EM_number <- sum(SCname_Manufacturing_date$Peripheral_name_SC_sum)
  print(paste0("現象別EM総数：",EM_number))
  print(n=100,SCname_Manufacturing_date %>% distinct(SC_name_num))
  
  # 結合
  SCname_Manufacturing_date <- 
    SCname_Manufacturing_date %>% 
    left_join(Number_by_Manufacturing_date$df, by="Manufacturing_date")
  
  print(SCname_Manufacturing_date)
  
  # 現象件数を台数比率へ換算した列追加
  SCname_Manufacturing_date <- 
    SCname_Manufacturing_date %>% 
    mutate(
      SC_sum_rate = Peripheral_name_SC_sum/
        (eval(parse(text = paste0("SCname_Manufacturing_date$",E_num))))
    ) 
  
  # SC現象別件数
  SC_count_by_Peripheral <- 
    SCname_Manufacturing_date %>% 
    group_by(SC_name_num) %>% 
    summarise(
      SC_count = sum(Peripheral_name_SC_sum)
    ) %>%
    arrange(-SC_count) %>% 
    ungroup() %>% 
    mutate(
      rate = SC_count/sum(SC_count),
      cum = cumsum(SC_count), 
      cumrate = cum/sum(SC_count),
      SC_name = forcats::as_factor(SC_name_num)
    )
  print(paste("EM総数",sum(SC_count_by_Peripheral$SC_count))) # 1283
  print(SC_count_by_Peripheral)
  
  # 変数のスケーラ
  variable_scaler <- function(p, lim1, lim2){
    to_zero <- p-lim2[1]
    y1_range <- lim1[2]-lim1[1]
    y2_range <- lim2[2]-lim2[1]
    scaled <- to_zero*y1_range/y2_range
    from_zero <- scaled + lim1[1]
    return(from_zero)
  }
  # 第2軸の目盛りのスケーラ
  axis_scaler <- function(p, lim1, lim2){
    to_zero <- p-lim1[1]
    y1_range <- lim1[2]-lim1[1]
    y2_range <- lim2[2]-lim2[1]
    scaled <- to_zero*y2_range/y1_range
    from_zero <- scaled + lim2[1]
    return(from_zero)
  }
  breaks <- unlist(breaks)
  y1.lim <- unlist(y1.lim)
  y2.lim <- c(0,1)
  g_plate <- 
    ggplot(SC_count_by_Peripheral, aes(reorder(x = SC_name, X = -SC_count))) + 
    scale_y_continuous(limit=y1.lim,  # 第1軸の範囲
                       breaks=breaks,  # 第1軸の目盛り
                       sec.axis=sec_axis(
                         ~(axis_scaler(., y1.lim, y2.lim)), # 軸スケーリング
                         breaks=seq(0, 1, 0.2), # 第2軸の目盛り
                         name="累積比（折れ線）")  # y2のラベルはここで設定する
    ) +
    geom_bar(aes(y = SC_count, fill=SC_name), stat = "identity") +
    labs(x = "現象名称", y = "EM件数", title = str_c(graph_title,"　EM総数:",EM_number,"件")) +
    theme_bw() +
    theme(title = element_text(size = 12), text = element_text(size = 12)) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
    geom_path(aes(y = variable_scaler(cumrate, y1.lim, y2.lim), group = 0), colour="blue") + 
    geom_point(aes(y = variable_scaler(cumrate, y1.lim, y2.lim)), colour="blue")+
    geom_text(aes(SC_name, SC_count+hjust, label = round(SC_count,2)),color="black", size = 3)
  plot(g_plate)
  # グラフ保存
  if (graph_save == TRUE){
    print(graph_width)
    ggsave(graph_path, plot = g_plate, device = cairo_pdf, dpi=300, width=graph_width, height=graph_height)
  }
  return(g_plate)
}
