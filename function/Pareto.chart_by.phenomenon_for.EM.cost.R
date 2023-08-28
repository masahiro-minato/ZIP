Pareto.chart_by.phenomenon_for.EM.cost <- function(
    df = Metis_MF3_2211,            # 保守データ
    T_location = c("ADF部"),        # 処置部
    P_name = c("COOK-C"),           # 周辺機名
    breaks = seq(0, 6000, 1000),　  # グラフ第1軸の目盛り
    y1.lim = c(0,6000),             # グラフ第1軸の範囲
    file.save = FALSE,              # ファイルの保存
    file_path = "./tsv_data/Metis_MF3_COOK_CECOST.tsv",
    graph.save = FALSE,             # グラフの保存
    graph.title = "Metis-MF3 COOK-C EMでのCE訪問費用(2019.2～2022.11)",
    graph.path = "./PDF/Metis-MF3_COOK-C_CE費用パレート図.pdf",
    graph.width = 14, # グラフの横幅
    graph.height = 6, # グラフの高さ
    hjust = 80        # 棒グラフ上部記載の数値高さ
){
  # 保守データからEM件数データの抽出を行う
  # df:保守データのデータフレーム
  # T_location:処置部 ex. c("ADF部")
  # P_name:テーマ名　ex. c("COOK-C")
  # breaks:グラフ第1軸の目盛り ex. seq(0, 6000, 1000)
  # y1.lim:グラフ第1軸の範囲 ex. c(0,6000)
  # file.save:ファイル保存の有無
  # file_path:ファイル保存時のパス
  # graph.save:グラフの保存
  # graph.title:グラフタイトル
  # graph.path:グラフ保存時のパス
  # graph.width:グラフの横幅
  # graph.height:グラフの高さ 
  # hjus:棒グラフ上部記載の数値高さ
  # 戻り値:ファイル
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # リストのベクトル化
  breaks <- unlist(breaks)
  y1.lim <- unlist(y1.lim)
  hjust <- unlist(hjust)
  # CE訪問費用
  CECOST <- 
    df %>% 
    filter(Treatment_location %in% T_location) %>% 
    filter(Peripheral_name %in% P_name) %>% 
    mutate(
      CEcost = case_when(
        CE_Working_hours <= 30 ~ 8000 + 4000,
        CE_Working_hours > 30 ~ 8000 + 4000 + (CE_Working_hours-30)/10*(4000/3),
        TRUE ~ 0
      )
    ) %>% 
    group_by(SC_name, Peripheral_name) %>%
    summarise(
      # EM_count = n(),
      CE_cost = round(sum(CEcost)/10000,0),
      .groups = "drop"
    ) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    arrange(-CE_cost) %>% 
    mutate(
      rate = CE_cost/sum(CE_cost),
      cum = cumsum(CE_cost), 
      cumrate = cum/sum(CE_cost),
      SC_name = forcats::as_factor(SC_name)
    ) %>% 
    ungroup()
  T_cost <- paste0("総額:",sum(CECOST$CE_cost),"万円")
  print(paste0(P_name," CE訪問費用総額:",sum(CECOST$CE_cost),"万円"))
  
  if (file.save == TRUE){
    write_tsv(CECOST, file_path)
  }
  
  # パレート図
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
  
  y1.lim <- unlist(y1.lim)
  y2.lim <- c(0,1)
  g_Pareto.chart.CECOST <- 
    ggplot(CECOST, aes(reorder(x = SC_name, X = -CE_cost))) + 
    scale_y_continuous(limit=y1.lim,   # 第1軸の範囲
                       breaks=breaks,  # 第1軸の目盛り
                       sec.axis=sec_axis(
                         ~(axis_scaler(., y1.lim, y2.lim)), # 軸スケーリング
                         breaks=seq(0, 1, 0.2),    # 第2軸の目盛り
                         name="累積比（折れ線）")  # y2のラベルはここで設定する
    ) +
    geom_bar(aes(y = CE_cost, fill=SC_name), stat = "identity") +
    labs(x = "現象名称", y = "CE訪問費用(万円)", title = paste(graph.title, T_cost,sep = " ")) +
    theme_bw() +
    theme(title = element_text(size = 12), text = element_text(size = 12)) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)) +
    geom_path(aes(y = variable_scaler(cumrate, y1.lim, y2.lim), group = 0), colour="blue") + 
    geom_point(aes(y = variable_scaler(cumrate, y1.lim, y2.lim)), colour="blue") +
    geom_text(aes(SC_name, CE_cost+hjust, label = round(CE_cost,2)),color="black", size = 2.5)
  plot(g_Pareto.chart.CECOST)
  # グラフ保存
  if (graph.save == TRUE){
    graph.width <- unlist(graph.width)
    graph.height <- unlist(graph.height)
    ggsave(graph.path, plot=g_Pareto.chart.CECOST, device=cairo_pdf, dpi=300, width=graph.width, height=graph.height)
  }
  
  return(g_Pareto.chart.CECOST)
}


# Pareto.chart_by.phenomenon_for.EM.cost()
