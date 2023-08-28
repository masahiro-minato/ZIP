Create.graph_for.EM.rate_number <- function(
  df.mt,
  df.num,
  field = c("ADF部"),
  choice.mt = c("COOK-C","SINAI-H"),
  EM.rate = "EM.rate",
  X_date = seq(as.Date("2019-01-01"), as.Date("2022-11-30"), by = "day"),
  file.save = FALSE,
  write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_month-2.tsv",
  graph_title = "Metis-MF3_ADF EM率",
  legend.position = c(0.85, 0.95),
  x.breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),
  y.breaks = NA,
  ylim = c(0, 0.2),
  graph.save = FALSE,
  save.graph_path = './PDF/Metis-MF3_ADF_EM.rate-dayly.pdf',
  graph.width = 10, 
  graph.height = 6
){
  ### EMデータからEM件数・EM率の時系列グラフ化を行う
  # df.mt:保守データのデータフレーム
  # df.num:納品日データのデータフレーム 尚、1列目にdateデータが必要
  # field:分野の選択　ex. c("ADF部") or c("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部")など
  # choice.mt:テーマ名をベクトルで表示　ex. c("COOK-C","SINAI-H")
  # EM.rate:EM件数orEM率の選択　ex. "EM" or "EM.rate"
  # file.save:ファイル保存の有無
  # write.file_path:ファイル保存時のパス
  # graph_title:グラフのタイトル
  # legend.position:凡例の表示位置　ex. c(0.85, 0.95)
  # x.breaks:グラフ横軸の目盛 NA指定すると自動設定　ex. seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month")
  # y.breaks:グラフ縦軸の目盛 NA指定すると自動設定　ex. seq(0, 0.2, 0.05)
  # ylim:y軸の表示範囲　ex. c(0, 0.2)
  # graph.save:グラフ保存の有無
  # save.graph_path:グラフ保存時のパス
  # graph.width:グラフの幅
  # graph.height:グラフの高さ
  # 戻り値:ファイル
  
  # リストをベクトルへ変換
  y.breaks <- unlist(y.breaks)
  ylim <- unlist(ylim)
  print(y.breaks)
  print(ylim)
  # EMデータ
  df.EM <- 
    Create.file_for.EM_number(
      df = df.mt,
      field = field,
      choice = choice.mt,
      file.save = FALSE
    )
  
  print(unlist(choice.mt))
  # choice.EM,choice.rateの作成
  choice.EM <- 
    choice.mt %>% 
    # str_sub(start=2, end=-2) %>% 
    paste(c(".EM"), sep="")
  choice.EM.name <- choice.EM
  choice.rate <- paste(choice.EM, c(".rate"), sep="")
  choice <-
    str_c("`",choice.mt,"`")
  choice.EM <-
    str_c("`",choice.EM,"`")
  print("①-----")
  date.X_date <- tibble(date = X_date)
  # print(df.num)
  print(names(df.num[["df"]]))
  df.num[["df"]] <- 
    df.num[["df"]] %>% 
    rename(date = names(df.num[["df"]])[1])
  print("②----")
  df.EM <- 
    df.EM %>% 
    rename(date = names(df.EM)[1])
  print("③---")
  EM.rate_by_daily <- 
    date.X_date %>%
    left_join(df.EM) %>% 
    left_join(df.num[["df"]], by=c("date")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(across(.cols = str_sub(choice, start = 2, end = -2), cumsum)) 
    
  column_order <- 
    names(EM.rate_by_daily) %>% 
    sort()
  
  EM.rate_by_daily <- 
    EM.rate_by_daily %>% 
    select(all_of(column_order)) %>% 
    select(date, everything())
  
  for (i in 1:length(choice.EM)){
    EM.rate_by_daily <- 
      EM.rate_by_daily %>% 
      mutate(
        !!choice.rate[i] := eval(parse(text = choice.EM[i]))/eval(parse(text = choice[i]))*100
      )
  }
  
  # ピボット変換
  EM.rate_by_daily_pivot <- 
    pivot_longer(data = EM.rate_by_daily, cols = ends_with(EM.rate)) 
  
  # 列要素
  EM.rate_by_daily_pivot %>% 
    distinct(name)
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # 折れ線グラフ
  # print(unlist(choice.mt))
  # print(choice.EM)
  # print(choice.rate)
  Att.labs <- unlist(choice.mt)
  if (EM.rate == "EM.rate"){
    names(Att.labs) <- choice.rate
    y.name = "EM率（％）"
  }else if(EM.rate == "EM"){
    names(Att.labs) <- choice.EM.name
    y.name = "EM件数"
  }
  
  g_EM <- 
    ggplot(data = EM.rate_by_daily_pivot, 
           mapping = aes(x=date,y=value,colour=name)) +
    geom_line(linewidth = 0.8) +
    theme_bw() + 
    theme(text = element_text(size = 16), title = element_text(size = 14)) +
    labs(x = "年月日",y = y.name, title = graph_title) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 16)) +
    facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs))
  if (!anyNA(x.breaks)){
    g_EM <- 
      g_EM + scale_x_date(breaks = x.breaks,labels=date_format("%Y/%m"))
  }
  if (!anyNA(y.breaks)){
    g_EM <-
      g_EM + scale_y_continuous(breaks = y.breaks) + coord_cartesian(ylim = ylim)
  }
  plot(g_EM)
  
  # グラフ保存
  if (graph.save == TRUE){
    ggsave(file=save.graph_path, device=cairo_pdf, plot=g_EM, dpi=300, w=graph.width, h=graph.height)
  }
  return(g_EM)
}

