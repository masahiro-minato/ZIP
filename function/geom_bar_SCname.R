geom_bar_SCname <- function(
  df, x, y, fill, legend_name, x_labels, y_labels, title, file_path,
  y.sum = NA, #"Peripheral_name_EM",
  x.breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),
  labels = date_format("%Y/%m"),
  y.breaks = NA, 
  ylim = NA,
  f_wrap=TRUE, 
  ncol=3, 
  g_save=TRUE, 
  w=14, 
  h=8,
  legend_position = "right", 
  legend_justification = "center",
  legend_ncol = 2
  ){
  # 現象SC数を棒グラフにて表示する
  
  # df:データフレーム名 文字列ではない
  # x~file_pathまでの引数は文字列にて指定
  # x:横軸のデータ列名
  # y:縦軸のデータ列名
  # fill:色分け対象のデータ列名
  # legend_name:凡例に表示する名称
  # x_labels:X軸のラベル
  # y_labels:y軸のラベル
  # title:グラフタイトル
  # file_path:保存先パスを含んだグラフ名称
  # f_wrap:現象毎の個別グラフの作成有無
  # ncol:個別グラフの列数
  # g_saveはTRUEにて保存するが、それ以外は保存しない
  # w:PDF保存時の横幅
  # h:PDF保存時の高さ
  # legend_position:凡例表示位置の横位置
  # legend_justification:凡例表示位置の縦位置
  # legend_ncol:凡例の列数
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # 注意メッセージ
  if (is.character(df)){
    print("dfはtibble型で指示すること")
  }
  # リストのベクトル化
  legend_position <-  unlist(legend_position)
  legend_justification <- unlist(legend_justification)
  h <- unlist(h)
  w <- unlist(w)
  # EM総数
  if (y == "Peripheral_name_SC_sum"){
    EM.num <- sum(eval(parse(text = paste("df$",y,sep=""))))
    print(str_c("EM総数:",EM.num,"件"))
    title <- paste0(title, " ", str_c("EM総数:",EM.num,"件"))
  }
  # グラフ描画
  print(df)
  if (f_wrap == TRUE){
    x.size <- 10
    y.size <- 12
  }else{
    x.size <- 16
    y.size <- 16
  }
    
  g <- 
    ggplot(df,
           aes(x = eval(parse(text = paste("df$",x,sep=""))), 
               y = eval(parse(text = paste("df$",y,sep=""))), 
               fill = eval(parse(text = paste("df$",fill,sep=""))))) +
    # ggplot(eval(parse(text = df)),
           # aes(x = eval(parse(text = paste(df,"$",x,sep=""))), 
           #     y = eval(parse(text = paste(df,"$",y,sep=""))), 
           #     fill = eval(parse(text = paste(df,"$",fill,sep=""))))) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(fill = legend_name, x = x_labels, y = y_labels, title = title) +
    scale_color_hue(name = legend_name) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(size = x.size),
      axis.text.y = element_text(size = y.size),
      legend.position = legend_position, 
      legend.justification = legend_justification,
      legend.background = element_rect(fill = NA, linewidth = 0.2),
      legend.title = element_blank(),
      legend.text = element_text(size = 11)) +
    guides(fill=guide_legend(ncol=legend_ncol, #.凡例の列数、行数を変える
                             reverse=FALSE)) #.凡例の順番を変える
  if (!anyNA(x.breaks)){
    g <- 
      g + scale_x_date(breaks = x.breaks,labels=labels)
  }
  if (!anyNA(y.breaks)){
    g <- g + scale_y_continuous(breaks = y.breaks) + coord_cartesian(ylim = ylim)
  }
  if(f_wrap == FALSE && !anyNA(y.sum)){
    hjust <- max(eval(parse(text = paste0("df$",y.sum)))) * 0.01
    g <- g + geom_text(aes(eval(parse(text = paste0("df$",x))), 
                           eval(parse(text = paste0("df$",y.sum))) + hjust, 
                           label = round(eval(parse(text = paste0("df$",y.sum))),2)),color="black", size = 4)
  }
  
  # 現象別描画
  if(f_wrap == TRUE){
    num.phenomenon <- length(unique(eval(parse(text = paste0("df$",fill)))))
    h <- ceiling(num.phenomenon/ncol)*w/18*1.5 + 1.0*w/18
    print(str_c("現象数:",num.phenomenon," h:",h))
    g <- g + facet_wrap(~eval(parse(text = paste("df$",fill,sep=""))), ncol = ncol) +
      guides(fill = "none") +
      # strip.placementは "inside" (初期値) または "outside"が指定可能
      theme(strip.background = element_rect(fill="ivory"),
            # strip.background = element_blank(), 
            strip.text = element_text(size = 12)) 
  }
  plot(g)
  
  # グラフ保存
  if(g_save == TRUE){
    file_path = file_path
    ggsave(file=file_path, device=cairo_pdf, plot=g, dpi=300, w=w, h=h)
  }
  
  # グラフを返す
  return(g)
}

