Create.histogram_for.EM.count <- function(
    df_EM = Metis_MF3_2211,
    df_machine = Metis_MIF_2211,
    treatment.area = c("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
    choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
    col.name = "Machine_numbers",
    graph_save = FALSE,
    y.breaks = NA,
    x.breaks = NA,
    graph.width = 8,
    graph.height = 8,
    file_path = './PDF/Metis-MF3_Finisher_EMcount.pdf',
    grapf_title = "Metis-MF3 Finisher EM回数(2019.02～2022.11)",
    hjust = 1000
){
  # QISSのEM数,機器データからEM回数ヒストグラムを描く
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # リストのベクトル化
  choice <- unlist(choice)
  y.breaks <- unlist(y.breaks)
  x.breaks <- unlist(x.breaks)
  #EM数カウント
  EM_by.col.name <- 
  Create.file_for.EM_number(
    df = df_EM,
    field = treatment.area,
    choice = choice,
    col.name = col.name,
    file.save = FALSE)
  
  # One-hot化
  Metis_by.One.hot <-
    One_hot_Peripheral_machine.2(df=df_machine)
  
  # 結合
  Metis_by.One.hot <- 
    Metis_by.One.hot %>% 
    left_join(EM_by.col.name, by=col.name)
  
  col_EM <- 
    names(select(Metis_by.One.hot, c(ends_with(".EM")))) %>% sort()
  
  # EM発生無し機の情報を含めてのEM回数
  Metis_num.ENCount <<- Metis_by.One.hot
  for (colum in col_EM) {
    Metis_num.ENCount <- 
      Metis_num.ENCount %>% 
        mutate(
          across(.cols = c(all_of(colum)),
                 .fns = ~{case_when(is.na(.) & eval(parse(text = paste0("`",str_sub(colum, start=1, end=-4),"`"))) == 1 ~ 0,
                                    !is.na(.)& eval(parse(text = paste0("`",str_sub(colum, start=1, end=-4),"`"))) == 1 ~ as.double(.),
                                    TRUE ~ NaN)},
                 .names = "{col}.Count")
        )
  }
  print(names(Metis_num.ENCount))
  # Metis_num.ENCount <- 
  #   Metis_num.ENCount  %>% 
  #   select(Machine_numbers, `VOLGA-E.EM.Count`, `AMUR-C中綴じ.EM.Count`, `AMUR-C(HY).EM.Count`,
  #          `VOLGA-E`, `AMUR-C(HY)`, `AMUR-C中綴じ`)
  
  # na.omit(Metis_MF3_Finisher_ENCount)
  
  print(paste0("Metis-MF3 本体台数:",nrow(Metis_num.ENCount)))   # 159997
  for (colum in col_EM){
    print(paste0(str_sub(colum, start=1, end=-4)," 台数:",
                 sum(eval(parse(text = str_c("Metis_num.ENCount$`",str_sub(colum, start=1, end=-4),"`"))))))
    print(paste0(str_sub(colum, start=1, end=-4)," 最大EM件数:",
                 max(eval(parse(text = str_c("Metis_num.ENCount$`",colum,".Count`"))), na.rm = TRUE)))
  }
  # print(paste0("VOLGA_E 台数:",sum(Metis_num.ENCount$`VOLGA-E`)))  # 18393
  # print(paste0("AMUR_C_中綴じ 台数:",sum(Metis_num.ENCount$`AMUR-C中綴じ`))) # 8854
  # print(paste0("AMUR_C(HY) 台数:",sum(Metis_num.ENCount$`AMUR-C(HY)`)))        # 2456
  # 
  # print(paste0("VOLGA_E 最大EM件数:",max(Metis_num.ENCount$`VOLGA-E.EM.Count`, na.rm = TRUE)))      # 7
  # print(paste0("AMUR_C_中綴じ 最大EM件数:",max(Metis_num.ENCount$`AMUR-C中綴じ.EM.Count`, na.rm = TRUE))) # 9
  # print(paste0("AMUR_C(HY) 最大EM件数:",max(Metis_num.ENCount$`AMUR-C(HY).EM.Count`, na.rm = TRUE)))  # 4
  
  bins <- 0
  for (colum in col_EM){
    bin <- max(eval(parse(text = str_c("Metis_num.ENCount$`",colum,".Count`"))), na.rm = TRUE)
    if (bin > bins){
      bins <- bin
    }
  }
  bins <- bins + 1
  print(str_c("bins:",bins))
  
  # bins <- max(max(Metis_num.ENCount$VOLGA_E.EM.Count, na.rm = TRUE),
  #             max(Metis_num.ENCount$AMUR_C.EM.Count, na.rm = TRUE),
  #             max(Metis_num.ENCount$AMUR_CHY.EM.Count, na.rm = TRUE)) +1
  # ピボット変換
  Metis_num.ENCount.pivot <<- 
    pivot_longer(data = Metis_num.ENCount, cols = ends_with("Count")) %>% 
    subset(!(is.nan(value)))
  # nが最大のvalueの値を抽出
  Count.value <- 
    Metis_num.ENCount.pivot %>% 
    group_by(value) %>% 
    summarise(n = n())
  value.nmax <- 
    Count.value[Count.value$n == max(Count.value$n),]$value
  # hjustの設定
  Count.name <- 
    Metis_num.ENCount.pivot %>% 
    filter(value == value.nmax) %>% 
      group_by(name) %>% 
      summarise(count = n())
  hjust <- max(Count.name$count)/15*length(choice)/(3-0.5)
  print(str_c("hjust:",hjust))
  # 末尾が.Countの列名をベクトルとして抽出
  col_Count <- 
    names(select(Metis_num.ENCount, c(ends_with(".Count")))) %>% sort()

  # ヒストグラム
  Att.labs <- choice %>% sort()
  names(Att.labs) <- col_Count
  g_EMcount_hist <- 
    ggplot(data = Metis_num.ENCount.pivot,
           mapping = aes(x = value, fill = name)) +
    geom_histogram(bins = bins, colour = "black",
                   position = position_dodge(), alpha = 0.8) +
    geom_text(aes(y = after_stat(count) + hjust, label = after_stat(count)), stat = "bin", binwidth = 1, size = 3) +
    facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs)) +
    labs(x = "EM回数", y = "count", fill = "機種", title = grapf_title) +
    theme_bw() + 
    theme(text = element_text(size = 10*graph.width/8+3)) +
    theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
    guides(fill = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 12)) 
  if(!anyNA(y.breaks)){
    g_EMcount_hist <- g_EMcount_hist + scale_y_continuous(breaks = y.breaks, 
                                                          # 単位k(1000)をつけるため値自体は1/1000する
                                                          labels = label_number(scale = 1/1000, suffix = 'k'))
  }
  if(!anyNA(x.breaks)){
    g_EMcount_hist <- g_EMcount_hist + scale_x_continuous(breaks = x.breaks)
  }
  plot(g_EMcount_hist)
  
  if(graph_save == TRUE){
    # グラフ保存
    file_path=file_path
    ggsave(file=file_path, device=cairo_pdf, plot=g_EMcount_hist, dpi=300, w=graph.width, h=graph.height)
  }
  return(g_EMcount_hist)
}

# Create.histogram_for.EM.count()
