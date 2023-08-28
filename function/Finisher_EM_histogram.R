Finisher_EM_histogram <- function(
  df_EM = Metis_MF3_2211,
  df_machine =Metis_MIF_2211,
  # df_EM = Metis_MF3_distinct,
  # df_machine =Metis_sheet3,
  graph_save = FALSE,
  y.breaks = seq(0, 16000, 2000),
  x.breaks = seq(0, 10, 1),
  file_path = './PDF/Metis-MF3_Finisher_EMcount.pdf',
  grapf_title = "Metis-MF3 Finisher EM回数(2019.02～2022.02)"
  ){
  # QISSのEM数,機器データからFinisherのEM回数ヒストグラムを描く
  
  # FinisherのEM数カウント
  Metis_MF3_EM_Finisher_by_Machine_numbers <- 
    Metis_MF3_Finisher_EMcount(df=df_EM)
  
  # One-hot化
  Metis_MF3_by_model <-
    One_hot_Peripheral_machine(df=df_machine)
  
  # 結合
  Metis_MF3_by_model <- 
    Metis_MF3_by_model %>% 
    left_join(Metis_MF3_EM_Finisher_by_Machine_numbers, by="Machine_numbers")
  
  # EM発生無し機の情報を含めてのEM回数
  Metis_MF3_Finisher_ENCount <<- 
    Metis_MF3_by_model %>% 
    mutate(
      VOLGA_E_EM_Count = case_when(is.na(VOLGA_E_EM) & VOLGA_E == 1 ~ 0, 
                                   !is.na(VOLGA_E_EM)& VOLGA_E == 1 ~ as.double(VOLGA_E_EM),
                                   TRUE ~ NaN),
      AMUR_C_EM_Count = case_when(is.na(AMUR_C_EM) & AMUR_C_saddle == 1 ~ 0, 
                                  !is.na(AMUR_C_EM)& AMUR_C_saddle == 1 ~ as.double(AMUR_C_EM),
                                  TRUE ~ NaN),
      AMUR_CHY_EM_Count = case_when(is.na(AMUR_CHY_EM) & AMUR_C_HY == 1 ~ 0, 
                                    !is.na(AMUR_CHY_EM)& AMUR_C_HY == 1 ~ as.double(AMUR_CHY_EM),
                                    TRUE ~ NaN)
    ) %>% 
    select(Machine_numbers, VOLGA_E_EM_Count, AMUR_C_EM_Count, AMUR_CHY_EM_Count, Finisher,
           VOLGA_E, AMUR_C_HY, AMUR_C_saddle)
  
  # na.omit(Metis_MF3_Finisher_ENCount)
  
  print(paste0("Metis-MF3 本体台数:",nrow(Metis_MF3_Finisher_ENCount)))   # 130420 160048
  print(paste0("VOLGA_E 台数:",sum(Metis_MF3_Finisher_ENCount$VOLGA_E)))  # 15251 18406
  print(paste0("AMUR_C_中綴じ 台数:",sum(Metis_MF3_Finisher_ENCount$AMUR_C_saddle))) # 7317 8876
  print(paste0("AMUR_C(HY) 台数:",sum(Metis_MF3_Finisher_ENCount$AMUR_C_HY)))        # 2017 2472
  
  print(paste0("VOLGA_E 最大EM件数:",max(Metis_MF3_Finisher_ENCount$VOLGA_E_EM_Count,na.rm = TRUE)))      # 7
  print(paste0("AMUR_C_中綴じ 最大EM件数:",max(Metis_MF3_Finisher_ENCount$AMUR_C_EM_Count,na.rm = TRUE))) # 9
  print(paste0("AMUR_C(HY) 最大EM件数:",max(Metis_MF3_Finisher_ENCount$AMUR_CHY_EM_Count,na.rm = TRUE)))  # 4
  
  bins <- max(max(Metis_MF3_Finisher_ENCount$VOLGA_E_EM_Count,na.rm = TRUE),
              max(Metis_MF3_Finisher_ENCount$AMUR_C_EM_Count,na.rm = TRUE),
              max(Metis_MF3_Finisher_ENCount$AMUR_CHY_EM_Count,na.rm = TRUE)) +1
  # ピボット変換
  Metis_MF3_Finisher_ENCount_pivot <- 
    pivot_longer(data = Metis_MF3_Finisher_ENCount, cols = ends_with("Count")) %>% 
    subset(!(is.nan(value)))
  
  # フォント設定
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  
  # ヒストグラム
  Att.labs <- c("VOLGA-E", "AMUR-C中綴じ", "AMUR-C(HY)")
  names(Att.labs) <- c("VOLGA_E_EM_Count", "AMUR_C_EM_Count", "AMUR_CHY_EM_Count")
  g_EMcount_hist <- 
    ggplot(data = Metis_MF3_Finisher_ENCount_pivot,
           mapping = aes(x = value, fill = name)) +
    geom_histogram(bins = bins, colour = "black",
                   position = position_dodge(), alpha = 0.8) +
    geom_text(aes(y = after_stat(count) + 1000, label = after_stat(count)), stat = "bin", binwidth = 1, size = 5) +
    # facet_wrap(~name, ncol = 1) +
    facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs)) +
    labs(x = "EM回数", y = "count", fill = "機種", title = grapf_title) +
    # scale_fill_hue(labels = c(VOLGA_E="VOLGA-E",AMUR_CHY="AMUR-C(HY)",AMUR_C="AMUR-C")) +
    theme_bw() + 
    theme(text = element_text(size = 14)) +
    scale_y_continuous(breaks = y.breaks, labels = label_comma()) +
    scale_x_continuous(breaks = x.breaks) +
    theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
    guides(fill = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 15)) 
  
  if(graph_save == TRUE){
    # グラフ保存
    file_path=file_path
    ggsave(file=file_path, device=cairo_pdf, plot=g_EMcount_hist, dpi=300, w=8, h=8)
  }
  return(g_EMcount_hist)
}
