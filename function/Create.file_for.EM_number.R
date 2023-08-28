Create.file_for.EM_number <- function(
    df = Metis_MF3_2211,
    field = c("ADF部"),
    choice = c("COOK-C","SINAI-H"),
    col.name = "Maintenance_date",
    file.save = FALSE,
    write.file_path = "./tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv"
){
  # 保守データからEM件数データの抽出を行う
  # df:保守データのデータフレーム
  # field:分野
  # choice:テーマ名をベクトルで表示　ex. c("COOK-C","SINAI-H")
  # col.name:グループ化の選択列　　ex. "Maintenance_date"
  # file.save:ファイル保存の有無
  # write.file_path:ファイル保存時のパス
  # 戻り値:ファイル
  
  # EM件数
  EM.number_pivot <- 
    df %>% 
    filter(Treatment_location %in% field) %>%
    filter(Peripheral_name %in% choice) %>%
    group_by(eval(parse(text = col.name)),Peripheral_name) %>%
    summarise(EM = n(), .groups = "drop") %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  # 縦型から横型へ変換
  EM.number_by_col.name <- 
    EM.number_pivot %>% 
    pivot_wider(
      names_from = Peripheral_name,
      values_from = EM
    )%>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  # Maintenance_dateを先頭へ
  EM.number_by_col.name <-
    EM.number_by_col.name %>%
    select(`eval(parse(text = col.name))`, everything())
  # 列名の末尾に".EM"を追加
  colnames(EM.number_by_col.name) <- 
    names(EM.number_by_col.name) %>% 
    paste0(c(".EM"))
  # Maintenance_date列名を元に戻す
  EM.number_by_col.name <- 
    EM.number_by_col.name %>% 
    rename(!!col.name := `eval(parse(text = col.name)).EM`)
  
  print(EM.number_by_col.name)
  
  if (file.save == TRUE){
    write_tsv(EM.number_by_col.name, write.file_path)
  }
  
  return(EM.number_by_col.name)
}

# Metis_MF3_EM_ADF_by_Maintenance_date <- 
#   Create.file_for.EM_number(
#     df = Metis_MF3_2211,
#     field = c("ADF部"),
#     choice = c("COOK-C","SINAI-H"),
#     file.save = FALSE,
#     write.file_path = "./VOLGA/tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv"
# )
# 
# Metis_MF3_EM_FIN_by_Maintenance_date <- 
#   Create.file_for.EM_number(
#     df = Metis_MF3_2211,
#     field = c("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
#     choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
#     file.save = FALSE,
#     write.file_path = "./VOLGA/tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv")
