################################################################################
# パッケージ名: ICISIP2024.R
# 1st リリース: 2024年6月16日 (Ver. 1.0)
# 1. 基本統計量をCSV出力します。
# 2. 平均±SD、ヒストグラム、蜂群図、バイオリン図をプロットします。
#
# 必須ライブラリ: tidyverse, ggbeeswarm, moments
#  tidyverse: https://www.tidyverse.org
#   データのモデル化、変換、視覚化のコアパッケージ
#  ggbeeswarm: https://github.com/eclarke/ggbeeswarm
#   蜂群図
#  moments: https://cran.r-project.org/web/packages/moments/index.html
#   基本統計量
################################################################################

# ファイルの名前を指定します。適宜変更してください。
data_file <- 'data_all.csv'

# フォルダの名前を指定します。適宜変更してください。
work_dir <- system("echo $PWD", intern = TRUE)

# 質問は全部で4個、固定です。
q_label <- paste0('Q', 1:4)

# 作業ディレクトリを変更します。
setwd(work_dir)

# データのモデル化、変換、視覚化のために、tidyverseライブラリを読み込みます。
suppressPackageStartupMessages(library(tidyverse))

# CSVファイルをtibble形式で読み込みます。
data_tibble <- read_csv(data_file, show_col_types = FALSE)
q_num <- ncol(data_tibble) - 4 # 質問数

################################################################################
# 0. データを分類します。
################################################################################

# 実験協力者、評価方式別の分類
data_tibble_ai_ls_text1 <- data_tibble |> filter(TYPE=='AI' & SCALE=='LS' & TEXT=='TEXT1')
data_tibble_ai_ls_text2 <- data_tibble |> filter(TYPE=='AI' & SCALE=='LS' & TEXT=='TEXT2')
data_tibble_ai_ls_text3 <- data_tibble |> filter(TYPE=='AI' & SCALE=='LS' & TEXT=='TEXT3')

data_tibble_ai_vas_text1 <- data_tibble |> filter(TYPE=='AI' & SCALE=='VAS' & TEXT=='TEXT1')
data_tibble_ai_vas_text2 <- data_tibble |> filter(TYPE=='AI' & SCALE=='VAS' & TEXT=='TEXT2')
data_tibble_ai_vas_text3 <- data_tibble |> filter(TYPE=='AI' & SCALE=='VAS' & TEXT=='TEXT3')

data_tibble_hm_ls_text1 <- data_tibble |> filter(TYPE=='HM' & SCALE=='LS' & TEXT=='TEXT1')
data_tibble_hm_ls_text2 <- data_tibble |> filter(TYPE=='HM' & SCALE=='LS' & TEXT=='TEXT2')
data_tibble_hm_ls_text3 <- data_tibble |> filter(TYPE=='HM' & SCALE=='LS' & TEXT=='TEXT3')

data_tibble_hm_vas_text1 <- data_tibble |> filter(TYPE=='HM' & SCALE=='VAS' & TEXT=='TEXT1')
data_tibble_hm_vas_text2 <- data_tibble |> filter(TYPE=='HM' & SCALE=='VAS' & TEXT=='TEXT2')
data_tibble_hm_vas_text3 <- data_tibble |> filter(TYPE=='HM' & SCALE=='VAS' & TEXT=='TEXT3')

################################################################################
# 1. 基本統計量、平均±標準偏差の折れ線グラフを描画します。
# AI LS:  #FF8C00 - ダークオレンジ
# AI VAS: #DC143C - クリムゾンレッド
# HM LS:  #99CCFF - 淡い空色
# HM VAS: #003399 - 濃紺
################################################################################

# 平均±標準偏差が0未満、1より大きい時に正規化する関数を定義します。
normalize <- function(x) {
  temp <- function(x) if(x<0) return(0) else if(x>1) return(1) else return(x)
  sapply(x, temp)
}

# 標準誤差を求める関数を定義します。
se <- function(x) {return(sd(x)/sqrt(length(x)))}

################################################################################
# Text1
################################################################################

# Text1の平均と標準偏差を求めます。
dsv_text1 <- data.frame(
  mean_ai_ls  = apply(data_tibble_ai_ls_text1[2:5],2, mean),
  mean_ai_vas = apply(data_tibble_ai_vas_text1[2:5],2,mean),
  mean_hm_ls  = apply(data_tibble_hm_ls_text1[2:5],2, mean),
  mean_hm_vas = apply(data_tibble_hm_vas_text1[2:5],2,mean),
  sd_ai_ls    = apply(data_tibble_ai_ls_text1[2:5],2,   sd),
  sd_ai_vas   = apply(data_tibble_ai_vas_text1[2:5],2,  sd),
  sd_hm_ls    = apply(data_tibble_hm_ls_text1[2:5],2,   sd),
  sd_hm_vas   = apply(data_tibble_hm_vas_text1[2:5],2,  sd)
)

# Text1の平均と標準偏差に着目したグラフを描画します。
mean_sd_text1 <- data.frame(
  no    = 1:q_num,
  q     = q_label,
  
  mean_ai_ls  = dsv_text1[,1],
  plus_ai_ls  = normalize(dsv_text1[,1]+dsv_text1[,5]),
  minus_ai_ls = normalize(dsv_text1[,1]-dsv_text1[,5]),

  mean_ai_vas = dsv_text1[,2],
  plus_ai_vas = normalize(dsv_text1[,2]+dsv_text1[,6]),
  minus_ai_vas= normalize(dsv_text1[,2]-dsv_text1[,6]),
  
  mean_hm_ls  = dsv_text1[,3],
  plus_hm_ls  = normalize(dsv_text1[,3]+dsv_text1[,7]),
  minus_hm_ls = normalize(dsv_text1[,3]-dsv_text1[,7]),
  
  mean_hm_vas = dsv_text1[,4],
  plus_hm_vas = normalize(dsv_text1[,4]+dsv_text1[,8]),
  minus_hm_vas= normalize(dsv_text1[,4]-dsv_text1[,8])
)
write.csv(mean_sd_text1,'Fig01_mean_sd_text1.csv',quote=FALSE)

# 平均±標準偏差を描画します(LS, Text1)
title_str <- sprintf("Mean and SD, LS, Text1, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text1),nrow(data_tibble_hm_ls_text1))

mean_sd_ls_text1_g <- ggplot(data=mean_sd_text1,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_ls,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_ls,ymax=plus_ai_ls,width=0.3, color='AI'),alpha=1.0,position='dodge') +
  
  geom_line(aes(x=rev(no),y=mean_hm_ls, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_ls,ymax=plus_hm_ls,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#FF8C00', 'Human'='#99CCFF')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_ls_text1_g
ggsave('Fig01_mean_sd_ls_text1.svg',mean_sd_ls_text1_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig01_mean_sd_ls_text1.pdf',mean_sd_ls_text1_g,device='pdf',width=297, height=210,units='mm',dpi=600)

# 平均±標準偏差を描画します(VAS, Text1)
title_str <- sprintf("Mean and SD, VAS, Text1, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text1),nrow(data_tibble_hm_vas_text1))

mean_sd_vas_text1_g <- ggplot(data=mean_sd_text1,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_vas,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_vas,ymax=plus_ai_vas,width=0.3, color='AI'),alpha=1.0,position='dodge') +

  geom_line(aes(x=rev(no),y=mean_hm_vas, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_vas,ymax=plus_hm_vas,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#DC143C', 'Human'='#003399')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_vas_text1_g
ggsave('Fig02_mean_sd_vas_text1.svg',mean_sd_vas_text1_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig02_mean_sd_vas_text1.pdf',mean_sd_vas_text1_g,device='pdf',width=297, height=210,units='mm',dpi=600)

################################################################################
# Text2
################################################################################

# Text2の平均と標準偏差を求めます。
dsv_text2 <- data.frame(
  mean_ai_ls  = apply(data_tibble_ai_ls_text2[2:5],2, mean),
  mean_ai_vas = apply(data_tibble_ai_vas_text2[2:5],2,mean),
  mean_hm_ls  = apply(data_tibble_hm_ls_text2[2:5],2, mean),
  mean_hm_vas = apply(data_tibble_hm_vas_text2[2:5],2,mean),
  sd_ai_ls    = apply(data_tibble_ai_ls_text2[2:5],2,   sd),
  sd_ai_vas   = apply(data_tibble_ai_vas_text2[2:5],2,  sd),
  sd_hm_ls    = apply(data_tibble_hm_ls_text2[2:5],2,   sd),
  sd_hm_vas   = apply(data_tibble_hm_vas_text2[2:5],2,  sd)
)

# Text2の平均と標準偏差に着目したグラフを描画します。
mean_sd_text2 <- data.frame(
  no    = 1:q_num,
  q     = q_label,
  
  mean_ai_ls  = dsv_text2[,1],
  plus_ai_ls  = normalize(dsv_text2[,1]+dsv_text2[,5]),
  minus_ai_ls = normalize(dsv_text2[,1]-dsv_text2[,5]),
  
  mean_ai_vas = dsv_text2[,2],
  plus_ai_vas = normalize(dsv_text2[,2]+dsv_text2[,6]),
  minus_ai_vas= normalize(dsv_text2[,2]-dsv_text2[,6]),
  
  mean_hm_ls  = dsv_text2[,3],
  plus_hm_ls  = normalize(dsv_text2[,3]+dsv_text2[,7]),
  minus_hm_ls = normalize(dsv_text2[,3]-dsv_text2[,7]),
  
  mean_hm_vas = dsv_text2[,4],
  plus_hm_vas = normalize(dsv_text2[,4]+dsv_text2[,8]),
  minus_hm_vas= normalize(dsv_text2[,4]-dsv_text2[,8])
)
write.csv(mean_sd_text2,'Fig03_mean_sd_text2.csv',quote=FALSE)

# 平均±標準偏差を描画します(LS, Text2)
title_str <- sprintf("Mean and SD, LS, Text2, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text2),nrow(data_tibble_hm_ls_text2))

mean_sd_ls_text2_g <- ggplot(data=mean_sd_text2,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_ls,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_ls,ymax=plus_ai_ls,width=0.3, color='AI'),alpha=1.0,position='dodge') +
  
  geom_line(aes(x=rev(no),y=mean_hm_ls, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_ls,ymax=plus_hm_ls,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#FF8C00', 'Human'='#99CCFF')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_ls_text2_g
ggsave('Fig03_mean_sd_ls_text2.svg',mean_sd_ls_text2_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig03_mean_sd_ls_text2.pdf',mean_sd_ls_text2_g,device='pdf',width=297, height=210,units='mm',dpi=600)

# 平均±標準偏差を描画します(VAS, Text2)
title_str <- sprintf("Mean and SD, VAS, Text2, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text2),nrow(data_tibble_hm_vas_text2))

mean_sd_vas_text2_g <- ggplot(data=mean_sd_text2,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_vas,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_vas,ymax=plus_ai_vas,width=0.3, color='AI'),alpha=1.0,position='dodge') +
  
  geom_line(aes(x=rev(no),y=mean_hm_vas, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_vas,ymax=plus_hm_vas,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#DC143C', 'Human'='#003399')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_vas_text2_g
ggsave('Fig04_mean_sd_vas_text2.svg',mean_sd_vas_text2_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig04_mean_sd_vas_text2.pdf',mean_sd_vas_text2_g,device='pdf',width=297, height=210,units='mm',dpi=600)

################################################################################
# Text3
################################################################################

# Text3の平均と標準偏差を求めます。
dsv_text3 <- data.frame(
  mean_ai_ls  = apply(data_tibble_ai_ls_text3[2:5],2, mean),
  mean_ai_vas = apply(data_tibble_ai_vas_text3[2:5],2,mean),
  mean_hm_ls  = apply(data_tibble_hm_ls_text3[2:5],2, mean),
  mean_hm_vas = apply(data_tibble_hm_vas_text3[2:5],2,mean),
  sd_ai_ls    = apply(data_tibble_ai_ls_text3[2:5],2,   sd),
  sd_ai_vas   = apply(data_tibble_ai_vas_text3[2:5],2,  sd),
  sd_hm_ls    = apply(data_tibble_hm_ls_text3[2:5],2,   sd),
  sd_hm_vas   = apply(data_tibble_hm_vas_text3[2:5],2,  sd)
)

# Text3の平均と標準偏差に着目したグラフを描画します。
mean_sd_text3 <- data.frame(
  no    = 1:q_num,
  q     = q_label,
  
  mean_ai_ls  = dsv_text3[,1],
  plus_ai_ls  = normalize(dsv_text3[,1]+dsv_text3[,5]),
  minus_ai_ls = normalize(dsv_text3[,1]-dsv_text3[,5]),
  
  mean_ai_vas = dsv_text3[,2],
  plus_ai_vas = normalize(dsv_text3[,2]+dsv_text3[,6]),
  minus_ai_vas= normalize(dsv_text3[,2]-dsv_text3[,6]),
  
  mean_hm_ls  = dsv_text3[,3],
  plus_hm_ls  = normalize(dsv_text3[,3]+dsv_text3[,7]),
  minus_hm_ls = normalize(dsv_text3[,3]-dsv_text3[,7]),
  
  mean_hm_vas = dsv_text3[,4],
  plus_hm_vas = normalize(dsv_text3[,4]+dsv_text3[,8]),
  minus_hm_vas= normalize(dsv_text3[,4]-dsv_text3[,8])
)
write.csv(mean_sd_text3,'Fig05_mean_sd_text3.csv',quote=FALSE)

# 平均±標準偏差を描画します(LS, Text3)
title_str <- sprintf("Mean and SD, LS, Text3, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text3),nrow(data_tibble_hm_ls_text3))

mean_sd_ls_text3_g <- ggplot(data=mean_sd_text3,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_ls,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_ls,ymax=plus_ai_ls,width=0.3, color='AI'),alpha=1.0,position='dodge') +
  
  geom_line(aes(x=rev(no),y=mean_hm_ls, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_ls,ymax=plus_hm_ls,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#FF8C00', 'Human'='#99CCFF')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_ls_text3_g
ggsave('Fig05_mean_sd_ls_text3.svg',mean_sd_ls_text3_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig05_mean_sd_ls_text3.pdf',mean_sd_ls_text3_g,device='pdf',width=297, height=210,units='mm',dpi=600)

# 平均±標準偏差を描画します(VAS, Text3)
title_str <- sprintf("Mean and SD, VAS, Text3, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text3),nrow(data_tibble_hm_vas_text3))

mean_sd_vas_text3_g <- ggplot(data=mean_sd_text3,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=mean_ai_vas,color='AI'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_ai_vas,ymax=plus_ai_vas,width=0.3, color='AI'),alpha=1.0,position='dodge') +
  
  geom_line(aes(x=rev(no),y=mean_hm_vas, color='Human'),alpha=1.0) +
  geom_errorbar(aes(x=rev(no),ymin=minus_hm_vas,ymax=plus_hm_vas,width=0.3, color='Human'),alpha=1.0,position='dodge') +
  
  # 凡例の設定
  scale_color_manual(name=title_str, values=c('AI'='#DC143C', 'Human'='#003399')) +
  
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
mean_sd_vas_text3_g
ggsave('Fig06_mean_sd_vas_text3.svg',mean_sd_vas_text3_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig06_mean_sd_vas_text3.pdf',mean_sd_vas_text3_g,device='pdf',width=297, height=210,units='mm',dpi=600)

################################################################################
# 箱ひげ図、バイオリンプロットと群蜂図を描画します。
# AI LS:  #FF8C00 - ダークオレンジ
# AI VAS: #DC143C - クリムゾンレッド
# HM LS:  #99CCFF - 淡い空色
# HM VAS: #003399 - 濃紺
################################################################################
library(ggbeeswarm)

################################################################################
# Text1, LS
################################################################################

# データを選別します。
data_tibble_ls_text1  <- data_tibble |> filter(SCALE=='LS'  & TEXT=='TEXT1')

# ggplot2が扱えるフォーマットに変換します。
data_ls_text1 <- data_tibble_ls_text1 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("LS, Text1, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text1),nrow(data_tibble_hm_ls_text1))

# 描画します。
plots_ls_text1_g <- ggplot(data_ls_text1, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_ls_text1, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_ls_text1, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#FF8C00', 'HM' = '#99CCFF')) +
  scale_fill_manual(values = c('AI' = '#FF8C00', 'HM' = '#99CCFF'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_ls_text1_g

# ファイルに保存します。
ggsave("Fig07_plots_ls_text1.svg", plots_ls_text1_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig07_plots_ls_text1.pdf", plots_ls_text1_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_ls_text1,'Fig07_data_tibble_ls_text1.csv',quote=FALSE)

################################################################################
# Text1, VAS
################################################################################

# データを選別します。
data_tibble_vas_text1 <- data_tibble |> filter(SCALE=='VAS' & TEXT=='TEXT1')

# ggplot2が扱えるフォーマットに変換します。
data_vas_text1 <- data_tibble_vas_text1 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("VAS, Text1, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text1),nrow(data_tibble_hm_vas_text1))

# 描画します。
plots_vas_text1_g <- ggplot(data_vas_text1, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_vas_text1, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_vas_text1, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#DC143C', 'HM' = '#003399')) +
  scale_fill_manual(values = c('AI' = '#DC143C', 'HM' = '#003399'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_vas_text1_g

# ファイルに保存します。
ggsave("Fig08_plots_vas_text1.svg", plots_vas_text1_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig08_plots_vas_text1.pdf", plots_vas_text1_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_vas_text1,'Fig08_data_tibble_vas_text1.csv',quote=FALSE)

################################################################################
# Text2, LS
################################################################################

# データを選別します。
data_tibble_ls_text2  <- data_tibble |> filter(SCALE=='LS'  & TEXT=='TEXT1')

# ggplot2が扱えるフォーマットに変換します。
data_ls_text2 <- data_tibble_ls_text2 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("LS, Text2, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text2),nrow(data_tibble_hm_ls_text2))

# 描画します。
plots_ls_text2_g <- ggplot(data_ls_text2, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_ls_text2, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_ls_text2, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#FF8C00', 'HM' = '#99CCFF')) +
  scale_fill_manual(values = c('AI' = '#FF8C00', 'HM' = '#99CCFF'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_ls_text2_g

# ファイルに保存します。
ggsave("Fig09_plots_ls_text2.svg", plots_ls_text2_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig09_plots_ls_text2.pdf", plots_ls_text2_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_ls_text2,'Fig09_data_tibble_ls_text2.csv',quote=FALSE)

################################################################################
# Text2, VAS
################################################################################

# データを選別します。
data_tibble_vas_text2 <- data_tibble |> filter(SCALE=='VAS' & TEXT=='TEXT2')

# ggplot2が扱えるフォーマットに変換します。
data_vas_text2 <- data_tibble_vas_text2 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("VAS, Text2, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text2),nrow(data_tibble_hm_vas_text2))

# 描画します。
plots_vas_text2_g <- ggplot(data_vas_text2, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_vas_text2, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_vas_text2, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#DC143C', 'HM' = '#003399')) +
  scale_fill_manual(values = c('AI' = '#DC143C', 'HM' = '#003399'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_vas_text2_g

# ファイルに保存します。
ggsave("Fig10_plots_vas_text2.svg", plots_vas_text2_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig10_plots_vas_text2.pdf", plots_vas_text2_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_vas_text2,'Fig10_data_tibble_vas_text2.csv',quote=FALSE)

################################################################################
# Text3, LS
################################################################################

# データを選別します。
data_tibble_ls_text3  <- data_tibble |> filter(SCALE=='LS'  & TEXT=='TEXT3')

# ggplot2が扱えるフォーマットに変換します。
data_ls_text3 <- data_tibble_ls_text3 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("LS, Text3, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text3),nrow(data_tibble_hm_ls_text3))

# 描画します。
plots_ls_text3_g <- ggplot(data_ls_text3, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_ls_text3, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_ls_text3, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#FF8C00', 'HM' = '#99CCFF')) +
  scale_fill_manual(values = c('AI' = '#FF8C00', 'HM' = '#99CCFF'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_ls_text3_g

# ファイルに保存します。
ggsave("Fig11_plots_ls_text3.svg", plots_ls_text3_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig11_plots_ls_text3.pdf", plots_ls_text3_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_ls_text3,'Fig11_data_tibble_ls_text3.csv',quote=FALSE)

################################################################################
# Text3, VAS
################################################################################

# データを選別します。
data_tibble_vas_text3 <- data_tibble |> filter(SCALE=='VAS' & TEXT=='TEXT3')

# ggplot2が扱えるフォーマットに変換します。
data_vas_text3 <- data_tibble_vas_text3 |>
  pivot_longer(cols=any_of(q_label),names_to='Question',values_to='Value')

# 図のタイトル文字列を作成します。
title_str <- sprintf("VAS, Text3, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_vas_text3),nrow(data_tibble_hm_vas_text3))

# 描画します。
plots_vas_text3_g <- ggplot(data_vas_text3, aes(x = Question, y = Value, fill = TYPE, color = TYPE)) +
  geom_violin(data = subset(data_vas_text3, TYPE == 'AI'), alpha = 0.4) +
  geom_violin(data = subset(data_vas_text3, TYPE == 'HM'), alpha = 0.4) +
  geom_beeswarm(size = 2, alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev(q_label)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.25), limits = c(0.0, 1.0)) +
  labs(x='', y='Value') +
  scale_color_manual(name = title_str, values = c('AI' = '#DC143C', 'HM' = '#003399')) +
  scale_fill_manual(values = c('AI' = '#DC143C', 'HM' = '#003399'), guide = 'none') +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 16),
    legend.position = 'top'
  )

# 描画を確認します。
plots_vas_text3_g

# ファイルに保存します。
ggsave("Fig12_plots_vas_text3.svg", plots_vas_text3_g, device = "svg", width = 297, height = 210, units = "mm", dpi = 600)
ggsave("Fig12_plots_vas_text3.pdf", plots_vas_text3_g, device = "pdf", width = 297, height = 210, units = "mm", dpi = 600)
write.csv(data_tibble_vas_text2,'Fig12_data_tibble_vas_text3.csv',quote=FALSE)

################################################################################
# 全記述統計量 LSとVASでText1, Text2, Text3の違いを検出できるかどうか確認
################################################################################

ls_text1 <- data.frame(
  max_ai_ls      = apply(data_tibble_ai_ls_text1[2:5],2, max),      #  1
  min_ai_ls      = apply(data_tibble_ai_ls_text1[2:5],2, min),      #  2
  mean_ai_ls     = apply(data_tibble_ai_ls_text1[2:5],2, mean),     #  3
  median_ai_ls   = apply(data_tibble_ai_ls_text1[2:5],2, median),   #  4
  var_ai_ls      = apply(data_tibble_ai_ls_text1[2:5],2, var),      #  5
  sd_ai_ls       = apply(data_tibble_ai_ls_text1[2:5],2, sd),       #  6
  se_ai_ls       = apply(data_tibble_ai_ls_text1[2:5],2, se),       #  7
  skewness_ai_ls = apply(data_tibble_ai_ls_text1[2:5],2, skewness), #  8
  kurtosis_ai_ls = apply(data_tibble_ai_ls_text1[2:5],2, kurtosis), #  9
  max_hm_ls      = apply(data_tibble_hm_ls_text1[2:5],2, max),      # 10
  min_hm_ls      = apply(data_tibble_hm_ls_text1[2:5],2, min),      # 11
  mean_hm_ls     = apply(data_tibble_hm_ls_text1[2:5],2, mean),     # 12
  median_hm_ls   = apply(data_tibble_hm_ls_text1[2:5],2, median),   # 13
  var_hm_ls      = apply(data_tibble_hm_ls_text1[2:5],2, var),      # 14
  sd_hm_ls       = apply(data_tibble_hm_ls_text1[2:5],2, sd),       # 15
  se_hm_ls       = apply(data_tibble_hm_ls_text1[2:5],2, se),       # 16
  skewness_hm_ls = apply(data_tibble_hm_ls_text1[2:5],2, skewness), # 17
  kurtosis_hm_ls = apply(data_tibble_hm_ls_text1[2:5],2, kurtosis)  # 18
)
ls_text1
write.csv(ls_text1,'Tab1_ls_text1.csv',quote=FALSE)

vas_text1 <- data.frame(
  max_ai_vas      = apply(data_tibble_ai_vas_text1[2:5],2, max),      #  1
  min_ai_vas      = apply(data_tibble_ai_vas_text1[2:5],2, min),      #  2
  mean_ai_vas     = apply(data_tibble_ai_vas_text1[2:5],2, mean),     #  3
  median_ai_vas   = apply(data_tibble_ai_vas_text1[2:5],2, median),   #  4
  var_ai_vas      = apply(data_tibble_ai_vas_text1[2:5],2, var),      #  5
  sd_ai_vas       = apply(data_tibble_ai_vas_text1[2:5],2, sd),       #  6
  se_ai_vas       = apply(data_tibble_ai_vas_text1[2:5],2, se),       #  7
  skewness_ai_vas = apply(data_tibble_ai_vas_text1[2:5],2, skewness), #  8
  kurtosis_ai_vas = apply(data_tibble_ai_vas_text1[2:5],2, kurtosis), #  9
  max_hm_vas      = apply(data_tibble_hm_vas_text1[2:5],2, max),      # 10
  min_hm_vas      = apply(data_tibble_hm_vas_text1[2:5],2, min),      # 11
  mean_hm_vas     = apply(data_tibble_hm_vas_text1[2:5],2, mean),     # 12
  median_hm_vas   = apply(data_tibble_hm_vas_text1[2:5],2, median),   # 13
  var_hm_vas      = apply(data_tibble_hm_vas_text1[2:5],2, var),      # 14
  sd_hm_vas       = apply(data_tibble_hm_vas_text1[2:5],2, sd),       # 15
  se_hm_vas       = apply(data_tibble_hm_vas_text1[2:5],2, se),       # 16
  skewness_hm_vas = apply(data_tibble_hm_vas_text1[2:5],2, skewness), # 17
  kurtosis_hm_vas = apply(data_tibble_hm_vas_text1[2:5],2, kurtosis)  # 18
)
vas_text1
write.csv(vas_text1,'Tab1_vas_text1.csv',quote=FALSE)

ls_text2 <- data.frame(
  max_ai_ls      = apply(data_tibble_ai_ls_text2[2:5],2, max),      #  1
  min_ai_ls      = apply(data_tibble_ai_ls_text2[2:5],2, min),      #  2
  mean_ai_ls     = apply(data_tibble_ai_ls_text2[2:5],2, mean),     #  3
  median_ai_ls   = apply(data_tibble_ai_ls_text2[2:5],2, median),   #  4
  var_ai_ls      = apply(data_tibble_ai_ls_text2[2:5],2, var),      #  5
  sd_ai_ls       = apply(data_tibble_ai_ls_text2[2:5],2, sd),       #  6
  se_ai_ls       = apply(data_tibble_ai_ls_text2[2:5],2, se),       #  7
  skewness_ai_ls = apply(data_tibble_ai_ls_text2[2:5],2, skewness), #  8
  kurtosis_ai_ls = apply(data_tibble_ai_ls_text2[2:5],2, kurtosis), #  9
  max_hm_ls      = apply(data_tibble_hm_ls_text2[2:5],2, max),      # 10
  min_hm_ls      = apply(data_tibble_hm_ls_text2[2:5],2, min),      # 11
  mean_hm_ls     = apply(data_tibble_hm_ls_text2[2:5],2, mean),     # 12
  median_hm_ls   = apply(data_tibble_hm_ls_text2[2:5],2, median),   # 13
  var_hm_ls      = apply(data_tibble_hm_ls_text2[2:5],2, var),      # 14
  sd_hm_ls       = apply(data_tibble_hm_ls_text2[2:5],2, sd),       # 15
  se_hm_ls       = apply(data_tibble_hm_ls_text2[2:5],2, se),       # 16
  skewness_hm_ls = apply(data_tibble_hm_ls_text2[2:5],2, skewness), # 17
  kurtosis_hm_ls = apply(data_tibble_hm_ls_text2[2:5],2, kurtosis)  # 18
)
ls_text2
write.csv(ls_text2,'Tab2_ls_text2.csv',quote=FALSE)

vas_text2 <- data.frame(
  max_ai_vas      = apply(data_tibble_ai_vas_text2[2:5],2, max),      #  1
  min_ai_vas      = apply(data_tibble_ai_vas_text2[2:5],2, min),      #  2
  mean_ai_vas     = apply(data_tibble_ai_vas_text2[2:5],2, mean),     #  3
  median_ai_vas   = apply(data_tibble_ai_vas_text2[2:5],2, median),   #  4
  var_ai_vas      = apply(data_tibble_ai_vas_text2[2:5],2, var),      #  5
  sd_ai_vas       = apply(data_tibble_ai_vas_text2[2:5],2, sd),       #  6
  se_ai_vas       = apply(data_tibble_ai_vas_text2[2:5],2, se),       #  7
  skewness_ai_vas = apply(data_tibble_ai_vas_text2[2:5],2, skewness), #  8
  kurtosis_ai_vas = apply(data_tibble_ai_vas_text2[2:5],2, kurtosis), #  9
  max_hm_vas      = apply(data_tibble_hm_vas_text2[2:5],2, max),      # 10
  min_hm_vas      = apply(data_tibble_hm_vas_text2[2:5],2, min),      # 11
  mean_hm_vas     = apply(data_tibble_hm_vas_text2[2:5],2, mean),     # 12
  median_hm_vas   = apply(data_tibble_hm_vas_text2[2:5],2, median),   # 13
  var_hm_vas      = apply(data_tibble_hm_vas_text2[2:5],2, var),      # 14
  sd_hm_vas       = apply(data_tibble_hm_vas_text2[2:5],2, sd),       # 15
  se_hm_vas       = apply(data_tibble_hm_vas_text2[2:5],2, se),       # 16
  skewness_hm_vas = apply(data_tibble_hm_vas_text2[2:5],2, skewness), # 17
  kurtosis_hm_vas = apply(data_tibble_hm_vas_text2[2:5],2, kurtosis)  # 18
)
vas_text2
write.csv(vas_text2,'Tab2_vas_text2.csv',quote=FALSE)

ls_text3 <- data.frame(
  max_ai_ls      = apply(data_tibble_ai_ls_text3[2:5],2, max),      #  1
  min_ai_ls      = apply(data_tibble_ai_ls_text3[2:5],2, min),      #  2
  mean_ai_ls     = apply(data_tibble_ai_ls_text3[2:5],2, mean),     #  3
  median_ai_ls   = apply(data_tibble_ai_ls_text3[2:5],2, median),   #  4
  var_ai_ls      = apply(data_tibble_ai_ls_text3[2:5],2, var),      #  5
  sd_ai_ls       = apply(data_tibble_ai_ls_text3[2:5],2, sd),       #  6
  se_ai_ls       = apply(data_tibble_ai_ls_text3[2:5],2, se),       #  7
  skewness_ai_ls = apply(data_tibble_ai_ls_text3[2:5],2, skewness), #  8
  kurtosis_ai_ls = apply(data_tibble_ai_ls_text3[2:5],2, kurtosis), #  9
  max_hm_ls      = apply(data_tibble_hm_ls_text3[2:5],2, max),      # 10
  min_hm_ls      = apply(data_tibble_hm_ls_text3[2:5],2, min),      # 11
  mean_hm_ls     = apply(data_tibble_hm_ls_text3[2:5],2, mean),     # 12
  median_hm_ls   = apply(data_tibble_hm_ls_text3[2:5],2, median),   # 13
  var_hm_ls      = apply(data_tibble_hm_ls_text3[2:5],2, var),      # 14
  sd_hm_ls       = apply(data_tibble_hm_ls_text3[2:5],2, sd),       # 15
  se_hm_ls       = apply(data_tibble_hm_ls_text3[2:5],2, se),       # 16
  skewness_hm_ls = apply(data_tibble_hm_ls_text3[2:5],2, skewness), # 17
  kurtosis_hm_ls = apply(data_tibble_hm_ls_text3[2:5],2, kurtosis)  # 18
)
ls_text3
write.csv(ls_text3,'Tab3_ls_text3.csv',quote=FALSE)

vas_text3 <- data.frame(
  max_ai_vas      = apply(data_tibble_ai_vas_text3[2:5],2, max),      #  1
  min_ai_vas      = apply(data_tibble_ai_vas_text3[2:5],2, min),      #  2
  mean_ai_vas     = apply(data_tibble_ai_vas_text3[2:5],2, mean),     #  3
  median_ai_vas   = apply(data_tibble_ai_vas_text3[2:5],2, median),   #  4
  var_ai_vas      = apply(data_tibble_ai_vas_text3[2:5],2, var),      #  5
  sd_ai_vas       = apply(data_tibble_ai_vas_text3[2:5],2, sd),       #  6
  se_ai_vas       = apply(data_tibble_ai_vas_text3[2:5],2, se),       #  7
  skewness_ai_vas = apply(data_tibble_ai_vas_text3[2:5],2, skewness), #  8
  kurtosis_ai_vas = apply(data_tibble_ai_vas_text3[2:5],2, kurtosis), #  9
  max_hm_vas      = apply(data_tibble_hm_vas_text3[2:5],2, max),      # 10
  min_hm_vas      = apply(data_tibble_hm_vas_text3[2:5],2, min),      # 11
  mean_hm_vas     = apply(data_tibble_hm_vas_text3[2:5],2, mean),     # 12
  median_hm_vas   = apply(data_tibble_hm_vas_text3[2:5],2, median),   # 13
  var_hm_vas      = apply(data_tibble_hm_vas_text3[2:5],2, var),      # 14
  sd_hm_vas       = apply(data_tibble_hm_vas_text3[2:5],2, sd),       # 15
  se_hm_vas       = apply(data_tibble_hm_vas_text3[2:5],2, se),       # 16
  skewness_hm_vas = apply(data_tibble_hm_vas_text3[2:5],2, skewness), # 17
  kurtosis_hm_vas = apply(data_tibble_hm_vas_text3[2:5],2, kurtosis)  # 18
)
vas_text3
write.csv(vas_text3,'Tab3_vas_text3.csv',quote=FALSE)

################################################################################
# 中央値に着目したグラフを描画します。
################################################################################
# Text1
################################################################################
median_text1 <- data.frame(
  no     = 1:q_num,
  q      = q_label,
  ai_ls  = apply(data_tibble_ai_ls_text1[2:5], 2, median),
  hm_ls  = apply(data_tibble_hm_ls_text1[2:5], 2, median),
  ai_vas = apply(data_tibble_ai_vas_text1[2:5],2, median),
  hm_vas = apply(data_tibble_hm_vas_text1[2:5],2, median)
)

# 平均±標準偏差を描画します(LS, Text1)
title_str <- sprintf("Median, Text1, n:AI=%d, Human=%d",
                    nrow(data_tibble_ai_ls_text1),nrow(data_tibble_hm_ls_text1))

median_ls_text1_g <- ggplot(data=median_text1,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=ai_ls, color='AI_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_ls, color='HM_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=ai_vas,color='AI_VAS'),alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_vas,color='HM_VAS'),alpha=1.0) +
  # 凡例の設定
  scale_color_manual(name=title_str, 
    values=c('AI_LS' ='#FF8C00','HM_LS' ='#99CCFF',
             'AI_VAS'='#DC143C','HM_VAS'='#003399')) +
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
median_ls_text1_g
ggsave('Fig21_median_ls_text1.svg',median_ls_text1_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig22_median_ls_text1.pdf',median_ls_text1_g,device='pdf',width=297, height=210,units='mm',dpi=600)

################################################################################
# Text2
################################################################################
median_text2 <- data.frame(
  no     = 1:q_num,
  q      = q_label,
  ai_ls  = apply(data_tibble_ai_ls_text2[2:5], 2, median),
  hm_ls  = apply(data_tibble_hm_ls_text2[2:5], 2, median),
  ai_vas = apply(data_tibble_ai_vas_text2[2:5],2, median),
  hm_vas = apply(data_tibble_hm_vas_text2[2:5],2, median)
)

# 平均±標準偏差を描画します(LS, Text1)
title_str <- sprintf("Median, Text2, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text2),nrow(data_tibble_hm_ls_text2))

median_ls_text2_g <- ggplot(data=median_text2,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=ai_ls, color='AI_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_ls, color='HM_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=ai_vas,color='AI_VAS'),alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_vas,color='HM_VAS'),alpha=1.0) +
  # 凡例の設定
  scale_color_manual(name=title_str, 
                     values=c('AI_LS' ='#FF8C00','HM_LS' ='#99CCFF',
                              'AI_VAS'='#DC143C','HM_VAS'='#003399')) +
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
median_ls_text2_g
ggsave('Fig23_median_ls_text2.svg',median_ls_text2_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig24_median_ls_text2.pdf',median_ls_text2_g,device='pdf',width=297, height=210,units='mm',dpi=600)

################################################################################
# Text3
################################################################################
median_text3 <- data.frame(
  no     = 1:q_num,
  q      = q_label,
  ai_ls  = apply(data_tibble_ai_ls_text3[2:5], 2, median),
  hm_ls  = apply(data_tibble_hm_ls_text3[2:5], 2, median),
  ai_vas = apply(data_tibble_ai_vas_text3[2:5],2, median),
  hm_vas = apply(data_tibble_hm_vas_text3[2:5],2, median)
)

# 平均±標準偏差を描画します(LS, Text1)
title_str <- sprintf("Median, Text3, n:AI=%d, Human=%d",
                     nrow(data_tibble_ai_ls_text3),nrow(data_tibble_hm_ls_text3))

median_ls_text3_g <- ggplot(data=median_text3,aes(ymin=0,ymax=1)) +
  geom_line(aes(x=rev(no),y=ai_ls, color='AI_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_ls, color='HM_LS'), alpha=1.0) +
  geom_line(aes(x=rev(no),y=ai_vas,color='AI_VAS'),alpha=1.0) +
  geom_line(aes(x=rev(no),y=hm_vas,color='HM_VAS'),alpha=1.0) +
  # 凡例の設定
  scale_color_manual(name=title_str, 
                     values=c('AI_LS' ='#FF8C00','HM_LS' ='#99CCFF',
                              'AI_VAS'='#DC143C','HM_VAS'='#003399')) +
  # 基本設定
  labs(x='',y='Value') +
  scale_x_discrete(limits=rev(q_label)) +
  scale_y_continuous(breaks=seq(0.0,1.0,by=0.25),limits=c(0.0,1.0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16),  # タイトルのサイズ
    axis.title.x  = element_text(size = 24),  # X軸のタイトルのサイズ
    axis.title.y  = element_text(size = 24),  # Y軸のタイトルのサイズ
    axis.text.x   = element_text(size = 16),  # X軸のテキストのサイズ
    axis.text.y   = element_text(size = 16),  # Y軸のテキストのサイズ
    legend.title  = element_text(size = 16),  # 凡例のタイトルのサイズ
    legend.text   = element_text(size = 14),  # 凡例のテキストのサイズ
    legend.position = 'top'                   # 凡例の位置
  )
median_ls_text3_g
ggsave('Fig25_median_ls_text3.svg',median_ls_text3_g,device='svg',width=297, height=210,units='mm',dpi=600)
ggsave('Fig26_median_ls_text3.pdf',median_ls_text3_g,device='pdf',width=297, height=210,units='mm',dpi=600)

