<h1 align="center">
  <p align="center">Covid-19 近況數據儀表版</p>
  <a href="https://shinycovid-19.herokuapp.com/"><img src="https://upload.cc/i1/2023/02/09/TmBZGp.png" alt="app"></a>
</h1>

透過 R 語言、Data Visualization 與 R shiny，將新冠肺炎對於產業衝擊的影響製作成數據儀表板。並即時產生簡易的描述性統計，方便觀察者了解數據變動的意義。

- Shiny App：https://xcswapjohn.shinyapps.io/Covid_19/
- Heroku App：https://shinycovid-19.herokuapp.com/

## 簡介

在第五屆深度資料力計畫中，主要以財政部營業稅申報資料，研究各產業的營業額狀況，並與美國、英國比較。並且，以 **Logistic Regression**、**LDA**、**KNN**...等機器學習方法，訓練模型。利用失業率、油價、營業額年增率等預測變數，預估銷售額。

最後以 Accuracy、AUC 來評估模型的效益。

- [專案報告書](https://1drv.ms/b/s!AiTjghiuXYI5zSCeckWOgzxSuwr8?e=jahIuX)
- [專案簡報](https://1drv.ms/b/s!AiTjghiuXYI5zVtVi5P6kT1CN1op?e=WM8bQi)

#### 專案部屬

視覺化網站以 [R shiny](https://shiny.rstudio.com/) 建構完成，並部屬至 shinyapps 的伺服器，雖然過程快速方便，但網站運行較慢。因此，以 Heroku 平台作為解決方案。

#### 團隊成員

<img width="150px" src="https://upload.cc/i1/2023/02/10/Byf9UV.png" alt="photo">

##### 指導業師
