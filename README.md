
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpaccidents <a href="https://nononoexe.github.io/jpaccidents/"><img src="man/figures/logo.png" align="right" height="138" alt="jpaccidents website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/jpaccidents)](https://CRAN.R-project.org/package=jpaccidents)
[![R-CMD-check](https://github.com/NONONOexe/jpaccidents/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NONONOexe/jpaccidents/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/NONONOexe/jpaccidents/graph/badge.svg)](https://app.codecov.io/gh/NONONOexe/jpaccidents)
<!-- badges: end -->

jpaccidents is package for handling the traffic accident data in Japan.
This package covers the traffic accident data provided by the [National
Police Agency of Japan](https://www.npa.go.jp/english/index.html).

> jpaccidentsは日本の交通事故のデータを扱うためのパッケージです。
> このパッケージでは、[警察庁](https://www.npa.go.jp/index.html)が提供する交通事故データを対象としています。

## Installation

You can install the development version of jpaccidents like so:

> jpaccidentsの開発版は以下のコマンドからインストールできます。

``` r
install.packages("remotes")
remotes::install_git("https://github.com/NONONOexe/jpaccidents")
```

## Usage

Load the package as follows:

> パッケージを以下のようにして読み込みます。

``` r
library(jpaccidents)
```

The traffic accident data can be used follows:

> 交通事故のデータは次のようにして利用することができます。

``` r
downloaded_path <- download_accident_main_data(download_dir = "download-dir-path", year = 2022)
main_data <- read_accident_main_data(downloaded_path)
```

There are three types of traffic accident data: main (honhyo),
supplementary (hojyuhyo), and highway (kosokuhyo) data. The package
provides functions for each of these. Use the function appropriate for
the data you need. See also `? download_accident_main_data` for more
information.

> 交通事故のデータは本票、補充票、高速票の3種類があります。
> それぞれに対応する関数が用意されています。
> 必要なデータに応じて利用してください。
> 詳しくは`? download_accident_main_data`を参照してください。

## License

This package is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details. The traffic accident data
downloaded using this package is provided by the National Police Agency
of Japan under the [Creative Commons Attribution 4.0 International
License](https://creativecommons.org/licenses/by/4.0/).

> このパッケージはMITライセンスの下で提供されています。詳細は[LICENSE](LICENSE)ファイルを参照してください。
> また、本パッケージによりダウンロードできる交通事故データは、警察庁により[クリエイティブ・コモンズ
> 表示 4.0 国際
> ライセンス](https://creativecommons.org/licenses/by/4.0/deed.ja)の下で提供されています。

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

> このプロジェクトは[行動規範](CODE_OF_CONDUCT.md)を設けています。
> したがって、このプロジェクトでは参加者にこの行動規範に従うことを求めます。
