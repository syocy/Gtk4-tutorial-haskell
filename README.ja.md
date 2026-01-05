# Gtk4-tutorial-haskell

[English](README.md) | [日本語](README.ja.md)

このレポジトリは ToshioCP 氏の
[GTK 4 tutorial](https://toshiocp.github.io/Gtk4-tutorial/) のサンプルコードを、
[haskell-gi](https://hackage.haskell.org/package/haskell-gi) によって
Haskell に翻訳することを試みる学習用のプロジェクトです。

- [GTK 4 tutorial](https://toshiocp.github.io/Gtk4-tutorial/) のサンプルコードが他言語だとどのように記述されるか興味がある人
- Haskell による GUI アプリケーションの開発に興味がある人

を想定しています。

現在のところ、このレポジトリは Section 3 から 8 の内容をカバーしています。

本レポジトリは個人プロジェクトであり、ToshioCP 氏や GTK プロジェクト、haskell-gi プロジェクトとは関係ありません。

## 想定するユーザー・前提

- Haskell と cabal による基本的な開発方法が分かっていること
- GTK 4 アプリケーションが実行可能な環境であること
- haskell-gi の必須パッケージ・ライブラリをインストールしていること
    - インストール方法は haskell-gi の README を参照してください: https://github.com/haskell-gi/haskell-gi?tab=readme-ov-file#readme

なお、作者は Windows WSL2 上の Ubuntu で動作確認をしています。

## ディレクトリ構成

`app/` ディレクトリ以下に、GTK 4 tutorial の各セクションのサンプルコードに対応する Haskell ファイル (.hs) があります。

Haskell ファイルは可能な限り単体で完結し実行できる状態を保つようにしています。
それぞれのファイルに対応する executable が .cabal に定義されています。

例:

- [`app/Sec04_2.hs`](app/Sec04_2.hs): GTK 4 Tutorial Section 4 の 2 つ目の実行可能な単位のサンプルと対応する Haskell コード
- `sec04-2`: ↑を実行する executable 名

## ビルド方法およびビルドにあたっての注意事項

このプロジェクトでは個別の executable を指定してビルドすることを推奨します。

.cabal に多くの executable を定義しているため、
対象を指定しない `cabal build` はディスク使用量の圧迫を引き起こすおそれがあるためです。

ビルド時には対象を指定するようにしてください。

`cabal build`:

``` shell
cabal build sec04-1
```

`cabal run` および終了ステータスの確認:

``` shell
cabal run sec04-1 ; echo $?      # bash
cabal run sec04-1 ; echo $status # fish
```

または、動的リンクを有効にすることでディスク使用量を抑えられる可能性があります。  
(実際の設定方法は cabal のドキュメントをご確認ください)

``` haskell
-- cabal.project.local
executable-dynamic: True
```

もしくは、ややレガシーな方法として、

``` shell
cabal configure --enable-executable-dynamic
```

## 生成AIの利用に関する注記

誤解を避けるため、生成AIの利用範囲について明示します。

このレポジトリの開発においては、専ら英語翻訳と技術的な質問に生成AIを用いています。

生成AIによるコード (vibe coding) は含まれません。

## ライセンス

このレポジトリのライセンスは、オリジナル版を継承し [**GPL-3.0-or-later**](LICENSE) としています。

オリジナル版のライセンスについては以下を参照してください。

https://toshiocp.github.io/Gtk4-tutorial/sec1.html
