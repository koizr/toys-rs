# プログラミング言語 Toys 処理系の Rust 実装

[WEB+DB PRESS Vol.125](https://gihyo.jp/magazine/wdpress/archive/2021/vol125) 特集1 「作って学ぶプログラミング言語のしくみ　インタプリタ、構文解析器、文法」において実装するプログラミング言語 Toys を Rust で実装したもの。

## TODO

- [x] AST とその評価
- [x] 構文解析器
- [x] 実行バイナリ
- [x] 文法の拡張
  - [x] `for`
  - [x] ラベル引数

## 実行方法

### `cargo run` を使う

```sh
# ファイルを指定
cargo run -- path/to/src.toys

# ソースコードをオプションで渡す
cargo run -- -e "define main() { println(1 + 2); }"
```

### 実行バイナリを使う

```sh
cargo build --release
cd ./target/release/
```

```sh
# ファイルを指定
toys-rs path/to/src.toys

# ソースコードをオプションで渡す
toys-rs -e "define main() { println(1 + 2); }"
```
