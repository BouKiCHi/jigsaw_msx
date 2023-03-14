# JigSaw

SFG-0xの音源ドライバです。曲データはMMLで記述します。

## 使用方法

コンパイラはWindowsのコマンドプロンプト上で動作します。
songsディレクトリで`make_jig.bat`を使用してコンパイルを行ってください。

```sh
make_jig test
```

再生はjigdataの中身をコピーして、
MSXのDOSプロンプトで次のようにコマンドを実行します。

```
jig test.jig
```

## 参考

当時のテキスト
[doc/jigsaw.txt](doc/jigsaw.txt)
