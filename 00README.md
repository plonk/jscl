# JSCL とは何ですか。

Common Lisp を JavaScript にコンパイルするソフトウェアです。JavaScript
は、ご存知のようにブラウザの標準言語です。ですから、Common Lisp で書い
たプログラムをブラウザで動かす為にJSCLが必要になります。

JavaScript を実行するソフトウェアは JavaScript エンジンと言います。ブ
ラウザには JavaScript エンジンが組み込まれているので、JavaScript プロ
グラムを実行することができます。

Google Chrome で使われている JS エンジンを Chrome V8 と言います。この
V8 エンジンを利用してサーバープログラムを作る仕組みが Node.js です。

# Node.js とは何ですか。

ブラウザの JavaScript 実行環境は、Web にある信頼できないコードを実行す
る必要から、サンドボックス化されていて、自由にマシンのディスクにアクセ
スしたり、ネットワーク接続を利用することができません。

Node.js ではこのような制限がありませんので、C や、SBCL のように OS の
フル機能を活用したプログラミングができます。

Node.js にも Common Lisp のように REPL が備わっており、node あるいは
nodejs という名前になっています。

```
$ node
> 1 + 1
2
>
```

Ctrl+C を2回押すと node コマンドが終了して、シェルプロンプトに戻ります。

# JSCLはどのようにブートストラップされますか

JSCL はコンパイラと Common Lisp ランタイム(関数・マクロ)からなっていま
すが、これらのほとんどは Common Lisp で書かれています。

SBCL 等、他の Lisp 処理系で JSCL コンパイラを実行して、Common Lisp で
書かれた JSCL コンパイラ自体のコードを JavaScript にコンパイルする処理
をブートストラップと言います。

この結果 `jscl.js` というファイルが作成されます。これは JSCL コンパイ
ラと、Common Lisp 関数のコードが入った「JavaScript モジュール」です。

node コマンドでこのモジュールを使ってみましょう。

```
$ node
> var jscl = require("./jscl.js")
[...]
undefined
> jscl.evaluateString("(+ 1 1)")
2
>
```

最初の入力行で jscl.js ファイルを読み込んで、JSCL モジュールを jscl と
いう変数に束縛しています。2つ目の入力行で 同モジュールの
evaluateString 関数を "(+ 1 1)" という文字列を引数に呼び出しました。
その結果、2 という数値が返りました。

evaluateString は Common Lisp のコードを受けとって、それを JavaScript
にコンパイルし、JS エンジンに実行させます。

これで JavaScript から Lisp コードを実行する方法がわかったわけですが、
Lisp コードを実行したいだけなら `jscl.evaluateString ...` と入力するの
は面倒ですね。

`repl-node.js` がそのためのプログラムです。

```
$ node repl-node.js
[...]
CL-USER> (+ 1 1)
2
CL-USER>
```

# REPLからLispファイルをコンパイルする

REPL から .lisp ファイルを .js にコンパイルするには LOAD 関数を使いま
す。変ですね。私も変だと思います。なぜ COMPILE-FILE 関数ではないのでしょ
うか？ コンパイルするのに実際にコードを実行しないと不都合があるのでしょ
うか。ともかく、LOAD 関数の :OUTPUT キーワード引数で出力ファイル名を指
定します。

以下の内容でディレクトリに hoge.lisp を用意しておきます。

```
(defun f ()
 (print "hello"))
```

```
$ node repl-node.js
[...]
CL-USER> (load "hoge.lisp" :output "hoge.js")

The bundle up 1 expressions into "hoge.js"
CL-USER>
```

hoge.js にコンパイルされました。このファイルは hoge.lisp をコンパイル
した内容しか含まないので、使うには jscl.js が同じディレクトリに存在す
る必要があります。また、Lisp 環境からこのファイルを読み込むことはでき
ません。(つまり、FASL のような使い方はできません。なぜ？)

なお、JSファイルへのコンパイルはブラウザからは行えず、`repl-node.js`
を使う必要があります。ブラウザ環境で動くプログラムは、ファイルの保存が
できないからです。

```
$ node
> require("./hoge.js")
[...]
{}
> require("./jscl.js").evaluateString("(f)")
"hello"


'hello'
>
```

hoge.js を読み込んだときに JSCL の起動メッセージが表示され、この時
jscl.js が読み込まれたことがわかります。2つ目の require は JSCL モジュー
ルにアクセスすることが目的で、モジュールは既に読み込まれているのでファ
イルのロードは行なわれません。きちんと hoge.js で定義した f 関数が使え
ることがわかります。

# なぜファイルをコンパイルするのか

理論的には jscl.js だけ JS 環境が読み込めばよくて、Lisp ファイルをロー
ドすることでユーザープログラムが実行できるのですが、JSCL が JavaScript
で動いている時、コンパイル処理は大変重いです。

ファイルが1000行もあれば分単位でロードに時間がかかります。

あなたの最新作のゲームをユーザーがプレイするのに、ブラウザでページを開
いてからゲームが動きだすまでに 1 分かかるのは、筋が通りません。

# ホスト環境でのコンパイル

遅い Lisp ソースのロードは、プログラムのディプロイ時において非現実的な
だけでなく、開発時にもストレスになり、プログラムを実行させたまま頻繁に
コードを変更して実験を重ねる Common Lisp の開発スタイルをほとんど不可
能にしてしまいます。

おそらく JSCL の出力する JavaScript があまり高速ではないことが原因で、
JavaScript で動く JSCL コンパイラは遅いのですが、SBCL などのホスト環境
で動くJSCL は少なくとも 1 桁は高速なように見えます。

実用的なディプロイ時の起動時間と開発時のイテレーション頻度を実現するに
は、ホスト環境での事前コンパイルが良さそうです。

# build-app.lisp

`build-app.lisp` をロードすると JSCL::MAKE という関数が定義されます。
MAKE は可変長引数の関数で、コンパイルするLispソースのファイル名を取り
ます。

例えば、次のようにすると JSCL モジュールと hoge.lisp の内容を含んだ
`main.js` が作成されます。

```
$ sbcl --load build-app.lisp --eval '(jscl::make "hoge.lisp")'
```

main.js は jscl.js と同様に Node.js 環境で require したり、ブラウザ環
境で script タグを使って読み込むことができます。
