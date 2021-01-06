#!/usr/bin/perl

use Cwd 'getcwd';
$pwd = getcwd;

# 処理系の指定
$latex = 'uplatex --halt-on-error --interaction=nonstopmode %O %S';
$dvipdf = 'dvipdfmx %O -o %D %S';
$bibtex = 'upbibtex %O %B';

$do_cd = 1; # ソースファイルのディレクトリにcdしてから処理する
$out_dir = "$pwd/target"; # 出力ディレクトリの指定
$pdf_previewer = 'open -g -a Skim'; # ビューアの指定