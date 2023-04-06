;;; skk-act.el --- 拡張ローマ字入力 "ACT" を SKK で使うための設定 -*- coding: utf-8 -*-

;; Copyright (C) 2003, 2007 IRIE Tetsuya <irie@t.email.ne.jp>

;; Author: IRIE Tetsuya <irie@t.email.ne.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; dvorak 配列での拡張ローマ字入力 "ACT" を SKK で使うための設定です．
;; "ACT" については，以下の URL を参照して下さい．
;;   http://www1.vecceed.ne.jp/~bemu/act/act_index.html

;; 使い方 - 下記の設定を .skk に加えてください．
;;          その後 Emacs(Mule) を再起動すれば skk による ACT での
;;          入力が可能です．

;;  (setq skk-use-act t)

;; 注意 1 - ACT では "q" を "おん" の入力に使うので，"q" のもともとの
;;          機能である `skk-toggle-characters' は "\" に割当てています．
;;          SKK 標準で "\" の `skk-input-by-code-or-menu' は割当てて
;;          いないのでマニュアルで呼出す必要があります．

;;      2 - 同様に "Q" も使用できませんので
;;          `skk-set-henkan-point-subr' は "|" に割当てています．

;;      3 - 純正の ACT では "la" で "ぁ" を入力します．しかし
;;          SKK では l を ASCII/かなモードの切り替えキーとして
;;          使用するので，"`a" で "ぁ" を入力できるようにしています．

;;      4 - SKK 標準の "z*" (「～」「…」など)，"x*" (「ぃ」「ゎ」な
;;          ど)は "`*" に割当てています．

;;      5 - デフォルトでは子音の後の "y" には2重母音の "ui" を割当て
;;          ているので，"y" を使った拗音の入力は無効です．"y" を使っ
;;          て拗音を入力したい場合は `skk-act-use-normal-y' を
;;          non-nil に設定して skk を起動して下さい．(skk-act をロー
;;          ドしたときの値が有効になります)

;;   キー割当て変更点
;;                                  SKK標準     ACT
;;  `skk-toggle-characters'        q         \
;;  `skk-set-henkan-point-subr'    Q         |
;;  `skk-input-by-code-or-menu'    \     割当てなし
;;  `skk-purge-from-jisyo'         X     割当てなし

;;; Code:

(require 'skk)

(eval-when-compile
  (defvar skk-jisx0201-rule-list)
  (defvar skk-jisx0201-base-rule-list))

(defvar skk-act-unnecessary-base-rule-list
  (let ((list
         `("bb" "cc" "dd" "ff" "gg" "jj" "kk" "pp" "rr" "ss" "tt" "vv"
           "ww" "xx" "yy" "zz"
           "cha" "che" "chi" "cho" "chu"
           "dha" "dhe" "dhi" "dho" "dhu"
           "ja" "je" "ji" "jo" "ju"
           "jya" "jye" "jyi" "jyo" "jyu"
           "ka" "ke" "ki" "ko" "ku"
           "kya" "kye" "kyi" "kyo" "kyu"
           "tsu"
           "tha" "the" "thi" "tho" "thu"
           "xa" "xe" "xi" "xo" "xu"
           "xka" "xke"
           "xtsu" "xtu"
           "xwa" "xwe" "xwi"
           "xya" "xyo" "xyu")))
    ;; skk-act-use-normal-y が nil であれば拗音も削除
    (unless skk-act-use-normal-y
      (setq list
            (append list
                    '("bya" "bye" "byi" "byo" "byu"
                      "cya" "cye" "cyi" "cyo" "cyu"
                      "dya" "dye" "dyi" "dyo" "dyu"
                      "fya" "fye" "fyi" "fyo" "fyu"
                      "gya" "gye" "gyi" "gyo" "gyu"
                      "hya" "hye" "hyi" "hyo" "hyu"
                      "mya" "mye" "myi" "myo" "myu"
                      "nya" "nye" "nyi" "nyo" "nyu"
                      "pya" "pye" "pyi" "pyo" "pyu"
                      "rya" "rye" "ryi" "ryo" "ryu"
                      "sya" "sye" "syi" "syo" "syu"
                      "tya" "tye" "tyi" "tyo" "tyu"
                      "zya" "zye" "zyi" "zyo" "zyu"))))
    list))

(defvar skk-act-additional-rom-kana-rule-list
  (let ((list
         '(("\\" nil skk-toggle-characters)
           ("|" nil skk-set-henkan-point-subr)
           ("`|" nil "|")
           ("'" nil ("ッ" . "っ"))
           ("`'" nil "'")
           ("`;" nil ";")
           ("`:" nil ":")
           ("`\"" nil "\"")
           ;; 標準の x* の置き換え
           ("`a" nil ("ァ" . "ぁ"))
           ("`i" nil ("ィ" . "ぃ"))
           ("`u" nil ("ゥ" . "ぅ"))
           ("`e" nil ("ェ" . "ぇ"))
           ("`o" nil ("ォ" . "ぉ"))
           ("`ca" nil ("ヵ" . "か"))
           ("`ce" nil ("ヶ" . "け"))
           ;;("`tsu" nil ("ッ" . "っ")) ; 「↑」と衝突するのでコメントアウト
           ;;("`tu" nil ("ッ" . "っ"))  ; 「↑」と衝突するのでコメントアウト
           ("`wa" nil ("ヮ" . "ゎ"))
           ("`we" nil ("ヱ" . "ゑ"))
           ("`wi" nil ("ヰ" . "ゐ"))
           ("`ya" nil ("ャ" . "ゃ"))
           ("`yo" nil ("ョ" . "ょ"))
           ("`yu" nil ("ュ" . "ゅ"))
           ;; ヤ行の互換キー
           ("`ys" nil ("ャ" . "ゃ"))    ; リファレンスにはなし
           ("`yn" nil ("ョ" . "ょ"))    ; リファレンスにはなし
           ("`yh" nil ("ュ" . "ゅ"))    ; リファレンスにはなし
           ;; 標準の z* の置き換え
           ("`," nil "‥")
           ("`-" nil "～")
           ("`." nil "…")
           ("`/" nil "・")
           ("`[" nil "『")
           ("`]" nil "』")
           ("`d" nil "←")
           ("`h" nil "↓")
           ("`t" nil "↑")
           ("`n" nil "→")
           ;; か行は c を使う
           ("ca" nil ("カ" . "か"))
           ("ci" nil ("キ" . "き"))
           ("cu" nil ("ク" . "く"))
           ("ce" nil ("ケ" . "け"))
           ("co" nil ("コ" . "こ"))
           ;; 清音，濁音，撥音拡張，二重母音拡張
           (";" nil ("アン" . "あん"))
           ("x" nil ("イン" . "いん"))
           ("k" nil ("ウン" . "うん"))
           ("j" nil ("エン" . "えん"))
           ("q" nil ("オン" . "おん"))
           ("c;" nil ("カン" . "かん"))
           ("cx" nil ("キン" . "きん"))
           ("ck" nil ("クン" . "くん"))
           ("cj" nil ("ケン" . "けん"))
           ("cq" nil ("コン" . "こん"))
           ("c'" nil ("カイ" . "かい"))
           ("cp" nil ("クウ" . "くう"))
           ("c." nil ("ケイ" . "けい"))
           ("c," nil ("コウ" . "こう"))
           ("s;" nil ("サン" . "さん"))
           ("sx" nil ("シン" . "しん"))
           ("sk" nil ("スン" . "すん"))
           ("sj" nil ("セン" . "せん"))
           ("sq" nil ("ソン" . "そん"))
           ("s'" nil ("サイ" . "さい"))
           ("sp" nil ("スウ" . "すう"))
           ("s." nil ("セイ" . "せい"))
           ("s," nil ("ソウ" . "そう"))
           ("t;" nil ("タン" . "たん"))
           ("tx" nil ("チン" . "ちん"))
           ("tk" nil ("ツン" . "つん"))
           ("tj" nil ("テン" . "てん"))
           ("tq" nil ("トン" . "とん"))
           ("t'" nil ("タイ" . "たい"))
           ("tp" nil ("ツウ" . "つう"))
           ("t." nil ("テイ" . "てい"))
           ("t," nil ("トウ" . "とう"))
           ("n;" nil ("ナン" . "なん"))
           ("nx" nil ("ニン" . "にん"))
           ("nk" nil ("ヌン" . "ぬん"))
           ("nj" nil ("ネン" . "ねん"))
           ("nq" nil ("ノン" . "のん"))
           ("n'" nil ("ナイ" . "ない"))
           ("np" nil ("ヌウ" . "ぬう"))
           ("n." nil ("ネイ" . "ねい"))
           ("n," nil ("ノウ" . "のう"))
           ("h;" nil ("ハン" . "はん"))
           ("hx" nil ("ヒン" . "ひん"))
           ("hk" nil ("フン" . "ふん"))
           ("hj" nil ("ヘン" . "へん"))
           ("hq" nil ("ホン" . "ほん"))
           ("h'" nil ("ハイ" . "はい"))
           ("hp" nil ("フウ" . "ふう"))
           ("h." nil ("ヘイ" . "へい"))
           ("h," nil ("ホウ" . "ほう"))
           ("m;" nil ("マン" . "まん"))
           ("mx" nil ("ミン" . "みん"))
           ("mk" nil ("ムン" . "むん"))
           ("mj" nil ("メン" . "めん"))
           ("mq" nil ("モン" . "もん"))
           ("m'" nil ("マイ" . "まい"))
           ("mp" nil ("ムウ" . "むう"))
           ("m." nil ("メイ" . "めい"))
           ("m," nil ("モウ" . "もう"))
           ("y;" nil ("ヤン" . "やん"))
           ;;("yx" nil ("イン" . "いん"))   ; リファレンスにはなし
           ("yk" nil ("ユン" . "ゆん"))
           ;;("yj" nil ("イェン" . "いぇん"))   ; リファレンスにはなし
           ("yq" nil ("ヨン" . "よん"))
           ("y'" nil ("ヤイ" . "やい"))
           ("yp" nil ("ユウ" . "ゆう"))
           ("y." nil ("イウ" . "いう"))
           ("y," nil ("ヨウ" . "よう"))
           ("r;" nil ("ラン" . "らん"))
           ("rx" nil ("リン" . "りん"))
           ("rk" nil ("ルン" . "るん"))
           ("rj" nil ("レン" . "れん"))
           ("rq" nil ("ロン" . "ろん"))
           ("r'" nil ("ライ" . "らい"))
           ("rp" nil ("ルウ" . "るう"))
           ("r." nil ("レイ" . "れい"))
           ("r," nil ("ロウ" . "ろう"))
           ("w;" nil ("ワン" . "わん"))
           ("wx" nil ("ウィン" . "うぃん"))
           ("wk" nil ("ウン" . "うん"))
           ("wj" nil ("ウェン" . "うぇん"))
           ("wq" nil ("ウォン" . "うぉん"))
           ("w'" nil ("ワイ" . "わい"))
           ("wp" nil ("ウゥ" . "うぅ"))
           ("w." nil ("ウェイ" . "うぇい"))
           ("w," nil ("ウォー" . "うぉー"))
           ("g;" nil ("ガン" . "がん"))
           ("gx" nil ("ギン" . "ぎん"))
           ("gk" nil ("グン" . "ぐん"))
           ("gj" nil ("ゲン" . "げん"))
           ("gq" nil ("ゴン" . "ごん"))
           ("g'" nil ("ガイ" . "がい"))
           ("gp" nil ("グウ" . "ぐう"))
           ("g." nil ("ゲイ" . "げい"))
           ("g," nil ("ゴウ" . "ごう"))
           ("z;" nil ("ザン" . "ざん"))
           ("zx" nil ("ジン" . "じん"))
           ("zk" nil ("ズン" . "ずん"))
           ("zj" nil ("ゼン" . "ぜん"))
           ("zq" nil ("ゾン" . "ぞん"))
           ("z'" nil ("ザイ" . "ざい"))
           ("zp" nil ("ズウ" . "ずう"))
           ("z." nil ("ゼイ" . "ぜい"))
           ("z," nil ("ゾウ" . "ぞう"))
           ("d;" nil ("ダン" . "だん"))
           ("dx" nil ("ヂン" . "ぢん"))
           ("dk" nil ("ヅン" . "づん"))
           ("dj" nil ("デン" . "でん"))
           ("dq" nil ("ドン" . "どん"))
           ("d'" nil ("ダイ" . "だい"))
           ("dp" nil ("ヅウ" . "づう"))
           ("d." nil ("デイ" . "でい"))
           ("d," nil ("ドウ" . "どう"))
           ("b;" nil ("バン" . "ばん"))
           ("bx" nil ("ビン" . "びん"))
           ("bk" nil ("ブン" . "ぶん"))
           ("bj" nil ("ベン" . "べん"))
           ("bq" nil ("ボン" . "ぼん"))
           ("b'" nil ("バイ" . "ばい"))
           ("bp" nil ("ブウ" . "ぶう"))
           ("b." nil ("ベイ" . "べい"))
           ("b," nil ("ボウ" . "ぼう"))
           ("p;" nil ("パン" . "ぱん"))
           ("px" nil ("ピン" . "ぴん"))
           ("pk" nil ("プン" . "ぷん"))
           ("pj" nil ("ペン" . "ぺん"))
           ("pq" nil ("ポン" . "ぽん"))
           ("p'" nil ("パイ" . "ぱい"))
           ("pp" nil ("プウ" . "ぷう"))
           ("p." nil ("ペイ" . "ぺい"))
           ("p," nil ("ポウ" . "ぽう"))
           ;; 拗音，撥音拡張，二重母音拡張
           ("cga" nil ("キャ" . "きゃ"))
           ("cgi" nil ("キィ" . "きぃ"))
           ("cgu" nil ("キュ" . "きゅ"))
           ("cge" nil ("キェ" . "きぇ"))
           ("cgo" nil ("キョ" . "きょ"))
           ("cg;" nil ("キャン" . "きゃん"))
           ("cgx" nil ("キィン" . "きぃん"))
           ("cgk" nil ("キュン" . "きゅん"))
           ("cgj" nil ("キェン" . "きぇん"))
           ("cgq" nil ("キョン" . "きょん"))
           ("cg'" nil ("キャイ" . "きゃい"))
           ("cgp" nil ("キュウ" . "きゅう"))
           ("cg." nil ("キェイ" . "きぇい"))
           ("cg," nil ("キョウ" . "きょう"))
           ;; 標準でこうなっているけど一応
           ("sha" nil ("シャ" . "しゃ"))
           ("shi" nil ("シィ" . "しぃ"))    ; リファレンスでは `し' だけど
           ("shu" nil ("シュ" . "しゅ"))
           ("she" nil ("シェ" . "しぇ"))
           ("sho" nil ("ショ" . "しょ"))
           ("sh;" nil ("シャン" . "しゃん"))
           ("shx" nil ("シィン" . "しぃん"))
           ("shk" nil ("シュン" . "しゅん"))
           ("shj" nil ("シェン" . "しぇん"))
           ("shq" nil ("ション" . "しょん"))
           ("sh'" nil ("シャイ" . "しゃい"))
           ("shp" nil ("シュウ" . "しゅう"))
           ("sh." nil ("シェイ" . "しぇい"))
           ("sh," nil ("ショウ" . "しょう"))
           ("tha" nil ("チャ" . "ちゃ"))
           ("thi" nil ("チィ" . "ちぃ"))
           ("thu" nil ("チュ" . "ちゅ"))
           ("the" nil ("チェ" . "ちぇ"))
           ("tho" nil ("チョ" . "ちょ"))
           ("th;" nil ("チャン" . "ちゃん"))
           ("thx" nil ("チィン" . "ちぃん"))
           ("thk" nil ("チュン" . "ちゅん"))
           ("thj" nil ("チェン" . "ちぇん"))
           ("thq" nil ("チョン" . "ちょん"))
           ("th'" nil ("チャイ" . "ちゃい"))
           ("thp" nil ("チュウ" . "ちゅう"))
           ("th." nil ("チェイ" . "ちぇい"))
           ("th," nil ("チョウ" . "ちょう"))
           ("nha" nil ("ニャ" . "にゃ"))
           ("nhi" nil ("ニィ" . "にぃ"))
           ("nhu" nil ("ニュ" . "にゅ"))
           ("nhe" nil ("ニェ" . "にぇ"))
           ("nho" nil ("ニョ" . "にょ"))
           ("nh;" nil ("ニャン" . "にゃん"))
           ("nhx" nil ("ニィン" . "にぃん"))
           ("nhk" nil ("ニュン" . "にゅん"))
           ("nhj" nil ("ニェン" . "にぇん"))
           ("nhq" nil ("ニョン" . "にょん"))
           ("nh'" nil ("ニャイ" . "にゃい"))
           ("nhp" nil ("ニュウ" . "にゅう"))
           ("nh." nil ("ニェイ" . "にぇい"))
           ("nh," nil ("ニョウ" . "にょう"))
           ("hna" nil ("ヒャ" . "ひゃ"))
           ("hni" nil ("ヒィ" . "ひぃ"))
           ("hnu" nil ("ヒュ" . "ひゅ"))
           ("hne" nil ("ヒェ" . "ひぇ"))
           ("hno" nil ("ヒョ" . "ひょ"))
           ("hn;" nil ("ヒャン" . "ひゃん"))
           ("hnx" nil ("ヒィン" . "ひぃん"))
           ("hnk" nil ("ヒュン" . "ひゅん"))
           ("hnj" nil ("ヒェン" . "ひぇん"))
           ("hnq" nil ("ヒョン" . "ひょん"))
           ("hn'" nil ("ヒャイ" . "ひゃい"))
           ("hnp" nil ("ヒュウ" . "ひゅう"))
           ("hn." nil ("ヒェイ" . "ひぇい"))
           ("hn," nil ("ヒョウ" . "ひょう"))
           ("mva" nil ("ミャ" . "みゃ"))
           ("mvi" nil ("ミィ" . "みぃ"))
           ("mvu" nil ("ミュ" . "みゅ"))
           ("mve" nil ("ミェ" . "みぇ"))
           ("mvo" nil ("ミョ" . "みょ"))
           ("mv;" nil ("ミャン" . "みゃん"))
           ("mvx" nil ("ミィン" . "みぃん"))
           ("mvk" nil ("ミュン" . "みゅん"))
           ("mvj" nil ("ミェン" . "みぇん"))
           ("mvq" nil ("ミョン" . "みょん"))
           ("mv'" nil ("ミャイ" . "みゃい"))
           ("mvp" nil ("ミュウ" . "みゅう"))
           ("mv." nil ("ミェイ" . "みぇい"))
           ("mv," nil ("ミョウ" . "みょう"))
           ("rga" nil ("リャ" . "りゃ"))
           ("rgi" nil ("リィ" . "りぃ"))
           ("rgu" nil ("リュ" . "りゅ"))
           ("rge" nil ("リェ" . "りぇ"))
           ("rgo" nil ("リョ" . "りょ"))
           ("rg;" nil ("リャン" . "りゃん"))
           ("rgx" nil ("リィン" . "りぃん"))
           ("rgk" nil ("リュン" . "りゅん"))
           ("rgj" nil ("リェン" . "りぇん"))
           ("rgq" nil ("リョン" . "りょん"))
           ("rg'" nil ("リャイ" . "りゃい"))
           ("rgp" nil ("リュウ" . "りゅう"))
           ("rg." nil ("リェイ" . "りぇい"))
           ("rg," nil ("リョウ" . "りょう"))
           ("gra" nil ("ギャ" . "ぎゃ"))
           ("gri" nil ("ギィ" . "ぎぃ"))
           ("gru" nil ("ギュ" . "ぎゅ"))
           ("gre" nil ("ギェ" . "ぎぇ"))
           ("gro" nil ("ギョ" . "ぎょ"))
           ("gr;" nil ("ギャン" . "ぎゃん"))
           ("grx" nil ("ギィン" . "ぎぃん"))
           ("grk" nil ("ギュン" . "ぎゅん"))
           ("grj" nil ("ギェン" . "ぎぇん"))
           ("grq" nil ("ギョン" . "ぎょん"))
           ("gr'" nil ("ギャイ" . "ぎゃい"))
           ("grp" nil ("ギュウ" . "ぎゅう"))
           ("gr." nil ("ギェイ" . "ぎぇい"))
           ("gr," nil ("ギョウ" . "ぎょう"))
           ("zma" nil ("ジャ" . "じゃ"))
           ("zmi" nil ("ジィ" . "じぃ"))
           ("zmu" nil ("ジュ" . "じゅ"))
           ("zme" nil ("ジェ" . "じぇ"))
           ("zmo" nil ("ジョ" . "じょ"))
           ("zm;" nil ("ジャン" . "じゃん"))
           ("zmx" nil ("ジィン" . "じぃん"))
           ("zmk" nil ("ジュン" . "じゅん"))
           ("zmj" nil ("ジェン" . "じぇん"))
           ("zmq" nil ("ジョン" . "じょん"))
           ("zm'" nil ("ジャイ" . "じゃい"))
           ("zmp" nil ("ジュウ" . "じゅう"))
           ("zm." nil ("ジェイ" . "じぇい"))
           ("zm," nil ("ジョウ" . "じょう"))
           ("dna" nil ("ヂャ" . "ぢゃ"))
           ("dni" nil ("ヂィ" . "ぢぃ"))
           ("dnu" nil ("ヂュ" . "ぢゅ"))
           ("dne" nil ("ヂェ" . "ぢぇ"))
           ("dno" nil ("ヂョ" . "ぢょ"))
           ("dn;" nil ("ヂャン" . "ぢゃん"))
           ("dnx" nil ("ヂィン" . "ぢぃん"))
           ("dnk" nil ("ヂュン" . "ぢゅん"))
           ("dnj" nil ("ヂェン" . "ぢぇん"))
           ("dnq" nil ("ヂョン" . "ぢょん"))
           ("dn'" nil ("ヂャイ" . "ぢゃい"))
           ("dnp" nil ("ヂュウ" . "ぢゅう"))
           ("dn." nil ("ヂェイ" . "ぢぇい"))
           ("dn," nil ("ヂョウ" . "ぢょう"))
           ("bva" nil ("ビャ" . "びゃ"))
           ("bvi" nil ("ビィ" . "びぃ"))
           ("bvu" nil ("ビュ" . "びゅ"))
           ("bve" nil ("ビェ" . "びぇ"))
           ("bvo" nil ("ビョ" . "びょ"))
           ("bv;" nil ("ビャン" . "びゃん"))
           ("bvx" nil ("ビィン" . "びぃん"))
           ("bvk" nil ("ビュン" . "びゅん"))
           ("bvj" nil ("ビェン" . "びぇん"))
           ("bvq" nil ("ビョン" . "びょん"))
           ("bv'" nil ("ビャイ" . "びゃい"))
           ("bvp" nil ("ビュウ" . "びゅう"))
           ("bv." nil ("ビェイ" . "びぇい"))
           ("bv," nil ("ビョウ" . "びょう"))
           ("pna" nil ("ピャ" . "ぴゃ"))
           ("pni" nil ("ピィ" . "ぴぃ"))
           ("pnu" nil ("ピュ" . "ぴゅ"))
           ("pne" nil ("ピェ" . "ぴぇ"))
           ("pno" nil ("ピョ" . "ぴょ"))
           ("pn;" nil ("ピャン" . "ぴゃん"))
           ("pnx" nil ("ピィン" . "ぴぃん"))
           ("pnk" nil ("ピュン" . "ぴゅん"))
           ("pnj" nil ("ピェン" . "ぴぇん"))
           ("pnq" nil ("ピョン" . "ぴょん"))
           ("pn'" nil ("ピャイ" . "ぴゃい"))
           ("pnp" nil ("ピュウ" . "ぴゅう"))
           ("pn." nil ("ピェイ" . "ぴぇい"))
           ("pn," nil ("ピョウ" . "ぴょう"))
           ;; 拗音(2ストローク系)，撥音拡張，二重母音拡張
           ("f;" nil ("ファン" . "ふぁん"))
           ("fx" nil ("フィン" . "ふぃん"))
           ("fk" nil ("フン" . "ふん"))
           ("fj" nil ("フェン" . "ふぇん"))
           ("fq" nil ("フォン" . "ふぉん"))
           ("f'" nil ("ファイ" . "ふぁい"))
           ("fp" nil ("フウ" . "ふう"))
           ("f." nil ("フェイ" . "ふぇい"))
           ("f," nil ("フォー" . "ふぉー"))
           ("v;" nil ("ヴァン" . "う゛ぁん"))
           ("vx" nil ("ヴィン" . "う゛ぃん"))
           ("vk" nil ("ヴン" . "う゛ん"))
           ("vj" nil ("ヴェン" . "う゛ぇん"))
           ("vq" nil ("ヴォン" . "う゛ぉん"))
           ("v'" nil ("ヴァイ" . "う゛ぁい"))
           ("vp" nil ("ヴー" . "う゛ー"))
           ("v." nil ("ヴェイ" . "う゛ぇい"))
           ("v," nil ("ヴォー" . "う゛ぉー"))
           ;; 頻出拗音の省略打ち
           ("pc" nil ("ピュウ" . "ぴゅう"))
           ("pl" nil ("ピョウ" . "ぴょう"))
           ("fc" nil ("フュー" . "ふゅー"))
           ("fl" nil ("フォー" . "ふぉー"))
           ("gc" nil ("ギュウ" . "ぎゅう"))
           ("gl" nil ("ギョウ" . "ぎょう"))
           ("cc" nil ("キュウ" . "きゅう"))
           ("cl" nil ("キョウ" . "きょう"))
           ("rc" nil ("リュウ" . "りゅう"))
           ("rl" nil ("リョウ" . "りょう"))
           ("ht" nil ("ヒュウ" . "ひゅう"))
           ("hs" nil ("ヒョウ" . "ひょう"))
           ("tt" nil ("チュウ" . "ちゅう"))
           ("ts" nil ("チョウ" . "ちょう"))
           ("nt" nil ("ニュウ" . "にゅう"))
           ("ns" nil ("ニョウ" . "にょう"))
           ("st" nil ("シュウ" . "しゅう"))
           ("ss" nil ("ショウ" . "しょう"))
           ("bw" nil ("ビュウ" . "びゅう"))
           ("bz" nil ("ビョウ" . "びょう"))
           ("mw" nil ("ミュー" . "みゅー"))
           ("mz" nil ("ミョウ" . "みょう"))
           ("wz" nil ("ウォー" . "うぉー"))
           ("vw" nil ("ヴュー" . "う゛ゅー"))
           ("vz" nil ("ヴォー" . "う゛ぉー"))
           ("zw" nil ("ジュウ" . "じゅう"))
           ("zz" nil ("ジョウ" . "じょう"))
           ;; 拗音＋ク・ツの省略打ち
           ("grr" nil ("ギョク" . "ぎょく"))
           ("grl" nil ("ギャク" . "ぎゃく"))
           ("cgr" nil ("キョク" . "きょく"))
           ("cgl" nil ("キャク" . "きゃく"))
           ("rgr" nil ("リョク" . "りょく"))
           ("rgl" nil ("リャク" . "りゃく"))
           ("hns" nil ("ヒャク" . "ひゃく"))
           ("thn" nil ("チョク" . "ちょく"))
           ("ths" nil ("チャク" . "ちゃく"))
           ("nhn" nil ("ニョク" . "にょく"))
           ("nhs" nil ("ニャク" . "にゃく"))
           ("shn" nil ("ショク" . "しょく"))
           ("shs" nil ("シャク" . "しゃく"))
           ("sht" nil ("シュツ" . "しゅつ"))
           ("pns" nil ("ピャク" . "ぴゃく"))
           ("bvv" nil ("ビョク" . "びょく"))
           ("bvz" nil ("ビャク" . "びゃく"))
           ("mvv" nil ("ミョク" . "みょく"))
           ("mvz" nil ("ミャク" . "みゃく"))
           ("zmv" nil ("ジョク" . "じょく"))
           ("zmz" nil ("ジャク" . "じゃく"))
           ("zmw" nil ("ジュツ" . "じゅつ"))
           ("shh" nil ("シュク" . "しゅく"))
           ("zmm" nil ("ジュク" . "じゅく"))
           ;; ヤ行の互換キー
           ("yh" nil ("ユ" . "ゆ"))
           ("yg" nil ("ユウ" . "ゆう"))
           ("yz" nil ("ヤン" . "やん"))
           ("ym" nil ("ユン" . "ゆん"))
           ("yv" nil ("ヨン" . "よん"))
           ;; パ行の互換キー
           ("ps" nil ("パ" . "ぱ"))
           ("pd" nil ("ピ" . "ぴ"))
           ("ph" nil ("プ" . "ぷ"))
           ("pt" nil ("ペ" . "ぺ"))
           ("pz" nil ("パン" . "ぱん"))
           ("pb" nil ("ピン" . "ぴん"))
           ("pm" nil ("プン" . "ぷん"))
           ("pw" nil ("ペン" . "ぺん"))
           ("pv" nil ("ポン" . "ぽん"))
           ;; ヤ行頻出文字列の省略打ち
           ("yy" nil ("イウ" . "いう"))
           ("yf" nil ("ヨリ" . "より"))
           ("yc" nil ("イウ" . "いう"))
           ("yr" nil ("ヨル" . "よる"))
           ("yl" nil ("ヤル" . "やる"))
           ("yd" nil ("ヨイ" . "よい"))
           ("yt" nil ("ヨッテ" . "よって"))
           ("yn" nil ("ヨク" . "よく"))
           ("ys" nil ("ヤク" . "やく"))
           ("yb" nil ("ユビ" . "ゆび"))
           ("yw" nil ("イワレ" . "いわれ"))
           ;; その他の頻出文字列の省略打ち
           ("ff" nil ("フリ" . "ふり"))
           ("fg" nil ("フル" . "ふる"))
           ("fr" nil ("フル" . "ふる"))
           ("fn" nil ("ファン" . "ふぁん"))
           ("fm" nil ("フム" . "ふむ"))
           ("gt" nil ("ゴト" . "ごと"))
           ("gn" nil ("ゴク" . "ごく"))
           ("gs" nil ("ガク" . "がく"))
           ("cr" nil ("カラ" . "から"))
           ("cd" nil ("カタ" . "かた"))
           ("ct" nil ("コト" . "こと"))
           ("cb" nil ("カンガエ" . "かんがえ"))
           ("cn" nil ("コク" . "こく"))
           ("cs" nil ("カク" . "かく"))
           ("rr" nil ("ラレ" . "られ"))
           ("rn" nil ("ラン" . "らん"))
           ("dg" nil ("ダガ" . "だが"))
           ("dc" nil ("デキ" . "でき"))
           ("dr" nil ("デアル" . "である"))
           ("dl" nil ("デショウ" . "でしょう"))
           ("dd" nil ("ノデ" . "ので"))
           ("dt" nil ("ダチ" . "だち"))
           ("ds" nil ("デス" . "です"))
           ("dm" nil ("デモ" . "でも"))
           ("hg" nil ("フル" . "ふる"))
           ("hc" nil ("ヒュウ" . "ひゅう"))
           ("hr" nil ("ヒトリ" . "ひとり"))
           ("hl" nil ("ヒョウ" . "ひょう"))
           ("hd" nil ("ホド" . "ほど"))
           ("hh" nil ("ヒト" . "ひと"))
           ("hz" nil ("ヒジョウ" . "ひじょう"))
           ("tf" nil ("トリ" . "とり"))
           ("tg" nil ("トシテ" . "として"))
           ("tc" nil ("ツイテ" . "ついて"))
           ("tr" nil ("トコロ" . "ところ"))
           ("tl" nil ("トク" . "とく"))
           ("td" nil ("トイウ" . "という"))
           ("tn" nil ("トノ" . "との"))
           ("tb" nil ("タビ" . "たび"))
           ("tm" nil ("タメ" . "ため"))
           ("tv" nil ("トキ" . "とき"))
           ("tz" nil ("テキ" . "てき"))
           ("nf" nil ("ナリ" . "なり"))
           ("nc" nil ("ニツイテ" . "について"))
           ("nr" nil ("ナル" . "なる"))
           ("nl" nil ("ナッタ" . "なった"))
           ("nd" nil ("ナド" . "など"))
           ("nb" nil ("ナケレバ" . "なければ"))
           ("nm" nil ("ナクテモ" . "なくても"))
           ("nw" nil ("ナクテハ" . "なくては"))
           ("nz" nil ("ナク" . "なく"))
           ("sf" nil ("サリ" . "さり"))
           ("sg" nil ("サレ" . "され"))
           ("sc" nil ("シタ" . "した"))
           ("sr" nil ("スル" . "する"))
           ("sd" nil ("サレ" . "され"))
           ("sm" nil ("シモ" . "しも"))
           ("snb" nil ("シナケレバ" . "しなければ"))
           ("snm" nil ("シナクテモ" . "しなくても"))
           ("snt" nil ("シナクテ" . "しなくて"))
           ("snw" nil ("シナクテハ" . "しなくては"))
           ("sz" nil ("ソレゾレ" . "それぞれ"))
           ("bc" nil ("ビュウ" . "びゅう"))
           ("br" nil ("バラ" . "ばら"))
           ("bl" nil ("ビョウ" . "びょう"))
           ("bh" nil ("ブツ" . "ぶつ"))
           ("bt" nil ("ベツ" . "べつ"))
           ("mc" nil ("ミュー" . "みゅー"))
           ("mr" nil ("マル" . "まる"))
           ("ml" nil ("ミョウ" . "みょう"))
           ("md" nil ("マデ" . "まで"))
           ("mt" nil ("マタ" . "また"))
           ("mn" nil ("モノ" . "もの"))
           ("ms" nil ("マス" . "ます"))
           ("mm" nil ("オモ" . "おも"))
           ("wr" nil ("ワレ" . "われ"))
           ("wt" nil ("ワタシ" . "わたし"))
           ("wn" nil ("ワレワレ" . "われわれ"))
           ("vm" nil ("コトナ" . "ことな"))
           ("vv" nil ("オナジ" . "おなじ"))
           ("zc" nil ("ジュウ" . "じゅう"))
           ("zr" nil ("ザル" . "ざる"))
           ("zt" nil ("ズツ" . "ずつ"))
           ("zn" nil ("ゾク" . "ぞく"))
           ("zs" nil ("ザク" . "ざく"))
           ("pf" nil ("プリ" . "ぷり"))
           ("pg" nil ("プル" . "ぷる"))
           ("pr" nil ("プロ" . "ぷろ"))
           ;; 拗音の打ち方(外来語)
           ("twa" nil ("テャ" . "てゃ"))
           ("twi" nil ("ティ" . "てぃ"))
           ("twu" nil ("テュ" . "てゅ"))
           ("twe" nil ("テェ" . "てぇ"))
           ("two" nil ("テョ" . "てょ"))
           ("tw;" nil ("テャン" . "てゃん"))
           ("twx" nil ("ティン" . "てぃん"))
           ("twk" nil ("テュン" . "てゅん"))
           ("twj" nil ("テェン" . "てぇん"))
           ("twq" nil ("テョン" . "てょん"))
           ("tw'" nil ("テャウ" . "てゃう"))
           ("twp" nil ("テュウ" . "てゅう"))
           ("tw." nil ("テェイ" . "てぇい"))
           ("tw," nil ("テョウ" . "てょう"))
           ("dba" nil ("デャ" . "でゃ"))
           ("dbi" nil ("ディ" . "でぃ"))
           ("dbu" nil ("デュ" . "でゅ"))
           ("dbe" nil ("デェ" . "でぇ"))
           ("dbo" nil ("デョ" . "でょ"))
           ("db;" nil ("デャン" . "でゃん"))
           ("dbx" nil ("ディン" . "でぃん"))
           ("dbk" nil ("デュン" . "でゅん"))
           ("dbj" nil ("デェン" . "でぇん"))
           ("dbq" nil ("デョン" . "でょん"))
           ("db'" nil ("デャウ" . "でゃう"))
           ("dbp" nil ("デュウ" . "でゅう"))
           ("db." nil ("デェイ" . "でぇい"))
           ("db," nil ("デョウ" . "でょう"))
           ("wma" nil ("ウァ" . "うぁ"))
           ("wmi" nil ("ウィ" . "うぃ"))
           ("wmu" nil ("ウゥ" . "うぅ"))
           ("wme" nil ("ウェ" . "うぇ"))
           ("wmo" nil ("ウォ" . "うぉ"))
           ("wm;" nil ("ウァン" . "うぁん"))
           ("wmx" nil ("ウィン" . "うぃん"))
           ("wmk" nil ("ウゥン" . "うぅん"))
           ("wmj" nil ("ウェン" . "うぇん"))
           ("wmq" nil ("ウォン" . "うぉん"))
           ("wm'" nil ("ウァウ" . "うぁう"))
           ("wmp" nil ("ウゥウ" . "うぅう"))
           ("wm." nil ("ウェイ" . "うぇい"))
           ("wm," nil ("ウォウ" . "うぉう")))))
    (unless skk-act-use-normal-y
      (setq list
            (append list
                    '(("cy" nil ("クイ" . "くい"))
                      ("sy" nil ("スイ" . "すい"))
                      ("ty" nil ("ツイ" . "つい"))
                      ("ny" nil ("ヌイ" . "ぬい"))
                      ("hy" nil ("フイ" . "ふい"))
                      ("my" nil ("ムイ" . "むい"))
                      ("yy" nil ("ユイ" . "ゆい"))
                      ("ry" nil ("ルイ" . "るい"))
                      ("wy" nil ("ウイ" . "うい"))
                      ("gy" nil ("グイ" . "ぐい"))
                      ("zy" nil ("ズイ" . "ずい"))
                      ("dy" nil ("ヅイ" . "づい"))
                      ("by" nil ("ブイ" . "ぶい"))
                      ("py" nil ("プイ" . "ぷい"))
                      ("cgy" nil ("キュイ" . "きゅい"))
                      ("shy" nil ("シュイ" . "しゅい"))
                      ("thy" nil ("チュイ" . "ちゅい"))
                      ("nhy" nil ("ニュイ" . "にゅい"))
                      ("hny" nil ("ヒュイ" . "ひゅい"))
                      ("mvy" nil ("ミュイ" . "みゅい"))
                      ("rgy" nil ("リュイ" . "りゅい"))
                      ("gry" nil ("ギュイ" . "ぎゅい"))
                      ("zmy" nil ("ジュイ" . "じゅい"))
                      ("dny" nil ("ヂュイ" . "ぢゅい"))
                      ("bvy" nil ("ビュイ" . "びゅい"))
                      ("pny" nil ("ピュイ" . "ぴゅい"))
                      ("fy" nil ("フイ" . "ふい"))  ; リファレンスにはなし
                      ("vy" nil ("ヴイ" . "う゛い"))    ; リファレンスにはなし
                      ("twy" nil ("テュイ" . "てゅい"))
                      ("dby" nil ("デュイ" . "でゅい"))
                      ("wmy" nil ("ウゥイ" . "うぅい"))))))
    ;; shift を押したままの二重母音拡張
    ;; `skk-special-midashi-char-list' に
    ;; < > が無い場合のみ追加する
    (unless (memq ?< skk-special-midashi-char-list)
      (setq list
            (append list
                    '(("c<" nil ("コウ" . "こう"))
                      ("s<" nil ("ソウ" . "そう"))
                      ("t<" nil ("トウ" . "とう"))
                      ("n<" nil ("ノウ" . "のう"))
                      ("h<" nil ("ホウ" . "ほう"))
                      ("m<" nil ("モウ" . "もう"))
                      ("y<" nil ("ヨウ" . "よう"))
                      ("r<" nil ("ロウ" . "ろう"))
                      ("w<" nil ("ウォー" . "うぉー"))
                      ("g<" nil ("ゴウ" . "ごう"))
                      ("z<" nil ("ゾウ" . "ぞう"))
                      ("d<" nil ("ドウ" . "どう"))
                      ("b<" nil ("ボウ" . "ぼう"))
                      ("p<" nil ("ポウ" . "ぽう"))
                      ("cg<" nil ("キョウ" . "きょう"))
                      ("sh<" nil ("ショウ" . "しょう"))
                      ("th<" nil ("チョウ" . "ちょう"))
                      ("nh<" nil ("ニョウ" . "にょう"))
                      ("hn<" nil ("ヒョウ" . "ひょう"))
                      ("mv<" nil ("ミョウ" . "みょう"))
                      ("rg<" nil ("リョウ" . "りょう"))
                      ("gr<" nil ("ギョウ" . "ぎょう"))
                      ("zm<" nil ("ジョウ" . "じょう"))
                      ("dn<" nil ("ヂョウ" . "ぢょう"))
                      ("bv<" nil ("ビョウ" . "びょう"))
                      ("pn<" nil ("ピョウ" . "ぴょう"))
                      ("f<" nil ("フォー" . "ふぉー"))
                      ("v<" nil ("ヴォー" . "う゛ぉー"))
                      ("tw<" nil ("テョウ" . "てょう"))
                      ("db<" nil ("デョウ" . "でょう"))
                      ("wm<" nil ("ウォウ" . "うぉう"))))))
    (unless (memq ?> skk-special-midashi-char-list)
      (setq list
            (append list
                    '(("c>" nil ("ケイ" . "けい"))
                      ("s>" nil ("セイ" . "せい"))
                      ("t>" nil ("テイ" . "てい"))
                      ("n>" nil ("ネイ" . "ねい"))
                      ("h>" nil ("ヘイ" . "へい"))
                      ("m>" nil ("メイ" . "めい"))
                      ("y>" nil ("イウ" . "いう"))
                      ("r>" nil ("レイ" . "れい"))
                      ("w>" nil ("ウェイ" . "うぇい"))
                      ("g>" nil ("ゲイ" . "げい"))
                      ("z>" nil ("ゼイ" . "ぜい"))
                      ("d>" nil ("デイ" . "でい"))
                      ("b>" nil ("ベイ" . "べい"))
                      ("p>" nil ("ペイ" . "ぺい"))
                      ("cg>" nil ("キェイ" . "きぇい"))
                      ("sh>" nil ("シェイ" . "しぇい"))
                      ("th>" nil ("チェイ" . "ちぇい"))
                      ("nh>" nil ("ニェイ" . "にぇい"))
                      ("hn>" nil ("ヒェイ" . "ひぇい"))
                      ("mv>" nil ("ミェイ" . "みぇい"))
                      ("rg>" nil ("リェイ" . "りぇい"))
                      ("gr>" nil ("ギェイ" . "ぎぇい"))
                      ("zm>" nil ("ジェイ" . "じぇい"))
                      ("dn>" nil ("ヂェイ" . "ぢぇい"))
                      ("bv>" nil ("ビェイ" . "びぇい"))
                      ("pn>" nil ("ピェイ" . "ぴぇい"))
                      ("f>" nil ("フェイ" . "ふぇい"))
                      ("v>" nil ("ヴェイ" . "う゛ぇい"))
                      ("tw>" nil ("テェイ" . "てぇい"))
                      ("db>" nil ("デェイ" . "でぇい"))
                      ("wm>" nil ("ウェイ" . "うぇい"))))))
    list))

;; " : は ' ; として変換させる
(setq skk-downcase-alist
      (append skk-downcase-alist '((?\" . ?\') (?: . ?\;))))

;; '「っ」 ;「あん」 Q「おん」 X「いん」 を変換ポイントに加える
(setq skk-set-henkan-point-key
      (append skk-set-henkan-point-key '(?\" ?: ?Q ?X)))

;; skk-rom-kana-base-rule-list から変換規則を削除する
(dolist (str skk-act-unnecessary-base-rule-list)
  (setq skk-rom-kana-base-rule-list
        (skk-del-alist str skk-rom-kana-base-rule-list)))

;; skk-rom-kana-rule-list から変換規則を削除する
(let ((del-list '("hh" "mm")))
  (dolist (str del-list)
    (setq skk-rom-kana-rule-list
          (skk-del-alist str skk-rom-kana-rule-list))))

;; ACT 特有の変換規則を追加する
(dolist (rule skk-act-additional-rom-kana-rule-list)
  (add-to-list 'skk-rom-kana-rule-list rule))

;; for jisx0201
(eval-after-load "skk-jisx0201"
  '(progn
     (dolist (str skk-act-unnecessary-base-rule-list)
       (setq skk-jisx0201-base-rule-list
             (skk-del-alist str skk-jisx0201-base-rule-list)))

     (let ((del-list '("hh" "mm")))
       (dolist (str del-list)
         (setq skk-jisx0201-base-rule-list
               (skk-del-alist str skk-jisx0201-base-rule-list))))

     (dolist (rule skk-act-additional-rom-kana-rule-list)
       (add-to-list 'skk-jisx0201-rule-list
                    (if (listp (nth 2 rule))
                        (list (nth 0 rule) (nth 1 rule)
                              (japanese-hankaku (car (nth 2 rule))))
                      rule)))

     (setq skk-jisx0201-base-rule-tree
           (skk-compile-rule-list skk-jisx0201-base-rule-list
                                  skk-jisx0201-rule-list))))

(run-hooks 'skk-act-load-hook)

(provide 'skk-act)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-act.el ends here
