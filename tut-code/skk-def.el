;;; skk-def.el --- SKK default definition -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
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

;; this file is to restore default setting for SKK.

;;; Code:

(require 'skk-vars)

(setq skk-start-henkan-char ?\040)

(setq skk-rom-kana-base-rule-list
      '(("a" nil ("ア" . "あ"))
        ("bb" "b" ("ッ" . "っ"))
        ("ba" nil ("バ" . "ば"))
        ("be" nil ("ベ" . "べ"))
        ("bi" nil ("ビ" . "び"))
        ("bo" nil ("ボ" . "ぼ"))
        ("bu" nil ("ブ" . "ぶ"))
        ("bya" nil ("ビャ" . "びゃ"))
        ("bye" nil ("ビェ" . "びぇ"))
        ("byi" nil ("ビィ" . "びぃ"))
        ("byo" nil ("ビョ" . "びょ"))
        ("byu" nil ("ビュ" . "びゅ"))
        ("cc" "c" ("ッ" . "っ"))
        ("cha" nil ("チャ" . "ちゃ"))
        ("che" nil ("チェ" . "ちぇ"))
        ("chi" nil ("チ" . "ち"))
        ("cho" nil ("チョ" . "ちょ"))
        ("chu" nil ("チュ" . "ちゅ"))
        ("cya" nil ("チャ" . "ちゃ"))
        ("cye" nil ("チェ" . "ちぇ"))
        ("cyi" nil ("チィ" . "ちぃ"))
        ("cyo" nil ("チョ" . "ちょ"))
        ("cyu" nil ("チュ" . "ちゅ"))
        ("dd" "d" ("ッ" . "っ"))
        ("da" nil ("ダ" . "だ"))
        ("de" nil ("デ" . "で"))
        ("dha" nil ("デャ" . "でゃ"))
        ("dhe" nil ("デェ" . "でぇ"))
        ("dhi" nil ("ディ" . "でぃ"))
        ("dho" nil ("デョ" . "でょ"))
        ("dhu" nil ("デュ" . "でゅ"))
        ("di" nil ("ヂ" . "ぢ"))
        ("do" nil ("ド" . "ど"))
        ("du" nil ("ヅ" . "づ"))
        ("dya" nil ("ヂャ" . "ぢゃ"))
        ("dye" nil ("ヂェ" . "ぢぇ"))
        ("dyi" nil ("ヂィ" . "ぢぃ"))
        ("dyo" nil ("ヂョ" . "ぢょ"))
        ("dyu" nil ("ヂュ" . "ぢゅ"))
        ("e" nil ("エ" . "え"))
        ("ff" "f" ("ッ" . "っ"))
        ("fa" nil ("ファ" . "ふぁ"))
        ("fe" nil ("フェ" . "ふぇ"))
        ("fi" nil ("フィ" . "ふぃ"))
        ("fo" nil ("フォ" . "ふぉ"))
        ("fu" nil ("フ" . "ふ"))
        ("fya" nil ("フャ" . "ふゃ"))
        ("fye" nil ("フェ" . "ふぇ"))
        ("fyi" nil ("フィ" . "ふぃ"))
        ("fyo" nil ("フョ" . "ふょ"))
        ("fyu" nil ("フュ" . "ふゅ"))
        ("gg" "g" ("ッ" . "っ"))
        ("ga" nil ("ガ" . "が"))
        ("ge" nil ("ゲ" . "げ"))
        ("gi" nil ("ギ" . "ぎ"))
        ("go" nil ("ゴ" . "ご"))
        ("gu" nil ("グ" . "ぐ"))
        ("gya" nil ("ギャ" . "ぎゃ"))
        ("gye" nil ("ギェ" . "ぎぇ"))
        ("gyi" nil ("ギィ" . "ぎぃ"))
        ("gyo" nil ("ギョ" . "ぎょ"))
        ("gyu" nil ("ギュ" . "ぎゅ"))
        ;;("h" "" ("オ" . "お"))
        ("ha" nil ("ハ" . "は"))
        ("he" nil ("ヘ" . "へ"))
        ("hi" nil ("ヒ" . "ひ"))
        ("ho" nil ("ホ" . "ほ"))
        ("hu" nil ("フ" . "ふ"))
        ("hya" nil ("ヒャ" . "ひゃ"))
        ("hye" nil ("ヒェ" . "ひぇ"))
        ("hyi" nil ("ヒィ" . "ひぃ"))
        ("hyo" nil ("ヒョ" . "ひょ"))
        ("hyu" nil ("ヒュ" . "ひゅ"))
        ("i" nil ("イ" . "い"))
        ("jj" "j" ("ッ" . "っ"))
        ("ja" nil ("ジャ" . "じゃ"))
        ("je" nil ("ジェ" . "じぇ"))
        ("ji" nil ("ジ" . "じ"))
        ("jo" nil ("ジョ" . "じょ"))
        ("ju" nil ("ジュ" . "じゅ"))
        ("jya" nil ("ジャ" . "じゃ"))
        ("jye" nil ("ジェ" . "じぇ"))
        ("jyi" nil ("ジィ" . "じぃ"))
        ("jyo" nil ("ジョ" . "じょ"))
        ("jyu" nil ("ジュ" . "じゅ"))
        ("kk" "k" ("ッ" . "っ"))
        ("ka" nil ("カ" . "か"))
        ("ke" nil ("ケ" . "け"))
        ("ki" nil ("キ" . "き"))
        ("ko" nil ("コ" . "こ"))
        ("ku" nil ("ク" . "く"))
        ("kya" nil ("キャ" . "きゃ"))
        ("kye" nil ("キェ" . "きぇ"))
        ("kyi" nil ("キィ" . "きぃ"))
        ("kyo" nil ("キョ" . "きょ"))
        ("kyu" nil ("キュ" . "きゅ"))
        ("ma" nil ("マ" . "ま"))
        ("me" nil ("メ" . "め"))
        ("mi" nil ("ミ" . "み"))
        ("mo" nil ("モ" . "も"))
        ("mu" nil ("ム" . "む"))
        ("mya" nil ("ミャ" . "みゃ"))
        ("mye" nil ("ミェ" . "みぇ"))
        ("myi" nil ("ミィ" . "みぃ"))
        ("myo" nil ("ミョ" . "みょ"))
        ("myu" nil ("ミュ" . "みゅ"))
        ("n" nil ("ン" . "ん"))
        ("n'" nil ("ン" . "ん"))
        ("na" nil ("ナ" . "な"))
        ("ne" nil ("ネ" . "ね"))
        ("ni" nil ("ニ" . "に"))
        ("nn" nil ("ン" . "ん"))
        ("no" nil ("ノ" . "の"))
        ("nu" nil ("ヌ" . "ぬ"))
        ("nya" nil ("ニャ" . "にゃ"))
        ("nye" nil ("ニェ" . "にぇ"))
        ("nyi" nil ("ニィ" . "にぃ"))
        ("nyo" nil ("ニョ" . "にょ"))
        ("nyu" nil ("ニュ" . "にゅ"))
        ("o" nil ("オ" . "お"))
        ("pp" "p" ("ッ" . "っ"))
        ("pa" nil ("パ" . "ぱ"))
        ("pe" nil ("ペ" . "ぺ"))
        ("pi" nil ("ピ" . "ぴ"))
        ("po" nil ("ポ" . "ぽ"))
        ("pu" nil ("プ" . "ぷ"))
        ("pya" nil ("ピャ" . "ぴゃ"))
        ("pye" nil ("ピェ" . "ぴぇ"))
        ("pyi" nil ("ピィ" . "ぴぃ"))
        ("pyo" nil ("ピョ" . "ぴょ"))
        ("pyu" nil ("ピュ" . "ぴゅ"))
        ("rr" "r" ("ッ" . "っ"))
        ("ra" nil ("ラ" . "ら"))
        ("re" nil ("レ" . "れ"))
        ("ri" nil ("リ" . "り"))
        ("ro" nil ("ロ" . "ろ"))
        ("ru" nil ("ル" . "る"))
        ("rya" nil ("リャ" . "りゃ"))
        ("rye" nil ("リェ" . "りぇ"))
        ("ryi" nil ("リィ" . "りぃ"))
        ("ryo" nil ("リョ" . "りょ"))
        ("ryu" nil ("リュ" . "りゅ"))
        ("ss" "s" ("ッ" . "っ"))
        ("sa" nil ("サ" . "さ"))
        ("se" nil ("セ" . "せ"))
        ("sha" nil ("シャ" . "しゃ"))
        ("she" nil ("シェ" . "しぇ"))
        ("shi" nil ("シ" . "し"))
        ("sho" nil ("ショ" . "しょ"))
        ("shu" nil ("シュ" . "しゅ"))
        ("si" nil ("シ" . "し"))
        ("so" nil ("ソ" . "そ"))
        ("su" nil ("ス" . "す"))
        ("sya" nil ("シャ" . "しゃ"))
        ("sye" nil ("シェ" . "しぇ"))
        ("syi" nil ("シィ" . "しぃ"))
        ("syo" nil ("ショ" . "しょ"))
        ("syu" nil ("シュ" . "しゅ"))
        ("tt" "t" ("ッ" . "っ"))
        ("ta" nil ("タ" . "た"))
        ("te" nil ("テ" . "て"))
        ("tha" nil ("テァ" . "てぁ"))
        ("the" nil ("テェ" . "てぇ"))
        ("thi" nil ("ティ" . "てぃ"))
        ("tho" nil ("テョ" . "てょ"))
        ("thu" nil ("テュ" . "てゅ"))
        ("ti" nil ("チ" . "ち"))
        ("to" nil ("ト" . "と"))
        ("tsu" nil ("ツ" . "つ"))
        ("tu" nil ("ツ" . "つ"))
        ("tya" nil ("チャ" . "ちゃ"))
        ("tye" nil ("チェ" . "ちぇ"))
        ("tyi" nil ("チィ" . "ちぃ"))
        ("tyo" nil ("チョ" . "ちょ"))
        ("tyu" nil ("チュ" . "ちゅ"))
        ("u" nil ("ウ" . "う"))
        ("vv" "v" ("ッ" . "っ"))
        ("va" nil ("ヴァ" . "う゛ぁ"))
        ("ve" nil ("ヴェ" . "う゛ぇ"))
        ("vi" nil ("ヴィ" . "う゛ぃ"))
        ("vo" nil ("ヴォ" . "う゛ぉ"))
        ("vu" nil ("ヴ" . "う゛"))
        ("ww" "w" ("ッ" . "っ"))
        ("wa" nil ("ワ" . "わ"))
        ("we" nil ("ウェ" . "うぇ"))
        ("wi" nil ("ウィ" . "うぃ"))
        ("wo" nil ("ヲ" . "を"))
        ("wu" nil ("ウ" . "う"))
        ("xx" "x" ("ッ" . "っ"))
        ("xa" nil ("ァ" . "ぁ"))
        ("xe" nil ("ェ" . "ぇ"))
        ("xi" nil ("ィ" . "ぃ"))
        ("xka" nil ("ヵ" . "か"))
        ("xke" nil ("ヶ" . "け"))
        ("xo" nil ("ォ" . "ぉ"))
        ("xtsu" nil ("ッ" . "っ"))
        ("xtu" nil ("ッ" . "っ"))
        ("xu" nil ("ゥ" . "ぅ"))
        ("xwa" nil ("ヮ" . "ゎ"))
        ("xwe" nil ("ヱ" . "ゑ"))
        ("xwi" nil ("ヰ" . "ゐ"))
        ("xya" nil ("ャ" . "ゃ"))
        ("xyo" nil ("ョ" . "ょ"))
        ("xyu" nil ("ュ" . "ゅ"))
        ("yy" "y" ("ッ" . "っ"))
        ("ya" nil ("ヤ" . "や"))
        ("ye" nil ("イェ" . "いぇ"))
        ("yo" nil ("ヨ" . "よ"))
        ("yu" nil ("ユ" . "ゆ"))
        ("zz" "z" ("ッ" . "っ"))
        ("z," nil "‥")
        ("z-" nil "～")
        ("z." nil "…")
        ("z/" nil "・")
        ("z[" nil "『")
        ("z]" nil "』")
        ("za" nil ("ザ" . "ざ"))
        ("ze" nil ("ゼ" . "ぜ"))
        ("zh" nil "←")
        ("zi" nil ("ジ" . "じ"))
        ("zj" nil "↓")
        ("zk" nil "↑")
        ("zl" nil "→")
        ("zo" nil ("ゾ" . "ぞ"))
        ("zu" nil ("ズ" . "ず"))
        ("zya" nil ("ジャ" . "じゃ"))
        ("zye" nil ("ジェ" . "じぇ"))
        ("zyi" nil ("ジィ" . "じぃ"))
        ("zyo" nil ("ジョ" . "じょ"))
        ("zyu" nil ("ジュ" . "じゅ"))
        ("." nil skk-current-kuten)
        ("," nil skk-current-touten)
        ("-" nil "ー")
        (":" nil "：")
        (";" nil "；")
        ("?" nil "？")
        ("[" nil "「")
        ("]" nil "」")
        ("l" nil skk-latin-mode)
        ("q" nil skk-toggle-kana)
        ("L" nil skk-jisx0208-latin-mode)
        ("Q" nil skk-set-henkan-point-subr)
        ("X" nil skk-purge-from-jisyo)
        ("/" nil skk-abbrev-mode)
        ("$" nil skk-display-code-for-char-at-point)
        ("@" nil skk-today)
        ("\\" nil skk-input-by-code-or-menu)))

(setq skk-rom-kana-rule-list
      '(("hh" "h" ("ッ" . "っ"))
        ("mm" "m" ("ン" . "ん"))))

(setq skk-try-completion-char ?\011)

(provide 'skk-def)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-def.el ends here
