;;; ============================================================================
;;;  锤纹阵列生成器  HAMMER TEXTURE GENERATOR   v2
;;;  命令：HT
;;;  平台：AutoCAD / 中望CAD / 浩辰CAD / Bricscad （AutoLISP 兼容）
;;;
;;;  原理：对抖动过的六方点阵求 Voronoi 单元 —— 单元大小相近、形状不规则、
;;;        紧密镶嵌、无空隙。单元多边形即锤痕。
;;;
;;;  输入：矩形宽、矩形高、球心间距、锤纹深度、不规则度 0-100
;;;        （不规则度 0 = 正六边形蜂窝，100 = 最乱）
;;;  输出：锤痕多边形、球心点两个图层；图形下方标注所需球心半径；
;;;        命令行同时打印全部参数
;;; ============================================================================

(vl-load-com)

;; ----------------------------------------------------------------- 随机数 ---
(setq *ht-seed* 12345)

(defun ht-srand (s / )
  (setq *ht-seed* (fix (abs s)))
  (if (< *ht-seed* 1) (setq *ht-seed* 12345))
)

(defun ht-rand ( / )
  (setq *ht-seed* (rem (+ (* *ht-seed* 1103515245.0) 12345.0) 2147483648.0))
  (/ *ht-seed* 2147483648.0)
)

(defun ht-gauss ( / u v)
  (setq u (+ 1e-9 (ht-rand)))
  (setq v (ht-rand))
  (* (sqrt (* -2.0 (log u))) (cos (* 6.283185307179586 v)))
)

;; ------------------------------------------------------------------- 工具 ---
(defun ht-dist (x1 y1 x2 y2)
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))
)

;; 半平面裁剪：保留多边形中离 P 比离 Q 更近的部分
(defun ht-clip (poly px py qx qy / A B C out n i p q fp fq tt)
  (setq A (* 2.0 (- qx px)))
  (setq B (* 2.0 (- qy py)))
  (setq C (- (+ (* px px) (* py py)) (+ (* qx qx) (* qy qy))))
  (setq out nil n (length poly) i 0)
  (while (< i n)
    (setq p (nth i poly))
    (setq q (nth (rem (1+ i) n) poly))
    (setq fp (+ (* A (car p)) (* B (cadr p)) C))
    (setq fq (+ (* A (car q)) (* B (cadr q)) C))
    (cond
      ((<= fp 1e-9)
       (setq out (cons p out))
       (if (> fq 1e-9)
         (progn
           (setq tt (/ fp (- fp fq)))
           (setq out (cons (list (+ (car p) (* tt (- (car q) (car p))))
                                 (+ (cadr p) (* tt (- (cadr q) (cadr p))))) out))
         )
       )
      )
      ((<= fq 1e-9)
       (setq tt (/ fp (- fp fq)))
       (setq out (cons (list (+ (car p) (* tt (- (car q) (car p))))
                             (+ (cadr p) (* tt (- (cadr q) (cadr p))))) out))
      )
    )
    (setq i (1+ i))
  )
  (reverse out)
)

;; ------------------------------------------------------------------- 图层 ---
(defun ht-layer (nm col)
  (if (not (tblsearch "LAYER" nm))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 nm) '(70 . 0) (cons 62 col)))
  )
)

(defun ht-poly (poly lay / data p)
  (if (> (length poly) 2)
    (progn
      (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                       (cons 8 lay) '(100 . "AcDbPolyline")
                       (cons 90 (length poly)) '(70 . 1)))
      (foreach p poly
        (setq data (append data (list (cons 10 (list (car p) (cadr p))))))
      )
      (entmake data)
    )
  )
)

(defun ht-point (x y lay)
  (entmake (list '(0 . "POINT") '(100 . "AcDbEntity")
                 (cons 8 lay) '(100 . "AcDbPoint")
                 (cons 10 (list x y 0.0))))
)

;; ------------------------------------------------------------------- 文字 ---
;; 建立中文文字样式。
;; 用单一 TTF（黑体 simhei.ttf），不用 simsun.ttc —— ttc 是字体集合，
;; 部分 CAD 解析不了会替换成无中文字体，导致全变问号。
(defun ht-mkstyle (nm font / )
  (if (tblsearch "STYLE" nm)
    T
    (progn
      (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbTextStyleTableRecord")
                     (cons 2 nm) '(70 . 0) '(40 . 0.0)
                     '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 1.0)
                     (cons 3 font) '(4 . "")))
      (if (tblsearch "STYLE" nm) T nil)
    )
  )
)

;; 依次尝试：黑体 TTF -> 宋体 TTF -> SHX+大字体；全失败则返回 nil 走英文标注
(defun ht-style ( / )
  (cond
    ((ht-mkstyle "锤纹_标注" "simhei.ttf") "锤纹_标注")
    ((ht-mkstyle "锤纹_宋体" "simsun.ttf") "锤纹_宋体")
    (T nil)
  )
)

(defun ht-text (x y hgt txt lay sty / )
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 lay)
                 '(100 . "AcDbText")
                 (cons 10 (list x y 0.0))
                 (cons 40 hgt) (cons 1 txt) (cons 7 sty)
                 '(50 . 0.0) '(41 . 1.0) '(51 . 0.0) '(71 . 0) '(72 . 0)
                 '(100 . "AcDbText") '(73 . 0)))
)

;; --------------------------------------------------------------------- 块 ---
;; 生成不重名的块名：首次用原名，已存在则加 _1 / _2 ...
(defun ht-uniq (base / n nm)
  (setq n 0 nm base)
  (while (tblsearch "BLOCK" nm)
    (setq n (1+ n) nm (strcat base "_" (itoa n))))
  nm
)

(defun ht-block-begin (nm)
  (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0")
                 '(100 . "AcDbBlockBegin")
                 (cons 2 nm) '(70 . 0) '(10 0.0 0.0 0.0)
                 (cons 3 nm) '(1 . "")))
)

(defun ht-block-end ( )
  (entmake (list '(0 . "ENDBLK") '(100 . "AcDbEntity") '(8 . "0")
                 '(100 . "AcDbBlockEnd")))
)

(defun ht-insert (nm lay)
  (entmake (list '(0 . "INSERT") '(100 . "AcDbEntity") (cons 8 lay)
                 '(100 . "AcDbBlockReference")
                 (cons 2 nm) '(10 0.0 0.0 0.0)
                 '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
)

;; ------------------------------------------------------------------- 主命令 ---
(defun c:HT ( / W H pitch depth irr a r rowh jit rmin th pts plist qlist p q
                polys ctrs bname1 bname2 styname
                poly pt v npts cnt i j x y off dd dd2 ok tries xx yy seed amax)
  (princ "\n============== 锤纹阵列生成器 ==============")
  (setq W (getreal "\n矩形宽度 W <45.0>: "))
  (if (null W) (setq W 45.0))
  (setq H (getreal "\n矩形高度 H <7.0>: "))
  (if (null H) (setq H 7.0))
  (setq pitch (getreal "\n球心间距 <1.5>: "))
  (if (null pitch) (setq pitch 1.5))
  (setq depth (getreal "\n锤纹深度 <0.3>: "))
  (if (null depth) (setq depth 0.3))
  (setq irr (getreal "\n不规则度 0-100 <70>: "))
  (if (null irr) (setq irr 70.0))
  (if (< irr 0.0) (setq irr 0.0))
  (if (> irr 100.0) (setq irr 100.0))
  (setq irr (/ irr 100.0))

  ;; ---- 派生几何 ----------------------------------------------------------
  (setq a (/ pitch 1.7320508075688772))                   ; 名义单元外接圆半径
  (setq amax 0.0)                                         ; 实际覆盖半径（算完单元后得出）
  (setq r 0.0)                                            ; 球心半径（同上）
  (setq rowh (* pitch 0.8660254037844386))
  (setq jit (* pitch 0.15 irr))                           ; 抖动幅度
  (setq rmin (* pitch 0.82))                              ; 最小球心间距

  (princ "\n---------------- 参数 ----------------")
  (princ (strcat "\n  矩形尺寸    : " (rtos W 2 2) " x " (rtos H 2 2) " mm"))
  (princ (strcat "\n  球心间距    : " (rtos pitch 2 3) " mm"))
  (princ (strcat "\n  单元外接半径: " (rtos a 2 4) " mm    (= 间距 / 1.732)"))
  (princ (strcat "\n  锤纹深度    : " (rtos depth 2 2) " mm"))
  (princ (strcat "\n  不规则度    : " (rtos (* 100.0 irr) 2 0) " %    (抖动 "
                 (rtos jit 2 3) " mm，最小间距 " (rtos rmin 2 3) " mm)"))
  (princ "\n  球心半径    : 算完单元后按实测覆盖半径反算（保证球冠盖满，底面不露平面）")
  (princ "\n--------------------------------------")

  ;; ---- 随机种子 ----------------------------------------------------------
  (setq seed (* 100000.0 (- (getvar "DATE") (fix (getvar "DATE")))))
  (ht-srand (fix seed))
  (ht-layer "锤纹_多边形" 8)
  (ht-layer "锤纹_中心点" 1)

  ;; ---- 抖动六方点阵（带最小间距拒绝） -----------------------------------
  (princ "\n  正在生成点位...")
  (setq pts nil j -1)
  (while (<= (* j rowh) (+ H (* 2.0 rowh)))
    (setq y (* j rowh))
    (setq off (if (= (rem (+ j 100) 2) 0) 0.0 (/ pitch 2.0)))
    (setq i -1)
    (while (<= (* i pitch) (+ W (* 2.0 pitch)))
      (setq x (+ (* i pitch) off))
      ;; 最多试 12 次抖动，取第一个满足最小间距的位置
      (setq tries 0 ok nil)
      (while (and (< tries 12) (not ok))
        (setq xx (+ x (* jit (ht-gauss))))
        (setq yy (+ y (* jit (ht-gauss))))
        (setq ok T)
        (foreach q pts
          (if (< (ht-dist xx yy (car q) (cadr q)) rmin)
            (setq ok nil)
          )
        )
        (setq tries (1+ tries))
      )
      (if ok
        (setq pts (cons (list xx yy) pts))
        (setq pts (cons (list x y) pts))      ; 回退名义位置：宁可靠近，不留空洞
      )
      (setq i (1+ i))
    )
    (setq j (1+ j))
  )
  (setq pts (reverse pts))
  (setq npts (length pts))
  (princ (strcat "\n  种子点 " (itoa npts) " 个，正在计算单元（请稍候，不要按 ESC）..."))

  ;; ---- 用半平面裁剪求 Voronoi -------------------------------------------
  (setq cnt 0 polys nil ctrs nil)
  (setq plist pts)
  (while plist
    (setq p (car plist))
    (setq poly (list (list 0.0 0.0) (list W 0.0) (list W H) (list 0.0 H)))
    (setq qlist pts)
    (while qlist
      (setq q (car qlist))
      (if (not (and (equal (car p) (car q) 1e-9)
                    (equal (cadr p) (cadr q) 1e-9)))
        (progn
          (setq dd (ht-dist (car p) (cadr p) (car q) (cadr q)))
          (if (< dd (* 2.6 pitch))
            (setq poly (ht-clip poly (car p) (cadr p) (car q) (cadr q)))
          )
        )
      )
      (setq qlist (cdr qlist))
    )
    (if (> (length poly) 2)
      (progn
        (setq polys (cons poly polys))
        (if (and (>= (car p) 0.0) (<= (car p) W)
                 (>= (cadr p) 0.0) (<= (cadr p) H))
          (foreach v poly
            (if (and (>= (car v) -1e-9) (<= (car v) (+ W 1e-9))
                     (>= (cadr v) -1e-9) (<= (cadr v) (+ H 1e-9)))
              (progn
                (setq dd2 (ht-dist (car p) (cadr p) (car v) (cadr v)))
                (if (> dd2 amax) (setq amax dd2))
              )
            )
          )
        )
      )
    )
    (if (and (>= (car p) 0.0) (<= (car p) W)
             (>= (cadr p) 0.0) (<= (cadr p) H))
      (progn
        (setq ctrs (cons (list (car p) (cadr p)) ctrs))
        (setq cnt (1+ cnt))
      )
    )
    (setq plist (cdr plist))
  )

  ;; ---- 按实测覆盖半径反算球心半径 ---------------------------------------
  (if (< amax 1e-6) (setq amax (/ pitch 1.7320508075688772)))
  (setq amax (* amax 1.01))                     ; 1% 余量
  (setq a amax)
  (setq r (/ (+ (* a a) (* depth depth)) (* 2.0 depth)))
  (princ (strcat "\n  覆盖半径    : " (rtos amax 2 4) " mm   （区域内离最近球心的最远距离）"))
  (princ (strcat "\n  >>> 球心半径 r = (a^2+h^2)/(2h) = " (rtos r 2 4) " mm"))
  (princ "\n      球冠完全盖住单元，锤纹底部为球面，不会露出平面")

  ;; ---- 分别打包成块：多边形一个、中心点一个 -----------------------------
  (princ "\n  正在打包成块...")
  (setq bname1 (ht-uniq "锤纹_多边形"))
  (ht-block-begin bname1)
  (foreach poly (reverse polys) (ht-poly poly "0"))
  (ht-block-end)
  (ht-insert bname1 "锤纹_多边形")

  (setq bname2 (ht-uniq "锤纹_中心点"))
  (ht-block-begin bname2)
  (foreach pt (reverse ctrs) (ht-point (car pt) (cadr pt) "0"))
  (ht-block-end)
  (ht-insert bname2 "锤纹_中心点")

  ;; ---- 图下标注：所需球心半径 -------------------------------------------
  (ht-layer "锤纹_参数" 3)
  (setq th (* pitch 0.60))
  (setq styname (ht-style))
  (if styname
    (progn
      (ht-text 0.0 (- (* th 3.30)) th
               (strcat "球心半径 R = " (rtos r 2 3) " mm") "锤纹_参数" styname)
      (ht-text 0.0 (- (* th 4.70)) (* th 0.62)
               (strcat "球心间距 " (rtos pitch 2 3) " mm    锤纹深度 " (rtos depth 2 2)
                       " mm    不规则度 " (rtos (* 100.0 irr) 2 0) " %   覆盖半径 "
                       (rtos amax 2 3) " mm")
               "锤纹_参数" styname)
    )
    (progn
      (ht-text 0.0 (- (* th 3.30)) th
               (strcat "BALL RADIUS R = " (rtos r 2 3) " mm")
               "锤纹_参数" (getvar "TEXTSTYLE"))
      (ht-text 0.0 (- (* th 4.70)) (* th 0.62)
               (strcat "PITCH " (rtos pitch 2 3) "    DEPTH " (rtos depth 2 2)
                       "    IRREGULARITY " (rtos (* 100.0 irr) 2 0) " %")
               "锤纹_参数" (getvar "TEXTSTYLE"))
    )
  )

  (princ (strcat "\n  完成。矩形内球心 " (itoa cnt) " 个，多边形 "
                 (itoa (length polys)) " 个"))
  (princ (strcat "\n  块：" bname1 "（锤痕多边形）  " bname2 "（球心点）"))
  (if styname
    (princ (strcat "\n  标注字体样式：" styname "（黑体）"))
    (princ "\n  注意：未建成中文字体样式，标注已用英文。\n        如仍显示问号，请输入 STYLE，把样式「锤纹_标注」的字体改为 simhei.ttf")
  )
  (princ "\n  图层：锤纹_多边形 / 锤纹_中心点 / 锤纹_参数（图下标注，可在图层面板关闭）")
  (princ)
)

(princ "\n锤纹阵列生成器已加载。输入命令 HT 开始。")
(princ)
