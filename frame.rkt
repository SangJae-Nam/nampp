#lang racket/gui

(require wxme/image)
(require "check.scm")
 
;창 프레임
(define edit_frame (new frame% [label "[type-object-oriented] Edit"]
                               [width 800]
                               [height 600]
                               [x 100]
                               [y 100] ))
(define figure_frame (new frame% [label "[type-object-oriented] Output"]
                                 [width 600]
                                 [height 600]
                                 [x 900]
                                 [y 100]))
(define bottom_pane (new vertical-pane%  [parent edit_frame]))
(define top_pane (new vertical-pane%  [parent bottom_pane]))


;메뉴를 만들어 준다
(define menu_bar (new menu-bar% [parent edit_frame]))


;file 메뉴 
(define m_file (new menu% [label "File"] [parent menu_bar]))
(define m_new (new menu-item% [parent m_file] 
                               [label "new"]
                               [callback (lambda (button event) 
                                                   (new keymap%))]))
(define m_save-as (new menu-item% [parent m_file] 
                               [label "save as"]
                               [callback (lambda (button event) 
                                                   (send t save-file "" 'text #t))]))
(define m_load (new menu-item% [parent m_file] 
                               [label "load"]
                               [callback (lambda (button event) 
                                                   (send t load-file "" 'text #t))]))
(define m_exit (new menu-item% [parent m_file] 
                               [label "exit"]
                               [callback (lambda (button event) 
                                               (exit))]))
;edit 메뉴 
(define m_edit (new menu% [label "Edit"] [parent menu_bar]))
;font 메뉴 
(define m_font (new menu% [label "Font"] [parent menu_bar]))
(append-editor-operation-menu-items m_edit #f)
(append-editor-font-menu-items m_font)


(define output_window (new message% [parent bottom_pane]
                                    [label ""]
                                    [min-width 600]
                                    {min-height 100}))

(define d (new editor-canvas% [parent top_pane]))
(define t (new text%))
(send d set-editor t)

;창 전체를 보일 수 있게 해준다 
(send edit_frame show #t)

;실행 버튼
(new button% [parent edit_frame]
             [label "run"]
             [callback (lambda (button event)
                         ;;출력창에 결과 표시
                         (send figure_frame show #t)
                         ;;코드 입력창에 입력한 것을 인터프리트 한다. - 이 때 결과는 image-canvas에 그려진다.
                         (send t save-file "code_temp.txt" 'text #t)
                         (runfile "code_temp.txt")
;;(n_circle 40 100 100 "outline" "red")
;;(n_line 50 30 100 100 "black"  )
;;(n_triangle 40 60 80 100 100 "solid" "seagreen" )
;;(n_rectangle 40 20  200 200  "solid" "blue")
;;(n_polygon 70 5 400 400 "outline" "red"  )
;;(n_text "[type-object-oriented]" 200 100 24 "cyan" )
                         ;;runfile에서 저장된 이미지를 이미지를 캔버스에 그린다.
                         (draw-canvas))])


;캔버스에 그릴 이미지를 불러올 변수
(define canvas-bmp (make-object bitmap% 600 600))
;캔버스 변수
(define canvas (new canvas% [parent figure_frame]
     [min-width 600]
     [min-height 600]
     [style (list 'vscroll 'hscroll 'border)]
     [label "canvas"]
     [paint-callback (lambda (c dc)
                       (send dc draw-bitmap canvas-bmp 0 0))]))

;;화면에 출력하는 함수
(define draw-canvas (lambda ()
                      ;이미지를 bitmap% 객체에 불러오기
                      (send canvas-bmp load-file "img_temp.png" 'png)))