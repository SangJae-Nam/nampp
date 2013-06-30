#lang racket

(require 2htdp/image 2htdp/universe) 
(provide init-canvas n_line n_circle n_triangle n_rectangle n_polygon n_text save-image-canvas remove-dquote)

;빈 이미지 변수
(define image-canvas (empty-scene 600 600))

;;캔버스 초기화
(define init-canvas (lambda ()
                      (set! image-canvas (empty-scene 600 600))))


;;;;;;;;;;;그래픽 표현 함수 - 인터프리터에서 호출한다
;;line 그리는 함수
;; line함수는 0,0을 기준으로 (x,y)까지의 선을 그리기 때문에 argument로 입력된 두 점을 (0,0)으로 옮긴 직선을 생성한 후 이를 원래 좌표로 이동시킨다.
(define n_line
  (lambda(x y i j color)
    (set! image-canvas (place-image (line (- x i) (- y j) color) (/ (+ x i) 2) (/ (+ y j) 2) image-canvas))))

;;circle 그리는 함수
(define n_circle
  (lambda (r i j ls color)
    (set! image-canvas (place-image (circle r ls color) i j image-canvas))))

;;triangle 그리는 함수
(define n_triangle
  (lambda (a b c i j ls color)
    (set! image-canvas (place-image (triangle/sss a b c ls color) i j image-canvas))))

;;rectangle 그리는 함수
(define n_rectangle
  (lambda (a b i j ls color)
          (set! image-canvas (place-image (rectangle a b ls color) i j image-canvas))))

;;polygon 그리는 함수 
(define n_polygon
  (lambda(r n i j ls color)
    (set! image-canvas (place-image (regular-polygon r n ls color) i j image-canvas))))

;;문자 
(define n_text
  (lambda (t i j size color)
    (set! image-canvas (place-image (text t size color) i j image-canvas))))

(define save-image-canvas (lambda ()
  ;이미지를 파일에 저장
  (save-image image-canvas "img_temp.png" 600 600)))

;;string-exp에서 양쪽 끝의 "을 지워준다. - NAM++의 string-exp는 양쪽 끝에 "가 존재하기 때문에 그리기함수에서는 지워줘야 한다.
(define remove-dquote
  (lambda (str)
    (string-trim str "\"")))