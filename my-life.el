;;; my-life.el --- Life game

;; Copyright (C) 2013  OHTA Yuusuke

;; Author: OHTA Yuusuke <ohtysk at gmail.com>
;; Keywords: games

;;; Commentary:

;; (require 'my-life)
;; (global-set-key (kbd "C-M-m") 'life-start-random)
;; (global-set-key (kbd "C-M-i") 'life-start-file)
;; (global-set-key (kbd "C-M-y") 'life-restart)
;; (global-set-key (kbd "C-M-z") 'life-stop)

;; This program is simple life game for exercise

(require 'cl)
(require 'deferred)

(defun make-incriment-list (init size)
  (let (list (num init))
    (dotimes (i size)
      (setq list (cons num list))
      (setq num (1+ num)))
    (reverse list)))

(defun make-random-list (max length)
  (let (list)
    (dotimes (i length)
      (setq list (cons (random max) list)))
    list))

(defun set-at-index (index value list)
  (setf (nth index list) value))

(lexical-let* ((life-age 0)
               (life-age-end 1000)
               (life-total 0)
               (life-world-width 85)
               (life-world-height 46)
               (life-world-area (* life-world-width life-world-height))
               (life-first-world)
               (life-world (make-list life-world-area 0))
               (life-neighbors
                '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
               (life-neighbors-world)
               (continue)
               (wait-time 100)
               load-width
               load-height
               load-world)

  (defun get-x-by-index (n) (mod n life-world-width))

  (defun get-y-by-index (n) (/ n life-world-width))
  
  (defun get-point-by-index (n) (list (get-x-by-index n) (get-y-by-index n)))

  (defun get-index-by-point (x y) (+ (* y life-world-width) x))

  (defun get-shifted-index-by-point (x y dx dy)
    (get-index-by-point
     (mod (+ x dx) life-world-width)
     (mod (+ y dy) life-world-height)))

  (defun get-shifted-index-by-index (n dx dy)
    (get-index-by-point
     (mod (+ (get-x-by-index n) dx) life-world-width)
     (mod (+ (get-y-by-index n) dy) life-world-height)))

  (defun display-life-world (world buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (dotimes (y life-world-height)
        (dotimes (x life-world-width)
          (if (= (nth (get-index-by-point x y) world) 1)
              (insert "@ ")
            (insert ". "))
          (if (= (1+ x) life-world-width)
              (insert "\n"))))
      (insert "\n")
      (goto-char (point-min))
      ))

  (defun count-life-neighbors-at-index (n world)
    (reduce '+ (mapcar (lambda (xy-pair)
                         (or (nth (get-shifted-index-by-index
                                   n
                                   (nth 0 xy-pair)
                                   (nth 1 xy-pair)) world) 0))
                       life-neighbors)))

  (defun make-count-life-neighbors-world (world)
    (mapcar (lambda (n) (count-life-neighbors-at-index n world))
            (make-incriment-list 0 life-world-area)))

  (defun filter-count-life-neighbors-world (world neighbors-world)
    (mapcar (lambda (n)
              (let ((life (nth n world)) (neighbors (nth n neighbors-world)))
                (life-condition life neighbors)))
            (make-incriment-list 0 life-world-area)))

  (defun life-condition (life neighbors)
    (if (= life 1)
        ; 生きてる場合
        (if (or (= neighbors 2) (= neighbors 3))
            ;生存
            1
          ;過疎/過密
          0
          )
      ; 死んでる場合
      (if (= neighbors 3)
          ; 誕生
          1
        ; 何も起きない 
        0)))

  (defun life-init ()
    (setq life-age 0)
    (setq life-total 0)
    (setq life-world-area (* life-world-width life-world-height))
    (setq life-world (make-list life-world-area 0)))

  (defun sum-life-count ()
    (reduce '+ life-world))

  (defun life-random ()
    (mapc (lambda (n) (set-at-index n 1 life-world))
          (make-random-list life-world-area (/ life-world-area 5)))
    (kill-on-edges life-world)
    (setq life-total (sum-life-count))
    (with-current-buffer (get-buffer-create "*life*")
      (delete-region (point-min) (point-max))
      (insert
       (format "make random world (%d)\n" life-total)))
    (setq life-first-world life-world)
    (display-life-world life-world (get-buffer-create "*life*")))

  (defun kill-on-edges (world)
    ;; x方向の辺を殺す
    (mapc (lambda (n) (set-at-index n 0 world))
          (make-incriment-list 0 life-world-width))
    ;; y方向の辺を殺す
    (mapc (lambda (n) (set-at-index (* n life-world-width) 0 world))
          (make-incriment-list 0 life-world-height))
    )

  (defun life-next-age ()
    (interactive)
    (setq life-age (1+ life-age))    
    (setq life-neighbors-world (make-count-life-neighbors-world life-world))
    (setq life-world (filter-count-life-neighbors-world life-world life-neighbors-world))
    (setq life-total (sum-life-count))
    (kill-on-edges life-world)
    (with-current-buffer (get-buffer-create "*life*")
      (delete-region (point-min) (point-max))
      (goto-char (point-max))
      (insert (format "%d age (%d)\n" life-age life-total)))
    (display-life-world life-world (get-buffer-create "*life*")))

  (defun life-start-random ()
    (interactive)
    (deferred:$
      (deferred:next
        (lambda (x)
          (setq continue t)
          (switch-to-buffer (get-buffer-create "*life*"))
          (highlight-phrase "*")
          (life-init)
          (life-random)))
      (deferred:nextc it
        (deferred:lambda (x)
          (life-next-age)
          (if (and (> life-age-end life-age) continue) 
              (deferred:nextc (deferred:wait wait-time) self))))
      (deferred:nextc it
        (lambda (x)
          (setq continue nil)))))

  (defun life-start-file (file)
    (interactive "fLoad first world: ")
    (life-load file)
    (deferred:$
      (deferred:next
        (lambda (x)
          (setq continue t)
          (switch-to-buffer (get-buffer-create "*life*"))
          (highlight-phrase "*")))
      (deferred:nextc it
        (deferred:lambda (x)
          (life-next-age)
          (if (and (> life-age-end life-age) continue) 
              (deferred:nextc (deferred:wait wait-time) self))))
      (deferred:nextc it
        (lambda (x)
          (setq continue nil)))))

  (defun life-stop ()
    (interactive)
    (setq continue nil))

  (defun life-restart ()
    (interactive)
    (deferred:$
      (deferred:next
        (lambda (x)
          (setq continue t)
          (switch-to-buffer (get-buffer-create "*life*"))))
      (deferred:nextc it
        (deferred:lambda (x)
          (life-next-age)
          (if (and (> life-age-end life-age) continue) 
              (deferred:nextc (deferred:wait wait-time) self))))
      (deferred:nextc it
        (lambda (x)
          (setq continue nil)))))

  (defun life-save (file)
    (interactive "FSave first world: ")
    (with-temp-buffer
      (insert (format "(setq load-width %d)\n" life-world-width))
      (insert (format "(setq load-height %d)\n" life-world-height))
      (insert (format "(setq load-world '%s)\n" life-first-world))
      (write-file (expand-file-name file))))

  (defun life-load (file)
    (interactive "fLoad first world: ")
    (load-file (expand-file-name file))
    (setq life-age 0)
    (setq life-total 0)
    (setq life-world-width load-width)
    (setq life-world-height load-height)
    (setq life-world-area (* life-world-width life-world-height))
    (setq life-first-world load-world)
    (setq life-world life-first-world)
    (setq life-total (sum-life-count))
    (with-current-buffer (get-buffer-create "*life*")
      (insert (format "load world (%d)\n" life-total))
      (display-life-world life-world (get-buffer-create "*life*"))))
  )

(provide 'my-life)
