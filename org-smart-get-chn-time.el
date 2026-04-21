;;; org-smart-get-chn-time.el --- 类滴答清单，智能识别字符串中第一个中文时间戳 -*- lexical-binding: t; -*-
;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2026.04.16

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'org)

(defun org-sgct-chn2num (chn-str)
  "将中文数字'零'到'九百九十九'转换为整数，支持'两'作为'二'的变体。"
  (save-match-data
    (let* ((num 0)
           (digits `(("零" . 0) ("一" . 1) ("二" . 2) ("两" . 2) ("三" . 3) ("四" . 4)
                     ("五" . 5) ("六" . 6) ("七" . 7) ("八" . 8) ("九" . 9)))
           (hundreds-pos (string-match "百" chn-str))
           (tens-pos (string-match "十" chn-str)))
      ;; 处理百位
      (when hundreds-pos
        (let* ((char-before (substring chn-str 0 hundreds-pos))
               (digit (cdr (assoc char-before digits))))
        (setq num (+ num (* digit 100)))))
      
      ;; 处理十位
      (when tens-pos
        (let* ((char-before (if hundreds-pos
                                (substring chn-str (1+ hundreds-pos) tens-pos)
                              (substring chn-str 0 tens-pos)))
               (digit (cdr (assoc char-before digits))))
          (cond
           ;; "十" 或 "一十" 的情况
           ((or (string= "" char-before) (string= "一" char-before))
            (setq num (+ num 10)))
           ;; "零" 在十位前，例如 "一百零五" 中的 "零"
           ((string= "零" char-before))
            ;; "零" 本身不增加数值，但会影响个位的解析，此处无需额外操作
           (t
            (setq num (+ num (* (or digit 1) 10)))))))
      
      ;; 处理个位
      (let* ((last-char (substring chn-str -1))
             (last-digit (cdr (assoc last-char digits))))
        (when (and last-digit (not (or (string= last-char "百") (string= last-char "十"))))
          ;; 如果 "零" 出现在末尾，例如 "一百一十"，last-char会是"十"，所以这里的判断是有效的
          (if (and tens-pos (= 0 (length (substring chn-str (1+ tens-pos)))))
              ;; 类似于 "二十", "三十" 这样的整十数，个位不再处理
              ()
            (setq num (+ num last-digit)))))
      
      ;; 处理特殊情况 "十"
      (if (string= "十" chn-str)
          10
        num))))

(defun org-sgct-digital-string (str)
  "识别字符串中的中文数字并替换为阿拉伯数字。"
  (let ((re "[零一二两三四五六七八九十百]+")
        (res str)
        (start 0))
    (while (string-match re res start)
      (let* ((match-str (match-string 0 res))
             (num (org-sgct-chn2num match-str)))
        (setq res (replace-match (number-to-string num) t t res))
        (setq start (+ (match-beginning 0) (length (number-to-string num))))))
    res))

(defun org-sgct-expand-abbrev-string (str)
  "识别字符串中的缩写并展开。"
  (replace-regexp-in-string
   "今早\\|明早\\|今晚\\|明晚"
   (lambda (matched)
     (cond ((string= matched "今早") "今天早上")
           ((string= matched "明早") "明天早上")
           ((string= matched "今晚") "今天晚上")
           ((string= matched "明晚") "明天晚上")
           (t matched)))
   str))

(defun org-sgct-get-time (str)
  "对于给定的字符串 STR，返回转换后的时间字符串（不带括号）。"
  (let* ((work-str (org-sgct-expand-abbrev-string (org-sgct-digital-string str)))
         (now (decode-time (current-time)))
         (now-h (decoded-time-hour now))
         (now-dow (decoded-time-weekday now)) ;; 0是周日
         (res-date nil)
         (res-time nil))

    ;; 情况 1: 处理「...后」的相对时间
    (if (string-match "\\([0-9年周个月天小时分钟]+?\\)后" work-str)
        (let ((rel-content (match-string 1 work-str))
              (sec-offset 0))
          ;; 日期部分转换为 +Ny/m/w/d
          (dolist (unit '(("年" . "y") ("个月" . "m") ("月" . "m") ("周" . "w") ("天" . "d")))
            (when (string-match (concat "\\([0-9]+\\)" (car unit)) rel-content)
              (setq res-date (concat "+" (match-string 1 rel-content) (cdr unit)))))
          ;; 时间部分累计秒数
          (let ((h-off 0) (m-off 0))
            (when (string-match "\\([0-9]+\\)小时" rel-content)
              (setq h-off (string-to-number (match-string 1 rel-content))))
            (when (string-match "\\([0-9]+\\)\\(分钟\\|分\\)" rel-content)
              (setq m-off (string-to-number (match-string 1 rel-content))))
            (setq sec-offset (+ (* h-off 3600) (* m-off 60))))
          ;; 转换为绝对 HH:MM
          (when (> sec-offset 0)
            (setq res-time (format-time-string "%H:%M" (time-add (current-time) sec-offset)))))

      ;; 情况 2: 处理常规日期和时间
      ;; A. 处理日期
      (cond
       ;; 识别具体的 X月Y日 或 X月
       ((string-match "\\([0-9]+\\)月\\(\\([0-9]+\\)日\\)?" work-str)
        (setq res-date (concat (match-string 1 work-str) "-" (or (match-string 3 work-str) "1"))))
       ((string-match "今天" work-str) (setq res-date "+0d"))
       ((string-match "明天" work-str) (setq res-date "+1d"))
       ((string-match "后天" work-str) (setq res-date "+2d"))
       ((string-match "\\(下+周\\|周\\|星期\\)\\([1-7一二三四五六日天]\\)" work-str)
        (let* ((is-next (cond ((string= (match-string 1 work-str) "下下周") 2)
                              ((string= (match-string 1 work-str) "下周") 1)
                              (t nil)))
               (day-raw (match-string 2 work-str))
               (target-dow (cond ((member day-raw '("1" "一")) 1)
                                 ((member day-raw '("2" "二")) 2)
                                 ((member day-raw '("3" "三")) 3)
                                 ((member day-raw '("4" "四")) 4)
                                 ((member day-raw '("5" "五")) 5)
                                 ((member day-raw '("6" "六")) 6)
                                 ((member day-raw '("7" "日" "天")) 0)))
               (day-name (elt '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") target-dow)))
          (if (not is-next)
              (setq res-date (concat "+" day-name))
              ;; 逻辑：下周且目标日还没过则跳一周(+2Name)，否则即指下周(+Name)
              (when (> target-dow now-dow)
                (setq is-next (1+ is-next)))
              (setq res-date (concat "+" (number-to-string is-next) day-name))))))

      ;; B. 处理时间 (12小时/24小时逻辑与滴答清单相似)
      (let ((hour-shift 0)
            (period-explicit nil)) ;; 标记用户是否明确说了 上午/下午
        (cond
         ((string-match "\\(凌晨\\|早上\\|上午\\|中午\\)" work-str) 
          (setq period-explicit (match-string 1 work-str)))
         ((string-match "\\(下午\\|傍晚\\|晚上\\)" work-str) 
          (setq hour-shift 12 period-explicit (match-string 1 work-str))))
        (if (string-match "\\([0-9]+\\)点\\(半\\|[0-9]+分?\\)?" work-str)
            (let* ((h (string-to-number (match-string 1 work-str)))
                   (m-raw (match-string 2 work-str))
                   (m (cond ((not m-raw) 0) ((string= m-raw "半") 30) (t (string-to-number m-raw))))
                   (final-h (+ h hour-shift)))
              ;; 只有在用户没说“上午/下午”且没有明确日期时
              (when (and (not period-explicit)
                         (member res-date '(nil "+0d"))
                         (< final-h now-h))
                (if (< (+ final-h 12) now-h)
                    (setq res-date "+1d")
                  (setq final-h (+ final-h 12))))
              (setq res-time (format "%02d:%02d" final-h m)))
          ;; 只有修饰词的情况
          (when period-explicit
            (setq res-time (pcase period-explicit
                             ("凌晨" "01:00")
                             ("早上" "07:00")
                             ("上午" "09:00")
                             ("中午" "12:00")
                             ("下午" "14:00")
                             ("傍晚" "18:00")
                             ("晚上" "20:00")))))))

    ;; 拼接最终字符串
    (let ((out (concat (or res-date "") (if (and res-date res-time) " " "") (or res-time ""))))
      (if (string-empty-p out)
          nil ;; 或者返回原始字符串
        out))))

(provide 'org-smart-get-chn-time)
;;; org-smart-get-chn-time.el ends here
