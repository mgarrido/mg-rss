; $Id: mg-rss.el,v 1.13 2003/11/02 19:12:33 mgarrido Exp $
;
;;; mg-rss.el --- Agregador rss para Emacs
;;;
;;; Autor: Manuel J. Garrido <jgarridosc@wanadoo.es>
;;;
;;; COMENTARIO
;;;
;;; mg-rss es un agregador rss para emacs. La idea la tomé de este artículo:
;;; http://www.xml.com/lpt/a/2003/01/02/tr.html Lo que yo he hecho
;;; ha sido modificar la hoja de estilos para que en lugar de generar html,
;;; genere código lisp, y llamar al procesador XSLT para que acceda a las
;;; fuentes rss y haga la transformación.
;;; 
;;; USO
;;; 
;;; Es necesario definir la variable "mg-rss-sources", especificando en ella las
;;; direcciones de las fuentes rss y la función que se quiere usar para leer una
;;; noticia concreta. Esta función es "browse-url" por defecto.
;;; Una vez definidas las fuentes, se puede ejecutar "M-x mg-rss". Esto generará
;;; un nuevo buffer en el que aparecerán los titulares de las noticias. 
;;; Situándose sobre uno de ellos se puede pulsar ENTER para leer la noticia. Si 
;;; se pulsa ENTER sobre el encabezado de una fuente, sus noticias se expanden o 
;;; contraen, y pulsando "u" se actualizan las noticias.
;;; 
;;; REQUISITOS
;;; 
;;; * Xalan-C++ Puedes conseguirlo aquí: 
;;;      http://xml.apache.org/xalan-c/index.html
;;;
;;; ARTÍCULOS RELACIONADOS
;;;
;;; What is RSS?
;;;     http://www.xml.com/pub/a/2002/12/18/dive-into-xml.html
;;;
;;; Never Mind the Namespaces: An XSLT RSS Client.
;;;     http://www.xml.com/lpt/a/2003/01/02/tr.html
;;;
;;;
;;; HECHO
;;;
;;; * Configurar las fuentes de información con custom en lugar de con 
;;; rss_sources.xml
;;;
;;; * Poder asociar una función lisp a una fuente de forma que se pueda usar
;;; una función distinta de "browse-url" para ver sus noticias.
;;;
;;; * Poder expandir y contraer una fuente de forma que se muestren u oculten 
;;; sus noticias
;;;
;;; POR HACER
;;;
;;; * No todas las fuentes deberían mostrarse expandidas de entrada. Debería
;;; ser configurable cuales se muestran al empezar.
;;;
;;; * Añadir más posibilidades al mapa de teclado, al menos ir a fuente 
;;; siguiente o anterior.
;;;
;;; * Mostrar hora de actualización de fuentes y noticias.
;;;

; DEBUG
(progn
  (if (featurep 'mg-rss)
      (unload-feature 'mg-rss))
  (add-to-list 'load-path default-directory))
; FIN DEBUG

(defgroup mg-rss nil
  "Agregador rss"
  :version "2.0"
  :group 'comm)

(defcustom mg-rss-sources
  nil
  "Lista de fuentes rss"
  :group 'mg-rss
  :type '(alist :key-type (string :tag "Address")
                :value-type (string :value "browse-url")))

(defconst mg-rss-style-sheet
  (concat (file-name-directory (locate-library "mg-rss")) "getRSS2el.xsl")
  "Hoja de estilos")

(defconst mg-rss-buffer
  "*rss*"
  "Buffer en el que se presentarán las noticias")

;;;
;; Estructura de datos para controlar la presentación
;;;

; Tabla en la que se almacenará información acerca de la presentación de las 
; fuentes. Es una tabla hash en la que la clave será la dirección de la fuente
; y el valor será un array con estos datos: marcador del inicio, marcador del
; final, expandida (s/n) y las noticias
(setq mg-rss-show-ctrl (make-hash-table :test 'equal))

(defun mg-rss-set-start (source)
  "Asigna el marcador de inicio en la posición actual de point"
  (aset (gethash source mg-rss-show-ctrl) 0 (point-marker)))

(defun mg-rss-get-start (source)
  "Obtiene el marcador de inicio correspondiente a la fuente"
  (aref (gethash source mg-rss-show-ctrl) 0))

(defun mg-rss-get-end (source)
  "Obtiene el marcador de fin correspondiente a la fuente"
  (aref (gethash source mg-rss-show-ctrl) 1))

(defun mg-rss-set-end (source)
  "Asigna el marcador de fin en la posición actual de point"
  (aset (gethash source mg-rss-show-ctrl) 1 (point-marker)))

(defun mg-rss-toggle-expanded (source)
  "Conmuta el valor del indicador de fuente expandida"
  (let ((data-array (gethash source mg-rss-show-ctrl)))
    (aset data-array 2 (not (aref data-array 2)))))

(defun mg-rss-get-expanded (source)
  "Obtiene el valor del indicador de fuente expandida"
  (aref (gethash source mg-rss-show-ctrl) 2))

(defun mg-rss-set-expanded (source expanded)
  "Asigna el indicador de fuente expandida"
  (aset (gethash source mg-rss-show-ctrl) 2 expanded))

(defun mg-rss-set-news (source)
  "Asigna el bloque de noticias de una fuente"
  (aset (gethash source mg-rss-show-ctrl) 3 
        (buffer-substring (mg-rss-get-start source) 
                          (1- (mg-rss-get-end source)))))

(defun mg-rss-get-news (source)
  "Obtiene el bloque de noticias de una fuente"
  (aref (gethash source mg-rss-show-ctrl) 3))

;;;
;; Funciones para expandir y contraer noticias
;;;

(defun mg-rss-cond-update (key value)
  (if (and (>= (point) (mg-rss-get-start key))
       (<= (point) (mg-rss-get-end key)))
        (mg-rss-update key)))

(defun mg-rss-update-at-point ()
  "Actualiza las noticias de la fuente en la que está el cursor"
  (interactive) ; Debe ser interactivo para poder ser asignado a una tecla
  (maphash 'mg-rss-cond-update mg-rss-show-ctrl))

(defun mg-rss-update (source-addr)
  "Actualiza las noticias de una fuente"
  (if (mg-rss-get-expanded source-addr)
      (mg-rss-collapse source-addr))
  (mg-get-ind-rss source-addr (cdr (assoc source-addr mg-rss-sources))))

(defun mg-rss-expand (source-addr)
  "Expande una fuente. No actualiza sus noticias."
  (let ((source-news (mg-rss-get-news source-addr)))
    (if source-news
        (save-excursion
          (goto-char (mg-rss-get-start source-addr))
          (insert source-news)
          (mg-rss-toggle-expanded source-addr))
      (mg-get-ind-rss source-addr (cdr (assoc source-addr mg-rss-sources))))))

(defun mg-rss-collapse (source-addr)
  "Contrae una fuente."
  (mg-rss-set-news source-addr)
  (delete-region (mg-rss-get-start source-addr) 
                 (1- (mg-rss-get-end source-addr)))
  (mg-rss-toggle-expanded source-addr))

(defun mg-rss-collapse-or-expand (source-addr)
  "Contrae o expande una fuente, dependiendo de su estado,"
  (if (mg-rss-get-expanded source-addr)
      (mg-rss-collapse source-addr)
    (mg-rss-expand source-addr)))

;;;
;; Definición del modo RSS
;;;

(setq rss-map (make-keymap))
(suppress-keymap rss-map)
(define-key rss-map "" 'help-follow)
(define-key rss-map "u" 'mg-rss-update-at-point)

(defun rss-mode ()
  "Modo rss"
  (setq major-mode 'rss-mode)
  (setq mode-name "RSS")
  (use-local-map rss-map))

;;;
;; Enlace con xalan
;;;

(defun mg-rss-show-news (process event)
  "Función que se usará como centinela para mostrar los titulares"
  ; El nombre del proceso coincide con el nombre de la fuente
  (if (not (string= event "finished\n"))
      (progn
        (kill-buffer (process-buffer process))
        (error "Error en la transformación XSLT"))
    (save-excursion
      (set-buffer mg-rss-buffer)
      (let ((point-aux (point)))        ; Guardo el punto del buffer
        (goto-char (mg-rss-get-start (process-name process)))
        (eval-buffer (process-buffer process))
        (goto-char point-aux)))
    (pop-to-buffer mg-rss-buffer)
    (mg-rss-set-expanded (process-name process) t))
  (kill-buffer (process-buffer process)))

(defun mg-get-ind-rss (source-addr source-func)
  "Obtiene los titulares de una única fuente."
;;   (let ((rss-process (start-process source-addr
;;                                     (concat "*Xalan-rss-"  source-addr) "xalan"
;;                                     "-e" "latin-1"
;;                                     "-p" "func" (concat "'" source-func "'")
;;                                     source-addr
;;                                     mg-rss-style-sheet)))

  (let ((rss-process (start-process source-addr
                                    (concat "*Xalan-rss-"  source-addr) "xalan"
                                    "-q" ;"latin-1"
                                    "-param" "func" (concat "'" source-func "'")
                                    "-in" source-addr
                                    "-xsl" mg-rss-style-sheet)))

    (set-process-sentinel rss-process 'mg-rss-show-news)))

;;;
;; Función principal
;;;

(defun mg-rss ()
  (interactive)

  (if (get-buffer mg-rss-buffer) 
      (kill-buffer mg-rss-buffer))

  (get-buffer-create mg-rss-buffer)

  (save-excursion 
    (set-buffer mg-rss-buffer)
    (erase-buffer)
    (rss-mode)
    (dolist (source mg-rss-sources)
      (help-insert-xref-button (concat "FUENTE: " (car source) "\n") 
                               'mg-rss-collapse-or-expand (car source))
      ; FIXME. Debería haber una función que ocultara la estructura de 
      ; mg-rss-show-ctrl
      (puthash (car source) 
               (vector (point-marker) nil nil nil) 
               mg-rss-show-ctrl)
      (insert "\n")
      (mg-rss-set-end (car source)))
    (goto-char (point-min)))

  (dolist (source mg-rss-sources)   
    (mg-get-ind-rss (car source) (cdr source))))

(provide 'mg-rss)