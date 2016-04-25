(ns atisit.core
  (:require
   [seesaw.core :as ss]
   [seesaw.chooser :as chooser]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   [javax.imageio ImageIO]
   [javax.swing.filechooser FileNameExtensionFilter]
   [java.awt.image BufferedImage Raster IndexColorModel]
   [java.awt Rectangle Transparency Color]
   [java.io File])
  (:gen-class))

(defn read-image [location]
  (try
    (cast BufferedImage (ImageIO/read (io/file location)))
    (catch Exception e
      (.printStackTrace e)
      (System/exit 1))))

(defn snap-image-vertically [^BufferedImage image space]
  (let [width (.getWidth image)
        height (.getHeight image)
        cm (.getColorModel image)
        ^BufferedImage snapped (if (instance? IndexColorModel cm)
                                 (BufferedImage. (int (+ width space)) (int height) (int (.getType image)) cm)
                                 (BufferedImage. (int (+ width space)) (int height) (int (.getType image))))
        ^Raster first-half (.getData image (Rectangle. 0 0 (/ width 2) height))
        ^Raster second-half (.getData image (Rectangle. (/ width 2) 0 (/ width 2) height))
        ^Raster second-half (.createChild second-half (.getMinX second-half) (.getMinY second-half) (.getWidth second-half) (.getHeight second-half) (+ space (.getMinX second-half)) (.getMinY second-half) nil)
        alpha? (not= (.getTransparency image) Transparency/OPAQUE)
        snap-data (into-array Integer/TYPE (repeat (* height space) (if alpha?
                                                                      (.getRGB (Color. (int 255) (int 255) (int 255) (int 0)))
                                                                      (.getRGB (Color. (int 255) (int 255) (int 255) (int 255))))))]
    (doto
      snapped
      (.setData first-half)
      (.setData second-half)
      (.setRGB (/ width 2) 0 space height snap-data 0 space))))

(defn snap-image-horizontally [^BufferedImage image space]
  (let [width (.getWidth image)
        height (.getHeight image)
        cm (.getColorModel image)
        ^BufferedImage snapped (if (instance? IndexColorModel cm)
                                 (BufferedImage. (int width) (int (+ height space)) (int (.getType image)) cm)
                                 (BufferedImage. (int width) (int (+ height space)) (int (.getType image))))
        ^Raster first-half (.getData image (Rectangle. 0 0 width (/ height 2)))
        ^Raster second-half (.getData image (Rectangle. 0 (/ height 2) width (/ height 2)))
        ^Raster second-half (.createChild second-half (.getMinX second-half) (.getMinY second-half) (.getWidth second-half) (.getHeight second-half) (.getMinX second-half) (+ space (.getMinY second-half)) nil)
        alpha? (not= (.getTransparency image) Transparency/OPAQUE)
        snap-data (into-array Integer/TYPE (repeat (* width space) (if alpha?
                                                                     (.getRGB (Color. (int 255) (int 255) (int 255) (int 0)))
                                                                     (.getRGB (Color. (int 255) (int 255) (int 255) (int 255))))))]
    (doto
      snapped
      (.setData first-half)
      (.setData second-half)
      (.setRGB 0 (/ height 2) width space snap-data 0 width))))

(defn write-image [image location]
  (try
    (ImageIO/write image (last (string/split location #"\.")) (io/file location))
    (catch Exception e
      (.printStackTrace e)
      (System/exit 1))))

(declare atisit-frame)

(def snap-fns {:horizontal snap-image-horizontally
               :vertical snap-image-vertically})

(def selected-file-label (ss/label
                          :text "Select an image"
                          :halign :center
                          :valign :center
                          :bounds [150 10 200 20]))

(defn choose-file [& args]
  (chooser/choose-file
   atisit-frame
   :filters [(FileNameExtensionFilter. "Image Files" (into-array ["png" "PNG" "jpg" "JPG" "jpeg" "JPEG" "gif" "GIF"]))]
   :success-fn #(ss/config! selected-file-label :text (.getAbsolutePath (io/file %2)))
   :all-files? false))

(def browse-button (ss/button
                    :text "Browse"
                    :listen [:action choose-file]
                    :bounds [150 40 200 40]))

(def select-method (ss/button-group))

(def num-pixels (ss/text
                 :text "1"
                 :bounds [400 110 30 20]))

(def pixel-label (ss/label
                  :text "Pixels"
                  :halign :center
                  :valign :center
                  :bounds [366 85 100 20]))

(defn snap-image [& args]
  (let [selected (ss/id-of (ss/selection select-method))
        snap-fn (get snap-fns selected)
        location (ss/text selected-file-label)
        pixels (apply str (filter #(Character/isDigit %1) (ss/text num-pixels)))]
    (if (and (not (empty? pixels)) (not= location "Select an image"))
      (write-image (snap-fn (read-image location) (Integer/parseInt pixels)) (string/replace (last (string/split location (re-pattern (if (= "\\" (str File/separator))
                                                                                                                                        "\\\\"
                                                                                                                                        (str File/separator))))) #"\." "_snapped."))
      (if (= location "Select an image")
        (ss/alert atisit-frame "Select an image first!")
        (ss/alert atisit-frame "Enter a valid number of pixels.")))))

(def snap-button (ss/button
                  :text "Snap!"
                  :listen [:action snap-image]
                  :bounds [150 150 200 50]))

(def atisit-frame (ss/frame
                   :title "Actually The Image Snaps In Two"
                   :content (ss/xyz-panel
                             :items [selected-file-label
                                     browse-button
                                     (ss/radio
                                      :text "horizontal"
                                      :id :horizontal
                                      :group select-method
                                      :bounds [200 100 200 20]
                                      :selected? true)
                                     (ss/radio
                                      :text "vertical"
                                      :id :vertical
                                      :group select-method
                                      :bounds [200 120 200 20])
                                     pixel-label
                                     num-pixels
                                     snap-button])
                   :size [500 :by 250]
                   :resizable? false
                   :on-close :exit))

(defn init-gui []
  (ss/show!
   atisit-frame))

(defn -main
  [& args]
  (init-gui))
