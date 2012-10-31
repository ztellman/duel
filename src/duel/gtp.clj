;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns duel.gtp
  (:use
    [potemkin])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.java.shell :as sh])
  (:import
    [java.io
     PipedOutputStream
     PipedInputStream
     InputStream
     OutputStream]))

