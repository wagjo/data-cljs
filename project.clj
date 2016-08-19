(defproject com.wagjo/data-cljs "0.2.1-SNAPSHOT"
  :description "Finger trees and miscellaneous functions for ClojureScript data structures."
  :url "https://github.com/wagjo/data-cljs"
  :scm {:name "git" :url "https://github.com/wagjo/data-cljs"}
  :signing {:gpg-key "AFA4E115"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :source-paths ["src/cljs"]
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as ClojureScript"})
