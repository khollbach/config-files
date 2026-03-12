{:flowstorm
 {:dependencies [[com.github.flow-storm/clojure "RELEASE"]
                 [com.github.flow-storm/flow-storm-dbg "RELEASE"]]
  :exclusions [org.clojure/clojure]
  :jvm-opts ["-Dflowstorm.startRecording=false"
             "-Dclojure.storm.instrumentEnable=true"
             "-Dclojure.storm.instrumentAutoPrefixes=true"]}}
