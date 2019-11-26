(ns clj.pikkpakk
  (:require [badigeon.bundle :as bundle]
            [badigeon.compile :as compile]
            [badigeon.clean :as clean]
            [clojure.tools.cli :as cli]
            [clojure.string :as str])
  (:import (java.io File)
           (com.google.cloud.tools.jib.api LayerConfiguration RegistryImage Jib AbsoluteUnixPath DockerDaemonImage Containerizer Credential CredentialRetriever ImageReference JibContainerBuilder)
           (java.time Instant)
           (java.util.function BiFunction Consumer)
           (java.nio.file FileSystems Paths Path)
           (java.util Optional)
           (com.google.cloud.tools.jib.frontend CredentialRetrieverFactory)
           (java.lang.management ManagementFactory)))

(defn done-seconds []
  (format "%.2fs" (double (/ (.getUptime (ManagementFactory/getRuntimeMXBean)) 1000))))

(def classfile-matcher (-> (FileSystems/getDefault)
                           (.getPathMatcher "glob:**.class")))

(def timestamp-provider
  (reify BiFunction
    (apply [_ source-path destination-path]
      (if (.matches classfile-matcher source-path)
        (Instant/ofEpochSecond 8589934591)
        LayerConfiguration/DEFAULT_MODIFICATION_TIME))))

(defn ^Path get-path [^String path & rst]
  (Paths/get path (into-array String (or rst []))))

(defn explicit-credentials [username password]
  (when (and (string? username) (string? password))
    (reify CredentialRetriever
      (retrieve [_]
        (Optional/of (Credential/from username password))))))

(defn ns->class [ns]
  (str/replace ns "-" "_"))

(defn add-labels [^JibContainerBuilder jib-container-builder labels]
  (reduce (fn [acc [key value]]
            (.addLabel acc key value))
          jib-container-builder
          labels))

(defn add-layer [^JibContainerBuilder container-builder target-path]
  (.addLayer container-builder
             (-> (LayerConfiguration/builder)
                 (.setName target-path)
                 (.addEntryRecursive (get-path "target" target-path)
                                     (AbsoluteUnixPath/get (str "/app/" target-path))
                                     LayerConfiguration/DEFAULT_FILE_PERMISSIONS_PROVIDER
                                     timestamp-provider)
                 (.build))))

(def no-op-logger
  (reify Consumer
    (accept [this t]
      nil #_(println (.getMessage t)))))

(defn ^RegistryImage registry-image [^String img-name user pass]
  (-> (RegistryImage/named img-name)
      (.addCredentialRetriever (or (explicit-credentials user pass)
                                   (-> (CredentialRetrieverFactory/forImage (ImageReference/parse img-name) no-op-logger)
                                       (.dockerConfig))))))

(defn add-additional-tags [containerizer tags]
  (reduce (fn [acc tag]
            (.withAdditionalTag acc tag))
          containerizer
          tags))

(defn jib [{:keys [^String main
                   ^String base-image
                   ^String from-registry-username
                   ^String from-registry-password
                   ^String to-registry-username
                   ^String to-registry-password
                   ^String creation-time
                   ^String user
                   ^String image-name
                   image-type
                   additional-tag
                   label]}]
  (let [base-image (registry-image base-image from-registry-username from-registry-password)
        destination (case image-type
                      :docker (DockerDaemonImage/named image-name)
                      :registry (registry-image image-name to-registry-username to-registry-password))
        creation-time (->> ^String (or creation-time "0")
                           (Long/valueOf)
                           (Instant/ofEpochSecond))
        container-builder (Jib/from base-image)
        container (-> container-builder
                      (.setUser user)
                      (.setCreationTime creation-time)
                      (add-layer "jars")
                      (add-layer "lib")
                      (add-layer "classes")
                      (add-labels label)
                      (.setWorkingDirectory (AbsoluteUnixPath/get "/app"))
                      (.setEntrypoint (into-array String ["java"
                                                          "-Dclojure.main.report=stderr"
                                                          "-Dfile.encoding=UTF-8"
                                                          "-cp" (str/join ":" ["/app/classes" "/app/lib" "/app/jars/*"])
                                                          (ns->class main)]))
                      (.containerize (-> (Containerizer/to destination)
                                         (add-additional-tags additional-tag))))]
    (println "\uD83D\uDE9C"
             (case image-type
               :docker "Built container"
               :registry "Built and pushed container")
             image-name
             "with ImageId/digest" (.getHash (.getImageId container))
             "Container/digest" (.getHash (.getDigest container))
             "in" (done-seconds))))

(def image-types #{:docker :registry})

(def ^:private cli-options
  (concat
    [[nil "--image-name NAME" "Name of the image including tag, e.g. demo:latest"]
     [nil "--image-type TYPE" (str "Type of the image, one of: " (str/join ", " (map name image-types)))
      :validate [image-types (str "Supported image types: " (str/join ", " (map name image-types)))]
      :parse-fn keyword
      :default :docker
      :default-desc (name :docker)]
     [nil "--base-image BASE-IMAGE" "Base Docker image to use" :default "gcr.io/distroless/java:11"]
     [nil "--creation-time CREATION-TIME-EPOCH" "Set creation time of image in epoch seconds, e.g. $(git log -1 --pretty=format:%ct) Defaults to 0."]
     [nil "--additional-tag TAG" "Additional tag for the image, e.g latest. Repeat to add multiple tags" :assoc-fn #(update %1 %2 conj %3)]
     [nil "--label LABEL=VALUE" "Set a label for the image, e.g. GIT_COMMIT=${CI_COMMIT_SHORT_SHA}. Repeat to add multiple labels." :assoc-fn #(update %1 %2 conj (str/split %3 #"="))]
     [nil "--user USER" "Set the user and group to run the container as. Valid formats are: user, uid, user:group, uid:gid, uid:group, user:gid"]
     [nil "--from-registry-username USER" "Set the username to use when pulling from registry, e.g. gitlab-ci-token."]
     [nil "--from-registry-password PASSWORD" "Set the password to use when pulling from registry, e.g. ${CI_JOB_TOKEN}."]
     [nil "--to-registry-username USER" "Set the username to use when deploying to registry, e.g. gitlab-ci-token."]
     [nil "--to-registry-password PASSWORD" "Set the password to use when deploying to registry, e.g. ${CI_JOB_TOKEN}."]
     ["-q" "--quiet" "Don't print a progress bar nor a start of build message" :default false] ; TODO implement
     ["-v" "--verbose" "Print status of image building" :default false] ; TODO implement
     ["-m" "--main SYMBOL" "Main namespace"]]
    [["-h" "--help" "show this help"]]))

(defn- show-usage
  [summary exit-code]
  (println (->>
             [(format "Usage: clj -m %s [options]" (str *ns*))
              ""
              "Options:"
              summary]
             (str/join \newline)))
  (System/exit exit-code))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn set-timbre-log-level! []
  (try
    (eval
      '(do
         (require '[taoensso.timbre :as timbre])
         (timbre/set-level! :info)))
    (catch Exception e
      nil)))

(defn make-container [{:keys [main] :as options}]
  (set-timbre-log-level!)
  (clean/clean "target")
  (bundle/bundle (bundle/make-out-path 'lib nil))
  (-> (File. (File. "target" "lib") "lib")
      (.renameTo (File. "target" "jars")))
  (compile/compile (symbol main) {:compile-path "target/classes"})
  (jib options))

(defn -main [& args]
  (let [{{:keys [main
                 image-name
                 help]
          :as   options} :options
         :as             parsed-opts} (cli/parse-opts args cli-options)
        errors (:errors parsed-opts)
        summary (:summary parsed-opts)]
    (cond (or (nil? main) (nil? image-name))
          (show-usage summary 1)
          help
          (show-usage summary 0)
          errors
          (do (println (error-msg errors))
              (System/exit 1))
          :else
          (make-container options))))
