(ns spel.ink
  (:require ["inkjs/dist/ink.js" :as ink :refer [Story]]
            [applied-science.js-interop :as j]
            [lambdaisland.puck.types :as types])
  (:require-macros [spel.ink :refer [inline-file]]))

(types/register-keys-printer ink/Story 'ink/Story [:_listDefinitions :_mainContentContainer :_state])
(types/register-keys-printer ink/InkList 'ink/InkList [:origins])

(defn bind-fn [story name f]
  (.BindExternalFunction ^js story name f))

(defn story
  ([json]
   (story json nil))
  ([json {:keys [functions]}]
   (let [story (Story. json)]
     (doseq [[n f] functions]
       (bind-fn story (name n) f))
     story)))

(defn continue! [story]
  (.Continue ^js story))

(defn can-continue? [story]
  (.-canContinue ^js story))

(defn text [story]
  (.-currentText ^js story))

(defn tags [story]
  (.-currentTags ^js story))

(defn choices [story]
  (for [choice (.-currentChoices ^js story)]
    (let [{:keys [text index threadAtGeneration
                  sourcePath targetPath isInvisibleDefault
                  originalThreadIndex]} (j/lookup choice)]
      {:text text
       :index index
       :thread-at-generation threadAtGeneration
       :source-path sourcePath
       :target-path targetPath
       :invisible-default? isInvisibleDefault
       :original-thread-index originalThreadIndex})))

(defn make-choice! [story index]
  (.ChooseChoiceIndex ^js story index))

(defn goto-path! [story path]
  (.ChoosePathString ^js story path))

(defn variable-state [story]
  (js->clj (.-variablesState ^js story)))

(defn set-variable [story variable value]
  (j/assoc-in! story [:variablesState variable] value))

(comment
  (def json (subs (inline-file "resources/public/scenario.json") 1))

  (.toString (fn [] (js/JSON.parse json)))

  (subs json 1)

  (def story (Story. json))

  (.Continue story)
  (.-canContinue story)
  (.-currentChoices story)
  (.ChooseChoiceIndex story 0)

  (.ChoosePathString story "gang")

  (js-keys (.-variablesState story))

  (.BindExternalFunction story "foo" (fn [] (js/console.log "in foo"))))

;;  EXTERNAL playSound(soundName)
;;  ~ playSound("thing")
