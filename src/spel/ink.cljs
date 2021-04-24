(ns spel.ink
  (:require ["inkjs/dist/ink.js" :refer [Story]])
  (:require-macros [spel.ink :refer [inline-file]]))

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
