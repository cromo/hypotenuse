(import [omniORB [CORBA]] CosNaming)

(defn default-orb []
  (CORBA.ORB-init))

(defn name-components [context-name]
  (list-comp (CosNaming.NameComponent (str component) (str ""))
    [component (.split context-name "/")]))

(defn resolve-reference [reference-name &rest args]
  (setv args (dict (zip (slice args 0 None 2) (slice args 1 None 2))))
  (when (not (in :using args)) (assoc args :using (default-orb)))
  (let
    [[orb (get args :using)]
      [obj-name (str reference-name)]
      [obj (.resolve-initial-references orb obj-name)]]
    (if (in :as args)
      (.-narrow obj (get args :as))
      obj)))

;; Write a resolve function that takes the following keyword arguments:
;; name-context (mandatory)
;; :using orb (defaults to default orb)
;; :in root-context (defaults to "NameService")
;; :as type (default None; returns CORBA Object)
(defn resolve-name [context-name &rest args]
  (setv args (dict (zip (slice args 0 None 2) (slice args 1 None 2))))
  (when (not-in :using args) (assoc args :using (default-orb)))
  (when (not-in :in args) (assoc args :in "NameService"))
  (let
    [[orb (get args :using)]
      [passed-context (get args :in)]
      [root-context
        (if (string? passed-context)
          (resolve-reference (str passed-context) :as CosNaming.NamingContext)
          passed-context)]
      [obj (.resolve root-context (name-components context-name))]]
    (if (in :as args)
      (.-narrow obj (get args :as))
      obj)))

(defn iter-naming-context [context]
  (let [[it (-> context (.list 0) (get 1))]
    [val (.next_one it)]]
    (while (get val 0)
      (yield (get val 1))
      (setv val (.next_one it)))))