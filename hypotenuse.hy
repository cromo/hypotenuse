(import [omniORB [CORBA]] CosNaming)

(defn get-default-orb []
  (CORBA.ORB-init))

(defn name-components [context-name]
  (list-comp (CosNaming.NameComponent (str component) (str ""))
    [component (.split context-name "/")]))

(defn resolve-reference [reference-name &rest kwargs]
  (let
    [[args (dict (zip (slice kwargs 0 None 2) (slice kwargs 1 None 2)))]
      [orb (.get args :using (get-default-orb))]
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
(defn resolve-name [context-name &rest kwargs]
  (let
    [[args (dict (zip (slice kwargs 0 None 2) (slice kwargs 1 None 2)))]
      [orb (.get args :using (get-default-orb))]
      [passed-context (.get args :in "NameService")]
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