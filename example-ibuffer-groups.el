;; add example groups
(add-to-list 'ibuffer-saved-filter-groups
	     `("example"
	       ,my-default-filter-groups
	       ("C" (mode . c-mode))))

;; override my-groups to load example groups
(setq my-groups "example")
