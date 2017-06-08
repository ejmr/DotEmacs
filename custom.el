(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#C2C2C2" "#161616" "#252525" "#080808" "#0E0E0E" "#161616" "#080808" "#080808"])
 '(ansi-term-color-vector
   [unspecified "#f7f7f7" "#7c7c7c" "#8e8e8e" "#a0a0a0" "#686868" "#747474" "#686868" "#464646"] t)
 '(beacon-color "#F8BBD0")
 '(browse-url-browser-function (quote eww-browse-url))
 '(browse-url-generic-program "conkeror")
 '(caps-lock-mode nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tao-yang)))
 '(custom-safe-themes
   (quote
    ("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "d09925822ad3bd9e0877c5fa95925aafe38650ab4769aff141a5099bc01e09bb" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "825719a9c429bee280aef50758e18b904a1d977cb52b34d2dca35e4baf610211" "ebe90a9395fe0f2ac2f9bf8589daf28dc027b427b3e569e7b1eea795a924a136" "3e8ea6a37f17fd9e0828dee76b7ba709319c4d93b7b21742684fadd918e8aca3" "8f4643948c6cefdf71c19297a83292e3f37c5d072aa8bd53a8d2e64729ba6ee6" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "ad8a32bcf262128230482908e4786fdf4973cebbff45fd8bffa8a61cf74dec28" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "13a821b5a1adc2c287911d0f126242d47503c0be0a9ced5ea5a642a66fe03fe0" "f75cb64bbea67bb0c50c1ae4c0d5848faf7786250f669522b05cd0641a9a310a" "a285e7bc4714013b6cd74ac4663b9fa5e8f734e989fcca42069c45ed702d4b72" "ff3ca9d675ad57854fb5485831a3c7d5f34aa47af4de0c24335b7efa02dbb8f9" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "ec13410d459f1b67158c500d13d290560fc4dad2edaaa22e33a4d1df08e8f887" "7f4b67cb8aff9eb76ef818b3e41ed5f03581799f8e31899c93ec85b0ef049ceb" "9541f1dc11258239ef02aa1a5e9db3e1e46bc8fb1d7dbe83946c1541ae6dbdf9" "4a51697271e3fd202d3da73bad80d0ec5cab7e0bb1db30f79fd1d6dd6a7812dc" "42e53050264917f453ac88ffa84f3ae79187f5e47ac4ae1d094b7b5b60cf6c82" "d3a7eea7ebc9a82b42c47e49517f7a1454116487f6907cf2f5c2df4b09b50fc1" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "01490fa7832bdadd9502e5e1c831e2b18a5f48c37ed48780741081d0414ee7f2" "3cddc1775f6c26573a69315dacd5fd45a6cd04df539b6354281d316985f254f3" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "f869a5d068a371532c82027cdf1feefdc5768757c78c48a7e0177e90651503ad" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "a2dd771a05705be2a6e6adb6ddbc7a27ebf49edab1dffdbefe243096becba7c9" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "dd6e52a5b1180f5c8bf408764a32867e2fa86594ded78a29040cafce6a4ea808" "ecb9fe1d5b165a35499191a909b2b5710a52935614058b327a39bfbbb07c7dc8" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "38e66a2a20fa9a27af5ffc4f4dd54f69e3fef6b51be7b351e137b24958bfebd7" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "8062d7fd259d3232d69b38db3b15d4ac44a70bf620cbc5b3926a6e16c74d6a5a" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "a14dd0b389ef4d6f867d3072d84935603bc88657a18e0409bbb460e7cfc991f5" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "eb07ee737bae7860ff12a4dbd2dcb9ff9712e517cfd6279fa74f04a17b6e1ba6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "130319ab9b4f97439d1b8fd72345ab77b43301cf29dddc88edb01e2bc3aff1e7" "20e23cba00cf376ea6f20049022241c02a315547fc86df007544852c94ab44cb" "707227acad0cf8d4db55dcf1e574b3644b68eab8aca4a8ce6635c8830bc72144" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "0b7ee9bac81558c11000b65100f29b09488ff9182c083fb303c7f13fd0ec8d2b" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "dd1cf47034b1c20f5f43cd91ae76f00abef05f91b7be57d94653c493bcf41dda" "704a108ab29ea8fc8feb718a538e9ce45d732dce41e5b04585f43a6352c5519e" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "23ccf46b0d05ae80ee0661b91a083427a6c61e7a260227d37e36833d862ccffc" "63dd8ce36f352b92dbf4f80e912ac68216c1d7cf6ae98195e287fd7c7f7cb189" "5d3e0746023fc5e246eb3e0e48c1ccb5ce0387fc4273896c6cf02ee349c2eba8" "1c656eb3f6ae6c84ced46282cb4ed697bffe2f6c764bb5a737ed7ca6d068f798" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "japanese-hiragana")
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
			 (:color "#c0c0c0"))
     (implicitParams :underline
		     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000"))))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-color "#F0F0F0")
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-gjslint javascript-jscs javascript-standard json-jsonlint json-python-json less lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-mdl rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(free-keys-modifiers (quote ("" "C" "M" "C-M" "s")))
 '(global-company-mode nil)
 '(global-flycheck-mode nil)
 '(global-origami-mode t)
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(hl-sexp-background-color "#efebe9")
 '(indent-guide-global-mode t)
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(linum-format " %7i ")
 '(linum-relative-global-mode nil)
 '(magit-diff-use-overlays nil)
 '(neo-smart-open t)
 '(neo-theme (quote nerd))
 '(nrepl-message-colors
   (quote
    ("#ee11dd" "#8584ae" "#b4f5fe" "#4c406d" "#ffe000" "#ffa500" "#ffa500" "#DC8CC3")))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (worf sublime-themes beginend json-navigator flycheck-package wandbox color-theme-sanityinc-tomorrow git-link psysh cmake-ide zerodark-theme elfeed arjen-grey-theme nubox find-file-in-project magic-latex-buffer elisp-refs org-parser link-hint ace-jump-buffer avy-flycheck ace-flyspell iflipb suggest stupid-indent-mode neon-mode qwe alda-mode dashboard browse-at-remote company-rtags company-php flycheck-rtags ivy-rtags malinka yankpad org-board ox-pandoc ox-asciidoc org-brain ob-php org-readme neotree schrute focus avy-menu rpn-calc ace-isearch copy-as-format emacsql-sqlite emacsql kill-or-bury-alive modalka el-mock mmt highlight-blocks vlf ini-mode symbol-overlay lispy mandm-theme lenlen-theme po-mode hierarchy intellij-theme lentic assess nord-theme rebecca-theme xmlgen brainfuck-mode diff-hl irony general resize-window iedit kaolin-theme restclient nhexl-mode god-mode vdiff composable dumb-jump ivy-todo direnv yaml-mode restart-emacs counsel-gtags counsel-dash wc-mode gandalf-theme white-theme paper-theme tern make-it-so git-timemachine flycheck-rust cargo editorconfig-custom-majormode editorconfig selected cov counsel-projectile projectile lice indent-guide tldr auto-dim-other-buffers emmet-mode go-mode dracula-theme base16-theme tango-plus-theme runner ryo-modal fish-mode 0xc realgud origami company-mode org-mode org-install polymode webpaste cycle-quotes green-screen-theme haskell-mode goose-theme flatui-theme color-theme-modern rainbow-identifiers rainbow-delimiters seoul256-theme punpun-theme plain-theme paganini-theme organic-green-theme material-theme majapahit-theme labburn-theme hydandata-light-theme avk-emacs-themes alect-themes leuven-theme madhat2r-theme zenburn-theme plan9-theme jazz-theme apropospriate-theme eziam-theme fn find-temp-file auto-yasnippet ace-link flycheck-mypy flyspell-correct-ivy free-keys adoc-mode dired-atool dired-efap hl-todo dired-k ws-butler tiny rich-minority flycheck-clangcheck clang-format ace-window syntactic-close linum-relative duplicate-thing clojure-mode company-quickhelp operate-on-number firestarter org tao-theme moe-theme creamsody-theme doom-themes zweilight-theme yasnippet which-key tomatinho pandoc-mode js2-mode github-theme darkroom corral pp-c-l caps-lock company-lua avy-zap fountain-mode python-mode php-mode key-seq win-switch ivy-hydra ivy-pages avy recursive-narrow nameless company flycheck rust-mode tup-mode markdown-mode quickrun comment-dwim-2 key-chord nasm-mode refine counsel change-inner expand-region lua-mode forth-mode solarized-theme ivy hydra use-package)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(safe-local-variable-values
   (quote
    ((firestarter . ert-run-tests-interactively)
     (firestarter byte-compile-file
		  (buffer-file-name)))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tabbar-background-color "#ffffff")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-enable-imenu-support t)
 '(vc-annotate-background "#D9D9D9")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#3C3C3C")
     (60 . "#3C3C3C")
     (80 . "#252525")
     (100 . "#252525")
     (120 . "#161616")
     (140 . "#161616")
     (160 . "#0E0E0E")
     (180 . "#0E0E0E")
     (200 . "#0E0E0E")
     (220 . "#080808")
     (240 . "#080808")
     (260 . "#080808")
     (280 . "#080808")
     (300 . "#080808")
     (320 . "#080808")
     (340 . "#080808")
     (360 . "#080808"))))
 '(vc-annotate-very-old-color "#161616")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:foreground "#a99"))))
 '(markdown-markup-face ((t (:foreground "#AAA"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(pp^L-highlight ((t (:box (:line-width 3 :style released-button)))))
 '(which-func ((t (:slant italic :weight bold)))))
