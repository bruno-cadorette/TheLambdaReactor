elm-package install --yes
copy "src\Frontend\external\Collage.elm" "elm-stuff\packages\elm-lang\core\3.0.0\src\Graphics" /y
copy "src\Frontend\external\Collage.js" "elm-stuff\packages\elm-lang\core\3.0.0\src\Native\Graphics" /y
elm-make