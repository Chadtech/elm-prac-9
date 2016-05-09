module Source where

root : String
--root = "https://raw.githubusercontent.com/Chadtech/elm-prac-7/master/public/"
root = "./"

src : String -> String
src str = root ++ str ++ ".png"