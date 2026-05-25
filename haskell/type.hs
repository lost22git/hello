main = do
  (print . osString) $ Linux {distro = "Debian", version = "Buster"}

-- data type
--
data OS = Linux {distro :: String, version :: String}
        | Mac {name::String}
        | Windows {version::String}
        deriving (Show, Eq)

osString :: OS -> String
osString (Linux distro version) = distro ++ " " ++ version
osString (Mac name) = "Mac " ++ name
osString (Windows version) = "Windows " ++ version
