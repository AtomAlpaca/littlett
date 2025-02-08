import Base
import Equal

main :: IO ()
main = do
    print
        (case (alphaEqual (Lam (Name "x") (Var (Name "x"))) (Lam (Name "y") (Var (Name "y")))) of
        True -> "Yes"
        False -> "No")
