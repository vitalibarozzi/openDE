
import Physics.ODE.Body as Body
import Physics.ODE.World as World

main :: IO ()
main = do
    w <- World.create 
    b <- Body.create w
    error . show $ (w,b)
    putStrLn "Test suite not yet implemented"
