import Control.Applicative

main = do 
    [xa,ya,xb,yb,xc,yc] <- fmap ( read :: String -> Float ) . words <$> getLine
    -- 座標点がA(0,0) B(a,b) C(c,d)の場合の面積は|ad-bc|/2である．
    -- すなわち，どれかの座標を原点とする座標系を考え，それに合わせて残り２点を元の座標系からシフトさせて
    -- 以上の計算を行えば良い．
    -- A(xa,ya) B(xb,yb) C(xc,yc)の場合，Aを原点とする座標系にすれば
    -- A(0,0) B(xb-xa,yb-ya) c(xc-xa,yc-ya)という新しい座標系に適した点になる．
    putStrLn $ show(abs( ((xb - xa) * (yc - ya) - (yb - ya) * (xc - xa)) ) / 2.0)
