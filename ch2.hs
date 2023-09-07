data Shape = Rectangle Side Side 
            | Ellipse Radius Radius
            | RtTriangle Side Side
            | Polygon [Vertex]
    deriving Show

type Side = Float
type Radius = Float
type Vertex = (Float, Float)

{-2.1
    defining rectangle and rtTriangle in terms of vertex
-}

Rectangle x y = Polygon [(0,0), (x,0), (x,y), (0,y)]  --[(0,0), (s1,0), (s1,s2), (0,s2)]

rtTriangle x y = Polygon [(0,0), (x,0), (0,y)]

{-2.2
    regularPolygon definition with n sides of length s
-}


regularPolygon :: Int -> Side -> Shape
regularPolygon n s = Polygon (map get_v [1..n])
                    where angle = 2*pi/ (fromIntegral n)
                          half_a = angle/2
                          new_angle i = fromIntegral i*angle
                          r = (s/2)/ (sin half_a)
                          get_v i = (r*cos(new_angle i),r*sin(new_angle i))

{- 2.3
area (Rectangle s1 s2) = area (Polygon [(0,0),(s1,0),(s1,s2),(0,s2)])

Heron's formula :
A = sqrt(s(s-a)(s-b)(s-c))
    where s = semiperimeter = (a+b+c)/2
          a,b,c = lengeth between vertices

starting from RHS
RHS = triArea (0,0) (s1,0) (s1,s2) + areaPolygon (0,0):(s1,s2):(0,s2)
    (applying heron's formula on triArea)
    a = s1, b = s2, c = sqrt(s1^2+s2^2)
    => sqrt $ s(s-a)(s-b)(s-c)
    s = (a+b+c)/2 = (s1+s2+c)/2
    s(s-s1)(s-s2)(s-c) = in two parts
    s(s-c) = (s1+s2+c)/2 * [(s1+s2+c)/2-c] = 1/4 * (s1+s2 + c) (s1+s2 - c)
                                           = 1/4 * (s1+s2)^2 - c^2
                                           = 1/4 * (s1+s2)^2 - (s1^2+s2^2)
                                           = 1/4 * (s1^2 + 2*s1*s2 + s2^2    - s1^2 - s2^2)
                                           = 1/4 * 2*s1*s2 = 1/2 * s1* s2
    (s-s1)(s-s2) = [(s1+s2+c)/2-s1][(s1+s2+c)/2 - s2] = 1/2 * (c+s2-s1) (c-(s2-s1)) =1/4 * (c^2 - (s2-s1)^2)
                 = 1/4 * (s1^2+s2^2) - [s2^2+s1^2-2*s1*s2] 
                 = 1/4 * 2*s1*s2 = 1/2 * s1*s2

    therefore sqrt [s(s-c) * (s-s1)(s-s2)] = 1/2 * s1*s2 --this is area formed by first 3 vertices of rectangle

    Similarly - the other remaining triangle (which will recursively compute until wildcard pattern which has 0 area is hit) will also give area of 1/2*s1*s2
    
    Combining 1/2*s1*s2 + 1/2*s1*s2 = s1*s2 = area (Rectangle s1 s2)

    As we set out to do. Hence proved. QED -}


convex :: Shape -> Bool

convex (Rectangle _ _) = True
