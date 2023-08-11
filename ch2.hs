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
