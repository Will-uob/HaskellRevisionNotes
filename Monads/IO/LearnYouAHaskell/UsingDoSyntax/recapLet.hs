cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * (r^2)
    in sideArea + 2 * topArea

-- In list comprehensions, we omit the in part of the let binding because the visibility
-- of the names is already predefined there.
-- However, we could use a let in binding in a predicate and the names 
-- defined would only be visible to that predicate.
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
