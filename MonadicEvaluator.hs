module MonadicEvaluator where

import TypesAndTerms

type Store = [(Loc, Val)]

emptystore :: Store
emptystore = []

updatel :: Loc -> Val -> Store -> Maybe Store
updatel l v [] = Nothing
updatel l v ((l',v'):xs) = if l == l' then Just ((l, v):xs) else
                             let rest = updatel l v xs in
                             case rest of
                               Nothing -> Nothing
                               (Just rest') -> Just ((l', v'):rest')

newtype Evaluator a = Evaluator { con::(Store -> (a, Store)) }

instance Monad Evaluator where
    -- return :: a -> Parser a
    return v = Evaluator (\s -> (v, s))
    -- (>>=) :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
    (Evaluator e) >>= f =
        Evaluator (\s -> let (v1, s1) = e s in
                         let (v2, s2) = con (f v1) s1 in
                         (v2, s2))

nextloc :: Evaluator Loc
nextloc = Evaluator (\s -> case s of
                             [] -> (0, s)
                             ((xl, xv):xs) -> (xl + 1, s))

lookuploc :: Loc -> Evaluator Val
lookuploc l = Evaluator (\s -> let v = lookup l s in
                               case v of
                                 Nothing -> error ("location " ++ show l
                                                   ++ " not in memory")
                                 Just v1 -> (v1, s))

updateloc :: Loc -> Val -> Evaluator ()
updateloc l v = Evaluator (\s -> let s' = updatel l v s in
                                 case s' of
                                   Nothing -> error ("location " ++
                                                     show l ++
                                                     " not in memory")
                                   Just s1 -> ((), s1))

addtostate :: Loc -> Val -> Evaluator ()
addtostate l v = Evaluator (\s -> ((), (l, v):s))

tmeval :: Tm -> Env -> Evaluator Val
tmeval (TmTrue) n = return (ValBool True)
tmeval (TmFalse) n = return (ValBool False)
tmeval (TmIf tm1 tm2 tm3) n = do (ValBool valbool) <- tmeval tm1 n
                                 valthen <- tmeval tm2 n
                                 valelse  <- tmeval tm3 n
                                 if valbool then
                                     return valthen else
                                     return valelse
tmeval (TmZero) n = return (ValNum 0)
tmeval (TmSucc tm) n = do (ValNum valnum) <- tmeval tm n
                          return (ValNum (valnum + 1))
tmeval (TmPred tm) n = do (ValNum valnum) <- tmeval tm n
                          if (valnum - 1) < 0 then
                              return (ValNum 0) else
                              return (ValNum (valnum - 1))
tmeval (TmIsZero tm) n = do (ValNum valnum) <- tmeval tm n
                            if valnum == 0 then
                                return (ValBool True) else
                                return (ValBool False)
tmeval (TmVar var) n = let val = (n var) in
                       case val of 
                         Nothing -> error ("variable " ++ show var ++
                                                       " not in context")
                         (Just x) -> return x
tmeval (TmAbs var ty tm) n = return (ValAbs var tm n)
tmeval (TmApp tm1 tm2) n = do tm1' <- tmeval tm1 n
                              (ValAbs var tm1' n') <- tmeval tm1 n
                              val2 <- tmeval tm2 n
                              x <- tmeval tm1' (extend n' var val2)
                              return x
-- tmeval (TmFix tm) n = do (ValAbs var tm1 n1) <- tmeval tm n
--                          val2 <- tmeval tm1
--                                  (extend n1 var
--                                   var
--                                   (tmeval (TmFix (TmAbs var TyUnit tm1))
--                                           n1))
--                          return val2
-- tmeval (TmFix var ty tm) n = return (ValFix var tm n)
-- tmeval (TmFix var ty tm) n = Evaluator (\s -> (f, s'))
--     where (f, s') = (tmeval tm (extend n var f))
--    where f = tmeval tm (extend n var (con f 
tmeval (TmFix var ty tm) n = Evaluator
                             (\s -> let (f, s') =
                                            con (tmeval tm (extend n var f)) s in
                                    (f, s'))
tmeval (TmUnit) n = return (ValUnit)
tmeval (TmRef tm) n = do val <- tmeval tm n
                         l <- nextloc
                         addtostate l val
                         return (ValLoc l)
tmeval (TmDeref tm) n = do (ValLoc loc) <- tmeval tm n
                           val <- lookuploc loc
                           return val
tmeval (TmAssign tm1 tm2) n = do (ValLoc loc) <- tmeval tm1 n
                                 val <- tmeval tm2 n
                                 updateloc loc val
                                 return ValUnit
tmeval (TmLoc loc) n = return (ValLoc loc)

eval :: Tm -> Val
eval tm = fst (con (tmeval tm emptyenv) emptystore)