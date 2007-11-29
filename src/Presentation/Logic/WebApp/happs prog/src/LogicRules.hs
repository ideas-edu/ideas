{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}


module LogicRules where


import LogicFormula 


-- Rewrite rule
eliminateImplRule (f1 :->: f2) = (Not f1) :||: f2
eliminateImplRule _            = error "Error: eliminateImplRule"


eliminateEqvRule (f1 :<->: f2) = (f1 :&&: f2) :||: ((Not f1) :&&: (Not f2))
eliminateEqvRule _             = error "Error: eliminateEqvRule"  


deMorganRule (Not (f1 :&&: f2))       = (Not f1) :||: (Not f2)
deMorganRule (Not (f1 :||: f2))       = (Not f1) :&&: (Not f2)
deMorganRule ((Not f1) :||: (Not f2)) = (Not (f1 :&&: f2))
deMorganRule ((Not f1) :&&: (Not f2)) = (Not (f1 :||: f2))
deMorganRule _                        = error "Error: deMorganRule" 


doubleNegRule (Not (Not f)) = f
doubleNegRule _             = error "Error: doubleNegRule"


distributeRule (f1 :&&: (f2 :||: f3)) = (f1 :&&: f2) :||: (f1 :&&: f3)
distributeRule (f1 :||: (f2 :&&: f3)) = (f1 :||: f2) :&&: (f1 :||: f3)
distributeRule ((f2 :||: f3) :&&: f1) = (f2 :&&: f1) :||: (f3 :&&: f1)
distributeRule ((f2 :&&: f3) :||: f1) = (f2 :||: f1) :&&: (f3 :||: f1)
distributeRule _                      = error "Error: distributeRule"


idempotencyRule (f1 :&&: f2) = if f1 == f2 then f1 else (f1 :&&: f2)
idempotencyRule (f1 :||: f2) = if f1 == f2 then f1 else (f1 :||: f2)
idempotencyRule _            = error "Error: idempotencyRule"


falseTrueRule (T :||: f)         = T
falseTrueRule (f :||: T)         = T
falseTrueRule (T :&&: f)         = f
falseTrueRule (f :&&: T)         = f
falseTrueRule (F :||: f)         = f
falseTrueRule (f :||: F)         = f
falseTrueRule (F :&&: f)         = F
falseTrueRule (f :&&: F)         = F
falseTrueRule (f1 :||: Not (f2)) = if f1 == f2 then T else (f1 :||: Not (f2))
falseTrueRule (Not (f1) :||: f2) = if f1 == f2 then T else (Not (f1) :||: f2)
falseTrueRule (f1 :&&: Not (f2)) = if f1 == f2 then F else (f1 :&&: Not (f2))
falseTrueRule (Not (f1) :&&: f2) = if f1 == f2 then F else (Not (f1) :&&: f2)
falseTrueRule (Not F)            = T
falseTrueRule (Not T)            = F
falseTrueRule _                  = error "Error: falseTrueRule"



