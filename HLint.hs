import "hint" HLint.Default
import "hint" HLint.HLint

infixl 4 <$>
infixr 9 .
infixl 1 <&>
infixl 1 &
infixl 3 <|>
infixl 4 *>
infixl 4 <*
infixl 4 <*>
infixr 0 $
infixr 6 <>
infixr 5 ++
-- warn "my-a" = a (b $ c d)  ==> a . b $ c d
-- warn "my-b" = a (b *> c) ==> a $ b *> c
-- warn "my-c" = a (b (c d)) ==> a (b $ c d)
-- warn "my-d" = [a (b c)] ==> [a $ b c]
warn "Use liftA2" = a <$> b <*> c ==> liftA2 a b c
warn "my-e" = (a $ b c (d e), f) ==> (a . b c $ d e, f)
warn "my-f" = [a b (c d), e] ==> [a b $ c d, e]
warn "my-g" = (if a then (b $ c) else (b $ d)) ==> (b $ if a then c else d)
warn "my-h" = (do x <- a ; return $ b x) ==> b <$> a
warn "my-ha" = (do x <- a ; b x) ==> b <*> a
warn "my-i" = (\x -> a <$> b x) ==> fmap a . b
warn "my-j" = either (f . a) (f . b) ==> f . either a b
warn "my-ja" = either (f . a) (f . b) c ==> f . either a b c
warn "my-k" = (\x -> f x >>= y) ==> f >=> y
