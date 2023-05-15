import Prelude (undefined)

data False                                        -- empty type

data True = True                                  -- unit type

data And a b = And { proj1 :: a, proj2 :: b }     -- product

data Or a b                                       -- sum
  = Left a
  | Right b

type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

-- Natural deduction introduction and elimination rules

trueIntro :: True                                   -- true introduction
trueIntro = True

falseElim :: False -> b                             -- false elimination
falseElim x = case x of

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro ab a = ab a

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim ab a = ab a

andIntro :: a -> b -> And a b                       -- and introduction
andIntro a b = And a b

andElimL :: And a b -> a                            -- and elimination 1
andElimL = proj1

andElimR :: And a b -> b                            -- and elimination 2
andElimR = proj2

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL = Left

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR = Right

orElim :: Or a b -> (a -> c) -> (b -> c) -> c       -- or elimination
orElim or ac bc = case or of
  Left a -> ac a
  Right b -> bc b

notElim :: Not p -> p -> c                          -- not elimination 
notElim notA a = falseElim ( notA a)

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f = f

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro fab fba = And fab fba

iffElimL :: Iff a b -> a -> b                       -- iff elimination 1
iffElimL (And fab _fba) a = fab a

iffElimR :: Iff a b -> b -> a                       -- iff elimination 1
iffElimR = (And _fab fba) b = fba b

-- Hilbert-style axiomatization for intuitionistic propositional logic

ax1 :: a -> b -> a
ax1 a b = implIntro (\a -> implIntro (\b -> a))

ax2 :: (a -> b) -> (a -> (b -> c)) -> a -> c
ax2 f g a = implIntro (\f -> implIntro (\g -> implIntro (\a -> implElim ( implElim g a) (implElim f a)))) f g a

ax3 :: a -> b -> And a b
ax3 = implIntro (\a -> implIntro (\b -> andIntro a b))

ax4 :: And a b -> a
ax4 = andElimL

ax5 :: And a b -> b
ax5 = andElimR

ax6 :: a -> Or a b
ax6 = orIntroL

ax7 :: b -> Or a b
ax7 = orIntroR

ax8 :: (a -> c) -> (b -> c) -> Or a b -> c
ax8 ac bc ab = orElim ab ac bc

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 ab anb = notIntro (\a -> notElim (anb a) (ab a)) 

ax10 :: Not a -> a -> b
ax10 na a = notElim na a 

modusPonens :: (a -> b) -> a -> b
modusPonens f = f

-- Several tautologies

pNPFalse :: p -> Not p -> False
pNPFalse = undefined

deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 = undefined

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 = undefined

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 = undefined

type DeMorgan4 = forall p q . Not (And p q) -> Or (Not p) (Not q)

-- Classical axioms

type ExcludedMiddle = forall a. Or a (Not a)
type DoubleNegation = forall a. Not (Not a) -> a
type PeirceLaw = forall p q. ((p -> q) -> p) -> p

excludedMiddleImplDoubleNeg :: ExcludedMiddle -> DoubleNegation
excludedMiddleImplDoubleNeg em = undefined

doubleNegImplExcludedMiddle :: DoubleNegation -> ExcludedMiddle -- hard
doubleNegImplExcludedMiddle dn = undefined

classicDeMorgan4 :: ExcludedMiddle -> DeMorgan4
classicDeMorgan4 em = undefined

