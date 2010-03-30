{-# OPTIONS --universe-polymorphism #-}
module Reflection where

open import Common.Prelude
open import Common.Reflect

data _≡_ {A : Set}(x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

data Id {A : Set}(x : A) : (B : Set) → B → Set where
  course : Id x A x

primitive primTrustMe : {A : Set}{x y : A} → x ≡ y

postulate badName : QName

name_ : Term → QName
name (def x _) = x
name (con x _) = x
name _ = badName

open import Common.Level

unCheck : Term → Term
unCheck (def x (_ ∷ _ ∷ arg _ t ∷ [])) = t
unCheck t = unknown

mutual
  data Check {a}{A : Set a}(x : A) : Set where
    _is_of_ : (t t′ : Term) →
              Id (primTrustMe {x = unCheck t} {t′}
                 )
                 (t′ ≡ t′) refl → Check x

  `Check : QName
  `Check = name quote (Check {zero}{QName})

test₁ : Check ({A : Set} → A → A)
test₁ = quoteGoal t in
        t is pi (arg true sort) (pi (arg false (var 0 [])) (var 1 []))
        of course

test₂ : (X : Set) → Check (λ (x : X) → x)
test₂ X = quoteGoal t in
          t is lam false (var 0 []) of course

infixr 40 _`∷_

_`∷_ : Term → Term → Term
x `∷ xs = con (name quote (_∷_ {Bool})) (arg false x ∷ arg false xs ∷ [])
`[]    = quote ([] {Bool})
`true  = quote true
`false = quote false

test₃ : Check (true ∷ false ∷ [])
test₃ = quoteGoal t in
        t is `true `∷ `false `∷ `[] of course

`List : Term → Term
`List A = def (name quote List) (arg false A ∷ [])
`Nat    = quote Nat

test₄ : Check (List Nat)
test₄ = quoteGoal t in
        t is `List `Nat of course
