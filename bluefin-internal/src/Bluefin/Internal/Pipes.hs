module Bluefin.Internal.Pipes where

import Bluefin.Internal hiding (yield, await)
import qualified Bluefin.Internal
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Void (Void, absurd)
import Prelude hiding (break, print, takeWhile)
import qualified Prelude

data Proxy a' a b' b e = MkProxy (Coroutine a' a e) (Coroutine b b' e)

type Pipe a = Proxy () a ()

type Producer = Proxy Void () ()

type Consumer a = Pipe a Void

type Effect = Producer Void

infixl 7 >->

(>->) ::
  (e1 :> es) =>
  (forall e. Proxy a' a () b e -> Eff (e :& es) r) ->
  (forall e. Proxy () b c' c e -> Eff (e :& es) r) ->
  Proxy a' a c' c e1 ->
  -- | ͘
  Eff es r
(>->) k1 k2 (MkProxy c1 c2) =
  receiveStream
    (\c -> useImplIn k2 (MkProxy (mapHandle c) (mapHandle c2)))
    (\s -> useImplIn k1 (MkProxy (mapHandle c1) (mapHandle s)))

infixr 7 <-<

(<-<) ::
  (e1 :> es) =>
  (forall e. Proxy () b c' c e -> Eff (e :& es) r) ->
  (forall e. Proxy a' a () b e -> Eff (e :& es) r) ->
  Proxy a' a c' c e1 ->
  -- | ͘
  Eff es r
k1 <-< k2 = k2 >-> k1

for ::
  (e1 :> es) =>
  (forall e. Proxy x' x b' b e -> Eff (e :& es) a') ->
  (b -> forall e. Proxy x' x c' c e -> Eff (e :& es) b') ->
  Proxy x' x c' c e1 ->
  -- | ͘
  Eff es a'
for k1 k2 (MkProxy c1 c2) =
  forEach (\bk -> useImplIn k1 (MkProxy (mapHandle c1) (mapHandle bk))) $ \b_ ->
    useImplIn (k2 b_) (MkProxy (mapHandle c1) (mapHandle c2))

infixr 4 ~>

(~>) ::
  (e1 :> es) =>
  (a -> forall e. Proxy x' x b' b e -> Eff (e :& es) a') ->
  (b -> forall e. Proxy x' x c' c e -> Eff (e :& es) b') ->
  a ->
  Proxy x' x c' c e1 ->
  -- | ͘
  Eff es a'
(k1 ~> k2) a = for (k1 a) k2

infixl 4 <~

(<~) ::
  (e1 :> es) =>
  (b -> forall e. Proxy x' x c' c e -> Eff (e :& es) b') ->
  (a -> forall e. Proxy x' x b' b e -> Eff (e :& es) a') ->
  a ->
  Proxy x' x c' c e1 ->
  -- | ͘
  Eff es a'
k2 <~ k1 = k1 ~> k2

reverseProxy :: Proxy a' a b' b e -> Proxy b b' a a' e
reverseProxy (MkProxy c1 c2) = MkProxy c2 c1

infixl 5 >~

(>~) ::
  (e1 :> es) =>
  (forall e. Proxy a' a y' y e -> Eff (e :& es) b) ->
  (forall e. Proxy () b y' y e -> Eff (e :& es) c) ->
  Proxy a' a y' y e1 ->
  -- | ͘
  Eff es c
(>~) k1 k2 p =
  for
    ( \p1 ->
        k2 (reverseProxy p1)
    )
    (\() p1 -> k1 (reverseProxy p1))
    (reverseProxy p)

infixr 5 ~<

(~<) ::
  (e1 :> es) =>
  (forall e. Proxy () b y' y e -> Eff (e :& es) c) ->
  (forall e. Proxy a' a y' y e -> Eff (e :& es) b) ->
  Proxy a' a y' y e1 ->
  -- | ͘
  Eff es c
(~<) k1 k2 = (>~) k2 k1

cat :: Pipe a a e -> Eff (e :& es) r
cat (MkProxy c1 c2) = forever $ do
  a <- yieldCoroutine c1 ()
  yieldCoroutine c2 a

runEffect ::
  (forall e. Effect e -> Eff (e :& es) r) ->
  -- | ͘
  Eff es r
runEffect k =
  forEach
    ( \c1 ->
        forEach
          ( \c2 ->
              useImplIn
                k
                (MkProxy (mapHandle c1) (mapHandle c2))
          )
          absurd
    )
    absurd

yield ::
  (e :> es) =>
  Proxy x1 x () a e ->
  a ->
  -- | ͘
  Eff es ()
yield (MkProxy _ c) = Bluefin.Internal.yield c

await :: (e :> es) => Proxy () a y' y e -> Eff es a
await (MkProxy c _) = yieldCoroutine c ()

-- | @pipe@'s 'next' doesn't exist in Bluefin
next :: ()
next = ()

each ::
  (Foldable f) =>
  f a ->
  Proxy x' x () a e ->
  -- | ͘
  Eff (e :& es) ()
each f p = for_ f (yield p)

repeatM ::
  (e :> es) =>
  Eff es a ->
  Proxy x' x () a e ->
  -- | ͘
  Eff es r
repeatM e p = forever $ do
  a <- e
  yield p a

replicateM ::
  (e :> es) =>
  Int ->
  Eff es a ->
  Proxy x' x () a e ->
  -- | ͘
  Eff es ()
replicateM n e p = for_ [0 .. n] $ \_ -> do
  a <- e
  yield p a

print ::
  (e2 :> es, e1 :> es, Show a) =>
  IOE e1 ->
  Consumer a e2 ->
  -- | ͘
  Eff es r
print io p = forever $ do
  a <- await p
  effIO io (Prelude.print a)

unfoldr ::
  (e :> es) =>
  (s -> Eff es (Either r (a, s))) ->
  s ->
  Proxy x1 x () a e ->
  -- | ͘
  Eff es r
unfoldr next_ sInit p =
  withEarlyReturn $ \break -> evalState sInit $ \ss -> forever $ do
    s <- get ss
    useImpl (next_ s) >>= \case
      Left r -> returnEarly break r
      Right (a, s') -> do
        put ss s'
        yield p a

mapM_ ::
  (e :> es) =>
  (a -> Eff es ()) ->
  Proxy () a b b' e ->
  -- | ͘
  Eff es r
mapM_ f = for cat (\a _ -> useImpl (f a))

drain ::
  (e :> es) =>
  Proxy () b c' c e ->
  -- | ͘
  Eff es r
drain = for cat (\_ _ -> pure ())

map ::
  (e :> es) =>
  (a -> b) ->
  Pipe a b e ->
  -- | ͘
  Eff es r
map f = for cat (\a p1 -> yield p1 (f a))

mapM ::
  (e :> es) =>
  (a -> Eff es b) ->
  Pipe a b e ->
  -- | ͘
  Eff es r
mapM f = for cat $ \a p -> do
  b_ <- useImpl (f a)
  yield p b_

takeWhile' ::
  (e :> es) =>
  (r -> Bool) ->
  Pipe r r e ->
  -- | ͘
  Eff es r
takeWhile' predicate p = withEarlyReturn $ \early -> forever $ do
  a <- await p
  if predicate a
    then yield p a
    else returnEarly early a

stdinLn ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Producer String e2 ->
  -- | ͘
  Eff es r
stdinLn io c = forever $ do
  line <- effIO io getLine
  yield c line

stdoutLn ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Consumer String e2 ->
  -- | ͘
  Eff es r
stdoutLn io c = forever $ do
  line <- await c
  effIO io (putStrLn line)
