{-# LANGUAGE DeriveFunctor #-}
module Hproc.Hproc (hproc) where
import Prelude  ((.),String,(++),fmap,Maybe(Just,Nothing),
                 Either(Right,Left),Double,Int,Show,Functor,print,(==),($),words,zip,fromRational,
                 sequence,
                 (+),(*),(/),
                 fromIntegral,(>=))
import Data.Amp.Hylo (Fix(Fix), unFix, cata, ana)
import Text.Read (readMaybe)
import Text.Printf (printf)
data StreamF  e a = StreamF e a deriving (Functor,Show)
data ChannelF e a = NilF | ChannelF e a deriving (Functor,Show)
type Carrier      = Either Receiver Sender
type ConnectorF   = ChannelF Transfer
type InterfaceF   = ConnectorF Carrier
output  ::            Fix (ConnectorF) -> Carrier
input   :: Carrier -> Fix (ConnectorF)
output  = cata send
input   = ana receive
amp     = (output . input)
type Token    = Int
type Transfer = Maybe Int 
type Receiver = [String]
type Sender   = ([Int],Int,Maybe [Double],[String])
send::               InterfaceF -> Carrier
receive:: Carrier -> InterfaceF
send NilF                                   = Right ([],0,Just [],[])
send (ChannelF Nothing  (Right x))          = Right x
send (ChannelF (Just x) (Right (s,t,_,_)))  = let ns = x:s
                                                  nt = x+t
                                                  ps = sequence $ fmap (\c -> cputPercent c nt) ns
                                                  fs = formatPercent ps
                                              in Right (ns,nt,ps,fs)
receive (Left ("cpu":ws))  = ChannelF Nothing       (Left ws)
receive (Left (w:ws))      = ChannelF (readMaybe w) (Left ws)
receive (Left [])          = NilF
cputPercent x 0            = Nothing
cputPercent x t            = (Just (fromRational $ 100*(fromIntegral(x) / fromIntegral(t))))::Maybe Double
formatPercent Nothing      = ["Impossible to compute values"]
formatPercent (Just x)     = fmap (\v -> if (v>=10) then printf "%2.3f" v else printf " %2.3f" v ) x
unRight (Right x) = x
names = ["user", "nice", "system", "idle", "iowait", "irq", "softirq", "steal", "guest", "guest_nice"]
addNames (s,t,p,f) = (s,t,p,zip names f) 
hxproc = addNames . unRight . amp . Left . words 
fxproc (s,t,p,f) = f
hproc = fxproc . hxproc
tests = do test1
           test2
           test3
           test4
test1 = print $ hxproc "cpu  300000 5000 100000 1000000 10000 0 20000 0 0 0"
test2 = print $ hxproc "cpu  000000 0000 000000 0000000 00000 0 00000 0 0 0"
test3 = print $ 
              (hxproc "cpu  000000 0000 000000 0000000 00000 0 00000 0 0 0") 
               ==
              ([0,0,0,0,0,0,0,0,0,0],0,Nothing,[("user","Impossible to compute values")])
test4 = print $ 
               (hxproc "cpu  300000 5000 100000 1000000 10000 0 20000 0 0 0") 
               ==
               ([300000,5000,100000,1000000,10000,0,20000,0,0,0],
                1435000,
                Just [20.905923344947734,0.34843205574912894,6.968641114982578,69.68641114982579,0.6968641114982579,0.0,1.3937282229965158,0.0,0.0,0.0],
                [("user","20.91"),("nice","0.35"),("system","6.97"),("idle","69.69"),("iowait","0.70"),("cputirq","0.00"),("cputsoftirq","1.39"),("cputsteal","0.00"),("cputguest","0.00"),("cputguest_nice","0.00")])
