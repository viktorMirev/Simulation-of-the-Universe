
import System.Random
import Data.List
import Data.Time
import Data.Time.Clock.POSIX



-- all particles
data Particle = H 
    | BottomQ
    | BottomAQ 
    | W 
    | Gluon
    | TLepton 
    | ATLepton 
    | CharmQ 
    | CharmAQ 
    | Z 
    | Photon 
    | Muon 
    | AMuon 
    | TopQ 
    | TopAQ 
    | DownQ 
    | DownAQ 
    | WeirdQ 
    | WeirdAQ
    | Pozitron
    | Electron
    | Neutrino
    | ANeutrino
    | UpQ
    | UpAQ
    deriving (Show)

-- decay possibilities for the HIGGS Boson
decayH :: Int -> [Particle]
decayH n
     | n <= 648000  = [BottomQ, BottomAQ]
     | n <= 789000  = [W, W]
     | n <= 877200  = [Gluon, Gluon]
     | n <= 947600  = [TLepton, ATLepton]
     | n <= 980300  = [CharmQ, CharmAQ]
     | n <= 996200  = [Z, Z]
     | n <= 998430  = [Photon, Photon]
     | n <= 999540  = [Photon, Z]
     | n <= 999784  = [Muon, AMuon]
     | n <= 1000000 = [TopQ, TopAQ]

-- decay possibilities for the W bosson 
decayW :: Int -> [Particle]
decayW n
     | n <= 333333  = [Pozitron, Neutrino]
     | n <= 666666  = [AMuon, Neutrino]
     | n <= 1000000 = [ATLepton, Neutrino]

-- decay possibilities for the Z bosson
decayZ :: Int -> [Particle]
decayZ n
     | n <= 206000  = [Neutrino, ANeutrino]
     | n <= 240000  = [Electron, Pozitron]
     | n <= 274000  = [Muon, AMuon]
     | n <= 308000  = [TLepton, TLepton]
     | n <= 460000  = [DownQ, DownAQ]
     | n <= 612000  = [WeirdQ, WeirdAQ]
     | n <= 764000  = [BottomQ, BottomAQ]
     | n <= 882000  = [UpQ, UpAQ]
     | n <= 1000000 = [CharmQ, CharmAQ]

--decay possibilities for the TopQ
decayTopQ :: Int -> [Particle]
decayTopQ n
        | n <= 333333  = [W, DownQ]
        | n <= 666666  = [W, WeirdQ]
        | n <= 1000000 = [W, BottomQ]

--decay possibilities for the TopAQ
decayTopAQ :: Int -> [Particle]
decayTopAQ n
         | n <= 333333  = [W, DownAQ]
         | n <= 666666  = [W, WeirdAQ]
         | n <= 1000000 = [W, BottomAQ]

-- determine is particle an Active one (prone to decay)
isActive :: Particle -> Bool
isActive H     = True
isActive W     = True
isActive Z     = True
isActive TopQ  = True
isActive TopAQ = True
isActive _     = False

-- strip all but active particles 
active :: [Particle] -> [Particle]
active ps = filter isActive ps

-- strip all but stable particles ( considered stable in the task)
stable :: [Particle] -> [Particle]
stable ps = filter (\ p -> not $ isActive p) ps

-- raturn random Int and the next generator
randomIntAndNext :: StdGen -> (Int, StdGen)
randomIntAndNext g0 = (a, g1)
  where
    (a,g1) = randomR (1,1000000) g0

-- decay single particle using the given random generator
decayIt :: StdGen -> Particle -> [Particle]
decayIt g H = do
     if rand <= 433
          then decayH randDecay
          else [H]
     where 
          (rand, g1)     = randomIntAndNext g
          (randDecay, _) = randomIntAndNext g1
decayIt g W = do
     if rand <= 500000
          then decayW randDecay
          else [W]
     where 
          (rand, g1)     = randomIntAndNext g
          (randDecay, _) = randomIntAndNext g1
decayIt g Z = do
     if rand <= 500000
          then decayZ randDecay
          else [Z]
     where 
          (rand, g1)     = randomIntAndNext g
          (randDecay, _) = randomIntAndNext g1
decayIt g TopQ = do
     if rand <= 129500
          then decayTopQ randDecay
          else [TopQ]
     where 
          (rand, g1)     = randomIntAndNext g
          (randDecay, _) = randomIntAndNext g1
decayIt g TopAQ = do
     if rand <= 129500
          then decayTopAQ randDecay
          else [TopAQ]
     where 
          (rand, g1)     = randomIntAndNext g
          (randDecay, _) = randomIntAndNext g1


-- return the next Nth Random Generator to provide more randomness
nextNthGen :: Int -> StdGen -> StdGen
nextNthGen 0 g = next
          where
               (_, next) = randomIntAndNext g
nextNthGen n g = nextNthGen (n-1) next
          where
               (_, next) = randomIntAndNext g


-- try to decay the particles (will be used only if the list contains active particles)
-- we use nextNthGen with the length of the nv because at every iteration is differen ALWAYS!
decayThem ::StdGen -> [Particle] -> [Particle]
decayThem g (p:ps) = foldl (\ nv v -> ((decayIt (nextNthGen (length nv) g) v) ++ nv )) (decayIt g p) ps

-- performs decayThem until the number of the particles changes
-- ( which in our case means that decay has happened)
-- in physics there is no such thing one particle to decay 
-- into just 1 thing so change in the number will always be present
decayUntilChange :: StdGen -> Int -> [Particle] -> [Particle]
decayUntilChange g prevN ps = if currLen /= prevN
                           then ps
                           else (stables ++ (decayUntilChange next currALen (decayThem g actives)))
                    where
                         actives   = active ps
                         stables   = stable ps
                         currLen   = length ps
                         currALen  = length actives
                         (_, next) = randomIntAndNext g

-- the base of the simulation which prints every different condition
base :: Int -> [Particle] -> IO()
base n ps = if (length actives) == 0 
               then do
                    print "---------------"
                    print "The final state"
                    print ps
               else do
                    g <- newStdGen
                    print ((show n) ++ ".")
                    print ps
                    base (n+1) (decayUntilChange g currLen ps)
          where actives = active ps
                currLen = length ps
      
