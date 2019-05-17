{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import Debug.Trace

import Data.Ord

import qualified Data.List as L
import qualified Data.Set as S


type Depth = Int
data Node s a = Root { depth :: Depth, state :: s, offsprings :: [Node s a] } | 
                NodeCell { depth :: Depth, info :: (a, s), parent :: Node s a, offsprings :: [Node s a] } deriving (Eq)

{-
    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState (Root _ st _) = st
nodeState (NodeCell _ (_, st) _ _) = st

{-
    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}
generateOffsprings :: (ProblemState s a) => [(a, s)] -> Node s a -> [Node s a]
generateOffsprings [] _ = []
generateOffsprings (x:xs) parent_node = new_node : (generateOffsprings xs parent_node)
    where new_node = NodeCell { depth = (depth parent_node) + 1, info = x, parent = parent_node,
                                offsprings = generateOffsprings (successors (nodeState new_node)) new_node }

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace st = root_node
    where root_node = Root {depth = 0, state = st, offsprings = generateOffsprings (successors st) root_node}

{-
    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace node = node { offsprings = new_offs } 
  where new_offs = map orderStateSpace sorted_succs
        sorted_succs = L.sortBy (comparing (heuristic . nodeState)) (offsprings node)

{-
    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.
-}
dfsUtil :: (ProblemState s a, Ord s, Eq s)
            => Node s a                -- Nodul curent
            -> ([Node s a], S.Set s)   -- (rezultat, visited)
            -> Depth                   -- Adancimea maxima de explorare
            -> ([Node s a], S.Set s)   -- Ce returnam
dfsUtil node (list, visited) max_depth
    | (depth node) > max_depth = (list, visited)
    | S.member (nodeState node) visited == True = (list, visited)
    | otherwise = foldl func (list ++ [node], S.insert (nodeState node) visited) (offsprings node)
    where func = (\acc next_node -> dfsUtil next_node acc max_depth)

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs node max_depth = fst (dfsUtil node ([], S.empty) max_depth)
    
{-
    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

dfsIterativeCaller :: (ProblemState s a, Ord s, Eq s)
    => Node s a         -- Nodul sursa pentru dfs
    -> Int              -- Adancimea maxima
    -> Int              -- Nr. de noduri parcurse pana acum
    -> (Node s a, Int)  -- Nr. total de noduri parcurse
dfsIterativeCaller node max_depth counter
    | null (filter (\x -> isGoal (nodeState x)) node_list) == True = dfsIterativeCaller node (max_depth + 1) (counter + (length node_list))
    | otherwise = head [(nod, idx) | (idx, nod) <- zip [counter..] node_list, isGoal (nodeState nod)]
    where node_list = limitedDfs node max_depth

iterativeDeepening :: (ProblemState s a, Ord s, Eq s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening node = dfsIterativeCaller node 0 0

{-
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath (Root _ _ _) = []
extractPath node = (extractPath (parent node)) ++ [info node]

{-
    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve stt False = extractPath $ fst $ iterativeDeepening $ createStateSpace stt
solve stt True = extractPath $ fst $ iterativeDeepening $ orderStateSpace $ createStateSpace stt

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))

{-
    Debug

    La finalul unei linii -> `debug` "mesaj"
-}
debug :: c -> String -> c
debug = flip trace