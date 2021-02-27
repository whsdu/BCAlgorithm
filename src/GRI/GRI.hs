{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module GRI.GRI
    ( GRI (..)
    , Rs (..)
    , Rd (..)
    , gri
    , griSupportPath
    , griActivated
    , griNecessaryLanguage
    )
    where 

import Control.Monad.Reader

import Env 
import qualified Space.Meta as M 
import qualified Space.Language as L 
import qualified Utility.Language as LU


newtype Rs = Rs { unpackRs :: [(L.Language, L.Literal)] }
newtype Rd = Rd { unpackRd :: [(L.Literal, L.Literal)] }
data GRI = GRI 
    { griGetN ::L.Language
    , griGetRs :: Rs
    , griGetRd :: Rd
    }

instance Show GRI where 
    show gri = 
                "N in GRI: " ++  show (griGetN gri) ++ "\n" ++ 
                "Rs in GRI: " ++  show (unpackRs . griGetRs $ gri) ++ "\n" ++
                "Rd in GRI: " ++  show (unpackRd . griGetRd $ gri) 
gri :: 
    ( MonadReader env m 
    , UseRuleOnly env 
    , MonadIO m
    ) => m GRI 
gri = do 
    sRule <- L.getStrictRules <$> grab @L.StrictRules
    dRule <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    let 
        rules = sRule ++ dRule 
        rs = Rs $ getRs rules <$> rules 
        ds = Rd $ getRd rules 
    pure $ GRI rules rs ds 
    where 
        getRs :: L.Language -> L.Literal -> (L.Language, L.Literal)
        getRs lang n = 
            let 
                supportN = [l | l <- lang, L.conC l `elem` L.body n]
            in (supportN, n)
        getRd :: L.Language -> [(L.Literal, L.Literal)]
        getRd lang = do 
            a <- lang 
            b <- lang 
            guard $ a `LU.isAttack` b 
            pure (a,b)

-- | It seems only Atom has no support path.
-- This is based on LU.langAL and basically the same.
-- TODOs: 
-- 1. Test if multiple rule draw the same conclusion, like in the Tree structure     
-- 2. In the `if` expression below: it would be quite impossible that `n notElem of snd rs`
griSupportPath ::
    ( LU.LanguageContext m 
    , MonadIO m 
    ) => GRI -> L.Literal -> m [L.Language]
griSupportPath gri n = 
    let 
        c = L.conC n 
        rs = unpackRs . griGetRs $ gri 
    in if n `notElem` (snd <$> rs) 
            then pure []
        else 
            do  
                supportlang <- LU.langAL c 
                pure . M.rmdups $ [ fst r  | r <- rs, snd r `elem` supportlang] ++ [[n]]

-- | This is similar with langASG \\
-- TODOs: need more further test. 
griNecessaryLanguage 
    :: 
    ( LU.LanguageContext m 
    , MonadIO m
    ) => GRI -> L.Literal -> m L.Language
griNecessaryLanguage gri l = 
    let rd = unpackRd . griGetRd $ gri 
    in accNec rd [l]
    where 
        accNec attackPair initL = do 
            supportLang <- M.rmdups . concat <$> forM (L.conC <$> initL) LU.langAL 
            let 
                defender = [ snd r | r <- attackPair, fst r `elem` supportLang]
                attacker = [ fst r | r <- attackPair, snd r `elem` supportLang]
                all = M.rmdups $ initL ++ supportLang ++ defender ++ attacker 
            if all == initL 
                then pure all 
                else accNec attackPair all 

-- | Properties of GRI
--  Redundent in constructing BC
griActivated :: 
    ( LU.LanguageContext m 
    , MonadIO m
    ) => GRI -> L.Literal -> m Bool 
griActivated gri n = do 
    supportPath <- griSupportPath gri n 
    pure $ [n] `elem` supportPath 

-- | property of GRI 
-- Redundent in constructing BC
-- griConnected :: 

