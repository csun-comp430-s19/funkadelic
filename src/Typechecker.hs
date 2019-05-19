{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Prelude hiding (map)
import Data.List
import Parser
import Data.Map hiding (map, findIndex, splitAt)
import Data.Functor.Identity

-- Type environment
data Gamma = Gamma (Env, TldMap, TcDef, TcImp) deriving (Show)
data Env = Env [(Identifier, Type)] deriving (Show)
data TldMap = TldMap [(Type, [CDef])] deriving (Show)
data TcDef = TcDef [(Identifier, [SignatureDef])] deriving (Show)
data TcImp = TcImp [(Identifier, [SignatureImp])] deriving (Show)

typecheckProgram :: ([Tld], Exp) -> State Gamma (Maybe Type)
typecheckProgram (tlds, exp) = do
    _ <- mapM typecheck tlds
    _ <- typecheck exp
    gamma <- get
    return (Just (Type (Identifier "good")))
    
    


addEntryToEnv :: Identifier -> Type -> Gamma -> Gamma
addEntryToEnv n t (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = Gamma (Env (l ++ [(n,t)]), TldMap m, TcDef td, TcImp ti)

addUdtToGamma :: Type -> [CDef] -> Gamma -> Gamma
addUdtToGamma t cDefs (Gamma (env , (TldMap tldMap), tcD, tcP)) = Gamma(env, TldMap (tldMap ++ [(t, cDefs)]), tcD, tcP)


addTcDefToGamma :: Identifier -> [SignatureDef] -> Gamma -> Gamma
addTcDefToGamma n s (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = Gamma (Env l, TldMap m, TcDef (td ++ [(n, s)]), TcImp ti)

addTcImpToGamma :: Identifier -> [SignatureImp] -> Gamma -> Gamma
addTcImpToGamma n s (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = Gamma (Env l, TldMap m, TcDef td, TcImp (ti ++ [(n, s)]))

insertTcDefsToGamma :: Identifier -> [SignatureDef] -> Gamma -> Maybe Gamma
insertTcDefsToGamma n s (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = do
    case td == [] of
        True -> return (Gamma (Env l, TldMap m, TcDef [(n, s)], TcImp ti))
        False -> do
            index <- findIndex (==n) [name | (name, sigs) <- td]
            let newTcDef = (n, ([sigs | (name, sigs) <- td] !! index ++ s))
            let (x,_:ys) = splitAt index td
            let newTd = x ++ newTcDef : ys
            return (Gamma (Env l, TldMap m, TcDef newTd, TcImp ti))

insertTcImpsToGamma :: Identifier -> [SignatureImp] -> Gamma -> Maybe Gamma
insertTcImpsToGamma n s (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = do
    case ti == [] of
        True -> return (Gamma (Env l, TldMap m, TcDef td, TcImp [(n, s)]))
        False -> do 
            index <- findIndex (==n) [name | (name, imps) <- ti]
            let newTcImps = (n, ([imps | (name, imps) <- ti] !! index ++ s))
            let (x,_:ys) = splitAt index ti
            let newTi = x ++ newTcImps : ys
            return (Gamma (Env l, TldMap m, TcDef td, TcImp newTi))

getType :: Identifier -> Gamma -> Maybe Type

getIdentifiers :: Type -> Gamma -> [CDef]
getIdentifiers t (Gamma (_, TldMap m, _, _)) = do
    case Data.Map.lookup t gMap  of
        Nothing -> []
        Just cs -> cs
    where
        gMap = fromList m

getTypesFromPme :: Pme -> CDef -> State Gamma (Maybe Type)
getTypesFromPme (PatternMatchExpression a _ _) (UnaryConstructor i _)  = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case getType a gamma of
                Just (Type (Identifier t2)) -> do
                    case t2 == t1 of
                        True -> return (Just (Type (Identifier t2)))
                        False -> return Nothing
                Nothing -> return Nothing
        Nothing -> return Nothing
getTypesFromPme (PatternMatchExpression a _ _) (NullaryConstructor i) = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case getType a gamma of
                Just (Type (Identifier t2)) -> do
                    case t2 == t1 of
                        True -> return (Just (Type (Identifier t2)))
                        False -> return Nothing
                Nothing -> return Nothing
        Nothing -> return Nothing
getType x (Gamma (Env l, _, _, _)) = Data.Map.lookup x gMap
    where
        gMap = fromList l

getImpType :: Identifier -> Identifier -> Type -> Gamma -> Maybe (Maybe Type)
getImpType tcn tcf i (Gamma (_, _, _, TcImp ti)) = do
    let gMap = fromList ti
    let tcImps = Data.Map.lookup tcn gMap
    case tcImps of
        Nothing -> return Nothing
        Just imps ->
            case Data.Map.lookup (tcf, i) gMap2 of
                Nothing -> return Nothing
                Just foundOut -> return (Just foundOut)
            where
                gMap2 = fromList (map getSigImpNameInOut imps)

getTcDefIdentifiers :: Identifier -> Gamma -> Maybe [SignatureDef]
getTcDefIdentifiers tc (Gamma (_, _, TcDef td, _)) = Data.Map.lookup tc gMap
    where
        gMap = fromList td 

getTcImpIdentifiers :: Identifier -> Gamma -> Maybe [SignatureImp]
getTcImpIdentifiers tc (Gamma (_, _, _, TcImp ti)) = do
    sigImps <- Data.Map.lookup tc gMap
    return sigImps
    where
        gMap = fromList ti

tcDefSigExists :: (Identifier, SignatureDef, Gamma) -> Maybe Bool
tcDefSigExists (tcName, sigDef, gamma) =
    case tcDefs of
        Nothing -> return False
        Just x -> 
            case find (==sigDef) x of
                Nothing -> return False
                Just _ -> return True
    where 
        tcDefs = getTcDefIdentifiers tcName gamma

getSigDefName :: SignatureDef -> (Identifier, Bool)
getSigDefName (SigDef name g1 g2) = (name, True)

getSigImpNameIn :: SignatureImp -> (Identifier, Type)
getSigImpNameIn (SigImp sigName inType outType inputName body) = (sigName, inType)

getSigImpNameInOut :: SignatureImp -> ((Identifier, Type), Type)
getSigImpNameInOut (SigImp sigName inType outType inputName body) = ((sigName, inType), (outType))
    
tcImpGood :: (Identifier, SignatureImp, Gamma) -> Maybe Bool
tcImpGood (tcName, (SigImp sigName inType outType inputName body), gamma) =  do
    case tcDefs of 
        Nothing -> return False -- We return a reject if typeclass has no signatures
        Just defs ->
            case defs == [] of
                True -> return False
                False ->
                    case Data.Map.lookup sigName sigNames of
                        Nothing -> return False -- Does NOT have a sig def
                        Just x ->
                            case tcImps of
                                Nothing -> return True -- Does NOT have any sig imps
                                Just imps ->
                                    case find (==(sigName, inType)) impNameTypes of
                                        Nothing -> return True
                                        Just foundImp -> return False -- found the implementation so send false
                                    where 
                                        impNameTypes = map getSigImpNameIn imps
                    where sigNames = fromList (map getSigDefName defs)
    where 
        tcImps = getTcImpIdentifiers tcName gamma
        tcDefs = getTcDefIdentifiers tcName gamma

pmeTypeCheck :: Pme -> (Type, Type) -> State Gamma (Maybe Type)
pmeTypeCheck (PatternMatchExpression i _ e) (pt, rt) = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case (Type (Identifier t1)) == pt of
                True -> do
                    et <- typecheck e
                    case et == Just rt of
                        True -> return (Just rt)
                        False -> return Nothing
                False -> return Nothing
        Nothing -> return Nothing 

reduceList :: [(Maybe Type)] -> (Maybe Type)
reduceList [] = Nothing
reduceList ((Just h):t) = reduceListWithType t h
reduceList ((Nothing):t) = Nothing 

reduceListWithType :: [(Maybe Type)] -> Type -> (Maybe Type)
reduceListWithType [] t1 = (Just t1)
reduceListWithType (head:tail) t1 = do 
    case head == Nothing of
        True -> Nothing
        False -> do
            case head == Just t1 of
                True -> reduceListWithType tail t1
                False -> Nothing

class Typecheck a where
    typecheck :: a -> State Gamma (Maybe Type)


instance Typecheck Tld where
    typecheck (Func (FuncDefUnary fName var inType body outType)) = do
        gamma <- get
        _ <- put $ addEntryToEnv var inType gamma
        actualOutType <- typecheck body
        case actualOutType == Just outType of
            True -> do
                _ <- put $ addEntryToEnv fName functionType gamma
                return (Just functionType)
                where
                    functionType = FunctionType inType outType
            False -> return Nothing
    typecheck (Func (FuncDefNullary fName body t)) = do
        actualType <- typecheck body
        case actualType == Just t of
            True -> do
                gamma <- get
                _ <- put $ addEntryToEnv fName t gamma
                return $ Just t
            False -> return Nothing
    typecheck (TypeclassDef defName defSigs) = do
        gamma <- get
        case getTcDefIdentifiers defName gamma == Nothing of
            True -> do
                gamma <- get
                _ <- put $ addTcDefToGamma defName defSigs gamma
                return $ Just (Type (Identifier "typeclass"))
            False -> do
                case anyExist of
                    Nothing -> do
                        case newGamma of
                            Nothing -> return Nothing
                            Just x -> do
                                put $ x
                                return $ Just (Type (Identifier "typeclass"))
                        where 
                            newGamma = insertTcDefsToGamma defName defSigs gamma
                    Just x -> return Nothing
                where 
                    sigExistList = map tcDefSigExists [(defName, sig, gamma) | sig <- defSigs]
                    gMap = fromList [(sigExist, True) | sigExist <- sigExistList]
                    anyExist = Data.Map.lookup (Just True) gMap
    typecheck (TypeclassImp defName defImps) = do
        gamma <- get
        case True == True of
            True ->
                case anyBadImps of
                    Nothing ->
                        case anyInconsistentImps of
                            Nothing ->
                                case newGamma of 
                                    Nothing -> return Nothing
                                    Just x -> do
                                        put $ x
                                        return $ Just (Type (Identifier "typeclassImp"))
                                where 
                                    newGamma = insertTcImpsToGamma defName defImps gamma  
                            Just x -> return Nothing
                        where        
                            isAgreeable' (SigImp sigName inType outType inputName body) = do
                                case fst (runState (typecheck body) gamma) of
                                    Nothing -> return False
                                    Just x -> return (outType == x)
                            agreeableList = map isAgreeable' defImps       
                            gMap2 = fromList [(isAgree, True) | isAgree <- agreeableList]
                            anyInconsistentImps = Data.Map.lookup (Just False) gMap2 
                    Just x -> return Nothing
                where 
                    impsGood = map tcImpGood [(defName, imp, gamma) | imp <- defImps]
                    gMap = fromList [(impExist, True) | impExist <- impsGood]
                    anyBadImps = Data.Map.lookup (Just False) gMap
            False -> return Nothing
    typecheck (DataDef (Identifier t) cDefs) = do
        gamma <- get
        _ <- put $ addUdtToGamma (mkType t) cDefs gamma
        return $ Just (mkType t)


instance Typecheck IExp where
    typecheck (IExpVar id) = do
        gamma <- get
        return $ getType id gamma
            
    typecheck iExp = return (Just $ mkType $ "Int")

instance Typecheck Exp where
    typecheck (ExpInteger ei) = return $ Just $ mkType "Int"
    typecheck (ExpString es) = return $ Just $ mkType "String"
    typecheck (ExpIExp eie) = typecheck eie
    typecheck (ExpVariable x) = do
        gamma <- get
        return $ getType x gamma
    typecheck (ExpLambda e1 t1 e2 t2) = do
        e1t <- typecheck e1
        e2t <- typecheck e2
        case e1t == (Just t1) && e2t == (Just t2) of
            True -> return (Just t2)
            False -> return Nothing
    typecheck (ExpUnaryFOCall fName param) = do
        actualType <- typecheck param
        gamma <- get
        case getType fName gamma of
            Just (FunctionType inType outType) ->
                case typesMatch of
                    True -> return (Just outType)
                    False -> return Nothing
                where
                    typesMatch = (Just inType) == actualType
            otherwise -> return Nothing
    typecheck (ExpNullaryFOCall fName) = do
        gamma <- get
        return $ getType fName gamma
    typecheck (ExpPatternMatchCall e1 paramType returnType pmes) = do 
        e1t <- typecheck e1
        gamma <- get 
        case e1t == (Just paramType) of
            True -> do
                case getIdentifiers paramType gamma of
                    constructs -> do
                        listPmeTypes <- sequence $ zipWith getTypesFromPme pmes constructs
                        case reduceList listPmeTypes of
                            Just a -> do
                                pmeTypeResults <- sequence $ zipWith pmeTypeCheck pmes (Prelude.take (length pmes) (repeat (paramType, returnType)))
                                case reduceList pmeTypeResults of
                                    Just b -> return (Just returnType)
                                    Nothing -> return Nothing
                            Nothing -> return Nothing                         
            False -> return Nothing
        
    typecheck (TypeclassCallInt (ExpAtomInt _) (Typeclass tc) (TypeclassFunc tcfun)) = do
        gamma <- get
        return $ join $ getImpType tc tcfun (mkType "Int") gamma
    typecheck (TypeclassCallStr (ExpAtomStr _) (Typeclass tc) (TypeclassFunc tcfun)) = do
        gamma <- get
        return $ join $ getImpType tc tcfun (mkType "Str") gamma
    typecheck (TypeclassCallVar (ExpAtomVar var) (Typeclass tc) (TypeclassFunc tcfun)) = do
        gamma <- get
        let t = getType var gamma in
            case t of
                Nothing -> return Nothing
                Just t -> return $ join $ getImpType tc tcfun t gamma
    typecheck (ExpTuple exps (ProductType types)) = head $ zipWith typecheckElement exps types

typecheckElement :: Exp -> Type -> State Gamma (Maybe Type)
typecheckElement e t = do
    mEt <- typecheck e
    case mEt of
        Just et -> case t == et of
            True -> return (Just t)
            False -> return Nothing
        Nothing -> return Nothing