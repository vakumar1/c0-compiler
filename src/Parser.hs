{-# OPTIONS_GHC -w #-}
module Parser (
    parser,
) where

import Tokens
import Ast
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,78) ([8256,896,0,4,0,16384,32800,3,0,31,0,0,0,0,516,56,0,0,0,0,0,0,0,0,0,7936,0,16384,32800,3,1024,14338,0,8256,896,0,516,56,16384,32800,3,8192,31,0,0,0,0,256,0,0,16,0,0,1,0,61440,1,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Intconst","';'","'('","'['","'{'","')'","']'","'}'","'+'","'-'","'*'","'/'","'%'","'+='","'-='","'*='","'/='","'%='","'='","ident","dec","hex","while","for","continue","break","return","assert","true","false","null","alloc","alloc_arr","int","bool","void","char","string","eof","%eof"]
        bit_start = st Prelude.* 44
        bit_end = (st Prelude.+ 1) Prelude.* 44
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..43]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (7) = happyShift action_2
action_0 (14) = happyShift action_5
action_0 (24) = happyShift action_6
action_0 (25) = happyShift action_7
action_0 (26) = happyShift action_8
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (7) = happyShift action_2
action_2 (14) = happyShift action_5
action_2 (24) = happyShift action_6
action_2 (25) = happyShift action_7
action_2 (26) = happyShift action_8
action_2 (4) = happyGoto action_15
action_2 (5) = happyGoto action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (13) = happyShift action_10
action_3 (14) = happyShift action_11
action_3 (15) = happyShift action_12
action_3 (16) = happyShift action_13
action_3 (17) = happyShift action_14
action_3 (44) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (7) = happyShift action_2
action_5 (14) = happyShift action_5
action_5 (24) = happyShift action_6
action_5 (25) = happyShift action_7
action_5 (26) = happyShift action_8
action_5 (4) = happyGoto action_9
action_5 (5) = happyGoto action_4
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_3

action_7 _ = happyReduce_10

action_8 _ = happyReduce_11

action_9 (13) = happyShift action_10
action_9 (14) = happyShift action_11
action_9 (15) = happyShift action_12
action_9 (16) = happyShift action_13
action_9 (17) = happyShift action_14
action_9 _ = happyReduce_9

action_10 (7) = happyShift action_2
action_10 (14) = happyShift action_5
action_10 (24) = happyShift action_6
action_10 (25) = happyShift action_7
action_10 (26) = happyShift action_8
action_10 (4) = happyGoto action_21
action_10 (5) = happyGoto action_4
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (7) = happyShift action_2
action_11 (14) = happyShift action_5
action_11 (24) = happyShift action_6
action_11 (25) = happyShift action_7
action_11 (26) = happyShift action_8
action_11 (4) = happyGoto action_20
action_11 (5) = happyGoto action_4
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (7) = happyShift action_2
action_12 (14) = happyShift action_5
action_12 (24) = happyShift action_6
action_12 (25) = happyShift action_7
action_12 (26) = happyShift action_8
action_12 (4) = happyGoto action_19
action_12 (5) = happyGoto action_4
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (7) = happyShift action_2
action_13 (14) = happyShift action_5
action_13 (24) = happyShift action_6
action_13 (25) = happyShift action_7
action_13 (26) = happyShift action_8
action_13 (4) = happyGoto action_18
action_13 (5) = happyGoto action_4
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (7) = happyShift action_2
action_14 (14) = happyShift action_5
action_14 (24) = happyShift action_6
action_14 (25) = happyShift action_7
action_14 (26) = happyShift action_8
action_14 (4) = happyGoto action_17
action_14 (5) = happyGoto action_4
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (10) = happyShift action_16
action_15 (13) = happyShift action_10
action_15 (14) = happyShift action_11
action_15 (15) = happyShift action_12
action_15 (16) = happyShift action_13
action_15 (17) = happyShift action_14
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_1

action_17 (13) = happyShift action_10
action_17 _ = happyReduce_8

action_18 (13) = happyShift action_10
action_18 _ = happyReduce_7

action_19 (13) = happyShift action_10
action_19 _ = happyReduce_6

action_20 (13) = happyShift action_10
action_20 (14) = happyShift action_11
action_20 (15) = happyShift action_12
action_20 (16) = happyShift action_13
action_20 (17) = happyShift action_14
action_20 _ = happyReduce_5

action_21 _ = happyReduce_4

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (INTCONST_EXP happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (IDENTIFIER_EXP happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (UNOP_EXP (Unop happy_var_1 happy_var_2)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (DECNUM_INTCONST happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (HEXNUM_INTCONST happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token SEMICOLON _ -> cont 6;
	Token OPEN_PAREN _ -> cont 7;
	Token OPEN_BRACK _ -> cont 8;
	Token OPEN_BRACE _ -> cont 9;
	Token CLOSE_PAREN _ -> cont 10;
	Token CLOSE_BRACK _ -> cont 11;
	Token CLOSE_BRACE _ -> cont 12;
	Token PLUS _ -> cont 13;
	Token DASH _ -> cont 14;
	Token STAR _ -> cont 15;
	Token SLASH _ -> cont 16;
	Token PERC _ -> cont 17;
	Token PLUS_EQ _ -> cont 18;
	Token DASH_EQ _ -> cont 19;
	Token STAR_EQ _ -> cont 20;
	Token SLASH_EQ _ -> cont 21;
	Token PERC_EQ _ -> cont 22;
	Token EQUAL _ -> cont 23;
	Token (IDENTIFIER _) _ -> cont 24;
	Token (DECNUM _) _ -> cont 25;
	Token (HEXNUM _) _ -> cont 26;
	Token WHILE _ -> cont 27;
	Token OPEN_PAREN _ -> cont 28;
	Token CONTINUE _ -> cont 29;
	Token BREAK _ -> cont 30;
	Token RETURN _ -> cont 31;
	Token ASSERT _ -> cont 32;
	Token TRUE _ -> cont 33;
	Token FALSE _ -> cont 34;
	Token NULL _ -> cont 35;
	Token ALLOC _ -> cont 36;
	Token ALLOC_ARRAY _ -> cont 37;
	Token OPEN_PAREN _ -> cont 38;
	Token BOOL _ -> cont 39;
	Token VOID _ -> cont 40;
	Token CHAR _ -> cont 41;
	Token STRING _ -> cont 42;
	Token EOF _ -> cont 43;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens = error (show tokens)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
