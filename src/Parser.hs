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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,130) ([0,0,8192,0,0,0,2,0,64,0,0,0,0,8192,0,0,0,0,0,8,256,0,0,2048,0,0,0,0,0,0,2,1032,2,1024,0,0,512,2048,516,4096,0,0,0,1,0,0,0,63,0,512,2048,0,0,0,0,0,258,56,0,0,128,0,0,512,0,4096,248,0,0,0,0,0,4128,896,0,512,14337,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,8192,32784,3,0,258,56,0,4128,896,0,512,14337,0,8192,32784,3,0,258,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63488,0,0,32768,15,0,0,248,0,0,3968,0,0,63488,0,0,32768,15,0,0,0,0,0,3968,0,0,63744,0,0,0,0,0,8192,32784,3,0,258,56,0,4128,896,0,512,14337,0,8192,32784,3,0,258,56,0,63488,0,0,32768,0,0,0,8,0,0,128,0,0,63488,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Program","Block","Stmts","Stmt","Decl","Simp","Lval","Exp","Intconst","';'","'('","'['","'{'","')'","']'","'}'","'+'","'-'","'*'","'/'","'%'","'+='","'-='","'*='","'/='","'%='","'='","main","ident","dec","hex","while","for","continue","break","return","assert","true","false","null","alloc","alloc_arr","int","bool","void","char","string","eof","%eof"]
        bit_start = st Prelude.* 52
        bit_end = (st Prelude.+ 1) Prelude.* 52
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..51]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (46) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (46) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (31) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (52) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (14) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (48) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (17) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (16) = happyShift action_9
action_7 (5) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_1

action_9 (14) = happyShift action_15
action_9 (32) = happyShift action_16
action_9 (39) = happyShift action_17
action_9 (46) = happyShift action_18
action_9 (6) = happyGoto action_10
action_9 (7) = happyGoto action_11
action_9 (8) = happyGoto action_12
action_9 (9) = happyGoto action_13
action_9 (10) = happyGoto action_14
action_9 _ = happyReduce_3

action_10 (19) = happyShift action_37
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (14) = happyShift action_15
action_11 (32) = happyShift action_16
action_11 (39) = happyShift action_17
action_11 (46) = happyShift action_18
action_11 (6) = happyGoto action_36
action_11 (7) = happyGoto action_11
action_11 (8) = happyGoto action_12
action_11 (9) = happyGoto action_13
action_11 (10) = happyGoto action_14
action_11 _ = happyReduce_3

action_12 (13) = happyShift action_35
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (13) = happyShift action_34
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (25) = happyShift action_28
action_14 (26) = happyShift action_29
action_14 (27) = happyShift action_30
action_14 (28) = happyShift action_31
action_14 (29) = happyShift action_32
action_14 (30) = happyShift action_33
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_15
action_15 (32) = happyShift action_16
action_15 (10) = happyGoto action_27
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_16

action_17 (14) = happyShift action_22
action_17 (21) = happyShift action_23
action_17 (32) = happyShift action_24
action_17 (33) = happyShift action_25
action_17 (34) = happyShift action_26
action_17 (11) = happyGoto action_20
action_17 (12) = happyGoto action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (32) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (30) = happyShift action_53
action_19 _ = happyReduce_8

action_20 (13) = happyShift action_47
action_20 (20) = happyShift action_48
action_20 (21) = happyShift action_49
action_20 (22) = happyShift action_50
action_20 (23) = happyShift action_51
action_20 (24) = happyShift action_52
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_19

action_22 (14) = happyShift action_22
action_22 (21) = happyShift action_23
action_22 (32) = happyShift action_24
action_22 (33) = happyShift action_25
action_22 (34) = happyShift action_26
action_22 (11) = happyGoto action_46
action_22 (12) = happyGoto action_21
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_22
action_23 (21) = happyShift action_23
action_23 (32) = happyShift action_24
action_23 (33) = happyShift action_25
action_23 (34) = happyShift action_26
action_23 (11) = happyGoto action_45
action_23 (12) = happyGoto action_21
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_20

action_25 _ = happyReduce_27

action_26 _ = happyReduce_28

action_27 (17) = happyShift action_44
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_22
action_28 (21) = happyShift action_23
action_28 (32) = happyShift action_24
action_28 (33) = happyShift action_25
action_28 (34) = happyShift action_26
action_28 (11) = happyGoto action_43
action_28 (12) = happyGoto action_21
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_22
action_29 (21) = happyShift action_23
action_29 (32) = happyShift action_24
action_29 (33) = happyShift action_25
action_29 (34) = happyShift action_26
action_29 (11) = happyGoto action_42
action_29 (12) = happyGoto action_21
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (14) = happyShift action_22
action_30 (21) = happyShift action_23
action_30 (32) = happyShift action_24
action_30 (33) = happyShift action_25
action_30 (34) = happyShift action_26
action_30 (11) = happyGoto action_41
action_30 (12) = happyGoto action_21
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (14) = happyShift action_22
action_31 (21) = happyShift action_23
action_31 (32) = happyShift action_24
action_31 (33) = happyShift action_25
action_31 (34) = happyShift action_26
action_31 (11) = happyGoto action_40
action_31 (12) = happyGoto action_21
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (14) = happyShift action_22
action_32 (21) = happyShift action_23
action_32 (32) = happyShift action_24
action_32 (33) = happyShift action_25
action_32 (34) = happyShift action_26
action_32 (11) = happyGoto action_39
action_32 (12) = happyGoto action_21
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (14) = happyShift action_22
action_33 (21) = happyShift action_23
action_33 (32) = happyShift action_24
action_33 (33) = happyShift action_25
action_33 (34) = happyShift action_26
action_33 (11) = happyGoto action_38
action_33 (12) = happyGoto action_21
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_6

action_35 _ = happyReduce_5

action_36 _ = happyReduce_4

action_37 _ = happyReduce_2

action_38 (20) = happyShift action_48
action_38 (21) = happyShift action_49
action_38 (22) = happyShift action_50
action_38 (23) = happyShift action_51
action_38 (24) = happyShift action_52
action_38 _ = happyReduce_10

action_39 (20) = happyShift action_48
action_39 (21) = happyShift action_49
action_39 (22) = happyShift action_50
action_39 (23) = happyShift action_51
action_39 (24) = happyShift action_52
action_39 _ = happyReduce_15

action_40 (20) = happyShift action_48
action_40 (21) = happyShift action_49
action_40 (22) = happyShift action_50
action_40 (23) = happyShift action_51
action_40 (24) = happyShift action_52
action_40 _ = happyReduce_14

action_41 (20) = happyShift action_48
action_41 (21) = happyShift action_49
action_41 (22) = happyShift action_50
action_41 (23) = happyShift action_51
action_41 (24) = happyShift action_52
action_41 _ = happyReduce_13

action_42 (20) = happyShift action_48
action_42 (21) = happyShift action_49
action_42 (22) = happyShift action_50
action_42 (23) = happyShift action_51
action_42 (24) = happyShift action_52
action_42 _ = happyReduce_12

action_43 (20) = happyShift action_48
action_43 (21) = happyShift action_49
action_43 (22) = happyShift action_50
action_43 (23) = happyShift action_51
action_43 (24) = happyShift action_52
action_43 _ = happyReduce_11

action_44 _ = happyReduce_17

action_45 (20) = happyShift action_48
action_45 (21) = happyShift action_49
action_45 (22) = happyShift action_50
action_45 (23) = happyShift action_51
action_45 (24) = happyShift action_52
action_45 _ = happyReduce_26

action_46 (17) = happyShift action_60
action_46 (20) = happyShift action_48
action_46 (21) = happyShift action_49
action_46 (22) = happyShift action_50
action_46 (23) = happyShift action_51
action_46 (24) = happyShift action_52
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_7

action_48 (14) = happyShift action_22
action_48 (21) = happyShift action_23
action_48 (32) = happyShift action_24
action_48 (33) = happyShift action_25
action_48 (34) = happyShift action_26
action_48 (11) = happyGoto action_59
action_48 (12) = happyGoto action_21
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (14) = happyShift action_22
action_49 (21) = happyShift action_23
action_49 (32) = happyShift action_24
action_49 (33) = happyShift action_25
action_49 (34) = happyShift action_26
action_49 (11) = happyGoto action_58
action_49 (12) = happyGoto action_21
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (14) = happyShift action_22
action_50 (21) = happyShift action_23
action_50 (32) = happyShift action_24
action_50 (33) = happyShift action_25
action_50 (34) = happyShift action_26
action_50 (11) = happyGoto action_57
action_50 (12) = happyGoto action_21
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (14) = happyShift action_22
action_51 (21) = happyShift action_23
action_51 (32) = happyShift action_24
action_51 (33) = happyShift action_25
action_51 (34) = happyShift action_26
action_51 (11) = happyGoto action_56
action_51 (12) = happyGoto action_21
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (14) = happyShift action_22
action_52 (21) = happyShift action_23
action_52 (32) = happyShift action_24
action_52 (33) = happyShift action_25
action_52 (34) = happyShift action_26
action_52 (11) = happyGoto action_55
action_52 (12) = happyGoto action_21
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (14) = happyShift action_22
action_53 (21) = happyShift action_23
action_53 (32) = happyShift action_24
action_53 (33) = happyShift action_25
action_53 (34) = happyShift action_26
action_53 (11) = happyGoto action_54
action_53 (12) = happyGoto action_21
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (20) = happyShift action_48
action_54 (21) = happyShift action_49
action_54 (22) = happyShift action_50
action_54 (23) = happyShift action_51
action_54 (24) = happyShift action_52
action_54 _ = happyReduce_9

action_55 (20) = happyShift action_48
action_55 _ = happyReduce_25

action_56 (20) = happyShift action_48
action_56 _ = happyReduce_24

action_57 (20) = happyShift action_48
action_57 _ = happyReduce_23

action_58 (20) = happyShift action_48
action_58 (21) = happyShift action_49
action_58 (22) = happyShift action_50
action_58 (23) = happyShift action_51
action_58 (24) = happyShift action_52
action_58 _ = happyReduce_22

action_59 _ = happyReduce_21

action_60 _ = happyReduce_18

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  6 happyReduction_3
happyReduction_3  =  HappyAbsSyn6
		 ([]
	)

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1):(happy_var_2)
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (DECL_STMT happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (SIMP_STMT happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (RET_STMT happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Decl happy_var_2 Nothing
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Decl happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Simp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (Lval happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (INTCONST_EXP happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (IDENTIFIER_EXP happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  11 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (UNOP_EXP (Unop happy_var_1 happy_var_2)
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (DECNUM_INTCONST happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (HEXNUM_INTCONST happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 52 52 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token SEMICOLON _ -> cont 13;
	Token OPEN_PAREN _ -> cont 14;
	Token OPEN_BRACK _ -> cont 15;
	Token OPEN_BRACE _ -> cont 16;
	Token CLOSE_PAREN _ -> cont 17;
	Token CLOSE_BRACK _ -> cont 18;
	Token CLOSE_BRACE _ -> cont 19;
	Token PLUS _ -> cont 20;
	Token DASH _ -> cont 21;
	Token STAR _ -> cont 22;
	Token SLASH _ -> cont 23;
	Token PERC _ -> cont 24;
	Token PLUS_EQ _ -> cont 25;
	Token DASH_EQ _ -> cont 26;
	Token STAR_EQ _ -> cont 27;
	Token SLASH_EQ _ -> cont 28;
	Token PERC_EQ _ -> cont 29;
	Token EQUAL _ -> cont 30;
	Token (IDENTIFIER "main") _ -> cont 31;
	Token (IDENTIFIER _) _ -> cont 32;
	Token (DECNUM _) _ -> cont 33;
	Token (HEXNUM _) _ -> cont 34;
	Token WHILE _ -> cont 35;
	Token FOR _ -> cont 36;
	Token CONTINUE _ -> cont 37;
	Token BREAK _ -> cont 38;
	Token RETURN _ -> cont 39;
	Token ASSERT _ -> cont 40;
	Token TRUE _ -> cont 41;
	Token FALSE _ -> cont 42;
	Token NULL _ -> cont 43;
	Token ALLOC _ -> cont 44;
	Token ALLOC_ARRAY _ -> cont 45;
	Token INT _ -> cont 46;
	Token BOOL _ -> cont 47;
	Token VOID _ -> cont 48;
	Token CHAR _ -> cont 49;
	Token STRING _ -> cont 50;
	Token EOF _ -> cont 51;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 52 tk tks = happyError' (tks, explist)
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
