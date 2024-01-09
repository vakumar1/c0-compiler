{-# OPTIONS_GHC -w #-}
module Parser (
    parser,
) where

import Tokens
import Types
import Ast
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
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
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,265) ([0,0,0,0,2048,0,0,0,0,8192,0,0,0,0,1,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,128,0,4,0,0,0,0,8,0,0,0,0,0,0,0,0,0,4256,3,32768,24643,0,0,0,0,0,0,16384,0,0,0,0,10240,196,0,4320,24,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,65528,0,0,32768,65487,15,0,0,2048,196,0,224,16,8192,784,0,896,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,196,0,224,16,0,0,0,0,0,0,0,0,0,0,0,53121,4095,0,0,0,50184,0,57344,4096,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,16384,65342,63,0,0,0,0,0,0,0,32768,3136,0,3584,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4128,3,32768,16387,0,16512,12,0,14,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,784,0,896,64,0,62432,1023,0,0,0,53120,8191,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,196,0,224,16,0,64760,255,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Function","Block","Stmts","Stmt","Simp","Asn","Decl","Type","Post","Lval","Asnop","Postop","Exp","Binop","Unop","';'","'('","'['","'{'","')'","']'","'}'","'+'","'-'","'*'","'/'","'%'","'!'","'~'","'<<'","'>>'","'<'","'>'","'<='","'>='","'=='","'!='","'&'","'^'","'|'","'&&'","'||'","'?'","':'","'='","'+='","'-='","'*='","'/='","'%='","'&='","'^='","'|='","'<<='","'>>='","'++'","'--'","main","ident","dec","hex","while","for","continue","break","return","assert","true","false","null","alloc","alloc_arr","int","bool","void","char","string","eof","%eof"]
        bit_start = st Prelude.* 82
        bit_end = (st Prelude.+ 1) Prelude.* 82
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..81]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (76) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (76) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (61) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (82) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (20) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (78) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (23) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (22) = happyShift action_9
action_7 (5) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_1

action_9 (20) = happyShift action_21
action_9 (22) = happyShift action_9
action_9 (27) = happyShift action_22
action_9 (31) = happyShift action_23
action_9 (32) = happyShift action_24
action_9 (62) = happyShift action_25
action_9 (63) = happyShift action_26
action_9 (64) = happyShift action_27
action_9 (69) = happyShift action_28
action_9 (76) = happyShift action_29
action_9 (77) = happyShift action_30
action_9 (5) = happyGoto action_10
action_9 (6) = happyGoto action_11
action_9 (7) = happyGoto action_12
action_9 (8) = happyGoto action_13
action_9 (9) = happyGoto action_14
action_9 (10) = happyGoto action_15
action_9 (11) = happyGoto action_16
action_9 (12) = happyGoto action_17
action_9 (13) = happyGoto action_18
action_9 (16) = happyGoto action_19
action_9 (18) = happyGoto action_20
action_9 _ = happyReduce_3

action_10 _ = happyReduce_6

action_11 (25) = happyShift action_76
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (20) = happyShift action_21
action_12 (22) = happyShift action_9
action_12 (27) = happyShift action_22
action_12 (31) = happyShift action_23
action_12 (32) = happyShift action_24
action_12 (62) = happyShift action_25
action_12 (63) = happyShift action_26
action_12 (64) = happyShift action_27
action_12 (69) = happyShift action_28
action_12 (76) = happyShift action_29
action_12 (77) = happyShift action_30
action_12 (5) = happyGoto action_10
action_12 (6) = happyGoto action_75
action_12 (7) = happyGoto action_12
action_12 (8) = happyGoto action_13
action_12 (9) = happyGoto action_14
action_12 (10) = happyGoto action_15
action_12 (11) = happyGoto action_16
action_12 (12) = happyGoto action_17
action_12 (13) = happyGoto action_18
action_12 (16) = happyGoto action_19
action_12 (18) = happyGoto action_20
action_12 _ = happyReduce_3

action_13 (19) = happyShift action_74
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_8

action_15 _ = happyReduce_9

action_16 (62) = happyShift action_73
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_10

action_18 (48) = happyShift action_60
action_18 (49) = happyShift action_61
action_18 (50) = happyShift action_62
action_18 (51) = happyShift action_63
action_18 (52) = happyShift action_64
action_18 (53) = happyShift action_65
action_18 (54) = happyShift action_66
action_18 (55) = happyShift action_67
action_18 (56) = happyShift action_68
action_18 (57) = happyShift action_69
action_18 (58) = happyShift action_70
action_18 (59) = happyShift action_71
action_18 (60) = happyShift action_72
action_18 (14) = happyGoto action_58
action_18 (15) = happyGoto action_59
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (26) = happyShift action_39
action_19 (27) = happyShift action_40
action_19 (28) = happyShift action_41
action_19 (29) = happyShift action_42
action_19 (30) = happyShift action_43
action_19 (33) = happyShift action_44
action_19 (34) = happyShift action_45
action_19 (35) = happyShift action_46
action_19 (36) = happyShift action_47
action_19 (37) = happyShift action_48
action_19 (38) = happyShift action_49
action_19 (39) = happyShift action_50
action_19 (40) = happyShift action_51
action_19 (41) = happyShift action_52
action_19 (42) = happyShift action_53
action_19 (43) = happyShift action_54
action_19 (44) = happyShift action_55
action_19 (45) = happyShift action_56
action_19 (46) = happyShift action_57
action_19 (17) = happyGoto action_38
action_19 _ = happyReduce_11

action_20 (20) = happyShift action_32
action_20 (27) = happyShift action_22
action_20 (31) = happyShift action_23
action_20 (32) = happyShift action_24
action_20 (62) = happyShift action_33
action_20 (63) = happyShift action_26
action_20 (64) = happyShift action_27
action_20 (77) = happyShift action_34
action_20 (16) = happyGoto action_37
action_20 (18) = happyGoto action_20
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (20) = happyShift action_21
action_21 (27) = happyShift action_22
action_21 (31) = happyShift action_23
action_21 (32) = happyShift action_24
action_21 (62) = happyShift action_25
action_21 (63) = happyShift action_26
action_21 (64) = happyShift action_27
action_21 (77) = happyShift action_34
action_21 (13) = happyGoto action_35
action_21 (16) = happyGoto action_36
action_21 (18) = happyGoto action_20
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_59

action_23 _ = happyReduce_61

action_24 _ = happyReduce_60

action_25 (23) = happyReduce_37
action_25 (48) = happyReduce_18
action_25 (49) = happyReduce_18
action_25 (50) = happyReduce_18
action_25 (51) = happyReduce_18
action_25 (52) = happyReduce_18
action_25 (53) = happyReduce_18
action_25 (54) = happyReduce_18
action_25 (55) = happyReduce_18
action_25 (56) = happyReduce_18
action_25 (57) = happyReduce_18
action_25 (58) = happyReduce_18
action_25 (59) = happyReduce_18
action_25 (60) = happyReduce_18
action_25 _ = happyReduce_37

action_26 _ = happyReduce_35

action_27 _ = happyReduce_34

action_28 (20) = happyShift action_32
action_28 (27) = happyShift action_22
action_28 (31) = happyShift action_23
action_28 (32) = happyShift action_24
action_28 (62) = happyShift action_33
action_28 (63) = happyShift action_26
action_28 (64) = happyShift action_27
action_28 (77) = happyShift action_34
action_28 (16) = happyGoto action_31
action_28 (18) = happyGoto action_20
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_15

action_30 (62) = happyReduce_16
action_30 _ = happyReduce_36

action_31 (19) = happyShift action_83
action_31 (26) = happyShift action_39
action_31 (27) = happyShift action_40
action_31 (28) = happyShift action_41
action_31 (29) = happyShift action_42
action_31 (30) = happyShift action_43
action_31 (33) = happyShift action_44
action_31 (34) = happyShift action_45
action_31 (35) = happyShift action_46
action_31 (36) = happyShift action_47
action_31 (37) = happyShift action_48
action_31 (38) = happyShift action_49
action_31 (39) = happyShift action_50
action_31 (40) = happyShift action_51
action_31 (41) = happyShift action_52
action_31 (42) = happyShift action_53
action_31 (43) = happyShift action_54
action_31 (44) = happyShift action_55
action_31 (45) = happyShift action_56
action_31 (46) = happyShift action_57
action_31 (17) = happyGoto action_38
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (20) = happyShift action_32
action_32 (27) = happyShift action_22
action_32 (31) = happyShift action_23
action_32 (32) = happyShift action_24
action_32 (62) = happyShift action_33
action_32 (63) = happyShift action_26
action_32 (64) = happyShift action_27
action_32 (77) = happyShift action_34
action_32 (16) = happyGoto action_36
action_32 (18) = happyGoto action_20
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_37

action_34 _ = happyReduce_36

action_35 (23) = happyShift action_82
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (23) = happyShift action_81
action_36 (26) = happyShift action_39
action_36 (27) = happyShift action_40
action_36 (28) = happyShift action_41
action_36 (29) = happyShift action_42
action_36 (30) = happyShift action_43
action_36 (33) = happyShift action_44
action_36 (34) = happyShift action_45
action_36 (35) = happyShift action_46
action_36 (36) = happyShift action_47
action_36 (37) = happyShift action_48
action_36 (38) = happyShift action_49
action_36 (39) = happyShift action_50
action_36 (40) = happyShift action_51
action_36 (41) = happyShift action_52
action_36 (42) = happyShift action_53
action_36 (43) = happyShift action_54
action_36 (44) = happyShift action_55
action_36 (45) = happyShift action_56
action_36 (46) = happyShift action_57
action_36 (17) = happyGoto action_38
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (26) = happyShift action_39
action_37 (27) = happyShift action_40
action_37 (28) = happyShift action_41
action_37 (29) = happyShift action_42
action_37 (30) = happyShift action_43
action_37 (33) = happyShift action_44
action_37 (34) = happyShift action_45
action_37 (35) = happyShift action_46
action_37 (36) = happyShift action_47
action_37 (37) = happyShift action_48
action_37 (38) = happyShift action_49
action_37 (39) = happyShift action_50
action_37 (40) = happyShift action_51
action_37 (41) = happyShift action_52
action_37 (42) = happyShift action_53
action_37 (43) = happyShift action_54
action_37 (44) = happyShift action_55
action_37 (45) = happyShift action_56
action_37 (46) = happyShift action_57
action_37 (17) = happyGoto action_38
action_37 _ = happyReduce_39

action_38 (20) = happyShift action_32
action_38 (27) = happyShift action_22
action_38 (31) = happyShift action_23
action_38 (32) = happyShift action_24
action_38 (62) = happyShift action_33
action_38 (63) = happyShift action_26
action_38 (64) = happyShift action_27
action_38 (77) = happyShift action_34
action_38 (16) = happyGoto action_80
action_38 (18) = happyGoto action_20
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_41

action_40 _ = happyReduce_42

action_41 _ = happyReduce_43

action_42 _ = happyReduce_44

action_43 _ = happyReduce_45

action_44 _ = happyReduce_58

action_45 _ = happyReduce_57

action_46 _ = happyReduce_46

action_47 _ = happyReduce_48

action_48 _ = happyReduce_47

action_49 _ = happyReduce_49

action_50 _ = happyReduce_50

action_51 _ = happyReduce_51

action_52 _ = happyReduce_54

action_53 _ = happyReduce_55

action_54 _ = happyReduce_56

action_55 _ = happyReduce_52

action_56 _ = happyReduce_53

action_57 (20) = happyShift action_32
action_57 (27) = happyShift action_22
action_57 (31) = happyShift action_23
action_57 (32) = happyShift action_24
action_57 (62) = happyShift action_33
action_57 (63) = happyShift action_26
action_57 (64) = happyShift action_27
action_57 (77) = happyShift action_34
action_57 (16) = happyGoto action_79
action_57 (18) = happyGoto action_20
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (20) = happyShift action_32
action_58 (27) = happyShift action_22
action_58 (31) = happyShift action_23
action_58 (32) = happyShift action_24
action_58 (62) = happyShift action_33
action_58 (63) = happyShift action_26
action_58 (64) = happyShift action_27
action_58 (77) = happyShift action_34
action_58 (16) = happyGoto action_78
action_58 (18) = happyGoto action_20
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_17

action_60 _ = happyReduce_20

action_61 _ = happyReduce_21

action_62 _ = happyReduce_22

action_63 _ = happyReduce_23

action_64 _ = happyReduce_24

action_65 _ = happyReduce_25

action_66 _ = happyReduce_26

action_67 _ = happyReduce_27

action_68 _ = happyReduce_28

action_69 _ = happyReduce_29

action_70 _ = happyReduce_30

action_71 _ = happyReduce_31

action_72 _ = happyReduce_32

action_73 (48) = happyShift action_77
action_73 _ = happyReduce_13

action_74 _ = happyReduce_5

action_75 _ = happyReduce_4

action_76 _ = happyReduce_2

action_77 (20) = happyShift action_32
action_77 (27) = happyShift action_22
action_77 (31) = happyShift action_23
action_77 (32) = happyShift action_24
action_77 (62) = happyShift action_33
action_77 (63) = happyShift action_26
action_77 (64) = happyShift action_27
action_77 (77) = happyShift action_34
action_77 (16) = happyGoto action_85
action_77 (18) = happyGoto action_20
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (26) = happyShift action_39
action_78 (27) = happyShift action_40
action_78 (28) = happyShift action_41
action_78 (29) = happyShift action_42
action_78 (30) = happyShift action_43
action_78 (33) = happyShift action_44
action_78 (34) = happyShift action_45
action_78 (35) = happyShift action_46
action_78 (36) = happyShift action_47
action_78 (37) = happyShift action_48
action_78 (38) = happyShift action_49
action_78 (39) = happyShift action_50
action_78 (40) = happyShift action_51
action_78 (41) = happyShift action_52
action_78 (42) = happyShift action_53
action_78 (43) = happyShift action_54
action_78 (44) = happyShift action_55
action_78 (45) = happyShift action_56
action_78 (46) = happyShift action_57
action_78 (17) = happyGoto action_38
action_78 _ = happyReduce_12

action_79 (26) = happyShift action_39
action_79 (27) = happyShift action_40
action_79 (28) = happyShift action_41
action_79 (29) = happyShift action_42
action_79 (30) = happyShift action_43
action_79 (33) = happyShift action_44
action_79 (34) = happyShift action_45
action_79 (35) = happyShift action_46
action_79 (36) = happyShift action_47
action_79 (37) = happyShift action_48
action_79 (38) = happyShift action_49
action_79 (39) = happyShift action_50
action_79 (40) = happyShift action_51
action_79 (41) = happyShift action_52
action_79 (42) = happyShift action_53
action_79 (43) = happyShift action_54
action_79 (44) = happyShift action_55
action_79 (45) = happyShift action_56
action_79 (46) = happyShift action_57
action_79 (47) = happyShift action_84
action_79 (17) = happyGoto action_38
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (26) = happyShift action_39
action_80 (27) = happyShift action_40
action_80 (28) = happyShift action_41
action_80 (29) = happyShift action_42
action_80 (30) = happyShift action_43
action_80 (33) = happyShift action_44
action_80 (34) = happyShift action_45
action_80 (35) = happyShift action_46
action_80 (36) = happyShift action_47
action_80 (37) = happyShift action_48
action_80 (38) = happyShift action_49
action_80 (39) = happyShift action_50
action_80 (40) = happyShift action_51
action_80 (41) = happyShift action_52
action_80 (42) = happyShift action_53
action_80 (43) = happyShift action_54
action_80 (44) = happyShift action_55
action_80 (45) = happyShift action_56
action_80 (46) = happyShift action_57
action_80 (17) = happyGoto action_38
action_80 _ = happyReduce_38

action_81 _ = happyReduce_33

action_82 _ = happyReduce_19

action_83 _ = happyReduce_7

action_84 (20) = happyShift action_32
action_84 (27) = happyShift action_22
action_84 (31) = happyShift action_23
action_84 (32) = happyShift action_24
action_84 (62) = happyShift action_33
action_84 (63) = happyShift action_26
action_84 (64) = happyShift action_27
action_84 (77) = happyShift action_34
action_84 (16) = happyGoto action_86
action_84 (18) = happyGoto action_20
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (26) = happyShift action_39
action_85 (27) = happyShift action_40
action_85 (28) = happyShift action_41
action_85 (29) = happyShift action_42
action_85 (30) = happyShift action_43
action_85 (33) = happyShift action_44
action_85 (34) = happyShift action_45
action_85 (35) = happyShift action_46
action_85 (36) = happyShift action_47
action_85 (37) = happyShift action_48
action_85 (38) = happyShift action_49
action_85 (39) = happyShift action_50
action_85 (40) = happyShift action_51
action_85 (41) = happyShift action_52
action_85 (42) = happyShift action_53
action_85 (43) = happyShift action_54
action_85 (44) = happyShift action_55
action_85 (45) = happyShift action_56
action_85 (46) = happyShift action_57
action_85 (17) = happyGoto action_38
action_85 _ = happyReduce_14

action_86 (26) = happyShift action_39
action_86 (27) = happyShift action_40
action_86 (28) = happyShift action_41
action_86 (29) = happyShift action_42
action_86 (30) = happyShift action_43
action_86 (33) = happyShift action_44
action_86 (34) = happyShift action_45
action_86 (35) = happyShift action_46
action_86 (36) = happyShift action_47
action_86 (37) = happyShift action_48
action_86 (38) = happyShift action_49
action_86 (39) = happyShift action_50
action_86 (40) = happyShift action_51
action_86 (41) = happyShift action_52
action_86 (42) = happyShift action_53
action_86 (43) = happyShift action_54
action_86 (44) = happyShift action_55
action_86 (45) = happyShift action_56
action_86 (46) = happyShift action_57
action_86 (17) = happyGoto action_38
action_86 _ = happyReduce_40

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Function happy_var_2 (Type INT_TYPE happy_var_1) happy_var_6
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
		 (SIMP_STMT happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (BLOCK_STMT happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (RET_STMT happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (ASN_SIMP happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (DECL_SIMP happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (POSTOP_SIMP happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn8
		 (EXP_SIMP happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn9
		 (Asn happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Decl happy_var_2 (Type INT_TYPE happy_var_1) Nothing Nothing
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Decl happy_var_2 (Type INT_TYPE happy_var_1) (Just happy_var_3) (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (Type INT_TYPE happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (Type BOOL_TYPE happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (Post happy_var_2 happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (Lval happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  14 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (HEXNUM_EXP happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (DECNUM_EXP happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  16 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (BOOL_EXP happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  16 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (IDENTIFIER_EXP happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (BINOP_EXP (Binop happy_var_2 happy_var_1 happy_var_3)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  16 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (UNOP_EXP (Unop happy_var_1 happy_var_2)
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 16 happyReduction_40
happyReduction_40 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (TERN_EXP happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  17 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  17 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  17 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  17 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  17 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  17 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  17 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  17 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  17 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  17 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  17 happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  17 happyReduction_52
happyReduction_52 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  17 happyReduction_53
happyReduction_53 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  17 happyReduction_54
happyReduction_54 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  17 happyReduction_55
happyReduction_55 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  17 happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  17 happyReduction_57
happyReduction_57 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  17 happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  18 happyReduction_59
happyReduction_59 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  18 happyReduction_60
happyReduction_60 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  18 happyReduction_61
happyReduction_61 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 82 82 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token SEMICOLON _ -> cont 19;
	Token OPEN_PAREN _ -> cont 20;
	Token OPEN_BRACK _ -> cont 21;
	Token OPEN_BRACE _ -> cont 22;
	Token CLOSE_PAREN _ -> cont 23;
	Token CLOSE_BRACK _ -> cont 24;
	Token CLOSE_BRACE _ -> cont 25;
	Token PLUS _ -> cont 26;
	Token DASH _ -> cont 27;
	Token STAR _ -> cont 28;
	Token SLASH _ -> cont 29;
	Token PERC _ -> cont 30;
	Token EXCL _ -> cont 31;
	Token TILDE _ -> cont 32;
	Token LEFT_LEFT _ -> cont 33;
	Token RIGHT_RIGHT _ -> cont 34;
	Token LEFT _ -> cont 35;
	Token RIGHT _ -> cont 36;
	Token LEFT_EQ _ -> cont 37;
	Token RIGHT_EQ _ -> cont 38;
	Token EQ_EQ _ -> cont 39;
	Token EXCL_EQ _ -> cont 40;
	Token AMP _ -> cont 41;
	Token CARET _ -> cont 42;
	Token PIPE _ -> cont 43;
	Token AMP_AMP _ -> cont 44;
	Token PIPE_PIPE _ -> cont 45;
	Token QUEST _ -> cont 46;
	Token COLON _ -> cont 47;
	Token EQUAL _ -> cont 48;
	Token PLUS_EQ _ -> cont 49;
	Token DASH_EQ _ -> cont 50;
	Token STAR_EQ _ -> cont 51;
	Token SLASH_EQ _ -> cont 52;
	Token PERC_EQ _ -> cont 53;
	Token AMP_EQ _ -> cont 54;
	Token CARET_EQ _ -> cont 55;
	Token PIPE_EQ _ -> cont 56;
	Token LEFT_LEFT_EQ _ -> cont 57;
	Token RIGHT_RIGHT_EQ _ -> cont 58;
	Token PLUS_PLUS _ -> cont 59;
	Token DASH_DASH _ -> cont 60;
	Token (IDENTIFIER "main") _ -> cont 61;
	Token (IDENTIFIER _) _ -> cont 62;
	Token (DECNUM _) _ -> cont 63;
	Token (HEXNUM _) _ -> cont 64;
	Token WHILE _ -> cont 65;
	Token FOR _ -> cont 66;
	Token CONTINUE _ -> cont 67;
	Token BREAK _ -> cont 68;
	Token RETURN _ -> cont 69;
	Token ASSERT _ -> cont 70;
	Token TRUE _ -> cont 71;
	Token FALSE _ -> cont 72;
	Token NULL _ -> cont 73;
	Token ALLOC _ -> cont 74;
	Token ALLOC_ARRAY _ -> cont 75;
	Token INT _ -> cont 76;
	Token BOOL _ -> cont 77;
	Token VOID _ -> cont 78;
	Token CHAR _ -> cont 79;
	Token STRING _ -> cont 80;
	Token EOF _ -> cont 81;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 82 tk tks = happyError' (tks, explist)
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
