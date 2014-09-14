pragma License (Unrestricted);
--  extended unit, not in RM
package Ada.Wide_Wide_Characters.Latin_1 is
   pragma Pure;

   --  Control characters:

   NUL : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (0);
   SOH : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (1);
   STX : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (2);
   ETX : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (3);
   EOT : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (4);
   ENQ : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (5);
   ACK : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (6);
   BEL : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (7);
   BS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (8);
   HT : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (9);
   LF : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (10);
   VT : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (11);
   FF : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (12);
   CR : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (13);
   SO : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (14);
   SI : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (15);

   DLE : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (16);
   DC1 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (17);
   DC2 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (18);
   DC3 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (19);
   DC4 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (20);
   NAK : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (21);
   SYN : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (22);
   ETB : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (23);
   CAN : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (24);
   EM : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (25);
   SUB : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (26);
   ESC : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (27);
   FS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (28);
   GS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (29);
   RS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (30);
   US : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (31);

   --  ISO 646 graphic characters:

   Space : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (32); -- ' '
   Exclamation : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (33); -- '!'
   Quotation : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (34); -- '"'
   Number_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (35); -- '#'
   Dollar_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (36); -- '$'
   Percent_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (37); -- '%'
   Ampersand : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (38); -- '&'
   Apostrophe : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (39); -- '''
   Left_Parenthesis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (40); -- '('
   Right_Parenthesis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (41); -- ')'
   Asterisk : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (42); -- '*'
   Plus_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (43); -- '+'
   Comma : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (44); -- ','
   Hyphen : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (45); -- '-'
   Minus_Sign : Wide_Wide_Character
      renames Hyphen;
   Full_Stop : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (46); -- '.'
   Solidus : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (47); -- '/'

   --  Decimal digits '0' though '9' are at positions 48 through 57

   Colon : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (58); -- ':'
   Semicolon : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (59); -- ';'
   Less_Than_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (60); -- '<'
   Equals_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (61); -- '='
   Greater_Than_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (62); -- '>'
   Question : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (63); -- '?'
   Commercial_At : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (64); -- '@'

   --  Letters 'A' through 'Z' are at positions 65 through 90

   Left_Square_Bracket : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (91); -- '['
   Reverse_Solidus : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (92); -- '\'
   Right_Square_Bracket : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (93); -- ']'
   Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (94); -- '^'
   Low_Line : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (95); -- '_'

   Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (96); -- '`'
   LC_A : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (97); -- 'a'
   LC_B : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (98); -- 'b'
   LC_C : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (99); -- 'c'
   LC_D : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (100); -- 'd'
   LC_E : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (101); -- 'e'
   LC_F : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (102); -- 'f'
   LC_G : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (103); -- 'g'
   LC_H : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (104); -- 'h'
   LC_I : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (105); -- 'i'
   LC_J : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (106); -- 'j'
   LC_K : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (107); -- 'k'
   LC_L : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (108); -- 'l'
   LC_M : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (109); -- 'm'
   LC_N : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (110); -- 'n'
   LC_O : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (111); -- 'o'

   LC_P : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (112); -- 'p'
   LC_Q : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (113); -- 'q'
   LC_R : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (114); -- 'r'
   LC_S : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (115); -- 's'
   LC_T : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (116); -- 't'
   LC_U : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (117); -- 'u'
   LC_V : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (118); -- 'v'
   LC_W : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (119); -- 'w'
   LC_X : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (120); -- 'x'
   LC_Y : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (121); -- 'y'
   LC_Z : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (122); -- 'z'
   Left_Curly_Bracket : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (123); -- '{'
   Vertical_Line : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (124); -- '|'
   Right_Curly_Bracket : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (125); -- '}'
   Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (126); -- '~'
   DEL : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (127);

   --  ISO 6429 control characters:

   IS4 : Wide_Wide_Character renames FS;
   IS3 : Wide_Wide_Character renames GS;
   IS2 : Wide_Wide_Character renames RS;
   IS1 : Wide_Wide_Character renames US;

   Reserved_128 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (128);
   Reserved_129 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (129);
   BPH : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (130);
   NBH : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (131);
   Reserved_132 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (132);
   NEL : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (133);
   SSA : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (134);
   ESA : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (135);
   HTS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (136);
   HTJ : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (137);
   VTS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (138);
   PLD : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (139);
   PLU : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (140);
   RI : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (141);
   SS2 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (142);
   SS3 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (143);

   DCS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (144);
   PU1 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (145);
   PU2 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (146);
   STS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (147);
   CCH : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (148);
   MW : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (149);
   SPA : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (150);
   EPA : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (151);

   SOS : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (152);
   Reserved_153 : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (153);
   SCI : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (154);
   CSI : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (155);
   ST : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (156);
   OSC : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (157);
   PM : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (158);
   APC : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (159);

   --  Other graphic characters:

   --  Character positions 160 (16#A0#) .. 175 (16#AF#):
   No_Break_Space : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (160); -- ' '
   NBSP : Wide_Wide_Character
      renames No_Break_Space;
   Inverted_Exclamation : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (161); -- '¡'
   Cent_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (162); -- '¢'
   Pound_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (163); -- '£'
   Currency_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (164); -- '¤'
   Yen_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (165); -- '¥'
   Broken_Bar : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (166); -- '¦'
   Section_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (167); -- '§'
   Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (168); -- '¨'
   Copyright_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (169); -- '©'
   Feminine_Ordinal_Indicator : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (170); -- 'ª'
   Left_Angle_Quotation : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (171); -- '«'
   Not_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (172); -- '¬'
   Soft_Hyphen : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (173); -- ' '
   Registered_Trade_Mark_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (174); -- '®'
   Macron : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (175); -- '¯'

   --  Character positions 176 (16#B0#) .. 191 (16#BF#):
   Degree_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (176); -- '°'
   Ring_Above : Wide_Wide_Character
      renames Degree_Sign;
   Plus_Minus_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (177); -- '±'
   Superscript_Two : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (178); -- '²'
   Superscript_Three : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (179); -- '³'
   Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (180); -- '´'
   Micro_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (181); -- 'µ'
   Pilcrow_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (182); -- '¶'
   Paragraph_Sign : Wide_Wide_Character
      renames Pilcrow_Sign;
   Middle_Dot : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (183); -- '·'
   Cedilla : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (184); -- '¸'
   Superscript_One : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (185); -- '¹'
   Masculine_Ordinal_Indicator : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (186); -- 'º'
   Right_Angle_Quotation : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (187); -- '»'
   Fraction_One_Quarter : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (188); -- '¼'
   Fraction_One_Half : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (189); -- '½'
   Fraction_Three_Quarters : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (190); -- '¾'
   Inverted_Question : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (191); -- '¿'

   --  Character positions 192 (16#C0#) .. 207 (16#CF#):
   UC_A_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (192); -- 'À'
   UC_A_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (193); -- 'Á'
   UC_A_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (194); -- 'Â'
   UC_A_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (195); -- 'Ã'
   UC_A_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (196); -- 'Ä'
   UC_A_Ring : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (197); -- 'Å'
   UC_AE_Diphthong : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (198); -- 'Æ'
   UC_C_Cedilla : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (199); -- 'Ç'
   UC_E_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (200); -- 'È'
   UC_E_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (201); -- 'É'
   UC_E_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (202); -- 'Ê'
   UC_E_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (203); -- 'Ë'
   UC_I_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (204); -- 'Ì'
   UC_I_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (205); -- 'Í'
   UC_I_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (206); -- 'Î'
   UC_I_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (207); -- 'Ï'

   --  Character positions 208 (16#D0#) .. 223 (16#DF#):
   UC_Icelandic_Eth : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (208); -- 'Ð'
   UC_N_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (209); -- 'Ñ'
   UC_O_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (210); -- 'Ò'
   UC_O_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (211); -- 'Ó'
   UC_O_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (212); -- 'Ô'
   UC_O_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (213); -- 'Õ'
   UC_O_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (214); -- 'Ö'
   Multiplication_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (215); -- '×'
   UC_O_Oblique_Stroke : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (216); -- 'Ø'
   UC_U_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (217); -- 'Ù'
   UC_U_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (218); -- 'Ú'
   UC_U_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (219); -- 'Û'
   UC_U_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (220); -- 'Ü'
   UC_Y_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (221); -- 'Ý'
   UC_Icelandic_Thorn : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (222); -- 'Þ'
   LC_German_Sharp_S : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (223); -- 'ß'

   --  Character positions 224 (16#E0#) .. 239 (16#EF#):
   LC_A_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (224); -- 'à'
   LC_A_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (225); -- 'á'
   LC_A_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (226); -- 'â'
   LC_A_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (227); -- 'ã'
   LC_A_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (228); -- 'ä'
   LC_A_Ring : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (229); -- 'å'
   LC_AE_Diphthong : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (230); -- 'æ'
   LC_C_Cedilla : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (231); -- 'ç'
   LC_E_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (232); -- 'è'
   LC_E_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (233); -- 'é'
   LC_E_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (234); -- 'ê'
   LC_E_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (235); -- 'ë'
   LC_I_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (236); -- 'ì'
   LC_I_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (237); -- 'í'
   LC_I_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (238); -- 'î'
   LC_I_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (239); -- 'ï'

   --  Character positions 240 (16#F0#) .. 255 (16#FF#):
   LC_Icelandic_Eth : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (240); -- 'ð'
   LC_N_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (241); -- 'ñ'
   LC_O_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (242); -- 'ò'
   LC_O_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (243); -- 'ó'
   LC_O_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (244); -- 'ô'
   LC_O_Tilde : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (245); -- 'õ'
   LC_O_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (246); -- 'ö'
   Division_Sign : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (247); -- '÷';
   LC_O_Oblique_Stroke : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (248); -- 'ø'
   LC_U_Grave : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (249); -- 'ù'
   LC_U_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (250); -- 'ú'
   LC_U_Circumflex : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (251); -- 'û';
   LC_U_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (252); -- 'ü'
   LC_Y_Acute : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (253); -- 'ý'
   LC_Icelandic_Thorn : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (254); -- 'þ'
   LC_Y_Diaeresis : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (255); -- 'ÿ'

end Ada.Wide_Wide_Characters.Latin_1;
