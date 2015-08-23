pragma License (Unrestricted);
package Ada.Characters.Latin_1 is
   pragma Pure;

   --  Control characters:

   NUL : constant Character := Character'Val (0);
   SOH : constant Character := Character'Val (1);
   STX : constant Character := Character'Val (2);
   ETX : constant Character := Character'Val (3);
   EOT : constant Character := Character'Val (4);
   ENQ : constant Character := Character'Val (5);
   ACK : constant Character := Character'Val (6);
   BEL : constant Character := Character'Val (7);
   BS : constant Character := Character'Val (8);
   HT : constant Character := Character'Val (9);
   LF : constant Character := Character'Val (10);
   VT : constant Character := Character'Val (11);
   FF : constant Character := Character'Val (12);
   CR : constant Character := Character'Val (13);
   SO : constant Character := Character'Val (14);
   SI : constant Character := Character'Val (15);

   DLE : constant Character := Character'Val (16);
   DC1 : constant Character := Character'Val (17);
   DC2 : constant Character := Character'Val (18);
   DC3 : constant Character := Character'Val (19);
   DC4 : constant Character := Character'Val (20);
   NAK : constant Character := Character'Val (21);
   SYN : constant Character := Character'Val (22);
   ETB : constant Character := Character'Val (23);
   CAN : constant Character := Character'Val (24);
   EM : constant Character := Character'Val (25);
   SUB : constant Character := Character'Val (26);
   ESC : constant Character := Character'Val (27);
   FS : constant Character := Character'Val (28);
   GS : constant Character := Character'Val (29);
   RS : constant Character := Character'Val (30);
   US : constant Character := Character'Val (31);

   --  ISO 646 graphic characters:

   Space : constant Character := Character'Val (32); -- ' '
   Exclamation : constant Character := Character'Val (33); -- '!'
   Quotation : constant Character := Character'Val (34); -- '"'
   Number_Sign : constant Character := Character'Val (35); -- '#'
   Dollar_Sign : constant Character := Character'Val (36); -- '$'
   Percent_Sign : constant Character := Character'Val (37); -- '%'
   Ampersand : constant Character := Character'Val (38); -- '&'
   Apostrophe : constant Character := Character'Val (39); -- '''
   Left_Parenthesis : constant Character := Character'Val (40); -- '('
   Right_Parenthesis : constant Character := Character'Val (41); -- ')'
   Asterisk : constant Character := Character'Val (42); -- '*'
   Plus_Sign : constant Character := Character'Val (43); -- '+'
   Comma : constant Character := Character'Val (44); -- ','
   Hyphen : constant Character := Character'Val (45); -- '-'
   Minus_Sign : Character
      renames Hyphen;
   Full_Stop : constant Character := Character'Val (46); -- '.'
   Solidus : constant Character := Character'Val (47); -- '/'

   --  Decimal digits '0' though '9' are at positions 48 through 57

   Colon : constant Character := Character'Val (58); -- ':'
   Semicolon : constant Character := Character'Val (59); -- ';'
   Less_Than_Sign : constant Character := Character'Val (60); -- '<'
   Equals_Sign : constant Character := Character'Val (61); -- '='
   Greater_Than_Sign : constant Character := Character'Val (62); -- '>'
   Question : constant Character := Character'Val (63); -- '?'
   Commercial_At : constant Character := Character'Val (64); -- '@'

   --  Letters 'A' through 'Z' are at positions 65 through 90

   Left_Square_Bracket : constant Character := Character'Val (91); -- '['
   Reverse_Solidus : constant Character := Character'Val (92); -- '\'
   Right_Square_Bracket : constant Character := Character'Val (93); -- ']'
   Circumflex : constant Character := Character'Val (94); -- '^'
   Low_Line : constant Character := Character'Val (95); -- '_'

   Grave : constant Character := Character'Val (96); -- '`'
   LC_A : constant Character := Character'Val (97); -- 'a'
   LC_B : constant Character := Character'Val (98); -- 'b'
   LC_C : constant Character := Character'Val (99); -- 'c'
   LC_D : constant Character := Character'Val (100); -- 'd'
   LC_E : constant Character := Character'Val (101); -- 'e'
   LC_F : constant Character := Character'Val (102); -- 'f'
   LC_G : constant Character := Character'Val (103); -- 'g'
   LC_H : constant Character := Character'Val (104); -- 'h'
   LC_I : constant Character := Character'Val (105); -- 'i'
   LC_J : constant Character := Character'Val (106); -- 'j'
   LC_K : constant Character := Character'Val (107); -- 'k'
   LC_L : constant Character := Character'Val (108); -- 'l'
   LC_M : constant Character := Character'Val (109); -- 'm'
   LC_N : constant Character := Character'Val (110); -- 'n'
   LC_O : constant Character := Character'Val (111); -- 'o'

   LC_P : constant Character := Character'Val (112); -- 'p'
   LC_Q : constant Character := Character'Val (113); -- 'q'
   LC_R : constant Character := Character'Val (114); -- 'r'
   LC_S : constant Character := Character'Val (115); -- 's'
   LC_T : constant Character := Character'Val (116); -- 't'
   LC_U : constant Character := Character'Val (117); -- 'u'
   LC_V : constant Character := Character'Val (118); -- 'v'
   LC_W : constant Character := Character'Val (119); -- 'w'
   LC_X : constant Character := Character'Val (120); -- 'x'
   LC_Y : constant Character := Character'Val (121); -- 'y'
   LC_Z : constant Character := Character'Val (122); -- 'z'
   Left_Curly_Bracket : constant Character := Character'Val (123); -- '{'
   Vertical_Line : constant Character := Character'Val (124); -- '|'
   Right_Curly_Bracket : constant Character := Character'Val (125); -- '}'
   Tilde : constant Character := Character'Val (126); -- '~'
   DEL : constant Character := Character'Val (127);

   --  ISO 6429 control characters:

   IS4 : Character renames FS;
   IS3 : Character renames GS;
   IS2 : Character renames RS;
   IS1 : Character renames US;

   --  modified from here
   --  These constants are stored as UTF-8 as a String.

   Reserved_128 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#80#);
   Reserved_129 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#81#);
   BPH : constant String :=
      Character'Val (16#c2#) & Character'Val (16#82#);
   NBH : constant String :=
      Character'Val (16#c2#) & Character'Val (16#83#);
   Reserved_132 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#84#);
   NEL : constant String :=
      Character'Val (16#c2#) & Character'Val (16#85#);
   SSA : constant String :=
      Character'Val (16#c2#) & Character'Val (16#86#);
   ESA : constant String :=
      Character'Val (16#c2#) & Character'Val (16#87#);
   HTS : constant String :=
      Character'Val (16#c2#) & Character'Val (16#88#);
   HTJ : constant String :=
      Character'Val (16#c2#) & Character'Val (16#89#);
   VTS : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8a#);
   PLD : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8b#);
   PLU : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8c#);
   RI : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8d#);
   SS2 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8e#);
   SS3 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#8f#);

   DCS : constant String :=
      Character'Val (16#c2#) & Character'Val (16#90#);
   PU1 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#91#);
   PU2 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#92#);
   STS : constant String :=
      Character'Val (16#c2#) & Character'Val (16#93#);
   CCH : constant String :=
      Character'Val (16#c2#) & Character'Val (16#94#);
   MW : constant String :=
      Character'Val (16#c2#) & Character'Val (16#95#);
   SPA : constant String :=
      Character'Val (16#c2#) & Character'Val (16#96#);
   EPA : constant String :=
      Character'Val (16#c2#) & Character'Val (16#97#);

   SOS : constant String :=
      Character'Val (16#c2#) & Character'Val (16#98#);
   Reserved_153 : constant String :=
      Character'Val (16#c2#) & Character'Val (16#99#);
   SCI : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9a#);
   CSI : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9b#);
   ST : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9c#);
   OSC : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9d#);
   PM : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9e#);
   APC : constant String :=
      Character'Val (16#c2#) & Character'Val (16#9f#);

   --  Other graphic characters:

   --  Character positions 160 (16#A0#) .. 175 (16#AF#):
   No_Break_Space : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a0#); -- ' '
   NBSP : String
      renames No_Break_Space;
   Inverted_Exclamation : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a1#); -- '¡'
   Cent_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a2#); -- '¢'
   Pound_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a3#); -- '£'
   Currency_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a4#); -- '¤'
   Yen_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a5#); -- '¥'
   Broken_Bar : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a6#); -- '¦'
   Section_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a7#); -- '§'
   Diaeresis : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a8#); -- '¨'
   Copyright_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#a9#); -- '©'
   Feminine_Ordinal_Indicator : constant String :=
      Character'Val (16#c2#) & Character'Val (16#aa#); -- 'ª'
   Left_Angle_Quotation : constant String :=
      Character'Val (16#c2#) & Character'Val (16#ab#); -- '«'
   Not_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#ac#); -- '¬'
   Soft_Hyphen : constant String :=
      Character'Val (16#c2#) & Character'Val (16#ad#); -- ' '
   Registered_Trade_Mark_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#ae#); -- '®'
   Macron : constant String :=
      Character'Val (16#c2#) & Character'Val (16#af#); -- '¯'

   --  Character positions 176 (16#B0#) .. 191 (16#BF#):
   Degree_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b0#); -- '°'
   Ring_Above : String
      renames Degree_Sign;
   Plus_Minus_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b1#); -- '±'
   Superscript_Two : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b2#); -- '²'
   Superscript_Three : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b3#); -- '³'
   Acute : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b4#); -- '´'
   Micro_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b5#); -- 'µ'
   Pilcrow_Sign : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b6#); -- '¶'
   Paragraph_Sign : String
      renames Pilcrow_Sign;
   Middle_Dot : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b7#); -- '·'
   Cedilla : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b8#); -- '¸'
   Superscript_One : constant String :=
      Character'Val (16#c2#) & Character'Val (16#b9#); -- '¹'
   Masculine_Ordinal_Indicator : constant String :=
      Character'Val (16#c2#) & Character'Val (16#ba#); -- 'º'
   Right_Angle_Quotation : constant String :=
      Character'Val (16#c2#) & Character'Val (16#bb#); -- '»'
   Fraction_One_Quarter : constant String :=
      Character'Val (16#c2#) & Character'Val (16#bc#); -- '¼'
   Fraction_One_Half : constant String :=
      Character'Val (16#c2#) & Character'Val (16#bd#); -- '½'
   Fraction_Three_Quarters : constant String :=
      Character'Val (16#c2#) & Character'Val (16#be#); -- '¾'
   Inverted_Question : constant String :=
      Character'Val (16#c2#) & Character'Val (16#bf#); -- '¿'

   --  Character positions 192 (16#C0#) .. 207 (16#CF#):
   UC_A_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#80#); -- 'À'
   UC_A_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#81#); -- 'Á'
   UC_A_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#82#); -- 'Â'
   UC_A_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#83#); -- 'Ã'
   UC_A_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#84#); -- 'Ä'
   UC_A_Ring : constant String :=
      Character'Val (16#c3#) & Character'Val (16#85#); -- 'Å'
   UC_AE_Diphthong : constant String :=
      Character'Val (16#c3#) & Character'Val (16#86#); -- 'Æ'
   UC_C_Cedilla : constant String :=
      Character'Val (16#c3#) & Character'Val (16#87#); -- 'Ç'
   UC_E_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#88#); -- 'È'
   UC_E_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#89#); -- 'É'
   UC_E_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8a#); -- 'Ê'
   UC_E_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8b#); -- 'Ë'
   UC_I_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8c#); -- 'Ì'
   UC_I_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8d#); -- 'Í'
   UC_I_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8e#); -- 'Î'
   UC_I_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#8f#); -- 'Ï'

   --  Character positions 208 (16#D0#) .. 223 (16#DF#):
   UC_Icelandic_Eth : constant String :=
      Character'Val (16#c3#) & Character'Val (16#90#); -- 'Ð'
   UC_N_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#91#); -- 'Ñ'
   UC_O_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#92#); -- 'Ò'
   UC_O_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#93#); -- 'Ó'
   UC_O_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#94#); -- 'Ô'
   UC_O_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#95#); -- 'Õ'
   UC_O_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#96#); -- 'Ö'
   Multiplication_Sign : constant String :=
      Character'Val (16#c3#) & Character'Val (16#97#); -- '×'
   UC_O_Oblique_Stroke : constant String :=
      Character'Val (16#c3#) & Character'Val (16#98#); -- 'Ø'
   UC_U_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#99#); -- 'Ù'
   UC_U_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9a#); -- 'Ú'
   UC_U_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9b#); -- 'Û'
   UC_U_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9c#); -- 'Ü'
   UC_Y_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9d#); -- 'Ý'
   UC_Icelandic_Thorn : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9e#); -- 'Þ'
   LC_German_Sharp_S : constant String :=
      Character'Val (16#c3#) & Character'Val (16#9f#); -- 'ß'

   --  Character positions 224 (16#E0#) .. 239 (16#EF#):
   LC_A_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a0#); -- 'à'
   LC_A_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a1#); -- 'á'
   LC_A_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a2#); -- 'â'
   LC_A_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a3#); -- 'ã'
   LC_A_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a4#); -- 'ä'
   LC_A_Ring : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a5#); -- 'å'
   LC_AE_Diphthong : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a6#); -- 'æ'
   LC_C_Cedilla : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a7#); -- 'ç'
   LC_E_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a8#); -- 'è'
   LC_E_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#a9#); -- 'é'
   LC_E_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#aa#); -- 'ê'
   LC_E_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#ab#); -- 'ë'
   LC_I_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#ac#); -- 'ì'
   LC_I_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#ad#); -- 'í'
   LC_I_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#ae#); -- 'î'
   LC_I_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#af#); -- 'ï'

   --  Character positions 240 (16#F0#) .. 255 (16#FF#):
   LC_Icelandic_Eth : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b0#); -- 'ð'
   LC_N_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b1#); -- 'ñ'
   LC_O_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b2#); -- 'ò'
   LC_O_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b3#); -- 'ó'
   LC_O_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b4#); -- 'ô'
   LC_O_Tilde : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b5#); -- 'õ'
   LC_O_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b6#); -- 'ö'
   Division_Sign : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b7#); -- '÷';
   LC_O_Oblique_Stroke : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b8#); -- 'ø'
   LC_U_Grave : constant String :=
      Character'Val (16#c3#) & Character'Val (16#b9#); -- 'ù'
   LC_U_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#ba#); -- 'ú'
   LC_U_Circumflex : constant String :=
      Character'Val (16#c3#) & Character'Val (16#bb#); -- 'û';
   LC_U_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#bc#); -- 'ü'
   LC_Y_Acute : constant String :=
      Character'Val (16#c3#) & Character'Val (16#bd#); -- 'ý'
   LC_Icelandic_Thorn : constant String :=
      Character'Val (16#c3#) & Character'Val (16#be#); -- 'þ'
   LC_Y_Diaeresis : constant String :=
      Character'Val (16#c3#) & Character'Val (16#bf#); -- 'ÿ'

end Ada.Characters.Latin_1;
