pragma License (Unrestricted);
--  implementation unit, translated from UnicodeData.txt (13, 14)
package Ada.UCD.Simple_Case_Mapping is
   pragma Pure;

   L_Total : constant := 1043;
   U_Total : constant := 1051;

   type Run_Length_8 is mod 2 ** 8;

   type Compressed_Item_Type is record
      Start : UCS_2;
      Length : Run_Length_8;
      Diff : Difference_8;
   end record;
   pragma Suppress_Initialization (Compressed_Item_Type);
   pragma Pack (Compressed_Item_Type);
   pragma Compile_Time_Error (Compressed_Item_Type'Size /= 32, "packed?");

   type Compressed_Type is array (Positive range <>) of Compressed_Item_Type;
   pragma Suppress_Initialization (Compressed_Type);
   pragma Pack (Compressed_Type);
   pragma Compile_Time_Error (Compressed_Type'Component_Size /= 32, "packed?");

   subtype SL_Table_XXXX_Type is Map_16x1_Type (1 .. 617);
   subtype SL_Table_XXXX_Compressed_Type is Compressed_Type (1 .. 32);
   subtype SL_Table_1XXXX_Compressed_Type is Compressed_Type (1 .. 1);
   subtype DL_Table_XXXX_Type is Map_16x1_Type (1 .. 10);
   subtype DU_Table_XXXX_Type is Map_16x1_Type (1 .. 18);

   SL_Table_XXXX : constant SL_Table_XXXX_Type := (
      (16#0100#, 16#0101#),
      (16#0102#, 16#0103#),
      (16#0104#, 16#0105#),
      (16#0106#, 16#0107#),
      (16#0108#, 16#0109#),
      (16#010A#, 16#010B#),
      (16#010C#, 16#010D#),
      (16#010E#, 16#010F#),
      (16#0110#, 16#0111#),
      (16#0112#, 16#0113#),
      (16#0114#, 16#0115#),
      (16#0116#, 16#0117#),
      (16#0118#, 16#0119#),
      (16#011A#, 16#011B#),
      (16#011C#, 16#011D#),
      (16#011E#, 16#011F#),
      (16#0120#, 16#0121#),
      (16#0122#, 16#0123#),
      (16#0124#, 16#0125#),
      (16#0126#, 16#0127#),
      (16#0128#, 16#0129#),
      (16#012A#, 16#012B#),
      (16#012C#, 16#012D#),
      (16#012E#, 16#012F#),
      (16#0132#, 16#0133#),
      (16#0134#, 16#0135#),
      (16#0136#, 16#0137#),
      (16#0139#, 16#013A#),
      (16#013B#, 16#013C#),
      (16#013D#, 16#013E#),
      (16#013F#, 16#0140#),
      (16#0141#, 16#0142#),
      (16#0143#, 16#0144#),
      (16#0145#, 16#0146#),
      (16#0147#, 16#0148#),
      (16#014A#, 16#014B#),
      (16#014C#, 16#014D#),
      (16#014E#, 16#014F#),
      (16#0150#, 16#0151#),
      (16#0152#, 16#0153#),
      (16#0154#, 16#0155#),
      (16#0156#, 16#0157#),
      (16#0158#, 16#0159#),
      (16#015A#, 16#015B#),
      (16#015C#, 16#015D#),
      (16#015E#, 16#015F#),
      (16#0160#, 16#0161#),
      (16#0162#, 16#0163#),
      (16#0164#, 16#0165#),
      (16#0166#, 16#0167#),
      (16#0168#, 16#0169#),
      (16#016A#, 16#016B#),
      (16#016C#, 16#016D#),
      (16#016E#, 16#016F#),
      (16#0170#, 16#0171#),
      (16#0172#, 16#0173#),
      (16#0174#, 16#0175#),
      (16#0176#, 16#0177#),
      (16#0178#, 16#00FF#),
      (16#0179#, 16#017A#),
      (16#017B#, 16#017C#),
      (16#017D#, 16#017E#),
      (16#0181#, 16#0253#),
      (16#0182#, 16#0183#),
      (16#0184#, 16#0185#),
      (16#0186#, 16#0254#),
      (16#0187#, 16#0188#),
      (16#0189#, 16#0256#),
      (16#018A#, 16#0257#),
      (16#018B#, 16#018C#),
      (16#018E#, 16#01DD#),
      (16#018F#, 16#0259#),
      (16#0190#, 16#025B#),
      (16#0191#, 16#0192#),
      (16#0193#, 16#0260#),
      (16#0194#, 16#0263#),
      (16#0196#, 16#0269#),
      (16#0197#, 16#0268#),
      (16#0198#, 16#0199#),
      (16#019C#, 16#026F#),
      (16#019D#, 16#0272#),
      (16#019F#, 16#0275#),
      (16#01A0#, 16#01A1#),
      (16#01A2#, 16#01A3#),
      (16#01A4#, 16#01A5#),
      (16#01A6#, 16#0280#),
      (16#01A7#, 16#01A8#),
      (16#01A9#, 16#0283#),
      (16#01AC#, 16#01AD#),
      (16#01AE#, 16#0288#),
      (16#01AF#, 16#01B0#),
      (16#01B1#, 16#028A#),
      (16#01B2#, 16#028B#),
      (16#01B3#, 16#01B4#),
      (16#01B5#, 16#01B6#),
      (16#01B7#, 16#0292#),
      (16#01B8#, 16#01B9#),
      (16#01BC#, 16#01BD#),
      (16#01C4#, 16#01C6#),
      (16#01C7#, 16#01C9#),
      (16#01CA#, 16#01CC#),
      (16#01CD#, 16#01CE#),
      (16#01CF#, 16#01D0#),
      (16#01D1#, 16#01D2#),
      (16#01D3#, 16#01D4#),
      (16#01D5#, 16#01D6#),
      (16#01D7#, 16#01D8#),
      (16#01D9#, 16#01DA#),
      (16#01DB#, 16#01DC#),
      (16#01DE#, 16#01DF#),
      (16#01E0#, 16#01E1#),
      (16#01E2#, 16#01E3#),
      (16#01E4#, 16#01E5#),
      (16#01E6#, 16#01E7#),
      (16#01E8#, 16#01E9#),
      (16#01EA#, 16#01EB#),
      (16#01EC#, 16#01ED#),
      (16#01EE#, 16#01EF#),
      (16#01F1#, 16#01F3#),
      (16#01F4#, 16#01F5#),
      (16#01F6#, 16#0195#),
      (16#01F7#, 16#01BF#),
      (16#01F8#, 16#01F9#),
      (16#01FA#, 16#01FB#),
      (16#01FC#, 16#01FD#),
      (16#01FE#, 16#01FF#),
      (16#0200#, 16#0201#),
      (16#0202#, 16#0203#),
      (16#0204#, 16#0205#),
      (16#0206#, 16#0207#),
      (16#0208#, 16#0209#),
      (16#020A#, 16#020B#),
      (16#020C#, 16#020D#),
      (16#020E#, 16#020F#),
      (16#0210#, 16#0211#),
      (16#0212#, 16#0213#),
      (16#0214#, 16#0215#),
      (16#0216#, 16#0217#),
      (16#0218#, 16#0219#),
      (16#021A#, 16#021B#),
      (16#021C#, 16#021D#),
      (16#021E#, 16#021F#),
      (16#0220#, 16#019E#),
      (16#0222#, 16#0223#),
      (16#0224#, 16#0225#),
      (16#0226#, 16#0227#),
      (16#0228#, 16#0229#),
      (16#022A#, 16#022B#),
      (16#022C#, 16#022D#),
      (16#022E#, 16#022F#),
      (16#0230#, 16#0231#),
      (16#0232#, 16#0233#),
      (16#023A#, 16#2C65#),
      (16#023B#, 16#023C#),
      (16#023D#, 16#019A#),
      (16#023E#, 16#2C66#),
      (16#0241#, 16#0242#),
      (16#0243#, 16#0180#),
      (16#0244#, 16#0289#),
      (16#0245#, 16#028C#),
      (16#0246#, 16#0247#),
      (16#0248#, 16#0249#),
      (16#024A#, 16#024B#),
      (16#024C#, 16#024D#),
      (16#024E#, 16#024F#),
      (16#0370#, 16#0371#),
      (16#0372#, 16#0373#),
      (16#0376#, 16#0377#),
      (16#0386#, 16#03AC#),
      (16#038C#, 16#03CC#),
      (16#03CF#, 16#03D7#),
      (16#03D8#, 16#03D9#),
      (16#03DA#, 16#03DB#),
      (16#03DC#, 16#03DD#),
      (16#03DE#, 16#03DF#),
      (16#03E0#, 16#03E1#),
      (16#03E2#, 16#03E3#),
      (16#03E4#, 16#03E5#),
      (16#03E6#, 16#03E7#),
      (16#03E8#, 16#03E9#),
      (16#03EA#, 16#03EB#),
      (16#03EC#, 16#03ED#),
      (16#03EE#, 16#03EF#),
      (16#03F7#, 16#03F8#),
      (16#03F9#, 16#03F2#),
      (16#03FA#, 16#03FB#),
      (16#03FD#, 16#037B#),
      (16#03FE#, 16#037C#),
      (16#03FF#, 16#037D#),
      (16#0460#, 16#0461#),
      (16#0462#, 16#0463#),
      (16#0464#, 16#0465#),
      (16#0466#, 16#0467#),
      (16#0468#, 16#0469#),
      (16#046A#, 16#046B#),
      (16#046C#, 16#046D#),
      (16#046E#, 16#046F#),
      (16#0470#, 16#0471#),
      (16#0472#, 16#0473#),
      (16#0474#, 16#0475#),
      (16#0476#, 16#0477#),
      (16#0478#, 16#0479#),
      (16#047A#, 16#047B#),
      (16#047C#, 16#047D#),
      (16#047E#, 16#047F#),
      (16#0480#, 16#0481#),
      (16#048A#, 16#048B#),
      (16#048C#, 16#048D#),
      (16#048E#, 16#048F#),
      (16#0490#, 16#0491#),
      (16#0492#, 16#0493#),
      (16#0494#, 16#0495#),
      (16#0496#, 16#0497#),
      (16#0498#, 16#0499#),
      (16#049A#, 16#049B#),
      (16#049C#, 16#049D#),
      (16#049E#, 16#049F#),
      (16#04A0#, 16#04A1#),
      (16#04A2#, 16#04A3#),
      (16#04A4#, 16#04A5#),
      (16#04A6#, 16#04A7#),
      (16#04A8#, 16#04A9#),
      (16#04AA#, 16#04AB#),
      (16#04AC#, 16#04AD#),
      (16#04AE#, 16#04AF#),
      (16#04B0#, 16#04B1#),
      (16#04B2#, 16#04B3#),
      (16#04B4#, 16#04B5#),
      (16#04B6#, 16#04B7#),
      (16#04B8#, 16#04B9#),
      (16#04BA#, 16#04BB#),
      (16#04BC#, 16#04BD#),
      (16#04BE#, 16#04BF#),
      (16#04C0#, 16#04CF#),
      (16#04C1#, 16#04C2#),
      (16#04C3#, 16#04C4#),
      (16#04C5#, 16#04C6#),
      (16#04C7#, 16#04C8#),
      (16#04C9#, 16#04CA#),
      (16#04CB#, 16#04CC#),
      (16#04CD#, 16#04CE#),
      (16#04D0#, 16#04D1#),
      (16#04D2#, 16#04D3#),
      (16#04D4#, 16#04D5#),
      (16#04D6#, 16#04D7#),
      (16#04D8#, 16#04D9#),
      (16#04DA#, 16#04DB#),
      (16#04DC#, 16#04DD#),
      (16#04DE#, 16#04DF#),
      (16#04E0#, 16#04E1#),
      (16#04E2#, 16#04E3#),
      (16#04E4#, 16#04E5#),
      (16#04E6#, 16#04E7#),
      (16#04E8#, 16#04E9#),
      (16#04EA#, 16#04EB#),
      (16#04EC#, 16#04ED#),
      (16#04EE#, 16#04EF#),
      (16#04F0#, 16#04F1#),
      (16#04F2#, 16#04F3#),
      (16#04F4#, 16#04F5#),
      (16#04F6#, 16#04F7#),
      (16#04F8#, 16#04F9#),
      (16#04FA#, 16#04FB#),
      (16#04FC#, 16#04FD#),
      (16#04FE#, 16#04FF#),
      (16#0500#, 16#0501#),
      (16#0502#, 16#0503#),
      (16#0504#, 16#0505#),
      (16#0506#, 16#0507#),
      (16#0508#, 16#0509#),
      (16#050A#, 16#050B#),
      (16#050C#, 16#050D#),
      (16#050E#, 16#050F#),
      (16#0510#, 16#0511#),
      (16#0512#, 16#0513#),
      (16#0514#, 16#0515#),
      (16#0516#, 16#0517#),
      (16#0518#, 16#0519#),
      (16#051A#, 16#051B#),
      (16#051C#, 16#051D#),
      (16#051E#, 16#051F#),
      (16#0520#, 16#0521#),
      (16#0522#, 16#0523#),
      (16#0524#, 16#0525#),
      (16#0526#, 16#0527#),
      (16#10A0#, 16#2D00#),
      (16#10A1#, 16#2D01#),
      (16#10A2#, 16#2D02#),
      (16#10A3#, 16#2D03#),
      (16#10A4#, 16#2D04#),
      (16#10A5#, 16#2D05#),
      (16#10A6#, 16#2D06#),
      (16#10A7#, 16#2D07#),
      (16#10A8#, 16#2D08#),
      (16#10A9#, 16#2D09#),
      (16#10AA#, 16#2D0A#),
      (16#10AB#, 16#2D0B#),
      (16#10AC#, 16#2D0C#),
      (16#10AD#, 16#2D0D#),
      (16#10AE#, 16#2D0E#),
      (16#10AF#, 16#2D0F#),
      (16#10B0#, 16#2D10#),
      (16#10B1#, 16#2D11#),
      (16#10B2#, 16#2D12#),
      (16#10B3#, 16#2D13#),
      (16#10B4#, 16#2D14#),
      (16#10B5#, 16#2D15#),
      (16#10B6#, 16#2D16#),
      (16#10B7#, 16#2D17#),
      (16#10B8#, 16#2D18#),
      (16#10B9#, 16#2D19#),
      (16#10BA#, 16#2D1A#),
      (16#10BB#, 16#2D1B#),
      (16#10BC#, 16#2D1C#),
      (16#10BD#, 16#2D1D#),
      (16#10BE#, 16#2D1E#),
      (16#10BF#, 16#2D1F#),
      (16#10C0#, 16#2D20#),
      (16#10C1#, 16#2D21#),
      (16#10C2#, 16#2D22#),
      (16#10C3#, 16#2D23#),
      (16#10C4#, 16#2D24#),
      (16#10C5#, 16#2D25#),
      (16#10C7#, 16#2D27#),
      (16#10CD#, 16#2D2D#),
      (16#1E00#, 16#1E01#),
      (16#1E02#, 16#1E03#),
      (16#1E04#, 16#1E05#),
      (16#1E06#, 16#1E07#),
      (16#1E08#, 16#1E09#),
      (16#1E0A#, 16#1E0B#),
      (16#1E0C#, 16#1E0D#),
      (16#1E0E#, 16#1E0F#),
      (16#1E10#, 16#1E11#),
      (16#1E12#, 16#1E13#),
      (16#1E14#, 16#1E15#),
      (16#1E16#, 16#1E17#),
      (16#1E18#, 16#1E19#),
      (16#1E1A#, 16#1E1B#),
      (16#1E1C#, 16#1E1D#),
      (16#1E1E#, 16#1E1F#),
      (16#1E20#, 16#1E21#),
      (16#1E22#, 16#1E23#),
      (16#1E24#, 16#1E25#),
      (16#1E26#, 16#1E27#),
      (16#1E28#, 16#1E29#),
      (16#1E2A#, 16#1E2B#),
      (16#1E2C#, 16#1E2D#),
      (16#1E2E#, 16#1E2F#),
      (16#1E30#, 16#1E31#),
      (16#1E32#, 16#1E33#),
      (16#1E34#, 16#1E35#),
      (16#1E36#, 16#1E37#),
      (16#1E38#, 16#1E39#),
      (16#1E3A#, 16#1E3B#),
      (16#1E3C#, 16#1E3D#),
      (16#1E3E#, 16#1E3F#),
      (16#1E40#, 16#1E41#),
      (16#1E42#, 16#1E43#),
      (16#1E44#, 16#1E45#),
      (16#1E46#, 16#1E47#),
      (16#1E48#, 16#1E49#),
      (16#1E4A#, 16#1E4B#),
      (16#1E4C#, 16#1E4D#),
      (16#1E4E#, 16#1E4F#),
      (16#1E50#, 16#1E51#),
      (16#1E52#, 16#1E53#),
      (16#1E54#, 16#1E55#),
      (16#1E56#, 16#1E57#),
      (16#1E58#, 16#1E59#),
      (16#1E5A#, 16#1E5B#),
      (16#1E5C#, 16#1E5D#),
      (16#1E5E#, 16#1E5F#),
      (16#1E60#, 16#1E61#),
      (16#1E62#, 16#1E63#),
      (16#1E64#, 16#1E65#),
      (16#1E66#, 16#1E67#),
      (16#1E68#, 16#1E69#),
      (16#1E6A#, 16#1E6B#),
      (16#1E6C#, 16#1E6D#),
      (16#1E6E#, 16#1E6F#),
      (16#1E70#, 16#1E71#),
      (16#1E72#, 16#1E73#),
      (16#1E74#, 16#1E75#),
      (16#1E76#, 16#1E77#),
      (16#1E78#, 16#1E79#),
      (16#1E7A#, 16#1E7B#),
      (16#1E7C#, 16#1E7D#),
      (16#1E7E#, 16#1E7F#),
      (16#1E80#, 16#1E81#),
      (16#1E82#, 16#1E83#),
      (16#1E84#, 16#1E85#),
      (16#1E86#, 16#1E87#),
      (16#1E88#, 16#1E89#),
      (16#1E8A#, 16#1E8B#),
      (16#1E8C#, 16#1E8D#),
      (16#1E8E#, 16#1E8F#),
      (16#1E90#, 16#1E91#),
      (16#1E92#, 16#1E93#),
      (16#1E94#, 16#1E95#),
      (16#1EA0#, 16#1EA1#),
      (16#1EA2#, 16#1EA3#),
      (16#1EA4#, 16#1EA5#),
      (16#1EA6#, 16#1EA7#),
      (16#1EA8#, 16#1EA9#),
      (16#1EAA#, 16#1EAB#),
      (16#1EAC#, 16#1EAD#),
      (16#1EAE#, 16#1EAF#),
      (16#1EB0#, 16#1EB1#),
      (16#1EB2#, 16#1EB3#),
      (16#1EB4#, 16#1EB5#),
      (16#1EB6#, 16#1EB7#),
      (16#1EB8#, 16#1EB9#),
      (16#1EBA#, 16#1EBB#),
      (16#1EBC#, 16#1EBD#),
      (16#1EBE#, 16#1EBF#),
      (16#1EC0#, 16#1EC1#),
      (16#1EC2#, 16#1EC3#),
      (16#1EC4#, 16#1EC5#),
      (16#1EC6#, 16#1EC7#),
      (16#1EC8#, 16#1EC9#),
      (16#1ECA#, 16#1ECB#),
      (16#1ECC#, 16#1ECD#),
      (16#1ECE#, 16#1ECF#),
      (16#1ED0#, 16#1ED1#),
      (16#1ED2#, 16#1ED3#),
      (16#1ED4#, 16#1ED5#),
      (16#1ED6#, 16#1ED7#),
      (16#1ED8#, 16#1ED9#),
      (16#1EDA#, 16#1EDB#),
      (16#1EDC#, 16#1EDD#),
      (16#1EDE#, 16#1EDF#),
      (16#1EE0#, 16#1EE1#),
      (16#1EE2#, 16#1EE3#),
      (16#1EE4#, 16#1EE5#),
      (16#1EE6#, 16#1EE7#),
      (16#1EE8#, 16#1EE9#),
      (16#1EEA#, 16#1EEB#),
      (16#1EEC#, 16#1EED#),
      (16#1EEE#, 16#1EEF#),
      (16#1EF0#, 16#1EF1#),
      (16#1EF2#, 16#1EF3#),
      (16#1EF4#, 16#1EF5#),
      (16#1EF6#, 16#1EF7#),
      (16#1EF8#, 16#1EF9#),
      (16#1EFA#, 16#1EFB#),
      (16#1EFC#, 16#1EFD#),
      (16#1EFE#, 16#1EFF#),
      (16#1F59#, 16#1F51#),
      (16#1F5B#, 16#1F53#),
      (16#1F5D#, 16#1F55#),
      (16#1F5F#, 16#1F57#),
      (16#1FBC#, 16#1FB3#),
      (16#1FCC#, 16#1FC3#),
      (16#1FEC#, 16#1FE5#),
      (16#1FFC#, 16#1FF3#),
      (16#2132#, 16#214E#),
      (16#2183#, 16#2184#),
      (16#2C60#, 16#2C61#),
      (16#2C62#, 16#026B#),
      (16#2C63#, 16#1D7D#),
      (16#2C64#, 16#027D#),
      (16#2C67#, 16#2C68#),
      (16#2C69#, 16#2C6A#),
      (16#2C6B#, 16#2C6C#),
      (16#2C6D#, 16#0251#),
      (16#2C6E#, 16#0271#),
      (16#2C6F#, 16#0250#),
      (16#2C70#, 16#0252#),
      (16#2C72#, 16#2C73#),
      (16#2C75#, 16#2C76#),
      (16#2C7E#, 16#023F#),
      (16#2C7F#, 16#0240#),
      (16#2C80#, 16#2C81#),
      (16#2C82#, 16#2C83#),
      (16#2C84#, 16#2C85#),
      (16#2C86#, 16#2C87#),
      (16#2C88#, 16#2C89#),
      (16#2C8A#, 16#2C8B#),
      (16#2C8C#, 16#2C8D#),
      (16#2C8E#, 16#2C8F#),
      (16#2C90#, 16#2C91#),
      (16#2C92#, 16#2C93#),
      (16#2C94#, 16#2C95#),
      (16#2C96#, 16#2C97#),
      (16#2C98#, 16#2C99#),
      (16#2C9A#, 16#2C9B#),
      (16#2C9C#, 16#2C9D#),
      (16#2C9E#, 16#2C9F#),
      (16#2CA0#, 16#2CA1#),
      (16#2CA2#, 16#2CA3#),
      (16#2CA4#, 16#2CA5#),
      (16#2CA6#, 16#2CA7#),
      (16#2CA8#, 16#2CA9#),
      (16#2CAA#, 16#2CAB#),
      (16#2CAC#, 16#2CAD#),
      (16#2CAE#, 16#2CAF#),
      (16#2CB0#, 16#2CB1#),
      (16#2CB2#, 16#2CB3#),
      (16#2CB4#, 16#2CB5#),
      (16#2CB6#, 16#2CB7#),
      (16#2CB8#, 16#2CB9#),
      (16#2CBA#, 16#2CBB#),
      (16#2CBC#, 16#2CBD#),
      (16#2CBE#, 16#2CBF#),
      (16#2CC0#, 16#2CC1#),
      (16#2CC2#, 16#2CC3#),
      (16#2CC4#, 16#2CC5#),
      (16#2CC6#, 16#2CC7#),
      (16#2CC8#, 16#2CC9#),
      (16#2CCA#, 16#2CCB#),
      (16#2CCC#, 16#2CCD#),
      (16#2CCE#, 16#2CCF#),
      (16#2CD0#, 16#2CD1#),
      (16#2CD2#, 16#2CD3#),
      (16#2CD4#, 16#2CD5#),
      (16#2CD6#, 16#2CD7#),
      (16#2CD8#, 16#2CD9#),
      (16#2CDA#, 16#2CDB#),
      (16#2CDC#, 16#2CDD#),
      (16#2CDE#, 16#2CDF#),
      (16#2CE0#, 16#2CE1#),
      (16#2CE2#, 16#2CE3#),
      (16#2CEB#, 16#2CEC#),
      (16#2CED#, 16#2CEE#),
      (16#2CF2#, 16#2CF3#),
      (16#A640#, 16#A641#),
      (16#A642#, 16#A643#),
      (16#A644#, 16#A645#),
      (16#A646#, 16#A647#),
      (16#A648#, 16#A649#),
      (16#A64A#, 16#A64B#),
      (16#A64C#, 16#A64D#),
      (16#A64E#, 16#A64F#),
      (16#A650#, 16#A651#),
      (16#A652#, 16#A653#),
      (16#A654#, 16#A655#),
      (16#A656#, 16#A657#),
      (16#A658#, 16#A659#),
      (16#A65A#, 16#A65B#),
      (16#A65C#, 16#A65D#),
      (16#A65E#, 16#A65F#),
      (16#A660#, 16#A661#),
      (16#A662#, 16#A663#),
      (16#A664#, 16#A665#),
      (16#A666#, 16#A667#),
      (16#A668#, 16#A669#),
      (16#A66A#, 16#A66B#),
      (16#A66C#, 16#A66D#),
      (16#A680#, 16#A681#),
      (16#A682#, 16#A683#),
      (16#A684#, 16#A685#),
      (16#A686#, 16#A687#),
      (16#A688#, 16#A689#),
      (16#A68A#, 16#A68B#),
      (16#A68C#, 16#A68D#),
      (16#A68E#, 16#A68F#),
      (16#A690#, 16#A691#),
      (16#A692#, 16#A693#),
      (16#A694#, 16#A695#),
      (16#A696#, 16#A697#),
      (16#A722#, 16#A723#),
      (16#A724#, 16#A725#),
      (16#A726#, 16#A727#),
      (16#A728#, 16#A729#),
      (16#A72A#, 16#A72B#),
      (16#A72C#, 16#A72D#),
      (16#A72E#, 16#A72F#),
      (16#A732#, 16#A733#),
      (16#A734#, 16#A735#),
      (16#A736#, 16#A737#),
      (16#A738#, 16#A739#),
      (16#A73A#, 16#A73B#),
      (16#A73C#, 16#A73D#),
      (16#A73E#, 16#A73F#),
      (16#A740#, 16#A741#),
      (16#A742#, 16#A743#),
      (16#A744#, 16#A745#),
      (16#A746#, 16#A747#),
      (16#A748#, 16#A749#),
      (16#A74A#, 16#A74B#),
      (16#A74C#, 16#A74D#),
      (16#A74E#, 16#A74F#),
      (16#A750#, 16#A751#),
      (16#A752#, 16#A753#),
      (16#A754#, 16#A755#),
      (16#A756#, 16#A757#),
      (16#A758#, 16#A759#),
      (16#A75A#, 16#A75B#),
      (16#A75C#, 16#A75D#),
      (16#A75E#, 16#A75F#),
      (16#A760#, 16#A761#),
      (16#A762#, 16#A763#),
      (16#A764#, 16#A765#),
      (16#A766#, 16#A767#),
      (16#A768#, 16#A769#),
      (16#A76A#, 16#A76B#),
      (16#A76C#, 16#A76D#),
      (16#A76E#, 16#A76F#),
      (16#A779#, 16#A77A#),
      (16#A77B#, 16#A77C#),
      (16#A77D#, 16#1D79#),
      (16#A77E#, 16#A77F#),
      (16#A780#, 16#A781#),
      (16#A782#, 16#A783#),
      (16#A784#, 16#A785#),
      (16#A786#, 16#A787#),
      (16#A78B#, 16#A78C#),
      (16#A78D#, 16#0265#),
      (16#A790#, 16#A791#),
      (16#A792#, 16#A793#),
      (16#A7A0#, 16#A7A1#),
      (16#A7A2#, 16#A7A3#),
      (16#A7A4#, 16#A7A5#),
      (16#A7A6#, 16#A7A7#),
      (16#A7A8#, 16#A7A9#),
      (16#A7AA#, 16#0266#));

   SL_Table_XXXX_Compressed : constant SL_Table_XXXX_Compressed_Type := (
      (16#0041#, 26, 32),
      (16#00C0#, 23, 32),
      (16#00D8#, 7, 32),
      (16#0388#, 3, 37),
      (16#038E#, 2, 63),
      (16#0391#, 17, 32),
      (16#03A3#, 9, 32),
      (16#0400#, 16, 80),
      (16#0410#, 32, 32),
      (16#0531#, 38, 48),
      (16#1F08#, 8, -8),
      (16#1F18#, 6, -8),
      (16#1F28#, 8, -8),
      (16#1F38#, 8, -8),
      (16#1F48#, 6, -8),
      (16#1F68#, 8, -8),
      (16#1F88#, 8, -8),
      (16#1F98#, 8, -8),
      (16#1FA8#, 8, -8),
      (16#1FB8#, 2, -8),
      (16#1FBA#, 2, -74),
      (16#1FC8#, 4, -86),
      (16#1FD8#, 2, -8),
      (16#1FDA#, 2, -100),
      (16#1FE8#, 2, -8),
      (16#1FEA#, 2, -112),
      (16#1FF8#, 2, -128),
      (16#1FFA#, 2, -126),
      (16#2160#, 16, 16),
      (16#24B6#, 26, 26),
      (16#2C00#, 47, 48),
      (16#FF21#, 26, 32));

   SL_Table_1XXXX_Compressed : constant SL_Table_1XXXX_Compressed_Type := (
      1 => (16#0400#, 40, 40));

   DL_Table_XXXX : constant DL_Table_XXXX_Type := (
      (16#0130#, 16#0069#),
      (16#01C5#, 16#01C6#),
      (16#01C8#, 16#01C9#),
      (16#01CB#, 16#01CC#),
      (16#01F2#, 16#01F3#),
      (16#03F4#, 16#03B8#),
      (16#1E9E#, 16#00DF#),
      (16#2126#, 16#03C9#),
      (16#212A#, 16#006B#),
      (16#212B#, 16#00E5#));

   DU_Table_XXXX : constant DU_Table_XXXX_Type := (
      (16#00B5#, 16#039C#),
      (16#0131#, 16#0049#),
      (16#017F#, 16#0053#),
      (16#01C5#, 16#01C4#),
      (16#01C8#, 16#01C7#),
      (16#01CB#, 16#01CA#),
      (16#01F2#, 16#01F1#),
      (16#0345#, 16#0399#),
      (16#03C2#, 16#03A3#),
      (16#03D0#, 16#0392#),
      (16#03D1#, 16#0398#),
      (16#03D5#, 16#03A6#),
      (16#03D6#, 16#03A0#),
      (16#03F0#, 16#039A#),
      (16#03F1#, 16#03A1#),
      (16#03F5#, 16#0395#),
      (16#1E9B#, 16#1E60#),
      (16#1FBE#, 16#0399#));

end Ada.UCD.Simple_Case_Mapping;
