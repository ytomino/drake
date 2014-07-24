--  reference:
--  http://www.loc.gov/standards/iso639-2/php/code_list.php
pragma Check_Policy (Validate, Off);
with Ada.Locales.Inside;
with System.Once;
package body Ada.Locales is
   pragma Suppress (All_Checks);

   type Alpha_2_NP is new String (1 .. 2); -- no predicate
   type Alpha_3_NP is new String (1 .. 3);

   function Compare (Left, Right : Alpha_2_NP) return Integer;
   function Compare (Left, Right : Alpha_2_NP) return Integer is
      type Compare_Integer is mod 16#10000#;
   begin
      return
         Integer (Compare_Integer'(
            Character'Pos (Left (1)) * 16#100#
            or Character'Pos (Left (2))))
         - Integer (Compare_Integer'(
            Character'Pos (Right (1)) * 16#100#
            or Character'Pos (Right (2))));
   end Compare;

   function Compare (Left, Right : Alpha_3_NP) return Integer;
   function Compare (Left, Right : Alpha_3_NP) return Integer is
      type Compare_Integer is mod 16#1000000#;
   begin
      return
         Integer (Compare_Integer'(
            Character'Pos (Left (1)) * 16#10000#
            or Character'Pos (Left (2)) * 16#100#
            or Character'Pos (Left (3))))
         - Integer (Compare_Integer'(
            Character'Pos (Right (1)) * 16#10000#
            or Character'Pos (Right (2)) * 16#100#
            or Character'Pos (Right (3))));
   end Compare;

   type Language_Table_Element is record
      Alpha_3 : Alpha_3_NP;
      Alpha_2 : Alpha_2_NP;
   end record;
   pragma Suppress_Initialization (Language_Table_Element);
   type Language_Table_Array is
      array (Positive range <>) of Language_Table_Element;
   pragma Suppress_Initialization (Language_Table_Array);

   unde : constant Alpha_2_NP := (
      1 => ISO_639_Alpha_2_Unknown (1),
      2 => ISO_639_Alpha_2_Unknown (2));

   Language_Table : constant Language_Table_Array := (
      ("aar", "aa"), --  "Afar", "afar"
      ("abk", "ab"), --  "Abkhazian", "abkhaze"
      ("ace", unde), --  "Achinese", "aceh"
      ("ach", unde), --  "Acoli", "acoli"
      ("ada", unde), --  "Adangme", "adangme"
      ("ady", unde), --  "Adyghe; Adygei", "adyghé"
      ("afa", unde), --  "Afro-Asiatic languages", "afro-asiatiques, langues"
      ("afh", unde), --  "Afrihili", "afrihili"
      ("afr", "af"), --  "Afrikaans", "afrikaans"
      ("ain", unde), --  "Ainu", "aïnou"
      ("aka", "ak"), --  "Akan", "akan"
      ("akk", unde), --  "Akkadian", "akkadien"
      ("alb", "sq"), -- (B)
      ("sqi", "sq"), -- (T) "Albanian", "albanais"
      ("ale", unde), --  "Aleut", "aléoute"
      ("alg", unde), --  "Algonquian languages", "algonquines, langues"
      ("alt", unde), --  "Southern Altai", "altai du Sud"
      ("amh", "am"), --  "Amharic", "amharique"
      ("ang", unde), --  "English, Old (ca.450-1100)",
                     --  "anglo-saxon (ca.450-1100)"
      ("anp", unde), --  "Angika", "angika"
      ("apa", unde), --  "Apache languages", "apaches, langues"
      ("ara", "ar"), --  "Arabic", "arabe"
      ("arc", unde), --  "Official Aramaic (700-300 BCE); "
                     --  & "Imperial Aramaic (700-300 BCE)",
                     --  "araméen d'empire (700-300 BCE)"
      ("arg", "an"), --  "Aragonese", "aragonais"
      ("arm", "hy"), -- (B)
      ("hye", "hy"), -- (T) "Armenian", "arménien"
      ("arn", unde), --  "Mapudungun; Mapuche", "mapudungun; mapuche; mapuce"
      ("arp", unde), --  "Arapaho", "arapaho"
      ("art", unde), --  "Artificial languages", "artificielles, langues"
      ("arw", unde), --  "Arawak", "arawak"
      ("asm", "as"), --  "Assamese", "assamais"
      ("ast", unde), --  "Asturian; Bable; Leonese; Asturleonese",
                     --  "asturien; bable; léonais; asturoléonais"
      ("ath", unde), --  "Athapascan languages", "athapascanes, langues"
      ("aus", unde), --  "Australian languages", "australiennes, langues"
      ("ava", "av"), --  "Avaric", "avar"
      ("ave", "ae"), --  "Avestan", "avestique"
      ("awa", unde), --  "Awadhi", "awadhi"
      ("aym", "ay"), --  "Aymara", "aymara"
      ("aze", "az"), --  "Azerbaijani", "azéri"
      ("bad", unde), --  "Banda languages", "banda, langues"
      ("bai", unde), --  "Bamileke languages", "bamiléké, langues"
      ("bak", "ba"), --  "Bashkir", "bachkir"
      ("bal", unde), --  "Baluchi", "baloutchi"
      ("bam", "bm"), --  "Bambara", "bambara"
      ("ban", unde), --  "Balinese", "balinais"
      ("baq", "eu"), -- (B)
      ("eus", "eu"), -- (T) "Basque", "basque"
      ("bas", unde), --  "Basa", "basa"
      ("bat", unde), --  "Baltic languages", "baltes, langues"
      ("bej", unde), --  "Beja; Bedawiyet", "bedja"
      ("bel", "be"), --  "Belarusian", "biélorusse"
      ("bem", unde), --  "Bemba", "bemba"
      ("ben", "bn"), --  "Bengali", "bengali"
      ("ber", unde), --  "Berber languages", "berbères, langues"
      ("bho", unde), --  "Bhojpuri", "bhojpuri"
      ("bih", "bh"), --  "Bihari languages", "langues biharis"
      ("bik", unde), --  "Bikol", "bikol"
      ("bin", unde), --  "Bini; Edo", "bini; edo"
      ("bis", "bi"), --  "Bislama", "bichlamar"
      ("bla", unde), --  "Siksika", "blackfoot"
      ("bnt", unde), --  "Bantu languages", "bantou, langues"
      ("tib", "bo"), -- (B)
      ("bod", "bo"), -- (T) "Tibetan", "tibétain"
      ("bos", "bs"), --  "Bosnian", "bosniaque"
      ("bra", unde), --  "Braj", "braj"
      ("bre", "br"), --  "Breton", "breton"
      ("btk", unde), --  "Batak languages", "batak, langues"
      ("bua", unde), --  "Buriat", "bouriate"
      ("bug", unde), --  "Buginese", "bugi"
      ("bul", "bg"), --  "Bulgarian", "bulgare"
      ("bur", "my"), -- (B)
      ("mya", "my"), -- (T) "Burmese", "birman"
      ("byn", unde), --  "Blin; Bilin", "blin; bilen"
      ("cad", unde), --  "Caddo", "caddo"
      ("cai", unde), --  "Central American Indian languages",
                     --  "amérindiennes de l'Amérique centrale, langues"
      ("car", unde), --  "Galibi Carib", "karib; galibi; carib"
      ("cat", "ca"), --  "Catalan; Valencian", "catalan; valencien"
      ("cau", unde), --  "Caucasian languages", "caucasiennes, langues"
      ("ceb", unde), --  "Cebuano", "cebuano"
      ("cel", unde), --  "Celtic languages",
                     --  "celtiques, langues; celtes, langues"
      ("cze", "cs"), -- (B)
      ("ces", "cs"), -- (T) "Czech", "tchèque"
      ("cha", "ch"), --  "Chamorro", "chamorro"
      ("chb", unde), --  "Chibcha", "chibcha"
      ("che", "ce"), --  "Chechen", "tchétchène"
      ("chg", unde), --  "Chagatai", "djaghataï"
      ("chi", "zh"), -- (B)
      ("zho", "zh"), -- (T) "Chinese", "chinois"
      ("chk", unde), --  "Chuukese", "chuuk"
      ("chm", unde), --  "Mari", "mari"
      ("chn", unde), --  "Chinook jargon", "chinook, jargon"
      ("cho", unde), --  "Choctaw", "choctaw"
      ("chp", unde), --  "Chipewyan; Dene Suline", "chipewyan"
      ("chr", unde), --  "Cherokee", "cherokee"
      ("chu", "cu"), --  "Church Slavic; Old Slavonic; Church Slavonic; "
                     --  & "Old Bulgarian; Old Church Slavonic",
                     --  "slavon d'église; vieux slave; slavon liturgique; "
                     --  & "vieux bulgare"
      ("chv", "cv"), --  "Chuvash", "tchouvache"
      ("chy", unde), --  "Cheyenne", "cheyenne"
      ("cmc", unde), --  "Chamic languages", "chames, langues"
      ("cop", unde), --  "Coptic", "copte"
      ("cor", "kw"), --  "Cornish", "cornique"
      ("cos", "co"), --  "Corsican", "corse"
      ("cpe", unde), --  "Creoles and pidgins, English based",
                     --  "créoles et pidgins basés sur l'anglais"
      ("cpf", unde), --  "Creoles and pidgins, French-based",
                     --  "créoles et pidgins basés sur le français"
      ("cpp", unde), --  "Creoles and pidgins, Portuguese-based",
                     --  "créoles et pidgins basés sur le portugais"
      ("cre", "cr"), --  "Cree", "cree"
      ("crh", unde), --  "Crimean Tatar; Crimean Turkish", "tatar de Crimé"
      ("crp", unde), --  "Creoles and pidgins", "créoles et pidgins"
      ("csb", unde), --  "Kashubian", "kachoube"
      ("cus", unde), --  "Cushitic languages", "couchitiques, langues"
      ("wel", "cy"), -- (B)
      ("cym", "cy"), -- (T) "Welsh", "gallois"
      ("dak", unde), --  "Dakota", "dakota"
      ("dan", "da"), --  "Danish", "danois"
      ("dar", unde), --  "Dargwa", "dargwa"
      ("day", unde), --  "Land Dayak languages", "dayak, langues"
      ("del", unde), --  "Delaware", "delaware"
      ("den", unde), --  "Slave (Athapascan)", "esclave (athapascan)"
      ("ger", "de"), -- (B)
      ("deu", "de"), -- (T) "German", "allemand"
      ("dgr", unde), --  "Dogrib", "dogrib"
      ("din", unde), --  "Dinka", "dinka"
      ("div", "dv"), --  "Divehi; Dhivehi; Maldivian", "maldivien"
      ("doi", unde), --  "Dogri", "dogri"
      ("dra", unde), --  "Dravidian languages", "dravidiennes, langues"
      ("dsb", unde), --  "Lower Sorbian", "bas-sorabe"
      ("dua", unde), --  "Duala", "douala"
      ("dum", unde), --  "Dutch, Middle (ca.1050-1350)",
                     --  "néerlandais moyen (ca. 1050-1350)"
      ("dut", "nl"), -- (B)
      ("nld", "nl"), -- (T) "Dutch; Flemish", "néerlandais; flamand"
      ("dyu", unde), --  "Dyula", "dioula"
      ("dzo", "dz"), --  "Dzongkha", "dzongkha"
      ("efi", unde), --  "Efik", "efik"
      ("egy", unde), --  "Egyptian (Ancient)", "égyptien"
      ("eka", unde), --  "Ekajuk", "ekajuk"
      ("gre", "el"), -- (B)
      ("ell", "el"), -- (T) "Greek, Modern (1453-)",
                     --  "grec moderne (après 1453)"
      ("elx", unde), --  "Elamite", "élamite"
      ("eng", "en"), --  "English", "anglais"
      ("enm", unde), --  "English, Middle (1100-1500)",
                     --  "anglais moyen (1100-1500)"
      ("epo", "eo"), --  "Esperanto", "espéranto"
      ("est", "et"), --  "Estonian", "estonien"
      ("ewe", "ee"), --  "Ewe", "éwé"
      ("ewo", unde), --  "Ewondo", "éwondo"
      ("fan", unde), --  "Fang", "fang"
      ("fao", "fo"), --  "Faroese", "féroïen"
      ("per", "fa"), -- (B)
      ("fas", "fa"), -- (T) "Persian", "persan"
      ("fat", unde), --  "Fanti", "fanti"
      ("fij", "fj"), --  "Fijian", "fidjien"
      ("fil", unde), --  "Filipino; Pilipino", "filipino; pilipino"
      ("fin", "fi"), --  "Finnish", "finnois"
      ("fiu", unde), --  "Finno-Ugrian languages", "finno-ougriennes, langues"
      ("fon", unde), --  "Fon", "fon"
      ("fre", "fr"), -- (B)
      ("fra", "fr"), -- (T) "French", "français"
      ("frm", unde), --  "French, Middle (ca.1400-1600)",
                     --  "français moyen (1400-1600)"
      ("fro", unde), --  "French, Old (842-ca.1400)",
                     --  "français ancien (842-ca.1400)"
      ("frr", unde), --  "Northern Frisian", "frison septentrional"
      ("frs", unde), --  "Eastern Frisian", "frison oriental"
      ("fry", "fy"), --  "Western Frisian", "frison occidental"
      ("ful", "ff"), --  "Fulah", "peul"
      ("fur", unde), --  "Friulian", "frioulan"
      ("gaa", unde), --  "Ga", "ga"
      ("gay", unde), --  "Gayo", "gayo"
      ("gba", unde), --  "Gbaya", "gbaya"
      ("gem", unde), --  "Germanic languages", "germaniques, langues"
      ("geo", "ka"), -- (B)
      ("kat", "ka"), -- (T) "Georgian", "géorgien"
      ("gez", unde), --  "Geez", "guèze"
      ("gil", unde), --  "Gilbertese", "kiribati"
      ("gla", "gd"), --  "Gaelic; Scottish Gaelic",
                     --  "gaélique; gaélique écossais"
      ("gle", "ga"), --  "Irish", "irlandais"
      ("glg", "gl"), --  "Galician", "galicien"
      ("glv", "gv"), --  "Manx", "manx; mannois"
      ("gmh", unde), --  "German, Middle High (ca.1050-1500)",
                     --  "allemand, moyen haut (ca. 1050-1500)"
      ("goh", unde), --  "German, Old High (ca.750-1050)",
                     --  "allemand, vieux haut (ca. 750-1050)"
      ("gon", unde), --  "Gondi", "gond"
      ("gor", unde), --  "Gorontalo", "gorontalo"
      ("got", unde), --  "Gothic", "gothique"
      ("grb", unde), --  "Grebo", "grebo"
      ("grc", unde), --  "Greek, Ancient (to 1453)",
                     --  "grec ancien (jusqu'à 1453)"
      ("grn", "gn"), --  "Guarani", "guarani"
      ("gsw", unde), --  "Swiss German; Alemannic; Alsatian",
                     --  "suisse alémanique; alémanique; alsacien"
      ("guj", "gu"), --  "Gujarati", "goudjrati"
      ("gwi", unde), --  "Gwich'in", "gwich'in"
      ("hai", unde), --  "Haida", "haida"
      ("hat", "ht"), --  "Haitian; Haitian Creole",
                     --  "haïtien; créole haïtien"
      ("hau", "ha"), --  "Hausa", "haoussa"
      ("haw", unde), --  "Hawaiian", "hawaïen"
      ("heb", "he"), --  "Hebrew", "hébreu"
      ("her", "hz"), --  "Herero", "herero"
      ("hil", unde), --  "Hiligaynon", "hiligaynon"
      ("him", unde), --  "Himachali languages; Western Pahari languages",
                     --  "langues himachalis; langues paharis occidentales"
      ("hin", "hi"), --  "Hindi", "hindi"
      ("hit", unde), --  "Hittite", "hittite"
      ("hmn", unde), --  "Hmong; Mong", "hmong"
      ("hmo", "ho"), --  "Hiri Motu", "hiri motu"
      ("hrv", "hr"), --  "Croatian", "croate"
      ("hsb", unde), --  "Upper Sorbian", "haut-sorabe"
      ("hun", "hu"), --  "Hungarian", "hongrois"
      ("hup", unde), --  "Hupa", "hupa"
      ("iba", unde), --  "Iban", "iban"
      ("ibo", "ig"), --  "Igbo", "igbo"
      ("ice", "is"), -- (B)
      ("isl", "is"), -- (T) "Icelandic", "islandais"
      ("ido", "io"), --  "Ido", "ido"
      ("iii", "ii"), --  "Sichuan Yi; Nuosu", "yi de Sichuan"
      ("ijo", unde), --  "Ijo languages", "ijo, langues"
      ("iku", "iu"), --  "Inuktitut", "inuktitut"
      ("ile", "ie"), --  "Interlingue; Occidental", "interlingue"
      ("ilo", unde), --  "Iloko", "ilocano"
      ("ina", "ia"),
            --  "Interlingua (International Auxiliary Language Association)",
                     --  "interlingua (langue auxiliaire internationale)"
      ("inc", unde), --  "Indic languages", "indo-aryennes, langues"
      ("ind", "id"), --  "Indonesian", "indonésien"
      ("ine", unde), --  "Indo-European languages",
                     --  "indo-européennes, langues"
      ("inh", unde), --  "Ingush", "ingouche"
      ("ipk", "ik"), --  "Inupiaq", "inupiaq"
      ("ira", unde), --  "Iranian languages", "iraniennes, langues"
      ("iro", unde), --  "Iroquoian languages", "iroquoises, langues"
      ("ita", "it"), --  "Italian", "italien"
      ("jav", "jv"), --  "Javanese", "javanais"
      ("jbo", unde), --  "Lojban", "lojban"
      ("jpn", "ja"), --  "Japanese", "japonais"
      ("jpr", unde), --  "Judeo-Persian", "judéo-persan"
      ("jrb", unde), --  "Judeo-Arabic", "judéo-arabe"
      ("kaa", unde), --  "Kara-Kalpak", "karakalpak"
      ("kab", unde), --  "Kabyle", "kabyle"
      ("kac", unde), --  "Kachin; Jingpho", "kachin; jingpho"
      ("kal", "kl"), --  "Kalaallisut; Greenlandic", "groenlandais"
      ("kam", unde), --  "Kamba", "kamba"
      ("kan", "kn"), --  "Kannada", "kannada"
      ("kar", unde), --  "Karen languages", "karen, langues"
      ("kas", "ks"), --  "Kashmiri", "kashmiri"
      ("kau", "kr"), --  "Kanuri", "kanouri"
      ("kaw", unde), --  "Kawi", "kawi"
      ("kaz", "kk"), --  "Kazakh", "kazakh"
      ("kbd", unde), --  "Kabardian", "kabardien"
      ("kha", unde), --  "Khasi", "khasi"
      ("khi", unde), --  "Khoisan languages", "khoïsan, langues"
      ("khm", "km"), --  "Central Khmer", "khmer central"
      ("kho", unde), --  "Khotanese; Sakan", "khotanais; sakan"
      ("kik", "ki"), --  "Kikuyu; Gikuyu", "kikuyu"
      ("kin", "rw"), --  "Kinyarwanda", "rwanda"
      ("kir", "ky"), --  "Kirghiz; Kyrgyz", "kirghiz"
      ("kmb", unde), --  "Kimbundu", "kimbundu"
      ("kok", unde), --  "Konkani", "konkani"
      ("kom", "kv"), --  "Komi", "kom"
      ("kon", "kg"), --  "Kongo", "kongo"
      ("kor", "ko"), --  "Korean", "coréen"
      ("kos", unde), --  "Kosraean", "kosrae"
      ("kpe", unde), --  "Kpelle", "kpellé"
      ("krc", unde), --  "Karachay-Balkar", "karatchai balkar"
      ("krl", unde), --  "Karelian", "carélien"
      ("kro", unde), --  "Kru languages", "krou, langues"
      ("kru", unde), --  "Kurukh", "kurukh"
      ("kua", "kj"), --  "Kuanyama; Kwanyama", "kuanyama; kwanyama"
      ("kum", unde), --  "Kumyk", "koumyk"
      ("kur", "ku"), --  "Kurdish", "kurde"
      ("kut", unde), --  "Kutenai", "kutenai"
      ("lad", unde), --  "Ladino", "judéo-espagnol"
      ("lah", unde), --  "Lahnda", "lahnda"
      ("lam", unde), --  "Lamba", "lamba"
      ("lao", "lo"), --  "Lao", "lao"
      ("lat", "la"), --  "Latin", "latin"
      ("lav", "lv"), --  "Latvian", "letton"
      ("lez", unde), --  "Lezghian", "lezghien"
      ("lim", "li"), --  "Limburgan; Limburger; Limburgish", "limbourgeois"
      ("lin", "ln"), --  "Lingala", "lingala"
      ("lit", "lt"), --  "Lithuanian", "lituanien"
      ("lol", unde), --  "Mongo", "mongo"
      ("loz", unde), --  "Lozi", "lozi"
      ("ltz", "lb"), --  "Luxembourgish; Letzeburgesch", "luxembourgeois"
      ("lua", unde), --  "Luba-Lulua", "luba-lulua"
      ("lub", "lu"), --  "Luba-Katanga", "luba-katanga"
      ("lug", "lg"), --  "Ganda", "ganda"
      ("lui", unde), --  "Luiseno", "luiseno"
      ("lun", unde), --  "Lunda", "lunda"
      ("luo", unde), --  "Luo (Kenya and Tanzania)", "luo (Kenya et Tanzanie)"
      ("lus", unde), --  "Lushai", "lushai"
      ("mac", "mk"), -- (B)
      ("mkd", "mk"), -- (T) "Macedonian", "macédonien"
      ("mad", unde), --  "Madurese", "madourais"
      ("mag", unde), --  "Magahi", "magahi"
      ("mah", "mh"), --  "Marshallese", "marshall"
      ("mai", unde), --  "Maithili", "maithili"
      ("mak", unde), --  "Makasar", "makassar"
      ("mal", "ml"), --  "Malayalam", "malayalam"
      ("man", unde), --  "Mandingo", "mandingue"
      ("mao", "mi"), -- (B)
      ("mri", "mi"), -- (T) "Maori", "maori"
      ("map", unde), --  "Austronesian languages", "austronésiennes, langues"
      ("mar", "mr"), --  "Marathi", "marathe"
      ("mas", unde), --  "Masai", "massaï"
      ("may", "ms"), -- (B)
      ("msa", "ms"), -- (T) "Malay", "malais"
      ("mdf", unde), --  "Moksha", "moksa"
      ("mdr", unde), --  "Mandar", "mandar"
      ("men", unde), --  "Mende", "mendé"
      ("mga", unde), --  "Irish, Middle (900-1200)",
                     --  "irlandais moyen (900-1200)"
      ("mic", unde), --  "Mi'kmaq; Micmac", "mi'kmaq; micmac"
      ("min", unde), --  "Minangkabau", "minangkabau"
      ("mkh", unde), --  "Mon-Khmer languages", "môn-khmer, langues"
      ("mlg", "mg"), --  "Malagasy", "malgache"
      ("mlt", "mt"), --  "Maltese", "maltais"
      ("mnc", unde), --  "Manchu", "mandchou"
      ("mni", unde), --  "Manipuri", "manipuri"
      ("mno", unde), --  "Manobo languages", "manobo, langues"
      ("moh", unde), --  "Mohawk", "mohawk"
      ("mon", "mn"), --  "Mongolian", "mongol"
      ("mos", unde), --  "Mossi", "moré"
      ("mun", unde), --  "Munda languages", "mounda, langues"
      ("mus", unde), --  "Creek", "muskogee"
      ("mwl", unde), --  "Mirandese", "mirandais"
      ("mwr", unde), --  "Marwari", "marvari"
      ("myn", unde), --  "Mayan languages", "maya, langues"
      ("myv", unde), --  "Erzya", "erza"
      ("nah", unde), --  "Nahuatl languages", "nahuatl, langues"
      ("nai", unde), --  "North American Indian languages",
                     --  "nord-amérindiennes, langues"
      ("nap", unde), --  "Neapolitan", "napolitain"
      ("nau", "na"), --  "Nauru", "nauruan"
      ("nav", "nv"), --  "Navajo; Navaho", "navaho"
      ("nbl", "nr"), --  "Ndebele, South; South Ndebele", "ndébélé du Sud"
      ("nde", "nd"), --  "Ndebele, North; North Ndebele", "ndébélé du Nord"
      ("ndo", "ng"), --  "Ndonga", "ndonga"
      ("nds", unde), --  "Low German; Low Saxon; German, Low; Saxon, Low",
                     --  "bas allemand; bas saxon; allemand, bas; saxon, bas"
      ("nep", "ne"), --  "Nepali", "népalais"
      ("new", unde), --  "Nepal Bhasa; Newari", "nepal bhasa; newari"
      ("nia", unde), --  "Nias", "nias"
      ("nic", unde), --  "Niger-Kordofanian languages",
                     --  "nigéro-kordofaniennes, langues"
      ("niu", unde), --  "Niuean", "niué"
      ("nno", "nn"), --  "Norwegian Nynorsk; Nynorsk, Norwegian",
                     --  "norvégien nynorsk; nynorsk, norvégien"
      ("nob", "nb"), --  "Bokmål, Norwegian; Norwegian Bokmål",
                     --  "norvégien bokmål"
      ("nog", unde), --  "Nogai", "nogaï; nogay"
      ("non", unde), --  "Norse, Old", "norrois, vieux"
      ("nor", "no"), --  "Norwegian", "norvégien"
      ("nqo", unde), --  "N'Ko", "n'ko"
      ("nso", unde), --  "Pedi; Sepedi; Northern Sotho",
                     --  "pedi; sepedi; sotho du Nord"
      ("nub", unde), --  "Nubian languages", "nubiennes, langues"
      ("nwc", unde), --  "Classical Newari; Old Newari; Classical Nepal Bhasa",
                     --  "newari classique"
      ("nya", "ny"), --  "Chichewa; Chewa; Nyanja", "chichewa; chewa; nyanja"
      ("nym", unde), --  "Nyamwezi", "nyamwezi"
      ("nyn", unde), --  "Nyankole", "nyankolé"
      ("nyo", unde), --  "Nyoro", "nyoro"
      ("nzi", unde), --  "Nzima", "nzema"
      ("oci", "oc"), --  "Occitan (post 1500)", "occitan (après 1500)"
      ("oji", "oj"), --  "Ojibwa", "ojibwa"
      ("ori", "or"), --  "Oriya", "oriya"
      ("orm", "om"), --  "Oromo", "galla"
      ("osa", unde), --  "Osage", "osage"
      ("oss", "os"), --  "Ossetian; Ossetic", "ossète"
      ("ota", unde), --  "Turkish, Ottoman (1500-1928)",
                     --  "turc ottoman (1500-1928)"
      ("oto", unde), --  "Otomian languages", "otomi, langues"
      ("paa", unde), --  "Papuan languages", "papoues, langues"
      ("pag", unde), --  "Pangasinan", "pangasinan"
      ("pal", unde), --  "Pahlavi", "pahlavi"
      ("pam", unde), --  "Pampanga; Kapampangan", "pampangan"
      ("pan", "pa"), --  "Panjabi; Punjabi", "pendjabi"
      ("pap", unde), --  "Papiamento", "papiamento"
      ("pau", unde), --  "Palauan", "palau"
      ("peo", unde), --  "Persian, Old (ca.600-400 B.C.)",
                     --  "perse, vieux (ca. 600-400 av. J.-C.)"
      ("phi", unde), --  "Philippine languages", "philippines, langues"
      ("phn", unde), --  "Phoenician", "phénicien"
      ("pli", "pi"), --  "Pali", "pali"
      ("pol", "pl"), --  "Polish", "polonais"
      ("pon", unde), --  "Pohnpeian", "pohnpei"
      ("por", "pt"), --  "Portuguese", "portugais"
      ("pra", unde), --  "Prakrit languages", "prâkrit, langues"
      ("pro", unde), --  "Provençal, Old (to 1500); Occitan, Old (to 1500)",
                     --  "provençal ancien (jusqu'à 1500); "
                     --  & "occitan ancien (jusqu'à 1500)"
      ("pus", "ps"), --  "Pushto; Pashto", "pachto"
      ("que", "qu"), --  "Quechua", "quechua"
      ("raj", unde), --  "Rajasthani", "rajasthani"
      ("rap", unde), --  "Rapanui", "rapanui"
      ("rar", unde), --  "Rarotongan; Cook Islands Maori",
                     --  "rarotonga; maori des îles Cook"
      ("roa", unde), --  "Romance languages", "romanes, langues"
      ("roh", "rm"), --  "Romansh", "romanche"
      ("rom", unde), --  "Romany", "tsigane"
      ("rum", "ro"), -- (B)
      ("ron", "ro"), -- (T) "Romanian; Moldavian; Moldovan", "roumain; moldave"
      ("run", "rn"), --  "Rundi", "rundi"
      ("rup", unde), --  "Aromanian; Arumanian; Macedo-Romanian",
                     --  "aroumain; macédo-roumain"
      ("rus", "ru"), --  "Russian", "russe"
      ("sad", unde), --  "Sandawe", "sandawe"
      ("sag", "sg"), --  "Sango", "sango"
      ("sah", unde), --  "Yakut", "iakoute"
      ("sai", unde), --  "South American Indian languages",
                     --  "sud-amérindiennes, langues"
      ("sal", unde), --  "Salishan languages", "salishennes, langues"
      ("sam", unde), --  "Samaritan Aramaic", "samaritain"
      ("san", "sa"), --  "Sanskrit", "sanskrit"
      ("sas", unde), --  "Sasak", "sasak"
      ("sat", unde), --  "Santali", "santal"
      ("scn", unde), --  "Sicilian", "sicilien"
      ("sco", unde), --  "Scots", "écossais"
      ("sel", unde), --  "Selkup", "selkoupe"
      ("sem", unde), --  "Semitic languages", "sémitiques, langues"
      ("sga", unde), --  "Irish, Old (to 900)",
                     --  "irlandais ancien (jusqu'à 900)"
      ("sgn", unde), --  "Sign Languages", "langues des signes"
      ("shn", unde), --  "Shan", "chan"
      ("sid", unde), --  "Sidamo", "sidamo"
      ("sin", "si"), --  "Sinhala; Sinhalese", "singhalais"
      ("sio", unde), --  "Siouan languages", "sioux, langues"
      ("sit", unde), --  "Sino-Tibetan languages", "sino-tibétaines, langues"
      ("sla", unde), --  "Slavic languages", "slaves, langues"
      ("slo", "sk"), -- (B)
      ("slk", "sk"), -- (T) "Slovak", "slovaque"
      ("slv", "sl"), --  "Slovenian", "slovène"
      ("sma", unde), --  "Southern Sami", "sami du Sud"
      ("sme", "se"), --  "Northern Sami", "sami du Nord"
      ("smi", unde), --  "Sami languages", "sames, langues"
      ("smj", unde), --  "Lule Sami", "sami de Lule"
      ("smn", unde), --  "Inari Sami", "sami d'Inari"
      ("smo", "sm"), --  "Samoan", "samoan"
      ("sms", unde), --  "Skolt Sami", "sami skolt"
      ("sna", "sn"), --  "Shona", "shona"
      ("snd", "sd"), --  "Sindhi", "sindhi"
      ("snk", unde), --  "Soninke", "soninké"
      ("sog", unde), --  "Sogdian", "sogdien"
      ("som", "so"), --  "Somali", "somali"
      ("son", unde), --  "Songhai languages", "songhai, langues"
      ("sot", "st"), --  "Sotho, Southern", "sotho du Sud"
      ("spa", "es"), --  "Spanish; Castilian", "espagnol; castillan"
      ("srd", "sc"), --  "Sardinian", "sarde"
      ("srn", unde), --  "Sranan Tongo", "sranan tongo"
      ("srp", "sr"), --  "Serbian", "serbe"
      ("srr", unde), --  "Serer", "sérère"
      ("ssa", unde), --  "Nilo-Saharan languages", "nilo-sahariennes, langues"
      ("ssw", "ss"), --  "Swati", "swati"
      ("suk", unde), --  "Sukuma", "sukuma"
      ("sun", "su"), --  "Sundanese", "soundanais"
      ("sus", unde), --  "Susu", "soussou"
      ("sux", unde), --  "Sumerian", "sumérien"
      ("swa", "sw"), --  "Swahili", "swahili"
      ("swe", "sv"), --  "Swedish", "suédois"
      ("syc", unde), --  "Classical Syriac", "syriaque classique"
      ("syr", unde), --  "Syriac", "syriaque"
      ("tah", "ty"), --  "Tahitian", "tahitien"
      ("tai", unde), --  "Tai languages", "tai, langues"
      ("tam", "ta"), --  "Tamil", "tamoul"
      ("tat", "tt"), --  "Tatar", "tatar"
      ("tel", "te"), --  "Telugu", "télougou"
      ("tem", unde), --  "Timne", "temne"
      ("ter", unde), --  "Tereno", "tereno"
      ("tet", unde), --  "Tetum", "tetum"
      ("tgk", "tg"), --  "Tajik", "tadjik"
      ("tgl", "tl"), --  "Tagalog", "tagalog"
      ("tha", "th"), --  "Thai", "thaï"
      ("tig", unde), --  "Tigre", "tigré"
      ("tir", "ti"), --  "Tigrinya", "tigrigna"
      ("tiv", unde), --  "Tiv", "tiv"
      ("tkl", unde), --  "Tokelau", "tokelau"
      ("tlh", unde), --  "Klingon; tlhIngan-Hol", "klingon"
      ("tli", unde), --  "Tlingit", "tlingit"
      ("tmh", unde), --  "Tamashek", "tamacheq"
      ("tog", unde), --  "Tonga (Nyasa)", "tonga (Nyasa)"
      ("ton", "to"), --  "Tonga (Tonga Islands)", "tongan (Îles Tonga)"
      ("tpi", unde), --  "Tok Pisin", "tok pisin"
      ("tsi", unde), --  "Tsimshian", "tsimshian"
      ("tsn", "tn"), --  "Tswana", "tswana"
      ("tso", "ts"), --  "Tsonga", "tsonga"
      ("tuk", "tk"), --  "Turkmen", "turkmène"
      ("tum", unde), --  "Tumbuka", "tumbuka"
      ("tup", unde), --  "Tupi languages", "tupi, langues"
      ("tur", "tr"), --  "Turkish", "turc"
      ("tut", unde), --  "Altaic languages", "altaïques, langues"
      ("tvl", unde), --  "Tuvalu", "tuvalu"
      ("twi", "tw"), --  "Twi", "twi"
      ("tyv", unde), --  "Tuvinian", "touva"
      ("udm", unde), --  "Udmurt", "oudmourte"
      ("uga", unde), --  "Ugaritic", "ougaritique"
      ("uig", "ug"), --  "Uighur; Uyghur", "ouïgour"
      ("ukr", "uk"), --  "Ukrainian", "ukrainien"
      ("umb", unde), --  "Umbundu", "umbundu"
      ("urd", "ur"), --  "Urdu", "ourdou"
      ("uzb", "uz"), --  "Uzbek", "ouszbek"
      ("vai", unde), --  "Vai", "vaï"
      ("ven", "ve"), --  "Venda", "venda"
      ("vie", "vi"), --  "Vietnamese", "vietnamien"
      ("vol", "vo"), --  "Volapük", "volapük"
      ("vot", unde), --  "Votic", "vote"
      ("wak", unde), --  "Wakashan languages", "wakashanes, langues"
      ("wal", unde), --  "Wolaitta; Wolaytta", "wolaitta; wolaytta"
      ("war", unde), --  "Waray", "waray"
      ("was", unde), --  "Washo", "washo"
      ("wen", unde), --  "Sorbian languages", "sorabes, langues"
      ("wln", "wa"), --  "Walloon", "wallon"
      ("wol", "wo"), --  "Wolof", "wolof"
      ("xal", unde), --  "Kalmyk; Oirat", "kalmouk; oïrat"
      ("xho", "xh"), --  "Xhosa", "xhosa"
      ("yao", unde), --  "Yao", "yao"
      ("yap", unde), --  "Yapese", "yapois"
      ("yid", "yi"), --  "Yiddish", "yiddish"
      ("yor", "yo"), --  "Yoruba", "yoruba"
      ("ypk", unde), --  "Yupik languages", "yupik, langues"
      ("zap", unde), --  "Zapotec", "zapotèque"
      ("zbl", unde), --  "Blissymbols; Blissymbolics; Bliss",
                     --  "symboles Bliss; Bliss"
      ("zen", unde), --  "Zenaga", "zenaga"
      ("zha", "za"), --  "Zhuang; Chuang", "zhuang; chuang"
      ("znd", unde), --  "Zande languages", "zandé, langues"
      ("zul", "zu"), --  "Zulu", "zoulou"
      ("zun", unde), --  "Zuni", "zuni"
      ("zza", unde)); -- "Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki",
                     --  "zaza; dimili; dimli; kirdki; kirmanjki; zazaki"
   --  mis, mul, qaa-qtz, und, zxx are excluded

   Lang_Map_3 : access Language_Table_Array;
   Lang_Map_3_Flag : aliased System.Once.Flag := 0;

   procedure Lang_Map_3_Init;
   procedure Lang_Map_3_Init is
   begin
      --  copy table
      Lang_Map_3 := new Language_Table_Array'(Language_Table);
      --  check duplicated?
      pragma Check (Validate,
         (for all I in Lang_Map_3'First .. Lang_Map_3'Last - 1 =>
            (for all J in I + 1 .. Lang_Map_3'Last =>
               Lang_Map_3 (I).Alpha_3 /= Lang_Map_3 (J).Alpha_3)));
      --  sort
      for I in Lang_Map_3'First + 1 .. Lang_Map_3'Last loop
         for J in reverse Lang_Map_3'First .. I - 1 loop
            exit when Compare (
               Lang_Map_3 (J + 1).Alpha_3,
               Lang_Map_3 (J).Alpha_3) >= 0;
            declare
               Temp : constant Language_Table_Element := Lang_Map_3 (J);
            begin
               Lang_Map_3 (J) := Lang_Map_3 (J + 1);
               Lang_Map_3 (J + 1) := Temp;
            end;
         end loop;
      end loop;
   end Lang_Map_3_Init;

   function To_Alpha_2 (Item : ISO_639_Alpha_3) return ISO_639_Alpha_2 is
   begin
      System.Once.Initialize (Lang_Map_3_Flag'Access, Lang_Map_3_Init'Access);
      declare
         First : Positive := Lang_Map_3'First;
         Last : Natural := Lang_Map_3'Last;
      begin
         while First <= Last loop
            declare
               Middle : constant Positive := (First + Last) / 2;
               Middle_Item : Language_Table_Element
                  renames Lang_Map_3 (Middle);
               Compared : constant Integer := Compare (
                  Middle_Item.Alpha_3,
                  Alpha_3_NP (Item));
            begin
               if Compared > 0 then
                  Last := Middle - 1;
               elsif Compared < 0 then
                  First := Middle + 1;
               else
                  return (
                     1 => Middle_Item.Alpha_2 (1),
                     2 => Middle_Item.Alpha_2 (2));
               end if;
            end;
         end loop;
         return ISO_639_Alpha_2_Unknown;
      end;
   end To_Alpha_2;

   Lang_Map_2 : access Language_Table_Array;
   Lang_Map_2_Flag : aliased System.Once.Flag := 0;

   procedure Lang_Map_2_Init;
   procedure Lang_Map_2_Init is
   begin
      --  copy table
      --  select terminology (second)
      declare
         Num : Natural := 0;
         J : Positive;
      begin
         for I in Language_Table'Range loop
            if Language_Table (I).Alpha_2 /= unde
               and then (
                  I = Language_Table'Last
                  or else Language_Table (I).Alpha_2 /=
                     Language_Table (I + 1).Alpha_2)
            then
               Num := Num + 1;
            end if;
         end loop;
         Lang_Map_2 := new Language_Table_Array (1 .. Num);
         J := Lang_Map_2'First;
         for I in Language_Table'Range loop
            if Language_Table (I).Alpha_2 /= unde
               and then (
                  I = Language_Table'Last
                  or else Language_Table (I).Alpha_2 /=
                     Language_Table (I + 1).Alpha_2)
            then
               Lang_Map_2 (J) := Language_Table (I);
               J := J + 1;
            end if;
         end loop;
      end;
      --  check duplicated?
      pragma Check (Validate,
         (for all I in Lang_Map_2'First .. Lang_Map_2'Last - 1 =>
            (for all J in I + 1 .. Lang_Map_2'Last =>
               Lang_Map_2 (I).Alpha_2 /= Lang_Map_2 (J).Alpha_2)));
      --  sort
      for I in Lang_Map_2'First + 1 .. Lang_Map_2'Last loop
         for J in reverse Lang_Map_2'First .. I - 1 loop
            exit when Compare (
               Lang_Map_2 (J + 1).Alpha_2,
               Lang_Map_2 (J).Alpha_2) >= 0;
            declare
               Temp : constant Language_Table_Element := Lang_Map_2 (J);
            begin
               Lang_Map_2 (J) := Lang_Map_2 (J + 1);
               Lang_Map_2 (J + 1) := Temp;
            end;
         end loop;
      end loop;
   end Lang_Map_2_Init;

   function To_Alpha_3 (Item : ISO_639_Alpha_2) return ISO_639_Alpha_3 is
   begin
      System.Once.Initialize (Lang_Map_2_Flag'Access, Lang_Map_2_Init'Access);
      declare
         First : Positive := Lang_Map_2'First;
         Last : Natural := Lang_Map_2'Last;
      begin
         while First <= Last loop
            declare
               Middle : constant Positive := (First + Last) / 2;
               Middle_Item : Language_Table_Element
                  renames Lang_Map_2 (Middle);
               Compared : constant Integer := Compare (
                  Middle_Item.Alpha_2,
                  Alpha_2_NP (Item));
            begin
               if Compared > 0 then
                  Last := Middle - 1;
               elsif Compared < 0 then
                  First := Middle + 1;
               else
                  return (
                     1 => Middle_Item.Alpha_3 (1),
                     2 => Middle_Item.Alpha_3 (2),
                     3 => Middle_Item.Alpha_3 (3));
               end if;
            end;
         end loop;
         return ISO_639_Alpha_3_Unknown;
      end;
   end To_Alpha_3;

   function Language return ISO_639_Alpha_2
      renames Inside.Language;
   function Language return ISO_639_Alpha_3
      renames Inside.Language;
   function Country return ISO_3166_1_Alpha_2
      renames Inside.Country;

end Ada.Locales;
