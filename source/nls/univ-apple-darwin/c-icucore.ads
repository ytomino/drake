--  **********************************************************************
--  *   Copyright (C) 1996-2010, International Business Machines
--  *   Corporation and others.  All Rights Reserved.
--  **********************************************************************
--  Ada version: 2013 yt
pragma License (Unrestricted); -- MIT License
--  translated unit from icu4c of http://site.icu-project.org/
with C.stdint;
package C.icucore is
   pragma Preelaborate;

   package unicode is

      --  <unicode/umachine.h>
      package umachine is

         subtype UBool is stdint.int8_t;

         U_SIZEOF_UCHAR : constant := 2;

         type UChar is new Wide_Character;
         pragma Convention (C, UChar);
         type UChar_ptr is access all UChar;
         for UChar_ptr'Storage_Size use 0;
         pragma Convention (C, UChar_ptr);
         type UChar_const_ptr is access constant UChar;
         for UChar_const_ptr'Storage_Size use 0;
         pragma Convention (C, UChar_const_ptr);
         type UChar_array is array (C.size_t range <>) of aliased UChar;
         pragma Convention (C, UChar_array);

         type UChar32 is new Wide_Wide_Character;
         pragma Convention (C, UChar32);

      end umachine;

      --  <unicode/utypes.h>
      package utypes is

         type UErrorCode is (
            U_USING_FALLBACK_WARNING,
            U_ERROR_WARNING_LIMIT,
            U_ZERO_ERROR,
            U_ILLEGAL_ARGUMENT_ERROR,
            U_MISSING_RESOURCE_ERROR,
            U_INVALID_FORMAT_ERROR,
            U_FILE_ACCESS_ERROR,
            U_INTERNAL_PROGRAM_ERROR,
            U_MESSAGE_PARSE_ERROR,
            U_MEMORY_ALLOCATION_ERROR,
            U_INDEX_OUTOFBOUNDS_ERROR,
            U_PARSE_ERROR,
            U_INVALID_CHAR_FOUND,
            U_TRUNCATED_CHAR_FOUND,
            U_ILLEGAL_CHAR_FOUND,
            U_INVALID_TABLE_FORMAT,
            U_INVALID_TABLE_FILE,
            U_BUFFER_OVERFLOW_ERROR,
            U_UNSUPPORTED_ERROR,
            U_STANDARD_ERROR_LIMIT,
            U_PLUGIN_ERROR_LIMIT);
         for UErrorCode use (
            U_USING_FALLBACK_WARNING => -128,
            U_ERROR_WARNING_LIMIT => -119,
            U_ZERO_ERROR => 0,
            U_ILLEGAL_ARGUMENT_ERROR => 1,
            U_MISSING_RESOURCE_ERROR => 2,
            U_INVALID_FORMAT_ERROR => 3,
            U_FILE_ACCESS_ERROR => 4,
            U_INTERNAL_PROGRAM_ERROR => 5,
            U_MESSAGE_PARSE_ERROR => 6,
            U_MEMORY_ALLOCATION_ERROR => 7,
            U_INDEX_OUTOFBOUNDS_ERROR => 8,
            U_PARSE_ERROR => 9,
            U_INVALID_CHAR_FOUND => 10,
            U_TRUNCATED_CHAR_FOUND => 11,
            U_ILLEGAL_CHAR_FOUND => 12,
            U_INVALID_TABLE_FORMAT => 13,
            U_INVALID_TABLE_FILE => 14,
            U_BUFFER_OVERFLOW_ERROR => 15,
            U_UNSUPPORTED_ERROR => 16,
            U_STANDARD_ERROR_LIMIT => 31,
            U_PLUGIN_ERROR_LIMIT => 16#10502#);
         pragma Convention (C, UErrorCode);
         pragma Discard_Names (UErrorCode);

         function U_ERROR_LIMIT return UErrorCode
            renames U_PLUGIN_ERROR_LIMIT;

      end utypes;

      --  <unicode/ucnv_err.h>
      package ucnv_err is

         type UConverter (<>) is limited private;
         type UConverter_ptr is access all UConverter;
         for UConverter_ptr'Storage_Size use 0;

         type UConverterCallbackReason is (
            UCNV_UNASSIGNED,
            UCNV_ILLEGAL,
            UCNV_IRREGULAR,
            UCNV_RESET,
            UCNV_CLOSE,
            UCNV_CLONE);
         pragma Convention (C, UConverterCallbackReason);
         pragma Discard_Names (UConverterCallbackReason);

         type UConverterFromUnicodeArgs is record
            size : stdint.uint16_t;
            flush : umachine.UBool;
            converter : access UConverter;
            source : access constant umachine.UChar;
            sourceLimit : access constant umachine.UChar;
            target : access char;
            targetLimit : access constant char;
            offsets : access stdint.int32_t;
         end record;
         pragma Convention (C, UConverterFromUnicodeArgs);
         pragma Suppress_Initialization (UConverterFromUnicodeArgs);

         type UConverterToUnicodeArgs is record
            size : stdint.uint16_t;
            flush : umachine.UBool;
            converter : access UConverter;
            source : access constant char;
            sourceLimit : access constant char;
            target : access umachine.UChar;
            targetLimit : access constant umachine.UChar;
            offsets : access stdint.int32_t;
         end record;
         pragma Convention (C, UConverterToUnicodeArgs);
         pragma Suppress_Initialization (UConverterToUnicodeArgs);

         procedure UCNV_FROM_U_CALLBACK_STOP (
            context : void_const_ptr;
            fromUArgs : access UConverterFromUnicodeArgs;
            codeUnits : access constant umachine.UChar;
            length : stdint.int32_t;
            codePoint : umachine.UChar32;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_FROM_U_CALLBACK_STOP,
            "UCNV_FROM_U_CALLBACK_STOP");

         procedure UCNV_TO_U_CALLBACK_STOP (
            context : void_const_ptr;
            toUArgs : access UConverterToUnicodeArgs;
            codeUnits : access constant char;
            length : stdint.int32_t;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_TO_U_CALLBACK_STOP,
            "UCNV_TO_U_CALLBACK_STOP");

         procedure UCNV_FROM_U_CALLBACK_SKIP (
            context : void_const_ptr;
            fromUArgs : access UConverterFromUnicodeArgs;
            codeUnits : access constant umachine.UChar;
            length : stdint.int32_t;
            codePoint : umachine.UChar32;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_FROM_U_CALLBACK_SKIP,
            "UCNV_FROM_U_CALLBACK_SKIP");

         procedure UCNV_FROM_U_CALLBACK_SUBSTITUTE (
            context : void_const_ptr;
            fromUArgs : access UConverterFromUnicodeArgs;
            codeUnits : access constant umachine.UChar;
            length : stdint.int32_t;
            codePoint : umachine.UChar32;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_FROM_U_CALLBACK_SUBSTITUTE,
            "UCNV_FROM_U_CALLBACK_SUBSTITUTE");

         procedure UCNV_FROM_U_CALLBACK_ESCAPE (
            context : void_const_ptr;
            fromUArgs : access UConverterFromUnicodeArgs;
            codeUnits : access constant umachine.UChar;
            length : stdint.int32_t;
            codePoint : umachine.UChar32;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_FROM_U_CALLBACK_ESCAPE,
            "UCNV_FROM_U_CALLBACK_ESCAPE");

         procedure UCNV_TO_U_CALLBACK_SKIP (
            context : void_const_ptr;
            toUArgs : access UConverterToUnicodeArgs;
            codeUnits : access constant char;
            length : stdint.int32_t;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_TO_U_CALLBACK_SKIP,
            "UCNV_TO_U_CALLBACK_SKIP");

         procedure UCNV_TO_U_CALLBACK_SUBSTITUTE (
            context : void_const_ptr;
            toUArgs : access UConverterToUnicodeArgs;
            codeUnits : access constant char;
            length : stdint.int32_t;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_TO_U_CALLBACK_SUBSTITUTE,
            "UCNV_TO_U_CALLBACK_SUBSTITUTE");

         procedure UCNV_TO_U_CALLBACK_ESCAPE (
            context : void_const_ptr;
            toUArgs : access UConverterToUnicodeArgs;
            codeUnits : access constant char;
            length : stdint.int32_t;
            reason : UConverterCallbackReason;
            err : access utypes.UErrorCode);
         pragma Import (C, UCNV_TO_U_CALLBACK_ESCAPE,
            "UCNV_TO_U_CALLBACK_ESCAPE");

      private

         type UConverter is null record;
         pragma Convention (C, UConverter);
         pragma Suppress_Initialization (UConverter);

      end ucnv_err;

      --  <unicode/ucnv.h>
      package ucnv is

         type UConverterToUCallback is access procedure (
            context : void_const_ptr;
            args : access ucnv_err.UConverterToUnicodeArgs;
            codeUnits : access constant char;
            length : stdint.int32_t;
            reason : ucnv_err.UConverterCallbackReason;
            pErrorCode : access utypes.UErrorCode);
         pragma Convention (C, UConverterToUCallback);

         type UConverterFromUCallback is access procedure (
            context : void_const_ptr;
            args : access ucnv_err.UConverterFromUnicodeArgs;
            codeUnits : access constant umachine.UChar;
            length : stdint.int32_t;
            codePoint : umachine.UChar32;
            reason : ucnv_err.UConverterCallbackReason;
            pErrorCode : access utypes.UErrorCode);
         pragma Convention (C, UConverterFromUCallback);

         function ucnv_open (
            converterName : access constant char;
            err : access utypes.UErrorCode)
            return ucnv_err.UConverter_ptr;
         pragma Import (C, ucnv_open, "ucnv_open");

         procedure ucnv_close (
            converter : access ucnv_err.UConverter);
         pragma Import (C, ucnv_close, "ucnv_close");

         procedure ucnv_getSubstChars (
            converter : access constant ucnv_err.UConverter;
            subChars : access char;
            len : access stdint.int8_t;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_getSubstChars, "ucnv_getSubstChars");

         procedure ucnv_setSubstChars (
            converter : access ucnv_err.UConverter;
            subChars : access constant char;
            len : stdint.int8_t;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_setSubstChars, "ucnv_setSubstChars");

         function ucnv_getMinCharSize (
            converter : access constant ucnv_err.UConverter)
            return stdint.int8_t;
         pragma Import (C, ucnv_getMinCharSize, "ucnv_getMinCharSize");

         procedure ucnv_getToUCallBack (
            converter : access constant ucnv_err.UConverter;
            action : access UConverterToUCallback;
            context : access void_const_ptr);
         pragma Import (C, ucnv_getToUCallBack, "ucnv_getToUCallBack");

         procedure ucnv_getFromUCallBack (
            converter : access constant ucnv_err.UConverter;
            action : access UConverterFromUCallback;
            context : access void_const_ptr);
         pragma Import (C, ucnv_getFromUCallBack, "ucnv_getFromUCallBack");

         procedure ucnv_setToUCallBack (
            converter : access ucnv_err.UConverter;
            newAction : UConverterToUCallback;
            newContext : void_const_ptr;
            oldAction : access UConverterToUCallback;
            oldContext : access void_const_ptr;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_setToUCallBack, "ucnv_setToUCallBack");

         procedure ucnv_setFromUCallBack (
            converter : access ucnv_err.UConverter;
            newAction : UConverterFromUCallback;
            newContext : void_const_ptr;
            oldAction : access UConverterFromUCallback;
            oldContext : access void_const_ptr;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_setFromUCallBack, "ucnv_setFromUCallBack");

         procedure ucnv_fromUnicode (
            converter : access ucnv_err.UConverter;
            target : access char_ptr;
            targetLimit : access constant char;
            source : access umachine.UChar_const_ptr;
            sourceLimit : access constant umachine.UChar;
            offsets : access stdint.int32_t;
            flush : umachine.UBool;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_fromUnicode, "ucnv_fromUnicode");

         procedure ucnv_toUnicode (
            converter : access ucnv_err.UConverter;
            target : access umachine.UChar_ptr;
            targetLimit : access constant umachine.UChar;
            source : access char_const_ptr;
            sourceLimit : access constant char;
            offsets : access stdint.int32_t;
            flush : umachine.UBool;
            err : access utypes.UErrorCode);
         pragma Import (C, ucnv_toUnicode, "ucnv_toUnicode");

      end ucnv;

   end unicode;

   --  alias
   subtype UBool is unicode.umachine.UBool;
   subtype UChar is unicode.umachine.UChar;
   subtype UChar_ptr is unicode.umachine.UChar_ptr;
   subtype UChar_const_ptr is unicode.umachine.UChar_const_ptr;
   subtype UChar_array is unicode.umachine.UChar_array;
   subtype UChar32 is unicode.umachine.UChar32;
   subtype UErrorCode is unicode.utypes.UErrorCode;
   subtype UConverter is unicode.ucnv_err.UConverter;
   subtype UConverter_ptr is unicode.ucnv_err.UConverter_ptr;
   subtype UConverterCallbackReason is
      unicode.ucnv_err.UConverterCallbackReason;
   subtype UConverterFromUnicodeArgs is
      unicode.ucnv_err.UConverterFromUnicodeArgs;
   subtype UConverterToUnicodeArgs is
      unicode.ucnv_err.UConverterToUnicodeArgs;
   subtype UConverterToUCallback is
      unicode.ucnv.UConverterToUCallback;
   subtype UConverterFromUCallback is
      unicode.ucnv.UConverterFromUCallback;

end C.icucore;
