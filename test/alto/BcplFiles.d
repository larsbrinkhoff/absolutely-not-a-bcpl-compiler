// BcplFiles.d -- Bcpl-related file formats// Copyright Xerox Corporation 1980// Last modified February 9, 1980  1:13 AM by Boggs//----------------------------------------------------------------------------structure BLV:		// Bcpl Layout Vector - passed to program on startup//----------------------------------------------------------------------------[overlayAddress^0, 25 wordstartOfStatics word	// address of first staticendOfStatics word	// address of last staticstartOfCode word	// address of first word of codeafterLastCodeWord word	// 1 + largest address at which code is loaded			//  (normally endCode is the same, and the system			//  treats that value as the end of the program)endCode word		// first location which may be used for data; 			//  used by the system to set EndCoderelPairTable word	// /I switch: address in code area for table]manifest lBLV = size BLV/16//----------------------------------------------------------------------------structure SV:		// Format of an Alto RUN (save) file//----------------------------------------------------------------------------[H:			// This is a mangled BBHeader   [   startingAddress word	// Initial value for PC = SV.BLV.startOfCode   length word		// # full pages up to afterLastCodeWord   type word		// = 0: resident code has type A overlay format   nStaticLinks word	// # static links after afterLastCodeWord   blank^2, 11 word   ]BLV @BLV		// Bcpl layout vectorpage0^0, 277b word	// The first 16b words are ignored; the rest are 			//  used to set words 16b to 277b of memorystatics^0, 0 word	// Actually there are (BLV.endOfStatics-			//  BLV.startOfStatics + 1) words herecode^0, 0 word		// Actually there are (BLV.endCode- 			//  BLV.startOfCode) words hereend word]//----------------------------------------------------------------------------structure BBHeader:	// header of .BB overlay file or file segment//----------------------------------------------------------------------------[codeLoc word		// PC of first code word, controlled by /PcodeLength word		// # code words in overlaytype word		// 0 for /A, 1 for /BrelPairTable word	// FILE word location of relocation tablefileLength word		// file or segment length, in wordsoverlayPage word	// (alto page #)-1 of this disk pageblank^6b,17b word	// reserved]manifest lBBHeader = size BBHeader/16//----------------------------------------------------------------------------structure SYmsHeader:	// header of .SYMS file//----------------------------------------------------------------------------[version word		// version of BLDR that loadedfileLength word		// in wordsnamesAddr word		// file word location of name stringssymsAddr word		// location of static symbol descriptionsbrFilesAddr word	// location of .BR file descriptionsbinFilesAddr word	// location of .RUN and .BB file descripsblank^6b,17b word	// reserved]manifest lSYmsHeader = size SYmsHeader/16