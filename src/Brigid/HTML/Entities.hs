-- | This module exposes a number of convenience functions to write an HTML
-- entity. Each entity is written using the HTML decimal code. The names for
-- these entity constants were derived from a description of the entity's
-- printed appearance or function as a control character. They are ordered by
-- their HTML decimal code, ascending.
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://www.ee.ucl.ac.uk/mflanaga/java/HTMLandASCIItableC1.html
--
module Brigid.HTML.Entities
  ( nullCharacter
  , startOfHeader
  , startOfText
  , endOfText
  , endOfTransmission
  , enquiry
  , acknowledge
  , bell
  , backspace
  , characterTabulation
  , lineFeed
  , verticalTab
  , formFeed
  , carriageReturn
  , shiftOut
  , shiftIn
  , dataLinkEscape
  , deviceControl1
  , deviceControl2
  , deviceControl3
  , deviceControl4
  , negativeAcknowledge
  , synchronize
  , endTransmissionBlock
  , cancel
  , endOfMedium
  , substitute
  , escape
  , fileSeparator
  , groupSeparator
  , recordSeparator
  , unitSeparator
  , space
  , exclamationPoint
  , quotationMark
  , numberSign
  , dollarSign
  , percentSign
  , ampersand
  , singleQuote
  , openingParenthesis
  , closingParenthesis
  , asterisk
  , plusSign
  , comma
  , minusSignHyphen
  , period
  , slash
  , zero
  , one
  , two
  , three
  , four
  , five
  , six
  , seven
  , eight
  , nine
  , colon
  , semicolon
  , lessThanSign
  , equalSign
  , greaterThanSign
  , questionMark
  , atSymbol
  , upperCaseA
  , upperCaseB
  , upperCaseC
  , upperCaseD
  , upperCaseE
  , upperCaseF
  , upperCaseG
  , upperCaseH
  , upperCaseI
  , upperCaseJ
  , upperCaseK
  , upperCaseL
  , upperCaseM
  , upperCaseN
  , upperCaseO
  , upperCaseP
  , upperCaseQ
  , upperCaseR
  , upperCaseS
  , upperCaseT
  , upperCaseU
  , upperCaseV
  , upperCaseW
  , upperCaseX
  , upperCaseY
  , upperCaseZ
  , leftSquareBracket
  , backslash
  , rightSquareBracket
  , caretCircumflex
  , underscore
  , graveAccent
  , lowerCaseA
  , lowerCaseB
  , lowerCaseC
  , lowerCaseD
  , lowerCaseE
  , lowerCaseF
  , lowerCaseG
  , lowerCaseH
  , lowerCaseI
  , lowerCaseJ
  , lowerCaseK
  , lowerCaseL
  , lowerCaseM
  , lowerCaseN
  , lowerCaseO
  , lowerCaseP
  , lowerCaseQ
  , lowerCaseR
  , lowerCaseS
  , lowerCaseT
  , lowerCaseU
  , lowerCaseV
  , lowerCaseW
  , lowerCaseX
  , lowerCaseY
  , lowerCaseZ
  , leftCurlyBracket
  , verticalLine
  , rightCurlyBracket
  , equivalencySignTilde
  , delete
  , paddingCharacter
  , highOctetPreset
  , breakPermittedHere
  , noBreakHere
  , index
  , nextLine
  , startOfSelectedArea
  , endOfSelectedArea
  , ncharacterTabulationSet
  , characterTabulationSetWithJustification
  , lineTabulationSet
  , partialLineForward
  , partialLineBackward
  , reverseLineFeed
  , singleShift2
  , singleShift3
  , deviceControlString
  , privateUse1
  , privateUse2
  , setTransmitState
  , cancelCharacter
  , messageWaiting
  , startOfProtectedArea
  , endOfProtectedArea
  , startOfStringFollowedByAControlStringTerminatedBySt
  , singleGraphicCharacterIntroducer
  , singleCharacterIntroducer
  , controlSequenceIntroducer
  , stringTerminator
  , operatingSystemCommand
  , privacyMessage
  , applicationProgramCommand
  , noBreakSpace
  , invertedExclamationMark
  , centSign
  , poundSign
  , currencySign
  , yenSign
  , brokenVerticalBar
  , sectionSign
  , diaeresis
  , copyrightSign
  , feminineOrdinalIndicator
  , leftDoubleAngleQuotes
  , notSign
  , softHyphen
  , registeredSign
  , macron
  , degreeSign
  , plusMinusSign
  , superscriptTwoSquared
  , superscriptThreeCubed
  , acuteAccent
  , microSign
  , pilcrowSignParagraphSign
  , middleDot
  , cedilla
  , superscriptOne
  , masculineOrdinalIndicator
  , rightDoubleAngleQuotes
  , fractionOneQuarter
  , vulgarFractionOneHalf
  , fractionThreeQuarters
  , invertedQuestionMark
  , latinCapitalLetterAWithGrave
  , latinCapitalLetterAWithAcute
  , latinCapitalLetterAWithCircumflex
  , latinCapitalLetterAWithTilde
  , latinCapitalLetterAWithDiaeresis
  , latinCapitalLetterAWithRingAbove
  , latinCapitalLetterAe
  , latinCapitalLetterCWithCedilla
  , latinCapitalLetterEWithGrave
  , latinCapitalLetterEWithAcute
  , latinCapitalLetterEWithCircumflex
  , latinCapitalLetterEWithDiaeresis
  , latinCapitalLetterIWithGrave
  , latinCapitalLetterIWithAcute
  , latinCapitalLetterIWithCircumflex
  , latinCapitalLetterIWithDiaeresis
  , latinCapitalLetterEth
  , latinCapitalLetterNWithTilde
  , latinCapitalLetterOWithGrave
  , latinCapitalLetterOWithAcute
  , latinCapitalLetterOWithCircumflex
  , latinCapitalLetterOWithTilde
  , latinCapitalLetterOWithDiaeresis
  , multiplicationSign
  , latinCapitalLetterOWithSlash
  , latinCapitalLetterUWithGrave
  , latinCapitalLetterUWithAcute
  , latinCapitalLetterUWithCircumflex
  , latinCapitalLetterUWithDiaeresis
  , latinCapitalLetterYWithAcute
  , latinCapitalLetterThorn
  , latinSmallLetterSharpSEssZed
  , latinSmallLetterAWithGrave
  , latinSmallLetterAWithAcute
  , latinSmallLetterAWithCircumflex
  , latinSmallLetterAWithTilde
  , latinSmallLetterAWithDiaeresis
  , latinSmallLetterAWithRingAbove
  , latinSmallLetterAe
  , latinSmallLetterCWithCedilla
  , latinSmallLetterEWithGrave
  , latinSmallLetterEWithAcute
  , latinSmallLetterEWithCircumflex
  , latinSmallLetterEWithDiaeresis
  , latinSmallLetterIWithGrave
  , latinSmallLetterIWithAcute
  , latinSmallLetterIWithCircumflex
  , latinSmallLetterIWithDiaeresis
  , latinSmallLetterEth
  , latinSmallLetterNWithTilde
  , latinSmallLetterOWithGrave
  , latinSmallLetterOWithAcute
  , latinSmallLetterOWithCircumflex
  , latinSmallLetterOWithTilde
  , latinSmallLetterOWithDiaeresis
  , divisionSign
  , latinSmallLetterOWithSlash
  , latinSmallLetterUWithGrave
  , latinSmallLetterUWithAcute
  , latinSmallLetterUWithCircumflex
  , latinSmallLetterUWithDiaeresis
  , latinSmallLetterYWithAcute
  , latinSmallLetterThorn
  , latinSmallLetterYWithDiaeresis
  , latinCapitalLetterAWithMacron
  , latinSmallLetterAWithMacron
  , latinCapitalLetterAWithBreve
  , latinSmallLetterAWithBreve
  , latinCapitalLetterAWithOgonek
  , latinSmallLetterAWithOgonek
  , latinCapitalLetterCWithAcute
  , latinSmallLetterCWithAcute
  , latinCapitalLetterCWithCircumflex
  , latinSmallLetterCWithCircumflex
  , latinCapitalLetterCWithDotAbove
  , latinSmallLetterCWithDotAbove
  , latinCapitalLetterCWithCaron
  , latinSmallLetterCWithCaron
  , latinCapitalLetterDWithCaron
  , latinSmallLetterDWithCaron
  , latinCapitalLetterDWithStroke
  , latinSmallLetterDWithStroke
  , latinCapitalLetterEWithMacron
  , latinSmallLetterEWithMacron
  , latinCapitalLetterEWithDotAbove
  , latinSmallLetterEWithDotAbove
  , latinCapitalLetterEWithOgonek
  , latinSmallLetterEWithOgonek
  , latinCapitalLetterEWithCaron
  , latinSmallLetterEWithCaron
  , latinCapitalLetterGWithCircumflex
  , latinSmallLetterGWithCircumflex
  , latinCapitalLetterGWithBreve
  , latinSmallLetterGWithBreve
  , latinCapitalLetterGWithDotAbove
  , latinSmallLetterGWithDotAbove
  , latinCapitalLetterGWithCedilla
  , latinCapitalLetterHWithCircumflex
  , latinSmallLetterHWithCircumflex
  , latinCapitalLetterHWithStroke
  , latinSmallLetterHWithStroke
  , latinCapitalLetterIWithTilde
  , latinSmallLetterIWithTilde
  , latinCapitalLetterIWithMacron
  , latinSmallLetterIWithMacron
  , latinCapitalLetterIWithOgonek
  , latinSmallLetterIWithOgonek
  , latinCapitalLetterIWithDotAbove
  , latinSmallLetterDotlessI
  , latinCapitalLigatureIj
  , latinSmallLigatureIj
  , latinCapitalLetterJWithCircumflex
  , latinSmallLetterJWithCircumflex
  , latinCapitalLetterKWithCedilla
  , latinSmallLetterKWithCedilla
  , latinSmallLetterKra
  , latinCapitalLetterLWithAcute
  , latinSmallLetterLWithAcute
  , latinCapitalLetterLWithCedilla
  , latinSmallLetterLWithCedilla
  , latinCapitalLetterLWithCaron
  , latinSmallLetterLWithCaron
  , latinCapitalLetterLWithMiddleDot
  , latinSmallLetterLWithMiddleDot
  , latinCapitalLetterLWithStroke
  , latinSmallLetterLWithStroke
  , latinCapitalLetterNWithAcute
  , latinSmallLetterNWithAcute
  , latinCapitalLetterNWithCedilla
  , latinSmallLetterNWithCedilla
  , latinCapitalLetterNWithCaron
  , latinSmallLetterNWithCaron
  , latinSmallLetterNPrecededByApostrophe
  , latinCapitalLetterEng
  , latinSmallLetterEng
  , latinCapitalLetterOWithMacron
  , latinSmallLetterOWithMacron
  , latinCapitalLetterOWithDoubleAcute
  , latinSmallLetterOWithDoubleAcute
  , latinCapitalLetterOe
  , latinSmallLetterOe
  , latinCapitalLetterRWithAcute
  , latinSmallLetterRWithAcute
  , latinCapitalLetterRWithCedilla
  , latinSmallLetterRWithCedilla
  , latinCapitalLetterRWithCaron
  , latinSmallLetterRWithCaron
  , latinCapitalLetterSWithAcute
  , latinSmallLetterSWithAcute
  , latinCapitalLetterSWithCircumflex
  , latinSmallLetterSWithCircumflex
  , latinCapitalLetterSWithCedilla
  , latinSmallLetterSWithCedilla
  , latinCapitalLetterSWithCaron
  , latinSmallLetterSWithCaron
  , latinCapitalLetterTWithCedilla
  , latinSmallLetterTWithCedilla
  , latinCapitalLetterTWithCaron
  , latinSmallLetterTWithCaron
  , latinCapitalLetterTWithStroke
  , latinSmallLetterTWithStroke
  , latinCapitalLetterUWithTilde
  , latinSmallLetterUWithTilde
  , latinCapitalLetterUWithMacron
  , latinSmallLetterUWithMacron
  , latinCapitalLetterUWithBreve
  , latinSmallLetterUWithBreve
  , latinCapitalLetterUWithRingAbove
  , latinSmallLetterUWithRingAbove
  , latinCapitalLetterUWithDoubleAcute
  , latinSmallLetterUWithDoubleAcute
  , latinCapitalLetterUWithOgonek
  , latinSmallLetterUWithOgonek
  , latinCapitalLetterWWithCircumflex
  , latinSmallLetterWWithCircumflex
  , latinCapitalLetterYWithCircumflex
  , latinSmallLetterYWithCircumflex
  , latinCapitalLetterYWithDiaeresis
  , latinCapitalLetterZWithAcute
  , latinSmallLetterZWithAcute
  , latinCapitalLetterZWithDotAbove
  , latinSmallLetterZWithDotAbove
  , latinCapitalLetterZWithCaron
  , latinSmallLetterZWithCaron
  , latinSmallLetterTurnedDelta
  , latinSmallFWithHookFunctionFlorin
  , latinCapitalLetterZWithStroke
  , latinSmallLetterGWithAcute
  , latinSmallLetterDotlessJ
  , modifierLetterCircumflexAccent
  , caron
  , modifierLetterPlusSign
  , modifierLetterMinusSign
  , breve
  , dotAbove
  , ringAbove
  , ogonek
  , smallTilde
  , doubleAcuteAccent
  , combiningInvertedBreve
  , combiningLeftAngleAbove
  , combiningPlusSignBelow
  , combiningMinusSignBelow
  , combiningLowLine
  , combiningEqualsSignBelow
  , combiningLeftAngleBelow
  , combiningAlmostEqualToAbove
  , greekCapitalLetterAlpha
  , greekCapitalLetterBeta
  , greekCapitalLetterGamma
  , greekCapitalLetterDelta
  , greekCapitalLetterEpsilon
  , greekCapitalLetterZeta
  , greekCapitalLetterEta
  , greekCapitalLetterTheta
  , greekCapitalLetterIota
  , greekCapitalLetterKappa
  , greekCapitalLetterLambda
  , greekCapitalLetterMu
  , greekCapitalLetterNu
  , greekCapitalLetterXi
  , greekCapitalLetterOmicron
  , greekCapitalLetterPi
  , greekCapitalLetterRho
  , greekCapitalLetterSigma
  , greekCapitalLetterTau
  , greekCapitalLetterUpsilon
  , greekCapitalLetterPhi
  , greekCapitalLetterChi
  , greekCapitalLetterPsi
  , greekCapitalLetterOmega
  , greekSmallLetterAlpha
  , greekSmallLetterBeta
  , greekSmallLetterGamma
  , greekSmallLetterDelta
  , greekSmallLetterEpsilon
  , greekSmallLetterZeta
  , greekSmallLetterEta
  , greekSmallLetterTheta
  , greekSmallLetterIota
  , greekSmallLetterKappa
  , greekSmallLetterLambda
  , greekSmallLetterMu
  , greekSmallLetterNu
  , greekSmallLetterXi
  , greekSmallLetterOmicron
  , greekSmallLetterPi
  , greekSmallLetterRho
  , greekSmallLetterFinalSigma
  , greekSmallLetterSigma
  , greekSmallLetterTau
  , greekSmallLetterUpsilon
  , greekSmallLetterPhi
  , greekSmallLetterChi
  , greekSmallLetterPsi
  , greekSmallLetterOmega
  , greekThetaSymbol
  , greekUpsilonWithHookSymbol
  , greekPhiSymbol
  , greekPiSymbol
  , greekLetterDigamma
  , greekSmallLetterDigamma
  , greekKappaSymbol
  , greekRhoSymbol
  , greekLunateEpsilonSymbol
  , greekReversedLunateEpsilonSymbol
  , cyrillicCapitalLetterIo
  , cyrillicCapitalLetterDje
  , cyrillicCapitalLetterGje
  , cyrillicCapitalLetterUkrainianIe
  , cyrillicCapitalLetterDze
  , cyrillicCapitalLetterByelorussianUkrainianI
  , cyrillicCapitalLetterYi
  , cyrillicCapitalLetterJe
  , cyrillicCapitalLetterLje
  , cyrillicCapitalLetterNje
  , cyrillicCapitalLetterTshe
  , cyrillicCapitalLetterKje
  , cyrillicCapitalLetterShortU
  , cyrillicCapitalLetterDzhe
  , cyrillicCapitalLetterA
  , cyrillicCapitalLetterBe
  , cyrillicCapitalLetterVe
  , cyrillicCapitalLetterGhe
  , cyrillicCapitalLetterDe
  , cyrillicCapitalLetterIe
  , cyrillicCapitalLetterZhe
  , cyrillicCapitalLetterZe
  , cyrillicCapitalLetterI
  , cyrillicCapitalLetterShortI
  , cyrillicCapitalLetterKa
  , cyrillicCapitalLetterEl
  , cyrillicCapitalLetterEm
  , cyrillicCapitalLetterEn
  , cyrillicCapitalLetterO
  , cyrillicCapitalLetterPe
  , cyrillicCapitalLetterEr
  , cyrillicCapitalLetterEs
  , cyrillicCapitalLetterTe
  , cyrillicCapitalLetterU
  , cyrillicCapitalLetterEf
  , cyrillicCapitalLetterHa
  , cyrillicCapitalLetterTse
  , cyrillicCapitalLetterChe
  , cyrillicCapitalLetterSha
  , cyrillicCapitalLetterShcha
  , cyrillicCapitalLetterHardSign
  , cyrillicCapitalLetterYeru
  , cyrillicCapitalLetterSoftSign
  , cyrillicCapitalLetterE
  , cyrillicCapitalLetterYu
  , cyrillicCapitalLetterYa
  , cyrillicSmallLetterA
  , cyrillicSmallLetterBe
  , cyrillicSmallLetterVe
  , cyrillicSmallLetterGhe
  , cyrillicSmallLetterDe
  , cyrillicSmallLetterIe
  , cyrillicSmallLetterZhe
  , cyrillicSmallLetterZe
  , cyrillicSmallLetterI
  , cyrillicSmallLetterShortI
  , cyrillicSmallLetterKa
  , cyrillicSmallLetterEl
  , cyrillicSmallLetterEm
  , cyrillicSmallLetterEn
  , cyrillicSmallLetterO
  , cyrillicSmallLetterPe
  , cyrillicSmallLetterEr
  , cyrillicSmallLetterEs
  , cyrillicSmallLetterTe
  , cyrillicSmallLetterU
  , cyrillicSmallLetterEf
  , cyrillicSmallLetterHa
  , cyrillicSmallLetterTse
  , cyrillicSmallLetterChe
  , cyrillicSmallLetterSha
  , cyrillicSmallLetterShcha
  , cyrillicSmallLetterHardSign
  , cyrillicSmallLetterYeru
  , cyrillicSmallLetterSoftSign
  , cyrillicSmallLetterE
  , cyrillicSmallLetterYu
  , cyrillicSmallLetterYa
  , cyrillicSmallLetterIo
  , cyrillicSmallLetterDje
  , cyrillicSmallLetterGje
  , cyrillicSmallLetterUkrainianIe
  , cyrillicSmallLetterDze
  , cyrillicSmallLetterByelorussianUkrainianI
  , cyrillicSmallLetterYi
  , cyrillicSmallLetterJe
  , cyrillicSmallLetterLje
  , cyrillicSmallLetterNje
  , cyrillicSmallLetterTshe
  , cyrillicSmallLetterKje
  , cyrillicSmallLetterShortU
  , cyrillicSmallLetterDzhe
  , arabicPercentSign
  , canadianSyllabicsFinalPlus
  , modifierLetterSmallDelta
  , latinSmallLetterDelta
  , enSpace
  , emSpace
  , threePerEmSpace
  , fourPerEmSpace
  , figureSpace
  , punctuationSpace
  , thinSpace
  , hairSpace
  , zeroWidthSpace
  , zeroWidthNonJoiner
  , zeroWidthJoiner
  , leftToRightMark
  , rightToLeftMark
  , hyphen
  , enDash
  , emDash
  , horizontalBar
  , doubleVerticalLine
  , leftSingleQuotationMark
  , rightSingleQuotationMark
  , singleLow9QuotationMark
  , leftDoubleQuotationMark
  , rightDoubleQuotationMark
  , doubleLow9QuotationMark
  , dagger
  , doubleDagger
  , bullet
  , twoDotLeader
  , horizontalEllipsis
  , perMilleSignPerThousandSign
  , perTenThousandSign
  , prime
  , doublePrime
  , triplePrime
  , reversedPrime
  , singleLeftPointingAngleQuotationMark
  , singleRightPointingAngleQuotationMark
  , overline
  , caretInsertionPoint
  , hyphenBullet
  , fractionSlash
  , reversedSemicolon
  , commercialMinusSign
  , quadruplePrime
  , mediumMathematicalSpace
  , wordJoiner
  , functionApplication
  , invisibleTimes
  , invisibleSeparator
  , superscriptPlusSign
  , superscriptMinus
  , superscriptEqualsSign
  , subscriptPlusSign
  , subscriptMinus
  , subscriptEqualsSign
  , euroCurrencySign
  , colonSign
  , cruzeiroSign
  , frenchFrancSign
  , liraSign
  , millSign
  , nairaSign
  , pesetaSign
  , rupeeSign
  , wonSign
  , newSheqelSign
  , dongSign
  , euroSign
  , kipSign
  , tugrikSign
  , drachmaSign
  , germanPennySymbol
  , pesoSign
  , guaraniSign
  , australSign
  , hryvniaSign
  , cediSign
  , livreTournoisSign
  , spesmiloSign
  , tengeSign
  , indianRupeeSign
  , turkishLiraSign
  , nordicMarkSign
  , manatSign
  , rubleSign
  , lariSign
  , bitcoinSign
  , combiningThreeDotsAbove
  , combiningFourDotsAbove
  , doubleStruckCapitalC
  , careOf
  , scriptSmallG
  , scriptCapitalH
  , blackLetterCapitalH
  , doubleStruckCapitalH
  , planckConstant
  , planckConstantOverTwoPi
  , scriptCapitalI
  , blackLetterCapitalI
  , scriptCapitalL
  , scriptSmallL
  , doubleStruckCapitalN
  , numeroSign
  , soundRecordingCopyright
  , scriptCapitalP
  , doubleStruckCapitalP
  , doubleStruckCapitalQ
  , scriptCapitalR
  , blackLetterCapitalR
  , doubleStruckCapitalR
  , prescriptionTake
  , tradeMarkSign
  , doubleStruckCapitalZ
  , ohmSign
  , invertedOhmSign
  , blackLetterCapitalZ
  , turnedGreekSmallLetterIota
  , angstromSign
  , scriptCapitalB
  , blackLetterCapitalC
  , scriptSmallE
  , scriptCapitalE
  , scriptCapitalF
  , scriptCapitalM
  , scriptSmallO
  , alefSymbol
  , betSymbol
  , gimelSymbol
  , daletSymbol
  , doubleStruckNArySummation
  , doubleStruckItalicCapitalD
  , doubleStruckItalicSmallD
  , doubleStruckItalicSmallE
  , doubleStruckItalicSmallI
  , vulgarFractionOneSeventh
  , vulgarFractionOneNinth
  , vulgarFractionOneTenth
  , vulgarFractionOneThird
  , vulgarFractionTwoThirds
  , vulgarFractionOneFifth
  , vulgarFractionTwoFifths
  , vulgarFractionThreeFifths
  , vulgarFractionFourFifths
  , vulgarFractionOneSixth
  , vulgarFractionFiveSixths
  , vulgarFractionOneEighth
  , vulgarFractionThreeEighths
  , vulgarFractionFiveEighths
  , vulgarFractionSevenEighths
  , fractionNumeratorOne
  , leftwardsArrow
  , upwardsArrow
  , rightwardsArrow
  , downwardsArrow
  , leftRightArrow
  , upDownArrow
  , northWestArrow
  , northEastArrow
  , southEastArrow
  , southWestArrow
  , leftwardsArrowWithStroke
  , rightwardsArrowWithStroke
  , rightwardsWaveArrow
  , leftwardsTwoHeadedArrow
  , upwardsTwoHeadedArrow
  , rightwardsTwoHeadedArrow
  , downwardsTwoHeadedArrow
  , leftwardsArrowWithTail
  , rightwardsArrowWithTail
  , leftwardsArrowFromBar
  , upwardsArrowFromBar
  , rightwardsArrowFromBar
  , downwardsArrowFromBar
  , leftwardsArrowWithHook
  , rightwardsArrowWithHook
  , leftwardsArrowWithLoop
  , rightwardsArrowWithLoop
  , leftRightWaveArrow
  , leftRightArrowWithStroke
  , upwardsArrowWithTipLeftwards
  , upwardsArrowWithTipRightwards
  , downwardsArrowWithTipLeftwards
  , downwardsArrowWithTipRightwards
  , downwardsArrowWithCornerLeftwards
  , anticlockwiseTopSemicircleArrow
  , clockwiseTopSemicircleArrow
  , anticlockwiseOpenCircleArrow
  , clockwiseOpenCircleArrow
  , leftwardsHarpoonWithBarbUpwards
  , leftwardsHarpoonWithBarbDownwards
  , upwardsHarpoonWithBarbRightwards
  , upwardsHarpoonWithBarbLeftwards
  , rightwardsHarpoonWithBarbUpwards
  , rightwardsHarpoonWithBarbDownwards
  , downwardsHarpoonWithBarbRightwards
  , downwardsHarpoonWithBarbLeftwards
  , rightwardsArrowOverLeftwardsArrow
  , upwardsArrowLeftwardsOfDownwardsArrow
  , leftwardsArrowOverRightwardsArrow
  , leftwardsPairedArrows
  , upwardsPairedArrows
  , rightwardsPairedArrows
  , downwardsPairedArrows
  , leftwardsHarpoonOverRightwardsHarpoon
  , rightwardsHarpoonOverLeftwardsHarpoon
  , leftwardsDoubleArrowWithStroke
  , leftRightDoubleArrowWithStroke
  , rightwardsDoubleArrowWithStroke
  , leftwardsDoubleArrow
  , upwardsDoubleArrow
  , rightwardsDoubleArrow
  , downwardsDoubleArrow
  , leftRightDoubleArrow
  , upDownDoubleArrow
  , northWestDoubleArrow
  , northEastDoubleArrow
  , southEastDoubleArrow
  , southWestDoubleArrow
  , leftwardsTripleArrow
  , rightwardsTripleArrow
  , rightwardsSquiggleArrow
  , leftwardsArrowToBar
  , rightwardsArrowToBar
  , downwardsArrowLeftwardsOfUpwardsArrow
  , leftwardsOpenHeadedArrow
  , rightwardsOpenHeadedArrow
  , leftRightOpenHeadedArrow
  , forAll
  , complement
  , partialDifferential
  , thereExists
  , thereDoesNotExist
  , emptySet
  , nabla
  , elementOf
  , notAnElementOf
  , containsAsMember
  , doesNotContainAsMember
  , nAryProduct
  , nAryCoproduct
  , nArySummation
  , minusSign
  , minusOrPlusSign
  , dotPlus
  , divisionSlash
  , setMinus
  , asteriskOperator
  , ringOperator
  , squareRoot
  , cubeRoot
  , fourthRoot
  , proportionalTo
  , infinity
  , rightAngle
  , angle
  , measuredAngle
  , sphericalAngle
  , divides
  , doesNotDivide
  , parallelTo
  , notParallelTo
  , logicalAnd
  , logicalOr
  , intersection
  , union
  , integral
  , doubleIntegral
  , tripleIntegral
  , contourIntegral
  , surfaceIntegral
  , volumeIntegral
  , clockwiseIntegral
  , clockwiseContourIntegral
  , anticlockwiseContourIntegral
  , therefore
  , because
  , ratio
  , proportion
  , dotMinus
  , geometricProportion
  , homothetic
  , tildeOperator
  , reversedTilde
  , invertedLazyS
  , sineWave
  , wreathProduct
  , notTilde
  , minusTilde
  , asymptoticallyEqualTo
  , notAsymptoticallyEqualTo
  , approximatelyEqualTo
  , approximatelyButNotActuallyEqualTo
  , neitherApproximatelyNorActuallyEqualTo
  , almostEqualTo
  , notAlmostEqualTo
  , almostEqualOrEqualTo
  , tripleTilde
  , allEqualTo
  , equivalentTo
  , geometricallyEquivalentTo
  , differenceBetween
  , approachesTheLimit
  , geometricallyEqualTo
  , approximatelyEqualToOrTheImageOf
  , imageOfOrApproximatelyEqualTo
  , colonEquals
  , equalsColon
  , ringInEqualTo
  , ringEqualTo
  , estimates
  , equiangularTo
  , starEquals
  , deltaEqualTo
  , equalToByDefinition
  , questionedEqualTo
  , notEqualTo
  , identicalTo
  , notIdenticalTo
  , lessThanOrEqualTo
  , greaterThanOrEqualTo
  , lessThanOverEqualTo
  , greaterThanOverEqualTo
  , lessThanButNotEqualTo
  , greaterThanButNotEqualTo
  , muchLessThan
  , muchGreaterThan
  , between
  , notEquivalentTo
  , notLessThan
  , notGreaterThan
  , neitherLessThanNorEqualTo
  , neitherGreaterThanNorEqualTo
  , lessThanOrEquivalentTo
  , greaterThanOrEquivalentTo
  , neitherLessThanNorEquivalentTo
  , neitherGreaterThanNorEquivalentTo
  , lessThanOrGreaterThan
  , greaterThanOrLessThan
  , neitherLessThanNorGreaterThan
  , neitherGreaterThanNorLessThan
  , precedes
  , succeeds
  , precedesOrEqualTo
  , succeedsOrEqualTo
  , precedesOrEquivalentTo
  , succeedsOrEquivalentTo
  , doesNotPrecede
  , doesNotSucceed
  , subsetOf
  , supersetOf
  , notASubsetOf
  , notASupersetOf
  , subsetOfOrEqualTo
  , supersetOfOrEqualTo
  , neitherASubsetOfNorEqualTo
  , neitherASupersetOfNorEqualTo
  , subsetOfWithNotEqualTo
  , supersetOfWithNotEqualTo
  , multisetMultiplication
  , multisetUnion
  , squareImageOf
  , squareOriginalOf
  , squareImageOfOrEqualTo
  , squareOriginalOfOrEqualTo
  , squareCap
  , squareCup
  , circledPlus
  , circledMinus
  , circledTimes
  , circledDivisionSlash
  , circledDotOperator
  , circledRingOperator
  , circledAsteriskOperator
  , circledEquals
  , circledDash
  , squaredPlus
  , squaredMinus
  , squaredTimes
  , squaredDotOperator
  , rightTack
  , leftTack
  , downTack
  , upTack
  , models
  , true
  , forces
  , tripleVerticalBarRightTurnstile
  , doubleVerticalBarDoubleRightTurnstile
  , doesNotProve
  , notTrue
  , doesNotForce
  , negatedDoubleVerticalBarDoubleRightTurnstile
  , precedesUnderRelation
  , normalSubgroupOf
  , containsAsNormalSubgroup
  , normalSubgroupOfOrEqualTo
  , containsAsNormalSubgroupOrEqualTo
  , originalOf
  , imageOf
  , multimap
  , hermitianConjugateMatrix
  , intercalate
  , xor
  , nor
  , rightAngleWithArc
  , rightTriangle
  , nAryLogicalAnd
  , nAryLogicalOr
  , nAryIntersection
  , nAryUnion
  , diamondOperator
  , dotOperator
  , starOperator
  , divisionTimes
  , bowtie
  , leftNormalFactorSemidirectProduct
  , rightNormalFactorSemidirectProduct
  , leftSemidirectProduct
  , rightSemidirectProduct
  , reversedTildeEquals
  , curlyLogicalOr
  , curlyLogicalAnd
  , doubleSubset
  , doubleSuperset
  , doubleIntersection
  , doubleUnion
  , pitchfork
  , equalAndParallelTo
  , lessThanWithDot
  , greaterThanWithDot
  , veryMuchLessThan
  , veryMuchGreaterThan
  , lessThanEqualToOrGreaterThan
  , greaterThanEqualToOrLessThan
  , equalToOrLessThan
  , equalToOrGreaterThan
  , equalToOrPrecedes
  , equalToOrSucceeds
  , doesNotPrecedeOrEqual
  , doesNotSucceedOrEqual
  , notSquareImageOfOrEqualTo
  , notSquareOriginalOfOrEqualTo
  , squareImageOfOrNotEqualTo
  , squareOriginalOfOrNotEqualTo
  , lessThanButNotEquivalentTo
  , greaterThanButNotEquivalentTo
  , precedesButNotEquivalentTo
  , succeedsButNotEquivalentTo
  , notNormalSubgroupOf
  , doesNotContainAsNormalSubgroup
  , notNormalSubgroupOfOrEqualTo
  , doesNotContainAsNormalSubgroupOrEqual
  , verticalEllipsis
  , midlineHorizontalEllipsis
  , upRightDiagonalEllipsis
  , downRightDiagonalEllipsis
  , elementOfWithLongHorizontalStroke
  , elementOfWithVerticalBarAtEndOfHorizontalStroke
  , smallElementOfWithVerticalBarAtEndOfHorizontalStroke
  , elementOfWithDotAbove
  , elementOfWithOverbar
  , smallElementOfWithOverbar
  , elementOfWithTwoHorizontalStrokes
  , containsWithLongHorizontalStroke
  , containsWithVerticalBarAtEndOfHorizontalStroke
  , smallContainsWithVerticalBarAtEndOfHorizontalStroke
  , containsWithOverbar
  , smallContainsWithOverbar
  , projective
  , perspective
  , leftCeiling
  , rightCeiling
  , leftFloor
  , rightFloor
  , bottomRightCrop
  , bottomLeftCrop
  , topRightCrop
  , topLeftCrop
  , reversedNotSign
  , arc
  , segment
  , telephoneRecorder
  , positionIndicator
  , topLeftCorner
  , topRightCorner
  , bottomLeftCorner
  , bottomRightCorner
  , frown
  , smile
  , leftPointingAngleBracket
  , rightPointingAngleBracket
  , cylindricity
  , allAroundProfile
  , aplFunctionalSymbolIBeam
  , aplFunctionalSymbolQuadEqual
  , aplFunctionalSymbolCircleStile
  , aplFunctionalSymbolSlashBar
  , aplFunctionalSymbolQuadLessThan
  , aplFunctionalSymbolQuadGreaterThan
  , aplFunctionalSymbolDeltaStile
  , aplFunctionalSymbolQuadDelta
  , aplFunctionalSymbolDeltaUnderbar
  , aplFunctionalSymbolGreaterThanDiaeresis
  , aplFunctionalSymbolQuadNotEqual
  , rightAngleWithDownwardsZigzagArrow
  , upperLeftOrLowerRightCurlyBracketSection
  , upperRightOrLowerLeftCurlyBracketSection
  , summationTop
  , summationBottom
  , topSquareBracket
  , bottomSquareBracket
  , bottomSquareBracketOverTopSquareBracket
  , topParenthesis
  , bottomParenthesis
  , topCurlyBracket
  , bottomCurlyBracket
  , whiteTrapezium
  , electricalIntersection
  , openBox
  , circledLatinCapitalLetterS
  , boxDrawingsLightHorizontal
  , boxDrawingsLightVertical
  , boxDrawingsLightDownAndRight
  , boxDrawingsLightDownAndLeft
  , boxDrawingsLightUpAndRight
  , boxDrawingsLightUpAndLeft
  , boxDrawingsLightVerticalAndRight
  , boxDrawingsLightVerticalAndLeft
  , boxDrawingsLightDownAndHorizontal
  , boxDrawingsLightUpAndHorizontal
  , boxDrawingsLightVerticalAndHorizontal
  , boxDrawingsDoubleHorizontal
  , boxDrawingsDoubleVertical
  , boxDrawingsDownSingleAndRightDouble
  , boxDrawingsDownDoubleAndRightSingle
  , boxDrawingsDoubleDownAndRight
  , boxDrawingsDownSingleAndLeftDouble
  , boxDrawingsDownDoubleAndLeftSingle
  , boxDrawingsDoubleDownAndLeft
  , boxDrawingsUpSingleAndRightDouble
  , boxDrawingsUpDoubleAndRightSingle
  , boxDrawingsDoubleUpAndRight
  , boxDrawingsUpSingleAndLeftDouble
  , boxDrawingsUpDoubleAndLeftSingle
  , boxDrawingsDoubleUpAndLeft
  , boxDrawingsVerticalSingleAndRightDouble
  , boxDrawingsVerticalDoubleAndRightSingle
  , boxDrawingsDoubleVerticalAndRight
  , boxDrawingsVerticalSingleAndLeftDouble
  , boxDrawingsVerticalDoubleAndLeftSingle
  , boxDrawingsDoubleVerticalAndLeft
  , boxDrawingsDownSingleAndHorizontalDouble
  , boxDrawingsDownDoubleAndHorizontalSingle
  , boxDrawingsDoubleDownAndHorizontal
  , boxDrawingsUpSingleAndHorizontalDouble
  , boxDrawingsUpDoubleAndHorizontalSingle
  , boxDrawingsDoubleUpAndHorizontal
  , boxDrawingsVerticalSingleAndHorizontalDouble
  , boxDrawingsVerticalDoubleAndHorizontalSingle
  , boxDrawingsDoubleVerticalAndHorizontal
  , upperHalfBlock
  , lowerHalfBlock
  , fullBlock
  , lightShade
  , mediumShade
  , darkShade
  , blackSquare
  , whiteSquare
  , whiteSquareWithRoundedCorners
  , whiteSquareContainingBlackSmallSquare
  , squareWithHorizontalFill
  , squareWithVerticalFill
  , squareWithOrthogonalCrosshatchFill
  , squareWithUpperLeftToLowerRightFill
  , squareWithUpperRightToLowerLeftFill
  , squareWithDiagonalCrosshatchFill
  , blackSmallSquare
  , whiteSmallSquare
  , blackRectangle
  , whiteRectangle
  , blackVerticalRectangle
  , whiteVerticalRectangle
  , blackParallelogram
  , whiteParallelogram
  , blackUpPointingTriangle
  , whiteUpPointingTriangle
  , blackUpPointingSmallTriangle
  , whiteUpPointingSmallTriangle
  , blackRightPointingTriangle
  , whiteRightPointingTriangle
  , blackRightPointingSmallTriangle
  , whiteRightPointingSmallTriangle
  , blackDownPointingTriangle
  , whiteDownPointingTriangle
  , blackDownPointingSmallTriangle
  , whiteDownPointingSmallTriangle
  , blackLeftPointingTriangle
  , whiteLeftPointingTriangle
  , blackLeftPointingSmallTriangle
  , whiteLeftPointingSmallTriangle
  , blackDiamond
  , whiteDiamond
  , whiteDiamondContainingBlackSmallDiamond
  , lozenge
  , whiteCircle
  , dottedCircle
  , circleWithVerticalFill
  , blackCircle
  , circleWithLeftHalfBlack
  , circleWithRightHalfBlack
  , circleWithLowerHalfBlack
  , circleWithUpperHalfBlack
  , circleWithUpperRightQuadrantBlack
  , circleWithAllButUpperLeftQuadrantBlack
  , leftHalfBlackCircle
  , rightHalfBlackCircle
  , inverseWhiteCircle
  , upperHalfInverseWhiteCircle
  , lowerHalfInverseWhiteCircle
  , upperLeftQuadrantCircularArc
  , upperRightQuadrantCircularArc
  , lowerRightQuadrantCircularArc
  , lowerLeftQuadrantCircularArc
  , upperHalfCircle
  , lowerHalfCircle
  , blackLowerRightTriangle
  , blackLowerLeftTriangle
  , blackUpperLeftTriangle
  , blackUpperRightTriangle
  , squareWithLeftHalfBlack
  , squareWithRightHalfBlack
  , squareWithUpperLeftDiagonalHalfBlack
  , squareWithLowerRightDiagonalHalfBlack
  , whiteSquareWithVerticalBisectingLine
  , whiteUpPointingTriangleWithDot
  , upPointingTriangleWithLeftHalfBlack
  , upPointingTriangleWithRightHalfBlack
  , largeCircle
  , whiteSquareWithUpperLeftQuadrant
  , whiteSquareWithLowerLeftQuadrant
  , whiteSquareWithLowerRightQuadrant
  , whiteSquareWithUpperRightQuadrant
  , whiteCircleWithUpperLeftQuadrant
  , whiteCircleWithLowerLeftQuadrant
  , whiteCircleWithLowerRightQuadrant
  , whiteCircleWithUpperRightQuadrant
  , upperLeftTriangle
  , upperRightTriangle
  , lowerLeftTriangle
  , whiteMediumSquare
  , blackMediumSquare
  , whiteMediumSmallSquare
  , blackMediumSmallSquare
  , lowerRightTriangle
  , blackStar
  , whiteStar
  , blackTelephone
  , trigramForHeaven
  , trigramForLake
  , trigramForFire
  , trigramForThunder
  , trigramForWind
  , trigramForWater
  , trigramForMountain
  , trigramForEarth
  , femaleSign
  , maleSign
  , blackSpadeSuit
  , whiteDiamondSuit
  , blackClubSuit
  , blackHeartSuit
  , blackDiamondSuit
  , eighthNote
  , musicFlatSign
  , musicNaturalSign
  , musicSharpSign
  , whiteCircleWithDotRight
  , whiteCircleWithTwoDots
  , blackCircleWithWhiteDotRight
  , blackCircleWithTwoWhiteDots
  , mediumWhiteCircle
  , mediumBlackCircle
  , mediumSmallWhiteCircle
  , squaredKey
  , whiteDiamondInSquare
  , heavyWhiteDownPointingTriangle
  , squaredSaltire
  , fallingDiagonalInWhiteCircleInBlackSquare
  , squareFourCorners
  , cupOnBlackSquare
  , checkMark
  , ballotX
  , malteseCross
  , circledWhiteStar
  , sixPointedBlackStar
  , circledOpenCentreEightPointedStar
  , shadowedWhiteCircle
  , lowerRightDropShadowedWhiteSquare
  , upperRightDropShadowedWhiteSquare
  , lowerRightShadowedWhiteSquare
  , upperRightShadowedWhiteSquare
  , blackDiamondMinusWhiteX
  , lightVerticalBar
  , lightLeftTortoiseShellBracketOrnament
  , lightRightTortoiseShellBracketOrnament
  , heavyPlusSign
  , heavyMinusSign
  , heavyDivisionSign
  , threeDimensionalAngle
  , whiteTriangleContainingSmallWhiteTriangle
  , mathematicalRisingDiagonal
  , longDivision
  , mathematicalFallingDiagonal
  , squaredLogicalAnd
  , squaredLogicalOr
  , whiteDiamondWithCentredDot
  , upTackWithCircleAbove
  , lozengeDividedByHorizontalRule
  , whiteConcaveSidedDiamond
  , whiteConcaveSidedDiamondWithLeftwardsTick
  , whiteConcaveSidedDiamondWithRightwardsTick
  , whiteSquareWithLeftwardsTick
  , whiteSquareWithRightwardsTick
  , mathematicalLeftWhiteSquareBracket
  , mathematicalRightWhiteSquareBracket
  , mathematicalLeftAngleBracket
  , mathematicalRightAngleBracket
  , mathematicalLeftDoubleAngleBracket
  , mathematicalRightDoubleAngleBracket
  , mathematicalLeftWhiteTortoiseShellBracket
  , mathematicalRightWhiteTortoiseShellBracket
  , longLeftwardsArrow
  , longRightwardsArrow
  , longLeftRightArrow
  , longLeftwardsDoubleArrow
  , longRightwardsDoubleArrow
  , longLeftRightDoubleArrow
  , longRightwardsArrowFromBar
  , longRightwardsSquiggleArrow
  , leftwardsDoubleArrowWithVerticalStroke
  , rightwardsDoubleArrowWithVerticalStroke
  , leftRightDoubleArrowWithVerticalStroke
  , rightwardsTwoHeadedArrowFromBar
  , leftwardsDoubleDashArrow
  , rightwardsDoubleDashArrow
  , leftwardsTripleDashArrow
  , rightwardsTripleDashArrow
  , rightwardsTwoHeadedTripleDashArrow
  , rightwardsArrowWithDottedStem
  , upwardsArrowToBar
  , downwardsArrowToBar
  , rightwardsTwoHeadedArrowWithTail
  , leftwardsArrowTail
  , rightwardsArrowTail
  , leftwardsDoubleArrowTail
  , rightwardsDoubleArrowTail
  , leftwardsArrowToBlackDiamond
  , rightwardsArrowToBlackDiamond
  , leftwardsArrowFromBarToBlackDiamond
  , rightwardsArrowFromBarToBlackDiamond
  , northWestArrowWithHook
  , northEastArrowWithHook
  , southEastArrowWithHook
  , southWestArrowWithHook
  , northWestArrowAndNorthEastArrow
  , northEastArrowAndSouthEastArrow
  , southEastArrowAndSouthWestArrow
  , southWestArrowAndNorthWestArrow
  , risingDiagonalCrossingFallingDiagonal
  , fallingDiagonalCrossingRisingDiagonal
  , fallingDiagonalCrossingNorthEastArrow
  , risingDiagonalCrossingSouthEastArrow
  , waveArrowPointingDirectlyRight
  , arrowPointingRightwardsThenCurvingDownwards
  , arrowPointingDownwardsThenCurvingLeftwards
  , arrowPointingDownwardsThenCurvingRightwards
  , rightSideArcClockwiseArrow
  , leftSideArcAnticlockwiseArrow
  , topArcAnticlockwiseArrow
  , bottomArcAnticlockwiseArrow
  , topArcClockwiseArrowWithMinus
  , topArcAnticlockwiseArrowWithPlus
  , rightwardsArrowWithPlusBelow
  , leftRightArrowThroughSmallCircle
  , upwardsTwoHeadedArrowFromSmallCircle
  , leftBarbUpRightBarbDownHarpoon
  , leftBarbDownRightBarbUpHarpoon
  , upBarbRightDownBarbLeftHarpoon
  , upBarbLeftDownBarbRightHarpoon
  , leftBarbUpRightBarbUpHarpoon
  , upBarbRightDownBarbRightHarpoon
  , leftBarbDownRightBarbDownHarpoon
  , upBarbLeftDownBarbLeftHarpoon
  , leftwardsHarpoonWithBarbUpToBar
  , rightwardsHarpoonWithBarbUpToBar
  , upwardsHarpoonWithBarbRightToBar
  , downwardsHarpoonWithBarbRightToBar
  , leftwardsHarpoonWithBarbDownToBar
  , rightwardsHarpoonWithBarbDownToBar
  , upwardsHarpoonWithBarbLeftToBar
  , downwardsHarpoonWithBarbLeftToBar
  , leftwardsHarpoonWithBarbUpFromBar
  , rightwardsHarpoonWithBarbUpFromBar
  , upwardsHarpoonWithBarbRightFromBar
  , downwardsHarpoonWithBarbRightFromBar
  , leftwardsHarpoonWithBarbDownFromBar
  , rightwardsHarpoonWithBarbDownFromBar
  , upwardsHarpoonWithBarbLeftFromBar
  , downwardsHarpoonWithBarbLeftFromBar
  , leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown
  , upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight
  , rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown
  , downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight
  , leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp
  , leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown
  , rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp
  , rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown
  , leftwardsHarpoonWithBarbUpAboveLongDash
  , leftwardsHarpoonWithBarbDownBelowLongDash
  , rightwardsHarpoonWithBarbUpAboveLongDash
  , rightwardsHarpoonWithBarbDownBelowLongDash
  , upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight
  , downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight
  , rightDoubleArrowWithRoundedHead
  , equalsSignAboveRightwardsArrow
  , tildeOperatorAboveRightwardsArrow
  , leftwardsArrowAboveTildeOperator
  , rightwardsArrowAboveTildeOperator
  , rightwardsArrowAboveAlmostEqualTo
  , lessThanAboveLeftwardsArrow
  , leftwardsArrowThroughLessThan
  , greaterThanAboveRightwardsArrow
  , subsetAboveRightwardsArrow
  , supersetAboveLeftwardsArrow
  , leftFishTail
  , rightFishTail
  , upFishTail
  , downFishTail
  , leftWhiteParenthesis
  , rightWhiteParenthesis
  , leftSquareBracketWithUnderbar
  , rightSquareBracketWithUnderbar
  , leftSquareBracketWithTickInTopCorner
  , rightSquareBracketWithTickInBottomCorner
  , leftSquareBracketWithTickInBottomCorner
  , rightSquareBracketWithTickInTopCorner
  , leftAngleBracketWithDot
  , rightAngleBracketWithDot
  , leftArcLessThanBracket
  , rightArcGreaterThanBracket
  , doubleLeftArcGreaterThanBracket
  , doubleRightArcLessThanBracket
  , verticalZigzagLine
  , measuredAngleOpeningLeft
  , rightAngleVariantWithSquare
  , measuredRightAngleWithDot
  , angleWithSInside
  , acuteAngle
  , sphericalAngleOpeningLeft
  , sphericalAngleOpeningUp
  , turnedAngle
  , reversedAngle
  , angleWithUnderbar
  , reversedAngleWithUnderbar
  , obliqueAngleOpeningUp
  , obliqueAngleOpeningDown
  , measuredAngleWithOpenArmEndingInArrowPointingUpAndRight
  , measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft
  , measuredAngleWithOpenArmEndingInArrowPointingDownAndRight
  , measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft
  , measuredAngleWithOpenArmEndingInArrowPointingRightAndUp
  , measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp
  , measuredAngleWithOpenArmEndingInArrowPointingRightAndDown
  , measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown
  , reversedEmptySet
  , emptySetWithOverbar
  , emptySetWithSmallCircleAbove
  , emptySetWithRightArrowAbove
  , emptySetWithLeftArrowAbove
  , circleWithHorizontalBar
  , circledVerticalBar
  , circledParallel
  , circledReverseSolidus
  , circledPerpendicular
  , circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar
  , circleWithSuperimposedX
  , circledAnticlockwiseRotatedDivisionSign
  , circledWhiteBullet
  , circledBullet
  , circledLessThan
  , circledGreaterThan
  , circleWithSmallCircleToTheRight
  , circleWithTwoHorizontalStrokesToTheRight
  , squaredRisingDiagonalSlash
  , squaredFallingDiagonalSlash
  , squaredAsterisk
  , squaredSmallCircle
  , squaredSquare
  , twoJoinedSquares
  , triangleWithDotAbove
  , triangleWithUnderbar
  , sInTriangle
  , triangleWithSerifsAtBottom
  , rightTriangleAboveLeftTriangle
  , leftTriangleBesideVerticalBar
  , verticalBarBesideRightTriangle
  , leftDoubleWigglyFence
  , incompleteInfinity
  , tieOverInfinity
  , infinityNegatedWithVerticalBar
  , squareWithContouredOutline
  , equalsSignAndSlantedParallel
  , equalsSignAndSlantedParallelWithTildeAbove
  , identicalToAndSlantedParallel
  , downPointingTriangleWithLeftHalfBlack
  , downPointingTriangleWithRightHalfBlack
  , blackLozenge
  , errorBarredWhiteSquare
  , errorBarredBlackSquare
  , errorBarredWhiteDiamond
  , errorBarredBlackDiamond
  , errorBarredWhiteCircle
  , errorBarredBlackCircle
  , ruleDelayed
  , solidusWithOverbar
  , doublePlus
  , triplePlus
  , nAryCircledDotOperator
  , nAryCircledPlusOperator
  , nAryCircledTimesOperator
  , nAryUnionOperatorWithPlus
  , nArySquareIntersectionOperator
  , nArySquareUnionOperator
  , summationWithIntegral
  , quadrupleIntegralOperator
  , finitePartIntegral
  , integralWithDoubleStroke
  , integralAverageWithSlash
  , circulationFunction
  , anticlockwiseIntegration
  , lineIntegrationWithRectangularPathAroundPole
  , lineIntegrationWithSemicircularPathAroundPole
  , lineIntegrationNotIncludingThePole
  , integralAroundAPointOperator
  , quaternionIntegralOperator
  , integralWithLeftwardsArrowWithHook
  , integralWithTimesSign
  , integralWithIntersection
  , integralWithUnion
  , integralWithOverbar
  , integralWithUnderbar
  , largeLeftTriangleOperator
  , plusSignWithSmallCircleAbove
  , plusSignWithCircumflexAccentAbove
  , plusSignWithTildeAbove
  , plusSignWithDotBelow
  , plusSignWithTildeBelow
  , plusSignWithSubscriptTwo
  , plusSignWithBlackTriangle
  , minusSignWithCommaAbove
  , minusSignWithDotBelow
  , minusSignWithFallingDots
  , minusSignWithRisingDots
  , plusSignInLeftHalfCircle
  , plusSignInRightHalfCircle
  , vectorOrCrossProduct
  , multiplicationSignWithDotAbove
  , multiplicationSignWithUnderbar
  , smashProduct
  , multiplicationSignInLeftHalfCircle
  , multiplicationSignInRightHalfCircle
  , circledMultiplicationSignWithCircumflexAccent
  , multiplicationSignInDoubleCircle
  , circledDivisionSign
  , plusSignInTriangle
  , minusSignInTriangle
  , multiplicationSignInTriangle
  , interiorProduct
  , amalgamationOrCoproduct
  , intersectionWithDot
  , unionWithMinusSign
  , unionWithOverbar
  , intersectionWithOverbar
  , intersectionWithLogicalAnd
  , unionWithLogicalOr
  , unionAboveIntersection
  , intersectionAboveUnion
  , unionAboveBarAboveIntersection
  , intersectionAboveBarAboveUnion
  , unionBesideAndJoinedWithUnion
  , intersectionBesideAndJoinedWithIntersection
  , closedUnionWithSerifs
  , closedIntersectionWithSerifs
  , doubleSquareIntersection
  , closedUnionWithSerifsAndSmashProduct
  , doubleLogicalAnd
  , doubleLogicalOr
  , twoIntersectingLogicalAnd
  , twoIntersectingLogicalOr
  , slopingLargeOr
  , slopingLargeAnd
  , logicalAndWithMiddleStem
  , logicalOrWithMiddleStem
  , logicalAndWithHorizontalDash
  , logicalOrWithHorizontalDash
  , logicalAndWithUnderbar
  , equalsSignWithDotBelow
  , identicalWithDotAbove
  , tildeOperatorWithDotAbove
  , similarMinusSimilar
  , congruentWithDotAbove
  , equalsWithAsterisk
  , almostEqualToWithCircumflexAccent
  , approximatelyEqualOrEqualTo
  , equalsSignAbovePlusSign
  , plusSignAboveEqualsSign
  , equalsSignAboveTildeOperator
  , doubleColonEqual
  , twoConsecutiveEqualsSigns
  , threeConsecutiveEqualsSigns
  , equalsSignWithTwoDotsAboveAndTwoDotsBelow
  , equivalentWithFourDotsAbove
  , lessThanWithCircleInside
  , greaterThanWithCircleInside
  , lessThanWithQuestionMarkAbove
  , greaterThanWithQuestionMarkAbove
  , lessThanOrSlantedEqualTo
  , greaterThanOrSlantedEqualTo
  , lessThanOrSlantedEqualToWithDotInside
  , greaterThanOrSlantedEqualToWithDotInside
  , lessThanOrSlantedEqualToWithDotAbove
  , greaterThanOrSlantedEqualToWithDotAbove
  , lessThanOrSlantedEqualToWithDotAboveRight
  , greaterThanOrSlantedEqualToWithDotAboveLeft
  , lessThanOrApproximate
  , greaterThanOrApproximate
  , lessThanAndSingleLineNotEqualTo
  , greaterThanAndSingleLineNotEqualTo
  , lessThanAndNotApproximate
  , greaterThanAndNotApproximate
  , lessThanAboveDoubleLineEqualAboveGreaterThan
  , greaterThanAboveDoubleLineEqualAboveLessThan
  , lessThanAboveSimilarOrEqual
  , greaterThanAboveSimilarOrEqual
  , lessThanAboveSimilarAboveGreaterThan
  , greaterThanAboveSimilarAboveLessThan
  , lessThanAboveGreaterThanAboveDoubleLineEqual
  , greaterThanAboveLessThanAboveDoubleLineEqual
  , lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual
  , greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual
  , slantedEqualToOrLessThan
  , slantedEqualToOrGreaterThan
  , slantedEqualToOrLessThanWithDotInside
  , slantedEqualToOrGreaterThanWithDotInside
  , doubleLineEqualToOrLessThan
  , doubleLineEqualToOrGreaterThan
  , doubleLineSlantedEqualToOrGreaterThan
  , similarOrLessThan
  , similarOrGreaterThan
  , similarAboveLessThanAboveEqualsSign
  , similarAboveGreaterThanAboveEqualsSign
  , doubleNestedLessThan
  , doubleNestedGreaterThan
  , greaterThanOverlappingLessThan
  , greaterThanBesideLessThan
  , lessThanClosedByCurve
  , greaterThanClosedByCurve
  , lessThanClosedByCurveAboveSlantedEqual
  , greaterThanClosedByCurveAboveSlantedEqual
  , smallerThan
  , largerThan
  , smallerThanOrEqualTo
  , largerThanOrEqualTo
  , equalsSignWithBumpyAbove
  , precedesAboveSingleLineEqualsSign
  , succeedsAboveSingleLineEqualsSign
  , precedesAboveSingleLineNotEqualTo
  , succeedsAboveSingleLineNotEqualTo
  , precedesAboveEqualsSign
  , succeedsAboveEqualsSign
  , precedesAboveNotEqualTo
  , succeedsAboveNotEqualTo
  , precedesAboveAlmostEqualTo
  , succeedsAboveAlmostEqualTo
  , precedesAboveNotAlmostEqualTo
  , succeedsAboveNotAlmostEqualTo
  , doublePrecedes
  , doubleSucceeds
  , subsetWithDot
  , supersetWithDot
  , subsetWithPlusSignBelow
  , supersetWithPlusSignBelow
  , subsetWithMultiplicationSignBelow
  , supersetWithMultiplicationSignBelow
  , subsetOfOrEqualToWithDotAbove
  , supersetOfOrEqualToWithDotAbove
  , subsetOfAboveEqualsSign
  , supersetOfAboveEqualsSign
  , subsetOfAboveTildeOperator
  , supersetOfAboveTildeOperator
  , subsetOfAboveAlmostEqualTo
  , supersetOfAboveAlmostEqualTo
  , subsetOfAboveNotEqualTo
  , supersetOfAboveNotEqualTo
  , closedSubset
  , closedSuperset
  , closedSubsetOrEqualTo
  , closedSupersetOrEqualTo
  , subsetAboveSuperset
  , supersetAboveSubset
  , subsetAboveSubset
  , supersetAboveSuperset
  , supersetBesideSubset
  , supersetBesideAndJoinedByDashWithSubset
  , elementOfOpeningDownwards
  , pitchforkWithTeeTop
  , transversalIntersection
  , verticalBarDoubleLeftTurnstile
  , longDashFromLeftMemberOfDoubleVertical
  , shortDownTackWithOverbar
  , shortUpTackWithUnderbar
  , shortUpTackAboveShortDownTack
  , doubleUpTack
  , doubleStrokeNotSign
  , reversedDoubleStrokeNotSign
  , doesNotDivideWithReversedNegationSlash
  , verticalLineWithCircleAbove
  , verticalLineWithCircleBelow
  , downTackWithCircleBelow
  , parallelWithHorizontalStroke
  , parallelWithTildeOperator
  , tripleNestedGreaterThan
  , doubleLineSlantedGreaterThanOrEqualTo
  , doubleSolidusOperator
  , squareWithTopHalfBlack
  , squareWithBottomHalfBlack
  , squareWithUpperRightDiagonalHalfBlack
  , squareWithLowerLeftDiagonalHalfBlack
  , diamondWithLeftHalfBlack
  , diamondWithRightHalfBlack
  , diamondWithTopHalfBlack
  , diamondWithBottomHalfBlack
  , dottedSquare
  , blackLargeSquare
  , whiteLargeSquare
  , blackVerySmallSquare
  , whiteVerySmallSquare
  , blackPentagon
  , whitePentagon
  , whiteHexagon
  , blackHexagon
  , horizontalBlackHexagon
  , blackLargeCircle
  , blackMediumDiamond
  , whiteMediumDiamond
  , blackMediumLozenge
  , whiteMediumLozenge
  , blackSmallDiamond
  , blackSmallLozenge
  , whiteSmallLozenge
  , blackHorizontalEllipse
  , whiteHorizontalEllipse
  , blackVerticalEllipse
  , whiteVerticalEllipse
  , equalsSignAboveLeftwardsArrow
  , leftwardsArrowAboveReverseAlmostEqualTo
  , rightwardsArrowThroughGreaterThan
  , rightwardsArrowAboveReverseAlmostEqualTo
  , leftwardsArrowAboveAlmostEqualTo
  , blackRightPointingPentagon
  , whiteRightPointingPentagon
  , heavyLargeCircle
  , blackSquareCentred
  , blackDiamondCentred
  , turnedBlackPentagon
  , squarePositionIndicator
  , dottedRightPointingAngle
  , modifierLetterLowerRightCornerAngle
  , modifierLetterShortEqualsSign
  , latinSmallLigatureFf
  , latinSmallLigatureFi
  , latinSmallLigatureFl
  , latinSmallLigatureFfi
  , latinSmallLigatureFfl
  , smallPlusSign
  , smallHyphenMinus
  , smallGreaterThanSign
  , smallEqualsSign
  , smallPercentSign
  , fullwidthPercentSign
  , fullwidthPlusSign
  , fullwidthHyphenMinus
  , fullwidthEqualsSign
  , fullwidthGreaterThanSign
  , ugariticLetterDelta
  , mathematicalScriptCapitalA
  , mathematicalScriptCapitalC
  , mathematicalScriptCapitalD
  , mathematicalScriptCapitalG
  , mathematicalScriptCapitalJ
  , mathematicalScriptCapitalK
  , mathematicalScriptCapitalN
  , mathematicalScriptCapitalO
  , mathematicalScriptCapitalP
  , mathematicalScriptCapitalQ
  , mathematicalScriptCapitalS
  , mathematicalScriptCapitalT
  , mathematicalScriptCapitalU
  , mathematicalScriptCapitalV
  , mathematicalScriptCapitalW
  , mathematicalScriptCapitalX
  , mathematicalScriptCapitalY
  , mathematicalScriptCapitalZ
  , mathematicalScriptSmallA
  , mathematicalScriptSmallB
  , mathematicalScriptSmallC
  , mathematicalScriptSmallD
  , mathematicalScriptSmallF
  , mathematicalScriptSmallH
  , mathematicalScriptSmallI
  , mathematicalScriptSmallJ
  , mathematicalScriptSmallK
  , mathematicalScriptSmallL
  , mathematicalScriptSmallM
  , mathematicalScriptSmallN
  , mathematicalScriptSmallP
  , mathematicalScriptSmallQ
  , mathematicalScriptSmallR
  , mathematicalScriptSmallS
  , mathematicalScriptSmallT
  , mathematicalScriptSmallU
  , mathematicalScriptSmallV
  , mathematicalScriptSmallW
  , mathematicalScriptSmallX
  , mathematicalScriptSmallY
  , mathematicalScriptSmallZ
  , mathematicalFrakturCapitalA
  , mathematicalFrakturCapitalB
  , mathematicalFrakturCapitalD
  , mathematicalFrakturCapitalE
  , mathematicalFrakturCapitalF
  , mathematicalFrakturCapitalG
  , mathematicalFrakturCapitalJ
  , mathematicalFrakturCapitalK
  , mathematicalFrakturCapitalL
  , mathematicalFrakturCapitalM
  , mathematicalFrakturCapitalN
  , mathematicalFrakturCapitalO
  , mathematicalFrakturCapitalP
  , mathematicalFrakturCapitalQ
  , mathematicalFrakturCapitalS
  , mathematicalFrakturCapitalT
  , mathematicalFrakturCapitalU
  , mathematicalFrakturCapitalV
  , mathematicalFrakturCapitalW
  , mathematicalFrakturCapitalX
  , mathematicalFrakturCapitalY
  , mathematicalFrakturSmallA
  , mathematicalFrakturSmallB
  , mathematicalFrakturSmallC
  , mathematicalFrakturSmallD
  , mathematicalFrakturSmallE
  , mathematicalFrakturSmallF
  , mathematicalFrakturSmallG
  , mathematicalFrakturSmallH
  , mathematicalFrakturSmallI
  , mathematicalFrakturSmallJ
  , mathematicalFrakturSmallK
  , mathematicalFrakturSmallL
  , mathematicalFrakturSmallM
  , mathematicalFrakturSmallN
  , mathematicalFrakturSmallO
  , mathematicalFrakturSmallP
  , mathematicalFrakturSmallQ
  , mathematicalFrakturSmallR
  , mathematicalFrakturSmallS
  , mathematicalFrakturSmallT
  , mathematicalFrakturSmallU
  , mathematicalFrakturSmallV
  , mathematicalFrakturSmallW
  , mathematicalFrakturSmallX
  , mathematicalFrakturSmallY
  , mathematicalFrakturSmallZ
  , mathematicalDoubleStruckCapitalA
  , mathematicalDoubleStruckCapitalB
  , mathematicalDoubleStruckCapitalD
  , mathematicalDoubleStruckCapitalE
  , mathematicalDoubleStruckCapitalF
  , mathematicalDoubleStruckCapitalG
  , mathematicalDoubleStruckCapitalI
  , mathematicalDoubleStruckCapitalJ
  , mathematicalDoubleStruckCapitalK
  , mathematicalDoubleStruckCapitalL
  , mathematicalDoubleStruckCapitalM
  , mathematicalDoubleStruckCapitalO
  , mathematicalDoubleStruckCapitalS
  , mathematicalDoubleStruckCapitalT
  , mathematicalDoubleStruckCapitalU
  , mathematicalDoubleStruckCapitalV
  , mathematicalDoubleStruckCapitalW
  , mathematicalDoubleStruckCapitalX
  , mathematicalDoubleStruckCapitalY
  , mathematicalDoubleStruckSmallA
  , mathematicalDoubleStruckSmallB
  , mathematicalDoubleStruckSmallC
  , mathematicalDoubleStruckSmallD
  , mathematicalDoubleStruckSmallE
  , mathematicalDoubleStruckSmallF
  , mathematicalDoubleStruckSmallG
  , mathematicalDoubleStruckSmallH
  , mathematicalDoubleStruckSmallI
  , mathematicalDoubleStruckSmallJ
  , mathematicalDoubleStruckSmallK
  , mathematicalDoubleStruckSmallL
  , mathematicalDoubleStruckSmallM
  , mathematicalDoubleStruckSmallN
  , mathematicalDoubleStruckSmallO
  , mathematicalDoubleStruckSmallP
  , mathematicalDoubleStruckSmallQ
  , mathematicalDoubleStruckSmallR
  , mathematicalDoubleStruckSmallS
  , mathematicalDoubleStruckSmallT
  , mathematicalDoubleStruckSmallU
  , mathematicalDoubleStruckSmallV
  , mathematicalDoubleStruckSmallW
  , mathematicalDoubleStruckSmallX
  , mathematicalDoubleStruckSmallY
  , mathematicalDoubleStruckSmallZ
  ) where

import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (Tag_Entity))
import Brigid.HTML.Elements.Tags (Text)

-- | The null character HTML entity ('^@').
nullCharacter :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
nullCharacter = Tag_Entity "&#0;"

-- | The start of header HTML entity ('^A').
startOfHeader :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
startOfHeader = Tag_Entity "&#1;"

-- | The start of text HTML entity ('^B').
startOfText :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
startOfText = Tag_Entity "&#2;"

-- | The end of text HTML entity ('^C').
endOfText :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
endOfText = Tag_Entity "&#3;"

-- | The end of transmission HTML entity ('^D').
endOfTransmission :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
endOfTransmission = Tag_Entity "&#4;"

-- | The enquiry HTML entity ('^E').
enquiry :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
enquiry = Tag_Entity "&#5;"

-- | The acknowledge HTML entity ('^F').
acknowledge :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
acknowledge = Tag_Entity "&#6;"

-- | The bell (ring) HTML entity ('^G').
bell :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
bell = Tag_Entity "&#7;"

-- | The backspace HTML entity ('^H').
backspace :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
backspace = Tag_Entity "&#8;"

-- | The character tabulation / horizontal tabulation HTML entity ('^I').
characterTabulation :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
characterTabulation = Tag_Entity "&#9;"

-- | The line feed (lf) HTML entity ('^J').
lineFeed :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
lineFeed = Tag_Entity "&#10;"

-- | The vertical tab HTML entity ('^K').
verticalTab :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
verticalTab = Tag_Entity "&#11;"

-- | The form feed HTML entity ('^L').
formFeed :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
formFeed = Tag_Entity "&#12;"

-- | The carriage return HTML entity ('^M').
carriageReturn :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
carriageReturn = Tag_Entity "&#13;"

-- | The shift out HTML entity ('^N').
shiftOut :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
shiftOut = Tag_Entity "&#14;"

-- | The shift in HTML entity ('^O').
shiftIn :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
shiftIn = Tag_Entity "&#15;"

-- | The data link escape HTML entity ('^P').
dataLinkEscape :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
dataLinkEscape = Tag_Entity "&#16;"

-- | The device control 1 HTML entity ('^Q').
deviceControl1 :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
deviceControl1 = Tag_Entity "&#17;"

-- | The device control 2 HTML entity ('^R').
deviceControl2 :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
deviceControl2 = Tag_Entity "&#18;"

-- | The device control 3 HTML entity ('^S').
deviceControl3 :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
deviceControl3 = Tag_Entity "&#19;"

-- | The device control 4 HTML entity ('^T').
deviceControl4 :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
deviceControl4 = Tag_Entity "&#20;"

-- | The negative acknowledge HTML entity ('^U').
negativeAcknowledge :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
negativeAcknowledge = Tag_Entity "&#21;"

-- | The synchronize HTML entity ('^V').
synchronize :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
synchronize = Tag_Entity "&#22;"

-- | The end transmission block HTML entity ('^W').
endTransmissionBlock :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
endTransmissionBlock = Tag_Entity "&#23;"

-- | The cancel HTML entity ('^X').
cancel :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
cancel = Tag_Entity "&#24;"

-- | The end of medium HTML entity ('^Y').
endOfMedium :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
endOfMedium = Tag_Entity "&#25;"

-- | The substitute HTML entity ('^Z').
substitute :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
substitute = Tag_Entity "&#26;"

-- | The escape HTML entity ('^[').
escape :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
escape = Tag_Entity "&#27;"

-- | The file separator HTML entity ('^\').
fileSeparator :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
fileSeparator = Tag_Entity "&#28;"

-- | The group separator HTML entity ('^]').
groupSeparator :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
groupSeparator = Tag_Entity "&#29;"

-- | The record separator HTML entity ('^^').
recordSeparator :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
recordSeparator = Tag_Entity "&#30;"

-- | The unit separator HTML entity ('^_').
unitSeparator :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
unitSeparator = Tag_Entity "&#31;"

-- | The space HTML entity.
space :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
space = Tag_Entity "&#32;"

-- | The exclamation point HTML entity ('!').
exclamationPoint :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
exclamationPoint = Tag_Entity "&#33;"

-- | The quotation mark HTML entity ('"').
quotationMark :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
quotationMark = Tag_Entity "&#34;"

-- | The number sign HTML entity ('#').
numberSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
numberSign = Tag_Entity "&#35;"

-- | The dollar sign HTML entity ('$').
dollarSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
dollarSign = Tag_Entity "&#36;"

-- | The percent sign HTML entity ('%').
percentSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
percentSign = Tag_Entity "&#37;"

-- | The ampersand HTML entity ('&').
ampersand :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
ampersand = Tag_Entity "&#38;"

-- | The single quote HTML entity (''').
singleQuote :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
singleQuote = Tag_Entity "&#39;"

-- | The opening parenthesis HTML entity ('(').
openingParenthesis :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
openingParenthesis = Tag_Entity "&#40;"

-- | The closing parenthesis HTML entity (')').
closingParenthesis :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
closingParenthesis = Tag_Entity "&#41;"

-- | The asterisk HTML entity ('*').
asterisk :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
asterisk = Tag_Entity "&#42;"

-- | The plus sign HTML entity ('+').
plusSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
plusSign = Tag_Entity "&#43;"

-- | The comma HTML entity (',').
comma :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
comma = Tag_Entity "&#44;"

-- | The minus sign - hyphen HTML entity ('-').
minusSignHyphen :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
minusSignHyphen = Tag_Entity "&#45;"

-- | The period HTML entity ('.').
period :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
period = Tag_Entity "&#46;"

-- | The slash HTML entity ('/').
slash :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
slash = Tag_Entity "&#47;"

-- | The zero HTML entity ('0').
zero :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
zero = Tag_Entity "&#48;"

-- | The one HTML entity ('1').
one :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
one = Tag_Entity "&#49;"

-- | The two HTML entity ('2').
two :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
two = Tag_Entity "&#50;"

-- | The three HTML entity ('3').
three :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
three = Tag_Entity "&#51;"

-- | The four HTML entity ('4').
four :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
four = Tag_Entity "&#52;"

-- | The five HTML entity ('5').
five :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
five = Tag_Entity "&#53;"

-- | The six HTML entity ('6').
six :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
six = Tag_Entity "&#54;"

-- | The seven HTML entity ('7').
seven :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
seven = Tag_Entity "&#55;"

-- | The eight HTML entity ('8').
eight :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
eight = Tag_Entity "&#56;"

-- | The nine HTML entity ('9').
nine :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
nine = Tag_Entity "&#57;"

-- | The colon HTML entity (':').
colon :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
colon = Tag_Entity "&#58;"

-- | The semicolon HTML entity (';').
semicolon :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
semicolon = Tag_Entity "&#59;"

-- | The less-than sign HTML entity ('<').
lessThanSign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
lessThanSign = Tag_Entity "&#60;"

-- | The equal sign HTML entity ('=').
equalSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
equalSign = Tag_Entity "&#61;"

-- | The greater-than sign HTML entity ('>').
greaterThanSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
greaterThanSign = Tag_Entity "&#62;"

-- | The question mark HTML entity ('?').
questionMark :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
questionMark = Tag_Entity "&#63;"

-- | The at symbol HTML entity ('@').
atSymbol :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
atSymbol = Tag_Entity "&#64;"

-- | The upper case A HTML entity ('A').
upperCaseA :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseA = Tag_Entity "&#65;"

-- | The upper case B HTML entity ('B').
upperCaseB :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseB = Tag_Entity "&#66;"

-- | The upper case C  HTML entity ('C').
upperCaseC :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseC = Tag_Entity "&#67;"

-- | The upper case D  HTML entity ('D').
upperCaseD :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseD = Tag_Entity "&#68;"

-- | The upper case E  HTML entity ('E').
upperCaseE :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseE = Tag_Entity "&#69;"

-- | The upper case F  HTML entity ('F').
upperCaseF :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseF = Tag_Entity "&#70;"

-- | The upper case G  HTML entity ('G').
upperCaseG :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseG = Tag_Entity "&#71;"

-- | The upper case H  HTML entity ('H').
upperCaseH :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseH = Tag_Entity "&#72;"

-- | The upper case I  HTML entity ('I').
upperCaseI :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseI = Tag_Entity "&#73;"

-- | The upper case J  HTML entity ('J').
upperCaseJ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseJ = Tag_Entity "&#74;"

-- | The upper case K  HTML entity ('K').
upperCaseK :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseK = Tag_Entity "&#75;"

-- | The upper case L  HTML entity ('L').
upperCaseL :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseL = Tag_Entity "&#76;"

-- | The upper case M  HTML entity ('M').
upperCaseM :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseM = Tag_Entity "&#77;"

-- | The upper case N  HTML entity ('N').
upperCaseN :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseN = Tag_Entity "&#78;"

-- | The upper case O  HTML entity ('O').
upperCaseO :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseO = Tag_Entity "&#79;"

-- | The upper case P  HTML entity ('P').
upperCaseP :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseP = Tag_Entity "&#80;"

-- | The upper case Q  HTML entity ('Q').
upperCaseQ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseQ = Tag_Entity "&#81;"

-- | The upper case R  HTML entity ('R').
upperCaseR :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseR = Tag_Entity "&#82;"

-- | The upper case S  HTML entity ('S').
upperCaseS :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseS = Tag_Entity "&#83;"

-- | The upper case T  HTML entity ('T').
upperCaseT :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseT = Tag_Entity "&#84;"

-- | The upper case U  HTML entity ('U').
upperCaseU :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseU = Tag_Entity "&#85;"

-- | The upper case V  HTML entity ('V').
upperCaseV :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseV = Tag_Entity "&#86;"

-- | The upper case W  HTML entity ('W').
upperCaseW :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseW = Tag_Entity "&#87;"

-- | The upper case X  HTML entity ('X').
upperCaseX :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseX = Tag_Entity "&#88;"

-- | The upper case Y  HTML entity ('Y').
upperCaseY :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseY = Tag_Entity "&#89;"

-- | The upper case Z  HTML entity ('Z').
upperCaseZ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upperCaseZ = Tag_Entity "&#90;"

-- | The left square bracket HTML entity ('[').
leftSquareBracket :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
leftSquareBracket = Tag_Entity "&#91;"

-- | The backslash HTML entity ('\').
backslash :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
backslash = Tag_Entity "&#92;"

-- | The right square bracket HTML entity (']').
rightSquareBracket :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
rightSquareBracket = Tag_Entity "&#93;"

-- | The caret - circumflex HTML entity ('^').
caretCircumflex :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
caretCircumflex = Tag_Entity "&#94;"

-- | The underscore HTML entity ('_').
underscore :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
underscore = Tag_Entity "&#95;"

-- | The grave accent HTML entity ('`').
graveAccent :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
graveAccent = Tag_Entity "&#96;"

-- | The lower case a HTML entity ('a').
lowerCaseA :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseA = Tag_Entity "&#97;"

-- | The lower case b HTML entity ('b').
lowerCaseB :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseB = Tag_Entity "&#98;"

-- | The lower case c HTML entity ('c').
lowerCaseC :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseC = Tag_Entity "&#99;"

-- | The lower case d HTML entity ('d').
lowerCaseD :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseD = Tag_Entity "&#100;"

-- | The lower case e HTML entity ('e').
lowerCaseE :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseE = Tag_Entity "&#101;"

-- | The lower case f HTML entity ('f').
lowerCaseF :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseF = Tag_Entity "&#102;"

-- | The lower case g HTML entity ('g').
lowerCaseG :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseG = Tag_Entity "&#103;"

-- | The lower case h HTML entity ('h').
lowerCaseH :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseH = Tag_Entity "&#104;"

-- | The lower case i HTML entity ('i').
lowerCaseI :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseI = Tag_Entity "&#105;"

-- | The lower case j HTML entity ('j').
lowerCaseJ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseJ = Tag_Entity "&#106;"

-- | The lower case k HTML entity ('k').
lowerCaseK :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseK = Tag_Entity "&#107;"

-- | The lower case l HTML entity ('l').
lowerCaseL :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseL = Tag_Entity "&#108;"

-- | The lower case m HTML entity ('m').
lowerCaseM :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseM = Tag_Entity "&#109;"

-- | The lower case n HTML entity ('n').
lowerCaseN :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseN = Tag_Entity "&#110;"

-- | The lower case o HTML entity ('o').
lowerCaseO :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseO = Tag_Entity "&#111;"

-- | The lower case p HTML entity ('p').
lowerCaseP :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseP = Tag_Entity "&#112;"

-- | The lower case q HTML entity ('q').
lowerCaseQ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseQ = Tag_Entity "&#113;"

-- | The lower case r HTML entity ('r').
lowerCaseR :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseR = Tag_Entity "&#114;"

-- | The lower case s HTML entity ('s').
lowerCaseS :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseS = Tag_Entity "&#115;"

-- | The lower case t HTML entity ('t').
lowerCaseT :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseT = Tag_Entity "&#116;"

-- | The lower case u HTML entity ('u').
lowerCaseU :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseU = Tag_Entity "&#117;"

-- | The lower case v HTML entity ('v').
lowerCaseV :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseV = Tag_Entity "&#118;"

-- | The lower case w HTML entity ('w').
lowerCaseW :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseW = Tag_Entity "&#119;"

-- | The lower case x HTML entity ('x').
lowerCaseX :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseX = Tag_Entity "&#120;"

-- | The lower case y HTML entity ('y').
lowerCaseY :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseY = Tag_Entity "&#121;"

-- | The lower case z HTML entity ('z').
lowerCaseZ :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lowerCaseZ = Tag_Entity "&#122;"

-- | The left curly bracket HTML entity ('{').
leftCurlyBracket :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
leftCurlyBracket = Tag_Entity "&#123;"

-- | The vertical line HTML entity ('|').
verticalLine :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
verticalLine = Tag_Entity "&#124;"

-- | The right curly bracket HTML entity ('}').
rightCurlyBracket :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
rightCurlyBracket = Tag_Entity "&#125;"

-- | The equivalency sign - tilde HTML entity ('~').
equivalencySignTilde :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
equivalencySignTilde = Tag_Entity "&#126;"

-- | The delete (rubout) HTML entity ('^?').
delete :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
delete = Tag_Entity "&#127;"

-- | The Padding Character HTML entity ('Esc@').
paddingCharacter :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
paddingCharacter = Tag_Entity "&#128;"

-- | The High Octet Preset HTML entity ('EscA').
highOctetPreset :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
highOctetPreset = Tag_Entity "&#129;"

-- | The Break Permitted Here HTML entity ('EscB').
breakPermittedHere :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
breakPermittedHere = Tag_Entity "&#130;"

-- | The No Break Here HTML entity ('EscC').
noBreakHere :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
noBreakHere = Tag_Entity "&#131;"

-- | The Index HTML entity ('EscD').
index :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
index = Tag_Entity "&#132;"

-- | The Next Line HTML entity ('EscE').
nextLine :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
nextLine = Tag_Entity "&#133;"

-- | The Start of Selected Area HTML entity ('EscF').
startOfSelectedArea :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
startOfSelectedArea = Tag_Entity "&#134;"

-- | The End of Selected Area HTML entity ('EscG').
endOfSelectedArea :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
endOfSelectedArea = Tag_Entity "&#135;"

-- | The nCharacter Tabulation Set HTML entity ('EscH').
ncharacterTabulationSet :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
ncharacterTabulationSet = Tag_Entity "&#136;"

-- | The Character Tabulation Set with Justification HTML entity ('EscI').
characterTabulationSetWithJustification :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
characterTabulationSetWithJustification = Tag_Entity "&#137;"

-- | The Line Tabulation Set HTML entity ('EscJ').
lineTabulationSet :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
lineTabulationSet = Tag_Entity "&#138;"

-- | The Partial Line Forward HTML entity ('EscK').
partialLineForward :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
partialLineForward = Tag_Entity "&#139;"

-- | The Partial Line Backward HTML entity ('EscL').
partialLineBackward :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
partialLineBackward = Tag_Entity "&#140;"

-- | The Reverse Line Feed HTML entity ('EscM').
reverseLineFeed :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
reverseLineFeed = Tag_Entity "&#141;"

-- | The Single-Shift 2 HTML entity ('EscN').
singleShift2 :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
singleShift2 = Tag_Entity "&#142;"

-- | The Single-Shift 3 HTML entity ('EscO').
singleShift3 :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
singleShift3 = Tag_Entity "&#143;"

-- | The Device Control String HTML entity ('EscP').
deviceControlString :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
deviceControlString = Tag_Entity "&#144;"

-- | The Private Use 1 HTML entity ('EscQ').
privateUse1 :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
privateUse1 = Tag_Entity "&#145;"

-- | The Private Use 2 HTML entity ('EscR').
privateUse2 :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
privateUse2 = Tag_Entity "&#146;"

-- | The Set Transmit State HTML entity ('EscS').
setTransmitState :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
setTransmitState = Tag_Entity "&#147;"

-- | The Cancel character HTML entity ('EscT').
cancelCharacter :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
cancelCharacter = Tag_Entity "&#148;"

-- | The Message Waiting HTML entity ('EscU').
messageWaiting :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
messageWaiting = Tag_Entity "&#149;"

-- | The Start of Protected Area HTML entity ('EscV').
startOfProtectedArea :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
startOfProtectedArea = Tag_Entity "&#150;"

-- | The End of Protected Area HTML entity ('EscW').
endOfProtectedArea :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
endOfProtectedArea = Tag_Entity "&#151;"

-- | The Start of String Followed by a control string terminated by ST (0x9C) HTML entity ('EscX').
startOfStringFollowedByAControlStringTerminatedBySt :: ValidChild Text parent grandparent
                                                    => ChildHTML parent grandparent
startOfStringFollowedByAControlStringTerminatedBySt = Tag_Entity "&#152;"

-- | The Single Graphic Character Introducer HTML entity ('EscY').
singleGraphicCharacterIntroducer :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
singleGraphicCharacterIntroducer = Tag_Entity "&#153;"

-- | The Single Character Introducer HTML entity ('EscZ').
singleCharacterIntroducer :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
singleCharacterIntroducer = Tag_Entity "&#154;"

-- | The Control Sequence Introducer HTML entity ('Esc[').
controlSequenceIntroducer :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
controlSequenceIntroducer = Tag_Entity "&#155;"

-- | The String Terminator HTML entity ('Esc\').
stringTerminator :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
stringTerminator = Tag_Entity "&#156;"

-- | The Operating System Command HTML entity ('Esc]').
operatingSystemCommand :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
operatingSystemCommand = Tag_Entity "&#157;"

-- | The Privacy Message	 HTML entity ('Esc^').
privacyMessage :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
privacyMessage = Tag_Entity "&#158;"

-- | The Application Program Command HTML entity ('Esc_').
applicationProgramCommand :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
applicationProgramCommand = Tag_Entity "&#159;"

-- | The no-break space HTML entity.
noBreakSpace :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
noBreakSpace = Tag_Entity "&#160;"

-- | The inverted exclamation mark HTML entity ('¡').
invertedExclamationMark :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
invertedExclamationMark = Tag_Entity "&#161;"

-- | The cent sign HTML entity ('¢').
centSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
centSign = Tag_Entity "&#162;"

-- | The pound sign HTML entity ('£').
poundSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
poundSign = Tag_Entity "&#163;"

-- | The currency sign HTML entity ('¤').
currencySign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
currencySign = Tag_Entity "&#164;"

-- | The yen sign HTML entity ('¥').
yenSign :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
yenSign = Tag_Entity "&#165;"

-- | The broken vertical bar HTML entity ('¦').
brokenVerticalBar :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
brokenVerticalBar = Tag_Entity "&#166;"

-- | The section sign HTML entity ('§').
sectionSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
sectionSign = Tag_Entity "&#167;"

-- | The diaeresis HTML entity ('¨').
diaeresis :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
diaeresis = Tag_Entity "&#168;"

-- | The copyright sign HTML entity ('©').
copyrightSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
copyrightSign = Tag_Entity "&#169;"

-- | The feminine ordinal indicator HTML entity ('ª').
feminineOrdinalIndicator :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
feminineOrdinalIndicator = Tag_Entity "&#170;"

-- | The left double angle quotes HTML entity ('«').
leftDoubleAngleQuotes :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
leftDoubleAngleQuotes = Tag_Entity "&#171;"

-- | The not sign HTML entity ('¬').
notSign :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
notSign = Tag_Entity "&#172;"

-- | The soft hyphen HTML entity ('­').
softHyphen :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
softHyphen = Tag_Entity "&#173;"

-- | The registered sign HTML entity ('®').
registeredSign :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
registeredSign = Tag_Entity "&#174;"

-- | The macron HTML entity ('¯').
macron :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
macron = Tag_Entity "&#175;"

-- | The degree sign HTML entity ('°').
degreeSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
degreeSign = Tag_Entity "&#176;"

-- | The plus-minus sign HTML entity ('±').
plusMinusSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
plusMinusSign = Tag_Entity "&#177;"

-- | The superscript two - squared HTML entity ('²').
superscriptTwoSquared :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
superscriptTwoSquared = Tag_Entity "&#178;"

-- | The superscript three - cubed HTML entity ('³').
superscriptThreeCubed :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
superscriptThreeCubed = Tag_Entity "&#179;"

-- | The acute accent HTML entity ('´').
acuteAccent :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
acuteAccent = Tag_Entity "&#180;"

-- | The micro sign HTML entity ('µ').
microSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
microSign = Tag_Entity "&#181;"

-- | The pilcrow sign - paragraph sign HTML entity ('¶').
pilcrowSignParagraphSign :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
pilcrowSignParagraphSign = Tag_Entity "&#182;"

-- | The middle dot HTML entity ('·').
middleDot :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
middleDot = Tag_Entity "&#183;"

-- | The cedilla HTML entity ('¸').
cedilla :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
cedilla = Tag_Entity "&#184;"

-- | The superscript one HTML entity ('¹').
superscriptOne :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
superscriptOne = Tag_Entity "&#185;"

-- | The masculine ordinal indicator HTML entity ('º').
masculineOrdinalIndicator :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
masculineOrdinalIndicator = Tag_Entity "&#186;"

-- | The right double angle quotes HTML entity ('»').
rightDoubleAngleQuotes :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
rightDoubleAngleQuotes = Tag_Entity "&#187;"

-- | The fraction one quarter HTML entity ('¼').
fractionOneQuarter :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
fractionOneQuarter = Tag_Entity "&#188;"

-- | The vulgar fraction one half HTML entity ('½').
vulgarFractionOneHalf :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
vulgarFractionOneHalf = Tag_Entity "&#189;"

-- | The fraction three quarters HTML entity ('¾').
fractionThreeQuarters :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
fractionThreeQuarters = Tag_Entity "&#190;"

-- | The inverted question mark HTML entity ('¿').
invertedQuestionMark :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
invertedQuestionMark = Tag_Entity "&#191;"

-- | The latin capital letter a with grave HTML entity ('À').
latinCapitalLetterAWithGrave :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterAWithGrave = Tag_Entity "&#192;"

-- | The latin capital letter a with acute HTML entity ('Á').
latinCapitalLetterAWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterAWithAcute = Tag_Entity "&#193;"

-- | The latin capital letter a with circumflex HTML entity ('Â').
latinCapitalLetterAWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterAWithCircumflex = Tag_Entity "&#194;"

-- | The latin capital letter a with tilde HTML entity ('Ã').
latinCapitalLetterAWithTilde :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterAWithTilde = Tag_Entity "&#195;"

-- | The latin capital letter a with diaeresis HTML entity ('Ä').
latinCapitalLetterAWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterAWithDiaeresis = Tag_Entity "&#196;"

-- | The latin capital letter a with ring above HTML entity ('Å').
latinCapitalLetterAWithRingAbove :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterAWithRingAbove = Tag_Entity "&#197;"

-- | The latin capital letter ae HTML entity ('Æ').
latinCapitalLetterAe :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinCapitalLetterAe = Tag_Entity "&#198;"

-- | The latin capital letter c with cedilla HTML entity ('Ç').
latinCapitalLetterCWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterCWithCedilla = Tag_Entity "&#199;"

-- | The latin capital letter e with grave HTML entity ('È').
latinCapitalLetterEWithGrave :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterEWithGrave = Tag_Entity "&#200;"

-- | The latin capital letter e with acute HTML entity ('É').
latinCapitalLetterEWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterEWithAcute = Tag_Entity "&#201;"

-- | The latin capital letter e with circumflex HTML entity ('Ê').
latinCapitalLetterEWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterEWithCircumflex = Tag_Entity "&#202;"

-- | The latin capital letter e with diaeresis HTML entity ('Ë').
latinCapitalLetterEWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterEWithDiaeresis = Tag_Entity "&#203;"

-- | The latin capital letter i with grave HTML entity ('Ì').
latinCapitalLetterIWithGrave :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterIWithGrave = Tag_Entity "&#204;"

-- | The latin capital letter i with acute HTML entity ('Í').
latinCapitalLetterIWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterIWithAcute = Tag_Entity "&#205;"

-- | The latin capital letter i with circumflex HTML entity ('Î').
latinCapitalLetterIWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterIWithCircumflex = Tag_Entity "&#206;"

-- | The latin capital letter i with diaeresis HTML entity ('Ï').
latinCapitalLetterIWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterIWithDiaeresis = Tag_Entity "&#207;"

-- | The latin capital letter eth HTML entity ('Ð').
latinCapitalLetterEth :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinCapitalLetterEth = Tag_Entity "&#208;"

-- | The latin capital letter n with tilde HTML entity ('Ñ').
latinCapitalLetterNWithTilde :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterNWithTilde = Tag_Entity "&#209;"

-- | The latin capital letter o with grave HTML entity ('Ò').
latinCapitalLetterOWithGrave :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterOWithGrave = Tag_Entity "&#210;"

-- | The latin capital letter o with acute HTML entity ('Ó').
latinCapitalLetterOWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterOWithAcute = Tag_Entity "&#211;"

-- | The latin capital letter o with circumflex HTML entity ('Ô').
latinCapitalLetterOWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterOWithCircumflex = Tag_Entity "&#212;"

-- | The latin capital letter o with tilde HTML entity ('Õ').
latinCapitalLetterOWithTilde :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterOWithTilde = Tag_Entity "&#213;"

-- | The latin capital letter o with diaeresis HTML entity ('Ö').
latinCapitalLetterOWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterOWithDiaeresis = Tag_Entity "&#214;"

-- | The multiplication sign HTML entity ('×').
multiplicationSign :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
multiplicationSign = Tag_Entity "&#215;"

-- | The latin capital letter o with slash HTML entity ('Ø').
latinCapitalLetterOWithSlash :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterOWithSlash = Tag_Entity "&#216;"

-- | The latin capital letter u with grave HTML entity ('Ù').
latinCapitalLetterUWithGrave :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterUWithGrave = Tag_Entity "&#217;"

-- | The latin capital letter u with acute HTML entity ('Ú').
latinCapitalLetterUWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterUWithAcute = Tag_Entity "&#218;"

-- | The latin capital letter u with circumflex HTML entity ('Û').
latinCapitalLetterUWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterUWithCircumflex = Tag_Entity "&#219;"

-- | The latin capital letter u with diaeresis HTML entity ('Ü').
latinCapitalLetterUWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterUWithDiaeresis = Tag_Entity "&#220;"

-- | The latin capital letter y with acute HTML entity ('Ý').
latinCapitalLetterYWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterYWithAcute = Tag_Entity "&#221;"

-- | The latin capital letter thorn HTML entity ('Þ').
latinCapitalLetterThorn :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
latinCapitalLetterThorn = Tag_Entity "&#222;"

-- | The latin small letter sharp s - ess-zed HTML entity ('ß').
latinSmallLetterSharpSEssZed :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterSharpSEssZed = Tag_Entity "&#223;"

-- | The latin small letter a with grave HTML entity ('à').
latinSmallLetterAWithGrave :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterAWithGrave = Tag_Entity "&#224;"

-- | The latin small letter a with acute HTML entity ('á').
latinSmallLetterAWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterAWithAcute = Tag_Entity "&#225;"

-- | The latin small letter a with circumflex HTML entity ('â').
latinSmallLetterAWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterAWithCircumflex = Tag_Entity "&#226;"

-- | The latin small letter a with tilde HTML entity ('ã').
latinSmallLetterAWithTilde :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterAWithTilde = Tag_Entity "&#227;"

-- | The latin small letter a with diaeresis HTML entity ('ä').
latinSmallLetterAWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterAWithDiaeresis = Tag_Entity "&#228;"

-- | The latin small letter a with ring above HTML entity ('å').
latinSmallLetterAWithRingAbove :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterAWithRingAbove = Tag_Entity "&#229;"

-- | The latin small letter ae HTML entity ('æ').
latinSmallLetterAe :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
latinSmallLetterAe = Tag_Entity "&#230;"

-- | The latin small letter c with cedilla HTML entity ('ç').
latinSmallLetterCWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterCWithCedilla = Tag_Entity "&#231;"

-- | The latin small letter e with grave HTML entity ('è').
latinSmallLetterEWithGrave :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterEWithGrave = Tag_Entity "&#232;"

-- | The latin small letter e with acute HTML entity ('é').
latinSmallLetterEWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterEWithAcute = Tag_Entity "&#233;"

-- | The latin small letter e with circumflex HTML entity ('ê').
latinSmallLetterEWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterEWithCircumflex = Tag_Entity "&#234;"

-- | The latin small letter e with diaeresis HTML entity ('ë').
latinSmallLetterEWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterEWithDiaeresis = Tag_Entity "&#235;"

-- | The latin small letter i with grave HTML entity ('ì').
latinSmallLetterIWithGrave :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterIWithGrave = Tag_Entity "&#236;"

-- | The latin small letter i with acute HTML entity ('í').
latinSmallLetterIWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterIWithAcute = Tag_Entity "&#237;"

-- | The latin small letter i with circumflex HTML entity ('î').
latinSmallLetterIWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterIWithCircumflex = Tag_Entity "&#238;"

-- | The latin small letter i with diaeresis HTML entity ('ï').
latinSmallLetterIWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterIWithDiaeresis = Tag_Entity "&#239;"

-- | The latin small letter eth HTML entity ('ð').
latinSmallLetterEth :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
latinSmallLetterEth = Tag_Entity "&#240;"

-- | The latin small letter n with tilde HTML entity ('ñ').
latinSmallLetterNWithTilde :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterNWithTilde = Tag_Entity "&#241;"

-- | The latin small letter o with grave HTML entity ('ò').
latinSmallLetterOWithGrave :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterOWithGrave = Tag_Entity "&#242;"

-- | The latin small letter o with acute HTML entity ('ó').
latinSmallLetterOWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterOWithAcute = Tag_Entity "&#243;"

-- | The latin small letter o with circumflex HTML entity ('ô').
latinSmallLetterOWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterOWithCircumflex = Tag_Entity "&#244;"

-- | The latin small letter o with tilde HTML entity ('õ').
latinSmallLetterOWithTilde :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterOWithTilde = Tag_Entity "&#245;"

-- | The latin small letter o with diaeresis HTML entity ('ö').
latinSmallLetterOWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterOWithDiaeresis = Tag_Entity "&#246;"

-- | The division sign HTML entity ('÷').
divisionSign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
divisionSign = Tag_Entity "&#247;"

-- | The latin small letter o with slash HTML entity ('ø').
latinSmallLetterOWithSlash :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterOWithSlash = Tag_Entity "&#248;"

-- | The latin small letter u with grave HTML entity ('ù').
latinSmallLetterUWithGrave :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterUWithGrave = Tag_Entity "&#249;"

-- | The latin small letter u with acute HTML entity ('ú').
latinSmallLetterUWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterUWithAcute = Tag_Entity "&#250;"

-- | The latin small letter u with circumflex HTML entity ('û').
latinSmallLetterUWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterUWithCircumflex = Tag_Entity "&#251;"

-- | The latin small letter u with diaeresis HTML entity ('ü').
latinSmallLetterUWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterUWithDiaeresis = Tag_Entity "&#252;"

-- | The latin small letter y with acute HTML entity ('ý').
latinSmallLetterYWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterYWithAcute = Tag_Entity "&#253;"

-- | The latin small letter thorn HTML entity ('þ').
latinSmallLetterThorn :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinSmallLetterThorn = Tag_Entity "&#254;"

-- | The latin small letter y with diaeresis HTML entity ('ÿ').
latinSmallLetterYWithDiaeresis :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterYWithDiaeresis = Tag_Entity "&#255;"

-- | The latin capital letter a with macron HTML entity ('Ā').
latinCapitalLetterAWithMacron :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterAWithMacron = Tag_Entity "&#256;"

-- | The latin small letter a with macron HTML entity ('ā').
latinSmallLetterAWithMacron :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterAWithMacron = Tag_Entity "&#257;"

-- | The latin capital letter a with breve HTML entity ('Ă').
latinCapitalLetterAWithBreve :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterAWithBreve = Tag_Entity "&#258;"

-- | The latin small letter a with breve HTML entity ('ă').
latinSmallLetterAWithBreve :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterAWithBreve = Tag_Entity "&#259;"

-- | The latin capital letter a with ogonek HTML entity ('Ą').
latinCapitalLetterAWithOgonek :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterAWithOgonek = Tag_Entity "&#260;"

-- | The latin small letter a with ogonek HTML entity ('ą').
latinSmallLetterAWithOgonek :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterAWithOgonek = Tag_Entity "&#261;"

-- | The latin capital letter c with acute HTML entity ('Ć').
latinCapitalLetterCWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterCWithAcute = Tag_Entity "&#262;"

-- | The latin small letter c with acute HTML entity ('ć').
latinSmallLetterCWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterCWithAcute = Tag_Entity "&#263;"

-- | The latin capital letter c with circumflex HTML entity ('Ĉ').
latinCapitalLetterCWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterCWithCircumflex = Tag_Entity "&#264;"

-- | The latin small letter c with circumflex HTML entity ('ĉ').
latinSmallLetterCWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterCWithCircumflex = Tag_Entity "&#265;"

-- | The latin capital letter c with dot above HTML entity ('Ċ').
latinCapitalLetterCWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinCapitalLetterCWithDotAbove = Tag_Entity "&#266;"

-- | The latin small letter c with dot above HTML entity ('ċ').
latinSmallLetterCWithDotAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinSmallLetterCWithDotAbove = Tag_Entity "&#267;"

-- | The latin capital letter c with caron HTML entity ('Č').
latinCapitalLetterCWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterCWithCaron = Tag_Entity "&#268;"

-- | The latin small letter c with caron HTML entity ('č').
latinSmallLetterCWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterCWithCaron = Tag_Entity "&#269;"

-- | The latin capital letter d with caron HTML entity ('Ď').
latinCapitalLetterDWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterDWithCaron = Tag_Entity "&#270;"

-- | The latin small letter d with caron HTML entity ('ď').
latinSmallLetterDWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterDWithCaron = Tag_Entity "&#271;"

-- | The latin capital letter d with stroke HTML entity ('Đ').
latinCapitalLetterDWithStroke :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterDWithStroke = Tag_Entity "&#272;"

-- | The latin small letter d with stroke HTML entity ('đ').
latinSmallLetterDWithStroke :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterDWithStroke = Tag_Entity "&#273;"

-- | The latin capital letter e with macron HTML entity ('Ē').
latinCapitalLetterEWithMacron :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterEWithMacron = Tag_Entity "&#274;"

-- | The latin small letter e with macron HTML entity ('ē').
latinSmallLetterEWithMacron :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterEWithMacron = Tag_Entity "&#275;"

-- | The latin capital letter e with dot above HTML entity ('Ė').
latinCapitalLetterEWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinCapitalLetterEWithDotAbove = Tag_Entity "&#278;"

-- | The latin small letter e with dot above HTML entity ('ė').
latinSmallLetterEWithDotAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinSmallLetterEWithDotAbove = Tag_Entity "&#279;"

-- | The latin capital letter e with ogonek HTML entity ('Ę').
latinCapitalLetterEWithOgonek :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterEWithOgonek = Tag_Entity "&#280;"

-- | The latin small letter e with ogonek HTML entity ('ę').
latinSmallLetterEWithOgonek :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterEWithOgonek = Tag_Entity "&#281;"

-- | The latin capital letter e with caron HTML entity ('Ě').
latinCapitalLetterEWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterEWithCaron = Tag_Entity "&#282;"

-- | The latin small letter e with caron HTML entity ('ě').
latinSmallLetterEWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterEWithCaron = Tag_Entity "&#283;"

-- | The latin capital letter g with circumflex HTML entity ('Ĝ').
latinCapitalLetterGWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterGWithCircumflex = Tag_Entity "&#284;"

-- | The latin small letter g with circumflex HTML entity ('ĝ').
latinSmallLetterGWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterGWithCircumflex = Tag_Entity "&#285;"

-- | The latin capital letter g with breve HTML entity ('Ğ').
latinCapitalLetterGWithBreve :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterGWithBreve = Tag_Entity "&#286;"

-- | The latin small letter g with breve HTML entity ('ğ').
latinSmallLetterGWithBreve :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterGWithBreve = Tag_Entity "&#287;"

-- | The latin capital letter g with dot above HTML entity ('Ġ').
latinCapitalLetterGWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinCapitalLetterGWithDotAbove = Tag_Entity "&#288;"

-- | The latin small letter g with dot above HTML entity ('ġ').
latinSmallLetterGWithDotAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinSmallLetterGWithDotAbove = Tag_Entity "&#289;"

-- | The latin capital letter g with cedilla HTML entity ('Ģ').
latinCapitalLetterGWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterGWithCedilla = Tag_Entity "&#290;"

-- | The latin capital letter h with circumflex HTML entity ('Ĥ').
latinCapitalLetterHWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterHWithCircumflex = Tag_Entity "&#292;"

-- | The latin small letter h with circumflex HTML entity ('ĥ').
latinSmallLetterHWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterHWithCircumflex = Tag_Entity "&#293;"

-- | The latin capital letter h with stroke HTML entity ('Ħ').
latinCapitalLetterHWithStroke :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterHWithStroke = Tag_Entity "&#294;"

-- | The latin small letter h with stroke HTML entity ('ħ').
latinSmallLetterHWithStroke :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterHWithStroke = Tag_Entity "&#295;"

-- | The latin capital letter i with tilde HTML entity ('Ĩ').
latinCapitalLetterIWithTilde :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterIWithTilde = Tag_Entity "&#296;"

-- | The latin small letter i with tilde HTML entity ('ĩ').
latinSmallLetterIWithTilde :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterIWithTilde = Tag_Entity "&#297;"

-- | The latin capital letter i with macron HTML entity ('Ī').
latinCapitalLetterIWithMacron :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterIWithMacron = Tag_Entity "&#298;"

-- | The latin small letter i with macron HTML entity ('ī').
latinSmallLetterIWithMacron :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterIWithMacron = Tag_Entity "&#299;"

-- | The latin capital letter i with ogonek HTML entity ('Į').
latinCapitalLetterIWithOgonek :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterIWithOgonek = Tag_Entity "&#302;"

-- | The latin small letter i with ogonek HTML entity ('į').
latinSmallLetterIWithOgonek :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterIWithOgonek = Tag_Entity "&#303;"

-- | The latin capital letter i with dot above HTML entity ('İ').
latinCapitalLetterIWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinCapitalLetterIWithDotAbove = Tag_Entity "&#304;"

-- | The latin small letter dotless i HTML entity ('ı').
latinSmallLetterDotlessI :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
latinSmallLetterDotlessI = Tag_Entity "&#305;"

-- | The latin capital ligature ij HTML entity ('Ĳ').
latinCapitalLigatureIj :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
latinCapitalLigatureIj = Tag_Entity "&#306;"

-- | The latin small ligature ij HTML entity ('ĳ').
latinSmallLigatureIj :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinSmallLigatureIj = Tag_Entity "&#307;"

-- | The latin capital letter j with circumflex HTML entity ('Ĵ').
latinCapitalLetterJWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterJWithCircumflex = Tag_Entity "&#308;"

-- | The latin small letter j with circumflex HTML entity ('ĵ').
latinSmallLetterJWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterJWithCircumflex = Tag_Entity "&#309;"

-- | The latin capital letter k with cedilla HTML entity ('Ķ').
latinCapitalLetterKWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterKWithCedilla = Tag_Entity "&#310;"

-- | The latin small letter k with cedilla HTML entity ('ķ').
latinSmallLetterKWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterKWithCedilla = Tag_Entity "&#311;"

-- | The latin small letter kra HTML entity ('ĸ').
latinSmallLetterKra :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
latinSmallLetterKra = Tag_Entity "&#312;"

-- | The latin capital letter l with acute HTML entity ('Ĺ').
latinCapitalLetterLWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterLWithAcute = Tag_Entity "&#313;"

-- | The latin small letter l with acute HTML entity ('ĺ').
latinSmallLetterLWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterLWithAcute = Tag_Entity "&#314;"

-- | The latin capital letter l with cedilla HTML entity ('Ļ').
latinCapitalLetterLWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterLWithCedilla = Tag_Entity "&#315;"

-- | The latin small letter l with cedilla HTML entity ('ļ').
latinSmallLetterLWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterLWithCedilla = Tag_Entity "&#316;"

-- | The latin capital letter l with caron HTML entity ('Ľ').
latinCapitalLetterLWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterLWithCaron = Tag_Entity "&#317;"

-- | The latin small letter l with caron HTML entity ('ľ').
latinSmallLetterLWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterLWithCaron = Tag_Entity "&#318;"

-- | The latin capital letter l with middle dot HTML entity ('Ŀ').
latinCapitalLetterLWithMiddleDot :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterLWithMiddleDot = Tag_Entity "&#319;"

-- | The latin small letter l with middle dot HTML entity ('ŀ').
latinSmallLetterLWithMiddleDot :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterLWithMiddleDot = Tag_Entity "&#320;"

-- | The latin capital letter l with stroke HTML entity ('Ł').
latinCapitalLetterLWithStroke :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterLWithStroke = Tag_Entity "&#321;"

-- | The latin small letter l with stroke HTML entity ('ł').
latinSmallLetterLWithStroke :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterLWithStroke = Tag_Entity "&#322;"

-- | The latin capital letter n with acute HTML entity ('Ń').
latinCapitalLetterNWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterNWithAcute = Tag_Entity "&#323;"

-- | The latin small letter n with acute HTML entity ('ń').
latinSmallLetterNWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterNWithAcute = Tag_Entity "&#324;"

-- | The latin capital letter n with cedilla HTML entity ('Ņ').
latinCapitalLetterNWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterNWithCedilla = Tag_Entity "&#325;"

-- | The latin small letter n with cedilla HTML entity ('ņ').
latinSmallLetterNWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterNWithCedilla = Tag_Entity "&#326;"

-- | The latin capital letter n with caron HTML entity ('Ň').
latinCapitalLetterNWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterNWithCaron = Tag_Entity "&#327;"

-- | The latin small letter n with caron HTML entity ('ň').
latinSmallLetterNWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterNWithCaron = Tag_Entity "&#328;"

-- | The latin small letter n preceded by apostrophe HTML entity ('ŉ').
latinSmallLetterNPrecededByApostrophe :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
latinSmallLetterNPrecededByApostrophe = Tag_Entity "&#329;"

-- | The latin capital letter eng HTML entity ('Ŋ').
latinCapitalLetterEng :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinCapitalLetterEng = Tag_Entity "&#330;"

-- | The latin small letter eng HTML entity ('ŋ').
latinSmallLetterEng :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
latinSmallLetterEng = Tag_Entity "&#331;"

-- | The latin capital letter o with macron HTML entity ('Ō').
latinCapitalLetterOWithMacron :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterOWithMacron = Tag_Entity "&#332;"

-- | The latin small letter o with macron HTML entity ('ō').
latinSmallLetterOWithMacron :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterOWithMacron = Tag_Entity "&#333;"

-- | The latin capital letter o with double acute HTML entity ('Ő').
latinCapitalLetterOWithDoubleAcute :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
latinCapitalLetterOWithDoubleAcute = Tag_Entity "&#336;"

-- | The latin small letter o with double acute HTML entity ('ő').
latinSmallLetterOWithDoubleAcute :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinSmallLetterOWithDoubleAcute = Tag_Entity "&#337;"

-- | The latin capital letter oe HTML entity ('Œ').
latinCapitalLetterOe :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinCapitalLetterOe = Tag_Entity "&#338;"

-- | The latin small letter oe HTML entity ('œ').
latinSmallLetterOe :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
latinSmallLetterOe = Tag_Entity "&#339;"

-- | The latin capital letter r with acute HTML entity ('Ŕ').
latinCapitalLetterRWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterRWithAcute = Tag_Entity "&#340;"

-- | The latin small letter r with acute HTML entity ('ŕ').
latinSmallLetterRWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterRWithAcute = Tag_Entity "&#341;"

-- | The latin capital letter r with cedilla HTML entity ('Ŗ').
latinCapitalLetterRWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterRWithCedilla = Tag_Entity "&#342;"

-- | The latin small letter r with cedilla HTML entity ('ŗ').
latinSmallLetterRWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterRWithCedilla = Tag_Entity "&#343;"

-- | The latin capital letter r with caron HTML entity ('Ř').
latinCapitalLetterRWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterRWithCaron = Tag_Entity "&#344;"

-- | The latin small letter r with caron HTML entity ('ř').
latinSmallLetterRWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterRWithCaron = Tag_Entity "&#345;"

-- | The latin capital letter s with acute HTML entity ('Ś').
latinCapitalLetterSWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterSWithAcute = Tag_Entity "&#346;"

-- | The latin small letter s with acute HTML entity ('ś').
latinSmallLetterSWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterSWithAcute = Tag_Entity "&#347;"

-- | The latin capital letter s with circumflex HTML entity ('Ŝ').
latinCapitalLetterSWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterSWithCircumflex = Tag_Entity "&#348;"

-- | The latin small letter s with circumflex HTML entity ('ŝ').
latinSmallLetterSWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterSWithCircumflex = Tag_Entity "&#349;"

-- | The latin capital letter s with cedilla HTML entity ('Ş').
latinCapitalLetterSWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterSWithCedilla = Tag_Entity "&#350;"

-- | The latin small letter s with cedilla HTML entity ('ş').
latinSmallLetterSWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterSWithCedilla = Tag_Entity "&#351;"

-- | The latin capital letter s with caron HTML entity ('Š').
latinCapitalLetterSWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterSWithCaron = Tag_Entity "&#352;"

-- | The latin small letter s with caron HTML entity ('š').
latinSmallLetterSWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterSWithCaron = Tag_Entity "&#353;"

-- | The latin capital letter t with cedilla HTML entity ('Ţ').
latinCapitalLetterTWithCedilla :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinCapitalLetterTWithCedilla = Tag_Entity "&#354;"

-- | The latin small letter t with cedilla HTML entity ('ţ').
latinSmallLetterTWithCedilla :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinSmallLetterTWithCedilla = Tag_Entity "&#355;"

-- | The latin capital letter t with caron HTML entity ('Ť').
latinCapitalLetterTWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterTWithCaron = Tag_Entity "&#356;"

-- | The latin small letter t with caron HTML entity ('ť').
latinSmallLetterTWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterTWithCaron = Tag_Entity "&#357;"

-- | The latin capital letter t with stroke HTML entity ('Ŧ').
latinCapitalLetterTWithStroke :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterTWithStroke = Tag_Entity "&#358;"

-- | The latin small letter t with stroke HTML entity ('ŧ').
latinSmallLetterTWithStroke :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterTWithStroke = Tag_Entity "&#359;"

-- | The latin capital letter u with tilde HTML entity ('Ũ').
latinCapitalLetterUWithTilde :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterUWithTilde = Tag_Entity "&#360;"

-- | The latin small letter u with tilde HTML entity ('ũ').
latinSmallLetterUWithTilde :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterUWithTilde = Tag_Entity "&#361;"

-- | The latin capital letter u with macron HTML entity ('Ū').
latinCapitalLetterUWithMacron :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterUWithMacron = Tag_Entity "&#362;"

-- | The latin small letter u with macron HTML entity ('ū').
latinSmallLetterUWithMacron :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterUWithMacron = Tag_Entity "&#363;"

-- | The latin capital letter u with breve HTML entity ('Ŭ').
latinCapitalLetterUWithBreve :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterUWithBreve = Tag_Entity "&#364;"

-- | The latin small letter u with breve HTML entity ('ŭ').
latinSmallLetterUWithBreve :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterUWithBreve = Tag_Entity "&#365;"

-- | The latin capital letter u with ring above HTML entity ('Ů').
latinCapitalLetterUWithRingAbove :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterUWithRingAbove = Tag_Entity "&#366;"

-- | The latin small letter u with ring above HTML entity ('ů').
latinSmallLetterUWithRingAbove :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
latinSmallLetterUWithRingAbove = Tag_Entity "&#367;"

-- | The latin capital letter u with double acute HTML entity ('Ű').
latinCapitalLetterUWithDoubleAcute :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
latinCapitalLetterUWithDoubleAcute = Tag_Entity "&#368;"

-- | The latin small letter u with double acute HTML entity ('ű').
latinSmallLetterUWithDoubleAcute :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinSmallLetterUWithDoubleAcute = Tag_Entity "&#369;"

-- | The latin capital letter u with ogonek HTML entity ('Ų').
latinCapitalLetterUWithOgonek :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterUWithOgonek = Tag_Entity "&#370;"

-- | The latin small letter u with ogonek HTML entity ('ų').
latinSmallLetterUWithOgonek :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterUWithOgonek = Tag_Entity "&#371;"

-- | The latin capital letter w with circumflex HTML entity ('Ŵ').
latinCapitalLetterWWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterWWithCircumflex = Tag_Entity "&#372;"

-- | The latin small letter w with circumflex HTML entity ('ŵ').
latinSmallLetterWWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterWWithCircumflex = Tag_Entity "&#373;"

-- | The latin capital letter y with circumflex HTML entity ('Ŷ').
latinCapitalLetterYWithCircumflex :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinCapitalLetterYWithCircumflex = Tag_Entity "&#374;"

-- | The latin small letter y with circumflex HTML entity ('ŷ').
latinSmallLetterYWithCircumflex :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinSmallLetterYWithCircumflex = Tag_Entity "&#375;"

-- | The latin capital letter y with diaeresis HTML entity ('Ÿ').
latinCapitalLetterYWithDiaeresis :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
latinCapitalLetterYWithDiaeresis = Tag_Entity "&#376;"

-- | The latin capital letter z with acute HTML entity ('Ź').
latinCapitalLetterZWithAcute :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterZWithAcute = Tag_Entity "&#377;"

-- | The latin small letter z with acute HTML entity ('ź').
latinSmallLetterZWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterZWithAcute = Tag_Entity "&#378;"

-- | The latin capital letter z with dot above HTML entity ('Ż').
latinCapitalLetterZWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
latinCapitalLetterZWithDotAbove = Tag_Entity "&#379;"

-- | The latin small letter z with dot above HTML entity ('ż').
latinSmallLetterZWithDotAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinSmallLetterZWithDotAbove = Tag_Entity "&#380;"

-- | The latin capital letter z with caron HTML entity ('Ž').
latinCapitalLetterZWithCaron :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
latinCapitalLetterZWithCaron = Tag_Entity "&#381;"

-- | The latin small letter z with caron HTML entity ('ž').
latinSmallLetterZWithCaron :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterZWithCaron = Tag_Entity "&#382;"

-- | The Latin Small Letter Turned Delta HTML entity ('ƍ').
latinSmallLetterTurnedDelta :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
latinSmallLetterTurnedDelta = Tag_Entity "&#397;"

-- | The latin small f with hook - function  florin HTML entity ('ƒ').
latinSmallFWithHookFunctionFlorin :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
latinSmallFWithHookFunctionFlorin = Tag_Entity "&#402;"

-- | The latin capital letter z with stroke HTML entity ('Ƶ').
latinCapitalLetterZWithStroke :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
latinCapitalLetterZWithStroke = Tag_Entity "&#437;"

-- | The latin small letter g with acute HTML entity ('ǵ').
latinSmallLetterGWithAcute :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
latinSmallLetterGWithAcute = Tag_Entity "&#501;"

-- | The latin small letter dotless j HTML entity ('ȷ').
latinSmallLetterDotlessJ :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
latinSmallLetterDotlessJ = Tag_Entity "&#567;"

-- | The modifier letter circumflex accent HTML entity ('ˆ').
modifierLetterCircumflexAccent :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
modifierLetterCircumflexAccent = Tag_Entity "&#710;"

-- | The caron HTML entity ('ˇ').
caron :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
caron = Tag_Entity "&#711;"

-- | The Modifier Letter Plus Sign HTML entity ('˖').
modifierLetterPlusSign :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
modifierLetterPlusSign = Tag_Entity "&#726;"

-- | The Modifier Letter Minus Sign HTML entity ('˗').
modifierLetterMinusSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
modifierLetterMinusSign = Tag_Entity "&#727;"

-- | The breve HTML entity ('˘').
breve :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
breve = Tag_Entity "&#728;"

-- | The dot above HTML entity ('˙').
dotAbove :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
dotAbove = Tag_Entity "&#729;"

-- | The ring above HTML entity ('˚').
ringAbove :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
ringAbove = Tag_Entity "&#730;"

-- | The ogonek HTML entity ('˛').
ogonek :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
ogonek = Tag_Entity "&#731;"

-- | The small tilde HTML entity ('˜').
smallTilde :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
smallTilde = Tag_Entity "&#732;"

-- | The double acute accent HTML entity ('˝').
doubleAcuteAccent :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
doubleAcuteAccent = Tag_Entity "&#733;"

-- | The combining inverted breve HTML entity ('̑').
combiningInvertedBreve :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
combiningInvertedBreve = Tag_Entity "&#785;"

-- | The Combining Left Angle Above HTML entity ('̚').
combiningLeftAngleAbove :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
combiningLeftAngleAbove = Tag_Entity "&#794;"

-- | The Combining Plus Sign Below HTML entity ('̟').
combiningPlusSignBelow :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
combiningPlusSignBelow = Tag_Entity "&#799;"

-- | The Combining Minus Sign Below HTML entity ('̠').
combiningMinusSignBelow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
combiningMinusSignBelow = Tag_Entity "&#800;"

-- | The combining low line HTML entity ('̲').
combiningLowLine :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
combiningLowLine = Tag_Entity "&#818;"

-- | The Combining Equals Sign Below HTML entity ('͇').
combiningEqualsSignBelow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
combiningEqualsSignBelow = Tag_Entity "&#839;"

-- | The Combining Left Angle Below HTML entity ('͉').
combiningLeftAngleBelow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
combiningLeftAngleBelow = Tag_Entity "&#841;"

-- | The Combining Almost Equal To Above HTML entity ('͌').
combiningAlmostEqualToAbove :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
combiningAlmostEqualToAbove = Tag_Entity "&#844;"

-- | The greek capital letter alpha HTML entity ('Α').
greekCapitalLetterAlpha :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterAlpha = Tag_Entity "&#913;"

-- | The greek capital letter beta HTML entity ('Β').
greekCapitalLetterBeta :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
greekCapitalLetterBeta = Tag_Entity "&#914;"

-- | The greek capital letter gamma HTML entity ('Γ').
greekCapitalLetterGamma :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterGamma = Tag_Entity "&#915;"

-- | The greek capital letter delta HTML entity ('Δ').
greekCapitalLetterDelta :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterDelta = Tag_Entity "&#916;"

-- | The greek capital letter epsilon HTML entity ('Ε').
greekCapitalLetterEpsilon :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
greekCapitalLetterEpsilon = Tag_Entity "&#917;"

-- | The greek capital letter zeta HTML entity ('Ζ').
greekCapitalLetterZeta :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
greekCapitalLetterZeta = Tag_Entity "&#918;"

-- | The greek capital letter eta HTML entity ('Η').
greekCapitalLetterEta :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterEta = Tag_Entity "&#919;"

-- | The greek capital letter theta HTML entity ('Θ').
greekCapitalLetterTheta :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterTheta = Tag_Entity "&#920;"

-- | The greek capital letter iota HTML entity ('Ι').
greekCapitalLetterIota :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
greekCapitalLetterIota = Tag_Entity "&#921;"

-- | The greek capital letter kappa HTML entity ('Κ').
greekCapitalLetterKappa :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterKappa = Tag_Entity "&#922;"

-- | The greek capital letter lambda HTML entity ('Λ').
greekCapitalLetterLambda :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
greekCapitalLetterLambda = Tag_Entity "&#923;"

-- | The greek capital letter mu HTML entity ('Μ').
greekCapitalLetterMu :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekCapitalLetterMu = Tag_Entity "&#924;"

-- | The greek capital letter nu HTML entity ('Ν').
greekCapitalLetterNu :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekCapitalLetterNu = Tag_Entity "&#925;"

-- | The greek capital letter xi HTML entity ('Ξ').
greekCapitalLetterXi :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekCapitalLetterXi = Tag_Entity "&#926;"

-- | The greek capital letter omicron HTML entity ('Ο').
greekCapitalLetterOmicron :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
greekCapitalLetterOmicron = Tag_Entity "&#927;"

-- | The greek capital letter pi HTML entity ('Π').
greekCapitalLetterPi :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekCapitalLetterPi = Tag_Entity "&#928;"

-- | The greek capital letter rho HTML entity ('Ρ').
greekCapitalLetterRho :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterRho = Tag_Entity "&#929;"

-- | The greek capital letter sigma HTML entity ('Σ').
greekCapitalLetterSigma :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterSigma = Tag_Entity "&#931;"

-- | The greek capital letter tau HTML entity ('Τ').
greekCapitalLetterTau :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterTau = Tag_Entity "&#932;"

-- | The greek capital letter upsilon HTML entity ('Υ').
greekCapitalLetterUpsilon :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
greekCapitalLetterUpsilon = Tag_Entity "&#933;"

-- | The greek capital letter phi HTML entity ('Φ').
greekCapitalLetterPhi :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterPhi = Tag_Entity "&#934;"

-- | The greek capital letter chi HTML entity ('Χ').
greekCapitalLetterChi :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterChi = Tag_Entity "&#935;"

-- | The greek capital letter psi HTML entity ('Ψ').
greekCapitalLetterPsi :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekCapitalLetterPsi = Tag_Entity "&#936;"

-- | The greek capital letter omega HTML entity ('Ω').
greekCapitalLetterOmega :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekCapitalLetterOmega = Tag_Entity "&#937;"

-- | The greek small letter alpha HTML entity ('α').
greekSmallLetterAlpha :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterAlpha = Tag_Entity "&#945;"

-- | The greek small letter beta HTML entity ('β').
greekSmallLetterBeta :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekSmallLetterBeta = Tag_Entity "&#946;"

-- | The greek small letter gamma HTML entity ('γ').
greekSmallLetterGamma :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterGamma = Tag_Entity "&#947;"

-- | The greek small letter delta HTML entity ('δ').
greekSmallLetterDelta :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterDelta = Tag_Entity "&#948;"

-- | The greek small letter epsilon HTML entity ('ε').
greekSmallLetterEpsilon :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekSmallLetterEpsilon = Tag_Entity "&#949;"

-- | The greek small letter zeta HTML entity ('ζ').
greekSmallLetterZeta :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekSmallLetterZeta = Tag_Entity "&#950;"

-- | The greek small letter eta HTML entity ('η').
greekSmallLetterEta :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterEta = Tag_Entity "&#951;"

-- | The greek small letter theta HTML entity ('θ').
greekSmallLetterTheta :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterTheta = Tag_Entity "&#952;"

-- | The greek small letter iota HTML entity ('ι').
greekSmallLetterIota :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greekSmallLetterIota = Tag_Entity "&#953;"

-- | The greek small letter kappa HTML entity ('κ').
greekSmallLetterKappa :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterKappa = Tag_Entity "&#954;"

-- | The greek small letter lambda HTML entity ('λ').
greekSmallLetterLambda :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
greekSmallLetterLambda = Tag_Entity "&#955;"

-- | The greek small letter mu HTML entity ('μ').
greekSmallLetterMu :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greekSmallLetterMu = Tag_Entity "&#956;"

-- | The greek small letter nu HTML entity ('ν').
greekSmallLetterNu :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greekSmallLetterNu = Tag_Entity "&#957;"

-- | The greek small letter xi HTML entity ('ξ').
greekSmallLetterXi :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greekSmallLetterXi = Tag_Entity "&#958;"

-- | The greek small letter omicron HTML entity ('ο').
greekSmallLetterOmicron :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekSmallLetterOmicron = Tag_Entity "&#959;"

-- | The greek small letter pi HTML entity ('π').
greekSmallLetterPi :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greekSmallLetterPi = Tag_Entity "&#960;"

-- | The greek small letter rho HTML entity ('ρ').
greekSmallLetterRho :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterRho = Tag_Entity "&#961;"

-- | The greek small letter final sigma HTML entity ('ς').
greekSmallLetterFinalSigma :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
greekSmallLetterFinalSigma = Tag_Entity "&#962;"

-- | The greek small letter sigma HTML entity ('σ').
greekSmallLetterSigma :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterSigma = Tag_Entity "&#963;"

-- | The greek small letter tau HTML entity ('τ').
greekSmallLetterTau :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterTau = Tag_Entity "&#964;"

-- | The greek small letter upsilon HTML entity ('υ').
greekSmallLetterUpsilon :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekSmallLetterUpsilon = Tag_Entity "&#965;"

-- | The greek small letter phi HTML entity ('φ').
greekSmallLetterPhi :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterPhi = Tag_Entity "&#966;"

-- | The greek small letter chi HTML entity ('χ').
greekSmallLetterChi :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterChi = Tag_Entity "&#967;"

-- | The greek small letter psi HTML entity ('ψ').
greekSmallLetterPsi :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
greekSmallLetterPsi = Tag_Entity "&#968;"

-- | The greek small letter omega HTML entity ('ω').
greekSmallLetterOmega :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greekSmallLetterOmega = Tag_Entity "&#969;"

-- | The greek theta symbol HTML entity ('ϑ').
greekThetaSymbol :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
greekThetaSymbol = Tag_Entity "&#977;"

-- | The greek upsilon with hook symbol HTML entity ('ϒ').
greekUpsilonWithHookSymbol :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
greekUpsilonWithHookSymbol = Tag_Entity "&#978;"

-- | The greek phi symbol HTML entity ('ϕ').
greekPhiSymbol :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
greekPhiSymbol = Tag_Entity "&#981;"

-- | The greek pi symbol HTML entity ('ϖ').
greekPiSymbol :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
greekPiSymbol = Tag_Entity "&#982;"

-- | The greek letter digamma HTML entity ('Ϝ').
greekLetterDigamma :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greekLetterDigamma = Tag_Entity "&#988;"

-- | The greek small letter digamma HTML entity ('ϝ').
greekSmallLetterDigamma :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
greekSmallLetterDigamma = Tag_Entity "&#989;"

-- | The greek kappa symbol HTML entity ('ϰ').
greekKappaSymbol :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
greekKappaSymbol = Tag_Entity "&#1008;"

-- | The greek rho symbol HTML entity ('ϱ').
greekRhoSymbol :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
greekRhoSymbol = Tag_Entity "&#1009;"

-- | The greek lunate epsilon symbol HTML entity ('ϵ').
greekLunateEpsilonSymbol :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
greekLunateEpsilonSymbol = Tag_Entity "&#1013;"

-- | The greek reversed lunate epsilon symbol HTML entity ('϶').
greekReversedLunateEpsilonSymbol :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
greekReversedLunateEpsilonSymbol = Tag_Entity "&#1014;"

-- | The cyrillic capital letter io HTML entity ('Ё').
cyrillicCapitalLetterIo :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterIo = Tag_Entity "&#1025;"

-- | The cyrillic capital letter dje HTML entity ('Ђ').
cyrillicCapitalLetterDje :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterDje = Tag_Entity "&#1026;"

-- | The cyrillic capital letter gje HTML entity ('Ѓ').
cyrillicCapitalLetterGje :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterGje = Tag_Entity "&#1027;"

-- | The cyrillic capital letter ukrainian ie HTML entity ('Є').
cyrillicCapitalLetterUkrainianIe :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
cyrillicCapitalLetterUkrainianIe = Tag_Entity "&#1028;"

-- | The cyrillic capital letter dze HTML entity ('Ѕ').
cyrillicCapitalLetterDze :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterDze = Tag_Entity "&#1029;"

-- | The cyrillic capital letter byelorussian-ukrainian i HTML entity ('І').
cyrillicCapitalLetterByelorussianUkrainianI :: ValidChild Text parent grandparent
                                            => ChildHTML parent grandparent
cyrillicCapitalLetterByelorussianUkrainianI = Tag_Entity "&#1030;"

-- | The cyrillic capital letter yi HTML entity ('Ї').
cyrillicCapitalLetterYi :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterYi = Tag_Entity "&#1031;"

-- | The cyrillic capital letter je HTML entity ('Ј').
cyrillicCapitalLetterJe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterJe = Tag_Entity "&#1032;"

-- | The cyrillic capital letter lje HTML entity ('Љ').
cyrillicCapitalLetterLje :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterLje = Tag_Entity "&#1033;"

-- | The cyrillic capital letter nje HTML entity ('Њ').
cyrillicCapitalLetterNje :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterNje = Tag_Entity "&#1034;"

-- | The cyrillic capital letter tshe HTML entity ('Ћ').
cyrillicCapitalLetterTshe :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
cyrillicCapitalLetterTshe = Tag_Entity "&#1035;"

-- | The cyrillic capital letter kje HTML entity ('Ќ').
cyrillicCapitalLetterKje :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterKje = Tag_Entity "&#1036;"

-- | The cyrillic capital letter short u HTML entity ('Ў').
cyrillicCapitalLetterShortU :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
cyrillicCapitalLetterShortU = Tag_Entity "&#1038;"

-- | The cyrillic capital letter dzhe HTML entity ('Џ').
cyrillicCapitalLetterDzhe :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
cyrillicCapitalLetterDzhe = Tag_Entity "&#1039;"

-- | The cyrillic capital letter a HTML entity ('А').
cyrillicCapitalLetterA :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicCapitalLetterA = Tag_Entity "&#1040;"

-- | The cyrillic capital letter be HTML entity ('Б').
cyrillicCapitalLetterBe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterBe = Tag_Entity "&#1041;"

-- | The cyrillic capital letter ve HTML entity ('В').
cyrillicCapitalLetterVe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterVe = Tag_Entity "&#1042;"

-- | The cyrillic capital letter ghe HTML entity ('Г').
cyrillicCapitalLetterGhe :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterGhe = Tag_Entity "&#1043;"

-- | The cyrillic capital letter de HTML entity ('Д').
cyrillicCapitalLetterDe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterDe = Tag_Entity "&#1044;"

-- | The cyrillic capital letter ie HTML entity ('Е').
cyrillicCapitalLetterIe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterIe = Tag_Entity "&#1045;"

-- | The cyrillic capital letter zhe HTML entity ('Ж').
cyrillicCapitalLetterZhe :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterZhe = Tag_Entity "&#1046;"

-- | The cyrillic capital letter ze HTML entity ('З').
cyrillicCapitalLetterZe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterZe = Tag_Entity "&#1047;"

-- | The cyrillic capital letter i HTML entity ('И').
cyrillicCapitalLetterI :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicCapitalLetterI = Tag_Entity "&#1048;"

-- | The cyrillic capital letter short i HTML entity ('Й').
cyrillicCapitalLetterShortI :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
cyrillicCapitalLetterShortI = Tag_Entity "&#1049;"

-- | The cyrillic capital letter ka HTML entity ('К').
cyrillicCapitalLetterKa :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterKa = Tag_Entity "&#1050;"

-- | The cyrillic capital letter el HTML entity ('Л').
cyrillicCapitalLetterEl :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEl = Tag_Entity "&#1051;"

-- | The cyrillic capital letter em HTML entity ('М').
cyrillicCapitalLetterEm :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEm = Tag_Entity "&#1052;"

-- | The cyrillic capital letter en HTML entity ('Н').
cyrillicCapitalLetterEn :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEn = Tag_Entity "&#1053;"

-- | The cyrillic capital letter o HTML entity ('О').
cyrillicCapitalLetterO :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicCapitalLetterO = Tag_Entity "&#1054;"

-- | The cyrillic capital letter pe HTML entity ('П').
cyrillicCapitalLetterPe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterPe = Tag_Entity "&#1055;"

-- | The cyrillic capital letter er HTML entity ('Р').
cyrillicCapitalLetterEr :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEr = Tag_Entity "&#1056;"

-- | The cyrillic capital letter es HTML entity ('С').
cyrillicCapitalLetterEs :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEs = Tag_Entity "&#1057;"

-- | The cyrillic capital letter te HTML entity ('Т').
cyrillicCapitalLetterTe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterTe = Tag_Entity "&#1058;"

-- | The cyrillic capital letter u HTML entity ('У').
cyrillicCapitalLetterU :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicCapitalLetterU = Tag_Entity "&#1059;"

-- | The cyrillic capital letter ef HTML entity ('Ф').
cyrillicCapitalLetterEf :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterEf = Tag_Entity "&#1060;"

-- | The cyrillic capital letter ha HTML entity ('Х').
cyrillicCapitalLetterHa :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterHa = Tag_Entity "&#1061;"

-- | The cyrillic capital letter tse HTML entity ('Ц').
cyrillicCapitalLetterTse :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterTse = Tag_Entity "&#1062;"

-- | The cyrillic capital letter che HTML entity ('Ч').
cyrillicCapitalLetterChe :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterChe = Tag_Entity "&#1063;"

-- | The cyrillic capital letter sha HTML entity ('Ш').
cyrillicCapitalLetterSha :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicCapitalLetterSha = Tag_Entity "&#1064;"

-- | The cyrillic capital letter shcha HTML entity ('Щ').
cyrillicCapitalLetterShcha :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
cyrillicCapitalLetterShcha = Tag_Entity "&#1065;"

-- | The cyrillic capital letter hard sign HTML entity ('Ъ').
cyrillicCapitalLetterHardSign :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
cyrillicCapitalLetterHardSign = Tag_Entity "&#1066;"

-- | The cyrillic capital letter yeru HTML entity ('Ы').
cyrillicCapitalLetterYeru :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
cyrillicCapitalLetterYeru = Tag_Entity "&#1067;"

-- | The cyrillic capital letter soft sign HTML entity ('Ь').
cyrillicCapitalLetterSoftSign :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
cyrillicCapitalLetterSoftSign = Tag_Entity "&#1068;"

-- | The cyrillic capital letter e HTML entity ('Э').
cyrillicCapitalLetterE :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicCapitalLetterE = Tag_Entity "&#1069;"

-- | The cyrillic capital letter yu HTML entity ('Ю').
cyrillicCapitalLetterYu :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterYu = Tag_Entity "&#1070;"

-- | The cyrillic capital letter ya HTML entity ('Я').
cyrillicCapitalLetterYa :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicCapitalLetterYa = Tag_Entity "&#1071;"

-- | The cyrillic small letter a HTML entity ('а').
cyrillicSmallLetterA :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
cyrillicSmallLetterA = Tag_Entity "&#1072;"

-- | The cyrillic small letter be HTML entity ('б').
cyrillicSmallLetterBe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterBe = Tag_Entity "&#1073;"

-- | The cyrillic small letter ve HTML entity ('в').
cyrillicSmallLetterVe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterVe = Tag_Entity "&#1074;"

-- | The cyrillic small letter ghe HTML entity ('г').
cyrillicSmallLetterGhe :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterGhe = Tag_Entity "&#1075;"

-- | The cyrillic small letter de HTML entity ('д').
cyrillicSmallLetterDe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterDe = Tag_Entity "&#1076;"

-- | The cyrillic small letter ie HTML entity ('е').
cyrillicSmallLetterIe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterIe = Tag_Entity "&#1077;"

-- | The cyrillic small letter zhe HTML entity ('ж').
cyrillicSmallLetterZhe :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterZhe = Tag_Entity "&#1078;"

-- | The cyrillic small letter ze HTML entity ('з').
cyrillicSmallLetterZe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterZe = Tag_Entity "&#1079;"

-- | The cyrillic small letter i HTML entity ('и').
cyrillicSmallLetterI :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
cyrillicSmallLetterI = Tag_Entity "&#1080;"

-- | The cyrillic small letter short i HTML entity ('й').
cyrillicSmallLetterShortI :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
cyrillicSmallLetterShortI = Tag_Entity "&#1081;"

-- | The cyrillic small letter ka HTML entity ('к').
cyrillicSmallLetterKa :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterKa = Tag_Entity "&#1082;"

-- | The cyrillic small letter el HTML entity ('л').
cyrillicSmallLetterEl :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEl = Tag_Entity "&#1083;"

-- | The cyrillic small letter em HTML entity ('м').
cyrillicSmallLetterEm :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEm = Tag_Entity "&#1084;"

-- | The cyrillic small letter en HTML entity ('н').
cyrillicSmallLetterEn :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEn = Tag_Entity "&#1085;"

-- | The cyrillic small letter o HTML entity ('о').
cyrillicSmallLetterO :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
cyrillicSmallLetterO = Tag_Entity "&#1086;"

-- | The cyrillic small letter pe HTML entity ('п').
cyrillicSmallLetterPe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterPe = Tag_Entity "&#1087;"

-- | The cyrillic small letter er HTML entity ('р').
cyrillicSmallLetterEr :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEr = Tag_Entity "&#1088;"

-- | The cyrillic small letter es HTML entity ('с').
cyrillicSmallLetterEs :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEs = Tag_Entity "&#1089;"

-- | The cyrillic small letter te HTML entity ('т').
cyrillicSmallLetterTe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterTe = Tag_Entity "&#1090;"

-- | The cyrillic small letter u HTML entity ('у').
cyrillicSmallLetterU :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
cyrillicSmallLetterU = Tag_Entity "&#1091;"

-- | The cyrillic small letter ef HTML entity ('ф').
cyrillicSmallLetterEf :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterEf = Tag_Entity "&#1092;"

-- | The cyrillic small letter ha HTML entity ('х').
cyrillicSmallLetterHa :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterHa = Tag_Entity "&#1093;"

-- | The cyrillic small letter tse HTML entity ('ц').
cyrillicSmallLetterTse :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterTse = Tag_Entity "&#1094;"

-- | The cyrillic small letter che HTML entity ('ч').
cyrillicSmallLetterChe :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterChe = Tag_Entity "&#1095;"

-- | The cyrillic small letter sha HTML entity ('ш').
cyrillicSmallLetterSha :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterSha = Tag_Entity "&#1096;"

-- | The cyrillic small letter shcha HTML entity ('щ').
cyrillicSmallLetterShcha :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
cyrillicSmallLetterShcha = Tag_Entity "&#1097;"

-- | The cyrillic small letter hard sign HTML entity ('ъ').
cyrillicSmallLetterHardSign :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
cyrillicSmallLetterHardSign = Tag_Entity "&#1098;"

-- | The cyrillic small letter yeru HTML entity ('ы').
cyrillicSmallLetterYeru :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicSmallLetterYeru = Tag_Entity "&#1099;"

-- | The cyrillic small letter soft sign HTML entity ('ь').
cyrillicSmallLetterSoftSign :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
cyrillicSmallLetterSoftSign = Tag_Entity "&#1100;"

-- | The cyrillic small letter e HTML entity ('э').
cyrillicSmallLetterE :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
cyrillicSmallLetterE = Tag_Entity "&#1101;"

-- | The cyrillic small letter yu HTML entity ('ю').
cyrillicSmallLetterYu :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterYu = Tag_Entity "&#1102;"

-- | The cyrillic small letter ya HTML entity ('я').
cyrillicSmallLetterYa :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterYa = Tag_Entity "&#1103;"

-- | The cyrillic small letter io HTML entity ('ё').
cyrillicSmallLetterIo :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterIo = Tag_Entity "&#1105;"

-- | The cyrillic small letter dje HTML entity ('ђ').
cyrillicSmallLetterDje :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterDje = Tag_Entity "&#1106;"

-- | The cyrillic small letter gje HTML entity ('ѓ').
cyrillicSmallLetterGje :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterGje = Tag_Entity "&#1107;"

-- | The cyrillic small letter ukrainian ie HTML entity ('є').
cyrillicSmallLetterUkrainianIe :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
cyrillicSmallLetterUkrainianIe = Tag_Entity "&#1108;"

-- | The cyrillic small letter dze HTML entity ('ѕ').
cyrillicSmallLetterDze :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterDze = Tag_Entity "&#1109;"

-- | The cyrillic small letter byelorussian-ukrainian i HTML entity ('і').
cyrillicSmallLetterByelorussianUkrainianI :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
cyrillicSmallLetterByelorussianUkrainianI = Tag_Entity "&#1110;"

-- | The cyrillic small letter yi HTML entity ('ї').
cyrillicSmallLetterYi :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterYi = Tag_Entity "&#1111;"

-- | The cyrillic small letter je HTML entity ('ј').
cyrillicSmallLetterJe :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
cyrillicSmallLetterJe = Tag_Entity "&#1112;"

-- | The cyrillic small letter lje HTML entity ('љ').
cyrillicSmallLetterLje :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterLje = Tag_Entity "&#1113;"

-- | The cyrillic small letter nje HTML entity ('њ').
cyrillicSmallLetterNje :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterNje = Tag_Entity "&#1114;"

-- | The cyrillic small letter tshe HTML entity ('ћ').
cyrillicSmallLetterTshe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicSmallLetterTshe = Tag_Entity "&#1115;"

-- | The cyrillic small letter kje HTML entity ('ќ').
cyrillicSmallLetterKje :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
cyrillicSmallLetterKje = Tag_Entity "&#1116;"

-- | The cyrillic small letter short u HTML entity ('ў').
cyrillicSmallLetterShortU :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
cyrillicSmallLetterShortU = Tag_Entity "&#1118;"

-- | The cyrillic small letter dzhe HTML entity ('џ').
cyrillicSmallLetterDzhe :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
cyrillicSmallLetterDzhe = Tag_Entity "&#1119;"

-- | The Arabic Percent Sign HTML entity ('٪').
arabicPercentSign :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
arabicPercentSign = Tag_Entity "&#1642;"

-- | The Canadian Syllabics Final Plus HTML entity ('ᐩ').
canadianSyllabicsFinalPlus :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
canadianSyllabicsFinalPlus = Tag_Entity "&#5161;"

-- | The Modifier Letter Small Delta HTML entity ('ᵟ').
modifierLetterSmallDelta :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
modifierLetterSmallDelta = Tag_Entity "&#7519;"

-- | The Latin Small Letter Delta HTML entity ('ẟ').
latinSmallLetterDelta :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinSmallLetterDelta = Tag_Entity "&#7839;"

-- | The en space HTML entity.
enSpace :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
enSpace = Tag_Entity "&#8194;"

-- | The em space HTML entity.
emSpace :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
emSpace = Tag_Entity "&#8195;"

-- | The three-per-em space HTML entity.
threePerEmSpace :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
threePerEmSpace = Tag_Entity "&#8196;"

-- | The four-per-em space HTML entity.
fourPerEmSpace :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
fourPerEmSpace = Tag_Entity "&#8197;"

-- | The figure space HTML entity.
figureSpace :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
figureSpace = Tag_Entity "&#8199;"

-- | The punctuation space HTML entity.
punctuationSpace :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
punctuationSpace = Tag_Entity "&#8200;"

-- | The thin space HTML entity.
thinSpace :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
thinSpace = Tag_Entity "&#8201;"

-- | The hair space HTML entity.
hairSpace :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
hairSpace = Tag_Entity "&#8202;"

-- | The zero width space HTML entity.
zeroWidthSpace :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
zeroWidthSpace = Tag_Entity "&#8203;"

-- | The zero width non-joiner HTML entity.
zeroWidthNonJoiner :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
zeroWidthNonJoiner = Tag_Entity "&#8204;"

-- | The zero width joiner HTML entity.
zeroWidthJoiner :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
zeroWidthJoiner = Tag_Entity "&#8205;"

-- | The left-to-right mark HTML entity.
leftToRightMark :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
leftToRightMark = Tag_Entity "&#8206;"

-- | The right-to-left mark HTML entity.
rightToLeftMark :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
rightToLeftMark = Tag_Entity "&#8207;"

-- | The hyphen HTML entity ('‐').
hyphen :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
hyphen = Tag_Entity "&#8208;"

-- | The en dash HTML entity ('–').
enDash :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
enDash = Tag_Entity "&#8211;"

-- | The em dash HTML entity ('—').
emDash :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
emDash = Tag_Entity "&#8212;"

-- | The horizontal bar HTML entity ('―').
horizontalBar :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
horizontalBar = Tag_Entity "&#8213;"

-- | The double vertical line HTML entity ('‖').
doubleVerticalLine :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
doubleVerticalLine = Tag_Entity "&#8214;"

-- | The left single quotation mark HTML entity ('‘').
leftSingleQuotationMark :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
leftSingleQuotationMark = Tag_Entity "&#8216;"

-- | The right single quotation mark HTML entity ('’').
rightSingleQuotationMark :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
rightSingleQuotationMark = Tag_Entity "&#8217;"

-- | The single low-9 quotation mark HTML entity ('‚').
singleLow9QuotationMark :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
singleLow9QuotationMark = Tag_Entity "&#8218;"

-- | The left double quotation mark HTML entity ('“').
leftDoubleQuotationMark :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
leftDoubleQuotationMark = Tag_Entity "&#8220;"

-- | The right double quotation mark HTML entity ('”').
rightDoubleQuotationMark :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
rightDoubleQuotationMark = Tag_Entity "&#8221;"

-- | The double low-9 quotation mark HTML entity ('„').
doubleLow9QuotationMark :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
doubleLow9QuotationMark = Tag_Entity "&#8222;"

-- | The dagger HTML entity ('†').
dagger :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
dagger = Tag_Entity "&#8224;"

-- | The double dagger HTML entity ('‡').
doubleDagger :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
doubleDagger = Tag_Entity "&#8225;"

-- | The bullet HTML entity ('•').
bullet :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
bullet = Tag_Entity "&#8226;"

-- | The two dot leader HTML entity ('‥').
twoDotLeader :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
twoDotLeader = Tag_Entity "&#8229;"

-- | The horizontal ellipsis HTML entity ('…').
horizontalEllipsis :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
horizontalEllipsis = Tag_Entity "&#8230;"

-- | The per mille sign - per thousand sign HTML entity ('‰').
perMilleSignPerThousandSign :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
perMilleSignPerThousandSign = Tag_Entity "&#8240;"

-- | The per ten thousand sign HTML entity ('‱').
perTenThousandSign :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
perTenThousandSign = Tag_Entity "&#8241;"

-- | The prime = minutes = feet HTML entity ('′').
prime :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
prime = Tag_Entity "&#8242;"

-- | The double prime = seconds = inches HTML entity ('″').
doublePrime :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
doublePrime = Tag_Entity "&#8243;"

-- | The triple prime HTML entity ('‴').
triplePrime :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
triplePrime = Tag_Entity "&#8244;"

-- | The reversed prime HTML entity ('‵').
reversedPrime :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
reversedPrime = Tag_Entity "&#8245;"

-- | The single left-pointing angle quotation mark HTML entity ('‹').
singleLeftPointingAngleQuotationMark :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
singleLeftPointingAngleQuotationMark = Tag_Entity "&#8249;"

-- | The single right-pointing angle quotation mark HTML entity ('›').
singleRightPointingAngleQuotationMark :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
singleRightPointingAngleQuotationMark = Tag_Entity "&#8250;"

-- | The overline = spacing overscore HTML entity ('‾').
overline :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
overline = Tag_Entity "&#8254;"

-- | The caret insertion point HTML entity ('⁁').
caretInsertionPoint :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
caretInsertionPoint = Tag_Entity "&#8257;"

-- | The hyphen bullet HTML entity ('⁃').
hyphenBullet :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
hyphenBullet = Tag_Entity "&#8259;"

-- | The fraction slash HTML entity ('⁄').
fractionSlash :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
fractionSlash = Tag_Entity "&#8260;"

-- | The reversed semicolon HTML entity ('⁏').
reversedSemicolon :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
reversedSemicolon = Tag_Entity "&#8271;"

-- | The Commercial Minus Sign HTML entity ('⁒').
commercialMinusSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
commercialMinusSign = Tag_Entity "&#8274;"

-- | The quadruple prime HTML entity ('⁗').
quadruplePrime :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
quadruplePrime = Tag_Entity "&#8279;"

-- | The medium mathematical space HTML entity.
mediumMathematicalSpace :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
mediumMathematicalSpace = Tag_Entity "&#8287;"

-- | The word joiner HTML entity.
wordJoiner :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
wordJoiner = Tag_Entity "&#8288;"

-- | The function application HTML entity.
functionApplication :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
functionApplication = Tag_Entity "&#8289;"

-- | The invisible times HTML entity.
invisibleTimes :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
invisibleTimes = Tag_Entity "&#8290;"

-- | The invisible separator HTML entity.
invisibleSeparator :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
invisibleSeparator = Tag_Entity "&#8291;"

-- | The Superscript Plus Sign HTML entity ('⁺').
superscriptPlusSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
superscriptPlusSign = Tag_Entity "&#8314;"

-- | The Superscript Minus HTML entity ('⁻').
superscriptMinus :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
superscriptMinus = Tag_Entity "&#8315;"

-- | The Superscript Equals Sign HTML entity ('⁼').
superscriptEqualsSign :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
superscriptEqualsSign = Tag_Entity "&#8316;"

-- | The Subscript Plus Sign HTML entity ('₊').
subscriptPlusSign :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
subscriptPlusSign = Tag_Entity "&#8330;"

-- | The Subscript Minus HTML entity ('₋').
subscriptMinus :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
subscriptMinus = Tag_Entity "&#8331;"

-- | The Subscript Equals Sign HTML entity ('₌').
subscriptEqualsSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
subscriptEqualsSign = Tag_Entity "&#8332;"

-- | The euro-currency sign HTML entity ('₠').
euroCurrencySign :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
euroCurrencySign = Tag_Entity "&#8352;"

-- | The colon sign HTML entity ('₡').
colonSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
colonSign = Tag_Entity "&#8353;"

-- | The cruzeiro sign HTML entity ('₢').
cruzeiroSign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
cruzeiroSign = Tag_Entity "&#8354;"

-- | The french franc sign HTML entity ('₣').
frenchFrancSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
frenchFrancSign = Tag_Entity "&#8355;"

-- | The lira sign HTML entity ('₤').
liraSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
liraSign = Tag_Entity "&#8356;"

-- | The mill sign HTML entity ('₥').
millSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
millSign = Tag_Entity "&#8357;"

-- | The naira sign HTML entity ('₦').
nairaSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
nairaSign = Tag_Entity "&#8358;"

-- | The peseta sign HTML entity ('₧').
pesetaSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
pesetaSign = Tag_Entity "&#8359;"

-- | The rupee sign HTML entity ('₨').
rupeeSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
rupeeSign = Tag_Entity "&#8360;"

-- | The won sign HTML entity ('₩').
wonSign :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
wonSign = Tag_Entity "&#8361;"

-- | The new sheqel sign HTML entity ('₪').
newSheqelSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
newSheqelSign = Tag_Entity "&#8362;"

-- | The dong sign HTML entity ('₫').
dongSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
dongSign = Tag_Entity "&#8363;"

-- | The euro sign HTML entity ('€').
euroSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
euroSign = Tag_Entity "&#8364;"

-- | The kip sign HTML entity ('₭').
kipSign :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
kipSign = Tag_Entity "&#8365;"

-- | The tugrik sign HTML entity ('₮').
tugrikSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
tugrikSign = Tag_Entity "&#8366;"

-- | The drachma sign HTML entity ('₯').
drachmaSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
drachmaSign = Tag_Entity "&#8367;"

-- | The german penny symbol HTML entity ('₰').
germanPennySymbol :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
germanPennySymbol = Tag_Entity "&#8368;"

-- | The peso sign HTML entity ('₱').
pesoSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
pesoSign = Tag_Entity "&#8369;"

-- | The guarani sign HTML entity ('₲').
guaraniSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
guaraniSign = Tag_Entity "&#8370;"

-- | The austral sign HTML entity ('₳').
australSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
australSign = Tag_Entity "&#8371;"

-- | The hryvnia sign HTML entity ('₴').
hryvniaSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
hryvniaSign = Tag_Entity "&#8372;"

-- | The cedi sign HTML entity ('₵').
cediSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
cediSign = Tag_Entity "&#8373;"

-- | The livre tournois sign HTML entity ('₶').
livreTournoisSign :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
livreTournoisSign = Tag_Entity "&#8374;"

-- | The spesmilo sign HTML entity ('₷').
spesmiloSign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
spesmiloSign = Tag_Entity "&#8375;"

-- | The tenge sign HTML entity ('₸').
tengeSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
tengeSign = Tag_Entity "&#8376;"

-- | The indian rupee sign HTML entity ('₹').
indianRupeeSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
indianRupeeSign = Tag_Entity "&#8377;"

-- | The turkish lira sign HTML entity ('₺').
turkishLiraSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
turkishLiraSign = Tag_Entity "&#8378;"

-- | The nordic mark sign HTML entity ('₻').
nordicMarkSign :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
nordicMarkSign = Tag_Entity "&#8379;"

-- | The manat sign HTML entity ('₼').
manatSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
manatSign = Tag_Entity "&#8380;"

-- | The ruble sign HTML entity ('₽').
rubleSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
rubleSign = Tag_Entity "&#8381;"

-- | The lari sign HTML entity ('₾').
lariSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
lariSign = Tag_Entity "&#8382;"

-- | The bitcoin sign HTML entity ('₿').
bitcoinSign :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
bitcoinSign = Tag_Entity "&#8383;"

-- | The combining three dots above HTML entity ('⃛').
combiningThreeDotsAbove :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
combiningThreeDotsAbove = Tag_Entity "&#8411;"

-- | The combining four dots above HTML entity ('⃜').
combiningFourDotsAbove :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
combiningFourDotsAbove = Tag_Entity "&#8412;"

-- | The double-struck capital c HTML entity ('ℂ').
doubleStruckCapitalC :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalC = Tag_Entity "&#8450;"

-- | The care of HTML entity ('℅').
careOf :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
careOf = Tag_Entity "&#8453;"

-- | The script small g HTML entity ('ℊ').
scriptSmallG :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
scriptSmallG = Tag_Entity "&#8458;"

-- | The script capital h HTML entity ('ℋ').
scriptCapitalH :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalH = Tag_Entity "&#8459;"

-- | The black-letter capital h HTML entity ('ℌ').
blackLetterCapitalH :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackLetterCapitalH = Tag_Entity "&#8460;"

-- | The double-struck capital h HTML entity ('ℍ').
doubleStruckCapitalH :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalH = Tag_Entity "&#8461;"

-- | The planck constant HTML entity ('ℎ').
planckConstant :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
planckConstant = Tag_Entity "&#8462;"

-- | The planck constant over two pi HTML entity ('ℏ').
planckConstantOverTwoPi :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
planckConstantOverTwoPi = Tag_Entity "&#8463;"

-- | The script capital i HTML entity ('ℐ').
scriptCapitalI :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalI = Tag_Entity "&#8464;"

-- | The black-letter capital i HTML entity ('ℑ').
blackLetterCapitalI :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackLetterCapitalI = Tag_Entity "&#8465;"

-- | The script capital l HTML entity ('ℒ').
scriptCapitalL :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalL = Tag_Entity "&#8466;"

-- | The script small l HTML entity ('ℓ').
scriptSmallL :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
scriptSmallL = Tag_Entity "&#8467;"

-- | The double-struck capital n HTML entity ('ℕ').
doubleStruckCapitalN :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalN = Tag_Entity "&#8469;"

-- | The numero sign HTML entity ('№').
numeroSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
numeroSign = Tag_Entity "&#8470;"

-- | The sound recording copyright HTML entity ('℗').
soundRecordingCopyright :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
soundRecordingCopyright = Tag_Entity "&#8471;"

-- | The script capital p HTML entity ('℘').
scriptCapitalP :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalP = Tag_Entity "&#8472;"

-- | The double-struck capital p HTML entity ('ℙ').
doubleStruckCapitalP :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalP = Tag_Entity "&#8473;"

-- | The double-struck capital q HTML entity ('ℚ').
doubleStruckCapitalQ :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalQ = Tag_Entity "&#8474;"

-- | The script capital r HTML entity ('ℛ').
scriptCapitalR :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalR = Tag_Entity "&#8475;"

-- | The black-letter capital r HTML entity ('ℜ').
blackLetterCapitalR :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackLetterCapitalR = Tag_Entity "&#8476;"

-- | The double-struck capital r HTML entity ('ℝ').
doubleStruckCapitalR :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalR = Tag_Entity "&#8477;"

-- | The prescription take HTML entity ('℞').
prescriptionTake :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
prescriptionTake = Tag_Entity "&#8478;"

-- | The trade mark sign HTML entity ('™').
tradeMarkSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
tradeMarkSign = Tag_Entity "&#8482;"

-- | The double-struck capital z HTML entity ('ℤ').
doubleStruckCapitalZ :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleStruckCapitalZ = Tag_Entity "&#8484;"

-- | The ohm sign HTML entity ('Ω').
ohmSign :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
ohmSign = Tag_Entity "&#8486;"

-- | The inverted ohm sign HTML entity ('℧').
invertedOhmSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
invertedOhmSign = Tag_Entity "&#8487;"

-- | The black-letter capital z HTML entity ('ℨ').
blackLetterCapitalZ :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackLetterCapitalZ = Tag_Entity "&#8488;"

-- | The turned greek small letter iota HTML entity ('℩').
turnedGreekSmallLetterIota :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
turnedGreekSmallLetterIota = Tag_Entity "&#8489;"

-- | The angstrom sign HTML entity ('Å').
angstromSign :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
angstromSign = Tag_Entity "&#8491;"

-- | The script capital b HTML entity ('ℬ').
scriptCapitalB :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalB = Tag_Entity "&#8492;"

-- | The black-letter capital c HTML entity ('ℭ').
blackLetterCapitalC :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackLetterCapitalC = Tag_Entity "&#8493;"

-- | The script small e HTML entity ('ℯ').
scriptSmallE :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
scriptSmallE = Tag_Entity "&#8495;"

-- | The script capital e HTML entity ('ℰ').
scriptCapitalE :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalE = Tag_Entity "&#8496;"

-- | The script capital f HTML entity ('ℱ').
scriptCapitalF :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalF = Tag_Entity "&#8497;"

-- | The script capital m HTML entity ('ℳ').
scriptCapitalM :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
scriptCapitalM = Tag_Entity "&#8499;"

-- | The script small o HTML entity ('ℴ').
scriptSmallO :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
scriptSmallO = Tag_Entity "&#8500;"

-- | The alef symbol HTML entity ('ℵ').
alefSymbol :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
alefSymbol = Tag_Entity "&#8501;"

-- | The bet symbol HTML entity ('ℶ').
betSymbol :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
betSymbol = Tag_Entity "&#8502;"

-- | The gimel symbol HTML entity ('ℷ').
gimelSymbol :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
gimelSymbol = Tag_Entity "&#8503;"

-- | The dalet symbol HTML entity ('ℸ').
daletSymbol :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
daletSymbol = Tag_Entity "&#8504;"

-- | The Double-struck N-ary Summation HTML entity ('⅀').
doubleStruckNArySummation :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
doubleStruckNArySummation = Tag_Entity "&#8512;"

-- | The double-struck italic capital d HTML entity ('ⅅ').
doubleStruckItalicCapitalD :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
doubleStruckItalicCapitalD = Tag_Entity "&#8517;"

-- | The double-struck italic small d HTML entity ('ⅆ').
doubleStruckItalicSmallD :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
doubleStruckItalicSmallD = Tag_Entity "&#8518;"

-- | The double-struck italic small e HTML entity ('ⅇ').
doubleStruckItalicSmallE :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
doubleStruckItalicSmallE = Tag_Entity "&#8519;"

-- | The double-struck italic small i HTML entity ('ⅈ').
doubleStruckItalicSmallI :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
doubleStruckItalicSmallI = Tag_Entity "&#8520;"

-- | The Vulgar Fraction One Seventh HTML entity ('⅐').
vulgarFractionOneSeventh :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
vulgarFractionOneSeventh = Tag_Entity "&#8528;"

-- | The Vulgar Fraction One Ninth HTML entity ('⅑').
vulgarFractionOneNinth :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
vulgarFractionOneNinth = Tag_Entity "&#8529;"

-- | The Vulgar Fraction One Tenth HTML entity ('⅒').
vulgarFractionOneTenth :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
vulgarFractionOneTenth = Tag_Entity "&#8530;"

-- | The vulgar fraction one third HTML entity ('⅓').
vulgarFractionOneThird :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
vulgarFractionOneThird = Tag_Entity "&#8531;"

-- | The vulgar fraction two thirds HTML entity ('⅔').
vulgarFractionTwoThirds :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
vulgarFractionTwoThirds = Tag_Entity "&#8532;"

-- | The vulgar fraction one fifth HTML entity ('⅕').
vulgarFractionOneFifth :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
vulgarFractionOneFifth = Tag_Entity "&#8533;"

-- | The vulgar fraction two fifths HTML entity ('⅖').
vulgarFractionTwoFifths :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
vulgarFractionTwoFifths = Tag_Entity "&#8534;"

-- | The vulgar fraction three fifths HTML entity ('⅗').
vulgarFractionThreeFifths :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
vulgarFractionThreeFifths = Tag_Entity "&#8535;"

-- | The vulgar fraction four fifths HTML entity ('⅘').
vulgarFractionFourFifths :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
vulgarFractionFourFifths = Tag_Entity "&#8536;"

-- | The vulgar fraction one sixth HTML entity ('⅙').
vulgarFractionOneSixth :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
vulgarFractionOneSixth = Tag_Entity "&#8537;"

-- | The vulgar fraction five sixths HTML entity ('⅚').
vulgarFractionFiveSixths :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
vulgarFractionFiveSixths = Tag_Entity "&#8538;"

-- | The vulgar fraction one eighth HTML entity ('⅛').
vulgarFractionOneEighth :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
vulgarFractionOneEighth = Tag_Entity "&#8539;"

-- | The vulgar fraction three eighths HTML entity ('⅜').
vulgarFractionThreeEighths :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
vulgarFractionThreeEighths = Tag_Entity "&#8540;"

-- | The vulgar fraction five eighths HTML entity ('⅝').
vulgarFractionFiveEighths :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
vulgarFractionFiveEighths = Tag_Entity "&#8541;"

-- | The vulgar fraction seven eighths HTML entity ('⅞').
vulgarFractionSevenEighths :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
vulgarFractionSevenEighths = Tag_Entity "&#8542;"

-- | The Fraction Numerator One HTML entity ('⅟').
fractionNumeratorOne :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
fractionNumeratorOne = Tag_Entity "&#8543;"

-- | The leftwards arrow HTML entity ('←').
leftwardsArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
leftwardsArrow = Tag_Entity "&#8592;"

-- | The upwards arrow HTML entity ('↑').
upwardsArrow :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
upwardsArrow = Tag_Entity "&#8593;"

-- | The rightwards arrow HTML entity ('→').
rightwardsArrow :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
rightwardsArrow = Tag_Entity "&#8594;"

-- | The downwards arrow HTML entity ('↓').
downwardsArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
downwardsArrow = Tag_Entity "&#8595;"

-- | The left right arrow HTML entity ('↔').
leftRightArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
leftRightArrow = Tag_Entity "&#8596;"

-- | The up down arrow HTML entity ('↕').
upDownArrow :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
upDownArrow = Tag_Entity "&#8597;"

-- | The north west arrow HTML entity ('↖').
northWestArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
northWestArrow = Tag_Entity "&#8598;"

-- | The north east arrow HTML entity ('↗').
northEastArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
northEastArrow = Tag_Entity "&#8599;"

-- | The south east arrow HTML entity ('↘').
southEastArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
southEastArrow = Tag_Entity "&#8600;"

-- | The south west arrow HTML entity ('↙').
southWestArrow :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
southWestArrow = Tag_Entity "&#8601;"

-- | The leftwards arrow with stroke HTML entity ('↚').
leftwardsArrowWithStroke :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftwardsArrowWithStroke = Tag_Entity "&#8602;"

-- | The rightwards arrow with stroke HTML entity ('↛').
rightwardsArrowWithStroke :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightwardsArrowWithStroke = Tag_Entity "&#8603;"

-- | The rightwards wave arrow HTML entity ('↝').
rightwardsWaveArrow :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
rightwardsWaveArrow = Tag_Entity "&#8605;"

-- | The leftwards two headed arrow HTML entity ('↞').
leftwardsTwoHeadedArrow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
leftwardsTwoHeadedArrow = Tag_Entity "&#8606;"

-- | The upwards two headed arrow HTML entity ('↟').
upwardsTwoHeadedArrow :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
upwardsTwoHeadedArrow = Tag_Entity "&#8607;"

-- | The rightwards two headed arrow HTML entity ('↠').
rightwardsTwoHeadedArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
rightwardsTwoHeadedArrow = Tag_Entity "&#8608;"

-- | The downwards two headed arrow HTML entity ('↡').
downwardsTwoHeadedArrow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
downwardsTwoHeadedArrow = Tag_Entity "&#8609;"

-- | The leftwards arrow with tail HTML entity ('↢').
leftwardsArrowWithTail :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
leftwardsArrowWithTail = Tag_Entity "&#8610;"

-- | The rightwards arrow with tail HTML entity ('↣').
rightwardsArrowWithTail :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
rightwardsArrowWithTail = Tag_Entity "&#8611;"

-- | The leftwards arrow from bar HTML entity ('↤').
leftwardsArrowFromBar :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
leftwardsArrowFromBar = Tag_Entity "&#8612;"

-- | The upwards arrow from bar HTML entity ('↥').
upwardsArrowFromBar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
upwardsArrowFromBar = Tag_Entity "&#8613;"

-- | The rightwards arrow from bar HTML entity ('↦').
rightwardsArrowFromBar :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
rightwardsArrowFromBar = Tag_Entity "&#8614;"

-- | The downwards arrow from bar HTML entity ('↧').
downwardsArrowFromBar :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
downwardsArrowFromBar = Tag_Entity "&#8615;"

-- | The leftwards arrow with hook HTML entity ('↩').
leftwardsArrowWithHook :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
leftwardsArrowWithHook = Tag_Entity "&#8617;"

-- | The rightwards arrow with hook HTML entity ('↪').
rightwardsArrowWithHook :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
rightwardsArrowWithHook = Tag_Entity "&#8618;"

-- | The leftwards arrow with loop HTML entity ('↫').
leftwardsArrowWithLoop :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
leftwardsArrowWithLoop = Tag_Entity "&#8619;"

-- | The rightwards arrow with loop HTML entity ('↬').
rightwardsArrowWithLoop :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
rightwardsArrowWithLoop = Tag_Entity "&#8620;"

-- | The left right wave arrow HTML entity ('↭').
leftRightWaveArrow :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
leftRightWaveArrow = Tag_Entity "&#8621;"

-- | The left right arrow with stroke HTML entity ('↮').
leftRightArrowWithStroke :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftRightArrowWithStroke = Tag_Entity "&#8622;"

-- | The upwards arrow with tip leftwards HTML entity ('↰').
upwardsArrowWithTipLeftwards :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
upwardsArrowWithTipLeftwards = Tag_Entity "&#8624;"

-- | The upwards arrow with tip rightwards HTML entity ('↱').
upwardsArrowWithTipRightwards :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
upwardsArrowWithTipRightwards = Tag_Entity "&#8625;"

-- | The downwards arrow with tip leftwards HTML entity ('↲').
downwardsArrowWithTipLeftwards :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
downwardsArrowWithTipLeftwards = Tag_Entity "&#8626;"

-- | The downwards arrow with tip rightwards HTML entity ('↳').
downwardsArrowWithTipRightwards :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
downwardsArrowWithTipRightwards = Tag_Entity "&#8627;"

-- | The downwards arrow with corner leftwards = carriage return HTML entity ('↵').
downwardsArrowWithCornerLeftwards :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
downwardsArrowWithCornerLeftwards = Tag_Entity "&#8629;"

-- | The anticlockwise top semicircle arrow HTML entity ('↶').
anticlockwiseTopSemicircleArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
anticlockwiseTopSemicircleArrow = Tag_Entity "&#8630;"

-- | The clockwise top semicircle arrow HTML entity ('↷').
clockwiseTopSemicircleArrow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
clockwiseTopSemicircleArrow = Tag_Entity "&#8631;"

-- | The anticlockwise open circle arrow HTML entity ('↺').
anticlockwiseOpenCircleArrow :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
anticlockwiseOpenCircleArrow = Tag_Entity "&#8634;"

-- | The clockwise open circle arrow HTML entity ('↻').
clockwiseOpenCircleArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
clockwiseOpenCircleArrow = Tag_Entity "&#8635;"

-- | The leftwards harpoon with barb upwards HTML entity ('↼').
leftwardsHarpoonWithBarbUpwards :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpwards = Tag_Entity "&#8636;"

-- | The leftwards harpoon with barb downwards HTML entity ('↽').
leftwardsHarpoonWithBarbDownwards :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
leftwardsHarpoonWithBarbDownwards = Tag_Entity "&#8637;"

-- | The upwards harpoon with barb rightwards HTML entity ('↾').
upwardsHarpoonWithBarbRightwards :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
upwardsHarpoonWithBarbRightwards = Tag_Entity "&#8638;"

-- | The upwards harpoon with barb leftwards HTML entity ('↿').
upwardsHarpoonWithBarbLeftwards :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
upwardsHarpoonWithBarbLeftwards = Tag_Entity "&#8639;"

-- | The rightwards harpoon with barb upwards HTML entity ('⇀').
rightwardsHarpoonWithBarbUpwards :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpwards = Tag_Entity "&#8640;"

-- | The rightwards harpoon with barb downwards HTML entity ('⇁').
rightwardsHarpoonWithBarbDownwards :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightwardsHarpoonWithBarbDownwards = Tag_Entity "&#8641;"

-- | The downwards harpoon with barb rightwards HTML entity ('⇂').
downwardsHarpoonWithBarbRightwards :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
downwardsHarpoonWithBarbRightwards = Tag_Entity "&#8642;"

-- | The downwards harpoon with barb leftwards HTML entity ('⇃').
downwardsHarpoonWithBarbLeftwards :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
downwardsHarpoonWithBarbLeftwards = Tag_Entity "&#8643;"

-- | The rightwards arrow over leftwards arrow HTML entity ('⇄').
rightwardsArrowOverLeftwardsArrow :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
rightwardsArrowOverLeftwardsArrow = Tag_Entity "&#8644;"

-- | The upwards arrow leftwards of downwards arrow HTML entity ('⇅').
upwardsArrowLeftwardsOfDownwardsArrow :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
upwardsArrowLeftwardsOfDownwardsArrow = Tag_Entity "&#8645;"

-- | The leftwards arrow over rightwards arrow HTML entity ('⇆').
leftwardsArrowOverRightwardsArrow :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
leftwardsArrowOverRightwardsArrow = Tag_Entity "&#8646;"

-- | The leftwards paired arrows HTML entity ('⇇').
leftwardsPairedArrows :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
leftwardsPairedArrows = Tag_Entity "&#8647;"

-- | The upwards paired arrows HTML entity ('⇈').
upwardsPairedArrows :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
upwardsPairedArrows = Tag_Entity "&#8648;"

-- | The rightwards paired arrows HTML entity ('⇉').
rightwardsPairedArrows :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
rightwardsPairedArrows = Tag_Entity "&#8649;"

-- | The downwards paired arrows HTML entity ('⇊').
downwardsPairedArrows :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
downwardsPairedArrows = Tag_Entity "&#8650;"

-- | The leftwards harpoon over rightwards harpoon HTML entity ('⇋').
leftwardsHarpoonOverRightwardsHarpoon :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
leftwardsHarpoonOverRightwardsHarpoon = Tag_Entity "&#8651;"

-- | The rightwards harpoon over leftwards harpoon HTML entity ('⇌').
rightwardsHarpoonOverLeftwardsHarpoon :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
rightwardsHarpoonOverLeftwardsHarpoon = Tag_Entity "&#8652;"

-- | The leftwards double arrow with stroke HTML entity ('⇍').
leftwardsDoubleArrowWithStroke :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
leftwardsDoubleArrowWithStroke = Tag_Entity "&#8653;"

-- | The left right double arrow with stroke HTML entity ('⇎').
leftRightDoubleArrowWithStroke :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
leftRightDoubleArrowWithStroke = Tag_Entity "&#8654;"

-- | The rightwards double arrow with stroke HTML entity ('⇏').
rightwardsDoubleArrowWithStroke :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
rightwardsDoubleArrowWithStroke = Tag_Entity "&#8655;"

-- | The leftwards double arrow HTML entity ('⇐').
leftwardsDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
leftwardsDoubleArrow = Tag_Entity "&#8656;"

-- | The upwards double arrow HTML entity ('⇑').
upwardsDoubleArrow :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
upwardsDoubleArrow = Tag_Entity "&#8657;"

-- | The rightwards double arrow HTML entity ('⇒').
rightwardsDoubleArrow :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
rightwardsDoubleArrow = Tag_Entity "&#8658;"

-- | The downwards double arrow HTML entity ('⇓').
downwardsDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
downwardsDoubleArrow = Tag_Entity "&#8659;"

-- | The left right double arrow HTML entity ('⇔').
leftRightDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
leftRightDoubleArrow = Tag_Entity "&#8660;"

-- | The up down double arrow HTML entity ('⇕').
upDownDoubleArrow :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
upDownDoubleArrow = Tag_Entity "&#8661;"

-- | The north west double arrow HTML entity ('⇖').
northWestDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
northWestDoubleArrow = Tag_Entity "&#8662;"

-- | The north east double arrow HTML entity ('⇗').
northEastDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
northEastDoubleArrow = Tag_Entity "&#8663;"

-- | The south east double arrow HTML entity ('⇘').
southEastDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
southEastDoubleArrow = Tag_Entity "&#8664;"

-- | The south west double arrow HTML entity ('⇙').
southWestDoubleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
southWestDoubleArrow = Tag_Entity "&#8665;"

-- | The leftwards triple arrow HTML entity ('⇚').
leftwardsTripleArrow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
leftwardsTripleArrow = Tag_Entity "&#8666;"

-- | The rightwards triple arrow HTML entity ('⇛').
rightwardsTripleArrow :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
rightwardsTripleArrow = Tag_Entity "&#8667;"

-- | The rightwards squiggle arrow HTML entity ('⇝').
rightwardsSquiggleArrow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
rightwardsSquiggleArrow = Tag_Entity "&#8669;"

-- | The leftwards arrow to bar HTML entity ('⇤').
leftwardsArrowToBar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
leftwardsArrowToBar = Tag_Entity "&#8676;"

-- | The rightwards arrow to bar HTML entity ('⇥').
rightwardsArrowToBar :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
rightwardsArrowToBar = Tag_Entity "&#8677;"

-- | The downwards arrow leftwards of upwards arrow HTML entity ('⇵').
downwardsArrowLeftwardsOfUpwardsArrow :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
downwardsArrowLeftwardsOfUpwardsArrow = Tag_Entity "&#8693;"

-- | The leftwards open-headed arrow HTML entity ('⇽').
leftwardsOpenHeadedArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftwardsOpenHeadedArrow = Tag_Entity "&#8701;"

-- | The rightwards open-headed arrow HTML entity ('⇾').
rightwardsOpenHeadedArrow :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightwardsOpenHeadedArrow = Tag_Entity "&#8702;"

-- | The left right open-headed arrow HTML entity ('⇿').
leftRightOpenHeadedArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftRightOpenHeadedArrow = Tag_Entity "&#8703;"

-- | The for all HTML entity ('∀').
forAll :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
forAll = Tag_Entity "&#8704;"

-- | The complement HTML entity ('∁').
complement :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
complement = Tag_Entity "&#8705;"

-- | The partial differential HTML entity ('∂').
partialDifferential :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
partialDifferential = Tag_Entity "&#8706;"

-- | The there exists HTML entity ('∃').
thereExists :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
thereExists = Tag_Entity "&#8707;"

-- | The there does not exist HTML entity ('∄').
thereDoesNotExist :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
thereDoesNotExist = Tag_Entity "&#8708;"

-- | The empty set HTML entity ('∅').
emptySet :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
emptySet = Tag_Entity "&#8709;"

-- | The nabla HTML entity ('∇').
nabla :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
nabla = Tag_Entity "&#8711;"

-- | The element of HTML entity ('∈').
elementOf :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
elementOf = Tag_Entity "&#8712;"

-- | The not an element of HTML entity ('∉').
notAnElementOf :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
notAnElementOf = Tag_Entity "&#8713;"

-- | The contains as member HTML entity ('∋').
containsAsMember :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
containsAsMember = Tag_Entity "&#8715;"

-- | The does not contain as member HTML entity ('∌').
doesNotContainAsMember :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
doesNotContainAsMember = Tag_Entity "&#8716;"

-- | The n-ary product HTML entity ('∏').
nAryProduct :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
nAryProduct = Tag_Entity "&#8719;"

-- | The n-ary coproduct HTML entity ('∐').
nAryCoproduct :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
nAryCoproduct = Tag_Entity "&#8720;"

-- | The n-ary summation HTML entity ('∑').
nArySummation :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
nArySummation = Tag_Entity "&#8721;"

-- | The minus sign HTML entity ('−').
minusSign :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
minusSign = Tag_Entity "&#8722;"

-- | The minus-or-plus sign HTML entity ('∓').
minusOrPlusSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
minusOrPlusSign = Tag_Entity "&#8723;"

-- | The dot plus HTML entity ('∔').
dotPlus :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
dotPlus = Tag_Entity "&#8724;"

-- | The Division Slash HTML entity ('∕').
divisionSlash :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
divisionSlash = Tag_Entity "&#8725;"

-- | The set minus HTML entity ('∖').
setMinus :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
setMinus = Tag_Entity "&#8726;"

-- | The asterisk operator HTML entity ('∗').
asteriskOperator :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
asteriskOperator = Tag_Entity "&#8727;"

-- | The ring operator HTML entity ('∘').
ringOperator :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
ringOperator = Tag_Entity "&#8728;"

-- | The square root HTML entity ('√').
squareRoot :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
squareRoot = Tag_Entity "&#8730;"

-- | The Cube Root HTML entity ('∛').
cubeRoot :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
cubeRoot = Tag_Entity "&#8731;"

-- | The Fourth Root HTML entity ('∜').
fourthRoot :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
fourthRoot = Tag_Entity "&#8732;"

-- | The proportional to HTML entity ('∝').
proportionalTo :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
proportionalTo = Tag_Entity "&#8733;"

-- | The infinity HTML entity ('∞').
infinity :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
infinity = Tag_Entity "&#8734;"

-- | The right angle HTML entity ('∟').
rightAngle :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
rightAngle = Tag_Entity "&#8735;"

-- | The angle HTML entity ('∠').
angle :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
angle = Tag_Entity "&#8736;"

-- | The measured angle HTML entity ('∡').
measuredAngle :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
measuredAngle = Tag_Entity "&#8737;"

-- | The spherical angle HTML entity ('∢').
sphericalAngle :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
sphericalAngle = Tag_Entity "&#8738;"

-- | The divides HTML entity ('∣').
divides :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
divides = Tag_Entity "&#8739;"

-- | The does not divide HTML entity ('∤').
doesNotDivide :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
doesNotDivide = Tag_Entity "&#8740;"

-- | The parallel to HTML entity ('∥').
parallelTo :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
parallelTo = Tag_Entity "&#8741;"

-- | The not parallel to HTML entity ('∦').
notParallelTo :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
notParallelTo = Tag_Entity "&#8742;"

-- | The logical and HTML entity ('∧').
logicalAnd :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
logicalAnd = Tag_Entity "&#8743;"

-- | The logical or HTML entity ('∨').
logicalOr :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
logicalOr = Tag_Entity "&#8744;"

-- | The intersection = cap HTML entity ('∩').
intersection :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
intersection = Tag_Entity "&#8745;"

-- | The union = cup HTML entity ('∪').
union :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
union = Tag_Entity "&#8746;"

-- | The integral HTML entity ('∫').
integral :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
integral = Tag_Entity "&#8747;"

-- | The double integral HTML entity ('∬').
doubleIntegral :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doubleIntegral = Tag_Entity "&#8748;"

-- | The triple integral HTML entity ('∭').
tripleIntegral :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
tripleIntegral = Tag_Entity "&#8749;"

-- | The contour integral HTML entity ('∮').
contourIntegral :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
contourIntegral = Tag_Entity "&#8750;"

-- | The surface integral HTML entity ('∯').
surfaceIntegral :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
surfaceIntegral = Tag_Entity "&#8751;"

-- | The volume integral HTML entity ('∰').
volumeIntegral :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
volumeIntegral = Tag_Entity "&#8752;"

-- | The clockwise integral HTML entity ('∱').
clockwiseIntegral :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
clockwiseIntegral = Tag_Entity "&#8753;"

-- | The clockwise contour integral HTML entity ('∲').
clockwiseContourIntegral :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
clockwiseContourIntegral = Tag_Entity "&#8754;"

-- | The anticlockwise contour integral HTML entity ('∳').
anticlockwiseContourIntegral :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
anticlockwiseContourIntegral = Tag_Entity "&#8755;"

-- | The therefore HTML entity ('∴').
therefore :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
therefore = Tag_Entity "&#8756;"

-- | The because HTML entity ('∵').
because :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
because = Tag_Entity "&#8757;"

-- | The ratio HTML entity ('∶').
ratio :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
ratio = Tag_Entity "&#8758;"

-- | The proportion HTML entity ('∷').
proportion :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
proportion = Tag_Entity "&#8759;"

-- | The dot minus HTML entity ('∸').
dotMinus :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
dotMinus = Tag_Entity "&#8760;"

-- | The geometric proportion HTML entity ('∺').
geometricProportion :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
geometricProportion = Tag_Entity "&#8762;"

-- | The homothetic HTML entity ('∻').
homothetic :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
homothetic = Tag_Entity "&#8763;"

-- | The tilde operator HTML entity ('∼').
tildeOperator :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
tildeOperator = Tag_Entity "&#8764;"

-- | The reversed tilde HTML entity ('∽').
reversedTilde :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
reversedTilde = Tag_Entity "&#8765;"

-- | The inverted lazy s HTML entity ('∾').
invertedLazyS :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
invertedLazyS = Tag_Entity "&#8766;"

-- | The sine wave HTML entity ('∿').
sineWave :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
sineWave = Tag_Entity "&#8767;"

-- | The wreath product HTML entity ('≀').
wreathProduct :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
wreathProduct = Tag_Entity "&#8768;"

-- | The not tilde HTML entity ('≁').
notTilde :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
notTilde = Tag_Entity "&#8769;"

-- | The minus tilde HTML entity ('≂').
minusTilde :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
minusTilde = Tag_Entity "&#8770;"

-- | The asymptotically equal to HTML entity ('≃').
asymptoticallyEqualTo :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
asymptoticallyEqualTo = Tag_Entity "&#8771;"

-- | The not asymptotically equal to HTML entity ('≄').
notAsymptoticallyEqualTo :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
notAsymptoticallyEqualTo = Tag_Entity "&#8772;"

-- | The approximately equal to HTML entity ('≅').
approximatelyEqualTo :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
approximatelyEqualTo = Tag_Entity "&#8773;"

-- | The approximately but not actually equal to HTML entity ('≆').
approximatelyButNotActuallyEqualTo :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
approximatelyButNotActuallyEqualTo = Tag_Entity "&#8774;"

-- | The neither approximately nor actually equal to HTML entity ('≇').
neitherApproximatelyNorActuallyEqualTo :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
neitherApproximatelyNorActuallyEqualTo = Tag_Entity "&#8775;"

-- | The almost equal to HTML entity ('≈').
almostEqualTo :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
almostEqualTo = Tag_Entity "&#8776;"

-- | The not almost equal to HTML entity ('≉').
notAlmostEqualTo :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
notAlmostEqualTo = Tag_Entity "&#8777;"

-- | The almost equal or equal to HTML entity ('≊').
almostEqualOrEqualTo :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
almostEqualOrEqualTo = Tag_Entity "&#8778;"

-- | The triple tilde HTML entity ('≋').
tripleTilde :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
tripleTilde = Tag_Entity "&#8779;"

-- | The all equal to HTML entity ('≌').
allEqualTo :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
allEqualTo = Tag_Entity "&#8780;"

-- | The equivalent to HTML entity ('≍').
equivalentTo :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
equivalentTo = Tag_Entity "&#8781;"

-- | The geometrically equivalent to HTML entity ('≎').
geometricallyEquivalentTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
geometricallyEquivalentTo = Tag_Entity "&#8782;"

-- | The difference between HTML entity ('≏').
differenceBetween :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
differenceBetween = Tag_Entity "&#8783;"

-- | The approaches the limit HTML entity ('≐').
approachesTheLimit :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
approachesTheLimit = Tag_Entity "&#8784;"

-- | The geometrically equal to HTML entity ('≑').
geometricallyEqualTo :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
geometricallyEqualTo = Tag_Entity "&#8785;"

-- | The approximately equal to or the image of HTML entity ('≒').
approximatelyEqualToOrTheImageOf :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
approximatelyEqualToOrTheImageOf = Tag_Entity "&#8786;"

-- | The image of or approximately equal to HTML entity ('≓').
imageOfOrApproximatelyEqualTo :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
imageOfOrApproximatelyEqualTo = Tag_Entity "&#8787;"

-- | The colon equals HTML entity ('≔').
colonEquals :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
colonEquals = Tag_Entity "&#8788;"

-- | The equals colon HTML entity ('≕').
equalsColon :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
equalsColon = Tag_Entity "&#8789;"

-- | The ring in equal to HTML entity ('≖').
ringInEqualTo :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
ringInEqualTo = Tag_Entity "&#8790;"

-- | The ring equal to HTML entity ('≗').
ringEqualTo :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
ringEqualTo = Tag_Entity "&#8791;"

-- | The estimates HTML entity ('≙').
estimates :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
estimates = Tag_Entity "&#8793;"

-- | The equiangular to HTML entity ('≚').
equiangularTo :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
equiangularTo = Tag_Entity "&#8794;"

-- | The Star Equals HTML entity ('≛').
starEquals :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
starEquals = Tag_Entity "&#8795;"

-- | The delta equal to HTML entity ('≜').
deltaEqualTo :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
deltaEqualTo = Tag_Entity "&#8796;"

-- | The Equal To By Definition HTML entity ('≝').
equalToByDefinition :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
equalToByDefinition = Tag_Entity "&#8797;"

-- | The questioned equal to HTML entity ('≟').
questionedEqualTo :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
questionedEqualTo = Tag_Entity "&#8799;"

-- | The not equal to HTML entity ('≠').
notEqualTo :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
notEqualTo = Tag_Entity "&#8800;"

-- | The identical to HTML entity ('≡').
identicalTo :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
identicalTo = Tag_Entity "&#8801;"

-- | The not identical to HTML entity ('≢').
notIdenticalTo :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
notIdenticalTo = Tag_Entity "&#8802;"

-- | The less-than or equal to HTML entity ('≤').
lessThanOrEqualTo :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
lessThanOrEqualTo = Tag_Entity "&#8804;"

-- | The greater-than or equal to HTML entity ('≥').
greaterThanOrEqualTo :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
greaterThanOrEqualTo = Tag_Entity "&#8805;"

-- | The less-than over equal to HTML entity ('≦').
lessThanOverEqualTo :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
lessThanOverEqualTo = Tag_Entity "&#8806;"

-- | The greater-than over equal to HTML entity ('≧').
greaterThanOverEqualTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
greaterThanOverEqualTo = Tag_Entity "&#8807;"

-- | The less-than but not equal to HTML entity ('≨').
lessThanButNotEqualTo :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
lessThanButNotEqualTo = Tag_Entity "&#8808;"

-- | The greater-than but not equal to HTML entity ('≩').
greaterThanButNotEqualTo :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
greaterThanButNotEqualTo = Tag_Entity "&#8809;"

-- | The much less-than HTML entity ('≪').
muchLessThan :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
muchLessThan = Tag_Entity "&#8810;"

-- | The much greater-than HTML entity ('≫').
muchGreaterThan :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
muchGreaterThan = Tag_Entity "&#8811;"

-- | The between HTML entity ('≬').
between :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
between = Tag_Entity "&#8812;"

-- | The not equivalent to HTML entity ('≭').
notEquivalentTo :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
notEquivalentTo = Tag_Entity "&#8813;"

-- | The not less-than HTML entity ('≮').
notLessThan :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
notLessThan = Tag_Entity "&#8814;"

-- | The not greater-than HTML entity ('≯').
notGreaterThan :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
notGreaterThan = Tag_Entity "&#8815;"

-- | The neither less-than nor equal to HTML entity ('≰').
neitherLessThanNorEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
neitherLessThanNorEqualTo = Tag_Entity "&#8816;"

-- | The neither greater-than nor equal to HTML entity ('≱').
neitherGreaterThanNorEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
neitherGreaterThanNorEqualTo = Tag_Entity "&#8817;"

-- | The less-than or equivalent to HTML entity ('≲').
lessThanOrEquivalentTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
lessThanOrEquivalentTo = Tag_Entity "&#8818;"

-- | The greater-than or equivalent to HTML entity ('≳').
greaterThanOrEquivalentTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
greaterThanOrEquivalentTo = Tag_Entity "&#8819;"

-- | The neither less-than nor equivalent to HTML entity ('≴').
neitherLessThanNorEquivalentTo :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
neitherLessThanNorEquivalentTo = Tag_Entity "&#8820;"

-- | The neither greater-than nor equivalent to HTML entity ('≵').
neitherGreaterThanNorEquivalentTo :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
neitherGreaterThanNorEquivalentTo = Tag_Entity "&#8821;"

-- | The less-than or greater-than HTML entity ('≶').
lessThanOrGreaterThan :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
lessThanOrGreaterThan = Tag_Entity "&#8822;"

-- | The greater-than or less-than HTML entity ('≷').
greaterThanOrLessThan :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
greaterThanOrLessThan = Tag_Entity "&#8823;"

-- | The neither less-than nor greater-than HTML entity ('≸').
neitherLessThanNorGreaterThan :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
neitherLessThanNorGreaterThan = Tag_Entity "&#8824;"

-- | The neither greater-than nor less-than HTML entity ('≹').
neitherGreaterThanNorLessThan :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
neitherGreaterThanNorLessThan = Tag_Entity "&#8825;"

-- | The precedes HTML entity ('≺').
precedes :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
precedes = Tag_Entity "&#8826;"

-- | The succeeds HTML entity ('≻').
succeeds :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
succeeds = Tag_Entity "&#8827;"

-- | The precedes or equal to HTML entity ('≼').
precedesOrEqualTo :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
precedesOrEqualTo = Tag_Entity "&#8828;"

-- | The succeeds or equal to HTML entity ('≽').
succeedsOrEqualTo :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
succeedsOrEqualTo = Tag_Entity "&#8829;"

-- | The precedes or equivalent to HTML entity ('≾').
precedesOrEquivalentTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
precedesOrEquivalentTo = Tag_Entity "&#8830;"

-- | The succeeds or equivalent to HTML entity ('≿').
succeedsOrEquivalentTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
succeedsOrEquivalentTo = Tag_Entity "&#8831;"

-- | The does not precede HTML entity ('⊀').
doesNotPrecede :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doesNotPrecede = Tag_Entity "&#8832;"

-- | The does not succeed HTML entity ('⊁').
doesNotSucceed :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doesNotSucceed = Tag_Entity "&#8833;"

-- | The subset of HTML entity ('⊂').
subsetOf :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
subsetOf = Tag_Entity "&#8834;"

-- | The superset of HTML entity ('⊃').
supersetOf :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
supersetOf = Tag_Entity "&#8835;"

-- | The not a subset of HTML entity ('⊄').
notASubsetOf :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
notASubsetOf = Tag_Entity "&#8836;"

-- | The not a superset of HTML entity ('⊅').
notASupersetOf :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
notASupersetOf = Tag_Entity "&#8837;"

-- | The subset of or equal to HTML entity ('⊆').
subsetOfOrEqualTo :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
subsetOfOrEqualTo = Tag_Entity "&#8838;"

-- | The superset of or equal to HTML entity ('⊇').
supersetOfOrEqualTo :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
supersetOfOrEqualTo = Tag_Entity "&#8839;"

-- | The neither a subset of nor equal to HTML entity ('⊈').
neitherASubsetOfNorEqualTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
neitherASubsetOfNorEqualTo = Tag_Entity "&#8840;"

-- | The neither a superset of nor equal to HTML entity ('⊉').
neitherASupersetOfNorEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
neitherASupersetOfNorEqualTo = Tag_Entity "&#8841;"

-- | The subset of with not equal to HTML entity ('⊊').
subsetOfWithNotEqualTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
subsetOfWithNotEqualTo = Tag_Entity "&#8842;"

-- | The superset of with not equal to HTML entity ('⊋').
supersetOfWithNotEqualTo :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
supersetOfWithNotEqualTo = Tag_Entity "&#8843;"

-- | The multiset multiplication HTML entity ('⊍').
multisetMultiplication :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
multisetMultiplication = Tag_Entity "&#8845;"

-- | The multiset union HTML entity ('⊎').
multisetUnion :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
multisetUnion = Tag_Entity "&#8846;"

-- | The square image of HTML entity ('⊏').
squareImageOf :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
squareImageOf = Tag_Entity "&#8847;"

-- | The square original of HTML entity ('⊐').
squareOriginalOf :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
squareOriginalOf = Tag_Entity "&#8848;"

-- | The square image of or equal to HTML entity ('⊑').
squareImageOfOrEqualTo :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
squareImageOfOrEqualTo = Tag_Entity "&#8849;"

-- | The square original of or equal to HTML entity ('⊒').
squareOriginalOfOrEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
squareOriginalOfOrEqualTo = Tag_Entity "&#8850;"

-- | The square cap HTML entity ('⊓').
squareCap :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
squareCap = Tag_Entity "&#8851;"

-- | The square cup HTML entity ('⊔').
squareCup :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
squareCup = Tag_Entity "&#8852;"

-- | The circled plus HTML entity ('⊕').
circledPlus :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
circledPlus = Tag_Entity "&#8853;"

-- | The circled minus HTML entity ('⊖').
circledMinus :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
circledMinus = Tag_Entity "&#8854;"

-- | The circled times HTML entity ('⊗').
circledTimes :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
circledTimes = Tag_Entity "&#8855;"

-- | The circled division slash HTML entity ('⊘').
circledDivisionSlash :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
circledDivisionSlash = Tag_Entity "&#8856;"

-- | The circled dot operator HTML entity ('⊙').
circledDotOperator :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
circledDotOperator = Tag_Entity "&#8857;"

-- | The circled ring operator HTML entity ('⊚').
circledRingOperator :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
circledRingOperator = Tag_Entity "&#8858;"

-- | The circled asterisk operator HTML entity ('⊛').
circledAsteriskOperator :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
circledAsteriskOperator = Tag_Entity "&#8859;"

-- | The Circled Equals HTML entity ('⊜').
circledEquals :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
circledEquals = Tag_Entity "&#8860;"

-- | The circled dash HTML entity ('⊝').
circledDash :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
circledDash = Tag_Entity "&#8861;"

-- | The squared plus HTML entity ('⊞').
squaredPlus :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
squaredPlus = Tag_Entity "&#8862;"

-- | The squared minus HTML entity ('⊟').
squaredMinus :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
squaredMinus = Tag_Entity "&#8863;"

-- | The squared times HTML entity ('⊠').
squaredTimes :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
squaredTimes = Tag_Entity "&#8864;"

-- | The squared dot operator HTML entity ('⊡').
squaredDotOperator :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
squaredDotOperator = Tag_Entity "&#8865;"

-- | The right tack HTML entity ('⊢').
rightTack :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
rightTack = Tag_Entity "&#8866;"

-- | The left tack HTML entity ('⊣').
leftTack :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
leftTack = Tag_Entity "&#8867;"

-- | The down tack HTML entity ('⊤').
downTack :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
downTack = Tag_Entity "&#8868;"

-- | The up tack HTML entity ('⊥').
upTack :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
upTack = Tag_Entity "&#8869;"

-- | The models HTML entity ('⊧').
models :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
models = Tag_Entity "&#8871;"

-- | The true HTML entity ('⊨').
true :: ValidChild Text parent grandparent
     => ChildHTML parent grandparent
true = Tag_Entity "&#8872;"

-- | The forces HTML entity ('⊩').
forces :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
forces = Tag_Entity "&#8873;"

-- | The triple vertical bar right turnstile HTML entity ('⊪').
tripleVerticalBarRightTurnstile :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
tripleVerticalBarRightTurnstile = Tag_Entity "&#8874;"

-- | The double vertical bar double right turnstile HTML entity ('⊫').
doubleVerticalBarDoubleRightTurnstile :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
doubleVerticalBarDoubleRightTurnstile = Tag_Entity "&#8875;"

-- | The does not prove HTML entity ('⊬').
doesNotProve :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
doesNotProve = Tag_Entity "&#8876;"

-- | The not true HTML entity ('⊭').
notTrue :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
notTrue = Tag_Entity "&#8877;"

-- | The does not force HTML entity ('⊮').
doesNotForce :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
doesNotForce = Tag_Entity "&#8878;"

-- | The negated double vertical bar double right turnstile HTML entity ('⊯').
negatedDoubleVerticalBarDoubleRightTurnstile :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
negatedDoubleVerticalBarDoubleRightTurnstile = Tag_Entity "&#8879;"

-- | The precedes under relation HTML entity ('⊰').
precedesUnderRelation :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
precedesUnderRelation = Tag_Entity "&#8880;"

-- | The normal subgroup of HTML entity ('⊲').
normalSubgroupOf :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
normalSubgroupOf = Tag_Entity "&#8882;"

-- | The contains as normal subgroup HTML entity ('⊳').
containsAsNormalSubgroup :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
containsAsNormalSubgroup = Tag_Entity "&#8883;"

-- | The normal subgroup of or equal to HTML entity ('⊴').
normalSubgroupOfOrEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
normalSubgroupOfOrEqualTo = Tag_Entity "&#8884;"

-- | The contains as normal subgroup or equal to HTML entity ('⊵').
containsAsNormalSubgroupOrEqualTo :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
containsAsNormalSubgroupOrEqualTo = Tag_Entity "&#8885;"

-- | The original of HTML entity ('⊶').
originalOf :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
originalOf = Tag_Entity "&#8886;"

-- | The image of HTML entity ('⊷').
imageOf :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
imageOf = Tag_Entity "&#8887;"

-- | The multimap HTML entity ('⊸').
multimap :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
multimap = Tag_Entity "&#8888;"

-- | The hermitian conjugate matrix HTML entity ('⊹').
hermitianConjugateMatrix :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
hermitianConjugateMatrix = Tag_Entity "&#8889;"

-- | The intercalate HTML entity ('⊺').
intercalate :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
intercalate = Tag_Entity "&#8890;"

-- | The xor HTML entity ('⊻').
xor :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
xor = Tag_Entity "&#8891;"

-- | The nor HTML entity ('⊽').
nor :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
nor = Tag_Entity "&#8893;"

-- | The right angle with arc HTML entity ('⊾').
rightAngleWithArc :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
rightAngleWithArc = Tag_Entity "&#8894;"

-- | The right triangle HTML entity ('⊿').
rightTriangle :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
rightTriangle = Tag_Entity "&#8895;"

-- | The n-ary logical and HTML entity ('⋀').
nAryLogicalAnd :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
nAryLogicalAnd = Tag_Entity "&#8896;"

-- | The n-ary logical or HTML entity ('⋁').
nAryLogicalOr :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
nAryLogicalOr = Tag_Entity "&#8897;"

-- | The n-ary intersection HTML entity ('⋂').
nAryIntersection :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
nAryIntersection = Tag_Entity "&#8898;"

-- | The n-ary union HTML entity ('⋃').
nAryUnion :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
nAryUnion = Tag_Entity "&#8899;"

-- | The diamond operator HTML entity ('⋄').
diamondOperator :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
diamondOperator = Tag_Entity "&#8900;"

-- | The dot operator HTML entity ('⋅').
dotOperator :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
dotOperator = Tag_Entity "&#8901;"

-- | The star operator HTML entity ('⋆').
starOperator :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
starOperator = Tag_Entity "&#8902;"

-- | The division times HTML entity ('⋇').
divisionTimes :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
divisionTimes = Tag_Entity "&#8903;"

-- | The bowtie HTML entity ('⋈').
bowtie :: ValidChild Text parent grandparent
       => ChildHTML parent grandparent
bowtie = Tag_Entity "&#8904;"

-- | The left normal factor semidirect product HTML entity ('⋉').
leftNormalFactorSemidirectProduct :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
leftNormalFactorSemidirectProduct = Tag_Entity "&#8905;"

-- | The right normal factor semidirect product HTML entity ('⋊').
rightNormalFactorSemidirectProduct :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightNormalFactorSemidirectProduct = Tag_Entity "&#8906;"

-- | The left semidirect product HTML entity ('⋋').
leftSemidirectProduct :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
leftSemidirectProduct = Tag_Entity "&#8907;"

-- | The right semidirect product HTML entity ('⋌').
rightSemidirectProduct :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
rightSemidirectProduct = Tag_Entity "&#8908;"

-- | The reversed tilde equals HTML entity ('⋍').
reversedTildeEquals :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
reversedTildeEquals = Tag_Entity "&#8909;"

-- | The curly logical or HTML entity ('⋎').
curlyLogicalOr :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
curlyLogicalOr = Tag_Entity "&#8910;"

-- | The curly logical and HTML entity ('⋏').
curlyLogicalAnd :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
curlyLogicalAnd = Tag_Entity "&#8911;"

-- | The double subset HTML entity ('⋐').
doubleSubset :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
doubleSubset = Tag_Entity "&#8912;"

-- | The double superset HTML entity ('⋑').
doubleSuperset :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doubleSuperset = Tag_Entity "&#8913;"

-- | The double intersection HTML entity ('⋒').
doubleIntersection :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
doubleIntersection = Tag_Entity "&#8914;"

-- | The double union HTML entity ('⋓').
doubleUnion :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
doubleUnion = Tag_Entity "&#8915;"

-- | The pitchfork HTML entity ('⋔').
pitchfork :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
pitchfork = Tag_Entity "&#8916;"

-- | The equal and parallel to HTML entity ('⋕').
equalAndParallelTo :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
equalAndParallelTo = Tag_Entity "&#8917;"

-- | The less-than with dot HTML entity ('⋖').
lessThanWithDot :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
lessThanWithDot = Tag_Entity "&#8918;"

-- | The greater-than with dot HTML entity ('⋗').
greaterThanWithDot :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
greaterThanWithDot = Tag_Entity "&#8919;"

-- | The very much less-than HTML entity ('⋘').
veryMuchLessThan :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
veryMuchLessThan = Tag_Entity "&#8920;"

-- | The very much greater-than HTML entity ('⋙').
veryMuchGreaterThan :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
veryMuchGreaterThan = Tag_Entity "&#8921;"

-- | The less-than equal to or greater-than HTML entity ('⋚').
lessThanEqualToOrGreaterThan :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
lessThanEqualToOrGreaterThan = Tag_Entity "&#8922;"

-- | The greater-than equal to or less-than HTML entity ('⋛').
greaterThanEqualToOrLessThan :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
greaterThanEqualToOrLessThan = Tag_Entity "&#8923;"

-- | The Equal To Or Less-than HTML entity ('⋜').
equalToOrLessThan :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
equalToOrLessThan = Tag_Entity "&#8924;"

-- | The Equal To Or Greater-than HTML entity ('⋝').
equalToOrGreaterThan :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
equalToOrGreaterThan = Tag_Entity "&#8925;"

-- | The equal to or precedes HTML entity ('⋞').
equalToOrPrecedes :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
equalToOrPrecedes = Tag_Entity "&#8926;"

-- | The equal to or succeeds HTML entity ('⋟').
equalToOrSucceeds :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
equalToOrSucceeds = Tag_Entity "&#8927;"

-- | The does not precede or equal HTML entity ('⋠').
doesNotPrecedeOrEqual :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
doesNotPrecedeOrEqual = Tag_Entity "&#8928;"

-- | The does not succeed or equal HTML entity ('⋡').
doesNotSucceedOrEqual :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
doesNotSucceedOrEqual = Tag_Entity "&#8929;"

-- | The not square image of or equal to HTML entity ('⋢').
notSquareImageOfOrEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
notSquareImageOfOrEqualTo = Tag_Entity "&#8930;"

-- | The not square original of or equal to HTML entity ('⋣').
notSquareOriginalOfOrEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
notSquareOriginalOfOrEqualTo = Tag_Entity "&#8931;"

-- | The Square Image Of Or Not Equal To HTML entity ('⋤').
squareImageOfOrNotEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
squareImageOfOrNotEqualTo = Tag_Entity "&#8932;"

-- | The Square Original Of Or Not Equal To HTML entity ('⋥').
squareOriginalOfOrNotEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
squareOriginalOfOrNotEqualTo = Tag_Entity "&#8933;"

-- | The less-than but not equivalent to HTML entity ('⋦').
lessThanButNotEquivalentTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
lessThanButNotEquivalentTo = Tag_Entity "&#8934;"

-- | The greater-than but not equivalent to HTML entity ('⋧').
greaterThanButNotEquivalentTo :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
greaterThanButNotEquivalentTo = Tag_Entity "&#8935;"

-- | The precedes but not equivalent to HTML entity ('⋨').
precedesButNotEquivalentTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
precedesButNotEquivalentTo = Tag_Entity "&#8936;"

-- | The succeeds but not equivalent to HTML entity ('⋩').
succeedsButNotEquivalentTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
succeedsButNotEquivalentTo = Tag_Entity "&#8937;"

-- | The not normal subgroup of HTML entity ('⋪').
notNormalSubgroupOf :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
notNormalSubgroupOf = Tag_Entity "&#8938;"

-- | The does not contain as normal subgroup HTML entity ('⋫').
doesNotContainAsNormalSubgroup :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
doesNotContainAsNormalSubgroup = Tag_Entity "&#8939;"

-- | The not normal subgroup of or equal to HTML entity ('⋬').
notNormalSubgroupOfOrEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
notNormalSubgroupOfOrEqualTo = Tag_Entity "&#8940;"

-- | The does not contain as normal subgroup or equal HTML entity ('⋭').
doesNotContainAsNormalSubgroupOrEqual :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
doesNotContainAsNormalSubgroupOrEqual = Tag_Entity "&#8941;"

-- | The vertical ellipsis HTML entity ('⋮').
verticalEllipsis :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
verticalEllipsis = Tag_Entity "&#8942;"

-- | The midline horizontal ellipsis HTML entity ('⋯').
midlineHorizontalEllipsis :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
midlineHorizontalEllipsis = Tag_Entity "&#8943;"

-- | The up right diagonal ellipsis HTML entity ('⋰').
upRightDiagonalEllipsis :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
upRightDiagonalEllipsis = Tag_Entity "&#8944;"

-- | The down right diagonal ellipsis HTML entity ('⋱').
downRightDiagonalEllipsis :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
downRightDiagonalEllipsis = Tag_Entity "&#8945;"

-- | The element of with long horizontal stroke HTML entity ('⋲').
elementOfWithLongHorizontalStroke :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
elementOfWithLongHorizontalStroke = Tag_Entity "&#8946;"

-- | The element of with vertical bar at end of horizontal stroke HTML entity ('⋳').
elementOfWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Text parent grandparent
                                                => ChildHTML parent grandparent
elementOfWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity "&#8947;"

-- | The small element of with vertical bar at end of horizontal stroke HTML entity ('⋴').
smallElementOfWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Text parent grandparent
                                                     => ChildHTML parent grandparent
smallElementOfWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity "&#8948;"

-- | The element of with dot above HTML entity ('⋵').
elementOfWithDotAbove :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
elementOfWithDotAbove = Tag_Entity "&#8949;"

-- | The element of with overbar HTML entity ('⋶').
elementOfWithOverbar :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
elementOfWithOverbar = Tag_Entity "&#8950;"

-- | The small element of with overbar HTML entity ('⋷').
smallElementOfWithOverbar :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
smallElementOfWithOverbar = Tag_Entity "&#8951;"

-- | The element of with two horizontal strokes HTML entity ('⋹').
elementOfWithTwoHorizontalStrokes :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
elementOfWithTwoHorizontalStrokes = Tag_Entity "&#8953;"

-- | The contains with long horizontal stroke HTML entity ('⋺').
containsWithLongHorizontalStroke :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
containsWithLongHorizontalStroke = Tag_Entity "&#8954;"

-- | The contains with vertical bar at end of horizontal stroke HTML entity ('⋻').
containsWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Text parent grandparent
                                               => ChildHTML parent grandparent
containsWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity "&#8955;"

-- | The small contains with vertical bar at end of horizontal stroke HTML entity ('⋼').
smallContainsWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Text parent grandparent
                                                    => ChildHTML parent grandparent
smallContainsWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity "&#8956;"

-- | The contains with overbar HTML entity ('⋽').
containsWithOverbar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
containsWithOverbar = Tag_Entity "&#8957;"

-- | The small contains with overbar HTML entity ('⋾').
smallContainsWithOverbar :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
smallContainsWithOverbar = Tag_Entity "&#8958;"

-- | The projective HTML entity ('⌅').
projective :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
projective = Tag_Entity "&#8965;"

-- | The perspective HTML entity ('⌆').
perspective :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
perspective = Tag_Entity "&#8966;"

-- | The left ceiling HTML entity ('⌈').
leftCeiling :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
leftCeiling = Tag_Entity "&#8968;"

-- | The right ceiling HTML entity ('⌉').
rightCeiling :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
rightCeiling = Tag_Entity "&#8969;"

-- | The left floor HTML entity ('⌊').
leftFloor :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
leftFloor = Tag_Entity "&#8970;"

-- | The right floor HTML entity ('⌋').
rightFloor :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
rightFloor = Tag_Entity "&#8971;"

-- | The bottom right crop HTML entity ('⌌').
bottomRightCrop :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
bottomRightCrop = Tag_Entity "&#8972;"

-- | The bottom left crop HTML entity ('⌍').
bottomLeftCrop :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
bottomLeftCrop = Tag_Entity "&#8973;"

-- | The top right crop HTML entity ('⌎').
topRightCrop :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
topRightCrop = Tag_Entity "&#8974;"

-- | The top left crop HTML entity ('⌏').
topLeftCrop :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
topLeftCrop = Tag_Entity "&#8975;"

-- | The reversed not sign HTML entity ('⌐').
reversedNotSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
reversedNotSign = Tag_Entity "&#8976;"

-- | The arc HTML entity ('⌒').
arc :: ValidChild Text parent grandparent
    => ChildHTML parent grandparent
arc = Tag_Entity "&#8978;"

-- | The segment HTML entity ('⌓').
segment :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
segment = Tag_Entity "&#8979;"

-- | The telephone recorder HTML entity ('⌕').
telephoneRecorder :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
telephoneRecorder = Tag_Entity "&#8981;"

-- | The position indicator HTML entity ('⌖').
positionIndicator :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
positionIndicator = Tag_Entity "&#8982;"

-- | The top left corner HTML entity ('⌜').
topLeftCorner :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
topLeftCorner = Tag_Entity "&#8988;"

-- | The top right corner HTML entity ('⌝').
topRightCorner :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
topRightCorner = Tag_Entity "&#8989;"

-- | The bottom left corner HTML entity ('⌞').
bottomLeftCorner :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
bottomLeftCorner = Tag_Entity "&#8990;"

-- | The bottom right corner HTML entity ('⌟').
bottomRightCorner :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
bottomRightCorner = Tag_Entity "&#8991;"

-- | The frown HTML entity ('⌢').
frown :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
frown = Tag_Entity "&#8994;"

-- | The smile HTML entity ('⌣').
smile :: ValidChild Text parent grandparent
      => ChildHTML parent grandparent
smile = Tag_Entity "&#8995;"

-- | The left-pointing angle bracket = bra HTML entity ('〈').
leftPointingAngleBracket :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftPointingAngleBracket = Tag_Entity "&#9001;"

-- | The right-pointing angle bracket = ket HTML entity ('〉').
rightPointingAngleBracket :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightPointingAngleBracket = Tag_Entity "&#9002;"

-- | The cylindricity HTML entity ('⌭').
cylindricity :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
cylindricity = Tag_Entity "&#9005;"

-- | The all around-profile HTML entity ('⌮').
allAroundProfile :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
allAroundProfile = Tag_Entity "&#9006;"

-- | The apl functional symbol i-beam HTML entity ('⌶').
aplFunctionalSymbolIBeam :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
aplFunctionalSymbolIBeam = Tag_Entity "&#9014;"

-- | The Apl Functional Symbol Quad Equal HTML entity ('⌸').
aplFunctionalSymbolQuadEqual :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
aplFunctionalSymbolQuadEqual = Tag_Entity "&#9016;"

-- | The apl functional symbol circle stile HTML entity ('⌽').
aplFunctionalSymbolCircleStile :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
aplFunctionalSymbolCircleStile = Tag_Entity "&#9021;"

-- | The apl functional symbol slash bar HTML entity ('⌿').
aplFunctionalSymbolSlashBar :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
aplFunctionalSymbolSlashBar = Tag_Entity "&#9023;"

-- | The Apl Functional Symbol Quad Less-than HTML entity ('⍃').
aplFunctionalSymbolQuadLessThan :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
aplFunctionalSymbolQuadLessThan = Tag_Entity "&#9027;"

-- | The Apl Functional Symbol Quad Greater-than HTML entity ('⍄').
aplFunctionalSymbolQuadGreaterThan :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
aplFunctionalSymbolQuadGreaterThan = Tag_Entity "&#9028;"

-- | The Apl Functional Symbol Delta Stile HTML entity ('⍋').
aplFunctionalSymbolDeltaStile :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
aplFunctionalSymbolDeltaStile = Tag_Entity "&#9035;"

-- | The Apl Functional Symbol Quad Delta HTML entity ('⍍').
aplFunctionalSymbolQuadDelta :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
aplFunctionalSymbolQuadDelta = Tag_Entity "&#9037;"

-- | The Apl Functional Symbol Delta Underbar HTML entity ('⍙').
aplFunctionalSymbolDeltaUnderbar :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
aplFunctionalSymbolDeltaUnderbar = Tag_Entity "&#9049;"

-- | The Apl Functional Symbol Greater-than Diaeresis HTML entity ('⍩').
aplFunctionalSymbolGreaterThanDiaeresis :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
aplFunctionalSymbolGreaterThanDiaeresis = Tag_Entity "&#9065;"

-- | The Apl Functional Symbol Quad Not Equal HTML entity ('⍯').
aplFunctionalSymbolQuadNotEqual :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
aplFunctionalSymbolQuadNotEqual = Tag_Entity "&#9071;"

-- | The right angle with downwards zigzag arrow HTML entity ('⍼').
rightAngleWithDownwardsZigzagArrow :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightAngleWithDownwardsZigzagArrow = Tag_Entity "&#9084;"

-- | The upper left or lower right curly bracket section HTML entity ('⎰').
upperLeftOrLowerRightCurlyBracketSection :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
upperLeftOrLowerRightCurlyBracketSection = Tag_Entity "&#9136;"

-- | The upper right or lower left curly bracket section HTML entity ('⎱').
upperRightOrLowerLeftCurlyBracketSection :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
upperRightOrLowerLeftCurlyBracketSection = Tag_Entity "&#9137;"

-- | The Summation Top HTML entity ('⎲').
summationTop :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
summationTop = Tag_Entity "&#9138;"

-- | The Summation Bottom HTML entity ('⎳').
summationBottom :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
summationBottom = Tag_Entity "&#9139;"

-- | The top square bracket HTML entity ('⎴').
topSquareBracket :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
topSquareBracket = Tag_Entity "&#9140;"

-- | The bottom square bracket HTML entity ('⎵').
bottomSquareBracket :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
bottomSquareBracket = Tag_Entity "&#9141;"

-- | The bottom square bracket over top square bracket HTML entity ('⎶').
bottomSquareBracketOverTopSquareBracket :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
bottomSquareBracketOverTopSquareBracket = Tag_Entity "&#9142;"

-- | The top parenthesis HTML entity ('⏜').
topParenthesis :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
topParenthesis = Tag_Entity "&#9180;"

-- | The bottom parenthesis HTML entity ('⏝').
bottomParenthesis :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
bottomParenthesis = Tag_Entity "&#9181;"

-- | The top curly bracket HTML entity ('⏞').
topCurlyBracket :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
topCurlyBracket = Tag_Entity "&#9182;"

-- | The bottom curly bracket HTML entity ('⏟').
bottomCurlyBracket :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
bottomCurlyBracket = Tag_Entity "&#9183;"

-- | The white trapezium HTML entity ('⏢').
whiteTrapezium :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
whiteTrapezium = Tag_Entity "&#9186;"

-- | The electrical intersection HTML entity ('⏧').
electricalIntersection :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
electricalIntersection = Tag_Entity "&#9191;"

-- | The open box HTML entity ('␣').
openBox :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
openBox = Tag_Entity "&#9251;"

-- | The circled latin capital letter s HTML entity ('Ⓢ').
circledLatinCapitalLetterS :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
circledLatinCapitalLetterS = Tag_Entity "&#9416;"

-- | The box drawings light horizontal HTML entity ('─').
boxDrawingsLightHorizontal :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
boxDrawingsLightHorizontal = Tag_Entity "&#9472;"

-- | The box drawings light vertical HTML entity ('│').
boxDrawingsLightVertical :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
boxDrawingsLightVertical = Tag_Entity "&#9474;"

-- | The box drawings light down and right HTML entity ('┌').
boxDrawingsLightDownAndRight :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
boxDrawingsLightDownAndRight = Tag_Entity "&#9484;"

-- | The box drawings light down and left HTML entity ('┐').
boxDrawingsLightDownAndLeft :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
boxDrawingsLightDownAndLeft = Tag_Entity "&#9488;"

-- | The box drawings light up and right HTML entity ('└').
boxDrawingsLightUpAndRight :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
boxDrawingsLightUpAndRight = Tag_Entity "&#9492;"

-- | The box drawings light up and left HTML entity ('┘').
boxDrawingsLightUpAndLeft :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
boxDrawingsLightUpAndLeft = Tag_Entity "&#9496;"

-- | The box drawings light vertical and right HTML entity ('├').
boxDrawingsLightVerticalAndRight :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
boxDrawingsLightVerticalAndRight = Tag_Entity "&#9500;"

-- | The box drawings light vertical and left HTML entity ('┤').
boxDrawingsLightVerticalAndLeft :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
boxDrawingsLightVerticalAndLeft = Tag_Entity "&#9508;"

-- | The box drawings light down and horizontal HTML entity ('┬').
boxDrawingsLightDownAndHorizontal :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
boxDrawingsLightDownAndHorizontal = Tag_Entity "&#9516;"

-- | The box drawings light up and horizontal HTML entity ('┴').
boxDrawingsLightUpAndHorizontal :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
boxDrawingsLightUpAndHorizontal = Tag_Entity "&#9524;"

-- | The box drawings light vertical and horizontal HTML entity ('┼').
boxDrawingsLightVerticalAndHorizontal :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
boxDrawingsLightVerticalAndHorizontal = Tag_Entity "&#9532;"

-- | The box drawings double horizontal HTML entity ('═').
boxDrawingsDoubleHorizontal :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
boxDrawingsDoubleHorizontal = Tag_Entity "&#9552;"

-- | The box drawings double vertical HTML entity ('║').
boxDrawingsDoubleVertical :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
boxDrawingsDoubleVertical = Tag_Entity "&#9553;"

-- | The box drawings down single and right double HTML entity ('╒').
boxDrawingsDownSingleAndRightDouble :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
boxDrawingsDownSingleAndRightDouble = Tag_Entity "&#9554;"

-- | The box drawings down double and right single HTML entity ('╓').
boxDrawingsDownDoubleAndRightSingle :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
boxDrawingsDownDoubleAndRightSingle = Tag_Entity "&#9555;"

-- | The box drawings double down and right HTML entity ('╔').
boxDrawingsDoubleDownAndRight :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
boxDrawingsDoubleDownAndRight = Tag_Entity "&#9556;"

-- | The box drawings down single and left double HTML entity ('╕').
boxDrawingsDownSingleAndLeftDouble :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
boxDrawingsDownSingleAndLeftDouble = Tag_Entity "&#9557;"

-- | The box drawings down double and left single HTML entity ('╖').
boxDrawingsDownDoubleAndLeftSingle :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
boxDrawingsDownDoubleAndLeftSingle = Tag_Entity "&#9558;"

-- | The box drawings double down and left HTML entity ('╗').
boxDrawingsDoubleDownAndLeft :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
boxDrawingsDoubleDownAndLeft = Tag_Entity "&#9559;"

-- | The box drawings up single and right double HTML entity ('╘').
boxDrawingsUpSingleAndRightDouble :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
boxDrawingsUpSingleAndRightDouble = Tag_Entity "&#9560;"

-- | The box drawings up double and right single HTML entity ('╙').
boxDrawingsUpDoubleAndRightSingle :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
boxDrawingsUpDoubleAndRightSingle = Tag_Entity "&#9561;"

-- | The box drawings double up and right HTML entity ('╚').
boxDrawingsDoubleUpAndRight :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
boxDrawingsDoubleUpAndRight = Tag_Entity "&#9562;"

-- | The box drawings up single and left double HTML entity ('╛').
boxDrawingsUpSingleAndLeftDouble :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
boxDrawingsUpSingleAndLeftDouble = Tag_Entity "&#9563;"

-- | The box drawings up double and left single HTML entity ('╜').
boxDrawingsUpDoubleAndLeftSingle :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
boxDrawingsUpDoubleAndLeftSingle = Tag_Entity "&#9564;"

-- | The box drawings double up and left HTML entity ('╝').
boxDrawingsDoubleUpAndLeft :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
boxDrawingsDoubleUpAndLeft = Tag_Entity "&#9565;"

-- | The box drawings vertical single and right double HTML entity ('╞').
boxDrawingsVerticalSingleAndRightDouble :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
boxDrawingsVerticalSingleAndRightDouble = Tag_Entity "&#9566;"

-- | The box drawings vertical double and right single HTML entity ('╟').
boxDrawingsVerticalDoubleAndRightSingle :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
boxDrawingsVerticalDoubleAndRightSingle = Tag_Entity "&#9567;"

-- | The box drawings double vertical and right HTML entity ('╠').
boxDrawingsDoubleVerticalAndRight :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
boxDrawingsDoubleVerticalAndRight = Tag_Entity "&#9568;"

-- | The box drawings vertical single and left double HTML entity ('╡').
boxDrawingsVerticalSingleAndLeftDouble :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
boxDrawingsVerticalSingleAndLeftDouble = Tag_Entity "&#9569;"

-- | The box drawings vertical double and left single HTML entity ('╢').
boxDrawingsVerticalDoubleAndLeftSingle :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
boxDrawingsVerticalDoubleAndLeftSingle = Tag_Entity "&#9570;"

-- | The box drawings double vertical and left HTML entity ('╣').
boxDrawingsDoubleVerticalAndLeft :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
boxDrawingsDoubleVerticalAndLeft = Tag_Entity "&#9571;"

-- | The box drawings down single and horizontal double HTML entity ('╤').
boxDrawingsDownSingleAndHorizontalDouble :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
boxDrawingsDownSingleAndHorizontalDouble = Tag_Entity "&#9572;"

-- | The box drawings down double and horizontal single HTML entity ('╥').
boxDrawingsDownDoubleAndHorizontalSingle :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
boxDrawingsDownDoubleAndHorizontalSingle = Tag_Entity "&#9573;"

-- | The box drawings double down and horizontal HTML entity ('╦').
boxDrawingsDoubleDownAndHorizontal :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
boxDrawingsDoubleDownAndHorizontal = Tag_Entity "&#9574;"

-- | The box drawings up single and horizontal double HTML entity ('╧').
boxDrawingsUpSingleAndHorizontalDouble :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
boxDrawingsUpSingleAndHorizontalDouble = Tag_Entity "&#9575;"

-- | The box drawings up double and horizontal single HTML entity ('╨').
boxDrawingsUpDoubleAndHorizontalSingle :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
boxDrawingsUpDoubleAndHorizontalSingle = Tag_Entity "&#9576;"

-- | The box drawings double up and horizontal HTML entity ('╩').
boxDrawingsDoubleUpAndHorizontal :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
boxDrawingsDoubleUpAndHorizontal = Tag_Entity "&#9577;"

-- | The box drawings vertical single and horizontal double HTML entity ('╪').
boxDrawingsVerticalSingleAndHorizontalDouble :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
boxDrawingsVerticalSingleAndHorizontalDouble = Tag_Entity "&#9578;"

-- | The box drawings vertical double and horizontal single HTML entity ('╫').
boxDrawingsVerticalDoubleAndHorizontalSingle :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
boxDrawingsVerticalDoubleAndHorizontalSingle = Tag_Entity "&#9579;"

-- | The box drawings double vertical and horizontal HTML entity ('╬').
boxDrawingsDoubleVerticalAndHorizontal :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
boxDrawingsDoubleVerticalAndHorizontal = Tag_Entity "&#9580;"

-- | The upper half block HTML entity ('▀').
upperHalfBlock :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
upperHalfBlock = Tag_Entity "&#9600;"

-- | The lower half block HTML entity ('▄').
lowerHalfBlock :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
lowerHalfBlock = Tag_Entity "&#9604;"

-- | The full block HTML entity ('█').
fullBlock :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
fullBlock = Tag_Entity "&#9608;"

-- | The light shade HTML entity ('░').
lightShade :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
lightShade = Tag_Entity "&#9617;"

-- | The medium shade HTML entity ('▒').
mediumShade :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
mediumShade = Tag_Entity "&#9618;"

-- | The dark shade HTML entity ('▓').
darkShade :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
darkShade = Tag_Entity "&#9619;"

-- | The black square HTML entity ('■').
blackSquare :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
blackSquare = Tag_Entity "&#9632;"

-- | The white square HTML entity ('□').
whiteSquare :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
whiteSquare = Tag_Entity "&#9633;"

-- | The white square with rounded corners HTML entity ('▢').
whiteSquareWithRoundedCorners :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
whiteSquareWithRoundedCorners = Tag_Entity "&#9634;"

-- | The white square containing black small square HTML entity ('▣').
whiteSquareContainingBlackSmallSquare :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
whiteSquareContainingBlackSmallSquare = Tag_Entity "&#9635;"

-- | The square with horizontal fill HTML entity ('▤').
squareWithHorizontalFill :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
squareWithHorizontalFill = Tag_Entity "&#9636;"

-- | The square with vertical fill HTML entity ('▥').
squareWithVerticalFill :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
squareWithVerticalFill = Tag_Entity "&#9637;"

-- | The square with orthogonal crosshatch fill HTML entity ('▦').
squareWithOrthogonalCrosshatchFill :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
squareWithOrthogonalCrosshatchFill = Tag_Entity "&#9638;"

-- | The square with upper left to lower right fill HTML entity ('▧').
squareWithUpperLeftToLowerRightFill :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
squareWithUpperLeftToLowerRightFill = Tag_Entity "&#9639;"

-- | The square with upper right to lower left fill HTML entity ('▨').
squareWithUpperRightToLowerLeftFill :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
squareWithUpperRightToLowerLeftFill = Tag_Entity "&#9640;"

-- | The square with diagonal crosshatch fill HTML entity ('▩').
squareWithDiagonalCrosshatchFill :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
squareWithDiagonalCrosshatchFill = Tag_Entity "&#9641;"

-- | The black small square HTML entity ('▪').
blackSmallSquare :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
blackSmallSquare = Tag_Entity "&#9642;"

-- | The white small square HTML entity ('▫').
whiteSmallSquare :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
whiteSmallSquare = Tag_Entity "&#9643;"

-- | The Black Rectangle HTML entity ('▬').
blackRectangle :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
blackRectangle = Tag_Entity "&#9644;"

-- | The white rectangle HTML entity ('▭').
whiteRectangle :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
whiteRectangle = Tag_Entity "&#9645;"

-- | The black vertical rectangle HTML entity ('▮').
blackVerticalRectangle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
blackVerticalRectangle = Tag_Entity "&#9646;"

-- | The White Vertical Rectangle HTML entity ('▯').
whiteVerticalRectangle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
whiteVerticalRectangle = Tag_Entity "&#9647;"

-- | The Black Parallelogram HTML entity ('▰').
blackParallelogram :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
blackParallelogram = Tag_Entity "&#9648;"

-- | The white parallelogram HTML entity ('▱').
whiteParallelogram :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
whiteParallelogram = Tag_Entity "&#9649;"

-- | The Black Up-pointing Triangle HTML entity ('▲').
blackUpPointingTriangle :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
blackUpPointingTriangle = Tag_Entity "&#9650;"

-- | The white up-pointing triangle HTML entity ('△').
whiteUpPointingTriangle :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
whiteUpPointingTriangle = Tag_Entity "&#9651;"

-- | The black up-pointing small triangle HTML entity ('▴').
blackUpPointingSmallTriangle :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
blackUpPointingSmallTriangle = Tag_Entity "&#9652;"

-- | The white up-pointing small triangle HTML entity ('▵').
whiteUpPointingSmallTriangle :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
whiteUpPointingSmallTriangle = Tag_Entity "&#9653;"

-- | The Black Right-pointing Triangle HTML entity ('▶').
blackRightPointingTriangle :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
blackRightPointingTriangle = Tag_Entity "&#9654;"

-- | The White Right-pointing Triangle HTML entity ('▷').
whiteRightPointingTriangle :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
whiteRightPointingTriangle = Tag_Entity "&#9655;"

-- | The black right-pointing small triangle HTML entity ('▸').
blackRightPointingSmallTriangle :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
blackRightPointingSmallTriangle = Tag_Entity "&#9656;"

-- | The white right-pointing small triangle HTML entity ('▹').
whiteRightPointingSmallTriangle :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
whiteRightPointingSmallTriangle = Tag_Entity "&#9657;"

-- | The Black Down-pointing Triangle HTML entity ('▼').
blackDownPointingTriangle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
blackDownPointingTriangle = Tag_Entity "&#9660;"

-- | The white down-pointing triangle HTML entity ('▽').
whiteDownPointingTriangle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
whiteDownPointingTriangle = Tag_Entity "&#9661;"

-- | The black down-pointing small triangle HTML entity ('▾').
blackDownPointingSmallTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
blackDownPointingSmallTriangle = Tag_Entity "&#9662;"

-- | The white down-pointing small triangle HTML entity ('▿').
whiteDownPointingSmallTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
whiteDownPointingSmallTriangle = Tag_Entity "&#9663;"

-- | The Black Left-pointing Triangle HTML entity ('◀').
blackLeftPointingTriangle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
blackLeftPointingTriangle = Tag_Entity "&#9664;"

-- | The White Left-pointing Triangle HTML entity ('◁').
whiteLeftPointingTriangle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
whiteLeftPointingTriangle = Tag_Entity "&#9665;"

-- | The black left-pointing small triangle HTML entity ('◂').
blackLeftPointingSmallTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
blackLeftPointingSmallTriangle = Tag_Entity "&#9666;"

-- | The white left-pointing small triangle HTML entity ('◃').
whiteLeftPointingSmallTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
whiteLeftPointingSmallTriangle = Tag_Entity "&#9667;"

-- | The Black Diamond HTML entity ('◆').
blackDiamond :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
blackDiamond = Tag_Entity "&#9670;"

-- | The White Diamond HTML entity ('◇').
whiteDiamond :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
whiteDiamond = Tag_Entity "&#9671;"

-- | The White Diamond Containing Black Small Diamond HTML entity ('◈').
whiteDiamondContainingBlackSmallDiamond :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
whiteDiamondContainingBlackSmallDiamond = Tag_Entity "&#9672;"

-- | The lozenge HTML entity ('◊').
lozenge :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
lozenge = Tag_Entity "&#9674;"

-- | The white circle HTML entity ('○').
whiteCircle :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
whiteCircle = Tag_Entity "&#9675;"

-- | The Dotted Circle HTML entity ('◌').
dottedCircle :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
dottedCircle = Tag_Entity "&#9676;"

-- | The Circle With Vertical Fill HTML entity ('◍').
circleWithVerticalFill :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
circleWithVerticalFill = Tag_Entity "&#9677;"

-- | The Black Circle HTML entity ('●').
blackCircle :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
blackCircle = Tag_Entity "&#9679;"

-- | The Circle With Left Half Black HTML entity ('◐').
circleWithLeftHalfBlack :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
circleWithLeftHalfBlack = Tag_Entity "&#9680;"

-- | The Circle With Right Half Black HTML entity ('◑').
circleWithRightHalfBlack :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
circleWithRightHalfBlack = Tag_Entity "&#9681;"

-- | The Circle With Lower Half Black HTML entity ('◒').
circleWithLowerHalfBlack :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
circleWithLowerHalfBlack = Tag_Entity "&#9682;"

-- | The Circle With Upper Half Black HTML entity ('◓').
circleWithUpperHalfBlack :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
circleWithUpperHalfBlack = Tag_Entity "&#9683;"

-- | The Circle With Upper Right Quadrant Black HTML entity ('◔').
circleWithUpperRightQuadrantBlack :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
circleWithUpperRightQuadrantBlack = Tag_Entity "&#9684;"

-- | The Circle With All But Upper Left Quadrant Black HTML entity ('◕').
circleWithAllButUpperLeftQuadrantBlack :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
circleWithAllButUpperLeftQuadrantBlack = Tag_Entity "&#9685;"

-- | The Left Half Black Circle HTML entity ('◖').
leftHalfBlackCircle :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
leftHalfBlackCircle = Tag_Entity "&#9686;"

-- | The Right Half Black Circle HTML entity ('◗').
rightHalfBlackCircle :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
rightHalfBlackCircle = Tag_Entity "&#9687;"

-- | The Inverse White Circle HTML entity ('◙').
inverseWhiteCircle :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
inverseWhiteCircle = Tag_Entity "&#9689;"

-- | The Upper Half Inverse White Circle HTML entity ('◚').
upperHalfInverseWhiteCircle :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
upperHalfInverseWhiteCircle = Tag_Entity "&#9690;"

-- | The Lower Half Inverse White Circle HTML entity ('◛').
lowerHalfInverseWhiteCircle :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
lowerHalfInverseWhiteCircle = Tag_Entity "&#9691;"

-- | The Upper Left Quadrant Circular Arc HTML entity ('◜').
upperLeftQuadrantCircularArc :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
upperLeftQuadrantCircularArc = Tag_Entity "&#9692;"

-- | The Upper Right Quadrant Circular Arc HTML entity ('◝').
upperRightQuadrantCircularArc :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
upperRightQuadrantCircularArc = Tag_Entity "&#9693;"

-- | The Lower Right Quadrant Circular Arc HTML entity ('◞').
lowerRightQuadrantCircularArc :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
lowerRightQuadrantCircularArc = Tag_Entity "&#9694;"

-- | The Lower Left Quadrant Circular Arc HTML entity ('◟').
lowerLeftQuadrantCircularArc :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
lowerLeftQuadrantCircularArc = Tag_Entity "&#9695;"

-- | The Upper Half Circle HTML entity ('◠').
upperHalfCircle :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
upperHalfCircle = Tag_Entity "&#9696;"

-- | The Lower Half Circle HTML entity ('◡').
lowerHalfCircle :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
lowerHalfCircle = Tag_Entity "&#9697;"

-- | The Black Lower Right Triangle HTML entity ('◢').
blackLowerRightTriangle :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
blackLowerRightTriangle = Tag_Entity "&#9698;"

-- | The Black Lower Left Triangle HTML entity ('◣').
blackLowerLeftTriangle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
blackLowerLeftTriangle = Tag_Entity "&#9699;"

-- | The Black Upper Left Triangle HTML entity ('◤').
blackUpperLeftTriangle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
blackUpperLeftTriangle = Tag_Entity "&#9700;"

-- | The Black Upper Right Triangle HTML entity ('◥').
blackUpperRightTriangle :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
blackUpperRightTriangle = Tag_Entity "&#9701;"

-- | The square with left half black HTML entity ('◧').
squareWithLeftHalfBlack :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
squareWithLeftHalfBlack = Tag_Entity "&#9703;"

-- | The square with right half black HTML entity ('◨').
squareWithRightHalfBlack :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
squareWithRightHalfBlack = Tag_Entity "&#9704;"

-- | The square with upper left diagonal half black HTML entity ('◩').
squareWithUpperLeftDiagonalHalfBlack :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
squareWithUpperLeftDiagonalHalfBlack = Tag_Entity "&#9705;"

-- | The square with lower right diagonal half black HTML entity ('◪').
squareWithLowerRightDiagonalHalfBlack :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
squareWithLowerRightDiagonalHalfBlack = Tag_Entity "&#9706;"

-- | The white square with vertical bisecting line HTML entity ('◫').
whiteSquareWithVerticalBisectingLine :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
whiteSquareWithVerticalBisectingLine = Tag_Entity "&#9707;"

-- | The white up-pointing triangle with dot HTML entity ('◬').
whiteUpPointingTriangleWithDot :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
whiteUpPointingTriangleWithDot = Tag_Entity "&#9708;"

-- | The Up-pointing Triangle With Left Half Black HTML entity ('◭').
upPointingTriangleWithLeftHalfBlack :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
upPointingTriangleWithLeftHalfBlack = Tag_Entity "&#9709;"

-- | The Up-pointing Triangle With Right Half Black HTML entity ('◮').
upPointingTriangleWithRightHalfBlack :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
upPointingTriangleWithRightHalfBlack = Tag_Entity "&#9710;"

-- | The large circle HTML entity ('◯').
largeCircle :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
largeCircle = Tag_Entity "&#9711;"

-- | The white square with upper left quadrant HTML entity ('◰').
whiteSquareWithUpperLeftQuadrant :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
whiteSquareWithUpperLeftQuadrant = Tag_Entity "&#9712;"

-- | The white square with lower left quadrant HTML entity ('◱').
whiteSquareWithLowerLeftQuadrant :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
whiteSquareWithLowerLeftQuadrant = Tag_Entity "&#9713;"

-- | The white square with lower right quadrant HTML entity ('◲').
whiteSquareWithLowerRightQuadrant :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
whiteSquareWithLowerRightQuadrant = Tag_Entity "&#9714;"

-- | The white square with upper right quadrant HTML entity ('◳').
whiteSquareWithUpperRightQuadrant :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
whiteSquareWithUpperRightQuadrant = Tag_Entity "&#9715;"

-- | The White Circle With Upper Left Quadrant HTML entity ('◴').
whiteCircleWithUpperLeftQuadrant :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
whiteCircleWithUpperLeftQuadrant = Tag_Entity "&#9716;"

-- | The White Circle With Lower Left Quadrant HTML entity ('◵').
whiteCircleWithLowerLeftQuadrant :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
whiteCircleWithLowerLeftQuadrant = Tag_Entity "&#9717;"

-- | The White Circle With Lower Right Quadrant HTML entity ('◶').
whiteCircleWithLowerRightQuadrant :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
whiteCircleWithLowerRightQuadrant = Tag_Entity "&#9718;"

-- | The White Circle With Upper Right Quadrant HTML entity ('◷').
whiteCircleWithUpperRightQuadrant :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
whiteCircleWithUpperRightQuadrant = Tag_Entity "&#9719;"

-- | The upper left triangle HTML entity ('◸').
upperLeftTriangle :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
upperLeftTriangle = Tag_Entity "&#9720;"

-- | The upper right triangle HTML entity ('◹').
upperRightTriangle :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
upperRightTriangle = Tag_Entity "&#9721;"

-- | The lower left triangle HTML entity ('◺').
lowerLeftTriangle :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
lowerLeftTriangle = Tag_Entity "&#9722;"

-- | The white medium square HTML entity ('◻').
whiteMediumSquare :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
whiteMediumSquare = Tag_Entity "&#9723;"

-- | The black medium square HTML entity ('◼').
blackMediumSquare :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
blackMediumSquare = Tag_Entity "&#9724;"

-- | The white medium small square HTML entity ('◽').
whiteMediumSmallSquare :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
whiteMediumSmallSquare = Tag_Entity "&#9725;"

-- | The black medium small square HTML entity ('◾').
blackMediumSmallSquare :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
blackMediumSmallSquare = Tag_Entity "&#9726;"

-- | The Lower Right Triangle HTML entity ('◿').
lowerRightTriangle :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
lowerRightTriangle = Tag_Entity "&#9727;"

-- | The black star HTML entity ('★').
blackStar :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
blackStar = Tag_Entity "&#9733;"

-- | The white star HTML entity ('☆').
whiteStar :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
whiteStar = Tag_Entity "&#9734;"

-- | The black telephone HTML entity ('☎').
blackTelephone :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
blackTelephone = Tag_Entity "&#9742;"

-- | The Trigram For Heaven HTML entity ('☰').
trigramForHeaven :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
trigramForHeaven = Tag_Entity "&#9776;"

-- | The Trigram For Lake HTML entity ('☱').
trigramForLake :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
trigramForLake = Tag_Entity "&#9777;"

-- | The Trigram For Fire HTML entity ('☲').
trigramForFire :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
trigramForFire = Tag_Entity "&#9778;"

-- | The Trigram For Thunder HTML entity ('☳').
trigramForThunder :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
trigramForThunder = Tag_Entity "&#9779;"

-- | The Trigram For Wind HTML entity ('☴').
trigramForWind :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
trigramForWind = Tag_Entity "&#9780;"

-- | The Trigram For Water HTML entity ('☵').
trigramForWater :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
trigramForWater = Tag_Entity "&#9781;"

-- | The Trigram For Mountain HTML entity ('☶').
trigramForMountain :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
trigramForMountain = Tag_Entity "&#9782;"

-- | The Trigram For Earth HTML entity ('☷').
trigramForEarth :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
trigramForEarth = Tag_Entity "&#9783;"

-- | The female sign HTML entity ('♀').
femaleSign :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
femaleSign = Tag_Entity "&#9792;"

-- | The male sign HTML entity ('♂').
maleSign :: ValidChild Text parent grandparent
         => ChildHTML parent grandparent
maleSign = Tag_Entity "&#9794;"

-- | The black spade suit HTML entity ('♠').
blackSpadeSuit :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
blackSpadeSuit = Tag_Entity "&#9824;"

-- | The White Diamond Suit HTML entity ('♢').
whiteDiamondSuit :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
whiteDiamondSuit = Tag_Entity "&#9826;"

-- | The black club suit HTML entity ('♣').
blackClubSuit :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
blackClubSuit = Tag_Entity "&#9827;"

-- | The black heart suit HTML entity ('♥').
blackHeartSuit :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
blackHeartSuit = Tag_Entity "&#9829;"

-- | The black diamond suit HTML entity ('♦').
blackDiamondSuit :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
blackDiamondSuit = Tag_Entity "&#9830;"

-- | The eighth note HTML entity ('♪').
eighthNote :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
eighthNote = Tag_Entity "&#9834;"

-- | The music flat sign HTML entity ('♭').
musicFlatSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
musicFlatSign = Tag_Entity "&#9837;"

-- | The music natural sign HTML entity ('♮').
musicNaturalSign :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
musicNaturalSign = Tag_Entity "&#9838;"

-- | The music sharp sign HTML entity ('♯').
musicSharpSign :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
musicSharpSign = Tag_Entity "&#9839;"

-- | The White Circle With Dot Right HTML entity ('⚆').
whiteCircleWithDotRight :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
whiteCircleWithDotRight = Tag_Entity "&#9862;"

-- | The White Circle With Two Dots HTML entity ('⚇').
whiteCircleWithTwoDots :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
whiteCircleWithTwoDots = Tag_Entity "&#9863;"

-- | The Black Circle With White Dot Right HTML entity ('⚈').
blackCircleWithWhiteDotRight :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
blackCircleWithWhiteDotRight = Tag_Entity "&#9864;"

-- | The Black Circle With Two White Dots HTML entity ('⚉').
blackCircleWithTwoWhiteDots :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
blackCircleWithTwoWhiteDots = Tag_Entity "&#9865;"

-- | The Medium White Circle HTML entity ('⚪').
mediumWhiteCircle :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
mediumWhiteCircle = Tag_Entity "&#9898;"

-- | The Medium Black Circle HTML entity ('⚫').
mediumBlackCircle :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
mediumBlackCircle = Tag_Entity "&#9899;"

-- | The Medium Small White Circle HTML entity ('⚬').
mediumSmallWhiteCircle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
mediumSmallWhiteCircle = Tag_Entity "&#9900;"

-- | The squared key HTML entity ('⚿').
squaredKey :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
squaredKey = Tag_Entity "&#9919;"

-- | The white diamond in square HTML entity ('⛋').
whiteDiamondInSquare :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
whiteDiamondInSquare = Tag_Entity "&#9931;"

-- | The Heavy White Down-pointing Triangle HTML entity ('⛛').
heavyWhiteDownPointingTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
heavyWhiteDownPointingTriangle = Tag_Entity "&#9947;"

-- | The squared saltire HTML entity ('⛝').
squaredSaltire :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
squaredSaltire = Tag_Entity "&#9949;"

-- | The falling diagonal in white circle in black square HTML entity ('⛞').
fallingDiagonalInWhiteCircleInBlackSquare :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
fallingDiagonalInWhiteCircleInBlackSquare = Tag_Entity "&#9950;"

-- | The square four corners HTML entity ('⛶').
squareFourCorners :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
squareFourCorners = Tag_Entity "&#9974;"

-- | The cup on black square HTML entity ('⛾').
cupOnBlackSquare :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
cupOnBlackSquare = Tag_Entity "&#9982;"

-- | The check mark HTML entity ('✓').
checkMark :: ValidChild Text parent grandparent
          => ChildHTML parent grandparent
checkMark = Tag_Entity "&#10003;"

-- | The ballot x HTML entity ('✗').
ballotX :: ValidChild Text parent grandparent
        => ChildHTML parent grandparent
ballotX = Tag_Entity "&#10007;"

-- | The maltese cross HTML entity ('✠').
malteseCross :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
malteseCross = Tag_Entity "&#10016;"

-- | The Circled White Star HTML entity ('✪').
circledWhiteStar :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
circledWhiteStar = Tag_Entity "&#10026;"

-- | The six pointed black star HTML entity ('✶').
sixPointedBlackStar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
sixPointedBlackStar = Tag_Entity "&#10038;"

-- | The Circled Open Centre Eight Pointed Star HTML entity ('❂').
circledOpenCentreEightPointedStar :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
circledOpenCentreEightPointedStar = Tag_Entity "&#10050;"

-- | The Shadowed White Circle HTML entity ('❍').
shadowedWhiteCircle :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
shadowedWhiteCircle = Tag_Entity "&#10061;"

-- | The lower right drop-shadowed white square HTML entity ('❏').
lowerRightDropShadowedWhiteSquare :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
lowerRightDropShadowedWhiteSquare = Tag_Entity "&#10063;"

-- | The upper right drop-shadowed white square HTML entity ('❐').
upperRightDropShadowedWhiteSquare :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
upperRightDropShadowedWhiteSquare = Tag_Entity "&#10064;"

-- | The lower right shadowed white square HTML entity ('❑').
lowerRightShadowedWhiteSquare :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
lowerRightShadowedWhiteSquare = Tag_Entity "&#10065;"

-- | The upper right shadowed white square HTML entity ('❒').
upperRightShadowedWhiteSquare :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
upperRightShadowedWhiteSquare = Tag_Entity "&#10066;"

-- | The Black Diamond Minus White X HTML entity ('❖').
blackDiamondMinusWhiteX :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
blackDiamondMinusWhiteX = Tag_Entity "&#10070;"

-- | The light vertical bar HTML entity ('❘').
lightVerticalBar :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
lightVerticalBar = Tag_Entity "&#10072;"

-- | The light left tortoise shell bracket ornament HTML entity ('❲').
lightLeftTortoiseShellBracketOrnament :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
lightLeftTortoiseShellBracketOrnament = Tag_Entity "&#10098;"

-- | The light right tortoise shell bracket ornament HTML entity ('❳').
lightRightTortoiseShellBracketOrnament :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
lightRightTortoiseShellBracketOrnament = Tag_Entity "&#10099;"

-- | The Heavy Plus Sign HTML entity ('➕').
heavyPlusSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
heavyPlusSign = Tag_Entity "&#10133;"

-- | The Heavy Minus Sign HTML entity ('➖').
heavyMinusSign :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
heavyMinusSign = Tag_Entity "&#10134;"

-- | The Heavy Division Sign HTML entity ('➗').
heavyDivisionSign :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
heavyDivisionSign = Tag_Entity "&#10135;"

-- | The Three Dimensional Angle HTML entity ('⟀').
threeDimensionalAngle :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
threeDimensionalAngle = Tag_Entity "&#10176;"

-- | The White Triangle Containing Small White Triangle HTML entity ('⟁').
whiteTriangleContainingSmallWhiteTriangle :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
whiteTriangleContainingSmallWhiteTriangle = Tag_Entity "&#10177;"

-- | The Mathematical Rising Diagonal HTML entity ('⟋').
mathematicalRisingDiagonal :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalRisingDiagonal = Tag_Entity "&#10187;"

-- | The Long Division HTML entity ('⟌').
longDivision :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
longDivision = Tag_Entity "&#10188;"

-- | The Mathematical Falling Diagonal HTML entity ('⟍').
mathematicalFallingDiagonal :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFallingDiagonal = Tag_Entity "&#10189;"

-- | The squared logical and HTML entity ('⟎').
squaredLogicalAnd :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
squaredLogicalAnd = Tag_Entity "&#10190;"

-- | The squared logical or HTML entity ('⟏').
squaredLogicalOr :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
squaredLogicalOr = Tag_Entity "&#10191;"

-- | The White Diamond With Centred Dot HTML entity ('⟐').
whiteDiamondWithCentredDot :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
whiteDiamondWithCentredDot = Tag_Entity "&#10192;"

-- | The Up Tack With Circle Above HTML entity ('⟟').
upTackWithCircleAbove :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
upTackWithCircleAbove = Tag_Entity "&#10207;"

-- | The Lozenge Divided By Horizontal Rule HTML entity ('⟠').
lozengeDividedByHorizontalRule :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
lozengeDividedByHorizontalRule = Tag_Entity "&#10208;"

-- | The White Concave-sided Diamond HTML entity ('⟡').
whiteConcaveSidedDiamond :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
whiteConcaveSidedDiamond = Tag_Entity "&#10209;"

-- | The White Concave-sided Diamond With Leftwards Tick HTML entity ('⟢').
whiteConcaveSidedDiamondWithLeftwardsTick :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
whiteConcaveSidedDiamondWithLeftwardsTick = Tag_Entity "&#10210;"

-- | The White Concave-sided Diamond With Rightwards Tick HTML entity ('⟣').
whiteConcaveSidedDiamondWithRightwardsTick :: ValidChild Text parent grandparent
                                           => ChildHTML parent grandparent
whiteConcaveSidedDiamondWithRightwardsTick = Tag_Entity "&#10211;"

-- | The white square with leftwards tick HTML entity ('⟤').
whiteSquareWithLeftwardsTick :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
whiteSquareWithLeftwardsTick = Tag_Entity "&#10212;"

-- | The white square with rightwards tick HTML entity ('⟥').
whiteSquareWithRightwardsTick :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
whiteSquareWithRightwardsTick = Tag_Entity "&#10213;"

-- | The mathematical left white square bracket HTML entity ('⟦').
mathematicalLeftWhiteSquareBracket :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
mathematicalLeftWhiteSquareBracket = Tag_Entity "&#10214;"

-- | The mathematical right white square bracket HTML entity ('⟧').
mathematicalRightWhiteSquareBracket :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
mathematicalRightWhiteSquareBracket = Tag_Entity "&#10215;"

-- | The mathematical left angle bracket HTML entity ('⟨').
mathematicalLeftAngleBracket :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
mathematicalLeftAngleBracket = Tag_Entity "&#10216;"

-- | The mathematical right angle bracket HTML entity ('⟩').
mathematicalRightAngleBracket :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
mathematicalRightAngleBracket = Tag_Entity "&#10217;"

-- | The mathematical left double angle bracket HTML entity ('⟪').
mathematicalLeftDoubleAngleBracket :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
mathematicalLeftDoubleAngleBracket = Tag_Entity "&#10218;"

-- | The mathematical right double angle bracket HTML entity ('⟫').
mathematicalRightDoubleAngleBracket :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
mathematicalRightDoubleAngleBracket = Tag_Entity "&#10219;"

-- | The mathematical left white tortoise shell bracket HTML entity ('⟬').
mathematicalLeftWhiteTortoiseShellBracket :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
mathematicalLeftWhiteTortoiseShellBracket = Tag_Entity "&#10220;"

-- | The mathematical right white tortoise shell bracket HTML entity ('⟭').
mathematicalRightWhiteTortoiseShellBracket :: ValidChild Text parent grandparent
                                           => ChildHTML parent grandparent
mathematicalRightWhiteTortoiseShellBracket = Tag_Entity "&#10221;"

-- | The long leftwards arrow HTML entity ('⟵').
longLeftwardsArrow :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
longLeftwardsArrow = Tag_Entity "&#10229;"

-- | The long rightwards arrow HTML entity ('⟶').
longRightwardsArrow :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
longRightwardsArrow = Tag_Entity "&#10230;"

-- | The long left right arrow HTML entity ('⟷').
longLeftRightArrow :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
longLeftRightArrow = Tag_Entity "&#10231;"

-- | The long leftwards double arrow HTML entity ('⟸').
longLeftwardsDoubleArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
longLeftwardsDoubleArrow = Tag_Entity "&#10232;"

-- | The long rightwards double arrow HTML entity ('⟹').
longRightwardsDoubleArrow :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
longRightwardsDoubleArrow = Tag_Entity "&#10233;"

-- | The long left right double arrow HTML entity ('⟺').
longLeftRightDoubleArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
longLeftRightDoubleArrow = Tag_Entity "&#10234;"

-- | The long rightwards arrow from bar HTML entity ('⟼').
longRightwardsArrowFromBar :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
longRightwardsArrowFromBar = Tag_Entity "&#10236;"

-- | The long rightwards squiggle arrow HTML entity ('⟿').
longRightwardsSquiggleArrow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
longRightwardsSquiggleArrow = Tag_Entity "&#10239;"

-- | The leftwards double arrow with vertical stroke HTML entity ('⤂').
leftwardsDoubleArrowWithVerticalStroke :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
leftwardsDoubleArrowWithVerticalStroke = Tag_Entity "&#10498;"

-- | The rightwards double arrow with vertical stroke HTML entity ('⤃').
rightwardsDoubleArrowWithVerticalStroke :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
rightwardsDoubleArrowWithVerticalStroke = Tag_Entity "&#10499;"

-- | The left right double arrow with vertical stroke HTML entity ('⤄').
leftRightDoubleArrowWithVerticalStroke :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
leftRightDoubleArrowWithVerticalStroke = Tag_Entity "&#10500;"

-- | The rightwards two-headed arrow from bar HTML entity ('⤅').
rightwardsTwoHeadedArrowFromBar :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
rightwardsTwoHeadedArrowFromBar = Tag_Entity "&#10501;"

-- | The leftwards double dash arrow HTML entity ('⤌').
leftwardsDoubleDashArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftwardsDoubleDashArrow = Tag_Entity "&#10508;"

-- | The rightwards double dash arrow HTML entity ('⤍').
rightwardsDoubleDashArrow :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightwardsDoubleDashArrow = Tag_Entity "&#10509;"

-- | The leftwards triple dash arrow HTML entity ('⤎').
leftwardsTripleDashArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftwardsTripleDashArrow = Tag_Entity "&#10510;"

-- | The rightwards triple dash arrow HTML entity ('⤏').
rightwardsTripleDashArrow :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightwardsTripleDashArrow = Tag_Entity "&#10511;"

-- | The rightwards two-headed triple dash arrow HTML entity ('⤐').
rightwardsTwoHeadedTripleDashArrow :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightwardsTwoHeadedTripleDashArrow = Tag_Entity "&#10512;"

-- | The rightwards arrow with dotted stem HTML entity ('⤑').
rightwardsArrowWithDottedStem :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
rightwardsArrowWithDottedStem = Tag_Entity "&#10513;"

-- | The upwards arrow to bar HTML entity ('⤒').
upwardsArrowToBar :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
upwardsArrowToBar = Tag_Entity "&#10514;"

-- | The downwards arrow to bar HTML entity ('⤓').
downwardsArrowToBar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
downwardsArrowToBar = Tag_Entity "&#10515;"

-- | The rightwards two-headed arrow with tail HTML entity ('⤖').
rightwardsTwoHeadedArrowWithTail :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
rightwardsTwoHeadedArrowWithTail = Tag_Entity "&#10518;"

-- | The leftwards arrow-tail HTML entity ('⤙').
leftwardsArrowTail :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
leftwardsArrowTail = Tag_Entity "&#10521;"

-- | The rightwards arrow-tail HTML entity ('⤚').
rightwardsArrowTail :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
rightwardsArrowTail = Tag_Entity "&#10522;"

-- | The leftwards double arrow-tail HTML entity ('⤛').
leftwardsDoubleArrowTail :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
leftwardsDoubleArrowTail = Tag_Entity "&#10523;"

-- | The rightwards double arrow-tail HTML entity ('⤜').
rightwardsDoubleArrowTail :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
rightwardsDoubleArrowTail = Tag_Entity "&#10524;"

-- | The leftwards arrow to black diamond HTML entity ('⤝').
leftwardsArrowToBlackDiamond :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
leftwardsArrowToBlackDiamond = Tag_Entity "&#10525;"

-- | The rightwards arrow to black diamond HTML entity ('⤞').
rightwardsArrowToBlackDiamond :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
rightwardsArrowToBlackDiamond = Tag_Entity "&#10526;"

-- | The leftwards arrow from bar to black diamond HTML entity ('⤟').
leftwardsArrowFromBarToBlackDiamond :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
leftwardsArrowFromBarToBlackDiamond = Tag_Entity "&#10527;"

-- | The rightwards arrow from bar to black diamond HTML entity ('⤠').
rightwardsArrowFromBarToBlackDiamond :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
rightwardsArrowFromBarToBlackDiamond = Tag_Entity "&#10528;"

-- | The north west arrow with hook HTML entity ('⤣').
northWestArrowWithHook :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
northWestArrowWithHook = Tag_Entity "&#10531;"

-- | The north east arrow with hook HTML entity ('⤤').
northEastArrowWithHook :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
northEastArrowWithHook = Tag_Entity "&#10532;"

-- | The south east arrow with hook HTML entity ('⤥').
southEastArrowWithHook :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
southEastArrowWithHook = Tag_Entity "&#10533;"

-- | The south west arrow with hook HTML entity ('⤦').
southWestArrowWithHook :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
southWestArrowWithHook = Tag_Entity "&#10534;"

-- | The north west arrow and north east arrow HTML entity ('⤧').
northWestArrowAndNorthEastArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
northWestArrowAndNorthEastArrow = Tag_Entity "&#10535;"

-- | The north east arrow and south east arrow HTML entity ('⤨').
northEastArrowAndSouthEastArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
northEastArrowAndSouthEastArrow = Tag_Entity "&#10536;"

-- | The south east arrow and south west arrow HTML entity ('⤩').
southEastArrowAndSouthWestArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
southEastArrowAndSouthWestArrow = Tag_Entity "&#10537;"

-- | The south west arrow and north west arrow HTML entity ('⤪').
southWestArrowAndNorthWestArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
southWestArrowAndNorthWestArrow = Tag_Entity "&#10538;"

-- | The Rising Diagonal Crossing Falling Diagonal HTML entity ('⤫').
risingDiagonalCrossingFallingDiagonal :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
risingDiagonalCrossingFallingDiagonal = Tag_Entity "&#10539;"

-- | The Falling Diagonal Crossing Rising Diagonal HTML entity ('⤬').
fallingDiagonalCrossingRisingDiagonal :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
fallingDiagonalCrossingRisingDiagonal = Tag_Entity "&#10540;"

-- | The Falling Diagonal Crossing North East Arrow HTML entity ('⤯').
fallingDiagonalCrossingNorthEastArrow :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
fallingDiagonalCrossingNorthEastArrow = Tag_Entity "&#10543;"

-- | The Rising Diagonal Crossing South East Arrow HTML entity ('⤰').
risingDiagonalCrossingSouthEastArrow :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
risingDiagonalCrossingSouthEastArrow = Tag_Entity "&#10544;"

-- | The wave arrow pointing directly right HTML entity ('⤳').
waveArrowPointingDirectlyRight :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
waveArrowPointingDirectlyRight = Tag_Entity "&#10547;"

-- | The arrow pointing rightwards then curving downwards HTML entity ('⤵').
arrowPointingRightwardsThenCurvingDownwards :: ValidChild Text parent grandparent
                                            => ChildHTML parent grandparent
arrowPointingRightwardsThenCurvingDownwards = Tag_Entity "&#10549;"

-- | The arrow pointing downwards then curving leftwards HTML entity ('⤶').
arrowPointingDownwardsThenCurvingLeftwards :: ValidChild Text parent grandparent
                                           => ChildHTML parent grandparent
arrowPointingDownwardsThenCurvingLeftwards = Tag_Entity "&#10550;"

-- | The arrow pointing downwards then curving rightwards HTML entity ('⤷').
arrowPointingDownwardsThenCurvingRightwards :: ValidChild Text parent grandparent
                                            => ChildHTML parent grandparent
arrowPointingDownwardsThenCurvingRightwards = Tag_Entity "&#10551;"

-- | The right-side arc clockwise arrow HTML entity ('⤸').
rightSideArcClockwiseArrow :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
rightSideArcClockwiseArrow = Tag_Entity "&#10552;"

-- | The left-side arc anticlockwise arrow HTML entity ('⤹').
leftSideArcAnticlockwiseArrow :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
leftSideArcAnticlockwiseArrow = Tag_Entity "&#10553;"

-- | The Top Arc Anticlockwise Arrow HTML entity ('⤺').
topArcAnticlockwiseArrow :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
topArcAnticlockwiseArrow = Tag_Entity "&#10554;"

-- | The Bottom Arc Anticlockwise Arrow HTML entity ('⤻').
bottomArcAnticlockwiseArrow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
bottomArcAnticlockwiseArrow = Tag_Entity "&#10555;"

-- | The top arc clockwise arrow with minus HTML entity ('⤼').
topArcClockwiseArrowWithMinus :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
topArcClockwiseArrowWithMinus = Tag_Entity "&#10556;"

-- | The top arc anticlockwise arrow with plus HTML entity ('⤽').
topArcAnticlockwiseArrowWithPlus :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
topArcAnticlockwiseArrowWithPlus = Tag_Entity "&#10557;"

-- | The rightwards arrow with plus below HTML entity ('⥅').
rightwardsArrowWithPlusBelow :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
rightwardsArrowWithPlusBelow = Tag_Entity "&#10565;"

-- | The left right arrow through small circle HTML entity ('⥈').
leftRightArrowThroughSmallCircle :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
leftRightArrowThroughSmallCircle = Tag_Entity "&#10568;"

-- | The upwards two-headed arrow from small circle HTML entity ('⥉').
upwardsTwoHeadedArrowFromSmallCircle :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
upwardsTwoHeadedArrowFromSmallCircle = Tag_Entity "&#10569;"

-- | The left barb up right barb down harpoon HTML entity ('⥊').
leftBarbUpRightBarbDownHarpoon :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
leftBarbUpRightBarbDownHarpoon = Tag_Entity "&#10570;"

-- | The left barb down right barb up harpoon HTML entity ('⥋').
leftBarbDownRightBarbUpHarpoon :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
leftBarbDownRightBarbUpHarpoon = Tag_Entity "&#10571;"

-- | The Up Barb Right Down Barb Left Harpoon HTML entity ('⥌').
upBarbRightDownBarbLeftHarpoon :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
upBarbRightDownBarbLeftHarpoon = Tag_Entity "&#10572;"

-- | The Up Barb Left Down Barb Right Harpoon HTML entity ('⥍').
upBarbLeftDownBarbRightHarpoon :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
upBarbLeftDownBarbRightHarpoon = Tag_Entity "&#10573;"

-- | The left barb up right barb up harpoon HTML entity ('⥎').
leftBarbUpRightBarbUpHarpoon :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
leftBarbUpRightBarbUpHarpoon = Tag_Entity "&#10574;"

-- | The up barb right down barb right harpoon HTML entity ('⥏').
upBarbRightDownBarbRightHarpoon :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
upBarbRightDownBarbRightHarpoon = Tag_Entity "&#10575;"

-- | The left barb down right barb down harpoon HTML entity ('⥐').
leftBarbDownRightBarbDownHarpoon :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
leftBarbDownRightBarbDownHarpoon = Tag_Entity "&#10576;"

-- | The up barb left down barb left harpoon HTML entity ('⥑').
upBarbLeftDownBarbLeftHarpoon :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
upBarbLeftDownBarbLeftHarpoon = Tag_Entity "&#10577;"

-- | The leftwards harpoon with barb up to bar HTML entity ('⥒').
leftwardsHarpoonWithBarbUpToBar :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpToBar = Tag_Entity "&#10578;"

-- | The rightwards harpoon with barb up to bar HTML entity ('⥓').
rightwardsHarpoonWithBarbUpToBar :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpToBar = Tag_Entity "&#10579;"

-- | The upwards harpoon with barb right to bar HTML entity ('⥔').
upwardsHarpoonWithBarbRightToBar :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
upwardsHarpoonWithBarbRightToBar = Tag_Entity "&#10580;"

-- | The downwards harpoon with barb right to bar HTML entity ('⥕').
downwardsHarpoonWithBarbRightToBar :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
downwardsHarpoonWithBarbRightToBar = Tag_Entity "&#10581;"

-- | The leftwards harpoon with barb down to bar HTML entity ('⥖').
leftwardsHarpoonWithBarbDownToBar :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
leftwardsHarpoonWithBarbDownToBar = Tag_Entity "&#10582;"

-- | The rightwards harpoon with barb down to bar HTML entity ('⥗').
rightwardsHarpoonWithBarbDownToBar :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightwardsHarpoonWithBarbDownToBar = Tag_Entity "&#10583;"

-- | The upwards harpoon with barb left to bar HTML entity ('⥘').
upwardsHarpoonWithBarbLeftToBar :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
upwardsHarpoonWithBarbLeftToBar = Tag_Entity "&#10584;"

-- | The downwards harpoon with barb left to bar HTML entity ('⥙').
downwardsHarpoonWithBarbLeftToBar :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
downwardsHarpoonWithBarbLeftToBar = Tag_Entity "&#10585;"

-- | The leftwards harpoon with barb up from bar HTML entity ('⥚').
leftwardsHarpoonWithBarbUpFromBar :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpFromBar = Tag_Entity "&#10586;"

-- | The rightwards harpoon with barb up from bar HTML entity ('⥛').
rightwardsHarpoonWithBarbUpFromBar :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpFromBar = Tag_Entity "&#10587;"

-- | The upwards harpoon with barb right from bar HTML entity ('⥜').
upwardsHarpoonWithBarbRightFromBar :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
upwardsHarpoonWithBarbRightFromBar = Tag_Entity "&#10588;"

-- | The downwards harpoon with barb right from bar HTML entity ('⥝').
downwardsHarpoonWithBarbRightFromBar :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
downwardsHarpoonWithBarbRightFromBar = Tag_Entity "&#10589;"

-- | The leftwards harpoon with barb down from bar HTML entity ('⥞').
leftwardsHarpoonWithBarbDownFromBar :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
leftwardsHarpoonWithBarbDownFromBar = Tag_Entity "&#10590;"

-- | The rightwards harpoon with barb down from bar HTML entity ('⥟').
rightwardsHarpoonWithBarbDownFromBar :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
rightwardsHarpoonWithBarbDownFromBar = Tag_Entity "&#10591;"

-- | The upwards harpoon with barb left from bar HTML entity ('⥠').
upwardsHarpoonWithBarbLeftFromBar :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
upwardsHarpoonWithBarbLeftFromBar = Tag_Entity "&#10592;"

-- | The downwards harpoon with barb left from bar HTML entity ('⥡').
downwardsHarpoonWithBarbLeftFromBar :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
downwardsHarpoonWithBarbLeftFromBar = Tag_Entity "&#10593;"

-- | The leftwards harpoon with barb up above leftwards harpoon with barb down HTML entity ('⥢').
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown :: ValidChild Text parent grandparent
                                                            => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown = Tag_Entity "&#10594;"

-- | The upwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥣').
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: ValidChild Text parent grandparent
                                                            => ChildHTML parent grandparent
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = Tag_Entity "&#10595;"

-- | The rightwards harpoon with barb up above rightwards harpoon with barb down HTML entity ('⥤').
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown :: ValidChild Text parent grandparent
                                                              => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown = Tag_Entity "&#10596;"

-- | The downwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥥').
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: ValidChild Text parent grandparent
                                                                => ChildHTML parent grandparent
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = Tag_Entity "&#10597;"

-- | The leftwards harpoon with barb up above rightwards harpoon with barb up HTML entity ('⥦').
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp :: ValidChild Text parent grandparent
                                                           => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp = Tag_Entity "&#10598;"

-- | The leftwards harpoon with barb down above rightwards harpoon with barb down HTML entity ('⥧').
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown :: ValidChild Text parent grandparent
                                                               => ChildHTML parent grandparent
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown = Tag_Entity "&#10599;"

-- | The rightwards harpoon with barb up above leftwards harpoon with barb up HTML entity ('⥨').
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp :: ValidChild Text parent grandparent
                                                           => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp = Tag_Entity "&#10600;"

-- | The rightwards harpoon with barb down above leftwards harpoon with barb down HTML entity ('⥩').
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown :: ValidChild Text parent grandparent
                                                               => ChildHTML parent grandparent
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown = Tag_Entity "&#10601;"

-- | The leftwards harpoon with barb up above long dash HTML entity ('⥪').
leftwardsHarpoonWithBarbUpAboveLongDash :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
leftwardsHarpoonWithBarbUpAboveLongDash = Tag_Entity "&#10602;"

-- | The leftwards harpoon with barb down below long dash HTML entity ('⥫').
leftwardsHarpoonWithBarbDownBelowLongDash :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
leftwardsHarpoonWithBarbDownBelowLongDash = Tag_Entity "&#10603;"

-- | The rightwards harpoon with barb up above long dash HTML entity ('⥬').
rightwardsHarpoonWithBarbUpAboveLongDash :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
rightwardsHarpoonWithBarbUpAboveLongDash = Tag_Entity "&#10604;"

-- | The rightwards harpoon with barb down below long dash HTML entity ('⥭').
rightwardsHarpoonWithBarbDownBelowLongDash :: ValidChild Text parent grandparent
                                           => ChildHTML parent grandparent
rightwardsHarpoonWithBarbDownBelowLongDash = Tag_Entity "&#10605;"

-- | The upwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥮').
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: ValidChild Text parent grandparent
                                                              => ChildHTML parent grandparent
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = Tag_Entity "&#10606;"

-- | The downwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥯').
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: ValidChild Text parent grandparent
                                                              => ChildHTML parent grandparent
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = Tag_Entity "&#10607;"

-- | The right double arrow with rounded head HTML entity ('⥰').
rightDoubleArrowWithRoundedHead :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
rightDoubleArrowWithRoundedHead = Tag_Entity "&#10608;"

-- | The equals sign above rightwards arrow HTML entity ('⥱').
equalsSignAboveRightwardsArrow :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
equalsSignAboveRightwardsArrow = Tag_Entity "&#10609;"

-- | The tilde operator above rightwards arrow HTML entity ('⥲').
tildeOperatorAboveRightwardsArrow :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
tildeOperatorAboveRightwardsArrow = Tag_Entity "&#10610;"

-- | The leftwards arrow above tilde operator HTML entity ('⥳').
leftwardsArrowAboveTildeOperator :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
leftwardsArrowAboveTildeOperator = Tag_Entity "&#10611;"

-- | The rightwards arrow above tilde operator HTML entity ('⥴').
rightwardsArrowAboveTildeOperator :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
rightwardsArrowAboveTildeOperator = Tag_Entity "&#10612;"

-- | The rightwards arrow above almost equal to HTML entity ('⥵').
rightwardsArrowAboveAlmostEqualTo :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
rightwardsArrowAboveAlmostEqualTo = Tag_Entity "&#10613;"

-- | The less-than above leftwards arrow HTML entity ('⥶').
lessThanAboveLeftwardsArrow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
lessThanAboveLeftwardsArrow = Tag_Entity "&#10614;"

-- | The Leftwards Arrow Through Less-than HTML entity ('⥷').
leftwardsArrowThroughLessThan :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
leftwardsArrowThroughLessThan = Tag_Entity "&#10615;"

-- | The greater-than above rightwards arrow HTML entity ('⥸').
greaterThanAboveRightwardsArrow :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
greaterThanAboveRightwardsArrow = Tag_Entity "&#10616;"

-- | The subset above rightwards arrow HTML entity ('⥹').
subsetAboveRightwardsArrow :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
subsetAboveRightwardsArrow = Tag_Entity "&#10617;"

-- | The superset above leftwards arrow HTML entity ('⥻').
supersetAboveLeftwardsArrow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
supersetAboveLeftwardsArrow = Tag_Entity "&#10619;"

-- | The left fish tail HTML entity ('⥼').
leftFishTail :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
leftFishTail = Tag_Entity "&#10620;"

-- | The right fish tail HTML entity ('⥽').
rightFishTail :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
rightFishTail = Tag_Entity "&#10621;"

-- | The up fish tail HTML entity ('⥾').
upFishTail :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
upFishTail = Tag_Entity "&#10622;"

-- | The down fish tail HTML entity ('⥿').
downFishTail :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
downFishTail = Tag_Entity "&#10623;"

-- | The left white parenthesis HTML entity ('⦅').
leftWhiteParenthesis :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
leftWhiteParenthesis = Tag_Entity "&#10629;"

-- | The right white parenthesis HTML entity ('⦆').
rightWhiteParenthesis :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
rightWhiteParenthesis = Tag_Entity "&#10630;"

-- | The left square bracket with underbar HTML entity ('⦋').
leftSquareBracketWithUnderbar :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
leftSquareBracketWithUnderbar = Tag_Entity "&#10635;"

-- | The right square bracket with underbar HTML entity ('⦌').
rightSquareBracketWithUnderbar :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
rightSquareBracketWithUnderbar = Tag_Entity "&#10636;"

-- | The left square bracket with tick in top corner HTML entity ('⦍').
leftSquareBracketWithTickInTopCorner :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
leftSquareBracketWithTickInTopCorner = Tag_Entity "&#10637;"

-- | The right square bracket with tick in bottom corner HTML entity ('⦎').
rightSquareBracketWithTickInBottomCorner :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
rightSquareBracketWithTickInBottomCorner = Tag_Entity "&#10638;"

-- | The left square bracket with tick in bottom corner HTML entity ('⦏').
leftSquareBracketWithTickInBottomCorner :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
leftSquareBracketWithTickInBottomCorner = Tag_Entity "&#10639;"

-- | The right square bracket with tick in top corner HTML entity ('⦐').
rightSquareBracketWithTickInTopCorner :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
rightSquareBracketWithTickInTopCorner = Tag_Entity "&#10640;"

-- | The left angle bracket with dot HTML entity ('⦑').
leftAngleBracketWithDot :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
leftAngleBracketWithDot = Tag_Entity "&#10641;"

-- | The right angle bracket with dot HTML entity ('⦒').
rightAngleBracketWithDot :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
rightAngleBracketWithDot = Tag_Entity "&#10642;"

-- | The left arc less-than bracket HTML entity ('⦓').
leftArcLessThanBracket :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
leftArcLessThanBracket = Tag_Entity "&#10643;"

-- | The right arc greater-than bracket HTML entity ('⦔').
rightArcGreaterThanBracket :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
rightArcGreaterThanBracket = Tag_Entity "&#10644;"

-- | The double left arc greater-than bracket HTML entity ('⦕').
doubleLeftArcGreaterThanBracket :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
doubleLeftArcGreaterThanBracket = Tag_Entity "&#10645;"

-- | The double right arc less-than bracket HTML entity ('⦖').
doubleRightArcLessThanBracket :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
doubleRightArcLessThanBracket = Tag_Entity "&#10646;"

-- | The vertical zigzag line HTML entity ('⦚').
verticalZigzagLine :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
verticalZigzagLine = Tag_Entity "&#10650;"

-- | The Measured Angle Opening Left HTML entity ('⦛').
measuredAngleOpeningLeft :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
measuredAngleOpeningLeft = Tag_Entity "&#10651;"

-- | The right angle variant with square HTML entity ('⦜').
rightAngleVariantWithSquare :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
rightAngleVariantWithSquare = Tag_Entity "&#10652;"

-- | The measured right angle with dot HTML entity ('⦝').
measuredRightAngleWithDot :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
measuredRightAngleWithDot = Tag_Entity "&#10653;"

-- | The Angle With S Inside HTML entity ('⦞').
angleWithSInside :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
angleWithSInside = Tag_Entity "&#10654;"

-- | The Acute Angle HTML entity ('⦟').
acuteAngle :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
acuteAngle = Tag_Entity "&#10655;"

-- | The Spherical Angle Opening Left HTML entity ('⦠').
sphericalAngleOpeningLeft :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
sphericalAngleOpeningLeft = Tag_Entity "&#10656;"

-- | The Spherical Angle Opening Up HTML entity ('⦡').
sphericalAngleOpeningUp :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
sphericalAngleOpeningUp = Tag_Entity "&#10657;"

-- | The Turned Angle HTML entity ('⦢').
turnedAngle :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
turnedAngle = Tag_Entity "&#10658;"

-- | The Reversed Angle HTML entity ('⦣').
reversedAngle :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
reversedAngle = Tag_Entity "&#10659;"

-- | The angle with underbar HTML entity ('⦤').
angleWithUnderbar :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
angleWithUnderbar = Tag_Entity "&#10660;"

-- | The reversed angle with underbar HTML entity ('⦥').
reversedAngleWithUnderbar :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
reversedAngleWithUnderbar = Tag_Entity "&#10661;"

-- | The oblique angle opening up HTML entity ('⦦').
obliqueAngleOpeningUp :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
obliqueAngleOpeningUp = Tag_Entity "&#10662;"

-- | The oblique angle opening down HTML entity ('⦧').
obliqueAngleOpeningDown :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
obliqueAngleOpeningDown = Tag_Entity "&#10663;"

-- | The measured angle with open arm ending in arrow pointing up and right HTML entity ('⦨').
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight :: ValidChild Text parent grandparent
                                                        => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight = Tag_Entity "&#10664;"

-- | The measured angle with open arm ending in arrow pointing up and left HTML entity ('⦩').
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft :: ValidChild Text parent grandparent
                                                       => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft = Tag_Entity "&#10665;"

-- | The measured angle with open arm ending in arrow pointing down and right HTML entity ('⦪').
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight :: ValidChild Text parent grandparent
                                                          => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight = Tag_Entity "&#10666;"

-- | The measured angle with open arm ending in arrow pointing down and left HTML entity ('⦫').
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft :: ValidChild Text parent grandparent
                                                         => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft = Tag_Entity "&#10667;"

-- | The measured angle with open arm ending in arrow pointing right and up HTML entity ('⦬').
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp :: ValidChild Text parent grandparent
                                                        => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp = Tag_Entity "&#10668;"

-- | The measured angle with open arm ending in arrow pointing left and up HTML entity ('⦭').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp :: ValidChild Text parent grandparent
                                                       => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp = Tag_Entity "&#10669;"

-- | The measured angle with open arm ending in arrow pointing right and down HTML entity ('⦮').
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown :: ValidChild Text parent grandparent
                                                          => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown = Tag_Entity "&#10670;"

-- | The measured angle with open arm ending in arrow pointing left and down HTML entity ('⦯').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown :: ValidChild Text parent grandparent
                                                         => ChildHTML parent grandparent
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown = Tag_Entity "&#10671;"

-- | The reversed empty set HTML entity ('⦰').
reversedEmptySet :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
reversedEmptySet = Tag_Entity "&#10672;"

-- | The empty set with overbar HTML entity ('⦱').
emptySetWithOverbar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
emptySetWithOverbar = Tag_Entity "&#10673;"

-- | The empty set with small circle above HTML entity ('⦲').
emptySetWithSmallCircleAbove :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
emptySetWithSmallCircleAbove = Tag_Entity "&#10674;"

-- | The empty set with right arrow above HTML entity ('⦳').
emptySetWithRightArrowAbove :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
emptySetWithRightArrowAbove = Tag_Entity "&#10675;"

-- | The empty set with left arrow above HTML entity ('⦴').
emptySetWithLeftArrowAbove :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
emptySetWithLeftArrowAbove = Tag_Entity "&#10676;"

-- | The circle with horizontal bar HTML entity ('⦵').
circleWithHorizontalBar :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
circleWithHorizontalBar = Tag_Entity "&#10677;"

-- | The circled vertical bar HTML entity ('⦶').
circledVerticalBar :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
circledVerticalBar = Tag_Entity "&#10678;"

-- | The circled parallel HTML entity ('⦷').
circledParallel :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
circledParallel = Tag_Entity "&#10679;"

-- | The Circled Reverse Solidus HTML entity ('⦸').
circledReverseSolidus :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
circledReverseSolidus = Tag_Entity "&#10680;"

-- | The circled perpendicular HTML entity ('⦹').
circledPerpendicular :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
circledPerpendicular = Tag_Entity "&#10681;"

-- | The Circle Divided By Horizontal Bar And Top Half Divided By Vertical Bar HTML entity ('⦺').
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar :: ValidChild Text parent grandparent
                                                           => ChildHTML parent grandparent
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar = Tag_Entity "&#10682;"

-- | The circle with superimposed x HTML entity ('⦻').
circleWithSuperimposedX :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
circleWithSuperimposedX = Tag_Entity "&#10683;"

-- | The circled anticlockwise-rotated division sign HTML entity ('⦼').
circledAnticlockwiseRotatedDivisionSign :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
circledAnticlockwiseRotatedDivisionSign = Tag_Entity "&#10684;"

-- | The circled white bullet HTML entity ('⦾').
circledWhiteBullet :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
circledWhiteBullet = Tag_Entity "&#10686;"

-- | The circled bullet HTML entity ('⦿').
circledBullet :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
circledBullet = Tag_Entity "&#10687;"

-- | The circled less-than HTML entity ('⧀').
circledLessThan :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
circledLessThan = Tag_Entity "&#10688;"

-- | The circled greater-than HTML entity ('⧁').
circledGreaterThan :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
circledGreaterThan = Tag_Entity "&#10689;"

-- | The circle with small circle to the right HTML entity ('⧂').
circleWithSmallCircleToTheRight :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
circleWithSmallCircleToTheRight = Tag_Entity "&#10690;"

-- | The circle with two horizontal strokes to the right HTML entity ('⧃').
circleWithTwoHorizontalStrokesToTheRight :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
circleWithTwoHorizontalStrokesToTheRight = Tag_Entity "&#10691;"

-- | The squared rising diagonal slash HTML entity ('⧄').
squaredRisingDiagonalSlash :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
squaredRisingDiagonalSlash = Tag_Entity "&#10692;"

-- | The squared falling diagonal slash HTML entity ('⧅').
squaredFallingDiagonalSlash :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
squaredFallingDiagonalSlash = Tag_Entity "&#10693;"

-- | The squared asterisk HTML entity ('⧆').
squaredAsterisk :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
squaredAsterisk = Tag_Entity "&#10694;"

-- | The squared small circle HTML entity ('⧇').
squaredSmallCircle :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
squaredSmallCircle = Tag_Entity "&#10695;"

-- | The squared square HTML entity ('⧈').
squaredSquare :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
squaredSquare = Tag_Entity "&#10696;"

-- | The two joined squares HTML entity ('⧉').
twoJoinedSquares :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
twoJoinedSquares = Tag_Entity "&#10697;"

-- | The Triangle With Dot Above HTML entity ('⧊').
triangleWithDotAbove :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
triangleWithDotAbove = Tag_Entity "&#10698;"

-- | The Triangle With Underbar HTML entity ('⧋').
triangleWithUnderbar :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
triangleWithUnderbar = Tag_Entity "&#10699;"

-- | The S In Triangle HTML entity ('⧌').
sInTriangle :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
sInTriangle = Tag_Entity "&#10700;"

-- | The triangle with serifs at bottom HTML entity ('⧍').
triangleWithSerifsAtBottom :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
triangleWithSerifsAtBottom = Tag_Entity "&#10701;"

-- | The right triangle above left triangle HTML entity ('⧎').
rightTriangleAboveLeftTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
rightTriangleAboveLeftTriangle = Tag_Entity "&#10702;"

-- | The left triangle beside vertical bar HTML entity ('⧏').
leftTriangleBesideVerticalBar :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
leftTriangleBesideVerticalBar = Tag_Entity "&#10703;"

-- | The vertical bar beside right triangle HTML entity ('⧐').
verticalBarBesideRightTriangle :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
verticalBarBesideRightTriangle = Tag_Entity "&#10704;"

-- | The left double wiggly fence HTML entity ('⧚').
leftDoubleWigglyFence :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
leftDoubleWigglyFence = Tag_Entity "&#10714;"

-- | The incomplete infinity HTML entity ('⧜').
incompleteInfinity :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
incompleteInfinity = Tag_Entity "&#10716;"

-- | The tie over infinity HTML entity ('⧝').
tieOverInfinity :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
tieOverInfinity = Tag_Entity "&#10717;"

-- | The infinity negated with vertical bar HTML entity ('⧞').
infinityNegatedWithVerticalBar :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
infinityNegatedWithVerticalBar = Tag_Entity "&#10718;"

-- | The square with contoured outline HTML entity ('⧠').
squareWithContouredOutline :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
squareWithContouredOutline = Tag_Entity "&#10720;"

-- | The equals sign and slanted parallel HTML entity ('⧣').
equalsSignAndSlantedParallel :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
equalsSignAndSlantedParallel = Tag_Entity "&#10723;"

-- | The equals sign and slanted parallel with tilde above HTML entity ('⧤').
equalsSignAndSlantedParallelWithTildeAbove :: ValidChild Text parent grandparent
                                           => ChildHTML parent grandparent
equalsSignAndSlantedParallelWithTildeAbove = Tag_Entity "&#10724;"

-- | The identical to and slanted parallel HTML entity ('⧥').
identicalToAndSlantedParallel :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
identicalToAndSlantedParallel = Tag_Entity "&#10725;"

-- | The Down-pointing Triangle With Left Half Black HTML entity ('⧨').
downPointingTriangleWithLeftHalfBlack :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
downPointingTriangleWithLeftHalfBlack = Tag_Entity "&#10728;"

-- | The Down-pointing Triangle With Right Half Black HTML entity ('⧩').
downPointingTriangleWithRightHalfBlack :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
downPointingTriangleWithRightHalfBlack = Tag_Entity "&#10729;"

-- | The black lozenge HTML entity ('⧫').
blackLozenge :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
blackLozenge = Tag_Entity "&#10731;"

-- | The error-barred white square HTML entity ('⧮').
errorBarredWhiteSquare :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
errorBarredWhiteSquare = Tag_Entity "&#10734;"

-- | The error-barred black square HTML entity ('⧯').
errorBarredBlackSquare :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
errorBarredBlackSquare = Tag_Entity "&#10735;"

-- | The Error-barred White Diamond HTML entity ('⧰').
errorBarredWhiteDiamond :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
errorBarredWhiteDiamond = Tag_Entity "&#10736;"

-- | The Error-barred Black Diamond HTML entity ('⧱').
errorBarredBlackDiamond :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
errorBarredBlackDiamond = Tag_Entity "&#10737;"

-- | The Error-barred White Circle HTML entity ('⧲').
errorBarredWhiteCircle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
errorBarredWhiteCircle = Tag_Entity "&#10738;"

-- | The Error-barred Black Circle HTML entity ('⧳').
errorBarredBlackCircle :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
errorBarredBlackCircle = Tag_Entity "&#10739;"

-- | The rule-delayed HTML entity ('⧴').
ruleDelayed :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
ruleDelayed = Tag_Entity "&#10740;"

-- | The solidus with overbar HTML entity ('⧶').
solidusWithOverbar :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
solidusWithOverbar = Tag_Entity "&#10742;"

-- | The Double Plus HTML entity ('⧺').
doublePlus :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
doublePlus = Tag_Entity "&#10746;"

-- | The Triple Plus HTML entity ('⧻').
triplePlus :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
triplePlus = Tag_Entity "&#10747;"

-- | The n-ary circled dot operator HTML entity ('⨀').
nAryCircledDotOperator :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
nAryCircledDotOperator = Tag_Entity "&#10752;"

-- | The n-ary circled plus operator HTML entity ('⨁').
nAryCircledPlusOperator :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
nAryCircledPlusOperator = Tag_Entity "&#10753;"

-- | The n-ary circled times operator HTML entity ('⨂').
nAryCircledTimesOperator :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
nAryCircledTimesOperator = Tag_Entity "&#10754;"

-- | The n-ary union operator with plus HTML entity ('⨄').
nAryUnionOperatorWithPlus :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
nAryUnionOperatorWithPlus = Tag_Entity "&#10756;"

-- | The N-ary Square Intersection Operator HTML entity ('⨅').
nArySquareIntersectionOperator :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
nArySquareIntersectionOperator = Tag_Entity "&#10757;"

-- | The n-ary square union operator HTML entity ('⨆').
nArySquareUnionOperator :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
nArySquareUnionOperator = Tag_Entity "&#10758;"

-- | The Summation With Integral HTML entity ('⨋').
summationWithIntegral :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
summationWithIntegral = Tag_Entity "&#10763;"

-- | The quadruple integral operator HTML entity ('⨌').
quadrupleIntegralOperator :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
quadrupleIntegralOperator = Tag_Entity "&#10764;"

-- | The finite part integral HTML entity ('⨍').
finitePartIntegral :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
finitePartIntegral = Tag_Entity "&#10765;"

-- | The Integral With Double Stroke HTML entity ('⨎').
integralWithDoubleStroke :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
integralWithDoubleStroke = Tag_Entity "&#10766;"

-- | The Integral Average With Slash HTML entity ('⨏').
integralAverageWithSlash :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
integralAverageWithSlash = Tag_Entity "&#10767;"

-- | The circulation function HTML entity ('⨐').
circulationFunction :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
circulationFunction = Tag_Entity "&#10768;"

-- | The anticlockwise integration HTML entity ('⨑').
anticlockwiseIntegration :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
anticlockwiseIntegration = Tag_Entity "&#10769;"

-- | The line integration with rectangular path around pole HTML entity ('⨒').
lineIntegrationWithRectangularPathAroundPole :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
lineIntegrationWithRectangularPathAroundPole = Tag_Entity "&#10770;"

-- | The line integration with semicircular path around pole HTML entity ('⨓').
lineIntegrationWithSemicircularPathAroundPole :: ValidChild Text parent grandparent
                                              => ChildHTML parent grandparent
lineIntegrationWithSemicircularPathAroundPole = Tag_Entity "&#10771;"

-- | The line integration not including the pole HTML entity ('⨔').
lineIntegrationNotIncludingThePole :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
lineIntegrationNotIncludingThePole = Tag_Entity "&#10772;"

-- | The integral around a point operator HTML entity ('⨕').
integralAroundAPointOperator :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
integralAroundAPointOperator = Tag_Entity "&#10773;"

-- | The quaternion integral operator HTML entity ('⨖').
quaternionIntegralOperator :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
quaternionIntegralOperator = Tag_Entity "&#10774;"

-- | The integral with leftwards arrow with hook HTML entity ('⨗').
integralWithLeftwardsArrowWithHook :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
integralWithLeftwardsArrowWithHook = Tag_Entity "&#10775;"

-- | The Integral With Times Sign HTML entity ('⨘').
integralWithTimesSign :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
integralWithTimesSign = Tag_Entity "&#10776;"

-- | The Integral With Intersection HTML entity ('⨙').
integralWithIntersection :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
integralWithIntersection = Tag_Entity "&#10777;"

-- | The Integral With Union HTML entity ('⨚').
integralWithUnion :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
integralWithUnion = Tag_Entity "&#10778;"

-- | The Integral With Overbar HTML entity ('⨛').
integralWithOverbar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
integralWithOverbar = Tag_Entity "&#10779;"

-- | The Integral With Underbar HTML entity ('⨜').
integralWithUnderbar :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
integralWithUnderbar = Tag_Entity "&#10780;"

-- | The Large Left Triangle Operator HTML entity ('⨞').
largeLeftTriangleOperator :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
largeLeftTriangleOperator = Tag_Entity "&#10782;"

-- | The plus sign with small circle above HTML entity ('⨢').
plusSignWithSmallCircleAbove :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
plusSignWithSmallCircleAbove = Tag_Entity "&#10786;"

-- | The plus sign with circumflex accent above HTML entity ('⨣').
plusSignWithCircumflexAccentAbove :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
plusSignWithCircumflexAccentAbove = Tag_Entity "&#10787;"

-- | The plus sign with tilde above HTML entity ('⨤').
plusSignWithTildeAbove :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
plusSignWithTildeAbove = Tag_Entity "&#10788;"

-- | The plus sign with dot below HTML entity ('⨥').
plusSignWithDotBelow :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
plusSignWithDotBelow = Tag_Entity "&#10789;"

-- | The plus sign with tilde below HTML entity ('⨦').
plusSignWithTildeBelow :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
plusSignWithTildeBelow = Tag_Entity "&#10790;"

-- | The plus sign with subscript two HTML entity ('⨧').
plusSignWithSubscriptTwo :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
plusSignWithSubscriptTwo = Tag_Entity "&#10791;"

-- | The Plus Sign With Black Triangle HTML entity ('⨨').
plusSignWithBlackTriangle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
plusSignWithBlackTriangle = Tag_Entity "&#10792;"

-- | The minus sign with comma above HTML entity ('⨩').
minusSignWithCommaAbove :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
minusSignWithCommaAbove = Tag_Entity "&#10793;"

-- | The minus sign with dot below HTML entity ('⨪').
minusSignWithDotBelow :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
minusSignWithDotBelow = Tag_Entity "&#10794;"

-- | The Minus Sign With Falling Dots HTML entity ('⨫').
minusSignWithFallingDots :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
minusSignWithFallingDots = Tag_Entity "&#10795;"

-- | The Minus Sign With Rising Dots HTML entity ('⨬').
minusSignWithRisingDots :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
minusSignWithRisingDots = Tag_Entity "&#10796;"

-- | The plus sign in left half circle HTML entity ('⨭').
plusSignInLeftHalfCircle :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
plusSignInLeftHalfCircle = Tag_Entity "&#10797;"

-- | The plus sign in right half circle HTML entity ('⨮').
plusSignInRightHalfCircle :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
plusSignInRightHalfCircle = Tag_Entity "&#10798;"

-- | The vector or cross product HTML entity ('⨯').
vectorOrCrossProduct :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
vectorOrCrossProduct = Tag_Entity "&#10799;"

-- | The multiplication sign with dot above HTML entity ('⨰').
multiplicationSignWithDotAbove :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
multiplicationSignWithDotAbove = Tag_Entity "&#10800;"

-- | The multiplication sign with underbar HTML entity ('⨱').
multiplicationSignWithUnderbar :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
multiplicationSignWithUnderbar = Tag_Entity "&#10801;"

-- | The smash product HTML entity ('⨳').
smashProduct :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
smashProduct = Tag_Entity "&#10803;"

-- | The multiplication sign in left half circle HTML entity ('⨴').
multiplicationSignInLeftHalfCircle :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
multiplicationSignInLeftHalfCircle = Tag_Entity "&#10804;"

-- | The multiplication sign in right half circle HTML entity ('⨵').
multiplicationSignInRightHalfCircle :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
multiplicationSignInRightHalfCircle = Tag_Entity "&#10805;"

-- | The circled multiplication sign with circumflex accent HTML entity ('⨶').
circledMultiplicationSignWithCircumflexAccent :: ValidChild Text parent grandparent
                                              => ChildHTML parent grandparent
circledMultiplicationSignWithCircumflexAccent = Tag_Entity "&#10806;"

-- | The multiplication sign in double circle HTML entity ('⨷').
multiplicationSignInDoubleCircle :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
multiplicationSignInDoubleCircle = Tag_Entity "&#10807;"

-- | The circled division sign HTML entity ('⨸').
circledDivisionSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
circledDivisionSign = Tag_Entity "&#10808;"

-- | The plus sign in triangle HTML entity ('⨹').
plusSignInTriangle :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
plusSignInTriangle = Tag_Entity "&#10809;"

-- | The minus sign in triangle HTML entity ('⨺').
minusSignInTriangle :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
minusSignInTriangle = Tag_Entity "&#10810;"

-- | The multiplication sign in triangle HTML entity ('⨻').
multiplicationSignInTriangle :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
multiplicationSignInTriangle = Tag_Entity "&#10811;"

-- | The interior product HTML entity ('⨼').
interiorProduct :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
interiorProduct = Tag_Entity "&#10812;"

-- | The amalgamation or coproduct HTML entity ('⨿').
amalgamationOrCoproduct :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
amalgamationOrCoproduct = Tag_Entity "&#10815;"

-- | The intersection with dot HTML entity ('⩀').
intersectionWithDot :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
intersectionWithDot = Tag_Entity "&#10816;"

-- | The Union With Minus Sign HTML entity ('⩁').
unionWithMinusSign :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
unionWithMinusSign = Tag_Entity "&#10817;"

-- | The union with overbar HTML entity ('⩂').
unionWithOverbar :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
unionWithOverbar = Tag_Entity "&#10818;"

-- | The intersection with overbar HTML entity ('⩃').
intersectionWithOverbar :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
intersectionWithOverbar = Tag_Entity "&#10819;"

-- | The intersection with logical and HTML entity ('⩄').
intersectionWithLogicalAnd :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
intersectionWithLogicalAnd = Tag_Entity "&#10820;"

-- | The union with logical or HTML entity ('⩅').
unionWithLogicalOr :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
unionWithLogicalOr = Tag_Entity "&#10821;"

-- | The union above intersection HTML entity ('⩆').
unionAboveIntersection :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
unionAboveIntersection = Tag_Entity "&#10822;"

-- | The intersection above union HTML entity ('⩇').
intersectionAboveUnion :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
intersectionAboveUnion = Tag_Entity "&#10823;"

-- | The union above bar above intersection HTML entity ('⩈').
unionAboveBarAboveIntersection :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
unionAboveBarAboveIntersection = Tag_Entity "&#10824;"

-- | The intersection above bar above union HTML entity ('⩉').
intersectionAboveBarAboveUnion :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
intersectionAboveBarAboveUnion = Tag_Entity "&#10825;"

-- | The union beside and joined with union HTML entity ('⩊').
unionBesideAndJoinedWithUnion :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
unionBesideAndJoinedWithUnion = Tag_Entity "&#10826;"

-- | The intersection beside and joined with intersection HTML entity ('⩋').
intersectionBesideAndJoinedWithIntersection :: ValidChild Text parent grandparent
                                            => ChildHTML parent grandparent
intersectionBesideAndJoinedWithIntersection = Tag_Entity "&#10827;"

-- | The closed union with serifs HTML entity ('⩌').
closedUnionWithSerifs :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
closedUnionWithSerifs = Tag_Entity "&#10828;"

-- | The closed intersection with serifs HTML entity ('⩍').
closedIntersectionWithSerifs :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
closedIntersectionWithSerifs = Tag_Entity "&#10829;"

-- | The Double Square Intersection HTML entity ('⩎').
doubleSquareIntersection :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
doubleSquareIntersection = Tag_Entity "&#10830;"

-- | The closed union with serifs and smash product HTML entity ('⩐').
closedUnionWithSerifsAndSmashProduct :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
closedUnionWithSerifsAndSmashProduct = Tag_Entity "&#10832;"

-- | The double logical and HTML entity ('⩓').
doubleLogicalAnd :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
doubleLogicalAnd = Tag_Entity "&#10835;"

-- | The double logical or HTML entity ('⩔').
doubleLogicalOr :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
doubleLogicalOr = Tag_Entity "&#10836;"

-- | The two intersecting logical and HTML entity ('⩕').
twoIntersectingLogicalAnd :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
twoIntersectingLogicalAnd = Tag_Entity "&#10837;"

-- | The two intersecting logical or HTML entity ('⩖').
twoIntersectingLogicalOr :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
twoIntersectingLogicalOr = Tag_Entity "&#10838;"

-- | The sloping large or HTML entity ('⩗').
slopingLargeOr :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
slopingLargeOr = Tag_Entity "&#10839;"

-- | The sloping large and HTML entity ('⩘').
slopingLargeAnd :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
slopingLargeAnd = Tag_Entity "&#10840;"

-- | The logical and with middle stem HTML entity ('⩚').
logicalAndWithMiddleStem :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
logicalAndWithMiddleStem = Tag_Entity "&#10842;"

-- | The logical or with middle stem HTML entity ('⩛').
logicalOrWithMiddleStem :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
logicalOrWithMiddleStem = Tag_Entity "&#10843;"

-- | The logical and with horizontal dash HTML entity ('⩜').
logicalAndWithHorizontalDash :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
logicalAndWithHorizontalDash = Tag_Entity "&#10844;"

-- | The logical or with horizontal dash HTML entity ('⩝').
logicalOrWithHorizontalDash :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
logicalOrWithHorizontalDash = Tag_Entity "&#10845;"

-- | The logical and with underbar HTML entity ('⩟').
logicalAndWithUnderbar :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
logicalAndWithUnderbar = Tag_Entity "&#10847;"

-- | The equals sign with dot below HTML entity ('⩦').
equalsSignWithDotBelow :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
equalsSignWithDotBelow = Tag_Entity "&#10854;"

-- | The Identical With Dot Above HTML entity ('⩧').
identicalWithDotAbove :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
identicalWithDotAbove = Tag_Entity "&#10855;"

-- | The tilde operator with dot above HTML entity ('⩪').
tildeOperatorWithDotAbove :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
tildeOperatorWithDotAbove = Tag_Entity "&#10858;"

-- | The Similar Minus Similar HTML entity ('⩬').
similarMinusSimilar :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
similarMinusSimilar = Tag_Entity "&#10860;"

-- | The congruent with dot above HTML entity ('⩭').
congruentWithDotAbove :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
congruentWithDotAbove = Tag_Entity "&#10861;"

-- | The equals with asterisk HTML entity ('⩮').
equalsWithAsterisk :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
equalsWithAsterisk = Tag_Entity "&#10862;"

-- | The almost equal to with circumflex accent HTML entity ('⩯').
almostEqualToWithCircumflexAccent :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
almostEqualToWithCircumflexAccent = Tag_Entity "&#10863;"

-- | The approximately equal or equal to HTML entity ('⩰').
approximatelyEqualOrEqualTo :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
approximatelyEqualOrEqualTo = Tag_Entity "&#10864;"

-- | The equals sign above plus sign HTML entity ('⩱').
equalsSignAbovePlusSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
equalsSignAbovePlusSign = Tag_Entity "&#10865;"

-- | The plus sign above equals sign HTML entity ('⩲').
plusSignAboveEqualsSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
plusSignAboveEqualsSign = Tag_Entity "&#10866;"

-- | The equals sign above tilde operator HTML entity ('⩳').
equalsSignAboveTildeOperator :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
equalsSignAboveTildeOperator = Tag_Entity "&#10867;"

-- | The double colon equal HTML entity ('⩴').
doubleColonEqual :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
doubleColonEqual = Tag_Entity "&#10868;"

-- | The two consecutive equals signs HTML entity ('⩵').
twoConsecutiveEqualsSigns :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
twoConsecutiveEqualsSigns = Tag_Entity "&#10869;"

-- | The Three Consecutive Equals Signs HTML entity ('⩶').
threeConsecutiveEqualsSigns :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
threeConsecutiveEqualsSigns = Tag_Entity "&#10870;"

-- | The equals sign with two dots above and two dots below HTML entity ('⩷').
equalsSignWithTwoDotsAboveAndTwoDotsBelow :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
equalsSignWithTwoDotsAboveAndTwoDotsBelow = Tag_Entity "&#10871;"

-- | The equivalent with four dots above HTML entity ('⩸').
equivalentWithFourDotsAbove :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
equivalentWithFourDotsAbove = Tag_Entity "&#10872;"

-- | The less-than with circle inside HTML entity ('⩹').
lessThanWithCircleInside :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
lessThanWithCircleInside = Tag_Entity "&#10873;"

-- | The greater-than with circle inside HTML entity ('⩺').
greaterThanWithCircleInside :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
greaterThanWithCircleInside = Tag_Entity "&#10874;"

-- | The less-than with question mark above HTML entity ('⩻').
lessThanWithQuestionMarkAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
lessThanWithQuestionMarkAbove = Tag_Entity "&#10875;"

-- | The greater-than with question mark above HTML entity ('⩼').
greaterThanWithQuestionMarkAbove :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
greaterThanWithQuestionMarkAbove = Tag_Entity "&#10876;"

-- | The less-than or slanted equal to HTML entity ('⩽').
lessThanOrSlantedEqualTo :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
lessThanOrSlantedEqualTo = Tag_Entity "&#10877;"

-- | The greater-than or slanted equal to HTML entity ('⩾').
greaterThanOrSlantedEqualTo :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
greaterThanOrSlantedEqualTo = Tag_Entity "&#10878;"

-- | The less-than or slanted equal to with dot inside HTML entity ('⩿').
lessThanOrSlantedEqualToWithDotInside :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
lessThanOrSlantedEqualToWithDotInside = Tag_Entity "&#10879;"

-- | The greater-than or slanted equal to with dot inside HTML entity ('⪀').
greaterThanOrSlantedEqualToWithDotInside :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
greaterThanOrSlantedEqualToWithDotInside = Tag_Entity "&#10880;"

-- | The less-than or slanted equal to with dot above HTML entity ('⪁').
lessThanOrSlantedEqualToWithDotAbove :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
lessThanOrSlantedEqualToWithDotAbove = Tag_Entity "&#10881;"

-- | The greater-than or slanted equal to with dot above HTML entity ('⪂').
greaterThanOrSlantedEqualToWithDotAbove :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
greaterThanOrSlantedEqualToWithDotAbove = Tag_Entity "&#10882;"

-- | The less-than or slanted equal to with dot above right HTML entity ('⪃').
lessThanOrSlantedEqualToWithDotAboveRight :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
lessThanOrSlantedEqualToWithDotAboveRight = Tag_Entity "&#10883;"

-- | The greater-than or slanted equal to with dot above left HTML entity ('⪄').
greaterThanOrSlantedEqualToWithDotAboveLeft :: ValidChild Text parent grandparent
                                            => ChildHTML parent grandparent
greaterThanOrSlantedEqualToWithDotAboveLeft = Tag_Entity "&#10884;"

-- | The less-than or approximate HTML entity ('⪅').
lessThanOrApproximate :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
lessThanOrApproximate = Tag_Entity "&#10885;"

-- | The greater-than or approximate HTML entity ('⪆').
greaterThanOrApproximate :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
greaterThanOrApproximate = Tag_Entity "&#10886;"

-- | The less-than and single-line not equal to HTML entity ('⪇').
lessThanAndSingleLineNotEqualTo :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
lessThanAndSingleLineNotEqualTo = Tag_Entity "&#10887;"

-- | The greater-than and single-line not equal to HTML entity ('⪈').
greaterThanAndSingleLineNotEqualTo :: ValidChild Text parent grandparent
                                   => ChildHTML parent grandparent
greaterThanAndSingleLineNotEqualTo = Tag_Entity "&#10888;"

-- | The less-than and not approximate HTML entity ('⪉').
lessThanAndNotApproximate :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
lessThanAndNotApproximate = Tag_Entity "&#10889;"

-- | The greater-than and not approximate HTML entity ('⪊').
greaterThanAndNotApproximate :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
greaterThanAndNotApproximate = Tag_Entity "&#10890;"

-- | The less-than above double-line equal above greater-than HTML entity ('⪋').
lessThanAboveDoubleLineEqualAboveGreaterThan :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
lessThanAboveDoubleLineEqualAboveGreaterThan = Tag_Entity "&#10891;"

-- | The greater-than above double-line equal above less-than HTML entity ('⪌').
greaterThanAboveDoubleLineEqualAboveLessThan :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
greaterThanAboveDoubleLineEqualAboveLessThan = Tag_Entity "&#10892;"

-- | The less-than above similar or equal HTML entity ('⪍').
lessThanAboveSimilarOrEqual :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
lessThanAboveSimilarOrEqual = Tag_Entity "&#10893;"

-- | The greater-than above similar or equal HTML entity ('⪎').
greaterThanAboveSimilarOrEqual :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
greaterThanAboveSimilarOrEqual = Tag_Entity "&#10894;"

-- | The less-than above similar above greater-than HTML entity ('⪏').
lessThanAboveSimilarAboveGreaterThan :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
lessThanAboveSimilarAboveGreaterThan = Tag_Entity "&#10895;"

-- | The greater-than above similar above less-than HTML entity ('⪐').
greaterThanAboveSimilarAboveLessThan :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
greaterThanAboveSimilarAboveLessThan = Tag_Entity "&#10896;"

-- | The less-than above greater-than above double-line equal HTML entity ('⪑').
lessThanAboveGreaterThanAboveDoubleLineEqual :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
lessThanAboveGreaterThanAboveDoubleLineEqual = Tag_Entity "&#10897;"

-- | The greater-than above less-than above double-line equal HTML entity ('⪒').
greaterThanAboveLessThanAboveDoubleLineEqual :: ValidChild Text parent grandparent
                                             => ChildHTML parent grandparent
greaterThanAboveLessThanAboveDoubleLineEqual = Tag_Entity "&#10898;"

-- | The less-than above slanted equal above greater-than above slanted equal HTML entity ('⪓').
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual :: ValidChild Text parent grandparent
                                                           => ChildHTML parent grandparent
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual = Tag_Entity "&#10899;"

-- | The greater-than above slanted equal above less-than above slanted equal HTML entity ('⪔').
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual :: ValidChild Text parent grandparent
                                                           => ChildHTML parent grandparent
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual = Tag_Entity "&#10900;"

-- | The slanted equal to or less-than HTML entity ('⪕').
slantedEqualToOrLessThan :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
slantedEqualToOrLessThan = Tag_Entity "&#10901;"

-- | The slanted equal to or greater-than HTML entity ('⪖').
slantedEqualToOrGreaterThan :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
slantedEqualToOrGreaterThan = Tag_Entity "&#10902;"

-- | The slanted equal to or less-than with dot inside HTML entity ('⪗').
slantedEqualToOrLessThanWithDotInside :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
slantedEqualToOrLessThanWithDotInside = Tag_Entity "&#10903;"

-- | The slanted equal to or greater-than with dot inside HTML entity ('⪘').
slantedEqualToOrGreaterThanWithDotInside :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
slantedEqualToOrGreaterThanWithDotInside = Tag_Entity "&#10904;"

-- | The double-line equal to or less-than HTML entity ('⪙').
doubleLineEqualToOrLessThan :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
doubleLineEqualToOrLessThan = Tag_Entity "&#10905;"

-- | The double-line equal to or greater-than HTML entity ('⪚').
doubleLineEqualToOrGreaterThan :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
doubleLineEqualToOrGreaterThan = Tag_Entity "&#10906;"

-- | The Double-line Slanted Equal To Or Greater-than HTML entity ('⪜').
doubleLineSlantedEqualToOrGreaterThan :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
doubleLineSlantedEqualToOrGreaterThan = Tag_Entity "&#10908;"

-- | The similar or less-than HTML entity ('⪝').
similarOrLessThan :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
similarOrLessThan = Tag_Entity "&#10909;"

-- | The similar or greater-than HTML entity ('⪞').
similarOrGreaterThan :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
similarOrGreaterThan = Tag_Entity "&#10910;"

-- | The similar above less-than above equals sign HTML entity ('⪟').
similarAboveLessThanAboveEqualsSign :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
similarAboveLessThanAboveEqualsSign = Tag_Entity "&#10911;"

-- | The similar above greater-than above equals sign HTML entity ('⪠').
similarAboveGreaterThanAboveEqualsSign :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
similarAboveGreaterThanAboveEqualsSign = Tag_Entity "&#10912;"

-- | The double nested less-than HTML entity ('⪡').
doubleNestedLessThan :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
doubleNestedLessThan = Tag_Entity "&#10913;"

-- | The double nested greater-than HTML entity ('⪢').
doubleNestedGreaterThan :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
doubleNestedGreaterThan = Tag_Entity "&#10914;"

-- | The greater-than overlapping less-than HTML entity ('⪤').
greaterThanOverlappingLessThan :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
greaterThanOverlappingLessThan = Tag_Entity "&#10916;"

-- | The greater-than beside less-than HTML entity ('⪥').
greaterThanBesideLessThan :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
greaterThanBesideLessThan = Tag_Entity "&#10917;"

-- | The less-than closed by curve HTML entity ('⪦').
lessThanClosedByCurve :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
lessThanClosedByCurve = Tag_Entity "&#10918;"

-- | The greater-than closed by curve HTML entity ('⪧').
greaterThanClosedByCurve :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
greaterThanClosedByCurve = Tag_Entity "&#10919;"

-- | The less-than closed by curve above slanted equal HTML entity ('⪨').
lessThanClosedByCurveAboveSlantedEqual :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
lessThanClosedByCurveAboveSlantedEqual = Tag_Entity "&#10920;"

-- | The greater-than closed by curve above slanted equal HTML entity ('⪩').
greaterThanClosedByCurveAboveSlantedEqual :: ValidChild Text parent grandparent
                                          => ChildHTML parent grandparent
greaterThanClosedByCurveAboveSlantedEqual = Tag_Entity "&#10921;"

-- | The smaller than HTML entity ('⪪').
smallerThan :: ValidChild Text parent grandparent
            => ChildHTML parent grandparent
smallerThan = Tag_Entity "&#10922;"

-- | The larger than HTML entity ('⪫').
largerThan :: ValidChild Text parent grandparent
           => ChildHTML parent grandparent
largerThan = Tag_Entity "&#10923;"

-- | The smaller than or equal to HTML entity ('⪬').
smallerThanOrEqualTo :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
smallerThanOrEqualTo = Tag_Entity "&#10924;"

-- | The larger than or equal to HTML entity ('⪭').
largerThanOrEqualTo :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
largerThanOrEqualTo = Tag_Entity "&#10925;"

-- | The equals sign with bumpy above HTML entity ('⪮').
equalsSignWithBumpyAbove :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
equalsSignWithBumpyAbove = Tag_Entity "&#10926;"

-- | The precedes above single-line equals sign HTML entity ('⪯').
precedesAboveSingleLineEqualsSign :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
precedesAboveSingleLineEqualsSign = Tag_Entity "&#10927;"

-- | The succeeds above single-line equals sign HTML entity ('⪰').
succeedsAboveSingleLineEqualsSign :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
succeedsAboveSingleLineEqualsSign = Tag_Entity "&#10928;"

-- | The Precedes Above Single-line Not Equal To HTML entity ('⪱').
precedesAboveSingleLineNotEqualTo :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
precedesAboveSingleLineNotEqualTo = Tag_Entity "&#10929;"

-- | The Succeeds Above Single-line Not Equal To HTML entity ('⪲').
succeedsAboveSingleLineNotEqualTo :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
succeedsAboveSingleLineNotEqualTo = Tag_Entity "&#10930;"

-- | The precedes above equals sign HTML entity ('⪳').
precedesAboveEqualsSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
precedesAboveEqualsSign = Tag_Entity "&#10931;"

-- | The succeeds above equals sign HTML entity ('⪴').
succeedsAboveEqualsSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
succeedsAboveEqualsSign = Tag_Entity "&#10932;"

-- | The precedes above not equal to HTML entity ('⪵').
precedesAboveNotEqualTo :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
precedesAboveNotEqualTo = Tag_Entity "&#10933;"

-- | The succeeds above not equal to HTML entity ('⪶').
succeedsAboveNotEqualTo :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
succeedsAboveNotEqualTo = Tag_Entity "&#10934;"

-- | The precedes above almost equal to HTML entity ('⪷').
precedesAboveAlmostEqualTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
precedesAboveAlmostEqualTo = Tag_Entity "&#10935;"

-- | The succeeds above almost equal to HTML entity ('⪸').
succeedsAboveAlmostEqualTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
succeedsAboveAlmostEqualTo = Tag_Entity "&#10936;"

-- | The precedes above not almost equal to HTML entity ('⪹').
precedesAboveNotAlmostEqualTo :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
precedesAboveNotAlmostEqualTo = Tag_Entity "&#10937;"

-- | The succeeds above not almost equal to HTML entity ('⪺').
succeedsAboveNotAlmostEqualTo :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
succeedsAboveNotAlmostEqualTo = Tag_Entity "&#10938;"

-- | The double precedes HTML entity ('⪻').
doublePrecedes :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doublePrecedes = Tag_Entity "&#10939;"

-- | The double succeeds HTML entity ('⪼').
doubleSucceeds :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
doubleSucceeds = Tag_Entity "&#10940;"

-- | The subset with dot HTML entity ('⪽').
subsetWithDot :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
subsetWithDot = Tag_Entity "&#10941;"

-- | The superset with dot HTML entity ('⪾').
supersetWithDot :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
supersetWithDot = Tag_Entity "&#10942;"

-- | The subset with plus sign below HTML entity ('⪿').
subsetWithPlusSignBelow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
subsetWithPlusSignBelow = Tag_Entity "&#10943;"

-- | The superset with plus sign below HTML entity ('⫀').
supersetWithPlusSignBelow :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
supersetWithPlusSignBelow = Tag_Entity "&#10944;"

-- | The subset with multiplication sign below HTML entity ('⫁').
subsetWithMultiplicationSignBelow :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
subsetWithMultiplicationSignBelow = Tag_Entity "&#10945;"

-- | The superset with multiplication sign below HTML entity ('⫂').
supersetWithMultiplicationSignBelow :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
supersetWithMultiplicationSignBelow = Tag_Entity "&#10946;"

-- | The subset of or equal to with dot above HTML entity ('⫃').
subsetOfOrEqualToWithDotAbove :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
subsetOfOrEqualToWithDotAbove = Tag_Entity "&#10947;"

-- | The superset of or equal to with dot above HTML entity ('⫄').
supersetOfOrEqualToWithDotAbove :: ValidChild Text parent grandparent
                                => ChildHTML parent grandparent
supersetOfOrEqualToWithDotAbove = Tag_Entity "&#10948;"

-- | The subset of above equals sign HTML entity ('⫅').
subsetOfAboveEqualsSign :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
subsetOfAboveEqualsSign = Tag_Entity "&#10949;"

-- | The superset of above equals sign HTML entity ('⫆').
supersetOfAboveEqualsSign :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
supersetOfAboveEqualsSign = Tag_Entity "&#10950;"

-- | The subset of above tilde operator HTML entity ('⫇').
subsetOfAboveTildeOperator :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
subsetOfAboveTildeOperator = Tag_Entity "&#10951;"

-- | The superset of above tilde operator HTML entity ('⫈').
supersetOfAboveTildeOperator :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
supersetOfAboveTildeOperator = Tag_Entity "&#10952;"

-- | The Subset Of Above Almost Equal To HTML entity ('⫉').
subsetOfAboveAlmostEqualTo :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
subsetOfAboveAlmostEqualTo = Tag_Entity "&#10953;"

-- | The Superset Of Above Almost Equal To HTML entity ('⫊').
supersetOfAboveAlmostEqualTo :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
supersetOfAboveAlmostEqualTo = Tag_Entity "&#10954;"

-- | The subset of above not equal to HTML entity ('⫋').
subsetOfAboveNotEqualTo :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
subsetOfAboveNotEqualTo = Tag_Entity "&#10955;"

-- | The superset of above not equal to HTML entity ('⫌').
supersetOfAboveNotEqualTo :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
supersetOfAboveNotEqualTo = Tag_Entity "&#10956;"

-- | The closed subset HTML entity ('⫏').
closedSubset :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
closedSubset = Tag_Entity "&#10959;"

-- | The closed superset HTML entity ('⫐').
closedSuperset :: ValidChild Text parent grandparent
               => ChildHTML parent grandparent
closedSuperset = Tag_Entity "&#10960;"

-- | The closed subset or equal to HTML entity ('⫑').
closedSubsetOrEqualTo :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
closedSubsetOrEqualTo = Tag_Entity "&#10961;"

-- | The closed superset or equal to HTML entity ('⫒').
closedSupersetOrEqualTo :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
closedSupersetOrEqualTo = Tag_Entity "&#10962;"

-- | The subset above superset HTML entity ('⫓').
subsetAboveSuperset :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
subsetAboveSuperset = Tag_Entity "&#10963;"

-- | The superset above subset HTML entity ('⫔').
supersetAboveSubset :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
supersetAboveSubset = Tag_Entity "&#10964;"

-- | The subset above subset HTML entity ('⫕').
subsetAboveSubset :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
subsetAboveSubset = Tag_Entity "&#10965;"

-- | The superset above superset HTML entity ('⫖').
supersetAboveSuperset :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
supersetAboveSuperset = Tag_Entity "&#10966;"

-- | The superset beside subset HTML entity ('⫗').
supersetBesideSubset :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
supersetBesideSubset = Tag_Entity "&#10967;"

-- | The superset beside and joined by dash with subset HTML entity ('⫘').
supersetBesideAndJoinedByDashWithSubset :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
supersetBesideAndJoinedByDashWithSubset = Tag_Entity "&#10968;"

-- | The element of opening downwards HTML entity ('⫙').
elementOfOpeningDownwards :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
elementOfOpeningDownwards = Tag_Entity "&#10969;"

-- | The pitchfork with tee top HTML entity ('⫚').
pitchforkWithTeeTop :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
pitchforkWithTeeTop = Tag_Entity "&#10970;"

-- | The transversal intersection HTML entity ('⫛').
transversalIntersection :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
transversalIntersection = Tag_Entity "&#10971;"

-- | The vertical bar double left turnstile HTML entity ('⫤').
verticalBarDoubleLeftTurnstile :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
verticalBarDoubleLeftTurnstile = Tag_Entity "&#10980;"

-- | The long dash from left member of double vertical HTML entity ('⫦').
longDashFromLeftMemberOfDoubleVertical :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
longDashFromLeftMemberOfDoubleVertical = Tag_Entity "&#10982;"

-- | The short down tack with overbar HTML entity ('⫧').
shortDownTackWithOverbar :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
shortDownTackWithOverbar = Tag_Entity "&#10983;"

-- | The short up tack with underbar HTML entity ('⫨').
shortUpTackWithUnderbar :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
shortUpTackWithUnderbar = Tag_Entity "&#10984;"

-- | The short up tack above short down tack HTML entity ('⫩').
shortUpTackAboveShortDownTack :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
shortUpTackAboveShortDownTack = Tag_Entity "&#10985;"

-- | The double up tack HTML entity ('⫫').
doubleUpTack :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
doubleUpTack = Tag_Entity "&#10987;"

-- | The double stroke not sign HTML entity ('⫬').
doubleStrokeNotSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
doubleStrokeNotSign = Tag_Entity "&#10988;"

-- | The reversed double stroke not sign HTML entity ('⫭').
reversedDoubleStrokeNotSign :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
reversedDoubleStrokeNotSign = Tag_Entity "&#10989;"

-- | The does not divide with reversed negation slash HTML entity ('⫮').
doesNotDivideWithReversedNegationSlash :: ValidChild Text parent grandparent
                                       => ChildHTML parent grandparent
doesNotDivideWithReversedNegationSlash = Tag_Entity "&#10990;"

-- | The vertical line with circle above HTML entity ('⫯').
verticalLineWithCircleAbove :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
verticalLineWithCircleAbove = Tag_Entity "&#10991;"

-- | The vertical line with circle below HTML entity ('⫰').
verticalLineWithCircleBelow :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
verticalLineWithCircleBelow = Tag_Entity "&#10992;"

-- | The down tack with circle below HTML entity ('⫱').
downTackWithCircleBelow :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
downTackWithCircleBelow = Tag_Entity "&#10993;"

-- | The parallel with horizontal stroke HTML entity ('⫲').
parallelWithHorizontalStroke :: ValidChild Text parent grandparent
                             => ChildHTML parent grandparent
parallelWithHorizontalStroke = Tag_Entity "&#10994;"

-- | The parallel with tilde operator HTML entity ('⫳').
parallelWithTildeOperator :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
parallelWithTildeOperator = Tag_Entity "&#10995;"

-- | The Triple Nested Greater-than HTML entity ('⫸').
tripleNestedGreaterThan :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
tripleNestedGreaterThan = Tag_Entity "&#11000;"

-- | The Double-line Slanted Greater-than Or Equal To HTML entity ('⫺').
doubleLineSlantedGreaterThanOrEqualTo :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
doubleLineSlantedGreaterThanOrEqualTo = Tag_Entity "&#11002;"

-- | The double solidus operator HTML entity ('⫽').
doubleSolidusOperator :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
doubleSolidusOperator = Tag_Entity "&#11005;"

-- | The square with top half black HTML entity ('⬒').
squareWithTopHalfBlack :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
squareWithTopHalfBlack = Tag_Entity "&#11026;"

-- | The square with bottom half black HTML entity ('⬓').
squareWithBottomHalfBlack :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
squareWithBottomHalfBlack = Tag_Entity "&#11027;"

-- | The square with upper right diagonal half black HTML entity ('⬔').
squareWithUpperRightDiagonalHalfBlack :: ValidChild Text parent grandparent
                                      => ChildHTML parent grandparent
squareWithUpperRightDiagonalHalfBlack = Tag_Entity "&#11028;"

-- | The square with lower left diagonal half black HTML entity ('⬕').
squareWithLowerLeftDiagonalHalfBlack :: ValidChild Text parent grandparent
                                     => ChildHTML parent grandparent
squareWithLowerLeftDiagonalHalfBlack = Tag_Entity "&#11029;"

-- | The Diamond With Left Half Black HTML entity ('⬖').
diamondWithLeftHalfBlack :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
diamondWithLeftHalfBlack = Tag_Entity "&#11030;"

-- | The Diamond With Right Half Black HTML entity ('⬗').
diamondWithRightHalfBlack :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
diamondWithRightHalfBlack = Tag_Entity "&#11031;"

-- | The Diamond With Top Half Black HTML entity ('⬘').
diamondWithTopHalfBlack :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
diamondWithTopHalfBlack = Tag_Entity "&#11032;"

-- | The Diamond With Bottom Half Black HTML entity ('⬙').
diamondWithBottomHalfBlack :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
diamondWithBottomHalfBlack = Tag_Entity "&#11033;"

-- | The dotted square HTML entity ('⬚').
dottedSquare :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
dottedSquare = Tag_Entity "&#11034;"

-- | The black large square HTML entity ('⬛').
blackLargeSquare :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
blackLargeSquare = Tag_Entity "&#11035;"

-- | The white large square HTML entity ('⬜').
whiteLargeSquare :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
whiteLargeSquare = Tag_Entity "&#11036;"

-- | The black very small square HTML entity ('⬝').
blackVerySmallSquare :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
blackVerySmallSquare = Tag_Entity "&#11037;"

-- | The white very small square HTML entity ('⬞').
whiteVerySmallSquare :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
whiteVerySmallSquare = Tag_Entity "&#11038;"

-- | The Black Pentagon HTML entity ('⬟').
blackPentagon :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
blackPentagon = Tag_Entity "&#11039;"

-- | The White Pentagon HTML entity ('⬠').
whitePentagon :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
whitePentagon = Tag_Entity "&#11040;"

-- | The White Hexagon HTML entity ('⬡').
whiteHexagon :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
whiteHexagon = Tag_Entity "&#11041;"

-- | The Black Hexagon HTML entity ('⬢').
blackHexagon :: ValidChild Text parent grandparent
             => ChildHTML parent grandparent
blackHexagon = Tag_Entity "&#11042;"

-- | The Horizontal Black Hexagon HTML entity ('⬣').
horizontalBlackHexagon :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
horizontalBlackHexagon = Tag_Entity "&#11043;"

-- | The Black Large Circle HTML entity ('⬤').
blackLargeCircle :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
blackLargeCircle = Tag_Entity "&#11044;"

-- | The Black Medium Diamond HTML entity ('⬥').
blackMediumDiamond :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
blackMediumDiamond = Tag_Entity "&#11045;"

-- | The White Medium Diamond HTML entity ('⬦').
whiteMediumDiamond :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
whiteMediumDiamond = Tag_Entity "&#11046;"

-- | The Black Medium Lozenge HTML entity ('⬧').
blackMediumLozenge :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
blackMediumLozenge = Tag_Entity "&#11047;"

-- | The White Medium Lozenge HTML entity ('⬨').
whiteMediumLozenge :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
whiteMediumLozenge = Tag_Entity "&#11048;"

-- | The Black Small Diamond HTML entity ('⬩').
blackSmallDiamond :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
blackSmallDiamond = Tag_Entity "&#11049;"

-- | The Black Small Lozenge HTML entity ('⬪').
blackSmallLozenge :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
blackSmallLozenge = Tag_Entity "&#11050;"

-- | The White Small Lozenge HTML entity ('⬫').
whiteSmallLozenge :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
whiteSmallLozenge = Tag_Entity "&#11051;"

-- | The Black Horizontal Ellipse HTML entity ('⬬').
blackHorizontalEllipse :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
blackHorizontalEllipse = Tag_Entity "&#11052;"

-- | The White Horizontal Ellipse HTML entity ('⬭').
whiteHorizontalEllipse :: ValidChild Text parent grandparent
                       => ChildHTML parent grandparent
whiteHorizontalEllipse = Tag_Entity "&#11053;"

-- | The Black Vertical Ellipse HTML entity ('⬮').
blackVerticalEllipse :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
blackVerticalEllipse = Tag_Entity "&#11054;"

-- | The White Vertical Ellipse HTML entity ('⬯').
whiteVerticalEllipse :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
whiteVerticalEllipse = Tag_Entity "&#11055;"

-- | The Equals Sign Above Leftwards Arrow HTML entity ('⭀').
equalsSignAboveLeftwardsArrow :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
equalsSignAboveLeftwardsArrow = Tag_Entity "&#11072;"

-- | The Leftwards Arrow Above Reverse Almost Equal To HTML entity ('⭂').
leftwardsArrowAboveReverseAlmostEqualTo :: ValidChild Text parent grandparent
                                        => ChildHTML parent grandparent
leftwardsArrowAboveReverseAlmostEqualTo = Tag_Entity "&#11074;"

-- | The Rightwards Arrow Through Greater-than HTML entity ('⭃').
rightwardsArrowThroughGreaterThan :: ValidChild Text parent grandparent
                                  => ChildHTML parent grandparent
rightwardsArrowThroughGreaterThan = Tag_Entity "&#11075;"

-- | The Rightwards Arrow Above Reverse Almost Equal To HTML entity ('⭈').
rightwardsArrowAboveReverseAlmostEqualTo :: ValidChild Text parent grandparent
                                         => ChildHTML parent grandparent
rightwardsArrowAboveReverseAlmostEqualTo = Tag_Entity "&#11080;"

-- | The Leftwards Arrow Above Almost Equal To HTML entity ('⭊').
leftwardsArrowAboveAlmostEqualTo :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
leftwardsArrowAboveAlmostEqualTo = Tag_Entity "&#11082;"

-- | The Black Right-pointing Pentagon HTML entity ('⭓').
blackRightPointingPentagon :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
blackRightPointingPentagon = Tag_Entity "&#11091;"

-- | The White Right-pointing Pentagon HTML entity ('⭔').
whiteRightPointingPentagon :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
whiteRightPointingPentagon = Tag_Entity "&#11092;"

-- | The Heavy Large Circle HTML entity ('⭕').
heavyLargeCircle :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
heavyLargeCircle = Tag_Entity "&#11093;"

-- | The black square centred HTML entity ('⯀').
blackSquareCentred :: ValidChild Text parent grandparent
                   => ChildHTML parent grandparent
blackSquareCentred = Tag_Entity "&#11200;"

-- | The Black Diamond Centred HTML entity ('⯁').
blackDiamondCentred :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
blackDiamondCentred = Tag_Entity "&#11201;"

-- | The Turned Black Pentagon HTML entity ('⯂').
turnedBlackPentagon :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
turnedBlackPentagon = Tag_Entity "&#11202;"

-- | The square position indicator HTML entity ('⯐').
squarePositionIndicator :: ValidChild Text parent grandparent
                        => ChildHTML parent grandparent
squarePositionIndicator = Tag_Entity "&#11216;"

-- | The Dotted Right-pointing Angle HTML entity ('⸖').
dottedRightPointingAngle :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
dottedRightPointingAngle = Tag_Entity "&#11798;"

-- | The Modifier Letter Lower Right Corner Angle HTML entity ('ꜚ').
modifierLetterLowerRightCornerAngle :: ValidChild Text parent grandparent
                                    => ChildHTML parent grandparent
modifierLetterLowerRightCornerAngle = Tag_Entity "&#42778;"

-- | The Modifier Letter Short Equals Sign HTML entity ('꞊').
modifierLetterShortEqualsSign :: ValidChild Text parent grandparent
                              => ChildHTML parent grandparent
modifierLetterShortEqualsSign = Tag_Entity "&#42890;"

-- | The latin small ligature ff HTML entity ('ﬀ').
latinSmallLigatureFf :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinSmallLigatureFf = Tag_Entity "&#64256;"

-- | The latin small ligature fi HTML entity ('ﬁ').
latinSmallLigatureFi :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinSmallLigatureFi = Tag_Entity "&#64257;"

-- | The latin small ligature fl HTML entity ('ﬂ').
latinSmallLigatureFl :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
latinSmallLigatureFl = Tag_Entity "&#64258;"

-- | The latin small ligature ffi HTML entity ('ﬃ').
latinSmallLigatureFfi :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinSmallLigatureFfi = Tag_Entity "&#64259;"

-- | The latin small ligature ffl HTML entity ('ﬄ').
latinSmallLigatureFfl :: ValidChild Text parent grandparent
                      => ChildHTML parent grandparent
latinSmallLigatureFfl = Tag_Entity "&#64260;"

-- | The Small Plus Sign HTML entity ('﹢').
smallPlusSign :: ValidChild Text parent grandparent
              => ChildHTML parent grandparent
smallPlusSign = Tag_Entity "&#65122;"

-- | The Small Hyphen-minus HTML entity ('﹣').
smallHyphenMinus :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
smallHyphenMinus = Tag_Entity "&#65123;"

-- | The Small Greater-than Sign HTML entity ('﹥').
smallGreaterThanSign :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
smallGreaterThanSign = Tag_Entity "&#65125;"

-- | The Small Equals Sign HTML entity ('﹦').
smallEqualsSign :: ValidChild Text parent grandparent
                => ChildHTML parent grandparent
smallEqualsSign = Tag_Entity "&#65126;"

-- | The Small Percent Sign HTML entity ('﹪').
smallPercentSign :: ValidChild Text parent grandparent
                 => ChildHTML parent grandparent
smallPercentSign = Tag_Entity "&#65130;"

-- | The Fullwidth Percent Sign HTML entity ('％').
fullwidthPercentSign :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
fullwidthPercentSign = Tag_Entity "&#65285;"

-- | The Fullwidth Plus Sign HTML entity ('＋').
fullwidthPlusSign :: ValidChild Text parent grandparent
                  => ChildHTML parent grandparent
fullwidthPlusSign = Tag_Entity "&#65291;"

-- | The Fullwidth Hyphen-minus HTML entity ('－').
fullwidthHyphenMinus :: ValidChild Text parent grandparent
                     => ChildHTML parent grandparent
fullwidthHyphenMinus = Tag_Entity "&#65293;"

-- | The Fullwidth Equals Sign HTML entity ('＝').
fullwidthEqualsSign :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
fullwidthEqualsSign = Tag_Entity "&#65309;"

-- | The Fullwidth Greater-than Sign HTML entity ('＞').
fullwidthGreaterThanSign :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
fullwidthGreaterThanSign = Tag_Entity "&#65310;"

-- | The Ugaritic Letter Delta HTML entity ('𐎄').
ugariticLetterDelta :: ValidChild Text parent grandparent
                    => ChildHTML parent grandparent
ugariticLetterDelta = Tag_Entity "&#66436;"

-- | The mathematical script capital a HTML entity ('𝒜').
mathematicalScriptCapitalA :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalA = Tag_Entity "&#119964;"

-- | The mathematical script capital c HTML entity ('𝒞').
mathematicalScriptCapitalC :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalC = Tag_Entity "&#119966;"

-- | The mathematical script capital d HTML entity ('𝒟').
mathematicalScriptCapitalD :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalD = Tag_Entity "&#119967;"

-- | The mathematical script capital g HTML entity ('𝒢').
mathematicalScriptCapitalG :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalG = Tag_Entity "&#119970;"

-- | The mathematical script capital j HTML entity ('𝒥').
mathematicalScriptCapitalJ :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalJ = Tag_Entity "&#119973;"

-- | The mathematical script capital k HTML entity ('𝒦').
mathematicalScriptCapitalK :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalK = Tag_Entity "&#119974;"

-- | The mathematical script capital n HTML entity ('𝒩').
mathematicalScriptCapitalN :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalN = Tag_Entity "&#119977;"

-- | The mathematical script capital o HTML entity ('𝒪').
mathematicalScriptCapitalO :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalO = Tag_Entity "&#119978;"

-- | The mathematical script capital p HTML entity ('𝒫').
mathematicalScriptCapitalP :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalP = Tag_Entity "&#119979;"

-- | The mathematical script capital q HTML entity ('𝒬').
mathematicalScriptCapitalQ :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalQ = Tag_Entity "&#119980;"

-- | The mathematical script capital s HTML entity ('𝒮').
mathematicalScriptCapitalS :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalS = Tag_Entity "&#119982;"

-- | The mathematical script capital t HTML entity ('𝒯').
mathematicalScriptCapitalT :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalT = Tag_Entity "&#119983;"

-- | The mathematical script capital u HTML entity ('𝒰').
mathematicalScriptCapitalU :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalU = Tag_Entity "&#119984;"

-- | The mathematical script capital v HTML entity ('𝒱').
mathematicalScriptCapitalV :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalV = Tag_Entity "&#119985;"

-- | The mathematical script capital w HTML entity ('𝒲').
mathematicalScriptCapitalW :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalW = Tag_Entity "&#119986;"

-- | The mathematical script capital x HTML entity ('𝒳').
mathematicalScriptCapitalX :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalX = Tag_Entity "&#119987;"

-- | The mathematical script capital y HTML entity ('𝒴').
mathematicalScriptCapitalY :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalY = Tag_Entity "&#119988;"

-- | The mathematical script capital z HTML entity ('𝒵').
mathematicalScriptCapitalZ :: ValidChild Text parent grandparent
                           => ChildHTML parent grandparent
mathematicalScriptCapitalZ = Tag_Entity "&#119989;"

-- | The mathematical script small a HTML entity ('𝒶').
mathematicalScriptSmallA :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallA = Tag_Entity "&#119990;"

-- | The mathematical script small b HTML entity ('𝒷').
mathematicalScriptSmallB :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallB = Tag_Entity "&#119991;"

-- | The mathematical script small c HTML entity ('𝒸').
mathematicalScriptSmallC :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallC = Tag_Entity "&#119992;"

-- | The mathematical script small d HTML entity ('𝒹').
mathematicalScriptSmallD :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallD = Tag_Entity "&#119993;"

-- | The mathematical script small f HTML entity ('𝒻').
mathematicalScriptSmallF :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallF = Tag_Entity "&#119995;"

-- | The mathematical script small h HTML entity ('𝒽').
mathematicalScriptSmallH :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallH = Tag_Entity "&#119997;"

-- | The mathematical script small i HTML entity ('𝒾').
mathematicalScriptSmallI :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallI = Tag_Entity "&#119998;"

-- | The mathematical script small j HTML entity ('𝒿').
mathematicalScriptSmallJ :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallJ = Tag_Entity "&#119999;"

-- | The mathematical script small k HTML entity ('𝓀').
mathematicalScriptSmallK :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallK = Tag_Entity "&#120000;"

-- | The mathematical script small l HTML entity ('𝓁').
mathematicalScriptSmallL :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallL = Tag_Entity "&#120001;"

-- | The mathematical script small m HTML entity ('𝓂').
mathematicalScriptSmallM :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallM = Tag_Entity "&#120002;"

-- | The mathematical script small n HTML entity ('𝓃').
mathematicalScriptSmallN :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallN = Tag_Entity "&#120003;"

-- | The mathematical script small p HTML entity ('𝓅').
mathematicalScriptSmallP :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallP = Tag_Entity "&#120005;"

-- | The mathematical script small q HTML entity ('𝓆').
mathematicalScriptSmallQ :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallQ = Tag_Entity "&#120006;"

-- | The mathematical script small r HTML entity ('𝓇').
mathematicalScriptSmallR :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallR = Tag_Entity "&#120007;"

-- | The mathematical script small s HTML entity ('𝓈').
mathematicalScriptSmallS :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallS = Tag_Entity "&#120008;"

-- | The mathematical script small t HTML entity ('𝓉').
mathematicalScriptSmallT :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallT = Tag_Entity "&#120009;"

-- | The mathematical script small u HTML entity ('𝓊').
mathematicalScriptSmallU :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallU = Tag_Entity "&#120010;"

-- | The mathematical script small v HTML entity ('𝓋').
mathematicalScriptSmallV :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallV = Tag_Entity "&#120011;"

-- | The mathematical script small w HTML entity ('𝓌').
mathematicalScriptSmallW :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallW = Tag_Entity "&#120012;"

-- | The mathematical script small x HTML entity ('𝓍').
mathematicalScriptSmallX :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallX = Tag_Entity "&#120013;"

-- | The mathematical script small y HTML entity ('𝓎').
mathematicalScriptSmallY :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallY = Tag_Entity "&#120014;"

-- | The mathematical script small z HTML entity ('𝓏').
mathematicalScriptSmallZ :: ValidChild Text parent grandparent
                         => ChildHTML parent grandparent
mathematicalScriptSmallZ = Tag_Entity "&#120015;"

-- | The mathematical fraktur capital a HTML entity ('𝔄').
mathematicalFrakturCapitalA :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalA = Tag_Entity "&#120068;"

-- | The mathematical fraktur capital b HTML entity ('𝔅').
mathematicalFrakturCapitalB :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalB = Tag_Entity "&#120069;"

-- | The mathematical fraktur capital d HTML entity ('𝔇').
mathematicalFrakturCapitalD :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalD = Tag_Entity "&#120071;"

-- | The mathematical fraktur capital e HTML entity ('𝔈').
mathematicalFrakturCapitalE :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalE = Tag_Entity "&#120072;"

-- | The mathematical fraktur capital f HTML entity ('𝔉').
mathematicalFrakturCapitalF :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalF = Tag_Entity "&#120073;"

-- | The mathematical fraktur capital g HTML entity ('𝔊').
mathematicalFrakturCapitalG :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalG = Tag_Entity "&#120074;"

-- | The mathematical fraktur capital j HTML entity ('𝔍').
mathematicalFrakturCapitalJ :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalJ = Tag_Entity "&#120077;"

-- | The mathematical fraktur capital k HTML entity ('𝔎').
mathematicalFrakturCapitalK :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalK = Tag_Entity "&#120078;"

-- | The mathematical fraktur capital l HTML entity ('𝔏').
mathematicalFrakturCapitalL :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalL = Tag_Entity "&#120079;"

-- | The mathematical fraktur capital m HTML entity ('𝔐').
mathematicalFrakturCapitalM :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalM = Tag_Entity "&#120080;"

-- | The mathematical fraktur capital n HTML entity ('𝔑').
mathematicalFrakturCapitalN :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalN = Tag_Entity "&#120081;"

-- | The mathematical fraktur capital o HTML entity ('𝔒').
mathematicalFrakturCapitalO :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalO = Tag_Entity "&#120082;"

-- | The mathematical fraktur capital p HTML entity ('𝔓').
mathematicalFrakturCapitalP :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalP = Tag_Entity "&#120083;"

-- | The mathematical fraktur capital q HTML entity ('𝔔').
mathematicalFrakturCapitalQ :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalQ = Tag_Entity "&#120084;"

-- | The mathematical fraktur capital s HTML entity ('𝔖').
mathematicalFrakturCapitalS :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalS = Tag_Entity "&#120086;"

-- | The mathematical fraktur capital t HTML entity ('𝔗').
mathematicalFrakturCapitalT :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalT = Tag_Entity "&#120087;"

-- | The mathematical fraktur capital u HTML entity ('𝔘').
mathematicalFrakturCapitalU :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalU = Tag_Entity "&#120088;"

-- | The mathematical fraktur capital v HTML entity ('𝔙').
mathematicalFrakturCapitalV :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalV = Tag_Entity "&#120089;"

-- | The mathematical fraktur capital w HTML entity ('𝔚').
mathematicalFrakturCapitalW :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalW = Tag_Entity "&#120090;"

-- | The mathematical fraktur capital x HTML entity ('𝔛').
mathematicalFrakturCapitalX :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalX = Tag_Entity "&#120091;"

-- | The mathematical fraktur capital y HTML entity ('𝔜').
mathematicalFrakturCapitalY :: ValidChild Text parent grandparent
                            => ChildHTML parent grandparent
mathematicalFrakturCapitalY = Tag_Entity "&#120092;"

-- | The mathematical fraktur small a HTML entity ('𝔞').
mathematicalFrakturSmallA :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallA = Tag_Entity "&#120094;"

-- | The mathematical fraktur small b HTML entity ('𝔟').
mathematicalFrakturSmallB :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallB = Tag_Entity "&#120095;"

-- | The mathematical fraktur small c HTML entity ('𝔠').
mathematicalFrakturSmallC :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallC = Tag_Entity "&#120096;"

-- | The mathematical fraktur small d HTML entity ('𝔡').
mathematicalFrakturSmallD :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallD = Tag_Entity "&#120097;"

-- | The mathematical fraktur small e HTML entity ('𝔢').
mathematicalFrakturSmallE :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallE = Tag_Entity "&#120098;"

-- | The mathematical fraktur small f HTML entity ('𝔣').
mathematicalFrakturSmallF :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallF = Tag_Entity "&#120099;"

-- | The mathematical fraktur small g HTML entity ('𝔤').
mathematicalFrakturSmallG :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallG = Tag_Entity "&#120100;"

-- | The mathematical fraktur small h HTML entity ('𝔥').
mathematicalFrakturSmallH :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallH = Tag_Entity "&#120101;"

-- | The mathematical fraktur small i HTML entity ('𝔦').
mathematicalFrakturSmallI :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallI = Tag_Entity "&#120102;"

-- | The mathematical fraktur small j HTML entity ('𝔧').
mathematicalFrakturSmallJ :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallJ = Tag_Entity "&#120103;"

-- | The mathematical fraktur small k HTML entity ('𝔨').
mathematicalFrakturSmallK :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallK = Tag_Entity "&#120104;"

-- | The mathematical fraktur small l HTML entity ('𝔩').
mathematicalFrakturSmallL :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallL = Tag_Entity "&#120105;"

-- | The mathematical fraktur small m HTML entity ('𝔪').
mathematicalFrakturSmallM :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallM = Tag_Entity "&#120106;"

-- | The mathematical fraktur small n HTML entity ('𝔫').
mathematicalFrakturSmallN :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallN = Tag_Entity "&#120107;"

-- | The mathematical fraktur small o HTML entity ('𝔬').
mathematicalFrakturSmallO :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallO = Tag_Entity "&#120108;"

-- | The mathematical fraktur small p HTML entity ('𝔭').
mathematicalFrakturSmallP :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallP = Tag_Entity "&#120109;"

-- | The mathematical fraktur small q HTML entity ('𝔮').
mathematicalFrakturSmallQ :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallQ = Tag_Entity "&#120110;"

-- | The mathematical fraktur small r HTML entity ('𝔯').
mathematicalFrakturSmallR :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallR = Tag_Entity "&#120111;"

-- | The mathematical fraktur small s HTML entity ('𝔰').
mathematicalFrakturSmallS :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallS = Tag_Entity "&#120112;"

-- | The mathematical fraktur small t HTML entity ('𝔱').
mathematicalFrakturSmallT :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallT = Tag_Entity "&#120113;"

-- | The mathematical fraktur small u HTML entity ('𝔲').
mathematicalFrakturSmallU :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallU = Tag_Entity "&#120114;"

-- | The mathematical fraktur small v HTML entity ('𝔳').
mathematicalFrakturSmallV :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallV = Tag_Entity "&#120115;"

-- | The mathematical fraktur small w HTML entity ('𝔴').
mathematicalFrakturSmallW :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallW = Tag_Entity "&#120116;"

-- | The mathematical fraktur small x HTML entity ('𝔵').
mathematicalFrakturSmallX :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallX = Tag_Entity "&#120117;"

-- | The mathematical fraktur small y HTML entity ('𝔶').
mathematicalFrakturSmallY :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallY = Tag_Entity "&#120118;"

-- | The mathematical fraktur small z HTML entity ('𝔷').
mathematicalFrakturSmallZ :: ValidChild Text parent grandparent
                          => ChildHTML parent grandparent
mathematicalFrakturSmallZ = Tag_Entity "&#120119;"

-- | The mathematical double-struck capital a HTML entity ('𝔸').
mathematicalDoubleStruckCapitalA :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalA = Tag_Entity "&#120120;"

-- | The mathematical double-struck capital b HTML entity ('𝔹').
mathematicalDoubleStruckCapitalB :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalB = Tag_Entity "&#120121;"

-- | The mathematical double-struck capital d HTML entity ('𝔻').
mathematicalDoubleStruckCapitalD :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalD = Tag_Entity "&#120123;"

-- | The mathematical double-struck capital e HTML entity ('𝔼').
mathematicalDoubleStruckCapitalE :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalE = Tag_Entity "&#120124;"

-- | The mathematical double-struck capital f HTML entity ('𝔽').
mathematicalDoubleStruckCapitalF :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalF = Tag_Entity "&#120125;"

-- | The mathematical double-struck capital g HTML entity ('𝔾').
mathematicalDoubleStruckCapitalG :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalG = Tag_Entity "&#120126;"

-- | The mathematical double-struck capital i HTML entity ('𝕀').
mathematicalDoubleStruckCapitalI :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalI = Tag_Entity "&#120128;"

-- | The mathematical double-struck capital j HTML entity ('𝕁').
mathematicalDoubleStruckCapitalJ :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalJ = Tag_Entity "&#120129;"

-- | The mathematical double-struck capital k HTML entity ('𝕂').
mathematicalDoubleStruckCapitalK :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalK = Tag_Entity "&#120130;"

-- | The mathematical double-struck capital l HTML entity ('𝕃').
mathematicalDoubleStruckCapitalL :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalL = Tag_Entity "&#120131;"

-- | The mathematical double-struck capital m HTML entity ('𝕄').
mathematicalDoubleStruckCapitalM :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalM = Tag_Entity "&#120132;"

-- | The mathematical double-struck capital o HTML entity ('𝕆').
mathematicalDoubleStruckCapitalO :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalO = Tag_Entity "&#120134;"

-- | The mathematical double-struck capital s HTML entity ('𝕊').
mathematicalDoubleStruckCapitalS :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalS = Tag_Entity "&#120138;"

-- | The mathematical double-struck capital t HTML entity ('𝕋').
mathematicalDoubleStruckCapitalT :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalT = Tag_Entity "&#120139;"

-- | The mathematical double-struck capital u HTML entity ('𝕌').
mathematicalDoubleStruckCapitalU :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalU = Tag_Entity "&#120140;"

-- | The mathematical double-struck capital v HTML entity ('𝕍').
mathematicalDoubleStruckCapitalV :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalV = Tag_Entity "&#120141;"

-- | The mathematical double-struck capital w HTML entity ('𝕎').
mathematicalDoubleStruckCapitalW :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalW = Tag_Entity "&#120142;"

-- | The mathematical double-struck capital x HTML entity ('𝕏').
mathematicalDoubleStruckCapitalX :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalX = Tag_Entity "&#120143;"

-- | The mathematical double-struck capital y HTML entity ('𝕐').
mathematicalDoubleStruckCapitalY :: ValidChild Text parent grandparent
                                 => ChildHTML parent grandparent
mathematicalDoubleStruckCapitalY = Tag_Entity "&#120144;"

-- | The mathematical double-struck small a HTML entity ('𝕒').
mathematicalDoubleStruckSmallA :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallA = Tag_Entity "&#120146;"

-- | The mathematical double-struck small b HTML entity ('𝕓').
mathematicalDoubleStruckSmallB :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallB = Tag_Entity "&#120147;"

-- | The mathematical double-struck small c HTML entity ('𝕔').
mathematicalDoubleStruckSmallC :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallC = Tag_Entity "&#120148;"

-- | The mathematical double-struck small d HTML entity ('𝕕').
mathematicalDoubleStruckSmallD :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallD = Tag_Entity "&#120149;"

-- | The mathematical double-struck small e HTML entity ('𝕖').
mathematicalDoubleStruckSmallE :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallE = Tag_Entity "&#120150;"

-- | The mathematical double-struck small f HTML entity ('𝕗').
mathematicalDoubleStruckSmallF :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallF = Tag_Entity "&#120151;"

-- | The mathematical double-struck small g HTML entity ('𝕘').
mathematicalDoubleStruckSmallG :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallG = Tag_Entity "&#120152;"

-- | The mathematical double-struck small h HTML entity ('𝕙').
mathematicalDoubleStruckSmallH :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallH = Tag_Entity "&#120153;"

-- | The mathematical double-struck small i HTML entity ('𝕚').
mathematicalDoubleStruckSmallI :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallI = Tag_Entity "&#120154;"

-- | The mathematical double-struck small j HTML entity ('𝕛').
mathematicalDoubleStruckSmallJ :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallJ = Tag_Entity "&#120155;"

-- | The mathematical double-struck small k HTML entity ('𝕜').
mathematicalDoubleStruckSmallK :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallK = Tag_Entity "&#120156;"

-- | The mathematical double-struck small l HTML entity ('𝕝').
mathematicalDoubleStruckSmallL :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallL = Tag_Entity "&#120157;"

-- | The mathematical double-struck small m HTML entity ('𝕞').
mathematicalDoubleStruckSmallM :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallM = Tag_Entity "&#120158;"

-- | The mathematical double-struck small n HTML entity ('𝕟').
mathematicalDoubleStruckSmallN :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallN = Tag_Entity "&#120159;"

-- | The mathematical double-struck small o HTML entity ('𝕠').
mathematicalDoubleStruckSmallO :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallO = Tag_Entity "&#120160;"

-- | The mathematical double-struck small p HTML entity ('𝕡').
mathematicalDoubleStruckSmallP :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallP = Tag_Entity "&#120161;"

-- | The mathematical double-struck small q HTML entity ('𝕢').
mathematicalDoubleStruckSmallQ :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallQ = Tag_Entity "&#120162;"

-- | The mathematical double-struck small r HTML entity ('𝕣').
mathematicalDoubleStruckSmallR :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallR = Tag_Entity "&#120163;"

-- | The mathematical double-struck small s HTML entity ('𝕤').
mathematicalDoubleStruckSmallS :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallS = Tag_Entity "&#120164;"

-- | The mathematical double-struck small t HTML entity ('𝕥').
mathematicalDoubleStruckSmallT :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallT = Tag_Entity "&#120165;"

-- | The mathematical double-struck small u HTML entity ('𝕦').
mathematicalDoubleStruckSmallU :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallU = Tag_Entity "&#120166;"

-- | The mathematical double-struck small v HTML entity ('𝕧').
mathematicalDoubleStruckSmallV :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallV = Tag_Entity "&#120167;"

-- | The mathematical double-struck small w HTML entity ('𝕨').
mathematicalDoubleStruckSmallW :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallW = Tag_Entity "&#120168;"

-- | The mathematical double-struck small x HTML entity ('𝕩').
mathematicalDoubleStruckSmallX :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallX = Tag_Entity "&#120169;"

-- | The mathematical double-struck small y HTML entity ('𝕪').
mathematicalDoubleStruckSmallY :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallY = Tag_Entity "&#120170;"

-- | The mathematical double-struck small z HTML entity ('𝕫').
mathematicalDoubleStruckSmallZ :: ValidChild Text parent grandparent
                               => ChildHTML parent grandparent
mathematicalDoubleStruckSmallZ = Tag_Entity "&#120171;"