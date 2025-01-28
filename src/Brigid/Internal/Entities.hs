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
module Brigid.Internal.Entities
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

-- | The null character HTML entity ('^@').
nullCharacter :: String
nullCharacter = "&#0;"

-- | The start of header HTML entity ('^A').
startOfHeader :: String
startOfHeader = "&#1;"

-- | The start of text HTML entity ('^B').
startOfText :: String
startOfText = "&#2;"

-- | The end of text HTML entity ('^C').
endOfText :: String
endOfText = "&#3;"

-- | The end of transmission HTML entity ('^D').
endOfTransmission :: String
endOfTransmission = "&#4;"

-- | The enquiry HTML entity ('^E').
enquiry :: String
enquiry = "&#5;"

-- | The acknowledge HTML entity ('^F').
acknowledge :: String
acknowledge = "&#6;"

-- | The bell (ring) HTML entity ('^G').
bell :: String
bell = "&#7;"

-- | The backspace HTML entity ('^H').
backspace :: String
backspace = "&#8;"

-- | The character tabulation / horizontal tabulation HTML entity ('^I').
characterTabulation :: String
characterTabulation = "&#9;"

-- | The line feed (lf) HTML entity ('^J').
lineFeed :: String
lineFeed = "&#10;"

-- | The vertical tab HTML entity ('^K').
verticalTab :: String
verticalTab = "&#11;"

-- | The form feed HTML entity ('^L').
formFeed :: String
formFeed = "&#12;"

-- | The carriage return HTML entity ('^M').
carriageReturn :: String
carriageReturn = "&#13;"

-- | The shift out HTML entity ('^N').
shiftOut :: String
shiftOut = "&#14;"

-- | The shift in HTML entity ('^O').
shiftIn :: String
shiftIn = "&#15;"

-- | The data link escape HTML entity ('^P').
dataLinkEscape :: String
dataLinkEscape = "&#16;"

-- | The device control 1 HTML entity ('^Q').
deviceControl1 :: String
deviceControl1 = "&#17;"

-- | The device control 2 HTML entity ('^R').
deviceControl2 :: String
deviceControl2 = "&#18;"

-- | The device control 3 HTML entity ('^S').
deviceControl3 :: String
deviceControl3 = "&#19;"

-- | The device control 4 HTML entity ('^T').
deviceControl4 :: String
deviceControl4 = "&#20;"

-- | The negative acknowledge HTML entity ('^U').
negativeAcknowledge :: String
negativeAcknowledge = "&#21;"

-- | The synchronize HTML entity ('^V').
synchronize :: String
synchronize = "&#22;"

-- | The end transmission block HTML entity ('^W').
endTransmissionBlock :: String
endTransmissionBlock = "&#23;"

-- | The cancel HTML entity ('^X').
cancel :: String
cancel = "&#24;"

-- | The end of medium HTML entity ('^Y').
endOfMedium :: String
endOfMedium = "&#25;"

-- | The substitute HTML entity ('^Z').
substitute :: String
substitute = "&#26;"

-- | The escape HTML entity ('^[').
escape :: String
escape = "&#27;"

-- | The file separator HTML entity ('^\').
fileSeparator :: String
fileSeparator = "&#28;"

-- | The group separator HTML entity ('^]').
groupSeparator :: String
groupSeparator = "&#29;"

-- | The record separator HTML entity ('^^').
recordSeparator :: String
recordSeparator = "&#30;"

-- | The unit separator HTML entity ('^_').
unitSeparator :: String
unitSeparator = "&#31;"

-- | The space HTML entity.
space :: String
space = "&#32;"

-- | The exclamation point HTML entity ('!').
exclamationPoint :: String
exclamationPoint = "&#33;"

-- | The quotation mark HTML entity ('"').
quotationMark :: String
quotationMark = "&#34;"

-- | The number sign HTML entity ('#').
numberSign :: String
numberSign = "&#35;"

-- | The dollar sign HTML entity ('$').
dollarSign :: String
dollarSign = "&#36;"

-- | The percent sign HTML entity ('%').
percentSign :: String
percentSign = "&#37;"

-- | The ampersand HTML entity ('&').
ampersand :: String
ampersand = "&#38;"

-- | The single quote HTML entity (''').
singleQuote :: String
singleQuote = "&#39;"

-- | The opening parenthesis HTML entity ('(').
openingParenthesis :: String
openingParenthesis = "&#40;"

-- | The closing parenthesis HTML entity (')').
closingParenthesis :: String
closingParenthesis = "&#41;"

-- | The asterisk HTML entity ('*').
asterisk :: String
asterisk = "&#42;"

-- | The plus sign HTML entity ('+').
plusSign :: String
plusSign = "&#43;"

-- | The comma HTML entity (',').
comma :: String
comma = "&#44;"

-- | The minus sign - hyphen HTML entity ('-').
minusSignHyphen :: String
minusSignHyphen = "&#45;"

-- | The period HTML entity ('.').
period :: String
period = "&#46;"

-- | The slash HTML entity ('/').
slash :: String
slash = "&#47;"

-- | The zero HTML entity ('0').
zero :: String
zero = "&#48;"

-- | The one HTML entity ('1').
one :: String
one = "&#49;"

-- | The two HTML entity ('2').
two :: String
two = "&#50;"

-- | The three HTML entity ('3').
three :: String
three = "&#51;"

-- | The four HTML entity ('4').
four :: String
four = "&#52;"

-- | The five HTML entity ('5').
five :: String
five = "&#53;"

-- | The six HTML entity ('6').
six :: String
six = "&#54;"

-- | The seven HTML entity ('7').
seven :: String
seven = "&#55;"

-- | The eight HTML entity ('8').
eight :: String
eight = "&#56;"

-- | The nine HTML entity ('9').
nine :: String
nine = "&#57;"

-- | The colon HTML entity (':').
colon :: String
colon = "&#58;"

-- | The semicolon HTML entity (';').
semicolon :: String
semicolon = "&#59;"

-- | The less-than sign HTML entity ('<').
lessThanSign :: String
lessThanSign = "&#60;"

-- | The equal sign HTML entity ('=').
equalSign :: String
equalSign = "&#61;"

-- | The greater-than sign HTML entity ('>').
greaterThanSign :: String
greaterThanSign = "&#62;"

-- | The question mark HTML entity ('?').
questionMark :: String
questionMark = "&#63;"

-- | The at symbol HTML entity ('@').
atSymbol :: String
atSymbol = "&#64;"

-- | The upper case A HTML entity ('A').
upperCaseA :: String
upperCaseA = "&#65;"

-- | The upper case B HTML entity ('B').
upperCaseB :: String
upperCaseB = "&#66;"

-- | The upper case C  HTML entity ('C').
upperCaseC :: String
upperCaseC = "&#67;"

-- | The upper case D  HTML entity ('D').
upperCaseD :: String
upperCaseD = "&#68;"

-- | The upper case E  HTML entity ('E').
upperCaseE :: String
upperCaseE = "&#69;"

-- | The upper case F  HTML entity ('F').
upperCaseF :: String
upperCaseF = "&#70;"

-- | The upper case G  HTML entity ('G').
upperCaseG :: String
upperCaseG = "&#71;"

-- | The upper case H  HTML entity ('H').
upperCaseH :: String
upperCaseH = "&#72;"

-- | The upper case I  HTML entity ('I').
upperCaseI :: String
upperCaseI = "&#73;"

-- | The upper case J  HTML entity ('J').
upperCaseJ :: String
upperCaseJ = "&#74;"

-- | The upper case K  HTML entity ('K').
upperCaseK :: String
upperCaseK = "&#75;"

-- | The upper case L  HTML entity ('L').
upperCaseL :: String
upperCaseL = "&#76;"

-- | The upper case M  HTML entity ('M').
upperCaseM :: String
upperCaseM = "&#77;"

-- | The upper case N  HTML entity ('N').
upperCaseN :: String
upperCaseN = "&#78;"

-- | The upper case O  HTML entity ('O').
upperCaseO :: String
upperCaseO = "&#79;"

-- | The upper case P  HTML entity ('P').
upperCaseP :: String
upperCaseP = "&#80;"

-- | The upper case Q  HTML entity ('Q').
upperCaseQ :: String
upperCaseQ = "&#81;"

-- | The upper case R  HTML entity ('R').
upperCaseR :: String
upperCaseR = "&#82;"

-- | The upper case S  HTML entity ('S').
upperCaseS :: String
upperCaseS = "&#83;"

-- | The upper case T  HTML entity ('T').
upperCaseT :: String
upperCaseT = "&#84;"

-- | The upper case U  HTML entity ('U').
upperCaseU :: String
upperCaseU = "&#85;"

-- | The upper case V  HTML entity ('V').
upperCaseV :: String
upperCaseV = "&#86;"

-- | The upper case W  HTML entity ('W').
upperCaseW :: String
upperCaseW = "&#87;"

-- | The upper case X  HTML entity ('X').
upperCaseX :: String
upperCaseX = "&#88;"

-- | The upper case Y  HTML entity ('Y').
upperCaseY :: String
upperCaseY = "&#89;"

-- | The upper case Z  HTML entity ('Z').
upperCaseZ :: String
upperCaseZ = "&#90;"

-- | The left square bracket HTML entity ('[').
leftSquareBracket :: String
leftSquareBracket = "&#91;"

-- | The backslash HTML entity ('\').
backslash :: String
backslash = "&#92;"

-- | The right square bracket HTML entity (']').
rightSquareBracket :: String
rightSquareBracket = "&#93;"

-- | The caret - circumflex HTML entity ('^').
caretCircumflex :: String
caretCircumflex = "&#94;"

-- | The underscore HTML entity ('_').
underscore :: String
underscore = "&#95;"

-- | The grave accent HTML entity ('`').
graveAccent :: String
graveAccent = "&#96;"

-- | The lower case a HTML entity ('a').
lowerCaseA :: String
lowerCaseA = "&#97;"

-- | The lower case b HTML entity ('b').
lowerCaseB :: String
lowerCaseB = "&#98;"

-- | The lower case c HTML entity ('c').
lowerCaseC :: String
lowerCaseC = "&#99;"

-- | The lower case d HTML entity ('d').
lowerCaseD :: String
lowerCaseD = "&#100;"

-- | The lower case e HTML entity ('e').
lowerCaseE :: String
lowerCaseE = "&#101;"

-- | The lower case f HTML entity ('f').
lowerCaseF :: String
lowerCaseF = "&#102;"

-- | The lower case g HTML entity ('g').
lowerCaseG :: String
lowerCaseG = "&#103;"

-- | The lower case h HTML entity ('h').
lowerCaseH :: String
lowerCaseH = "&#104;"

-- | The lower case i HTML entity ('i').
lowerCaseI :: String
lowerCaseI = "&#105;"

-- | The lower case j HTML entity ('j').
lowerCaseJ :: String
lowerCaseJ = "&#106;"

-- | The lower case k HTML entity ('k').
lowerCaseK :: String
lowerCaseK = "&#107;"

-- | The lower case l HTML entity ('l').
lowerCaseL :: String
lowerCaseL = "&#108;"

-- | The lower case m HTML entity ('m').
lowerCaseM :: String
lowerCaseM = "&#109;"

-- | The lower case n HTML entity ('n').
lowerCaseN :: String
lowerCaseN = "&#110;"

-- | The lower case o HTML entity ('o').
lowerCaseO :: String
lowerCaseO = "&#111;"

-- | The lower case p HTML entity ('p').
lowerCaseP :: String
lowerCaseP = "&#112;"

-- | The lower case q HTML entity ('q').
lowerCaseQ :: String
lowerCaseQ = "&#113;"

-- | The lower case r HTML entity ('r').
lowerCaseR :: String
lowerCaseR = "&#114;"

-- | The lower case s HTML entity ('s').
lowerCaseS :: String
lowerCaseS = "&#115;"

-- | The lower case t HTML entity ('t').
lowerCaseT :: String
lowerCaseT = "&#116;"

-- | The lower case u HTML entity ('u').
lowerCaseU :: String
lowerCaseU = "&#117;"

-- | The lower case v HTML entity ('v').
lowerCaseV :: String
lowerCaseV = "&#118;"

-- | The lower case w HTML entity ('w').
lowerCaseW :: String
lowerCaseW = "&#119;"

-- | The lower case x HTML entity ('x').
lowerCaseX :: String
lowerCaseX = "&#120;"

-- | The lower case y HTML entity ('y').
lowerCaseY :: String
lowerCaseY = "&#121;"

-- | The lower case z HTML entity ('z').
lowerCaseZ :: String
lowerCaseZ = "&#122;"

-- | The left curly bracket HTML entity ('{').
leftCurlyBracket :: String
leftCurlyBracket = "&#123;"

-- | The vertical line HTML entity ('|').
verticalLine :: String
verticalLine = "&#124;"

-- | The right curly bracket HTML entity ('}').
rightCurlyBracket :: String
rightCurlyBracket = "&#125;"

-- | The equivalency sign - tilde HTML entity ('~').
equivalencySignTilde :: String
equivalencySignTilde = "&#126;"

-- | The delete (rubout) HTML entity ('^?').
delete :: String
delete = "&#127;"

-- | The Padding Character HTML entity ('Esc@').
paddingCharacter :: String
paddingCharacter = "&#128;"

-- | The High Octet Preset HTML entity ('EscA').
highOctetPreset :: String
highOctetPreset = "&#129;"

-- | The Break Permitted Here HTML entity ('EscB').
breakPermittedHere :: String
breakPermittedHere = "&#130;"

-- | The No Break Here HTML entity ('EscC').
noBreakHere :: String
noBreakHere = "&#131;"

-- | The Index HTML entity ('EscD').
index :: String
index = "&#132;"

-- | The Next Line HTML entity ('EscE').
nextLine :: String
nextLine = "&#133;"

-- | The Start of Selected Area HTML entity ('EscF').
startOfSelectedArea :: String
startOfSelectedArea = "&#134;"

-- | The End of Selected Area HTML entity ('EscG').
endOfSelectedArea :: String
endOfSelectedArea = "&#135;"

-- | The nCharacter Tabulation Set HTML entity ('EscH').
ncharacterTabulationSet :: String
ncharacterTabulationSet = "&#136;"

-- | The Character Tabulation Set with Justification HTML entity ('EscI').
characterTabulationSetWithJustification :: String
characterTabulationSetWithJustification = "&#137;"

-- | The Line Tabulation Set HTML entity ('EscJ').
lineTabulationSet :: String
lineTabulationSet = "&#138;"

-- | The Partial Line Forward HTML entity ('EscK').
partialLineForward :: String
partialLineForward = "&#139;"

-- | The Partial Line Backward HTML entity ('EscL').
partialLineBackward :: String
partialLineBackward = "&#140;"

-- | The Reverse Line Feed HTML entity ('EscM').
reverseLineFeed :: String
reverseLineFeed = "&#141;"

-- | The Single-Shift 2 HTML entity ('EscN').
singleShift2 :: String
singleShift2 = "&#142;"

-- | The Single-Shift 3 HTML entity ('EscO').
singleShift3 :: String
singleShift3 = "&#143;"

-- | The Device Control String HTML entity ('EscP').
deviceControlString :: String
deviceControlString = "&#144;"

-- | The Private Use 1 HTML entity ('EscQ').
privateUse1 :: String
privateUse1 = "&#145;"

-- | The Private Use 2 HTML entity ('EscR').
privateUse2 :: String
privateUse2 = "&#146;"

-- | The Set Transmit State HTML entity ('EscS').
setTransmitState :: String
setTransmitState = "&#147;"

-- | The Cancel character HTML entity ('EscT').
cancelCharacter :: String
cancelCharacter = "&#148;"

-- | The Message Waiting HTML entity ('EscU').
messageWaiting :: String
messageWaiting = "&#149;"

-- | The Start of Protected Area HTML entity ('EscV').
startOfProtectedArea :: String
startOfProtectedArea = "&#150;"

-- | The End of Protected Area HTML entity ('EscW').
endOfProtectedArea :: String
endOfProtectedArea = "&#151;"

-- | The Start of String Followed by a control string terminated by ST (0x9C) HTML entity ('EscX').
startOfStringFollowedByAControlStringTerminatedBySt :: String
startOfStringFollowedByAControlStringTerminatedBySt = "&#152;"

-- | The Single Graphic Character Introducer HTML entity ('EscY').
singleGraphicCharacterIntroducer :: String
singleGraphicCharacterIntroducer = "&#153;"

-- | The Single Character Introducer HTML entity ('EscZ').
singleCharacterIntroducer :: String
singleCharacterIntroducer = "&#154;"

-- | The Control Sequence Introducer HTML entity ('Esc[').
controlSequenceIntroducer :: String
controlSequenceIntroducer = "&#155;"

-- | The String Terminator HTML entity ('Esc\').
stringTerminator :: String
stringTerminator = "&#156;"

-- | The Operating System Command HTML entity ('Esc]').
operatingSystemCommand :: String
operatingSystemCommand = "&#157;"

-- | The Privacy Message	 HTML entity ('Esc^').
privacyMessage :: String
privacyMessage = "&#158;"

-- | The Application Program Command HTML entity ('Esc_').
applicationProgramCommand :: String
applicationProgramCommand = "&#159;"

-- | The no-break space HTML entity.
noBreakSpace :: String
noBreakSpace = "&#160;"

-- | The inverted exclamation mark HTML entity ('¡').
invertedExclamationMark :: String
invertedExclamationMark = "&#161;"

-- | The cent sign HTML entity ('¢').
centSign :: String
centSign = "&#162;"

-- | The pound sign HTML entity ('£').
poundSign :: String
poundSign = "&#163;"

-- | The currency sign HTML entity ('¤').
currencySign :: String
currencySign = "&#164;"

-- | The yen sign HTML entity ('¥').
yenSign :: String
yenSign = "&#165;"

-- | The broken vertical bar HTML entity ('¦').
brokenVerticalBar :: String
brokenVerticalBar = "&#166;"

-- | The section sign HTML entity ('§').
sectionSign :: String
sectionSign = "&#167;"

-- | The diaeresis HTML entity ('¨').
diaeresis :: String
diaeresis = "&#168;"

-- | The copyright sign HTML entity ('©').
copyrightSign :: String
copyrightSign = "&#169;"

-- | The feminine ordinal indicator HTML entity ('ª').
feminineOrdinalIndicator :: String
feminineOrdinalIndicator = "&#170;"

-- | The left double angle quotes HTML entity ('«').
leftDoubleAngleQuotes :: String
leftDoubleAngleQuotes = "&#171;"

-- | The not sign HTML entity ('¬').
notSign :: String
notSign = "&#172;"

-- | The soft hyphen HTML entity ('­').
softHyphen :: String
softHyphen = "&#173;"

-- | The registered sign HTML entity ('®').
registeredSign :: String
registeredSign = "&#174;"

-- | The macron HTML entity ('¯').
macron :: String
macron = "&#175;"

-- | The degree sign HTML entity ('°').
degreeSign :: String
degreeSign = "&#176;"

-- | The plus-minus sign HTML entity ('±').
plusMinusSign :: String
plusMinusSign = "&#177;"

-- | The superscript two - squared HTML entity ('²').
superscriptTwoSquared :: String
superscriptTwoSquared = "&#178;"

-- | The superscript three - cubed HTML entity ('³').
superscriptThreeCubed :: String
superscriptThreeCubed = "&#179;"

-- | The acute accent HTML entity ('´').
acuteAccent :: String
acuteAccent = "&#180;"

-- | The micro sign HTML entity ('µ').
microSign :: String
microSign = "&#181;"

-- | The pilcrow sign - paragraph sign HTML entity ('¶').
pilcrowSignParagraphSign :: String
pilcrowSignParagraphSign = "&#182;"

-- | The middle dot HTML entity ('·').
middleDot :: String
middleDot = "&#183;"

-- | The cedilla HTML entity ('¸').
cedilla :: String
cedilla = "&#184;"

-- | The superscript one HTML entity ('¹').
superscriptOne :: String
superscriptOne = "&#185;"

-- | The masculine ordinal indicator HTML entity ('º').
masculineOrdinalIndicator :: String
masculineOrdinalIndicator = "&#186;"

-- | The right double angle quotes HTML entity ('»').
rightDoubleAngleQuotes :: String
rightDoubleAngleQuotes = "&#187;"

-- | The fraction one quarter HTML entity ('¼').
fractionOneQuarter :: String
fractionOneQuarter = "&#188;"

-- | The vulgar fraction one half HTML entity ('½').
vulgarFractionOneHalf :: String
vulgarFractionOneHalf = "&#189;"

-- | The fraction three quarters HTML entity ('¾').
fractionThreeQuarters :: String
fractionThreeQuarters = "&#190;"

-- | The inverted question mark HTML entity ('¿').
invertedQuestionMark :: String
invertedQuestionMark = "&#191;"

-- | The latin capital letter a with grave HTML entity ('À').
latinCapitalLetterAWithGrave :: String
latinCapitalLetterAWithGrave = "&#192;"

-- | The latin capital letter a with acute HTML entity ('Á').
latinCapitalLetterAWithAcute :: String
latinCapitalLetterAWithAcute = "&#193;"

-- | The latin capital letter a with circumflex HTML entity ('Â').
latinCapitalLetterAWithCircumflex :: String
latinCapitalLetterAWithCircumflex = "&#194;"

-- | The latin capital letter a with tilde HTML entity ('Ã').
latinCapitalLetterAWithTilde :: String
latinCapitalLetterAWithTilde = "&#195;"

-- | The latin capital letter a with diaeresis HTML entity ('Ä').
latinCapitalLetterAWithDiaeresis :: String
latinCapitalLetterAWithDiaeresis = "&#196;"

-- | The latin capital letter a with ring above HTML entity ('Å').
latinCapitalLetterAWithRingAbove :: String
latinCapitalLetterAWithRingAbove = "&#197;"

-- | The latin capital letter ae HTML entity ('Æ').
latinCapitalLetterAe :: String
latinCapitalLetterAe = "&#198;"

-- | The latin capital letter c with cedilla HTML entity ('Ç').
latinCapitalLetterCWithCedilla :: String
latinCapitalLetterCWithCedilla = "&#199;"

-- | The latin capital letter e with grave HTML entity ('È').
latinCapitalLetterEWithGrave :: String
latinCapitalLetterEWithGrave = "&#200;"

-- | The latin capital letter e with acute HTML entity ('É').
latinCapitalLetterEWithAcute :: String
latinCapitalLetterEWithAcute = "&#201;"

-- | The latin capital letter e with circumflex HTML entity ('Ê').
latinCapitalLetterEWithCircumflex :: String
latinCapitalLetterEWithCircumflex = "&#202;"

-- | The latin capital letter e with diaeresis HTML entity ('Ë').
latinCapitalLetterEWithDiaeresis :: String
latinCapitalLetterEWithDiaeresis = "&#203;"

-- | The latin capital letter i with grave HTML entity ('Ì').
latinCapitalLetterIWithGrave :: String
latinCapitalLetterIWithGrave = "&#204;"

-- | The latin capital letter i with acute HTML entity ('Í').
latinCapitalLetterIWithAcute :: String
latinCapitalLetterIWithAcute = "&#205;"

-- | The latin capital letter i with circumflex HTML entity ('Î').
latinCapitalLetterIWithCircumflex :: String
latinCapitalLetterIWithCircumflex = "&#206;"

-- | The latin capital letter i with diaeresis HTML entity ('Ï').
latinCapitalLetterIWithDiaeresis :: String
latinCapitalLetterIWithDiaeresis = "&#207;"

-- | The latin capital letter eth HTML entity ('Ð').
latinCapitalLetterEth :: String
latinCapitalLetterEth = "&#208;"

-- | The latin capital letter n with tilde HTML entity ('Ñ').
latinCapitalLetterNWithTilde :: String
latinCapitalLetterNWithTilde = "&#209;"

-- | The latin capital letter o with grave HTML entity ('Ò').
latinCapitalLetterOWithGrave :: String
latinCapitalLetterOWithGrave = "&#210;"

-- | The latin capital letter o with acute HTML entity ('Ó').
latinCapitalLetterOWithAcute :: String
latinCapitalLetterOWithAcute = "&#211;"

-- | The latin capital letter o with circumflex HTML entity ('Ô').
latinCapitalLetterOWithCircumflex :: String
latinCapitalLetterOWithCircumflex = "&#212;"

-- | The latin capital letter o with tilde HTML entity ('Õ').
latinCapitalLetterOWithTilde :: String
latinCapitalLetterOWithTilde = "&#213;"

-- | The latin capital letter o with diaeresis HTML entity ('Ö').
latinCapitalLetterOWithDiaeresis :: String
latinCapitalLetterOWithDiaeresis = "&#214;"

-- | The multiplication sign HTML entity ('×').
multiplicationSign :: String
multiplicationSign = "&#215;"

-- | The latin capital letter o with slash HTML entity ('Ø').
latinCapitalLetterOWithSlash :: String
latinCapitalLetterOWithSlash = "&#216;"

-- | The latin capital letter u with grave HTML entity ('Ù').
latinCapitalLetterUWithGrave :: String
latinCapitalLetterUWithGrave = "&#217;"

-- | The latin capital letter u with acute HTML entity ('Ú').
latinCapitalLetterUWithAcute :: String
latinCapitalLetterUWithAcute = "&#218;"

-- | The latin capital letter u with circumflex HTML entity ('Û').
latinCapitalLetterUWithCircumflex :: String
latinCapitalLetterUWithCircumflex = "&#219;"

-- | The latin capital letter u with diaeresis HTML entity ('Ü').
latinCapitalLetterUWithDiaeresis :: String
latinCapitalLetterUWithDiaeresis = "&#220;"

-- | The latin capital letter y with acute HTML entity ('Ý').
latinCapitalLetterYWithAcute :: String
latinCapitalLetterYWithAcute = "&#221;"

-- | The latin capital letter thorn HTML entity ('Þ').
latinCapitalLetterThorn :: String
latinCapitalLetterThorn = "&#222;"

-- | The latin small letter sharp s - ess-zed HTML entity ('ß').
latinSmallLetterSharpSEssZed :: String
latinSmallLetterSharpSEssZed = "&#223;"

-- | The latin small letter a with grave HTML entity ('à').
latinSmallLetterAWithGrave :: String
latinSmallLetterAWithGrave = "&#224;"

-- | The latin small letter a with acute HTML entity ('á').
latinSmallLetterAWithAcute :: String
latinSmallLetterAWithAcute = "&#225;"

-- | The latin small letter a with circumflex HTML entity ('â').
latinSmallLetterAWithCircumflex :: String
latinSmallLetterAWithCircumflex = "&#226;"

-- | The latin small letter a with tilde HTML entity ('ã').
latinSmallLetterAWithTilde :: String
latinSmallLetterAWithTilde = "&#227;"

-- | The latin small letter a with diaeresis HTML entity ('ä').
latinSmallLetterAWithDiaeresis :: String
latinSmallLetterAWithDiaeresis = "&#228;"

-- | The latin small letter a with ring above HTML entity ('å').
latinSmallLetterAWithRingAbove :: String
latinSmallLetterAWithRingAbove = "&#229;"

-- | The latin small letter ae HTML entity ('æ').
latinSmallLetterAe :: String
latinSmallLetterAe = "&#230;"

-- | The latin small letter c with cedilla HTML entity ('ç').
latinSmallLetterCWithCedilla :: String
latinSmallLetterCWithCedilla = "&#231;"

-- | The latin small letter e with grave HTML entity ('è').
latinSmallLetterEWithGrave :: String
latinSmallLetterEWithGrave = "&#232;"

-- | The latin small letter e with acute HTML entity ('é').
latinSmallLetterEWithAcute :: String
latinSmallLetterEWithAcute = "&#233;"

-- | The latin small letter e with circumflex HTML entity ('ê').
latinSmallLetterEWithCircumflex :: String
latinSmallLetterEWithCircumflex = "&#234;"

-- | The latin small letter e with diaeresis HTML entity ('ë').
latinSmallLetterEWithDiaeresis :: String
latinSmallLetterEWithDiaeresis = "&#235;"

-- | The latin small letter i with grave HTML entity ('ì').
latinSmallLetterIWithGrave :: String
latinSmallLetterIWithGrave = "&#236;"

-- | The latin small letter i with acute HTML entity ('í').
latinSmallLetterIWithAcute :: String
latinSmallLetterIWithAcute = "&#237;"

-- | The latin small letter i with circumflex HTML entity ('î').
latinSmallLetterIWithCircumflex :: String
latinSmallLetterIWithCircumflex = "&#238;"

-- | The latin small letter i with diaeresis HTML entity ('ï').
latinSmallLetterIWithDiaeresis :: String
latinSmallLetterIWithDiaeresis = "&#239;"

-- | The latin small letter eth HTML entity ('ð').
latinSmallLetterEth :: String
latinSmallLetterEth = "&#240;"

-- | The latin small letter n with tilde HTML entity ('ñ').
latinSmallLetterNWithTilde :: String
latinSmallLetterNWithTilde = "&#241;"

-- | The latin small letter o with grave HTML entity ('ò').
latinSmallLetterOWithGrave :: String
latinSmallLetterOWithGrave = "&#242;"

-- | The latin small letter o with acute HTML entity ('ó').
latinSmallLetterOWithAcute :: String
latinSmallLetterOWithAcute = "&#243;"

-- | The latin small letter o with circumflex HTML entity ('ô').
latinSmallLetterOWithCircumflex :: String
latinSmallLetterOWithCircumflex = "&#244;"

-- | The latin small letter o with tilde HTML entity ('õ').
latinSmallLetterOWithTilde :: String
latinSmallLetterOWithTilde = "&#245;"

-- | The latin small letter o with diaeresis HTML entity ('ö').
latinSmallLetterOWithDiaeresis :: String
latinSmallLetterOWithDiaeresis = "&#246;"

-- | The division sign HTML entity ('÷').
divisionSign :: String
divisionSign = "&#247;"

-- | The latin small letter o with slash HTML entity ('ø').
latinSmallLetterOWithSlash :: String
latinSmallLetterOWithSlash = "&#248;"

-- | The latin small letter u with grave HTML entity ('ù').
latinSmallLetterUWithGrave :: String
latinSmallLetterUWithGrave = "&#249;"

-- | The latin small letter u with acute HTML entity ('ú').
latinSmallLetterUWithAcute :: String
latinSmallLetterUWithAcute = "&#250;"

-- | The latin small letter u with circumflex HTML entity ('û').
latinSmallLetterUWithCircumflex :: String
latinSmallLetterUWithCircumflex = "&#251;"

-- | The latin small letter u with diaeresis HTML entity ('ü').
latinSmallLetterUWithDiaeresis :: String
latinSmallLetterUWithDiaeresis = "&#252;"

-- | The latin small letter y with acute HTML entity ('ý').
latinSmallLetterYWithAcute :: String
latinSmallLetterYWithAcute = "&#253;"

-- | The latin small letter thorn HTML entity ('þ').
latinSmallLetterThorn :: String
latinSmallLetterThorn = "&#254;"

-- | The latin small letter y with diaeresis HTML entity ('ÿ').
latinSmallLetterYWithDiaeresis :: String
latinSmallLetterYWithDiaeresis = "&#255;"

-- | The latin capital letter a with macron HTML entity ('Ā').
latinCapitalLetterAWithMacron :: String
latinCapitalLetterAWithMacron = "&#256;"

-- | The latin small letter a with macron HTML entity ('ā').
latinSmallLetterAWithMacron :: String
latinSmallLetterAWithMacron = "&#257;"

-- | The latin capital letter a with breve HTML entity ('Ă').
latinCapitalLetterAWithBreve :: String
latinCapitalLetterAWithBreve = "&#258;"

-- | The latin small letter a with breve HTML entity ('ă').
latinSmallLetterAWithBreve :: String
latinSmallLetterAWithBreve = "&#259;"

-- | The latin capital letter a with ogonek HTML entity ('Ą').
latinCapitalLetterAWithOgonek :: String
latinCapitalLetterAWithOgonek = "&#260;"

-- | The latin small letter a with ogonek HTML entity ('ą').
latinSmallLetterAWithOgonek :: String
latinSmallLetterAWithOgonek = "&#261;"

-- | The latin capital letter c with acute HTML entity ('Ć').
latinCapitalLetterCWithAcute :: String
latinCapitalLetterCWithAcute = "&#262;"

-- | The latin small letter c with acute HTML entity ('ć').
latinSmallLetterCWithAcute :: String
latinSmallLetterCWithAcute = "&#263;"

-- | The latin capital letter c with circumflex HTML entity ('Ĉ').
latinCapitalLetterCWithCircumflex :: String
latinCapitalLetterCWithCircumflex = "&#264;"

-- | The latin small letter c with circumflex HTML entity ('ĉ').
latinSmallLetterCWithCircumflex :: String
latinSmallLetterCWithCircumflex = "&#265;"

-- | The latin capital letter c with dot above HTML entity ('Ċ').
latinCapitalLetterCWithDotAbove :: String
latinCapitalLetterCWithDotAbove = "&#266;"

-- | The latin small letter c with dot above HTML entity ('ċ').
latinSmallLetterCWithDotAbove :: String
latinSmallLetterCWithDotAbove = "&#267;"

-- | The latin capital letter c with caron HTML entity ('Č').
latinCapitalLetterCWithCaron :: String
latinCapitalLetterCWithCaron = "&#268;"

-- | The latin small letter c with caron HTML entity ('č').
latinSmallLetterCWithCaron :: String
latinSmallLetterCWithCaron = "&#269;"

-- | The latin capital letter d with caron HTML entity ('Ď').
latinCapitalLetterDWithCaron :: String
latinCapitalLetterDWithCaron = "&#270;"

-- | The latin small letter d with caron HTML entity ('ď').
latinSmallLetterDWithCaron :: String
latinSmallLetterDWithCaron = "&#271;"

-- | The latin capital letter d with stroke HTML entity ('Đ').
latinCapitalLetterDWithStroke :: String
latinCapitalLetterDWithStroke = "&#272;"

-- | The latin small letter d with stroke HTML entity ('đ').
latinSmallLetterDWithStroke :: String
latinSmallLetterDWithStroke = "&#273;"

-- | The latin capital letter e with macron HTML entity ('Ē').
latinCapitalLetterEWithMacron :: String
latinCapitalLetterEWithMacron = "&#274;"

-- | The latin small letter e with macron HTML entity ('ē').
latinSmallLetterEWithMacron :: String
latinSmallLetterEWithMacron = "&#275;"

-- | The latin capital letter e with dot above HTML entity ('Ė').
latinCapitalLetterEWithDotAbove :: String
latinCapitalLetterEWithDotAbove = "&#278;"

-- | The latin small letter e with dot above HTML entity ('ė').
latinSmallLetterEWithDotAbove :: String
latinSmallLetterEWithDotAbove = "&#279;"

-- | The latin capital letter e with ogonek HTML entity ('Ę').
latinCapitalLetterEWithOgonek :: String
latinCapitalLetterEWithOgonek = "&#280;"

-- | The latin small letter e with ogonek HTML entity ('ę').
latinSmallLetterEWithOgonek :: String
latinSmallLetterEWithOgonek = "&#281;"

-- | The latin capital letter e with caron HTML entity ('Ě').
latinCapitalLetterEWithCaron :: String
latinCapitalLetterEWithCaron = "&#282;"

-- | The latin small letter e with caron HTML entity ('ě').
latinSmallLetterEWithCaron :: String
latinSmallLetterEWithCaron = "&#283;"

-- | The latin capital letter g with circumflex HTML entity ('Ĝ').
latinCapitalLetterGWithCircumflex :: String
latinCapitalLetterGWithCircumflex = "&#284;"

-- | The latin small letter g with circumflex HTML entity ('ĝ').
latinSmallLetterGWithCircumflex :: String
latinSmallLetterGWithCircumflex = "&#285;"

-- | The latin capital letter g with breve HTML entity ('Ğ').
latinCapitalLetterGWithBreve :: String
latinCapitalLetterGWithBreve = "&#286;"

-- | The latin small letter g with breve HTML entity ('ğ').
latinSmallLetterGWithBreve :: String
latinSmallLetterGWithBreve = "&#287;"

-- | The latin capital letter g with dot above HTML entity ('Ġ').
latinCapitalLetterGWithDotAbove :: String
latinCapitalLetterGWithDotAbove = "&#288;"

-- | The latin small letter g with dot above HTML entity ('ġ').
latinSmallLetterGWithDotAbove :: String
latinSmallLetterGWithDotAbove = "&#289;"

-- | The latin capital letter g with cedilla HTML entity ('Ģ').
latinCapitalLetterGWithCedilla :: String
latinCapitalLetterGWithCedilla = "&#290;"

-- | The latin capital letter h with circumflex HTML entity ('Ĥ').
latinCapitalLetterHWithCircumflex :: String
latinCapitalLetterHWithCircumflex = "&#292;"

-- | The latin small letter h with circumflex HTML entity ('ĥ').
latinSmallLetterHWithCircumflex :: String
latinSmallLetterHWithCircumflex = "&#293;"

-- | The latin capital letter h with stroke HTML entity ('Ħ').
latinCapitalLetterHWithStroke :: String
latinCapitalLetterHWithStroke = "&#294;"

-- | The latin small letter h with stroke HTML entity ('ħ').
latinSmallLetterHWithStroke :: String
latinSmallLetterHWithStroke = "&#295;"

-- | The latin capital letter i with tilde HTML entity ('Ĩ').
latinCapitalLetterIWithTilde :: String
latinCapitalLetterIWithTilde = "&#296;"

-- | The latin small letter i with tilde HTML entity ('ĩ').
latinSmallLetterIWithTilde :: String
latinSmallLetterIWithTilde = "&#297;"

-- | The latin capital letter i with macron HTML entity ('Ī').
latinCapitalLetterIWithMacron :: String
latinCapitalLetterIWithMacron = "&#298;"

-- | The latin small letter i with macron HTML entity ('ī').
latinSmallLetterIWithMacron :: String
latinSmallLetterIWithMacron = "&#299;"

-- | The latin capital letter i with ogonek HTML entity ('Į').
latinCapitalLetterIWithOgonek :: String
latinCapitalLetterIWithOgonek = "&#302;"

-- | The latin small letter i with ogonek HTML entity ('į').
latinSmallLetterIWithOgonek :: String
latinSmallLetterIWithOgonek = "&#303;"

-- | The latin capital letter i with dot above HTML entity ('İ').
latinCapitalLetterIWithDotAbove :: String
latinCapitalLetterIWithDotAbove = "&#304;"

-- | The latin small letter dotless i HTML entity ('ı').
latinSmallLetterDotlessI :: String
latinSmallLetterDotlessI = "&#305;"

-- | The latin capital ligature ij HTML entity ('Ĳ').
latinCapitalLigatureIj :: String
latinCapitalLigatureIj = "&#306;"

-- | The latin small ligature ij HTML entity ('ĳ').
latinSmallLigatureIj :: String
latinSmallLigatureIj = "&#307;"

-- | The latin capital letter j with circumflex HTML entity ('Ĵ').
latinCapitalLetterJWithCircumflex :: String
latinCapitalLetterJWithCircumflex = "&#308;"

-- | The latin small letter j with circumflex HTML entity ('ĵ').
latinSmallLetterJWithCircumflex :: String
latinSmallLetterJWithCircumflex = "&#309;"

-- | The latin capital letter k with cedilla HTML entity ('Ķ').
latinCapitalLetterKWithCedilla :: String
latinCapitalLetterKWithCedilla = "&#310;"

-- | The latin small letter k with cedilla HTML entity ('ķ').
latinSmallLetterKWithCedilla :: String
latinSmallLetterKWithCedilla = "&#311;"

-- | The latin small letter kra HTML entity ('ĸ').
latinSmallLetterKra :: String
latinSmallLetterKra = "&#312;"

-- | The latin capital letter l with acute HTML entity ('Ĺ').
latinCapitalLetterLWithAcute :: String
latinCapitalLetterLWithAcute = "&#313;"

-- | The latin small letter l with acute HTML entity ('ĺ').
latinSmallLetterLWithAcute :: String
latinSmallLetterLWithAcute = "&#314;"

-- | The latin capital letter l with cedilla HTML entity ('Ļ').
latinCapitalLetterLWithCedilla :: String
latinCapitalLetterLWithCedilla = "&#315;"

-- | The latin small letter l with cedilla HTML entity ('ļ').
latinSmallLetterLWithCedilla :: String
latinSmallLetterLWithCedilla = "&#316;"

-- | The latin capital letter l with caron HTML entity ('Ľ').
latinCapitalLetterLWithCaron :: String
latinCapitalLetterLWithCaron = "&#317;"

-- | The latin small letter l with caron HTML entity ('ľ').
latinSmallLetterLWithCaron :: String
latinSmallLetterLWithCaron = "&#318;"

-- | The latin capital letter l with middle dot HTML entity ('Ŀ').
latinCapitalLetterLWithMiddleDot :: String
latinCapitalLetterLWithMiddleDot = "&#319;"

-- | The latin small letter l with middle dot HTML entity ('ŀ').
latinSmallLetterLWithMiddleDot :: String
latinSmallLetterLWithMiddleDot = "&#320;"

-- | The latin capital letter l with stroke HTML entity ('Ł').
latinCapitalLetterLWithStroke :: String
latinCapitalLetterLWithStroke = "&#321;"

-- | The latin small letter l with stroke HTML entity ('ł').
latinSmallLetterLWithStroke :: String
latinSmallLetterLWithStroke = "&#322;"

-- | The latin capital letter n with acute HTML entity ('Ń').
latinCapitalLetterNWithAcute :: String
latinCapitalLetterNWithAcute = "&#323;"

-- | The latin small letter n with acute HTML entity ('ń').
latinSmallLetterNWithAcute :: String
latinSmallLetterNWithAcute = "&#324;"

-- | The latin capital letter n with cedilla HTML entity ('Ņ').
latinCapitalLetterNWithCedilla :: String
latinCapitalLetterNWithCedilla = "&#325;"

-- | The latin small letter n with cedilla HTML entity ('ņ').
latinSmallLetterNWithCedilla :: String
latinSmallLetterNWithCedilla = "&#326;"

-- | The latin capital letter n with caron HTML entity ('Ň').
latinCapitalLetterNWithCaron :: String
latinCapitalLetterNWithCaron = "&#327;"

-- | The latin small letter n with caron HTML entity ('ň').
latinSmallLetterNWithCaron :: String
latinSmallLetterNWithCaron = "&#328;"

-- | The latin small letter n preceded by apostrophe HTML entity ('ŉ').
latinSmallLetterNPrecededByApostrophe :: String
latinSmallLetterNPrecededByApostrophe = "&#329;"

-- | The latin capital letter eng HTML entity ('Ŋ').
latinCapitalLetterEng :: String
latinCapitalLetterEng = "&#330;"

-- | The latin small letter eng HTML entity ('ŋ').
latinSmallLetterEng :: String
latinSmallLetterEng = "&#331;"

-- | The latin capital letter o with macron HTML entity ('Ō').
latinCapitalLetterOWithMacron :: String
latinCapitalLetterOWithMacron = "&#332;"

-- | The latin small letter o with macron HTML entity ('ō').
latinSmallLetterOWithMacron :: String
latinSmallLetterOWithMacron = "&#333;"

-- | The latin capital letter o with double acute HTML entity ('Ő').
latinCapitalLetterOWithDoubleAcute :: String
latinCapitalLetterOWithDoubleAcute = "&#336;"

-- | The latin small letter o with double acute HTML entity ('ő').
latinSmallLetterOWithDoubleAcute :: String
latinSmallLetterOWithDoubleAcute = "&#337;"

-- | The latin capital letter oe HTML entity ('Œ').
latinCapitalLetterOe :: String
latinCapitalLetterOe = "&#338;"

-- | The latin small letter oe HTML entity ('œ').
latinSmallLetterOe :: String
latinSmallLetterOe = "&#339;"

-- | The latin capital letter r with acute HTML entity ('Ŕ').
latinCapitalLetterRWithAcute :: String
latinCapitalLetterRWithAcute = "&#340;"

-- | The latin small letter r with acute HTML entity ('ŕ').
latinSmallLetterRWithAcute :: String
latinSmallLetterRWithAcute = "&#341;"

-- | The latin capital letter r with cedilla HTML entity ('Ŗ').
latinCapitalLetterRWithCedilla :: String
latinCapitalLetterRWithCedilla = "&#342;"

-- | The latin small letter r with cedilla HTML entity ('ŗ').
latinSmallLetterRWithCedilla :: String
latinSmallLetterRWithCedilla = "&#343;"

-- | The latin capital letter r with caron HTML entity ('Ř').
latinCapitalLetterRWithCaron :: String
latinCapitalLetterRWithCaron = "&#344;"

-- | The latin small letter r with caron HTML entity ('ř').
latinSmallLetterRWithCaron :: String
latinSmallLetterRWithCaron = "&#345;"

-- | The latin capital letter s with acute HTML entity ('Ś').
latinCapitalLetterSWithAcute :: String
latinCapitalLetterSWithAcute = "&#346;"

-- | The latin small letter s with acute HTML entity ('ś').
latinSmallLetterSWithAcute :: String
latinSmallLetterSWithAcute = "&#347;"

-- | The latin capital letter s with circumflex HTML entity ('Ŝ').
latinCapitalLetterSWithCircumflex :: String
latinCapitalLetterSWithCircumflex = "&#348;"

-- | The latin small letter s with circumflex HTML entity ('ŝ').
latinSmallLetterSWithCircumflex :: String
latinSmallLetterSWithCircumflex = "&#349;"

-- | The latin capital letter s with cedilla HTML entity ('Ş').
latinCapitalLetterSWithCedilla :: String
latinCapitalLetterSWithCedilla = "&#350;"

-- | The latin small letter s with cedilla HTML entity ('ş').
latinSmallLetterSWithCedilla :: String
latinSmallLetterSWithCedilla = "&#351;"

-- | The latin capital letter s with caron HTML entity ('Š').
latinCapitalLetterSWithCaron :: String
latinCapitalLetterSWithCaron = "&#352;"

-- | The latin small letter s with caron HTML entity ('š').
latinSmallLetterSWithCaron :: String
latinSmallLetterSWithCaron = "&#353;"

-- | The latin capital letter t with cedilla HTML entity ('Ţ').
latinCapitalLetterTWithCedilla :: String
latinCapitalLetterTWithCedilla = "&#354;"

-- | The latin small letter t with cedilla HTML entity ('ţ').
latinSmallLetterTWithCedilla :: String
latinSmallLetterTWithCedilla = "&#355;"

-- | The latin capital letter t with caron HTML entity ('Ť').
latinCapitalLetterTWithCaron :: String
latinCapitalLetterTWithCaron = "&#356;"

-- | The latin small letter t with caron HTML entity ('ť').
latinSmallLetterTWithCaron :: String
latinSmallLetterTWithCaron = "&#357;"

-- | The latin capital letter t with stroke HTML entity ('Ŧ').
latinCapitalLetterTWithStroke :: String
latinCapitalLetterTWithStroke = "&#358;"

-- | The latin small letter t with stroke HTML entity ('ŧ').
latinSmallLetterTWithStroke :: String
latinSmallLetterTWithStroke = "&#359;"

-- | The latin capital letter u with tilde HTML entity ('Ũ').
latinCapitalLetterUWithTilde :: String
latinCapitalLetterUWithTilde = "&#360;"

-- | The latin small letter u with tilde HTML entity ('ũ').
latinSmallLetterUWithTilde :: String
latinSmallLetterUWithTilde = "&#361;"

-- | The latin capital letter u with macron HTML entity ('Ū').
latinCapitalLetterUWithMacron :: String
latinCapitalLetterUWithMacron = "&#362;"

-- | The latin small letter u with macron HTML entity ('ū').
latinSmallLetterUWithMacron :: String
latinSmallLetterUWithMacron = "&#363;"

-- | The latin capital letter u with breve HTML entity ('Ŭ').
latinCapitalLetterUWithBreve :: String
latinCapitalLetterUWithBreve = "&#364;"

-- | The latin small letter u with breve HTML entity ('ŭ').
latinSmallLetterUWithBreve :: String
latinSmallLetterUWithBreve = "&#365;"

-- | The latin capital letter u with ring above HTML entity ('Ů').
latinCapitalLetterUWithRingAbove :: String
latinCapitalLetterUWithRingAbove = "&#366;"

-- | The latin small letter u with ring above HTML entity ('ů').
latinSmallLetterUWithRingAbove :: String
latinSmallLetterUWithRingAbove = "&#367;"

-- | The latin capital letter u with double acute HTML entity ('Ű').
latinCapitalLetterUWithDoubleAcute :: String
latinCapitalLetterUWithDoubleAcute = "&#368;"

-- | The latin small letter u with double acute HTML entity ('ű').
latinSmallLetterUWithDoubleAcute :: String
latinSmallLetterUWithDoubleAcute = "&#369;"

-- | The latin capital letter u with ogonek HTML entity ('Ų').
latinCapitalLetterUWithOgonek :: String
latinCapitalLetterUWithOgonek = "&#370;"

-- | The latin small letter u with ogonek HTML entity ('ų').
latinSmallLetterUWithOgonek :: String
latinSmallLetterUWithOgonek = "&#371;"

-- | The latin capital letter w with circumflex HTML entity ('Ŵ').
latinCapitalLetterWWithCircumflex :: String
latinCapitalLetterWWithCircumflex = "&#372;"

-- | The latin small letter w with circumflex HTML entity ('ŵ').
latinSmallLetterWWithCircumflex :: String
latinSmallLetterWWithCircumflex = "&#373;"

-- | The latin capital letter y with circumflex HTML entity ('Ŷ').
latinCapitalLetterYWithCircumflex :: String
latinCapitalLetterYWithCircumflex = "&#374;"

-- | The latin small letter y with circumflex HTML entity ('ŷ').
latinSmallLetterYWithCircumflex :: String
latinSmallLetterYWithCircumflex = "&#375;"

-- | The latin capital letter y with diaeresis HTML entity ('Ÿ').
latinCapitalLetterYWithDiaeresis :: String
latinCapitalLetterYWithDiaeresis = "&#376;"

-- | The latin capital letter z with acute HTML entity ('Ź').
latinCapitalLetterZWithAcute :: String
latinCapitalLetterZWithAcute = "&#377;"

-- | The latin small letter z with acute HTML entity ('ź').
latinSmallLetterZWithAcute :: String
latinSmallLetterZWithAcute = "&#378;"

-- | The latin capital letter z with dot above HTML entity ('Ż').
latinCapitalLetterZWithDotAbove :: String
latinCapitalLetterZWithDotAbove = "&#379;"

-- | The latin small letter z with dot above HTML entity ('ż').
latinSmallLetterZWithDotAbove :: String
latinSmallLetterZWithDotAbove = "&#380;"

-- | The latin capital letter z with caron HTML entity ('Ž').
latinCapitalLetterZWithCaron :: String
latinCapitalLetterZWithCaron = "&#381;"

-- | The latin small letter z with caron HTML entity ('ž').
latinSmallLetterZWithCaron :: String
latinSmallLetterZWithCaron = "&#382;"

-- | The Latin Small Letter Turned Delta HTML entity ('ƍ').
latinSmallLetterTurnedDelta :: String
latinSmallLetterTurnedDelta = "&#397;"

-- | The latin small f with hook - function  florin HTML entity ('ƒ').
latinSmallFWithHookFunctionFlorin :: String
latinSmallFWithHookFunctionFlorin = "&#402;"

-- | The latin capital letter z with stroke HTML entity ('Ƶ').
latinCapitalLetterZWithStroke :: String
latinCapitalLetterZWithStroke = "&#437;"

-- | The latin small letter g with acute HTML entity ('ǵ').
latinSmallLetterGWithAcute :: String
latinSmallLetterGWithAcute = "&#501;"

-- | The latin small letter dotless j HTML entity ('ȷ').
latinSmallLetterDotlessJ :: String
latinSmallLetterDotlessJ = "&#567;"

-- | The modifier letter circumflex accent HTML entity ('ˆ').
modifierLetterCircumflexAccent :: String
modifierLetterCircumflexAccent = "&#710;"

-- | The caron HTML entity ('ˇ').
caron :: String
caron = "&#711;"

-- | The Modifier Letter Plus Sign HTML entity ('˖').
modifierLetterPlusSign :: String
modifierLetterPlusSign = "&#726;"

-- | The Modifier Letter Minus Sign HTML entity ('˗').
modifierLetterMinusSign :: String
modifierLetterMinusSign = "&#727;"

-- | The breve HTML entity ('˘').
breve :: String
breve = "&#728;"

-- | The dot above HTML entity ('˙').
dotAbove :: String
dotAbove = "&#729;"

-- | The ring above HTML entity ('˚').
ringAbove :: String
ringAbove = "&#730;"

-- | The ogonek HTML entity ('˛').
ogonek :: String
ogonek = "&#731;"

-- | The small tilde HTML entity ('˜').
smallTilde :: String
smallTilde = "&#732;"

-- | The double acute accent HTML entity ('˝').
doubleAcuteAccent :: String
doubleAcuteAccent = "&#733;"

-- | The combining inverted breve HTML entity ('̑').
combiningInvertedBreve :: String
combiningInvertedBreve = "&#785;"

-- | The Combining Left Angle Above HTML entity ('̚').
combiningLeftAngleAbove :: String
combiningLeftAngleAbove = "&#794;"

-- | The Combining Plus Sign Below HTML entity ('̟').
combiningPlusSignBelow :: String
combiningPlusSignBelow = "&#799;"

-- | The Combining Minus Sign Below HTML entity ('̠').
combiningMinusSignBelow :: String
combiningMinusSignBelow = "&#800;"

-- | The combining low line HTML entity ('̲').
combiningLowLine :: String
combiningLowLine = "&#818;"

-- | The Combining Equals Sign Below HTML entity ('͇').
combiningEqualsSignBelow :: String
combiningEqualsSignBelow = "&#839;"

-- | The Combining Left Angle Below HTML entity ('͉').
combiningLeftAngleBelow :: String
combiningLeftAngleBelow = "&#841;"

-- | The Combining Almost Equal To Above HTML entity ('͌').
combiningAlmostEqualToAbove :: String
combiningAlmostEqualToAbove = "&#844;"

-- | The greek capital letter alpha HTML entity ('Α').
greekCapitalLetterAlpha :: String
greekCapitalLetterAlpha = "&#913;"

-- | The greek capital letter beta HTML entity ('Β').
greekCapitalLetterBeta :: String
greekCapitalLetterBeta = "&#914;"

-- | The greek capital letter gamma HTML entity ('Γ').
greekCapitalLetterGamma :: String
greekCapitalLetterGamma = "&#915;"

-- | The greek capital letter delta HTML entity ('Δ').
greekCapitalLetterDelta :: String
greekCapitalLetterDelta = "&#916;"

-- | The greek capital letter epsilon HTML entity ('Ε').
greekCapitalLetterEpsilon :: String
greekCapitalLetterEpsilon = "&#917;"

-- | The greek capital letter zeta HTML entity ('Ζ').
greekCapitalLetterZeta :: String
greekCapitalLetterZeta = "&#918;"

-- | The greek capital letter eta HTML entity ('Η').
greekCapitalLetterEta :: String
greekCapitalLetterEta = "&#919;"

-- | The greek capital letter theta HTML entity ('Θ').
greekCapitalLetterTheta :: String
greekCapitalLetterTheta = "&#920;"

-- | The greek capital letter iota HTML entity ('Ι').
greekCapitalLetterIota :: String
greekCapitalLetterIota = "&#921;"

-- | The greek capital letter kappa HTML entity ('Κ').
greekCapitalLetterKappa :: String
greekCapitalLetterKappa = "&#922;"

-- | The greek capital letter lambda HTML entity ('Λ').
greekCapitalLetterLambda :: String
greekCapitalLetterLambda = "&#923;"

-- | The greek capital letter mu HTML entity ('Μ').
greekCapitalLetterMu :: String
greekCapitalLetterMu = "&#924;"

-- | The greek capital letter nu HTML entity ('Ν').
greekCapitalLetterNu :: String
greekCapitalLetterNu = "&#925;"

-- | The greek capital letter xi HTML entity ('Ξ').
greekCapitalLetterXi :: String
greekCapitalLetterXi = "&#926;"

-- | The greek capital letter omicron HTML entity ('Ο').
greekCapitalLetterOmicron :: String
greekCapitalLetterOmicron = "&#927;"

-- | The greek capital letter pi HTML entity ('Π').
greekCapitalLetterPi :: String
greekCapitalLetterPi = "&#928;"

-- | The greek capital letter rho HTML entity ('Ρ').
greekCapitalLetterRho :: String
greekCapitalLetterRho = "&#929;"

-- | The greek capital letter sigma HTML entity ('Σ').
greekCapitalLetterSigma :: String
greekCapitalLetterSigma = "&#931;"

-- | The greek capital letter tau HTML entity ('Τ').
greekCapitalLetterTau :: String
greekCapitalLetterTau = "&#932;"

-- | The greek capital letter upsilon HTML entity ('Υ').
greekCapitalLetterUpsilon :: String
greekCapitalLetterUpsilon = "&#933;"

-- | The greek capital letter phi HTML entity ('Φ').
greekCapitalLetterPhi :: String
greekCapitalLetterPhi = "&#934;"

-- | The greek capital letter chi HTML entity ('Χ').
greekCapitalLetterChi :: String
greekCapitalLetterChi = "&#935;"

-- | The greek capital letter psi HTML entity ('Ψ').
greekCapitalLetterPsi :: String
greekCapitalLetterPsi = "&#936;"

-- | The greek capital letter omega HTML entity ('Ω').
greekCapitalLetterOmega :: String
greekCapitalLetterOmega = "&#937;"

-- | The greek small letter alpha HTML entity ('α').
greekSmallLetterAlpha :: String
greekSmallLetterAlpha = "&#945;"

-- | The greek small letter beta HTML entity ('β').
greekSmallLetterBeta :: String
greekSmallLetterBeta = "&#946;"

-- | The greek small letter gamma HTML entity ('γ').
greekSmallLetterGamma :: String
greekSmallLetterGamma = "&#947;"

-- | The greek small letter delta HTML entity ('δ').
greekSmallLetterDelta :: String
greekSmallLetterDelta = "&#948;"

-- | The greek small letter epsilon HTML entity ('ε').
greekSmallLetterEpsilon :: String
greekSmallLetterEpsilon = "&#949;"

-- | The greek small letter zeta HTML entity ('ζ').
greekSmallLetterZeta :: String
greekSmallLetterZeta = "&#950;"

-- | The greek small letter eta HTML entity ('η').
greekSmallLetterEta :: String
greekSmallLetterEta = "&#951;"

-- | The greek small letter theta HTML entity ('θ').
greekSmallLetterTheta :: String
greekSmallLetterTheta = "&#952;"

-- | The greek small letter iota HTML entity ('ι').
greekSmallLetterIota :: String
greekSmallLetterIota = "&#953;"

-- | The greek small letter kappa HTML entity ('κ').
greekSmallLetterKappa :: String
greekSmallLetterKappa = "&#954;"

-- | The greek small letter lambda HTML entity ('λ').
greekSmallLetterLambda :: String
greekSmallLetterLambda = "&#955;"

-- | The greek small letter mu HTML entity ('μ').
greekSmallLetterMu :: String
greekSmallLetterMu = "&#956;"

-- | The greek small letter nu HTML entity ('ν').
greekSmallLetterNu :: String
greekSmallLetterNu = "&#957;"

-- | The greek small letter xi HTML entity ('ξ').
greekSmallLetterXi :: String
greekSmallLetterXi = "&#958;"

-- | The greek small letter omicron HTML entity ('ο').
greekSmallLetterOmicron :: String
greekSmallLetterOmicron = "&#959;"

-- | The greek small letter pi HTML entity ('π').
greekSmallLetterPi :: String
greekSmallLetterPi = "&#960;"

-- | The greek small letter rho HTML entity ('ρ').
greekSmallLetterRho :: String
greekSmallLetterRho = "&#961;"

-- | The greek small letter final sigma HTML entity ('ς').
greekSmallLetterFinalSigma :: String
greekSmallLetterFinalSigma = "&#962;"

-- | The greek small letter sigma HTML entity ('σ').
greekSmallLetterSigma :: String
greekSmallLetterSigma = "&#963;"

-- | The greek small letter tau HTML entity ('τ').
greekSmallLetterTau :: String
greekSmallLetterTau = "&#964;"

-- | The greek small letter upsilon HTML entity ('υ').
greekSmallLetterUpsilon :: String
greekSmallLetterUpsilon = "&#965;"

-- | The greek small letter phi HTML entity ('φ').
greekSmallLetterPhi :: String
greekSmallLetterPhi = "&#966;"

-- | The greek small letter chi HTML entity ('χ').
greekSmallLetterChi :: String
greekSmallLetterChi = "&#967;"

-- | The greek small letter psi HTML entity ('ψ').
greekSmallLetterPsi :: String
greekSmallLetterPsi = "&#968;"

-- | The greek small letter omega HTML entity ('ω').
greekSmallLetterOmega :: String
greekSmallLetterOmega = "&#969;"

-- | The greek theta symbol HTML entity ('ϑ').
greekThetaSymbol :: String
greekThetaSymbol = "&#977;"

-- | The greek upsilon with hook symbol HTML entity ('ϒ').
greekUpsilonWithHookSymbol :: String
greekUpsilonWithHookSymbol = "&#978;"

-- | The greek phi symbol HTML entity ('ϕ').
greekPhiSymbol :: String
greekPhiSymbol = "&#981;"

-- | The greek pi symbol HTML entity ('ϖ').
greekPiSymbol :: String
greekPiSymbol = "&#982;"

-- | The greek letter digamma HTML entity ('Ϝ').
greekLetterDigamma :: String
greekLetterDigamma = "&#988;"

-- | The greek small letter digamma HTML entity ('ϝ').
greekSmallLetterDigamma :: String
greekSmallLetterDigamma = "&#989;"

-- | The greek kappa symbol HTML entity ('ϰ').
greekKappaSymbol :: String
greekKappaSymbol = "&#1008;"

-- | The greek rho symbol HTML entity ('ϱ').
greekRhoSymbol :: String
greekRhoSymbol = "&#1009;"

-- | The greek lunate epsilon symbol HTML entity ('ϵ').
greekLunateEpsilonSymbol :: String
greekLunateEpsilonSymbol = "&#1013;"

-- | The greek reversed lunate epsilon symbol HTML entity ('϶').
greekReversedLunateEpsilonSymbol :: String
greekReversedLunateEpsilonSymbol = "&#1014;"

-- | The cyrillic capital letter io HTML entity ('Ё').
cyrillicCapitalLetterIo :: String
cyrillicCapitalLetterIo = "&#1025;"

-- | The cyrillic capital letter dje HTML entity ('Ђ').
cyrillicCapitalLetterDje :: String
cyrillicCapitalLetterDje = "&#1026;"

-- | The cyrillic capital letter gje HTML entity ('Ѓ').
cyrillicCapitalLetterGje :: String
cyrillicCapitalLetterGje = "&#1027;"

-- | The cyrillic capital letter ukrainian ie HTML entity ('Є').
cyrillicCapitalLetterUkrainianIe :: String
cyrillicCapitalLetterUkrainianIe = "&#1028;"

-- | The cyrillic capital letter dze HTML entity ('Ѕ').
cyrillicCapitalLetterDze :: String
cyrillicCapitalLetterDze = "&#1029;"

-- | The cyrillic capital letter byelorussian-ukrainian i HTML entity ('І').
cyrillicCapitalLetterByelorussianUkrainianI :: String
cyrillicCapitalLetterByelorussianUkrainianI = "&#1030;"

-- | The cyrillic capital letter yi HTML entity ('Ї').
cyrillicCapitalLetterYi :: String
cyrillicCapitalLetterYi = "&#1031;"

-- | The cyrillic capital letter je HTML entity ('Ј').
cyrillicCapitalLetterJe :: String
cyrillicCapitalLetterJe = "&#1032;"

-- | The cyrillic capital letter lje HTML entity ('Љ').
cyrillicCapitalLetterLje :: String
cyrillicCapitalLetterLje = "&#1033;"

-- | The cyrillic capital letter nje HTML entity ('Њ').
cyrillicCapitalLetterNje :: String
cyrillicCapitalLetterNje = "&#1034;"

-- | The cyrillic capital letter tshe HTML entity ('Ћ').
cyrillicCapitalLetterTshe :: String
cyrillicCapitalLetterTshe = "&#1035;"

-- | The cyrillic capital letter kje HTML entity ('Ќ').
cyrillicCapitalLetterKje :: String
cyrillicCapitalLetterKje = "&#1036;"

-- | The cyrillic capital letter short u HTML entity ('Ў').
cyrillicCapitalLetterShortU :: String
cyrillicCapitalLetterShortU = "&#1038;"

-- | The cyrillic capital letter dzhe HTML entity ('Џ').
cyrillicCapitalLetterDzhe :: String
cyrillicCapitalLetterDzhe = "&#1039;"

-- | The cyrillic capital letter a HTML entity ('А').
cyrillicCapitalLetterA :: String
cyrillicCapitalLetterA = "&#1040;"

-- | The cyrillic capital letter be HTML entity ('Б').
cyrillicCapitalLetterBe :: String
cyrillicCapitalLetterBe = "&#1041;"

-- | The cyrillic capital letter ve HTML entity ('В').
cyrillicCapitalLetterVe :: String
cyrillicCapitalLetterVe = "&#1042;"

-- | The cyrillic capital letter ghe HTML entity ('Г').
cyrillicCapitalLetterGhe :: String
cyrillicCapitalLetterGhe = "&#1043;"

-- | The cyrillic capital letter de HTML entity ('Д').
cyrillicCapitalLetterDe :: String
cyrillicCapitalLetterDe = "&#1044;"

-- | The cyrillic capital letter ie HTML entity ('Е').
cyrillicCapitalLetterIe :: String
cyrillicCapitalLetterIe = "&#1045;"

-- | The cyrillic capital letter zhe HTML entity ('Ж').
cyrillicCapitalLetterZhe :: String
cyrillicCapitalLetterZhe = "&#1046;"

-- | The cyrillic capital letter ze HTML entity ('З').
cyrillicCapitalLetterZe :: String
cyrillicCapitalLetterZe = "&#1047;"

-- | The cyrillic capital letter i HTML entity ('И').
cyrillicCapitalLetterI :: String
cyrillicCapitalLetterI = "&#1048;"

-- | The cyrillic capital letter short i HTML entity ('Й').
cyrillicCapitalLetterShortI :: String
cyrillicCapitalLetterShortI = "&#1049;"

-- | The cyrillic capital letter ka HTML entity ('К').
cyrillicCapitalLetterKa :: String
cyrillicCapitalLetterKa = "&#1050;"

-- | The cyrillic capital letter el HTML entity ('Л').
cyrillicCapitalLetterEl :: String
cyrillicCapitalLetterEl = "&#1051;"

-- | The cyrillic capital letter em HTML entity ('М').
cyrillicCapitalLetterEm :: String
cyrillicCapitalLetterEm = "&#1052;"

-- | The cyrillic capital letter en HTML entity ('Н').
cyrillicCapitalLetterEn :: String
cyrillicCapitalLetterEn = "&#1053;"

-- | The cyrillic capital letter o HTML entity ('О').
cyrillicCapitalLetterO :: String
cyrillicCapitalLetterO = "&#1054;"

-- | The cyrillic capital letter pe HTML entity ('П').
cyrillicCapitalLetterPe :: String
cyrillicCapitalLetterPe = "&#1055;"

-- | The cyrillic capital letter er HTML entity ('Р').
cyrillicCapitalLetterEr :: String
cyrillicCapitalLetterEr = "&#1056;"

-- | The cyrillic capital letter es HTML entity ('С').
cyrillicCapitalLetterEs :: String
cyrillicCapitalLetterEs = "&#1057;"

-- | The cyrillic capital letter te HTML entity ('Т').
cyrillicCapitalLetterTe :: String
cyrillicCapitalLetterTe = "&#1058;"

-- | The cyrillic capital letter u HTML entity ('У').
cyrillicCapitalLetterU :: String
cyrillicCapitalLetterU = "&#1059;"

-- | The cyrillic capital letter ef HTML entity ('Ф').
cyrillicCapitalLetterEf :: String
cyrillicCapitalLetterEf = "&#1060;"

-- | The cyrillic capital letter ha HTML entity ('Х').
cyrillicCapitalLetterHa :: String
cyrillicCapitalLetterHa = "&#1061;"

-- | The cyrillic capital letter tse HTML entity ('Ц').
cyrillicCapitalLetterTse :: String
cyrillicCapitalLetterTse = "&#1062;"

-- | The cyrillic capital letter che HTML entity ('Ч').
cyrillicCapitalLetterChe :: String
cyrillicCapitalLetterChe = "&#1063;"

-- | The cyrillic capital letter sha HTML entity ('Ш').
cyrillicCapitalLetterSha :: String
cyrillicCapitalLetterSha = "&#1064;"

-- | The cyrillic capital letter shcha HTML entity ('Щ').
cyrillicCapitalLetterShcha :: String
cyrillicCapitalLetterShcha = "&#1065;"

-- | The cyrillic capital letter hard sign HTML entity ('Ъ').
cyrillicCapitalLetterHardSign :: String
cyrillicCapitalLetterHardSign = "&#1066;"

-- | The cyrillic capital letter yeru HTML entity ('Ы').
cyrillicCapitalLetterYeru :: String
cyrillicCapitalLetterYeru = "&#1067;"

-- | The cyrillic capital letter soft sign HTML entity ('Ь').
cyrillicCapitalLetterSoftSign :: String
cyrillicCapitalLetterSoftSign = "&#1068;"

-- | The cyrillic capital letter e HTML entity ('Э').
cyrillicCapitalLetterE :: String
cyrillicCapitalLetterE = "&#1069;"

-- | The cyrillic capital letter yu HTML entity ('Ю').
cyrillicCapitalLetterYu :: String
cyrillicCapitalLetterYu = "&#1070;"

-- | The cyrillic capital letter ya HTML entity ('Я').
cyrillicCapitalLetterYa :: String
cyrillicCapitalLetterYa = "&#1071;"

-- | The cyrillic small letter a HTML entity ('а').
cyrillicSmallLetterA :: String
cyrillicSmallLetterA = "&#1072;"

-- | The cyrillic small letter be HTML entity ('б').
cyrillicSmallLetterBe :: String
cyrillicSmallLetterBe = "&#1073;"

-- | The cyrillic small letter ve HTML entity ('в').
cyrillicSmallLetterVe :: String
cyrillicSmallLetterVe = "&#1074;"

-- | The cyrillic small letter ghe HTML entity ('г').
cyrillicSmallLetterGhe :: String
cyrillicSmallLetterGhe = "&#1075;"

-- | The cyrillic small letter de HTML entity ('д').
cyrillicSmallLetterDe :: String
cyrillicSmallLetterDe = "&#1076;"

-- | The cyrillic small letter ie HTML entity ('е').
cyrillicSmallLetterIe :: String
cyrillicSmallLetterIe = "&#1077;"

-- | The cyrillic small letter zhe HTML entity ('ж').
cyrillicSmallLetterZhe :: String
cyrillicSmallLetterZhe = "&#1078;"

-- | The cyrillic small letter ze HTML entity ('з').
cyrillicSmallLetterZe :: String
cyrillicSmallLetterZe = "&#1079;"

-- | The cyrillic small letter i HTML entity ('и').
cyrillicSmallLetterI :: String
cyrillicSmallLetterI = "&#1080;"

-- | The cyrillic small letter short i HTML entity ('й').
cyrillicSmallLetterShortI :: String
cyrillicSmallLetterShortI = "&#1081;"

-- | The cyrillic small letter ka HTML entity ('к').
cyrillicSmallLetterKa :: String
cyrillicSmallLetterKa = "&#1082;"

-- | The cyrillic small letter el HTML entity ('л').
cyrillicSmallLetterEl :: String
cyrillicSmallLetterEl = "&#1083;"

-- | The cyrillic small letter em HTML entity ('м').
cyrillicSmallLetterEm :: String
cyrillicSmallLetterEm = "&#1084;"

-- | The cyrillic small letter en HTML entity ('н').
cyrillicSmallLetterEn :: String
cyrillicSmallLetterEn = "&#1085;"

-- | The cyrillic small letter o HTML entity ('о').
cyrillicSmallLetterO :: String
cyrillicSmallLetterO = "&#1086;"

-- | The cyrillic small letter pe HTML entity ('п').
cyrillicSmallLetterPe :: String
cyrillicSmallLetterPe = "&#1087;"

-- | The cyrillic small letter er HTML entity ('р').
cyrillicSmallLetterEr :: String
cyrillicSmallLetterEr = "&#1088;"

-- | The cyrillic small letter es HTML entity ('с').
cyrillicSmallLetterEs :: String
cyrillicSmallLetterEs = "&#1089;"

-- | The cyrillic small letter te HTML entity ('т').
cyrillicSmallLetterTe :: String
cyrillicSmallLetterTe = "&#1090;"

-- | The cyrillic small letter u HTML entity ('у').
cyrillicSmallLetterU :: String
cyrillicSmallLetterU = "&#1091;"

-- | The cyrillic small letter ef HTML entity ('ф').
cyrillicSmallLetterEf :: String
cyrillicSmallLetterEf = "&#1092;"

-- | The cyrillic small letter ha HTML entity ('х').
cyrillicSmallLetterHa :: String
cyrillicSmallLetterHa = "&#1093;"

-- | The cyrillic small letter tse HTML entity ('ц').
cyrillicSmallLetterTse :: String
cyrillicSmallLetterTse = "&#1094;"

-- | The cyrillic small letter che HTML entity ('ч').
cyrillicSmallLetterChe :: String
cyrillicSmallLetterChe = "&#1095;"

-- | The cyrillic small letter sha HTML entity ('ш').
cyrillicSmallLetterSha :: String
cyrillicSmallLetterSha = "&#1096;"

-- | The cyrillic small letter shcha HTML entity ('щ').
cyrillicSmallLetterShcha :: String
cyrillicSmallLetterShcha = "&#1097;"

-- | The cyrillic small letter hard sign HTML entity ('ъ').
cyrillicSmallLetterHardSign :: String
cyrillicSmallLetterHardSign = "&#1098;"

-- | The cyrillic small letter yeru HTML entity ('ы').
cyrillicSmallLetterYeru :: String
cyrillicSmallLetterYeru = "&#1099;"

-- | The cyrillic small letter soft sign HTML entity ('ь').
cyrillicSmallLetterSoftSign :: String
cyrillicSmallLetterSoftSign = "&#1100;"

-- | The cyrillic small letter e HTML entity ('э').
cyrillicSmallLetterE :: String
cyrillicSmallLetterE = "&#1101;"

-- | The cyrillic small letter yu HTML entity ('ю').
cyrillicSmallLetterYu :: String
cyrillicSmallLetterYu = "&#1102;"

-- | The cyrillic small letter ya HTML entity ('я').
cyrillicSmallLetterYa :: String
cyrillicSmallLetterYa = "&#1103;"

-- | The cyrillic small letter io HTML entity ('ё').
cyrillicSmallLetterIo :: String
cyrillicSmallLetterIo = "&#1105;"

-- | The cyrillic small letter dje HTML entity ('ђ').
cyrillicSmallLetterDje :: String
cyrillicSmallLetterDje = "&#1106;"

-- | The cyrillic small letter gje HTML entity ('ѓ').
cyrillicSmallLetterGje :: String
cyrillicSmallLetterGje = "&#1107;"

-- | The cyrillic small letter ukrainian ie HTML entity ('є').
cyrillicSmallLetterUkrainianIe :: String
cyrillicSmallLetterUkrainianIe = "&#1108;"

-- | The cyrillic small letter dze HTML entity ('ѕ').
cyrillicSmallLetterDze :: String
cyrillicSmallLetterDze = "&#1109;"

-- | The cyrillic small letter byelorussian-ukrainian i HTML entity ('і').
cyrillicSmallLetterByelorussianUkrainianI :: String
cyrillicSmallLetterByelorussianUkrainianI = "&#1110;"

-- | The cyrillic small letter yi HTML entity ('ї').
cyrillicSmallLetterYi :: String
cyrillicSmallLetterYi = "&#1111;"

-- | The cyrillic small letter je HTML entity ('ј').
cyrillicSmallLetterJe :: String
cyrillicSmallLetterJe = "&#1112;"

-- | The cyrillic small letter lje HTML entity ('љ').
cyrillicSmallLetterLje :: String
cyrillicSmallLetterLje = "&#1113;"

-- | The cyrillic small letter nje HTML entity ('њ').
cyrillicSmallLetterNje :: String
cyrillicSmallLetterNje = "&#1114;"

-- | The cyrillic small letter tshe HTML entity ('ћ').
cyrillicSmallLetterTshe :: String
cyrillicSmallLetterTshe = "&#1115;"

-- | The cyrillic small letter kje HTML entity ('ќ').
cyrillicSmallLetterKje :: String
cyrillicSmallLetterKje = "&#1116;"

-- | The cyrillic small letter short u HTML entity ('ў').
cyrillicSmallLetterShortU :: String
cyrillicSmallLetterShortU = "&#1118;"

-- | The cyrillic small letter dzhe HTML entity ('џ').
cyrillicSmallLetterDzhe :: String
cyrillicSmallLetterDzhe = "&#1119;"

-- | The Arabic Percent Sign HTML entity ('٪').
arabicPercentSign :: String
arabicPercentSign = "&#1642;"

-- | The Canadian Syllabics Final Plus HTML entity ('ᐩ').
canadianSyllabicsFinalPlus :: String
canadianSyllabicsFinalPlus = "&#5161;"

-- | The Modifier Letter Small Delta HTML entity ('ᵟ').
modifierLetterSmallDelta :: String
modifierLetterSmallDelta = "&#7519;"

-- | The Latin Small Letter Delta HTML entity ('ẟ').
latinSmallLetterDelta :: String
latinSmallLetterDelta = "&#7839;"

-- | The en space HTML entity.
enSpace :: String
enSpace = "&#8194;"

-- | The em space HTML entity.
emSpace :: String
emSpace = "&#8195;"

-- | The three-per-em space HTML entity.
threePerEmSpace :: String
threePerEmSpace = "&#8196;"

-- | The four-per-em space HTML entity.
fourPerEmSpace :: String
fourPerEmSpace = "&#8197;"

-- | The figure space HTML entity.
figureSpace :: String
figureSpace = "&#8199;"

-- | The punctuation space HTML entity.
punctuationSpace :: String
punctuationSpace = "&#8200;"

-- | The thin space HTML entity.
thinSpace :: String
thinSpace = "&#8201;"

-- | The hair space HTML entity.
hairSpace :: String
hairSpace = "&#8202;"

-- | The zero width space HTML entity.
zeroWidthSpace :: String
zeroWidthSpace = "&#8203;"

-- | The zero width non-joiner HTML entity.
zeroWidthNonJoiner :: String
zeroWidthNonJoiner = "&#8204;"

-- | The zero width joiner HTML entity.
zeroWidthJoiner :: String
zeroWidthJoiner = "&#8205;"

-- | The left-to-right mark HTML entity.
leftToRightMark :: String
leftToRightMark = "&#8206;"

-- | The right-to-left mark HTML entity.
rightToLeftMark :: String
rightToLeftMark = "&#8207;"

-- | The hyphen HTML entity ('‐').
hyphen :: String
hyphen = "&#8208;"

-- | The en dash HTML entity ('–').
enDash :: String
enDash = "&#8211;"

-- | The em dash HTML entity ('—').
emDash :: String
emDash = "&#8212;"

-- | The horizontal bar HTML entity ('―').
horizontalBar :: String
horizontalBar = "&#8213;"

-- | The double vertical line HTML entity ('‖').
doubleVerticalLine :: String
doubleVerticalLine = "&#8214;"

-- | The left single quotation mark HTML entity ('‘').
leftSingleQuotationMark :: String
leftSingleQuotationMark = "&#8216;"

-- | The right single quotation mark HTML entity ('’').
rightSingleQuotationMark :: String
rightSingleQuotationMark = "&#8217;"

-- | The single low-9 quotation mark HTML entity ('‚').
singleLow9QuotationMark :: String
singleLow9QuotationMark = "&#8218;"

-- | The left double quotation mark HTML entity ('“').
leftDoubleQuotationMark :: String
leftDoubleQuotationMark = "&#8220;"

-- | The right double quotation mark HTML entity ('”').
rightDoubleQuotationMark :: String
rightDoubleQuotationMark = "&#8221;"

-- | The double low-9 quotation mark HTML entity ('„').
doubleLow9QuotationMark :: String
doubleLow9QuotationMark = "&#8222;"

-- | The dagger HTML entity ('†').
dagger :: String
dagger = "&#8224;"

-- | The double dagger HTML entity ('‡').
doubleDagger :: String
doubleDagger = "&#8225;"

-- | The bullet HTML entity ('•').
bullet :: String
bullet = "&#8226;"

-- | The two dot leader HTML entity ('‥').
twoDotLeader :: String
twoDotLeader = "&#8229;"

-- | The horizontal ellipsis HTML entity ('…').
horizontalEllipsis :: String
horizontalEllipsis = "&#8230;"

-- | The per mille sign - per thousand sign HTML entity ('‰').
perMilleSignPerThousandSign :: String
perMilleSignPerThousandSign = "&#8240;"

-- | The per ten thousand sign HTML entity ('‱').
perTenThousandSign :: String
perTenThousandSign = "&#8241;"

-- | The prime = minutes = feet HTML entity ('′').
prime :: String
prime = "&#8242;"

-- | The double prime = seconds = inches HTML entity ('″').
doublePrime :: String
doublePrime = "&#8243;"

-- | The triple prime HTML entity ('‴').
triplePrime :: String
triplePrime = "&#8244;"

-- | The reversed prime HTML entity ('‵').
reversedPrime :: String
reversedPrime = "&#8245;"

-- | The single left-pointing angle quotation mark HTML entity ('‹').
singleLeftPointingAngleQuotationMark :: String
singleLeftPointingAngleQuotationMark = "&#8249;"

-- | The single right-pointing angle quotation mark HTML entity ('›').
singleRightPointingAngleQuotationMark :: String
singleRightPointingAngleQuotationMark = "&#8250;"

-- | The overline = spacing overscore HTML entity ('‾').
overline :: String
overline = "&#8254;"

-- | The caret insertion point HTML entity ('⁁').
caretInsertionPoint :: String
caretInsertionPoint = "&#8257;"

-- | The hyphen bullet HTML entity ('⁃').
hyphenBullet :: String
hyphenBullet = "&#8259;"

-- | The fraction slash HTML entity ('⁄').
fractionSlash :: String
fractionSlash = "&#8260;"

-- | The reversed semicolon HTML entity ('⁏').
reversedSemicolon :: String
reversedSemicolon = "&#8271;"

-- | The Commercial Minus Sign HTML entity ('⁒').
commercialMinusSign :: String
commercialMinusSign = "&#8274;"

-- | The quadruple prime HTML entity ('⁗').
quadruplePrime :: String
quadruplePrime = "&#8279;"

-- | The medium mathematical space HTML entity.
mediumMathematicalSpace :: String
mediumMathematicalSpace = "&#8287;"

-- | The word joiner HTML entity.
wordJoiner :: String
wordJoiner = "&#8288;"

-- | The function application HTML entity.
functionApplication :: String
functionApplication = "&#8289;"

-- | The invisible times HTML entity.
invisibleTimes :: String
invisibleTimes = "&#8290;"

-- | The invisible separator HTML entity.
invisibleSeparator :: String
invisibleSeparator = "&#8291;"

-- | The Superscript Plus Sign HTML entity ('⁺').
superscriptPlusSign :: String
superscriptPlusSign = "&#8314;"

-- | The Superscript Minus HTML entity ('⁻').
superscriptMinus :: String
superscriptMinus = "&#8315;"

-- | The Superscript Equals Sign HTML entity ('⁼').
superscriptEqualsSign :: String
superscriptEqualsSign = "&#8316;"

-- | The Subscript Plus Sign HTML entity ('₊').
subscriptPlusSign :: String
subscriptPlusSign = "&#8330;"

-- | The Subscript Minus HTML entity ('₋').
subscriptMinus :: String
subscriptMinus = "&#8331;"

-- | The Subscript Equals Sign HTML entity ('₌').
subscriptEqualsSign :: String
subscriptEqualsSign = "&#8332;"

-- | The euro-currency sign HTML entity ('₠').
euroCurrencySign :: String
euroCurrencySign = "&#8352;"

-- | The colon sign HTML entity ('₡').
colonSign :: String
colonSign = "&#8353;"

-- | The cruzeiro sign HTML entity ('₢').
cruzeiroSign :: String
cruzeiroSign = "&#8354;"

-- | The french franc sign HTML entity ('₣').
frenchFrancSign :: String
frenchFrancSign = "&#8355;"

-- | The lira sign HTML entity ('₤').
liraSign :: String
liraSign = "&#8356;"

-- | The mill sign HTML entity ('₥').
millSign :: String
millSign = "&#8357;"

-- | The naira sign HTML entity ('₦').
nairaSign :: String
nairaSign = "&#8358;"

-- | The peseta sign HTML entity ('₧').
pesetaSign :: String
pesetaSign = "&#8359;"

-- | The rupee sign HTML entity ('₨').
rupeeSign :: String
rupeeSign = "&#8360;"

-- | The won sign HTML entity ('₩').
wonSign :: String
wonSign = "&#8361;"

-- | The new sheqel sign HTML entity ('₪').
newSheqelSign :: String
newSheqelSign = "&#8362;"

-- | The dong sign HTML entity ('₫').
dongSign :: String
dongSign = "&#8363;"

-- | The euro sign HTML entity ('€').
euroSign :: String
euroSign = "&#8364;"

-- | The kip sign HTML entity ('₭').
kipSign :: String
kipSign = "&#8365;"

-- | The tugrik sign HTML entity ('₮').
tugrikSign :: String
tugrikSign = "&#8366;"

-- | The drachma sign HTML entity ('₯').
drachmaSign :: String
drachmaSign = "&#8367;"

-- | The german penny symbol HTML entity ('₰').
germanPennySymbol :: String
germanPennySymbol = "&#8368;"

-- | The peso sign HTML entity ('₱').
pesoSign :: String
pesoSign = "&#8369;"

-- | The guarani sign HTML entity ('₲').
guaraniSign :: String
guaraniSign = "&#8370;"

-- | The austral sign HTML entity ('₳').
australSign :: String
australSign = "&#8371;"

-- | The hryvnia sign HTML entity ('₴').
hryvniaSign :: String
hryvniaSign = "&#8372;"

-- | The cedi sign HTML entity ('₵').
cediSign :: String
cediSign = "&#8373;"

-- | The livre tournois sign HTML entity ('₶').
livreTournoisSign :: String
livreTournoisSign = "&#8374;"

-- | The spesmilo sign HTML entity ('₷').
spesmiloSign :: String
spesmiloSign = "&#8375;"

-- | The tenge sign HTML entity ('₸').
tengeSign :: String
tengeSign = "&#8376;"

-- | The indian rupee sign HTML entity ('₹').
indianRupeeSign :: String
indianRupeeSign = "&#8377;"

-- | The turkish lira sign HTML entity ('₺').
turkishLiraSign :: String
turkishLiraSign = "&#8378;"

-- | The nordic mark sign HTML entity ('₻').
nordicMarkSign :: String
nordicMarkSign = "&#8379;"

-- | The manat sign HTML entity ('₼').
manatSign :: String
manatSign = "&#8380;"

-- | The ruble sign HTML entity ('₽').
rubleSign :: String
rubleSign = "&#8381;"

-- | The lari sign HTML entity ('₾').
lariSign :: String
lariSign = "&#8382;"

-- | The bitcoin sign HTML entity ('₿').
bitcoinSign :: String
bitcoinSign = "&#8383;"

-- | The combining three dots above HTML entity ('⃛').
combiningThreeDotsAbove :: String
combiningThreeDotsAbove = "&#8411;"

-- | The combining four dots above HTML entity ('⃜').
combiningFourDotsAbove :: String
combiningFourDotsAbove = "&#8412;"

-- | The double-struck capital c HTML entity ('ℂ').
doubleStruckCapitalC :: String
doubleStruckCapitalC = "&#8450;"

-- | The care of HTML entity ('℅').
careOf :: String
careOf = "&#8453;"

-- | The script small g HTML entity ('ℊ').
scriptSmallG :: String
scriptSmallG = "&#8458;"

-- | The script capital h HTML entity ('ℋ').
scriptCapitalH :: String
scriptCapitalH = "&#8459;"

-- | The black-letter capital h HTML entity ('ℌ').
blackLetterCapitalH :: String
blackLetterCapitalH = "&#8460;"

-- | The double-struck capital h HTML entity ('ℍ').
doubleStruckCapitalH :: String
doubleStruckCapitalH = "&#8461;"

-- | The planck constant HTML entity ('ℎ').
planckConstant :: String
planckConstant = "&#8462;"

-- | The planck constant over two pi HTML entity ('ℏ').
planckConstantOverTwoPi :: String
planckConstantOverTwoPi = "&#8463;"

-- | The script capital i HTML entity ('ℐ').
scriptCapitalI :: String
scriptCapitalI = "&#8464;"

-- | The black-letter capital i HTML entity ('ℑ').
blackLetterCapitalI :: String
blackLetterCapitalI = "&#8465;"

-- | The script capital l HTML entity ('ℒ').
scriptCapitalL :: String
scriptCapitalL = "&#8466;"

-- | The script small l HTML entity ('ℓ').
scriptSmallL :: String
scriptSmallL = "&#8467;"

-- | The double-struck capital n HTML entity ('ℕ').
doubleStruckCapitalN :: String
doubleStruckCapitalN = "&#8469;"

-- | The numero sign HTML entity ('№').
numeroSign :: String
numeroSign = "&#8470;"

-- | The sound recording copyright HTML entity ('℗').
soundRecordingCopyright :: String
soundRecordingCopyright = "&#8471;"

-- | The script capital p HTML entity ('℘').
scriptCapitalP :: String
scriptCapitalP = "&#8472;"

-- | The double-struck capital p HTML entity ('ℙ').
doubleStruckCapitalP :: String
doubleStruckCapitalP = "&#8473;"

-- | The double-struck capital q HTML entity ('ℚ').
doubleStruckCapitalQ :: String
doubleStruckCapitalQ = "&#8474;"

-- | The script capital r HTML entity ('ℛ').
scriptCapitalR :: String
scriptCapitalR = "&#8475;"

-- | The black-letter capital r HTML entity ('ℜ').
blackLetterCapitalR :: String
blackLetterCapitalR = "&#8476;"

-- | The double-struck capital r HTML entity ('ℝ').
doubleStruckCapitalR :: String
doubleStruckCapitalR = "&#8477;"

-- | The prescription take HTML entity ('℞').
prescriptionTake :: String
prescriptionTake = "&#8478;"

-- | The trade mark sign HTML entity ('™').
tradeMarkSign :: String
tradeMarkSign = "&#8482;"

-- | The double-struck capital z HTML entity ('ℤ').
doubleStruckCapitalZ :: String
doubleStruckCapitalZ = "&#8484;"

-- | The ohm sign HTML entity ('Ω').
ohmSign :: String
ohmSign = "&#8486;"

-- | The inverted ohm sign HTML entity ('℧').
invertedOhmSign :: String
invertedOhmSign = "&#8487;"

-- | The black-letter capital z HTML entity ('ℨ').
blackLetterCapitalZ :: String
blackLetterCapitalZ = "&#8488;"

-- | The turned greek small letter iota HTML entity ('℩').
turnedGreekSmallLetterIota :: String
turnedGreekSmallLetterIota = "&#8489;"

-- | The angstrom sign HTML entity ('Å').
angstromSign :: String
angstromSign = "&#8491;"

-- | The script capital b HTML entity ('ℬ').
scriptCapitalB :: String
scriptCapitalB = "&#8492;"

-- | The black-letter capital c HTML entity ('ℭ').
blackLetterCapitalC :: String
blackLetterCapitalC = "&#8493;"

-- | The script small e HTML entity ('ℯ').
scriptSmallE :: String
scriptSmallE = "&#8495;"

-- | The script capital e HTML entity ('ℰ').
scriptCapitalE :: String
scriptCapitalE = "&#8496;"

-- | The script capital f HTML entity ('ℱ').
scriptCapitalF :: String
scriptCapitalF = "&#8497;"

-- | The script capital m HTML entity ('ℳ').
scriptCapitalM :: String
scriptCapitalM = "&#8499;"

-- | The script small o HTML entity ('ℴ').
scriptSmallO :: String
scriptSmallO = "&#8500;"

-- | The alef symbol HTML entity ('ℵ').
alefSymbol :: String
alefSymbol = "&#8501;"

-- | The bet symbol HTML entity ('ℶ').
betSymbol :: String
betSymbol = "&#8502;"

-- | The gimel symbol HTML entity ('ℷ').
gimelSymbol :: String
gimelSymbol = "&#8503;"

-- | The dalet symbol HTML entity ('ℸ').
daletSymbol :: String
daletSymbol = "&#8504;"

-- | The Double-struck N-ary Summation HTML entity ('⅀').
doubleStruckNArySummation :: String
doubleStruckNArySummation = "&#8512;"

-- | The double-struck italic capital d HTML entity ('ⅅ').
doubleStruckItalicCapitalD :: String
doubleStruckItalicCapitalD = "&#8517;"

-- | The double-struck italic small d HTML entity ('ⅆ').
doubleStruckItalicSmallD :: String
doubleStruckItalicSmallD = "&#8518;"

-- | The double-struck italic small e HTML entity ('ⅇ').
doubleStruckItalicSmallE :: String
doubleStruckItalicSmallE = "&#8519;"

-- | The double-struck italic small i HTML entity ('ⅈ').
doubleStruckItalicSmallI :: String
doubleStruckItalicSmallI = "&#8520;"

-- | The Vulgar Fraction One Seventh HTML entity ('⅐').
vulgarFractionOneSeventh :: String
vulgarFractionOneSeventh = "&#8528;"

-- | The Vulgar Fraction One Ninth HTML entity ('⅑').
vulgarFractionOneNinth :: String
vulgarFractionOneNinth = "&#8529;"

-- | The Vulgar Fraction One Tenth HTML entity ('⅒').
vulgarFractionOneTenth :: String
vulgarFractionOneTenth = "&#8530;"

-- | The vulgar fraction one third HTML entity ('⅓').
vulgarFractionOneThird :: String
vulgarFractionOneThird = "&#8531;"

-- | The vulgar fraction two thirds HTML entity ('⅔').
vulgarFractionTwoThirds :: String
vulgarFractionTwoThirds = "&#8532;"

-- | The vulgar fraction one fifth HTML entity ('⅕').
vulgarFractionOneFifth :: String
vulgarFractionOneFifth = "&#8533;"

-- | The vulgar fraction two fifths HTML entity ('⅖').
vulgarFractionTwoFifths :: String
vulgarFractionTwoFifths = "&#8534;"

-- | The vulgar fraction three fifths HTML entity ('⅗').
vulgarFractionThreeFifths :: String
vulgarFractionThreeFifths = "&#8535;"

-- | The vulgar fraction four fifths HTML entity ('⅘').
vulgarFractionFourFifths :: String
vulgarFractionFourFifths = "&#8536;"

-- | The vulgar fraction one sixth HTML entity ('⅙').
vulgarFractionOneSixth :: String
vulgarFractionOneSixth = "&#8537;"

-- | The vulgar fraction five sixths HTML entity ('⅚').
vulgarFractionFiveSixths :: String
vulgarFractionFiveSixths = "&#8538;"

-- | The vulgar fraction one eighth HTML entity ('⅛').
vulgarFractionOneEighth :: String
vulgarFractionOneEighth = "&#8539;"

-- | The vulgar fraction three eighths HTML entity ('⅜').
vulgarFractionThreeEighths :: String
vulgarFractionThreeEighths = "&#8540;"

-- | The vulgar fraction five eighths HTML entity ('⅝').
vulgarFractionFiveEighths :: String
vulgarFractionFiveEighths = "&#8541;"

-- | The vulgar fraction seven eighths HTML entity ('⅞').
vulgarFractionSevenEighths :: String
vulgarFractionSevenEighths = "&#8542;"

-- | The Fraction Numerator One HTML entity ('⅟').
fractionNumeratorOne :: String
fractionNumeratorOne = "&#8543;"

-- | The leftwards arrow HTML entity ('←').
leftwardsArrow :: String
leftwardsArrow = "&#8592;"

-- | The upwards arrow HTML entity ('↑').
upwardsArrow :: String
upwardsArrow = "&#8593;"

-- | The rightwards arrow HTML entity ('→').
rightwardsArrow :: String
rightwardsArrow = "&#8594;"

-- | The downwards arrow HTML entity ('↓').
downwardsArrow :: String
downwardsArrow = "&#8595;"

-- | The left right arrow HTML entity ('↔').
leftRightArrow :: String
leftRightArrow = "&#8596;"

-- | The up down arrow HTML entity ('↕').
upDownArrow :: String
upDownArrow = "&#8597;"

-- | The north west arrow HTML entity ('↖').
northWestArrow :: String
northWestArrow = "&#8598;"

-- | The north east arrow HTML entity ('↗').
northEastArrow :: String
northEastArrow = "&#8599;"

-- | The south east arrow HTML entity ('↘').
southEastArrow :: String
southEastArrow = "&#8600;"

-- | The south west arrow HTML entity ('↙').
southWestArrow :: String
southWestArrow = "&#8601;"

-- | The leftwards arrow with stroke HTML entity ('↚').
leftwardsArrowWithStroke :: String
leftwardsArrowWithStroke = "&#8602;"

-- | The rightwards arrow with stroke HTML entity ('↛').
rightwardsArrowWithStroke :: String
rightwardsArrowWithStroke = "&#8603;"

-- | The rightwards wave arrow HTML entity ('↝').
rightwardsWaveArrow :: String
rightwardsWaveArrow = "&#8605;"

-- | The leftwards two headed arrow HTML entity ('↞').
leftwardsTwoHeadedArrow :: String
leftwardsTwoHeadedArrow = "&#8606;"

-- | The upwards two headed arrow HTML entity ('↟').
upwardsTwoHeadedArrow :: String
upwardsTwoHeadedArrow = "&#8607;"

-- | The rightwards two headed arrow HTML entity ('↠').
rightwardsTwoHeadedArrow :: String
rightwardsTwoHeadedArrow = "&#8608;"

-- | The downwards two headed arrow HTML entity ('↡').
downwardsTwoHeadedArrow :: String
downwardsTwoHeadedArrow = "&#8609;"

-- | The leftwards arrow with tail HTML entity ('↢').
leftwardsArrowWithTail :: String
leftwardsArrowWithTail = "&#8610;"

-- | The rightwards arrow with tail HTML entity ('↣').
rightwardsArrowWithTail :: String
rightwardsArrowWithTail = "&#8611;"

-- | The leftwards arrow from bar HTML entity ('↤').
leftwardsArrowFromBar :: String
leftwardsArrowFromBar = "&#8612;"

-- | The upwards arrow from bar HTML entity ('↥').
upwardsArrowFromBar :: String
upwardsArrowFromBar = "&#8613;"

-- | The rightwards arrow from bar HTML entity ('↦').
rightwardsArrowFromBar :: String
rightwardsArrowFromBar = "&#8614;"

-- | The downwards arrow from bar HTML entity ('↧').
downwardsArrowFromBar :: String
downwardsArrowFromBar = "&#8615;"

-- | The leftwards arrow with hook HTML entity ('↩').
leftwardsArrowWithHook :: String
leftwardsArrowWithHook = "&#8617;"

-- | The rightwards arrow with hook HTML entity ('↪').
rightwardsArrowWithHook :: String
rightwardsArrowWithHook = "&#8618;"

-- | The leftwards arrow with loop HTML entity ('↫').
leftwardsArrowWithLoop :: String
leftwardsArrowWithLoop = "&#8619;"

-- | The rightwards arrow with loop HTML entity ('↬').
rightwardsArrowWithLoop :: String
rightwardsArrowWithLoop = "&#8620;"

-- | The left right wave arrow HTML entity ('↭').
leftRightWaveArrow :: String
leftRightWaveArrow = "&#8621;"

-- | The left right arrow with stroke HTML entity ('↮').
leftRightArrowWithStroke :: String
leftRightArrowWithStroke = "&#8622;"

-- | The upwards arrow with tip leftwards HTML entity ('↰').
upwardsArrowWithTipLeftwards :: String
upwardsArrowWithTipLeftwards = "&#8624;"

-- | The upwards arrow with tip rightwards HTML entity ('↱').
upwardsArrowWithTipRightwards :: String
upwardsArrowWithTipRightwards = "&#8625;"

-- | The downwards arrow with tip leftwards HTML entity ('↲').
downwardsArrowWithTipLeftwards :: String
downwardsArrowWithTipLeftwards = "&#8626;"

-- | The downwards arrow with tip rightwards HTML entity ('↳').
downwardsArrowWithTipRightwards :: String
downwardsArrowWithTipRightwards = "&#8627;"

-- | The downwards arrow with corner leftwards = carriage return HTML entity ('↵').
downwardsArrowWithCornerLeftwards :: String
downwardsArrowWithCornerLeftwards = "&#8629;"

-- | The anticlockwise top semicircle arrow HTML entity ('↶').
anticlockwiseTopSemicircleArrow :: String
anticlockwiseTopSemicircleArrow = "&#8630;"

-- | The clockwise top semicircle arrow HTML entity ('↷').
clockwiseTopSemicircleArrow :: String
clockwiseTopSemicircleArrow = "&#8631;"

-- | The anticlockwise open circle arrow HTML entity ('↺').
anticlockwiseOpenCircleArrow :: String
anticlockwiseOpenCircleArrow = "&#8634;"

-- | The clockwise open circle arrow HTML entity ('↻').
clockwiseOpenCircleArrow :: String
clockwiseOpenCircleArrow = "&#8635;"

-- | The leftwards harpoon with barb upwards HTML entity ('↼').
leftwardsHarpoonWithBarbUpwards :: String
leftwardsHarpoonWithBarbUpwards = "&#8636;"

-- | The leftwards harpoon with barb downwards HTML entity ('↽').
leftwardsHarpoonWithBarbDownwards :: String
leftwardsHarpoonWithBarbDownwards = "&#8637;"

-- | The upwards harpoon with barb rightwards HTML entity ('↾').
upwardsHarpoonWithBarbRightwards :: String
upwardsHarpoonWithBarbRightwards = "&#8638;"

-- | The upwards harpoon with barb leftwards HTML entity ('↿').
upwardsHarpoonWithBarbLeftwards :: String
upwardsHarpoonWithBarbLeftwards = "&#8639;"

-- | The rightwards harpoon with barb upwards HTML entity ('⇀').
rightwardsHarpoonWithBarbUpwards :: String
rightwardsHarpoonWithBarbUpwards = "&#8640;"

-- | The rightwards harpoon with barb downwards HTML entity ('⇁').
rightwardsHarpoonWithBarbDownwards :: String
rightwardsHarpoonWithBarbDownwards = "&#8641;"

-- | The downwards harpoon with barb rightwards HTML entity ('⇂').
downwardsHarpoonWithBarbRightwards :: String
downwardsHarpoonWithBarbRightwards = "&#8642;"

-- | The downwards harpoon with barb leftwards HTML entity ('⇃').
downwardsHarpoonWithBarbLeftwards :: String
downwardsHarpoonWithBarbLeftwards = "&#8643;"

-- | The rightwards arrow over leftwards arrow HTML entity ('⇄').
rightwardsArrowOverLeftwardsArrow :: String
rightwardsArrowOverLeftwardsArrow = "&#8644;"

-- | The upwards arrow leftwards of downwards arrow HTML entity ('⇅').
upwardsArrowLeftwardsOfDownwardsArrow :: String
upwardsArrowLeftwardsOfDownwardsArrow = "&#8645;"

-- | The leftwards arrow over rightwards arrow HTML entity ('⇆').
leftwardsArrowOverRightwardsArrow :: String
leftwardsArrowOverRightwardsArrow = "&#8646;"

-- | The leftwards paired arrows HTML entity ('⇇').
leftwardsPairedArrows :: String
leftwardsPairedArrows = "&#8647;"

-- | The upwards paired arrows HTML entity ('⇈').
upwardsPairedArrows :: String
upwardsPairedArrows = "&#8648;"

-- | The rightwards paired arrows HTML entity ('⇉').
rightwardsPairedArrows :: String
rightwardsPairedArrows = "&#8649;"

-- | The downwards paired arrows HTML entity ('⇊').
downwardsPairedArrows :: String
downwardsPairedArrows = "&#8650;"

-- | The leftwards harpoon over rightwards harpoon HTML entity ('⇋').
leftwardsHarpoonOverRightwardsHarpoon :: String
leftwardsHarpoonOverRightwardsHarpoon = "&#8651;"

-- | The rightwards harpoon over leftwards harpoon HTML entity ('⇌').
rightwardsHarpoonOverLeftwardsHarpoon :: String
rightwardsHarpoonOverLeftwardsHarpoon = "&#8652;"

-- | The leftwards double arrow with stroke HTML entity ('⇍').
leftwardsDoubleArrowWithStroke :: String
leftwardsDoubleArrowWithStroke = "&#8653;"

-- | The left right double arrow with stroke HTML entity ('⇎').
leftRightDoubleArrowWithStroke :: String
leftRightDoubleArrowWithStroke = "&#8654;"

-- | The rightwards double arrow with stroke HTML entity ('⇏').
rightwardsDoubleArrowWithStroke :: String
rightwardsDoubleArrowWithStroke = "&#8655;"

-- | The leftwards double arrow HTML entity ('⇐').
leftwardsDoubleArrow :: String
leftwardsDoubleArrow = "&#8656;"

-- | The upwards double arrow HTML entity ('⇑').
upwardsDoubleArrow :: String
upwardsDoubleArrow = "&#8657;"

-- | The rightwards double arrow HTML entity ('⇒').
rightwardsDoubleArrow :: String
rightwardsDoubleArrow = "&#8658;"

-- | The downwards double arrow HTML entity ('⇓').
downwardsDoubleArrow :: String
downwardsDoubleArrow = "&#8659;"

-- | The left right double arrow HTML entity ('⇔').
leftRightDoubleArrow :: String
leftRightDoubleArrow = "&#8660;"

-- | The up down double arrow HTML entity ('⇕').
upDownDoubleArrow :: String
upDownDoubleArrow = "&#8661;"

-- | The north west double arrow HTML entity ('⇖').
northWestDoubleArrow :: String
northWestDoubleArrow = "&#8662;"

-- | The north east double arrow HTML entity ('⇗').
northEastDoubleArrow :: String
northEastDoubleArrow = "&#8663;"

-- | The south east double arrow HTML entity ('⇘').
southEastDoubleArrow :: String
southEastDoubleArrow = "&#8664;"

-- | The south west double arrow HTML entity ('⇙').
southWestDoubleArrow :: String
southWestDoubleArrow = "&#8665;"

-- | The leftwards triple arrow HTML entity ('⇚').
leftwardsTripleArrow :: String
leftwardsTripleArrow = "&#8666;"

-- | The rightwards triple arrow HTML entity ('⇛').
rightwardsTripleArrow :: String
rightwardsTripleArrow = "&#8667;"

-- | The rightwards squiggle arrow HTML entity ('⇝').
rightwardsSquiggleArrow :: String
rightwardsSquiggleArrow = "&#8669;"

-- | The leftwards arrow to bar HTML entity ('⇤').
leftwardsArrowToBar :: String
leftwardsArrowToBar = "&#8676;"

-- | The rightwards arrow to bar HTML entity ('⇥').
rightwardsArrowToBar :: String
rightwardsArrowToBar = "&#8677;"

-- | The downwards arrow leftwards of upwards arrow HTML entity ('⇵').
downwardsArrowLeftwardsOfUpwardsArrow :: String
downwardsArrowLeftwardsOfUpwardsArrow = "&#8693;"

-- | The leftwards open-headed arrow HTML entity ('⇽').
leftwardsOpenHeadedArrow :: String
leftwardsOpenHeadedArrow = "&#8701;"

-- | The rightwards open-headed arrow HTML entity ('⇾').
rightwardsOpenHeadedArrow :: String
rightwardsOpenHeadedArrow = "&#8702;"

-- | The left right open-headed arrow HTML entity ('⇿').
leftRightOpenHeadedArrow :: String
leftRightOpenHeadedArrow = "&#8703;"

-- | The for all HTML entity ('∀').
forAll :: String
forAll = "&#8704;"

-- | The complement HTML entity ('∁').
complement :: String
complement = "&#8705;"

-- | The partial differential HTML entity ('∂').
partialDifferential :: String
partialDifferential = "&#8706;"

-- | The there exists HTML entity ('∃').
thereExists :: String
thereExists = "&#8707;"

-- | The there does not exist HTML entity ('∄').
thereDoesNotExist :: String
thereDoesNotExist = "&#8708;"

-- | The empty set HTML entity ('∅').
emptySet :: String
emptySet = "&#8709;"

-- | The nabla HTML entity ('∇').
nabla :: String
nabla = "&#8711;"

-- | The element of HTML entity ('∈').
elementOf :: String
elementOf = "&#8712;"

-- | The not an element of HTML entity ('∉').
notAnElementOf :: String
notAnElementOf = "&#8713;"

-- | The contains as member HTML entity ('∋').
containsAsMember :: String
containsAsMember = "&#8715;"

-- | The does not contain as member HTML entity ('∌').
doesNotContainAsMember :: String
doesNotContainAsMember = "&#8716;"

-- | The n-ary product HTML entity ('∏').
nAryProduct :: String
nAryProduct = "&#8719;"

-- | The n-ary coproduct HTML entity ('∐').
nAryCoproduct :: String
nAryCoproduct = "&#8720;"

-- | The n-ary summation HTML entity ('∑').
nArySummation :: String
nArySummation = "&#8721;"

-- | The minus sign HTML entity ('−').
minusSign :: String
minusSign = "&#8722;"

-- | The minus-or-plus sign HTML entity ('∓').
minusOrPlusSign :: String
minusOrPlusSign = "&#8723;"

-- | The dot plus HTML entity ('∔').
dotPlus :: String
dotPlus = "&#8724;"

-- | The Division Slash HTML entity ('∕').
divisionSlash :: String
divisionSlash = "&#8725;"

-- | The set minus HTML entity ('∖').
setMinus :: String
setMinus = "&#8726;"

-- | The asterisk operator HTML entity ('∗').
asteriskOperator :: String
asteriskOperator = "&#8727;"

-- | The ring operator HTML entity ('∘').
ringOperator :: String
ringOperator = "&#8728;"

-- | The square root HTML entity ('√').
squareRoot :: String
squareRoot = "&#8730;"

-- | The Cube Root HTML entity ('∛').
cubeRoot :: String
cubeRoot = "&#8731;"

-- | The Fourth Root HTML entity ('∜').
fourthRoot :: String
fourthRoot = "&#8732;"

-- | The proportional to HTML entity ('∝').
proportionalTo :: String
proportionalTo = "&#8733;"

-- | The infinity HTML entity ('∞').
infinity :: String
infinity = "&#8734;"

-- | The right angle HTML entity ('∟').
rightAngle :: String
rightAngle = "&#8735;"

-- | The angle HTML entity ('∠').
angle :: String
angle = "&#8736;"

-- | The measured angle HTML entity ('∡').
measuredAngle :: String
measuredAngle = "&#8737;"

-- | The spherical angle HTML entity ('∢').
sphericalAngle :: String
sphericalAngle = "&#8738;"

-- | The divides HTML entity ('∣').
divides :: String
divides = "&#8739;"

-- | The does not divide HTML entity ('∤').
doesNotDivide :: String
doesNotDivide = "&#8740;"

-- | The parallel to HTML entity ('∥').
parallelTo :: String
parallelTo = "&#8741;"

-- | The not parallel to HTML entity ('∦').
notParallelTo :: String
notParallelTo = "&#8742;"

-- | The logical and HTML entity ('∧').
logicalAnd :: String
logicalAnd = "&#8743;"

-- | The logical or HTML entity ('∨').
logicalOr :: String
logicalOr = "&#8744;"

-- | The intersection = cap HTML entity ('∩').
intersection :: String
intersection = "&#8745;"

-- | The union = cup HTML entity ('∪').
union :: String
union = "&#8746;"

-- | The integral HTML entity ('∫').
integral :: String
integral = "&#8747;"

-- | The double integral HTML entity ('∬').
doubleIntegral :: String
doubleIntegral = "&#8748;"

-- | The triple integral HTML entity ('∭').
tripleIntegral :: String
tripleIntegral = "&#8749;"

-- | The contour integral HTML entity ('∮').
contourIntegral :: String
contourIntegral = "&#8750;"

-- | The surface integral HTML entity ('∯').
surfaceIntegral :: String
surfaceIntegral = "&#8751;"

-- | The volume integral HTML entity ('∰').
volumeIntegral :: String
volumeIntegral = "&#8752;"

-- | The clockwise integral HTML entity ('∱').
clockwiseIntegral :: String
clockwiseIntegral = "&#8753;"

-- | The clockwise contour integral HTML entity ('∲').
clockwiseContourIntegral :: String
clockwiseContourIntegral = "&#8754;"

-- | The anticlockwise contour integral HTML entity ('∳').
anticlockwiseContourIntegral :: String
anticlockwiseContourIntegral = "&#8755;"

-- | The therefore HTML entity ('∴').
therefore :: String
therefore = "&#8756;"

-- | The because HTML entity ('∵').
because :: String
because = "&#8757;"

-- | The ratio HTML entity ('∶').
ratio :: String
ratio = "&#8758;"

-- | The proportion HTML entity ('∷').
proportion :: String
proportion = "&#8759;"

-- | The dot minus HTML entity ('∸').
dotMinus :: String
dotMinus = "&#8760;"

-- | The geometric proportion HTML entity ('∺').
geometricProportion :: String
geometricProportion = "&#8762;"

-- | The homothetic HTML entity ('∻').
homothetic :: String
homothetic = "&#8763;"

-- | The tilde operator HTML entity ('∼').
tildeOperator :: String
tildeOperator = "&#8764;"

-- | The reversed tilde HTML entity ('∽').
reversedTilde :: String
reversedTilde = "&#8765;"

-- | The inverted lazy s HTML entity ('∾').
invertedLazyS :: String
invertedLazyS = "&#8766;"

-- | The sine wave HTML entity ('∿').
sineWave :: String
sineWave = "&#8767;"

-- | The wreath product HTML entity ('≀').
wreathProduct :: String
wreathProduct = "&#8768;"

-- | The not tilde HTML entity ('≁').
notTilde :: String
notTilde = "&#8769;"

-- | The minus tilde HTML entity ('≂').
minusTilde :: String
minusTilde = "&#8770;"

-- | The asymptotically equal to HTML entity ('≃').
asymptoticallyEqualTo :: String
asymptoticallyEqualTo = "&#8771;"

-- | The not asymptotically equal to HTML entity ('≄').
notAsymptoticallyEqualTo :: String
notAsymptoticallyEqualTo = "&#8772;"

-- | The approximately equal to HTML entity ('≅').
approximatelyEqualTo :: String
approximatelyEqualTo = "&#8773;"

-- | The approximately but not actually equal to HTML entity ('≆').
approximatelyButNotActuallyEqualTo :: String
approximatelyButNotActuallyEqualTo = "&#8774;"

-- | The neither approximately nor actually equal to HTML entity ('≇').
neitherApproximatelyNorActuallyEqualTo :: String
neitherApproximatelyNorActuallyEqualTo = "&#8775;"

-- | The almost equal to HTML entity ('≈').
almostEqualTo :: String
almostEqualTo = "&#8776;"

-- | The not almost equal to HTML entity ('≉').
notAlmostEqualTo :: String
notAlmostEqualTo = "&#8777;"

-- | The almost equal or equal to HTML entity ('≊').
almostEqualOrEqualTo :: String
almostEqualOrEqualTo = "&#8778;"

-- | The triple tilde HTML entity ('≋').
tripleTilde :: String
tripleTilde = "&#8779;"

-- | The all equal to HTML entity ('≌').
allEqualTo :: String
allEqualTo = "&#8780;"

-- | The equivalent to HTML entity ('≍').
equivalentTo :: String
equivalentTo = "&#8781;"

-- | The geometrically equivalent to HTML entity ('≎').
geometricallyEquivalentTo :: String
geometricallyEquivalentTo = "&#8782;"

-- | The difference between HTML entity ('≏').
differenceBetween :: String
differenceBetween = "&#8783;"

-- | The approaches the limit HTML entity ('≐').
approachesTheLimit :: String
approachesTheLimit = "&#8784;"

-- | The geometrically equal to HTML entity ('≑').
geometricallyEqualTo :: String
geometricallyEqualTo = "&#8785;"

-- | The approximately equal to or the image of HTML entity ('≒').
approximatelyEqualToOrTheImageOf :: String
approximatelyEqualToOrTheImageOf = "&#8786;"

-- | The image of or approximately equal to HTML entity ('≓').
imageOfOrApproximatelyEqualTo :: String
imageOfOrApproximatelyEqualTo = "&#8787;"

-- | The colon equals HTML entity ('≔').
colonEquals :: String
colonEquals = "&#8788;"

-- | The equals colon HTML entity ('≕').
equalsColon :: String
equalsColon = "&#8789;"

-- | The ring in equal to HTML entity ('≖').
ringInEqualTo :: String
ringInEqualTo = "&#8790;"

-- | The ring equal to HTML entity ('≗').
ringEqualTo :: String
ringEqualTo = "&#8791;"

-- | The estimates HTML entity ('≙').
estimates :: String
estimates = "&#8793;"

-- | The equiangular to HTML entity ('≚').
equiangularTo :: String
equiangularTo = "&#8794;"

-- | The Star Equals HTML entity ('≛').
starEquals :: String
starEquals = "&#8795;"

-- | The delta equal to HTML entity ('≜').
deltaEqualTo :: String
deltaEqualTo = "&#8796;"

-- | The Equal To By Definition HTML entity ('≝').
equalToByDefinition :: String
equalToByDefinition = "&#8797;"

-- | The questioned equal to HTML entity ('≟').
questionedEqualTo :: String
questionedEqualTo = "&#8799;"

-- | The not equal to HTML entity ('≠').
notEqualTo :: String
notEqualTo = "&#8800;"

-- | The identical to HTML entity ('≡').
identicalTo :: String
identicalTo = "&#8801;"

-- | The not identical to HTML entity ('≢').
notIdenticalTo :: String
notIdenticalTo = "&#8802;"

-- | The less-than or equal to HTML entity ('≤').
lessThanOrEqualTo :: String
lessThanOrEqualTo = "&#8804;"

-- | The greater-than or equal to HTML entity ('≥').
greaterThanOrEqualTo :: String
greaterThanOrEqualTo = "&#8805;"

-- | The less-than over equal to HTML entity ('≦').
lessThanOverEqualTo :: String
lessThanOverEqualTo = "&#8806;"

-- | The greater-than over equal to HTML entity ('≧').
greaterThanOverEqualTo :: String
greaterThanOverEqualTo = "&#8807;"

-- | The less-than but not equal to HTML entity ('≨').
lessThanButNotEqualTo :: String
lessThanButNotEqualTo = "&#8808;"

-- | The greater-than but not equal to HTML entity ('≩').
greaterThanButNotEqualTo :: String
greaterThanButNotEqualTo = "&#8809;"

-- | The much less-than HTML entity ('≪').
muchLessThan :: String
muchLessThan = "&#8810;"

-- | The much greater-than HTML entity ('≫').
muchGreaterThan :: String
muchGreaterThan = "&#8811;"

-- | The between HTML entity ('≬').
between :: String
between = "&#8812;"

-- | The not equivalent to HTML entity ('≭').
notEquivalentTo :: String
notEquivalentTo = "&#8813;"

-- | The not less-than HTML entity ('≮').
notLessThan :: String
notLessThan = "&#8814;"

-- | The not greater-than HTML entity ('≯').
notGreaterThan :: String
notGreaterThan = "&#8815;"

-- | The neither less-than nor equal to HTML entity ('≰').
neitherLessThanNorEqualTo :: String
neitherLessThanNorEqualTo = "&#8816;"

-- | The neither greater-than nor equal to HTML entity ('≱').
neitherGreaterThanNorEqualTo :: String
neitherGreaterThanNorEqualTo = "&#8817;"

-- | The less-than or equivalent to HTML entity ('≲').
lessThanOrEquivalentTo :: String
lessThanOrEquivalentTo = "&#8818;"

-- | The greater-than or equivalent to HTML entity ('≳').
greaterThanOrEquivalentTo :: String
greaterThanOrEquivalentTo = "&#8819;"

-- | The neither less-than nor equivalent to HTML entity ('≴').
neitherLessThanNorEquivalentTo :: String
neitherLessThanNorEquivalentTo = "&#8820;"

-- | The neither greater-than nor equivalent to HTML entity ('≵').
neitherGreaterThanNorEquivalentTo :: String
neitherGreaterThanNorEquivalentTo = "&#8821;"

-- | The less-than or greater-than HTML entity ('≶').
lessThanOrGreaterThan :: String
lessThanOrGreaterThan = "&#8822;"

-- | The greater-than or less-than HTML entity ('≷').
greaterThanOrLessThan :: String
greaterThanOrLessThan = "&#8823;"

-- | The neither less-than nor greater-than HTML entity ('≸').
neitherLessThanNorGreaterThan :: String
neitherLessThanNorGreaterThan = "&#8824;"

-- | The neither greater-than nor less-than HTML entity ('≹').
neitherGreaterThanNorLessThan :: String
neitherGreaterThanNorLessThan = "&#8825;"

-- | The precedes HTML entity ('≺').
precedes :: String
precedes = "&#8826;"

-- | The succeeds HTML entity ('≻').
succeeds :: String
succeeds = "&#8827;"

-- | The precedes or equal to HTML entity ('≼').
precedesOrEqualTo :: String
precedesOrEqualTo = "&#8828;"

-- | The succeeds or equal to HTML entity ('≽').
succeedsOrEqualTo :: String
succeedsOrEqualTo = "&#8829;"

-- | The precedes or equivalent to HTML entity ('≾').
precedesOrEquivalentTo :: String
precedesOrEquivalentTo = "&#8830;"

-- | The succeeds or equivalent to HTML entity ('≿').
succeedsOrEquivalentTo :: String
succeedsOrEquivalentTo = "&#8831;"

-- | The does not precede HTML entity ('⊀').
doesNotPrecede :: String
doesNotPrecede = "&#8832;"

-- | The does not succeed HTML entity ('⊁').
doesNotSucceed :: String
doesNotSucceed = "&#8833;"

-- | The subset of HTML entity ('⊂').
subsetOf :: String
subsetOf = "&#8834;"

-- | The superset of HTML entity ('⊃').
supersetOf :: String
supersetOf = "&#8835;"

-- | The not a subset of HTML entity ('⊄').
notASubsetOf :: String
notASubsetOf = "&#8836;"

-- | The not a superset of HTML entity ('⊅').
notASupersetOf :: String
notASupersetOf = "&#8837;"

-- | The subset of or equal to HTML entity ('⊆').
subsetOfOrEqualTo :: String
subsetOfOrEqualTo = "&#8838;"

-- | The superset of or equal to HTML entity ('⊇').
supersetOfOrEqualTo :: String
supersetOfOrEqualTo = "&#8839;"

-- | The neither a subset of nor equal to HTML entity ('⊈').
neitherASubsetOfNorEqualTo :: String
neitherASubsetOfNorEqualTo = "&#8840;"

-- | The neither a superset of nor equal to HTML entity ('⊉').
neitherASupersetOfNorEqualTo :: String
neitherASupersetOfNorEqualTo = "&#8841;"

-- | The subset of with not equal to HTML entity ('⊊').
subsetOfWithNotEqualTo :: String
subsetOfWithNotEqualTo = "&#8842;"

-- | The superset of with not equal to HTML entity ('⊋').
supersetOfWithNotEqualTo :: String
supersetOfWithNotEqualTo = "&#8843;"

-- | The multiset multiplication HTML entity ('⊍').
multisetMultiplication :: String
multisetMultiplication = "&#8845;"

-- | The multiset union HTML entity ('⊎').
multisetUnion :: String
multisetUnion = "&#8846;"

-- | The square image of HTML entity ('⊏').
squareImageOf :: String
squareImageOf = "&#8847;"

-- | The square original of HTML entity ('⊐').
squareOriginalOf :: String
squareOriginalOf = "&#8848;"

-- | The square image of or equal to HTML entity ('⊑').
squareImageOfOrEqualTo :: String
squareImageOfOrEqualTo = "&#8849;"

-- | The square original of or equal to HTML entity ('⊒').
squareOriginalOfOrEqualTo :: String
squareOriginalOfOrEqualTo = "&#8850;"

-- | The square cap HTML entity ('⊓').
squareCap :: String
squareCap = "&#8851;"

-- | The square cup HTML entity ('⊔').
squareCup :: String
squareCup = "&#8852;"

-- | The circled plus HTML entity ('⊕').
circledPlus :: String
circledPlus = "&#8853;"

-- | The circled minus HTML entity ('⊖').
circledMinus :: String
circledMinus = "&#8854;"

-- | The circled times HTML entity ('⊗').
circledTimes :: String
circledTimes = "&#8855;"

-- | The circled division slash HTML entity ('⊘').
circledDivisionSlash :: String
circledDivisionSlash = "&#8856;"

-- | The circled dot operator HTML entity ('⊙').
circledDotOperator :: String
circledDotOperator = "&#8857;"

-- | The circled ring operator HTML entity ('⊚').
circledRingOperator :: String
circledRingOperator = "&#8858;"

-- | The circled asterisk operator HTML entity ('⊛').
circledAsteriskOperator :: String
circledAsteriskOperator = "&#8859;"

-- | The Circled Equals HTML entity ('⊜').
circledEquals :: String
circledEquals = "&#8860;"

-- | The circled dash HTML entity ('⊝').
circledDash :: String
circledDash = "&#8861;"

-- | The squared plus HTML entity ('⊞').
squaredPlus :: String
squaredPlus = "&#8862;"

-- | The squared minus HTML entity ('⊟').
squaredMinus :: String
squaredMinus = "&#8863;"

-- | The squared times HTML entity ('⊠').
squaredTimes :: String
squaredTimes = "&#8864;"

-- | The squared dot operator HTML entity ('⊡').
squaredDotOperator :: String
squaredDotOperator = "&#8865;"

-- | The right tack HTML entity ('⊢').
rightTack :: String
rightTack = "&#8866;"

-- | The left tack HTML entity ('⊣').
leftTack :: String
leftTack = "&#8867;"

-- | The down tack HTML entity ('⊤').
downTack :: String
downTack = "&#8868;"

-- | The up tack HTML entity ('⊥').
upTack :: String
upTack = "&#8869;"

-- | The models HTML entity ('⊧').
models :: String
models = "&#8871;"

-- | The true HTML entity ('⊨').
true :: String
true = "&#8872;"

-- | The forces HTML entity ('⊩').
forces :: String
forces = "&#8873;"

-- | The triple vertical bar right turnstile HTML entity ('⊪').
tripleVerticalBarRightTurnstile :: String
tripleVerticalBarRightTurnstile = "&#8874;"

-- | The double vertical bar double right turnstile HTML entity ('⊫').
doubleVerticalBarDoubleRightTurnstile :: String
doubleVerticalBarDoubleRightTurnstile = "&#8875;"

-- | The does not prove HTML entity ('⊬').
doesNotProve :: String
doesNotProve = "&#8876;"

-- | The not true HTML entity ('⊭').
notTrue :: String
notTrue = "&#8877;"

-- | The does not force HTML entity ('⊮').
doesNotForce :: String
doesNotForce = "&#8878;"

-- | The negated double vertical bar double right turnstile HTML entity ('⊯').
negatedDoubleVerticalBarDoubleRightTurnstile :: String
negatedDoubleVerticalBarDoubleRightTurnstile = "&#8879;"

-- | The precedes under relation HTML entity ('⊰').
precedesUnderRelation :: String
precedesUnderRelation = "&#8880;"

-- | The normal subgroup of HTML entity ('⊲').
normalSubgroupOf :: String
normalSubgroupOf = "&#8882;"

-- | The contains as normal subgroup HTML entity ('⊳').
containsAsNormalSubgroup :: String
containsAsNormalSubgroup = "&#8883;"

-- | The normal subgroup of or equal to HTML entity ('⊴').
normalSubgroupOfOrEqualTo :: String
normalSubgroupOfOrEqualTo = "&#8884;"

-- | The contains as normal subgroup or equal to HTML entity ('⊵').
containsAsNormalSubgroupOrEqualTo :: String
containsAsNormalSubgroupOrEqualTo = "&#8885;"

-- | The original of HTML entity ('⊶').
originalOf :: String
originalOf = "&#8886;"

-- | The image of HTML entity ('⊷').
imageOf :: String
imageOf = "&#8887;"

-- | The multimap HTML entity ('⊸').
multimap :: String
multimap = "&#8888;"

-- | The hermitian conjugate matrix HTML entity ('⊹').
hermitianConjugateMatrix :: String
hermitianConjugateMatrix = "&#8889;"

-- | The intercalate HTML entity ('⊺').
intercalate :: String
intercalate = "&#8890;"

-- | The xor HTML entity ('⊻').
xor :: String
xor = "&#8891;"

-- | The nor HTML entity ('⊽').
nor :: String
nor = "&#8893;"

-- | The right angle with arc HTML entity ('⊾').
rightAngleWithArc :: String
rightAngleWithArc = "&#8894;"

-- | The right triangle HTML entity ('⊿').
rightTriangle :: String
rightTriangle = "&#8895;"

-- | The n-ary logical and HTML entity ('⋀').
nAryLogicalAnd :: String
nAryLogicalAnd = "&#8896;"

-- | The n-ary logical or HTML entity ('⋁').
nAryLogicalOr :: String
nAryLogicalOr = "&#8897;"

-- | The n-ary intersection HTML entity ('⋂').
nAryIntersection :: String
nAryIntersection = "&#8898;"

-- | The n-ary union HTML entity ('⋃').
nAryUnion :: String
nAryUnion = "&#8899;"

-- | The diamond operator HTML entity ('⋄').
diamondOperator :: String
diamondOperator = "&#8900;"

-- | The dot operator HTML entity ('⋅').
dotOperator :: String
dotOperator = "&#8901;"

-- | The star operator HTML entity ('⋆').
starOperator :: String
starOperator = "&#8902;"

-- | The division times HTML entity ('⋇').
divisionTimes :: String
divisionTimes = "&#8903;"

-- | The bowtie HTML entity ('⋈').
bowtie :: String
bowtie = "&#8904;"

-- | The left normal factor semidirect product HTML entity ('⋉').
leftNormalFactorSemidirectProduct :: String
leftNormalFactorSemidirectProduct = "&#8905;"

-- | The right normal factor semidirect product HTML entity ('⋊').
rightNormalFactorSemidirectProduct :: String
rightNormalFactorSemidirectProduct = "&#8906;"

-- | The left semidirect product HTML entity ('⋋').
leftSemidirectProduct :: String
leftSemidirectProduct = "&#8907;"

-- | The right semidirect product HTML entity ('⋌').
rightSemidirectProduct :: String
rightSemidirectProduct = "&#8908;"

-- | The reversed tilde equals HTML entity ('⋍').
reversedTildeEquals :: String
reversedTildeEquals = "&#8909;"

-- | The curly logical or HTML entity ('⋎').
curlyLogicalOr :: String
curlyLogicalOr = "&#8910;"

-- | The curly logical and HTML entity ('⋏').
curlyLogicalAnd :: String
curlyLogicalAnd = "&#8911;"

-- | The double subset HTML entity ('⋐').
doubleSubset :: String
doubleSubset = "&#8912;"

-- | The double superset HTML entity ('⋑').
doubleSuperset :: String
doubleSuperset = "&#8913;"

-- | The double intersection HTML entity ('⋒').
doubleIntersection :: String
doubleIntersection = "&#8914;"

-- | The double union HTML entity ('⋓').
doubleUnion :: String
doubleUnion = "&#8915;"

-- | The pitchfork HTML entity ('⋔').
pitchfork :: String
pitchfork = "&#8916;"

-- | The equal and parallel to HTML entity ('⋕').
equalAndParallelTo :: String
equalAndParallelTo = "&#8917;"

-- | The less-than with dot HTML entity ('⋖').
lessThanWithDot :: String
lessThanWithDot = "&#8918;"

-- | The greater-than with dot HTML entity ('⋗').
greaterThanWithDot :: String
greaterThanWithDot = "&#8919;"

-- | The very much less-than HTML entity ('⋘').
veryMuchLessThan :: String
veryMuchLessThan = "&#8920;"

-- | The very much greater-than HTML entity ('⋙').
veryMuchGreaterThan :: String
veryMuchGreaterThan = "&#8921;"

-- | The less-than equal to or greater-than HTML entity ('⋚').
lessThanEqualToOrGreaterThan :: String
lessThanEqualToOrGreaterThan = "&#8922;"

-- | The greater-than equal to or less-than HTML entity ('⋛').
greaterThanEqualToOrLessThan :: String
greaterThanEqualToOrLessThan = "&#8923;"

-- | The Equal To Or Less-than HTML entity ('⋜').
equalToOrLessThan :: String
equalToOrLessThan = "&#8924;"

-- | The Equal To Or Greater-than HTML entity ('⋝').
equalToOrGreaterThan :: String
equalToOrGreaterThan = "&#8925;"

-- | The equal to or precedes HTML entity ('⋞').
equalToOrPrecedes :: String
equalToOrPrecedes = "&#8926;"

-- | The equal to or succeeds HTML entity ('⋟').
equalToOrSucceeds :: String
equalToOrSucceeds = "&#8927;"

-- | The does not precede or equal HTML entity ('⋠').
doesNotPrecedeOrEqual :: String
doesNotPrecedeOrEqual = "&#8928;"

-- | The does not succeed or equal HTML entity ('⋡').
doesNotSucceedOrEqual :: String
doesNotSucceedOrEqual = "&#8929;"

-- | The not square image of or equal to HTML entity ('⋢').
notSquareImageOfOrEqualTo :: String
notSquareImageOfOrEqualTo = "&#8930;"

-- | The not square original of or equal to HTML entity ('⋣').
notSquareOriginalOfOrEqualTo :: String
notSquareOriginalOfOrEqualTo = "&#8931;"

-- | The Square Image Of Or Not Equal To HTML entity ('⋤').
squareImageOfOrNotEqualTo :: String
squareImageOfOrNotEqualTo = "&#8932;"

-- | The Square Original Of Or Not Equal To HTML entity ('⋥').
squareOriginalOfOrNotEqualTo :: String
squareOriginalOfOrNotEqualTo = "&#8933;"

-- | The less-than but not equivalent to HTML entity ('⋦').
lessThanButNotEquivalentTo :: String
lessThanButNotEquivalentTo = "&#8934;"

-- | The greater-than but not equivalent to HTML entity ('⋧').
greaterThanButNotEquivalentTo :: String
greaterThanButNotEquivalentTo = "&#8935;"

-- | The precedes but not equivalent to HTML entity ('⋨').
precedesButNotEquivalentTo :: String
precedesButNotEquivalentTo = "&#8936;"

-- | The succeeds but not equivalent to HTML entity ('⋩').
succeedsButNotEquivalentTo :: String
succeedsButNotEquivalentTo = "&#8937;"

-- | The not normal subgroup of HTML entity ('⋪').
notNormalSubgroupOf :: String
notNormalSubgroupOf = "&#8938;"

-- | The does not contain as normal subgroup HTML entity ('⋫').
doesNotContainAsNormalSubgroup :: String
doesNotContainAsNormalSubgroup = "&#8939;"

-- | The not normal subgroup of or equal to HTML entity ('⋬').
notNormalSubgroupOfOrEqualTo :: String
notNormalSubgroupOfOrEqualTo = "&#8940;"

-- | The does not contain as normal subgroup or equal HTML entity ('⋭').
doesNotContainAsNormalSubgroupOrEqual :: String
doesNotContainAsNormalSubgroupOrEqual = "&#8941;"

-- | The vertical ellipsis HTML entity ('⋮').
verticalEllipsis :: String
verticalEllipsis = "&#8942;"

-- | The midline horizontal ellipsis HTML entity ('⋯').
midlineHorizontalEllipsis :: String
midlineHorizontalEllipsis = "&#8943;"

-- | The up right diagonal ellipsis HTML entity ('⋰').
upRightDiagonalEllipsis :: String
upRightDiagonalEllipsis = "&#8944;"

-- | The down right diagonal ellipsis HTML entity ('⋱').
downRightDiagonalEllipsis :: String
downRightDiagonalEllipsis = "&#8945;"

-- | The element of with long horizontal stroke HTML entity ('⋲').
elementOfWithLongHorizontalStroke :: String
elementOfWithLongHorizontalStroke = "&#8946;"

-- | The element of with vertical bar at end of horizontal stroke HTML entity ('⋳').
elementOfWithVerticalBarAtEndOfHorizontalStroke :: String
elementOfWithVerticalBarAtEndOfHorizontalStroke = "&#8947;"

-- | The small element of with vertical bar at end of horizontal stroke HTML entity ('⋴').
smallElementOfWithVerticalBarAtEndOfHorizontalStroke :: String
smallElementOfWithVerticalBarAtEndOfHorizontalStroke = "&#8948;"

-- | The element of with dot above HTML entity ('⋵').
elementOfWithDotAbove :: String
elementOfWithDotAbove = "&#8949;"

-- | The element of with overbar HTML entity ('⋶').
elementOfWithOverbar :: String
elementOfWithOverbar = "&#8950;"

-- | The small element of with overbar HTML entity ('⋷').
smallElementOfWithOverbar :: String
smallElementOfWithOverbar = "&#8951;"

-- | The element of with two horizontal strokes HTML entity ('⋹').
elementOfWithTwoHorizontalStrokes :: String
elementOfWithTwoHorizontalStrokes = "&#8953;"

-- | The contains with long horizontal stroke HTML entity ('⋺').
containsWithLongHorizontalStroke :: String
containsWithLongHorizontalStroke = "&#8954;"

-- | The contains with vertical bar at end of horizontal stroke HTML entity ('⋻').
containsWithVerticalBarAtEndOfHorizontalStroke :: String
containsWithVerticalBarAtEndOfHorizontalStroke = "&#8955;"

-- | The small contains with vertical bar at end of horizontal stroke HTML entity ('⋼').
smallContainsWithVerticalBarAtEndOfHorizontalStroke :: String
smallContainsWithVerticalBarAtEndOfHorizontalStroke = "&#8956;"

-- | The contains with overbar HTML entity ('⋽').
containsWithOverbar :: String
containsWithOverbar = "&#8957;"

-- | The small contains with overbar HTML entity ('⋾').
smallContainsWithOverbar :: String
smallContainsWithOverbar = "&#8958;"

-- | The projective HTML entity ('⌅').
projective :: String
projective = "&#8965;"

-- | The perspective HTML entity ('⌆').
perspective :: String
perspective = "&#8966;"

-- | The left ceiling HTML entity ('⌈').
leftCeiling :: String
leftCeiling = "&#8968;"

-- | The right ceiling HTML entity ('⌉').
rightCeiling :: String
rightCeiling = "&#8969;"

-- | The left floor HTML entity ('⌊').
leftFloor :: String
leftFloor = "&#8970;"

-- | The right floor HTML entity ('⌋').
rightFloor :: String
rightFloor = "&#8971;"

-- | The bottom right crop HTML entity ('⌌').
bottomRightCrop :: String
bottomRightCrop = "&#8972;"

-- | The bottom left crop HTML entity ('⌍').
bottomLeftCrop :: String
bottomLeftCrop = "&#8973;"

-- | The top right crop HTML entity ('⌎').
topRightCrop :: String
topRightCrop = "&#8974;"

-- | The top left crop HTML entity ('⌏').
topLeftCrop :: String
topLeftCrop = "&#8975;"

-- | The reversed not sign HTML entity ('⌐').
reversedNotSign :: String
reversedNotSign = "&#8976;"

-- | The arc HTML entity ('⌒').
arc :: String
arc = "&#8978;"

-- | The segment HTML entity ('⌓').
segment :: String
segment = "&#8979;"

-- | The telephone recorder HTML entity ('⌕').
telephoneRecorder :: String
telephoneRecorder = "&#8981;"

-- | The position indicator HTML entity ('⌖').
positionIndicator :: String
positionIndicator = "&#8982;"

-- | The top left corner HTML entity ('⌜').
topLeftCorner :: String
topLeftCorner = "&#8988;"

-- | The top right corner HTML entity ('⌝').
topRightCorner :: String
topRightCorner = "&#8989;"

-- | The bottom left corner HTML entity ('⌞').
bottomLeftCorner :: String
bottomLeftCorner = "&#8990;"

-- | The bottom right corner HTML entity ('⌟').
bottomRightCorner :: String
bottomRightCorner = "&#8991;"

-- | The frown HTML entity ('⌢').
frown :: String
frown = "&#8994;"

-- | The smile HTML entity ('⌣').
smile :: String
smile = "&#8995;"

-- | The left-pointing angle bracket = bra HTML entity ('〈').
leftPointingAngleBracket :: String
leftPointingAngleBracket = "&#9001;"

-- | The right-pointing angle bracket = ket HTML entity ('〉').
rightPointingAngleBracket :: String
rightPointingAngleBracket = "&#9002;"

-- | The cylindricity HTML entity ('⌭').
cylindricity :: String
cylindricity = "&#9005;"

-- | The all around-profile HTML entity ('⌮').
allAroundProfile :: String
allAroundProfile = "&#9006;"

-- | The apl functional symbol i-beam HTML entity ('⌶').
aplFunctionalSymbolIBeam :: String
aplFunctionalSymbolIBeam = "&#9014;"

-- | The Apl Functional Symbol Quad Equal HTML entity ('⌸').
aplFunctionalSymbolQuadEqual :: String
aplFunctionalSymbolQuadEqual = "&#9016;"

-- | The apl functional symbol circle stile HTML entity ('⌽').
aplFunctionalSymbolCircleStile :: String
aplFunctionalSymbolCircleStile = "&#9021;"

-- | The apl functional symbol slash bar HTML entity ('⌿').
aplFunctionalSymbolSlashBar :: String
aplFunctionalSymbolSlashBar = "&#9023;"

-- | The Apl Functional Symbol Quad Less-than HTML entity ('⍃').
aplFunctionalSymbolQuadLessThan :: String
aplFunctionalSymbolQuadLessThan = "&#9027;"

-- | The Apl Functional Symbol Quad Greater-than HTML entity ('⍄').
aplFunctionalSymbolQuadGreaterThan :: String
aplFunctionalSymbolQuadGreaterThan = "&#9028;"

-- | The Apl Functional Symbol Delta Stile HTML entity ('⍋').
aplFunctionalSymbolDeltaStile :: String
aplFunctionalSymbolDeltaStile = "&#9035;"

-- | The Apl Functional Symbol Quad Delta HTML entity ('⍍').
aplFunctionalSymbolQuadDelta :: String
aplFunctionalSymbolQuadDelta = "&#9037;"

-- | The Apl Functional Symbol Delta Underbar HTML entity ('⍙').
aplFunctionalSymbolDeltaUnderbar :: String
aplFunctionalSymbolDeltaUnderbar = "&#9049;"

-- | The Apl Functional Symbol Greater-than Diaeresis HTML entity ('⍩').
aplFunctionalSymbolGreaterThanDiaeresis :: String
aplFunctionalSymbolGreaterThanDiaeresis = "&#9065;"

-- | The Apl Functional Symbol Quad Not Equal HTML entity ('⍯').
aplFunctionalSymbolQuadNotEqual :: String
aplFunctionalSymbolQuadNotEqual = "&#9071;"

-- | The right angle with downwards zigzag arrow HTML entity ('⍼').
rightAngleWithDownwardsZigzagArrow :: String
rightAngleWithDownwardsZigzagArrow = "&#9084;"

-- | The upper left or lower right curly bracket section HTML entity ('⎰').
upperLeftOrLowerRightCurlyBracketSection :: String
upperLeftOrLowerRightCurlyBracketSection = "&#9136;"

-- | The upper right or lower left curly bracket section HTML entity ('⎱').
upperRightOrLowerLeftCurlyBracketSection :: String
upperRightOrLowerLeftCurlyBracketSection = "&#9137;"

-- | The Summation Top HTML entity ('⎲').
summationTop :: String
summationTop = "&#9138;"

-- | The Summation Bottom HTML entity ('⎳').
summationBottom :: String
summationBottom = "&#9139;"

-- | The top square bracket HTML entity ('⎴').
topSquareBracket :: String
topSquareBracket = "&#9140;"

-- | The bottom square bracket HTML entity ('⎵').
bottomSquareBracket :: String
bottomSquareBracket = "&#9141;"

-- | The bottom square bracket over top square bracket HTML entity ('⎶').
bottomSquareBracketOverTopSquareBracket :: String
bottomSquareBracketOverTopSquareBracket = "&#9142;"

-- | The top parenthesis HTML entity ('⏜').
topParenthesis :: String
topParenthesis = "&#9180;"

-- | The bottom parenthesis HTML entity ('⏝').
bottomParenthesis :: String
bottomParenthesis = "&#9181;"

-- | The top curly bracket HTML entity ('⏞').
topCurlyBracket :: String
topCurlyBracket = "&#9182;"

-- | The bottom curly bracket HTML entity ('⏟').
bottomCurlyBracket :: String
bottomCurlyBracket = "&#9183;"

-- | The white trapezium HTML entity ('⏢').
whiteTrapezium :: String
whiteTrapezium = "&#9186;"

-- | The electrical intersection HTML entity ('⏧').
electricalIntersection :: String
electricalIntersection = "&#9191;"

-- | The open box HTML entity ('␣').
openBox :: String
openBox = "&#9251;"

-- | The circled latin capital letter s HTML entity ('Ⓢ').
circledLatinCapitalLetterS :: String
circledLatinCapitalLetterS = "&#9416;"

-- | The box drawings light horizontal HTML entity ('─').
boxDrawingsLightHorizontal :: String
boxDrawingsLightHorizontal = "&#9472;"

-- | The box drawings light vertical HTML entity ('│').
boxDrawingsLightVertical :: String
boxDrawingsLightVertical = "&#9474;"

-- | The box drawings light down and right HTML entity ('┌').
boxDrawingsLightDownAndRight :: String
boxDrawingsLightDownAndRight = "&#9484;"

-- | The box drawings light down and left HTML entity ('┐').
boxDrawingsLightDownAndLeft :: String
boxDrawingsLightDownAndLeft = "&#9488;"

-- | The box drawings light up and right HTML entity ('└').
boxDrawingsLightUpAndRight :: String
boxDrawingsLightUpAndRight = "&#9492;"

-- | The box drawings light up and left HTML entity ('┘').
boxDrawingsLightUpAndLeft :: String
boxDrawingsLightUpAndLeft = "&#9496;"

-- | The box drawings light vertical and right HTML entity ('├').
boxDrawingsLightVerticalAndRight :: String
boxDrawingsLightVerticalAndRight = "&#9500;"

-- | The box drawings light vertical and left HTML entity ('┤').
boxDrawingsLightVerticalAndLeft :: String
boxDrawingsLightVerticalAndLeft = "&#9508;"

-- | The box drawings light down and horizontal HTML entity ('┬').
boxDrawingsLightDownAndHorizontal :: String
boxDrawingsLightDownAndHorizontal = "&#9516;"

-- | The box drawings light up and horizontal HTML entity ('┴').
boxDrawingsLightUpAndHorizontal :: String
boxDrawingsLightUpAndHorizontal = "&#9524;"

-- | The box drawings light vertical and horizontal HTML entity ('┼').
boxDrawingsLightVerticalAndHorizontal :: String
boxDrawingsLightVerticalAndHorizontal = "&#9532;"

-- | The box drawings double horizontal HTML entity ('═').
boxDrawingsDoubleHorizontal :: String
boxDrawingsDoubleHorizontal = "&#9552;"

-- | The box drawings double vertical HTML entity ('║').
boxDrawingsDoubleVertical :: String
boxDrawingsDoubleVertical = "&#9553;"

-- | The box drawings down single and right double HTML entity ('╒').
boxDrawingsDownSingleAndRightDouble :: String
boxDrawingsDownSingleAndRightDouble = "&#9554;"

-- | The box drawings down double and right single HTML entity ('╓').
boxDrawingsDownDoubleAndRightSingle :: String
boxDrawingsDownDoubleAndRightSingle = "&#9555;"

-- | The box drawings double down and right HTML entity ('╔').
boxDrawingsDoubleDownAndRight :: String
boxDrawingsDoubleDownAndRight = "&#9556;"

-- | The box drawings down single and left double HTML entity ('╕').
boxDrawingsDownSingleAndLeftDouble :: String
boxDrawingsDownSingleAndLeftDouble = "&#9557;"

-- | The box drawings down double and left single HTML entity ('╖').
boxDrawingsDownDoubleAndLeftSingle :: String
boxDrawingsDownDoubleAndLeftSingle = "&#9558;"

-- | The box drawings double down and left HTML entity ('╗').
boxDrawingsDoubleDownAndLeft :: String
boxDrawingsDoubleDownAndLeft = "&#9559;"

-- | The box drawings up single and right double HTML entity ('╘').
boxDrawingsUpSingleAndRightDouble :: String
boxDrawingsUpSingleAndRightDouble = "&#9560;"

-- | The box drawings up double and right single HTML entity ('╙').
boxDrawingsUpDoubleAndRightSingle :: String
boxDrawingsUpDoubleAndRightSingle = "&#9561;"

-- | The box drawings double up and right HTML entity ('╚').
boxDrawingsDoubleUpAndRight :: String
boxDrawingsDoubleUpAndRight = "&#9562;"

-- | The box drawings up single and left double HTML entity ('╛').
boxDrawingsUpSingleAndLeftDouble :: String
boxDrawingsUpSingleAndLeftDouble = "&#9563;"

-- | The box drawings up double and left single HTML entity ('╜').
boxDrawingsUpDoubleAndLeftSingle :: String
boxDrawingsUpDoubleAndLeftSingle = "&#9564;"

-- | The box drawings double up and left HTML entity ('╝').
boxDrawingsDoubleUpAndLeft :: String
boxDrawingsDoubleUpAndLeft = "&#9565;"

-- | The box drawings vertical single and right double HTML entity ('╞').
boxDrawingsVerticalSingleAndRightDouble :: String
boxDrawingsVerticalSingleAndRightDouble = "&#9566;"

-- | The box drawings vertical double and right single HTML entity ('╟').
boxDrawingsVerticalDoubleAndRightSingle :: String
boxDrawingsVerticalDoubleAndRightSingle = "&#9567;"

-- | The box drawings double vertical and right HTML entity ('╠').
boxDrawingsDoubleVerticalAndRight :: String
boxDrawingsDoubleVerticalAndRight = "&#9568;"

-- | The box drawings vertical single and left double HTML entity ('╡').
boxDrawingsVerticalSingleAndLeftDouble :: String
boxDrawingsVerticalSingleAndLeftDouble = "&#9569;"

-- | The box drawings vertical double and left single HTML entity ('╢').
boxDrawingsVerticalDoubleAndLeftSingle :: String
boxDrawingsVerticalDoubleAndLeftSingle = "&#9570;"

-- | The box drawings double vertical and left HTML entity ('╣').
boxDrawingsDoubleVerticalAndLeft :: String
boxDrawingsDoubleVerticalAndLeft = "&#9571;"

-- | The box drawings down single and horizontal double HTML entity ('╤').
boxDrawingsDownSingleAndHorizontalDouble :: String
boxDrawingsDownSingleAndHorizontalDouble = "&#9572;"

-- | The box drawings down double and horizontal single HTML entity ('╥').
boxDrawingsDownDoubleAndHorizontalSingle :: String
boxDrawingsDownDoubleAndHorizontalSingle = "&#9573;"

-- | The box drawings double down and horizontal HTML entity ('╦').
boxDrawingsDoubleDownAndHorizontal :: String
boxDrawingsDoubleDownAndHorizontal = "&#9574;"

-- | The box drawings up single and horizontal double HTML entity ('╧').
boxDrawingsUpSingleAndHorizontalDouble :: String
boxDrawingsUpSingleAndHorizontalDouble = "&#9575;"

-- | The box drawings up double and horizontal single HTML entity ('╨').
boxDrawingsUpDoubleAndHorizontalSingle :: String
boxDrawingsUpDoubleAndHorizontalSingle = "&#9576;"

-- | The box drawings double up and horizontal HTML entity ('╩').
boxDrawingsDoubleUpAndHorizontal :: String
boxDrawingsDoubleUpAndHorizontal = "&#9577;"

-- | The box drawings vertical single and horizontal double HTML entity ('╪').
boxDrawingsVerticalSingleAndHorizontalDouble :: String
boxDrawingsVerticalSingleAndHorizontalDouble = "&#9578;"

-- | The box drawings vertical double and horizontal single HTML entity ('╫').
boxDrawingsVerticalDoubleAndHorizontalSingle :: String
boxDrawingsVerticalDoubleAndHorizontalSingle = "&#9579;"

-- | The box drawings double vertical and horizontal HTML entity ('╬').
boxDrawingsDoubleVerticalAndHorizontal :: String
boxDrawingsDoubleVerticalAndHorizontal = "&#9580;"

-- | The upper half block HTML entity ('▀').
upperHalfBlock :: String
upperHalfBlock = "&#9600;"

-- | The lower half block HTML entity ('▄').
lowerHalfBlock :: String
lowerHalfBlock = "&#9604;"

-- | The full block HTML entity ('█').
fullBlock :: String
fullBlock = "&#9608;"

-- | The light shade HTML entity ('░').
lightShade :: String
lightShade = "&#9617;"

-- | The medium shade HTML entity ('▒').
mediumShade :: String
mediumShade = "&#9618;"

-- | The dark shade HTML entity ('▓').
darkShade :: String
darkShade = "&#9619;"

-- | The black square HTML entity ('■').
blackSquare :: String
blackSquare = "&#9632;"

-- | The white square HTML entity ('□').
whiteSquare :: String
whiteSquare = "&#9633;"

-- | The white square with rounded corners HTML entity ('▢').
whiteSquareWithRoundedCorners :: String
whiteSquareWithRoundedCorners = "&#9634;"

-- | The white square containing black small square HTML entity ('▣').
whiteSquareContainingBlackSmallSquare :: String
whiteSquareContainingBlackSmallSquare = "&#9635;"

-- | The square with horizontal fill HTML entity ('▤').
squareWithHorizontalFill :: String
squareWithHorizontalFill = "&#9636;"

-- | The square with vertical fill HTML entity ('▥').
squareWithVerticalFill :: String
squareWithVerticalFill = "&#9637;"

-- | The square with orthogonal crosshatch fill HTML entity ('▦').
squareWithOrthogonalCrosshatchFill :: String
squareWithOrthogonalCrosshatchFill = "&#9638;"

-- | The square with upper left to lower right fill HTML entity ('▧').
squareWithUpperLeftToLowerRightFill :: String
squareWithUpperLeftToLowerRightFill = "&#9639;"

-- | The square with upper right to lower left fill HTML entity ('▨').
squareWithUpperRightToLowerLeftFill :: String
squareWithUpperRightToLowerLeftFill = "&#9640;"

-- | The square with diagonal crosshatch fill HTML entity ('▩').
squareWithDiagonalCrosshatchFill :: String
squareWithDiagonalCrosshatchFill = "&#9641;"

-- | The black small square HTML entity ('▪').
blackSmallSquare :: String
blackSmallSquare = "&#9642;"

-- | The white small square HTML entity ('▫').
whiteSmallSquare :: String
whiteSmallSquare = "&#9643;"

-- | The Black Rectangle HTML entity ('▬').
blackRectangle :: String
blackRectangle = "&#9644;"

-- | The white rectangle HTML entity ('▭').
whiteRectangle :: String
whiteRectangle = "&#9645;"

-- | The black vertical rectangle HTML entity ('▮').
blackVerticalRectangle :: String
blackVerticalRectangle = "&#9646;"

-- | The White Vertical Rectangle HTML entity ('▯').
whiteVerticalRectangle :: String
whiteVerticalRectangle = "&#9647;"

-- | The Black Parallelogram HTML entity ('▰').
blackParallelogram :: String
blackParallelogram = "&#9648;"

-- | The white parallelogram HTML entity ('▱').
whiteParallelogram :: String
whiteParallelogram = "&#9649;"

-- | The Black Up-pointing Triangle HTML entity ('▲').
blackUpPointingTriangle :: String
blackUpPointingTriangle = "&#9650;"

-- | The white up-pointing triangle HTML entity ('△').
whiteUpPointingTriangle :: String
whiteUpPointingTriangle = "&#9651;"

-- | The black up-pointing small triangle HTML entity ('▴').
blackUpPointingSmallTriangle :: String
blackUpPointingSmallTriangle = "&#9652;"

-- | The white up-pointing small triangle HTML entity ('▵').
whiteUpPointingSmallTriangle :: String
whiteUpPointingSmallTriangle = "&#9653;"

-- | The Black Right-pointing Triangle HTML entity ('▶').
blackRightPointingTriangle :: String
blackRightPointingTriangle = "&#9654;"

-- | The White Right-pointing Triangle HTML entity ('▷').
whiteRightPointingTriangle :: String
whiteRightPointingTriangle = "&#9655;"

-- | The black right-pointing small triangle HTML entity ('▸').
blackRightPointingSmallTriangle :: String
blackRightPointingSmallTriangle = "&#9656;"

-- | The white right-pointing small triangle HTML entity ('▹').
whiteRightPointingSmallTriangle :: String
whiteRightPointingSmallTriangle = "&#9657;"

-- | The Black Down-pointing Triangle HTML entity ('▼').
blackDownPointingTriangle :: String
blackDownPointingTriangle = "&#9660;"

-- | The white down-pointing triangle HTML entity ('▽').
whiteDownPointingTriangle :: String
whiteDownPointingTriangle = "&#9661;"

-- | The black down-pointing small triangle HTML entity ('▾').
blackDownPointingSmallTriangle :: String
blackDownPointingSmallTriangle = "&#9662;"

-- | The white down-pointing small triangle HTML entity ('▿').
whiteDownPointingSmallTriangle :: String
whiteDownPointingSmallTriangle = "&#9663;"

-- | The Black Left-pointing Triangle HTML entity ('◀').
blackLeftPointingTriangle :: String
blackLeftPointingTriangle = "&#9664;"

-- | The White Left-pointing Triangle HTML entity ('◁').
whiteLeftPointingTriangle :: String
whiteLeftPointingTriangle = "&#9665;"

-- | The black left-pointing small triangle HTML entity ('◂').
blackLeftPointingSmallTriangle :: String
blackLeftPointingSmallTriangle = "&#9666;"

-- | The white left-pointing small triangle HTML entity ('◃').
whiteLeftPointingSmallTriangle :: String
whiteLeftPointingSmallTriangle = "&#9667;"

-- | The Black Diamond HTML entity ('◆').
blackDiamond :: String
blackDiamond = "&#9670;"

-- | The White Diamond HTML entity ('◇').
whiteDiamond :: String
whiteDiamond = "&#9671;"

-- | The White Diamond Containing Black Small Diamond HTML entity ('◈').
whiteDiamondContainingBlackSmallDiamond :: String
whiteDiamondContainingBlackSmallDiamond = "&#9672;"

-- | The lozenge HTML entity ('◊').
lozenge :: String
lozenge = "&#9674;"

-- | The white circle HTML entity ('○').
whiteCircle :: String
whiteCircle = "&#9675;"

-- | The Dotted Circle HTML entity ('◌').
dottedCircle :: String
dottedCircle = "&#9676;"

-- | The Circle With Vertical Fill HTML entity ('◍').
circleWithVerticalFill :: String
circleWithVerticalFill = "&#9677;"

-- | The Black Circle HTML entity ('●').
blackCircle :: String
blackCircle = "&#9679;"

-- | The Circle With Left Half Black HTML entity ('◐').
circleWithLeftHalfBlack :: String
circleWithLeftHalfBlack = "&#9680;"

-- | The Circle With Right Half Black HTML entity ('◑').
circleWithRightHalfBlack :: String
circleWithRightHalfBlack = "&#9681;"

-- | The Circle With Lower Half Black HTML entity ('◒').
circleWithLowerHalfBlack :: String
circleWithLowerHalfBlack = "&#9682;"

-- | The Circle With Upper Half Black HTML entity ('◓').
circleWithUpperHalfBlack :: String
circleWithUpperHalfBlack = "&#9683;"

-- | The Circle With Upper Right Quadrant Black HTML entity ('◔').
circleWithUpperRightQuadrantBlack :: String
circleWithUpperRightQuadrantBlack = "&#9684;"

-- | The Circle With All But Upper Left Quadrant Black HTML entity ('◕').
circleWithAllButUpperLeftQuadrantBlack :: String
circleWithAllButUpperLeftQuadrantBlack = "&#9685;"

-- | The Left Half Black Circle HTML entity ('◖').
leftHalfBlackCircle :: String
leftHalfBlackCircle = "&#9686;"

-- | The Right Half Black Circle HTML entity ('◗').
rightHalfBlackCircle :: String
rightHalfBlackCircle = "&#9687;"

-- | The Inverse White Circle HTML entity ('◙').
inverseWhiteCircle :: String
inverseWhiteCircle = "&#9689;"

-- | The Upper Half Inverse White Circle HTML entity ('◚').
upperHalfInverseWhiteCircle :: String
upperHalfInverseWhiteCircle = "&#9690;"

-- | The Lower Half Inverse White Circle HTML entity ('◛').
lowerHalfInverseWhiteCircle :: String
lowerHalfInverseWhiteCircle = "&#9691;"

-- | The Upper Left Quadrant Circular Arc HTML entity ('◜').
upperLeftQuadrantCircularArc :: String
upperLeftQuadrantCircularArc = "&#9692;"

-- | The Upper Right Quadrant Circular Arc HTML entity ('◝').
upperRightQuadrantCircularArc :: String
upperRightQuadrantCircularArc = "&#9693;"

-- | The Lower Right Quadrant Circular Arc HTML entity ('◞').
lowerRightQuadrantCircularArc :: String
lowerRightQuadrantCircularArc = "&#9694;"

-- | The Lower Left Quadrant Circular Arc HTML entity ('◟').
lowerLeftQuadrantCircularArc :: String
lowerLeftQuadrantCircularArc = "&#9695;"

-- | The Upper Half Circle HTML entity ('◠').
upperHalfCircle :: String
upperHalfCircle = "&#9696;"

-- | The Lower Half Circle HTML entity ('◡').
lowerHalfCircle :: String
lowerHalfCircle = "&#9697;"

-- | The Black Lower Right Triangle HTML entity ('◢').
blackLowerRightTriangle :: String
blackLowerRightTriangle = "&#9698;"

-- | The Black Lower Left Triangle HTML entity ('◣').
blackLowerLeftTriangle :: String
blackLowerLeftTriangle = "&#9699;"

-- | The Black Upper Left Triangle HTML entity ('◤').
blackUpperLeftTriangle :: String
blackUpperLeftTriangle = "&#9700;"

-- | The Black Upper Right Triangle HTML entity ('◥').
blackUpperRightTriangle :: String
blackUpperRightTriangle = "&#9701;"

-- | The square with left half black HTML entity ('◧').
squareWithLeftHalfBlack :: String
squareWithLeftHalfBlack = "&#9703;"

-- | The square with right half black HTML entity ('◨').
squareWithRightHalfBlack :: String
squareWithRightHalfBlack = "&#9704;"

-- | The square with upper left diagonal half black HTML entity ('◩').
squareWithUpperLeftDiagonalHalfBlack :: String
squareWithUpperLeftDiagonalHalfBlack = "&#9705;"

-- | The square with lower right diagonal half black HTML entity ('◪').
squareWithLowerRightDiagonalHalfBlack :: String
squareWithLowerRightDiagonalHalfBlack = "&#9706;"

-- | The white square with vertical bisecting line HTML entity ('◫').
whiteSquareWithVerticalBisectingLine :: String
whiteSquareWithVerticalBisectingLine = "&#9707;"

-- | The white up-pointing triangle with dot HTML entity ('◬').
whiteUpPointingTriangleWithDot :: String
whiteUpPointingTriangleWithDot = "&#9708;"

-- | The Up-pointing Triangle With Left Half Black HTML entity ('◭').
upPointingTriangleWithLeftHalfBlack :: String
upPointingTriangleWithLeftHalfBlack = "&#9709;"

-- | The Up-pointing Triangle With Right Half Black HTML entity ('◮').
upPointingTriangleWithRightHalfBlack :: String
upPointingTriangleWithRightHalfBlack = "&#9710;"

-- | The large circle HTML entity ('◯').
largeCircle :: String
largeCircle = "&#9711;"

-- | The white square with upper left quadrant HTML entity ('◰').
whiteSquareWithUpperLeftQuadrant :: String
whiteSquareWithUpperLeftQuadrant = "&#9712;"

-- | The white square with lower left quadrant HTML entity ('◱').
whiteSquareWithLowerLeftQuadrant :: String
whiteSquareWithLowerLeftQuadrant = "&#9713;"

-- | The white square with lower right quadrant HTML entity ('◲').
whiteSquareWithLowerRightQuadrant :: String
whiteSquareWithLowerRightQuadrant = "&#9714;"

-- | The white square with upper right quadrant HTML entity ('◳').
whiteSquareWithUpperRightQuadrant :: String
whiteSquareWithUpperRightQuadrant = "&#9715;"

-- | The White Circle With Upper Left Quadrant HTML entity ('◴').
whiteCircleWithUpperLeftQuadrant :: String
whiteCircleWithUpperLeftQuadrant = "&#9716;"

-- | The White Circle With Lower Left Quadrant HTML entity ('◵').
whiteCircleWithLowerLeftQuadrant :: String
whiteCircleWithLowerLeftQuadrant = "&#9717;"

-- | The White Circle With Lower Right Quadrant HTML entity ('◶').
whiteCircleWithLowerRightQuadrant :: String
whiteCircleWithLowerRightQuadrant = "&#9718;"

-- | The White Circle With Upper Right Quadrant HTML entity ('◷').
whiteCircleWithUpperRightQuadrant :: String
whiteCircleWithUpperRightQuadrant = "&#9719;"

-- | The upper left triangle HTML entity ('◸').
upperLeftTriangle :: String
upperLeftTriangle = "&#9720;"

-- | The upper right triangle HTML entity ('◹').
upperRightTriangle :: String
upperRightTriangle = "&#9721;"

-- | The lower left triangle HTML entity ('◺').
lowerLeftTriangle :: String
lowerLeftTriangle = "&#9722;"

-- | The white medium square HTML entity ('◻').
whiteMediumSquare :: String
whiteMediumSquare = "&#9723;"

-- | The black medium square HTML entity ('◼').
blackMediumSquare :: String
blackMediumSquare = "&#9724;"

-- | The white medium small square HTML entity ('◽').
whiteMediumSmallSquare :: String
whiteMediumSmallSquare = "&#9725;"

-- | The black medium small square HTML entity ('◾').
blackMediumSmallSquare :: String
blackMediumSmallSquare = "&#9726;"

-- | The Lower Right Triangle HTML entity ('◿').
lowerRightTriangle :: String
lowerRightTriangle = "&#9727;"

-- | The black star HTML entity ('★').
blackStar :: String
blackStar = "&#9733;"

-- | The white star HTML entity ('☆').
whiteStar :: String
whiteStar = "&#9734;"

-- | The black telephone HTML entity ('☎').
blackTelephone :: String
blackTelephone = "&#9742;"

-- | The Trigram For Heaven HTML entity ('☰').
trigramForHeaven :: String
trigramForHeaven = "&#9776;"

-- | The Trigram For Lake HTML entity ('☱').
trigramForLake :: String
trigramForLake = "&#9777;"

-- | The Trigram For Fire HTML entity ('☲').
trigramForFire :: String
trigramForFire = "&#9778;"

-- | The Trigram For Thunder HTML entity ('☳').
trigramForThunder :: String
trigramForThunder = "&#9779;"

-- | The Trigram For Wind HTML entity ('☴').
trigramForWind :: String
trigramForWind = "&#9780;"

-- | The Trigram For Water HTML entity ('☵').
trigramForWater :: String
trigramForWater = "&#9781;"

-- | The Trigram For Mountain HTML entity ('☶').
trigramForMountain :: String
trigramForMountain = "&#9782;"

-- | The Trigram For Earth HTML entity ('☷').
trigramForEarth :: String
trigramForEarth = "&#9783;"

-- | The female sign HTML entity ('♀').
femaleSign :: String
femaleSign = "&#9792;"

-- | The male sign HTML entity ('♂').
maleSign :: String
maleSign = "&#9794;"

-- | The black spade suit HTML entity ('♠').
blackSpadeSuit :: String
blackSpadeSuit = "&#9824;"

-- | The White Diamond Suit HTML entity ('♢').
whiteDiamondSuit :: String
whiteDiamondSuit = "&#9826;"

-- | The black club suit HTML entity ('♣').
blackClubSuit :: String
blackClubSuit = "&#9827;"

-- | The black heart suit HTML entity ('♥').
blackHeartSuit :: String
blackHeartSuit = "&#9829;"

-- | The black diamond suit HTML entity ('♦').
blackDiamondSuit :: String
blackDiamondSuit = "&#9830;"

-- | The eighth note HTML entity ('♪').
eighthNote :: String
eighthNote = "&#9834;"

-- | The music flat sign HTML entity ('♭').
musicFlatSign :: String
musicFlatSign = "&#9837;"

-- | The music natural sign HTML entity ('♮').
musicNaturalSign :: String
musicNaturalSign = "&#9838;"

-- | The music sharp sign HTML entity ('♯').
musicSharpSign :: String
musicSharpSign = "&#9839;"

-- | The White Circle With Dot Right HTML entity ('⚆').
whiteCircleWithDotRight :: String
whiteCircleWithDotRight = "&#9862;"

-- | The White Circle With Two Dots HTML entity ('⚇').
whiteCircleWithTwoDots :: String
whiteCircleWithTwoDots = "&#9863;"

-- | The Black Circle With White Dot Right HTML entity ('⚈').
blackCircleWithWhiteDotRight :: String
blackCircleWithWhiteDotRight = "&#9864;"

-- | The Black Circle With Two White Dots HTML entity ('⚉').
blackCircleWithTwoWhiteDots :: String
blackCircleWithTwoWhiteDots = "&#9865;"

-- | The Medium White Circle HTML entity ('⚪').
mediumWhiteCircle :: String
mediumWhiteCircle = "&#9898;"

-- | The Medium Black Circle HTML entity ('⚫').
mediumBlackCircle :: String
mediumBlackCircle = "&#9899;"

-- | The Medium Small White Circle HTML entity ('⚬').
mediumSmallWhiteCircle :: String
mediumSmallWhiteCircle = "&#9900;"

-- | The squared key HTML entity ('⚿').
squaredKey :: String
squaredKey = "&#9919;"

-- | The white diamond in square HTML entity ('⛋').
whiteDiamondInSquare :: String
whiteDiamondInSquare = "&#9931;"

-- | The Heavy White Down-pointing Triangle HTML entity ('⛛').
heavyWhiteDownPointingTriangle :: String
heavyWhiteDownPointingTriangle = "&#9947;"

-- | The squared saltire HTML entity ('⛝').
squaredSaltire :: String
squaredSaltire = "&#9949;"

-- | The falling diagonal in white circle in black square HTML entity ('⛞').
fallingDiagonalInWhiteCircleInBlackSquare :: String
fallingDiagonalInWhiteCircleInBlackSquare = "&#9950;"

-- | The square four corners HTML entity ('⛶').
squareFourCorners :: String
squareFourCorners = "&#9974;"

-- | The cup on black square HTML entity ('⛾').
cupOnBlackSquare :: String
cupOnBlackSquare = "&#9982;"

-- | The check mark HTML entity ('✓').
checkMark :: String
checkMark = "&#10003;"

-- | The ballot x HTML entity ('✗').
ballotX :: String
ballotX = "&#10007;"

-- | The maltese cross HTML entity ('✠').
malteseCross :: String
malteseCross = "&#10016;"

-- | The Circled White Star HTML entity ('✪').
circledWhiteStar :: String
circledWhiteStar = "&#10026;"

-- | The six pointed black star HTML entity ('✶').
sixPointedBlackStar :: String
sixPointedBlackStar = "&#10038;"

-- | The Circled Open Centre Eight Pointed Star HTML entity ('❂').
circledOpenCentreEightPointedStar :: String
circledOpenCentreEightPointedStar = "&#10050;"

-- | The Shadowed White Circle HTML entity ('❍').
shadowedWhiteCircle :: String
shadowedWhiteCircle = "&#10061;"

-- | The lower right drop-shadowed white square HTML entity ('❏').
lowerRightDropShadowedWhiteSquare :: String
lowerRightDropShadowedWhiteSquare = "&#10063;"

-- | The upper right drop-shadowed white square HTML entity ('❐').
upperRightDropShadowedWhiteSquare :: String
upperRightDropShadowedWhiteSquare = "&#10064;"

-- | The lower right shadowed white square HTML entity ('❑').
lowerRightShadowedWhiteSquare :: String
lowerRightShadowedWhiteSquare = "&#10065;"

-- | The upper right shadowed white square HTML entity ('❒').
upperRightShadowedWhiteSquare :: String
upperRightShadowedWhiteSquare = "&#10066;"

-- | The Black Diamond Minus White X HTML entity ('❖').
blackDiamondMinusWhiteX :: String
blackDiamondMinusWhiteX = "&#10070;"

-- | The light vertical bar HTML entity ('❘').
lightVerticalBar :: String
lightVerticalBar = "&#10072;"

-- | The light left tortoise shell bracket ornament HTML entity ('❲').
lightLeftTortoiseShellBracketOrnament :: String
lightLeftTortoiseShellBracketOrnament = "&#10098;"

-- | The light right tortoise shell bracket ornament HTML entity ('❳').
lightRightTortoiseShellBracketOrnament :: String
lightRightTortoiseShellBracketOrnament = "&#10099;"

-- | The Heavy Plus Sign HTML entity ('➕').
heavyPlusSign :: String
heavyPlusSign = "&#10133;"

-- | The Heavy Minus Sign HTML entity ('➖').
heavyMinusSign :: String
heavyMinusSign = "&#10134;"

-- | The Heavy Division Sign HTML entity ('➗').
heavyDivisionSign :: String
heavyDivisionSign = "&#10135;"

-- | The Three Dimensional Angle HTML entity ('⟀').
threeDimensionalAngle :: String
threeDimensionalAngle = "&#10176;"

-- | The White Triangle Containing Small White Triangle HTML entity ('⟁').
whiteTriangleContainingSmallWhiteTriangle :: String
whiteTriangleContainingSmallWhiteTriangle = "&#10177;"

-- | The Mathematical Rising Diagonal HTML entity ('⟋').
mathematicalRisingDiagonal :: String
mathematicalRisingDiagonal = "&#10187;"

-- | The Long Division HTML entity ('⟌').
longDivision :: String
longDivision = "&#10188;"

-- | The Mathematical Falling Diagonal HTML entity ('⟍').
mathematicalFallingDiagonal :: String
mathematicalFallingDiagonal = "&#10189;"

-- | The squared logical and HTML entity ('⟎').
squaredLogicalAnd :: String
squaredLogicalAnd = "&#10190;"

-- | The squared logical or HTML entity ('⟏').
squaredLogicalOr :: String
squaredLogicalOr = "&#10191;"

-- | The White Diamond With Centred Dot HTML entity ('⟐').
whiteDiamondWithCentredDot :: String
whiteDiamondWithCentredDot = "&#10192;"

-- | The Up Tack With Circle Above HTML entity ('⟟').
upTackWithCircleAbove :: String
upTackWithCircleAbove = "&#10207;"

-- | The Lozenge Divided By Horizontal Rule HTML entity ('⟠').
lozengeDividedByHorizontalRule :: String
lozengeDividedByHorizontalRule = "&#10208;"

-- | The White Concave-sided Diamond HTML entity ('⟡').
whiteConcaveSidedDiamond :: String
whiteConcaveSidedDiamond = "&#10209;"

-- | The White Concave-sided Diamond With Leftwards Tick HTML entity ('⟢').
whiteConcaveSidedDiamondWithLeftwardsTick :: String
whiteConcaveSidedDiamondWithLeftwardsTick = "&#10210;"

-- | The White Concave-sided Diamond With Rightwards Tick HTML entity ('⟣').
whiteConcaveSidedDiamondWithRightwardsTick :: String
whiteConcaveSidedDiamondWithRightwardsTick = "&#10211;"

-- | The white square with leftwards tick HTML entity ('⟤').
whiteSquareWithLeftwardsTick :: String
whiteSquareWithLeftwardsTick = "&#10212;"

-- | The white square with rightwards tick HTML entity ('⟥').
whiteSquareWithRightwardsTick :: String
whiteSquareWithRightwardsTick = "&#10213;"

-- | The mathematical left white square bracket HTML entity ('⟦').
mathematicalLeftWhiteSquareBracket :: String
mathematicalLeftWhiteSquareBracket = "&#10214;"

-- | The mathematical right white square bracket HTML entity ('⟧').
mathematicalRightWhiteSquareBracket :: String
mathematicalRightWhiteSquareBracket = "&#10215;"

-- | The mathematical left angle bracket HTML entity ('⟨').
mathematicalLeftAngleBracket :: String
mathematicalLeftAngleBracket = "&#10216;"

-- | The mathematical right angle bracket HTML entity ('⟩').
mathematicalRightAngleBracket :: String
mathematicalRightAngleBracket = "&#10217;"

-- | The mathematical left double angle bracket HTML entity ('⟪').
mathematicalLeftDoubleAngleBracket :: String
mathematicalLeftDoubleAngleBracket = "&#10218;"

-- | The mathematical right double angle bracket HTML entity ('⟫').
mathematicalRightDoubleAngleBracket :: String
mathematicalRightDoubleAngleBracket = "&#10219;"

-- | The mathematical left white tortoise shell bracket HTML entity ('⟬').
mathematicalLeftWhiteTortoiseShellBracket :: String
mathematicalLeftWhiteTortoiseShellBracket = "&#10220;"

-- | The mathematical right white tortoise shell bracket HTML entity ('⟭').
mathematicalRightWhiteTortoiseShellBracket :: String
mathematicalRightWhiteTortoiseShellBracket = "&#10221;"

-- | The long leftwards arrow HTML entity ('⟵').
longLeftwardsArrow :: String
longLeftwardsArrow = "&#10229;"

-- | The long rightwards arrow HTML entity ('⟶').
longRightwardsArrow :: String
longRightwardsArrow = "&#10230;"

-- | The long left right arrow HTML entity ('⟷').
longLeftRightArrow :: String
longLeftRightArrow = "&#10231;"

-- | The long leftwards double arrow HTML entity ('⟸').
longLeftwardsDoubleArrow :: String
longLeftwardsDoubleArrow = "&#10232;"

-- | The long rightwards double arrow HTML entity ('⟹').
longRightwardsDoubleArrow :: String
longRightwardsDoubleArrow = "&#10233;"

-- | The long left right double arrow HTML entity ('⟺').
longLeftRightDoubleArrow :: String
longLeftRightDoubleArrow = "&#10234;"

-- | The long rightwards arrow from bar HTML entity ('⟼').
longRightwardsArrowFromBar :: String
longRightwardsArrowFromBar = "&#10236;"

-- | The long rightwards squiggle arrow HTML entity ('⟿').
longRightwardsSquiggleArrow :: String
longRightwardsSquiggleArrow = "&#10239;"

-- | The leftwards double arrow with vertical stroke HTML entity ('⤂').
leftwardsDoubleArrowWithVerticalStroke :: String
leftwardsDoubleArrowWithVerticalStroke = "&#10498;"

-- | The rightwards double arrow with vertical stroke HTML entity ('⤃').
rightwardsDoubleArrowWithVerticalStroke :: String
rightwardsDoubleArrowWithVerticalStroke = "&#10499;"

-- | The left right double arrow with vertical stroke HTML entity ('⤄').
leftRightDoubleArrowWithVerticalStroke :: String
leftRightDoubleArrowWithVerticalStroke = "&#10500;"

-- | The rightwards two-headed arrow from bar HTML entity ('⤅').
rightwardsTwoHeadedArrowFromBar :: String
rightwardsTwoHeadedArrowFromBar = "&#10501;"

-- | The leftwards double dash arrow HTML entity ('⤌').
leftwardsDoubleDashArrow :: String
leftwardsDoubleDashArrow = "&#10508;"

-- | The rightwards double dash arrow HTML entity ('⤍').
rightwardsDoubleDashArrow :: String
rightwardsDoubleDashArrow = "&#10509;"

-- | The leftwards triple dash arrow HTML entity ('⤎').
leftwardsTripleDashArrow :: String
leftwardsTripleDashArrow = "&#10510;"

-- | The rightwards triple dash arrow HTML entity ('⤏').
rightwardsTripleDashArrow :: String
rightwardsTripleDashArrow = "&#10511;"

-- | The rightwards two-headed triple dash arrow HTML entity ('⤐').
rightwardsTwoHeadedTripleDashArrow :: String
rightwardsTwoHeadedTripleDashArrow = "&#10512;"

-- | The rightwards arrow with dotted stem HTML entity ('⤑').
rightwardsArrowWithDottedStem :: String
rightwardsArrowWithDottedStem = "&#10513;"

-- | The upwards arrow to bar HTML entity ('⤒').
upwardsArrowToBar :: String
upwardsArrowToBar = "&#10514;"

-- | The downwards arrow to bar HTML entity ('⤓').
downwardsArrowToBar :: String
downwardsArrowToBar = "&#10515;"

-- | The rightwards two-headed arrow with tail HTML entity ('⤖').
rightwardsTwoHeadedArrowWithTail :: String
rightwardsTwoHeadedArrowWithTail = "&#10518;"

-- | The leftwards arrow-tail HTML entity ('⤙').
leftwardsArrowTail :: String
leftwardsArrowTail = "&#10521;"

-- | The rightwards arrow-tail HTML entity ('⤚').
rightwardsArrowTail :: String
rightwardsArrowTail = "&#10522;"

-- | The leftwards double arrow-tail HTML entity ('⤛').
leftwardsDoubleArrowTail :: String
leftwardsDoubleArrowTail = "&#10523;"

-- | The rightwards double arrow-tail HTML entity ('⤜').
rightwardsDoubleArrowTail :: String
rightwardsDoubleArrowTail = "&#10524;"

-- | The leftwards arrow to black diamond HTML entity ('⤝').
leftwardsArrowToBlackDiamond :: String
leftwardsArrowToBlackDiamond = "&#10525;"

-- | The rightwards arrow to black diamond HTML entity ('⤞').
rightwardsArrowToBlackDiamond :: String
rightwardsArrowToBlackDiamond = "&#10526;"

-- | The leftwards arrow from bar to black diamond HTML entity ('⤟').
leftwardsArrowFromBarToBlackDiamond :: String
leftwardsArrowFromBarToBlackDiamond = "&#10527;"

-- | The rightwards arrow from bar to black diamond HTML entity ('⤠').
rightwardsArrowFromBarToBlackDiamond :: String
rightwardsArrowFromBarToBlackDiamond = "&#10528;"

-- | The north west arrow with hook HTML entity ('⤣').
northWestArrowWithHook :: String
northWestArrowWithHook = "&#10531;"

-- | The north east arrow with hook HTML entity ('⤤').
northEastArrowWithHook :: String
northEastArrowWithHook = "&#10532;"

-- | The south east arrow with hook HTML entity ('⤥').
southEastArrowWithHook :: String
southEastArrowWithHook = "&#10533;"

-- | The south west arrow with hook HTML entity ('⤦').
southWestArrowWithHook :: String
southWestArrowWithHook = "&#10534;"

-- | The north west arrow and north east arrow HTML entity ('⤧').
northWestArrowAndNorthEastArrow :: String
northWestArrowAndNorthEastArrow = "&#10535;"

-- | The north east arrow and south east arrow HTML entity ('⤨').
northEastArrowAndSouthEastArrow :: String
northEastArrowAndSouthEastArrow = "&#10536;"

-- | The south east arrow and south west arrow HTML entity ('⤩').
southEastArrowAndSouthWestArrow :: String
southEastArrowAndSouthWestArrow = "&#10537;"

-- | The south west arrow and north west arrow HTML entity ('⤪').
southWestArrowAndNorthWestArrow :: String
southWestArrowAndNorthWestArrow = "&#10538;"

-- | The Rising Diagonal Crossing Falling Diagonal HTML entity ('⤫').
risingDiagonalCrossingFallingDiagonal :: String
risingDiagonalCrossingFallingDiagonal = "&#10539;"

-- | The Falling Diagonal Crossing Rising Diagonal HTML entity ('⤬').
fallingDiagonalCrossingRisingDiagonal :: String
fallingDiagonalCrossingRisingDiagonal = "&#10540;"

-- | The Falling Diagonal Crossing North East Arrow HTML entity ('⤯').
fallingDiagonalCrossingNorthEastArrow :: String
fallingDiagonalCrossingNorthEastArrow = "&#10543;"

-- | The Rising Diagonal Crossing South East Arrow HTML entity ('⤰').
risingDiagonalCrossingSouthEastArrow :: String
risingDiagonalCrossingSouthEastArrow = "&#10544;"

-- | The wave arrow pointing directly right HTML entity ('⤳').
waveArrowPointingDirectlyRight :: String
waveArrowPointingDirectlyRight = "&#10547;"

-- | The arrow pointing rightwards then curving downwards HTML entity ('⤵').
arrowPointingRightwardsThenCurvingDownwards :: String
arrowPointingRightwardsThenCurvingDownwards = "&#10549;"

-- | The arrow pointing downwards then curving leftwards HTML entity ('⤶').
arrowPointingDownwardsThenCurvingLeftwards :: String
arrowPointingDownwardsThenCurvingLeftwards = "&#10550;"

-- | The arrow pointing downwards then curving rightwards HTML entity ('⤷').
arrowPointingDownwardsThenCurvingRightwards :: String
arrowPointingDownwardsThenCurvingRightwards = "&#10551;"

-- | The right-side arc clockwise arrow HTML entity ('⤸').
rightSideArcClockwiseArrow :: String
rightSideArcClockwiseArrow = "&#10552;"

-- | The left-side arc anticlockwise arrow HTML entity ('⤹').
leftSideArcAnticlockwiseArrow :: String
leftSideArcAnticlockwiseArrow = "&#10553;"

-- | The Top Arc Anticlockwise Arrow HTML entity ('⤺').
topArcAnticlockwiseArrow :: String
topArcAnticlockwiseArrow = "&#10554;"

-- | The Bottom Arc Anticlockwise Arrow HTML entity ('⤻').
bottomArcAnticlockwiseArrow :: String
bottomArcAnticlockwiseArrow = "&#10555;"

-- | The top arc clockwise arrow with minus HTML entity ('⤼').
topArcClockwiseArrowWithMinus :: String
topArcClockwiseArrowWithMinus = "&#10556;"

-- | The top arc anticlockwise arrow with plus HTML entity ('⤽').
topArcAnticlockwiseArrowWithPlus :: String
topArcAnticlockwiseArrowWithPlus = "&#10557;"

-- | The rightwards arrow with plus below HTML entity ('⥅').
rightwardsArrowWithPlusBelow :: String
rightwardsArrowWithPlusBelow = "&#10565;"

-- | The left right arrow through small circle HTML entity ('⥈').
leftRightArrowThroughSmallCircle :: String
leftRightArrowThroughSmallCircle = "&#10568;"

-- | The upwards two-headed arrow from small circle HTML entity ('⥉').
upwardsTwoHeadedArrowFromSmallCircle :: String
upwardsTwoHeadedArrowFromSmallCircle = "&#10569;"

-- | The left barb up right barb down harpoon HTML entity ('⥊').
leftBarbUpRightBarbDownHarpoon :: String
leftBarbUpRightBarbDownHarpoon = "&#10570;"

-- | The left barb down right barb up harpoon HTML entity ('⥋').
leftBarbDownRightBarbUpHarpoon :: String
leftBarbDownRightBarbUpHarpoon = "&#10571;"

-- | The Up Barb Right Down Barb Left Harpoon HTML entity ('⥌').
upBarbRightDownBarbLeftHarpoon :: String
upBarbRightDownBarbLeftHarpoon = "&#10572;"

-- | The Up Barb Left Down Barb Right Harpoon HTML entity ('⥍').
upBarbLeftDownBarbRightHarpoon :: String
upBarbLeftDownBarbRightHarpoon = "&#10573;"

-- | The left barb up right barb up harpoon HTML entity ('⥎').
leftBarbUpRightBarbUpHarpoon :: String
leftBarbUpRightBarbUpHarpoon = "&#10574;"

-- | The up barb right down barb right harpoon HTML entity ('⥏').
upBarbRightDownBarbRightHarpoon :: String
upBarbRightDownBarbRightHarpoon = "&#10575;"

-- | The left barb down right barb down harpoon HTML entity ('⥐').
leftBarbDownRightBarbDownHarpoon :: String
leftBarbDownRightBarbDownHarpoon = "&#10576;"

-- | The up barb left down barb left harpoon HTML entity ('⥑').
upBarbLeftDownBarbLeftHarpoon :: String
upBarbLeftDownBarbLeftHarpoon = "&#10577;"

-- | The leftwards harpoon with barb up to bar HTML entity ('⥒').
leftwardsHarpoonWithBarbUpToBar :: String
leftwardsHarpoonWithBarbUpToBar = "&#10578;"

-- | The rightwards harpoon with barb up to bar HTML entity ('⥓').
rightwardsHarpoonWithBarbUpToBar :: String
rightwardsHarpoonWithBarbUpToBar = "&#10579;"

-- | The upwards harpoon with barb right to bar HTML entity ('⥔').
upwardsHarpoonWithBarbRightToBar :: String
upwardsHarpoonWithBarbRightToBar = "&#10580;"

-- | The downwards harpoon with barb right to bar HTML entity ('⥕').
downwardsHarpoonWithBarbRightToBar :: String
downwardsHarpoonWithBarbRightToBar = "&#10581;"

-- | The leftwards harpoon with barb down to bar HTML entity ('⥖').
leftwardsHarpoonWithBarbDownToBar :: String
leftwardsHarpoonWithBarbDownToBar = "&#10582;"

-- | The rightwards harpoon with barb down to bar HTML entity ('⥗').
rightwardsHarpoonWithBarbDownToBar :: String
rightwardsHarpoonWithBarbDownToBar = "&#10583;"

-- | The upwards harpoon with barb left to bar HTML entity ('⥘').
upwardsHarpoonWithBarbLeftToBar :: String
upwardsHarpoonWithBarbLeftToBar = "&#10584;"

-- | The downwards harpoon with barb left to bar HTML entity ('⥙').
downwardsHarpoonWithBarbLeftToBar :: String
downwardsHarpoonWithBarbLeftToBar = "&#10585;"

-- | The leftwards harpoon with barb up from bar HTML entity ('⥚').
leftwardsHarpoonWithBarbUpFromBar :: String
leftwardsHarpoonWithBarbUpFromBar = "&#10586;"

-- | The rightwards harpoon with barb up from bar HTML entity ('⥛').
rightwardsHarpoonWithBarbUpFromBar :: String
rightwardsHarpoonWithBarbUpFromBar = "&#10587;"

-- | The upwards harpoon with barb right from bar HTML entity ('⥜').
upwardsHarpoonWithBarbRightFromBar :: String
upwardsHarpoonWithBarbRightFromBar = "&#10588;"

-- | The downwards harpoon with barb right from bar HTML entity ('⥝').
downwardsHarpoonWithBarbRightFromBar :: String
downwardsHarpoonWithBarbRightFromBar = "&#10589;"

-- | The leftwards harpoon with barb down from bar HTML entity ('⥞').
leftwardsHarpoonWithBarbDownFromBar :: String
leftwardsHarpoonWithBarbDownFromBar = "&#10590;"

-- | The rightwards harpoon with barb down from bar HTML entity ('⥟').
rightwardsHarpoonWithBarbDownFromBar :: String
rightwardsHarpoonWithBarbDownFromBar = "&#10591;"

-- | The upwards harpoon with barb left from bar HTML entity ('⥠').
upwardsHarpoonWithBarbLeftFromBar :: String
upwardsHarpoonWithBarbLeftFromBar = "&#10592;"

-- | The downwards harpoon with barb left from bar HTML entity ('⥡').
downwardsHarpoonWithBarbLeftFromBar :: String
downwardsHarpoonWithBarbLeftFromBar = "&#10593;"

-- | The leftwards harpoon with barb up above leftwards harpoon with barb down HTML entity ('⥢').
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown :: String
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown = "&#10594;"

-- | The upwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥣').
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: String
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = "&#10595;"

-- | The rightwards harpoon with barb up above rightwards harpoon with barb down HTML entity ('⥤').
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown :: String
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown = "&#10596;"

-- | The downwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥥').
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: String
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = "&#10597;"

-- | The leftwards harpoon with barb up above rightwards harpoon with barb up HTML entity ('⥦').
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp :: String
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp = "&#10598;"

-- | The leftwards harpoon with barb down above rightwards harpoon with barb down HTML entity ('⥧').
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown :: String
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown = "&#10599;"

-- | The rightwards harpoon with barb up above leftwards harpoon with barb up HTML entity ('⥨').
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp :: String
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp = "&#10600;"

-- | The rightwards harpoon with barb down above leftwards harpoon with barb down HTML entity ('⥩').
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown :: String
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown = "&#10601;"

-- | The leftwards harpoon with barb up above long dash HTML entity ('⥪').
leftwardsHarpoonWithBarbUpAboveLongDash :: String
leftwardsHarpoonWithBarbUpAboveLongDash = "&#10602;"

-- | The leftwards harpoon with barb down below long dash HTML entity ('⥫').
leftwardsHarpoonWithBarbDownBelowLongDash :: String
leftwardsHarpoonWithBarbDownBelowLongDash = "&#10603;"

-- | The rightwards harpoon with barb up above long dash HTML entity ('⥬').
rightwardsHarpoonWithBarbUpAboveLongDash :: String
rightwardsHarpoonWithBarbUpAboveLongDash = "&#10604;"

-- | The rightwards harpoon with barb down below long dash HTML entity ('⥭').
rightwardsHarpoonWithBarbDownBelowLongDash :: String
rightwardsHarpoonWithBarbDownBelowLongDash = "&#10605;"

-- | The upwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥮').
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: String
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = "&#10606;"

-- | The downwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥯').
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: String
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = "&#10607;"

-- | The right double arrow with rounded head HTML entity ('⥰').
rightDoubleArrowWithRoundedHead :: String
rightDoubleArrowWithRoundedHead = "&#10608;"

-- | The equals sign above rightwards arrow HTML entity ('⥱').
equalsSignAboveRightwardsArrow :: String
equalsSignAboveRightwardsArrow = "&#10609;"

-- | The tilde operator above rightwards arrow HTML entity ('⥲').
tildeOperatorAboveRightwardsArrow :: String
tildeOperatorAboveRightwardsArrow = "&#10610;"

-- | The leftwards arrow above tilde operator HTML entity ('⥳').
leftwardsArrowAboveTildeOperator :: String
leftwardsArrowAboveTildeOperator = "&#10611;"

-- | The rightwards arrow above tilde operator HTML entity ('⥴').
rightwardsArrowAboveTildeOperator :: String
rightwardsArrowAboveTildeOperator = "&#10612;"

-- | The rightwards arrow above almost equal to HTML entity ('⥵').
rightwardsArrowAboveAlmostEqualTo :: String
rightwardsArrowAboveAlmostEqualTo = "&#10613;"

-- | The less-than above leftwards arrow HTML entity ('⥶').
lessThanAboveLeftwardsArrow :: String
lessThanAboveLeftwardsArrow = "&#10614;"

-- | The Leftwards Arrow Through Less-than HTML entity ('⥷').
leftwardsArrowThroughLessThan :: String
leftwardsArrowThroughLessThan = "&#10615;"

-- | The greater-than above rightwards arrow HTML entity ('⥸').
greaterThanAboveRightwardsArrow :: String
greaterThanAboveRightwardsArrow = "&#10616;"

-- | The subset above rightwards arrow HTML entity ('⥹').
subsetAboveRightwardsArrow :: String
subsetAboveRightwardsArrow = "&#10617;"

-- | The superset above leftwards arrow HTML entity ('⥻').
supersetAboveLeftwardsArrow :: String
supersetAboveLeftwardsArrow = "&#10619;"

-- | The left fish tail HTML entity ('⥼').
leftFishTail :: String
leftFishTail = "&#10620;"

-- | The right fish tail HTML entity ('⥽').
rightFishTail :: String
rightFishTail = "&#10621;"

-- | The up fish tail HTML entity ('⥾').
upFishTail :: String
upFishTail = "&#10622;"

-- | The down fish tail HTML entity ('⥿').
downFishTail :: String
downFishTail = "&#10623;"

-- | The left white parenthesis HTML entity ('⦅').
leftWhiteParenthesis :: String
leftWhiteParenthesis = "&#10629;"

-- | The right white parenthesis HTML entity ('⦆').
rightWhiteParenthesis :: String
rightWhiteParenthesis = "&#10630;"

-- | The left square bracket with underbar HTML entity ('⦋').
leftSquareBracketWithUnderbar :: String
leftSquareBracketWithUnderbar = "&#10635;"

-- | The right square bracket with underbar HTML entity ('⦌').
rightSquareBracketWithUnderbar :: String
rightSquareBracketWithUnderbar = "&#10636;"

-- | The left square bracket with tick in top corner HTML entity ('⦍').
leftSquareBracketWithTickInTopCorner :: String
leftSquareBracketWithTickInTopCorner = "&#10637;"

-- | The right square bracket with tick in bottom corner HTML entity ('⦎').
rightSquareBracketWithTickInBottomCorner :: String
rightSquareBracketWithTickInBottomCorner = "&#10638;"

-- | The left square bracket with tick in bottom corner HTML entity ('⦏').
leftSquareBracketWithTickInBottomCorner :: String
leftSquareBracketWithTickInBottomCorner = "&#10639;"

-- | The right square bracket with tick in top corner HTML entity ('⦐').
rightSquareBracketWithTickInTopCorner :: String
rightSquareBracketWithTickInTopCorner = "&#10640;"

-- | The left angle bracket with dot HTML entity ('⦑').
leftAngleBracketWithDot :: String
leftAngleBracketWithDot = "&#10641;"

-- | The right angle bracket with dot HTML entity ('⦒').
rightAngleBracketWithDot :: String
rightAngleBracketWithDot = "&#10642;"

-- | The left arc less-than bracket HTML entity ('⦓').
leftArcLessThanBracket :: String
leftArcLessThanBracket = "&#10643;"

-- | The right arc greater-than bracket HTML entity ('⦔').
rightArcGreaterThanBracket :: String
rightArcGreaterThanBracket = "&#10644;"

-- | The double left arc greater-than bracket HTML entity ('⦕').
doubleLeftArcGreaterThanBracket :: String
doubleLeftArcGreaterThanBracket = "&#10645;"

-- | The double right arc less-than bracket HTML entity ('⦖').
doubleRightArcLessThanBracket :: String
doubleRightArcLessThanBracket = "&#10646;"

-- | The vertical zigzag line HTML entity ('⦚').
verticalZigzagLine :: String
verticalZigzagLine = "&#10650;"

-- | The Measured Angle Opening Left HTML entity ('⦛').
measuredAngleOpeningLeft :: String
measuredAngleOpeningLeft = "&#10651;"

-- | The right angle variant with square HTML entity ('⦜').
rightAngleVariantWithSquare :: String
rightAngleVariantWithSquare = "&#10652;"

-- | The measured right angle with dot HTML entity ('⦝').
measuredRightAngleWithDot :: String
measuredRightAngleWithDot = "&#10653;"

-- | The Angle With S Inside HTML entity ('⦞').
angleWithSInside :: String
angleWithSInside = "&#10654;"

-- | The Acute Angle HTML entity ('⦟').
acuteAngle :: String
acuteAngle = "&#10655;"

-- | The Spherical Angle Opening Left HTML entity ('⦠').
sphericalAngleOpeningLeft :: String
sphericalAngleOpeningLeft = "&#10656;"

-- | The Spherical Angle Opening Up HTML entity ('⦡').
sphericalAngleOpeningUp :: String
sphericalAngleOpeningUp = "&#10657;"

-- | The Turned Angle HTML entity ('⦢').
turnedAngle :: String
turnedAngle = "&#10658;"

-- | The Reversed Angle HTML entity ('⦣').
reversedAngle :: String
reversedAngle = "&#10659;"

-- | The angle with underbar HTML entity ('⦤').
angleWithUnderbar :: String
angleWithUnderbar = "&#10660;"

-- | The reversed angle with underbar HTML entity ('⦥').
reversedAngleWithUnderbar :: String
reversedAngleWithUnderbar = "&#10661;"

-- | The oblique angle opening up HTML entity ('⦦').
obliqueAngleOpeningUp :: String
obliqueAngleOpeningUp = "&#10662;"

-- | The oblique angle opening down HTML entity ('⦧').
obliqueAngleOpeningDown :: String
obliqueAngleOpeningDown = "&#10663;"

-- | The measured angle with open arm ending in arrow pointing up and right HTML entity ('⦨').
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight :: String
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight = "&#10664;"

-- | The measured angle with open arm ending in arrow pointing up and left HTML entity ('⦩').
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft :: String
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft = "&#10665;"

-- | The measured angle with open arm ending in arrow pointing down and right HTML entity ('⦪').
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight :: String
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight = "&#10666;"

-- | The measured angle with open arm ending in arrow pointing down and left HTML entity ('⦫').
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft :: String
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft = "&#10667;"

-- | The measured angle with open arm ending in arrow pointing right and up HTML entity ('⦬').
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp :: String
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp = "&#10668;"

-- | The measured angle with open arm ending in arrow pointing left and up HTML entity ('⦭').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp :: String
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp = "&#10669;"

-- | The measured angle with open arm ending in arrow pointing right and down HTML entity ('⦮').
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown :: String
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown = "&#10670;"

-- | The measured angle with open arm ending in arrow pointing left and down HTML entity ('⦯').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown :: String
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown = "&#10671;"

-- | The reversed empty set HTML entity ('⦰').
reversedEmptySet :: String
reversedEmptySet = "&#10672;"

-- | The empty set with overbar HTML entity ('⦱').
emptySetWithOverbar :: String
emptySetWithOverbar = "&#10673;"

-- | The empty set with small circle above HTML entity ('⦲').
emptySetWithSmallCircleAbove :: String
emptySetWithSmallCircleAbove = "&#10674;"

-- | The empty set with right arrow above HTML entity ('⦳').
emptySetWithRightArrowAbove :: String
emptySetWithRightArrowAbove = "&#10675;"

-- | The empty set with left arrow above HTML entity ('⦴').
emptySetWithLeftArrowAbove :: String
emptySetWithLeftArrowAbove = "&#10676;"

-- | The circle with horizontal bar HTML entity ('⦵').
circleWithHorizontalBar :: String
circleWithHorizontalBar = "&#10677;"

-- | The circled vertical bar HTML entity ('⦶').
circledVerticalBar :: String
circledVerticalBar = "&#10678;"

-- | The circled parallel HTML entity ('⦷').
circledParallel :: String
circledParallel = "&#10679;"

-- | The Circled Reverse Solidus HTML entity ('⦸').
circledReverseSolidus :: String
circledReverseSolidus = "&#10680;"

-- | The circled perpendicular HTML entity ('⦹').
circledPerpendicular :: String
circledPerpendicular = "&#10681;"

-- | The Circle Divided By Horizontal Bar And Top Half Divided By Vertical Bar HTML entity ('⦺').
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar :: String
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar = "&#10682;"

-- | The circle with superimposed x HTML entity ('⦻').
circleWithSuperimposedX :: String
circleWithSuperimposedX = "&#10683;"

-- | The circled anticlockwise-rotated division sign HTML entity ('⦼').
circledAnticlockwiseRotatedDivisionSign :: String
circledAnticlockwiseRotatedDivisionSign = "&#10684;"

-- | The circled white bullet HTML entity ('⦾').
circledWhiteBullet :: String
circledWhiteBullet = "&#10686;"

-- | The circled bullet HTML entity ('⦿').
circledBullet :: String
circledBullet = "&#10687;"

-- | The circled less-than HTML entity ('⧀').
circledLessThan :: String
circledLessThan = "&#10688;"

-- | The circled greater-than HTML entity ('⧁').
circledGreaterThan :: String
circledGreaterThan = "&#10689;"

-- | The circle with small circle to the right HTML entity ('⧂').
circleWithSmallCircleToTheRight :: String
circleWithSmallCircleToTheRight = "&#10690;"

-- | The circle with two horizontal strokes to the right HTML entity ('⧃').
circleWithTwoHorizontalStrokesToTheRight :: String
circleWithTwoHorizontalStrokesToTheRight = "&#10691;"

-- | The squared rising diagonal slash HTML entity ('⧄').
squaredRisingDiagonalSlash :: String
squaredRisingDiagonalSlash = "&#10692;"

-- | The squared falling diagonal slash HTML entity ('⧅').
squaredFallingDiagonalSlash :: String
squaredFallingDiagonalSlash = "&#10693;"

-- | The squared asterisk HTML entity ('⧆').
squaredAsterisk :: String
squaredAsterisk = "&#10694;"

-- | The squared small circle HTML entity ('⧇').
squaredSmallCircle :: String
squaredSmallCircle = "&#10695;"

-- | The squared square HTML entity ('⧈').
squaredSquare :: String
squaredSquare = "&#10696;"

-- | The two joined squares HTML entity ('⧉').
twoJoinedSquares :: String
twoJoinedSquares = "&#10697;"

-- | The Triangle With Dot Above HTML entity ('⧊').
triangleWithDotAbove :: String
triangleWithDotAbove = "&#10698;"

-- | The Triangle With Underbar HTML entity ('⧋').
triangleWithUnderbar :: String
triangleWithUnderbar = "&#10699;"

-- | The S In Triangle HTML entity ('⧌').
sInTriangle :: String
sInTriangle = "&#10700;"

-- | The triangle with serifs at bottom HTML entity ('⧍').
triangleWithSerifsAtBottom :: String
triangleWithSerifsAtBottom = "&#10701;"

-- | The right triangle above left triangle HTML entity ('⧎').
rightTriangleAboveLeftTriangle :: String
rightTriangleAboveLeftTriangle = "&#10702;"

-- | The left triangle beside vertical bar HTML entity ('⧏').
leftTriangleBesideVerticalBar :: String
leftTriangleBesideVerticalBar = "&#10703;"

-- | The vertical bar beside right triangle HTML entity ('⧐').
verticalBarBesideRightTriangle :: String
verticalBarBesideRightTriangle = "&#10704;"

-- | The left double wiggly fence HTML entity ('⧚').
leftDoubleWigglyFence :: String
leftDoubleWigglyFence = "&#10714;"

-- | The incomplete infinity HTML entity ('⧜').
incompleteInfinity :: String
incompleteInfinity = "&#10716;"

-- | The tie over infinity HTML entity ('⧝').
tieOverInfinity :: String
tieOverInfinity = "&#10717;"

-- | The infinity negated with vertical bar HTML entity ('⧞').
infinityNegatedWithVerticalBar :: String
infinityNegatedWithVerticalBar = "&#10718;"

-- | The square with contoured outline HTML entity ('⧠').
squareWithContouredOutline :: String
squareWithContouredOutline = "&#10720;"

-- | The equals sign and slanted parallel HTML entity ('⧣').
equalsSignAndSlantedParallel :: String
equalsSignAndSlantedParallel = "&#10723;"

-- | The equals sign and slanted parallel with tilde above HTML entity ('⧤').
equalsSignAndSlantedParallelWithTildeAbove :: String
equalsSignAndSlantedParallelWithTildeAbove = "&#10724;"

-- | The identical to and slanted parallel HTML entity ('⧥').
identicalToAndSlantedParallel :: String
identicalToAndSlantedParallel = "&#10725;"

-- | The Down-pointing Triangle With Left Half Black HTML entity ('⧨').
downPointingTriangleWithLeftHalfBlack :: String
downPointingTriangleWithLeftHalfBlack = "&#10728;"

-- | The Down-pointing Triangle With Right Half Black HTML entity ('⧩').
downPointingTriangleWithRightHalfBlack :: String
downPointingTriangleWithRightHalfBlack = "&#10729;"

-- | The black lozenge HTML entity ('⧫').
blackLozenge :: String
blackLozenge = "&#10731;"

-- | The error-barred white square HTML entity ('⧮').
errorBarredWhiteSquare :: String
errorBarredWhiteSquare = "&#10734;"

-- | The error-barred black square HTML entity ('⧯').
errorBarredBlackSquare :: String
errorBarredBlackSquare = "&#10735;"

-- | The Error-barred White Diamond HTML entity ('⧰').
errorBarredWhiteDiamond :: String
errorBarredWhiteDiamond = "&#10736;"

-- | The Error-barred Black Diamond HTML entity ('⧱').
errorBarredBlackDiamond :: String
errorBarredBlackDiamond = "&#10737;"

-- | The Error-barred White Circle HTML entity ('⧲').
errorBarredWhiteCircle :: String
errorBarredWhiteCircle = "&#10738;"

-- | The Error-barred Black Circle HTML entity ('⧳').
errorBarredBlackCircle :: String
errorBarredBlackCircle = "&#10739;"

-- | The rule-delayed HTML entity ('⧴').
ruleDelayed :: String
ruleDelayed = "&#10740;"

-- | The solidus with overbar HTML entity ('⧶').
solidusWithOverbar :: String
solidusWithOverbar = "&#10742;"

-- | The Double Plus HTML entity ('⧺').
doublePlus :: String
doublePlus = "&#10746;"

-- | The Triple Plus HTML entity ('⧻').
triplePlus :: String
triplePlus = "&#10747;"

-- | The n-ary circled dot operator HTML entity ('⨀').
nAryCircledDotOperator :: String
nAryCircledDotOperator = "&#10752;"

-- | The n-ary circled plus operator HTML entity ('⨁').
nAryCircledPlusOperator :: String
nAryCircledPlusOperator = "&#10753;"

-- | The n-ary circled times operator HTML entity ('⨂').
nAryCircledTimesOperator :: String
nAryCircledTimesOperator = "&#10754;"

-- | The n-ary union operator with plus HTML entity ('⨄').
nAryUnionOperatorWithPlus :: String
nAryUnionOperatorWithPlus = "&#10756;"

-- | The N-ary Square Intersection Operator HTML entity ('⨅').
nArySquareIntersectionOperator :: String
nArySquareIntersectionOperator = "&#10757;"

-- | The n-ary square union operator HTML entity ('⨆').
nArySquareUnionOperator :: String
nArySquareUnionOperator = "&#10758;"

-- | The Summation With Integral HTML entity ('⨋').
summationWithIntegral :: String
summationWithIntegral = "&#10763;"

-- | The quadruple integral operator HTML entity ('⨌').
quadrupleIntegralOperator :: String
quadrupleIntegralOperator = "&#10764;"

-- | The finite part integral HTML entity ('⨍').
finitePartIntegral :: String
finitePartIntegral = "&#10765;"

-- | The Integral With Double Stroke HTML entity ('⨎').
integralWithDoubleStroke :: String
integralWithDoubleStroke = "&#10766;"

-- | The Integral Average With Slash HTML entity ('⨏').
integralAverageWithSlash :: String
integralAverageWithSlash = "&#10767;"

-- | The circulation function HTML entity ('⨐').
circulationFunction :: String
circulationFunction = "&#10768;"

-- | The anticlockwise integration HTML entity ('⨑').
anticlockwiseIntegration :: String
anticlockwiseIntegration = "&#10769;"

-- | The line integration with rectangular path around pole HTML entity ('⨒').
lineIntegrationWithRectangularPathAroundPole :: String
lineIntegrationWithRectangularPathAroundPole = "&#10770;"

-- | The line integration with semicircular path around pole HTML entity ('⨓').
lineIntegrationWithSemicircularPathAroundPole :: String
lineIntegrationWithSemicircularPathAroundPole = "&#10771;"

-- | The line integration not including the pole HTML entity ('⨔').
lineIntegrationNotIncludingThePole :: String
lineIntegrationNotIncludingThePole = "&#10772;"

-- | The integral around a point operator HTML entity ('⨕').
integralAroundAPointOperator :: String
integralAroundAPointOperator = "&#10773;"

-- | The quaternion integral operator HTML entity ('⨖').
quaternionIntegralOperator :: String
quaternionIntegralOperator = "&#10774;"

-- | The integral with leftwards arrow with hook HTML entity ('⨗').
integralWithLeftwardsArrowWithHook :: String
integralWithLeftwardsArrowWithHook = "&#10775;"

-- | The Integral With Times Sign HTML entity ('⨘').
integralWithTimesSign :: String
integralWithTimesSign = "&#10776;"

-- | The Integral With Intersection HTML entity ('⨙').
integralWithIntersection :: String
integralWithIntersection = "&#10777;"

-- | The Integral With Union HTML entity ('⨚').
integralWithUnion :: String
integralWithUnion = "&#10778;"

-- | The Integral With Overbar HTML entity ('⨛').
integralWithOverbar :: String
integralWithOverbar = "&#10779;"

-- | The Integral With Underbar HTML entity ('⨜').
integralWithUnderbar :: String
integralWithUnderbar = "&#10780;"

-- | The Large Left Triangle Operator HTML entity ('⨞').
largeLeftTriangleOperator :: String
largeLeftTriangleOperator = "&#10782;"

-- | The plus sign with small circle above HTML entity ('⨢').
plusSignWithSmallCircleAbove :: String
plusSignWithSmallCircleAbove = "&#10786;"

-- | The plus sign with circumflex accent above HTML entity ('⨣').
plusSignWithCircumflexAccentAbove :: String
plusSignWithCircumflexAccentAbove = "&#10787;"

-- | The plus sign with tilde above HTML entity ('⨤').
plusSignWithTildeAbove :: String
plusSignWithTildeAbove = "&#10788;"

-- | The plus sign with dot below HTML entity ('⨥').
plusSignWithDotBelow :: String
plusSignWithDotBelow = "&#10789;"

-- | The plus sign with tilde below HTML entity ('⨦').
plusSignWithTildeBelow :: String
plusSignWithTildeBelow = "&#10790;"

-- | The plus sign with subscript two HTML entity ('⨧').
plusSignWithSubscriptTwo :: String
plusSignWithSubscriptTwo = "&#10791;"

-- | The Plus Sign With Black Triangle HTML entity ('⨨').
plusSignWithBlackTriangle :: String
plusSignWithBlackTriangle = "&#10792;"

-- | The minus sign with comma above HTML entity ('⨩').
minusSignWithCommaAbove :: String
minusSignWithCommaAbove = "&#10793;"

-- | The minus sign with dot below HTML entity ('⨪').
minusSignWithDotBelow :: String
minusSignWithDotBelow = "&#10794;"

-- | The Minus Sign With Falling Dots HTML entity ('⨫').
minusSignWithFallingDots :: String
minusSignWithFallingDots = "&#10795;"

-- | The Minus Sign With Rising Dots HTML entity ('⨬').
minusSignWithRisingDots :: String
minusSignWithRisingDots = "&#10796;"

-- | The plus sign in left half circle HTML entity ('⨭').
plusSignInLeftHalfCircle :: String
plusSignInLeftHalfCircle = "&#10797;"

-- | The plus sign in right half circle HTML entity ('⨮').
plusSignInRightHalfCircle :: String
plusSignInRightHalfCircle = "&#10798;"

-- | The vector or cross product HTML entity ('⨯').
vectorOrCrossProduct :: String
vectorOrCrossProduct = "&#10799;"

-- | The multiplication sign with dot above HTML entity ('⨰').
multiplicationSignWithDotAbove :: String
multiplicationSignWithDotAbove = "&#10800;"

-- | The multiplication sign with underbar HTML entity ('⨱').
multiplicationSignWithUnderbar :: String
multiplicationSignWithUnderbar = "&#10801;"

-- | The smash product HTML entity ('⨳').
smashProduct :: String
smashProduct = "&#10803;"

-- | The multiplication sign in left half circle HTML entity ('⨴').
multiplicationSignInLeftHalfCircle :: String
multiplicationSignInLeftHalfCircle = "&#10804;"

-- | The multiplication sign in right half circle HTML entity ('⨵').
multiplicationSignInRightHalfCircle :: String
multiplicationSignInRightHalfCircle = "&#10805;"

-- | The circled multiplication sign with circumflex accent HTML entity ('⨶').
circledMultiplicationSignWithCircumflexAccent :: String
circledMultiplicationSignWithCircumflexAccent = "&#10806;"

-- | The multiplication sign in double circle HTML entity ('⨷').
multiplicationSignInDoubleCircle :: String
multiplicationSignInDoubleCircle = "&#10807;"

-- | The circled division sign HTML entity ('⨸').
circledDivisionSign :: String
circledDivisionSign = "&#10808;"

-- | The plus sign in triangle HTML entity ('⨹').
plusSignInTriangle :: String
plusSignInTriangle = "&#10809;"

-- | The minus sign in triangle HTML entity ('⨺').
minusSignInTriangle :: String
minusSignInTriangle = "&#10810;"

-- | The multiplication sign in triangle HTML entity ('⨻').
multiplicationSignInTriangle :: String
multiplicationSignInTriangle = "&#10811;"

-- | The interior product HTML entity ('⨼').
interiorProduct :: String
interiorProduct = "&#10812;"

-- | The amalgamation or coproduct HTML entity ('⨿').
amalgamationOrCoproduct :: String
amalgamationOrCoproduct = "&#10815;"

-- | The intersection with dot HTML entity ('⩀').
intersectionWithDot :: String
intersectionWithDot = "&#10816;"

-- | The Union With Minus Sign HTML entity ('⩁').
unionWithMinusSign :: String
unionWithMinusSign = "&#10817;"

-- | The union with overbar HTML entity ('⩂').
unionWithOverbar :: String
unionWithOverbar = "&#10818;"

-- | The intersection with overbar HTML entity ('⩃').
intersectionWithOverbar :: String
intersectionWithOverbar = "&#10819;"

-- | The intersection with logical and HTML entity ('⩄').
intersectionWithLogicalAnd :: String
intersectionWithLogicalAnd = "&#10820;"

-- | The union with logical or HTML entity ('⩅').
unionWithLogicalOr :: String
unionWithLogicalOr = "&#10821;"

-- | The union above intersection HTML entity ('⩆').
unionAboveIntersection :: String
unionAboveIntersection = "&#10822;"

-- | The intersection above union HTML entity ('⩇').
intersectionAboveUnion :: String
intersectionAboveUnion = "&#10823;"

-- | The union above bar above intersection HTML entity ('⩈').
unionAboveBarAboveIntersection :: String
unionAboveBarAboveIntersection = "&#10824;"

-- | The intersection above bar above union HTML entity ('⩉').
intersectionAboveBarAboveUnion :: String
intersectionAboveBarAboveUnion = "&#10825;"

-- | The union beside and joined with union HTML entity ('⩊').
unionBesideAndJoinedWithUnion :: String
unionBesideAndJoinedWithUnion = "&#10826;"

-- | The intersection beside and joined with intersection HTML entity ('⩋').
intersectionBesideAndJoinedWithIntersection :: String
intersectionBesideAndJoinedWithIntersection = "&#10827;"

-- | The closed union with serifs HTML entity ('⩌').
closedUnionWithSerifs :: String
closedUnionWithSerifs = "&#10828;"

-- | The closed intersection with serifs HTML entity ('⩍').
closedIntersectionWithSerifs :: String
closedIntersectionWithSerifs = "&#10829;"

-- | The Double Square Intersection HTML entity ('⩎').
doubleSquareIntersection :: String
doubleSquareIntersection = "&#10830;"

-- | The closed union with serifs and smash product HTML entity ('⩐').
closedUnionWithSerifsAndSmashProduct :: String
closedUnionWithSerifsAndSmashProduct = "&#10832;"

-- | The double logical and HTML entity ('⩓').
doubleLogicalAnd :: String
doubleLogicalAnd = "&#10835;"

-- | The double logical or HTML entity ('⩔').
doubleLogicalOr :: String
doubleLogicalOr = "&#10836;"

-- | The two intersecting logical and HTML entity ('⩕').
twoIntersectingLogicalAnd :: String
twoIntersectingLogicalAnd = "&#10837;"

-- | The two intersecting logical or HTML entity ('⩖').
twoIntersectingLogicalOr :: String
twoIntersectingLogicalOr = "&#10838;"

-- | The sloping large or HTML entity ('⩗').
slopingLargeOr :: String
slopingLargeOr = "&#10839;"

-- | The sloping large and HTML entity ('⩘').
slopingLargeAnd :: String
slopingLargeAnd = "&#10840;"

-- | The logical and with middle stem HTML entity ('⩚').
logicalAndWithMiddleStem :: String
logicalAndWithMiddleStem = "&#10842;"

-- | The logical or with middle stem HTML entity ('⩛').
logicalOrWithMiddleStem :: String
logicalOrWithMiddleStem = "&#10843;"

-- | The logical and with horizontal dash HTML entity ('⩜').
logicalAndWithHorizontalDash :: String
logicalAndWithHorizontalDash = "&#10844;"

-- | The logical or with horizontal dash HTML entity ('⩝').
logicalOrWithHorizontalDash :: String
logicalOrWithHorizontalDash = "&#10845;"

-- | The logical and with underbar HTML entity ('⩟').
logicalAndWithUnderbar :: String
logicalAndWithUnderbar = "&#10847;"

-- | The equals sign with dot below HTML entity ('⩦').
equalsSignWithDotBelow :: String
equalsSignWithDotBelow = "&#10854;"

-- | The Identical With Dot Above HTML entity ('⩧').
identicalWithDotAbove :: String
identicalWithDotAbove = "&#10855;"

-- | The tilde operator with dot above HTML entity ('⩪').
tildeOperatorWithDotAbove :: String
tildeOperatorWithDotAbove = "&#10858;"

-- | The Similar Minus Similar HTML entity ('⩬').
similarMinusSimilar :: String
similarMinusSimilar = "&#10860;"

-- | The congruent with dot above HTML entity ('⩭').
congruentWithDotAbove :: String
congruentWithDotAbove = "&#10861;"

-- | The equals with asterisk HTML entity ('⩮').
equalsWithAsterisk :: String
equalsWithAsterisk = "&#10862;"

-- | The almost equal to with circumflex accent HTML entity ('⩯').
almostEqualToWithCircumflexAccent :: String
almostEqualToWithCircumflexAccent = "&#10863;"

-- | The approximately equal or equal to HTML entity ('⩰').
approximatelyEqualOrEqualTo :: String
approximatelyEqualOrEqualTo = "&#10864;"

-- | The equals sign above plus sign HTML entity ('⩱').
equalsSignAbovePlusSign :: String
equalsSignAbovePlusSign = "&#10865;"

-- | The plus sign above equals sign HTML entity ('⩲').
plusSignAboveEqualsSign :: String
plusSignAboveEqualsSign = "&#10866;"

-- | The equals sign above tilde operator HTML entity ('⩳').
equalsSignAboveTildeOperator :: String
equalsSignAboveTildeOperator = "&#10867;"

-- | The double colon equal HTML entity ('⩴').
doubleColonEqual :: String
doubleColonEqual = "&#10868;"

-- | The two consecutive equals signs HTML entity ('⩵').
twoConsecutiveEqualsSigns :: String
twoConsecutiveEqualsSigns = "&#10869;"

-- | The Three Consecutive Equals Signs HTML entity ('⩶').
threeConsecutiveEqualsSigns :: String
threeConsecutiveEqualsSigns = "&#10870;"

-- | The equals sign with two dots above and two dots below HTML entity ('⩷').
equalsSignWithTwoDotsAboveAndTwoDotsBelow :: String
equalsSignWithTwoDotsAboveAndTwoDotsBelow = "&#10871;"

-- | The equivalent with four dots above HTML entity ('⩸').
equivalentWithFourDotsAbove :: String
equivalentWithFourDotsAbove = "&#10872;"

-- | The less-than with circle inside HTML entity ('⩹').
lessThanWithCircleInside :: String
lessThanWithCircleInside = "&#10873;"

-- | The greater-than with circle inside HTML entity ('⩺').
greaterThanWithCircleInside :: String
greaterThanWithCircleInside = "&#10874;"

-- | The less-than with question mark above HTML entity ('⩻').
lessThanWithQuestionMarkAbove :: String
lessThanWithQuestionMarkAbove = "&#10875;"

-- | The greater-than with question mark above HTML entity ('⩼').
greaterThanWithQuestionMarkAbove :: String
greaterThanWithQuestionMarkAbove = "&#10876;"

-- | The less-than or slanted equal to HTML entity ('⩽').
lessThanOrSlantedEqualTo :: String
lessThanOrSlantedEqualTo = "&#10877;"

-- | The greater-than or slanted equal to HTML entity ('⩾').
greaterThanOrSlantedEqualTo :: String
greaterThanOrSlantedEqualTo = "&#10878;"

-- | The less-than or slanted equal to with dot inside HTML entity ('⩿').
lessThanOrSlantedEqualToWithDotInside :: String
lessThanOrSlantedEqualToWithDotInside = "&#10879;"

-- | The greater-than or slanted equal to with dot inside HTML entity ('⪀').
greaterThanOrSlantedEqualToWithDotInside :: String
greaterThanOrSlantedEqualToWithDotInside = "&#10880;"

-- | The less-than or slanted equal to with dot above HTML entity ('⪁').
lessThanOrSlantedEqualToWithDotAbove :: String
lessThanOrSlantedEqualToWithDotAbove = "&#10881;"

-- | The greater-than or slanted equal to with dot above HTML entity ('⪂').
greaterThanOrSlantedEqualToWithDotAbove :: String
greaterThanOrSlantedEqualToWithDotAbove = "&#10882;"

-- | The less-than or slanted equal to with dot above right HTML entity ('⪃').
lessThanOrSlantedEqualToWithDotAboveRight :: String
lessThanOrSlantedEqualToWithDotAboveRight = "&#10883;"

-- | The greater-than or slanted equal to with dot above left HTML entity ('⪄').
greaterThanOrSlantedEqualToWithDotAboveLeft :: String
greaterThanOrSlantedEqualToWithDotAboveLeft = "&#10884;"

-- | The less-than or approximate HTML entity ('⪅').
lessThanOrApproximate :: String
lessThanOrApproximate = "&#10885;"

-- | The greater-than or approximate HTML entity ('⪆').
greaterThanOrApproximate :: String
greaterThanOrApproximate = "&#10886;"

-- | The less-than and single-line not equal to HTML entity ('⪇').
lessThanAndSingleLineNotEqualTo :: String
lessThanAndSingleLineNotEqualTo = "&#10887;"

-- | The greater-than and single-line not equal to HTML entity ('⪈').
greaterThanAndSingleLineNotEqualTo :: String
greaterThanAndSingleLineNotEqualTo = "&#10888;"

-- | The less-than and not approximate HTML entity ('⪉').
lessThanAndNotApproximate :: String
lessThanAndNotApproximate = "&#10889;"

-- | The greater-than and not approximate HTML entity ('⪊').
greaterThanAndNotApproximate :: String
greaterThanAndNotApproximate = "&#10890;"

-- | The less-than above double-line equal above greater-than HTML entity ('⪋').
lessThanAboveDoubleLineEqualAboveGreaterThan :: String
lessThanAboveDoubleLineEqualAboveGreaterThan = "&#10891;"

-- | The greater-than above double-line equal above less-than HTML entity ('⪌').
greaterThanAboveDoubleLineEqualAboveLessThan :: String
greaterThanAboveDoubleLineEqualAboveLessThan = "&#10892;"

-- | The less-than above similar or equal HTML entity ('⪍').
lessThanAboveSimilarOrEqual :: String
lessThanAboveSimilarOrEqual = "&#10893;"

-- | The greater-than above similar or equal HTML entity ('⪎').
greaterThanAboveSimilarOrEqual :: String
greaterThanAboveSimilarOrEqual = "&#10894;"

-- | The less-than above similar above greater-than HTML entity ('⪏').
lessThanAboveSimilarAboveGreaterThan :: String
lessThanAboveSimilarAboveGreaterThan = "&#10895;"

-- | The greater-than above similar above less-than HTML entity ('⪐').
greaterThanAboveSimilarAboveLessThan :: String
greaterThanAboveSimilarAboveLessThan = "&#10896;"

-- | The less-than above greater-than above double-line equal HTML entity ('⪑').
lessThanAboveGreaterThanAboveDoubleLineEqual :: String
lessThanAboveGreaterThanAboveDoubleLineEqual = "&#10897;"

-- | The greater-than above less-than above double-line equal HTML entity ('⪒').
greaterThanAboveLessThanAboveDoubleLineEqual :: String
greaterThanAboveLessThanAboveDoubleLineEqual = "&#10898;"

-- | The less-than above slanted equal above greater-than above slanted equal HTML entity ('⪓').
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual :: String
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual = "&#10899;"

-- | The greater-than above slanted equal above less-than above slanted equal HTML entity ('⪔').
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual :: String
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual = "&#10900;"

-- | The slanted equal to or less-than HTML entity ('⪕').
slantedEqualToOrLessThan :: String
slantedEqualToOrLessThan = "&#10901;"

-- | The slanted equal to or greater-than HTML entity ('⪖').
slantedEqualToOrGreaterThan :: String
slantedEqualToOrGreaterThan = "&#10902;"

-- | The slanted equal to or less-than with dot inside HTML entity ('⪗').
slantedEqualToOrLessThanWithDotInside :: String
slantedEqualToOrLessThanWithDotInside = "&#10903;"

-- | The slanted equal to or greater-than with dot inside HTML entity ('⪘').
slantedEqualToOrGreaterThanWithDotInside :: String
slantedEqualToOrGreaterThanWithDotInside = "&#10904;"

-- | The double-line equal to or less-than HTML entity ('⪙').
doubleLineEqualToOrLessThan :: String
doubleLineEqualToOrLessThan = "&#10905;"

-- | The double-line equal to or greater-than HTML entity ('⪚').
doubleLineEqualToOrGreaterThan :: String
doubleLineEqualToOrGreaterThan = "&#10906;"

-- | The Double-line Slanted Equal To Or Greater-than HTML entity ('⪜').
doubleLineSlantedEqualToOrGreaterThan :: String
doubleLineSlantedEqualToOrGreaterThan = "&#10908;"

-- | The similar or less-than HTML entity ('⪝').
similarOrLessThan :: String
similarOrLessThan = "&#10909;"

-- | The similar or greater-than HTML entity ('⪞').
similarOrGreaterThan :: String
similarOrGreaterThan = "&#10910;"

-- | The similar above less-than above equals sign HTML entity ('⪟').
similarAboveLessThanAboveEqualsSign :: String
similarAboveLessThanAboveEqualsSign = "&#10911;"

-- | The similar above greater-than above equals sign HTML entity ('⪠').
similarAboveGreaterThanAboveEqualsSign :: String
similarAboveGreaterThanAboveEqualsSign = "&#10912;"

-- | The double nested less-than HTML entity ('⪡').
doubleNestedLessThan :: String
doubleNestedLessThan = "&#10913;"

-- | The double nested greater-than HTML entity ('⪢').
doubleNestedGreaterThan :: String
doubleNestedGreaterThan = "&#10914;"

-- | The greater-than overlapping less-than HTML entity ('⪤').
greaterThanOverlappingLessThan :: String
greaterThanOverlappingLessThan = "&#10916;"

-- | The greater-than beside less-than HTML entity ('⪥').
greaterThanBesideLessThan :: String
greaterThanBesideLessThan = "&#10917;"

-- | The less-than closed by curve HTML entity ('⪦').
lessThanClosedByCurve :: String
lessThanClosedByCurve = "&#10918;"

-- | The greater-than closed by curve HTML entity ('⪧').
greaterThanClosedByCurve :: String
greaterThanClosedByCurve = "&#10919;"

-- | The less-than closed by curve above slanted equal HTML entity ('⪨').
lessThanClosedByCurveAboveSlantedEqual :: String
lessThanClosedByCurveAboveSlantedEqual = "&#10920;"

-- | The greater-than closed by curve above slanted equal HTML entity ('⪩').
greaterThanClosedByCurveAboveSlantedEqual :: String
greaterThanClosedByCurveAboveSlantedEqual = "&#10921;"

-- | The smaller than HTML entity ('⪪').
smallerThan :: String
smallerThan = "&#10922;"

-- | The larger than HTML entity ('⪫').
largerThan :: String
largerThan = "&#10923;"

-- | The smaller than or equal to HTML entity ('⪬').
smallerThanOrEqualTo :: String
smallerThanOrEqualTo = "&#10924;"

-- | The larger than or equal to HTML entity ('⪭').
largerThanOrEqualTo :: String
largerThanOrEqualTo = "&#10925;"

-- | The equals sign with bumpy above HTML entity ('⪮').
equalsSignWithBumpyAbove :: String
equalsSignWithBumpyAbove = "&#10926;"

-- | The precedes above single-line equals sign HTML entity ('⪯').
precedesAboveSingleLineEqualsSign :: String
precedesAboveSingleLineEqualsSign = "&#10927;"

-- | The succeeds above single-line equals sign HTML entity ('⪰').
succeedsAboveSingleLineEqualsSign :: String
succeedsAboveSingleLineEqualsSign = "&#10928;"

-- | The Precedes Above Single-line Not Equal To HTML entity ('⪱').
precedesAboveSingleLineNotEqualTo :: String
precedesAboveSingleLineNotEqualTo = "&#10929;"

-- | The Succeeds Above Single-line Not Equal To HTML entity ('⪲').
succeedsAboveSingleLineNotEqualTo :: String
succeedsAboveSingleLineNotEqualTo = "&#10930;"

-- | The precedes above equals sign HTML entity ('⪳').
precedesAboveEqualsSign :: String
precedesAboveEqualsSign = "&#10931;"

-- | The succeeds above equals sign HTML entity ('⪴').
succeedsAboveEqualsSign :: String
succeedsAboveEqualsSign = "&#10932;"

-- | The precedes above not equal to HTML entity ('⪵').
precedesAboveNotEqualTo :: String
precedesAboveNotEqualTo = "&#10933;"

-- | The succeeds above not equal to HTML entity ('⪶').
succeedsAboveNotEqualTo :: String
succeedsAboveNotEqualTo = "&#10934;"

-- | The precedes above almost equal to HTML entity ('⪷').
precedesAboveAlmostEqualTo :: String
precedesAboveAlmostEqualTo = "&#10935;"

-- | The succeeds above almost equal to HTML entity ('⪸').
succeedsAboveAlmostEqualTo :: String
succeedsAboveAlmostEqualTo = "&#10936;"

-- | The precedes above not almost equal to HTML entity ('⪹').
precedesAboveNotAlmostEqualTo :: String
precedesAboveNotAlmostEqualTo = "&#10937;"

-- | The succeeds above not almost equal to HTML entity ('⪺').
succeedsAboveNotAlmostEqualTo :: String
succeedsAboveNotAlmostEqualTo = "&#10938;"

-- | The double precedes HTML entity ('⪻').
doublePrecedes :: String
doublePrecedes = "&#10939;"

-- | The double succeeds HTML entity ('⪼').
doubleSucceeds :: String
doubleSucceeds = "&#10940;"

-- | The subset with dot HTML entity ('⪽').
subsetWithDot :: String
subsetWithDot = "&#10941;"

-- | The superset with dot HTML entity ('⪾').
supersetWithDot :: String
supersetWithDot = "&#10942;"

-- | The subset with plus sign below HTML entity ('⪿').
subsetWithPlusSignBelow :: String
subsetWithPlusSignBelow = "&#10943;"

-- | The superset with plus sign below HTML entity ('⫀').
supersetWithPlusSignBelow :: String
supersetWithPlusSignBelow = "&#10944;"

-- | The subset with multiplication sign below HTML entity ('⫁').
subsetWithMultiplicationSignBelow :: String
subsetWithMultiplicationSignBelow = "&#10945;"

-- | The superset with multiplication sign below HTML entity ('⫂').
supersetWithMultiplicationSignBelow :: String
supersetWithMultiplicationSignBelow = "&#10946;"

-- | The subset of or equal to with dot above HTML entity ('⫃').
subsetOfOrEqualToWithDotAbove :: String
subsetOfOrEqualToWithDotAbove = "&#10947;"

-- | The superset of or equal to with dot above HTML entity ('⫄').
supersetOfOrEqualToWithDotAbove :: String
supersetOfOrEqualToWithDotAbove = "&#10948;"

-- | The subset of above equals sign HTML entity ('⫅').
subsetOfAboveEqualsSign :: String
subsetOfAboveEqualsSign = "&#10949;"

-- | The superset of above equals sign HTML entity ('⫆').
supersetOfAboveEqualsSign :: String
supersetOfAboveEqualsSign = "&#10950;"

-- | The subset of above tilde operator HTML entity ('⫇').
subsetOfAboveTildeOperator :: String
subsetOfAboveTildeOperator = "&#10951;"

-- | The superset of above tilde operator HTML entity ('⫈').
supersetOfAboveTildeOperator :: String
supersetOfAboveTildeOperator = "&#10952;"

-- | The Subset Of Above Almost Equal To HTML entity ('⫉').
subsetOfAboveAlmostEqualTo :: String
subsetOfAboveAlmostEqualTo = "&#10953;"

-- | The Superset Of Above Almost Equal To HTML entity ('⫊').
supersetOfAboveAlmostEqualTo :: String
supersetOfAboveAlmostEqualTo = "&#10954;"

-- | The subset of above not equal to HTML entity ('⫋').
subsetOfAboveNotEqualTo :: String
subsetOfAboveNotEqualTo = "&#10955;"

-- | The superset of above not equal to HTML entity ('⫌').
supersetOfAboveNotEqualTo :: String
supersetOfAboveNotEqualTo = "&#10956;"

-- | The closed subset HTML entity ('⫏').
closedSubset :: String
closedSubset = "&#10959;"

-- | The closed superset HTML entity ('⫐').
closedSuperset :: String
closedSuperset = "&#10960;"

-- | The closed subset or equal to HTML entity ('⫑').
closedSubsetOrEqualTo :: String
closedSubsetOrEqualTo = "&#10961;"

-- | The closed superset or equal to HTML entity ('⫒').
closedSupersetOrEqualTo :: String
closedSupersetOrEqualTo = "&#10962;"

-- | The subset above superset HTML entity ('⫓').
subsetAboveSuperset :: String
subsetAboveSuperset = "&#10963;"

-- | The superset above subset HTML entity ('⫔').
supersetAboveSubset :: String
supersetAboveSubset = "&#10964;"

-- | The subset above subset HTML entity ('⫕').
subsetAboveSubset :: String
subsetAboveSubset = "&#10965;"

-- | The superset above superset HTML entity ('⫖').
supersetAboveSuperset :: String
supersetAboveSuperset = "&#10966;"

-- | The superset beside subset HTML entity ('⫗').
supersetBesideSubset :: String
supersetBesideSubset = "&#10967;"

-- | The superset beside and joined by dash with subset HTML entity ('⫘').
supersetBesideAndJoinedByDashWithSubset :: String
supersetBesideAndJoinedByDashWithSubset = "&#10968;"

-- | The element of opening downwards HTML entity ('⫙').
elementOfOpeningDownwards :: String
elementOfOpeningDownwards = "&#10969;"

-- | The pitchfork with tee top HTML entity ('⫚').
pitchforkWithTeeTop :: String
pitchforkWithTeeTop = "&#10970;"

-- | The transversal intersection HTML entity ('⫛').
transversalIntersection :: String
transversalIntersection = "&#10971;"

-- | The vertical bar double left turnstile HTML entity ('⫤').
verticalBarDoubleLeftTurnstile :: String
verticalBarDoubleLeftTurnstile = "&#10980;"

-- | The long dash from left member of double vertical HTML entity ('⫦').
longDashFromLeftMemberOfDoubleVertical :: String
longDashFromLeftMemberOfDoubleVertical = "&#10982;"

-- | The short down tack with overbar HTML entity ('⫧').
shortDownTackWithOverbar :: String
shortDownTackWithOverbar = "&#10983;"

-- | The short up tack with underbar HTML entity ('⫨').
shortUpTackWithUnderbar :: String
shortUpTackWithUnderbar = "&#10984;"

-- | The short up tack above short down tack HTML entity ('⫩').
shortUpTackAboveShortDownTack :: String
shortUpTackAboveShortDownTack = "&#10985;"

-- | The double up tack HTML entity ('⫫').
doubleUpTack :: String
doubleUpTack = "&#10987;"

-- | The double stroke not sign HTML entity ('⫬').
doubleStrokeNotSign :: String
doubleStrokeNotSign = "&#10988;"

-- | The reversed double stroke not sign HTML entity ('⫭').
reversedDoubleStrokeNotSign :: String
reversedDoubleStrokeNotSign = "&#10989;"

-- | The does not divide with reversed negation slash HTML entity ('⫮').
doesNotDivideWithReversedNegationSlash :: String
doesNotDivideWithReversedNegationSlash = "&#10990;"

-- | The vertical line with circle above HTML entity ('⫯').
verticalLineWithCircleAbove :: String
verticalLineWithCircleAbove = "&#10991;"

-- | The vertical line with circle below HTML entity ('⫰').
verticalLineWithCircleBelow :: String
verticalLineWithCircleBelow = "&#10992;"

-- | The down tack with circle below HTML entity ('⫱').
downTackWithCircleBelow :: String
downTackWithCircleBelow = "&#10993;"

-- | The parallel with horizontal stroke HTML entity ('⫲').
parallelWithHorizontalStroke :: String
parallelWithHorizontalStroke = "&#10994;"

-- | The parallel with tilde operator HTML entity ('⫳').
parallelWithTildeOperator :: String
parallelWithTildeOperator = "&#10995;"

-- | The Triple Nested Greater-than HTML entity ('⫸').
tripleNestedGreaterThan :: String
tripleNestedGreaterThan = "&#11000;"

-- | The Double-line Slanted Greater-than Or Equal To HTML entity ('⫺').
doubleLineSlantedGreaterThanOrEqualTo :: String
doubleLineSlantedGreaterThanOrEqualTo = "&#11002;"

-- | The double solidus operator HTML entity ('⫽').
doubleSolidusOperator :: String
doubleSolidusOperator = "&#11005;"

-- | The square with top half black HTML entity ('⬒').
squareWithTopHalfBlack :: String
squareWithTopHalfBlack = "&#11026;"

-- | The square with bottom half black HTML entity ('⬓').
squareWithBottomHalfBlack :: String
squareWithBottomHalfBlack = "&#11027;"

-- | The square with upper right diagonal half black HTML entity ('⬔').
squareWithUpperRightDiagonalHalfBlack :: String
squareWithUpperRightDiagonalHalfBlack = "&#11028;"

-- | The square with lower left diagonal half black HTML entity ('⬕').
squareWithLowerLeftDiagonalHalfBlack :: String
squareWithLowerLeftDiagonalHalfBlack = "&#11029;"

-- | The Diamond With Left Half Black HTML entity ('⬖').
diamondWithLeftHalfBlack :: String
diamondWithLeftHalfBlack = "&#11030;"

-- | The Diamond With Right Half Black HTML entity ('⬗').
diamondWithRightHalfBlack :: String
diamondWithRightHalfBlack = "&#11031;"

-- | The Diamond With Top Half Black HTML entity ('⬘').
diamondWithTopHalfBlack :: String
diamondWithTopHalfBlack = "&#11032;"

-- | The Diamond With Bottom Half Black HTML entity ('⬙').
diamondWithBottomHalfBlack :: String
diamondWithBottomHalfBlack = "&#11033;"

-- | The dotted square HTML entity ('⬚').
dottedSquare :: String
dottedSquare = "&#11034;"

-- | The black large square HTML entity ('⬛').
blackLargeSquare :: String
blackLargeSquare = "&#11035;"

-- | The white large square HTML entity ('⬜').
whiteLargeSquare :: String
whiteLargeSquare = "&#11036;"

-- | The black very small square HTML entity ('⬝').
blackVerySmallSquare :: String
blackVerySmallSquare = "&#11037;"

-- | The white very small square HTML entity ('⬞').
whiteVerySmallSquare :: String
whiteVerySmallSquare = "&#11038;"

-- | The Black Pentagon HTML entity ('⬟').
blackPentagon :: String
blackPentagon = "&#11039;"

-- | The White Pentagon HTML entity ('⬠').
whitePentagon :: String
whitePentagon = "&#11040;"

-- | The White Hexagon HTML entity ('⬡').
whiteHexagon :: String
whiteHexagon = "&#11041;"

-- | The Black Hexagon HTML entity ('⬢').
blackHexagon :: String
blackHexagon = "&#11042;"

-- | The Horizontal Black Hexagon HTML entity ('⬣').
horizontalBlackHexagon :: String
horizontalBlackHexagon = "&#11043;"

-- | The Black Large Circle HTML entity ('⬤').
blackLargeCircle :: String
blackLargeCircle = "&#11044;"

-- | The Black Medium Diamond HTML entity ('⬥').
blackMediumDiamond :: String
blackMediumDiamond = "&#11045;"

-- | The White Medium Diamond HTML entity ('⬦').
whiteMediumDiamond :: String
whiteMediumDiamond = "&#11046;"

-- | The Black Medium Lozenge HTML entity ('⬧').
blackMediumLozenge :: String
blackMediumLozenge = "&#11047;"

-- | The White Medium Lozenge HTML entity ('⬨').
whiteMediumLozenge :: String
whiteMediumLozenge = "&#11048;"

-- | The Black Small Diamond HTML entity ('⬩').
blackSmallDiamond :: String
blackSmallDiamond = "&#11049;"

-- | The Black Small Lozenge HTML entity ('⬪').
blackSmallLozenge :: String
blackSmallLozenge = "&#11050;"

-- | The White Small Lozenge HTML entity ('⬫').
whiteSmallLozenge :: String
whiteSmallLozenge = "&#11051;"

-- | The Black Horizontal Ellipse HTML entity ('⬬').
blackHorizontalEllipse :: String
blackHorizontalEllipse = "&#11052;"

-- | The White Horizontal Ellipse HTML entity ('⬭').
whiteHorizontalEllipse :: String
whiteHorizontalEllipse = "&#11053;"

-- | The Black Vertical Ellipse HTML entity ('⬮').
blackVerticalEllipse :: String
blackVerticalEllipse = "&#11054;"

-- | The White Vertical Ellipse HTML entity ('⬯').
whiteVerticalEllipse :: String
whiteVerticalEllipse = "&#11055;"

-- | The Equals Sign Above Leftwards Arrow HTML entity ('⭀').
equalsSignAboveLeftwardsArrow :: String
equalsSignAboveLeftwardsArrow = "&#11072;"

-- | The Leftwards Arrow Above Reverse Almost Equal To HTML entity ('⭂').
leftwardsArrowAboveReverseAlmostEqualTo :: String
leftwardsArrowAboveReverseAlmostEqualTo = "&#11074;"

-- | The Rightwards Arrow Through Greater-than HTML entity ('⭃').
rightwardsArrowThroughGreaterThan :: String
rightwardsArrowThroughGreaterThan = "&#11075;"

-- | The Rightwards Arrow Above Reverse Almost Equal To HTML entity ('⭈').
rightwardsArrowAboveReverseAlmostEqualTo :: String
rightwardsArrowAboveReverseAlmostEqualTo = "&#11080;"

-- | The Leftwards Arrow Above Almost Equal To HTML entity ('⭊').
leftwardsArrowAboveAlmostEqualTo :: String
leftwardsArrowAboveAlmostEqualTo = "&#11082;"

-- | The Black Right-pointing Pentagon HTML entity ('⭓').
blackRightPointingPentagon :: String
blackRightPointingPentagon = "&#11091;"

-- | The White Right-pointing Pentagon HTML entity ('⭔').
whiteRightPointingPentagon :: String
whiteRightPointingPentagon = "&#11092;"

-- | The Heavy Large Circle HTML entity ('⭕').
heavyLargeCircle :: String
heavyLargeCircle = "&#11093;"

-- | The black square centred HTML entity ('⯀').
blackSquareCentred :: String
blackSquareCentred = "&#11200;"

-- | The Black Diamond Centred HTML entity ('⯁').
blackDiamondCentred :: String
blackDiamondCentred = "&#11201;"

-- | The Turned Black Pentagon HTML entity ('⯂').
turnedBlackPentagon :: String
turnedBlackPentagon = "&#11202;"

-- | The square position indicator HTML entity ('⯐').
squarePositionIndicator :: String
squarePositionIndicator = "&#11216;"

-- | The Dotted Right-pointing Angle HTML entity ('⸖').
dottedRightPointingAngle :: String
dottedRightPointingAngle = "&#11798;"

-- | The Modifier Letter Lower Right Corner Angle HTML entity ('ꜚ').
modifierLetterLowerRightCornerAngle :: String
modifierLetterLowerRightCornerAngle = "&#42778;"

-- | The Modifier Letter Short Equals Sign HTML entity ('꞊').
modifierLetterShortEqualsSign :: String
modifierLetterShortEqualsSign = "&#42890;"

-- | The latin small ligature ff HTML entity ('ﬀ').
latinSmallLigatureFf :: String
latinSmallLigatureFf = "&#64256;"

-- | The latin small ligature fi HTML entity ('ﬁ').
latinSmallLigatureFi :: String
latinSmallLigatureFi = "&#64257;"

-- | The latin small ligature fl HTML entity ('ﬂ').
latinSmallLigatureFl :: String
latinSmallLigatureFl = "&#64258;"

-- | The latin small ligature ffi HTML entity ('ﬃ').
latinSmallLigatureFfi :: String
latinSmallLigatureFfi = "&#64259;"

-- | The latin small ligature ffl HTML entity ('ﬄ').
latinSmallLigatureFfl :: String
latinSmallLigatureFfl = "&#64260;"

-- | The Small Plus Sign HTML entity ('﹢').
smallPlusSign :: String
smallPlusSign = "&#65122;"

-- | The Small Hyphen-minus HTML entity ('﹣').
smallHyphenMinus :: String
smallHyphenMinus = "&#65123;"

-- | The Small Greater-than Sign HTML entity ('﹥').
smallGreaterThanSign :: String
smallGreaterThanSign = "&#65125;"

-- | The Small Equals Sign HTML entity ('﹦').
smallEqualsSign :: String
smallEqualsSign = "&#65126;"

-- | The Small Percent Sign HTML entity ('﹪').
smallPercentSign :: String
smallPercentSign = "&#65130;"

-- | The Fullwidth Percent Sign HTML entity ('％').
fullwidthPercentSign :: String
fullwidthPercentSign = "&#65285;"

-- | The Fullwidth Plus Sign HTML entity ('＋').
fullwidthPlusSign :: String
fullwidthPlusSign = "&#65291;"

-- | The Fullwidth Hyphen-minus HTML entity ('－').
fullwidthHyphenMinus :: String
fullwidthHyphenMinus = "&#65293;"

-- | The Fullwidth Equals Sign HTML entity ('＝').
fullwidthEqualsSign :: String
fullwidthEqualsSign = "&#65309;"

-- | The Fullwidth Greater-than Sign HTML entity ('＞').
fullwidthGreaterThanSign :: String
fullwidthGreaterThanSign = "&#65310;"

-- | The Ugaritic Letter Delta HTML entity ('𐎄').
ugariticLetterDelta :: String
ugariticLetterDelta = "&#66436;"

-- | The mathematical script capital a HTML entity ('𝒜').
mathematicalScriptCapitalA :: String
mathematicalScriptCapitalA = "&#119964;"

-- | The mathematical script capital c HTML entity ('𝒞').
mathematicalScriptCapitalC :: String
mathematicalScriptCapitalC = "&#119966;"

-- | The mathematical script capital d HTML entity ('𝒟').
mathematicalScriptCapitalD :: String
mathematicalScriptCapitalD = "&#119967;"

-- | The mathematical script capital g HTML entity ('𝒢').
mathematicalScriptCapitalG :: String
mathematicalScriptCapitalG = "&#119970;"

-- | The mathematical script capital j HTML entity ('𝒥').
mathematicalScriptCapitalJ :: String
mathematicalScriptCapitalJ = "&#119973;"

-- | The mathematical script capital k HTML entity ('𝒦').
mathematicalScriptCapitalK :: String
mathematicalScriptCapitalK = "&#119974;"

-- | The mathematical script capital n HTML entity ('𝒩').
mathematicalScriptCapitalN :: String
mathematicalScriptCapitalN = "&#119977;"

-- | The mathematical script capital o HTML entity ('𝒪').
mathematicalScriptCapitalO :: String
mathematicalScriptCapitalO = "&#119978;"

-- | The mathematical script capital p HTML entity ('𝒫').
mathematicalScriptCapitalP :: String
mathematicalScriptCapitalP = "&#119979;"

-- | The mathematical script capital q HTML entity ('𝒬').
mathematicalScriptCapitalQ :: String
mathematicalScriptCapitalQ = "&#119980;"

-- | The mathematical script capital s HTML entity ('𝒮').
mathematicalScriptCapitalS :: String
mathematicalScriptCapitalS = "&#119982;"

-- | The mathematical script capital t HTML entity ('𝒯').
mathematicalScriptCapitalT :: String
mathematicalScriptCapitalT = "&#119983;"

-- | The mathematical script capital u HTML entity ('𝒰').
mathematicalScriptCapitalU :: String
mathematicalScriptCapitalU = "&#119984;"

-- | The mathematical script capital v HTML entity ('𝒱').
mathematicalScriptCapitalV :: String
mathematicalScriptCapitalV = "&#119985;"

-- | The mathematical script capital w HTML entity ('𝒲').
mathematicalScriptCapitalW :: String
mathematicalScriptCapitalW = "&#119986;"

-- | The mathematical script capital x HTML entity ('𝒳').
mathematicalScriptCapitalX :: String
mathematicalScriptCapitalX = "&#119987;"

-- | The mathematical script capital y HTML entity ('𝒴').
mathematicalScriptCapitalY :: String
mathematicalScriptCapitalY = "&#119988;"

-- | The mathematical script capital z HTML entity ('𝒵').
mathematicalScriptCapitalZ :: String
mathematicalScriptCapitalZ = "&#119989;"

-- | The mathematical script small a HTML entity ('𝒶').
mathematicalScriptSmallA :: String
mathematicalScriptSmallA = "&#119990;"

-- | The mathematical script small b HTML entity ('𝒷').
mathematicalScriptSmallB :: String
mathematicalScriptSmallB = "&#119991;"

-- | The mathematical script small c HTML entity ('𝒸').
mathematicalScriptSmallC :: String
mathematicalScriptSmallC = "&#119992;"

-- | The mathematical script small d HTML entity ('𝒹').
mathematicalScriptSmallD :: String
mathematicalScriptSmallD = "&#119993;"

-- | The mathematical script small f HTML entity ('𝒻').
mathematicalScriptSmallF :: String
mathematicalScriptSmallF = "&#119995;"

-- | The mathematical script small h HTML entity ('𝒽').
mathematicalScriptSmallH :: String
mathematicalScriptSmallH = "&#119997;"

-- | The mathematical script small i HTML entity ('𝒾').
mathematicalScriptSmallI :: String
mathematicalScriptSmallI = "&#119998;"

-- | The mathematical script small j HTML entity ('𝒿').
mathematicalScriptSmallJ :: String
mathematicalScriptSmallJ = "&#119999;"

-- | The mathematical script small k HTML entity ('𝓀').
mathematicalScriptSmallK :: String
mathematicalScriptSmallK = "&#120000;"

-- | The mathematical script small l HTML entity ('𝓁').
mathematicalScriptSmallL :: String
mathematicalScriptSmallL = "&#120001;"

-- | The mathematical script small m HTML entity ('𝓂').
mathematicalScriptSmallM :: String
mathematicalScriptSmallM = "&#120002;"

-- | The mathematical script small n HTML entity ('𝓃').
mathematicalScriptSmallN :: String
mathematicalScriptSmallN = "&#120003;"

-- | The mathematical script small p HTML entity ('𝓅').
mathematicalScriptSmallP :: String
mathematicalScriptSmallP = "&#120005;"

-- | The mathematical script small q HTML entity ('𝓆').
mathematicalScriptSmallQ :: String
mathematicalScriptSmallQ = "&#120006;"

-- | The mathematical script small r HTML entity ('𝓇').
mathematicalScriptSmallR :: String
mathematicalScriptSmallR = "&#120007;"

-- | The mathematical script small s HTML entity ('𝓈').
mathematicalScriptSmallS :: String
mathematicalScriptSmallS = "&#120008;"

-- | The mathematical script small t HTML entity ('𝓉').
mathematicalScriptSmallT :: String
mathematicalScriptSmallT = "&#120009;"

-- | The mathematical script small u HTML entity ('𝓊').
mathematicalScriptSmallU :: String
mathematicalScriptSmallU = "&#120010;"

-- | The mathematical script small v HTML entity ('𝓋').
mathematicalScriptSmallV :: String
mathematicalScriptSmallV = "&#120011;"

-- | The mathematical script small w HTML entity ('𝓌').
mathematicalScriptSmallW :: String
mathematicalScriptSmallW = "&#120012;"

-- | The mathematical script small x HTML entity ('𝓍').
mathematicalScriptSmallX :: String
mathematicalScriptSmallX = "&#120013;"

-- | The mathematical script small y HTML entity ('𝓎').
mathematicalScriptSmallY :: String
mathematicalScriptSmallY = "&#120014;"

-- | The mathematical script small z HTML entity ('𝓏').
mathematicalScriptSmallZ :: String
mathematicalScriptSmallZ = "&#120015;"

-- | The mathematical fraktur capital a HTML entity ('𝔄').
mathematicalFrakturCapitalA :: String
mathematicalFrakturCapitalA = "&#120068;"

-- | The mathematical fraktur capital b HTML entity ('𝔅').
mathematicalFrakturCapitalB :: String
mathematicalFrakturCapitalB = "&#120069;"

-- | The mathematical fraktur capital d HTML entity ('𝔇').
mathematicalFrakturCapitalD :: String
mathematicalFrakturCapitalD = "&#120071;"

-- | The mathematical fraktur capital e HTML entity ('𝔈').
mathematicalFrakturCapitalE :: String
mathematicalFrakturCapitalE = "&#120072;"

-- | The mathematical fraktur capital f HTML entity ('𝔉').
mathematicalFrakturCapitalF :: String
mathematicalFrakturCapitalF = "&#120073;"

-- | The mathematical fraktur capital g HTML entity ('𝔊').
mathematicalFrakturCapitalG :: String
mathematicalFrakturCapitalG = "&#120074;"

-- | The mathematical fraktur capital j HTML entity ('𝔍').
mathematicalFrakturCapitalJ :: String
mathematicalFrakturCapitalJ = "&#120077;"

-- | The mathematical fraktur capital k HTML entity ('𝔎').
mathematicalFrakturCapitalK :: String
mathematicalFrakturCapitalK = "&#120078;"

-- | The mathematical fraktur capital l HTML entity ('𝔏').
mathematicalFrakturCapitalL :: String
mathematicalFrakturCapitalL = "&#120079;"

-- | The mathematical fraktur capital m HTML entity ('𝔐').
mathematicalFrakturCapitalM :: String
mathematicalFrakturCapitalM = "&#120080;"

-- | The mathematical fraktur capital n HTML entity ('𝔑').
mathematicalFrakturCapitalN :: String
mathematicalFrakturCapitalN = "&#120081;"

-- | The mathematical fraktur capital o HTML entity ('𝔒').
mathematicalFrakturCapitalO :: String
mathematicalFrakturCapitalO = "&#120082;"

-- | The mathematical fraktur capital p HTML entity ('𝔓').
mathematicalFrakturCapitalP :: String
mathematicalFrakturCapitalP = "&#120083;"

-- | The mathematical fraktur capital q HTML entity ('𝔔').
mathematicalFrakturCapitalQ :: String
mathematicalFrakturCapitalQ = "&#120084;"

-- | The mathematical fraktur capital s HTML entity ('𝔖').
mathematicalFrakturCapitalS :: String
mathematicalFrakturCapitalS = "&#120086;"

-- | The mathematical fraktur capital t HTML entity ('𝔗').
mathematicalFrakturCapitalT :: String
mathematicalFrakturCapitalT = "&#120087;"

-- | The mathematical fraktur capital u HTML entity ('𝔘').
mathematicalFrakturCapitalU :: String
mathematicalFrakturCapitalU = "&#120088;"

-- | The mathematical fraktur capital v HTML entity ('𝔙').
mathematicalFrakturCapitalV :: String
mathematicalFrakturCapitalV = "&#120089;"

-- | The mathematical fraktur capital w HTML entity ('𝔚').
mathematicalFrakturCapitalW :: String
mathematicalFrakturCapitalW = "&#120090;"

-- | The mathematical fraktur capital x HTML entity ('𝔛').
mathematicalFrakturCapitalX :: String
mathematicalFrakturCapitalX = "&#120091;"

-- | The mathematical fraktur capital y HTML entity ('𝔜').
mathematicalFrakturCapitalY :: String
mathematicalFrakturCapitalY = "&#120092;"

-- | The mathematical fraktur small a HTML entity ('𝔞').
mathematicalFrakturSmallA :: String
mathematicalFrakturSmallA = "&#120094;"

-- | The mathematical fraktur small b HTML entity ('𝔟').
mathematicalFrakturSmallB :: String
mathematicalFrakturSmallB = "&#120095;"

-- | The mathematical fraktur small c HTML entity ('𝔠').
mathematicalFrakturSmallC :: String
mathematicalFrakturSmallC = "&#120096;"

-- | The mathematical fraktur small d HTML entity ('𝔡').
mathematicalFrakturSmallD :: String
mathematicalFrakturSmallD = "&#120097;"

-- | The mathematical fraktur small e HTML entity ('𝔢').
mathematicalFrakturSmallE :: String
mathematicalFrakturSmallE = "&#120098;"

-- | The mathematical fraktur small f HTML entity ('𝔣').
mathematicalFrakturSmallF :: String
mathematicalFrakturSmallF = "&#120099;"

-- | The mathematical fraktur small g HTML entity ('𝔤').
mathematicalFrakturSmallG :: String
mathematicalFrakturSmallG = "&#120100;"

-- | The mathematical fraktur small h HTML entity ('𝔥').
mathematicalFrakturSmallH :: String
mathematicalFrakturSmallH = "&#120101;"

-- | The mathematical fraktur small i HTML entity ('𝔦').
mathematicalFrakturSmallI :: String
mathematicalFrakturSmallI = "&#120102;"

-- | The mathematical fraktur small j HTML entity ('𝔧').
mathematicalFrakturSmallJ :: String
mathematicalFrakturSmallJ = "&#120103;"

-- | The mathematical fraktur small k HTML entity ('𝔨').
mathematicalFrakturSmallK :: String
mathematicalFrakturSmallK = "&#120104;"

-- | The mathematical fraktur small l HTML entity ('𝔩').
mathematicalFrakturSmallL :: String
mathematicalFrakturSmallL = "&#120105;"

-- | The mathematical fraktur small m HTML entity ('𝔪').
mathematicalFrakturSmallM :: String
mathematicalFrakturSmallM = "&#120106;"

-- | The mathematical fraktur small n HTML entity ('𝔫').
mathematicalFrakturSmallN :: String
mathematicalFrakturSmallN = "&#120107;"

-- | The mathematical fraktur small o HTML entity ('𝔬').
mathematicalFrakturSmallO :: String
mathematicalFrakturSmallO = "&#120108;"

-- | The mathematical fraktur small p HTML entity ('𝔭').
mathematicalFrakturSmallP :: String
mathematicalFrakturSmallP = "&#120109;"

-- | The mathematical fraktur small q HTML entity ('𝔮').
mathematicalFrakturSmallQ :: String
mathematicalFrakturSmallQ = "&#120110;"

-- | The mathematical fraktur small r HTML entity ('𝔯').
mathematicalFrakturSmallR :: String
mathematicalFrakturSmallR = "&#120111;"

-- | The mathematical fraktur small s HTML entity ('𝔰').
mathematicalFrakturSmallS :: String
mathematicalFrakturSmallS = "&#120112;"

-- | The mathematical fraktur small t HTML entity ('𝔱').
mathematicalFrakturSmallT :: String
mathematicalFrakturSmallT = "&#120113;"

-- | The mathematical fraktur small u HTML entity ('𝔲').
mathematicalFrakturSmallU :: String
mathematicalFrakturSmallU = "&#120114;"

-- | The mathematical fraktur small v HTML entity ('𝔳').
mathematicalFrakturSmallV :: String
mathematicalFrakturSmallV = "&#120115;"

-- | The mathematical fraktur small w HTML entity ('𝔴').
mathematicalFrakturSmallW :: String
mathematicalFrakturSmallW = "&#120116;"

-- | The mathematical fraktur small x HTML entity ('𝔵').
mathematicalFrakturSmallX :: String
mathematicalFrakturSmallX = "&#120117;"

-- | The mathematical fraktur small y HTML entity ('𝔶').
mathematicalFrakturSmallY :: String
mathematicalFrakturSmallY = "&#120118;"

-- | The mathematical fraktur small z HTML entity ('𝔷').
mathematicalFrakturSmallZ :: String
mathematicalFrakturSmallZ = "&#120119;"

-- | The mathematical double-struck capital a HTML entity ('𝔸').
mathematicalDoubleStruckCapitalA :: String
mathematicalDoubleStruckCapitalA = "&#120120;"

-- | The mathematical double-struck capital b HTML entity ('𝔹').
mathematicalDoubleStruckCapitalB :: String
mathematicalDoubleStruckCapitalB = "&#120121;"

-- | The mathematical double-struck capital d HTML entity ('𝔻').
mathematicalDoubleStruckCapitalD :: String
mathematicalDoubleStruckCapitalD = "&#120123;"

-- | The mathematical double-struck capital e HTML entity ('𝔼').
mathematicalDoubleStruckCapitalE :: String
mathematicalDoubleStruckCapitalE = "&#120124;"

-- | The mathematical double-struck capital f HTML entity ('𝔽').
mathematicalDoubleStruckCapitalF :: String
mathematicalDoubleStruckCapitalF = "&#120125;"

-- | The mathematical double-struck capital g HTML entity ('𝔾').
mathematicalDoubleStruckCapitalG :: String
mathematicalDoubleStruckCapitalG = "&#120126;"

-- | The mathematical double-struck capital i HTML entity ('𝕀').
mathematicalDoubleStruckCapitalI :: String
mathematicalDoubleStruckCapitalI = "&#120128;"

-- | The mathematical double-struck capital j HTML entity ('𝕁').
mathematicalDoubleStruckCapitalJ :: String
mathematicalDoubleStruckCapitalJ = "&#120129;"

-- | The mathematical double-struck capital k HTML entity ('𝕂').
mathematicalDoubleStruckCapitalK :: String
mathematicalDoubleStruckCapitalK = "&#120130;"

-- | The mathematical double-struck capital l HTML entity ('𝕃').
mathematicalDoubleStruckCapitalL :: String
mathematicalDoubleStruckCapitalL = "&#120131;"

-- | The mathematical double-struck capital m HTML entity ('𝕄').
mathematicalDoubleStruckCapitalM :: String
mathematicalDoubleStruckCapitalM = "&#120132;"

-- | The mathematical double-struck capital o HTML entity ('𝕆').
mathematicalDoubleStruckCapitalO :: String
mathematicalDoubleStruckCapitalO = "&#120134;"

-- | The mathematical double-struck capital s HTML entity ('𝕊').
mathematicalDoubleStruckCapitalS :: String
mathematicalDoubleStruckCapitalS = "&#120138;"

-- | The mathematical double-struck capital t HTML entity ('𝕋').
mathematicalDoubleStruckCapitalT :: String
mathematicalDoubleStruckCapitalT = "&#120139;"

-- | The mathematical double-struck capital u HTML entity ('𝕌').
mathematicalDoubleStruckCapitalU :: String
mathematicalDoubleStruckCapitalU = "&#120140;"

-- | The mathematical double-struck capital v HTML entity ('𝕍').
mathematicalDoubleStruckCapitalV :: String
mathematicalDoubleStruckCapitalV = "&#120141;"

-- | The mathematical double-struck capital w HTML entity ('𝕎').
mathematicalDoubleStruckCapitalW :: String
mathematicalDoubleStruckCapitalW = "&#120142;"

-- | The mathematical double-struck capital x HTML entity ('𝕏').
mathematicalDoubleStruckCapitalX :: String
mathematicalDoubleStruckCapitalX = "&#120143;"

-- | The mathematical double-struck capital y HTML entity ('𝕐').
mathematicalDoubleStruckCapitalY :: String
mathematicalDoubleStruckCapitalY = "&#120144;"

-- | The mathematical double-struck small a HTML entity ('𝕒').
mathematicalDoubleStruckSmallA :: String
mathematicalDoubleStruckSmallA = "&#120146;"

-- | The mathematical double-struck small b HTML entity ('𝕓').
mathematicalDoubleStruckSmallB :: String
mathematicalDoubleStruckSmallB = "&#120147;"

-- | The mathematical double-struck small c HTML entity ('𝕔').
mathematicalDoubleStruckSmallC :: String
mathematicalDoubleStruckSmallC = "&#120148;"

-- | The mathematical double-struck small d HTML entity ('𝕕').
mathematicalDoubleStruckSmallD :: String
mathematicalDoubleStruckSmallD = "&#120149;"

-- | The mathematical double-struck small e HTML entity ('𝕖').
mathematicalDoubleStruckSmallE :: String
mathematicalDoubleStruckSmallE = "&#120150;"

-- | The mathematical double-struck small f HTML entity ('𝕗').
mathematicalDoubleStruckSmallF :: String
mathematicalDoubleStruckSmallF = "&#120151;"

-- | The mathematical double-struck small g HTML entity ('𝕘').
mathematicalDoubleStruckSmallG :: String
mathematicalDoubleStruckSmallG = "&#120152;"

-- | The mathematical double-struck small h HTML entity ('𝕙').
mathematicalDoubleStruckSmallH :: String
mathematicalDoubleStruckSmallH = "&#120153;"

-- | The mathematical double-struck small i HTML entity ('𝕚').
mathematicalDoubleStruckSmallI :: String
mathematicalDoubleStruckSmallI = "&#120154;"

-- | The mathematical double-struck small j HTML entity ('𝕛').
mathematicalDoubleStruckSmallJ :: String
mathematicalDoubleStruckSmallJ = "&#120155;"

-- | The mathematical double-struck small k HTML entity ('𝕜').
mathematicalDoubleStruckSmallK :: String
mathematicalDoubleStruckSmallK = "&#120156;"

-- | The mathematical double-struck small l HTML entity ('𝕝').
mathematicalDoubleStruckSmallL :: String
mathematicalDoubleStruckSmallL = "&#120157;"

-- | The mathematical double-struck small m HTML entity ('𝕞').
mathematicalDoubleStruckSmallM :: String
mathematicalDoubleStruckSmallM = "&#120158;"

-- | The mathematical double-struck small n HTML entity ('𝕟').
mathematicalDoubleStruckSmallN :: String
mathematicalDoubleStruckSmallN = "&#120159;"

-- | The mathematical double-struck small o HTML entity ('𝕠').
mathematicalDoubleStruckSmallO :: String
mathematicalDoubleStruckSmallO = "&#120160;"

-- | The mathematical double-struck small p HTML entity ('𝕡').
mathematicalDoubleStruckSmallP :: String
mathematicalDoubleStruckSmallP = "&#120161;"

-- | The mathematical double-struck small q HTML entity ('𝕢').
mathematicalDoubleStruckSmallQ :: String
mathematicalDoubleStruckSmallQ = "&#120162;"

-- | The mathematical double-struck small r HTML entity ('𝕣').
mathematicalDoubleStruckSmallR :: String
mathematicalDoubleStruckSmallR = "&#120163;"

-- | The mathematical double-struck small s HTML entity ('𝕤').
mathematicalDoubleStruckSmallS :: String
mathematicalDoubleStruckSmallS = "&#120164;"

-- | The mathematical double-struck small t HTML entity ('𝕥').
mathematicalDoubleStruckSmallT :: String
mathematicalDoubleStruckSmallT = "&#120165;"

-- | The mathematical double-struck small u HTML entity ('𝕦').
mathematicalDoubleStruckSmallU :: String
mathematicalDoubleStruckSmallU = "&#120166;"

-- | The mathematical double-struck small v HTML entity ('𝕧').
mathematicalDoubleStruckSmallV :: String
mathematicalDoubleStruckSmallV = "&#120167;"

-- | The mathematical double-struck small w HTML entity ('𝕨').
mathematicalDoubleStruckSmallW :: String
mathematicalDoubleStruckSmallW = "&#120168;"

-- | The mathematical double-struck small x HTML entity ('𝕩').
mathematicalDoubleStruckSmallX :: String
mathematicalDoubleStruckSmallX = "&#120169;"

-- | The mathematical double-struck small y HTML entity ('𝕪').
mathematicalDoubleStruckSmallY :: String
mathematicalDoubleStruckSmallY = "&#120170;"

-- | The mathematical double-struck small z HTML entity ('𝕫').
mathematicalDoubleStruckSmallZ :: String
mathematicalDoubleStruckSmallZ = "&#120171;"