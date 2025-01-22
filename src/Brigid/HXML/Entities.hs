module Brigid.HXML.Entities
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

import Brigid.HXML.Elements.Children (ValidChild)
import Brigid.HXML.Elements.Internal (ChildHXML (Tag_Entity))
import Brigid.HXML.Elements.Tags (Content)
import Brigid.Internal.Entities qualified as Entity

-- | The null character HTML entity ('^@').
nullCharacter :: ValidChild Content parent
              => ChildHXML parent
nullCharacter = Tag_Entity Entity.nullCharacter

-- | The start of header HTML entity ('^A').
startOfHeader :: ValidChild Content parent
              => ChildHXML parent
startOfHeader = Tag_Entity Entity.startOfHeader

-- | The start of text HTML entity ('^B').
startOfText :: ValidChild Content parent
            => ChildHXML parent
startOfText = Tag_Entity Entity.startOfText

-- | The end of text HTML entity ('^C').
endOfText :: ValidChild Content parent
          => ChildHXML parent
endOfText = Tag_Entity Entity.endOfText

-- | The end of transmission HTML entity ('^D').
endOfTransmission :: ValidChild Content parent
                  => ChildHXML parent
endOfTransmission = Tag_Entity Entity.endOfTransmission

-- | The enquiry HTML entity ('^E').
enquiry :: ValidChild Content parent
        => ChildHXML parent
enquiry = Tag_Entity Entity.enquiry

-- | The acknowledge HTML entity ('^F').
acknowledge :: ValidChild Content parent
            => ChildHXML parent
acknowledge = Tag_Entity Entity.acknowledge

-- | The bell (ring) HTML entity ('^G').
bell :: ValidChild Content parent
     => ChildHXML parent
bell = Tag_Entity Entity.bell

-- | The backspace HTML entity ('^H').
backspace :: ValidChild Content parent
          => ChildHXML parent
backspace = Tag_Entity Entity.backspace

-- | The character tabulation / horizontal tabulation HTML entity ('^I').
characterTabulation :: ValidChild Content parent
                    => ChildHXML parent
characterTabulation = Tag_Entity Entity.characterTabulation

-- | The line feed (lf) HTML entity ('^J').
lineFeed :: ValidChild Content parent
         => ChildHXML parent
lineFeed = Tag_Entity Entity.lineFeed

-- | The vertical tab HTML entity ('^K').
verticalTab :: ValidChild Content parent
            => ChildHXML parent
verticalTab = Tag_Entity Entity.verticalTab

-- | The form feed HTML entity ('^L').
formFeed :: ValidChild Content parent
         => ChildHXML parent
formFeed = Tag_Entity Entity.formFeed

-- | The carriage return HTML entity ('^M').
carriageReturn :: ValidChild Content parent
               => ChildHXML parent
carriageReturn = Tag_Entity Entity.carriageReturn

-- | The shift out HTML entity ('^N').
shiftOut :: ValidChild Content parent
         => ChildHXML parent
shiftOut = Tag_Entity Entity.shiftOut

-- | The shift in HTML entity ('^O').
shiftIn :: ValidChild Content parent
        => ChildHXML parent
shiftIn = Tag_Entity Entity.shiftIn

-- | The data link escape HTML entity ('^P').
dataLinkEscape :: ValidChild Content parent
               => ChildHXML parent
dataLinkEscape = Tag_Entity Entity.dataLinkEscape

-- | The device control 1 HTML entity ('^Q').
deviceControl1 :: ValidChild Content parent
               => ChildHXML parent
deviceControl1 = Tag_Entity Entity.deviceControl1

-- | The device control 2 HTML entity ('^R').
deviceControl2 :: ValidChild Content parent
               => ChildHXML parent
deviceControl2 = Tag_Entity Entity.deviceControl2

-- | The device control 3 HTML entity ('^S').
deviceControl3 :: ValidChild Content parent
               => ChildHXML parent
deviceControl3 = Tag_Entity Entity.deviceControl3

-- | The device control 4 HTML entity ('^T').
deviceControl4 :: ValidChild Content parent
               => ChildHXML parent
deviceControl4 = Tag_Entity Entity.deviceControl4

-- | The negative acknowledge HTML entity ('^U').
negativeAcknowledge :: ValidChild Content parent
                    => ChildHXML parent
negativeAcknowledge = Tag_Entity Entity.negativeAcknowledge

-- | The synchronize HTML entity ('^V').
synchronize :: ValidChild Content parent
            => ChildHXML parent
synchronize = Tag_Entity Entity.synchronize

-- | The end transmission block HTML entity ('^W').
endTransmissionBlock :: ValidChild Content parent
                     => ChildHXML parent
endTransmissionBlock = Tag_Entity Entity.endTransmissionBlock

-- | The cancel HTML entity ('^X').
cancel :: ValidChild Content parent
       => ChildHXML parent
cancel = Tag_Entity Entity.cancel

-- | The end of medium HTML entity ('^Y').
endOfMedium :: ValidChild Content parent
            => ChildHXML parent
endOfMedium = Tag_Entity Entity.endOfMedium

-- | The substitute HTML entity ('^Z').
substitute :: ValidChild Content parent
           => ChildHXML parent
substitute = Tag_Entity Entity.substitute

-- | The escape HTML entity ('^[').
escape :: ValidChild Content parent
       => ChildHXML parent
escape = Tag_Entity Entity.escape

-- | The file separator HTML entity ('^\').
fileSeparator :: ValidChild Content parent
              => ChildHXML parent
fileSeparator = Tag_Entity Entity.fileSeparator

-- | The group separator HTML entity ('^]').
groupSeparator :: ValidChild Content parent
               => ChildHXML parent
groupSeparator = Tag_Entity Entity.groupSeparator

-- | The record separator HTML entity ('^^').
recordSeparator :: ValidChild Content parent
                => ChildHXML parent
recordSeparator = Tag_Entity Entity.recordSeparator

-- | The unit separator HTML entity ('^_').
unitSeparator :: ValidChild Content parent
              => ChildHXML parent
unitSeparator = Tag_Entity Entity.unitSeparator

-- | The space HTML entity.
space :: ValidChild Content parent
      => ChildHXML parent
space = Tag_Entity Entity.space

-- | The exclamation point HTML entity ('!').
exclamationPoint :: ValidChild Content parent
                 => ChildHXML parent
exclamationPoint = Tag_Entity Entity.exclamationPoint

-- | The quotation mark HTML entity ('"').
quotationMark :: ValidChild Content parent
              => ChildHXML parent
quotationMark = Tag_Entity Entity.quotationMark

-- | The number sign HTML entity ('#').
numberSign :: ValidChild Content parent
           => ChildHXML parent
numberSign = Tag_Entity Entity.numberSign

-- | The dollar sign HTML entity ('$').
dollarSign :: ValidChild Content parent
           => ChildHXML parent
dollarSign = Tag_Entity Entity.dollarSign

-- | The percent sign HTML entity ('%').
percentSign :: ValidChild Content parent
            => ChildHXML parent
percentSign = Tag_Entity Entity.percentSign

-- | The ampersand HTML entity ('&').
ampersand :: ValidChild Content parent
          => ChildHXML parent
ampersand = Tag_Entity Entity.ampersand

-- | The single quote HTML entity (''').
singleQuote :: ValidChild Content parent
            => ChildHXML parent
singleQuote = Tag_Entity Entity.singleQuote

-- | The opening parenthesis HTML entity ('(').
openingParenthesis :: ValidChild Content parent
                   => ChildHXML parent
openingParenthesis = Tag_Entity Entity.openingParenthesis

-- | The closing parenthesis HTML entity (')').
closingParenthesis :: ValidChild Content parent
                   => ChildHXML parent
closingParenthesis = Tag_Entity Entity.closingParenthesis

-- | The asterisk HTML entity ('*').
asterisk :: ValidChild Content parent
         => ChildHXML parent
asterisk = Tag_Entity Entity.asterisk

-- | The plus sign HTML entity ('+').
plusSign :: ValidChild Content parent
         => ChildHXML parent
plusSign = Tag_Entity Entity.plusSign

-- | The comma HTML entity (',').
comma :: ValidChild Content parent
      => ChildHXML parent
comma = Tag_Entity Entity.comma

-- | The minus sign - hyphen HTML entity ('-').
minusSignHyphen :: ValidChild Content parent
                => ChildHXML parent
minusSignHyphen = Tag_Entity Entity.minusSignHyphen

-- | The period HTML entity ('.').
period :: ValidChild Content parent
       => ChildHXML parent
period = Tag_Entity Entity.period

-- | The slash HTML entity ('/').
slash :: ValidChild Content parent
      => ChildHXML parent
slash = Tag_Entity Entity.slash

-- | The zero HTML entity ('0').
zero :: ValidChild Content parent
     => ChildHXML parent
zero = Tag_Entity Entity.zero

-- | The one HTML entity ('1').
one :: ValidChild Content parent
    => ChildHXML parent
one = Tag_Entity Entity.one

-- | The two HTML entity ('2').
two :: ValidChild Content parent
    => ChildHXML parent
two = Tag_Entity Entity.two

-- | The three HTML entity ('3').
three :: ValidChild Content parent
      => ChildHXML parent
three = Tag_Entity Entity.three

-- | The four HTML entity ('4').
four :: ValidChild Content parent
     => ChildHXML parent
four = Tag_Entity Entity.four

-- | The five HTML entity ('5').
five :: ValidChild Content parent
     => ChildHXML parent
five = Tag_Entity Entity.five

-- | The six HTML entity ('6').
six :: ValidChild Content parent
    => ChildHXML parent
six = Tag_Entity Entity.six

-- | The seven HTML entity ('7').
seven :: ValidChild Content parent
      => ChildHXML parent
seven = Tag_Entity Entity.seven

-- | The eight HTML entity ('8').
eight :: ValidChild Content parent
      => ChildHXML parent
eight = Tag_Entity Entity.eight

-- | The nine HTML entity ('9').
nine :: ValidChild Content parent
     => ChildHXML parent
nine = Tag_Entity Entity.nine

-- | The colon HTML entity (':').
colon :: ValidChild Content parent
      => ChildHXML parent
colon = Tag_Entity Entity.colon

-- | The semicolon HTML entity (';').
semicolon :: ValidChild Content parent
          => ChildHXML parent
semicolon = Tag_Entity Entity.semicolon

-- | The less-than sign HTML entity ('<').
lessThanSign :: ValidChild Content parent
             => ChildHXML parent
lessThanSign = Tag_Entity Entity.lessThanSign

-- | The equal sign HTML entity ('=').
equalSign :: ValidChild Content parent
          => ChildHXML parent
equalSign = Tag_Entity Entity.equalSign

-- | The greater-than sign HTML entity ('>').
greaterThanSign :: ValidChild Content parent
                => ChildHXML parent
greaterThanSign = Tag_Entity Entity.greaterThanSign

-- | The question mark HTML entity ('?').
questionMark :: ValidChild Content parent
             => ChildHXML parent
questionMark = Tag_Entity Entity.questionMark

-- | The at symbol HTML entity ('@').
atSymbol :: ValidChild Content parent
         => ChildHXML parent
atSymbol = Tag_Entity Entity.atSymbol

-- | The upper case A HTML entity ('A').
upperCaseA :: ValidChild Content parent
           => ChildHXML parent
upperCaseA = Tag_Entity Entity.upperCaseA

-- | The upper case B HTML entity ('B').
upperCaseB :: ValidChild Content parent
           => ChildHXML parent
upperCaseB = Tag_Entity Entity.upperCaseB

-- | The upper case C  HTML entity ('C').
upperCaseC :: ValidChild Content parent
           => ChildHXML parent
upperCaseC = Tag_Entity Entity.upperCaseC

-- | The upper case D  HTML entity ('D').
upperCaseD :: ValidChild Content parent
           => ChildHXML parent
upperCaseD = Tag_Entity Entity.upperCaseD

-- | The upper case E  HTML entity ('E').
upperCaseE :: ValidChild Content parent
           => ChildHXML parent
upperCaseE = Tag_Entity Entity.upperCaseE

-- | The upper case F  HTML entity ('F').
upperCaseF :: ValidChild Content parent
           => ChildHXML parent
upperCaseF = Tag_Entity Entity.upperCaseF

-- | The upper case G  HTML entity ('G').
upperCaseG :: ValidChild Content parent
           => ChildHXML parent
upperCaseG = Tag_Entity Entity.upperCaseG

-- | The upper case H  HTML entity ('H').
upperCaseH :: ValidChild Content parent
           => ChildHXML parent
upperCaseH = Tag_Entity Entity.upperCaseH

-- | The upper case I  HTML entity ('I').
upperCaseI :: ValidChild Content parent
           => ChildHXML parent
upperCaseI = Tag_Entity Entity.upperCaseI

-- | The upper case J  HTML entity ('J').
upperCaseJ :: ValidChild Content parent
           => ChildHXML parent
upperCaseJ = Tag_Entity Entity.upperCaseJ

-- | The upper case K  HTML entity ('K').
upperCaseK :: ValidChild Content parent
           => ChildHXML parent
upperCaseK = Tag_Entity Entity.upperCaseK

-- | The upper case L  HTML entity ('L').
upperCaseL :: ValidChild Content parent
           => ChildHXML parent
upperCaseL = Tag_Entity Entity.upperCaseL

-- | The upper case M  HTML entity ('M').
upperCaseM :: ValidChild Content parent
           => ChildHXML parent
upperCaseM = Tag_Entity Entity.upperCaseM

-- | The upper case N  HTML entity ('N').
upperCaseN :: ValidChild Content parent
           => ChildHXML parent
upperCaseN = Tag_Entity Entity.upperCaseN

-- | The upper case O  HTML entity ('O').
upperCaseO :: ValidChild Content parent
           => ChildHXML parent
upperCaseO = Tag_Entity Entity.upperCaseO

-- | The upper case P  HTML entity ('P').
upperCaseP :: ValidChild Content parent
           => ChildHXML parent
upperCaseP = Tag_Entity Entity.upperCaseP

-- | The upper case Q  HTML entity ('Q').
upperCaseQ :: ValidChild Content parent
           => ChildHXML parent
upperCaseQ = Tag_Entity Entity.upperCaseQ

-- | The upper case R  HTML entity ('R').
upperCaseR :: ValidChild Content parent
           => ChildHXML parent
upperCaseR = Tag_Entity Entity.upperCaseR

-- | The upper case S  HTML entity ('S').
upperCaseS :: ValidChild Content parent
           => ChildHXML parent
upperCaseS = Tag_Entity Entity.upperCaseS

-- | The upper case T  HTML entity ('T').
upperCaseT :: ValidChild Content parent
           => ChildHXML parent
upperCaseT = Tag_Entity Entity.upperCaseT

-- | The upper case U  HTML entity ('U').
upperCaseU :: ValidChild Content parent
           => ChildHXML parent
upperCaseU = Tag_Entity Entity.upperCaseU

-- | The upper case V  HTML entity ('V').
upperCaseV :: ValidChild Content parent
           => ChildHXML parent
upperCaseV = Tag_Entity Entity.upperCaseV

-- | The upper case W  HTML entity ('W').
upperCaseW :: ValidChild Content parent
           => ChildHXML parent
upperCaseW = Tag_Entity Entity.upperCaseW

-- | The upper case X  HTML entity ('X').
upperCaseX :: ValidChild Content parent
           => ChildHXML parent
upperCaseX = Tag_Entity Entity.upperCaseX

-- | The upper case Y  HTML entity ('Y').
upperCaseY :: ValidChild Content parent
           => ChildHXML parent
upperCaseY = Tag_Entity Entity.upperCaseY

-- | The upper case Z  HTML entity ('Z').
upperCaseZ :: ValidChild Content parent
           => ChildHXML parent
upperCaseZ = Tag_Entity Entity.upperCaseZ

-- | The left square bracket HTML entity ('[').
leftSquareBracket :: ValidChild Content parent
                  => ChildHXML parent
leftSquareBracket = Tag_Entity Entity.leftSquareBracket

-- | The backslash HTML entity ('\').
backslash :: ValidChild Content parent
          => ChildHXML parent
backslash = Tag_Entity Entity.backslash

-- | The right square bracket HTML entity (']').
rightSquareBracket :: ValidChild Content parent
                   => ChildHXML parent
rightSquareBracket = Tag_Entity Entity.rightSquareBracket

-- | The caret - circumflex HTML entity ('^').
caretCircumflex :: ValidChild Content parent
                => ChildHXML parent
caretCircumflex = Tag_Entity Entity.caretCircumflex

-- | The underscore HTML entity ('_').
underscore :: ValidChild Content parent
           => ChildHXML parent
underscore = Tag_Entity Entity.underscore

-- | The grave accent HTML entity ('`').
graveAccent :: ValidChild Content parent
            => ChildHXML parent
graveAccent = Tag_Entity Entity.graveAccent

-- | The lower case a HTML entity ('a').
lowerCaseA :: ValidChild Content parent
           => ChildHXML parent
lowerCaseA = Tag_Entity Entity.lowerCaseA

-- | The lower case b HTML entity ('b').
lowerCaseB :: ValidChild Content parent
           => ChildHXML parent
lowerCaseB = Tag_Entity Entity.lowerCaseB

-- | The lower case c HTML entity ('c').
lowerCaseC :: ValidChild Content parent
           => ChildHXML parent
lowerCaseC = Tag_Entity Entity.lowerCaseC

-- | The lower case d HTML entity ('d').
lowerCaseD :: ValidChild Content parent
           => ChildHXML parent
lowerCaseD = Tag_Entity Entity.lowerCaseD

-- | The lower case e HTML entity ('e').
lowerCaseE :: ValidChild Content parent
           => ChildHXML parent
lowerCaseE = Tag_Entity Entity.lowerCaseE

-- | The lower case f HTML entity ('f').
lowerCaseF :: ValidChild Content parent
           => ChildHXML parent
lowerCaseF = Tag_Entity Entity.lowerCaseF

-- | The lower case g HTML entity ('g').
lowerCaseG :: ValidChild Content parent
           => ChildHXML parent
lowerCaseG = Tag_Entity Entity.lowerCaseG

-- | The lower case h HTML entity ('h').
lowerCaseH :: ValidChild Content parent
           => ChildHXML parent
lowerCaseH = Tag_Entity Entity.lowerCaseH

-- | The lower case i HTML entity ('i').
lowerCaseI :: ValidChild Content parent
           => ChildHXML parent
lowerCaseI = Tag_Entity Entity.lowerCaseI

-- | The lower case j HTML entity ('j').
lowerCaseJ :: ValidChild Content parent
           => ChildHXML parent
lowerCaseJ = Tag_Entity Entity.lowerCaseJ

-- | The lower case k HTML entity ('k').
lowerCaseK :: ValidChild Content parent
           => ChildHXML parent
lowerCaseK = Tag_Entity Entity.lowerCaseK

-- | The lower case l HTML entity ('l').
lowerCaseL :: ValidChild Content parent
           => ChildHXML parent
lowerCaseL = Tag_Entity Entity.lowerCaseL

-- | The lower case m HTML entity ('m').
lowerCaseM :: ValidChild Content parent
           => ChildHXML parent
lowerCaseM = Tag_Entity Entity.lowerCaseM

-- | The lower case n HTML entity ('n').
lowerCaseN :: ValidChild Content parent
           => ChildHXML parent
lowerCaseN = Tag_Entity Entity.lowerCaseN

-- | The lower case o HTML entity ('o').
lowerCaseO :: ValidChild Content parent
           => ChildHXML parent
lowerCaseO = Tag_Entity Entity.lowerCaseO

-- | The lower case p HTML entity ('p').
lowerCaseP :: ValidChild Content parent
           => ChildHXML parent
lowerCaseP = Tag_Entity Entity.lowerCaseP

-- | The lower case q HTML entity ('q').
lowerCaseQ :: ValidChild Content parent
           => ChildHXML parent
lowerCaseQ = Tag_Entity Entity.lowerCaseQ

-- | The lower case r HTML entity ('r').
lowerCaseR :: ValidChild Content parent
           => ChildHXML parent
lowerCaseR = Tag_Entity Entity.lowerCaseR

-- | The lower case s HTML entity ('s').
lowerCaseS :: ValidChild Content parent
           => ChildHXML parent
lowerCaseS = Tag_Entity Entity.lowerCaseS

-- | The lower case t HTML entity ('t').
lowerCaseT :: ValidChild Content parent
           => ChildHXML parent
lowerCaseT = Tag_Entity Entity.lowerCaseT

-- | The lower case u HTML entity ('u').
lowerCaseU :: ValidChild Content parent
           => ChildHXML parent
lowerCaseU = Tag_Entity Entity.lowerCaseU

-- | The lower case v HTML entity ('v').
lowerCaseV :: ValidChild Content parent
           => ChildHXML parent
lowerCaseV = Tag_Entity Entity.lowerCaseV

-- | The lower case w HTML entity ('w').
lowerCaseW :: ValidChild Content parent
           => ChildHXML parent
lowerCaseW = Tag_Entity Entity.lowerCaseW

-- | The lower case x HTML entity ('x').
lowerCaseX :: ValidChild Content parent
           => ChildHXML parent
lowerCaseX = Tag_Entity Entity.lowerCaseX

-- | The lower case y HTML entity ('y').
lowerCaseY :: ValidChild Content parent
           => ChildHXML parent
lowerCaseY = Tag_Entity Entity.lowerCaseY

-- | The lower case z HTML entity ('z').
lowerCaseZ :: ValidChild Content parent
           => ChildHXML parent
lowerCaseZ = Tag_Entity Entity.lowerCaseZ

-- | The left curly bracket HTML entity ('{').
leftCurlyBracket :: ValidChild Content parent
                 => ChildHXML parent
leftCurlyBracket = Tag_Entity Entity.leftCurlyBracket

-- | The vertical line HTML entity ('|').
verticalLine :: ValidChild Content parent
             => ChildHXML parent
verticalLine = Tag_Entity Entity.verticalLine

-- | The right curly bracket HTML entity ('}').
rightCurlyBracket :: ValidChild Content parent
                  => ChildHXML parent
rightCurlyBracket = Tag_Entity Entity.rightCurlyBracket

-- | The equivalency sign - tilde HTML entity ('~').
equivalencySignTilde :: ValidChild Content parent
                     => ChildHXML parent
equivalencySignTilde = Tag_Entity Entity.equivalencySignTilde

-- | The delete (rubout) HTML entity ('^?').
delete :: ValidChild Content parent
       => ChildHXML parent
delete = Tag_Entity Entity.delete

-- | The Padding Character HTML entity ('Esc@').
paddingCharacter :: ValidChild Content parent
                 => ChildHXML parent
paddingCharacter = Tag_Entity Entity.paddingCharacter

-- | The High Octet Preset HTML entity ('EscA').
highOctetPreset :: ValidChild Content parent
                => ChildHXML parent
highOctetPreset = Tag_Entity Entity.highOctetPreset

-- | The Break Permitted Here HTML entity ('EscB').
breakPermittedHere :: ValidChild Content parent
                   => ChildHXML parent
breakPermittedHere = Tag_Entity Entity.breakPermittedHere

-- | The No Break Here HTML entity ('EscC').
noBreakHere :: ValidChild Content parent
            => ChildHXML parent
noBreakHere = Tag_Entity Entity.noBreakHere

-- | The Index HTML entity ('EscD').
index :: ValidChild Content parent
      => ChildHXML parent
index = Tag_Entity Entity.index

-- | The Next Line HTML entity ('EscE').
nextLine :: ValidChild Content parent
         => ChildHXML parent
nextLine = Tag_Entity Entity.nextLine

-- | The Start of Selected Area HTML entity ('EscF').
startOfSelectedArea :: ValidChild Content parent
                    => ChildHXML parent
startOfSelectedArea = Tag_Entity Entity.startOfSelectedArea

-- | The End of Selected Area HTML entity ('EscG').
endOfSelectedArea :: ValidChild Content parent
                  => ChildHXML parent
endOfSelectedArea = Tag_Entity Entity.endOfSelectedArea

-- | The nCharacter Tabulation Set HTML entity ('EscH').
ncharacterTabulationSet :: ValidChild Content parent
                        => ChildHXML parent
ncharacterTabulationSet = Tag_Entity Entity.ncharacterTabulationSet

-- | The Character Tabulation Set with Justification HTML entity ('EscI').
characterTabulationSetWithJustification :: ValidChild Content parent
                                        => ChildHXML parent
characterTabulationSetWithJustification = Tag_Entity Entity.characterTabulationSetWithJustification

-- | The Line Tabulation Set HTML entity ('EscJ').
lineTabulationSet :: ValidChild Content parent
                  => ChildHXML parent
lineTabulationSet = Tag_Entity Entity.lineTabulationSet

-- | The Partial Line Forward HTML entity ('EscK').
partialLineForward :: ValidChild Content parent
                   => ChildHXML parent
partialLineForward = Tag_Entity Entity.partialLineForward

-- | The Partial Line Backward HTML entity ('EscL').
partialLineBackward :: ValidChild Content parent
                    => ChildHXML parent
partialLineBackward = Tag_Entity Entity.partialLineBackward

-- | The Reverse Line Feed HTML entity ('EscM').
reverseLineFeed :: ValidChild Content parent
                => ChildHXML parent
reverseLineFeed = Tag_Entity Entity.reverseLineFeed

-- | The Single-Shift 2 HTML entity ('EscN').
singleShift2 :: ValidChild Content parent
             => ChildHXML parent
singleShift2 = Tag_Entity Entity.singleShift2

-- | The Single-Shift 3 HTML entity ('EscO').
singleShift3 :: ValidChild Content parent
             => ChildHXML parent
singleShift3 = Tag_Entity Entity.singleShift3

-- | The Device Control String HTML entity ('EscP').
deviceControlString :: ValidChild Content parent
                    => ChildHXML parent
deviceControlString = Tag_Entity Entity.deviceControlString

-- | The Private Use 1 HTML entity ('EscQ').
privateUse1 :: ValidChild Content parent
            => ChildHXML parent
privateUse1 = Tag_Entity Entity.privateUse1

-- | The Private Use 2 HTML entity ('EscR').
privateUse2 :: ValidChild Content parent
            => ChildHXML parent
privateUse2 = Tag_Entity Entity.privateUse2

-- | The Set Transmit State HTML entity ('EscS').
setTransmitState :: ValidChild Content parent
                 => ChildHXML parent
setTransmitState = Tag_Entity Entity.setTransmitState

-- | The Cancel character HTML entity ('EscT').
cancelCharacter :: ValidChild Content parent
                => ChildHXML parent
cancelCharacter = Tag_Entity Entity.cancelCharacter

-- | The Message Waiting HTML entity ('EscU').
messageWaiting :: ValidChild Content parent
               => ChildHXML parent
messageWaiting = Tag_Entity Entity.messageWaiting

-- | The Start of Protected Area HTML entity ('EscV').
startOfProtectedArea :: ValidChild Content parent
                     => ChildHXML parent
startOfProtectedArea = Tag_Entity Entity.startOfProtectedArea

-- | The End of Protected Area HTML entity ('EscW').
endOfProtectedArea :: ValidChild Content parent
                   => ChildHXML parent
endOfProtectedArea = Tag_Entity Entity.endOfProtectedArea

-- | The Start of String Followed by a control string terminated by ST (0x9C) HTML entity ('EscX').
startOfStringFollowedByAControlStringTerminatedBySt :: ValidChild Content parent
                                                    => ChildHXML parent
startOfStringFollowedByAControlStringTerminatedBySt = Tag_Entity Entity.startOfStringFollowedByAControlStringTerminatedBySt

-- | The Single Graphic Character Introducer HTML entity ('EscY').
singleGraphicCharacterIntroducer :: ValidChild Content parent
                                 => ChildHXML parent
singleGraphicCharacterIntroducer = Tag_Entity Entity.singleGraphicCharacterIntroducer

-- | The Single Character Introducer HTML entity ('EscZ').
singleCharacterIntroducer :: ValidChild Content parent
                          => ChildHXML parent
singleCharacterIntroducer = Tag_Entity Entity.singleCharacterIntroducer

-- | The Control Sequence Introducer HTML entity ('Esc[').
controlSequenceIntroducer :: ValidChild Content parent
                          => ChildHXML parent
controlSequenceIntroducer = Tag_Entity Entity.controlSequenceIntroducer

-- | The String Terminator HTML entity ('Esc\').
stringTerminator :: ValidChild Content parent
                 => ChildHXML parent
stringTerminator = Tag_Entity Entity.stringTerminator

-- | The Operating System Command HTML entity ('Esc]').
operatingSystemCommand :: ValidChild Content parent
                       => ChildHXML parent
operatingSystemCommand = Tag_Entity Entity.operatingSystemCommand

-- | The Privacy Message	 HTML entity ('Esc^').
privacyMessage :: ValidChild Content parent
               => ChildHXML parent
privacyMessage = Tag_Entity Entity.privacyMessage

-- | The Application Program Command HTML entity ('Esc_').
applicationProgramCommand :: ValidChild Content parent
                          => ChildHXML parent
applicationProgramCommand = Tag_Entity Entity.applicationProgramCommand

-- | The no-break space HTML entity.
noBreakSpace :: ValidChild Content parent
             => ChildHXML parent
noBreakSpace = Tag_Entity Entity.noBreakSpace

-- | The inverted exclamation mark HTML entity ('¡').
invertedExclamationMark :: ValidChild Content parent
                        => ChildHXML parent
invertedExclamationMark = Tag_Entity Entity.invertedExclamationMark

-- | The cent sign HTML entity ('¢').
centSign :: ValidChild Content parent
         => ChildHXML parent
centSign = Tag_Entity Entity.centSign

-- | The pound sign HTML entity ('£').
poundSign :: ValidChild Content parent
          => ChildHXML parent
poundSign = Tag_Entity Entity.poundSign

-- | The currency sign HTML entity ('¤').
currencySign :: ValidChild Content parent
             => ChildHXML parent
currencySign = Tag_Entity Entity.currencySign

-- | The yen sign HTML entity ('¥').
yenSign :: ValidChild Content parent
        => ChildHXML parent
yenSign = Tag_Entity Entity.yenSign

-- | The broken vertical bar HTML entity ('¦').
brokenVerticalBar :: ValidChild Content parent
                  => ChildHXML parent
brokenVerticalBar = Tag_Entity Entity.brokenVerticalBar

-- | The section sign HTML entity ('§').
sectionSign :: ValidChild Content parent
            => ChildHXML parent
sectionSign = Tag_Entity Entity.sectionSign

-- | The diaeresis HTML entity ('¨').
diaeresis :: ValidChild Content parent
          => ChildHXML parent
diaeresis = Tag_Entity Entity.diaeresis

-- | The copyright sign HTML entity ('©').
copyrightSign :: ValidChild Content parent
              => ChildHXML parent
copyrightSign = Tag_Entity Entity.copyrightSign

-- | The feminine ordinal indicator HTML entity ('ª').
feminineOrdinalIndicator :: ValidChild Content parent
                         => ChildHXML parent
feminineOrdinalIndicator = Tag_Entity Entity.feminineOrdinalIndicator

-- | The left double angle quotes HTML entity ('«').
leftDoubleAngleQuotes :: ValidChild Content parent
                      => ChildHXML parent
leftDoubleAngleQuotes = Tag_Entity Entity.leftDoubleAngleQuotes

-- | The not sign HTML entity ('¬').
notSign :: ValidChild Content parent
        => ChildHXML parent
notSign = Tag_Entity Entity.notSign

-- | The soft hyphen HTML entity ('­').
softHyphen :: ValidChild Content parent
           => ChildHXML parent
softHyphen = Tag_Entity Entity.softHyphen

-- | The registered sign HTML entity ('®').
registeredSign :: ValidChild Content parent
               => ChildHXML parent
registeredSign = Tag_Entity Entity.registeredSign

-- | The macron HTML entity ('¯').
macron :: ValidChild Content parent
       => ChildHXML parent
macron = Tag_Entity Entity.macron

-- | The degree sign HTML entity ('°').
degreeSign :: ValidChild Content parent
           => ChildHXML parent
degreeSign = Tag_Entity Entity.degreeSign

-- | The plus-minus sign HTML entity ('±').
plusMinusSign :: ValidChild Content parent
              => ChildHXML parent
plusMinusSign = Tag_Entity Entity.plusMinusSign

-- | The superscript two - squared HTML entity ('²').
superscriptTwoSquared :: ValidChild Content parent
                      => ChildHXML parent
superscriptTwoSquared = Tag_Entity Entity.superscriptTwoSquared

-- | The superscript three - cubed HTML entity ('³').
superscriptThreeCubed :: ValidChild Content parent
                      => ChildHXML parent
superscriptThreeCubed = Tag_Entity Entity.superscriptThreeCubed

-- | The acute accent HTML entity ('´').
acuteAccent :: ValidChild Content parent
            => ChildHXML parent
acuteAccent = Tag_Entity Entity.acuteAccent

-- | The micro sign HTML entity ('µ').
microSign :: ValidChild Content parent
          => ChildHXML parent
microSign = Tag_Entity Entity.microSign

-- | The pilcrow sign - paragraph sign HTML entity ('¶').
pilcrowSignParagraphSign :: ValidChild Content parent
                         => ChildHXML parent
pilcrowSignParagraphSign = Tag_Entity Entity.pilcrowSignParagraphSign

-- | The middle dot HTML entity ('·').
middleDot :: ValidChild Content parent
          => ChildHXML parent
middleDot = Tag_Entity Entity.middleDot

-- | The cedilla HTML entity ('¸').
cedilla :: ValidChild Content parent
        => ChildHXML parent
cedilla = Tag_Entity Entity.cedilla

-- | The superscript one HTML entity ('¹').
superscriptOne :: ValidChild Content parent
               => ChildHXML parent
superscriptOne = Tag_Entity Entity.superscriptOne

-- | The masculine ordinal indicator HTML entity ('º').
masculineOrdinalIndicator :: ValidChild Content parent
                          => ChildHXML parent
masculineOrdinalIndicator = Tag_Entity Entity.masculineOrdinalIndicator

-- | The right double angle quotes HTML entity ('»').
rightDoubleAngleQuotes :: ValidChild Content parent
                       => ChildHXML parent
rightDoubleAngleQuotes = Tag_Entity Entity.rightDoubleAngleQuotes

-- | The fraction one quarter HTML entity ('¼').
fractionOneQuarter :: ValidChild Content parent
                   => ChildHXML parent
fractionOneQuarter = Tag_Entity Entity.fractionOneQuarter

-- | The vulgar fraction one half HTML entity ('½').
vulgarFractionOneHalf :: ValidChild Content parent
                      => ChildHXML parent
vulgarFractionOneHalf = Tag_Entity Entity.vulgarFractionOneHalf

-- | The fraction three quarters HTML entity ('¾').
fractionThreeQuarters :: ValidChild Content parent
                      => ChildHXML parent
fractionThreeQuarters = Tag_Entity Entity.fractionThreeQuarters

-- | The inverted question mark HTML entity ('¿').
invertedQuestionMark :: ValidChild Content parent
                     => ChildHXML parent
invertedQuestionMark = Tag_Entity Entity.invertedQuestionMark

-- | The latin capital letter a with grave HTML entity ('À').
latinCapitalLetterAWithGrave :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterAWithGrave = Tag_Entity Entity.latinCapitalLetterAWithGrave

-- | The latin capital letter a with acute HTML entity ('Á').
latinCapitalLetterAWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterAWithAcute = Tag_Entity Entity.latinCapitalLetterAWithAcute

-- | The latin capital letter a with circumflex HTML entity ('Â').
latinCapitalLetterAWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterAWithCircumflex = Tag_Entity Entity.latinCapitalLetterAWithCircumflex

-- | The latin capital letter a with tilde HTML entity ('Ã').
latinCapitalLetterAWithTilde :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterAWithTilde = Tag_Entity Entity.latinCapitalLetterAWithTilde

-- | The latin capital letter a with diaeresis HTML entity ('Ä').
latinCapitalLetterAWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterAWithDiaeresis = Tag_Entity Entity.latinCapitalLetterAWithDiaeresis

-- | The latin capital letter a with ring above HTML entity ('Å').
latinCapitalLetterAWithRingAbove :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterAWithRingAbove = Tag_Entity Entity.latinCapitalLetterAWithRingAbove

-- | The latin capital letter ae HTML entity ('Æ').
latinCapitalLetterAe :: ValidChild Content parent
                     => ChildHXML parent
latinCapitalLetterAe = Tag_Entity Entity.latinCapitalLetterAe

-- | The latin capital letter c with cedilla HTML entity ('Ç').
latinCapitalLetterCWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterCWithCedilla = Tag_Entity Entity.latinCapitalLetterCWithCedilla

-- | The latin capital letter e with grave HTML entity ('È').
latinCapitalLetterEWithGrave :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterEWithGrave = Tag_Entity Entity.latinCapitalLetterEWithGrave

-- | The latin capital letter e with acute HTML entity ('É').
latinCapitalLetterEWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterEWithAcute = Tag_Entity Entity.latinCapitalLetterEWithAcute

-- | The latin capital letter e with circumflex HTML entity ('Ê').
latinCapitalLetterEWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterEWithCircumflex = Tag_Entity Entity.latinCapitalLetterEWithCircumflex

-- | The latin capital letter e with diaeresis HTML entity ('Ë').
latinCapitalLetterEWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterEWithDiaeresis = Tag_Entity Entity.latinCapitalLetterEWithDiaeresis

-- | The latin capital letter i with grave HTML entity ('Ì').
latinCapitalLetterIWithGrave :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterIWithGrave = Tag_Entity Entity.latinCapitalLetterIWithGrave

-- | The latin capital letter i with acute HTML entity ('Í').
latinCapitalLetterIWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterIWithAcute = Tag_Entity Entity.latinCapitalLetterIWithAcute

-- | The latin capital letter i with circumflex HTML entity ('Î').
latinCapitalLetterIWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterIWithCircumflex = Tag_Entity Entity.latinCapitalLetterIWithCircumflex

-- | The latin capital letter i with diaeresis HTML entity ('Ï').
latinCapitalLetterIWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterIWithDiaeresis = Tag_Entity Entity.latinCapitalLetterIWithDiaeresis

-- | The latin capital letter eth HTML entity ('Ð').
latinCapitalLetterEth :: ValidChild Content parent
                      => ChildHXML parent
latinCapitalLetterEth = Tag_Entity Entity.latinCapitalLetterEth

-- | The latin capital letter n with tilde HTML entity ('Ñ').
latinCapitalLetterNWithTilde :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterNWithTilde = Tag_Entity Entity.latinCapitalLetterNWithTilde

-- | The latin capital letter o with grave HTML entity ('Ò').
latinCapitalLetterOWithGrave :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterOWithGrave = Tag_Entity Entity.latinCapitalLetterOWithGrave

-- | The latin capital letter o with acute HTML entity ('Ó').
latinCapitalLetterOWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterOWithAcute = Tag_Entity Entity.latinCapitalLetterOWithAcute

-- | The latin capital letter o with circumflex HTML entity ('Ô').
latinCapitalLetterOWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterOWithCircumflex = Tag_Entity Entity.latinCapitalLetterOWithCircumflex

-- | The latin capital letter o with tilde HTML entity ('Õ').
latinCapitalLetterOWithTilde :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterOWithTilde = Tag_Entity Entity.latinCapitalLetterOWithTilde

-- | The latin capital letter o with diaeresis HTML entity ('Ö').
latinCapitalLetterOWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterOWithDiaeresis = Tag_Entity Entity.latinCapitalLetterOWithDiaeresis

-- | The multiplication sign HTML entity ('×').
multiplicationSign :: ValidChild Content parent
                   => ChildHXML parent
multiplicationSign = Tag_Entity Entity.multiplicationSign

-- | The latin capital letter o with slash HTML entity ('Ø').
latinCapitalLetterOWithSlash :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterOWithSlash = Tag_Entity Entity.latinCapitalLetterOWithSlash

-- | The latin capital letter u with grave HTML entity ('Ù').
latinCapitalLetterUWithGrave :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterUWithGrave = Tag_Entity Entity.latinCapitalLetterUWithGrave

-- | The latin capital letter u with acute HTML entity ('Ú').
latinCapitalLetterUWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterUWithAcute = Tag_Entity Entity.latinCapitalLetterUWithAcute

-- | The latin capital letter u with circumflex HTML entity ('Û').
latinCapitalLetterUWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterUWithCircumflex = Tag_Entity Entity.latinCapitalLetterUWithCircumflex

-- | The latin capital letter u with diaeresis HTML entity ('Ü').
latinCapitalLetterUWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterUWithDiaeresis = Tag_Entity Entity.latinCapitalLetterUWithDiaeresis

-- | The latin capital letter y with acute HTML entity ('Ý').
latinCapitalLetterYWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterYWithAcute = Tag_Entity Entity.latinCapitalLetterYWithAcute

-- | The latin capital letter thorn HTML entity ('Þ').
latinCapitalLetterThorn :: ValidChild Content parent
                        => ChildHXML parent
latinCapitalLetterThorn = Tag_Entity Entity.latinCapitalLetterThorn

-- | The latin small letter sharp s - ess-zed HTML entity ('ß').
latinSmallLetterSharpSEssZed :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterSharpSEssZed = Tag_Entity Entity.latinSmallLetterSharpSEssZed

-- | The latin small letter a with grave HTML entity ('à').
latinSmallLetterAWithGrave :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterAWithGrave = Tag_Entity Entity.latinSmallLetterAWithGrave

-- | The latin small letter a with acute HTML entity ('á').
latinSmallLetterAWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterAWithAcute = Tag_Entity Entity.latinSmallLetterAWithAcute

-- | The latin small letter a with circumflex HTML entity ('â').
latinSmallLetterAWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterAWithCircumflex = Tag_Entity Entity.latinSmallLetterAWithCircumflex

-- | The latin small letter a with tilde HTML entity ('ã').
latinSmallLetterAWithTilde :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterAWithTilde = Tag_Entity Entity.latinSmallLetterAWithTilde

-- | The latin small letter a with diaeresis HTML entity ('ä').
latinSmallLetterAWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterAWithDiaeresis = Tag_Entity Entity.latinSmallLetterAWithDiaeresis

-- | The latin small letter a with ring above HTML entity ('å').
latinSmallLetterAWithRingAbove :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterAWithRingAbove = Tag_Entity Entity.latinSmallLetterAWithRingAbove

-- | The latin small letter ae HTML entity ('æ').
latinSmallLetterAe :: ValidChild Content parent
                   => ChildHXML parent
latinSmallLetterAe = Tag_Entity Entity.latinSmallLetterAe

-- | The latin small letter c with cedilla HTML entity ('ç').
latinSmallLetterCWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterCWithCedilla = Tag_Entity Entity.latinSmallLetterCWithCedilla

-- | The latin small letter e with grave HTML entity ('è').
latinSmallLetterEWithGrave :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterEWithGrave = Tag_Entity Entity.latinSmallLetterEWithGrave

-- | The latin small letter e with acute HTML entity ('é').
latinSmallLetterEWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterEWithAcute = Tag_Entity Entity.latinSmallLetterEWithAcute

-- | The latin small letter e with circumflex HTML entity ('ê').
latinSmallLetterEWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterEWithCircumflex = Tag_Entity Entity.latinSmallLetterEWithCircumflex

-- | The latin small letter e with diaeresis HTML entity ('ë').
latinSmallLetterEWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterEWithDiaeresis = Tag_Entity Entity.latinSmallLetterEWithDiaeresis

-- | The latin small letter i with grave HTML entity ('ì').
latinSmallLetterIWithGrave :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterIWithGrave = Tag_Entity Entity.latinSmallLetterIWithGrave

-- | The latin small letter i with acute HTML entity ('í').
latinSmallLetterIWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterIWithAcute = Tag_Entity Entity.latinSmallLetterIWithAcute

-- | The latin small letter i with circumflex HTML entity ('î').
latinSmallLetterIWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterIWithCircumflex = Tag_Entity Entity.latinSmallLetterIWithCircumflex

-- | The latin small letter i with diaeresis HTML entity ('ï').
latinSmallLetterIWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterIWithDiaeresis = Tag_Entity Entity.latinSmallLetterIWithDiaeresis

-- | The latin small letter eth HTML entity ('ð').
latinSmallLetterEth :: ValidChild Content parent
                    => ChildHXML parent
latinSmallLetterEth = Tag_Entity Entity.latinSmallLetterEth

-- | The latin small letter n with tilde HTML entity ('ñ').
latinSmallLetterNWithTilde :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterNWithTilde = Tag_Entity Entity.latinSmallLetterNWithTilde

-- | The latin small letter o with grave HTML entity ('ò').
latinSmallLetterOWithGrave :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterOWithGrave = Tag_Entity Entity.latinSmallLetterOWithGrave

-- | The latin small letter o with acute HTML entity ('ó').
latinSmallLetterOWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterOWithAcute = Tag_Entity Entity.latinSmallLetterOWithAcute

-- | The latin small letter o with circumflex HTML entity ('ô').
latinSmallLetterOWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterOWithCircumflex = Tag_Entity Entity.latinSmallLetterOWithCircumflex

-- | The latin small letter o with tilde HTML entity ('õ').
latinSmallLetterOWithTilde :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterOWithTilde = Tag_Entity Entity.latinSmallLetterOWithTilde

-- | The latin small letter o with diaeresis HTML entity ('ö').
latinSmallLetterOWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterOWithDiaeresis = Tag_Entity Entity.latinSmallLetterOWithDiaeresis

-- | The division sign HTML entity ('÷').
divisionSign :: ValidChild Content parent
             => ChildHXML parent
divisionSign = Tag_Entity Entity.divisionSign

-- | The latin small letter o with slash HTML entity ('ø').
latinSmallLetterOWithSlash :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterOWithSlash = Tag_Entity Entity.latinSmallLetterOWithSlash

-- | The latin small letter u with grave HTML entity ('ù').
latinSmallLetterUWithGrave :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterUWithGrave = Tag_Entity Entity.latinSmallLetterUWithGrave

-- | The latin small letter u with acute HTML entity ('ú').
latinSmallLetterUWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterUWithAcute = Tag_Entity Entity.latinSmallLetterUWithAcute

-- | The latin small letter u with circumflex HTML entity ('û').
latinSmallLetterUWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterUWithCircumflex = Tag_Entity Entity.latinSmallLetterUWithCircumflex

-- | The latin small letter u with diaeresis HTML entity ('ü').
latinSmallLetterUWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterUWithDiaeresis = Tag_Entity Entity.latinSmallLetterUWithDiaeresis

-- | The latin small letter y with acute HTML entity ('ý').
latinSmallLetterYWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterYWithAcute = Tag_Entity Entity.latinSmallLetterYWithAcute

-- | The latin small letter thorn HTML entity ('þ').
latinSmallLetterThorn :: ValidChild Content parent
                      => ChildHXML parent
latinSmallLetterThorn = Tag_Entity Entity.latinSmallLetterThorn

-- | The latin small letter y with diaeresis HTML entity ('ÿ').
latinSmallLetterYWithDiaeresis :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterYWithDiaeresis = Tag_Entity Entity.latinSmallLetterYWithDiaeresis

-- | The latin capital letter a with macron HTML entity ('Ā').
latinCapitalLetterAWithMacron :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterAWithMacron = Tag_Entity Entity.latinCapitalLetterAWithMacron

-- | The latin small letter a with macron HTML entity ('ā').
latinSmallLetterAWithMacron :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterAWithMacron = Tag_Entity Entity.latinSmallLetterAWithMacron

-- | The latin capital letter a with breve HTML entity ('Ă').
latinCapitalLetterAWithBreve :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterAWithBreve = Tag_Entity Entity.latinCapitalLetterAWithBreve

-- | The latin small letter a with breve HTML entity ('ă').
latinSmallLetterAWithBreve :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterAWithBreve = Tag_Entity Entity.latinSmallLetterAWithBreve

-- | The latin capital letter a with ogonek HTML entity ('Ą').
latinCapitalLetterAWithOgonek :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterAWithOgonek = Tag_Entity Entity.latinCapitalLetterAWithOgonek

-- | The latin small letter a with ogonek HTML entity ('ą').
latinSmallLetterAWithOgonek :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterAWithOgonek = Tag_Entity Entity.latinSmallLetterAWithOgonek

-- | The latin capital letter c with acute HTML entity ('Ć').
latinCapitalLetterCWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterCWithAcute = Tag_Entity Entity.latinCapitalLetterCWithAcute

-- | The latin small letter c with acute HTML entity ('ć').
latinSmallLetterCWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterCWithAcute = Tag_Entity Entity.latinSmallLetterCWithAcute

-- | The latin capital letter c with circumflex HTML entity ('Ĉ').
latinCapitalLetterCWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterCWithCircumflex = Tag_Entity Entity.latinCapitalLetterCWithCircumflex

-- | The latin small letter c with circumflex HTML entity ('ĉ').
latinSmallLetterCWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterCWithCircumflex = Tag_Entity Entity.latinSmallLetterCWithCircumflex

-- | The latin capital letter c with dot above HTML entity ('Ċ').
latinCapitalLetterCWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
latinCapitalLetterCWithDotAbove = Tag_Entity Entity.latinCapitalLetterCWithDotAbove

-- | The latin small letter c with dot above HTML entity ('ċ').
latinSmallLetterCWithDotAbove :: ValidChild Content parent
                              => ChildHXML parent
latinSmallLetterCWithDotAbove = Tag_Entity Entity.latinSmallLetterCWithDotAbove

-- | The latin capital letter c with caron HTML entity ('Č').
latinCapitalLetterCWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterCWithCaron = Tag_Entity Entity.latinCapitalLetterCWithCaron

-- | The latin small letter c with caron HTML entity ('č').
latinSmallLetterCWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterCWithCaron = Tag_Entity Entity.latinSmallLetterCWithCaron

-- | The latin capital letter d with caron HTML entity ('Ď').
latinCapitalLetterDWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterDWithCaron = Tag_Entity Entity.latinCapitalLetterDWithCaron

-- | The latin small letter d with caron HTML entity ('ď').
latinSmallLetterDWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterDWithCaron = Tag_Entity Entity.latinSmallLetterDWithCaron

-- | The latin capital letter d with stroke HTML entity ('Đ').
latinCapitalLetterDWithStroke :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterDWithStroke = Tag_Entity Entity.latinCapitalLetterDWithStroke

-- | The latin small letter d with stroke HTML entity ('đ').
latinSmallLetterDWithStroke :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterDWithStroke = Tag_Entity Entity.latinSmallLetterDWithStroke

-- | The latin capital letter e with macron HTML entity ('Ē').
latinCapitalLetterEWithMacron :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterEWithMacron = Tag_Entity Entity.latinCapitalLetterEWithMacron

-- | The latin small letter e with macron HTML entity ('ē').
latinSmallLetterEWithMacron :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterEWithMacron = Tag_Entity Entity.latinSmallLetterEWithMacron

-- | The latin capital letter e with dot above HTML entity ('Ė').
latinCapitalLetterEWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
latinCapitalLetterEWithDotAbove = Tag_Entity Entity.latinCapitalLetterEWithDotAbove

-- | The latin small letter e with dot above HTML entity ('ė').
latinSmallLetterEWithDotAbove :: ValidChild Content parent
                              => ChildHXML parent
latinSmallLetterEWithDotAbove = Tag_Entity Entity.latinSmallLetterEWithDotAbove

-- | The latin capital letter e with ogonek HTML entity ('Ę').
latinCapitalLetterEWithOgonek :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterEWithOgonek = Tag_Entity Entity.latinCapitalLetterEWithOgonek

-- | The latin small letter e with ogonek HTML entity ('ę').
latinSmallLetterEWithOgonek :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterEWithOgonek = Tag_Entity Entity.latinSmallLetterEWithOgonek

-- | The latin capital letter e with caron HTML entity ('Ě').
latinCapitalLetterEWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterEWithCaron = Tag_Entity Entity.latinCapitalLetterEWithCaron

-- | The latin small letter e with caron HTML entity ('ě').
latinSmallLetterEWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterEWithCaron = Tag_Entity Entity.latinSmallLetterEWithCaron

-- | The latin capital letter g with circumflex HTML entity ('Ĝ').
latinCapitalLetterGWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterGWithCircumflex = Tag_Entity Entity.latinCapitalLetterGWithCircumflex

-- | The latin small letter g with circumflex HTML entity ('ĝ').
latinSmallLetterGWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterGWithCircumflex = Tag_Entity Entity.latinSmallLetterGWithCircumflex

-- | The latin capital letter g with breve HTML entity ('Ğ').
latinCapitalLetterGWithBreve :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterGWithBreve = Tag_Entity Entity.latinCapitalLetterGWithBreve

-- | The latin small letter g with breve HTML entity ('ğ').
latinSmallLetterGWithBreve :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterGWithBreve = Tag_Entity Entity.latinSmallLetterGWithBreve

-- | The latin capital letter g with dot above HTML entity ('Ġ').
latinCapitalLetterGWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
latinCapitalLetterGWithDotAbove = Tag_Entity Entity.latinCapitalLetterGWithDotAbove

-- | The latin small letter g with dot above HTML entity ('ġ').
latinSmallLetterGWithDotAbove :: ValidChild Content parent
                              => ChildHXML parent
latinSmallLetterGWithDotAbove = Tag_Entity Entity.latinSmallLetterGWithDotAbove

-- | The latin capital letter g with cedilla HTML entity ('Ģ').
latinCapitalLetterGWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterGWithCedilla = Tag_Entity Entity.latinCapitalLetterGWithCedilla

-- | The latin capital letter h with circumflex HTML entity ('Ĥ').
latinCapitalLetterHWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterHWithCircumflex = Tag_Entity Entity.latinCapitalLetterHWithCircumflex

-- | The latin small letter h with circumflex HTML entity ('ĥ').
latinSmallLetterHWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterHWithCircumflex = Tag_Entity Entity.latinSmallLetterHWithCircumflex

-- | The latin capital letter h with stroke HTML entity ('Ħ').
latinCapitalLetterHWithStroke :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterHWithStroke = Tag_Entity Entity.latinCapitalLetterHWithStroke

-- | The latin small letter h with stroke HTML entity ('ħ').
latinSmallLetterHWithStroke :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterHWithStroke = Tag_Entity Entity.latinSmallLetterHWithStroke

-- | The latin capital letter i with tilde HTML entity ('Ĩ').
latinCapitalLetterIWithTilde :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterIWithTilde = Tag_Entity Entity.latinCapitalLetterIWithTilde

-- | The latin small letter i with tilde HTML entity ('ĩ').
latinSmallLetterIWithTilde :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterIWithTilde = Tag_Entity Entity.latinSmallLetterIWithTilde

-- | The latin capital letter i with macron HTML entity ('Ī').
latinCapitalLetterIWithMacron :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterIWithMacron = Tag_Entity Entity.latinCapitalLetterIWithMacron

-- | The latin small letter i with macron HTML entity ('ī').
latinSmallLetterIWithMacron :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterIWithMacron = Tag_Entity Entity.latinSmallLetterIWithMacron

-- | The latin capital letter i with ogonek HTML entity ('Į').
latinCapitalLetterIWithOgonek :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterIWithOgonek = Tag_Entity Entity.latinCapitalLetterIWithOgonek

-- | The latin small letter i with ogonek HTML entity ('į').
latinSmallLetterIWithOgonek :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterIWithOgonek = Tag_Entity Entity.latinSmallLetterIWithOgonek

-- | The latin capital letter i with dot above HTML entity ('İ').
latinCapitalLetterIWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
latinCapitalLetterIWithDotAbove = Tag_Entity Entity.latinCapitalLetterIWithDotAbove

-- | The latin small letter dotless i HTML entity ('ı').
latinSmallLetterDotlessI :: ValidChild Content parent
                         => ChildHXML parent
latinSmallLetterDotlessI = Tag_Entity Entity.latinSmallLetterDotlessI

-- | The latin capital ligature ij HTML entity ('Ĳ').
latinCapitalLigatureIj :: ValidChild Content parent
                       => ChildHXML parent
latinCapitalLigatureIj = Tag_Entity Entity.latinCapitalLigatureIj

-- | The latin small ligature ij HTML entity ('ĳ').
latinSmallLigatureIj :: ValidChild Content parent
                     => ChildHXML parent
latinSmallLigatureIj = Tag_Entity Entity.latinSmallLigatureIj

-- | The latin capital letter j with circumflex HTML entity ('Ĵ').
latinCapitalLetterJWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterJWithCircumflex = Tag_Entity Entity.latinCapitalLetterJWithCircumflex

-- | The latin small letter j with circumflex HTML entity ('ĵ').
latinSmallLetterJWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterJWithCircumflex = Tag_Entity Entity.latinSmallLetterJWithCircumflex

-- | The latin capital letter k with cedilla HTML entity ('Ķ').
latinCapitalLetterKWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterKWithCedilla = Tag_Entity Entity.latinCapitalLetterKWithCedilla

-- | The latin small letter k with cedilla HTML entity ('ķ').
latinSmallLetterKWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterKWithCedilla = Tag_Entity Entity.latinSmallLetterKWithCedilla

-- | The latin small letter kra HTML entity ('ĸ').
latinSmallLetterKra :: ValidChild Content parent
                    => ChildHXML parent
latinSmallLetterKra = Tag_Entity Entity.latinSmallLetterKra

-- | The latin capital letter l with acute HTML entity ('Ĺ').
latinCapitalLetterLWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterLWithAcute = Tag_Entity Entity.latinCapitalLetterLWithAcute

-- | The latin small letter l with acute HTML entity ('ĺ').
latinSmallLetterLWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterLWithAcute = Tag_Entity Entity.latinSmallLetterLWithAcute

-- | The latin capital letter l with cedilla HTML entity ('Ļ').
latinCapitalLetterLWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterLWithCedilla = Tag_Entity Entity.latinCapitalLetterLWithCedilla

-- | The latin small letter l with cedilla HTML entity ('ļ').
latinSmallLetterLWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterLWithCedilla = Tag_Entity Entity.latinSmallLetterLWithCedilla

-- | The latin capital letter l with caron HTML entity ('Ľ').
latinCapitalLetterLWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterLWithCaron = Tag_Entity Entity.latinCapitalLetterLWithCaron

-- | The latin small letter l with caron HTML entity ('ľ').
latinSmallLetterLWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterLWithCaron = Tag_Entity Entity.latinSmallLetterLWithCaron

-- | The latin capital letter l with middle dot HTML entity ('Ŀ').
latinCapitalLetterLWithMiddleDot :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterLWithMiddleDot = Tag_Entity Entity.latinCapitalLetterLWithMiddleDot

-- | The latin small letter l with middle dot HTML entity ('ŀ').
latinSmallLetterLWithMiddleDot :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterLWithMiddleDot = Tag_Entity Entity.latinSmallLetterLWithMiddleDot

-- | The latin capital letter l with stroke HTML entity ('Ł').
latinCapitalLetterLWithStroke :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterLWithStroke = Tag_Entity Entity.latinCapitalLetterLWithStroke

-- | The latin small letter l with stroke HTML entity ('ł').
latinSmallLetterLWithStroke :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterLWithStroke = Tag_Entity Entity.latinSmallLetterLWithStroke

-- | The latin capital letter n with acute HTML entity ('Ń').
latinCapitalLetterNWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterNWithAcute = Tag_Entity Entity.latinCapitalLetterNWithAcute

-- | The latin small letter n with acute HTML entity ('ń').
latinSmallLetterNWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterNWithAcute = Tag_Entity Entity.latinSmallLetterNWithAcute

-- | The latin capital letter n with cedilla HTML entity ('Ņ').
latinCapitalLetterNWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterNWithCedilla = Tag_Entity Entity.latinCapitalLetterNWithCedilla

-- | The latin small letter n with cedilla HTML entity ('ņ').
latinSmallLetterNWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterNWithCedilla = Tag_Entity Entity.latinSmallLetterNWithCedilla

-- | The latin capital letter n with caron HTML entity ('Ň').
latinCapitalLetterNWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterNWithCaron = Tag_Entity Entity.latinCapitalLetterNWithCaron

-- | The latin small letter n with caron HTML entity ('ň').
latinSmallLetterNWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterNWithCaron = Tag_Entity Entity.latinSmallLetterNWithCaron

-- | The latin small letter n preceded by apostrophe HTML entity ('ŉ').
latinSmallLetterNPrecededByApostrophe :: ValidChild Content parent
                                      => ChildHXML parent
latinSmallLetterNPrecededByApostrophe = Tag_Entity Entity.latinSmallLetterNPrecededByApostrophe

-- | The latin capital letter eng HTML entity ('Ŋ').
latinCapitalLetterEng :: ValidChild Content parent
                      => ChildHXML parent
latinCapitalLetterEng = Tag_Entity Entity.latinCapitalLetterEng

-- | The latin small letter eng HTML entity ('ŋ').
latinSmallLetterEng :: ValidChild Content parent
                    => ChildHXML parent
latinSmallLetterEng = Tag_Entity Entity.latinSmallLetterEng

-- | The latin capital letter o with macron HTML entity ('Ō').
latinCapitalLetterOWithMacron :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterOWithMacron = Tag_Entity Entity.latinCapitalLetterOWithMacron

-- | The latin small letter o with macron HTML entity ('ō').
latinSmallLetterOWithMacron :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterOWithMacron = Tag_Entity Entity.latinSmallLetterOWithMacron

-- | The latin capital letter o with double acute HTML entity ('Ő').
latinCapitalLetterOWithDoubleAcute :: ValidChild Content parent
                                   => ChildHXML parent
latinCapitalLetterOWithDoubleAcute = Tag_Entity Entity.latinCapitalLetterOWithDoubleAcute

-- | The latin small letter o with double acute HTML entity ('ő').
latinSmallLetterOWithDoubleAcute :: ValidChild Content parent
                                 => ChildHXML parent
latinSmallLetterOWithDoubleAcute = Tag_Entity Entity.latinSmallLetterOWithDoubleAcute

-- | The latin capital letter oe HTML entity ('Œ').
latinCapitalLetterOe :: ValidChild Content parent
                     => ChildHXML parent
latinCapitalLetterOe = Tag_Entity Entity.latinCapitalLetterOe

-- | The latin small letter oe HTML entity ('œ').
latinSmallLetterOe :: ValidChild Content parent
                   => ChildHXML parent
latinSmallLetterOe = Tag_Entity Entity.latinSmallLetterOe

-- | The latin capital letter r with acute HTML entity ('Ŕ').
latinCapitalLetterRWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterRWithAcute = Tag_Entity Entity.latinCapitalLetterRWithAcute

-- | The latin small letter r with acute HTML entity ('ŕ').
latinSmallLetterRWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterRWithAcute = Tag_Entity Entity.latinSmallLetterRWithAcute

-- | The latin capital letter r with cedilla HTML entity ('Ŗ').
latinCapitalLetterRWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterRWithCedilla = Tag_Entity Entity.latinCapitalLetterRWithCedilla

-- | The latin small letter r with cedilla HTML entity ('ŗ').
latinSmallLetterRWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterRWithCedilla = Tag_Entity Entity.latinSmallLetterRWithCedilla

-- | The latin capital letter r with caron HTML entity ('Ř').
latinCapitalLetterRWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterRWithCaron = Tag_Entity Entity.latinCapitalLetterRWithCaron

-- | The latin small letter r with caron HTML entity ('ř').
latinSmallLetterRWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterRWithCaron = Tag_Entity Entity.latinSmallLetterRWithCaron

-- | The latin capital letter s with acute HTML entity ('Ś').
latinCapitalLetterSWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterSWithAcute = Tag_Entity Entity.latinCapitalLetterSWithAcute

-- | The latin small letter s with acute HTML entity ('ś').
latinSmallLetterSWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterSWithAcute = Tag_Entity Entity.latinSmallLetterSWithAcute

-- | The latin capital letter s with circumflex HTML entity ('Ŝ').
latinCapitalLetterSWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterSWithCircumflex = Tag_Entity Entity.latinCapitalLetterSWithCircumflex

-- | The latin small letter s with circumflex HTML entity ('ŝ').
latinSmallLetterSWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterSWithCircumflex = Tag_Entity Entity.latinSmallLetterSWithCircumflex

-- | The latin capital letter s with cedilla HTML entity ('Ş').
latinCapitalLetterSWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterSWithCedilla = Tag_Entity Entity.latinCapitalLetterSWithCedilla

-- | The latin small letter s with cedilla HTML entity ('ş').
latinSmallLetterSWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterSWithCedilla = Tag_Entity Entity.latinSmallLetterSWithCedilla

-- | The latin capital letter s with caron HTML entity ('Š').
latinCapitalLetterSWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterSWithCaron = Tag_Entity Entity.latinCapitalLetterSWithCaron

-- | The latin small letter s with caron HTML entity ('š').
latinSmallLetterSWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterSWithCaron = Tag_Entity Entity.latinSmallLetterSWithCaron

-- | The latin capital letter t with cedilla HTML entity ('Ţ').
latinCapitalLetterTWithCedilla :: ValidChild Content parent
                               => ChildHXML parent
latinCapitalLetterTWithCedilla = Tag_Entity Entity.latinCapitalLetterTWithCedilla

-- | The latin small letter t with cedilla HTML entity ('ţ').
latinSmallLetterTWithCedilla :: ValidChild Content parent
                             => ChildHXML parent
latinSmallLetterTWithCedilla = Tag_Entity Entity.latinSmallLetterTWithCedilla

-- | The latin capital letter t with caron HTML entity ('Ť').
latinCapitalLetterTWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterTWithCaron = Tag_Entity Entity.latinCapitalLetterTWithCaron

-- | The latin small letter t with caron HTML entity ('ť').
latinSmallLetterTWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterTWithCaron = Tag_Entity Entity.latinSmallLetterTWithCaron

-- | The latin capital letter t with stroke HTML entity ('Ŧ').
latinCapitalLetterTWithStroke :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterTWithStroke = Tag_Entity Entity.latinCapitalLetterTWithStroke

-- | The latin small letter t with stroke HTML entity ('ŧ').
latinSmallLetterTWithStroke :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterTWithStroke = Tag_Entity Entity.latinSmallLetterTWithStroke

-- | The latin capital letter u with tilde HTML entity ('Ũ').
latinCapitalLetterUWithTilde :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterUWithTilde = Tag_Entity Entity.latinCapitalLetterUWithTilde

-- | The latin small letter u with tilde HTML entity ('ũ').
latinSmallLetterUWithTilde :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterUWithTilde = Tag_Entity Entity.latinSmallLetterUWithTilde

-- | The latin capital letter u with macron HTML entity ('Ū').
latinCapitalLetterUWithMacron :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterUWithMacron = Tag_Entity Entity.latinCapitalLetterUWithMacron

-- | The latin small letter u with macron HTML entity ('ū').
latinSmallLetterUWithMacron :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterUWithMacron = Tag_Entity Entity.latinSmallLetterUWithMacron

-- | The latin capital letter u with breve HTML entity ('Ŭ').
latinCapitalLetterUWithBreve :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterUWithBreve = Tag_Entity Entity.latinCapitalLetterUWithBreve

-- | The latin small letter u with breve HTML entity ('ŭ').
latinSmallLetterUWithBreve :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterUWithBreve = Tag_Entity Entity.latinSmallLetterUWithBreve

-- | The latin capital letter u with ring above HTML entity ('Ů').
latinCapitalLetterUWithRingAbove :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterUWithRingAbove = Tag_Entity Entity.latinCapitalLetterUWithRingAbove

-- | The latin small letter u with ring above HTML entity ('ů').
latinSmallLetterUWithRingAbove :: ValidChild Content parent
                               => ChildHXML parent
latinSmallLetterUWithRingAbove = Tag_Entity Entity.latinSmallLetterUWithRingAbove

-- | The latin capital letter u with double acute HTML entity ('Ű').
latinCapitalLetterUWithDoubleAcute :: ValidChild Content parent
                                   => ChildHXML parent
latinCapitalLetterUWithDoubleAcute = Tag_Entity Entity.latinCapitalLetterUWithDoubleAcute

-- | The latin small letter u with double acute HTML entity ('ű').
latinSmallLetterUWithDoubleAcute :: ValidChild Content parent
                                 => ChildHXML parent
latinSmallLetterUWithDoubleAcute = Tag_Entity Entity.latinSmallLetterUWithDoubleAcute

-- | The latin capital letter u with ogonek HTML entity ('Ų').
latinCapitalLetterUWithOgonek :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterUWithOgonek = Tag_Entity Entity.latinCapitalLetterUWithOgonek

-- | The latin small letter u with ogonek HTML entity ('ų').
latinSmallLetterUWithOgonek :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterUWithOgonek = Tag_Entity Entity.latinSmallLetterUWithOgonek

-- | The latin capital letter w with circumflex HTML entity ('Ŵ').
latinCapitalLetterWWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterWWithCircumflex = Tag_Entity Entity.latinCapitalLetterWWithCircumflex

-- | The latin small letter w with circumflex HTML entity ('ŵ').
latinSmallLetterWWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterWWithCircumflex = Tag_Entity Entity.latinSmallLetterWWithCircumflex

-- | The latin capital letter y with circumflex HTML entity ('Ŷ').
latinCapitalLetterYWithCircumflex :: ValidChild Content parent
                                  => ChildHXML parent
latinCapitalLetterYWithCircumflex = Tag_Entity Entity.latinCapitalLetterYWithCircumflex

-- | The latin small letter y with circumflex HTML entity ('ŷ').
latinSmallLetterYWithCircumflex :: ValidChild Content parent
                                => ChildHXML parent
latinSmallLetterYWithCircumflex = Tag_Entity Entity.latinSmallLetterYWithCircumflex

-- | The latin capital letter y with diaeresis HTML entity ('Ÿ').
latinCapitalLetterYWithDiaeresis :: ValidChild Content parent
                                 => ChildHXML parent
latinCapitalLetterYWithDiaeresis = Tag_Entity Entity.latinCapitalLetterYWithDiaeresis

-- | The latin capital letter z with acute HTML entity ('Ź').
latinCapitalLetterZWithAcute :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterZWithAcute = Tag_Entity Entity.latinCapitalLetterZWithAcute

-- | The latin small letter z with acute HTML entity ('ź').
latinSmallLetterZWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterZWithAcute = Tag_Entity Entity.latinSmallLetterZWithAcute

-- | The latin capital letter z with dot above HTML entity ('Ż').
latinCapitalLetterZWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
latinCapitalLetterZWithDotAbove = Tag_Entity Entity.latinCapitalLetterZWithDotAbove

-- | The latin small letter z with dot above HTML entity ('ż').
latinSmallLetterZWithDotAbove :: ValidChild Content parent
                              => ChildHXML parent
latinSmallLetterZWithDotAbove = Tag_Entity Entity.latinSmallLetterZWithDotAbove

-- | The latin capital letter z with caron HTML entity ('Ž').
latinCapitalLetterZWithCaron :: ValidChild Content parent
                             => ChildHXML parent
latinCapitalLetterZWithCaron = Tag_Entity Entity.latinCapitalLetterZWithCaron

-- | The latin small letter z with caron HTML entity ('ž').
latinSmallLetterZWithCaron :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterZWithCaron = Tag_Entity Entity.latinSmallLetterZWithCaron

-- | The Latin Small Letter Turned Delta HTML entity ('ƍ').
latinSmallLetterTurnedDelta :: ValidChild Content parent
                            => ChildHXML parent
latinSmallLetterTurnedDelta = Tag_Entity Entity.latinSmallLetterTurnedDelta

-- | The latin small f with hook - function  florin HTML entity ('ƒ').
latinSmallFWithHookFunctionFlorin :: ValidChild Content parent
                                  => ChildHXML parent
latinSmallFWithHookFunctionFlorin = Tag_Entity Entity.latinSmallFWithHookFunctionFlorin

-- | The latin capital letter z with stroke HTML entity ('Ƶ').
latinCapitalLetterZWithStroke :: ValidChild Content parent
                              => ChildHXML parent
latinCapitalLetterZWithStroke = Tag_Entity Entity.latinCapitalLetterZWithStroke

-- | The latin small letter g with acute HTML entity ('ǵ').
latinSmallLetterGWithAcute :: ValidChild Content parent
                           => ChildHXML parent
latinSmallLetterGWithAcute = Tag_Entity Entity.latinSmallLetterGWithAcute

-- | The latin small letter dotless j HTML entity ('ȷ').
latinSmallLetterDotlessJ :: ValidChild Content parent
                         => ChildHXML parent
latinSmallLetterDotlessJ = Tag_Entity Entity.latinSmallLetterDotlessJ

-- | The modifier letter circumflex accent HTML entity ('ˆ').
modifierLetterCircumflexAccent :: ValidChild Content parent
                               => ChildHXML parent
modifierLetterCircumflexAccent = Tag_Entity Entity.modifierLetterCircumflexAccent

-- | The caron HTML entity ('ˇ').
caron :: ValidChild Content parent
      => ChildHXML parent
caron = Tag_Entity Entity.caron

-- | The Modifier Letter Plus Sign HTML entity ('˖').
modifierLetterPlusSign :: ValidChild Content parent
                       => ChildHXML parent
modifierLetterPlusSign = Tag_Entity Entity.modifierLetterPlusSign

-- | The Modifier Letter Minus Sign HTML entity ('˗').
modifierLetterMinusSign :: ValidChild Content parent
                        => ChildHXML parent
modifierLetterMinusSign = Tag_Entity Entity.modifierLetterMinusSign

-- | The breve HTML entity ('˘').
breve :: ValidChild Content parent
      => ChildHXML parent
breve = Tag_Entity Entity.breve

-- | The dot above HTML entity ('˙').
dotAbove :: ValidChild Content parent
         => ChildHXML parent
dotAbove = Tag_Entity Entity.dotAbove

-- | The ring above HTML entity ('˚').
ringAbove :: ValidChild Content parent
          => ChildHXML parent
ringAbove = Tag_Entity Entity.ringAbove

-- | The ogonek HTML entity ('˛').
ogonek :: ValidChild Content parent
       => ChildHXML parent
ogonek = Tag_Entity Entity.ogonek

-- | The small tilde HTML entity ('˜').
smallTilde :: ValidChild Content parent
           => ChildHXML parent
smallTilde = Tag_Entity Entity.smallTilde

-- | The double acute accent HTML entity ('˝').
doubleAcuteAccent :: ValidChild Content parent
                  => ChildHXML parent
doubleAcuteAccent = Tag_Entity Entity.doubleAcuteAccent

-- | The combining inverted breve HTML entity ('̑').
combiningInvertedBreve :: ValidChild Content parent
                       => ChildHXML parent
combiningInvertedBreve = Tag_Entity Entity.combiningInvertedBreve

-- | The Combining Left Angle Above HTML entity ('̚').
combiningLeftAngleAbove :: ValidChild Content parent
                        => ChildHXML parent
combiningLeftAngleAbove = Tag_Entity Entity.combiningLeftAngleAbove

-- | The Combining Plus Sign Below HTML entity ('̟').
combiningPlusSignBelow :: ValidChild Content parent
                       => ChildHXML parent
combiningPlusSignBelow = Tag_Entity Entity.combiningPlusSignBelow

-- | The Combining Minus Sign Below HTML entity ('̠').
combiningMinusSignBelow :: ValidChild Content parent
                        => ChildHXML parent
combiningMinusSignBelow = Tag_Entity Entity.combiningMinusSignBelow

-- | The combining low line HTML entity ('̲').
combiningLowLine :: ValidChild Content parent
                 => ChildHXML parent
combiningLowLine = Tag_Entity Entity.combiningLowLine

-- | The Combining Equals Sign Below HTML entity ('͇').
combiningEqualsSignBelow :: ValidChild Content parent
                         => ChildHXML parent
combiningEqualsSignBelow = Tag_Entity Entity.combiningEqualsSignBelow

-- | The Combining Left Angle Below HTML entity ('͉').
combiningLeftAngleBelow :: ValidChild Content parent
                        => ChildHXML parent
combiningLeftAngleBelow = Tag_Entity Entity.combiningLeftAngleBelow

-- | The Combining Almost Equal To Above HTML entity ('͌').
combiningAlmostEqualToAbove :: ValidChild Content parent
                            => ChildHXML parent
combiningAlmostEqualToAbove = Tag_Entity Entity.combiningAlmostEqualToAbove

-- | The greek capital letter alpha HTML entity ('Α').
greekCapitalLetterAlpha :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterAlpha = Tag_Entity Entity.greekCapitalLetterAlpha

-- | The greek capital letter beta HTML entity ('Β').
greekCapitalLetterBeta :: ValidChild Content parent
                       => ChildHXML parent
greekCapitalLetterBeta = Tag_Entity Entity.greekCapitalLetterBeta

-- | The greek capital letter gamma HTML entity ('Γ').
greekCapitalLetterGamma :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterGamma = Tag_Entity Entity.greekCapitalLetterGamma

-- | The greek capital letter delta HTML entity ('Δ').
greekCapitalLetterDelta :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterDelta = Tag_Entity Entity.greekCapitalLetterDelta

-- | The greek capital letter epsilon HTML entity ('Ε').
greekCapitalLetterEpsilon :: ValidChild Content parent
                          => ChildHXML parent
greekCapitalLetterEpsilon = Tag_Entity Entity.greekCapitalLetterEpsilon

-- | The greek capital letter zeta HTML entity ('Ζ').
greekCapitalLetterZeta :: ValidChild Content parent
                       => ChildHXML parent
greekCapitalLetterZeta = Tag_Entity Entity.greekCapitalLetterZeta

-- | The greek capital letter eta HTML entity ('Η').
greekCapitalLetterEta :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterEta = Tag_Entity Entity.greekCapitalLetterEta

-- | The greek capital letter theta HTML entity ('Θ').
greekCapitalLetterTheta :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterTheta = Tag_Entity Entity.greekCapitalLetterTheta

-- | The greek capital letter iota HTML entity ('Ι').
greekCapitalLetterIota :: ValidChild Content parent
                       => ChildHXML parent
greekCapitalLetterIota = Tag_Entity Entity.greekCapitalLetterIota

-- | The greek capital letter kappa HTML entity ('Κ').
greekCapitalLetterKappa :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterKappa = Tag_Entity Entity.greekCapitalLetterKappa

-- | The greek capital letter lambda HTML entity ('Λ').
greekCapitalLetterLambda :: ValidChild Content parent
                         => ChildHXML parent
greekCapitalLetterLambda = Tag_Entity Entity.greekCapitalLetterLambda

-- | The greek capital letter mu HTML entity ('Μ').
greekCapitalLetterMu :: ValidChild Content parent
                     => ChildHXML parent
greekCapitalLetterMu = Tag_Entity Entity.greekCapitalLetterMu

-- | The greek capital letter nu HTML entity ('Ν').
greekCapitalLetterNu :: ValidChild Content parent
                     => ChildHXML parent
greekCapitalLetterNu = Tag_Entity Entity.greekCapitalLetterNu

-- | The greek capital letter xi HTML entity ('Ξ').
greekCapitalLetterXi :: ValidChild Content parent
                     => ChildHXML parent
greekCapitalLetterXi = Tag_Entity Entity.greekCapitalLetterXi

-- | The greek capital letter omicron HTML entity ('Ο').
greekCapitalLetterOmicron :: ValidChild Content parent
                          => ChildHXML parent
greekCapitalLetterOmicron = Tag_Entity Entity.greekCapitalLetterOmicron

-- | The greek capital letter pi HTML entity ('Π').
greekCapitalLetterPi :: ValidChild Content parent
                     => ChildHXML parent
greekCapitalLetterPi = Tag_Entity Entity.greekCapitalLetterPi

-- | The greek capital letter rho HTML entity ('Ρ').
greekCapitalLetterRho :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterRho = Tag_Entity Entity.greekCapitalLetterRho

-- | The greek capital letter sigma HTML entity ('Σ').
greekCapitalLetterSigma :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterSigma = Tag_Entity Entity.greekCapitalLetterSigma

-- | The greek capital letter tau HTML entity ('Τ').
greekCapitalLetterTau :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterTau = Tag_Entity Entity.greekCapitalLetterTau

-- | The greek capital letter upsilon HTML entity ('Υ').
greekCapitalLetterUpsilon :: ValidChild Content parent
                          => ChildHXML parent
greekCapitalLetterUpsilon = Tag_Entity Entity.greekCapitalLetterUpsilon

-- | The greek capital letter phi HTML entity ('Φ').
greekCapitalLetterPhi :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterPhi = Tag_Entity Entity.greekCapitalLetterPhi

-- | The greek capital letter chi HTML entity ('Χ').
greekCapitalLetterChi :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterChi = Tag_Entity Entity.greekCapitalLetterChi

-- | The greek capital letter psi HTML entity ('Ψ').
greekCapitalLetterPsi :: ValidChild Content parent
                      => ChildHXML parent
greekCapitalLetterPsi = Tag_Entity Entity.greekCapitalLetterPsi

-- | The greek capital letter omega HTML entity ('Ω').
greekCapitalLetterOmega :: ValidChild Content parent
                        => ChildHXML parent
greekCapitalLetterOmega = Tag_Entity Entity.greekCapitalLetterOmega

-- | The greek small letter alpha HTML entity ('α').
greekSmallLetterAlpha :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterAlpha = Tag_Entity Entity.greekSmallLetterAlpha

-- | The greek small letter beta HTML entity ('β').
greekSmallLetterBeta :: ValidChild Content parent
                     => ChildHXML parent
greekSmallLetterBeta = Tag_Entity Entity.greekSmallLetterBeta

-- | The greek small letter gamma HTML entity ('γ').
greekSmallLetterGamma :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterGamma = Tag_Entity Entity.greekSmallLetterGamma

-- | The greek small letter delta HTML entity ('δ').
greekSmallLetterDelta :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterDelta = Tag_Entity Entity.greekSmallLetterDelta

-- | The greek small letter epsilon HTML entity ('ε').
greekSmallLetterEpsilon :: ValidChild Content parent
                        => ChildHXML parent
greekSmallLetterEpsilon = Tag_Entity Entity.greekSmallLetterEpsilon

-- | The greek small letter zeta HTML entity ('ζ').
greekSmallLetterZeta :: ValidChild Content parent
                     => ChildHXML parent
greekSmallLetterZeta = Tag_Entity Entity.greekSmallLetterZeta

-- | The greek small letter eta HTML entity ('η').
greekSmallLetterEta :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterEta = Tag_Entity Entity.greekSmallLetterEta

-- | The greek small letter theta HTML entity ('θ').
greekSmallLetterTheta :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterTheta = Tag_Entity Entity.greekSmallLetterTheta

-- | The greek small letter iota HTML entity ('ι').
greekSmallLetterIota :: ValidChild Content parent
                     => ChildHXML parent
greekSmallLetterIota = Tag_Entity Entity.greekSmallLetterIota

-- | The greek small letter kappa HTML entity ('κ').
greekSmallLetterKappa :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterKappa = Tag_Entity Entity.greekSmallLetterKappa

-- | The greek small letter lambda HTML entity ('λ').
greekSmallLetterLambda :: ValidChild Content parent
                       => ChildHXML parent
greekSmallLetterLambda = Tag_Entity Entity.greekSmallLetterLambda

-- | The greek small letter mu HTML entity ('μ').
greekSmallLetterMu :: ValidChild Content parent
                   => ChildHXML parent
greekSmallLetterMu = Tag_Entity Entity.greekSmallLetterMu

-- | The greek small letter nu HTML entity ('ν').
greekSmallLetterNu :: ValidChild Content parent
                   => ChildHXML parent
greekSmallLetterNu = Tag_Entity Entity.greekSmallLetterNu

-- | The greek small letter xi HTML entity ('ξ').
greekSmallLetterXi :: ValidChild Content parent
                   => ChildHXML parent
greekSmallLetterXi = Tag_Entity Entity.greekSmallLetterXi

-- | The greek small letter omicron HTML entity ('ο').
greekSmallLetterOmicron :: ValidChild Content parent
                        => ChildHXML parent
greekSmallLetterOmicron = Tag_Entity Entity.greekSmallLetterOmicron

-- | The greek small letter pi HTML entity ('π').
greekSmallLetterPi :: ValidChild Content parent
                   => ChildHXML parent
greekSmallLetterPi = Tag_Entity Entity.greekSmallLetterPi

-- | The greek small letter rho HTML entity ('ρ').
greekSmallLetterRho :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterRho = Tag_Entity Entity.greekSmallLetterRho

-- | The greek small letter final sigma HTML entity ('ς').
greekSmallLetterFinalSigma :: ValidChild Content parent
                           => ChildHXML parent
greekSmallLetterFinalSigma = Tag_Entity Entity.greekSmallLetterFinalSigma

-- | The greek small letter sigma HTML entity ('σ').
greekSmallLetterSigma :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterSigma = Tag_Entity Entity.greekSmallLetterSigma

-- | The greek small letter tau HTML entity ('τ').
greekSmallLetterTau :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterTau = Tag_Entity Entity.greekSmallLetterTau

-- | The greek small letter upsilon HTML entity ('υ').
greekSmallLetterUpsilon :: ValidChild Content parent
                        => ChildHXML parent
greekSmallLetterUpsilon = Tag_Entity Entity.greekSmallLetterUpsilon

-- | The greek small letter phi HTML entity ('φ').
greekSmallLetterPhi :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterPhi = Tag_Entity Entity.greekSmallLetterPhi

-- | The greek small letter chi HTML entity ('χ').
greekSmallLetterChi :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterChi = Tag_Entity Entity.greekSmallLetterChi

-- | The greek small letter psi HTML entity ('ψ').
greekSmallLetterPsi :: ValidChild Content parent
                    => ChildHXML parent
greekSmallLetterPsi = Tag_Entity Entity.greekSmallLetterPsi

-- | The greek small letter omega HTML entity ('ω').
greekSmallLetterOmega :: ValidChild Content parent
                      => ChildHXML parent
greekSmallLetterOmega = Tag_Entity Entity.greekSmallLetterOmega

-- | The greek theta symbol HTML entity ('ϑ').
greekThetaSymbol :: ValidChild Content parent
                 => ChildHXML parent
greekThetaSymbol = Tag_Entity Entity.greekThetaSymbol

-- | The greek upsilon with hook symbol HTML entity ('ϒ').
greekUpsilonWithHookSymbol :: ValidChild Content parent
                           => ChildHXML parent
greekUpsilonWithHookSymbol = Tag_Entity Entity.greekUpsilonWithHookSymbol

-- | The greek phi symbol HTML entity ('ϕ').
greekPhiSymbol :: ValidChild Content parent
               => ChildHXML parent
greekPhiSymbol = Tag_Entity Entity.greekPhiSymbol

-- | The greek pi symbol HTML entity ('ϖ').
greekPiSymbol :: ValidChild Content parent
              => ChildHXML parent
greekPiSymbol = Tag_Entity Entity.greekPiSymbol

-- | The greek letter digamma HTML entity ('Ϝ').
greekLetterDigamma :: ValidChild Content parent
                   => ChildHXML parent
greekLetterDigamma = Tag_Entity Entity.greekLetterDigamma

-- | The greek small letter digamma HTML entity ('ϝ').
greekSmallLetterDigamma :: ValidChild Content parent
                        => ChildHXML parent
greekSmallLetterDigamma = Tag_Entity Entity.greekSmallLetterDigamma

-- | The greek kappa symbol HTML entity ('ϰ').
greekKappaSymbol :: ValidChild Content parent
                 => ChildHXML parent
greekKappaSymbol = Tag_Entity Entity.greekKappaSymbol

-- | The greek rho symbol HTML entity ('ϱ').
greekRhoSymbol :: ValidChild Content parent
               => ChildHXML parent
greekRhoSymbol = Tag_Entity Entity.greekRhoSymbol

-- | The greek lunate epsilon symbol HTML entity ('ϵ').
greekLunateEpsilonSymbol :: ValidChild Content parent
                         => ChildHXML parent
greekLunateEpsilonSymbol = Tag_Entity Entity.greekLunateEpsilonSymbol

-- | The greek reversed lunate epsilon symbol HTML entity ('϶').
greekReversedLunateEpsilonSymbol :: ValidChild Content parent
                                 => ChildHXML parent
greekReversedLunateEpsilonSymbol = Tag_Entity Entity.greekReversedLunateEpsilonSymbol

-- | The cyrillic capital letter io HTML entity ('Ё').
cyrillicCapitalLetterIo :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterIo = Tag_Entity Entity.cyrillicCapitalLetterIo

-- | The cyrillic capital letter dje HTML entity ('Ђ').
cyrillicCapitalLetterDje :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterDje = Tag_Entity Entity.cyrillicCapitalLetterDje

-- | The cyrillic capital letter gje HTML entity ('Ѓ').
cyrillicCapitalLetterGje :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterGje = Tag_Entity Entity.cyrillicCapitalLetterGje

-- | The cyrillic capital letter ukrainian ie HTML entity ('Є').
cyrillicCapitalLetterUkrainianIe :: ValidChild Content parent
                                 => ChildHXML parent
cyrillicCapitalLetterUkrainianIe = Tag_Entity Entity.cyrillicCapitalLetterUkrainianIe

-- | The cyrillic capital letter dze HTML entity ('Ѕ').
cyrillicCapitalLetterDze :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterDze = Tag_Entity Entity.cyrillicCapitalLetterDze

-- | The cyrillic capital letter byelorussian-ukrainian i HTML entity ('І').
cyrillicCapitalLetterByelorussianUkrainianI :: ValidChild Content parent
                                            => ChildHXML parent
cyrillicCapitalLetterByelorussianUkrainianI = Tag_Entity Entity.cyrillicCapitalLetterByelorussianUkrainianI

-- | The cyrillic capital letter yi HTML entity ('Ї').
cyrillicCapitalLetterYi :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterYi = Tag_Entity Entity.cyrillicCapitalLetterYi

-- | The cyrillic capital letter je HTML entity ('Ј').
cyrillicCapitalLetterJe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterJe = Tag_Entity Entity.cyrillicCapitalLetterJe

-- | The cyrillic capital letter lje HTML entity ('Љ').
cyrillicCapitalLetterLje :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterLje = Tag_Entity Entity.cyrillicCapitalLetterLje

-- | The cyrillic capital letter nje HTML entity ('Њ').
cyrillicCapitalLetterNje :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterNje = Tag_Entity Entity.cyrillicCapitalLetterNje

-- | The cyrillic capital letter tshe HTML entity ('Ћ').
cyrillicCapitalLetterTshe :: ValidChild Content parent
                          => ChildHXML parent
cyrillicCapitalLetterTshe = Tag_Entity Entity.cyrillicCapitalLetterTshe

-- | The cyrillic capital letter kje HTML entity ('Ќ').
cyrillicCapitalLetterKje :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterKje = Tag_Entity Entity.cyrillicCapitalLetterKje

-- | The cyrillic capital letter short u HTML entity ('Ў').
cyrillicCapitalLetterShortU :: ValidChild Content parent
                            => ChildHXML parent
cyrillicCapitalLetterShortU = Tag_Entity Entity.cyrillicCapitalLetterShortU

-- | The cyrillic capital letter dzhe HTML entity ('Џ').
cyrillicCapitalLetterDzhe :: ValidChild Content parent
                          => ChildHXML parent
cyrillicCapitalLetterDzhe = Tag_Entity Entity.cyrillicCapitalLetterDzhe

-- | The cyrillic capital letter a HTML entity ('А').
cyrillicCapitalLetterA :: ValidChild Content parent
                       => ChildHXML parent
cyrillicCapitalLetterA = Tag_Entity Entity.cyrillicCapitalLetterA

-- | The cyrillic capital letter be HTML entity ('Б').
cyrillicCapitalLetterBe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterBe = Tag_Entity Entity.cyrillicCapitalLetterBe

-- | The cyrillic capital letter ve HTML entity ('В').
cyrillicCapitalLetterVe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterVe = Tag_Entity Entity.cyrillicCapitalLetterVe

-- | The cyrillic capital letter ghe HTML entity ('Г').
cyrillicCapitalLetterGhe :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterGhe = Tag_Entity Entity.cyrillicCapitalLetterGhe

-- | The cyrillic capital letter de HTML entity ('Д').
cyrillicCapitalLetterDe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterDe = Tag_Entity Entity.cyrillicCapitalLetterDe

-- | The cyrillic capital letter ie HTML entity ('Е').
cyrillicCapitalLetterIe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterIe = Tag_Entity Entity.cyrillicCapitalLetterIe

-- | The cyrillic capital letter zhe HTML entity ('Ж').
cyrillicCapitalLetterZhe :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterZhe = Tag_Entity Entity.cyrillicCapitalLetterZhe

-- | The cyrillic capital letter ze HTML entity ('З').
cyrillicCapitalLetterZe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterZe = Tag_Entity Entity.cyrillicCapitalLetterZe

-- | The cyrillic capital letter i HTML entity ('И').
cyrillicCapitalLetterI :: ValidChild Content parent
                       => ChildHXML parent
cyrillicCapitalLetterI = Tag_Entity Entity.cyrillicCapitalLetterI

-- | The cyrillic capital letter short i HTML entity ('Й').
cyrillicCapitalLetterShortI :: ValidChild Content parent
                            => ChildHXML parent
cyrillicCapitalLetterShortI = Tag_Entity Entity.cyrillicCapitalLetterShortI

-- | The cyrillic capital letter ka HTML entity ('К').
cyrillicCapitalLetterKa :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterKa = Tag_Entity Entity.cyrillicCapitalLetterKa

-- | The cyrillic capital letter el HTML entity ('Л').
cyrillicCapitalLetterEl :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEl = Tag_Entity Entity.cyrillicCapitalLetterEl

-- | The cyrillic capital letter em HTML entity ('М').
cyrillicCapitalLetterEm :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEm = Tag_Entity Entity.cyrillicCapitalLetterEm

-- | The cyrillic capital letter en HTML entity ('Н').
cyrillicCapitalLetterEn :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEn = Tag_Entity Entity.cyrillicCapitalLetterEn

-- | The cyrillic capital letter o HTML entity ('О').
cyrillicCapitalLetterO :: ValidChild Content parent
                       => ChildHXML parent
cyrillicCapitalLetterO = Tag_Entity Entity.cyrillicCapitalLetterO

-- | The cyrillic capital letter pe HTML entity ('П').
cyrillicCapitalLetterPe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterPe = Tag_Entity Entity.cyrillicCapitalLetterPe

-- | The cyrillic capital letter er HTML entity ('Р').
cyrillicCapitalLetterEr :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEr = Tag_Entity Entity.cyrillicCapitalLetterEr

-- | The cyrillic capital letter es HTML entity ('С').
cyrillicCapitalLetterEs :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEs = Tag_Entity Entity.cyrillicCapitalLetterEs

-- | The cyrillic capital letter te HTML entity ('Т').
cyrillicCapitalLetterTe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterTe = Tag_Entity Entity.cyrillicCapitalLetterTe

-- | The cyrillic capital letter u HTML entity ('У').
cyrillicCapitalLetterU :: ValidChild Content parent
                       => ChildHXML parent
cyrillicCapitalLetterU = Tag_Entity Entity.cyrillicCapitalLetterU

-- | The cyrillic capital letter ef HTML entity ('Ф').
cyrillicCapitalLetterEf :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterEf = Tag_Entity Entity.cyrillicCapitalLetterEf

-- | The cyrillic capital letter ha HTML entity ('Х').
cyrillicCapitalLetterHa :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterHa = Tag_Entity Entity.cyrillicCapitalLetterHa

-- | The cyrillic capital letter tse HTML entity ('Ц').
cyrillicCapitalLetterTse :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterTse = Tag_Entity Entity.cyrillicCapitalLetterTse

-- | The cyrillic capital letter che HTML entity ('Ч').
cyrillicCapitalLetterChe :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterChe = Tag_Entity Entity.cyrillicCapitalLetterChe

-- | The cyrillic capital letter sha HTML entity ('Ш').
cyrillicCapitalLetterSha :: ValidChild Content parent
                         => ChildHXML parent
cyrillicCapitalLetterSha = Tag_Entity Entity.cyrillicCapitalLetterSha

-- | The cyrillic capital letter shcha HTML entity ('Щ').
cyrillicCapitalLetterShcha :: ValidChild Content parent
                           => ChildHXML parent
cyrillicCapitalLetterShcha = Tag_Entity Entity.cyrillicCapitalLetterShcha

-- | The cyrillic capital letter hard sign HTML entity ('Ъ').
cyrillicCapitalLetterHardSign :: ValidChild Content parent
                              => ChildHXML parent
cyrillicCapitalLetterHardSign = Tag_Entity Entity.cyrillicCapitalLetterHardSign

-- | The cyrillic capital letter yeru HTML entity ('Ы').
cyrillicCapitalLetterYeru :: ValidChild Content parent
                          => ChildHXML parent
cyrillicCapitalLetterYeru = Tag_Entity Entity.cyrillicCapitalLetterYeru

-- | The cyrillic capital letter soft sign HTML entity ('Ь').
cyrillicCapitalLetterSoftSign :: ValidChild Content parent
                              => ChildHXML parent
cyrillicCapitalLetterSoftSign = Tag_Entity Entity.cyrillicCapitalLetterSoftSign

-- | The cyrillic capital letter e HTML entity ('Э').
cyrillicCapitalLetterE :: ValidChild Content parent
                       => ChildHXML parent
cyrillicCapitalLetterE = Tag_Entity Entity.cyrillicCapitalLetterE

-- | The cyrillic capital letter yu HTML entity ('Ю').
cyrillicCapitalLetterYu :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterYu = Tag_Entity Entity.cyrillicCapitalLetterYu

-- | The cyrillic capital letter ya HTML entity ('Я').
cyrillicCapitalLetterYa :: ValidChild Content parent
                        => ChildHXML parent
cyrillicCapitalLetterYa = Tag_Entity Entity.cyrillicCapitalLetterYa

-- | The cyrillic small letter a HTML entity ('а').
cyrillicSmallLetterA :: ValidChild Content parent
                     => ChildHXML parent
cyrillicSmallLetterA = Tag_Entity Entity.cyrillicSmallLetterA

-- | The cyrillic small letter be HTML entity ('б').
cyrillicSmallLetterBe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterBe = Tag_Entity Entity.cyrillicSmallLetterBe

-- | The cyrillic small letter ve HTML entity ('в').
cyrillicSmallLetterVe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterVe = Tag_Entity Entity.cyrillicSmallLetterVe

-- | The cyrillic small letter ghe HTML entity ('г').
cyrillicSmallLetterGhe :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterGhe = Tag_Entity Entity.cyrillicSmallLetterGhe

-- | The cyrillic small letter de HTML entity ('д').
cyrillicSmallLetterDe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterDe = Tag_Entity Entity.cyrillicSmallLetterDe

-- | The cyrillic small letter ie HTML entity ('е').
cyrillicSmallLetterIe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterIe = Tag_Entity Entity.cyrillicSmallLetterIe

-- | The cyrillic small letter zhe HTML entity ('ж').
cyrillicSmallLetterZhe :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterZhe = Tag_Entity Entity.cyrillicSmallLetterZhe

-- | The cyrillic small letter ze HTML entity ('з').
cyrillicSmallLetterZe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterZe = Tag_Entity Entity.cyrillicSmallLetterZe

-- | The cyrillic small letter i HTML entity ('и').
cyrillicSmallLetterI :: ValidChild Content parent
                     => ChildHXML parent
cyrillicSmallLetterI = Tag_Entity Entity.cyrillicSmallLetterI

-- | The cyrillic small letter short i HTML entity ('й').
cyrillicSmallLetterShortI :: ValidChild Content parent
                          => ChildHXML parent
cyrillicSmallLetterShortI = Tag_Entity Entity.cyrillicSmallLetterShortI

-- | The cyrillic small letter ka HTML entity ('к').
cyrillicSmallLetterKa :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterKa = Tag_Entity Entity.cyrillicSmallLetterKa

-- | The cyrillic small letter el HTML entity ('л').
cyrillicSmallLetterEl :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEl = Tag_Entity Entity.cyrillicSmallLetterEl

-- | The cyrillic small letter em HTML entity ('м').
cyrillicSmallLetterEm :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEm = Tag_Entity Entity.cyrillicSmallLetterEm

-- | The cyrillic small letter en HTML entity ('н').
cyrillicSmallLetterEn :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEn = Tag_Entity Entity.cyrillicSmallLetterEn

-- | The cyrillic small letter o HTML entity ('о').
cyrillicSmallLetterO :: ValidChild Content parent
                     => ChildHXML parent
cyrillicSmallLetterO = Tag_Entity Entity.cyrillicSmallLetterO

-- | The cyrillic small letter pe HTML entity ('п').
cyrillicSmallLetterPe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterPe = Tag_Entity Entity.cyrillicSmallLetterPe

-- | The cyrillic small letter er HTML entity ('р').
cyrillicSmallLetterEr :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEr = Tag_Entity Entity.cyrillicSmallLetterEr

-- | The cyrillic small letter es HTML entity ('с').
cyrillicSmallLetterEs :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEs = Tag_Entity Entity.cyrillicSmallLetterEs

-- | The cyrillic small letter te HTML entity ('т').
cyrillicSmallLetterTe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterTe = Tag_Entity Entity.cyrillicSmallLetterTe

-- | The cyrillic small letter u HTML entity ('у').
cyrillicSmallLetterU :: ValidChild Content parent
                     => ChildHXML parent
cyrillicSmallLetterU = Tag_Entity Entity.cyrillicSmallLetterU

-- | The cyrillic small letter ef HTML entity ('ф').
cyrillicSmallLetterEf :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterEf = Tag_Entity Entity.cyrillicSmallLetterEf

-- | The cyrillic small letter ha HTML entity ('х').
cyrillicSmallLetterHa :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterHa = Tag_Entity Entity.cyrillicSmallLetterHa

-- | The cyrillic small letter tse HTML entity ('ц').
cyrillicSmallLetterTse :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterTse = Tag_Entity Entity.cyrillicSmallLetterTse

-- | The cyrillic small letter che HTML entity ('ч').
cyrillicSmallLetterChe :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterChe = Tag_Entity Entity.cyrillicSmallLetterChe

-- | The cyrillic small letter sha HTML entity ('ш').
cyrillicSmallLetterSha :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterSha = Tag_Entity Entity.cyrillicSmallLetterSha

-- | The cyrillic small letter shcha HTML entity ('щ').
cyrillicSmallLetterShcha :: ValidChild Content parent
                         => ChildHXML parent
cyrillicSmallLetterShcha = Tag_Entity Entity.cyrillicSmallLetterShcha

-- | The cyrillic small letter hard sign HTML entity ('ъ').
cyrillicSmallLetterHardSign :: ValidChild Content parent
                            => ChildHXML parent
cyrillicSmallLetterHardSign = Tag_Entity Entity.cyrillicSmallLetterHardSign

-- | The cyrillic small letter yeru HTML entity ('ы').
cyrillicSmallLetterYeru :: ValidChild Content parent
                        => ChildHXML parent
cyrillicSmallLetterYeru = Tag_Entity Entity.cyrillicSmallLetterYeru

-- | The cyrillic small letter soft sign HTML entity ('ь').
cyrillicSmallLetterSoftSign :: ValidChild Content parent
                            => ChildHXML parent
cyrillicSmallLetterSoftSign = Tag_Entity Entity.cyrillicSmallLetterSoftSign

-- | The cyrillic small letter e HTML entity ('э').
cyrillicSmallLetterE :: ValidChild Content parent
                     => ChildHXML parent
cyrillicSmallLetterE = Tag_Entity Entity.cyrillicSmallLetterE

-- | The cyrillic small letter yu HTML entity ('ю').
cyrillicSmallLetterYu :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterYu = Tag_Entity Entity.cyrillicSmallLetterYu

-- | The cyrillic small letter ya HTML entity ('я').
cyrillicSmallLetterYa :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterYa = Tag_Entity Entity.cyrillicSmallLetterYa

-- | The cyrillic small letter io HTML entity ('ё').
cyrillicSmallLetterIo :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterIo = Tag_Entity Entity.cyrillicSmallLetterIo

-- | The cyrillic small letter dje HTML entity ('ђ').
cyrillicSmallLetterDje :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterDje = Tag_Entity Entity.cyrillicSmallLetterDje

-- | The cyrillic small letter gje HTML entity ('ѓ').
cyrillicSmallLetterGje :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterGje = Tag_Entity Entity.cyrillicSmallLetterGje

-- | The cyrillic small letter ukrainian ie HTML entity ('є').
cyrillicSmallLetterUkrainianIe :: ValidChild Content parent
                               => ChildHXML parent
cyrillicSmallLetterUkrainianIe = Tag_Entity Entity.cyrillicSmallLetterUkrainianIe

-- | The cyrillic small letter dze HTML entity ('ѕ').
cyrillicSmallLetterDze :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterDze = Tag_Entity Entity.cyrillicSmallLetterDze

-- | The cyrillic small letter byelorussian-ukrainian i HTML entity ('і').
cyrillicSmallLetterByelorussianUkrainianI :: ValidChild Content parent
                                          => ChildHXML parent
cyrillicSmallLetterByelorussianUkrainianI = Tag_Entity Entity.cyrillicSmallLetterByelorussianUkrainianI

-- | The cyrillic small letter yi HTML entity ('ї').
cyrillicSmallLetterYi :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterYi = Tag_Entity Entity.cyrillicSmallLetterYi

-- | The cyrillic small letter je HTML entity ('ј').
cyrillicSmallLetterJe :: ValidChild Content parent
                      => ChildHXML parent
cyrillicSmallLetterJe = Tag_Entity Entity.cyrillicSmallLetterJe

-- | The cyrillic small letter lje HTML entity ('љ').
cyrillicSmallLetterLje :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterLje = Tag_Entity Entity.cyrillicSmallLetterLje

-- | The cyrillic small letter nje HTML entity ('њ').
cyrillicSmallLetterNje :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterNje = Tag_Entity Entity.cyrillicSmallLetterNje

-- | The cyrillic small letter tshe HTML entity ('ћ').
cyrillicSmallLetterTshe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicSmallLetterTshe = Tag_Entity Entity.cyrillicSmallLetterTshe

-- | The cyrillic small letter kje HTML entity ('ќ').
cyrillicSmallLetterKje :: ValidChild Content parent
                       => ChildHXML parent
cyrillicSmallLetterKje = Tag_Entity Entity.cyrillicSmallLetterKje

-- | The cyrillic small letter short u HTML entity ('ў').
cyrillicSmallLetterShortU :: ValidChild Content parent
                          => ChildHXML parent
cyrillicSmallLetterShortU = Tag_Entity Entity.cyrillicSmallLetterShortU

-- | The cyrillic small letter dzhe HTML entity ('џ').
cyrillicSmallLetterDzhe :: ValidChild Content parent
                        => ChildHXML parent
cyrillicSmallLetterDzhe = Tag_Entity Entity.cyrillicSmallLetterDzhe

-- | The Arabic Percent Sign HTML entity ('٪').
arabicPercentSign :: ValidChild Content parent
                  => ChildHXML parent
arabicPercentSign = Tag_Entity Entity.arabicPercentSign

-- | The Canadian Syllabics Final Plus HTML entity ('ᐩ').
canadianSyllabicsFinalPlus :: ValidChild Content parent
                           => ChildHXML parent
canadianSyllabicsFinalPlus = Tag_Entity Entity.canadianSyllabicsFinalPlus

-- | The Modifier Letter Small Delta HTML entity ('ᵟ').
modifierLetterSmallDelta :: ValidChild Content parent
                         => ChildHXML parent
modifierLetterSmallDelta = Tag_Entity Entity.modifierLetterSmallDelta

-- | The Latin Small Letter Delta HTML entity ('ẟ').
latinSmallLetterDelta :: ValidChild Content parent
                      => ChildHXML parent
latinSmallLetterDelta = Tag_Entity Entity.latinSmallLetterDelta

-- | The en space HTML entity.
enSpace :: ValidChild Content parent
        => ChildHXML parent
enSpace = Tag_Entity Entity.enSpace

-- | The em space HTML entity.
emSpace :: ValidChild Content parent
        => ChildHXML parent
emSpace = Tag_Entity Entity.emSpace

-- | The three-per-em space HTML entity.
threePerEmSpace :: ValidChild Content parent
                => ChildHXML parent
threePerEmSpace = Tag_Entity Entity.threePerEmSpace

-- | The four-per-em space HTML entity.
fourPerEmSpace :: ValidChild Content parent
               => ChildHXML parent
fourPerEmSpace = Tag_Entity Entity.fourPerEmSpace

-- | The figure space HTML entity.
figureSpace :: ValidChild Content parent
            => ChildHXML parent
figureSpace = Tag_Entity Entity.figureSpace

-- | The punctuation space HTML entity.
punctuationSpace :: ValidChild Content parent
                 => ChildHXML parent
punctuationSpace = Tag_Entity Entity.punctuationSpace

-- | The thin space HTML entity.
thinSpace :: ValidChild Content parent
          => ChildHXML parent
thinSpace = Tag_Entity Entity.thinSpace

-- | The hair space HTML entity.
hairSpace :: ValidChild Content parent
          => ChildHXML parent
hairSpace = Tag_Entity Entity.hairSpace

-- | The zero width space HTML entity.
zeroWidthSpace :: ValidChild Content parent
               => ChildHXML parent
zeroWidthSpace = Tag_Entity Entity.zeroWidthSpace

-- | The zero width non-joiner HTML entity.
zeroWidthNonJoiner :: ValidChild Content parent
                   => ChildHXML parent
zeroWidthNonJoiner = Tag_Entity Entity.zeroWidthNonJoiner

-- | The zero width joiner HTML entity.
zeroWidthJoiner :: ValidChild Content parent
                => ChildHXML parent
zeroWidthJoiner = Tag_Entity Entity.zeroWidthJoiner

-- | The left-to-right mark HTML entity.
leftToRightMark :: ValidChild Content parent
                => ChildHXML parent
leftToRightMark = Tag_Entity Entity.leftToRightMark

-- | The right-to-left mark HTML entity.
rightToLeftMark :: ValidChild Content parent
                => ChildHXML parent
rightToLeftMark = Tag_Entity Entity.rightToLeftMark

-- | The hyphen HTML entity ('‐').
hyphen :: ValidChild Content parent
       => ChildHXML parent
hyphen = Tag_Entity Entity.hyphen

-- | The en dash HTML entity ('–').
enDash :: ValidChild Content parent
       => ChildHXML parent
enDash = Tag_Entity Entity.enDash

-- | The em dash HTML entity ('—').
emDash :: ValidChild Content parent
       => ChildHXML parent
emDash = Tag_Entity Entity.emDash

-- | The horizontal bar HTML entity ('―').
horizontalBar :: ValidChild Content parent
              => ChildHXML parent
horizontalBar = Tag_Entity Entity.horizontalBar

-- | The double vertical line HTML entity ('‖').
doubleVerticalLine :: ValidChild Content parent
                   => ChildHXML parent
doubleVerticalLine = Tag_Entity Entity.doubleVerticalLine

-- | The left single quotation mark HTML entity ('‘').
leftSingleQuotationMark :: ValidChild Content parent
                        => ChildHXML parent
leftSingleQuotationMark = Tag_Entity Entity.leftSingleQuotationMark

-- | The right single quotation mark HTML entity ('’').
rightSingleQuotationMark :: ValidChild Content parent
                         => ChildHXML parent
rightSingleQuotationMark = Tag_Entity Entity.rightSingleQuotationMark

-- | The single low-9 quotation mark HTML entity ('‚').
singleLow9QuotationMark :: ValidChild Content parent
                        => ChildHXML parent
singleLow9QuotationMark = Tag_Entity Entity.singleLow9QuotationMark

-- | The left double quotation mark HTML entity ('“').
leftDoubleQuotationMark :: ValidChild Content parent
                        => ChildHXML parent
leftDoubleQuotationMark = Tag_Entity Entity.leftDoubleQuotationMark

-- | The right double quotation mark HTML entity ('”').
rightDoubleQuotationMark :: ValidChild Content parent
                         => ChildHXML parent
rightDoubleQuotationMark = Tag_Entity Entity.rightDoubleQuotationMark

-- | The double low-9 quotation mark HTML entity ('„').
doubleLow9QuotationMark :: ValidChild Content parent
                        => ChildHXML parent
doubleLow9QuotationMark = Tag_Entity Entity.doubleLow9QuotationMark

-- | The dagger HTML entity ('†').
dagger :: ValidChild Content parent
       => ChildHXML parent
dagger = Tag_Entity Entity.dagger

-- | The double dagger HTML entity ('‡').
doubleDagger :: ValidChild Content parent
             => ChildHXML parent
doubleDagger = Tag_Entity Entity.doubleDagger

-- | The bullet HTML entity ('•').
bullet :: ValidChild Content parent
       => ChildHXML parent
bullet = Tag_Entity Entity.bullet

-- | The two dot leader HTML entity ('‥').
twoDotLeader :: ValidChild Content parent
             => ChildHXML parent
twoDotLeader = Tag_Entity Entity.twoDotLeader

-- | The horizontal ellipsis HTML entity ('…').
horizontalEllipsis :: ValidChild Content parent
                   => ChildHXML parent
horizontalEllipsis = Tag_Entity Entity.horizontalEllipsis

-- | The per mille sign - per thousand sign HTML entity ('‰').
perMilleSignPerThousandSign :: ValidChild Content parent
                            => ChildHXML parent
perMilleSignPerThousandSign = Tag_Entity Entity.perMilleSignPerThousandSign

-- | The per ten thousand sign HTML entity ('‱').
perTenThousandSign :: ValidChild Content parent
                   => ChildHXML parent
perTenThousandSign = Tag_Entity Entity.perTenThousandSign

-- | The prime = minutes = feet HTML entity ('′').
prime :: ValidChild Content parent
      => ChildHXML parent
prime = Tag_Entity Entity.prime

-- | The double prime = seconds = inches HTML entity ('″').
doublePrime :: ValidChild Content parent
            => ChildHXML parent
doublePrime = Tag_Entity Entity.doublePrime

-- | The triple prime HTML entity ('‴').
triplePrime :: ValidChild Content parent
            => ChildHXML parent
triplePrime = Tag_Entity Entity.triplePrime

-- | The reversed prime HTML entity ('‵').
reversedPrime :: ValidChild Content parent
              => ChildHXML parent
reversedPrime = Tag_Entity Entity.reversedPrime

-- | The single left-pointing angle quotation mark HTML entity ('‹').
singleLeftPointingAngleQuotationMark :: ValidChild Content parent
                                     => ChildHXML parent
singleLeftPointingAngleQuotationMark = Tag_Entity Entity.singleLeftPointingAngleQuotationMark

-- | The single right-pointing angle quotation mark HTML entity ('›').
singleRightPointingAngleQuotationMark :: ValidChild Content parent
                                      => ChildHXML parent
singleRightPointingAngleQuotationMark = Tag_Entity Entity.singleRightPointingAngleQuotationMark

-- | The overline = spacing overscore HTML entity ('‾').
overline :: ValidChild Content parent
         => ChildHXML parent
overline = Tag_Entity Entity.overline

-- | The caret insertion point HTML entity ('⁁').
caretInsertionPoint :: ValidChild Content parent
                    => ChildHXML parent
caretInsertionPoint = Tag_Entity Entity.caretInsertionPoint

-- | The hyphen bullet HTML entity ('⁃').
hyphenBullet :: ValidChild Content parent
             => ChildHXML parent
hyphenBullet = Tag_Entity Entity.hyphenBullet

-- | The fraction slash HTML entity ('⁄').
fractionSlash :: ValidChild Content parent
              => ChildHXML parent
fractionSlash = Tag_Entity Entity.fractionSlash

-- | The reversed semicolon HTML entity ('⁏').
reversedSemicolon :: ValidChild Content parent
                  => ChildHXML parent
reversedSemicolon = Tag_Entity Entity.reversedSemicolon

-- | The Commercial Minus Sign HTML entity ('⁒').
commercialMinusSign :: ValidChild Content parent
                    => ChildHXML parent
commercialMinusSign = Tag_Entity Entity.commercialMinusSign

-- | The quadruple prime HTML entity ('⁗').
quadruplePrime :: ValidChild Content parent
               => ChildHXML parent
quadruplePrime = Tag_Entity Entity.quadruplePrime

-- | The medium mathematical space HTML entity.
mediumMathematicalSpace :: ValidChild Content parent
                        => ChildHXML parent
mediumMathematicalSpace = Tag_Entity Entity.mediumMathematicalSpace

-- | The word joiner HTML entity.
wordJoiner :: ValidChild Content parent
           => ChildHXML parent
wordJoiner = Tag_Entity Entity.wordJoiner

-- | The function application HTML entity.
functionApplication :: ValidChild Content parent
                    => ChildHXML parent
functionApplication = Tag_Entity Entity.functionApplication

-- | The invisible times HTML entity.
invisibleTimes :: ValidChild Content parent
               => ChildHXML parent
invisibleTimes = Tag_Entity Entity.invisibleTimes

-- | The invisible separator HTML entity.
invisibleSeparator :: ValidChild Content parent
                   => ChildHXML parent
invisibleSeparator = Tag_Entity Entity.invisibleSeparator

-- | The Superscript Plus Sign HTML entity ('⁺').
superscriptPlusSign :: ValidChild Content parent
                    => ChildHXML parent
superscriptPlusSign = Tag_Entity Entity.superscriptPlusSign

-- | The Superscript Minus HTML entity ('⁻').
superscriptMinus :: ValidChild Content parent
                 => ChildHXML parent
superscriptMinus = Tag_Entity Entity.superscriptMinus

-- | The Superscript Equals Sign HTML entity ('⁼').
superscriptEqualsSign :: ValidChild Content parent
                      => ChildHXML parent
superscriptEqualsSign = Tag_Entity Entity.superscriptEqualsSign

-- | The Subscript Plus Sign HTML entity ('₊').
subscriptPlusSign :: ValidChild Content parent
                  => ChildHXML parent
subscriptPlusSign = Tag_Entity Entity.subscriptPlusSign

-- | The Subscript Minus HTML entity ('₋').
subscriptMinus :: ValidChild Content parent
               => ChildHXML parent
subscriptMinus = Tag_Entity Entity.subscriptMinus

-- | The Subscript Equals Sign HTML entity ('₌').
subscriptEqualsSign :: ValidChild Content parent
                    => ChildHXML parent
subscriptEqualsSign = Tag_Entity Entity.subscriptEqualsSign

-- | The euro-currency sign HTML entity ('₠').
euroCurrencySign :: ValidChild Content parent
                 => ChildHXML parent
euroCurrencySign = Tag_Entity Entity.euroCurrencySign

-- | The colon sign HTML entity ('₡').
colonSign :: ValidChild Content parent
          => ChildHXML parent
colonSign = Tag_Entity Entity.colonSign

-- | The cruzeiro sign HTML entity ('₢').
cruzeiroSign :: ValidChild Content parent
             => ChildHXML parent
cruzeiroSign = Tag_Entity Entity.cruzeiroSign

-- | The french franc sign HTML entity ('₣').
frenchFrancSign :: ValidChild Content parent
                => ChildHXML parent
frenchFrancSign = Tag_Entity Entity.frenchFrancSign

-- | The lira sign HTML entity ('₤').
liraSign :: ValidChild Content parent
         => ChildHXML parent
liraSign = Tag_Entity Entity.liraSign

-- | The mill sign HTML entity ('₥').
millSign :: ValidChild Content parent
         => ChildHXML parent
millSign = Tag_Entity Entity.millSign

-- | The naira sign HTML entity ('₦').
nairaSign :: ValidChild Content parent
          => ChildHXML parent
nairaSign = Tag_Entity Entity.nairaSign

-- | The peseta sign HTML entity ('₧').
pesetaSign :: ValidChild Content parent
           => ChildHXML parent
pesetaSign = Tag_Entity Entity.pesetaSign

-- | The rupee sign HTML entity ('₨').
rupeeSign :: ValidChild Content parent
          => ChildHXML parent
rupeeSign = Tag_Entity Entity.rupeeSign

-- | The won sign HTML entity ('₩').
wonSign :: ValidChild Content parent
        => ChildHXML parent
wonSign = Tag_Entity Entity.wonSign

-- | The new sheqel sign HTML entity ('₪').
newSheqelSign :: ValidChild Content parent
              => ChildHXML parent
newSheqelSign = Tag_Entity Entity.newSheqelSign

-- | The dong sign HTML entity ('₫').
dongSign :: ValidChild Content parent
         => ChildHXML parent
dongSign = Tag_Entity Entity.dongSign

-- | The euro sign HTML entity ('€').
euroSign :: ValidChild Content parent
         => ChildHXML parent
euroSign = Tag_Entity Entity.euroSign

-- | The kip sign HTML entity ('₭').
kipSign :: ValidChild Content parent
        => ChildHXML parent
kipSign = Tag_Entity Entity.kipSign

-- | The tugrik sign HTML entity ('₮').
tugrikSign :: ValidChild Content parent
           => ChildHXML parent
tugrikSign = Tag_Entity Entity.tugrikSign

-- | The drachma sign HTML entity ('₯').
drachmaSign :: ValidChild Content parent
            => ChildHXML parent
drachmaSign = Tag_Entity Entity.drachmaSign

-- | The german penny symbol HTML entity ('₰').
germanPennySymbol :: ValidChild Content parent
                  => ChildHXML parent
germanPennySymbol = Tag_Entity Entity.germanPennySymbol

-- | The peso sign HTML entity ('₱').
pesoSign :: ValidChild Content parent
         => ChildHXML parent
pesoSign = Tag_Entity Entity.pesoSign

-- | The guarani sign HTML entity ('₲').
guaraniSign :: ValidChild Content parent
            => ChildHXML parent
guaraniSign = Tag_Entity Entity.guaraniSign

-- | The austral sign HTML entity ('₳').
australSign :: ValidChild Content parent
            => ChildHXML parent
australSign = Tag_Entity Entity.australSign

-- | The hryvnia sign HTML entity ('₴').
hryvniaSign :: ValidChild Content parent
            => ChildHXML parent
hryvniaSign = Tag_Entity Entity.hryvniaSign

-- | The cedi sign HTML entity ('₵').
cediSign :: ValidChild Content parent
         => ChildHXML parent
cediSign = Tag_Entity Entity.cediSign

-- | The livre tournois sign HTML entity ('₶').
livreTournoisSign :: ValidChild Content parent
                  => ChildHXML parent
livreTournoisSign = Tag_Entity Entity.livreTournoisSign

-- | The spesmilo sign HTML entity ('₷').
spesmiloSign :: ValidChild Content parent
             => ChildHXML parent
spesmiloSign = Tag_Entity Entity.spesmiloSign

-- | The tenge sign HTML entity ('₸').
tengeSign :: ValidChild Content parent
          => ChildHXML parent
tengeSign = Tag_Entity Entity.tengeSign

-- | The indian rupee sign HTML entity ('₹').
indianRupeeSign :: ValidChild Content parent
                => ChildHXML parent
indianRupeeSign = Tag_Entity Entity.indianRupeeSign

-- | The turkish lira sign HTML entity ('₺').
turkishLiraSign :: ValidChild Content parent
                => ChildHXML parent
turkishLiraSign = Tag_Entity Entity.turkishLiraSign

-- | The nordic mark sign HTML entity ('₻').
nordicMarkSign :: ValidChild Content parent
               => ChildHXML parent
nordicMarkSign = Tag_Entity Entity.nordicMarkSign

-- | The manat sign HTML entity ('₼').
manatSign :: ValidChild Content parent
          => ChildHXML parent
manatSign = Tag_Entity Entity.manatSign

-- | The ruble sign HTML entity ('₽').
rubleSign :: ValidChild Content parent
          => ChildHXML parent
rubleSign = Tag_Entity Entity.rubleSign

-- | The lari sign HTML entity ('₾').
lariSign :: ValidChild Content parent
         => ChildHXML parent
lariSign = Tag_Entity Entity.lariSign

-- | The bitcoin sign HTML entity ('₿').
bitcoinSign :: ValidChild Content parent
            => ChildHXML parent
bitcoinSign = Tag_Entity Entity.bitcoinSign

-- | The combining three dots above HTML entity ('⃛').
combiningThreeDotsAbove :: ValidChild Content parent
                        => ChildHXML parent
combiningThreeDotsAbove = Tag_Entity Entity.combiningThreeDotsAbove

-- | The combining four dots above HTML entity ('⃜').
combiningFourDotsAbove :: ValidChild Content parent
                       => ChildHXML parent
combiningFourDotsAbove = Tag_Entity Entity.combiningFourDotsAbove

-- | The double-struck capital c HTML entity ('ℂ').
doubleStruckCapitalC :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalC = Tag_Entity Entity.doubleStruckCapitalC

-- | The care of HTML entity ('℅').
careOf :: ValidChild Content parent
       => ChildHXML parent
careOf = Tag_Entity Entity.careOf

-- | The script small g HTML entity ('ℊ').
scriptSmallG :: ValidChild Content parent
             => ChildHXML parent
scriptSmallG = Tag_Entity Entity.scriptSmallG

-- | The script capital h HTML entity ('ℋ').
scriptCapitalH :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalH = Tag_Entity Entity.scriptCapitalH

-- | The black-letter capital h HTML entity ('ℌ').
blackLetterCapitalH :: ValidChild Content parent
                    => ChildHXML parent
blackLetterCapitalH = Tag_Entity Entity.blackLetterCapitalH

-- | The double-struck capital h HTML entity ('ℍ').
doubleStruckCapitalH :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalH = Tag_Entity Entity.doubleStruckCapitalH

-- | The planck constant HTML entity ('ℎ').
planckConstant :: ValidChild Content parent
               => ChildHXML parent
planckConstant = Tag_Entity Entity.planckConstant

-- | The planck constant over two pi HTML entity ('ℏ').
planckConstantOverTwoPi :: ValidChild Content parent
                        => ChildHXML parent
planckConstantOverTwoPi = Tag_Entity Entity.planckConstantOverTwoPi

-- | The script capital i HTML entity ('ℐ').
scriptCapitalI :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalI = Tag_Entity Entity.scriptCapitalI

-- | The black-letter capital i HTML entity ('ℑ').
blackLetterCapitalI :: ValidChild Content parent
                    => ChildHXML parent
blackLetterCapitalI = Tag_Entity Entity.blackLetterCapitalI

-- | The script capital l HTML entity ('ℒ').
scriptCapitalL :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalL = Tag_Entity Entity.scriptCapitalL

-- | The script small l HTML entity ('ℓ').
scriptSmallL :: ValidChild Content parent
             => ChildHXML parent
scriptSmallL = Tag_Entity Entity.scriptSmallL

-- | The double-struck capital n HTML entity ('ℕ').
doubleStruckCapitalN :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalN = Tag_Entity Entity.doubleStruckCapitalN

-- | The numero sign HTML entity ('№').
numeroSign :: ValidChild Content parent
           => ChildHXML parent
numeroSign = Tag_Entity Entity.numeroSign

-- | The sound recording copyright HTML entity ('℗').
soundRecordingCopyright :: ValidChild Content parent
                        => ChildHXML parent
soundRecordingCopyright = Tag_Entity Entity.soundRecordingCopyright

-- | The script capital p HTML entity ('℘').
scriptCapitalP :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalP = Tag_Entity Entity.scriptCapitalP

-- | The double-struck capital p HTML entity ('ℙ').
doubleStruckCapitalP :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalP = Tag_Entity Entity.doubleStruckCapitalP

-- | The double-struck capital q HTML entity ('ℚ').
doubleStruckCapitalQ :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalQ = Tag_Entity Entity.doubleStruckCapitalQ

-- | The script capital r HTML entity ('ℛ').
scriptCapitalR :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalR = Tag_Entity Entity.scriptCapitalR

-- | The black-letter capital r HTML entity ('ℜ').
blackLetterCapitalR :: ValidChild Content parent
                    => ChildHXML parent
blackLetterCapitalR = Tag_Entity Entity.blackLetterCapitalR

-- | The double-struck capital r HTML entity ('ℝ').
doubleStruckCapitalR :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalR = Tag_Entity Entity.doubleStruckCapitalR

-- | The prescription take HTML entity ('℞').
prescriptionTake :: ValidChild Content parent
                 => ChildHXML parent
prescriptionTake = Tag_Entity Entity.prescriptionTake

-- | The trade mark sign HTML entity ('™').
tradeMarkSign :: ValidChild Content parent
              => ChildHXML parent
tradeMarkSign = Tag_Entity Entity.tradeMarkSign

-- | The double-struck capital z HTML entity ('ℤ').
doubleStruckCapitalZ :: ValidChild Content parent
                     => ChildHXML parent
doubleStruckCapitalZ = Tag_Entity Entity.doubleStruckCapitalZ

-- | The ohm sign HTML entity ('Ω').
ohmSign :: ValidChild Content parent
        => ChildHXML parent
ohmSign = Tag_Entity Entity.ohmSign

-- | The inverted ohm sign HTML entity ('℧').
invertedOhmSign :: ValidChild Content parent
                => ChildHXML parent
invertedOhmSign = Tag_Entity Entity.invertedOhmSign

-- | The black-letter capital z HTML entity ('ℨ').
blackLetterCapitalZ :: ValidChild Content parent
                    => ChildHXML parent
blackLetterCapitalZ = Tag_Entity Entity.blackLetterCapitalZ

-- | The turned greek small letter iota HTML entity ('℩').
turnedGreekSmallLetterIota :: ValidChild Content parent
                           => ChildHXML parent
turnedGreekSmallLetterIota = Tag_Entity Entity.turnedGreekSmallLetterIota

-- | The angstrom sign HTML entity ('Å').
angstromSign :: ValidChild Content parent
             => ChildHXML parent
angstromSign = Tag_Entity Entity.angstromSign

-- | The script capital b HTML entity ('ℬ').
scriptCapitalB :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalB = Tag_Entity Entity.scriptCapitalB

-- | The black-letter capital c HTML entity ('ℭ').
blackLetterCapitalC :: ValidChild Content parent
                    => ChildHXML parent
blackLetterCapitalC = Tag_Entity Entity.blackLetterCapitalC

-- | The script small e HTML entity ('ℯ').
scriptSmallE :: ValidChild Content parent
             => ChildHXML parent
scriptSmallE = Tag_Entity Entity.scriptSmallE

-- | The script capital e HTML entity ('ℰ').
scriptCapitalE :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalE = Tag_Entity Entity.scriptCapitalE

-- | The script capital f HTML entity ('ℱ').
scriptCapitalF :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalF = Tag_Entity Entity.scriptCapitalF

-- | The script capital m HTML entity ('ℳ').
scriptCapitalM :: ValidChild Content parent
               => ChildHXML parent
scriptCapitalM = Tag_Entity Entity.scriptCapitalM

-- | The script small o HTML entity ('ℴ').
scriptSmallO :: ValidChild Content parent
             => ChildHXML parent
scriptSmallO = Tag_Entity Entity.scriptSmallO

-- | The alef symbol HTML entity ('ℵ').
alefSymbol :: ValidChild Content parent
           => ChildHXML parent
alefSymbol = Tag_Entity Entity.alefSymbol

-- | The bet symbol HTML entity ('ℶ').
betSymbol :: ValidChild Content parent
          => ChildHXML parent
betSymbol = Tag_Entity Entity.betSymbol

-- | The gimel symbol HTML entity ('ℷ').
gimelSymbol :: ValidChild Content parent
            => ChildHXML parent
gimelSymbol = Tag_Entity Entity.gimelSymbol

-- | The dalet symbol HTML entity ('ℸ').
daletSymbol :: ValidChild Content parent
            => ChildHXML parent
daletSymbol = Tag_Entity Entity.daletSymbol

-- | The Double-struck N-ary Summation HTML entity ('⅀').
doubleStruckNArySummation :: ValidChild Content parent
                          => ChildHXML parent
doubleStruckNArySummation = Tag_Entity Entity.doubleStruckNArySummation

-- | The double-struck italic capital d HTML entity ('ⅅ').
doubleStruckItalicCapitalD :: ValidChild Content parent
                           => ChildHXML parent
doubleStruckItalicCapitalD = Tag_Entity Entity.doubleStruckItalicCapitalD

-- | The double-struck italic small d HTML entity ('ⅆ').
doubleStruckItalicSmallD :: ValidChild Content parent
                         => ChildHXML parent
doubleStruckItalicSmallD = Tag_Entity Entity.doubleStruckItalicSmallD

-- | The double-struck italic small e HTML entity ('ⅇ').
doubleStruckItalicSmallE :: ValidChild Content parent
                         => ChildHXML parent
doubleStruckItalicSmallE = Tag_Entity Entity.doubleStruckItalicSmallE

-- | The double-struck italic small i HTML entity ('ⅈ').
doubleStruckItalicSmallI :: ValidChild Content parent
                         => ChildHXML parent
doubleStruckItalicSmallI = Tag_Entity Entity.doubleStruckItalicSmallI

-- | The Vulgar Fraction One Seventh HTML entity ('⅐').
vulgarFractionOneSeventh :: ValidChild Content parent
                         => ChildHXML parent
vulgarFractionOneSeventh = Tag_Entity Entity.vulgarFractionOneSeventh

-- | The Vulgar Fraction One Ninth HTML entity ('⅑').
vulgarFractionOneNinth :: ValidChild Content parent
                       => ChildHXML parent
vulgarFractionOneNinth = Tag_Entity Entity.vulgarFractionOneNinth

-- | The Vulgar Fraction One Tenth HTML entity ('⅒').
vulgarFractionOneTenth :: ValidChild Content parent
                       => ChildHXML parent
vulgarFractionOneTenth = Tag_Entity Entity.vulgarFractionOneTenth

-- | The vulgar fraction one third HTML entity ('⅓').
vulgarFractionOneThird :: ValidChild Content parent
                       => ChildHXML parent
vulgarFractionOneThird = Tag_Entity Entity.vulgarFractionOneThird

-- | The vulgar fraction two thirds HTML entity ('⅔').
vulgarFractionTwoThirds :: ValidChild Content parent
                        => ChildHXML parent
vulgarFractionTwoThirds = Tag_Entity Entity.vulgarFractionTwoThirds

-- | The vulgar fraction one fifth HTML entity ('⅕').
vulgarFractionOneFifth :: ValidChild Content parent
                       => ChildHXML parent
vulgarFractionOneFifth = Tag_Entity Entity.vulgarFractionOneFifth

-- | The vulgar fraction two fifths HTML entity ('⅖').
vulgarFractionTwoFifths :: ValidChild Content parent
                        => ChildHXML parent
vulgarFractionTwoFifths = Tag_Entity Entity.vulgarFractionTwoFifths

-- | The vulgar fraction three fifths HTML entity ('⅗').
vulgarFractionThreeFifths :: ValidChild Content parent
                          => ChildHXML parent
vulgarFractionThreeFifths = Tag_Entity Entity.vulgarFractionThreeFifths

-- | The vulgar fraction four fifths HTML entity ('⅘').
vulgarFractionFourFifths :: ValidChild Content parent
                         => ChildHXML parent
vulgarFractionFourFifths = Tag_Entity Entity.vulgarFractionFourFifths

-- | The vulgar fraction one sixth HTML entity ('⅙').
vulgarFractionOneSixth :: ValidChild Content parent
                       => ChildHXML parent
vulgarFractionOneSixth = Tag_Entity Entity.vulgarFractionOneSixth

-- | The vulgar fraction five sixths HTML entity ('⅚').
vulgarFractionFiveSixths :: ValidChild Content parent
                         => ChildHXML parent
vulgarFractionFiveSixths = Tag_Entity Entity.vulgarFractionFiveSixths

-- | The vulgar fraction one eighth HTML entity ('⅛').
vulgarFractionOneEighth :: ValidChild Content parent
                        => ChildHXML parent
vulgarFractionOneEighth = Tag_Entity Entity.vulgarFractionOneEighth

-- | The vulgar fraction three eighths HTML entity ('⅜').
vulgarFractionThreeEighths :: ValidChild Content parent
                           => ChildHXML parent
vulgarFractionThreeEighths = Tag_Entity Entity.vulgarFractionThreeEighths

-- | The vulgar fraction five eighths HTML entity ('⅝').
vulgarFractionFiveEighths :: ValidChild Content parent
                          => ChildHXML parent
vulgarFractionFiveEighths = Tag_Entity Entity.vulgarFractionFiveEighths

-- | The vulgar fraction seven eighths HTML entity ('⅞').
vulgarFractionSevenEighths :: ValidChild Content parent
                           => ChildHXML parent
vulgarFractionSevenEighths = Tag_Entity Entity.vulgarFractionSevenEighths

-- | The Fraction Numerator One HTML entity ('⅟').
fractionNumeratorOne :: ValidChild Content parent
                     => ChildHXML parent
fractionNumeratorOne = Tag_Entity Entity.fractionNumeratorOne

-- | The leftwards arrow HTML entity ('←').
leftwardsArrow :: ValidChild Content parent
               => ChildHXML parent
leftwardsArrow = Tag_Entity Entity.leftwardsArrow

-- | The upwards arrow HTML entity ('↑').
upwardsArrow :: ValidChild Content parent
             => ChildHXML parent
upwardsArrow = Tag_Entity Entity.upwardsArrow

-- | The rightwards arrow HTML entity ('→').
rightwardsArrow :: ValidChild Content parent
                => ChildHXML parent
rightwardsArrow = Tag_Entity Entity.rightwardsArrow

-- | The downwards arrow HTML entity ('↓').
downwardsArrow :: ValidChild Content parent
               => ChildHXML parent
downwardsArrow = Tag_Entity Entity.downwardsArrow

-- | The left right arrow HTML entity ('↔').
leftRightArrow :: ValidChild Content parent
               => ChildHXML parent
leftRightArrow = Tag_Entity Entity.leftRightArrow

-- | The up down arrow HTML entity ('↕').
upDownArrow :: ValidChild Content parent
            => ChildHXML parent
upDownArrow = Tag_Entity Entity.upDownArrow

-- | The north west arrow HTML entity ('↖').
northWestArrow :: ValidChild Content parent
               => ChildHXML parent
northWestArrow = Tag_Entity Entity.northWestArrow

-- | The north east arrow HTML entity ('↗').
northEastArrow :: ValidChild Content parent
               => ChildHXML parent
northEastArrow = Tag_Entity Entity.northEastArrow

-- | The south east arrow HTML entity ('↘').
southEastArrow :: ValidChild Content parent
               => ChildHXML parent
southEastArrow = Tag_Entity Entity.southEastArrow

-- | The south west arrow HTML entity ('↙').
southWestArrow :: ValidChild Content parent
               => ChildHXML parent
southWestArrow = Tag_Entity Entity.southWestArrow

-- | The leftwards arrow with stroke HTML entity ('↚').
leftwardsArrowWithStroke :: ValidChild Content parent
                         => ChildHXML parent
leftwardsArrowWithStroke = Tag_Entity Entity.leftwardsArrowWithStroke

-- | The rightwards arrow with stroke HTML entity ('↛').
rightwardsArrowWithStroke :: ValidChild Content parent
                          => ChildHXML parent
rightwardsArrowWithStroke = Tag_Entity Entity.rightwardsArrowWithStroke

-- | The rightwards wave arrow HTML entity ('↝').
rightwardsWaveArrow :: ValidChild Content parent
                    => ChildHXML parent
rightwardsWaveArrow = Tag_Entity Entity.rightwardsWaveArrow

-- | The leftwards two headed arrow HTML entity ('↞').
leftwardsTwoHeadedArrow :: ValidChild Content parent
                        => ChildHXML parent
leftwardsTwoHeadedArrow = Tag_Entity Entity.leftwardsTwoHeadedArrow

-- | The upwards two headed arrow HTML entity ('↟').
upwardsTwoHeadedArrow :: ValidChild Content parent
                      => ChildHXML parent
upwardsTwoHeadedArrow = Tag_Entity Entity.upwardsTwoHeadedArrow

-- | The rightwards two headed arrow HTML entity ('↠').
rightwardsTwoHeadedArrow :: ValidChild Content parent
                         => ChildHXML parent
rightwardsTwoHeadedArrow = Tag_Entity Entity.rightwardsTwoHeadedArrow

-- | The downwards two headed arrow HTML entity ('↡').
downwardsTwoHeadedArrow :: ValidChild Content parent
                        => ChildHXML parent
downwardsTwoHeadedArrow = Tag_Entity Entity.downwardsTwoHeadedArrow

-- | The leftwards arrow with tail HTML entity ('↢').
leftwardsArrowWithTail :: ValidChild Content parent
                       => ChildHXML parent
leftwardsArrowWithTail = Tag_Entity Entity.leftwardsArrowWithTail

-- | The rightwards arrow with tail HTML entity ('↣').
rightwardsArrowWithTail :: ValidChild Content parent
                        => ChildHXML parent
rightwardsArrowWithTail = Tag_Entity Entity.rightwardsArrowWithTail

-- | The leftwards arrow from bar HTML entity ('↤').
leftwardsArrowFromBar :: ValidChild Content parent
                      => ChildHXML parent
leftwardsArrowFromBar = Tag_Entity Entity.leftwardsArrowFromBar

-- | The upwards arrow from bar HTML entity ('↥').
upwardsArrowFromBar :: ValidChild Content parent
                    => ChildHXML parent
upwardsArrowFromBar = Tag_Entity Entity.upwardsArrowFromBar

-- | The rightwards arrow from bar HTML entity ('↦').
rightwardsArrowFromBar :: ValidChild Content parent
                       => ChildHXML parent
rightwardsArrowFromBar = Tag_Entity Entity.rightwardsArrowFromBar

-- | The downwards arrow from bar HTML entity ('↧').
downwardsArrowFromBar :: ValidChild Content parent
                      => ChildHXML parent
downwardsArrowFromBar = Tag_Entity Entity.downwardsArrowFromBar

-- | The leftwards arrow with hook HTML entity ('↩').
leftwardsArrowWithHook :: ValidChild Content parent
                       => ChildHXML parent
leftwardsArrowWithHook = Tag_Entity Entity.leftwardsArrowWithHook

-- | The rightwards arrow with hook HTML entity ('↪').
rightwardsArrowWithHook :: ValidChild Content parent
                        => ChildHXML parent
rightwardsArrowWithHook = Tag_Entity Entity.rightwardsArrowWithHook

-- | The leftwards arrow with loop HTML entity ('↫').
leftwardsArrowWithLoop :: ValidChild Content parent
                       => ChildHXML parent
leftwardsArrowWithLoop = Tag_Entity Entity.leftwardsArrowWithLoop

-- | The rightwards arrow with loop HTML entity ('↬').
rightwardsArrowWithLoop :: ValidChild Content parent
                        => ChildHXML parent
rightwardsArrowWithLoop = Tag_Entity Entity.rightwardsArrowWithLoop

-- | The left right wave arrow HTML entity ('↭').
leftRightWaveArrow :: ValidChild Content parent
                   => ChildHXML parent
leftRightWaveArrow = Tag_Entity Entity.leftRightWaveArrow

-- | The left right arrow with stroke HTML entity ('↮').
leftRightArrowWithStroke :: ValidChild Content parent
                         => ChildHXML parent
leftRightArrowWithStroke = Tag_Entity Entity.leftRightArrowWithStroke

-- | The upwards arrow with tip leftwards HTML entity ('↰').
upwardsArrowWithTipLeftwards :: ValidChild Content parent
                             => ChildHXML parent
upwardsArrowWithTipLeftwards = Tag_Entity Entity.upwardsArrowWithTipLeftwards

-- | The upwards arrow with tip rightwards HTML entity ('↱').
upwardsArrowWithTipRightwards :: ValidChild Content parent
                              => ChildHXML parent
upwardsArrowWithTipRightwards = Tag_Entity Entity.upwardsArrowWithTipRightwards

-- | The downwards arrow with tip leftwards HTML entity ('↲').
downwardsArrowWithTipLeftwards :: ValidChild Content parent
                               => ChildHXML parent
downwardsArrowWithTipLeftwards = Tag_Entity Entity.downwardsArrowWithTipLeftwards

-- | The downwards arrow with tip rightwards HTML entity ('↳').
downwardsArrowWithTipRightwards :: ValidChild Content parent
                                => ChildHXML parent
downwardsArrowWithTipRightwards = Tag_Entity Entity.downwardsArrowWithTipRightwards

-- | The downwards arrow with corner leftwards = carriage return HTML entity ('↵').
downwardsArrowWithCornerLeftwards :: ValidChild Content parent
                                  => ChildHXML parent
downwardsArrowWithCornerLeftwards = Tag_Entity Entity.downwardsArrowWithCornerLeftwards

-- | The anticlockwise top semicircle arrow HTML entity ('↶').
anticlockwiseTopSemicircleArrow :: ValidChild Content parent
                                => ChildHXML parent
anticlockwiseTopSemicircleArrow = Tag_Entity Entity.anticlockwiseTopSemicircleArrow

-- | The clockwise top semicircle arrow HTML entity ('↷').
clockwiseTopSemicircleArrow :: ValidChild Content parent
                            => ChildHXML parent
clockwiseTopSemicircleArrow = Tag_Entity Entity.clockwiseTopSemicircleArrow

-- | The anticlockwise open circle arrow HTML entity ('↺').
anticlockwiseOpenCircleArrow :: ValidChild Content parent
                             => ChildHXML parent
anticlockwiseOpenCircleArrow = Tag_Entity Entity.anticlockwiseOpenCircleArrow

-- | The clockwise open circle arrow HTML entity ('↻').
clockwiseOpenCircleArrow :: ValidChild Content parent
                         => ChildHXML parent
clockwiseOpenCircleArrow = Tag_Entity Entity.clockwiseOpenCircleArrow

-- | The leftwards harpoon with barb upwards HTML entity ('↼').
leftwardsHarpoonWithBarbUpwards :: ValidChild Content parent
                                => ChildHXML parent
leftwardsHarpoonWithBarbUpwards = Tag_Entity Entity.leftwardsHarpoonWithBarbUpwards

-- | The leftwards harpoon with barb downwards HTML entity ('↽').
leftwardsHarpoonWithBarbDownwards :: ValidChild Content parent
                                  => ChildHXML parent
leftwardsHarpoonWithBarbDownwards = Tag_Entity Entity.leftwardsHarpoonWithBarbDownwards

-- | The upwards harpoon with barb rightwards HTML entity ('↾').
upwardsHarpoonWithBarbRightwards :: ValidChild Content parent
                                 => ChildHXML parent
upwardsHarpoonWithBarbRightwards = Tag_Entity Entity.upwardsHarpoonWithBarbRightwards

-- | The upwards harpoon with barb leftwards HTML entity ('↿').
upwardsHarpoonWithBarbLeftwards :: ValidChild Content parent
                                => ChildHXML parent
upwardsHarpoonWithBarbLeftwards = Tag_Entity Entity.upwardsHarpoonWithBarbLeftwards

-- | The rightwards harpoon with barb upwards HTML entity ('⇀').
rightwardsHarpoonWithBarbUpwards :: ValidChild Content parent
                                 => ChildHXML parent
rightwardsHarpoonWithBarbUpwards = Tag_Entity Entity.rightwardsHarpoonWithBarbUpwards

-- | The rightwards harpoon with barb downwards HTML entity ('⇁').
rightwardsHarpoonWithBarbDownwards :: ValidChild Content parent
                                   => ChildHXML parent
rightwardsHarpoonWithBarbDownwards = Tag_Entity Entity.rightwardsHarpoonWithBarbDownwards

-- | The downwards harpoon with barb rightwards HTML entity ('⇂').
downwardsHarpoonWithBarbRightwards :: ValidChild Content parent
                                   => ChildHXML parent
downwardsHarpoonWithBarbRightwards = Tag_Entity Entity.downwardsHarpoonWithBarbRightwards

-- | The downwards harpoon with barb leftwards HTML entity ('⇃').
downwardsHarpoonWithBarbLeftwards :: ValidChild Content parent
                                  => ChildHXML parent
downwardsHarpoonWithBarbLeftwards = Tag_Entity Entity.downwardsHarpoonWithBarbLeftwards

-- | The rightwards arrow over leftwards arrow HTML entity ('⇄').
rightwardsArrowOverLeftwardsArrow :: ValidChild Content parent
                                  => ChildHXML parent
rightwardsArrowOverLeftwardsArrow = Tag_Entity Entity.rightwardsArrowOverLeftwardsArrow

-- | The upwards arrow leftwards of downwards arrow HTML entity ('⇅').
upwardsArrowLeftwardsOfDownwardsArrow :: ValidChild Content parent
                                      => ChildHXML parent
upwardsArrowLeftwardsOfDownwardsArrow = Tag_Entity Entity.upwardsArrowLeftwardsOfDownwardsArrow

-- | The leftwards arrow over rightwards arrow HTML entity ('⇆').
leftwardsArrowOverRightwardsArrow :: ValidChild Content parent
                                  => ChildHXML parent
leftwardsArrowOverRightwardsArrow = Tag_Entity Entity.leftwardsArrowOverRightwardsArrow

-- | The leftwards paired arrows HTML entity ('⇇').
leftwardsPairedArrows :: ValidChild Content parent
                      => ChildHXML parent
leftwardsPairedArrows = Tag_Entity Entity.leftwardsPairedArrows

-- | The upwards paired arrows HTML entity ('⇈').
upwardsPairedArrows :: ValidChild Content parent
                    => ChildHXML parent
upwardsPairedArrows = Tag_Entity Entity.upwardsPairedArrows

-- | The rightwards paired arrows HTML entity ('⇉').
rightwardsPairedArrows :: ValidChild Content parent
                       => ChildHXML parent
rightwardsPairedArrows = Tag_Entity Entity.rightwardsPairedArrows

-- | The downwards paired arrows HTML entity ('⇊').
downwardsPairedArrows :: ValidChild Content parent
                      => ChildHXML parent
downwardsPairedArrows = Tag_Entity Entity.downwardsPairedArrows

-- | The leftwards harpoon over rightwards harpoon HTML entity ('⇋').
leftwardsHarpoonOverRightwardsHarpoon :: ValidChild Content parent
                                      => ChildHXML parent
leftwardsHarpoonOverRightwardsHarpoon = Tag_Entity Entity.leftwardsHarpoonOverRightwardsHarpoon

-- | The rightwards harpoon over leftwards harpoon HTML entity ('⇌').
rightwardsHarpoonOverLeftwardsHarpoon :: ValidChild Content parent
                                      => ChildHXML parent
rightwardsHarpoonOverLeftwardsHarpoon = Tag_Entity Entity.rightwardsHarpoonOverLeftwardsHarpoon

-- | The leftwards double arrow with stroke HTML entity ('⇍').
leftwardsDoubleArrowWithStroke :: ValidChild Content parent
                               => ChildHXML parent
leftwardsDoubleArrowWithStroke = Tag_Entity Entity.leftwardsDoubleArrowWithStroke

-- | The left right double arrow with stroke HTML entity ('⇎').
leftRightDoubleArrowWithStroke :: ValidChild Content parent
                               => ChildHXML parent
leftRightDoubleArrowWithStroke = Tag_Entity Entity.leftRightDoubleArrowWithStroke

-- | The rightwards double arrow with stroke HTML entity ('⇏').
rightwardsDoubleArrowWithStroke :: ValidChild Content parent
                                => ChildHXML parent
rightwardsDoubleArrowWithStroke = Tag_Entity Entity.rightwardsDoubleArrowWithStroke

-- | The leftwards double arrow HTML entity ('⇐').
leftwardsDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
leftwardsDoubleArrow = Tag_Entity Entity.leftwardsDoubleArrow

-- | The upwards double arrow HTML entity ('⇑').
upwardsDoubleArrow :: ValidChild Content parent
                   => ChildHXML parent
upwardsDoubleArrow = Tag_Entity Entity.upwardsDoubleArrow

-- | The rightwards double arrow HTML entity ('⇒').
rightwardsDoubleArrow :: ValidChild Content parent
                      => ChildHXML parent
rightwardsDoubleArrow = Tag_Entity Entity.rightwardsDoubleArrow

-- | The downwards double arrow HTML entity ('⇓').
downwardsDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
downwardsDoubleArrow = Tag_Entity Entity.downwardsDoubleArrow

-- | The left right double arrow HTML entity ('⇔').
leftRightDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
leftRightDoubleArrow = Tag_Entity Entity.leftRightDoubleArrow

-- | The up down double arrow HTML entity ('⇕').
upDownDoubleArrow :: ValidChild Content parent
                  => ChildHXML parent
upDownDoubleArrow = Tag_Entity Entity.upDownDoubleArrow

-- | The north west double arrow HTML entity ('⇖').
northWestDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
northWestDoubleArrow = Tag_Entity Entity.northWestDoubleArrow

-- | The north east double arrow HTML entity ('⇗').
northEastDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
northEastDoubleArrow = Tag_Entity Entity.northEastDoubleArrow

-- | The south east double arrow HTML entity ('⇘').
southEastDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
southEastDoubleArrow = Tag_Entity Entity.southEastDoubleArrow

-- | The south west double arrow HTML entity ('⇙').
southWestDoubleArrow :: ValidChild Content parent
                     => ChildHXML parent
southWestDoubleArrow = Tag_Entity Entity.southWestDoubleArrow

-- | The leftwards triple arrow HTML entity ('⇚').
leftwardsTripleArrow :: ValidChild Content parent
                     => ChildHXML parent
leftwardsTripleArrow = Tag_Entity Entity.leftwardsTripleArrow

-- | The rightwards triple arrow HTML entity ('⇛').
rightwardsTripleArrow :: ValidChild Content parent
                      => ChildHXML parent
rightwardsTripleArrow = Tag_Entity Entity.rightwardsTripleArrow

-- | The rightwards squiggle arrow HTML entity ('⇝').
rightwardsSquiggleArrow :: ValidChild Content parent
                        => ChildHXML parent
rightwardsSquiggleArrow = Tag_Entity Entity.rightwardsSquiggleArrow

-- | The leftwards arrow to bar HTML entity ('⇤').
leftwardsArrowToBar :: ValidChild Content parent
                    => ChildHXML parent
leftwardsArrowToBar = Tag_Entity Entity.leftwardsArrowToBar

-- | The rightwards arrow to bar HTML entity ('⇥').
rightwardsArrowToBar :: ValidChild Content parent
                     => ChildHXML parent
rightwardsArrowToBar = Tag_Entity Entity.rightwardsArrowToBar

-- | The downwards arrow leftwards of upwards arrow HTML entity ('⇵').
downwardsArrowLeftwardsOfUpwardsArrow :: ValidChild Content parent
                                      => ChildHXML parent
downwardsArrowLeftwardsOfUpwardsArrow = Tag_Entity Entity.downwardsArrowLeftwardsOfUpwardsArrow

-- | The leftwards open-headed arrow HTML entity ('⇽').
leftwardsOpenHeadedArrow :: ValidChild Content parent
                         => ChildHXML parent
leftwardsOpenHeadedArrow = Tag_Entity Entity.leftwardsOpenHeadedArrow

-- | The rightwards open-headed arrow HTML entity ('⇾').
rightwardsOpenHeadedArrow :: ValidChild Content parent
                          => ChildHXML parent
rightwardsOpenHeadedArrow = Tag_Entity Entity.rightwardsOpenHeadedArrow

-- | The left right open-headed arrow HTML entity ('⇿').
leftRightOpenHeadedArrow :: ValidChild Content parent
                         => ChildHXML parent
leftRightOpenHeadedArrow = Tag_Entity Entity.leftRightOpenHeadedArrow

-- | The for all HTML entity ('∀').
forAll :: ValidChild Content parent
       => ChildHXML parent
forAll = Tag_Entity Entity.forAll

-- | The complement HTML entity ('∁').
complement :: ValidChild Content parent
           => ChildHXML parent
complement = Tag_Entity Entity.complement

-- | The partial differential HTML entity ('∂').
partialDifferential :: ValidChild Content parent
                    => ChildHXML parent
partialDifferential = Tag_Entity Entity.partialDifferential

-- | The there exists HTML entity ('∃').
thereExists :: ValidChild Content parent
            => ChildHXML parent
thereExists = Tag_Entity Entity.thereExists

-- | The there does not exist HTML entity ('∄').
thereDoesNotExist :: ValidChild Content parent
                  => ChildHXML parent
thereDoesNotExist = Tag_Entity Entity.thereDoesNotExist

-- | The empty set HTML entity ('∅').
emptySet :: ValidChild Content parent
         => ChildHXML parent
emptySet = Tag_Entity Entity.emptySet

-- | The nabla HTML entity ('∇').
nabla :: ValidChild Content parent
      => ChildHXML parent
nabla = Tag_Entity Entity.nabla

-- | The element of HTML entity ('∈').
elementOf :: ValidChild Content parent
          => ChildHXML parent
elementOf = Tag_Entity Entity.elementOf

-- | The not an element of HTML entity ('∉').
notAnElementOf :: ValidChild Content parent
               => ChildHXML parent
notAnElementOf = Tag_Entity Entity.notAnElementOf

-- | The contains as member HTML entity ('∋').
containsAsMember :: ValidChild Content parent
                 => ChildHXML parent
containsAsMember = Tag_Entity Entity.containsAsMember

-- | The does not contain as member HTML entity ('∌').
doesNotContainAsMember :: ValidChild Content parent
                       => ChildHXML parent
doesNotContainAsMember = Tag_Entity Entity.doesNotContainAsMember

-- | The n-ary product HTML entity ('∏').
nAryProduct :: ValidChild Content parent
            => ChildHXML parent
nAryProduct = Tag_Entity Entity.nAryProduct

-- | The n-ary coproduct HTML entity ('∐').
nAryCoproduct :: ValidChild Content parent
              => ChildHXML parent
nAryCoproduct = Tag_Entity Entity.nAryCoproduct

-- | The n-ary summation HTML entity ('∑').
nArySummation :: ValidChild Content parent
              => ChildHXML parent
nArySummation = Tag_Entity Entity.nArySummation

-- | The minus sign HTML entity ('−').
minusSign :: ValidChild Content parent
          => ChildHXML parent
minusSign = Tag_Entity Entity.minusSign

-- | The minus-or-plus sign HTML entity ('∓').
minusOrPlusSign :: ValidChild Content parent
                => ChildHXML parent
minusOrPlusSign = Tag_Entity Entity.minusOrPlusSign

-- | The dot plus HTML entity ('∔').
dotPlus :: ValidChild Content parent
        => ChildHXML parent
dotPlus = Tag_Entity Entity.dotPlus

-- | The Division Slash HTML entity ('∕').
divisionSlash :: ValidChild Content parent
              => ChildHXML parent
divisionSlash = Tag_Entity Entity.divisionSlash

-- | The set minus HTML entity ('∖').
setMinus :: ValidChild Content parent
         => ChildHXML parent
setMinus = Tag_Entity Entity.setMinus

-- | The asterisk operator HTML entity ('∗').
asteriskOperator :: ValidChild Content parent
                 => ChildHXML parent
asteriskOperator = Tag_Entity Entity.asteriskOperator

-- | The ring operator HTML entity ('∘').
ringOperator :: ValidChild Content parent
             => ChildHXML parent
ringOperator = Tag_Entity Entity.ringOperator

-- | The square root HTML entity ('√').
squareRoot :: ValidChild Content parent
           => ChildHXML parent
squareRoot = Tag_Entity Entity.squareRoot

-- | The Cube Root HTML entity ('∛').
cubeRoot :: ValidChild Content parent
         => ChildHXML parent
cubeRoot = Tag_Entity Entity.cubeRoot

-- | The Fourth Root HTML entity ('∜').
fourthRoot :: ValidChild Content parent
           => ChildHXML parent
fourthRoot = Tag_Entity Entity.fourthRoot

-- | The proportional to HTML entity ('∝').
proportionalTo :: ValidChild Content parent
               => ChildHXML parent
proportionalTo = Tag_Entity Entity.proportionalTo

-- | The infinity HTML entity ('∞').
infinity :: ValidChild Content parent
         => ChildHXML parent
infinity = Tag_Entity Entity.infinity

-- | The right angle HTML entity ('∟').
rightAngle :: ValidChild Content parent
           => ChildHXML parent
rightAngle = Tag_Entity Entity.rightAngle

-- | The angle HTML entity ('∠').
angle :: ValidChild Content parent
      => ChildHXML parent
angle = Tag_Entity Entity.angle

-- | The measured angle HTML entity ('∡').
measuredAngle :: ValidChild Content parent
              => ChildHXML parent
measuredAngle = Tag_Entity Entity.measuredAngle

-- | The spherical angle HTML entity ('∢').
sphericalAngle :: ValidChild Content parent
               => ChildHXML parent
sphericalAngle = Tag_Entity Entity.sphericalAngle

-- | The divides HTML entity ('∣').
divides :: ValidChild Content parent
        => ChildHXML parent
divides = Tag_Entity Entity.divides

-- | The does not divide HTML entity ('∤').
doesNotDivide :: ValidChild Content parent
              => ChildHXML parent
doesNotDivide = Tag_Entity Entity.doesNotDivide

-- | The parallel to HTML entity ('∥').
parallelTo :: ValidChild Content parent
           => ChildHXML parent
parallelTo = Tag_Entity Entity.parallelTo

-- | The not parallel to HTML entity ('∦').
notParallelTo :: ValidChild Content parent
              => ChildHXML parent
notParallelTo = Tag_Entity Entity.notParallelTo

-- | The logical and HTML entity ('∧').
logicalAnd :: ValidChild Content parent
           => ChildHXML parent
logicalAnd = Tag_Entity Entity.logicalAnd

-- | The logical or HTML entity ('∨').
logicalOr :: ValidChild Content parent
          => ChildHXML parent
logicalOr = Tag_Entity Entity.logicalOr

-- | The intersection = cap HTML entity ('∩').
intersection :: ValidChild Content parent
             => ChildHXML parent
intersection = Tag_Entity Entity.intersection

-- | The union = cup HTML entity ('∪').
union :: ValidChild Content parent
      => ChildHXML parent
union = Tag_Entity Entity.union

-- | The integral HTML entity ('∫').
integral :: ValidChild Content parent
         => ChildHXML parent
integral = Tag_Entity Entity.integral

-- | The double integral HTML entity ('∬').
doubleIntegral :: ValidChild Content parent
               => ChildHXML parent
doubleIntegral = Tag_Entity Entity.doubleIntegral

-- | The triple integral HTML entity ('∭').
tripleIntegral :: ValidChild Content parent
               => ChildHXML parent
tripleIntegral = Tag_Entity Entity.tripleIntegral

-- | The contour integral HTML entity ('∮').
contourIntegral :: ValidChild Content parent
                => ChildHXML parent
contourIntegral = Tag_Entity Entity.contourIntegral

-- | The surface integral HTML entity ('∯').
surfaceIntegral :: ValidChild Content parent
                => ChildHXML parent
surfaceIntegral = Tag_Entity Entity.surfaceIntegral

-- | The volume integral HTML entity ('∰').
volumeIntegral :: ValidChild Content parent
               => ChildHXML parent
volumeIntegral = Tag_Entity Entity.volumeIntegral

-- | The clockwise integral HTML entity ('∱').
clockwiseIntegral :: ValidChild Content parent
                  => ChildHXML parent
clockwiseIntegral = Tag_Entity Entity.clockwiseIntegral

-- | The clockwise contour integral HTML entity ('∲').
clockwiseContourIntegral :: ValidChild Content parent
                         => ChildHXML parent
clockwiseContourIntegral = Tag_Entity Entity.clockwiseContourIntegral

-- | The anticlockwise contour integral HTML entity ('∳').
anticlockwiseContourIntegral :: ValidChild Content parent
                             => ChildHXML parent
anticlockwiseContourIntegral = Tag_Entity Entity.anticlockwiseContourIntegral

-- | The therefore HTML entity ('∴').
therefore :: ValidChild Content parent
          => ChildHXML parent
therefore = Tag_Entity Entity.therefore

-- | The because HTML entity ('∵').
because :: ValidChild Content parent
        => ChildHXML parent
because = Tag_Entity Entity.because

-- | The ratio HTML entity ('∶').
ratio :: ValidChild Content parent
      => ChildHXML parent
ratio = Tag_Entity Entity.ratio

-- | The proportion HTML entity ('∷').
proportion :: ValidChild Content parent
           => ChildHXML parent
proportion = Tag_Entity Entity.proportion

-- | The dot minus HTML entity ('∸').
dotMinus :: ValidChild Content parent
         => ChildHXML parent
dotMinus = Tag_Entity Entity.dotMinus

-- | The geometric proportion HTML entity ('∺').
geometricProportion :: ValidChild Content parent
                    => ChildHXML parent
geometricProportion = Tag_Entity Entity.geometricProportion

-- | The homothetic HTML entity ('∻').
homothetic :: ValidChild Content parent
           => ChildHXML parent
homothetic = Tag_Entity Entity.homothetic

-- | The tilde operator HTML entity ('∼').
tildeOperator :: ValidChild Content parent
              => ChildHXML parent
tildeOperator = Tag_Entity Entity.tildeOperator

-- | The reversed tilde HTML entity ('∽').
reversedTilde :: ValidChild Content parent
              => ChildHXML parent
reversedTilde = Tag_Entity Entity.reversedTilde

-- | The inverted lazy s HTML entity ('∾').
invertedLazyS :: ValidChild Content parent
              => ChildHXML parent
invertedLazyS = Tag_Entity Entity.invertedLazyS

-- | The sine wave HTML entity ('∿').
sineWave :: ValidChild Content parent
         => ChildHXML parent
sineWave = Tag_Entity Entity.sineWave

-- | The wreath product HTML entity ('≀').
wreathProduct :: ValidChild Content parent
              => ChildHXML parent
wreathProduct = Tag_Entity Entity.wreathProduct

-- | The not tilde HTML entity ('≁').
notTilde :: ValidChild Content parent
         => ChildHXML parent
notTilde = Tag_Entity Entity.notTilde

-- | The minus tilde HTML entity ('≂').
minusTilde :: ValidChild Content parent
           => ChildHXML parent
minusTilde = Tag_Entity Entity.minusTilde

-- | The asymptotically equal to HTML entity ('≃').
asymptoticallyEqualTo :: ValidChild Content parent
                      => ChildHXML parent
asymptoticallyEqualTo = Tag_Entity Entity.asymptoticallyEqualTo

-- | The not asymptotically equal to HTML entity ('≄').
notAsymptoticallyEqualTo :: ValidChild Content parent
                         => ChildHXML parent
notAsymptoticallyEqualTo = Tag_Entity Entity.notAsymptoticallyEqualTo

-- | The approximately equal to HTML entity ('≅').
approximatelyEqualTo :: ValidChild Content parent
                     => ChildHXML parent
approximatelyEqualTo = Tag_Entity Entity.approximatelyEqualTo

-- | The approximately but not actually equal to HTML entity ('≆').
approximatelyButNotActuallyEqualTo :: ValidChild Content parent
                                   => ChildHXML parent
approximatelyButNotActuallyEqualTo = Tag_Entity Entity.approximatelyButNotActuallyEqualTo

-- | The neither approximately nor actually equal to HTML entity ('≇').
neitherApproximatelyNorActuallyEqualTo :: ValidChild Content parent
                                       => ChildHXML parent
neitherApproximatelyNorActuallyEqualTo = Tag_Entity Entity.neitherApproximatelyNorActuallyEqualTo

-- | The almost equal to HTML entity ('≈').
almostEqualTo :: ValidChild Content parent
              => ChildHXML parent
almostEqualTo = Tag_Entity Entity.almostEqualTo

-- | The not almost equal to HTML entity ('≉').
notAlmostEqualTo :: ValidChild Content parent
                 => ChildHXML parent
notAlmostEqualTo = Tag_Entity Entity.notAlmostEqualTo

-- | The almost equal or equal to HTML entity ('≊').
almostEqualOrEqualTo :: ValidChild Content parent
                     => ChildHXML parent
almostEqualOrEqualTo = Tag_Entity Entity.almostEqualOrEqualTo

-- | The triple tilde HTML entity ('≋').
tripleTilde :: ValidChild Content parent
            => ChildHXML parent
tripleTilde = Tag_Entity Entity.tripleTilde

-- | The all equal to HTML entity ('≌').
allEqualTo :: ValidChild Content parent
           => ChildHXML parent
allEqualTo = Tag_Entity Entity.allEqualTo

-- | The equivalent to HTML entity ('≍').
equivalentTo :: ValidChild Content parent
             => ChildHXML parent
equivalentTo = Tag_Entity Entity.equivalentTo

-- | The geometrically equivalent to HTML entity ('≎').
geometricallyEquivalentTo :: ValidChild Content parent
                          => ChildHXML parent
geometricallyEquivalentTo = Tag_Entity Entity.geometricallyEquivalentTo

-- | The difference between HTML entity ('≏').
differenceBetween :: ValidChild Content parent
                  => ChildHXML parent
differenceBetween = Tag_Entity Entity.differenceBetween

-- | The approaches the limit HTML entity ('≐').
approachesTheLimit :: ValidChild Content parent
                   => ChildHXML parent
approachesTheLimit = Tag_Entity Entity.approachesTheLimit

-- | The geometrically equal to HTML entity ('≑').
geometricallyEqualTo :: ValidChild Content parent
                     => ChildHXML parent
geometricallyEqualTo = Tag_Entity Entity.geometricallyEqualTo

-- | The approximately equal to or the image of HTML entity ('≒').
approximatelyEqualToOrTheImageOf :: ValidChild Content parent
                                 => ChildHXML parent
approximatelyEqualToOrTheImageOf = Tag_Entity Entity.approximatelyEqualToOrTheImageOf

-- | The image of or approximately equal to HTML entity ('≓').
imageOfOrApproximatelyEqualTo :: ValidChild Content parent
                              => ChildHXML parent
imageOfOrApproximatelyEqualTo = Tag_Entity Entity.imageOfOrApproximatelyEqualTo

-- | The colon equals HTML entity ('≔').
colonEquals :: ValidChild Content parent
            => ChildHXML parent
colonEquals = Tag_Entity Entity.colonEquals

-- | The equals colon HTML entity ('≕').
equalsColon :: ValidChild Content parent
            => ChildHXML parent
equalsColon = Tag_Entity Entity.equalsColon

-- | The ring in equal to HTML entity ('≖').
ringInEqualTo :: ValidChild Content parent
              => ChildHXML parent
ringInEqualTo = Tag_Entity Entity.ringInEqualTo

-- | The ring equal to HTML entity ('≗').
ringEqualTo :: ValidChild Content parent
            => ChildHXML parent
ringEqualTo = Tag_Entity Entity.ringEqualTo

-- | The estimates HTML entity ('≙').
estimates :: ValidChild Content parent
          => ChildHXML parent
estimates = Tag_Entity Entity.estimates

-- | The equiangular to HTML entity ('≚').
equiangularTo :: ValidChild Content parent
              => ChildHXML parent
equiangularTo = Tag_Entity Entity.equiangularTo

-- | The Star Equals HTML entity ('≛').
starEquals :: ValidChild Content parent
           => ChildHXML parent
starEquals = Tag_Entity Entity.starEquals

-- | The delta equal to HTML entity ('≜').
deltaEqualTo :: ValidChild Content parent
             => ChildHXML parent
deltaEqualTo = Tag_Entity Entity.deltaEqualTo

-- | The Equal To By Definition HTML entity ('≝').
equalToByDefinition :: ValidChild Content parent
                    => ChildHXML parent
equalToByDefinition = Tag_Entity Entity.equalToByDefinition

-- | The questioned equal to HTML entity ('≟').
questionedEqualTo :: ValidChild Content parent
                  => ChildHXML parent
questionedEqualTo = Tag_Entity Entity.questionedEqualTo

-- | The not equal to HTML entity ('≠').
notEqualTo :: ValidChild Content parent
           => ChildHXML parent
notEqualTo = Tag_Entity Entity.notEqualTo

-- | The identical to HTML entity ('≡').
identicalTo :: ValidChild Content parent
            => ChildHXML parent
identicalTo = Tag_Entity Entity.identicalTo

-- | The not identical to HTML entity ('≢').
notIdenticalTo :: ValidChild Content parent
               => ChildHXML parent
notIdenticalTo = Tag_Entity Entity.notIdenticalTo

-- | The less-than or equal to HTML entity ('≤').
lessThanOrEqualTo :: ValidChild Content parent
                  => ChildHXML parent
lessThanOrEqualTo = Tag_Entity Entity.lessThanOrEqualTo

-- | The greater-than or equal to HTML entity ('≥').
greaterThanOrEqualTo :: ValidChild Content parent
                     => ChildHXML parent
greaterThanOrEqualTo = Tag_Entity Entity.greaterThanOrEqualTo

-- | The less-than over equal to HTML entity ('≦').
lessThanOverEqualTo :: ValidChild Content parent
                    => ChildHXML parent
lessThanOverEqualTo = Tag_Entity Entity.lessThanOverEqualTo

-- | The greater-than over equal to HTML entity ('≧').
greaterThanOverEqualTo :: ValidChild Content parent
                       => ChildHXML parent
greaterThanOverEqualTo = Tag_Entity Entity.greaterThanOverEqualTo

-- | The less-than but not equal to HTML entity ('≨').
lessThanButNotEqualTo :: ValidChild Content parent
                      => ChildHXML parent
lessThanButNotEqualTo = Tag_Entity Entity.lessThanButNotEqualTo

-- | The greater-than but not equal to HTML entity ('≩').
greaterThanButNotEqualTo :: ValidChild Content parent
                         => ChildHXML parent
greaterThanButNotEqualTo = Tag_Entity Entity.greaterThanButNotEqualTo

-- | The much less-than HTML entity ('≪').
muchLessThan :: ValidChild Content parent
             => ChildHXML parent
muchLessThan = Tag_Entity Entity.muchLessThan

-- | The much greater-than HTML entity ('≫').
muchGreaterThan :: ValidChild Content parent
                => ChildHXML parent
muchGreaterThan = Tag_Entity Entity.muchGreaterThan

-- | The between HTML entity ('≬').
between :: ValidChild Content parent
        => ChildHXML parent
between = Tag_Entity Entity.between

-- | The not equivalent to HTML entity ('≭').
notEquivalentTo :: ValidChild Content parent
                => ChildHXML parent
notEquivalentTo = Tag_Entity Entity.notEquivalentTo

-- | The not less-than HTML entity ('≮').
notLessThan :: ValidChild Content parent
            => ChildHXML parent
notLessThan = Tag_Entity Entity.notLessThan

-- | The not greater-than HTML entity ('≯').
notGreaterThan :: ValidChild Content parent
               => ChildHXML parent
notGreaterThan = Tag_Entity Entity.notGreaterThan

-- | The neither less-than nor equal to HTML entity ('≰').
neitherLessThanNorEqualTo :: ValidChild Content parent
                          => ChildHXML parent
neitherLessThanNorEqualTo = Tag_Entity Entity.neitherLessThanNorEqualTo

-- | The neither greater-than nor equal to HTML entity ('≱').
neitherGreaterThanNorEqualTo :: ValidChild Content parent
                             => ChildHXML parent
neitherGreaterThanNorEqualTo = Tag_Entity Entity.neitherGreaterThanNorEqualTo

-- | The less-than or equivalent to HTML entity ('≲').
lessThanOrEquivalentTo :: ValidChild Content parent
                       => ChildHXML parent
lessThanOrEquivalentTo = Tag_Entity Entity.lessThanOrEquivalentTo

-- | The greater-than or equivalent to HTML entity ('≳').
greaterThanOrEquivalentTo :: ValidChild Content parent
                          => ChildHXML parent
greaterThanOrEquivalentTo = Tag_Entity Entity.greaterThanOrEquivalentTo

-- | The neither less-than nor equivalent to HTML entity ('≴').
neitherLessThanNorEquivalentTo :: ValidChild Content parent
                               => ChildHXML parent
neitherLessThanNorEquivalentTo = Tag_Entity Entity.neitherLessThanNorEquivalentTo

-- | The neither greater-than nor equivalent to HTML entity ('≵').
neitherGreaterThanNorEquivalentTo :: ValidChild Content parent
                                  => ChildHXML parent
neitherGreaterThanNorEquivalentTo = Tag_Entity Entity.neitherGreaterThanNorEquivalentTo

-- | The less-than or greater-than HTML entity ('≶').
lessThanOrGreaterThan :: ValidChild Content parent
                      => ChildHXML parent
lessThanOrGreaterThan = Tag_Entity Entity.lessThanOrGreaterThan

-- | The greater-than or less-than HTML entity ('≷').
greaterThanOrLessThan :: ValidChild Content parent
                      => ChildHXML parent
greaterThanOrLessThan = Tag_Entity Entity.greaterThanOrLessThan

-- | The neither less-than nor greater-than HTML entity ('≸').
neitherLessThanNorGreaterThan :: ValidChild Content parent
                              => ChildHXML parent
neitherLessThanNorGreaterThan = Tag_Entity Entity.neitherLessThanNorGreaterThan

-- | The neither greater-than nor less-than HTML entity ('≹').
neitherGreaterThanNorLessThan :: ValidChild Content parent
                              => ChildHXML parent
neitherGreaterThanNorLessThan = Tag_Entity Entity.neitherGreaterThanNorLessThan

-- | The precedes HTML entity ('≺').
precedes :: ValidChild Content parent
         => ChildHXML parent
precedes = Tag_Entity Entity.precedes

-- | The succeeds HTML entity ('≻').
succeeds :: ValidChild Content parent
         => ChildHXML parent
succeeds = Tag_Entity Entity.succeeds

-- | The precedes or equal to HTML entity ('≼').
precedesOrEqualTo :: ValidChild Content parent
                  => ChildHXML parent
precedesOrEqualTo = Tag_Entity Entity.precedesOrEqualTo

-- | The succeeds or equal to HTML entity ('≽').
succeedsOrEqualTo :: ValidChild Content parent
                  => ChildHXML parent
succeedsOrEqualTo = Tag_Entity Entity.succeedsOrEqualTo

-- | The precedes or equivalent to HTML entity ('≾').
precedesOrEquivalentTo :: ValidChild Content parent
                       => ChildHXML parent
precedesOrEquivalentTo = Tag_Entity Entity.precedesOrEquivalentTo

-- | The succeeds or equivalent to HTML entity ('≿').
succeedsOrEquivalentTo :: ValidChild Content parent
                       => ChildHXML parent
succeedsOrEquivalentTo = Tag_Entity Entity.succeedsOrEquivalentTo

-- | The does not precede HTML entity ('⊀').
doesNotPrecede :: ValidChild Content parent
               => ChildHXML parent
doesNotPrecede = Tag_Entity Entity.doesNotPrecede

-- | The does not succeed HTML entity ('⊁').
doesNotSucceed :: ValidChild Content parent
               => ChildHXML parent
doesNotSucceed = Tag_Entity Entity.doesNotSucceed

-- | The subset of HTML entity ('⊂').
subsetOf :: ValidChild Content parent
         => ChildHXML parent
subsetOf = Tag_Entity Entity.subsetOf

-- | The superset of HTML entity ('⊃').
supersetOf :: ValidChild Content parent
           => ChildHXML parent
supersetOf = Tag_Entity Entity.supersetOf

-- | The not a subset of HTML entity ('⊄').
notASubsetOf :: ValidChild Content parent
             => ChildHXML parent
notASubsetOf = Tag_Entity Entity.notASubsetOf

-- | The not a superset of HTML entity ('⊅').
notASupersetOf :: ValidChild Content parent
               => ChildHXML parent
notASupersetOf = Tag_Entity Entity.notASupersetOf

-- | The subset of or equal to HTML entity ('⊆').
subsetOfOrEqualTo :: ValidChild Content parent
                  => ChildHXML parent
subsetOfOrEqualTo = Tag_Entity Entity.subsetOfOrEqualTo

-- | The superset of or equal to HTML entity ('⊇').
supersetOfOrEqualTo :: ValidChild Content parent
                    => ChildHXML parent
supersetOfOrEqualTo = Tag_Entity Entity.supersetOfOrEqualTo

-- | The neither a subset of nor equal to HTML entity ('⊈').
neitherASubsetOfNorEqualTo :: ValidChild Content parent
                           => ChildHXML parent
neitherASubsetOfNorEqualTo = Tag_Entity Entity.neitherASubsetOfNorEqualTo

-- | The neither a superset of nor equal to HTML entity ('⊉').
neitherASupersetOfNorEqualTo :: ValidChild Content parent
                             => ChildHXML parent
neitherASupersetOfNorEqualTo = Tag_Entity Entity.neitherASupersetOfNorEqualTo

-- | The subset of with not equal to HTML entity ('⊊').
subsetOfWithNotEqualTo :: ValidChild Content parent
                       => ChildHXML parent
subsetOfWithNotEqualTo = Tag_Entity Entity.subsetOfWithNotEqualTo

-- | The superset of with not equal to HTML entity ('⊋').
supersetOfWithNotEqualTo :: ValidChild Content parent
                         => ChildHXML parent
supersetOfWithNotEqualTo = Tag_Entity Entity.supersetOfWithNotEqualTo

-- | The multiset multiplication HTML entity ('⊍').
multisetMultiplication :: ValidChild Content parent
                       => ChildHXML parent
multisetMultiplication = Tag_Entity Entity.multisetMultiplication

-- | The multiset union HTML entity ('⊎').
multisetUnion :: ValidChild Content parent
              => ChildHXML parent
multisetUnion = Tag_Entity Entity.multisetUnion

-- | The square image of HTML entity ('⊏').
squareImageOf :: ValidChild Content parent
              => ChildHXML parent
squareImageOf = Tag_Entity Entity.squareImageOf

-- | The square original of HTML entity ('⊐').
squareOriginalOf :: ValidChild Content parent
                 => ChildHXML parent
squareOriginalOf = Tag_Entity Entity.squareOriginalOf

-- | The square image of or equal to HTML entity ('⊑').
squareImageOfOrEqualTo :: ValidChild Content parent
                       => ChildHXML parent
squareImageOfOrEqualTo = Tag_Entity Entity.squareImageOfOrEqualTo

-- | The square original of or equal to HTML entity ('⊒').
squareOriginalOfOrEqualTo :: ValidChild Content parent
                          => ChildHXML parent
squareOriginalOfOrEqualTo = Tag_Entity Entity.squareOriginalOfOrEqualTo

-- | The square cap HTML entity ('⊓').
squareCap :: ValidChild Content parent
          => ChildHXML parent
squareCap = Tag_Entity Entity.squareCap

-- | The square cup HTML entity ('⊔').
squareCup :: ValidChild Content parent
          => ChildHXML parent
squareCup = Tag_Entity Entity.squareCup

-- | The circled plus HTML entity ('⊕').
circledPlus :: ValidChild Content parent
            => ChildHXML parent
circledPlus = Tag_Entity Entity.circledPlus

-- | The circled minus HTML entity ('⊖').
circledMinus :: ValidChild Content parent
             => ChildHXML parent
circledMinus = Tag_Entity Entity.circledMinus

-- | The circled times HTML entity ('⊗').
circledTimes :: ValidChild Content parent
             => ChildHXML parent
circledTimes = Tag_Entity Entity.circledTimes

-- | The circled division slash HTML entity ('⊘').
circledDivisionSlash :: ValidChild Content parent
                     => ChildHXML parent
circledDivisionSlash = Tag_Entity Entity.circledDivisionSlash

-- | The circled dot operator HTML entity ('⊙').
circledDotOperator :: ValidChild Content parent
                   => ChildHXML parent
circledDotOperator = Tag_Entity Entity.circledDotOperator

-- | The circled ring operator HTML entity ('⊚').
circledRingOperator :: ValidChild Content parent
                    => ChildHXML parent
circledRingOperator = Tag_Entity Entity.circledRingOperator

-- | The circled asterisk operator HTML entity ('⊛').
circledAsteriskOperator :: ValidChild Content parent
                        => ChildHXML parent
circledAsteriskOperator = Tag_Entity Entity.circledAsteriskOperator

-- | The Circled Equals HTML entity ('⊜').
circledEquals :: ValidChild Content parent
              => ChildHXML parent
circledEquals = Tag_Entity Entity.circledEquals

-- | The circled dash HTML entity ('⊝').
circledDash :: ValidChild Content parent
            => ChildHXML parent
circledDash = Tag_Entity Entity.circledDash

-- | The squared plus HTML entity ('⊞').
squaredPlus :: ValidChild Content parent
            => ChildHXML parent
squaredPlus = Tag_Entity Entity.squaredPlus

-- | The squared minus HTML entity ('⊟').
squaredMinus :: ValidChild Content parent
             => ChildHXML parent
squaredMinus = Tag_Entity Entity.squaredMinus

-- | The squared times HTML entity ('⊠').
squaredTimes :: ValidChild Content parent
             => ChildHXML parent
squaredTimes = Tag_Entity Entity.squaredTimes

-- | The squared dot operator HTML entity ('⊡').
squaredDotOperator :: ValidChild Content parent
                   => ChildHXML parent
squaredDotOperator = Tag_Entity Entity.squaredDotOperator

-- | The right tack HTML entity ('⊢').
rightTack :: ValidChild Content parent
          => ChildHXML parent
rightTack = Tag_Entity Entity.rightTack

-- | The left tack HTML entity ('⊣').
leftTack :: ValidChild Content parent
         => ChildHXML parent
leftTack = Tag_Entity Entity.leftTack

-- | The down tack HTML entity ('⊤').
downTack :: ValidChild Content parent
         => ChildHXML parent
downTack = Tag_Entity Entity.downTack

-- | The up tack HTML entity ('⊥').
upTack :: ValidChild Content parent
       => ChildHXML parent
upTack = Tag_Entity Entity.upTack

-- | The models HTML entity ('⊧').
models :: ValidChild Content parent
       => ChildHXML parent
models = Tag_Entity Entity.models

-- | The true HTML entity ('⊨').
true :: ValidChild Content parent
     => ChildHXML parent
true = Tag_Entity Entity.true

-- | The forces HTML entity ('⊩').
forces :: ValidChild Content parent
       => ChildHXML parent
forces = Tag_Entity Entity.forces

-- | The triple vertical bar right turnstile HTML entity ('⊪').
tripleVerticalBarRightTurnstile :: ValidChild Content parent
                                => ChildHXML parent
tripleVerticalBarRightTurnstile = Tag_Entity Entity.tripleVerticalBarRightTurnstile

-- | The double vertical bar double right turnstile HTML entity ('⊫').
doubleVerticalBarDoubleRightTurnstile :: ValidChild Content parent
                                      => ChildHXML parent
doubleVerticalBarDoubleRightTurnstile = Tag_Entity Entity.doubleVerticalBarDoubleRightTurnstile

-- | The does not prove HTML entity ('⊬').
doesNotProve :: ValidChild Content parent
             => ChildHXML parent
doesNotProve = Tag_Entity Entity.doesNotProve

-- | The not true HTML entity ('⊭').
notTrue :: ValidChild Content parent
        => ChildHXML parent
notTrue = Tag_Entity Entity.notTrue

-- | The does not force HTML entity ('⊮').
doesNotForce :: ValidChild Content parent
             => ChildHXML parent
doesNotForce = Tag_Entity Entity.doesNotForce

-- | The negated double vertical bar double right turnstile HTML entity ('⊯').
negatedDoubleVerticalBarDoubleRightTurnstile :: ValidChild Content parent
                                             => ChildHXML parent
negatedDoubleVerticalBarDoubleRightTurnstile = Tag_Entity Entity.negatedDoubleVerticalBarDoubleRightTurnstile

-- | The precedes under relation HTML entity ('⊰').
precedesUnderRelation :: ValidChild Content parent
                      => ChildHXML parent
precedesUnderRelation = Tag_Entity Entity.precedesUnderRelation

-- | The normal subgroup of HTML entity ('⊲').
normalSubgroupOf :: ValidChild Content parent
                 => ChildHXML parent
normalSubgroupOf = Tag_Entity Entity.normalSubgroupOf

-- | The contains as normal subgroup HTML entity ('⊳').
containsAsNormalSubgroup :: ValidChild Content parent
                         => ChildHXML parent
containsAsNormalSubgroup = Tag_Entity Entity.containsAsNormalSubgroup

-- | The normal subgroup of or equal to HTML entity ('⊴').
normalSubgroupOfOrEqualTo :: ValidChild Content parent
                          => ChildHXML parent
normalSubgroupOfOrEqualTo = Tag_Entity Entity.normalSubgroupOfOrEqualTo

-- | The contains as normal subgroup or equal to HTML entity ('⊵').
containsAsNormalSubgroupOrEqualTo :: ValidChild Content parent
                                  => ChildHXML parent
containsAsNormalSubgroupOrEqualTo = Tag_Entity Entity.containsAsNormalSubgroupOrEqualTo

-- | The original of HTML entity ('⊶').
originalOf :: ValidChild Content parent
           => ChildHXML parent
originalOf = Tag_Entity Entity.originalOf

-- | The image of HTML entity ('⊷').
imageOf :: ValidChild Content parent
        => ChildHXML parent
imageOf = Tag_Entity Entity.imageOf

-- | The multimap HTML entity ('⊸').
multimap :: ValidChild Content parent
         => ChildHXML parent
multimap = Tag_Entity Entity.multimap

-- | The hermitian conjugate matrix HTML entity ('⊹').
hermitianConjugateMatrix :: ValidChild Content parent
                         => ChildHXML parent
hermitianConjugateMatrix = Tag_Entity Entity.hermitianConjugateMatrix

-- | The intercalate HTML entity ('⊺').
intercalate :: ValidChild Content parent
            => ChildHXML parent
intercalate = Tag_Entity Entity.intercalate

-- | The xor HTML entity ('⊻').
xor :: ValidChild Content parent
    => ChildHXML parent
xor = Tag_Entity Entity.xor

-- | The nor HTML entity ('⊽').
nor :: ValidChild Content parent
    => ChildHXML parent
nor = Tag_Entity Entity.nor

-- | The right angle with arc HTML entity ('⊾').
rightAngleWithArc :: ValidChild Content parent
                  => ChildHXML parent
rightAngleWithArc = Tag_Entity Entity.rightAngleWithArc

-- | The right triangle HTML entity ('⊿').
rightTriangle :: ValidChild Content parent
              => ChildHXML parent
rightTriangle = Tag_Entity Entity.rightTriangle

-- | The n-ary logical and HTML entity ('⋀').
nAryLogicalAnd :: ValidChild Content parent
               => ChildHXML parent
nAryLogicalAnd = Tag_Entity Entity.nAryLogicalAnd

-- | The n-ary logical or HTML entity ('⋁').
nAryLogicalOr :: ValidChild Content parent
              => ChildHXML parent
nAryLogicalOr = Tag_Entity Entity.nAryLogicalOr

-- | The n-ary intersection HTML entity ('⋂').
nAryIntersection :: ValidChild Content parent
                 => ChildHXML parent
nAryIntersection = Tag_Entity Entity.nAryIntersection

-- | The n-ary union HTML entity ('⋃').
nAryUnion :: ValidChild Content parent
          => ChildHXML parent
nAryUnion = Tag_Entity Entity.nAryUnion

-- | The diamond operator HTML entity ('⋄').
diamondOperator :: ValidChild Content parent
                => ChildHXML parent
diamondOperator = Tag_Entity Entity.diamondOperator

-- | The dot operator HTML entity ('⋅').
dotOperator :: ValidChild Content parent
            => ChildHXML parent
dotOperator = Tag_Entity Entity.dotOperator

-- | The star operator HTML entity ('⋆').
starOperator :: ValidChild Content parent
             => ChildHXML parent
starOperator = Tag_Entity Entity.starOperator

-- | The division times HTML entity ('⋇').
divisionTimes :: ValidChild Content parent
              => ChildHXML parent
divisionTimes = Tag_Entity Entity.divisionTimes

-- | The bowtie HTML entity ('⋈').
bowtie :: ValidChild Content parent
       => ChildHXML parent
bowtie = Tag_Entity Entity.bowtie

-- | The left normal factor semidirect product HTML entity ('⋉').
leftNormalFactorSemidirectProduct :: ValidChild Content parent
                                  => ChildHXML parent
leftNormalFactorSemidirectProduct = Tag_Entity Entity.leftNormalFactorSemidirectProduct

-- | The right normal factor semidirect product HTML entity ('⋊').
rightNormalFactorSemidirectProduct :: ValidChild Content parent
                                   => ChildHXML parent
rightNormalFactorSemidirectProduct = Tag_Entity Entity.rightNormalFactorSemidirectProduct

-- | The left semidirect product HTML entity ('⋋').
leftSemidirectProduct :: ValidChild Content parent
                      => ChildHXML parent
leftSemidirectProduct = Tag_Entity Entity.leftSemidirectProduct

-- | The right semidirect product HTML entity ('⋌').
rightSemidirectProduct :: ValidChild Content parent
                       => ChildHXML parent
rightSemidirectProduct = Tag_Entity Entity.rightSemidirectProduct

-- | The reversed tilde equals HTML entity ('⋍').
reversedTildeEquals :: ValidChild Content parent
                    => ChildHXML parent
reversedTildeEquals = Tag_Entity Entity.reversedTildeEquals

-- | The curly logical or HTML entity ('⋎').
curlyLogicalOr :: ValidChild Content parent
               => ChildHXML parent
curlyLogicalOr = Tag_Entity Entity.curlyLogicalOr

-- | The curly logical and HTML entity ('⋏').
curlyLogicalAnd :: ValidChild Content parent
                => ChildHXML parent
curlyLogicalAnd = Tag_Entity Entity.curlyLogicalAnd

-- | The double subset HTML entity ('⋐').
doubleSubset :: ValidChild Content parent
             => ChildHXML parent
doubleSubset = Tag_Entity Entity.doubleSubset

-- | The double superset HTML entity ('⋑').
doubleSuperset :: ValidChild Content parent
               => ChildHXML parent
doubleSuperset = Tag_Entity Entity.doubleSuperset

-- | The double intersection HTML entity ('⋒').
doubleIntersection :: ValidChild Content parent
                   => ChildHXML parent
doubleIntersection = Tag_Entity Entity.doubleIntersection

-- | The double union HTML entity ('⋓').
doubleUnion :: ValidChild Content parent
            => ChildHXML parent
doubleUnion = Tag_Entity Entity.doubleUnion

-- | The pitchfork HTML entity ('⋔').
pitchfork :: ValidChild Content parent
          => ChildHXML parent
pitchfork = Tag_Entity Entity.pitchfork

-- | The equal and parallel to HTML entity ('⋕').
equalAndParallelTo :: ValidChild Content parent
                   => ChildHXML parent
equalAndParallelTo = Tag_Entity Entity.equalAndParallelTo

-- | The less-than with dot HTML entity ('⋖').
lessThanWithDot :: ValidChild Content parent
                => ChildHXML parent
lessThanWithDot = Tag_Entity Entity.lessThanWithDot

-- | The greater-than with dot HTML entity ('⋗').
greaterThanWithDot :: ValidChild Content parent
                   => ChildHXML parent
greaterThanWithDot = Tag_Entity Entity.greaterThanWithDot

-- | The very much less-than HTML entity ('⋘').
veryMuchLessThan :: ValidChild Content parent
                 => ChildHXML parent
veryMuchLessThan = Tag_Entity Entity.veryMuchLessThan

-- | The very much greater-than HTML entity ('⋙').
veryMuchGreaterThan :: ValidChild Content parent
                    => ChildHXML parent
veryMuchGreaterThan = Tag_Entity Entity.veryMuchGreaterThan

-- | The less-than equal to or greater-than HTML entity ('⋚').
lessThanEqualToOrGreaterThan :: ValidChild Content parent
                             => ChildHXML parent
lessThanEqualToOrGreaterThan = Tag_Entity Entity.lessThanEqualToOrGreaterThan

-- | The greater-than equal to or less-than HTML entity ('⋛').
greaterThanEqualToOrLessThan :: ValidChild Content parent
                             => ChildHXML parent
greaterThanEqualToOrLessThan = Tag_Entity Entity.greaterThanEqualToOrLessThan

-- | The Equal To Or Less-than HTML entity ('⋜').
equalToOrLessThan :: ValidChild Content parent
                  => ChildHXML parent
equalToOrLessThan = Tag_Entity Entity.equalToOrLessThan

-- | The Equal To Or Greater-than HTML entity ('⋝').
equalToOrGreaterThan :: ValidChild Content parent
                     => ChildHXML parent
equalToOrGreaterThan = Tag_Entity Entity.equalToOrGreaterThan

-- | The equal to or precedes HTML entity ('⋞').
equalToOrPrecedes :: ValidChild Content parent
                  => ChildHXML parent
equalToOrPrecedes = Tag_Entity Entity.equalToOrPrecedes

-- | The equal to or succeeds HTML entity ('⋟').
equalToOrSucceeds :: ValidChild Content parent
                  => ChildHXML parent
equalToOrSucceeds = Tag_Entity Entity.equalToOrSucceeds

-- | The does not precede or equal HTML entity ('⋠').
doesNotPrecedeOrEqual :: ValidChild Content parent
                      => ChildHXML parent
doesNotPrecedeOrEqual = Tag_Entity Entity.doesNotPrecedeOrEqual

-- | The does not succeed or equal HTML entity ('⋡').
doesNotSucceedOrEqual :: ValidChild Content parent
                      => ChildHXML parent
doesNotSucceedOrEqual = Tag_Entity Entity.doesNotSucceedOrEqual

-- | The not square image of or equal to HTML entity ('⋢').
notSquareImageOfOrEqualTo :: ValidChild Content parent
                          => ChildHXML parent
notSquareImageOfOrEqualTo = Tag_Entity Entity.notSquareImageOfOrEqualTo

-- | The not square original of or equal to HTML entity ('⋣').
notSquareOriginalOfOrEqualTo :: ValidChild Content parent
                             => ChildHXML parent
notSquareOriginalOfOrEqualTo = Tag_Entity Entity.notSquareOriginalOfOrEqualTo

-- | The Square Image Of Or Not Equal To HTML entity ('⋤').
squareImageOfOrNotEqualTo :: ValidChild Content parent
                          => ChildHXML parent
squareImageOfOrNotEqualTo = Tag_Entity Entity.squareImageOfOrNotEqualTo

-- | The Square Original Of Or Not Equal To HTML entity ('⋥').
squareOriginalOfOrNotEqualTo :: ValidChild Content parent
                             => ChildHXML parent
squareOriginalOfOrNotEqualTo = Tag_Entity Entity.squareOriginalOfOrNotEqualTo

-- | The less-than but not equivalent to HTML entity ('⋦').
lessThanButNotEquivalentTo :: ValidChild Content parent
                           => ChildHXML parent
lessThanButNotEquivalentTo = Tag_Entity Entity.lessThanButNotEquivalentTo

-- | The greater-than but not equivalent to HTML entity ('⋧').
greaterThanButNotEquivalentTo :: ValidChild Content parent
                              => ChildHXML parent
greaterThanButNotEquivalentTo = Tag_Entity Entity.greaterThanButNotEquivalentTo

-- | The precedes but not equivalent to HTML entity ('⋨').
precedesButNotEquivalentTo :: ValidChild Content parent
                           => ChildHXML parent
precedesButNotEquivalentTo = Tag_Entity Entity.precedesButNotEquivalentTo

-- | The succeeds but not equivalent to HTML entity ('⋩').
succeedsButNotEquivalentTo :: ValidChild Content parent
                           => ChildHXML parent
succeedsButNotEquivalentTo = Tag_Entity Entity.succeedsButNotEquivalentTo

-- | The not normal subgroup of HTML entity ('⋪').
notNormalSubgroupOf :: ValidChild Content parent
                    => ChildHXML parent
notNormalSubgroupOf = Tag_Entity Entity.notNormalSubgroupOf

-- | The does not contain as normal subgroup HTML entity ('⋫').
doesNotContainAsNormalSubgroup :: ValidChild Content parent
                               => ChildHXML parent
doesNotContainAsNormalSubgroup = Tag_Entity Entity.doesNotContainAsNormalSubgroup

-- | The not normal subgroup of or equal to HTML entity ('⋬').
notNormalSubgroupOfOrEqualTo :: ValidChild Content parent
                             => ChildHXML parent
notNormalSubgroupOfOrEqualTo = Tag_Entity Entity.notNormalSubgroupOfOrEqualTo

-- | The does not contain as normal subgroup or equal HTML entity ('⋭').
doesNotContainAsNormalSubgroupOrEqual :: ValidChild Content parent
                                      => ChildHXML parent
doesNotContainAsNormalSubgroupOrEqual = Tag_Entity Entity.doesNotContainAsNormalSubgroupOrEqual

-- | The vertical ellipsis HTML entity ('⋮').
verticalEllipsis :: ValidChild Content parent
                 => ChildHXML parent
verticalEllipsis = Tag_Entity Entity.verticalEllipsis

-- | The midline horizontal ellipsis HTML entity ('⋯').
midlineHorizontalEllipsis :: ValidChild Content parent
                          => ChildHXML parent
midlineHorizontalEllipsis = Tag_Entity Entity.midlineHorizontalEllipsis

-- | The up right diagonal ellipsis HTML entity ('⋰').
upRightDiagonalEllipsis :: ValidChild Content parent
                        => ChildHXML parent
upRightDiagonalEllipsis = Tag_Entity Entity.upRightDiagonalEllipsis

-- | The down right diagonal ellipsis HTML entity ('⋱').
downRightDiagonalEllipsis :: ValidChild Content parent
                          => ChildHXML parent
downRightDiagonalEllipsis = Tag_Entity Entity.downRightDiagonalEllipsis

-- | The element of with long horizontal stroke HTML entity ('⋲').
elementOfWithLongHorizontalStroke :: ValidChild Content parent
                                  => ChildHXML parent
elementOfWithLongHorizontalStroke = Tag_Entity Entity.elementOfWithLongHorizontalStroke

-- | The element of with vertical bar at end of horizontal stroke HTML entity ('⋳').
elementOfWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Content parent
                                                => ChildHXML parent
elementOfWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity Entity.elementOfWithVerticalBarAtEndOfHorizontalStroke

-- | The small element of with vertical bar at end of horizontal stroke HTML entity ('⋴').
smallElementOfWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Content parent
                                                     => ChildHXML parent
smallElementOfWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity Entity.smallElementOfWithVerticalBarAtEndOfHorizontalStroke

-- | The element of with dot above HTML entity ('⋵').
elementOfWithDotAbove :: ValidChild Content parent
                      => ChildHXML parent
elementOfWithDotAbove = Tag_Entity Entity.elementOfWithDotAbove

-- | The element of with overbar HTML entity ('⋶').
elementOfWithOverbar :: ValidChild Content parent
                     => ChildHXML parent
elementOfWithOverbar = Tag_Entity Entity.elementOfWithOverbar

-- | The small element of with overbar HTML entity ('⋷').
smallElementOfWithOverbar :: ValidChild Content parent
                          => ChildHXML parent
smallElementOfWithOverbar = Tag_Entity Entity.smallElementOfWithOverbar

-- | The element of with two horizontal strokes HTML entity ('⋹').
elementOfWithTwoHorizontalStrokes :: ValidChild Content parent
                                  => ChildHXML parent
elementOfWithTwoHorizontalStrokes = Tag_Entity Entity.elementOfWithTwoHorizontalStrokes

-- | The contains with long horizontal stroke HTML entity ('⋺').
containsWithLongHorizontalStroke :: ValidChild Content parent
                                 => ChildHXML parent
containsWithLongHorizontalStroke = Tag_Entity Entity.containsWithLongHorizontalStroke

-- | The contains with vertical bar at end of horizontal stroke HTML entity ('⋻').
containsWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Content parent
                                               => ChildHXML parent
containsWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity Entity.containsWithVerticalBarAtEndOfHorizontalStroke

-- | The small contains with vertical bar at end of horizontal stroke HTML entity ('⋼').
smallContainsWithVerticalBarAtEndOfHorizontalStroke :: ValidChild Content parent
                                                    => ChildHXML parent
smallContainsWithVerticalBarAtEndOfHorizontalStroke = Tag_Entity Entity.smallContainsWithVerticalBarAtEndOfHorizontalStroke

-- | The contains with overbar HTML entity ('⋽').
containsWithOverbar :: ValidChild Content parent
                    => ChildHXML parent
containsWithOverbar = Tag_Entity Entity.containsWithOverbar

-- | The small contains with overbar HTML entity ('⋾').
smallContainsWithOverbar :: ValidChild Content parent
                         => ChildHXML parent
smallContainsWithOverbar = Tag_Entity Entity.smallContainsWithOverbar

-- | The projective HTML entity ('⌅').
projective :: ValidChild Content parent
           => ChildHXML parent
projective = Tag_Entity Entity.projective

-- | The perspective HTML entity ('⌆').
perspective :: ValidChild Content parent
            => ChildHXML parent
perspective = Tag_Entity Entity.perspective

-- | The left ceiling HTML entity ('⌈').
leftCeiling :: ValidChild Content parent
            => ChildHXML parent
leftCeiling = Tag_Entity Entity.leftCeiling

-- | The right ceiling HTML entity ('⌉').
rightCeiling :: ValidChild Content parent
             => ChildHXML parent
rightCeiling = Tag_Entity Entity.rightCeiling

-- | The left floor HTML entity ('⌊').
leftFloor :: ValidChild Content parent
          => ChildHXML parent
leftFloor = Tag_Entity Entity.leftFloor

-- | The right floor HTML entity ('⌋').
rightFloor :: ValidChild Content parent
           => ChildHXML parent
rightFloor = Tag_Entity Entity.rightFloor

-- | The bottom right crop HTML entity ('⌌').
bottomRightCrop :: ValidChild Content parent
                => ChildHXML parent
bottomRightCrop = Tag_Entity Entity.bottomRightCrop

-- | The bottom left crop HTML entity ('⌍').
bottomLeftCrop :: ValidChild Content parent
               => ChildHXML parent
bottomLeftCrop = Tag_Entity Entity.bottomLeftCrop

-- | The top right crop HTML entity ('⌎').
topRightCrop :: ValidChild Content parent
             => ChildHXML parent
topRightCrop = Tag_Entity Entity.topRightCrop

-- | The top left crop HTML entity ('⌏').
topLeftCrop :: ValidChild Content parent
            => ChildHXML parent
topLeftCrop = Tag_Entity Entity.topLeftCrop

-- | The reversed not sign HTML entity ('⌐').
reversedNotSign :: ValidChild Content parent
                => ChildHXML parent
reversedNotSign = Tag_Entity Entity.reversedNotSign

-- | The arc HTML entity ('⌒').
arc :: ValidChild Content parent
    => ChildHXML parent
arc = Tag_Entity Entity.arc

-- | The segment HTML entity ('⌓').
segment :: ValidChild Content parent
        => ChildHXML parent
segment = Tag_Entity Entity.segment

-- | The telephone recorder HTML entity ('⌕').
telephoneRecorder :: ValidChild Content parent
                  => ChildHXML parent
telephoneRecorder = Tag_Entity Entity.telephoneRecorder

-- | The position indicator HTML entity ('⌖').
positionIndicator :: ValidChild Content parent
                  => ChildHXML parent
positionIndicator = Tag_Entity Entity.positionIndicator

-- | The top left corner HTML entity ('⌜').
topLeftCorner :: ValidChild Content parent
              => ChildHXML parent
topLeftCorner = Tag_Entity Entity.topLeftCorner

-- | The top right corner HTML entity ('⌝').
topRightCorner :: ValidChild Content parent
               => ChildHXML parent
topRightCorner = Tag_Entity Entity.topRightCorner

-- | The bottom left corner HTML entity ('⌞').
bottomLeftCorner :: ValidChild Content parent
                 => ChildHXML parent
bottomLeftCorner = Tag_Entity Entity.bottomLeftCorner

-- | The bottom right corner HTML entity ('⌟').
bottomRightCorner :: ValidChild Content parent
                  => ChildHXML parent
bottomRightCorner = Tag_Entity Entity.bottomRightCorner

-- | The frown HTML entity ('⌢').
frown :: ValidChild Content parent
      => ChildHXML parent
frown = Tag_Entity Entity.frown

-- | The smile HTML entity ('⌣').
smile :: ValidChild Content parent
      => ChildHXML parent
smile = Tag_Entity Entity.smile

-- | The left-pointing angle bracket = bra HTML entity ('〈').
leftPointingAngleBracket :: ValidChild Content parent
                         => ChildHXML parent
leftPointingAngleBracket = Tag_Entity Entity.leftPointingAngleBracket

-- | The right-pointing angle bracket = ket HTML entity ('〉').
rightPointingAngleBracket :: ValidChild Content parent
                          => ChildHXML parent
rightPointingAngleBracket = Tag_Entity Entity.rightPointingAngleBracket

-- | The cylindricity HTML entity ('⌭').
cylindricity :: ValidChild Content parent
             => ChildHXML parent
cylindricity = Tag_Entity Entity.cylindricity

-- | The all around-profile HTML entity ('⌮').
allAroundProfile :: ValidChild Content parent
                 => ChildHXML parent
allAroundProfile = Tag_Entity Entity.allAroundProfile

-- | The apl functional symbol i-beam HTML entity ('⌶').
aplFunctionalSymbolIBeam :: ValidChild Content parent
                         => ChildHXML parent
aplFunctionalSymbolIBeam = Tag_Entity Entity.aplFunctionalSymbolIBeam

-- | The Apl Functional Symbol Quad Equal HTML entity ('⌸').
aplFunctionalSymbolQuadEqual :: ValidChild Content parent
                             => ChildHXML parent
aplFunctionalSymbolQuadEqual = Tag_Entity Entity.aplFunctionalSymbolQuadEqual

-- | The apl functional symbol circle stile HTML entity ('⌽').
aplFunctionalSymbolCircleStile :: ValidChild Content parent
                               => ChildHXML parent
aplFunctionalSymbolCircleStile = Tag_Entity Entity.aplFunctionalSymbolCircleStile

-- | The apl functional symbol slash bar HTML entity ('⌿').
aplFunctionalSymbolSlashBar :: ValidChild Content parent
                            => ChildHXML parent
aplFunctionalSymbolSlashBar = Tag_Entity Entity.aplFunctionalSymbolSlashBar

-- | The Apl Functional Symbol Quad Less-than HTML entity ('⍃').
aplFunctionalSymbolQuadLessThan :: ValidChild Content parent
                                => ChildHXML parent
aplFunctionalSymbolQuadLessThan = Tag_Entity Entity.aplFunctionalSymbolQuadLessThan

-- | The Apl Functional Symbol Quad Greater-than HTML entity ('⍄').
aplFunctionalSymbolQuadGreaterThan :: ValidChild Content parent
                                   => ChildHXML parent
aplFunctionalSymbolQuadGreaterThan = Tag_Entity Entity.aplFunctionalSymbolQuadGreaterThan

-- | The Apl Functional Symbol Delta Stile HTML entity ('⍋').
aplFunctionalSymbolDeltaStile :: ValidChild Content parent
                              => ChildHXML parent
aplFunctionalSymbolDeltaStile = Tag_Entity Entity.aplFunctionalSymbolDeltaStile

-- | The Apl Functional Symbol Quad Delta HTML entity ('⍍').
aplFunctionalSymbolQuadDelta :: ValidChild Content parent
                             => ChildHXML parent
aplFunctionalSymbolQuadDelta = Tag_Entity Entity.aplFunctionalSymbolQuadDelta

-- | The Apl Functional Symbol Delta Underbar HTML entity ('⍙').
aplFunctionalSymbolDeltaUnderbar :: ValidChild Content parent
                                 => ChildHXML parent
aplFunctionalSymbolDeltaUnderbar = Tag_Entity Entity.aplFunctionalSymbolDeltaUnderbar

-- | The Apl Functional Symbol Greater-than Diaeresis HTML entity ('⍩').
aplFunctionalSymbolGreaterThanDiaeresis :: ValidChild Content parent
                                        => ChildHXML parent
aplFunctionalSymbolGreaterThanDiaeresis = Tag_Entity Entity.aplFunctionalSymbolGreaterThanDiaeresis

-- | The Apl Functional Symbol Quad Not Equal HTML entity ('⍯').
aplFunctionalSymbolQuadNotEqual :: ValidChild Content parent
                                => ChildHXML parent
aplFunctionalSymbolQuadNotEqual = Tag_Entity Entity.aplFunctionalSymbolQuadNotEqual

-- | The right angle with downwards zigzag arrow HTML entity ('⍼').
rightAngleWithDownwardsZigzagArrow :: ValidChild Content parent
                                   => ChildHXML parent
rightAngleWithDownwardsZigzagArrow = Tag_Entity Entity.rightAngleWithDownwardsZigzagArrow

-- | The upper left or lower right curly bracket section HTML entity ('⎰').
upperLeftOrLowerRightCurlyBracketSection :: ValidChild Content parent
                                         => ChildHXML parent
upperLeftOrLowerRightCurlyBracketSection = Tag_Entity Entity.upperLeftOrLowerRightCurlyBracketSection

-- | The upper right or lower left curly bracket section HTML entity ('⎱').
upperRightOrLowerLeftCurlyBracketSection :: ValidChild Content parent
                                         => ChildHXML parent
upperRightOrLowerLeftCurlyBracketSection = Tag_Entity Entity.upperRightOrLowerLeftCurlyBracketSection

-- | The Summation Top HTML entity ('⎲').
summationTop :: ValidChild Content parent
             => ChildHXML parent
summationTop = Tag_Entity Entity.summationTop

-- | The Summation Bottom HTML entity ('⎳').
summationBottom :: ValidChild Content parent
                => ChildHXML parent
summationBottom = Tag_Entity Entity.summationBottom

-- | The top square bracket HTML entity ('⎴').
topSquareBracket :: ValidChild Content parent
                 => ChildHXML parent
topSquareBracket = Tag_Entity Entity.topSquareBracket

-- | The bottom square bracket HTML entity ('⎵').
bottomSquareBracket :: ValidChild Content parent
                    => ChildHXML parent
bottomSquareBracket = Tag_Entity Entity.bottomSquareBracket

-- | The bottom square bracket over top square bracket HTML entity ('⎶').
bottomSquareBracketOverTopSquareBracket :: ValidChild Content parent
                                        => ChildHXML parent
bottomSquareBracketOverTopSquareBracket = Tag_Entity Entity.bottomSquareBracketOverTopSquareBracket

-- | The top parenthesis HTML entity ('⏜').
topParenthesis :: ValidChild Content parent
               => ChildHXML parent
topParenthesis = Tag_Entity Entity.topParenthesis

-- | The bottom parenthesis HTML entity ('⏝').
bottomParenthesis :: ValidChild Content parent
                  => ChildHXML parent
bottomParenthesis = Tag_Entity Entity.bottomParenthesis

-- | The top curly bracket HTML entity ('⏞').
topCurlyBracket :: ValidChild Content parent
                => ChildHXML parent
topCurlyBracket = Tag_Entity Entity.topCurlyBracket

-- | The bottom curly bracket HTML entity ('⏟').
bottomCurlyBracket :: ValidChild Content parent
                   => ChildHXML parent
bottomCurlyBracket = Tag_Entity Entity.bottomCurlyBracket

-- | The white trapezium HTML entity ('⏢').
whiteTrapezium :: ValidChild Content parent
               => ChildHXML parent
whiteTrapezium = Tag_Entity Entity.whiteTrapezium

-- | The electrical intersection HTML entity ('⏧').
electricalIntersection :: ValidChild Content parent
                       => ChildHXML parent
electricalIntersection = Tag_Entity Entity.electricalIntersection

-- | The open box HTML entity ('␣').
openBox :: ValidChild Content parent
        => ChildHXML parent
openBox = Tag_Entity Entity.openBox

-- | The circled latin capital letter s HTML entity ('Ⓢ').
circledLatinCapitalLetterS :: ValidChild Content parent
                           => ChildHXML parent
circledLatinCapitalLetterS = Tag_Entity Entity.circledLatinCapitalLetterS

-- | The box drawings light horizontal HTML entity ('─').
boxDrawingsLightHorizontal :: ValidChild Content parent
                           => ChildHXML parent
boxDrawingsLightHorizontal = Tag_Entity Entity.boxDrawingsLightHorizontal

-- | The box drawings light vertical HTML entity ('│').
boxDrawingsLightVertical :: ValidChild Content parent
                         => ChildHXML parent
boxDrawingsLightVertical = Tag_Entity Entity.boxDrawingsLightVertical

-- | The box drawings light down and right HTML entity ('┌').
boxDrawingsLightDownAndRight :: ValidChild Content parent
                             => ChildHXML parent
boxDrawingsLightDownAndRight = Tag_Entity Entity.boxDrawingsLightDownAndRight

-- | The box drawings light down and left HTML entity ('┐').
boxDrawingsLightDownAndLeft :: ValidChild Content parent
                            => ChildHXML parent
boxDrawingsLightDownAndLeft = Tag_Entity Entity.boxDrawingsLightDownAndLeft

-- | The box drawings light up and right HTML entity ('└').
boxDrawingsLightUpAndRight :: ValidChild Content parent
                           => ChildHXML parent
boxDrawingsLightUpAndRight = Tag_Entity Entity.boxDrawingsLightUpAndRight

-- | The box drawings light up and left HTML entity ('┘').
boxDrawingsLightUpAndLeft :: ValidChild Content parent
                          => ChildHXML parent
boxDrawingsLightUpAndLeft = Tag_Entity Entity.boxDrawingsLightUpAndLeft

-- | The box drawings light vertical and right HTML entity ('├').
boxDrawingsLightVerticalAndRight :: ValidChild Content parent
                                 => ChildHXML parent
boxDrawingsLightVerticalAndRight = Tag_Entity Entity.boxDrawingsLightVerticalAndRight

-- | The box drawings light vertical and left HTML entity ('┤').
boxDrawingsLightVerticalAndLeft :: ValidChild Content parent
                                => ChildHXML parent
boxDrawingsLightVerticalAndLeft = Tag_Entity Entity.boxDrawingsLightVerticalAndLeft

-- | The box drawings light down and horizontal HTML entity ('┬').
boxDrawingsLightDownAndHorizontal :: ValidChild Content parent
                                  => ChildHXML parent
boxDrawingsLightDownAndHorizontal = Tag_Entity Entity.boxDrawingsLightDownAndHorizontal

-- | The box drawings light up and horizontal HTML entity ('┴').
boxDrawingsLightUpAndHorizontal :: ValidChild Content parent
                                => ChildHXML parent
boxDrawingsLightUpAndHorizontal = Tag_Entity Entity.boxDrawingsLightUpAndHorizontal

-- | The box drawings light vertical and horizontal HTML entity ('┼').
boxDrawingsLightVerticalAndHorizontal :: ValidChild Content parent
                                      => ChildHXML parent
boxDrawingsLightVerticalAndHorizontal = Tag_Entity Entity.boxDrawingsLightVerticalAndHorizontal

-- | The box drawings double horizontal HTML entity ('═').
boxDrawingsDoubleHorizontal :: ValidChild Content parent
                            => ChildHXML parent
boxDrawingsDoubleHorizontal = Tag_Entity Entity.boxDrawingsDoubleHorizontal

-- | The box drawings double vertical HTML entity ('║').
boxDrawingsDoubleVertical :: ValidChild Content parent
                          => ChildHXML parent
boxDrawingsDoubleVertical = Tag_Entity Entity.boxDrawingsDoubleVertical

-- | The box drawings down single and right double HTML entity ('╒').
boxDrawingsDownSingleAndRightDouble :: ValidChild Content parent
                                    => ChildHXML parent
boxDrawingsDownSingleAndRightDouble = Tag_Entity Entity.boxDrawingsDownSingleAndRightDouble

-- | The box drawings down double and right single HTML entity ('╓').
boxDrawingsDownDoubleAndRightSingle :: ValidChild Content parent
                                    => ChildHXML parent
boxDrawingsDownDoubleAndRightSingle = Tag_Entity Entity.boxDrawingsDownDoubleAndRightSingle

-- | The box drawings double down and right HTML entity ('╔').
boxDrawingsDoubleDownAndRight :: ValidChild Content parent
                              => ChildHXML parent
boxDrawingsDoubleDownAndRight = Tag_Entity Entity.boxDrawingsDoubleDownAndRight

-- | The box drawings down single and left double HTML entity ('╕').
boxDrawingsDownSingleAndLeftDouble :: ValidChild Content parent
                                   => ChildHXML parent
boxDrawingsDownSingleAndLeftDouble = Tag_Entity Entity.boxDrawingsDownSingleAndLeftDouble

-- | The box drawings down double and left single HTML entity ('╖').
boxDrawingsDownDoubleAndLeftSingle :: ValidChild Content parent
                                   => ChildHXML parent
boxDrawingsDownDoubleAndLeftSingle = Tag_Entity Entity.boxDrawingsDownDoubleAndLeftSingle

-- | The box drawings double down and left HTML entity ('╗').
boxDrawingsDoubleDownAndLeft :: ValidChild Content parent
                             => ChildHXML parent
boxDrawingsDoubleDownAndLeft = Tag_Entity Entity.boxDrawingsDoubleDownAndLeft

-- | The box drawings up single and right double HTML entity ('╘').
boxDrawingsUpSingleAndRightDouble :: ValidChild Content parent
                                  => ChildHXML parent
boxDrawingsUpSingleAndRightDouble = Tag_Entity Entity.boxDrawingsUpSingleAndRightDouble

-- | The box drawings up double and right single HTML entity ('╙').
boxDrawingsUpDoubleAndRightSingle :: ValidChild Content parent
                                  => ChildHXML parent
boxDrawingsUpDoubleAndRightSingle = Tag_Entity Entity.boxDrawingsUpDoubleAndRightSingle

-- | The box drawings double up and right HTML entity ('╚').
boxDrawingsDoubleUpAndRight :: ValidChild Content parent
                            => ChildHXML parent
boxDrawingsDoubleUpAndRight = Tag_Entity Entity.boxDrawingsDoubleUpAndRight

-- | The box drawings up single and left double HTML entity ('╛').
boxDrawingsUpSingleAndLeftDouble :: ValidChild Content parent
                                 => ChildHXML parent
boxDrawingsUpSingleAndLeftDouble = Tag_Entity Entity.boxDrawingsUpSingleAndLeftDouble

-- | The box drawings up double and left single HTML entity ('╜').
boxDrawingsUpDoubleAndLeftSingle :: ValidChild Content parent
                                 => ChildHXML parent
boxDrawingsUpDoubleAndLeftSingle = Tag_Entity Entity.boxDrawingsUpDoubleAndLeftSingle

-- | The box drawings double up and left HTML entity ('╝').
boxDrawingsDoubleUpAndLeft :: ValidChild Content parent
                           => ChildHXML parent
boxDrawingsDoubleUpAndLeft = Tag_Entity Entity.boxDrawingsDoubleUpAndLeft

-- | The box drawings vertical single and right double HTML entity ('╞').
boxDrawingsVerticalSingleAndRightDouble :: ValidChild Content parent
                                        => ChildHXML parent
boxDrawingsVerticalSingleAndRightDouble = Tag_Entity Entity.boxDrawingsVerticalSingleAndRightDouble

-- | The box drawings vertical double and right single HTML entity ('╟').
boxDrawingsVerticalDoubleAndRightSingle :: ValidChild Content parent
                                        => ChildHXML parent
boxDrawingsVerticalDoubleAndRightSingle = Tag_Entity Entity.boxDrawingsVerticalDoubleAndRightSingle

-- | The box drawings double vertical and right HTML entity ('╠').
boxDrawingsDoubleVerticalAndRight :: ValidChild Content parent
                                  => ChildHXML parent
boxDrawingsDoubleVerticalAndRight = Tag_Entity Entity.boxDrawingsDoubleVerticalAndRight

-- | The box drawings vertical single and left double HTML entity ('╡').
boxDrawingsVerticalSingleAndLeftDouble :: ValidChild Content parent
                                       => ChildHXML parent
boxDrawingsVerticalSingleAndLeftDouble = Tag_Entity Entity.boxDrawingsVerticalSingleAndLeftDouble

-- | The box drawings vertical double and left single HTML entity ('╢').
boxDrawingsVerticalDoubleAndLeftSingle :: ValidChild Content parent
                                       => ChildHXML parent
boxDrawingsVerticalDoubleAndLeftSingle = Tag_Entity Entity.boxDrawingsVerticalDoubleAndLeftSingle

-- | The box drawings double vertical and left HTML entity ('╣').
boxDrawingsDoubleVerticalAndLeft :: ValidChild Content parent
                                 => ChildHXML parent
boxDrawingsDoubleVerticalAndLeft = Tag_Entity Entity.boxDrawingsDoubleVerticalAndLeft

-- | The box drawings down single and horizontal double HTML entity ('╤').
boxDrawingsDownSingleAndHorizontalDouble :: ValidChild Content parent
                                         => ChildHXML parent
boxDrawingsDownSingleAndHorizontalDouble = Tag_Entity Entity.boxDrawingsDownSingleAndHorizontalDouble

-- | The box drawings down double and horizontal single HTML entity ('╥').
boxDrawingsDownDoubleAndHorizontalSingle :: ValidChild Content parent
                                         => ChildHXML parent
boxDrawingsDownDoubleAndHorizontalSingle = Tag_Entity Entity.boxDrawingsDownDoubleAndHorizontalSingle

-- | The box drawings double down and horizontal HTML entity ('╦').
boxDrawingsDoubleDownAndHorizontal :: ValidChild Content parent
                                   => ChildHXML parent
boxDrawingsDoubleDownAndHorizontal = Tag_Entity Entity.boxDrawingsDoubleDownAndHorizontal

-- | The box drawings up single and horizontal double HTML entity ('╧').
boxDrawingsUpSingleAndHorizontalDouble :: ValidChild Content parent
                                       => ChildHXML parent
boxDrawingsUpSingleAndHorizontalDouble = Tag_Entity Entity.boxDrawingsUpSingleAndHorizontalDouble

-- | The box drawings up double and horizontal single HTML entity ('╨').
boxDrawingsUpDoubleAndHorizontalSingle :: ValidChild Content parent
                                       => ChildHXML parent
boxDrawingsUpDoubleAndHorizontalSingle = Tag_Entity Entity.boxDrawingsUpDoubleAndHorizontalSingle

-- | The box drawings double up and horizontal HTML entity ('╩').
boxDrawingsDoubleUpAndHorizontal :: ValidChild Content parent
                                 => ChildHXML parent
boxDrawingsDoubleUpAndHorizontal = Tag_Entity Entity.boxDrawingsDoubleUpAndHorizontal

-- | The box drawings vertical single and horizontal double HTML entity ('╪').
boxDrawingsVerticalSingleAndHorizontalDouble :: ValidChild Content parent
                                             => ChildHXML parent
boxDrawingsVerticalSingleAndHorizontalDouble = Tag_Entity Entity.boxDrawingsVerticalSingleAndHorizontalDouble

-- | The box drawings vertical double and horizontal single HTML entity ('╫').
boxDrawingsVerticalDoubleAndHorizontalSingle :: ValidChild Content parent
                                             => ChildHXML parent
boxDrawingsVerticalDoubleAndHorizontalSingle = Tag_Entity Entity.boxDrawingsVerticalDoubleAndHorizontalSingle

-- | The box drawings double vertical and horizontal HTML entity ('╬').
boxDrawingsDoubleVerticalAndHorizontal :: ValidChild Content parent
                                       => ChildHXML parent
boxDrawingsDoubleVerticalAndHorizontal = Tag_Entity Entity.boxDrawingsDoubleVerticalAndHorizontal

-- | The upper half block HTML entity ('▀').
upperHalfBlock :: ValidChild Content parent
               => ChildHXML parent
upperHalfBlock = Tag_Entity Entity.upperHalfBlock

-- | The lower half block HTML entity ('▄').
lowerHalfBlock :: ValidChild Content parent
               => ChildHXML parent
lowerHalfBlock = Tag_Entity Entity.lowerHalfBlock

-- | The full block HTML entity ('█').
fullBlock :: ValidChild Content parent
          => ChildHXML parent
fullBlock = Tag_Entity Entity.fullBlock

-- | The light shade HTML entity ('░').
lightShade :: ValidChild Content parent
           => ChildHXML parent
lightShade = Tag_Entity Entity.lightShade

-- | The medium shade HTML entity ('▒').
mediumShade :: ValidChild Content parent
            => ChildHXML parent
mediumShade = Tag_Entity Entity.mediumShade

-- | The dark shade HTML entity ('▓').
darkShade :: ValidChild Content parent
          => ChildHXML parent
darkShade = Tag_Entity Entity.darkShade

-- | The black square HTML entity ('■').
blackSquare :: ValidChild Content parent
            => ChildHXML parent
blackSquare = Tag_Entity Entity.blackSquare

-- | The white square HTML entity ('□').
whiteSquare :: ValidChild Content parent
            => ChildHXML parent
whiteSquare = Tag_Entity Entity.whiteSquare

-- | The white square with rounded corners HTML entity ('▢').
whiteSquareWithRoundedCorners :: ValidChild Content parent
                              => ChildHXML parent
whiteSquareWithRoundedCorners = Tag_Entity Entity.whiteSquareWithRoundedCorners

-- | The white square containing black small square HTML entity ('▣').
whiteSquareContainingBlackSmallSquare :: ValidChild Content parent
                                      => ChildHXML parent
whiteSquareContainingBlackSmallSquare = Tag_Entity Entity.whiteSquareContainingBlackSmallSquare

-- | The square with horizontal fill HTML entity ('▤').
squareWithHorizontalFill :: ValidChild Content parent
                         => ChildHXML parent
squareWithHorizontalFill = Tag_Entity Entity.squareWithHorizontalFill

-- | The square with vertical fill HTML entity ('▥').
squareWithVerticalFill :: ValidChild Content parent
                       => ChildHXML parent
squareWithVerticalFill = Tag_Entity Entity.squareWithVerticalFill

-- | The square with orthogonal crosshatch fill HTML entity ('▦').
squareWithOrthogonalCrosshatchFill :: ValidChild Content parent
                                   => ChildHXML parent
squareWithOrthogonalCrosshatchFill = Tag_Entity Entity.squareWithOrthogonalCrosshatchFill

-- | The square with upper left to lower right fill HTML entity ('▧').
squareWithUpperLeftToLowerRightFill :: ValidChild Content parent
                                    => ChildHXML parent
squareWithUpperLeftToLowerRightFill = Tag_Entity Entity.squareWithUpperLeftToLowerRightFill

-- | The square with upper right to lower left fill HTML entity ('▨').
squareWithUpperRightToLowerLeftFill :: ValidChild Content parent
                                    => ChildHXML parent
squareWithUpperRightToLowerLeftFill = Tag_Entity Entity.squareWithUpperRightToLowerLeftFill

-- | The square with diagonal crosshatch fill HTML entity ('▩').
squareWithDiagonalCrosshatchFill :: ValidChild Content parent
                                 => ChildHXML parent
squareWithDiagonalCrosshatchFill = Tag_Entity Entity.squareWithDiagonalCrosshatchFill

-- | The black small square HTML entity ('▪').
blackSmallSquare :: ValidChild Content parent
                 => ChildHXML parent
blackSmallSquare = Tag_Entity Entity.blackSmallSquare

-- | The white small square HTML entity ('▫').
whiteSmallSquare :: ValidChild Content parent
                 => ChildHXML parent
whiteSmallSquare = Tag_Entity Entity.whiteSmallSquare

-- | The Black Rectangle HTML entity ('▬').
blackRectangle :: ValidChild Content parent
               => ChildHXML parent
blackRectangle = Tag_Entity Entity.blackRectangle

-- | The white rectangle HTML entity ('▭').
whiteRectangle :: ValidChild Content parent
               => ChildHXML parent
whiteRectangle = Tag_Entity Entity.whiteRectangle

-- | The black vertical rectangle HTML entity ('▮').
blackVerticalRectangle :: ValidChild Content parent
                       => ChildHXML parent
blackVerticalRectangle = Tag_Entity Entity.blackVerticalRectangle

-- | The White Vertical Rectangle HTML entity ('▯').
whiteVerticalRectangle :: ValidChild Content parent
                       => ChildHXML parent
whiteVerticalRectangle = Tag_Entity Entity.whiteVerticalRectangle

-- | The Black Parallelogram HTML entity ('▰').
blackParallelogram :: ValidChild Content parent
                   => ChildHXML parent
blackParallelogram = Tag_Entity Entity.blackParallelogram

-- | The white parallelogram HTML entity ('▱').
whiteParallelogram :: ValidChild Content parent
                   => ChildHXML parent
whiteParallelogram = Tag_Entity Entity.whiteParallelogram

-- | The Black Up-pointing Triangle HTML entity ('▲').
blackUpPointingTriangle :: ValidChild Content parent
                        => ChildHXML parent
blackUpPointingTriangle = Tag_Entity Entity.blackUpPointingTriangle

-- | The white up-pointing triangle HTML entity ('△').
whiteUpPointingTriangle :: ValidChild Content parent
                        => ChildHXML parent
whiteUpPointingTriangle = Tag_Entity Entity.whiteUpPointingTriangle

-- | The black up-pointing small triangle HTML entity ('▴').
blackUpPointingSmallTriangle :: ValidChild Content parent
                             => ChildHXML parent
blackUpPointingSmallTriangle = Tag_Entity Entity.blackUpPointingSmallTriangle

-- | The white up-pointing small triangle HTML entity ('▵').
whiteUpPointingSmallTriangle :: ValidChild Content parent
                             => ChildHXML parent
whiteUpPointingSmallTriangle = Tag_Entity Entity.whiteUpPointingSmallTriangle

-- | The Black Right-pointing Triangle HTML entity ('▶').
blackRightPointingTriangle :: ValidChild Content parent
                           => ChildHXML parent
blackRightPointingTriangle = Tag_Entity Entity.blackRightPointingTriangle

-- | The White Right-pointing Triangle HTML entity ('▷').
whiteRightPointingTriangle :: ValidChild Content parent
                           => ChildHXML parent
whiteRightPointingTriangle = Tag_Entity Entity.whiteRightPointingTriangle

-- | The black right-pointing small triangle HTML entity ('▸').
blackRightPointingSmallTriangle :: ValidChild Content parent
                                => ChildHXML parent
blackRightPointingSmallTriangle = Tag_Entity Entity.blackRightPointingSmallTriangle

-- | The white right-pointing small triangle HTML entity ('▹').
whiteRightPointingSmallTriangle :: ValidChild Content parent
                                => ChildHXML parent
whiteRightPointingSmallTriangle = Tag_Entity Entity.whiteRightPointingSmallTriangle

-- | The Black Down-pointing Triangle HTML entity ('▼').
blackDownPointingTriangle :: ValidChild Content parent
                          => ChildHXML parent
blackDownPointingTriangle = Tag_Entity Entity.blackDownPointingTriangle

-- | The white down-pointing triangle HTML entity ('▽').
whiteDownPointingTriangle :: ValidChild Content parent
                          => ChildHXML parent
whiteDownPointingTriangle = Tag_Entity Entity.whiteDownPointingTriangle

-- | The black down-pointing small triangle HTML entity ('▾').
blackDownPointingSmallTriangle :: ValidChild Content parent
                               => ChildHXML parent
blackDownPointingSmallTriangle = Tag_Entity Entity.blackDownPointingSmallTriangle

-- | The white down-pointing small triangle HTML entity ('▿').
whiteDownPointingSmallTriangle :: ValidChild Content parent
                               => ChildHXML parent
whiteDownPointingSmallTriangle = Tag_Entity Entity.whiteDownPointingSmallTriangle

-- | The Black Left-pointing Triangle HTML entity ('◀').
blackLeftPointingTriangle :: ValidChild Content parent
                          => ChildHXML parent
blackLeftPointingTriangle = Tag_Entity Entity.blackLeftPointingTriangle

-- | The White Left-pointing Triangle HTML entity ('◁').
whiteLeftPointingTriangle :: ValidChild Content parent
                          => ChildHXML parent
whiteLeftPointingTriangle = Tag_Entity Entity.whiteLeftPointingTriangle

-- | The black left-pointing small triangle HTML entity ('◂').
blackLeftPointingSmallTriangle :: ValidChild Content parent
                               => ChildHXML parent
blackLeftPointingSmallTriangle = Tag_Entity Entity.blackLeftPointingSmallTriangle

-- | The white left-pointing small triangle HTML entity ('◃').
whiteLeftPointingSmallTriangle :: ValidChild Content parent
                               => ChildHXML parent
whiteLeftPointingSmallTriangle = Tag_Entity Entity.whiteLeftPointingSmallTriangle

-- | The Black Diamond HTML entity ('◆').
blackDiamond :: ValidChild Content parent
             => ChildHXML parent
blackDiamond = Tag_Entity Entity.blackDiamond

-- | The White Diamond HTML entity ('◇').
whiteDiamond :: ValidChild Content parent
             => ChildHXML parent
whiteDiamond = Tag_Entity Entity.whiteDiamond

-- | The White Diamond Containing Black Small Diamond HTML entity ('◈').
whiteDiamondContainingBlackSmallDiamond :: ValidChild Content parent
                                        => ChildHXML parent
whiteDiamondContainingBlackSmallDiamond = Tag_Entity Entity.whiteDiamondContainingBlackSmallDiamond

-- | The lozenge HTML entity ('◊').
lozenge :: ValidChild Content parent
        => ChildHXML parent
lozenge = Tag_Entity Entity.lozenge

-- | The white circle HTML entity ('○').
whiteCircle :: ValidChild Content parent
            => ChildHXML parent
whiteCircle = Tag_Entity Entity.whiteCircle

-- | The Dotted Circle HTML entity ('◌').
dottedCircle :: ValidChild Content parent
             => ChildHXML parent
dottedCircle = Tag_Entity Entity.dottedCircle

-- | The Circle With Vertical Fill HTML entity ('◍').
circleWithVerticalFill :: ValidChild Content parent
                       => ChildHXML parent
circleWithVerticalFill = Tag_Entity Entity.circleWithVerticalFill

-- | The Black Circle HTML entity ('●').
blackCircle :: ValidChild Content parent
            => ChildHXML parent
blackCircle = Tag_Entity Entity.blackCircle

-- | The Circle With Left Half Black HTML entity ('◐').
circleWithLeftHalfBlack :: ValidChild Content parent
                        => ChildHXML parent
circleWithLeftHalfBlack = Tag_Entity Entity.circleWithLeftHalfBlack

-- | The Circle With Right Half Black HTML entity ('◑').
circleWithRightHalfBlack :: ValidChild Content parent
                         => ChildHXML parent
circleWithRightHalfBlack = Tag_Entity Entity.circleWithRightHalfBlack

-- | The Circle With Lower Half Black HTML entity ('◒').
circleWithLowerHalfBlack :: ValidChild Content parent
                         => ChildHXML parent
circleWithLowerHalfBlack = Tag_Entity Entity.circleWithLowerHalfBlack

-- | The Circle With Upper Half Black HTML entity ('◓').
circleWithUpperHalfBlack :: ValidChild Content parent
                         => ChildHXML parent
circleWithUpperHalfBlack = Tag_Entity Entity.circleWithUpperHalfBlack

-- | The Circle With Upper Right Quadrant Black HTML entity ('◔').
circleWithUpperRightQuadrantBlack :: ValidChild Content parent
                                  => ChildHXML parent
circleWithUpperRightQuadrantBlack = Tag_Entity Entity.circleWithUpperRightQuadrantBlack

-- | The Circle With All But Upper Left Quadrant Black HTML entity ('◕').
circleWithAllButUpperLeftQuadrantBlack :: ValidChild Content parent
                                       => ChildHXML parent
circleWithAllButUpperLeftQuadrantBlack = Tag_Entity Entity.circleWithAllButUpperLeftQuadrantBlack

-- | The Left Half Black Circle HTML entity ('◖').
leftHalfBlackCircle :: ValidChild Content parent
                    => ChildHXML parent
leftHalfBlackCircle = Tag_Entity Entity.leftHalfBlackCircle

-- | The Right Half Black Circle HTML entity ('◗').
rightHalfBlackCircle :: ValidChild Content parent
                     => ChildHXML parent
rightHalfBlackCircle = Tag_Entity Entity.rightHalfBlackCircle

-- | The Inverse White Circle HTML entity ('◙').
inverseWhiteCircle :: ValidChild Content parent
                   => ChildHXML parent
inverseWhiteCircle = Tag_Entity Entity.inverseWhiteCircle

-- | The Upper Half Inverse White Circle HTML entity ('◚').
upperHalfInverseWhiteCircle :: ValidChild Content parent
                            => ChildHXML parent
upperHalfInverseWhiteCircle = Tag_Entity Entity.upperHalfInverseWhiteCircle

-- | The Lower Half Inverse White Circle HTML entity ('◛').
lowerHalfInverseWhiteCircle :: ValidChild Content parent
                            => ChildHXML parent
lowerHalfInverseWhiteCircle = Tag_Entity Entity.lowerHalfInverseWhiteCircle

-- | The Upper Left Quadrant Circular Arc HTML entity ('◜').
upperLeftQuadrantCircularArc :: ValidChild Content parent
                             => ChildHXML parent
upperLeftQuadrantCircularArc = Tag_Entity Entity.upperLeftQuadrantCircularArc

-- | The Upper Right Quadrant Circular Arc HTML entity ('◝').
upperRightQuadrantCircularArc :: ValidChild Content parent
                              => ChildHXML parent
upperRightQuadrantCircularArc = Tag_Entity Entity.upperRightQuadrantCircularArc

-- | The Lower Right Quadrant Circular Arc HTML entity ('◞').
lowerRightQuadrantCircularArc :: ValidChild Content parent
                              => ChildHXML parent
lowerRightQuadrantCircularArc = Tag_Entity Entity.lowerRightQuadrantCircularArc

-- | The Lower Left Quadrant Circular Arc HTML entity ('◟').
lowerLeftQuadrantCircularArc :: ValidChild Content parent
                             => ChildHXML parent
lowerLeftQuadrantCircularArc = Tag_Entity Entity.lowerLeftQuadrantCircularArc

-- | The Upper Half Circle HTML entity ('◠').
upperHalfCircle :: ValidChild Content parent
                => ChildHXML parent
upperHalfCircle = Tag_Entity Entity.upperHalfCircle

-- | The Lower Half Circle HTML entity ('◡').
lowerHalfCircle :: ValidChild Content parent
                => ChildHXML parent
lowerHalfCircle = Tag_Entity Entity.lowerHalfCircle

-- | The Black Lower Right Triangle HTML entity ('◢').
blackLowerRightTriangle :: ValidChild Content parent
                        => ChildHXML parent
blackLowerRightTriangle = Tag_Entity Entity.blackLowerRightTriangle

-- | The Black Lower Left Triangle HTML entity ('◣').
blackLowerLeftTriangle :: ValidChild Content parent
                       => ChildHXML parent
blackLowerLeftTriangle = Tag_Entity Entity.blackLowerLeftTriangle

-- | The Black Upper Left Triangle HTML entity ('◤').
blackUpperLeftTriangle :: ValidChild Content parent
                       => ChildHXML parent
blackUpperLeftTriangle = Tag_Entity Entity.blackUpperLeftTriangle

-- | The Black Upper Right Triangle HTML entity ('◥').
blackUpperRightTriangle :: ValidChild Content parent
                        => ChildHXML parent
blackUpperRightTriangle = Tag_Entity Entity.blackUpperRightTriangle

-- | The square with left half black HTML entity ('◧').
squareWithLeftHalfBlack :: ValidChild Content parent
                        => ChildHXML parent
squareWithLeftHalfBlack = Tag_Entity Entity.squareWithLeftHalfBlack

-- | The square with right half black HTML entity ('◨').
squareWithRightHalfBlack :: ValidChild Content parent
                         => ChildHXML parent
squareWithRightHalfBlack = Tag_Entity Entity.squareWithRightHalfBlack

-- | The square with upper left diagonal half black HTML entity ('◩').
squareWithUpperLeftDiagonalHalfBlack :: ValidChild Content parent
                                     => ChildHXML parent
squareWithUpperLeftDiagonalHalfBlack = Tag_Entity Entity.squareWithUpperLeftDiagonalHalfBlack

-- | The square with lower right diagonal half black HTML entity ('◪').
squareWithLowerRightDiagonalHalfBlack :: ValidChild Content parent
                                      => ChildHXML parent
squareWithLowerRightDiagonalHalfBlack = Tag_Entity Entity.squareWithLowerRightDiagonalHalfBlack

-- | The white square with vertical bisecting line HTML entity ('◫').
whiteSquareWithVerticalBisectingLine :: ValidChild Content parent
                                     => ChildHXML parent
whiteSquareWithVerticalBisectingLine = Tag_Entity Entity.whiteSquareWithVerticalBisectingLine

-- | The white up-pointing triangle with dot HTML entity ('◬').
whiteUpPointingTriangleWithDot :: ValidChild Content parent
                               => ChildHXML parent
whiteUpPointingTriangleWithDot = Tag_Entity Entity.whiteUpPointingTriangleWithDot

-- | The Up-pointing Triangle With Left Half Black HTML entity ('◭').
upPointingTriangleWithLeftHalfBlack :: ValidChild Content parent
                                    => ChildHXML parent
upPointingTriangleWithLeftHalfBlack = Tag_Entity Entity.upPointingTriangleWithLeftHalfBlack

-- | The Up-pointing Triangle With Right Half Black HTML entity ('◮').
upPointingTriangleWithRightHalfBlack :: ValidChild Content parent
                                     => ChildHXML parent
upPointingTriangleWithRightHalfBlack = Tag_Entity Entity.upPointingTriangleWithRightHalfBlack

-- | The large circle HTML entity ('◯').
largeCircle :: ValidChild Content parent
            => ChildHXML parent
largeCircle = Tag_Entity Entity.largeCircle

-- | The white square with upper left quadrant HTML entity ('◰').
whiteSquareWithUpperLeftQuadrant :: ValidChild Content parent
                                 => ChildHXML parent
whiteSquareWithUpperLeftQuadrant = Tag_Entity Entity.whiteSquareWithUpperLeftQuadrant

-- | The white square with lower left quadrant HTML entity ('◱').
whiteSquareWithLowerLeftQuadrant :: ValidChild Content parent
                                 => ChildHXML parent
whiteSquareWithLowerLeftQuadrant = Tag_Entity Entity.whiteSquareWithLowerLeftQuadrant

-- | The white square with lower right quadrant HTML entity ('◲').
whiteSquareWithLowerRightQuadrant :: ValidChild Content parent
                                  => ChildHXML parent
whiteSquareWithLowerRightQuadrant = Tag_Entity Entity.whiteSquareWithLowerRightQuadrant

-- | The white square with upper right quadrant HTML entity ('◳').
whiteSquareWithUpperRightQuadrant :: ValidChild Content parent
                                  => ChildHXML parent
whiteSquareWithUpperRightQuadrant = Tag_Entity Entity.whiteSquareWithUpperRightQuadrant

-- | The White Circle With Upper Left Quadrant HTML entity ('◴').
whiteCircleWithUpperLeftQuadrant :: ValidChild Content parent
                                 => ChildHXML parent
whiteCircleWithUpperLeftQuadrant = Tag_Entity Entity.whiteCircleWithUpperLeftQuadrant

-- | The White Circle With Lower Left Quadrant HTML entity ('◵').
whiteCircleWithLowerLeftQuadrant :: ValidChild Content parent
                                 => ChildHXML parent
whiteCircleWithLowerLeftQuadrant = Tag_Entity Entity.whiteCircleWithLowerLeftQuadrant

-- | The White Circle With Lower Right Quadrant HTML entity ('◶').
whiteCircleWithLowerRightQuadrant :: ValidChild Content parent
                                  => ChildHXML parent
whiteCircleWithLowerRightQuadrant = Tag_Entity Entity.whiteCircleWithLowerRightQuadrant

-- | The White Circle With Upper Right Quadrant HTML entity ('◷').
whiteCircleWithUpperRightQuadrant :: ValidChild Content parent
                                  => ChildHXML parent
whiteCircleWithUpperRightQuadrant = Tag_Entity Entity.whiteCircleWithUpperRightQuadrant

-- | The upper left triangle HTML entity ('◸').
upperLeftTriangle :: ValidChild Content parent
                  => ChildHXML parent
upperLeftTriangle = Tag_Entity Entity.upperLeftTriangle

-- | The upper right triangle HTML entity ('◹').
upperRightTriangle :: ValidChild Content parent
                   => ChildHXML parent
upperRightTriangle = Tag_Entity Entity.upperRightTriangle

-- | The lower left triangle HTML entity ('◺').
lowerLeftTriangle :: ValidChild Content parent
                  => ChildHXML parent
lowerLeftTriangle = Tag_Entity Entity.lowerLeftTriangle

-- | The white medium square HTML entity ('◻').
whiteMediumSquare :: ValidChild Content parent
                  => ChildHXML parent
whiteMediumSquare = Tag_Entity Entity.whiteMediumSquare

-- | The black medium square HTML entity ('◼').
blackMediumSquare :: ValidChild Content parent
                  => ChildHXML parent
blackMediumSquare = Tag_Entity Entity.blackMediumSquare

-- | The white medium small square HTML entity ('◽').
whiteMediumSmallSquare :: ValidChild Content parent
                       => ChildHXML parent
whiteMediumSmallSquare = Tag_Entity Entity.whiteMediumSmallSquare

-- | The black medium small square HTML entity ('◾').
blackMediumSmallSquare :: ValidChild Content parent
                       => ChildHXML parent
blackMediumSmallSquare = Tag_Entity Entity.blackMediumSmallSquare

-- | The Lower Right Triangle HTML entity ('◿').
lowerRightTriangle :: ValidChild Content parent
                   => ChildHXML parent
lowerRightTriangle = Tag_Entity Entity.lowerRightTriangle

-- | The black star HTML entity ('★').
blackStar :: ValidChild Content parent
          => ChildHXML parent
blackStar = Tag_Entity Entity.blackStar

-- | The white star HTML entity ('☆').
whiteStar :: ValidChild Content parent
          => ChildHXML parent
whiteStar = Tag_Entity Entity.whiteStar

-- | The black telephone HTML entity ('☎').
blackTelephone :: ValidChild Content parent
               => ChildHXML parent
blackTelephone = Tag_Entity Entity.blackTelephone

-- | The Trigram For Heaven HTML entity ('☰').
trigramForHeaven :: ValidChild Content parent
                 => ChildHXML parent
trigramForHeaven = Tag_Entity Entity.trigramForHeaven

-- | The Trigram For Lake HTML entity ('☱').
trigramForLake :: ValidChild Content parent
               => ChildHXML parent
trigramForLake = Tag_Entity Entity.trigramForLake

-- | The Trigram For Fire HTML entity ('☲').
trigramForFire :: ValidChild Content parent
               => ChildHXML parent
trigramForFire = Tag_Entity Entity.trigramForFire

-- | The Trigram For Thunder HTML entity ('☳').
trigramForThunder :: ValidChild Content parent
                  => ChildHXML parent
trigramForThunder = Tag_Entity Entity.trigramForThunder

-- | The Trigram For Wind HTML entity ('☴').
trigramForWind :: ValidChild Content parent
               => ChildHXML parent
trigramForWind = Tag_Entity Entity.trigramForWind

-- | The Trigram For Water HTML entity ('☵').
trigramForWater :: ValidChild Content parent
                => ChildHXML parent
trigramForWater = Tag_Entity Entity.trigramForWater

-- | The Trigram For Mountain HTML entity ('☶').
trigramForMountain :: ValidChild Content parent
                   => ChildHXML parent
trigramForMountain = Tag_Entity Entity.trigramForMountain

-- | The Trigram For Earth HTML entity ('☷').
trigramForEarth :: ValidChild Content parent
                => ChildHXML parent
trigramForEarth = Tag_Entity Entity.trigramForEarth

-- | The female sign HTML entity ('♀').
femaleSign :: ValidChild Content parent
           => ChildHXML parent
femaleSign = Tag_Entity Entity.femaleSign

-- | The male sign HTML entity ('♂').
maleSign :: ValidChild Content parent
         => ChildHXML parent
maleSign = Tag_Entity Entity.maleSign

-- | The black spade suit HTML entity ('♠').
blackSpadeSuit :: ValidChild Content parent
               => ChildHXML parent
blackSpadeSuit = Tag_Entity Entity.blackSpadeSuit

-- | The White Diamond Suit HTML entity ('♢').
whiteDiamondSuit :: ValidChild Content parent
                 => ChildHXML parent
whiteDiamondSuit = Tag_Entity Entity.whiteDiamondSuit

-- | The black club suit HTML entity ('♣').
blackClubSuit :: ValidChild Content parent
              => ChildHXML parent
blackClubSuit = Tag_Entity Entity.blackClubSuit

-- | The black heart suit HTML entity ('♥').
blackHeartSuit :: ValidChild Content parent
               => ChildHXML parent
blackHeartSuit = Tag_Entity Entity.blackHeartSuit

-- | The black diamond suit HTML entity ('♦').
blackDiamondSuit :: ValidChild Content parent
                 => ChildHXML parent
blackDiamondSuit = Tag_Entity Entity.blackDiamondSuit

-- | The eighth note HTML entity ('♪').
eighthNote :: ValidChild Content parent
           => ChildHXML parent
eighthNote = Tag_Entity Entity.eighthNote

-- | The music flat sign HTML entity ('♭').
musicFlatSign :: ValidChild Content parent
              => ChildHXML parent
musicFlatSign = Tag_Entity Entity.musicFlatSign

-- | The music natural sign HTML entity ('♮').
musicNaturalSign :: ValidChild Content parent
                 => ChildHXML parent
musicNaturalSign = Tag_Entity Entity.musicNaturalSign

-- | The music sharp sign HTML entity ('♯').
musicSharpSign :: ValidChild Content parent
               => ChildHXML parent
musicSharpSign = Tag_Entity Entity.musicSharpSign

-- | The White Circle With Dot Right HTML entity ('⚆').
whiteCircleWithDotRight :: ValidChild Content parent
                        => ChildHXML parent
whiteCircleWithDotRight = Tag_Entity Entity.whiteCircleWithDotRight

-- | The White Circle With Two Dots HTML entity ('⚇').
whiteCircleWithTwoDots :: ValidChild Content parent
                       => ChildHXML parent
whiteCircleWithTwoDots = Tag_Entity Entity.whiteCircleWithTwoDots

-- | The Black Circle With White Dot Right HTML entity ('⚈').
blackCircleWithWhiteDotRight :: ValidChild Content parent
                             => ChildHXML parent
blackCircleWithWhiteDotRight = Tag_Entity Entity.blackCircleWithWhiteDotRight

-- | The Black Circle With Two White Dots HTML entity ('⚉').
blackCircleWithTwoWhiteDots :: ValidChild Content parent
                            => ChildHXML parent
blackCircleWithTwoWhiteDots = Tag_Entity Entity.blackCircleWithTwoWhiteDots

-- | The Medium White Circle HTML entity ('⚪').
mediumWhiteCircle :: ValidChild Content parent
                  => ChildHXML parent
mediumWhiteCircle = Tag_Entity Entity.mediumWhiteCircle

-- | The Medium Black Circle HTML entity ('⚫').
mediumBlackCircle :: ValidChild Content parent
                  => ChildHXML parent
mediumBlackCircle = Tag_Entity Entity.mediumBlackCircle

-- | The Medium Small White Circle HTML entity ('⚬').
mediumSmallWhiteCircle :: ValidChild Content parent
                       => ChildHXML parent
mediumSmallWhiteCircle = Tag_Entity Entity.mediumSmallWhiteCircle

-- | The squared key HTML entity ('⚿').
squaredKey :: ValidChild Content parent
           => ChildHXML parent
squaredKey = Tag_Entity Entity.squaredKey

-- | The white diamond in square HTML entity ('⛋').
whiteDiamondInSquare :: ValidChild Content parent
                     => ChildHXML parent
whiteDiamondInSquare = Tag_Entity Entity.whiteDiamondInSquare

-- | The Heavy White Down-pointing Triangle HTML entity ('⛛').
heavyWhiteDownPointingTriangle :: ValidChild Content parent
                               => ChildHXML parent
heavyWhiteDownPointingTriangle = Tag_Entity Entity.heavyWhiteDownPointingTriangle

-- | The squared saltire HTML entity ('⛝').
squaredSaltire :: ValidChild Content parent
               => ChildHXML parent
squaredSaltire = Tag_Entity Entity.squaredSaltire

-- | The falling diagonal in white circle in black square HTML entity ('⛞').
fallingDiagonalInWhiteCircleInBlackSquare :: ValidChild Content parent
                                          => ChildHXML parent
fallingDiagonalInWhiteCircleInBlackSquare = Tag_Entity Entity.fallingDiagonalInWhiteCircleInBlackSquare

-- | The square four corners HTML entity ('⛶').
squareFourCorners :: ValidChild Content parent
                  => ChildHXML parent
squareFourCorners = Tag_Entity Entity.squareFourCorners

-- | The cup on black square HTML entity ('⛾').
cupOnBlackSquare :: ValidChild Content parent
                 => ChildHXML parent
cupOnBlackSquare = Tag_Entity Entity.cupOnBlackSquare

-- | The check mark HTML entity ('✓').
checkMark :: ValidChild Content parent
          => ChildHXML parent
checkMark = Tag_Entity Entity.checkMark

-- | The ballot x HTML entity ('✗').
ballotX :: ValidChild Content parent
        => ChildHXML parent
ballotX = Tag_Entity Entity.ballotX

-- | The maltese cross HTML entity ('✠').
malteseCross :: ValidChild Content parent
             => ChildHXML parent
malteseCross = Tag_Entity Entity.malteseCross

-- | The Circled White Star HTML entity ('✪').
circledWhiteStar :: ValidChild Content parent
                 => ChildHXML parent
circledWhiteStar = Tag_Entity Entity.circledWhiteStar

-- | The six pointed black star HTML entity ('✶').
sixPointedBlackStar :: ValidChild Content parent
                    => ChildHXML parent
sixPointedBlackStar = Tag_Entity Entity.sixPointedBlackStar

-- | The Circled Open Centre Eight Pointed Star HTML entity ('❂').
circledOpenCentreEightPointedStar :: ValidChild Content parent
                                  => ChildHXML parent
circledOpenCentreEightPointedStar = Tag_Entity Entity.circledOpenCentreEightPointedStar

-- | The Shadowed White Circle HTML entity ('❍').
shadowedWhiteCircle :: ValidChild Content parent
                    => ChildHXML parent
shadowedWhiteCircle = Tag_Entity Entity.shadowedWhiteCircle

-- | The lower right drop-shadowed white square HTML entity ('❏').
lowerRightDropShadowedWhiteSquare :: ValidChild Content parent
                                  => ChildHXML parent
lowerRightDropShadowedWhiteSquare = Tag_Entity Entity.lowerRightDropShadowedWhiteSquare

-- | The upper right drop-shadowed white square HTML entity ('❐').
upperRightDropShadowedWhiteSquare :: ValidChild Content parent
                                  => ChildHXML parent
upperRightDropShadowedWhiteSquare = Tag_Entity Entity.upperRightDropShadowedWhiteSquare

-- | The lower right shadowed white square HTML entity ('❑').
lowerRightShadowedWhiteSquare :: ValidChild Content parent
                              => ChildHXML parent
lowerRightShadowedWhiteSquare = Tag_Entity Entity.lowerRightShadowedWhiteSquare

-- | The upper right shadowed white square HTML entity ('❒').
upperRightShadowedWhiteSquare :: ValidChild Content parent
                              => ChildHXML parent
upperRightShadowedWhiteSquare = Tag_Entity Entity.upperRightShadowedWhiteSquare

-- | The Black Diamond Minus White X HTML entity ('❖').
blackDiamondMinusWhiteX :: ValidChild Content parent
                        => ChildHXML parent
blackDiamondMinusWhiteX = Tag_Entity Entity.blackDiamondMinusWhiteX

-- | The light vertical bar HTML entity ('❘').
lightVerticalBar :: ValidChild Content parent
                 => ChildHXML parent
lightVerticalBar = Tag_Entity Entity.lightVerticalBar

-- | The light left tortoise shell bracket ornament HTML entity ('❲').
lightLeftTortoiseShellBracketOrnament :: ValidChild Content parent
                                      => ChildHXML parent
lightLeftTortoiseShellBracketOrnament = Tag_Entity Entity.lightLeftTortoiseShellBracketOrnament

-- | The light right tortoise shell bracket ornament HTML entity ('❳').
lightRightTortoiseShellBracketOrnament :: ValidChild Content parent
                                       => ChildHXML parent
lightRightTortoiseShellBracketOrnament = Tag_Entity Entity.lightRightTortoiseShellBracketOrnament

-- | The Heavy Plus Sign HTML entity ('➕').
heavyPlusSign :: ValidChild Content parent
              => ChildHXML parent
heavyPlusSign = Tag_Entity Entity.heavyPlusSign

-- | The Heavy Minus Sign HTML entity ('➖').
heavyMinusSign :: ValidChild Content parent
               => ChildHXML parent
heavyMinusSign = Tag_Entity Entity.heavyMinusSign

-- | The Heavy Division Sign HTML entity ('➗').
heavyDivisionSign :: ValidChild Content parent
                  => ChildHXML parent
heavyDivisionSign = Tag_Entity Entity.heavyDivisionSign

-- | The Three Dimensional Angle HTML entity ('⟀').
threeDimensionalAngle :: ValidChild Content parent
                      => ChildHXML parent
threeDimensionalAngle = Tag_Entity Entity.threeDimensionalAngle

-- | The White Triangle Containing Small White Triangle HTML entity ('⟁').
whiteTriangleContainingSmallWhiteTriangle :: ValidChild Content parent
                                          => ChildHXML parent
whiteTriangleContainingSmallWhiteTriangle = Tag_Entity Entity.whiteTriangleContainingSmallWhiteTriangle

-- | The Mathematical Rising Diagonal HTML entity ('⟋').
mathematicalRisingDiagonal :: ValidChild Content parent
                           => ChildHXML parent
mathematicalRisingDiagonal = Tag_Entity Entity.mathematicalRisingDiagonal

-- | The Long Division HTML entity ('⟌').
longDivision :: ValidChild Content parent
             => ChildHXML parent
longDivision = Tag_Entity Entity.longDivision

-- | The Mathematical Falling Diagonal HTML entity ('⟍').
mathematicalFallingDiagonal :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFallingDiagonal = Tag_Entity Entity.mathematicalFallingDiagonal

-- | The squared logical and HTML entity ('⟎').
squaredLogicalAnd :: ValidChild Content parent
                  => ChildHXML parent
squaredLogicalAnd = Tag_Entity Entity.squaredLogicalAnd

-- | The squared logical or HTML entity ('⟏').
squaredLogicalOr :: ValidChild Content parent
                 => ChildHXML parent
squaredLogicalOr = Tag_Entity Entity.squaredLogicalOr

-- | The White Diamond With Centred Dot HTML entity ('⟐').
whiteDiamondWithCentredDot :: ValidChild Content parent
                           => ChildHXML parent
whiteDiamondWithCentredDot = Tag_Entity Entity.whiteDiamondWithCentredDot

-- | The Up Tack With Circle Above HTML entity ('⟟').
upTackWithCircleAbove :: ValidChild Content parent
                      => ChildHXML parent
upTackWithCircleAbove = Tag_Entity Entity.upTackWithCircleAbove

-- | The Lozenge Divided By Horizontal Rule HTML entity ('⟠').
lozengeDividedByHorizontalRule :: ValidChild Content parent
                               => ChildHXML parent
lozengeDividedByHorizontalRule = Tag_Entity Entity.lozengeDividedByHorizontalRule

-- | The White Concave-sided Diamond HTML entity ('⟡').
whiteConcaveSidedDiamond :: ValidChild Content parent
                         => ChildHXML parent
whiteConcaveSidedDiamond = Tag_Entity Entity.whiteConcaveSidedDiamond

-- | The White Concave-sided Diamond With Leftwards Tick HTML entity ('⟢').
whiteConcaveSidedDiamondWithLeftwardsTick :: ValidChild Content parent
                                          => ChildHXML parent
whiteConcaveSidedDiamondWithLeftwardsTick = Tag_Entity Entity.whiteConcaveSidedDiamondWithLeftwardsTick

-- | The White Concave-sided Diamond With Rightwards Tick HTML entity ('⟣').
whiteConcaveSidedDiamondWithRightwardsTick :: ValidChild Content parent
                                           => ChildHXML parent
whiteConcaveSidedDiamondWithRightwardsTick = Tag_Entity Entity.whiteConcaveSidedDiamondWithRightwardsTick

-- | The white square with leftwards tick HTML entity ('⟤').
whiteSquareWithLeftwardsTick :: ValidChild Content parent
                             => ChildHXML parent
whiteSquareWithLeftwardsTick = Tag_Entity Entity.whiteSquareWithLeftwardsTick

-- | The white square with rightwards tick HTML entity ('⟥').
whiteSquareWithRightwardsTick :: ValidChild Content parent
                              => ChildHXML parent
whiteSquareWithRightwardsTick = Tag_Entity Entity.whiteSquareWithRightwardsTick

-- | The mathematical left white square bracket HTML entity ('⟦').
mathematicalLeftWhiteSquareBracket :: ValidChild Content parent
                                   => ChildHXML parent
mathematicalLeftWhiteSquareBracket = Tag_Entity Entity.mathematicalLeftWhiteSquareBracket

-- | The mathematical right white square bracket HTML entity ('⟧').
mathematicalRightWhiteSquareBracket :: ValidChild Content parent
                                    => ChildHXML parent
mathematicalRightWhiteSquareBracket = Tag_Entity Entity.mathematicalRightWhiteSquareBracket

-- | The mathematical left angle bracket HTML entity ('⟨').
mathematicalLeftAngleBracket :: ValidChild Content parent
                             => ChildHXML parent
mathematicalLeftAngleBracket = Tag_Entity Entity.mathematicalLeftAngleBracket

-- | The mathematical right angle bracket HTML entity ('⟩').
mathematicalRightAngleBracket :: ValidChild Content parent
                              => ChildHXML parent
mathematicalRightAngleBracket = Tag_Entity Entity.mathematicalRightAngleBracket

-- | The mathematical left double angle bracket HTML entity ('⟪').
mathematicalLeftDoubleAngleBracket :: ValidChild Content parent
                                   => ChildHXML parent
mathematicalLeftDoubleAngleBracket = Tag_Entity Entity.mathematicalLeftDoubleAngleBracket

-- | The mathematical right double angle bracket HTML entity ('⟫').
mathematicalRightDoubleAngleBracket :: ValidChild Content parent
                                    => ChildHXML parent
mathematicalRightDoubleAngleBracket = Tag_Entity Entity.mathematicalRightDoubleAngleBracket

-- | The mathematical left white tortoise shell bracket HTML entity ('⟬').
mathematicalLeftWhiteTortoiseShellBracket :: ValidChild Content parent
                                          => ChildHXML parent
mathematicalLeftWhiteTortoiseShellBracket = Tag_Entity Entity.mathematicalLeftWhiteTortoiseShellBracket

-- | The mathematical right white tortoise shell bracket HTML entity ('⟭').
mathematicalRightWhiteTortoiseShellBracket :: ValidChild Content parent
                                           => ChildHXML parent
mathematicalRightWhiteTortoiseShellBracket = Tag_Entity Entity.mathematicalRightWhiteTortoiseShellBracket

-- | The long leftwards arrow HTML entity ('⟵').
longLeftwardsArrow :: ValidChild Content parent
                   => ChildHXML parent
longLeftwardsArrow = Tag_Entity Entity.longLeftwardsArrow

-- | The long rightwards arrow HTML entity ('⟶').
longRightwardsArrow :: ValidChild Content parent
                    => ChildHXML parent
longRightwardsArrow = Tag_Entity Entity.longRightwardsArrow

-- | The long left right arrow HTML entity ('⟷').
longLeftRightArrow :: ValidChild Content parent
                   => ChildHXML parent
longLeftRightArrow = Tag_Entity Entity.longLeftRightArrow

-- | The long leftwards double arrow HTML entity ('⟸').
longLeftwardsDoubleArrow :: ValidChild Content parent
                         => ChildHXML parent
longLeftwardsDoubleArrow = Tag_Entity Entity.longLeftwardsDoubleArrow

-- | The long rightwards double arrow HTML entity ('⟹').
longRightwardsDoubleArrow :: ValidChild Content parent
                          => ChildHXML parent
longRightwardsDoubleArrow = Tag_Entity Entity.longRightwardsDoubleArrow

-- | The long left right double arrow HTML entity ('⟺').
longLeftRightDoubleArrow :: ValidChild Content parent
                         => ChildHXML parent
longLeftRightDoubleArrow = Tag_Entity Entity.longLeftRightDoubleArrow

-- | The long rightwards arrow from bar HTML entity ('⟼').
longRightwardsArrowFromBar :: ValidChild Content parent
                           => ChildHXML parent
longRightwardsArrowFromBar = Tag_Entity Entity.longRightwardsArrowFromBar

-- | The long rightwards squiggle arrow HTML entity ('⟿').
longRightwardsSquiggleArrow :: ValidChild Content parent
                            => ChildHXML parent
longRightwardsSquiggleArrow = Tag_Entity Entity.longRightwardsSquiggleArrow

-- | The leftwards double arrow with vertical stroke HTML entity ('⤂').
leftwardsDoubleArrowWithVerticalStroke :: ValidChild Content parent
                                       => ChildHXML parent
leftwardsDoubleArrowWithVerticalStroke = Tag_Entity Entity.leftwardsDoubleArrowWithVerticalStroke

-- | The rightwards double arrow with vertical stroke HTML entity ('⤃').
rightwardsDoubleArrowWithVerticalStroke :: ValidChild Content parent
                                        => ChildHXML parent
rightwardsDoubleArrowWithVerticalStroke = Tag_Entity Entity.rightwardsDoubleArrowWithVerticalStroke

-- | The left right double arrow with vertical stroke HTML entity ('⤄').
leftRightDoubleArrowWithVerticalStroke :: ValidChild Content parent
                                       => ChildHXML parent
leftRightDoubleArrowWithVerticalStroke = Tag_Entity Entity.leftRightDoubleArrowWithVerticalStroke

-- | The rightwards two-headed arrow from bar HTML entity ('⤅').
rightwardsTwoHeadedArrowFromBar :: ValidChild Content parent
                                => ChildHXML parent
rightwardsTwoHeadedArrowFromBar = Tag_Entity Entity.rightwardsTwoHeadedArrowFromBar

-- | The leftwards double dash arrow HTML entity ('⤌').
leftwardsDoubleDashArrow :: ValidChild Content parent
                         => ChildHXML parent
leftwardsDoubleDashArrow = Tag_Entity Entity.leftwardsDoubleDashArrow

-- | The rightwards double dash arrow HTML entity ('⤍').
rightwardsDoubleDashArrow :: ValidChild Content parent
                          => ChildHXML parent
rightwardsDoubleDashArrow = Tag_Entity Entity.rightwardsDoubleDashArrow

-- | The leftwards triple dash arrow HTML entity ('⤎').
leftwardsTripleDashArrow :: ValidChild Content parent
                         => ChildHXML parent
leftwardsTripleDashArrow = Tag_Entity Entity.leftwardsTripleDashArrow

-- | The rightwards triple dash arrow HTML entity ('⤏').
rightwardsTripleDashArrow :: ValidChild Content parent
                          => ChildHXML parent
rightwardsTripleDashArrow = Tag_Entity Entity.rightwardsTripleDashArrow

-- | The rightwards two-headed triple dash arrow HTML entity ('⤐').
rightwardsTwoHeadedTripleDashArrow :: ValidChild Content parent
                                   => ChildHXML parent
rightwardsTwoHeadedTripleDashArrow = Tag_Entity Entity.rightwardsTwoHeadedTripleDashArrow

-- | The rightwards arrow with dotted stem HTML entity ('⤑').
rightwardsArrowWithDottedStem :: ValidChild Content parent
                              => ChildHXML parent
rightwardsArrowWithDottedStem = Tag_Entity Entity.rightwardsArrowWithDottedStem

-- | The upwards arrow to bar HTML entity ('⤒').
upwardsArrowToBar :: ValidChild Content parent
                  => ChildHXML parent
upwardsArrowToBar = Tag_Entity Entity.upwardsArrowToBar

-- | The downwards arrow to bar HTML entity ('⤓').
downwardsArrowToBar :: ValidChild Content parent
                    => ChildHXML parent
downwardsArrowToBar = Tag_Entity Entity.downwardsArrowToBar

-- | The rightwards two-headed arrow with tail HTML entity ('⤖').
rightwardsTwoHeadedArrowWithTail :: ValidChild Content parent
                                 => ChildHXML parent
rightwardsTwoHeadedArrowWithTail = Tag_Entity Entity.rightwardsTwoHeadedArrowWithTail

-- | The leftwards arrow-tail HTML entity ('⤙').
leftwardsArrowTail :: ValidChild Content parent
                   => ChildHXML parent
leftwardsArrowTail = Tag_Entity Entity.leftwardsArrowTail

-- | The rightwards arrow-tail HTML entity ('⤚').
rightwardsArrowTail :: ValidChild Content parent
                    => ChildHXML parent
rightwardsArrowTail = Tag_Entity Entity.rightwardsArrowTail

-- | The leftwards double arrow-tail HTML entity ('⤛').
leftwardsDoubleArrowTail :: ValidChild Content parent
                         => ChildHXML parent
leftwardsDoubleArrowTail = Tag_Entity Entity.leftwardsDoubleArrowTail

-- | The rightwards double arrow-tail HTML entity ('⤜').
rightwardsDoubleArrowTail :: ValidChild Content parent
                          => ChildHXML parent
rightwardsDoubleArrowTail = Tag_Entity Entity.rightwardsDoubleArrowTail

-- | The leftwards arrow to black diamond HTML entity ('⤝').
leftwardsArrowToBlackDiamond :: ValidChild Content parent
                             => ChildHXML parent
leftwardsArrowToBlackDiamond = Tag_Entity Entity.leftwardsArrowToBlackDiamond

-- | The rightwards arrow to black diamond HTML entity ('⤞').
rightwardsArrowToBlackDiamond :: ValidChild Content parent
                              => ChildHXML parent
rightwardsArrowToBlackDiamond = Tag_Entity Entity.rightwardsArrowToBlackDiamond

-- | The leftwards arrow from bar to black diamond HTML entity ('⤟').
leftwardsArrowFromBarToBlackDiamond :: ValidChild Content parent
                                    => ChildHXML parent
leftwardsArrowFromBarToBlackDiamond = Tag_Entity Entity.leftwardsArrowFromBarToBlackDiamond

-- | The rightwards arrow from bar to black diamond HTML entity ('⤠').
rightwardsArrowFromBarToBlackDiamond :: ValidChild Content parent
                                     => ChildHXML parent
rightwardsArrowFromBarToBlackDiamond = Tag_Entity Entity.rightwardsArrowFromBarToBlackDiamond

-- | The north west arrow with hook HTML entity ('⤣').
northWestArrowWithHook :: ValidChild Content parent
                       => ChildHXML parent
northWestArrowWithHook = Tag_Entity Entity.northWestArrowWithHook

-- | The north east arrow with hook HTML entity ('⤤').
northEastArrowWithHook :: ValidChild Content parent
                       => ChildHXML parent
northEastArrowWithHook = Tag_Entity Entity.northEastArrowWithHook

-- | The south east arrow with hook HTML entity ('⤥').
southEastArrowWithHook :: ValidChild Content parent
                       => ChildHXML parent
southEastArrowWithHook = Tag_Entity Entity.southEastArrowWithHook

-- | The south west arrow with hook HTML entity ('⤦').
southWestArrowWithHook :: ValidChild Content parent
                       => ChildHXML parent
southWestArrowWithHook = Tag_Entity Entity.southWestArrowWithHook

-- | The north west arrow and north east arrow HTML entity ('⤧').
northWestArrowAndNorthEastArrow :: ValidChild Content parent
                                => ChildHXML parent
northWestArrowAndNorthEastArrow = Tag_Entity Entity.northWestArrowAndNorthEastArrow

-- | The north east arrow and south east arrow HTML entity ('⤨').
northEastArrowAndSouthEastArrow :: ValidChild Content parent
                                => ChildHXML parent
northEastArrowAndSouthEastArrow = Tag_Entity Entity.northEastArrowAndSouthEastArrow

-- | The south east arrow and south west arrow HTML entity ('⤩').
southEastArrowAndSouthWestArrow :: ValidChild Content parent
                                => ChildHXML parent
southEastArrowAndSouthWestArrow = Tag_Entity Entity.southEastArrowAndSouthWestArrow

-- | The south west arrow and north west arrow HTML entity ('⤪').
southWestArrowAndNorthWestArrow :: ValidChild Content parent
                                => ChildHXML parent
southWestArrowAndNorthWestArrow = Tag_Entity Entity.southWestArrowAndNorthWestArrow

-- | The Rising Diagonal Crossing Falling Diagonal HTML entity ('⤫').
risingDiagonalCrossingFallingDiagonal :: ValidChild Content parent
                                      => ChildHXML parent
risingDiagonalCrossingFallingDiagonal = Tag_Entity Entity.risingDiagonalCrossingFallingDiagonal

-- | The Falling Diagonal Crossing Rising Diagonal HTML entity ('⤬').
fallingDiagonalCrossingRisingDiagonal :: ValidChild Content parent
                                      => ChildHXML parent
fallingDiagonalCrossingRisingDiagonal = Tag_Entity Entity.fallingDiagonalCrossingRisingDiagonal

-- | The Falling Diagonal Crossing North East Arrow HTML entity ('⤯').
fallingDiagonalCrossingNorthEastArrow :: ValidChild Content parent
                                      => ChildHXML parent
fallingDiagonalCrossingNorthEastArrow = Tag_Entity Entity.fallingDiagonalCrossingNorthEastArrow

-- | The Rising Diagonal Crossing South East Arrow HTML entity ('⤰').
risingDiagonalCrossingSouthEastArrow :: ValidChild Content parent
                                     => ChildHXML parent
risingDiagonalCrossingSouthEastArrow = Tag_Entity Entity.risingDiagonalCrossingSouthEastArrow

-- | The wave arrow pointing directly right HTML entity ('⤳').
waveArrowPointingDirectlyRight :: ValidChild Content parent
                               => ChildHXML parent
waveArrowPointingDirectlyRight = Tag_Entity Entity.waveArrowPointingDirectlyRight

-- | The arrow pointing rightwards then curving downwards HTML entity ('⤵').
arrowPointingRightwardsThenCurvingDownwards :: ValidChild Content parent
                                            => ChildHXML parent
arrowPointingRightwardsThenCurvingDownwards = Tag_Entity Entity.arrowPointingRightwardsThenCurvingDownwards

-- | The arrow pointing downwards then curving leftwards HTML entity ('⤶').
arrowPointingDownwardsThenCurvingLeftwards :: ValidChild Content parent
                                           => ChildHXML parent
arrowPointingDownwardsThenCurvingLeftwards = Tag_Entity Entity.arrowPointingDownwardsThenCurvingLeftwards

-- | The arrow pointing downwards then curving rightwards HTML entity ('⤷').
arrowPointingDownwardsThenCurvingRightwards :: ValidChild Content parent
                                            => ChildHXML parent
arrowPointingDownwardsThenCurvingRightwards = Tag_Entity Entity.arrowPointingDownwardsThenCurvingRightwards

-- | The right-side arc clockwise arrow HTML entity ('⤸').
rightSideArcClockwiseArrow :: ValidChild Content parent
                           => ChildHXML parent
rightSideArcClockwiseArrow = Tag_Entity Entity.rightSideArcClockwiseArrow

-- | The left-side arc anticlockwise arrow HTML entity ('⤹').
leftSideArcAnticlockwiseArrow :: ValidChild Content parent
                              => ChildHXML parent
leftSideArcAnticlockwiseArrow = Tag_Entity Entity.leftSideArcAnticlockwiseArrow

-- | The Top Arc Anticlockwise Arrow HTML entity ('⤺').
topArcAnticlockwiseArrow :: ValidChild Content parent
                         => ChildHXML parent
topArcAnticlockwiseArrow = Tag_Entity Entity.topArcAnticlockwiseArrow

-- | The Bottom Arc Anticlockwise Arrow HTML entity ('⤻').
bottomArcAnticlockwiseArrow :: ValidChild Content parent
                            => ChildHXML parent
bottomArcAnticlockwiseArrow = Tag_Entity Entity.bottomArcAnticlockwiseArrow

-- | The top arc clockwise arrow with minus HTML entity ('⤼').
topArcClockwiseArrowWithMinus :: ValidChild Content parent
                              => ChildHXML parent
topArcClockwiseArrowWithMinus = Tag_Entity Entity.topArcClockwiseArrowWithMinus

-- | The top arc anticlockwise arrow with plus HTML entity ('⤽').
topArcAnticlockwiseArrowWithPlus :: ValidChild Content parent
                                 => ChildHXML parent
topArcAnticlockwiseArrowWithPlus = Tag_Entity Entity.topArcAnticlockwiseArrowWithPlus

-- | The rightwards arrow with plus below HTML entity ('⥅').
rightwardsArrowWithPlusBelow :: ValidChild Content parent
                             => ChildHXML parent
rightwardsArrowWithPlusBelow = Tag_Entity Entity.rightwardsArrowWithPlusBelow

-- | The left right arrow through small circle HTML entity ('⥈').
leftRightArrowThroughSmallCircle :: ValidChild Content parent
                                 => ChildHXML parent
leftRightArrowThroughSmallCircle = Tag_Entity Entity.leftRightArrowThroughSmallCircle

-- | The upwards two-headed arrow from small circle HTML entity ('⥉').
upwardsTwoHeadedArrowFromSmallCircle :: ValidChild Content parent
                                     => ChildHXML parent
upwardsTwoHeadedArrowFromSmallCircle = Tag_Entity Entity.upwardsTwoHeadedArrowFromSmallCircle

-- | The left barb up right barb down harpoon HTML entity ('⥊').
leftBarbUpRightBarbDownHarpoon :: ValidChild Content parent
                               => ChildHXML parent
leftBarbUpRightBarbDownHarpoon = Tag_Entity Entity.leftBarbUpRightBarbDownHarpoon

-- | The left barb down right barb up harpoon HTML entity ('⥋').
leftBarbDownRightBarbUpHarpoon :: ValidChild Content parent
                               => ChildHXML parent
leftBarbDownRightBarbUpHarpoon = Tag_Entity Entity.leftBarbDownRightBarbUpHarpoon

-- | The Up Barb Right Down Barb Left Harpoon HTML entity ('⥌').
upBarbRightDownBarbLeftHarpoon :: ValidChild Content parent
                               => ChildHXML parent
upBarbRightDownBarbLeftHarpoon = Tag_Entity Entity.upBarbRightDownBarbLeftHarpoon

-- | The Up Barb Left Down Barb Right Harpoon HTML entity ('⥍').
upBarbLeftDownBarbRightHarpoon :: ValidChild Content parent
                               => ChildHXML parent
upBarbLeftDownBarbRightHarpoon = Tag_Entity Entity.upBarbLeftDownBarbRightHarpoon

-- | The left barb up right barb up harpoon HTML entity ('⥎').
leftBarbUpRightBarbUpHarpoon :: ValidChild Content parent
                             => ChildHXML parent
leftBarbUpRightBarbUpHarpoon = Tag_Entity Entity.leftBarbUpRightBarbUpHarpoon

-- | The up barb right down barb right harpoon HTML entity ('⥏').
upBarbRightDownBarbRightHarpoon :: ValidChild Content parent
                                => ChildHXML parent
upBarbRightDownBarbRightHarpoon = Tag_Entity Entity.upBarbRightDownBarbRightHarpoon

-- | The left barb down right barb down harpoon HTML entity ('⥐').
leftBarbDownRightBarbDownHarpoon :: ValidChild Content parent
                                 => ChildHXML parent
leftBarbDownRightBarbDownHarpoon = Tag_Entity Entity.leftBarbDownRightBarbDownHarpoon

-- | The up barb left down barb left harpoon HTML entity ('⥑').
upBarbLeftDownBarbLeftHarpoon :: ValidChild Content parent
                              => ChildHXML parent
upBarbLeftDownBarbLeftHarpoon = Tag_Entity Entity.upBarbLeftDownBarbLeftHarpoon

-- | The leftwards harpoon with barb up to bar HTML entity ('⥒').
leftwardsHarpoonWithBarbUpToBar :: ValidChild Content parent
                                => ChildHXML parent
leftwardsHarpoonWithBarbUpToBar = Tag_Entity Entity.leftwardsHarpoonWithBarbUpToBar

-- | The rightwards harpoon with barb up to bar HTML entity ('⥓').
rightwardsHarpoonWithBarbUpToBar :: ValidChild Content parent
                                 => ChildHXML parent
rightwardsHarpoonWithBarbUpToBar = Tag_Entity Entity.rightwardsHarpoonWithBarbUpToBar

-- | The upwards harpoon with barb right to bar HTML entity ('⥔').
upwardsHarpoonWithBarbRightToBar :: ValidChild Content parent
                                 => ChildHXML parent
upwardsHarpoonWithBarbRightToBar = Tag_Entity Entity.upwardsHarpoonWithBarbRightToBar

-- | The downwards harpoon with barb right to bar HTML entity ('⥕').
downwardsHarpoonWithBarbRightToBar :: ValidChild Content parent
                                   => ChildHXML parent
downwardsHarpoonWithBarbRightToBar = Tag_Entity Entity.downwardsHarpoonWithBarbRightToBar

-- | The leftwards harpoon with barb down to bar HTML entity ('⥖').
leftwardsHarpoonWithBarbDownToBar :: ValidChild Content parent
                                  => ChildHXML parent
leftwardsHarpoonWithBarbDownToBar = Tag_Entity Entity.leftwardsHarpoonWithBarbDownToBar

-- | The rightwards harpoon with barb down to bar HTML entity ('⥗').
rightwardsHarpoonWithBarbDownToBar :: ValidChild Content parent
                                   => ChildHXML parent
rightwardsHarpoonWithBarbDownToBar = Tag_Entity Entity.rightwardsHarpoonWithBarbDownToBar

-- | The upwards harpoon with barb left to bar HTML entity ('⥘').
upwardsHarpoonWithBarbLeftToBar :: ValidChild Content parent
                                => ChildHXML parent
upwardsHarpoonWithBarbLeftToBar = Tag_Entity Entity.upwardsHarpoonWithBarbLeftToBar

-- | The downwards harpoon with barb left to bar HTML entity ('⥙').
downwardsHarpoonWithBarbLeftToBar :: ValidChild Content parent
                                  => ChildHXML parent
downwardsHarpoonWithBarbLeftToBar = Tag_Entity Entity.downwardsHarpoonWithBarbLeftToBar

-- | The leftwards harpoon with barb up from bar HTML entity ('⥚').
leftwardsHarpoonWithBarbUpFromBar :: ValidChild Content parent
                                  => ChildHXML parent
leftwardsHarpoonWithBarbUpFromBar = Tag_Entity Entity.leftwardsHarpoonWithBarbUpFromBar

-- | The rightwards harpoon with barb up from bar HTML entity ('⥛').
rightwardsHarpoonWithBarbUpFromBar :: ValidChild Content parent
                                   => ChildHXML parent
rightwardsHarpoonWithBarbUpFromBar = Tag_Entity Entity.rightwardsHarpoonWithBarbUpFromBar

-- | The upwards harpoon with barb right from bar HTML entity ('⥜').
upwardsHarpoonWithBarbRightFromBar :: ValidChild Content parent
                                   => ChildHXML parent
upwardsHarpoonWithBarbRightFromBar = Tag_Entity Entity.upwardsHarpoonWithBarbRightFromBar

-- | The downwards harpoon with barb right from bar HTML entity ('⥝').
downwardsHarpoonWithBarbRightFromBar :: ValidChild Content parent
                                     => ChildHXML parent
downwardsHarpoonWithBarbRightFromBar = Tag_Entity Entity.downwardsHarpoonWithBarbRightFromBar

-- | The leftwards harpoon with barb down from bar HTML entity ('⥞').
leftwardsHarpoonWithBarbDownFromBar :: ValidChild Content parent
                                    => ChildHXML parent
leftwardsHarpoonWithBarbDownFromBar = Tag_Entity Entity.leftwardsHarpoonWithBarbDownFromBar

-- | The rightwards harpoon with barb down from bar HTML entity ('⥟').
rightwardsHarpoonWithBarbDownFromBar :: ValidChild Content parent
                                     => ChildHXML parent
rightwardsHarpoonWithBarbDownFromBar = Tag_Entity Entity.rightwardsHarpoonWithBarbDownFromBar

-- | The upwards harpoon with barb left from bar HTML entity ('⥠').
upwardsHarpoonWithBarbLeftFromBar :: ValidChild Content parent
                                  => ChildHXML parent
upwardsHarpoonWithBarbLeftFromBar = Tag_Entity Entity.upwardsHarpoonWithBarbLeftFromBar

-- | The downwards harpoon with barb left from bar HTML entity ('⥡').
downwardsHarpoonWithBarbLeftFromBar :: ValidChild Content parent
                                    => ChildHXML parent
downwardsHarpoonWithBarbLeftFromBar = Tag_Entity Entity.downwardsHarpoonWithBarbLeftFromBar

-- | The leftwards harpoon with barb up above leftwards harpoon with barb down HTML entity ('⥢').
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown :: ValidChild Content parent
                                                            => ChildHXML parent
leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown = Tag_Entity Entity.leftwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbDown

-- | The upwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥣').
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: ValidChild Content parent
                                                            => ChildHXML parent
upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = Tag_Entity Entity.upwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight

-- | The rightwards harpoon with barb up above rightwards harpoon with barb down HTML entity ('⥤').
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown :: ValidChild Content parent
                                                              => ChildHXML parent
rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown = Tag_Entity Entity.rightwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbDown

-- | The downwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥥').
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: ValidChild Content parent
                                                                => ChildHXML parent
downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = Tag_Entity Entity.downwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight

-- | The leftwards harpoon with barb up above rightwards harpoon with barb up HTML entity ('⥦').
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp :: ValidChild Content parent
                                                           => ChildHXML parent
leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp = Tag_Entity Entity.leftwardsHarpoonWithBarbUpAboveRightwardsHarpoonWithBarbUp

-- | The leftwards harpoon with barb down above rightwards harpoon with barb down HTML entity ('⥧').
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown :: ValidChild Content parent
                                                               => ChildHXML parent
leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown = Tag_Entity Entity.leftwardsHarpoonWithBarbDownAboveRightwardsHarpoonWithBarbDown

-- | The rightwards harpoon with barb up above leftwards harpoon with barb up HTML entity ('⥨').
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp :: ValidChild Content parent
                                                           => ChildHXML parent
rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp = Tag_Entity Entity.rightwardsHarpoonWithBarbUpAboveLeftwardsHarpoonWithBarbUp

-- | The rightwards harpoon with barb down above leftwards harpoon with barb down HTML entity ('⥩').
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown :: ValidChild Content parent
                                                               => ChildHXML parent
rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown = Tag_Entity Entity.rightwardsHarpoonWithBarbDownAboveLeftwardsHarpoonWithBarbDown

-- | The leftwards harpoon with barb up above long dash HTML entity ('⥪').
leftwardsHarpoonWithBarbUpAboveLongDash :: ValidChild Content parent
                                        => ChildHXML parent
leftwardsHarpoonWithBarbUpAboveLongDash = Tag_Entity Entity.leftwardsHarpoonWithBarbUpAboveLongDash

-- | The leftwards harpoon with barb down below long dash HTML entity ('⥫').
leftwardsHarpoonWithBarbDownBelowLongDash :: ValidChild Content parent
                                          => ChildHXML parent
leftwardsHarpoonWithBarbDownBelowLongDash = Tag_Entity Entity.leftwardsHarpoonWithBarbDownBelowLongDash

-- | The rightwards harpoon with barb up above long dash HTML entity ('⥬').
rightwardsHarpoonWithBarbUpAboveLongDash :: ValidChild Content parent
                                         => ChildHXML parent
rightwardsHarpoonWithBarbUpAboveLongDash = Tag_Entity Entity.rightwardsHarpoonWithBarbUpAboveLongDash

-- | The rightwards harpoon with barb down below long dash HTML entity ('⥭').
rightwardsHarpoonWithBarbDownBelowLongDash :: ValidChild Content parent
                                           => ChildHXML parent
rightwardsHarpoonWithBarbDownBelowLongDash = Tag_Entity Entity.rightwardsHarpoonWithBarbDownBelowLongDash

-- | The upwards harpoon with barb left beside downwards harpoon with barb right HTML entity ('⥮').
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight :: ValidChild Content parent
                                                              => ChildHXML parent
upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight = Tag_Entity Entity.upwardsHarpoonWithBarbLeftBesideDownwardsHarpoonWithBarbRight

-- | The downwards harpoon with barb left beside upwards harpoon with barb right HTML entity ('⥯').
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight :: ValidChild Content parent
                                                              => ChildHXML parent
downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight = Tag_Entity Entity.downwardsHarpoonWithBarbLeftBesideUpwardsHarpoonWithBarbRight

-- | The right double arrow with rounded head HTML entity ('⥰').
rightDoubleArrowWithRoundedHead :: ValidChild Content parent
                                => ChildHXML parent
rightDoubleArrowWithRoundedHead = Tag_Entity Entity.rightDoubleArrowWithRoundedHead

-- | The equals sign above rightwards arrow HTML entity ('⥱').
equalsSignAboveRightwardsArrow :: ValidChild Content parent
                               => ChildHXML parent
equalsSignAboveRightwardsArrow = Tag_Entity Entity.equalsSignAboveRightwardsArrow

-- | The tilde operator above rightwards arrow HTML entity ('⥲').
tildeOperatorAboveRightwardsArrow :: ValidChild Content parent
                                  => ChildHXML parent
tildeOperatorAboveRightwardsArrow = Tag_Entity Entity.tildeOperatorAboveRightwardsArrow

-- | The leftwards arrow above tilde operator HTML entity ('⥳').
leftwardsArrowAboveTildeOperator :: ValidChild Content parent
                                 => ChildHXML parent
leftwardsArrowAboveTildeOperator = Tag_Entity Entity.leftwardsArrowAboveTildeOperator

-- | The rightwards arrow above tilde operator HTML entity ('⥴').
rightwardsArrowAboveTildeOperator :: ValidChild Content parent
                                  => ChildHXML parent
rightwardsArrowAboveTildeOperator = Tag_Entity Entity.rightwardsArrowAboveTildeOperator

-- | The rightwards arrow above almost equal to HTML entity ('⥵').
rightwardsArrowAboveAlmostEqualTo :: ValidChild Content parent
                                  => ChildHXML parent
rightwardsArrowAboveAlmostEqualTo = Tag_Entity Entity.rightwardsArrowAboveAlmostEqualTo

-- | The less-than above leftwards arrow HTML entity ('⥶').
lessThanAboveLeftwardsArrow :: ValidChild Content parent
                            => ChildHXML parent
lessThanAboveLeftwardsArrow = Tag_Entity Entity.lessThanAboveLeftwardsArrow

-- | The Leftwards Arrow Through Less-than HTML entity ('⥷').
leftwardsArrowThroughLessThan :: ValidChild Content parent
                              => ChildHXML parent
leftwardsArrowThroughLessThan = Tag_Entity Entity.leftwardsArrowThroughLessThan

-- | The greater-than above rightwards arrow HTML entity ('⥸').
greaterThanAboveRightwardsArrow :: ValidChild Content parent
                                => ChildHXML parent
greaterThanAboveRightwardsArrow = Tag_Entity Entity.greaterThanAboveRightwardsArrow

-- | The subset above rightwards arrow HTML entity ('⥹').
subsetAboveRightwardsArrow :: ValidChild Content parent
                           => ChildHXML parent
subsetAboveRightwardsArrow = Tag_Entity Entity.subsetAboveRightwardsArrow

-- | The superset above leftwards arrow HTML entity ('⥻').
supersetAboveLeftwardsArrow :: ValidChild Content parent
                            => ChildHXML parent
supersetAboveLeftwardsArrow = Tag_Entity Entity.supersetAboveLeftwardsArrow

-- | The left fish tail HTML entity ('⥼').
leftFishTail :: ValidChild Content parent
             => ChildHXML parent
leftFishTail = Tag_Entity Entity.leftFishTail

-- | The right fish tail HTML entity ('⥽').
rightFishTail :: ValidChild Content parent
              => ChildHXML parent
rightFishTail = Tag_Entity Entity.rightFishTail

-- | The up fish tail HTML entity ('⥾').
upFishTail :: ValidChild Content parent
           => ChildHXML parent
upFishTail = Tag_Entity Entity.upFishTail

-- | The down fish tail HTML entity ('⥿').
downFishTail :: ValidChild Content parent
             => ChildHXML parent
downFishTail = Tag_Entity Entity.downFishTail

-- | The left white parenthesis HTML entity ('⦅').
leftWhiteParenthesis :: ValidChild Content parent
                     => ChildHXML parent
leftWhiteParenthesis = Tag_Entity Entity.leftWhiteParenthesis

-- | The right white parenthesis HTML entity ('⦆').
rightWhiteParenthesis :: ValidChild Content parent
                      => ChildHXML parent
rightWhiteParenthesis = Tag_Entity Entity.rightWhiteParenthesis

-- | The left square bracket with underbar HTML entity ('⦋').
leftSquareBracketWithUnderbar :: ValidChild Content parent
                              => ChildHXML parent
leftSquareBracketWithUnderbar = Tag_Entity Entity.leftSquareBracketWithUnderbar

-- | The right square bracket with underbar HTML entity ('⦌').
rightSquareBracketWithUnderbar :: ValidChild Content parent
                               => ChildHXML parent
rightSquareBracketWithUnderbar = Tag_Entity Entity.rightSquareBracketWithUnderbar

-- | The left square bracket with tick in top corner HTML entity ('⦍').
leftSquareBracketWithTickInTopCorner :: ValidChild Content parent
                                     => ChildHXML parent
leftSquareBracketWithTickInTopCorner = Tag_Entity Entity.leftSquareBracketWithTickInTopCorner

-- | The right square bracket with tick in bottom corner HTML entity ('⦎').
rightSquareBracketWithTickInBottomCorner :: ValidChild Content parent
                                         => ChildHXML parent
rightSquareBracketWithTickInBottomCorner = Tag_Entity Entity.rightSquareBracketWithTickInBottomCorner

-- | The left square bracket with tick in bottom corner HTML entity ('⦏').
leftSquareBracketWithTickInBottomCorner :: ValidChild Content parent
                                        => ChildHXML parent
leftSquareBracketWithTickInBottomCorner = Tag_Entity Entity.leftSquareBracketWithTickInBottomCorner

-- | The right square bracket with tick in top corner HTML entity ('⦐').
rightSquareBracketWithTickInTopCorner :: ValidChild Content parent
                                      => ChildHXML parent
rightSquareBracketWithTickInTopCorner = Tag_Entity Entity.rightSquareBracketWithTickInTopCorner

-- | The left angle bracket with dot HTML entity ('⦑').
leftAngleBracketWithDot :: ValidChild Content parent
                        => ChildHXML parent
leftAngleBracketWithDot = Tag_Entity Entity.leftAngleBracketWithDot

-- | The right angle bracket with dot HTML entity ('⦒').
rightAngleBracketWithDot :: ValidChild Content parent
                         => ChildHXML parent
rightAngleBracketWithDot = Tag_Entity Entity.rightAngleBracketWithDot

-- | The left arc less-than bracket HTML entity ('⦓').
leftArcLessThanBracket :: ValidChild Content parent
                       => ChildHXML parent
leftArcLessThanBracket = Tag_Entity Entity.leftArcLessThanBracket

-- | The right arc greater-than bracket HTML entity ('⦔').
rightArcGreaterThanBracket :: ValidChild Content parent
                           => ChildHXML parent
rightArcGreaterThanBracket = Tag_Entity Entity.rightArcGreaterThanBracket

-- | The double left arc greater-than bracket HTML entity ('⦕').
doubleLeftArcGreaterThanBracket :: ValidChild Content parent
                                => ChildHXML parent
doubleLeftArcGreaterThanBracket = Tag_Entity Entity.doubleLeftArcGreaterThanBracket

-- | The double right arc less-than bracket HTML entity ('⦖').
doubleRightArcLessThanBracket :: ValidChild Content parent
                              => ChildHXML parent
doubleRightArcLessThanBracket = Tag_Entity Entity.doubleRightArcLessThanBracket

-- | The vertical zigzag line HTML entity ('⦚').
verticalZigzagLine :: ValidChild Content parent
                   => ChildHXML parent
verticalZigzagLine = Tag_Entity Entity.verticalZigzagLine

-- | The Measured Angle Opening Left HTML entity ('⦛').
measuredAngleOpeningLeft :: ValidChild Content parent
                         => ChildHXML parent
measuredAngleOpeningLeft = Tag_Entity Entity.measuredAngleOpeningLeft

-- | The right angle variant with square HTML entity ('⦜').
rightAngleVariantWithSquare :: ValidChild Content parent
                            => ChildHXML parent
rightAngleVariantWithSquare = Tag_Entity Entity.rightAngleVariantWithSquare

-- | The measured right angle with dot HTML entity ('⦝').
measuredRightAngleWithDot :: ValidChild Content parent
                          => ChildHXML parent
measuredRightAngleWithDot = Tag_Entity Entity.measuredRightAngleWithDot

-- | The Angle With S Inside HTML entity ('⦞').
angleWithSInside :: ValidChild Content parent
                 => ChildHXML parent
angleWithSInside = Tag_Entity Entity.angleWithSInside

-- | The Acute Angle HTML entity ('⦟').
acuteAngle :: ValidChild Content parent
           => ChildHXML parent
acuteAngle = Tag_Entity Entity.acuteAngle

-- | The Spherical Angle Opening Left HTML entity ('⦠').
sphericalAngleOpeningLeft :: ValidChild Content parent
                          => ChildHXML parent
sphericalAngleOpeningLeft = Tag_Entity Entity.sphericalAngleOpeningLeft

-- | The Spherical Angle Opening Up HTML entity ('⦡').
sphericalAngleOpeningUp :: ValidChild Content parent
                        => ChildHXML parent
sphericalAngleOpeningUp = Tag_Entity Entity.sphericalAngleOpeningUp

-- | The Turned Angle HTML entity ('⦢').
turnedAngle :: ValidChild Content parent
            => ChildHXML parent
turnedAngle = Tag_Entity Entity.turnedAngle

-- | The Reversed Angle HTML entity ('⦣').
reversedAngle :: ValidChild Content parent
              => ChildHXML parent
reversedAngle = Tag_Entity Entity.reversedAngle

-- | The angle with underbar HTML entity ('⦤').
angleWithUnderbar :: ValidChild Content parent
                  => ChildHXML parent
angleWithUnderbar = Tag_Entity Entity.angleWithUnderbar

-- | The reversed angle with underbar HTML entity ('⦥').
reversedAngleWithUnderbar :: ValidChild Content parent
                          => ChildHXML parent
reversedAngleWithUnderbar = Tag_Entity Entity.reversedAngleWithUnderbar

-- | The oblique angle opening up HTML entity ('⦦').
obliqueAngleOpeningUp :: ValidChild Content parent
                      => ChildHXML parent
obliqueAngleOpeningUp = Tag_Entity Entity.obliqueAngleOpeningUp

-- | The oblique angle opening down HTML entity ('⦧').
obliqueAngleOpeningDown :: ValidChild Content parent
                        => ChildHXML parent
obliqueAngleOpeningDown = Tag_Entity Entity.obliqueAngleOpeningDown

-- | The measured angle with open arm ending in arrow pointing up and right HTML entity ('⦨').
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight :: ValidChild Content parent
                                                        => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingUpAndRight = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingUpAndRight

-- | The measured angle with open arm ending in arrow pointing up and left HTML entity ('⦩').
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft :: ValidChild Content parent
                                                       => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingUpAndLeft

-- | The measured angle with open arm ending in arrow pointing down and right HTML entity ('⦪').
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight :: ValidChild Content parent
                                                          => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingDownAndRight = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingDownAndRight

-- | The measured angle with open arm ending in arrow pointing down and left HTML entity ('⦫').
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft :: ValidChild Content parent
                                                         => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingDownAndLeft

-- | The measured angle with open arm ending in arrow pointing right and up HTML entity ('⦬').
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp :: ValidChild Content parent
                                                        => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingRightAndUp = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingRightAndUp

-- | The measured angle with open arm ending in arrow pointing left and up HTML entity ('⦭').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp :: ValidChild Content parent
                                                       => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingLeftAndUp

-- | The measured angle with open arm ending in arrow pointing right and down HTML entity ('⦮').
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown :: ValidChild Content parent
                                                          => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingRightAndDown = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingRightAndDown

-- | The measured angle with open arm ending in arrow pointing left and down HTML entity ('⦯').
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown :: ValidChild Content parent
                                                         => ChildHXML parent
measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown = Tag_Entity Entity.measuredAngleWithOpenArmEndingInArrowPointingLeftAndDown

-- | The reversed empty set HTML entity ('⦰').
reversedEmptySet :: ValidChild Content parent
                 => ChildHXML parent
reversedEmptySet = Tag_Entity Entity.reversedEmptySet

-- | The empty set with overbar HTML entity ('⦱').
emptySetWithOverbar :: ValidChild Content parent
                    => ChildHXML parent
emptySetWithOverbar = Tag_Entity Entity.emptySetWithOverbar

-- | The empty set with small circle above HTML entity ('⦲').
emptySetWithSmallCircleAbove :: ValidChild Content parent
                             => ChildHXML parent
emptySetWithSmallCircleAbove = Tag_Entity Entity.emptySetWithSmallCircleAbove

-- | The empty set with right arrow above HTML entity ('⦳').
emptySetWithRightArrowAbove :: ValidChild Content parent
                            => ChildHXML parent
emptySetWithRightArrowAbove = Tag_Entity Entity.emptySetWithRightArrowAbove

-- | The empty set with left arrow above HTML entity ('⦴').
emptySetWithLeftArrowAbove :: ValidChild Content parent
                           => ChildHXML parent
emptySetWithLeftArrowAbove = Tag_Entity Entity.emptySetWithLeftArrowAbove

-- | The circle with horizontal bar HTML entity ('⦵').
circleWithHorizontalBar :: ValidChild Content parent
                        => ChildHXML parent
circleWithHorizontalBar = Tag_Entity Entity.circleWithHorizontalBar

-- | The circled vertical bar HTML entity ('⦶').
circledVerticalBar :: ValidChild Content parent
                   => ChildHXML parent
circledVerticalBar = Tag_Entity Entity.circledVerticalBar

-- | The circled parallel HTML entity ('⦷').
circledParallel :: ValidChild Content parent
                => ChildHXML parent
circledParallel = Tag_Entity Entity.circledParallel

-- | The Circled Reverse Solidus HTML entity ('⦸').
circledReverseSolidus :: ValidChild Content parent
                      => ChildHXML parent
circledReverseSolidus = Tag_Entity Entity.circledReverseSolidus

-- | The circled perpendicular HTML entity ('⦹').
circledPerpendicular :: ValidChild Content parent
                     => ChildHXML parent
circledPerpendicular = Tag_Entity Entity.circledPerpendicular

-- | The Circle Divided By Horizontal Bar And Top Half Divided By Vertical Bar HTML entity ('⦺').
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar :: ValidChild Content parent
                                                           => ChildHXML parent
circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar = Tag_Entity Entity.circleDividedByHorizontalBarAndTopHalfDividedByVerticalBar

-- | The circle with superimposed x HTML entity ('⦻').
circleWithSuperimposedX :: ValidChild Content parent
                        => ChildHXML parent
circleWithSuperimposedX = Tag_Entity Entity.circleWithSuperimposedX

-- | The circled anticlockwise-rotated division sign HTML entity ('⦼').
circledAnticlockwiseRotatedDivisionSign :: ValidChild Content parent
                                        => ChildHXML parent
circledAnticlockwiseRotatedDivisionSign = Tag_Entity Entity.circledAnticlockwiseRotatedDivisionSign

-- | The circled white bullet HTML entity ('⦾').
circledWhiteBullet :: ValidChild Content parent
                   => ChildHXML parent
circledWhiteBullet = Tag_Entity Entity.circledWhiteBullet

-- | The circled bullet HTML entity ('⦿').
circledBullet :: ValidChild Content parent
              => ChildHXML parent
circledBullet = Tag_Entity Entity.circledBullet

-- | The circled less-than HTML entity ('⧀').
circledLessThan :: ValidChild Content parent
                => ChildHXML parent
circledLessThan = Tag_Entity Entity.circledLessThan

-- | The circled greater-than HTML entity ('⧁').
circledGreaterThan :: ValidChild Content parent
                   => ChildHXML parent
circledGreaterThan = Tag_Entity Entity.circledGreaterThan

-- | The circle with small circle to the right HTML entity ('⧂').
circleWithSmallCircleToTheRight :: ValidChild Content parent
                                => ChildHXML parent
circleWithSmallCircleToTheRight = Tag_Entity Entity.circleWithSmallCircleToTheRight

-- | The circle with two horizontal strokes to the right HTML entity ('⧃').
circleWithTwoHorizontalStrokesToTheRight :: ValidChild Content parent
                                         => ChildHXML parent
circleWithTwoHorizontalStrokesToTheRight = Tag_Entity Entity.circleWithTwoHorizontalStrokesToTheRight

-- | The squared rising diagonal slash HTML entity ('⧄').
squaredRisingDiagonalSlash :: ValidChild Content parent
                           => ChildHXML parent
squaredRisingDiagonalSlash = Tag_Entity Entity.squaredRisingDiagonalSlash

-- | The squared falling diagonal slash HTML entity ('⧅').
squaredFallingDiagonalSlash :: ValidChild Content parent
                            => ChildHXML parent
squaredFallingDiagonalSlash = Tag_Entity Entity.squaredFallingDiagonalSlash

-- | The squared asterisk HTML entity ('⧆').
squaredAsterisk :: ValidChild Content parent
                => ChildHXML parent
squaredAsterisk = Tag_Entity Entity.squaredAsterisk

-- | The squared small circle HTML entity ('⧇').
squaredSmallCircle :: ValidChild Content parent
                   => ChildHXML parent
squaredSmallCircle = Tag_Entity Entity.squaredSmallCircle

-- | The squared square HTML entity ('⧈').
squaredSquare :: ValidChild Content parent
              => ChildHXML parent
squaredSquare = Tag_Entity Entity.squaredSquare

-- | The two joined squares HTML entity ('⧉').
twoJoinedSquares :: ValidChild Content parent
                 => ChildHXML parent
twoJoinedSquares = Tag_Entity Entity.twoJoinedSquares

-- | The Triangle With Dot Above HTML entity ('⧊').
triangleWithDotAbove :: ValidChild Content parent
                     => ChildHXML parent
triangleWithDotAbove = Tag_Entity Entity.triangleWithDotAbove

-- | The Triangle With Underbar HTML entity ('⧋').
triangleWithUnderbar :: ValidChild Content parent
                     => ChildHXML parent
triangleWithUnderbar = Tag_Entity Entity.triangleWithUnderbar

-- | The S In Triangle HTML entity ('⧌').
sInTriangle :: ValidChild Content parent
            => ChildHXML parent
sInTriangle = Tag_Entity Entity.sInTriangle

-- | The triangle with serifs at bottom HTML entity ('⧍').
triangleWithSerifsAtBottom :: ValidChild Content parent
                           => ChildHXML parent
triangleWithSerifsAtBottom = Tag_Entity Entity.triangleWithSerifsAtBottom

-- | The right triangle above left triangle HTML entity ('⧎').
rightTriangleAboveLeftTriangle :: ValidChild Content parent
                               => ChildHXML parent
rightTriangleAboveLeftTriangle = Tag_Entity Entity.rightTriangleAboveLeftTriangle

-- | The left triangle beside vertical bar HTML entity ('⧏').
leftTriangleBesideVerticalBar :: ValidChild Content parent
                              => ChildHXML parent
leftTriangleBesideVerticalBar = Tag_Entity Entity.leftTriangleBesideVerticalBar

-- | The vertical bar beside right triangle HTML entity ('⧐').
verticalBarBesideRightTriangle :: ValidChild Content parent
                               => ChildHXML parent
verticalBarBesideRightTriangle = Tag_Entity Entity.verticalBarBesideRightTriangle

-- | The left double wiggly fence HTML entity ('⧚').
leftDoubleWigglyFence :: ValidChild Content parent
                      => ChildHXML parent
leftDoubleWigglyFence = Tag_Entity Entity.leftDoubleWigglyFence

-- | The incomplete infinity HTML entity ('⧜').
incompleteInfinity :: ValidChild Content parent
                   => ChildHXML parent
incompleteInfinity = Tag_Entity Entity.incompleteInfinity

-- | The tie over infinity HTML entity ('⧝').
tieOverInfinity :: ValidChild Content parent
                => ChildHXML parent
tieOverInfinity = Tag_Entity Entity.tieOverInfinity

-- | The infinity negated with vertical bar HTML entity ('⧞').
infinityNegatedWithVerticalBar :: ValidChild Content parent
                               => ChildHXML parent
infinityNegatedWithVerticalBar = Tag_Entity Entity.infinityNegatedWithVerticalBar

-- | The square with contoured outline HTML entity ('⧠').
squareWithContouredOutline :: ValidChild Content parent
                           => ChildHXML parent
squareWithContouredOutline = Tag_Entity Entity.squareWithContouredOutline

-- | The equals sign and slanted parallel HTML entity ('⧣').
equalsSignAndSlantedParallel :: ValidChild Content parent
                             => ChildHXML parent
equalsSignAndSlantedParallel = Tag_Entity Entity.equalsSignAndSlantedParallel

-- | The equals sign and slanted parallel with tilde above HTML entity ('⧤').
equalsSignAndSlantedParallelWithTildeAbove :: ValidChild Content parent
                                           => ChildHXML parent
equalsSignAndSlantedParallelWithTildeAbove = Tag_Entity Entity.equalsSignAndSlantedParallelWithTildeAbove

-- | The identical to and slanted parallel HTML entity ('⧥').
identicalToAndSlantedParallel :: ValidChild Content parent
                              => ChildHXML parent
identicalToAndSlantedParallel = Tag_Entity Entity.identicalToAndSlantedParallel

-- | The Down-pointing Triangle With Left Half Black HTML entity ('⧨').
downPointingTriangleWithLeftHalfBlack :: ValidChild Content parent
                                      => ChildHXML parent
downPointingTriangleWithLeftHalfBlack = Tag_Entity Entity.downPointingTriangleWithLeftHalfBlack

-- | The Down-pointing Triangle With Right Half Black HTML entity ('⧩').
downPointingTriangleWithRightHalfBlack :: ValidChild Content parent
                                       => ChildHXML parent
downPointingTriangleWithRightHalfBlack = Tag_Entity Entity.downPointingTriangleWithRightHalfBlack

-- | The black lozenge HTML entity ('⧫').
blackLozenge :: ValidChild Content parent
             => ChildHXML parent
blackLozenge = Tag_Entity Entity.blackLozenge

-- | The error-barred white square HTML entity ('⧮').
errorBarredWhiteSquare :: ValidChild Content parent
                       => ChildHXML parent
errorBarredWhiteSquare = Tag_Entity Entity.errorBarredWhiteSquare

-- | The error-barred black square HTML entity ('⧯').
errorBarredBlackSquare :: ValidChild Content parent
                       => ChildHXML parent
errorBarredBlackSquare = Tag_Entity Entity.errorBarredBlackSquare

-- | The Error-barred White Diamond HTML entity ('⧰').
errorBarredWhiteDiamond :: ValidChild Content parent
                        => ChildHXML parent
errorBarredWhiteDiamond = Tag_Entity Entity.errorBarredWhiteDiamond

-- | The Error-barred Black Diamond HTML entity ('⧱').
errorBarredBlackDiamond :: ValidChild Content parent
                        => ChildHXML parent
errorBarredBlackDiamond = Tag_Entity Entity.errorBarredBlackDiamond

-- | The Error-barred White Circle HTML entity ('⧲').
errorBarredWhiteCircle :: ValidChild Content parent
                       => ChildHXML parent
errorBarredWhiteCircle = Tag_Entity Entity.errorBarredWhiteCircle

-- | The Error-barred Black Circle HTML entity ('⧳').
errorBarredBlackCircle :: ValidChild Content parent
                       => ChildHXML parent
errorBarredBlackCircle = Tag_Entity Entity.errorBarredBlackCircle

-- | The rule-delayed HTML entity ('⧴').
ruleDelayed :: ValidChild Content parent
            => ChildHXML parent
ruleDelayed = Tag_Entity Entity.ruleDelayed

-- | The solidus with overbar HTML entity ('⧶').
solidusWithOverbar :: ValidChild Content parent
                   => ChildHXML parent
solidusWithOverbar = Tag_Entity Entity.solidusWithOverbar

-- | The Double Plus HTML entity ('⧺').
doublePlus :: ValidChild Content parent
           => ChildHXML parent
doublePlus = Tag_Entity Entity.doublePlus

-- | The Triple Plus HTML entity ('⧻').
triplePlus :: ValidChild Content parent
           => ChildHXML parent
triplePlus = Tag_Entity Entity.triplePlus

-- | The n-ary circled dot operator HTML entity ('⨀').
nAryCircledDotOperator :: ValidChild Content parent
                       => ChildHXML parent
nAryCircledDotOperator = Tag_Entity Entity.nAryCircledDotOperator

-- | The n-ary circled plus operator HTML entity ('⨁').
nAryCircledPlusOperator :: ValidChild Content parent
                        => ChildHXML parent
nAryCircledPlusOperator = Tag_Entity Entity.nAryCircledPlusOperator

-- | The n-ary circled times operator HTML entity ('⨂').
nAryCircledTimesOperator :: ValidChild Content parent
                         => ChildHXML parent
nAryCircledTimesOperator = Tag_Entity Entity.nAryCircledTimesOperator

-- | The n-ary union operator with plus HTML entity ('⨄').
nAryUnionOperatorWithPlus :: ValidChild Content parent
                          => ChildHXML parent
nAryUnionOperatorWithPlus = Tag_Entity Entity.nAryUnionOperatorWithPlus

-- | The N-ary Square Intersection Operator HTML entity ('⨅').
nArySquareIntersectionOperator :: ValidChild Content parent
                               => ChildHXML parent
nArySquareIntersectionOperator = Tag_Entity Entity.nArySquareIntersectionOperator

-- | The n-ary square union operator HTML entity ('⨆').
nArySquareUnionOperator :: ValidChild Content parent
                        => ChildHXML parent
nArySquareUnionOperator = Tag_Entity Entity.nArySquareUnionOperator

-- | The Summation With Integral HTML entity ('⨋').
summationWithIntegral :: ValidChild Content parent
                      => ChildHXML parent
summationWithIntegral = Tag_Entity Entity.summationWithIntegral

-- | The quadruple integral operator HTML entity ('⨌').
quadrupleIntegralOperator :: ValidChild Content parent
                          => ChildHXML parent
quadrupleIntegralOperator = Tag_Entity Entity.quadrupleIntegralOperator

-- | The finite part integral HTML entity ('⨍').
finitePartIntegral :: ValidChild Content parent
                   => ChildHXML parent
finitePartIntegral = Tag_Entity Entity.finitePartIntegral

-- | The Integral With Double Stroke HTML entity ('⨎').
integralWithDoubleStroke :: ValidChild Content parent
                         => ChildHXML parent
integralWithDoubleStroke = Tag_Entity Entity.integralWithDoubleStroke

-- | The Integral Average With Slash HTML entity ('⨏').
integralAverageWithSlash :: ValidChild Content parent
                         => ChildHXML parent
integralAverageWithSlash = Tag_Entity Entity.integralAverageWithSlash

-- | The circulation function HTML entity ('⨐').
circulationFunction :: ValidChild Content parent
                    => ChildHXML parent
circulationFunction = Tag_Entity Entity.circulationFunction

-- | The anticlockwise integration HTML entity ('⨑').
anticlockwiseIntegration :: ValidChild Content parent
                         => ChildHXML parent
anticlockwiseIntegration = Tag_Entity Entity.anticlockwiseIntegration

-- | The line integration with rectangular path around pole HTML entity ('⨒').
lineIntegrationWithRectangularPathAroundPole :: ValidChild Content parent
                                             => ChildHXML parent
lineIntegrationWithRectangularPathAroundPole = Tag_Entity Entity.lineIntegrationWithRectangularPathAroundPole

-- | The line integration with semicircular path around pole HTML entity ('⨓').
lineIntegrationWithSemicircularPathAroundPole :: ValidChild Content parent
                                              => ChildHXML parent
lineIntegrationWithSemicircularPathAroundPole = Tag_Entity Entity.lineIntegrationWithSemicircularPathAroundPole

-- | The line integration not including the pole HTML entity ('⨔').
lineIntegrationNotIncludingThePole :: ValidChild Content parent
                                   => ChildHXML parent
lineIntegrationNotIncludingThePole = Tag_Entity Entity.lineIntegrationNotIncludingThePole

-- | The integral around a point operator HTML entity ('⨕').
integralAroundAPointOperator :: ValidChild Content parent
                             => ChildHXML parent
integralAroundAPointOperator = Tag_Entity Entity.integralAroundAPointOperator

-- | The quaternion integral operator HTML entity ('⨖').
quaternionIntegralOperator :: ValidChild Content parent
                           => ChildHXML parent
quaternionIntegralOperator = Tag_Entity Entity.quaternionIntegralOperator

-- | The integral with leftwards arrow with hook HTML entity ('⨗').
integralWithLeftwardsArrowWithHook :: ValidChild Content parent
                                   => ChildHXML parent
integralWithLeftwardsArrowWithHook = Tag_Entity Entity.integralWithLeftwardsArrowWithHook

-- | The Integral With Times Sign HTML entity ('⨘').
integralWithTimesSign :: ValidChild Content parent
                      => ChildHXML parent
integralWithTimesSign = Tag_Entity Entity.integralWithTimesSign

-- | The Integral With Intersection HTML entity ('⨙').
integralWithIntersection :: ValidChild Content parent
                         => ChildHXML parent
integralWithIntersection = Tag_Entity Entity.integralWithIntersection

-- | The Integral With Union HTML entity ('⨚').
integralWithUnion :: ValidChild Content parent
                  => ChildHXML parent
integralWithUnion = Tag_Entity Entity.integralWithUnion

-- | The Integral With Overbar HTML entity ('⨛').
integralWithOverbar :: ValidChild Content parent
                    => ChildHXML parent
integralWithOverbar = Tag_Entity Entity.integralWithOverbar

-- | The Integral With Underbar HTML entity ('⨜').
integralWithUnderbar :: ValidChild Content parent
                     => ChildHXML parent
integralWithUnderbar = Tag_Entity Entity.integralWithUnderbar

-- | The Large Left Triangle Operator HTML entity ('⨞').
largeLeftTriangleOperator :: ValidChild Content parent
                          => ChildHXML parent
largeLeftTriangleOperator = Tag_Entity Entity.largeLeftTriangleOperator

-- | The plus sign with small circle above HTML entity ('⨢').
plusSignWithSmallCircleAbove :: ValidChild Content parent
                             => ChildHXML parent
plusSignWithSmallCircleAbove = Tag_Entity Entity.plusSignWithSmallCircleAbove

-- | The plus sign with circumflex accent above HTML entity ('⨣').
plusSignWithCircumflexAccentAbove :: ValidChild Content parent
                                  => ChildHXML parent
plusSignWithCircumflexAccentAbove = Tag_Entity Entity.plusSignWithCircumflexAccentAbove

-- | The plus sign with tilde above HTML entity ('⨤').
plusSignWithTildeAbove :: ValidChild Content parent
                       => ChildHXML parent
plusSignWithTildeAbove = Tag_Entity Entity.plusSignWithTildeAbove

-- | The plus sign with dot below HTML entity ('⨥').
plusSignWithDotBelow :: ValidChild Content parent
                     => ChildHXML parent
plusSignWithDotBelow = Tag_Entity Entity.plusSignWithDotBelow

-- | The plus sign with tilde below HTML entity ('⨦').
plusSignWithTildeBelow :: ValidChild Content parent
                       => ChildHXML parent
plusSignWithTildeBelow = Tag_Entity Entity.plusSignWithTildeBelow

-- | The plus sign with subscript two HTML entity ('⨧').
plusSignWithSubscriptTwo :: ValidChild Content parent
                         => ChildHXML parent
plusSignWithSubscriptTwo = Tag_Entity Entity.plusSignWithSubscriptTwo

-- | The Plus Sign With Black Triangle HTML entity ('⨨').
plusSignWithBlackTriangle :: ValidChild Content parent
                          => ChildHXML parent
plusSignWithBlackTriangle = Tag_Entity Entity.plusSignWithBlackTriangle

-- | The minus sign with comma above HTML entity ('⨩').
minusSignWithCommaAbove :: ValidChild Content parent
                        => ChildHXML parent
minusSignWithCommaAbove = Tag_Entity Entity.minusSignWithCommaAbove

-- | The minus sign with dot below HTML entity ('⨪').
minusSignWithDotBelow :: ValidChild Content parent
                      => ChildHXML parent
minusSignWithDotBelow = Tag_Entity Entity.minusSignWithDotBelow

-- | The Minus Sign With Falling Dots HTML entity ('⨫').
minusSignWithFallingDots :: ValidChild Content parent
                         => ChildHXML parent
minusSignWithFallingDots = Tag_Entity Entity.minusSignWithFallingDots

-- | The Minus Sign With Rising Dots HTML entity ('⨬').
minusSignWithRisingDots :: ValidChild Content parent
                        => ChildHXML parent
minusSignWithRisingDots = Tag_Entity Entity.minusSignWithRisingDots

-- | The plus sign in left half circle HTML entity ('⨭').
plusSignInLeftHalfCircle :: ValidChild Content parent
                         => ChildHXML parent
plusSignInLeftHalfCircle = Tag_Entity Entity.plusSignInLeftHalfCircle

-- | The plus sign in right half circle HTML entity ('⨮').
plusSignInRightHalfCircle :: ValidChild Content parent
                          => ChildHXML parent
plusSignInRightHalfCircle = Tag_Entity Entity.plusSignInRightHalfCircle

-- | The vector or cross product HTML entity ('⨯').
vectorOrCrossProduct :: ValidChild Content parent
                     => ChildHXML parent
vectorOrCrossProduct = Tag_Entity Entity.vectorOrCrossProduct

-- | The multiplication sign with dot above HTML entity ('⨰').
multiplicationSignWithDotAbove :: ValidChild Content parent
                               => ChildHXML parent
multiplicationSignWithDotAbove = Tag_Entity Entity.multiplicationSignWithDotAbove

-- | The multiplication sign with underbar HTML entity ('⨱').
multiplicationSignWithUnderbar :: ValidChild Content parent
                               => ChildHXML parent
multiplicationSignWithUnderbar = Tag_Entity Entity.multiplicationSignWithUnderbar

-- | The smash product HTML entity ('⨳').
smashProduct :: ValidChild Content parent
             => ChildHXML parent
smashProduct = Tag_Entity Entity.smashProduct

-- | The multiplication sign in left half circle HTML entity ('⨴').
multiplicationSignInLeftHalfCircle :: ValidChild Content parent
                                   => ChildHXML parent
multiplicationSignInLeftHalfCircle = Tag_Entity Entity.multiplicationSignInLeftHalfCircle

-- | The multiplication sign in right half circle HTML entity ('⨵').
multiplicationSignInRightHalfCircle :: ValidChild Content parent
                                    => ChildHXML parent
multiplicationSignInRightHalfCircle = Tag_Entity Entity.multiplicationSignInRightHalfCircle

-- | The circled multiplication sign with circumflex accent HTML entity ('⨶').
circledMultiplicationSignWithCircumflexAccent :: ValidChild Content parent
                                              => ChildHXML parent
circledMultiplicationSignWithCircumflexAccent = Tag_Entity Entity.circledMultiplicationSignWithCircumflexAccent

-- | The multiplication sign in double circle HTML entity ('⨷').
multiplicationSignInDoubleCircle :: ValidChild Content parent
                                 => ChildHXML parent
multiplicationSignInDoubleCircle = Tag_Entity Entity.multiplicationSignInDoubleCircle

-- | The circled division sign HTML entity ('⨸').
circledDivisionSign :: ValidChild Content parent
                    => ChildHXML parent
circledDivisionSign = Tag_Entity Entity.circledDivisionSign

-- | The plus sign in triangle HTML entity ('⨹').
plusSignInTriangle :: ValidChild Content parent
                   => ChildHXML parent
plusSignInTriangle = Tag_Entity Entity.plusSignInTriangle

-- | The minus sign in triangle HTML entity ('⨺').
minusSignInTriangle :: ValidChild Content parent
                    => ChildHXML parent
minusSignInTriangle = Tag_Entity Entity.minusSignInTriangle

-- | The multiplication sign in triangle HTML entity ('⨻').
multiplicationSignInTriangle :: ValidChild Content parent
                             => ChildHXML parent
multiplicationSignInTriangle = Tag_Entity Entity.multiplicationSignInTriangle

-- | The interior product HTML entity ('⨼').
interiorProduct :: ValidChild Content parent
                => ChildHXML parent
interiorProduct = Tag_Entity Entity.interiorProduct

-- | The amalgamation or coproduct HTML entity ('⨿').
amalgamationOrCoproduct :: ValidChild Content parent
                        => ChildHXML parent
amalgamationOrCoproduct = Tag_Entity Entity.amalgamationOrCoproduct

-- | The intersection with dot HTML entity ('⩀').
intersectionWithDot :: ValidChild Content parent
                    => ChildHXML parent
intersectionWithDot = Tag_Entity Entity.intersectionWithDot

-- | The Union With Minus Sign HTML entity ('⩁').
unionWithMinusSign :: ValidChild Content parent
                   => ChildHXML parent
unionWithMinusSign = Tag_Entity Entity.unionWithMinusSign

-- | The union with overbar HTML entity ('⩂').
unionWithOverbar :: ValidChild Content parent
                 => ChildHXML parent
unionWithOverbar = Tag_Entity Entity.unionWithOverbar

-- | The intersection with overbar HTML entity ('⩃').
intersectionWithOverbar :: ValidChild Content parent
                        => ChildHXML parent
intersectionWithOverbar = Tag_Entity Entity.intersectionWithOverbar

-- | The intersection with logical and HTML entity ('⩄').
intersectionWithLogicalAnd :: ValidChild Content parent
                           => ChildHXML parent
intersectionWithLogicalAnd = Tag_Entity Entity.intersectionWithLogicalAnd

-- | The union with logical or HTML entity ('⩅').
unionWithLogicalOr :: ValidChild Content parent
                   => ChildHXML parent
unionWithLogicalOr = Tag_Entity Entity.unionWithLogicalOr

-- | The union above intersection HTML entity ('⩆').
unionAboveIntersection :: ValidChild Content parent
                       => ChildHXML parent
unionAboveIntersection = Tag_Entity Entity.unionAboveIntersection

-- | The intersection above union HTML entity ('⩇').
intersectionAboveUnion :: ValidChild Content parent
                       => ChildHXML parent
intersectionAboveUnion = Tag_Entity Entity.intersectionAboveUnion

-- | The union above bar above intersection HTML entity ('⩈').
unionAboveBarAboveIntersection :: ValidChild Content parent
                               => ChildHXML parent
unionAboveBarAboveIntersection = Tag_Entity Entity.unionAboveBarAboveIntersection

-- | The intersection above bar above union HTML entity ('⩉').
intersectionAboveBarAboveUnion :: ValidChild Content parent
                               => ChildHXML parent
intersectionAboveBarAboveUnion = Tag_Entity Entity.intersectionAboveBarAboveUnion

-- | The union beside and joined with union HTML entity ('⩊').
unionBesideAndJoinedWithUnion :: ValidChild Content parent
                              => ChildHXML parent
unionBesideAndJoinedWithUnion = Tag_Entity Entity.unionBesideAndJoinedWithUnion

-- | The intersection beside and joined with intersection HTML entity ('⩋').
intersectionBesideAndJoinedWithIntersection :: ValidChild Content parent
                                            => ChildHXML parent
intersectionBesideAndJoinedWithIntersection = Tag_Entity Entity.intersectionBesideAndJoinedWithIntersection

-- | The closed union with serifs HTML entity ('⩌').
closedUnionWithSerifs :: ValidChild Content parent
                      => ChildHXML parent
closedUnionWithSerifs = Tag_Entity Entity.closedUnionWithSerifs

-- | The closed intersection with serifs HTML entity ('⩍').
closedIntersectionWithSerifs :: ValidChild Content parent
                             => ChildHXML parent
closedIntersectionWithSerifs = Tag_Entity Entity.closedIntersectionWithSerifs

-- | The Double Square Intersection HTML entity ('⩎').
doubleSquareIntersection :: ValidChild Content parent
                         => ChildHXML parent
doubleSquareIntersection = Tag_Entity Entity.doubleSquareIntersection

-- | The closed union with serifs and smash product HTML entity ('⩐').
closedUnionWithSerifsAndSmashProduct :: ValidChild Content parent
                                     => ChildHXML parent
closedUnionWithSerifsAndSmashProduct = Tag_Entity Entity.closedUnionWithSerifsAndSmashProduct

-- | The double logical and HTML entity ('⩓').
doubleLogicalAnd :: ValidChild Content parent
                 => ChildHXML parent
doubleLogicalAnd = Tag_Entity Entity.doubleLogicalAnd

-- | The double logical or HTML entity ('⩔').
doubleLogicalOr :: ValidChild Content parent
                => ChildHXML parent
doubleLogicalOr = Tag_Entity Entity.doubleLogicalOr

-- | The two intersecting logical and HTML entity ('⩕').
twoIntersectingLogicalAnd :: ValidChild Content parent
                          => ChildHXML parent
twoIntersectingLogicalAnd = Tag_Entity Entity.twoIntersectingLogicalAnd

-- | The two intersecting logical or HTML entity ('⩖').
twoIntersectingLogicalOr :: ValidChild Content parent
                         => ChildHXML parent
twoIntersectingLogicalOr = Tag_Entity Entity.twoIntersectingLogicalOr

-- | The sloping large or HTML entity ('⩗').
slopingLargeOr :: ValidChild Content parent
               => ChildHXML parent
slopingLargeOr = Tag_Entity Entity.slopingLargeOr

-- | The sloping large and HTML entity ('⩘').
slopingLargeAnd :: ValidChild Content parent
                => ChildHXML parent
slopingLargeAnd = Tag_Entity Entity.slopingLargeAnd

-- | The logical and with middle stem HTML entity ('⩚').
logicalAndWithMiddleStem :: ValidChild Content parent
                         => ChildHXML parent
logicalAndWithMiddleStem = Tag_Entity Entity.logicalAndWithMiddleStem

-- | The logical or with middle stem HTML entity ('⩛').
logicalOrWithMiddleStem :: ValidChild Content parent
                        => ChildHXML parent
logicalOrWithMiddleStem = Tag_Entity Entity.logicalOrWithMiddleStem

-- | The logical and with horizontal dash HTML entity ('⩜').
logicalAndWithHorizontalDash :: ValidChild Content parent
                             => ChildHXML parent
logicalAndWithHorizontalDash = Tag_Entity Entity.logicalAndWithHorizontalDash

-- | The logical or with horizontal dash HTML entity ('⩝').
logicalOrWithHorizontalDash :: ValidChild Content parent
                            => ChildHXML parent
logicalOrWithHorizontalDash = Tag_Entity Entity.logicalOrWithHorizontalDash

-- | The logical and with underbar HTML entity ('⩟').
logicalAndWithUnderbar :: ValidChild Content parent
                       => ChildHXML parent
logicalAndWithUnderbar = Tag_Entity Entity.logicalAndWithUnderbar

-- | The equals sign with dot below HTML entity ('⩦').
equalsSignWithDotBelow :: ValidChild Content parent
                       => ChildHXML parent
equalsSignWithDotBelow = Tag_Entity Entity.equalsSignWithDotBelow

-- | The Identical With Dot Above HTML entity ('⩧').
identicalWithDotAbove :: ValidChild Content parent
                      => ChildHXML parent
identicalWithDotAbove = Tag_Entity Entity.identicalWithDotAbove

-- | The tilde operator with dot above HTML entity ('⩪').
tildeOperatorWithDotAbove :: ValidChild Content parent
                          => ChildHXML parent
tildeOperatorWithDotAbove = Tag_Entity Entity.tildeOperatorWithDotAbove

-- | The Similar Minus Similar HTML entity ('⩬').
similarMinusSimilar :: ValidChild Content parent
                    => ChildHXML parent
similarMinusSimilar = Tag_Entity Entity.similarMinusSimilar

-- | The congruent with dot above HTML entity ('⩭').
congruentWithDotAbove :: ValidChild Content parent
                      => ChildHXML parent
congruentWithDotAbove = Tag_Entity Entity.congruentWithDotAbove

-- | The equals with asterisk HTML entity ('⩮').
equalsWithAsterisk :: ValidChild Content parent
                   => ChildHXML parent
equalsWithAsterisk = Tag_Entity Entity.equalsWithAsterisk

-- | The almost equal to with circumflex accent HTML entity ('⩯').
almostEqualToWithCircumflexAccent :: ValidChild Content parent
                                  => ChildHXML parent
almostEqualToWithCircumflexAccent = Tag_Entity Entity.almostEqualToWithCircumflexAccent

-- | The approximately equal or equal to HTML entity ('⩰').
approximatelyEqualOrEqualTo :: ValidChild Content parent
                            => ChildHXML parent
approximatelyEqualOrEqualTo = Tag_Entity Entity.approximatelyEqualOrEqualTo

-- | The equals sign above plus sign HTML entity ('⩱').
equalsSignAbovePlusSign :: ValidChild Content parent
                        => ChildHXML parent
equalsSignAbovePlusSign = Tag_Entity Entity.equalsSignAbovePlusSign

-- | The plus sign above equals sign HTML entity ('⩲').
plusSignAboveEqualsSign :: ValidChild Content parent
                        => ChildHXML parent
plusSignAboveEqualsSign = Tag_Entity Entity.plusSignAboveEqualsSign

-- | The equals sign above tilde operator HTML entity ('⩳').
equalsSignAboveTildeOperator :: ValidChild Content parent
                             => ChildHXML parent
equalsSignAboveTildeOperator = Tag_Entity Entity.equalsSignAboveTildeOperator

-- | The double colon equal HTML entity ('⩴').
doubleColonEqual :: ValidChild Content parent
                 => ChildHXML parent
doubleColonEqual = Tag_Entity Entity.doubleColonEqual

-- | The two consecutive equals signs HTML entity ('⩵').
twoConsecutiveEqualsSigns :: ValidChild Content parent
                          => ChildHXML parent
twoConsecutiveEqualsSigns = Tag_Entity Entity.twoConsecutiveEqualsSigns

-- | The Three Consecutive Equals Signs HTML entity ('⩶').
threeConsecutiveEqualsSigns :: ValidChild Content parent
                            => ChildHXML parent
threeConsecutiveEqualsSigns = Tag_Entity Entity.threeConsecutiveEqualsSigns

-- | The equals sign with two dots above and two dots below HTML entity ('⩷').
equalsSignWithTwoDotsAboveAndTwoDotsBelow :: ValidChild Content parent
                                          => ChildHXML parent
equalsSignWithTwoDotsAboveAndTwoDotsBelow = Tag_Entity Entity.equalsSignWithTwoDotsAboveAndTwoDotsBelow

-- | The equivalent with four dots above HTML entity ('⩸').
equivalentWithFourDotsAbove :: ValidChild Content parent
                            => ChildHXML parent
equivalentWithFourDotsAbove = Tag_Entity Entity.equivalentWithFourDotsAbove

-- | The less-than with circle inside HTML entity ('⩹').
lessThanWithCircleInside :: ValidChild Content parent
                         => ChildHXML parent
lessThanWithCircleInside = Tag_Entity Entity.lessThanWithCircleInside

-- | The greater-than with circle inside HTML entity ('⩺').
greaterThanWithCircleInside :: ValidChild Content parent
                            => ChildHXML parent
greaterThanWithCircleInside = Tag_Entity Entity.greaterThanWithCircleInside

-- | The less-than with question mark above HTML entity ('⩻').
lessThanWithQuestionMarkAbove :: ValidChild Content parent
                              => ChildHXML parent
lessThanWithQuestionMarkAbove = Tag_Entity Entity.lessThanWithQuestionMarkAbove

-- | The greater-than with question mark above HTML entity ('⩼').
greaterThanWithQuestionMarkAbove :: ValidChild Content parent
                                 => ChildHXML parent
greaterThanWithQuestionMarkAbove = Tag_Entity Entity.greaterThanWithQuestionMarkAbove

-- | The less-than or slanted equal to HTML entity ('⩽').
lessThanOrSlantedEqualTo :: ValidChild Content parent
                         => ChildHXML parent
lessThanOrSlantedEqualTo = Tag_Entity Entity.lessThanOrSlantedEqualTo

-- | The greater-than or slanted equal to HTML entity ('⩾').
greaterThanOrSlantedEqualTo :: ValidChild Content parent
                            => ChildHXML parent
greaterThanOrSlantedEqualTo = Tag_Entity Entity.greaterThanOrSlantedEqualTo

-- | The less-than or slanted equal to with dot inside HTML entity ('⩿').
lessThanOrSlantedEqualToWithDotInside :: ValidChild Content parent
                                      => ChildHXML parent
lessThanOrSlantedEqualToWithDotInside = Tag_Entity Entity.lessThanOrSlantedEqualToWithDotInside

-- | The greater-than or slanted equal to with dot inside HTML entity ('⪀').
greaterThanOrSlantedEqualToWithDotInside :: ValidChild Content parent
                                         => ChildHXML parent
greaterThanOrSlantedEqualToWithDotInside = Tag_Entity Entity.greaterThanOrSlantedEqualToWithDotInside

-- | The less-than or slanted equal to with dot above HTML entity ('⪁').
lessThanOrSlantedEqualToWithDotAbove :: ValidChild Content parent
                                     => ChildHXML parent
lessThanOrSlantedEqualToWithDotAbove = Tag_Entity Entity.lessThanOrSlantedEqualToWithDotAbove

-- | The greater-than or slanted equal to with dot above HTML entity ('⪂').
greaterThanOrSlantedEqualToWithDotAbove :: ValidChild Content parent
                                        => ChildHXML parent
greaterThanOrSlantedEqualToWithDotAbove = Tag_Entity Entity.greaterThanOrSlantedEqualToWithDotAbove

-- | The less-than or slanted equal to with dot above right HTML entity ('⪃').
lessThanOrSlantedEqualToWithDotAboveRight :: ValidChild Content parent
                                          => ChildHXML parent
lessThanOrSlantedEqualToWithDotAboveRight = Tag_Entity Entity.lessThanOrSlantedEqualToWithDotAboveRight

-- | The greater-than or slanted equal to with dot above left HTML entity ('⪄').
greaterThanOrSlantedEqualToWithDotAboveLeft :: ValidChild Content parent
                                            => ChildHXML parent
greaterThanOrSlantedEqualToWithDotAboveLeft = Tag_Entity Entity.greaterThanOrSlantedEqualToWithDotAboveLeft

-- | The less-than or approximate HTML entity ('⪅').
lessThanOrApproximate :: ValidChild Content parent
                      => ChildHXML parent
lessThanOrApproximate = Tag_Entity Entity.lessThanOrApproximate

-- | The greater-than or approximate HTML entity ('⪆').
greaterThanOrApproximate :: ValidChild Content parent
                         => ChildHXML parent
greaterThanOrApproximate = Tag_Entity Entity.greaterThanOrApproximate

-- | The less-than and single-line not equal to HTML entity ('⪇').
lessThanAndSingleLineNotEqualTo :: ValidChild Content parent
                                => ChildHXML parent
lessThanAndSingleLineNotEqualTo = Tag_Entity Entity.lessThanAndSingleLineNotEqualTo

-- | The greater-than and single-line not equal to HTML entity ('⪈').
greaterThanAndSingleLineNotEqualTo :: ValidChild Content parent
                                   => ChildHXML parent
greaterThanAndSingleLineNotEqualTo = Tag_Entity Entity.greaterThanAndSingleLineNotEqualTo

-- | The less-than and not approximate HTML entity ('⪉').
lessThanAndNotApproximate :: ValidChild Content parent
                          => ChildHXML parent
lessThanAndNotApproximate = Tag_Entity Entity.lessThanAndNotApproximate

-- | The greater-than and not approximate HTML entity ('⪊').
greaterThanAndNotApproximate :: ValidChild Content parent
                             => ChildHXML parent
greaterThanAndNotApproximate = Tag_Entity Entity.greaterThanAndNotApproximate

-- | The less-than above double-line equal above greater-than HTML entity ('⪋').
lessThanAboveDoubleLineEqualAboveGreaterThan :: ValidChild Content parent
                                             => ChildHXML parent
lessThanAboveDoubleLineEqualAboveGreaterThan = Tag_Entity Entity.lessThanAboveDoubleLineEqualAboveGreaterThan

-- | The greater-than above double-line equal above less-than HTML entity ('⪌').
greaterThanAboveDoubleLineEqualAboveLessThan :: ValidChild Content parent
                                             => ChildHXML parent
greaterThanAboveDoubleLineEqualAboveLessThan = Tag_Entity Entity.greaterThanAboveDoubleLineEqualAboveLessThan

-- | The less-than above similar or equal HTML entity ('⪍').
lessThanAboveSimilarOrEqual :: ValidChild Content parent
                            => ChildHXML parent
lessThanAboveSimilarOrEqual = Tag_Entity Entity.lessThanAboveSimilarOrEqual

-- | The greater-than above similar or equal HTML entity ('⪎').
greaterThanAboveSimilarOrEqual :: ValidChild Content parent
                               => ChildHXML parent
greaterThanAboveSimilarOrEqual = Tag_Entity Entity.greaterThanAboveSimilarOrEqual

-- | The less-than above similar above greater-than HTML entity ('⪏').
lessThanAboveSimilarAboveGreaterThan :: ValidChild Content parent
                                     => ChildHXML parent
lessThanAboveSimilarAboveGreaterThan = Tag_Entity Entity.lessThanAboveSimilarAboveGreaterThan

-- | The greater-than above similar above less-than HTML entity ('⪐').
greaterThanAboveSimilarAboveLessThan :: ValidChild Content parent
                                     => ChildHXML parent
greaterThanAboveSimilarAboveLessThan = Tag_Entity Entity.greaterThanAboveSimilarAboveLessThan

-- | The less-than above greater-than above double-line equal HTML entity ('⪑').
lessThanAboveGreaterThanAboveDoubleLineEqual :: ValidChild Content parent
                                             => ChildHXML parent
lessThanAboveGreaterThanAboveDoubleLineEqual = Tag_Entity Entity.lessThanAboveGreaterThanAboveDoubleLineEqual

-- | The greater-than above less-than above double-line equal HTML entity ('⪒').
greaterThanAboveLessThanAboveDoubleLineEqual :: ValidChild Content parent
                                             => ChildHXML parent
greaterThanAboveLessThanAboveDoubleLineEqual = Tag_Entity Entity.greaterThanAboveLessThanAboveDoubleLineEqual

-- | The less-than above slanted equal above greater-than above slanted equal HTML entity ('⪓').
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual :: ValidChild Content parent
                                                           => ChildHXML parent
lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual = Tag_Entity Entity.lessThanAboveSlantedEqualAboveGreaterThanAboveSlantedEqual

-- | The greater-than above slanted equal above less-than above slanted equal HTML entity ('⪔').
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual :: ValidChild Content parent
                                                           => ChildHXML parent
greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual = Tag_Entity Entity.greaterThanAboveSlantedEqualAboveLessThanAboveSlantedEqual

-- | The slanted equal to or less-than HTML entity ('⪕').
slantedEqualToOrLessThan :: ValidChild Content parent
                         => ChildHXML parent
slantedEqualToOrLessThan = Tag_Entity Entity.slantedEqualToOrLessThan

-- | The slanted equal to or greater-than HTML entity ('⪖').
slantedEqualToOrGreaterThan :: ValidChild Content parent
                            => ChildHXML parent
slantedEqualToOrGreaterThan = Tag_Entity Entity.slantedEqualToOrGreaterThan

-- | The slanted equal to or less-than with dot inside HTML entity ('⪗').
slantedEqualToOrLessThanWithDotInside :: ValidChild Content parent
                                      => ChildHXML parent
slantedEqualToOrLessThanWithDotInside = Tag_Entity Entity.slantedEqualToOrLessThanWithDotInside

-- | The slanted equal to or greater-than with dot inside HTML entity ('⪘').
slantedEqualToOrGreaterThanWithDotInside :: ValidChild Content parent
                                         => ChildHXML parent
slantedEqualToOrGreaterThanWithDotInside = Tag_Entity Entity.slantedEqualToOrGreaterThanWithDotInside

-- | The double-line equal to or less-than HTML entity ('⪙').
doubleLineEqualToOrLessThan :: ValidChild Content parent
                            => ChildHXML parent
doubleLineEqualToOrLessThan = Tag_Entity Entity.doubleLineEqualToOrLessThan

-- | The double-line equal to or greater-than HTML entity ('⪚').
doubleLineEqualToOrGreaterThan :: ValidChild Content parent
                               => ChildHXML parent
doubleLineEqualToOrGreaterThan = Tag_Entity Entity.doubleLineEqualToOrGreaterThan

-- | The Double-line Slanted Equal To Or Greater-than HTML entity ('⪜').
doubleLineSlantedEqualToOrGreaterThan :: ValidChild Content parent
                                      => ChildHXML parent
doubleLineSlantedEqualToOrGreaterThan = Tag_Entity Entity.doubleLineSlantedEqualToOrGreaterThan

-- | The similar or less-than HTML entity ('⪝').
similarOrLessThan :: ValidChild Content parent
                  => ChildHXML parent
similarOrLessThan = Tag_Entity Entity.similarOrLessThan

-- | The similar or greater-than HTML entity ('⪞').
similarOrGreaterThan :: ValidChild Content parent
                     => ChildHXML parent
similarOrGreaterThan = Tag_Entity Entity.similarOrGreaterThan

-- | The similar above less-than above equals sign HTML entity ('⪟').
similarAboveLessThanAboveEqualsSign :: ValidChild Content parent
                                    => ChildHXML parent
similarAboveLessThanAboveEqualsSign = Tag_Entity Entity.similarAboveLessThanAboveEqualsSign

-- | The similar above greater-than above equals sign HTML entity ('⪠').
similarAboveGreaterThanAboveEqualsSign :: ValidChild Content parent
                                       => ChildHXML parent
similarAboveGreaterThanAboveEqualsSign = Tag_Entity Entity.similarAboveGreaterThanAboveEqualsSign

-- | The double nested less-than HTML entity ('⪡').
doubleNestedLessThan :: ValidChild Content parent
                     => ChildHXML parent
doubleNestedLessThan = Tag_Entity Entity.doubleNestedLessThan

-- | The double nested greater-than HTML entity ('⪢').
doubleNestedGreaterThan :: ValidChild Content parent
                        => ChildHXML parent
doubleNestedGreaterThan = Tag_Entity Entity.doubleNestedGreaterThan

-- | The greater-than overlapping less-than HTML entity ('⪤').
greaterThanOverlappingLessThan :: ValidChild Content parent
                               => ChildHXML parent
greaterThanOverlappingLessThan = Tag_Entity Entity.greaterThanOverlappingLessThan

-- | The greater-than beside less-than HTML entity ('⪥').
greaterThanBesideLessThan :: ValidChild Content parent
                          => ChildHXML parent
greaterThanBesideLessThan = Tag_Entity Entity.greaterThanBesideLessThan

-- | The less-than closed by curve HTML entity ('⪦').
lessThanClosedByCurve :: ValidChild Content parent
                      => ChildHXML parent
lessThanClosedByCurve = Tag_Entity Entity.lessThanClosedByCurve

-- | The greater-than closed by curve HTML entity ('⪧').
greaterThanClosedByCurve :: ValidChild Content parent
                         => ChildHXML parent
greaterThanClosedByCurve = Tag_Entity Entity.greaterThanClosedByCurve

-- | The less-than closed by curve above slanted equal HTML entity ('⪨').
lessThanClosedByCurveAboveSlantedEqual :: ValidChild Content parent
                                       => ChildHXML parent
lessThanClosedByCurveAboveSlantedEqual = Tag_Entity Entity.lessThanClosedByCurveAboveSlantedEqual

-- | The greater-than closed by curve above slanted equal HTML entity ('⪩').
greaterThanClosedByCurveAboveSlantedEqual :: ValidChild Content parent
                                          => ChildHXML parent
greaterThanClosedByCurveAboveSlantedEqual = Tag_Entity Entity.greaterThanClosedByCurveAboveSlantedEqual

-- | The smaller than HTML entity ('⪪').
smallerThan :: ValidChild Content parent
            => ChildHXML parent
smallerThan = Tag_Entity Entity.smallerThan

-- | The larger than HTML entity ('⪫').
largerThan :: ValidChild Content parent
           => ChildHXML parent
largerThan = Tag_Entity Entity.largerThan

-- | The smaller than or equal to HTML entity ('⪬').
smallerThanOrEqualTo :: ValidChild Content parent
                     => ChildHXML parent
smallerThanOrEqualTo = Tag_Entity Entity.smallerThanOrEqualTo

-- | The larger than or equal to HTML entity ('⪭').
largerThanOrEqualTo :: ValidChild Content parent
                    => ChildHXML parent
largerThanOrEqualTo = Tag_Entity Entity.largerThanOrEqualTo

-- | The equals sign with bumpy above HTML entity ('⪮').
equalsSignWithBumpyAbove :: ValidChild Content parent
                         => ChildHXML parent
equalsSignWithBumpyAbove = Tag_Entity Entity.equalsSignWithBumpyAbove

-- | The precedes above single-line equals sign HTML entity ('⪯').
precedesAboveSingleLineEqualsSign :: ValidChild Content parent
                                  => ChildHXML parent
precedesAboveSingleLineEqualsSign = Tag_Entity Entity.precedesAboveSingleLineEqualsSign

-- | The succeeds above single-line equals sign HTML entity ('⪰').
succeedsAboveSingleLineEqualsSign :: ValidChild Content parent
                                  => ChildHXML parent
succeedsAboveSingleLineEqualsSign = Tag_Entity Entity.succeedsAboveSingleLineEqualsSign

-- | The Precedes Above Single-line Not Equal To HTML entity ('⪱').
precedesAboveSingleLineNotEqualTo :: ValidChild Content parent
                                  => ChildHXML parent
precedesAboveSingleLineNotEqualTo = Tag_Entity Entity.precedesAboveSingleLineNotEqualTo

-- | The Succeeds Above Single-line Not Equal To HTML entity ('⪲').
succeedsAboveSingleLineNotEqualTo :: ValidChild Content parent
                                  => ChildHXML parent
succeedsAboveSingleLineNotEqualTo = Tag_Entity Entity.succeedsAboveSingleLineNotEqualTo

-- | The precedes above equals sign HTML entity ('⪳').
precedesAboveEqualsSign :: ValidChild Content parent
                        => ChildHXML parent
precedesAboveEqualsSign = Tag_Entity Entity.precedesAboveEqualsSign

-- | The succeeds above equals sign HTML entity ('⪴').
succeedsAboveEqualsSign :: ValidChild Content parent
                        => ChildHXML parent
succeedsAboveEqualsSign = Tag_Entity Entity.succeedsAboveEqualsSign

-- | The precedes above not equal to HTML entity ('⪵').
precedesAboveNotEqualTo :: ValidChild Content parent
                        => ChildHXML parent
precedesAboveNotEqualTo = Tag_Entity Entity.precedesAboveNotEqualTo

-- | The succeeds above not equal to HTML entity ('⪶').
succeedsAboveNotEqualTo :: ValidChild Content parent
                        => ChildHXML parent
succeedsAboveNotEqualTo = Tag_Entity Entity.succeedsAboveNotEqualTo

-- | The precedes above almost equal to HTML entity ('⪷').
precedesAboveAlmostEqualTo :: ValidChild Content parent
                           => ChildHXML parent
precedesAboveAlmostEqualTo = Tag_Entity Entity.precedesAboveAlmostEqualTo

-- | The succeeds above almost equal to HTML entity ('⪸').
succeedsAboveAlmostEqualTo :: ValidChild Content parent
                           => ChildHXML parent
succeedsAboveAlmostEqualTo = Tag_Entity Entity.succeedsAboveAlmostEqualTo

-- | The precedes above not almost equal to HTML entity ('⪹').
precedesAboveNotAlmostEqualTo :: ValidChild Content parent
                              => ChildHXML parent
precedesAboveNotAlmostEqualTo = Tag_Entity Entity.precedesAboveNotAlmostEqualTo

-- | The succeeds above not almost equal to HTML entity ('⪺').
succeedsAboveNotAlmostEqualTo :: ValidChild Content parent
                              => ChildHXML parent
succeedsAboveNotAlmostEqualTo = Tag_Entity Entity.succeedsAboveNotAlmostEqualTo

-- | The double precedes HTML entity ('⪻').
doublePrecedes :: ValidChild Content parent
               => ChildHXML parent
doublePrecedes = Tag_Entity Entity.doublePrecedes

-- | The double succeeds HTML entity ('⪼').
doubleSucceeds :: ValidChild Content parent
               => ChildHXML parent
doubleSucceeds = Tag_Entity Entity.doubleSucceeds

-- | The subset with dot HTML entity ('⪽').
subsetWithDot :: ValidChild Content parent
              => ChildHXML parent
subsetWithDot = Tag_Entity Entity.subsetWithDot

-- | The superset with dot HTML entity ('⪾').
supersetWithDot :: ValidChild Content parent
                => ChildHXML parent
supersetWithDot = Tag_Entity Entity.supersetWithDot

-- | The subset with plus sign below HTML entity ('⪿').
subsetWithPlusSignBelow :: ValidChild Content parent
                        => ChildHXML parent
subsetWithPlusSignBelow = Tag_Entity Entity.subsetWithPlusSignBelow

-- | The superset with plus sign below HTML entity ('⫀').
supersetWithPlusSignBelow :: ValidChild Content parent
                          => ChildHXML parent
supersetWithPlusSignBelow = Tag_Entity Entity.supersetWithPlusSignBelow

-- | The subset with multiplication sign below HTML entity ('⫁').
subsetWithMultiplicationSignBelow :: ValidChild Content parent
                                  => ChildHXML parent
subsetWithMultiplicationSignBelow = Tag_Entity Entity.subsetWithMultiplicationSignBelow

-- | The superset with multiplication sign below HTML entity ('⫂').
supersetWithMultiplicationSignBelow :: ValidChild Content parent
                                    => ChildHXML parent
supersetWithMultiplicationSignBelow = Tag_Entity Entity.supersetWithMultiplicationSignBelow

-- | The subset of or equal to with dot above HTML entity ('⫃').
subsetOfOrEqualToWithDotAbove :: ValidChild Content parent
                              => ChildHXML parent
subsetOfOrEqualToWithDotAbove = Tag_Entity Entity.subsetOfOrEqualToWithDotAbove

-- | The superset of or equal to with dot above HTML entity ('⫄').
supersetOfOrEqualToWithDotAbove :: ValidChild Content parent
                                => ChildHXML parent
supersetOfOrEqualToWithDotAbove = Tag_Entity Entity.supersetOfOrEqualToWithDotAbove

-- | The subset of above equals sign HTML entity ('⫅').
subsetOfAboveEqualsSign :: ValidChild Content parent
                        => ChildHXML parent
subsetOfAboveEqualsSign = Tag_Entity Entity.subsetOfAboveEqualsSign

-- | The superset of above equals sign HTML entity ('⫆').
supersetOfAboveEqualsSign :: ValidChild Content parent
                          => ChildHXML parent
supersetOfAboveEqualsSign = Tag_Entity Entity.supersetOfAboveEqualsSign

-- | The subset of above tilde operator HTML entity ('⫇').
subsetOfAboveTildeOperator :: ValidChild Content parent
                           => ChildHXML parent
subsetOfAboveTildeOperator = Tag_Entity Entity.subsetOfAboveTildeOperator

-- | The superset of above tilde operator HTML entity ('⫈').
supersetOfAboveTildeOperator :: ValidChild Content parent
                             => ChildHXML parent
supersetOfAboveTildeOperator = Tag_Entity Entity.supersetOfAboveTildeOperator

-- | The Subset Of Above Almost Equal To HTML entity ('⫉').
subsetOfAboveAlmostEqualTo :: ValidChild Content parent
                           => ChildHXML parent
subsetOfAboveAlmostEqualTo = Tag_Entity Entity.subsetOfAboveAlmostEqualTo

-- | The Superset Of Above Almost Equal To HTML entity ('⫊').
supersetOfAboveAlmostEqualTo :: ValidChild Content parent
                             => ChildHXML parent
supersetOfAboveAlmostEqualTo = Tag_Entity Entity.supersetOfAboveAlmostEqualTo

-- | The subset of above not equal to HTML entity ('⫋').
subsetOfAboveNotEqualTo :: ValidChild Content parent
                        => ChildHXML parent
subsetOfAboveNotEqualTo = Tag_Entity Entity.subsetOfAboveNotEqualTo

-- | The superset of above not equal to HTML entity ('⫌').
supersetOfAboveNotEqualTo :: ValidChild Content parent
                          => ChildHXML parent
supersetOfAboveNotEqualTo = Tag_Entity Entity.supersetOfAboveNotEqualTo

-- | The closed subset HTML entity ('⫏').
closedSubset :: ValidChild Content parent
             => ChildHXML parent
closedSubset = Tag_Entity Entity.closedSubset

-- | The closed superset HTML entity ('⫐').
closedSuperset :: ValidChild Content parent
               => ChildHXML parent
closedSuperset = Tag_Entity Entity.closedSuperset

-- | The closed subset or equal to HTML entity ('⫑').
closedSubsetOrEqualTo :: ValidChild Content parent
                      => ChildHXML parent
closedSubsetOrEqualTo = Tag_Entity Entity.closedSubsetOrEqualTo

-- | The closed superset or equal to HTML entity ('⫒').
closedSupersetOrEqualTo :: ValidChild Content parent
                        => ChildHXML parent
closedSupersetOrEqualTo = Tag_Entity Entity.closedSupersetOrEqualTo

-- | The subset above superset HTML entity ('⫓').
subsetAboveSuperset :: ValidChild Content parent
                    => ChildHXML parent
subsetAboveSuperset = Tag_Entity Entity.subsetAboveSuperset

-- | The superset above subset HTML entity ('⫔').
supersetAboveSubset :: ValidChild Content parent
                    => ChildHXML parent
supersetAboveSubset = Tag_Entity Entity.supersetAboveSubset

-- | The subset above subset HTML entity ('⫕').
subsetAboveSubset :: ValidChild Content parent
                  => ChildHXML parent
subsetAboveSubset = Tag_Entity Entity.subsetAboveSubset

-- | The superset above superset HTML entity ('⫖').
supersetAboveSuperset :: ValidChild Content parent
                      => ChildHXML parent
supersetAboveSuperset = Tag_Entity Entity.supersetAboveSuperset

-- | The superset beside subset HTML entity ('⫗').
supersetBesideSubset :: ValidChild Content parent
                     => ChildHXML parent
supersetBesideSubset = Tag_Entity Entity.supersetBesideSubset

-- | The superset beside and joined by dash with subset HTML entity ('⫘').
supersetBesideAndJoinedByDashWithSubset :: ValidChild Content parent
                                        => ChildHXML parent
supersetBesideAndJoinedByDashWithSubset = Tag_Entity Entity.supersetBesideAndJoinedByDashWithSubset

-- | The element of opening downwards HTML entity ('⫙').
elementOfOpeningDownwards :: ValidChild Content parent
                          => ChildHXML parent
elementOfOpeningDownwards = Tag_Entity Entity.elementOfOpeningDownwards

-- | The pitchfork with tee top HTML entity ('⫚').
pitchforkWithTeeTop :: ValidChild Content parent
                    => ChildHXML parent
pitchforkWithTeeTop = Tag_Entity Entity.pitchforkWithTeeTop

-- | The transversal intersection HTML entity ('⫛').
transversalIntersection :: ValidChild Content parent
                        => ChildHXML parent
transversalIntersection = Tag_Entity Entity.transversalIntersection

-- | The vertical bar double left turnstile HTML entity ('⫤').
verticalBarDoubleLeftTurnstile :: ValidChild Content parent
                               => ChildHXML parent
verticalBarDoubleLeftTurnstile = Tag_Entity Entity.verticalBarDoubleLeftTurnstile

-- | The long dash from left member of double vertical HTML entity ('⫦').
longDashFromLeftMemberOfDoubleVertical :: ValidChild Content parent
                                       => ChildHXML parent
longDashFromLeftMemberOfDoubleVertical = Tag_Entity Entity.longDashFromLeftMemberOfDoubleVertical

-- | The short down tack with overbar HTML entity ('⫧').
shortDownTackWithOverbar :: ValidChild Content parent
                         => ChildHXML parent
shortDownTackWithOverbar = Tag_Entity Entity.shortDownTackWithOverbar

-- | The short up tack with underbar HTML entity ('⫨').
shortUpTackWithUnderbar :: ValidChild Content parent
                        => ChildHXML parent
shortUpTackWithUnderbar = Tag_Entity Entity.shortUpTackWithUnderbar

-- | The short up tack above short down tack HTML entity ('⫩').
shortUpTackAboveShortDownTack :: ValidChild Content parent
                              => ChildHXML parent
shortUpTackAboveShortDownTack = Tag_Entity Entity.shortUpTackAboveShortDownTack

-- | The double up tack HTML entity ('⫫').
doubleUpTack :: ValidChild Content parent
             => ChildHXML parent
doubleUpTack = Tag_Entity Entity.doubleUpTack

-- | The double stroke not sign HTML entity ('⫬').
doubleStrokeNotSign :: ValidChild Content parent
                    => ChildHXML parent
doubleStrokeNotSign = Tag_Entity Entity.doubleStrokeNotSign

-- | The reversed double stroke not sign HTML entity ('⫭').
reversedDoubleStrokeNotSign :: ValidChild Content parent
                            => ChildHXML parent
reversedDoubleStrokeNotSign = Tag_Entity Entity.reversedDoubleStrokeNotSign

-- | The does not divide with reversed negation slash HTML entity ('⫮').
doesNotDivideWithReversedNegationSlash :: ValidChild Content parent
                                       => ChildHXML parent
doesNotDivideWithReversedNegationSlash = Tag_Entity Entity.doesNotDivideWithReversedNegationSlash

-- | The vertical line with circle above HTML entity ('⫯').
verticalLineWithCircleAbove :: ValidChild Content parent
                            => ChildHXML parent
verticalLineWithCircleAbove = Tag_Entity Entity.verticalLineWithCircleAbove

-- | The vertical line with circle below HTML entity ('⫰').
verticalLineWithCircleBelow :: ValidChild Content parent
                            => ChildHXML parent
verticalLineWithCircleBelow = Tag_Entity Entity.verticalLineWithCircleBelow

-- | The down tack with circle below HTML entity ('⫱').
downTackWithCircleBelow :: ValidChild Content parent
                        => ChildHXML parent
downTackWithCircleBelow = Tag_Entity Entity.downTackWithCircleBelow

-- | The parallel with horizontal stroke HTML entity ('⫲').
parallelWithHorizontalStroke :: ValidChild Content parent
                             => ChildHXML parent
parallelWithHorizontalStroke = Tag_Entity Entity.parallelWithHorizontalStroke

-- | The parallel with tilde operator HTML entity ('⫳').
parallelWithTildeOperator :: ValidChild Content parent
                          => ChildHXML parent
parallelWithTildeOperator = Tag_Entity Entity.parallelWithTildeOperator

-- | The Triple Nested Greater-than HTML entity ('⫸').
tripleNestedGreaterThan :: ValidChild Content parent
                        => ChildHXML parent
tripleNestedGreaterThan = Tag_Entity Entity.tripleNestedGreaterThan

-- | The Double-line Slanted Greater-than Or Equal To HTML entity ('⫺').
doubleLineSlantedGreaterThanOrEqualTo :: ValidChild Content parent
                                      => ChildHXML parent
doubleLineSlantedGreaterThanOrEqualTo = Tag_Entity Entity.doubleLineSlantedGreaterThanOrEqualTo

-- | The double solidus operator HTML entity ('⫽').
doubleSolidusOperator :: ValidChild Content parent
                      => ChildHXML parent
doubleSolidusOperator = Tag_Entity Entity.doubleSolidusOperator

-- | The square with top half black HTML entity ('⬒').
squareWithTopHalfBlack :: ValidChild Content parent
                       => ChildHXML parent
squareWithTopHalfBlack = Tag_Entity Entity.squareWithTopHalfBlack

-- | The square with bottom half black HTML entity ('⬓').
squareWithBottomHalfBlack :: ValidChild Content parent
                          => ChildHXML parent
squareWithBottomHalfBlack = Tag_Entity Entity.squareWithBottomHalfBlack

-- | The square with upper right diagonal half black HTML entity ('⬔').
squareWithUpperRightDiagonalHalfBlack :: ValidChild Content parent
                                      => ChildHXML parent
squareWithUpperRightDiagonalHalfBlack = Tag_Entity Entity.squareWithUpperRightDiagonalHalfBlack

-- | The square with lower left diagonal half black HTML entity ('⬕').
squareWithLowerLeftDiagonalHalfBlack :: ValidChild Content parent
                                     => ChildHXML parent
squareWithLowerLeftDiagonalHalfBlack = Tag_Entity Entity.squareWithLowerLeftDiagonalHalfBlack

-- | The Diamond With Left Half Black HTML entity ('⬖').
diamondWithLeftHalfBlack :: ValidChild Content parent
                         => ChildHXML parent
diamondWithLeftHalfBlack = Tag_Entity Entity.diamondWithLeftHalfBlack

-- | The Diamond With Right Half Black HTML entity ('⬗').
diamondWithRightHalfBlack :: ValidChild Content parent
                          => ChildHXML parent
diamondWithRightHalfBlack = Tag_Entity Entity.diamondWithRightHalfBlack

-- | The Diamond With Top Half Black HTML entity ('⬘').
diamondWithTopHalfBlack :: ValidChild Content parent
                        => ChildHXML parent
diamondWithTopHalfBlack = Tag_Entity Entity.diamondWithTopHalfBlack

-- | The Diamond With Bottom Half Black HTML entity ('⬙').
diamondWithBottomHalfBlack :: ValidChild Content parent
                           => ChildHXML parent
diamondWithBottomHalfBlack = Tag_Entity Entity.diamondWithBottomHalfBlack

-- | The dotted square HTML entity ('⬚').
dottedSquare :: ValidChild Content parent
             => ChildHXML parent
dottedSquare = Tag_Entity Entity.dottedSquare

-- | The black large square HTML entity ('⬛').
blackLargeSquare :: ValidChild Content parent
                 => ChildHXML parent
blackLargeSquare = Tag_Entity Entity.blackLargeSquare

-- | The white large square HTML entity ('⬜').
whiteLargeSquare :: ValidChild Content parent
                 => ChildHXML parent
whiteLargeSquare = Tag_Entity Entity.whiteLargeSquare

-- | The black very small square HTML entity ('⬝').
blackVerySmallSquare :: ValidChild Content parent
                     => ChildHXML parent
blackVerySmallSquare = Tag_Entity Entity.blackVerySmallSquare

-- | The white very small square HTML entity ('⬞').
whiteVerySmallSquare :: ValidChild Content parent
                     => ChildHXML parent
whiteVerySmallSquare = Tag_Entity Entity.whiteVerySmallSquare

-- | The Black Pentagon HTML entity ('⬟').
blackPentagon :: ValidChild Content parent
              => ChildHXML parent
blackPentagon = Tag_Entity Entity.blackPentagon

-- | The White Pentagon HTML entity ('⬠').
whitePentagon :: ValidChild Content parent
              => ChildHXML parent
whitePentagon = Tag_Entity Entity.whitePentagon

-- | The White Hexagon HTML entity ('⬡').
whiteHexagon :: ValidChild Content parent
             => ChildHXML parent
whiteHexagon = Tag_Entity Entity.whiteHexagon

-- | The Black Hexagon HTML entity ('⬢').
blackHexagon :: ValidChild Content parent
             => ChildHXML parent
blackHexagon = Tag_Entity Entity.blackHexagon

-- | The Horizontal Black Hexagon HTML entity ('⬣').
horizontalBlackHexagon :: ValidChild Content parent
                       => ChildHXML parent
horizontalBlackHexagon = Tag_Entity Entity.horizontalBlackHexagon

-- | The Black Large Circle HTML entity ('⬤').
blackLargeCircle :: ValidChild Content parent
                 => ChildHXML parent
blackLargeCircle = Tag_Entity Entity.blackLargeCircle

-- | The Black Medium Diamond HTML entity ('⬥').
blackMediumDiamond :: ValidChild Content parent
                   => ChildHXML parent
blackMediumDiamond = Tag_Entity Entity.blackMediumDiamond

-- | The White Medium Diamond HTML entity ('⬦').
whiteMediumDiamond :: ValidChild Content parent
                   => ChildHXML parent
whiteMediumDiamond = Tag_Entity Entity.whiteMediumDiamond

-- | The Black Medium Lozenge HTML entity ('⬧').
blackMediumLozenge :: ValidChild Content parent
                   => ChildHXML parent
blackMediumLozenge = Tag_Entity Entity.blackMediumLozenge

-- | The White Medium Lozenge HTML entity ('⬨').
whiteMediumLozenge :: ValidChild Content parent
                   => ChildHXML parent
whiteMediumLozenge = Tag_Entity Entity.whiteMediumLozenge

-- | The Black Small Diamond HTML entity ('⬩').
blackSmallDiamond :: ValidChild Content parent
                  => ChildHXML parent
blackSmallDiamond = Tag_Entity Entity.blackSmallDiamond

-- | The Black Small Lozenge HTML entity ('⬪').
blackSmallLozenge :: ValidChild Content parent
                  => ChildHXML parent
blackSmallLozenge = Tag_Entity Entity.blackSmallLozenge

-- | The White Small Lozenge HTML entity ('⬫').
whiteSmallLozenge :: ValidChild Content parent
                  => ChildHXML parent
whiteSmallLozenge = Tag_Entity Entity.whiteSmallLozenge

-- | The Black Horizontal Ellipse HTML entity ('⬬').
blackHorizontalEllipse :: ValidChild Content parent
                       => ChildHXML parent
blackHorizontalEllipse = Tag_Entity Entity.blackHorizontalEllipse

-- | The White Horizontal Ellipse HTML entity ('⬭').
whiteHorizontalEllipse :: ValidChild Content parent
                       => ChildHXML parent
whiteHorizontalEllipse = Tag_Entity Entity.whiteHorizontalEllipse

-- | The Black Vertical Ellipse HTML entity ('⬮').
blackVerticalEllipse :: ValidChild Content parent
                     => ChildHXML parent
blackVerticalEllipse = Tag_Entity Entity.blackVerticalEllipse

-- | The White Vertical Ellipse HTML entity ('⬯').
whiteVerticalEllipse :: ValidChild Content parent
                     => ChildHXML parent
whiteVerticalEllipse = Tag_Entity Entity.whiteVerticalEllipse

-- | The Equals Sign Above Leftwards Arrow HTML entity ('⭀').
equalsSignAboveLeftwardsArrow :: ValidChild Content parent
                              => ChildHXML parent
equalsSignAboveLeftwardsArrow = Tag_Entity Entity.equalsSignAboveLeftwardsArrow

-- | The Leftwards Arrow Above Reverse Almost Equal To HTML entity ('⭂').
leftwardsArrowAboveReverseAlmostEqualTo :: ValidChild Content parent
                                        => ChildHXML parent
leftwardsArrowAboveReverseAlmostEqualTo = Tag_Entity Entity.leftwardsArrowAboveReverseAlmostEqualTo

-- | The Rightwards Arrow Through Greater-than HTML entity ('⭃').
rightwardsArrowThroughGreaterThan :: ValidChild Content parent
                                  => ChildHXML parent
rightwardsArrowThroughGreaterThan = Tag_Entity Entity.rightwardsArrowThroughGreaterThan

-- | The Rightwards Arrow Above Reverse Almost Equal To HTML entity ('⭈').
rightwardsArrowAboveReverseAlmostEqualTo :: ValidChild Content parent
                                         => ChildHXML parent
rightwardsArrowAboveReverseAlmostEqualTo = Tag_Entity Entity.rightwardsArrowAboveReverseAlmostEqualTo

-- | The Leftwards Arrow Above Almost Equal To HTML entity ('⭊').
leftwardsArrowAboveAlmostEqualTo :: ValidChild Content parent
                                 => ChildHXML parent
leftwardsArrowAboveAlmostEqualTo = Tag_Entity Entity.leftwardsArrowAboveAlmostEqualTo

-- | The Black Right-pointing Pentagon HTML entity ('⭓').
blackRightPointingPentagon :: ValidChild Content parent
                           => ChildHXML parent
blackRightPointingPentagon = Tag_Entity Entity.blackRightPointingPentagon

-- | The White Right-pointing Pentagon HTML entity ('⭔').
whiteRightPointingPentagon :: ValidChild Content parent
                           => ChildHXML parent
whiteRightPointingPentagon = Tag_Entity Entity.whiteRightPointingPentagon

-- | The Heavy Large Circle HTML entity ('⭕').
heavyLargeCircle :: ValidChild Content parent
                 => ChildHXML parent
heavyLargeCircle = Tag_Entity Entity.heavyLargeCircle

-- | The black square centred HTML entity ('⯀').
blackSquareCentred :: ValidChild Content parent
                   => ChildHXML parent
blackSquareCentred = Tag_Entity Entity.blackSquareCentred

-- | The Black Diamond Centred HTML entity ('⯁').
blackDiamondCentred :: ValidChild Content parent
                    => ChildHXML parent
blackDiamondCentred = Tag_Entity Entity.blackDiamondCentred

-- | The Turned Black Pentagon HTML entity ('⯂').
turnedBlackPentagon :: ValidChild Content parent
                    => ChildHXML parent
turnedBlackPentagon = Tag_Entity Entity.turnedBlackPentagon

-- | The square position indicator HTML entity ('⯐').
squarePositionIndicator :: ValidChild Content parent
                        => ChildHXML parent
squarePositionIndicator = Tag_Entity Entity.squarePositionIndicator

-- | The Dotted Right-pointing Angle HTML entity ('⸖').
dottedRightPointingAngle :: ValidChild Content parent
                         => ChildHXML parent
dottedRightPointingAngle = Tag_Entity Entity.dottedRightPointingAngle

-- | The Modifier Letter Lower Right Corner Angle HTML entity ('ꜚ').
modifierLetterLowerRightCornerAngle :: ValidChild Content parent
                                    => ChildHXML parent
modifierLetterLowerRightCornerAngle = Tag_Entity Entity.modifierLetterLowerRightCornerAngle

-- | The Modifier Letter Short Equals Sign HTML entity ('꞊').
modifierLetterShortEqualsSign :: ValidChild Content parent
                              => ChildHXML parent
modifierLetterShortEqualsSign = Tag_Entity Entity.modifierLetterShortEqualsSign

-- | The latin small ligature ff HTML entity ('ﬀ').
latinSmallLigatureFf :: ValidChild Content parent
                     => ChildHXML parent
latinSmallLigatureFf = Tag_Entity Entity.latinSmallLigatureFf

-- | The latin small ligature fi HTML entity ('ﬁ').
latinSmallLigatureFi :: ValidChild Content parent
                     => ChildHXML parent
latinSmallLigatureFi = Tag_Entity Entity.latinSmallLigatureFi

-- | The latin small ligature fl HTML entity ('ﬂ').
latinSmallLigatureFl :: ValidChild Content parent
                     => ChildHXML parent
latinSmallLigatureFl = Tag_Entity Entity.latinSmallLigatureFl

-- | The latin small ligature ffi HTML entity ('ﬃ').
latinSmallLigatureFfi :: ValidChild Content parent
                      => ChildHXML parent
latinSmallLigatureFfi = Tag_Entity Entity.latinSmallLigatureFfi

-- | The latin small ligature ffl HTML entity ('ﬄ').
latinSmallLigatureFfl :: ValidChild Content parent
                      => ChildHXML parent
latinSmallLigatureFfl = Tag_Entity Entity.latinSmallLigatureFfl

-- | The Small Plus Sign HTML entity ('﹢').
smallPlusSign :: ValidChild Content parent
              => ChildHXML parent
smallPlusSign = Tag_Entity Entity.smallPlusSign

-- | The Small Hyphen-minus HTML entity ('﹣').
smallHyphenMinus :: ValidChild Content parent
                 => ChildHXML parent
smallHyphenMinus = Tag_Entity Entity.smallHyphenMinus

-- | The Small Greater-than Sign HTML entity ('﹥').
smallGreaterThanSign :: ValidChild Content parent
                     => ChildHXML parent
smallGreaterThanSign = Tag_Entity Entity.smallGreaterThanSign

-- | The Small Equals Sign HTML entity ('﹦').
smallEqualsSign :: ValidChild Content parent
                => ChildHXML parent
smallEqualsSign = Tag_Entity Entity.smallEqualsSign

-- | The Small Percent Sign HTML entity ('﹪').
smallPercentSign :: ValidChild Content parent
                 => ChildHXML parent
smallPercentSign = Tag_Entity Entity.smallPercentSign

-- | The Fullwidth Percent Sign HTML entity ('％').
fullwidthPercentSign :: ValidChild Content parent
                     => ChildHXML parent
fullwidthPercentSign = Tag_Entity Entity.fullwidthPercentSign

-- | The Fullwidth Plus Sign HTML entity ('＋').
fullwidthPlusSign :: ValidChild Content parent
                  => ChildHXML parent
fullwidthPlusSign = Tag_Entity Entity.fullwidthPlusSign

-- | The Fullwidth Hyphen-minus HTML entity ('－').
fullwidthHyphenMinus :: ValidChild Content parent
                     => ChildHXML parent
fullwidthHyphenMinus = Tag_Entity Entity.fullwidthHyphenMinus

-- | The Fullwidth Equals Sign HTML entity ('＝').
fullwidthEqualsSign :: ValidChild Content parent
                    => ChildHXML parent
fullwidthEqualsSign = Tag_Entity Entity.fullwidthEqualsSign

-- | The Fullwidth Greater-than Sign HTML entity ('＞').
fullwidthGreaterThanSign :: ValidChild Content parent
                         => ChildHXML parent
fullwidthGreaterThanSign = Tag_Entity Entity.fullwidthGreaterThanSign

-- | The Ugaritic Letter Delta HTML entity ('𐎄').
ugariticLetterDelta :: ValidChild Content parent
                    => ChildHXML parent
ugariticLetterDelta = Tag_Entity Entity.ugariticLetterDelta

-- | The mathematical script capital a HTML entity ('𝒜').
mathematicalScriptCapitalA :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalA = Tag_Entity Entity.mathematicalScriptCapitalA

-- | The mathematical script capital c HTML entity ('𝒞').
mathematicalScriptCapitalC :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalC = Tag_Entity Entity.mathematicalScriptCapitalC

-- | The mathematical script capital d HTML entity ('𝒟').
mathematicalScriptCapitalD :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalD = Tag_Entity Entity.mathematicalScriptCapitalD

-- | The mathematical script capital g HTML entity ('𝒢').
mathematicalScriptCapitalG :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalG = Tag_Entity Entity.mathematicalScriptCapitalG

-- | The mathematical script capital j HTML entity ('𝒥').
mathematicalScriptCapitalJ :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalJ = Tag_Entity Entity.mathematicalScriptCapitalJ

-- | The mathematical script capital k HTML entity ('𝒦').
mathematicalScriptCapitalK :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalK = Tag_Entity Entity.mathematicalScriptCapitalK

-- | The mathematical script capital n HTML entity ('𝒩').
mathematicalScriptCapitalN :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalN = Tag_Entity Entity.mathematicalScriptCapitalN

-- | The mathematical script capital o HTML entity ('𝒪').
mathematicalScriptCapitalO :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalO = Tag_Entity Entity.mathematicalScriptCapitalO

-- | The mathematical script capital p HTML entity ('𝒫').
mathematicalScriptCapitalP :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalP = Tag_Entity Entity.mathematicalScriptCapitalP

-- | The mathematical script capital q HTML entity ('𝒬').
mathematicalScriptCapitalQ :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalQ = Tag_Entity Entity.mathematicalScriptCapitalQ

-- | The mathematical script capital s HTML entity ('𝒮').
mathematicalScriptCapitalS :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalS = Tag_Entity Entity.mathematicalScriptCapitalS

-- | The mathematical script capital t HTML entity ('𝒯').
mathematicalScriptCapitalT :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalT = Tag_Entity Entity.mathematicalScriptCapitalT

-- | The mathematical script capital u HTML entity ('𝒰').
mathematicalScriptCapitalU :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalU = Tag_Entity Entity.mathematicalScriptCapitalU

-- | The mathematical script capital v HTML entity ('𝒱').
mathematicalScriptCapitalV :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalV = Tag_Entity Entity.mathematicalScriptCapitalV

-- | The mathematical script capital w HTML entity ('𝒲').
mathematicalScriptCapitalW :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalW = Tag_Entity Entity.mathematicalScriptCapitalW

-- | The mathematical script capital x HTML entity ('𝒳').
mathematicalScriptCapitalX :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalX = Tag_Entity Entity.mathematicalScriptCapitalX

-- | The mathematical script capital y HTML entity ('𝒴').
mathematicalScriptCapitalY :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalY = Tag_Entity Entity.mathematicalScriptCapitalY

-- | The mathematical script capital z HTML entity ('𝒵').
mathematicalScriptCapitalZ :: ValidChild Content parent
                           => ChildHXML parent
mathematicalScriptCapitalZ = Tag_Entity Entity.mathematicalScriptCapitalZ

-- | The mathematical script small a HTML entity ('𝒶').
mathematicalScriptSmallA :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallA = Tag_Entity Entity.mathematicalScriptSmallA

-- | The mathematical script small b HTML entity ('𝒷').
mathematicalScriptSmallB :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallB = Tag_Entity Entity.mathematicalScriptSmallB

-- | The mathematical script small c HTML entity ('𝒸').
mathematicalScriptSmallC :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallC = Tag_Entity Entity.mathematicalScriptSmallC

-- | The mathematical script small d HTML entity ('𝒹').
mathematicalScriptSmallD :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallD = Tag_Entity Entity.mathematicalScriptSmallD

-- | The mathematical script small f HTML entity ('𝒻').
mathematicalScriptSmallF :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallF = Tag_Entity Entity.mathematicalScriptSmallF

-- | The mathematical script small h HTML entity ('𝒽').
mathematicalScriptSmallH :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallH = Tag_Entity Entity.mathematicalScriptSmallH

-- | The mathematical script small i HTML entity ('𝒾').
mathematicalScriptSmallI :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallI = Tag_Entity Entity.mathematicalScriptSmallI

-- | The mathematical script small j HTML entity ('𝒿').
mathematicalScriptSmallJ :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallJ = Tag_Entity Entity.mathematicalScriptSmallJ

-- | The mathematical script small k HTML entity ('𝓀').
mathematicalScriptSmallK :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallK = Tag_Entity Entity.mathematicalScriptSmallK

-- | The mathematical script small l HTML entity ('𝓁').
mathematicalScriptSmallL :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallL = Tag_Entity Entity.mathematicalScriptSmallL

-- | The mathematical script small m HTML entity ('𝓂').
mathematicalScriptSmallM :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallM = Tag_Entity Entity.mathematicalScriptSmallM

-- | The mathematical script small n HTML entity ('𝓃').
mathematicalScriptSmallN :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallN = Tag_Entity Entity.mathematicalScriptSmallN

-- | The mathematical script small p HTML entity ('𝓅').
mathematicalScriptSmallP :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallP = Tag_Entity Entity.mathematicalScriptSmallP

-- | The mathematical script small q HTML entity ('𝓆').
mathematicalScriptSmallQ :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallQ = Tag_Entity Entity.mathematicalScriptSmallQ

-- | The mathematical script small r HTML entity ('𝓇').
mathematicalScriptSmallR :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallR = Tag_Entity Entity.mathematicalScriptSmallR

-- | The mathematical script small s HTML entity ('𝓈').
mathematicalScriptSmallS :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallS = Tag_Entity Entity.mathematicalScriptSmallS

-- | The mathematical script small t HTML entity ('𝓉').
mathematicalScriptSmallT :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallT = Tag_Entity Entity.mathematicalScriptSmallT

-- | The mathematical script small u HTML entity ('𝓊').
mathematicalScriptSmallU :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallU = Tag_Entity Entity.mathematicalScriptSmallU

-- | The mathematical script small v HTML entity ('𝓋').
mathematicalScriptSmallV :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallV = Tag_Entity Entity.mathematicalScriptSmallV

-- | The mathematical script small w HTML entity ('𝓌').
mathematicalScriptSmallW :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallW = Tag_Entity Entity.mathematicalScriptSmallW

-- | The mathematical script small x HTML entity ('𝓍').
mathematicalScriptSmallX :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallX = Tag_Entity Entity.mathematicalScriptSmallX

-- | The mathematical script small y HTML entity ('𝓎').
mathematicalScriptSmallY :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallY = Tag_Entity Entity.mathematicalScriptSmallY

-- | The mathematical script small z HTML entity ('𝓏').
mathematicalScriptSmallZ :: ValidChild Content parent
                         => ChildHXML parent
mathematicalScriptSmallZ = Tag_Entity Entity.mathematicalScriptSmallZ

-- | The mathematical fraktur capital a HTML entity ('𝔄').
mathematicalFrakturCapitalA :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalA = Tag_Entity Entity.mathematicalFrakturCapitalA

-- | The mathematical fraktur capital b HTML entity ('𝔅').
mathematicalFrakturCapitalB :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalB = Tag_Entity Entity.mathematicalFrakturCapitalB

-- | The mathematical fraktur capital d HTML entity ('𝔇').
mathematicalFrakturCapitalD :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalD = Tag_Entity Entity.mathematicalFrakturCapitalD

-- | The mathematical fraktur capital e HTML entity ('𝔈').
mathematicalFrakturCapitalE :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalE = Tag_Entity Entity.mathematicalFrakturCapitalE

-- | The mathematical fraktur capital f HTML entity ('𝔉').
mathematicalFrakturCapitalF :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalF = Tag_Entity Entity.mathematicalFrakturCapitalF

-- | The mathematical fraktur capital g HTML entity ('𝔊').
mathematicalFrakturCapitalG :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalG = Tag_Entity Entity.mathematicalFrakturCapitalG

-- | The mathematical fraktur capital j HTML entity ('𝔍').
mathematicalFrakturCapitalJ :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalJ = Tag_Entity Entity.mathematicalFrakturCapitalJ

-- | The mathematical fraktur capital k HTML entity ('𝔎').
mathematicalFrakturCapitalK :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalK = Tag_Entity Entity.mathematicalFrakturCapitalK

-- | The mathematical fraktur capital l HTML entity ('𝔏').
mathematicalFrakturCapitalL :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalL = Tag_Entity Entity.mathematicalFrakturCapitalL

-- | The mathematical fraktur capital m HTML entity ('𝔐').
mathematicalFrakturCapitalM :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalM = Tag_Entity Entity.mathematicalFrakturCapitalM

-- | The mathematical fraktur capital n HTML entity ('𝔑').
mathematicalFrakturCapitalN :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalN = Tag_Entity Entity.mathematicalFrakturCapitalN

-- | The mathematical fraktur capital o HTML entity ('𝔒').
mathematicalFrakturCapitalO :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalO = Tag_Entity Entity.mathematicalFrakturCapitalO

-- | The mathematical fraktur capital p HTML entity ('𝔓').
mathematicalFrakturCapitalP :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalP = Tag_Entity Entity.mathematicalFrakturCapitalP

-- | The mathematical fraktur capital q HTML entity ('𝔔').
mathematicalFrakturCapitalQ :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalQ = Tag_Entity Entity.mathematicalFrakturCapitalQ

-- | The mathematical fraktur capital s HTML entity ('𝔖').
mathematicalFrakturCapitalS :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalS = Tag_Entity Entity.mathematicalFrakturCapitalS

-- | The mathematical fraktur capital t HTML entity ('𝔗').
mathematicalFrakturCapitalT :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalT = Tag_Entity Entity.mathematicalFrakturCapitalT

-- | The mathematical fraktur capital u HTML entity ('𝔘').
mathematicalFrakturCapitalU :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalU = Tag_Entity Entity.mathematicalFrakturCapitalU

-- | The mathematical fraktur capital v HTML entity ('𝔙').
mathematicalFrakturCapitalV :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalV = Tag_Entity Entity.mathematicalFrakturCapitalV

-- | The mathematical fraktur capital w HTML entity ('𝔚').
mathematicalFrakturCapitalW :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalW = Tag_Entity Entity.mathematicalFrakturCapitalW

-- | The mathematical fraktur capital x HTML entity ('𝔛').
mathematicalFrakturCapitalX :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalX = Tag_Entity Entity.mathematicalFrakturCapitalX

-- | The mathematical fraktur capital y HTML entity ('𝔜').
mathematicalFrakturCapitalY :: ValidChild Content parent
                            => ChildHXML parent
mathematicalFrakturCapitalY = Tag_Entity Entity.mathematicalFrakturCapitalY

-- | The mathematical fraktur small a HTML entity ('𝔞').
mathematicalFrakturSmallA :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallA = Tag_Entity Entity.mathematicalFrakturSmallA

-- | The mathematical fraktur small b HTML entity ('𝔟').
mathematicalFrakturSmallB :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallB = Tag_Entity Entity.mathematicalFrakturSmallB

-- | The mathematical fraktur small c HTML entity ('𝔠').
mathematicalFrakturSmallC :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallC = Tag_Entity Entity.mathematicalFrakturSmallC

-- | The mathematical fraktur small d HTML entity ('𝔡').
mathematicalFrakturSmallD :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallD = Tag_Entity Entity.mathematicalFrakturSmallD

-- | The mathematical fraktur small e HTML entity ('𝔢').
mathematicalFrakturSmallE :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallE = Tag_Entity Entity.mathematicalFrakturSmallE

-- | The mathematical fraktur small f HTML entity ('𝔣').
mathematicalFrakturSmallF :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallF = Tag_Entity Entity.mathematicalFrakturSmallF

-- | The mathematical fraktur small g HTML entity ('𝔤').
mathematicalFrakturSmallG :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallG = Tag_Entity Entity.mathematicalFrakturSmallG

-- | The mathematical fraktur small h HTML entity ('𝔥').
mathematicalFrakturSmallH :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallH = Tag_Entity Entity.mathematicalFrakturSmallH

-- | The mathematical fraktur small i HTML entity ('𝔦').
mathematicalFrakturSmallI :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallI = Tag_Entity Entity.mathematicalFrakturSmallI

-- | The mathematical fraktur small j HTML entity ('𝔧').
mathematicalFrakturSmallJ :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallJ = Tag_Entity Entity.mathematicalFrakturSmallJ

-- | The mathematical fraktur small k HTML entity ('𝔨').
mathematicalFrakturSmallK :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallK = Tag_Entity Entity.mathematicalFrakturSmallK

-- | The mathematical fraktur small l HTML entity ('𝔩').
mathematicalFrakturSmallL :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallL = Tag_Entity Entity.mathematicalFrakturSmallL

-- | The mathematical fraktur small m HTML entity ('𝔪').
mathematicalFrakturSmallM :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallM = Tag_Entity Entity.mathematicalFrakturSmallM

-- | The mathematical fraktur small n HTML entity ('𝔫').
mathematicalFrakturSmallN :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallN = Tag_Entity Entity.mathematicalFrakturSmallN

-- | The mathematical fraktur small o HTML entity ('𝔬').
mathematicalFrakturSmallO :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallO = Tag_Entity Entity.mathematicalFrakturSmallO

-- | The mathematical fraktur small p HTML entity ('𝔭').
mathematicalFrakturSmallP :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallP = Tag_Entity Entity.mathematicalFrakturSmallP

-- | The mathematical fraktur small q HTML entity ('𝔮').
mathematicalFrakturSmallQ :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallQ = Tag_Entity Entity.mathematicalFrakturSmallQ

-- | The mathematical fraktur small r HTML entity ('𝔯').
mathematicalFrakturSmallR :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallR = Tag_Entity Entity.mathematicalFrakturSmallR

-- | The mathematical fraktur small s HTML entity ('𝔰').
mathematicalFrakturSmallS :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallS = Tag_Entity Entity.mathematicalFrakturSmallS

-- | The mathematical fraktur small t HTML entity ('𝔱').
mathematicalFrakturSmallT :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallT = Tag_Entity Entity.mathematicalFrakturSmallT

-- | The mathematical fraktur small u HTML entity ('𝔲').
mathematicalFrakturSmallU :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallU = Tag_Entity Entity.mathematicalFrakturSmallU

-- | The mathematical fraktur small v HTML entity ('𝔳').
mathematicalFrakturSmallV :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallV = Tag_Entity Entity.mathematicalFrakturSmallV

-- | The mathematical fraktur small w HTML entity ('𝔴').
mathematicalFrakturSmallW :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallW = Tag_Entity Entity.mathematicalFrakturSmallW

-- | The mathematical fraktur small x HTML entity ('𝔵').
mathematicalFrakturSmallX :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallX = Tag_Entity Entity.mathematicalFrakturSmallX

-- | The mathematical fraktur small y HTML entity ('𝔶').
mathematicalFrakturSmallY :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallY = Tag_Entity Entity.mathematicalFrakturSmallY

-- | The mathematical fraktur small z HTML entity ('𝔷').
mathematicalFrakturSmallZ :: ValidChild Content parent
                          => ChildHXML parent
mathematicalFrakturSmallZ = Tag_Entity Entity.mathematicalFrakturSmallZ

-- | The mathematical double-struck capital a HTML entity ('𝔸').
mathematicalDoubleStruckCapitalA :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalA = Tag_Entity Entity.mathematicalDoubleStruckCapitalA

-- | The mathematical double-struck capital b HTML entity ('𝔹').
mathematicalDoubleStruckCapitalB :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalB = Tag_Entity Entity.mathematicalDoubleStruckCapitalB

-- | The mathematical double-struck capital d HTML entity ('𝔻').
mathematicalDoubleStruckCapitalD :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalD = Tag_Entity Entity.mathematicalDoubleStruckCapitalD

-- | The mathematical double-struck capital e HTML entity ('𝔼').
mathematicalDoubleStruckCapitalE :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalE = Tag_Entity Entity.mathematicalDoubleStruckCapitalE

-- | The mathematical double-struck capital f HTML entity ('𝔽').
mathematicalDoubleStruckCapitalF :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalF = Tag_Entity Entity.mathematicalDoubleStruckCapitalF

-- | The mathematical double-struck capital g HTML entity ('𝔾').
mathematicalDoubleStruckCapitalG :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalG = Tag_Entity Entity.mathematicalDoubleStruckCapitalG

-- | The mathematical double-struck capital i HTML entity ('𝕀').
mathematicalDoubleStruckCapitalI :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalI = Tag_Entity Entity.mathematicalDoubleStruckCapitalI

-- | The mathematical double-struck capital j HTML entity ('𝕁').
mathematicalDoubleStruckCapitalJ :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalJ = Tag_Entity Entity.mathematicalDoubleStruckCapitalJ

-- | The mathematical double-struck capital k HTML entity ('𝕂').
mathematicalDoubleStruckCapitalK :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalK = Tag_Entity Entity.mathematicalDoubleStruckCapitalK

-- | The mathematical double-struck capital l HTML entity ('𝕃').
mathematicalDoubleStruckCapitalL :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalL = Tag_Entity Entity.mathematicalDoubleStruckCapitalL

-- | The mathematical double-struck capital m HTML entity ('𝕄').
mathematicalDoubleStruckCapitalM :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalM = Tag_Entity Entity.mathematicalDoubleStruckCapitalM

-- | The mathematical double-struck capital o HTML entity ('𝕆').
mathematicalDoubleStruckCapitalO :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalO = Tag_Entity Entity.mathematicalDoubleStruckCapitalO

-- | The mathematical double-struck capital s HTML entity ('𝕊').
mathematicalDoubleStruckCapitalS :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalS = Tag_Entity Entity.mathematicalDoubleStruckCapitalS

-- | The mathematical double-struck capital t HTML entity ('𝕋').
mathematicalDoubleStruckCapitalT :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalT = Tag_Entity Entity.mathematicalDoubleStruckCapitalT

-- | The mathematical double-struck capital u HTML entity ('𝕌').
mathematicalDoubleStruckCapitalU :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalU = Tag_Entity Entity.mathematicalDoubleStruckCapitalU

-- | The mathematical double-struck capital v HTML entity ('𝕍').
mathematicalDoubleStruckCapitalV :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalV = Tag_Entity Entity.mathematicalDoubleStruckCapitalV

-- | The mathematical double-struck capital w HTML entity ('𝕎').
mathematicalDoubleStruckCapitalW :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalW = Tag_Entity Entity.mathematicalDoubleStruckCapitalW

-- | The mathematical double-struck capital x HTML entity ('𝕏').
mathematicalDoubleStruckCapitalX :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalX = Tag_Entity Entity.mathematicalDoubleStruckCapitalX

-- | The mathematical double-struck capital y HTML entity ('𝕐').
mathematicalDoubleStruckCapitalY :: ValidChild Content parent
                                 => ChildHXML parent
mathematicalDoubleStruckCapitalY = Tag_Entity Entity.mathematicalDoubleStruckCapitalY

-- | The mathematical double-struck small a HTML entity ('𝕒').
mathematicalDoubleStruckSmallA :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallA = Tag_Entity Entity.mathematicalDoubleStruckSmallA

-- | The mathematical double-struck small b HTML entity ('𝕓').
mathematicalDoubleStruckSmallB :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallB = Tag_Entity Entity.mathematicalDoubleStruckSmallB

-- | The mathematical double-struck small c HTML entity ('𝕔').
mathematicalDoubleStruckSmallC :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallC = Tag_Entity Entity.mathematicalDoubleStruckSmallC

-- | The mathematical double-struck small d HTML entity ('𝕕').
mathematicalDoubleStruckSmallD :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallD = Tag_Entity Entity.mathematicalDoubleStruckSmallD

-- | The mathematical double-struck small e HTML entity ('𝕖').
mathematicalDoubleStruckSmallE :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallE = Tag_Entity Entity.mathematicalDoubleStruckSmallE

-- | The mathematical double-struck small f HTML entity ('𝕗').
mathematicalDoubleStruckSmallF :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallF = Tag_Entity Entity.mathematicalDoubleStruckSmallF

-- | The mathematical double-struck small g HTML entity ('𝕘').
mathematicalDoubleStruckSmallG :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallG = Tag_Entity Entity.mathematicalDoubleStruckSmallG

-- | The mathematical double-struck small h HTML entity ('𝕙').
mathematicalDoubleStruckSmallH :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallH = Tag_Entity Entity.mathematicalDoubleStruckSmallH

-- | The mathematical double-struck small i HTML entity ('𝕚').
mathematicalDoubleStruckSmallI :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallI = Tag_Entity Entity.mathematicalDoubleStruckSmallI

-- | The mathematical double-struck small j HTML entity ('𝕛').
mathematicalDoubleStruckSmallJ :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallJ = Tag_Entity Entity.mathematicalDoubleStruckSmallJ

-- | The mathematical double-struck small k HTML entity ('𝕜').
mathematicalDoubleStruckSmallK :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallK = Tag_Entity Entity.mathematicalDoubleStruckSmallK

-- | The mathematical double-struck small l HTML entity ('𝕝').
mathematicalDoubleStruckSmallL :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallL = Tag_Entity Entity.mathematicalDoubleStruckSmallL

-- | The mathematical double-struck small m HTML entity ('𝕞').
mathematicalDoubleStruckSmallM :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallM = Tag_Entity Entity.mathematicalDoubleStruckSmallM

-- | The mathematical double-struck small n HTML entity ('𝕟').
mathematicalDoubleStruckSmallN :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallN = Tag_Entity Entity.mathematicalDoubleStruckSmallN

-- | The mathematical double-struck small o HTML entity ('𝕠').
mathematicalDoubleStruckSmallO :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallO = Tag_Entity Entity.mathematicalDoubleStruckSmallO

-- | The mathematical double-struck small p HTML entity ('𝕡').
mathematicalDoubleStruckSmallP :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallP = Tag_Entity Entity.mathematicalDoubleStruckSmallP

-- | The mathematical double-struck small q HTML entity ('𝕢').
mathematicalDoubleStruckSmallQ :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallQ = Tag_Entity Entity.mathematicalDoubleStruckSmallQ

-- | The mathematical double-struck small r HTML entity ('𝕣').
mathematicalDoubleStruckSmallR :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallR = Tag_Entity Entity.mathematicalDoubleStruckSmallR

-- | The mathematical double-struck small s HTML entity ('𝕤').
mathematicalDoubleStruckSmallS :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallS = Tag_Entity Entity.mathematicalDoubleStruckSmallS

-- | The mathematical double-struck small t HTML entity ('𝕥').
mathematicalDoubleStruckSmallT :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallT = Tag_Entity Entity.mathematicalDoubleStruckSmallT

-- | The mathematical double-struck small u HTML entity ('𝕦').
mathematicalDoubleStruckSmallU :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallU = Tag_Entity Entity.mathematicalDoubleStruckSmallU

-- | The mathematical double-struck small v HTML entity ('𝕧').
mathematicalDoubleStruckSmallV :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallV = Tag_Entity Entity.mathematicalDoubleStruckSmallV

-- | The mathematical double-struck small w HTML entity ('𝕨').
mathematicalDoubleStruckSmallW :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallW = Tag_Entity Entity.mathematicalDoubleStruckSmallW

-- | The mathematical double-struck small x HTML entity ('𝕩').
mathematicalDoubleStruckSmallX :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallX = Tag_Entity Entity.mathematicalDoubleStruckSmallX

-- | The mathematical double-struck small y HTML entity ('𝕪').
mathematicalDoubleStruckSmallY :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallY = Tag_Entity Entity.mathematicalDoubleStruckSmallY

-- | The mathematical double-struck small z HTML entity ('𝕫').
mathematicalDoubleStruckSmallZ :: ValidChild Content parent
                               => ChildHXML parent
mathematicalDoubleStruckSmallZ = Tag_Entity Entity.mathematicalDoubleStruckSmallZ