 -- A complete list of the MIME types and their details (from which this module
-- was assembled) can be found here:
--
-- https://s-randomfiles.s3.amazonaws.com/mime/allMimeTypes.json
--
module Brigid.Types.MIME_Types
  ( MIME_Type
      ( Application_1dInterleavedParityfec
      , Application_3gppImsXml
      , Application_Acad
      , Application_Activemessage
      , Application_AndrewInset
      , Application_Applefile
      , Application_Applixware
      , Application_Arj
      , Application_AtomXml
      , Application_AtomcatXml
      , Application_Atomicmail
      , Application_AtomsvcXml
      , Application_AuthPolicyXml
      , Application_Base64
      , Application_BatchSmtp
      , Application_BeepXml
      , Application_Binhex
      , Application_Binhex4
      , Application_Book
      , Application_CalendarXml
      , Application_Cals1840
      , Application_CcmpXml
      , Application_CcxmlXml
      , Application_Cdf
      , Application_CdmiCapability
      , Application_CdmiContainer
      , Application_CdmiDomain
      , Application_CdmiObject
      , Application_CdmiQueue
      , Application_Cea2018Xml
      , Application_CellmlXml
      , Application_Cfw
      , Application_Clariscad
      , Application_CnrpXml
      , Application_Commonground
      , Application_ConferenceInfoXml
      , Application_CplXml
      , Application_CstaXml
      , Application_CstadataXml
      , Application_CuSeeme
      , Application_Cybercash
      , Application_DavmountXml
      , Application_DcaRft
      , Application_DecDx
      , Application_DialogInfoXml
      , Application_Dicom
      , Application_Dns
      , Application_DocbookXml
      , Application_Drafting
      , Application_DskppXml
      , Application_Dsptype
      , Application_DsscDer
      , Application_DsscXml
      , Application_Dvcs
      , Application_Dxf
      , Application_Ecmascript
      , Application_EdiConsent
      , Application_EdiX12
      , Application_Edifact
      , Application_EmmaXml
      , Application_Envoy
      , Application_EppXml
      , Application_EpubZip
      , Application_Eshop
      , Application_Example
      , Application_Excel
      , Application_Exi
      , Application_Fastinfoset
      , Application_Fastsoap
      , Application_Fits
      , Application_FontTdpfr
      , Application_FontWoff
      , Application_Fractals
      , Application_FrameworkAttributesXml
      , Application_Freeloader
      , Application_Futuresplash
      , Application_Ghostview
      , Application_GmlXml
      , Application_Gnutar
      , Application_GpxXml
      , Application_Groupwise
      , Application_Gxf
      , Application_H224
      , Application_HeldXml
      , Application_Hlp
      , Application_Hta
      , Application_Http
      , Application_Hyperstudio
      , Application_IDeas
      , Application_IbeKeyRequestXml
      , Application_IbePkgReplyXml
      , Application_IbePpData
      , Application_Iges
      , Application_ImIscomposingXml
      , Application_Index
      , Application_IndexCmd
      , Application_IndexObj
      , Application_IndexResponse
      , Application_IndexVnd
      , Application_Inf
      , Application_InkmlXml
      , Application_InternetPropertyStream
      , Application_Iotp
      , Application_Ipfix
      , Application_Ipp
      , Application_Isup
      , Application_Java
      , Application_JavaArchive
      , Application_JavaByteCode
      , Application_JavaSerializedObject
      , Application_JavaVm
      , Application_Javascript
      , Application_Json
      , Application_JsonmlJson
      , Application_KpmlRequestXml
      , Application_KpmlResponseXml
      , Application_Lha
      , Application_LostXml
      , Application_Lzx
      , Application_MacBinary
      , Application_MacBinhex
      , Application_MacBinhex40
      , Application_MacCompactpro
      , Application_Macbinary
      , Application_Macwriteii
      , Application_MadsXml
      , Application_Marc
      , Application_MarcxmlXml
      , Application_Mathematica
      , Application_MathematicaOld
      , Application_MathmlContentXml
      , Application_MathmlPresentationXml
      , Application_MathmlXml
      , Application_Mbedlet
      , Application_MbmsAssociatedProcedureDescriptionXml
      , Application_MbmsDeregisterXml
      , Application_MbmsEnvelopeXml
      , Application_MbmsMskResponseXml
      , Application_MbmsMskXml
      , Application_MbmsProtectionDescriptionXml
      , Application_MbmsReceptionReportXml
      , Application_MbmsRegisterResponseXml
      , Application_MbmsRegisterXml
      , Application_MbmsUserServiceDescriptionXml
      , Application_Mbox
      , Application_Mcad
      , Application_Media_controlXml
      , Application_MediaservercontrolXml
      , Application_Metalink4Xml
      , Application_MetalinkXml
      , Application_MetsXml
      , Application_Mikey
      , Application_Mime
      , Application_ModsXml
      , Application_MossKeys
      , Application_MossSignature
      , Application_MosskeyData
      , Application_MosskeyRequest
      , Application_Mp21
      , Application_Mp4
      , Application_Mpeg4Generic
      , Application_Mpeg4Iod
      , Application_Mpeg4IodXmt
      , Application_Msaccess
      , Application_MscIvrXml
      , Application_MscMixerXml
      , Application_Msonenote
      , Application_Mspowerpoint
      , Application_Msword
      , Application_Mswrite
      , Application_Mxf
      , Application_Nasdata
      , Application_Netmc
      , Application_NewsCheckgroups
      , Application_NewsGroupinfo
      , Application_NewsMessageId
      , Application_NewsTransmission
      , Application_Nss
      , Application_OcspRequest
      , Application_OcspResponse
      , Application_OctetStream
      , Application_Oda
      , Application_OebpsPackageXml
      , Application_Ogg
      , Application_Olescript
      , Application_OmdocXml
      , Application_Onenote
      , Application_Oxps
      , Application_Parityfec
      , Application_PatchOpsErrorXml
      , Application_Pdf
      , Application_PgpEncrypted
      , Application_PgpKeys
      , Application_PgpSignature
      , Application_PicsRules
      , Application_PidfDiffXml
      , Application_PidfXml
      , Application_Pkcs10
      , Application_Pkcs12
      , Application_Pkcs7Mime
      , Application_Pkcs7Signature
      , Application_Pkcs8
      , Application_PkcsCrl
      , Application_PkixAttrCert
      , Application_PkixCert
      , Application_PkixCrl
      , Application_PkixPkipath
      , Application_Pkixcmp
      , Application_Plain
      , Application_PlsXml
      , Application_PocSettingsXml
      , Application_Postscript
      , Application_Powerpoint
      , Application_Pro_eng
      , Application_PrsAlvestrandTitraxSheet
      , Application_PrsCww
      , Application_PrsNprend
      , Application_PrsPlucker
      , Application_PrsRdfXmlCrypt
      , Application_PrsXsfXml
      , Application_PskcXml
      , Application_Qsig
      , Application_Rar
      , Application_RdfXml
      , Application_ReginfoXml
      , Application_RelaxNgCompactSyntax
      , Application_RemotePrinting
      , Application_ResourceListsDiffXml
      , Application_ResourceListsXml
      , Application_RingingTones
      , Application_Riscos
      , Application_RlmiXml
      , Application_RlsServicesXml
      , Application_RpkiGhostbusters
      , Application_RpkiManifest
      , Application_RpkiRoa
      , Application_RpkiUpdown
      , Application_RsdXml
      , Application_RssXml
      , Application_Rtf
      , Application_Rtx
      , Application_SamlassertionXml
      , Application_SamlmetadataXml
      , Application_SbmlXml
      , Application_ScvpCvRequest
      , Application_ScvpCvResponse
      , Application_ScvpVpRequest
      , Application_ScvpVpResponse
      , Application_Sdp
      , Application_Sea
      , Application_Set
      , Application_SetPayment
      , Application_SetPaymentInitiation
      , Application_SetRegistration
      , Application_SetRegistrationInitiation
      , Application_Sgml
      , Application_SgmlOpenCatalog
      , Application_ShfXml
      , Application_Sieve
      , Application_SimpleFilterXml
      , Application_SimpleMessageSummary
      , Application_Simplesymbolcontainer
      , Application_Sla
      , Application_Slate
      , Application_Smil
      , Application_SmilXml
      , Application_SoapFastinfoset
      , Application_SoapXml
      , Application_Solids
      , Application_Sounder
      , Application_SparqlQuery
      , Application_SparqlResultsXml
      , Application_SpiritsEventXml
      , Application_Srgs
      , Application_SrgsXml
      , Application_SruXml
      , Application_SsdlXml
      , Application_SsmlXml
      , Application_Step
      , Application_Streamingmedia
      , Application_TampApexUpdate
      , Application_TampApexUpdateConfirm
      , Application_TampCommunityUpdate
      , Application_TampCommunityUpdateConfirm
      , Application_TampError
      , Application_TampSequenceAdjust
      , Application_TampSequenceAdjustConfirm
      , Application_TampStatusQuery
      , Application_TampStatusResponse
      , Application_TampUpdate
      , Application_TampUpdateConfirm
      , Application_TeiXml
      , Application_ThraudXml
      , Application_TimestampQuery
      , Application_TimestampReply
      , Application_TimestampedData
      , Application_Toolbook
      , Application_TveTrigger
      , Application_Ulpfec
      , Application_VcardXml
      , Application_Vda
      , Application_Vemmi
      , Application_VividenceScriptfile
      , Application_Vnd3gpp2BcmcsinfoXml
      , Application_Vnd3gpp2Sms
      , Application_Vnd3gpp2Tcap
      , Application_Vnd3gppBsfXml
      , Application_Vnd3gppPicBwLarge
      , Application_Vnd3gppPicBwSmall
      , Application_Vnd3gppPicBwVar
      , Application_Vnd3gppSms
      , Application_Vnd3mPostItNotes
      , Application_VndAccpacSimplyAso
      , Application_VndAccpacSimplyImp
      , Application_VndAcucobol
      , Application_VndAcucorp
      , Application_VndAdobeAirApplicationInstallerPackageZip
      , Application_VndAdobeFormscentralFcdt
      , Application_VndAdobeFxp
      , Application_VndAdobePartialUpload
      , Application_VndAdobeXdpXml
      , Application_VndAdobeXfdf
      , Application_VndAetherImp
      , Application_VndAhBarcode
      , Application_VndAheadSpace
      , Application_VndAirzipFilesecureAzf
      , Application_VndAirzipFilesecureAzs
      , Application_VndAmazonEbook
      , Application_VndAmericandynamicsAcc
      , Application_VndAmigaAmi
      , Application_VndAmundsenMazeXml
      , Application_VndAndroidPackageArchive
      , Application_VndAnserWebCertificateIssueInitiation
      , Application_VndAnserWebFundsTransferInitiation
      , Application_VndAntixGameComponent
      , Application_VndAppleInstallerXml
      , Application_VndAppleMpegurl
      , Application_VndArastraSwi
      , Application_VndAristanetworksSwi
      , Application_VndAstraeaSoftwareIota
      , Application_VndAudiograph
      , Application_VndAutopackage
      , Application_VndAvistarXml
      , Application_VndBlueiceMultipass
      , Application_VndBluetoothEpOob
      , Application_VndBmi
      , Application_VndBusinessobjects
      , Application_VndCabJscript
      , Application_VndCanonCpdl
      , Application_VndCanonLips
      , Application_VndCendioThinlincClientconf
      , Application_VndChemdrawXml
      , Application_VndChipnutsKaraokeMmd
      , Application_VndCinderella
      , Application_VndCirpackIsdnExt
      , Application_VndClaymore
      , Application_VndCloantoRp9
      , Application_VndClonkC4group
      , Application_VndCluetrustCartomobileConfig
      , Application_VndCluetrustCartomobileConfigPkg
      , Application_VndCollectionJson
      , Application_VndCommerceBattelle
      , Application_VndCommonspace
      , Application_VndComsocaller
      , Application_VndContactCmsg
      , Application_VndCosmocaller
      , Application_VndCrickClicker
      , Application_VndCrickClickerKeyboard
      , Application_VndCrickClickerPalette
      , Application_VndCrickClickerTemplate
      , Application_VndCrickClickerWordbank
      , Application_VndCriticaltoolsWbsXml
      , Application_VndCtcPosml
      , Application_VndCtctWsXml
      , Application_VndCupsPdf
      , Application_VndCupsPostscript
      , Application_VndCupsPpd
      , Application_VndCupsRaster
      , Application_VndCupsRaw
      , Application_VndCurl
      , Application_VndCurlCar
      , Application_VndCurlPcurl
      , Application_VndCybank
      , Application_VndDart
      , Application_VndDataVisionRdz
      , Application_VndDeceData
      , Application_VndDeceTtmlXml
      , Application_VndDeceUnspecified
      , Application_VndDeceZip
      , Application_VndDenovoFcselayoutLink
      , Application_VndDirBiPlateDlNosuffix
      , Application_VndDna
      , Application_VndDolbyMlp
      , Application_VndDolbyMobile1
      , Application_VndDolbyMobile2
      , Application_VndDpgraph
      , Application_VndDreamfactory
      , Application_VndDsKeypoint
      , Application_VndDvbAit
      , Application_VndDvbDvbj
      , Application_VndDvbEsgcontainer
      , Application_VndDvbIpdcdftnotifaccess
      , Application_VndDvbIpdcesgaccess
      , Application_VndDvbIpdcesgaccess2
      , Application_VndDvbIpdcesgpdd
      , Application_VndDvbIpdcroaming
      , Application_VndDvbIptvAlfecBase
      , Application_VndDvbIptvAlfecEnhancement
      , Application_VndDvbNotifAggregateRootXml
      , Application_VndDvbNotifContainerXml
      , Application_VndDvbNotifGenericXml
      , Application_VndDvbNotifIaMsglistXml
      , Application_VndDvbNotifIaRegistrationRequestXml
      , Application_VndDvbNotifIaRegistrationResponseXml
      , Application_VndDvbNotifInitXml
      , Application_VndDvbPfr
      , Application_VndDvbService
      , Application_VndDxr
      , Application_VndDynageo
      , Application_VndEasykaraokeCdgdownload
      , Application_VndEcdisUpdate
      , Application_VndEcowinChart
      , Application_VndEcowinFilerequest
      , Application_VndEcowinFileupdate
      , Application_VndEcowinSeries
      , Application_VndEcowinSeriesrequest
      , Application_VndEcowinSeriesupdate
      , Application_VndEmclientAccessrequestXml
      , Application_VndEnliven
      , Application_VndEprintsDataXml
      , Application_VndEpsonEsf
      , Application_VndEpsonMsf
      , Application_VndEpsonQuickanime
      , Application_VndEpsonSalt
      , Application_VndEpsonSsf
      , Application_VndEricssonQuickcall
      , Application_VndEszigno3Xml
      , Application_VndEtsiAocXml
      , Application_VndEtsiCugXml
      , Application_VndEtsiIptvcommandXml
      , Application_VndEtsiIptvdiscoveryXml
      , Application_VndEtsiIptvprofileXml
      , Application_VndEtsiIptvsadBcXml
      , Application_VndEtsiIptvsadCodXml
      , Application_VndEtsiIptvsadNpvrXml
      , Application_VndEtsiIptvserviceXml
      , Application_VndEtsiIptvsyncXml
      , Application_VndEtsiIptvueprofileXml
      , Application_VndEtsiMcidXml
      , Application_VndEtsiOverloadControlPolicyDatasetXml
      , Application_VndEtsiSciXml
      , Application_VndEtsiSimservsXml
      , Application_VndEtsiTslDer
      , Application_VndEtsiTslXml
      , Application_VndEudoraData
      , Application_VndEzpixAlbum
      , Application_VndEzpixPackage
      , Application_VndFSecureMobile
      , Application_VndFdf
      , Application_VndFdsnMseed
      , Application_VndFdsnSeed
      , Application_VndFfsns
      , Application_VndFints
      , Application_VndFlographit
      , Application_VndFluxtimeClip
      , Application_VndFontFontforgeSfd
      , Application_VndFramemaker
      , Application_VndFrogansFnc
      , Application_VndFrogansLtf
      , Application_VndFscWeblaunch
      , Application_VndFujitsuOasys
      , Application_VndFujitsuOasys2
      , Application_VndFujitsuOasys3
      , Application_VndFujitsuOasysgp
      , Application_VndFujitsuOasysprs
      , Application_VndFujixeroxArt4
      , Application_VndFujixeroxArtEx
      , Application_VndFujixeroxDdd
      , Application_VndFujixeroxDocuworks
      , Application_VndFujixeroxDocuworksBinder
      , Application_VndFujixeroxHbpl
      , Application_VndFutMisnet
      , Application_VndFuzzysheet
      , Application_VndGenomatixTuxedo
      , Application_VndGeocubeXml
      , Application_VndGeogebraFile
      , Application_VndGeogebraTool
      , Application_VndGeometryExplorer
      , Application_VndGeonext
      , Application_VndGeoplan
      , Application_VndGeospace
      , Application_VndGlobalplatformCardContentMgt
      , Application_VndGlobalplatformCardContentMgtResponse
      , Application_VndGmx
      , Application_VndGoogleEarthKmlXml
      , Application_VndGoogleEarthKmz
      , Application_VndGrafeq
      , Application_VndGridmp
      , Application_VndGrooveAccount
      , Application_VndGrooveHelp
      , Application_VndGrooveIdentityMessage
      , Application_VndGrooveInjector
      , Application_VndGrooveToolMessage
      , Application_VndGrooveToolTemplate
      , Application_VndGrooveVcard
      , Application_VndHalJson
      , Application_VndHalXml
      , Application_VndHandheldEntertainmentXml
      , Application_VndHbci
      , Application_VndHclBireports
      , Application_VndHheLessonPlayer
      , Application_VndHpHpgl
      , Application_VndHpHpid
      , Application_VndHpHps
      , Application_VndHpJlyt
      , Application_VndHpPcl
      , Application_VndHpPclxl
      , Application_VndHttphone
      , Application_VndHydrostatixSofData
      , Application_VndHzn3dCrossword
      , Application_VndIbmAfplinedata
      , Application_VndIbmElectronicMedia
      , Application_VndIbmMinipay
      , Application_VndIbmModcap
      , Application_VndIbmRightsManagement
      , Application_VndIbmSecureContainer
      , Application_VndIccprofile
      , Application_VndIgloader
      , Application_VndImmervisionIvp
      , Application_VndImmervisionIvu
      , Application_VndInformedcontrolRmsXml
      , Application_VndInformixVisionary
      , Application_VndInfotechProject
      , Application_VndInfotechProjectXml
      , Application_VndInnopathWampNotification
      , Application_VndInsorsIgm
      , Application_VndInterconFormnet
      , Application_VndIntergeo
      , Application_VndIntertrustDigibox
      , Application_VndIntertrustNncp
      , Application_VndIntuQbo
      , Application_VndIntuQfx
      , Application_VndIptcG2ConceptitemXml
      , Application_VndIptcG2KnowledgeitemXml
      , Application_VndIptcG2NewsitemXml
      , Application_VndIptcG2NewsmessageXml
      , Application_VndIptcG2PackageitemXml
      , Application_VndIptcG2PlanningitemXml
      , Application_VndIpunpluggedRcprofile
      , Application_VndIrepositoryPackageXml
      , Application_VndIsXpr
      , Application_VndIsacFcs
      , Application_VndJam
      , Application_VndJapannetDirectoryService
      , Application_VndJapannetJpnstoreWakeup
      , Application_VndJapannetPaymentWakeup
      , Application_VndJapannetRegistration
      , Application_VndJapannetRegistrationWakeup
      , Application_VndJapannetSetstoreWakeup
      , Application_VndJapannetVerification
      , Application_VndJapannetVerificationWakeup
      , Application_VndJcpJavameMidletRms
      , Application_VndJisp
      , Application_VndJoostJodaArchive
      , Application_VndKahootz
      , Application_VndKdeKarbon
      , Application_VndKdeKchart
      , Application_VndKdeKformula
      , Application_VndKdeKivio
      , Application_VndKdeKontour
      , Application_VndKdeKpresenter
      , Application_VndKdeKspread
      , Application_VndKdeKword
      , Application_VndKenameaapp
      , Application_VndKidspiration
      , Application_VndKinar
      , Application_VndKoan
      , Application_VndKodakDescriptor
      , Application_VndLasLasXml
      , Application_VndLibertyRequestXml
      , Application_VndLlamagraphicsLifeBalanceDesktop
      , Application_VndLlamagraphicsLifeBalanceExchangeXml
      , Application_VndLotus123
      , Application_VndLotusApproach
      , Application_VndLotusFreelance
      , Application_VndLotusNotes
      , Application_VndLotusOrganizer
      , Application_VndLotusScreencam
      , Application_VndLotusWordpro
      , Application_VndMacportsPortpkg
      , Application_VndMarlinDrmActiontokenXml
      , Application_VndMarlinDrmConftokenXml
      , Application_VndMarlinDrmLicenseXml
      , Application_VndMarlinDrmMdcf
      , Application_VndMcd
      , Application_VndMedcalcdata
      , Application_VndMediastationCdkey
      , Application_VndMeridianSlingshot
      , Application_VndMfer
      , Application_VndMfmp
      , Application_VndMicrografxFlo
      , Application_VndMicrografxIgx
      , Application_VndMif
      , Application_VndMinisoftHp3000Save
      , Application_VndMitsubishiMistyGuardTrustweb
      , Application_VndMobiusDaf
      , Application_VndMobiusDis
      , Application_VndMobiusMbk
      , Application_VndMobiusMqy
      , Application_VndMobiusMsl
      , Application_VndMobiusPlc
      , Application_VndMobiusTxf
      , Application_VndMophunApplication
      , Application_VndMophunCertificate
      , Application_VndMotorolaFlexsuite
      , Application_VndMotorolaFlexsuiteAdsi
      , Application_VndMotorolaFlexsuiteFis
      , Application_VndMotorolaFlexsuiteGotap
      , Application_VndMotorolaFlexsuiteKmr
      , Application_VndMotorolaFlexsuiteTtc
      , Application_VndMotorolaFlexsuiteWem
      , Application_VndMotorolaIprm
      , Application_VndMozillaXulXml
      , Application_VndMsArtgalry
      , Application_VndMsAsf
      , Application_VndMsCabCompressed
      , Application_VndMsColorIccprofile
      , Application_VndMsExcel
      , Application_VndMsExcelAddinMacroenabled12
      , Application_VndMsExcelSheetBinaryMacroenabled12
      , Application_VndMsExcelSheetMacroenabled12
      , Application_VndMsExcelTemplateMacroenabled12
      , Application_VndMsFontobject
      , Application_VndMsHtmlhelp
      , Application_VndMsIms
      , Application_VndMsLrm
      , Application_VndMsOfficeActivexXml
      , Application_VndMsOfficetheme
      , Application_VndMsOpentype
      , Application_VndMsOutlook
      , Application_VndMsPackageObfuscatedOpentype
      , Application_VndMsPkiCertstore
      , Application_VndMsPkiPko
      , Application_VndMsPkiSeccat
      , Application_VndMsPkiStl
      , Application_VndMsPkicertstore
      , Application_VndMsPkiseccat
      , Application_VndMsPkistl
      , Application_VndMsPlayreadyInitiatorXml
      , Application_VndMsPowerpoint
      , Application_VndMsPowerpointAddinMacroenabled12
      , Application_VndMsPowerpointPresentationMacroenabled12
      , Application_VndMsPowerpointSlideMacroenabled12
      , Application_VndMsPowerpointSlideshowMacroenabled12
      , Application_VndMsPowerpointTemplateMacroenabled12
      , Application_VndMsPrintingPrintticketXml
      , Application_VndMsProject
      , Application_VndMsTnef
      , Application_VndMsWmdrmLicChlgReq
      , Application_VndMsWmdrmLicResp
      , Application_VndMsWmdrmMeterChlgReq
      , Application_VndMsWmdrmMeterResp
      , Application_VndMsWordDocumentMacroenabled12
      , Application_VndMsWordTemplateMacroenabled12
      , Application_VndMsWorks
      , Application_VndMsWpl
      , Application_VndMsXpsdocument
      , Application_VndMseq
      , Application_VndMsign
      , Application_VndMultiadCreator
      , Application_VndMultiadCreatorCif
      , Application_VndMusicNiff
      , Application_VndMusician
      , Application_VndMuveeStyle
      , Application_VndMynfc
      , Application_VndNcdControl
      , Application_VndNcdReference
      , Application_VndNervana
      , Application_VndNetfpx
      , Application_VndNeurolanguageNlu
      , Application_VndNitf
      , Application_VndNoblenetDirectory
      , Application_VndNoblenetSealer
      , Application_VndNoblenetWeb
      , Application_VndNokiaCatalogs
      , Application_VndNokiaConfigurationMessage
      , Application_VndNokiaConmlWbxml
      , Application_VndNokiaConmlXml
      , Application_VndNokiaIptvConfigXml
      , Application_VndNokiaIsdsRadioPresets
      , Application_VndNokiaLandmarkWbxml
      , Application_VndNokiaLandmarkXml
      , Application_VndNokiaLandmarkcollectionXml
      , Application_VndNokiaNGageAcXml
      , Application_VndNokiaNGageData
      , Application_VndNokiaNGageSymbianInstall
      , Application_VndNokiaNcd
      , Application_VndNokiaPcdWbxml
      , Application_VndNokiaPcdXml
      , Application_VndNokiaRadioPreset
      , Application_VndNokiaRadioPresets
      , Application_VndNokiaRingingTone
      , Application_VndNovadigmEdm
      , Application_VndNovadigmEdx
      , Application_VndNovadigmExt
      , Application_VndNttLocalFileTransfer
      , Application_VndNttLocalSipTa_remote
      , Application_VndNttLocalSipTa_tcp_stream
      , Application_VndOasisOpendocumentChart
      , Application_VndOasisOpendocumentChartTemplate
      , Application_VndOasisOpendocumentDatabase
      , Application_VndOasisOpendocumentFormula
      , Application_VndOasisOpendocumentFormulaTemplate
      , Application_VndOasisOpendocumentGraphics
      , Application_VndOasisOpendocumentGraphicsTemplate
      , Application_VndOasisOpendocumentImage
      , Application_VndOasisOpendocumentImageTemplate
      , Application_VndOasisOpendocumentPresentation
      , Application_VndOasisOpendocumentPresentationTemplate
      , Application_VndOasisOpendocumentSpreadsheet
      , Application_VndOasisOpendocumentSpreadsheetTemplate
      , Application_VndOasisOpendocumentText
      , Application_VndOasisOpendocumentTextMaster
      , Application_VndOasisOpendocumentTextTemplate
      , Application_VndOasisOpendocumentTextWeb
      , Application_VndObn
      , Application_VndOftnL10nJson
      , Application_VndOipfContentaccessdownloadXml
      , Application_VndOipfContentaccessstreamingXml
      , Application_VndOipfCspgHexbinary
      , Application_VndOipfDaeSvgXml
      , Application_VndOipfDaeXhtmlXml
      , Application_VndOipfMippvcontrolmessageXml
      , Application_VndOipfPaeGem
      , Application_VndOipfSpdiscoveryXml
      , Application_VndOipfSpdlistXml
      , Application_VndOipfUeprofileXml
      , Application_VndOipfUserprofileXml
      , Application_VndOlpcSugar
      , Application_VndOmaBcastAssociatedProcedureParameterXml
      , Application_VndOmaBcastDrmTriggerXml
      , Application_VndOmaBcastImdXml
      , Application_VndOmaBcastLtkm
      , Application_VndOmaBcastNotificationXml
      , Application_VndOmaBcastProvisioningtrigger
      , Application_VndOmaBcastSgboot
      , Application_VndOmaBcastSgddXml
      , Application_VndOmaBcastSgdu
      , Application_VndOmaBcastSimpleSymbolContainer
      , Application_VndOmaBcastSmartcardTriggerXml
      , Application_VndOmaBcastSprovXml
      , Application_VndOmaBcastStkm
      , Application_VndOmaCabAddressBookXml
      , Application_VndOmaCabFeatureHandlerXml
      , Application_VndOmaCabPccXml
      , Application_VndOmaCabUserPrefsXml
      , Application_VndOmaDcd
      , Application_VndOmaDcdc
      , Application_VndOmaDd2Xml
      , Application_VndOmaDrmRisdXml
      , Application_VndOmaGroupUsageListXml
      , Application_VndOmaPalXml
      , Application_VndOmaPocDetailedProgressReportXml
      , Application_VndOmaPocFinalReportXml
      , Application_VndOmaPocGroupsXml
      , Application_VndOmaPocInvocationDescriptorXml
      , Application_VndOmaPocOptimizedProgressReportXml
      , Application_VndOmaPush
      , Application_VndOmaScidmMessagesXml
      , Application_VndOmaScwsConfig
      , Application_VndOmaScwsHttpRequest
      , Application_VndOmaScwsHttpResponse
      , Application_VndOmaXcapDirectoryXml
      , Application_VndOmadsEmailXml
      , Application_VndOmadsFileXml
      , Application_VndOmadsFolderXml
      , Application_VndOmalocSuplInit
      , Application_VndOpenofficeorgExtension
      , Application_VndOpenxmlformatsOfficedocumentCustomPropertiesXml
      , Application_VndOpenxmlformatsOfficedocumentCustomxmlpropertiesXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlChartXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlChartshapesXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramcolorsXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramdataXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramlayoutXml
      , Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramstyleXml
      , Application_VndOpenxmlformatsOfficedocumentExtendedPropertiesXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentauthorsXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentsXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlHandoutmasterXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesmasterXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesslideXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentation
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentationMainXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlPrespropsXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlide
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidelayoutXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidemasterXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshow
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshowMainXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideupdateinfoXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlTablestylesXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlTagsXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplate
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplateMainXml
      , Application_VndOpenxmlformatsOfficedocumentPresentationmlViewpropsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCalcchainXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlChartsheetXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCommentsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlConnectionsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlDialogsheetXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlExternallinkXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcachedefinitionXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcacherecordsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivottableXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlQuerytableXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionheadersXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionlogXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSharedstringsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheet
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetMainXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetmetadataXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlStylesXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTableXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTablesinglecellsXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplateMainXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlUsernamesXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlVolatiledependenciesXml
      , Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlWorksheetXml
      , Application_VndOpenxmlformatsOfficedocumentThemeXml
      , Application_VndOpenxmlformatsOfficedocumentThemeoverrideXml
      , Application_VndOpenxmlformatsOfficedocumentVmldrawing
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlCommentsXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocument
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentGlossaryXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentMainXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlEndnotesXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFonttableXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFooterXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFootnotesXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlNumberingXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlSettingsXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlStylesXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplate
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplateMainXml
      , Application_VndOpenxmlformatsOfficedocumentWordprocessingmlWebsettingsXml
      , Application_VndOpenxmlformatsPackageCorePropertiesXml
      , Application_VndOpenxmlformatsPackageDigitalSignatureXmlsignatureXml
      , Application_VndOpenxmlformatsPackageRelationshipsXml
      , Application_VndOsaNetdeploy
      , Application_VndOsgeoMapguidePackage
      , Application_VndOsgiBundle
      , Application_VndOsgiDp
      , Application_VndOsgiSubsystem
      , Application_VndOtpsCtKipXml
      , Application_VndPalm
      , Application_VndPaosXml
      , Application_VndPawaafile
      , Application_VndPgFormat
      , Application_VndPgOsasli
      , Application_VndPiaccessApplicationLicence
      , Application_VndPicsel
      , Application_VndPmiWidget
      , Application_VndPocGroupAdvertisementXml
      , Application_VndPocketlearn
      , Application_VndPowerbuilder6
      , Application_VndPowerbuilder6S
      , Application_VndPowerbuilder7
      , Application_VndPowerbuilder75
      , Application_VndPowerbuilder75S
      , Application_VndPowerbuilder7S
      , Application_VndPreminet
      , Application_VndPreviewsystemsBox
      , Application_VndProteusMagazine
      , Application_VndPublishareDeltaTree
      , Application_VndPviPtid1
      , Application_VndPwgMultiplexed
      , Application_VndPwgXhtmlPrintXml
      , Application_VndQualcommBrewAppRes
      , Application_VndQuarkQuarkxpress
      , Application_VndQuobjectQuoxdocument
      , Application_VndRadisysMomlXml
      , Application_VndRadisysMsmlAuditConfXml
      , Application_VndRadisysMsmlAuditConnXml
      , Application_VndRadisysMsmlAuditDialogXml
      , Application_VndRadisysMsmlAuditStreamXml
      , Application_VndRadisysMsmlAuditXml
      , Application_VndRadisysMsmlConfXml
      , Application_VndRadisysMsmlDialogBaseXml
      , Application_VndRadisysMsmlDialogFaxDetectXml
      , Application_VndRadisysMsmlDialogFaxSendrecvXml
      , Application_VndRadisysMsmlDialogGroupXml
      , Application_VndRadisysMsmlDialogSpeechXml
      , Application_VndRadisysMsmlDialogTransformXml
      , Application_VndRadisysMsmlDialogXml
      , Application_VndRadisysMsmlXml
      , Application_VndRainstorData
      , Application_VndRapid
      , Application_VndRealvncBed
      , Application_VndRecordareMusicxml
      , Application_VndRecordareMusicxmlXml
      , Application_VndRenlearnRlprint
      , Application_VndRigCryptonote
      , Application_VndRimCod
      , Application_VndRnRealmedia
      , Application_VndRnRealmediaVbr
      , Application_VndRnRealplayer
      , Application_VndRoute66Link66Xml
      , Application_VndRs274x
      , Application_VndRuckusDownload
      , Application_VndS3sms
      , Application_VndSailingtrackerTrack
      , Application_VndSbmCid
      , Application_VndSbmMid2
      , Application_VndScribus
      , Application_VndSealed3df
      , Application_VndSealedCsf
      , Application_VndSealedDoc
      , Application_VndSealedEml
      , Application_VndSealedMht
      , Application_VndSealedNet
      , Application_VndSealedPpt
      , Application_VndSealedTiff
      , Application_VndSealedXls
      , Application_VndSealedmediaSoftsealHtml
      , Application_VndSealedmediaSoftsealPdf
      , Application_VndSeemail
      , Application_VndSema
      , Application_VndSemd
      , Application_VndSemf
      , Application_VndShanaInformedFormdata
      , Application_VndShanaInformedFormtemplate
      , Application_VndShanaInformedInterchange
      , Application_VndShanaInformedPackage
      , Application_VndSimtechMindmapper
      , Application_VndSmaf
      , Application_VndSmartNotebook
      , Application_VndSmartTeacher
      , Application_VndSoftware602FillerFormXml
      , Application_VndSoftware602FillerFormXmlZip
      , Application_VndSolentSdkmXml
      , Application_VndSpotfireDxp
      , Application_VndSpotfireSfs
      , Application_VndSssCod
      , Application_VndSssDtf
      , Application_VndSssNtf
      , Application_VndStardivisionCalc
      , Application_VndStardivisionDraw
      , Application_VndStardivisionImpress
      , Application_VndStardivisionMath
      , Application_VndStardivisionWriter
      , Application_VndStardivisionWriterGlobal
      , Application_VndStepmaniaPackage
      , Application_VndStepmaniaStepchart
      , Application_VndStreetStream
      , Application_VndSunWadlXml
      , Application_VndSunXmlCalc
      , Application_VndSunXmlCalcTemplate
      , Application_VndSunXmlDraw
      , Application_VndSunXmlDrawTemplate
      , Application_VndSunXmlImpress
      , Application_VndSunXmlImpressTemplate
      , Application_VndSunXmlMath
      , Application_VndSunXmlWriter
      , Application_VndSunXmlWriterGlobal
      , Application_VndSunXmlWriterTemplate
      , Application_VndSusCalendar
      , Application_VndSvd
      , Application_VndSwiftviewIcs
      , Application_VndSymbianInstall
      , Application_VndSyncmlDmNotification
      , Application_VndSyncmlDmWbxml
      , Application_VndSyncmlDmXml
      , Application_VndSyncmlDsNotification
      , Application_VndSyncmlXml
      , Application_VndTaoIntentModuleArchive
      , Application_VndTcpdumpPcap
      , Application_VndTmobileLivetv
      , Application_VndTridTpt
      , Application_VndTriscapeMxs
      , Application_VndTrueapp
      , Application_VndTruedoc
      , Application_VndTveTrigger
      , Application_VndUbisoftWebplayer
      , Application_VndUfdl
      , Application_VndUiqTheme
      , Application_VndUmajin
      , Application_VndUnity
      , Application_VndUomlXml
      , Application_VndUplanetAlert
      , Application_VndUplanetAlertWbxml
      , Application_VndUplanetBearerChoice
      , Application_VndUplanetBearerChoiceWbxml
      , Application_VndUplanetCacheop
      , Application_VndUplanetCacheopWbxml
      , Application_VndUplanetChannel
      , Application_VndUplanetChannelWbxml
      , Application_VndUplanetList
      , Application_VndUplanetListWbxml
      , Application_VndUplanetListcmd
      , Application_VndUplanetListcmdWbxml
      , Application_VndUplanetSignal
      , Application_VndVcx
      , Application_VndVdStudy
      , Application_VndVectorworks
      , Application_VndVerimatrixVcas
      , Application_VndVidsoftVidconference
      , Application_VndVisio
      , Application_VndVisionary
      , Application_VndVividenceScriptfile
      , Application_VndVsf
      , Application_VndWapSic
      , Application_VndWapSlc
      , Application_VndWapWbxml
      , Application_VndWapWmlc
      , Application_VndWapWmlscriptc
      , Application_VndWebturbo
      , Application_VndWfaWsc
      , Application_VndWmc
      , Application_VndWmfBootstrap
      , Application_VndWolframMathematica
      , Application_VndWolframMathematicaPackage
      , Application_VndWolframPlayer
      , Application_VndWordperfect
      , Application_VndWqd
      , Application_VndWrqHp3000Labelled
      , Application_VndWtStf
      , Application_VndWvCspWbxml
      , Application_VndWvCspXml
      , Application_VndWvSspXml
      , Application_VndXara
      , Application_VndXfdl
      , Application_VndXfdlWebform
      , Application_VndXmiXml
      , Application_VndXmpieCpkg
      , Application_VndXmpieDpkg
      , Application_VndXmpiePlan
      , Application_VndXmpiePpkg
      , Application_VndXmpieXlim
      , Application_VndYamahaHvDic
      , Application_VndYamahaHvScript
      , Application_VndYamahaHvVoice
      , Application_VndYamahaOpenscoreformat
      , Application_VndYamahaOpenscoreformatOsfpvgXml
      , Application_VndYamahaRemoteSetup
      , Application_VndYamahaSmafAudio
      , Application_VndYamahaSmafPhrase
      , Application_VndYamahaThroughNgn
      , Application_VndYamahaTunnelUdpencap
      , Application_VndYellowriverCustomMenu
      , Application_VndZul
      , Application_VndZzazzDeckXml
      , Application_VocaltecMediaDesc
      , Application_VocaltecMediaFile
      , Application_VoicexmlXml
      , Application_VqRtcpxr
      , Application_WatcherinfoXml
      , Application_WhoisppQuery
      , Application_WhoisppResponse
      , Application_Widget
      , Application_Winhlp
      , Application_Wita
      , Application_Wordperfect
      , Application_Wordperfect51
      , Application_Wordperfect60
      , Application_Wordperfect61
      , Application_WsdlXml
      , Application_WspolicyXml
      , Application_X123
      , Application_X400Bp
      , Application_X7zCompressed
      , Application_XAbiword
      , Application_XAceCompressed
      , Application_XAim
      , Application_XAmf
      , Application_XAppleDiskimage
      , Application_XAuthorwareBin
      , Application_XAuthorwareMap
      , Application_XAuthorwareSeg
      , Application_XBcpio
      , Application_XBinary
      , Application_XBinhex40
      , Application_XBittorrent
      , Application_XBlorb
      , Application_XBsh
      , Application_XBytecodeElisp
      , Application_XBytecodeElispCompiledelisp
      , Application_XBytecodePython
      , Application_XBzip
      , Application_XBzip2
      , Application_XCbr
      , Application_XCdf
      , Application_XCdlink
      , Application_XCfsCompressed
      , Application_XChat
      , Application_XChessPgn
      , Application_XChm
      , Application_XChromeExtension
      , Application_XCmuRaster
      , Application_XCocoa
      , Application_XCompactpro
      , Application_XCompress
      , Application_XCompressed
      , Application_XConference
      , Application_XCore
      , Application_XCpio
      , Application_XCpt
      , Application_XCsh
      , Application_XDebianPackage
      , Application_XDeepv
      , Application_XDgcCompressed
      , Application_XDirector
      , Application_XDms
      , Application_XDoom
      , Application_XDtbncxXml
      , Application_XDtbookXml
      , Application_XDtbresourceXml
      , Application_XDvi
      , Application_XElc
      , Application_XEnvoy
      , Application_XEsrehber
      , Application_XEva
      , Application_XExcel
      , Application_XExecutable
      , Application_XFlac
      , Application_XFont
      , Application_XFontBdf
      , Application_XFontDos
      , Application_XFontFramemaker
      , Application_XFontGhostscript
      , Application_XFontLibgrx
      , Application_XFontLinuxPsf
      , Application_XFontOtf
      , Application_XFontPcf
      , Application_XFontSnf
      , Application_XFontSpeedo
      , Application_XFontSunosNews
      , Application_XFontTtf
      , Application_XFontType1
      , Application_XFontVfont
      , Application_XFontWoff
      , Application_XFrame
      , Application_XFreearc
      , Application_XFreelance
      , Application_XFuturesplash
      , Application_XGcaCompressed
      , Application_XGlulx
      , Application_XGnumeric
      , Application_XGoSgf
      , Application_XGrampsXml
      , Application_XGraphingCalculator
      , Application_XGsp
      , Application_XGss
      , Application_XGtar
      , Application_XGzip
      , Application_XHdf
      , Application_XHelpfile
      , Application_XHttpdImap
      , Application_XHttpdPhp
      , Application_XHttpdPhp3
      , Application_XHttpdPhp3Preprocessed
      , Application_XHttpdPhp4
      , Application_XHttpdPhpSource
      , Application_XIca
      , Application_XIma
      , Application_XInstallInstructions
      , Application_XInternetSignup
      , Application_XInternettSignup
      , Application_XInventor
      , Application_XIp2
      , Application_XIphone
      , Application_XIso9660Image
      , Application_XJavaApplet
      , Application_XJavaArchive
      , Application_XJavaBean
      , Application_XJavaClass
      , Application_XJavaCommerce
      , Application_XJavaJnlpFile
      , Application_XJavaSerializedObject
      , Application_XJavaVm
      , Application_XJavascript
      , Application_XKchart
      , Application_XKdelnk
      , Application_XKillustrator
      , Application_XKoan
      , Application_XKpresenter
      , Application_XKsh
      , Application_XKspread
      , Application_XKword
      , Application_XLatex
      , Application_XLha
      , Application_XLisp
      , Application_XLivescreen
      , Application_XLotus
      , Application_XLotusscreencam
      , Application_XLuaBytecode
      , Application_XLzh
      , Application_XLzhCompressed
      , Application_XLzx
      , Application_XMacBinhex40
      , Application_XMacbinary
      , Application_XMagicCapPackage10
      , Application_XMaker
      , Application_XMathcad
      , Application_XMeme
      , Application_XMidi
      , Application_XMie
      , Application_XMif
      , Application_XMixTransfer
      , Application_XMobipocketEbook
      , Application_XMpegurl
      , Application_XMplayer2
      , Application_XMsApplication
      , Application_XMsShortcut
      , Application_XMsWmd
      , Application_XMsWmz
      , Application_XMsXbap
      , Application_XMsaccess
      , Application_XMsbinder
      , Application_XMscardfile
      , Application_XMsclip
      , Application_XMsdosProgram
      , Application_XMsdownload
      , Application_XMsexcel
      , Application_XMsi
      , Application_XMsmediaview
      , Application_XMsmetafile
      , Application_XMsmoney
      , Application_XMspowerpoint
      , Application_XMspublisher
      , Application_XMsschedule
      , Application_XMsterminal
      , Application_XMswrite
      , Application_XNaviAnimation
      , Application_XNavidoc
      , Application_XNavimap
      , Application_XNavistyle
      , Application_XNetcdf
      , Application_XNewtonCompatiblePkg
      , Application_XNokia9000CommunicatorAddOnSoftware
      , Application_XNsProxyAutoconfig
      , Application_XNwc
      , Application_XNzb
      , Application_XObject
      , Application_XOmc
      , Application_XOmcdatamaker
      , Application_XOmcregerator
      , Application_XOzApplication
      , Application_XPagemaker
      , Application_XPcl
      , Application_XPerfmon
      , Application_XPixclscript
      , Application_XPkcs10
      , Application_XPkcs12
      , Application_XPkcs7Certificates
      , Application_XPkcs7Certreqresp
      , Application_XPkcs7Crl
      , Application_XPkcs7Mime
      , Application_XPkcs7Signature
      , Application_XPointplus
      , Application_XPortableAnymap
      , Application_XProject
      , Application_XPythonCode
      , Application_XQpro
      , Application_XQuicktimeplayer
      , Application_XRarCompressed
      , Application_XRedhatPackageManager
      , Application_XResearchInfoSystems
      , Application_XRpm
      , Application_XRtf
      , Application_XRx
      , Application_XSdp
      , Application_XSea
      , Application_XSeelogo
      , Application_XSh
      , Application_XShar
      , Application_XShellscript
      , Application_XShockwaveFlash
      , Application_XSilverlightApp
      , Application_XSit
      , Application_XSprite
      , Application_XSql
      , Application_XStuffit
      , Application_XStuffitx
      , Application_XSubrip
      , Application_XSv4cpio
      , Application_XSv4crc
      , Application_XT3vmImage
      , Application_XTads
      , Application_XTar
      , Application_XTbook
      , Application_XTcl
      , Application_XTex
      , Application_XTexGf
      , Application_XTexPk
      , Application_XTexTfm
      , Application_XTexinfo
      , Application_XTgif
      , Application_XTrash
      , Application_XTroff
      , Application_XTroffMan
      , Application_XTroffMe
      , Application_XTroffMs
      , Application_XTroffMsvideo
      , Application_XUstar
      , Application_XVideolan
      , Application_XVisio
      , Application_XVndAudioexplosionMzz
      , Application_XVndLsXpix
      , Application_XVrml
      , Application_XWaisSource
      , Application_XWebAppManifestJson
      , Application_XWingz
      , Application_XWinhelp
      , Application_XWintalk
      , Application_XWorld
      , Application_XWpwin
      , Application_XWri
      , Application_XX509CaCert
      , Application_XX509UserCert
      , Application_XXcf
      , Application_XXfig
      , Application_XXliffXml
      , Application_XXpinstall
      , Application_XXz
      , Application_XZipCompressed
      , Application_XZmachine
      , Application_XamlXml
      , Application_XcapAttXml
      , Application_XcapCapsXml
      , Application_XcapDiffXml
      , Application_XcapElXml
      , Application_XcapErrorXml
      , Application_XcapNsXml
      , Application_XconConferenceInfoDiffXml
      , Application_XconConferenceInfoXml
      , Application_XencXml
      , Application_XhtmlVoiceXml
      , Application_XhtmlXml
      , Application_Xml
      , Application_XmlDtd
      , Application_XmlExternalParsedEntity
      , Application_XmppXml
      , Application_XopXml
      , Application_XprocXml
      , Application_XsltXml
      , Application_XspfXml
      , Application_XvXml
      , Application_Yang
      , Application_YinXml
      , Application_YndMsPkipko
      , Application_Zip
      , Audio_1dInterleavedParityfec
      , Audio_32kadpcm
      , Audio_3gpp
      , Audio_3gpp2
      , Audio_Ac3
      , Audio_Adpcm
      , Audio_Aiff
      , Audio_Amr
      , Audio_AmrWb
      , Audio_Asc
      , Audio_Atrac3
      , Audio_AtracAdvancedLossless
      , Audio_AtracX
      , Audio_Basic
      , Audio_Bv16
      , Audio_Bv32
      , Audio_Clearmode
      , Audio_Cn
      , Audio_Dat12
      , Audio_Dls
      , Audio_DsrEs201108
      , Audio_DsrEs202050
      , Audio_DsrEs202211
      , Audio_DsrEs202212
      , Audio_Dv
      , Audio_Dvi4
      , Audio_Eac3
      , Audio_Evrc
      , Audio_Evrc0
      , Audio_Evrc1
      , Audio_EvrcQcp
      , Audio_Evrcb
      , Audio_Evrcb0
      , Audio_Evrcb1
      , Audio_Evrcwb
      , Audio_Evrcwb0
      , Audio_Evrcwb1
      , Audio_Example
      , Audio_Flac
      , Audio_Fwdred
      , Audio_G719
      , Audio_G722
      , Audio_G7221
      , Audio_G723
      , Audio_G72616
      , Audio_G72624
      , Audio_G72632
      , Audio_G72640
      , Audio_G728
      , Audio_G729
      , Audio_G7291
      , Audio_G729d
      , Audio_G729e
      , Audio_Gsm
      , Audio_GsmEfr
      , Audio_GsmHr08
      , Audio_Ilbc
      , Audio_IpMr_v25
      , Audio_Isac
      , Audio_It
      , Audio_L16
      , Audio_L20
      , Audio_L24
      , Audio_L8
      , Audio_Lpc
      , Audio_Make
      , Audio_MakeMyFunk
      , Audio_Mid
      , Audio_Midi
      , Audio_MobileXmf
      , Audio_Mod
      , Audio_Mp4
      , Audio_Mp4aLatm
      , Audio_Mpa
      , Audio_MpaRobust
      , Audio_Mpeg
      , Audio_Mpeg3
      , Audio_Mpeg4Generic
      , Audio_Mpegurl
      , Audio_Musepack
      , Audio_Nspaudio
      , Audio_Ogg
      , Audio_Opus
      , Audio_Parityfec
      , Audio_Pcma
      , Audio_PcmaWb
      , Audio_Pcmu
      , Audio_PcmuWb
      , Audio_PrsSid
      , Audio_Qcelp
      , Audio_Red
      , Audio_RtpEncAescm128
      , Audio_RtpMidi
      , Audio_Rtx
      , Audio_S3m
      , Audio_Silk
      , Audio_Smv
      , Audio_Smv0
      , Audio_SmvQcp
      , Audio_SpMidi
      , Audio_Speex
      , Audio_T140c
      , Audio_T38
      , Audio_TelephoneEvent
      , Audio_Tone
      , Audio_TspAudio
      , Audio_Tsplayer
      , Audio_Uemclip
      , Audio_Ulpfec
      , Audio_Vdvi
      , Audio_VmrWb
      , Audio_Vnd3gppIufp
      , Audio_Vnd4sb
      , Audio_VndAudiokoz
      , Audio_VndCelp
      , Audio_VndCiscoNse
      , Audio_VndCmlesRadioEvents
      , Audio_VndCnsAnp1
      , Audio_VndCnsInf1
      , Audio_VndDeceAudio
      , Audio_VndDigitalWinds
      , Audio_VndDlnaAdts
      , Audio_VndDolbyHeaac1
      , Audio_VndDolbyHeaac2
      , Audio_VndDolbyMlp
      , Audio_VndDolbyMps
      , Audio_VndDolbyPl2
      , Audio_VndDolbyPl2x
      , Audio_VndDolbyPl2z
      , Audio_VndDolbyPulse1
      , Audio_VndDra
      , Audio_VndDts
      , Audio_VndDtsHd
      , Audio_VndDvbFile
      , Audio_VndEveradPlj
      , Audio_VndHnsAudio
      , Audio_VndLucentVoice
      , Audio_VndMsPlayreadyMediaPya
      , Audio_VndNokiaMobileXmf
      , Audio_VndNortelVbk
      , Audio_VndNueraEcelp4800
      , Audio_VndNueraEcelp7470
      , Audio_VndNueraEcelp9600
      , Audio_VndOctelSbc
      , Audio_VndQcelp
      , Audio_VndRhetorex32kadpcm
      , Audio_VndRip
      , Audio_VndSealedmediaSoftsealMpeg
      , Audio_VndVmxCvsd
      , Audio_Voc
      , Audio_Vorbis
      , Audio_VorbisConfig
      , Audio_Voxware
      , Audio_Wav
      , Audio_Webm
      , Audio_XAac
      , Audio_XAdpcm
      , Audio_XAiff
      , Audio_XAu
      , Audio_XCaf
      , Audio_XFlac
      , Audio_XGsm
      , Audio_XJam
      , Audio_XLiveaudio
      , Audio_XMatroska
      , Audio_XMid
      , Audio_XMidi
      , Audio_XMod
      , Audio_XMpeg
      , Audio_XMpeg3
      , Audio_XMpegurl
      , Audio_XMpequrl
      , Audio_XMsWax
      , Audio_XMsWma
      , Audio_XNspaudio
      , Audio_XPnRealaudio
      , Audio_XPnRealaudioPlugin
      , Audio_XPsid
      , Audio_XRealaudio
      , Audio_XScpls
      , Audio_XSd2
      , Audio_XTta
      , Audio_XTwinvq
      , Audio_XTwinvqPlugin
      , Audio_XVndAudioexplosionMjuicemediafile
      , Audio_XVoc
      , Audio_XWav
      , Audio_Xm
      , Chemical_XCdx
      , Chemical_XCif
      , Chemical_XCmdf
      , Chemical_XCml
      , Chemical_XCsml
      , Chemical_XPdb
      , Chemical_XXyz
      , Content_Unknown
      , Drawing_XDwf
      , Drawing_XDwfOld
      , Font_Opentype
      , IWorld_IVrml
      , Image_Bmp
      , Image_Cgm
      , Image_CisCod
      , Image_CmuRaster
      , Image_Example
      , Image_Fif
      , Image_Fits
      , Image_Florian
      , Image_G3fax
      , Image_Gif
      , Image_Ief
      , Image_Jp2
      , Image_Jpeg
      , Image_Jpm
      , Image_Jpx
      , Image_Jutvision
      , Image_Ktx
      , Image_Naplps
      , Image_Pcx
      , Image_Pict
      , Image_Pipeg
      , Image_Pjpeg
      , Image_Png
      , Image_PrsBtif
      , Image_PrsPti
      , Image_Sgi
      , Image_SvgXml
      , Image_T38
      , Image_Tiff
      , Image_TiffFx
      , Image_Vasa
      , Image_VndAdobePhotoshop
      , Image_VndCnsInf2
      , Image_VndDeceGraphic
      , Image_VndDjvu
      , Image_VndDvbSubtitle
      , Image_VndDwg
      , Image_VndDxf
      , Image_VndFastbidsheet
      , Image_VndFpx
      , Image_VndFst
      , Image_VndFujixeroxEdmicsMmr
      , Image_VndFujixeroxEdmicsRlc
      , Image_VndGlobalgraphicsPgb
      , Image_VndMicrosoftIcon
      , Image_VndMix
      , Image_VndMsModi
      , Image_VndMsPhoto
      , Image_VndNetFpx
      , Image_VndRadiance
      , Image_VndRnRealflash
      , Image_VndRnRealpix
      , Image_VndSealedPng
      , Image_VndSealedmediaSoftsealGif
      , Image_VndSealedmediaSoftsealJpg
      , Image_VndSvf
      , Image_VndWapWbmp
      , Image_VndXiff
      , Image_Webp
      , Image_X3ds
      , Image_XCmuRast
      , Image_XCmuRaster
      , Image_XCmx
      , Image_XCoreldraw
      , Image_XCoreldrawpattern
      , Image_XCoreldrawtemplate
      , Image_XCorelphotopaint
      , Image_XDwg
      , Image_XFreehand
      , Image_XIcon
      , Image_XJg
      , Image_XJng
      , Image_XJps
      , Image_XMrsidImage
      , Image_XMsBmp
      , Image_XNiff
      , Image_XPcx
      , Image_XPhotoshop
      , Image_XPict
      , Image_XPortableAnymap
      , Image_XPortableBitmap
      , Image_XPortableGraymap
      , Image_XPortableGreymap
      , Image_XPortablePixmap
      , Image_XQuicktime
      , Image_XRgb
      , Image_XTga
      , Image_XTiff
      , Image_XWindowsBmp
      , Image_XXbitmap
      , Image_XXbm
      , Image_XXpixmap
      , Image_XXwd
      , Image_XXwindowdump
      , Image_Xbm
      , Image_Xpm
      , Inode_Blockdevice
      , Inode_Chardevice
      , Inode_Directory
      , Inode_DirectoryLocked
      , Inode_Fifo
      , Inode_Socket
      , Message_Cpim
      , Message_DeliveryStatus
      , Message_DispositionNotification
      , Message_Example
      , Message_ExternalBody
      , Message_FeedbackReport
      , Message_Global
      , Message_GlobalDeliveryStatus
      , Message_GlobalDispositionNotification
      , Message_GlobalHeaders
      , Message_Http
      , Message_ImdnXml
      , Message_News
      , Message_Partial
      , Message_Rfc822
      , Message_SHttp
      , Message_Sip
      , Message_Sipfrag
      , Message_TrackingStatus
      , Message_VndSiSimp
      , Model_Example
      , Model_Iges
      , Model_Mesh
      , Model_VndColladaXml
      , Model_VndDwf
      , Model_VndFlatland3dml
      , Model_VndGdl
      , Model_VndGsGdl
      , Model_VndGtw
      , Model_VndMomlXml
      , Model_VndMts
      , Model_VndParasolidTransmitBinary
      , Model_VndParasolidTransmitText
      , Model_VndVtu
      , Model_Vrml
      , Model_X3dBinary
      , Model_X3dVrml
      , Model_X3dXml
      , Model_XPov
      , Multipart_Alternative
      , Multipart_Appledouble
      , Multipart_Byteranges
      , Multipart_Digest
      , Multipart_Encrypted
      , Multipart_Example
      , Multipart_FormData
      , Multipart_HeaderSet
      , Multipart_Mixed
      , Multipart_Parallel
      , Multipart_Related
      , Multipart_Report
      , Multipart_Signed
      , Multipart_VoiceMessage
      , Multipart_XGzip
      , Multipart_XUstar
      , Multipart_XZip
      , Music_Crescendo
      , Music_XKaraoke
      , Paleovu_XPv
      , Text_1dInterleavedParityfec
      , Text_Asp
      , Text_CacheManifest
      , Text_Calendar
      , Text_CommaSeparatedValues
      , Text_Css
      , Text_Csv
      , Text_Directory
      , Text_Dns
      , Text_Ecmascript
      , Text_English
      , Text_Enriched
      , Text_EventStream
      , Text_Example
      , Text_Fwdred
      , Text_H323
      , Text_Html
      , Text_Iuls
      , Text_Javascript
      , Text_Mathml
      , Text_Mcf
      , Text_N3
      , Text_Parityfec
      , Text_Pascal
      , Text_Plain
      , Text_PlainBas
      , Text_PrsFallensteinRst
      , Text_PrsLinesTag
      , Text_Red
      , Text_Rfc822Headers
      , Text_Richtext
      , Text_Rtf
      , Text_RtpEncAescm128
      , Text_Rtx
      , Text_Scriplet
      , Text_Scriptlet
      , Text_Sgml
      , Text_T140
      , Text_TabSeparatedValues
      , Text_Texmacs
      , Text_Troff
      , Text_Turtle
      , Text_Ulpfec
      , Text_UriList
      , Text_Vcard
      , Text_VndAbc
      , Text_VndCurl
      , Text_VndCurlDcurl
      , Text_VndCurlMcurl
      , Text_VndCurlScurl
      , Text_VndDmclientscript
      , Text_VndDvbSubtitle
      , Text_VndEsmertecThemeDescriptor
      , Text_VndFlatland3dml
      , Text_VndFly
      , Text_VndFmiFlexstor
      , Text_VndGraphviz
      , Text_VndIn3d3dml
      , Text_VndIn3dSpot
      , Text_VndIptcNewsml
      , Text_VndIptcNitf
      , Text_VndLatexZ
      , Text_VndMotorolaReflex
      , Text_VndMsMediapackage
      , Text_VndNet2phoneCommcenterCommand
      , Text_VndRadisysMsmlBasicLayout
      , Text_VndRnRealtext
      , Text_VndSiUricatalogue
      , Text_VndSunJ2meAppDescriptor
      , Text_VndTrolltechLinguist
      , Text_VndWapSi
      , Text_VndWapSl
      , Text_VndWapWml
      , Text_VndWapWmlscript
      , Text_Vtt
      , Text_Webviewhtml
      , Text_XAsm
      , Text_XAudiosoftIntra
      , Text_XC
      , Text_XCHdr
      , Text_XCSrc
      , Text_XChdr
      , Text_XComponent
      , Text_XCrontab
      , Text_XCsh
      , Text_XCsrc
      , Text_XFortran
      , Text_XH
      , Text_XJava
      , Text_XJavaSource
      , Text_XLaAsf
      , Text_XLua
      , Text_XM
      , Text_XMakefile
      , Text_XMarkdown
      , Text_XMoc
      , Text_XNfo
      , Text_XOpml
      , Text_XPascal
      , Text_XPcsGcd
      , Text_XPerl
      , Text_XPython
      , Text_XScript
      , Text_XScriptCsh
      , Text_XScriptElisp
      , Text_XScriptGuile
      , Text_XScriptKsh
      , Text_XScriptLisp
      , Text_XScriptPerl
      , Text_XScriptPerlModule
      , Text_XScriptPhyton
      , Text_XScriptRexx
      , Text_XScriptScheme
      , Text_XScriptSh
      , Text_XScriptTcl
      , Text_XScriptTcsh
      , Text_XScriptZsh
      , Text_XServerParsedHtml
      , Text_XSetext
      , Text_XSfv
      , Text_XSgml
      , Text_XSh
      , Text_XSpeech
      , Text_XTcl
      , Text_XTex
      , Text_XUil
      , Text_XUuencode
      , Text_XVcalendar
      , Text_XVcard
      , Text_Xml
      , Text_XmlExternalParsedEntity
      , Unknown_Unknown
      , Video_1dInterleavedParityfec
      , Video_3gpp
      , Video_3gpp2
      , Video_3gppTt
      , Video_Animaflex
      , Video_Avi
      , Video_AvsVideo
      , Video_Bmpeg
      , Video_Bt656
      , Video_Celb
      , Video_Dl
      , Video_Dv
      , Video_Example
      , Video_Flc
      , Video_Fli
      , Video_Gl
      , Video_H261
      , Video_H263
      , Video_H2631998
      , Video_H2632000
      , Video_H264
      , Video_H264Rcdo
      , Video_H264Svc
      , Video_Jpeg
      , Video_Jpeg2000
      , Video_Jpm
      , Video_Mj2
      , Video_Mp1s
      , Video_Mp2p
      , Video_Mp2t
      , Video_Mp4
      , Video_Mp4vEs
      , Video_Mpeg
      , Video_Mpeg4Generic
      , Video_Mpv
      , Video_Msvideo
      , Video_Nv
      , Video_Ogg
      , Video_Parityfec
      , Video_Pointer
      , Video_Quicktime
      , Video_Raw
      , Video_RtpEncAescm128
      , Video_Rtx
      , Video_Smpte292m
      , Video_Ulpfec
      , Video_Vc1
      , Video_Vdo
      , Video_Vivo
      , Video_VndCctv
      , Video_VndDeceHd
      , Video_VndDeceMobile
      , Video_VndDeceMp4
      , Video_VndDecePd
      , Video_VndDeceSd
      , Video_VndDeceVideo
      , Video_VndDirectvMpeg
      , Video_VndDirectvMpegTts
      , Video_VndDlnaMpegTts
      , Video_VndDvbFile
      , Video_VndFvt
      , Video_VndHnsVideo
      , Video_VndIptvforum1dparityfec1010
      , Video_VndIptvforum1dparityfec2005
      , Video_VndIptvforum2dparityfec1010
      , Video_VndIptvforum2dparityfec2005
      , Video_VndIptvforumTtsavc
      , Video_VndIptvforumTtsmpeg2
      , Video_VndMotorolaVideo
      , Video_VndMotorolaVideop
      , Video_VndMpegurl
      , Video_VndMsPlayreadyMediaPyv
      , Video_VndMts
      , Video_VndNokiaInterleavedMultimedia
      , Video_VndNokiaVideovoip
      , Video_VndObjectvideo
      , Video_VndRnRealvideo
      , Video_VndSealedMpeg1
      , Video_VndSealedMpeg4
      , Video_VndSealedSwf
      , Video_VndSealedmediaSoftsealMov
      , Video_VndUvvuMp4
      , Video_VndVivo
      , Video_Vosaic
      , Video_Webm
      , Video_XAmtDemorun
      , Video_XAmtShowrun
      , Video_XAtomic3dFeature
      , Video_XDl
      , Video_XDv
      , Video_XF4v
      , Video_XFli
      , Video_XFlv
      , Video_XGl
      , Video_XIsvideo
      , Video_XLaAsf
      , Video_XM4v
      , Video_XMatroska
      , Video_XMng
      , Video_XMotionJpeg
      , Video_XMpeg
      , Video_XMpeq2a
      , Video_XMsAsf
      , Video_XMsAsfPlugin
      , Video_XMsVob
      , Video_XMsWm
      , Video_XMsWmv
      , Video_XMsWmx
      , Video_XMsWvx
      , Video_XMsvideo
      , Video_XQtc
      , Video_XScm
      , Video_XSgiMovie
      , Video_XSmv
      , Windows_Metafile
      , Www_Mime
      , XConference_XCooltalk
      , XMusic_XMidi
      , XWorld_X3dmf
      , XWorld_XSvr
      , XWorld_XVrml
      , XWorld_XVrt
      , Xgl_Drawing
      , Xgl_Movie
      )
  , mimeTypeToBytes
  , mimeTypeToText
  , mimeTypeExtensions
  ) where
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data MIME_Type
  = Application_1dInterleavedParityfec
  | Application_3gppImsXml
  | Application_Acad
  | Application_Activemessage
  | Application_AndrewInset
  | Application_Applefile
  | Application_Applixware
  | Application_Arj
  | Application_AtomXml
  | Application_AtomcatXml
  | Application_Atomicmail
  | Application_AtomsvcXml
  | Application_AuthPolicyXml
  | Application_Base64
  | Application_BatchSmtp
  | Application_BeepXml
  | Application_Binhex
  | Application_Binhex4
  | Application_Book
  | Application_CalendarXml
  | Application_Cals1840
  | Application_CcmpXml
  | Application_CcxmlXml
  | Application_Cdf
  | Application_CdmiCapability
  | Application_CdmiContainer
  | Application_CdmiDomain
  | Application_CdmiObject
  | Application_CdmiQueue
  | Application_Cea2018Xml
  | Application_CellmlXml
  | Application_Cfw
  | Application_Clariscad
  | Application_CnrpXml
  | Application_Commonground
  | Application_ConferenceInfoXml
  | Application_CplXml
  | Application_CstaXml
  | Application_CstadataXml
  | Application_CuSeeme
  | Application_Cybercash
  | Application_DavmountXml
  | Application_DcaRft
  | Application_DecDx
  | Application_DialogInfoXml
  | Application_Dicom
  | Application_Dns
  | Application_DocbookXml
  | Application_Drafting
  | Application_DskppXml
  | Application_Dsptype
  | Application_DsscDer
  | Application_DsscXml
  | Application_Dvcs
  | Application_Dxf
  | Application_Ecmascript
  | Application_EdiConsent
  | Application_EdiX12
  | Application_Edifact
  | Application_EmmaXml
  | Application_Envoy
  | Application_EppXml
  | Application_EpubZip
  | Application_Eshop
  | Application_Example
  | Application_Excel
  | Application_Exi
  | Application_Fastinfoset
  | Application_Fastsoap
  | Application_Fits
  | Application_FontTdpfr
  | Application_FontWoff
  | Application_Fractals
  | Application_FrameworkAttributesXml
  | Application_Freeloader
  | Application_Futuresplash
  | Application_Ghostview
  | Application_GmlXml
  | Application_Gnutar
  | Application_GpxXml
  | Application_Groupwise
  | Application_Gxf
  | Application_H224
  | Application_HeldXml
  | Application_Hlp
  | Application_Hta
  | Application_Http
  | Application_Hyperstudio
  | Application_IDeas
  | Application_IbeKeyRequestXml
  | Application_IbePkgReplyXml
  | Application_IbePpData
  | Application_Iges
  | Application_ImIscomposingXml
  | Application_Index
  | Application_IndexCmd
  | Application_IndexObj
  | Application_IndexResponse
  | Application_IndexVnd
  | Application_Inf
  | Application_InkmlXml
  | Application_InternetPropertyStream
  | Application_Iotp
  | Application_Ipfix
  | Application_Ipp
  | Application_Isup
  | Application_Java
  | Application_JavaArchive
  | Application_JavaByteCode
  | Application_JavaSerializedObject
  | Application_JavaVm
  | Application_Javascript
  | Application_Json
  | Application_JsonmlJson
  | Application_KpmlRequestXml
  | Application_KpmlResponseXml
  | Application_Lha
  | Application_LostXml
  | Application_Lzx
  | Application_MacBinary
  | Application_MacBinhex
  | Application_MacBinhex40
  | Application_MacCompactpro
  | Application_Macbinary
  | Application_Macwriteii
  | Application_MadsXml
  | Application_Marc
  | Application_MarcxmlXml
  | Application_Mathematica
  | Application_MathematicaOld
  | Application_MathmlContentXml
  | Application_MathmlPresentationXml
  | Application_MathmlXml
  | Application_Mbedlet
  | Application_MbmsAssociatedProcedureDescriptionXml
  | Application_MbmsDeregisterXml
  | Application_MbmsEnvelopeXml
  | Application_MbmsMskResponseXml
  | Application_MbmsMskXml
  | Application_MbmsProtectionDescriptionXml
  | Application_MbmsReceptionReportXml
  | Application_MbmsRegisterResponseXml
  | Application_MbmsRegisterXml
  | Application_MbmsUserServiceDescriptionXml
  | Application_Mbox
  | Application_Mcad
  | Application_Media_controlXml
  | Application_MediaservercontrolXml
  | Application_Metalink4Xml
  | Application_MetalinkXml
  | Application_MetsXml
  | Application_Mikey
  | Application_Mime
  | Application_ModsXml
  | Application_MossKeys
  | Application_MossSignature
  | Application_MosskeyData
  | Application_MosskeyRequest
  | Application_Mp21
  | Application_Mp4
  | Application_Mpeg4Generic
  | Application_Mpeg4Iod
  | Application_Mpeg4IodXmt
  | Application_Msaccess
  | Application_MscIvrXml
  | Application_MscMixerXml
  | Application_Msonenote
  | Application_Mspowerpoint
  | Application_Msword
  | Application_Mswrite
  | Application_Mxf
  | Application_Nasdata
  | Application_Netmc
  | Application_NewsCheckgroups
  | Application_NewsGroupinfo
  | Application_NewsMessageId
  | Application_NewsTransmission
  | Application_Nss
  | Application_OcspRequest
  | Application_OcspResponse
  | Application_OctetStream
  | Application_Oda
  | Application_OebpsPackageXml
  | Application_Ogg
  | Application_Olescript
  | Application_OmdocXml
  | Application_Onenote
  | Application_Oxps
  | Application_Parityfec
  | Application_PatchOpsErrorXml
  | Application_Pdf
  | Application_PgpEncrypted
  | Application_PgpKeys
  | Application_PgpSignature
  | Application_PicsRules
  | Application_PidfDiffXml
  | Application_PidfXml
  | Application_Pkcs10
  | Application_Pkcs12
  | Application_Pkcs7Mime
  | Application_Pkcs7Signature
  | Application_Pkcs8
  | Application_PkcsCrl
  | Application_PkixAttrCert
  | Application_PkixCert
  | Application_PkixCrl
  | Application_PkixPkipath
  | Application_Pkixcmp
  | Application_Plain
  | Application_PlsXml
  | Application_PocSettingsXml
  | Application_Postscript
  | Application_Powerpoint
  | Application_Pro_eng
  | Application_PrsAlvestrandTitraxSheet
  | Application_PrsCww
  | Application_PrsNprend
  | Application_PrsPlucker
  | Application_PrsRdfXmlCrypt
  | Application_PrsXsfXml
  | Application_PskcXml
  | Application_Qsig
  | Application_Rar
  | Application_RdfXml
  | Application_ReginfoXml
  | Application_RelaxNgCompactSyntax
  | Application_RemotePrinting
  | Application_ResourceListsDiffXml
  | Application_ResourceListsXml
  | Application_RingingTones
  | Application_Riscos
  | Application_RlmiXml
  | Application_RlsServicesXml
  | Application_RpkiGhostbusters
  | Application_RpkiManifest
  | Application_RpkiRoa
  | Application_RpkiUpdown
  | Application_RsdXml
  | Application_RssXml
  | Application_Rtf
  | Application_Rtx
  | Application_SamlassertionXml
  | Application_SamlmetadataXml
  | Application_SbmlXml
  | Application_ScvpCvRequest
  | Application_ScvpCvResponse
  | Application_ScvpVpRequest
  | Application_ScvpVpResponse
  | Application_Sdp
  | Application_Sea
  | Application_Set
  | Application_SetPayment
  | Application_SetPaymentInitiation
  | Application_SetRegistration
  | Application_SetRegistrationInitiation
  | Application_Sgml
  | Application_SgmlOpenCatalog
  | Application_ShfXml
  | Application_Sieve
  | Application_SimpleFilterXml
  | Application_SimpleMessageSummary
  | Application_Simplesymbolcontainer
  | Application_Sla
  | Application_Slate
  | Application_Smil
  | Application_SmilXml
  | Application_SoapFastinfoset
  | Application_SoapXml
  | Application_Solids
  | Application_Sounder
  | Application_SparqlQuery
  | Application_SparqlResultsXml
  | Application_SpiritsEventXml
  | Application_Srgs
  | Application_SrgsXml
  | Application_SruXml
  | Application_SsdlXml
  | Application_SsmlXml
  | Application_Step
  | Application_Streamingmedia
  | Application_TampApexUpdate
  | Application_TampApexUpdateConfirm
  | Application_TampCommunityUpdate
  | Application_TampCommunityUpdateConfirm
  | Application_TampError
  | Application_TampSequenceAdjust
  | Application_TampSequenceAdjustConfirm
  | Application_TampStatusQuery
  | Application_TampStatusResponse
  | Application_TampUpdate
  | Application_TampUpdateConfirm
  | Application_TeiXml
  | Application_ThraudXml
  | Application_TimestampQuery
  | Application_TimestampReply
  | Application_TimestampedData
  | Application_Toolbook
  | Application_TveTrigger
  | Application_Ulpfec
  | Application_VcardXml
  | Application_Vda
  | Application_Vemmi
  | Application_VividenceScriptfile
  | Application_Vnd3gpp2BcmcsinfoXml
  | Application_Vnd3gpp2Sms
  | Application_Vnd3gpp2Tcap
  | Application_Vnd3gppBsfXml
  | Application_Vnd3gppPicBwLarge
  | Application_Vnd3gppPicBwSmall
  | Application_Vnd3gppPicBwVar
  | Application_Vnd3gppSms
  | Application_Vnd3mPostItNotes
  | Application_VndAccpacSimplyAso
  | Application_VndAccpacSimplyImp
  | Application_VndAcucobol
  | Application_VndAcucorp
  | Application_VndAdobeAirApplicationInstallerPackageZip
  | Application_VndAdobeFormscentralFcdt
  | Application_VndAdobeFxp
  | Application_VndAdobePartialUpload
  | Application_VndAdobeXdpXml
  | Application_VndAdobeXfdf
  | Application_VndAetherImp
  | Application_VndAhBarcode
  | Application_VndAheadSpace
  | Application_VndAirzipFilesecureAzf
  | Application_VndAirzipFilesecureAzs
  | Application_VndAmazonEbook
  | Application_VndAmericandynamicsAcc
  | Application_VndAmigaAmi
  | Application_VndAmundsenMazeXml
  | Application_VndAndroidPackageArchive
  | Application_VndAnserWebCertificateIssueInitiation
  | Application_VndAnserWebFundsTransferInitiation
  | Application_VndAntixGameComponent
  | Application_VndAppleInstallerXml
  | Application_VndAppleMpegurl
  | Application_VndArastraSwi
  | Application_VndAristanetworksSwi
  | Application_VndAstraeaSoftwareIota
  | Application_VndAudiograph
  | Application_VndAutopackage
  | Application_VndAvistarXml
  | Application_VndBlueiceMultipass
  | Application_VndBluetoothEpOob
  | Application_VndBmi
  | Application_VndBusinessobjects
  | Application_VndCabJscript
  | Application_VndCanonCpdl
  | Application_VndCanonLips
  | Application_VndCendioThinlincClientconf
  | Application_VndChemdrawXml
  | Application_VndChipnutsKaraokeMmd
  | Application_VndCinderella
  | Application_VndCirpackIsdnExt
  | Application_VndClaymore
  | Application_VndCloantoRp9
  | Application_VndClonkC4group
  | Application_VndCluetrustCartomobileConfig
  | Application_VndCluetrustCartomobileConfigPkg
  | Application_VndCollectionJson
  | Application_VndCommerceBattelle
  | Application_VndCommonspace
  | Application_VndComsocaller
  | Application_VndContactCmsg
  | Application_VndCosmocaller
  | Application_VndCrickClicker
  | Application_VndCrickClickerKeyboard
  | Application_VndCrickClickerPalette
  | Application_VndCrickClickerTemplate
  | Application_VndCrickClickerWordbank
  | Application_VndCriticaltoolsWbsXml
  | Application_VndCtcPosml
  | Application_VndCtctWsXml
  | Application_VndCupsPdf
  | Application_VndCupsPostscript
  | Application_VndCupsPpd
  | Application_VndCupsRaster
  | Application_VndCupsRaw
  | Application_VndCurl
  | Application_VndCurlCar
  | Application_VndCurlPcurl
  | Application_VndCybank
  | Application_VndDart
  | Application_VndDataVisionRdz
  | Application_VndDeceData
  | Application_VndDeceTtmlXml
  | Application_VndDeceUnspecified
  | Application_VndDeceZip
  | Application_VndDenovoFcselayoutLink
  | Application_VndDirBiPlateDlNosuffix
  | Application_VndDna
  | Application_VndDolbyMlp
  | Application_VndDolbyMobile1
  | Application_VndDolbyMobile2
  | Application_VndDpgraph
  | Application_VndDreamfactory
  | Application_VndDsKeypoint
  | Application_VndDvbAit
  | Application_VndDvbDvbj
  | Application_VndDvbEsgcontainer
  | Application_VndDvbIpdcdftnotifaccess
  | Application_VndDvbIpdcesgaccess
  | Application_VndDvbIpdcesgaccess2
  | Application_VndDvbIpdcesgpdd
  | Application_VndDvbIpdcroaming
  | Application_VndDvbIptvAlfecBase
  | Application_VndDvbIptvAlfecEnhancement
  | Application_VndDvbNotifAggregateRootXml
  | Application_VndDvbNotifContainerXml
  | Application_VndDvbNotifGenericXml
  | Application_VndDvbNotifIaMsglistXml
  | Application_VndDvbNotifIaRegistrationRequestXml
  | Application_VndDvbNotifIaRegistrationResponseXml
  | Application_VndDvbNotifInitXml
  | Application_VndDvbPfr
  | Application_VndDvbService
  | Application_VndDxr
  | Application_VndDynageo
  | Application_VndEasykaraokeCdgdownload
  | Application_VndEcdisUpdate
  | Application_VndEcowinChart
  | Application_VndEcowinFilerequest
  | Application_VndEcowinFileupdate
  | Application_VndEcowinSeries
  | Application_VndEcowinSeriesrequest
  | Application_VndEcowinSeriesupdate
  | Application_VndEmclientAccessrequestXml
  | Application_VndEnliven
  | Application_VndEprintsDataXml
  | Application_VndEpsonEsf
  | Application_VndEpsonMsf
  | Application_VndEpsonQuickanime
  | Application_VndEpsonSalt
  | Application_VndEpsonSsf
  | Application_VndEricssonQuickcall
  | Application_VndEszigno3Xml
  | Application_VndEtsiAocXml
  | Application_VndEtsiCugXml
  | Application_VndEtsiIptvcommandXml
  | Application_VndEtsiIptvdiscoveryXml
  | Application_VndEtsiIptvprofileXml
  | Application_VndEtsiIptvsadBcXml
  | Application_VndEtsiIptvsadCodXml
  | Application_VndEtsiIptvsadNpvrXml
  | Application_VndEtsiIptvserviceXml
  | Application_VndEtsiIptvsyncXml
  | Application_VndEtsiIptvueprofileXml
  | Application_VndEtsiMcidXml
  | Application_VndEtsiOverloadControlPolicyDatasetXml
  | Application_VndEtsiSciXml
  | Application_VndEtsiSimservsXml
  | Application_VndEtsiTslDer
  | Application_VndEtsiTslXml
  | Application_VndEudoraData
  | Application_VndEzpixAlbum
  | Application_VndEzpixPackage
  | Application_VndFSecureMobile
  | Application_VndFdf
  | Application_VndFdsnMseed
  | Application_VndFdsnSeed
  | Application_VndFfsns
  | Application_VndFints
  | Application_VndFlographit
  | Application_VndFluxtimeClip
  | Application_VndFontFontforgeSfd
  | Application_VndFramemaker
  | Application_VndFrogansFnc
  | Application_VndFrogansLtf
  | Application_VndFscWeblaunch
  | Application_VndFujitsuOasys
  | Application_VndFujitsuOasys2
  | Application_VndFujitsuOasys3
  | Application_VndFujitsuOasysgp
  | Application_VndFujitsuOasysprs
  | Application_VndFujixeroxArt4
  | Application_VndFujixeroxArtEx
  | Application_VndFujixeroxDdd
  | Application_VndFujixeroxDocuworks
  | Application_VndFujixeroxDocuworksBinder
  | Application_VndFujixeroxHbpl
  | Application_VndFutMisnet
  | Application_VndFuzzysheet
  | Application_VndGenomatixTuxedo
  | Application_VndGeocubeXml
  | Application_VndGeogebraFile
  | Application_VndGeogebraTool
  | Application_VndGeometryExplorer
  | Application_VndGeonext
  | Application_VndGeoplan
  | Application_VndGeospace
  | Application_VndGlobalplatformCardContentMgt
  | Application_VndGlobalplatformCardContentMgtResponse
  | Application_VndGmx
  | Application_VndGoogleEarthKmlXml
  | Application_VndGoogleEarthKmz
  | Application_VndGrafeq
  | Application_VndGridmp
  | Application_VndGrooveAccount
  | Application_VndGrooveHelp
  | Application_VndGrooveIdentityMessage
  | Application_VndGrooveInjector
  | Application_VndGrooveToolMessage
  | Application_VndGrooveToolTemplate
  | Application_VndGrooveVcard
  | Application_VndHalJson
  | Application_VndHalXml
  | Application_VndHandheldEntertainmentXml
  | Application_VndHbci
  | Application_VndHclBireports
  | Application_VndHheLessonPlayer
  | Application_VndHpHpgl
  | Application_VndHpHpid
  | Application_VndHpHps
  | Application_VndHpJlyt
  | Application_VndHpPcl
  | Application_VndHpPclxl
  | Application_VndHttphone
  | Application_VndHydrostatixSofData
  | Application_VndHzn3dCrossword
  | Application_VndIbmAfplinedata
  | Application_VndIbmElectronicMedia
  | Application_VndIbmMinipay
  | Application_VndIbmModcap
  | Application_VndIbmRightsManagement
  | Application_VndIbmSecureContainer
  | Application_VndIccprofile
  | Application_VndIgloader
  | Application_VndImmervisionIvp
  | Application_VndImmervisionIvu
  | Application_VndInformedcontrolRmsXml
  | Application_VndInformixVisionary
  | Application_VndInfotechProject
  | Application_VndInfotechProjectXml
  | Application_VndInnopathWampNotification
  | Application_VndInsorsIgm
  | Application_VndInterconFormnet
  | Application_VndIntergeo
  | Application_VndIntertrustDigibox
  | Application_VndIntertrustNncp
  | Application_VndIntuQbo
  | Application_VndIntuQfx
  | Application_VndIptcG2ConceptitemXml
  | Application_VndIptcG2KnowledgeitemXml
  | Application_VndIptcG2NewsitemXml
  | Application_VndIptcG2NewsmessageXml
  | Application_VndIptcG2PackageitemXml
  | Application_VndIptcG2PlanningitemXml
  | Application_VndIpunpluggedRcprofile
  | Application_VndIrepositoryPackageXml
  | Application_VndIsXpr
  | Application_VndIsacFcs
  | Application_VndJam
  | Application_VndJapannetDirectoryService
  | Application_VndJapannetJpnstoreWakeup
  | Application_VndJapannetPaymentWakeup
  | Application_VndJapannetRegistration
  | Application_VndJapannetRegistrationWakeup
  | Application_VndJapannetSetstoreWakeup
  | Application_VndJapannetVerification
  | Application_VndJapannetVerificationWakeup
  | Application_VndJcpJavameMidletRms
  | Application_VndJisp
  | Application_VndJoostJodaArchive
  | Application_VndKahootz
  | Application_VndKdeKarbon
  | Application_VndKdeKchart
  | Application_VndKdeKformula
  | Application_VndKdeKivio
  | Application_VndKdeKontour
  | Application_VndKdeKpresenter
  | Application_VndKdeKspread
  | Application_VndKdeKword
  | Application_VndKenameaapp
  | Application_VndKidspiration
  | Application_VndKinar
  | Application_VndKoan
  | Application_VndKodakDescriptor
  | Application_VndLasLasXml
  | Application_VndLibertyRequestXml
  | Application_VndLlamagraphicsLifeBalanceDesktop
  | Application_VndLlamagraphicsLifeBalanceExchangeXml
  | Application_VndLotus123
  | Application_VndLotusApproach
  | Application_VndLotusFreelance
  | Application_VndLotusNotes
  | Application_VndLotusOrganizer
  | Application_VndLotusScreencam
  | Application_VndLotusWordpro
  | Application_VndMacportsPortpkg
  | Application_VndMarlinDrmActiontokenXml
  | Application_VndMarlinDrmConftokenXml
  | Application_VndMarlinDrmLicenseXml
  | Application_VndMarlinDrmMdcf
  | Application_VndMcd
  | Application_VndMedcalcdata
  | Application_VndMediastationCdkey
  | Application_VndMeridianSlingshot
  | Application_VndMfer
  | Application_VndMfmp
  | Application_VndMicrografxFlo
  | Application_VndMicrografxIgx
  | Application_VndMif
  | Application_VndMinisoftHp3000Save
  | Application_VndMitsubishiMistyGuardTrustweb
  | Application_VndMobiusDaf
  | Application_VndMobiusDis
  | Application_VndMobiusMbk
  | Application_VndMobiusMqy
  | Application_VndMobiusMsl
  | Application_VndMobiusPlc
  | Application_VndMobiusTxf
  | Application_VndMophunApplication
  | Application_VndMophunCertificate
  | Application_VndMotorolaFlexsuite
  | Application_VndMotorolaFlexsuiteAdsi
  | Application_VndMotorolaFlexsuiteFis
  | Application_VndMotorolaFlexsuiteGotap
  | Application_VndMotorolaFlexsuiteKmr
  | Application_VndMotorolaFlexsuiteTtc
  | Application_VndMotorolaFlexsuiteWem
  | Application_VndMotorolaIprm
  | Application_VndMozillaXulXml
  | Application_VndMsArtgalry
  | Application_VndMsAsf
  | Application_VndMsCabCompressed
  | Application_VndMsColorIccprofile
  | Application_VndMsExcel
  | Application_VndMsExcelAddinMacroenabled12
  | Application_VndMsExcelSheetBinaryMacroenabled12
  | Application_VndMsExcelSheetMacroenabled12
  | Application_VndMsExcelTemplateMacroenabled12
  | Application_VndMsFontobject
  | Application_VndMsHtmlhelp
  | Application_VndMsIms
  | Application_VndMsLrm
  | Application_VndMsOfficeActivexXml
  | Application_VndMsOfficetheme
  | Application_VndMsOpentype
  | Application_VndMsOutlook
  | Application_VndMsPackageObfuscatedOpentype
  | Application_VndMsPkiCertstore
  | Application_VndMsPkiPko
  | Application_VndMsPkiSeccat
  | Application_VndMsPkiStl
  | Application_VndMsPkicertstore
  | Application_VndMsPkiseccat
  | Application_VndMsPkistl
  | Application_VndMsPlayreadyInitiatorXml
  | Application_VndMsPowerpoint
  | Application_VndMsPowerpointAddinMacroenabled12
  | Application_VndMsPowerpointPresentationMacroenabled12
  | Application_VndMsPowerpointSlideMacroenabled12
  | Application_VndMsPowerpointSlideshowMacroenabled12
  | Application_VndMsPowerpointTemplateMacroenabled12
  | Application_VndMsPrintingPrintticketXml
  | Application_VndMsProject
  | Application_VndMsTnef
  | Application_VndMsWmdrmLicChlgReq
  | Application_VndMsWmdrmLicResp
  | Application_VndMsWmdrmMeterChlgReq
  | Application_VndMsWmdrmMeterResp
  | Application_VndMsWordDocumentMacroenabled12
  | Application_VndMsWordTemplateMacroenabled12
  | Application_VndMsWorks
  | Application_VndMsWpl
  | Application_VndMsXpsdocument
  | Application_VndMseq
  | Application_VndMsign
  | Application_VndMultiadCreator
  | Application_VndMultiadCreatorCif
  | Application_VndMusicNiff
  | Application_VndMusician
  | Application_VndMuveeStyle
  | Application_VndMynfc
  | Application_VndNcdControl
  | Application_VndNcdReference
  | Application_VndNervana
  | Application_VndNetfpx
  | Application_VndNeurolanguageNlu
  | Application_VndNitf
  | Application_VndNoblenetDirectory
  | Application_VndNoblenetSealer
  | Application_VndNoblenetWeb
  | Application_VndNokiaCatalogs
  | Application_VndNokiaConfigurationMessage
  | Application_VndNokiaConmlWbxml
  | Application_VndNokiaConmlXml
  | Application_VndNokiaIptvConfigXml
  | Application_VndNokiaIsdsRadioPresets
  | Application_VndNokiaLandmarkWbxml
  | Application_VndNokiaLandmarkXml
  | Application_VndNokiaLandmarkcollectionXml
  | Application_VndNokiaNGageAcXml
  | Application_VndNokiaNGageData
  | Application_VndNokiaNGageSymbianInstall
  | Application_VndNokiaNcd
  | Application_VndNokiaPcdWbxml
  | Application_VndNokiaPcdXml
  | Application_VndNokiaRadioPreset
  | Application_VndNokiaRadioPresets
  | Application_VndNokiaRingingTone
  | Application_VndNovadigmEdm
  | Application_VndNovadigmEdx
  | Application_VndNovadigmExt
  | Application_VndNttLocalFileTransfer
  | Application_VndNttLocalSipTa_remote
  | Application_VndNttLocalSipTa_tcp_stream
  | Application_VndOasisOpendocumentChart
  | Application_VndOasisOpendocumentChartTemplate
  | Application_VndOasisOpendocumentDatabase
  | Application_VndOasisOpendocumentFormula
  | Application_VndOasisOpendocumentFormulaTemplate
  | Application_VndOasisOpendocumentGraphics
  | Application_VndOasisOpendocumentGraphicsTemplate
  | Application_VndOasisOpendocumentImage
  | Application_VndOasisOpendocumentImageTemplate
  | Application_VndOasisOpendocumentPresentation
  | Application_VndOasisOpendocumentPresentationTemplate
  | Application_VndOasisOpendocumentSpreadsheet
  | Application_VndOasisOpendocumentSpreadsheetTemplate
  | Application_VndOasisOpendocumentText
  | Application_VndOasisOpendocumentTextMaster
  | Application_VndOasisOpendocumentTextTemplate
  | Application_VndOasisOpendocumentTextWeb
  | Application_VndObn
  | Application_VndOftnL10nJson
  | Application_VndOipfContentaccessdownloadXml
  | Application_VndOipfContentaccessstreamingXml
  | Application_VndOipfCspgHexbinary
  | Application_VndOipfDaeSvgXml
  | Application_VndOipfDaeXhtmlXml
  | Application_VndOipfMippvcontrolmessageXml
  | Application_VndOipfPaeGem
  | Application_VndOipfSpdiscoveryXml
  | Application_VndOipfSpdlistXml
  | Application_VndOipfUeprofileXml
  | Application_VndOipfUserprofileXml
  | Application_VndOlpcSugar
  | Application_VndOmaBcastAssociatedProcedureParameterXml
  | Application_VndOmaBcastDrmTriggerXml
  | Application_VndOmaBcastImdXml
  | Application_VndOmaBcastLtkm
  | Application_VndOmaBcastNotificationXml
  | Application_VndOmaBcastProvisioningtrigger
  | Application_VndOmaBcastSgboot
  | Application_VndOmaBcastSgddXml
  | Application_VndOmaBcastSgdu
  | Application_VndOmaBcastSimpleSymbolContainer
  | Application_VndOmaBcastSmartcardTriggerXml
  | Application_VndOmaBcastSprovXml
  | Application_VndOmaBcastStkm
  | Application_VndOmaCabAddressBookXml
  | Application_VndOmaCabFeatureHandlerXml
  | Application_VndOmaCabPccXml
  | Application_VndOmaCabUserPrefsXml
  | Application_VndOmaDcd
  | Application_VndOmaDcdc
  | Application_VndOmaDd2Xml
  | Application_VndOmaDrmRisdXml
  | Application_VndOmaGroupUsageListXml
  | Application_VndOmaPalXml
  | Application_VndOmaPocDetailedProgressReportXml
  | Application_VndOmaPocFinalReportXml
  | Application_VndOmaPocGroupsXml
  | Application_VndOmaPocInvocationDescriptorXml
  | Application_VndOmaPocOptimizedProgressReportXml
  | Application_VndOmaPush
  | Application_VndOmaScidmMessagesXml
  | Application_VndOmaScwsConfig
  | Application_VndOmaScwsHttpRequest
  | Application_VndOmaScwsHttpResponse
  | Application_VndOmaXcapDirectoryXml
  | Application_VndOmadsEmailXml
  | Application_VndOmadsFileXml
  | Application_VndOmadsFolderXml
  | Application_VndOmalocSuplInit
  | Application_VndOpenofficeorgExtension
  | Application_VndOpenxmlformatsOfficedocumentCustomPropertiesXml
  | Application_VndOpenxmlformatsOfficedocumentCustomxmlpropertiesXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlChartXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlChartshapesXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramcolorsXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramdataXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramlayoutXml
  | Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramstyleXml
  | Application_VndOpenxmlformatsOfficedocumentExtendedPropertiesXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentauthorsXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentsXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlHandoutmasterXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesmasterXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesslideXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentation
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentationMainXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlPrespropsXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlide
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidelayoutXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidemasterXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshow
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshowMainXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideupdateinfoXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlTablestylesXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlTagsXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplate
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplateMainXml
  | Application_VndOpenxmlformatsOfficedocumentPresentationmlViewpropsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCalcchainXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlChartsheetXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCommentsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlConnectionsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlDialogsheetXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlExternallinkXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcachedefinitionXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcacherecordsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivottableXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlQuerytableXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionheadersXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionlogXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSharedstringsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheet
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetMainXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetmetadataXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlStylesXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTableXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTablesinglecellsXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplateMainXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlUsernamesXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlVolatiledependenciesXml
  | Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlWorksheetXml
  | Application_VndOpenxmlformatsOfficedocumentThemeXml
  | Application_VndOpenxmlformatsOfficedocumentThemeoverrideXml
  | Application_VndOpenxmlformatsOfficedocumentVmldrawing
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlCommentsXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocument
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentGlossaryXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentMainXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlEndnotesXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFonttableXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFooterXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFootnotesXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlNumberingXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlSettingsXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlStylesXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplate
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplateMainXml
  | Application_VndOpenxmlformatsOfficedocumentWordprocessingmlWebsettingsXml
  | Application_VndOpenxmlformatsPackageCorePropertiesXml
  | Application_VndOpenxmlformatsPackageDigitalSignatureXmlsignatureXml
  | Application_VndOpenxmlformatsPackageRelationshipsXml
  | Application_VndOsaNetdeploy
  | Application_VndOsgeoMapguidePackage
  | Application_VndOsgiBundle
  | Application_VndOsgiDp
  | Application_VndOsgiSubsystem
  | Application_VndOtpsCtKipXml
  | Application_VndPalm
  | Application_VndPaosXml
  | Application_VndPawaafile
  | Application_VndPgFormat
  | Application_VndPgOsasli
  | Application_VndPiaccessApplicationLicence
  | Application_VndPicsel
  | Application_VndPmiWidget
  | Application_VndPocGroupAdvertisementXml
  | Application_VndPocketlearn
  | Application_VndPowerbuilder6
  | Application_VndPowerbuilder6S
  | Application_VndPowerbuilder7
  | Application_VndPowerbuilder75
  | Application_VndPowerbuilder75S
  | Application_VndPowerbuilder7S
  | Application_VndPreminet
  | Application_VndPreviewsystemsBox
  | Application_VndProteusMagazine
  | Application_VndPublishareDeltaTree
  | Application_VndPviPtid1
  | Application_VndPwgMultiplexed
  | Application_VndPwgXhtmlPrintXml
  | Application_VndQualcommBrewAppRes
  | Application_VndQuarkQuarkxpress
  | Application_VndQuobjectQuoxdocument
  | Application_VndRadisysMomlXml
  | Application_VndRadisysMsmlAuditConfXml
  | Application_VndRadisysMsmlAuditConnXml
  | Application_VndRadisysMsmlAuditDialogXml
  | Application_VndRadisysMsmlAuditStreamXml
  | Application_VndRadisysMsmlAuditXml
  | Application_VndRadisysMsmlConfXml
  | Application_VndRadisysMsmlDialogBaseXml
  | Application_VndRadisysMsmlDialogFaxDetectXml
  | Application_VndRadisysMsmlDialogFaxSendrecvXml
  | Application_VndRadisysMsmlDialogGroupXml
  | Application_VndRadisysMsmlDialogSpeechXml
  | Application_VndRadisysMsmlDialogTransformXml
  | Application_VndRadisysMsmlDialogXml
  | Application_VndRadisysMsmlXml
  | Application_VndRainstorData
  | Application_VndRapid
  | Application_VndRealvncBed
  | Application_VndRecordareMusicxml
  | Application_VndRecordareMusicxmlXml
  | Application_VndRenlearnRlprint
  | Application_VndRigCryptonote
  | Application_VndRimCod
  | Application_VndRnRealmedia
  | Application_VndRnRealmediaVbr
  | Application_VndRnRealplayer
  | Application_VndRoute66Link66Xml
  | Application_VndRs274x
  | Application_VndRuckusDownload
  | Application_VndS3sms
  | Application_VndSailingtrackerTrack
  | Application_VndSbmCid
  | Application_VndSbmMid2
  | Application_VndScribus
  | Application_VndSealed3df
  | Application_VndSealedCsf
  | Application_VndSealedDoc
  | Application_VndSealedEml
  | Application_VndSealedMht
  | Application_VndSealedNet
  | Application_VndSealedPpt
  | Application_VndSealedTiff
  | Application_VndSealedXls
  | Application_VndSealedmediaSoftsealHtml
  | Application_VndSealedmediaSoftsealPdf
  | Application_VndSeemail
  | Application_VndSema
  | Application_VndSemd
  | Application_VndSemf
  | Application_VndShanaInformedFormdata
  | Application_VndShanaInformedFormtemplate
  | Application_VndShanaInformedInterchange
  | Application_VndShanaInformedPackage
  | Application_VndSimtechMindmapper
  | Application_VndSmaf
  | Application_VndSmartNotebook
  | Application_VndSmartTeacher
  | Application_VndSoftware602FillerFormXml
  | Application_VndSoftware602FillerFormXmlZip
  | Application_VndSolentSdkmXml
  | Application_VndSpotfireDxp
  | Application_VndSpotfireSfs
  | Application_VndSssCod
  | Application_VndSssDtf
  | Application_VndSssNtf
  | Application_VndStardivisionCalc
  | Application_VndStardivisionDraw
  | Application_VndStardivisionImpress
  | Application_VndStardivisionMath
  | Application_VndStardivisionWriter
  | Application_VndStardivisionWriterGlobal
  | Application_VndStepmaniaPackage
  | Application_VndStepmaniaStepchart
  | Application_VndStreetStream
  | Application_VndSunWadlXml
  | Application_VndSunXmlCalc
  | Application_VndSunXmlCalcTemplate
  | Application_VndSunXmlDraw
  | Application_VndSunXmlDrawTemplate
  | Application_VndSunXmlImpress
  | Application_VndSunXmlImpressTemplate
  | Application_VndSunXmlMath
  | Application_VndSunXmlWriter
  | Application_VndSunXmlWriterGlobal
  | Application_VndSunXmlWriterTemplate
  | Application_VndSusCalendar
  | Application_VndSvd
  | Application_VndSwiftviewIcs
  | Application_VndSymbianInstall
  | Application_VndSyncmlDmNotification
  | Application_VndSyncmlDmWbxml
  | Application_VndSyncmlDmXml
  | Application_VndSyncmlDsNotification
  | Application_VndSyncmlXml
  | Application_VndTaoIntentModuleArchive
  | Application_VndTcpdumpPcap
  | Application_VndTmobileLivetv
  | Application_VndTridTpt
  | Application_VndTriscapeMxs
  | Application_VndTrueapp
  | Application_VndTruedoc
  | Application_VndTveTrigger
  | Application_VndUbisoftWebplayer
  | Application_VndUfdl
  | Application_VndUiqTheme
  | Application_VndUmajin
  | Application_VndUnity
  | Application_VndUomlXml
  | Application_VndUplanetAlert
  | Application_VndUplanetAlertWbxml
  | Application_VndUplanetBearerChoice
  | Application_VndUplanetBearerChoiceWbxml
  | Application_VndUplanetCacheop
  | Application_VndUplanetCacheopWbxml
  | Application_VndUplanetChannel
  | Application_VndUplanetChannelWbxml
  | Application_VndUplanetList
  | Application_VndUplanetListWbxml
  | Application_VndUplanetListcmd
  | Application_VndUplanetListcmdWbxml
  | Application_VndUplanetSignal
  | Application_VndVcx
  | Application_VndVdStudy
  | Application_VndVectorworks
  | Application_VndVerimatrixVcas
  | Application_VndVidsoftVidconference
  | Application_VndVisio
  | Application_VndVisionary
  | Application_VndVividenceScriptfile
  | Application_VndVsf
  | Application_VndWapSic
  | Application_VndWapSlc
  | Application_VndWapWbxml
  | Application_VndWapWmlc
  | Application_VndWapWmlscriptc
  | Application_VndWebturbo
  | Application_VndWfaWsc
  | Application_VndWmc
  | Application_VndWmfBootstrap
  | Application_VndWolframMathematica
  | Application_VndWolframMathematicaPackage
  | Application_VndWolframPlayer
  | Application_VndWordperfect
  | Application_VndWqd
  | Application_VndWrqHp3000Labelled
  | Application_VndWtStf
  | Application_VndWvCspWbxml
  | Application_VndWvCspXml
  | Application_VndWvSspXml
  | Application_VndXara
  | Application_VndXfdl
  | Application_VndXfdlWebform
  | Application_VndXmiXml
  | Application_VndXmpieCpkg
  | Application_VndXmpieDpkg
  | Application_VndXmpiePlan
  | Application_VndXmpiePpkg
  | Application_VndXmpieXlim
  | Application_VndYamahaHvDic
  | Application_VndYamahaHvScript
  | Application_VndYamahaHvVoice
  | Application_VndYamahaOpenscoreformat
  | Application_VndYamahaOpenscoreformatOsfpvgXml
  | Application_VndYamahaRemoteSetup
  | Application_VndYamahaSmafAudio
  | Application_VndYamahaSmafPhrase
  | Application_VndYamahaThroughNgn
  | Application_VndYamahaTunnelUdpencap
  | Application_VndYellowriverCustomMenu
  | Application_VndZul
  | Application_VndZzazzDeckXml
  | Application_VocaltecMediaDesc
  | Application_VocaltecMediaFile
  | Application_VoicexmlXml
  | Application_VqRtcpxr
  | Application_WatcherinfoXml
  | Application_WhoisppQuery
  | Application_WhoisppResponse
  | Application_Widget
  | Application_Winhlp
  | Application_Wita
  | Application_Wordperfect
  | Application_Wordperfect51
  | Application_Wordperfect60
  | Application_Wordperfect61
  | Application_WsdlXml
  | Application_WspolicyXml
  | Application_X123
  | Application_X400Bp
  | Application_X7zCompressed
  | Application_XAbiword
  | Application_XAceCompressed
  | Application_XAim
  | Application_XAmf
  | Application_XAppleDiskimage
  | Application_XAuthorwareBin
  | Application_XAuthorwareMap
  | Application_XAuthorwareSeg
  | Application_XBcpio
  | Application_XBinary
  | Application_XBinhex40
  | Application_XBittorrent
  | Application_XBlorb
  | Application_XBsh
  | Application_XBytecodeElisp
  | Application_XBytecodeElispCompiledelisp
  | Application_XBytecodePython
  | Application_XBzip
  | Application_XBzip2
  | Application_XCbr
  | Application_XCdf
  | Application_XCdlink
  | Application_XCfsCompressed
  | Application_XChat
  | Application_XChessPgn
  | Application_XChm
  | Application_XChromeExtension
  | Application_XCmuRaster
  | Application_XCocoa
  | Application_XCompactpro
  | Application_XCompress
  | Application_XCompressed
  | Application_XConference
  | Application_XCore
  | Application_XCpio
  | Application_XCpt
  | Application_XCsh
  | Application_XDebianPackage
  | Application_XDeepv
  | Application_XDgcCompressed
  | Application_XDirector
  | Application_XDms
  | Application_XDoom
  | Application_XDtbncxXml
  | Application_XDtbookXml
  | Application_XDtbresourceXml
  | Application_XDvi
  | Application_XElc
  | Application_XEnvoy
  | Application_XEsrehber
  | Application_XEva
  | Application_XExcel
  | Application_XExecutable
  | Application_XFlac
  | Application_XFont
  | Application_XFontBdf
  | Application_XFontDos
  | Application_XFontFramemaker
  | Application_XFontGhostscript
  | Application_XFontLibgrx
  | Application_XFontLinuxPsf
  | Application_XFontOtf
  | Application_XFontPcf
  | Application_XFontSnf
  | Application_XFontSpeedo
  | Application_XFontSunosNews
  | Application_XFontTtf
  | Application_XFontType1
  | Application_XFontVfont
  | Application_XFontWoff
  | Application_XFrame
  | Application_XFreearc
  | Application_XFreelance
  | Application_XFuturesplash
  | Application_XGcaCompressed
  | Application_XGlulx
  | Application_XGnumeric
  | Application_XGoSgf
  | Application_XGrampsXml
  | Application_XGraphingCalculator
  | Application_XGsp
  | Application_XGss
  | Application_XGtar
  | Application_XGzip
  | Application_XHdf
  | Application_XHelpfile
  | Application_XHttpdImap
  | Application_XHttpdPhp
  | Application_XHttpdPhp3
  | Application_XHttpdPhp3Preprocessed
  | Application_XHttpdPhp4
  | Application_XHttpdPhpSource
  | Application_XIca
  | Application_XIma
  | Application_XInstallInstructions
  | Application_XInternetSignup
  | Application_XInternettSignup
  | Application_XInventor
  | Application_XIp2
  | Application_XIphone
  | Application_XIso9660Image
  | Application_XJavaApplet
  | Application_XJavaArchive
  | Application_XJavaBean
  | Application_XJavaClass
  | Application_XJavaCommerce
  | Application_XJavaJnlpFile
  | Application_XJavaSerializedObject
  | Application_XJavaVm
  | Application_XJavascript
  | Application_XKchart
  | Application_XKdelnk
  | Application_XKillustrator
  | Application_XKoan
  | Application_XKpresenter
  | Application_XKsh
  | Application_XKspread
  | Application_XKword
  | Application_XLatex
  | Application_XLha
  | Application_XLisp
  | Application_XLivescreen
  | Application_XLotus
  | Application_XLotusscreencam
  | Application_XLuaBytecode
  | Application_XLzh
  | Application_XLzhCompressed
  | Application_XLzx
  | Application_XMacBinhex40
  | Application_XMacbinary
  | Application_XMagicCapPackage10
  | Application_XMaker
  | Application_XMathcad
  | Application_XMeme
  | Application_XMidi
  | Application_XMie
  | Application_XMif
  | Application_XMixTransfer
  | Application_XMobipocketEbook
  | Application_XMpegurl
  | Application_XMplayer2
  | Application_XMsApplication
  | Application_XMsShortcut
  | Application_XMsWmd
  | Application_XMsWmz
  | Application_XMsXbap
  | Application_XMsaccess
  | Application_XMsbinder
  | Application_XMscardfile
  | Application_XMsclip
  | Application_XMsdosProgram
  | Application_XMsdownload
  | Application_XMsexcel
  | Application_XMsi
  | Application_XMsmediaview
  | Application_XMsmetafile
  | Application_XMsmoney
  | Application_XMspowerpoint
  | Application_XMspublisher
  | Application_XMsschedule
  | Application_XMsterminal
  | Application_XMswrite
  | Application_XNaviAnimation
  | Application_XNavidoc
  | Application_XNavimap
  | Application_XNavistyle
  | Application_XNetcdf
  | Application_XNewtonCompatiblePkg
  | Application_XNokia9000CommunicatorAddOnSoftware
  | Application_XNsProxyAutoconfig
  | Application_XNwc
  | Application_XNzb
  | Application_XObject
  | Application_XOmc
  | Application_XOmcdatamaker
  | Application_XOmcregerator
  | Application_XOzApplication
  | Application_XPagemaker
  | Application_XPcl
  | Application_XPerfmon
  | Application_XPixclscript
  | Application_XPkcs10
  | Application_XPkcs12
  | Application_XPkcs7Certificates
  | Application_XPkcs7Certreqresp
  | Application_XPkcs7Crl
  | Application_XPkcs7Mime
  | Application_XPkcs7Signature
  | Application_XPointplus
  | Application_XPortableAnymap
  | Application_XProject
  | Application_XPythonCode
  | Application_XQpro
  | Application_XQuicktimeplayer
  | Application_XRarCompressed
  | Application_XRedhatPackageManager
  | Application_XResearchInfoSystems
  | Application_XRpm
  | Application_XRtf
  | Application_XRx
  | Application_XSdp
  | Application_XSea
  | Application_XSeelogo
  | Application_XSh
  | Application_XShar
  | Application_XShellscript
  | Application_XShockwaveFlash
  | Application_XSilverlightApp
  | Application_XSit
  | Application_XSprite
  | Application_XSql
  | Application_XStuffit
  | Application_XStuffitx
  | Application_XSubrip
  | Application_XSv4cpio
  | Application_XSv4crc
  | Application_XT3vmImage
  | Application_XTads
  | Application_XTar
  | Application_XTbook
  | Application_XTcl
  | Application_XTex
  | Application_XTexGf
  | Application_XTexPk
  | Application_XTexTfm
  | Application_XTexinfo
  | Application_XTgif
  | Application_XTrash
  | Application_XTroff
  | Application_XTroffMan
  | Application_XTroffMe
  | Application_XTroffMs
  | Application_XTroffMsvideo
  | Application_XUstar
  | Application_XVideolan
  | Application_XVisio
  | Application_XVndAudioexplosionMzz
  | Application_XVndLsXpix
  | Application_XVrml
  | Application_XWaisSource
  | Application_XWebAppManifestJson
  | Application_XWingz
  | Application_XWinhelp
  | Application_XWintalk
  | Application_XWorld
  | Application_XWpwin
  | Application_XWri
  | Application_XX509CaCert
  | Application_XX509UserCert
  | Application_XXcf
  | Application_XXfig
  | Application_XXliffXml
  | Application_XXpinstall
  | Application_XXz
  | Application_XZipCompressed
  | Application_XZmachine
  | Application_XamlXml
  | Application_XcapAttXml
  | Application_XcapCapsXml
  | Application_XcapDiffXml
  | Application_XcapElXml
  | Application_XcapErrorXml
  | Application_XcapNsXml
  | Application_XconConferenceInfoDiffXml
  | Application_XconConferenceInfoXml
  | Application_XencXml
  | Application_XhtmlVoiceXml
  | Application_XhtmlXml
  | Application_Xml
  | Application_XmlDtd
  | Application_XmlExternalParsedEntity
  | Application_XmppXml
  | Application_XopXml
  | Application_XprocXml
  | Application_XsltXml
  | Application_XspfXml
  | Application_XvXml
  | Application_Yang
  | Application_YinXml
  | Application_YndMsPkipko
  | Application_Zip
  | Audio_1dInterleavedParityfec
  | Audio_32kadpcm
  | Audio_3gpp
  | Audio_3gpp2
  | Audio_Ac3
  | Audio_Adpcm
  | Audio_Aiff
  | Audio_Amr
  | Audio_AmrWb
  | Audio_Asc
  | Audio_Atrac3
  | Audio_AtracAdvancedLossless
  | Audio_AtracX
  | Audio_Basic
  | Audio_Bv16
  | Audio_Bv32
  | Audio_Clearmode
  | Audio_Cn
  | Audio_Dat12
  | Audio_Dls
  | Audio_DsrEs201108
  | Audio_DsrEs202050
  | Audio_DsrEs202211
  | Audio_DsrEs202212
  | Audio_Dv
  | Audio_Dvi4
  | Audio_Eac3
  | Audio_Evrc
  | Audio_Evrc0
  | Audio_Evrc1
  | Audio_EvrcQcp
  | Audio_Evrcb
  | Audio_Evrcb0
  | Audio_Evrcb1
  | Audio_Evrcwb
  | Audio_Evrcwb0
  | Audio_Evrcwb1
  | Audio_Example
  | Audio_Flac
  | Audio_Fwdred
  | Audio_G719
  | Audio_G722
  | Audio_G7221
  | Audio_G723
  | Audio_G72616
  | Audio_G72624
  | Audio_G72632
  | Audio_G72640
  | Audio_G728
  | Audio_G729
  | Audio_G7291
  | Audio_G729d
  | Audio_G729e
  | Audio_Gsm
  | Audio_GsmEfr
  | Audio_GsmHr08
  | Audio_Ilbc
  | Audio_IpMr_v25
  | Audio_Isac
  | Audio_It
  | Audio_L16
  | Audio_L20
  | Audio_L24
  | Audio_L8
  | Audio_Lpc
  | Audio_Make
  | Audio_MakeMyFunk
  | Audio_Mid
  | Audio_Midi
  | Audio_MobileXmf
  | Audio_Mod
  | Audio_Mp4
  | Audio_Mp4aLatm
  | Audio_Mpa
  | Audio_MpaRobust
  | Audio_Mpeg
  | Audio_Mpeg3
  | Audio_Mpeg4Generic
  | Audio_Mpegurl
  | Audio_Musepack
  | Audio_Nspaudio
  | Audio_Ogg
  | Audio_Opus
  | Audio_Parityfec
  | Audio_Pcma
  | Audio_PcmaWb
  | Audio_Pcmu
  | Audio_PcmuWb
  | Audio_PrsSid
  | Audio_Qcelp
  | Audio_Red
  | Audio_RtpEncAescm128
  | Audio_RtpMidi
  | Audio_Rtx
  | Audio_S3m
  | Audio_Silk
  | Audio_Smv
  | Audio_Smv0
  | Audio_SmvQcp
  | Audio_SpMidi
  | Audio_Speex
  | Audio_T140c
  | Audio_T38
  | Audio_TelephoneEvent
  | Audio_Tone
  | Audio_TspAudio
  | Audio_Tsplayer
  | Audio_Uemclip
  | Audio_Ulpfec
  | Audio_Vdvi
  | Audio_VmrWb
  | Audio_Vnd3gppIufp
  | Audio_Vnd4sb
  | Audio_VndAudiokoz
  | Audio_VndCelp
  | Audio_VndCiscoNse
  | Audio_VndCmlesRadioEvents
  | Audio_VndCnsAnp1
  | Audio_VndCnsInf1
  | Audio_VndDeceAudio
  | Audio_VndDigitalWinds
  | Audio_VndDlnaAdts
  | Audio_VndDolbyHeaac1
  | Audio_VndDolbyHeaac2
  | Audio_VndDolbyMlp
  | Audio_VndDolbyMps
  | Audio_VndDolbyPl2
  | Audio_VndDolbyPl2x
  | Audio_VndDolbyPl2z
  | Audio_VndDolbyPulse1
  | Audio_VndDra
  | Audio_VndDts
  | Audio_VndDtsHd
  | Audio_VndDvbFile
  | Audio_VndEveradPlj
  | Audio_VndHnsAudio
  | Audio_VndLucentVoice
  | Audio_VndMsPlayreadyMediaPya
  | Audio_VndNokiaMobileXmf
  | Audio_VndNortelVbk
  | Audio_VndNueraEcelp4800
  | Audio_VndNueraEcelp7470
  | Audio_VndNueraEcelp9600
  | Audio_VndOctelSbc
  | Audio_VndQcelp
  | Audio_VndRhetorex32kadpcm
  | Audio_VndRip
  | Audio_VndSealedmediaSoftsealMpeg
  | Audio_VndVmxCvsd
  | Audio_Voc
  | Audio_Vorbis
  | Audio_VorbisConfig
  | Audio_Voxware
  | Audio_Wav
  | Audio_Webm
  | Audio_XAac
  | Audio_XAdpcm
  | Audio_XAiff
  | Audio_XAu
  | Audio_XCaf
  | Audio_XFlac
  | Audio_XGsm
  | Audio_XJam
  | Audio_XLiveaudio
  | Audio_XMatroska
  | Audio_XMid
  | Audio_XMidi
  | Audio_XMod
  | Audio_XMpeg
  | Audio_XMpeg3
  | Audio_XMpegurl
  | Audio_XMpequrl
  | Audio_XMsWax
  | Audio_XMsWma
  | Audio_XNspaudio
  | Audio_XPnRealaudio
  | Audio_XPnRealaudioPlugin
  | Audio_XPsid
  | Audio_XRealaudio
  | Audio_XScpls
  | Audio_XSd2
  | Audio_XTta
  | Audio_XTwinvq
  | Audio_XTwinvqPlugin
  | Audio_XVndAudioexplosionMjuicemediafile
  | Audio_XVoc
  | Audio_XWav
  | Audio_Xm
  | Chemical_XCdx
  | Chemical_XCif
  | Chemical_XCmdf
  | Chemical_XCml
  | Chemical_XCsml
  | Chemical_XPdb
  | Chemical_XXyz
  | Content_Unknown
  | Drawing_XDwf
  | Drawing_XDwfOld
  | Font_Opentype
  | IWorld_IVrml
  | Image_Bmp
  | Image_Cgm
  | Image_CisCod
  | Image_CmuRaster
  | Image_Example
  | Image_Fif
  | Image_Fits
  | Image_Florian
  | Image_G3fax
  | Image_Gif
  | Image_Ief
  | Image_Jp2
  | Image_Jpeg
  | Image_Jpm
  | Image_Jpx
  | Image_Jutvision
  | Image_Ktx
  | Image_Naplps
  | Image_Pcx
  | Image_Pict
  | Image_Pipeg
  | Image_Pjpeg
  | Image_Png
  | Image_PrsBtif
  | Image_PrsPti
  | Image_Sgi
  | Image_SvgXml
  | Image_T38
  | Image_Tiff
  | Image_TiffFx
  | Image_Vasa
  | Image_VndAdobePhotoshop
  | Image_VndCnsInf2
  | Image_VndDeceGraphic
  | Image_VndDjvu
  | Image_VndDvbSubtitle
  | Image_VndDwg
  | Image_VndDxf
  | Image_VndFastbidsheet
  | Image_VndFpx
  | Image_VndFst
  | Image_VndFujixeroxEdmicsMmr
  | Image_VndFujixeroxEdmicsRlc
  | Image_VndGlobalgraphicsPgb
  | Image_VndMicrosoftIcon
  | Image_VndMix
  | Image_VndMsModi
  | Image_VndMsPhoto
  | Image_VndNetFpx
  | Image_VndRadiance
  | Image_VndRnRealflash
  | Image_VndRnRealpix
  | Image_VndSealedPng
  | Image_VndSealedmediaSoftsealGif
  | Image_VndSealedmediaSoftsealJpg
  | Image_VndSvf
  | Image_VndWapWbmp
  | Image_VndXiff
  | Image_Webp
  | Image_X3ds
  | Image_XCmuRast
  | Image_XCmuRaster
  | Image_XCmx
  | Image_XCoreldraw
  | Image_XCoreldrawpattern
  | Image_XCoreldrawtemplate
  | Image_XCorelphotopaint
  | Image_XDwg
  | Image_XFreehand
  | Image_XIcon
  | Image_XJg
  | Image_XJng
  | Image_XJps
  | Image_XMrsidImage
  | Image_XMsBmp
  | Image_XNiff
  | Image_XPcx
  | Image_XPhotoshop
  | Image_XPict
  | Image_XPortableAnymap
  | Image_XPortableBitmap
  | Image_XPortableGraymap
  | Image_XPortableGreymap
  | Image_XPortablePixmap
  | Image_XQuicktime
  | Image_XRgb
  | Image_XTga
  | Image_XTiff
  | Image_XWindowsBmp
  | Image_XXbitmap
  | Image_XXbm
  | Image_XXpixmap
  | Image_XXwd
  | Image_XXwindowdump
  | Image_Xbm
  | Image_Xpm
  | Inode_Blockdevice
  | Inode_Chardevice
  | Inode_Directory
  | Inode_DirectoryLocked
  | Inode_Fifo
  | Inode_Socket
  | Message_Cpim
  | Message_DeliveryStatus
  | Message_DispositionNotification
  | Message_Example
  | Message_ExternalBody
  | Message_FeedbackReport
  | Message_Global
  | Message_GlobalDeliveryStatus
  | Message_GlobalDispositionNotification
  | Message_GlobalHeaders
  | Message_Http
  | Message_ImdnXml
  | Message_News
  | Message_Partial
  | Message_Rfc822
  | Message_SHttp
  | Message_Sip
  | Message_Sipfrag
  | Message_TrackingStatus
  | Message_VndSiSimp
  | Model_Example
  | Model_Iges
  | Model_Mesh
  | Model_VndColladaXml
  | Model_VndDwf
  | Model_VndFlatland3dml
  | Model_VndGdl
  | Model_VndGsGdl
  | Model_VndGtw
  | Model_VndMomlXml
  | Model_VndMts
  | Model_VndParasolidTransmitBinary
  | Model_VndParasolidTransmitText
  | Model_VndVtu
  | Model_Vrml
  | Model_X3dBinary
  | Model_X3dVrml
  | Model_X3dXml
  | Model_XPov
  | Multipart_Alternative
  | Multipart_Appledouble
  | Multipart_Byteranges
  | Multipart_Digest
  | Multipart_Encrypted
  | Multipart_Example
  | Multipart_FormData
  | Multipart_HeaderSet
  | Multipart_Mixed
  | Multipart_Parallel
  | Multipart_Related
  | Multipart_Report
  | Multipart_Signed
  | Multipart_VoiceMessage
  | Multipart_XGzip
  | Multipart_XUstar
  | Multipart_XZip
  | Music_Crescendo
  | Music_XKaraoke
  | Paleovu_XPv
  | Text_1dInterleavedParityfec
  | Text_Asp
  | Text_CacheManifest
  | Text_Calendar
  | Text_CommaSeparatedValues
  | Text_Css
  | Text_Csv
  | Text_Directory
  | Text_Dns
  | Text_Ecmascript
  | Text_English
  | Text_Enriched
  | Text_EventStream
  | Text_Example
  | Text_Fwdred
  | Text_H323
  | Text_Html
  | Text_Iuls
  | Text_Javascript
  | Text_Mathml
  | Text_Mcf
  | Text_N3
  | Text_Parityfec
  | Text_Pascal
  | Text_Plain
  | Text_PlainBas
  | Text_PrsFallensteinRst
  | Text_PrsLinesTag
  | Text_Red
  | Text_Rfc822Headers
  | Text_Richtext
  | Text_Rtf
  | Text_RtpEncAescm128
  | Text_Rtx
  | Text_Scriplet
  | Text_Scriptlet
  | Text_Sgml
  | Text_T140
  | Text_TabSeparatedValues
  | Text_Texmacs
  | Text_Troff
  | Text_Turtle
  | Text_Ulpfec
  | Text_UriList
  | Text_Vcard
  | Text_VndAbc
  | Text_VndCurl
  | Text_VndCurlDcurl
  | Text_VndCurlMcurl
  | Text_VndCurlScurl
  | Text_VndDmclientscript
  | Text_VndDvbSubtitle
  | Text_VndEsmertecThemeDescriptor
  | Text_VndFlatland3dml
  | Text_VndFly
  | Text_VndFmiFlexstor
  | Text_VndGraphviz
  | Text_VndIn3d3dml
  | Text_VndIn3dSpot
  | Text_VndIptcNewsml
  | Text_VndIptcNitf
  | Text_VndLatexZ
  | Text_VndMotorolaReflex
  | Text_VndMsMediapackage
  | Text_VndNet2phoneCommcenterCommand
  | Text_VndRadisysMsmlBasicLayout
  | Text_VndRnRealtext
  | Text_VndSiUricatalogue
  | Text_VndSunJ2meAppDescriptor
  | Text_VndTrolltechLinguist
  | Text_VndWapSi
  | Text_VndWapSl
  | Text_VndWapWml
  | Text_VndWapWmlscript
  | Text_Vtt
  | Text_Webviewhtml
  | Text_XAsm
  | Text_XAudiosoftIntra
  | Text_XC
  | Text_XCHdr
  | Text_XCSrc
  | Text_XChdr
  | Text_XComponent
  | Text_XCrontab
  | Text_XCsh
  | Text_XCsrc
  | Text_XFortran
  | Text_XH
  | Text_XJava
  | Text_XJavaSource
  | Text_XLaAsf
  | Text_XLua
  | Text_XM
  | Text_XMakefile
  | Text_XMarkdown
  | Text_XMoc
  | Text_XNfo
  | Text_XOpml
  | Text_XPascal
  | Text_XPcsGcd
  | Text_XPerl
  | Text_XPython
  | Text_XScript
  | Text_XScriptCsh
  | Text_XScriptElisp
  | Text_XScriptGuile
  | Text_XScriptKsh
  | Text_XScriptLisp
  | Text_XScriptPerl
  | Text_XScriptPerlModule
  | Text_XScriptPhyton
  | Text_XScriptRexx
  | Text_XScriptScheme
  | Text_XScriptSh
  | Text_XScriptTcl
  | Text_XScriptTcsh
  | Text_XScriptZsh
  | Text_XServerParsedHtml
  | Text_XSetext
  | Text_XSfv
  | Text_XSgml
  | Text_XSh
  | Text_XSpeech
  | Text_XTcl
  | Text_XTex
  | Text_XUil
  | Text_XUuencode
  | Text_XVcalendar
  | Text_XVcard
  | Text_Xml
  | Text_XmlExternalParsedEntity
  | Unknown_Unknown
  | Video_1dInterleavedParityfec
  | Video_3gpp
  | Video_3gpp2
  | Video_3gppTt
  | Video_Animaflex
  | Video_Avi
  | Video_AvsVideo
  | Video_Bmpeg
  | Video_Bt656
  | Video_Celb
  | Video_Dl
  | Video_Dv
  | Video_Example
  | Video_Flc
  | Video_Fli
  | Video_Gl
  | Video_H261
  | Video_H263
  | Video_H2631998
  | Video_H2632000
  | Video_H264
  | Video_H264Rcdo
  | Video_H264Svc
  | Video_Jpeg
  | Video_Jpeg2000
  | Video_Jpm
  | Video_Mj2
  | Video_Mp1s
  | Video_Mp2p
  | Video_Mp2t
  | Video_Mp4
  | Video_Mp4vEs
  | Video_Mpeg
  | Video_Mpeg4Generic
  | Video_Mpv
  | Video_Msvideo
  | Video_Nv
  | Video_Ogg
  | Video_Parityfec
  | Video_Pointer
  | Video_Quicktime
  | Video_Raw
  | Video_RtpEncAescm128
  | Video_Rtx
  | Video_Smpte292m
  | Video_Ulpfec
  | Video_Vc1
  | Video_Vdo
  | Video_Vivo
  | Video_VndCctv
  | Video_VndDeceHd
  | Video_VndDeceMobile
  | Video_VndDeceMp4
  | Video_VndDecePd
  | Video_VndDeceSd
  | Video_VndDeceVideo
  | Video_VndDirectvMpeg
  | Video_VndDirectvMpegTts
  | Video_VndDlnaMpegTts
  | Video_VndDvbFile
  | Video_VndFvt
  | Video_VndHnsVideo
  | Video_VndIptvforum1dparityfec1010
  | Video_VndIptvforum1dparityfec2005
  | Video_VndIptvforum2dparityfec1010
  | Video_VndIptvforum2dparityfec2005
  | Video_VndIptvforumTtsavc
  | Video_VndIptvforumTtsmpeg2
  | Video_VndMotorolaVideo
  | Video_VndMotorolaVideop
  | Video_VndMpegurl
  | Video_VndMsPlayreadyMediaPyv
  | Video_VndMts
  | Video_VndNokiaInterleavedMultimedia
  | Video_VndNokiaVideovoip
  | Video_VndObjectvideo
  | Video_VndRnRealvideo
  | Video_VndSealedMpeg1
  | Video_VndSealedMpeg4
  | Video_VndSealedSwf
  | Video_VndSealedmediaSoftsealMov
  | Video_VndUvvuMp4
  | Video_VndVivo
  | Video_Vosaic
  | Video_Webm
  | Video_XAmtDemorun
  | Video_XAmtShowrun
  | Video_XAtomic3dFeature
  | Video_XDl
  | Video_XDv
  | Video_XF4v
  | Video_XFli
  | Video_XFlv
  | Video_XGl
  | Video_XIsvideo
  | Video_XLaAsf
  | Video_XM4v
  | Video_XMatroska
  | Video_XMng
  | Video_XMotionJpeg
  | Video_XMpeg
  | Video_XMpeq2a
  | Video_XMsAsf
  | Video_XMsAsfPlugin
  | Video_XMsVob
  | Video_XMsWm
  | Video_XMsWmv
  | Video_XMsWmx
  | Video_XMsWvx
  | Video_XMsvideo
  | Video_XQtc
  | Video_XScm
  | Video_XSgiMovie
  | Video_XSmv
  | Windows_Metafile
  | Www_Mime
  | XConference_XCooltalk
  | XMusic_XMidi
  | XWorld_X3dmf
  | XWorld_XSvr
  | XWorld_XVrml
  | XWorld_XVrt
  | Xgl_Drawing
  | Xgl_Movie
  deriving (Bounded, Enum, Eq)

mimeTypeToBytes :: MIME_Type -> LBS.ByteString
mimeTypeToBytes mimeType =
  case mimeType of
    Application_1dInterleavedParityfec -> "application/1d-interleaved-parityfec"
    Application_3gppImsXml -> "application/3gpp-ims+xml"
    Application_Acad -> "application/acad"
    Application_Activemessage -> "application/activemessage"
    Application_AndrewInset -> "application/andrew-inset"
    Application_Applefile -> "application/applefile"
    Application_Applixware -> "application/applixware"
    Application_Arj -> "application/arj"
    Application_AtomXml -> "application/atom+xml"
    Application_AtomcatXml -> "application/atomcat+xml"
    Application_Atomicmail -> "application/atomicmail"
    Application_AtomsvcXml -> "application/atomsvc+xml"
    Application_AuthPolicyXml -> "application/auth-policy+xml"
    Application_Base64 -> "application/base64"
    Application_BatchSmtp -> "application/batch-SMTP"
    Application_BeepXml -> "application/beep+xml"
    Application_Binhex -> "application/binhex"
    Application_Binhex4 -> "application/binhex4"
    Application_Book -> "application/book"
    Application_CalendarXml -> "application/calendar+xml"
    Application_Cals1840 -> "application/cals-1840"
    Application_CcmpXml -> "application/ccmp+xml"
    Application_CcxmlXml -> "application/ccxml+xml"
    Application_Cdf -> "application/cdf"
    Application_CdmiCapability -> "application/cdmi-capability"
    Application_CdmiContainer -> "application/cdmi-container"
    Application_CdmiDomain -> "application/cdmi-domain"
    Application_CdmiObject -> "application/cdmi-object"
    Application_CdmiQueue -> "application/cdmi-queue"
    Application_Cea2018Xml -> "application/cea-2018+xml"
    Application_CellmlXml -> "application/cellml+xml"
    Application_Cfw -> "application/cfw"
    Application_Clariscad -> "application/clariscad"
    Application_CnrpXml -> "application/cnrp+xml"
    Application_Commonground -> "application/commonground"
    Application_ConferenceInfoXml -> "application/conference-info+xml"
    Application_CplXml -> "application/cpl+xml"
    Application_CstaXml -> "application/csta+xml"
    Application_CstadataXml -> "application/cstadata+xml"
    Application_CuSeeme -> "application/cu-seeme"
    Application_Cybercash -> "application/cybercash"
    Application_DavmountXml -> "application/davmount+xml"
    Application_DcaRft -> "application/dca-rft"
    Application_DecDx -> "application/dec-dx"
    Application_DialogInfoXml -> "application/dialog-info+xml"
    Application_Dicom -> "application/dicom"
    Application_Dns -> "application/dns"
    Application_DocbookXml -> "application/docbook+xml"
    Application_Drafting -> "application/drafting"
    Application_DskppXml -> "application/dskpp+xml"
    Application_Dsptype -> "application/dsptype"
    Application_DsscDer -> "application/dssc+der"
    Application_DsscXml -> "application/dssc+xml"
    Application_Dvcs -> "application/dvcs"
    Application_Dxf -> "application/dxf"
    Application_Ecmascript -> "application/ecmascript"
    Application_EdiConsent -> "application/edi-consent"
    Application_EdiX12 -> "application/edi-x12"
    Application_Edifact -> "application/edifact"
    Application_EmmaXml -> "application/emma+xml"
    Application_Envoy -> "application/envoy"
    Application_EppXml -> "application/epp+xml"
    Application_EpubZip -> "application/epub+zip"
    Application_Eshop -> "application/eshop"
    Application_Example -> "application/example"
    Application_Excel -> "application/excel"
    Application_Exi -> "application/exi"
    Application_Fastinfoset -> "application/fastinfoset"
    Application_Fastsoap -> "application/fastsoap"
    Application_Fits -> "application/fits"
    Application_FontTdpfr -> "application/font-tdpfr"
    Application_FontWoff -> "application/font-woff"
    Application_Fractals -> "application/fractals"
    Application_FrameworkAttributesXml -> "application/framework-attributes+xml"
    Application_Freeloader -> "application/freeloader"
    Application_Futuresplash -> "application/futuresplash"
    Application_Ghostview -> "application/ghostview"
    Application_GmlXml -> "application/gml+xml"
    Application_Gnutar -> "application/gnutar"
    Application_GpxXml -> "application/gpx+xml"
    Application_Groupwise -> "application/groupwise"
    Application_Gxf -> "application/gxf"
    Application_H224 -> "application/h224"
    Application_HeldXml -> "application/held+xml"
    Application_Hlp -> "application/hlp"
    Application_Hta -> "application/hta"
    Application_Http -> "application/http"
    Application_Hyperstudio -> "application/hyperstudio"
    Application_IDeas -> "application/i-deas"
    Application_IbeKeyRequestXml -> "application/ibe-key-request+xml"
    Application_IbePkgReplyXml -> "application/ibe-pkg-reply+xml"
    Application_IbePpData -> "application/ibe-pp-data"
    Application_Iges -> "application/iges"
    Application_ImIscomposingXml -> "application/im-iscomposing+xml"
    Application_Index -> "application/index"
    Application_IndexCmd -> "application/index.cmd"
    Application_IndexObj -> "application/index.obj"
    Application_IndexResponse -> "application/index.response"
    Application_IndexVnd -> "application/index.vnd"
    Application_Inf -> "application/inf"
    Application_InkmlXml -> "application/inkml+xml"
    Application_InternetPropertyStream -> "application/internet-property-stream"
    Application_Iotp -> "application/iotp"
    Application_Ipfix -> "application/ipfix"
    Application_Ipp -> "application/ipp"
    Application_Isup -> "application/isup"
    Application_Java -> "application/java"
    Application_JavaArchive -> "application/java-archive"
    Application_JavaByteCode -> "application/java-byte-code"
    Application_JavaSerializedObject -> "application/java-serialized-object"
    Application_JavaVm -> "application/java-vm"
    Application_Javascript -> "application/javascript"
    Application_Json -> "application/json"
    Application_JsonmlJson -> "application/jsonml+json"
    Application_KpmlRequestXml -> "application/kpml-request+xml"
    Application_KpmlResponseXml -> "application/kpml-response+xml"
    Application_Lha -> "application/lha"
    Application_LostXml -> "application/lost+xml"
    Application_Lzx -> "application/lzx"
    Application_MacBinary -> "application/mac-binary"
    Application_MacBinhex -> "application/mac-binhex"
    Application_MacBinhex40 -> "application/mac-binhex40"
    Application_MacCompactpro -> "application/mac-compactpro"
    Application_Macbinary -> "application/macbinary"
    Application_Macwriteii -> "application/macwriteii"
    Application_MadsXml -> "application/mads+xml"
    Application_Marc -> "application/marc"
    Application_MarcxmlXml -> "application/marcxml+xml"
    Application_Mathematica -> "application/mathematica"
    Application_MathematicaOld -> "application/mathematica-old"
    Application_MathmlContentXml -> "application/mathml-content+xml"
    Application_MathmlPresentationXml -> "application/mathml-presentation+xml"
    Application_MathmlXml -> "application/mathml+xml"
    Application_Mbedlet -> "application/mbedlet"
    Application_MbmsAssociatedProcedureDescriptionXml -> "application/mbms-associated-procedure-description+xml"
    Application_MbmsDeregisterXml -> "application/mbms-deregister+xml"
    Application_MbmsEnvelopeXml -> "application/mbms-envelope+xml"
    Application_MbmsMskResponseXml -> "application/mbms-msk-response+xml"
    Application_MbmsMskXml -> "application/mbms-msk+xml"
    Application_MbmsProtectionDescriptionXml -> "application/mbms-protection-description+xml"
    Application_MbmsReceptionReportXml -> "application/mbms-reception-report+xml"
    Application_MbmsRegisterResponseXml -> "application/mbms-register-response+xml"
    Application_MbmsRegisterXml -> "application/mbms-register+xml"
    Application_MbmsUserServiceDescriptionXml -> "application/mbms-user-service-description+xml"
    Application_Mbox -> "application/mbox"
    Application_Mcad -> "application/mcad"
    Application_Media_controlXml -> "application/media_control+xml"
    Application_MediaservercontrolXml -> "application/mediaservercontrol+xml"
    Application_Metalink4Xml -> "application/metalink4+xml"
    Application_MetalinkXml -> "application/metalink+xml"
    Application_MetsXml -> "application/mets+xml"
    Application_Mikey -> "application/mikey"
    Application_Mime -> "application/mime"
    Application_ModsXml -> "application/mods+xml"
    Application_MossKeys -> "application/moss-keys"
    Application_MossSignature -> "application/moss-signature"
    Application_MosskeyData -> "application/mosskey-data"
    Application_MosskeyRequest -> "application/mosskey-request"
    Application_Mp21 -> "application/mp21"
    Application_Mp4 -> "application/mp4"
    Application_Mpeg4Generic -> "application/mpeg4-generic"
    Application_Mpeg4Iod -> "application/mpeg4-iod"
    Application_Mpeg4IodXmt -> "application/mpeg4-iod-xmt"
    Application_Msaccess -> "application/msaccess"
    Application_MscIvrXml -> "application/msc-ivr+xml"
    Application_MscMixerXml -> "application/msc-mixer+xml"
    Application_Msonenote -> "application/msonenote"
    Application_Mspowerpoint -> "application/mspowerpoint"
    Application_Msword -> "application/msword"
    Application_Mswrite -> "application/mswrite"
    Application_Mxf -> "application/mxf"
    Application_Nasdata -> "application/nasdata"
    Application_Netmc -> "application/netmc"
    Application_NewsCheckgroups -> "application/news-checkgroups"
    Application_NewsGroupinfo -> "application/news-groupinfo"
    Application_NewsMessageId -> "application/news-message-id"
    Application_NewsTransmission -> "application/news-transmission"
    Application_Nss -> "application/nss"
    Application_OcspRequest -> "application/ocsp-request"
    Application_OcspResponse -> "application/ocsp-response"
    Application_OctetStream -> "application/octet-stream"
    Application_Oda -> "application/oda"
    Application_OebpsPackageXml -> "application/oebps-package+xml"
    Application_Ogg -> "application/ogg"
    Application_Olescript -> "application/olescript"
    Application_OmdocXml -> "application/omdoc+xml"
    Application_Onenote -> "application/onenote"
    Application_Oxps -> "application/oxps"
    Application_Parityfec -> "application/parityfec"
    Application_PatchOpsErrorXml -> "application/patch-ops-error+xml"
    Application_Pdf -> "application/pdf"
    Application_PgpEncrypted -> "application/pgp-encrypted"
    Application_PgpKeys -> "application/pgp-keys"
    Application_PgpSignature -> "application/pgp-signature"
    Application_PicsRules -> "application/pics-rules"
    Application_PidfDiffXml -> "application/pidf-diff+xml"
    Application_PidfXml -> "application/pidf+xml"
    Application_Pkcs10 -> "application/pkcs10"
    Application_Pkcs12 -> "application/pkcs-12"
    Application_Pkcs7Mime -> "application/pkcs7-mime"
    Application_Pkcs7Signature -> "application/pkcs7-signature"
    Application_Pkcs8 -> "application/pkcs8"
    Application_PkcsCrl -> "application/pkcs-crl"
    Application_PkixAttrCert -> "application/pkix-attr-cert"
    Application_PkixCert -> "application/pkix-cert"
    Application_PkixCrl -> "application/pkix-crl"
    Application_PkixPkipath -> "application/pkix-pkipath"
    Application_Pkixcmp -> "application/pkixcmp"
    Application_Plain -> "application/plain"
    Application_PlsXml -> "application/pls+xml"
    Application_PocSettingsXml -> "application/poc-settings+xml"
    Application_Postscript -> "application/postscript"
    Application_Powerpoint -> "application/powerpoint"
    Application_Pro_eng -> "application/pro_eng"
    Application_PrsAlvestrandTitraxSheet -> "application/prs.alvestrand.titrax-sheet"
    Application_PrsCww -> "application/prs.cww"
    Application_PrsNprend -> "application/prs.nprend"
    Application_PrsPlucker -> "application/prs.plucker"
    Application_PrsRdfXmlCrypt -> "application/prs.rdf-xml-crypt"
    Application_PrsXsfXml -> "application/prs.xsf+xml"
    Application_PskcXml -> "application/pskc+xml"
    Application_Qsig -> "application/qsig"
    Application_Rar -> "application/rar"
    Application_RdfXml -> "application/rdf+xml"
    Application_ReginfoXml -> "application/reginfo+xml"
    Application_RelaxNgCompactSyntax -> "application/relax-ng-compact-syntax"
    Application_RemotePrinting -> "application/remote-printing"
    Application_ResourceListsDiffXml -> "application/resource-lists-diff+xml"
    Application_ResourceListsXml -> "application/resource-lists+xml"
    Application_RingingTones -> "application/ringing-tones"
    Application_Riscos -> "application/riscos"
    Application_RlmiXml -> "application/rlmi+xml"
    Application_RlsServicesXml -> "application/rls-services+xml"
    Application_RpkiGhostbusters -> "application/rpki-ghostbusters"
    Application_RpkiManifest -> "application/rpki-manifest"
    Application_RpkiRoa -> "application/rpki-roa"
    Application_RpkiUpdown -> "application/rpki-updown"
    Application_RsdXml -> "application/rsd+xml"
    Application_RssXml -> "application/rss+xml"
    Application_Rtf -> "application/rtf"
    Application_Rtx -> "application/rtx"
    Application_SamlassertionXml -> "application/samlassertion+xml"
    Application_SamlmetadataXml -> "application/samlmetadata+xml"
    Application_SbmlXml -> "application/sbml+xml"
    Application_ScvpCvRequest -> "application/scvp-cv-request"
    Application_ScvpCvResponse -> "application/scvp-cv-response"
    Application_ScvpVpRequest -> "application/scvp-vp-request"
    Application_ScvpVpResponse -> "application/scvp-vp-response"
    Application_Sdp -> "application/sdp"
    Application_Sea -> "application/sea"
    Application_Set -> "application/set"
    Application_SetPayment -> "application/set-payment"
    Application_SetPaymentInitiation -> "application/set-payment-initiation"
    Application_SetRegistration -> "application/set-registration"
    Application_SetRegistrationInitiation -> "application/set-registration-initiation"
    Application_Sgml -> "application/sgml"
    Application_SgmlOpenCatalog -> "application/sgml-open-catalog"
    Application_ShfXml -> "application/shf+xml"
    Application_Sieve -> "application/sieve"
    Application_SimpleFilterXml -> "application/simple-filter+xml"
    Application_SimpleMessageSummary -> "application/simple-message-summary"
    Application_Simplesymbolcontainer -> "application/simplesymbolcontainer"
    Application_Sla -> "application/sla"
    Application_Slate -> "application/slate"
    Application_Smil -> "application/smil"
    Application_SmilXml -> "application/smil+xml"
    Application_SoapFastinfoset -> "application/soap+fastinfoset"
    Application_SoapXml -> "application/soap+xml"
    Application_Solids -> "application/solids"
    Application_Sounder -> "application/sounder"
    Application_SparqlQuery -> "application/sparql-query"
    Application_SparqlResultsXml -> "application/sparql-results+xml"
    Application_SpiritsEventXml -> "application/spirits-event+xml"
    Application_Srgs -> "application/srgs"
    Application_SrgsXml -> "application/srgs+xml"
    Application_SruXml -> "application/sru+xml"
    Application_SsdlXml -> "application/ssdl+xml"
    Application_SsmlXml -> "application/ssml+xml"
    Application_Step -> "application/step"
    Application_Streamingmedia -> "application/streamingmedia"
    Application_TampApexUpdate -> "application/tamp-apex-update"
    Application_TampApexUpdateConfirm -> "application/tamp-apex-update-confirm"
    Application_TampCommunityUpdate -> "application/tamp-community-update"
    Application_TampCommunityUpdateConfirm -> "application/tamp-community-update-confirm"
    Application_TampError -> "application/tamp-error"
    Application_TampSequenceAdjust -> "application/tamp-sequence-adjust"
    Application_TampSequenceAdjustConfirm -> "application/tamp-sequence-adjust-confirm"
    Application_TampStatusQuery -> "application/tamp-status-query"
    Application_TampStatusResponse -> "application/tamp-status-response"
    Application_TampUpdate -> "application/tamp-update"
    Application_TampUpdateConfirm -> "application/tamp-update-confirm"
    Application_TeiXml -> "application/tei+xml"
    Application_ThraudXml -> "application/thraud+xml"
    Application_TimestampQuery -> "application/timestamp-query"
    Application_TimestampReply -> "application/timestamp-reply"
    Application_TimestampedData -> "application/timestamped-data"
    Application_Toolbook -> "application/toolbook"
    Application_TveTrigger -> "application/tve-trigger"
    Application_Ulpfec -> "application/ulpfec"
    Application_VcardXml -> "application/vcard+xml"
    Application_Vda -> "application/vda"
    Application_Vemmi -> "application/vemmi"
    Application_VividenceScriptfile -> "application/vividence.scriptfile"
    Application_Vnd3gpp2BcmcsinfoXml -> "application/vnd.3gpp2.bcmcsinfo+xml"
    Application_Vnd3gpp2Sms -> "application/vnd.3gpp2.sms"
    Application_Vnd3gpp2Tcap -> "application/vnd.3gpp2.tcap"
    Application_Vnd3gppBsfXml -> "application/vnd.3gpp.bsf+xml"
    Application_Vnd3gppPicBwLarge -> "application/vnd.3gpp.pic-bw-large"
    Application_Vnd3gppPicBwSmall -> "application/vnd.3gpp.pic-bw-small"
    Application_Vnd3gppPicBwVar -> "application/vnd.3gpp.pic-bw-var"
    Application_Vnd3gppSms -> "application/vnd.3gpp.sms"
    Application_Vnd3mPostItNotes -> "application/vnd.3M.Post-it-Notes"
    Application_VndAccpacSimplyAso -> "application/vnd.accpac.simply.aso"
    Application_VndAccpacSimplyImp -> "application/vnd.accpac.simply.imp"
    Application_VndAcucobol -> "application/vnd.acucobol"
    Application_VndAcucorp -> "application/vnd.acucorp"
    Application_VndAdobeAirApplicationInstallerPackageZip -> "application/vnd.adobe.air-application-installer-package+zip"
    Application_VndAdobeFormscentralFcdt -> "application/vnd.adobe.formscentral.fcdt"
    Application_VndAdobeFxp -> "application/vnd.adobe.fxp"
    Application_VndAdobePartialUpload -> "application/vnd.adobe.partial-upload"
    Application_VndAdobeXdpXml -> "application/vnd.adobe.xdp+xml"
    Application_VndAdobeXfdf -> "application/vnd.adobe.xfdf"
    Application_VndAetherImp -> "application/vnd.aether.imp"
    Application_VndAhBarcode -> "application/vnd.ah-barcode"
    Application_VndAheadSpace -> "application/vnd.ahead.space"
    Application_VndAirzipFilesecureAzf -> "application/vnd.airzip.filesecure.azf"
    Application_VndAirzipFilesecureAzs -> "application/vnd.airzip.filesecure.azs"
    Application_VndAmazonEbook -> "application/vnd.amazon.ebook"
    Application_VndAmericandynamicsAcc -> "application/vnd.americandynamics.acc"
    Application_VndAmigaAmi -> "application/vnd.amiga.ami"
    Application_VndAmundsenMazeXml -> "application/vnd.amundsen.maze+xml"
    Application_VndAndroidPackageArchive -> "application/vnd.android.package-archive"
    Application_VndAnserWebCertificateIssueInitiation -> "application/vnd.anser-web-certificate-issue-initiation"
    Application_VndAnserWebFundsTransferInitiation -> "application/vnd.anser-web-funds-transfer-initiation"
    Application_VndAntixGameComponent -> "application/vnd.antix.game-component"
    Application_VndAppleInstallerXml -> "application/vnd.apple.installer+xml"
    Application_VndAppleMpegurl -> "application/vnd.apple.mpegurl"
    Application_VndArastraSwi -> "application/vnd.arastra.swi"
    Application_VndAristanetworksSwi -> "application/vnd.aristanetworks.swi"
    Application_VndAstraeaSoftwareIota -> "application/vnd.astraea-software.iota"
    Application_VndAudiograph -> "application/vnd.audiograph"
    Application_VndAutopackage -> "application/vnd.autopackage"
    Application_VndAvistarXml -> "application/vnd.avistar+xml"
    Application_VndBlueiceMultipass -> "application/vnd.blueice.multipass"
    Application_VndBluetoothEpOob -> "application/vnd.bluetooth.ep.oob"
    Application_VndBmi -> "application/vnd.bmi"
    Application_VndBusinessobjects -> "application/vnd.businessobjects"
    Application_VndCabJscript -> "application/vnd.cab-jscript"
    Application_VndCanonCpdl -> "application/vnd.canon-cpdl"
    Application_VndCanonLips -> "application/vnd.canon-lips"
    Application_VndCendioThinlincClientconf -> "application/vnd.cendio.thinlinc.clientconf"
    Application_VndChemdrawXml -> "application/vnd.chemdraw+xml"
    Application_VndChipnutsKaraokeMmd -> "application/vnd.chipnuts.karaoke-mmd"
    Application_VndCinderella -> "application/vnd.cinderella"
    Application_VndCirpackIsdnExt -> "application/vnd.cirpack.isdn-ext"
    Application_VndClaymore -> "application/vnd.claymore"
    Application_VndCloantoRp9 -> "application/vnd.cloanto.rp9"
    Application_VndClonkC4group -> "application/vnd.clonk.c4group"
    Application_VndCluetrustCartomobileConfig -> "application/vnd.cluetrust.cartomobile-config"
    Application_VndCluetrustCartomobileConfigPkg -> "application/vnd.cluetrust.cartomobile-config-pkg"
    Application_VndCollectionJson -> "application/vnd.collection+json"
    Application_VndCommerceBattelle -> "application/vnd.commerce-battelle"
    Application_VndCommonspace -> "application/vnd.commonspace"
    Application_VndComsocaller -> "application/vnd.comsocaller"
    Application_VndContactCmsg -> "application/vnd.contact.cmsg"
    Application_VndCosmocaller -> "application/vnd.cosmocaller"
    Application_VndCrickClicker -> "application/vnd.crick.clicker"
    Application_VndCrickClickerKeyboard -> "application/vnd.crick.clicker.keyboard"
    Application_VndCrickClickerPalette -> "application/vnd.crick.clicker.palette"
    Application_VndCrickClickerTemplate -> "application/vnd.crick.clicker.template"
    Application_VndCrickClickerWordbank -> "application/vnd.crick.clicker.wordbank"
    Application_VndCriticaltoolsWbsXml -> "application/vnd.criticaltools.wbs+xml"
    Application_VndCtcPosml -> "application/vnd.ctc-posml"
    Application_VndCtctWsXml -> "application/vnd.ctct.ws+xml"
    Application_VndCupsPdf -> "application/vnd.cups-pdf"
    Application_VndCupsPostscript -> "application/vnd.cups-postscript"
    Application_VndCupsPpd -> "application/vnd.cups-ppd"
    Application_VndCupsRaster -> "application/vnd.cups-raster"
    Application_VndCupsRaw -> "application/vnd.cups-raw"
    Application_VndCurl -> "application/vnd.curl"
    Application_VndCurlCar -> "application/vnd.curl.car"
    Application_VndCurlPcurl -> "application/vnd.curl.pcurl"
    Application_VndCybank -> "application/vnd.cybank"
    Application_VndDart -> "application/vnd.dart"
    Application_VndDataVisionRdz -> "application/vnd.data-vision.rdz"
    Application_VndDeceData -> "application/vnd.dece.data"
    Application_VndDeceTtmlXml -> "application/vnd.dece.ttml+xml"
    Application_VndDeceUnspecified -> "application/vnd.dece.unspecified"
    Application_VndDeceZip -> "application/vnd.dece.zip"
    Application_VndDenovoFcselayoutLink -> "application/vnd.denovo.fcselayout-link"
    Application_VndDirBiPlateDlNosuffix -> "application/vnd.dir-bi.plate-dl-nosuffix"
    Application_VndDna -> "application/vnd.dna"
    Application_VndDolbyMlp -> "application/vnd.dolby.mlp"
    Application_VndDolbyMobile1 -> "application/vnd.dolby.mobile.1"
    Application_VndDolbyMobile2 -> "application/vnd.dolby.mobile.2"
    Application_VndDpgraph -> "application/vnd.dpgraph"
    Application_VndDreamfactory -> "application/vnd.dreamfactory"
    Application_VndDsKeypoint -> "application/vnd.ds-keypoint"
    Application_VndDvbAit -> "application/vnd.dvb.ait"
    Application_VndDvbDvbj -> "application/vnd.dvb.dvbj"
    Application_VndDvbEsgcontainer -> "application/vnd.dvb.esgcontainer"
    Application_VndDvbIpdcdftnotifaccess -> "application/vnd.dvb.ipdcdftnotifaccess"
    Application_VndDvbIpdcesgaccess -> "application/vnd.dvb.ipdcesgaccess"
    Application_VndDvbIpdcesgaccess2 -> "application/vnd.dvb.ipdcesgaccess2"
    Application_VndDvbIpdcesgpdd -> "application/vnd.dvb.ipdcesgpdd"
    Application_VndDvbIpdcroaming -> "application/vnd.dvb.ipdcroaming"
    Application_VndDvbIptvAlfecBase -> "application/vnd.dvb.iptv.alfec-base"
    Application_VndDvbIptvAlfecEnhancement -> "application/vnd.dvb.iptv.alfec-enhancement"
    Application_VndDvbNotifAggregateRootXml -> "application/vnd.dvb.notif-aggregate-root+xml"
    Application_VndDvbNotifContainerXml -> "application/vnd.dvb.notif-container+xml"
    Application_VndDvbNotifGenericXml -> "application/vnd.dvb.notif-generic+xml"
    Application_VndDvbNotifIaMsglistXml -> "application/vnd.dvb.notif-ia-msglist+xml"
    Application_VndDvbNotifIaRegistrationRequestXml -> "application/vnd.dvb.notif-ia-registration-request+xml"
    Application_VndDvbNotifIaRegistrationResponseXml -> "application/vnd.dvb.notif-ia-registration-response+xml"
    Application_VndDvbNotifInitXml -> "application/vnd.dvb.notif-init+xml"
    Application_VndDvbPfr -> "application/vnd.dvb.pfr"
    Application_VndDvbService -> "application/vnd.dvb.service"
    Application_VndDxr -> "application/vnd.dxr"
    Application_VndDynageo -> "application/vnd.dynageo"
    Application_VndEasykaraokeCdgdownload -> "application/vnd.easykaraoke.cdgdownload"
    Application_VndEcdisUpdate -> "application/vnd.ecdis-update"
    Application_VndEcowinChart -> "application/vnd.ecowin.chart"
    Application_VndEcowinFilerequest -> "application/vnd.ecowin.filerequest"
    Application_VndEcowinFileupdate -> "application/vnd.ecowin.fileupdate"
    Application_VndEcowinSeries -> "application/vnd.ecowin.series"
    Application_VndEcowinSeriesrequest -> "application/vnd.ecowin.seriesrequest"
    Application_VndEcowinSeriesupdate -> "application/vnd.ecowin.seriesupdate"
    Application_VndEmclientAccessrequestXml -> "application/vnd.emclient.accessrequest+xml"
    Application_VndEnliven -> "application/vnd.enliven"
    Application_VndEprintsDataXml -> "application/vnd.eprints.data+xml"
    Application_VndEpsonEsf -> "application/vnd.epson.esf"
    Application_VndEpsonMsf -> "application/vnd.epson.msf"
    Application_VndEpsonQuickanime -> "application/vnd.epson.quickanime"
    Application_VndEpsonSalt -> "application/vnd.epson.salt"
    Application_VndEpsonSsf -> "application/vnd.epson.ssf"
    Application_VndEricssonQuickcall -> "application/vnd.ericsson.quickcall"
    Application_VndEszigno3Xml -> "application/vnd.eszigno3+xml"
    Application_VndEtsiAocXml -> "application/vnd.etsi.aoc+xml"
    Application_VndEtsiCugXml -> "application/vnd.etsi.cug+xml"
    Application_VndEtsiIptvcommandXml -> "application/vnd.etsi.iptvcommand+xml"
    Application_VndEtsiIptvdiscoveryXml -> "application/vnd.etsi.iptvdiscovery+xml"
    Application_VndEtsiIptvprofileXml -> "application/vnd.etsi.iptvprofile+xml"
    Application_VndEtsiIptvsadBcXml -> "application/vnd.etsi.iptvsad-bc+xml"
    Application_VndEtsiIptvsadCodXml -> "application/vnd.etsi.iptvsad-cod+xml"
    Application_VndEtsiIptvsadNpvrXml -> "application/vnd.etsi.iptvsad-npvr+xml"
    Application_VndEtsiIptvserviceXml -> "application/vnd.etsi.iptvservice+xml"
    Application_VndEtsiIptvsyncXml -> "application/vnd.etsi.iptvsync+xml"
    Application_VndEtsiIptvueprofileXml -> "application/vnd.etsi.iptvueprofile+xml"
    Application_VndEtsiMcidXml -> "application/vnd.etsi.mcid+xml"
    Application_VndEtsiOverloadControlPolicyDatasetXml -> "application/vnd.etsi.overload-control-policy-dataset+xml"
    Application_VndEtsiSciXml -> "application/vnd.etsi.sci+xml"
    Application_VndEtsiSimservsXml -> "application/vnd.etsi.simservs+xml"
    Application_VndEtsiTslDer -> "application/vnd.etsi.tsl.der"
    Application_VndEtsiTslXml -> "application/vnd.etsi.tsl+xml"
    Application_VndEudoraData -> "application/vnd.eudora.data"
    Application_VndEzpixAlbum -> "application/vnd.ezpix-album"
    Application_VndEzpixPackage -> "application/vnd.ezpix-package"
    Application_VndFSecureMobile -> "application/vnd.f-secure.mobile"
    Application_VndFdf -> "application/vnd.fdf"
    Application_VndFdsnMseed -> "application/vnd.fdsn.mseed"
    Application_VndFdsnSeed -> "application/vnd.fdsn.seed"
    Application_VndFfsns -> "application/vnd.ffsns"
    Application_VndFints -> "application/vnd.fints"
    Application_VndFlographit -> "application/vnd.flographit"
    Application_VndFluxtimeClip -> "application/vnd.fluxtime.clip"
    Application_VndFontFontforgeSfd -> "application/vnd.font-fontforge-sfd"
    Application_VndFramemaker -> "application/vnd.framemaker"
    Application_VndFrogansFnc -> "application/vnd.frogans.fnc"
    Application_VndFrogansLtf -> "application/vnd.frogans.ltf"
    Application_VndFscWeblaunch -> "application/vnd.fsc.weblaunch"
    Application_VndFujitsuOasys -> "application/vnd.fujitsu.oasys"
    Application_VndFujitsuOasys2 -> "application/vnd.fujitsu.oasys2"
    Application_VndFujitsuOasys3 -> "application/vnd.fujitsu.oasys3"
    Application_VndFujitsuOasysgp -> "application/vnd.fujitsu.oasysgp"
    Application_VndFujitsuOasysprs -> "application/vnd.fujitsu.oasysprs"
    Application_VndFujixeroxArt4 -> "application/vnd.fujixerox.art4"
    Application_VndFujixeroxArtEx -> "application/vnd.fujixerox.art-ex"
    Application_VndFujixeroxDdd -> "application/vnd.fujixerox.ddd"
    Application_VndFujixeroxDocuworks -> "application/vnd.fujixerox.docuworks"
    Application_VndFujixeroxDocuworksBinder -> "application/vnd.fujixerox.docuworks.binder"
    Application_VndFujixeroxHbpl -> "application/vnd.fujixerox.hbpl"
    Application_VndFutMisnet -> "application/vnd.fut-misnet"
    Application_VndFuzzysheet -> "application/vnd.fuzzysheet"
    Application_VndGenomatixTuxedo -> "application/vnd.genomatix.tuxedo"
    Application_VndGeocubeXml -> "application/vnd.geocube+xml"
    Application_VndGeogebraFile -> "application/vnd.geogebra.file"
    Application_VndGeogebraTool -> "application/vnd.geogebra.tool"
    Application_VndGeometryExplorer -> "application/vnd.geometry-explorer"
    Application_VndGeonext -> "application/vnd.geonext"
    Application_VndGeoplan -> "application/vnd.geoplan"
    Application_VndGeospace -> "application/vnd.geospace"
    Application_VndGlobalplatformCardContentMgt -> "application/vnd.globalplatform.card-content-mgt"
    Application_VndGlobalplatformCardContentMgtResponse -> "application/vnd.globalplatform.card-content-mgt-response"
    Application_VndGmx -> "application/vnd.gmx"
    Application_VndGoogleEarthKmlXml -> "application/vnd.google-earth.kml+xml"
    Application_VndGoogleEarthKmz -> "application/vnd.google-earth.kmz"
    Application_VndGrafeq -> "application/vnd.grafeq"
    Application_VndGridmp -> "application/vnd.gridmp"
    Application_VndGrooveAccount -> "application/vnd.groove-account"
    Application_VndGrooveHelp -> "application/vnd.groove-help"
    Application_VndGrooveIdentityMessage -> "application/vnd.groove-identity-message"
    Application_VndGrooveInjector -> "application/vnd.groove-injector"
    Application_VndGrooveToolMessage -> "application/vnd.groove-tool-message"
    Application_VndGrooveToolTemplate -> "application/vnd.groove-tool-template"
    Application_VndGrooveVcard -> "application/vnd.groove-vcard"
    Application_VndHalJson -> "application/vnd.hal+json"
    Application_VndHalXml -> "application/vnd.hal+xml"
    Application_VndHandheldEntertainmentXml -> "application/vnd.handheld-entertainment+xml"
    Application_VndHbci -> "application/vnd.hbci"
    Application_VndHclBireports -> "application/vnd.hcl-bireports"
    Application_VndHheLessonPlayer -> "application/vnd.hhe.lesson-player"
    Application_VndHpHpgl -> "application/vnd.hp-HPGL"
    Application_VndHpHpid -> "application/vnd.hp-hpid"
    Application_VndHpHps -> "application/vnd.hp-hps"
    Application_VndHpJlyt -> "application/vnd.hp-jlyt"
    Application_VndHpPcl -> "application/vnd.hp-PCL"
    Application_VndHpPclxl -> "application/vnd.hp-PCLXL"
    Application_VndHttphone -> "application/vnd.httphone"
    Application_VndHydrostatixSofData -> "application/vnd.hydrostatix.sof-data"
    Application_VndHzn3dCrossword -> "application/vnd.hzn-3d-crossword"
    Application_VndIbmAfplinedata -> "application/vnd.ibm.afplinedata"
    Application_VndIbmElectronicMedia -> "application/vnd.ibm.electronic-media"
    Application_VndIbmMinipay -> "application/vnd.ibm.MiniPay"
    Application_VndIbmModcap -> "application/vnd.ibm.modcap"
    Application_VndIbmRightsManagement -> "application/vnd.ibm.rights-management"
    Application_VndIbmSecureContainer -> "application/vnd.ibm.secure-container"
    Application_VndIccprofile -> "application/vnd.iccprofile"
    Application_VndIgloader -> "application/vnd.igloader"
    Application_VndImmervisionIvp -> "application/vnd.immervision-ivp"
    Application_VndImmervisionIvu -> "application/vnd.immervision-ivu"
    Application_VndInformedcontrolRmsXml -> "application/vnd.informedcontrol.rms+xml"
    Application_VndInformixVisionary -> "application/vnd.informix-visionary"
    Application_VndInfotechProject -> "application/vnd.infotech.project"
    Application_VndInfotechProjectXml -> "application/vnd.infotech.project+xml"
    Application_VndInnopathWampNotification -> "application/vnd.innopath.wamp.notification"
    Application_VndInsorsIgm -> "application/vnd.insors.igm"
    Application_VndInterconFormnet -> "application/vnd.intercon.formnet"
    Application_VndIntergeo -> "application/vnd.intergeo"
    Application_VndIntertrustDigibox -> "application/vnd.intertrust.digibox"
    Application_VndIntertrustNncp -> "application/vnd.intertrust.nncp"
    Application_VndIntuQbo -> "application/vnd.intu.qbo"
    Application_VndIntuQfx -> "application/vnd.intu.qfx"
    Application_VndIptcG2ConceptitemXml -> "application/vnd.iptc.g2.conceptitem+xml"
    Application_VndIptcG2KnowledgeitemXml -> "application/vnd.iptc.g2.knowledgeitem+xml"
    Application_VndIptcG2NewsitemXml -> "application/vnd.iptc.g2.newsitem+xml"
    Application_VndIptcG2NewsmessageXml -> "application/vnd.iptc.g2.newsmessage+xml"
    Application_VndIptcG2PackageitemXml -> "application/vnd.iptc.g2.packageitem+xml"
    Application_VndIptcG2PlanningitemXml -> "application/vnd.iptc.g2.planningitem+xml"
    Application_VndIpunpluggedRcprofile -> "application/vnd.ipunplugged.rcprofile"
    Application_VndIrepositoryPackageXml -> "application/vnd.irepository.package+xml"
    Application_VndIsXpr -> "application/vnd.is-xpr"
    Application_VndIsacFcs -> "application/vnd.isac.fcs"
    Application_VndJam -> "application/vnd.jam"
    Application_VndJapannetDirectoryService -> "application/vnd.japannet-directory-service"
    Application_VndJapannetJpnstoreWakeup -> "application/vnd.japannet-jpnstore-wakeup"
    Application_VndJapannetPaymentWakeup -> "application/vnd.japannet-payment-wakeup"
    Application_VndJapannetRegistration -> "application/vnd.japannet-registration"
    Application_VndJapannetRegistrationWakeup -> "application/vnd.japannet-registration-wakeup"
    Application_VndJapannetSetstoreWakeup -> "application/vnd.japannet-setstore-wakeup"
    Application_VndJapannetVerification -> "application/vnd.japannet-verification"
    Application_VndJapannetVerificationWakeup -> "application/vnd.japannet-verification-wakeup"
    Application_VndJcpJavameMidletRms -> "application/vnd.jcp.javame.midlet-rms"
    Application_VndJisp -> "application/vnd.jisp"
    Application_VndJoostJodaArchive -> "application/vnd.joost.joda-archive"
    Application_VndKahootz -> "application/vnd.kahootz"
    Application_VndKdeKarbon -> "application/vnd.kde.karbon"
    Application_VndKdeKchart -> "application/vnd.kde.kchart"
    Application_VndKdeKformula -> "application/vnd.kde.kformula"
    Application_VndKdeKivio -> "application/vnd.kde.kivio"
    Application_VndKdeKontour -> "application/vnd.kde.kontour"
    Application_VndKdeKpresenter -> "application/vnd.kde.kpresenter"
    Application_VndKdeKspread -> "application/vnd.kde.kspread"
    Application_VndKdeKword -> "application/vnd.kde.kword"
    Application_VndKenameaapp -> "application/vnd.kenameaapp"
    Application_VndKidspiration -> "application/vnd.kidspiration"
    Application_VndKinar -> "application/vnd.kinar"
    Application_VndKoan -> "application/vnd.koan"
    Application_VndKodakDescriptor -> "application/vnd.kodak-descriptor"
    Application_VndLasLasXml -> "application/vnd.las.las+xml"
    Application_VndLibertyRequestXml -> "application/vnd.liberty-request+xml"
    Application_VndLlamagraphicsLifeBalanceDesktop -> "application/vnd.llamagraphics.life-balance.desktop"
    Application_VndLlamagraphicsLifeBalanceExchangeXml -> "application/vnd.llamagraphics.life-balance.exchange+xml"
    Application_VndLotus123 -> "application/vnd.lotus-1-2-3"
    Application_VndLotusApproach -> "application/vnd.lotus-approach"
    Application_VndLotusFreelance -> "application/vnd.lotus-freelance"
    Application_VndLotusNotes -> "application/vnd.lotus-notes"
    Application_VndLotusOrganizer -> "application/vnd.lotus-organizer"
    Application_VndLotusScreencam -> "application/vnd.lotus-screencam"
    Application_VndLotusWordpro -> "application/vnd.lotus-wordpro"
    Application_VndMacportsPortpkg -> "application/vnd.macports.portpkg"
    Application_VndMarlinDrmActiontokenXml -> "application/vnd.marlin.drm.actiontoken+xml"
    Application_VndMarlinDrmConftokenXml -> "application/vnd.marlin.drm.conftoken+xml"
    Application_VndMarlinDrmLicenseXml -> "application/vnd.marlin.drm.license+xml"
    Application_VndMarlinDrmMdcf -> "application/vnd.marlin.drm.mdcf"
    Application_VndMcd -> "application/vnd.mcd"
    Application_VndMedcalcdata -> "application/vnd.medcalcdata"
    Application_VndMediastationCdkey -> "application/vnd.mediastation.cdkey"
    Application_VndMeridianSlingshot -> "application/vnd.meridian-slingshot"
    Application_VndMfer -> "application/vnd.mfer"
    Application_VndMfmp -> "application/vnd.mfmp"
    Application_VndMicrografxFlo -> "application/vnd.micrografx.flo"
    Application_VndMicrografxIgx -> "application/vnd.micrografx.igx"
    Application_VndMif -> "application/vnd.mif"
    Application_VndMinisoftHp3000Save -> "application/vnd.minisoft-hp3000-save"
    Application_VndMitsubishiMistyGuardTrustweb -> "application/vnd.mitsubishi.misty-guard.trustweb"
    Application_VndMobiusDaf -> "application/vnd.mobius.daf"
    Application_VndMobiusDis -> "application/vnd.mobius.dis"
    Application_VndMobiusMbk -> "application/vnd.mobius.mbk"
    Application_VndMobiusMqy -> "application/vnd.mobius.mqy"
    Application_VndMobiusMsl -> "application/vnd.mobius.msl"
    Application_VndMobiusPlc -> "application/vnd.mobius.plc"
    Application_VndMobiusTxf -> "application/vnd.mobius.txf"
    Application_VndMophunApplication -> "application/vnd.mophun.application"
    Application_VndMophunCertificate -> "application/vnd.mophun.certificate"
    Application_VndMotorolaFlexsuite -> "application/vnd.motorola.flexsuite"
    Application_VndMotorolaFlexsuiteAdsi -> "application/vnd.motorola.flexsuite.adsi"
    Application_VndMotorolaFlexsuiteFis -> "application/vnd.motorola.flexsuite.fis"
    Application_VndMotorolaFlexsuiteGotap -> "application/vnd.motorola.flexsuite.gotap"
    Application_VndMotorolaFlexsuiteKmr -> "application/vnd.motorola.flexsuite.kmr"
    Application_VndMotorolaFlexsuiteTtc -> "application/vnd.motorola.flexsuite.ttc"
    Application_VndMotorolaFlexsuiteWem -> "application/vnd.motorola.flexsuite.wem"
    Application_VndMotorolaIprm -> "application/vnd.motorola.iprm"
    Application_VndMozillaXulXml -> "application/vnd.mozilla.xul+xml"
    Application_VndMsArtgalry -> "application/vnd.ms-artgalry"
    Application_VndMsAsf -> "application/vnd.ms-asf"
    Application_VndMsCabCompressed -> "application/vnd.ms-cab-compressed"
    Application_VndMsColorIccprofile -> "application/vnd.ms-color.iccprofile"
    Application_VndMsExcel -> "application/vnd.ms-excel"
    Application_VndMsExcelAddinMacroenabled12 -> "application/vnd.ms-excel.addin.macroEnabled.12"
    Application_VndMsExcelSheetBinaryMacroenabled12 -> "application/vnd.ms-excel.sheet.binary.macroEnabled.12"
    Application_VndMsExcelSheetMacroenabled12 -> "application/vnd.ms-excel.sheet.macroEnabled.12"
    Application_VndMsExcelTemplateMacroenabled12 -> "application/vnd.ms-excel.template.macroEnabled.12"
    Application_VndMsFontobject -> "application/vnd.ms-fontobject"
    Application_VndMsHtmlhelp -> "application/vnd.ms-htmlhelp"
    Application_VndMsIms -> "application/vnd.ms-ims"
    Application_VndMsLrm -> "application/vnd.ms-lrm"
    Application_VndMsOfficeActivexXml -> "application/vnd.ms-office.activex+xml"
    Application_VndMsOfficetheme -> "application/vnd.ms-officetheme"
    Application_VndMsOpentype -> "application/vnd.ms-opentype"
    Application_VndMsOutlook -> "application/vnd.ms-outlook"
    Application_VndMsPackageObfuscatedOpentype -> "application/vnd.ms-package.obfuscated-opentype"
    Application_VndMsPkiCertstore -> "application/vnd.ms-pki.certstore"
    Application_VndMsPkiPko -> "application/vnd.ms-pki.pko"
    Application_VndMsPkiSeccat -> "application/vnd.ms-pki.seccat"
    Application_VndMsPkiStl -> "application/vnd.ms-pki.stl"
    Application_VndMsPkicertstore -> "application/vnd.ms-pkicertstore"
    Application_VndMsPkiseccat -> "application/vnd.ms-pkiseccat"
    Application_VndMsPkistl -> "application/vnd.ms-pkistl"
    Application_VndMsPlayreadyInitiatorXml -> "application/vnd.ms-playready.initiator+xml"
    Application_VndMsPowerpoint -> "application/vnd.ms-powerpoint"
    Application_VndMsPowerpointAddinMacroenabled12 -> "application/vnd.ms-powerpoint.addin.macroEnabled.12"
    Application_VndMsPowerpointPresentationMacroenabled12 -> "application/vnd.ms-powerpoint.presentation.macroEnabled.12"
    Application_VndMsPowerpointSlideMacroenabled12 -> "application/vnd.ms-powerpoint.slide.macroEnabled.12"
    Application_VndMsPowerpointSlideshowMacroenabled12 -> "application/vnd.ms-powerpoint.slideshow.macroEnabled.12"
    Application_VndMsPowerpointTemplateMacroenabled12 -> "application/vnd.ms-powerpoint.template.macroEnabled.12"
    Application_VndMsPrintingPrintticketXml -> "application/vnd.ms-printing.printticket+xml"
    Application_VndMsProject -> "application/vnd.ms-project"
    Application_VndMsTnef -> "application/vnd.ms-tnef"
    Application_VndMsWmdrmLicChlgReq -> "application/vnd.ms-wmdrm.lic-chlg-req"
    Application_VndMsWmdrmLicResp -> "application/vnd.ms-wmdrm.lic-resp"
    Application_VndMsWmdrmMeterChlgReq -> "application/vnd.ms-wmdrm.meter-chlg-req"
    Application_VndMsWmdrmMeterResp -> "application/vnd.ms-wmdrm.meter-resp"
    Application_VndMsWordDocumentMacroenabled12 -> "application/vnd.ms-word.document.macroEnabled.12"
    Application_VndMsWordTemplateMacroenabled12 -> "application/vnd.ms-word.template.macroEnabled.12"
    Application_VndMsWorks -> "application/vnd.ms-works"
    Application_VndMsWpl -> "application/vnd.ms-wpl"
    Application_VndMsXpsdocument -> "application/vnd.ms-xpsdocument"
    Application_VndMseq -> "application/vnd.mseq"
    Application_VndMsign -> "application/vnd.msign"
    Application_VndMultiadCreator -> "application/vnd.multiad.creator"
    Application_VndMultiadCreatorCif -> "application/vnd.multiad.creator.cif"
    Application_VndMusicNiff -> "application/vnd.music-niff"
    Application_VndMusician -> "application/vnd.musician"
    Application_VndMuveeStyle -> "application/vnd.muvee.style"
    Application_VndMynfc -> "application/vnd.mynfc"
    Application_VndNcdControl -> "application/vnd.ncd.control"
    Application_VndNcdReference -> "application/vnd.ncd.reference"
    Application_VndNervana -> "application/vnd.nervana"
    Application_VndNetfpx -> "application/vnd.netfpx"
    Application_VndNeurolanguageNlu -> "application/vnd.neurolanguage.nlu"
    Application_VndNitf -> "application/vnd.nitf"
    Application_VndNoblenetDirectory -> "application/vnd.noblenet-directory"
    Application_VndNoblenetSealer -> "application/vnd.noblenet-sealer"
    Application_VndNoblenetWeb -> "application/vnd.noblenet-web"
    Application_VndNokiaCatalogs -> "application/vnd.nokia.catalogs"
    Application_VndNokiaConfigurationMessage -> "application/vnd.nokia.configuration-message"
    Application_VndNokiaConmlWbxml -> "application/vnd.nokia.conml+wbxml"
    Application_VndNokiaConmlXml -> "application/vnd.nokia.conml+xml"
    Application_VndNokiaIptvConfigXml -> "application/vnd.nokia.iptv.config+xml"
    Application_VndNokiaIsdsRadioPresets -> "application/vnd.nokia.isds-radio-presets"
    Application_VndNokiaLandmarkWbxml -> "application/vnd.nokia.landmark+wbxml"
    Application_VndNokiaLandmarkXml -> "application/vnd.nokia.landmark+xml"
    Application_VndNokiaLandmarkcollectionXml -> "application/vnd.nokia.landmarkcollection+xml"
    Application_VndNokiaNGageAcXml -> "application/vnd.nokia.n-gage.ac+xml"
    Application_VndNokiaNGageData -> "application/vnd.nokia.n-gage.data"
    Application_VndNokiaNGageSymbianInstall -> "application/vnd.nokia.n-gage.symbian.install"
    Application_VndNokiaNcd -> "application/vnd.nokia.ncd"
    Application_VndNokiaPcdWbxml -> "application/vnd.nokia.pcd+wbxml"
    Application_VndNokiaPcdXml -> "application/vnd.nokia.pcd+xml"
    Application_VndNokiaRadioPreset -> "application/vnd.nokia.radio-preset"
    Application_VndNokiaRadioPresets -> "application/vnd.nokia.radio-presets"
    Application_VndNokiaRingingTone -> "application/vnd.nokia.ringing-tone"
    Application_VndNovadigmEdm -> "application/vnd.novadigm.EDM"
    Application_VndNovadigmEdx -> "application/vnd.novadigm.EDX"
    Application_VndNovadigmExt -> "application/vnd.novadigm.EXT"
    Application_VndNttLocalFileTransfer -> "application/vnd.ntt-local.file-transfer"
    Application_VndNttLocalSipTa_remote -> "application/vnd.ntt-local.sip-ta_remote"
    Application_VndNttLocalSipTa_tcp_stream -> "application/vnd.ntt-local.sip-ta_tcp_stream"
    Application_VndOasisOpendocumentChart -> "application/vnd.oasis.opendocument.chart"
    Application_VndOasisOpendocumentChartTemplate -> "application/vnd.oasis.opendocument.chart-template"
    Application_VndOasisOpendocumentDatabase -> "application/vnd.oasis.opendocument.database"
    Application_VndOasisOpendocumentFormula -> "application/vnd.oasis.opendocument.formula"
    Application_VndOasisOpendocumentFormulaTemplate -> "application/vnd.oasis.opendocument.formula-template"
    Application_VndOasisOpendocumentGraphics -> "application/vnd.oasis.opendocument.graphics"
    Application_VndOasisOpendocumentGraphicsTemplate -> "application/vnd.oasis.opendocument.graphics-template"
    Application_VndOasisOpendocumentImage -> "application/vnd.oasis.opendocument.image"
    Application_VndOasisOpendocumentImageTemplate -> "application/vnd.oasis.opendocument.image-template"
    Application_VndOasisOpendocumentPresentation -> "application/vnd.oasis.opendocument.presentation"
    Application_VndOasisOpendocumentPresentationTemplate -> "application/vnd.oasis.opendocument.presentation-template"
    Application_VndOasisOpendocumentSpreadsheet -> "application/vnd.oasis.opendocument.spreadsheet"
    Application_VndOasisOpendocumentSpreadsheetTemplate -> "application/vnd.oasis.opendocument.spreadsheet-template"
    Application_VndOasisOpendocumentText -> "application/vnd.oasis.opendocument.text"
    Application_VndOasisOpendocumentTextMaster -> "application/vnd.oasis.opendocument.text-master"
    Application_VndOasisOpendocumentTextTemplate -> "application/vnd.oasis.opendocument.text-template"
    Application_VndOasisOpendocumentTextWeb -> "application/vnd.oasis.opendocument.text-web"
    Application_VndObn -> "application/vnd.obn"
    Application_VndOftnL10nJson -> "application/vnd.oftn.l10n+json"
    Application_VndOipfContentaccessdownloadXml -> "application/vnd.oipf.contentaccessdownload+xml"
    Application_VndOipfContentaccessstreamingXml -> "application/vnd.oipf.contentaccessstreaming+xml"
    Application_VndOipfCspgHexbinary -> "application/vnd.oipf.cspg-hexbinary"
    Application_VndOipfDaeSvgXml -> "application/vnd.oipf.dae.svg+xml"
    Application_VndOipfDaeXhtmlXml -> "application/vnd.oipf.dae.xhtml+xml"
    Application_VndOipfMippvcontrolmessageXml -> "application/vnd.oipf.mippvcontrolmessage+xml"
    Application_VndOipfPaeGem -> "application/vnd.oipf.pae.gem"
    Application_VndOipfSpdiscoveryXml -> "application/vnd.oipf.spdiscovery+xml"
    Application_VndOipfSpdlistXml -> "application/vnd.oipf.spdlist+xml"
    Application_VndOipfUeprofileXml -> "application/vnd.oipf.ueprofile+xml"
    Application_VndOipfUserprofileXml -> "application/vnd.oipf.userprofile+xml"
    Application_VndOlpcSugar -> "application/vnd.olpc-sugar"
    Application_VndOmaBcastAssociatedProcedureParameterXml -> "application/vnd.oma.bcast.associated-procedure-parameter+xml"
    Application_VndOmaBcastDrmTriggerXml -> "application/vnd.oma.bcast.drm-trigger+xml"
    Application_VndOmaBcastImdXml -> "application/vnd.oma.bcast.imd+xml"
    Application_VndOmaBcastLtkm -> "application/vnd.oma.bcast.ltkm"
    Application_VndOmaBcastNotificationXml -> "application/vnd.oma.bcast.notification+xml"
    Application_VndOmaBcastProvisioningtrigger -> "application/vnd.oma.bcast.provisioningtrigger"
    Application_VndOmaBcastSgboot -> "application/vnd.oma.bcast.sgboot"
    Application_VndOmaBcastSgddXml -> "application/vnd.oma.bcast.sgdd+xml"
    Application_VndOmaBcastSgdu -> "application/vnd.oma.bcast.sgdu"
    Application_VndOmaBcastSimpleSymbolContainer -> "application/vnd.oma.bcast.simple-symbol-container"
    Application_VndOmaBcastSmartcardTriggerXml -> "application/vnd.oma.bcast.smartcard-trigger+xml"
    Application_VndOmaBcastSprovXml -> "application/vnd.oma.bcast.sprov+xml"
    Application_VndOmaBcastStkm -> "application/vnd.oma.bcast.stkm"
    Application_VndOmaCabAddressBookXml -> "application/vnd.oma.cab-address-book+xml"
    Application_VndOmaCabFeatureHandlerXml -> "application/vnd.oma.cab-feature-handler+xml"
    Application_VndOmaCabPccXml -> "application/vnd.oma.cab-pcc+xml"
    Application_VndOmaCabUserPrefsXml -> "application/vnd.oma.cab-user-prefs+xml"
    Application_VndOmaDcd -> "application/vnd.oma.dcd"
    Application_VndOmaDcdc -> "application/vnd.oma.dcdc"
    Application_VndOmaDd2Xml -> "application/vnd.oma.dd2+xml"
    Application_VndOmaDrmRisdXml -> "application/vnd.oma.drm.risd+xml"
    Application_VndOmaGroupUsageListXml -> "application/vnd.oma.group-usage-list+xml"
    Application_VndOmaPalXml -> "application/vnd.oma.pal+xml"
    Application_VndOmaPocDetailedProgressReportXml -> "application/vnd.oma.poc.detailed-progress-report+xml"
    Application_VndOmaPocFinalReportXml -> "application/vnd.oma.poc.final-report+xml"
    Application_VndOmaPocGroupsXml -> "application/vnd.oma.poc.groups+xml"
    Application_VndOmaPocInvocationDescriptorXml -> "application/vnd.oma.poc.invocation-descriptor+xml"
    Application_VndOmaPocOptimizedProgressReportXml -> "application/vnd.oma.poc.optimized-progress-report+xml"
    Application_VndOmaPush -> "application/vnd.oma.push"
    Application_VndOmaScidmMessagesXml -> "application/vnd.oma.scidm.messages+xml"
    Application_VndOmaScwsConfig -> "application/vnd.oma-scws-config"
    Application_VndOmaScwsHttpRequest -> "application/vnd.oma-scws-http-request"
    Application_VndOmaScwsHttpResponse -> "application/vnd.oma-scws-http-response"
    Application_VndOmaXcapDirectoryXml -> "application/vnd.oma.xcap-directory+xml"
    Application_VndOmadsEmailXml -> "application/vnd.omads-email+xml"
    Application_VndOmadsFileXml -> "application/vnd.omads-file+xml"
    Application_VndOmadsFolderXml -> "application/vnd.omads-folder+xml"
    Application_VndOmalocSuplInit -> "application/vnd.omaloc-supl-init"
    Application_VndOpenofficeorgExtension -> "application/vnd.openofficeorg.extension"
    Application_VndOpenxmlformatsOfficedocumentCustomPropertiesXml -> "application/vnd.openxmlformats-officedocument.custom-properties+xml"
    Application_VndOpenxmlformatsOfficedocumentCustomxmlpropertiesXml -> "application/vnd.openxmlformats-officedocument.customxmlproperties+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingXml -> "application/vnd.openxmlformats-officedocument.drawing+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartXml -> "application/vnd.openxmlformats-officedocument.drawingml.chart+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartshapesXml -> "application/vnd.openxmlformats-officedocument.drawingml.chartshapes+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramcolorsXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramcolors+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramdataXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramdata+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramlayoutXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramlayout+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramstyleXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramstyle+xml"
    Application_VndOpenxmlformatsOfficedocumentExtendedPropertiesXml -> "application/vnd.openxmlformats-officedocument.extended-properties+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentauthorsXml -> "application/vnd.openxmlformats-officedocument.presentationml.commentauthors+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentsXml -> "application/vnd.openxmlformats-officedocument.presentationml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlHandoutmasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.handoutmaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesmasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.notesmaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesslideXml -> "application/vnd.openxmlformats-officedocument.presentationml.notesslide+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentation -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentationMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPrespropsXml -> "application/vnd.openxmlformats-officedocument.presentationml.presprops+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlide -> "application/vnd.openxmlformats-officedocument.presentationml.slide"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideXml -> "application/vnd.openxmlformats-officedocument.presentationml.slide+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidelayoutXml -> "application/vnd.openxmlformats-officedocument.presentationml.slidelayout+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidemasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.slidemaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshow -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshowMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideupdateinfoXml -> "application/vnd.openxmlformats-officedocument.presentationml.slideupdateinfo+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTablestylesXml -> "application/vnd.openxmlformats-officedocument.presentationml.tablestyles+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTagsXml -> "application/vnd.openxmlformats-officedocument.presentationml.tags+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplate -> "application/vnd.openxmlformats-officedocument.presentationml.template"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlViewpropsXml -> "application/vnd.openxmlformats-officedocument.presentationml.viewprops+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCalcchainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.calcchain+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlChartsheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCommentsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlConnectionsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.connections+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlDialogsheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlExternallinkXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.externallink+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcachedefinitionXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotcachedefinition+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcacherecordsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotcacherecords+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivottableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivottable+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlQuerytableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.querytable+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionheadersXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionheaders+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionlogXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionlog+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSharedstringsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedstrings+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheet -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetMainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetmetadataXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheetmetadata+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlStylesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTablesinglecellsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.tablesinglecells+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlUsernamesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.usernames+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlVolatiledependenciesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.volatiledependencies+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlWorksheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    Application_VndOpenxmlformatsOfficedocumentThemeXml -> "application/vnd.openxmlformats-officedocument.theme+xml"
    Application_VndOpenxmlformatsOfficedocumentThemeoverrideXml -> "application/vnd.openxmlformats-officedocument.themeoverride+xml"
    Application_VndOpenxmlformatsOfficedocumentVmldrawing -> "application/vnd.openxmlformats-officedocument.vmldrawing"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlCommentsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocument -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentGlossaryXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentMainXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlEndnotesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFonttableXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.fonttable+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFooterXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFootnotesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlNumberingXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlSettingsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlStylesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplate -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlWebsettingsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.websettings+xml"
    Application_VndOpenxmlformatsPackageCorePropertiesXml -> "application/vnd.openxmlformats-package.core-properties+xml"
    Application_VndOpenxmlformatsPackageDigitalSignatureXmlsignatureXml -> "application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml"
    Application_VndOpenxmlformatsPackageRelationshipsXml -> "application/vnd.openxmlformats-package.relationships+xml"
    Application_VndOsaNetdeploy -> "application/vnd.osa.netdeploy"
    Application_VndOsgeoMapguidePackage -> "application/vnd.osgeo.mapguide.package"
    Application_VndOsgiBundle -> "application/vnd.osgi.bundle"
    Application_VndOsgiDp -> "application/vnd.osgi.dp"
    Application_VndOsgiSubsystem -> "application/vnd.osgi.subsystem"
    Application_VndOtpsCtKipXml -> "application/vnd.otps.ct-kip+xml"
    Application_VndPalm -> "application/vnd.palm"
    Application_VndPaosXml -> "application/vnd.paos.xml"
    Application_VndPawaafile -> "application/vnd.pawaafile"
    Application_VndPgFormat -> "application/vnd.pg.format"
    Application_VndPgOsasli -> "application/vnd.pg.osasli"
    Application_VndPiaccessApplicationLicence -> "application/vnd.piaccess.application-licence"
    Application_VndPicsel -> "application/vnd.picsel"
    Application_VndPmiWidget -> "application/vnd.pmi.widget"
    Application_VndPocGroupAdvertisementXml -> "application/vnd.poc.group-advertisement+xml"
    Application_VndPocketlearn -> "application/vnd.pocketlearn"
    Application_VndPowerbuilder6 -> "application/vnd.powerbuilder6"
    Application_VndPowerbuilder6S -> "application/vnd.powerbuilder6-s"
    Application_VndPowerbuilder7 -> "application/vnd.powerbuilder7"
    Application_VndPowerbuilder75 -> "application/vnd.powerbuilder75"
    Application_VndPowerbuilder75S -> "application/vnd.powerbuilder75-s"
    Application_VndPowerbuilder7S -> "application/vnd.powerbuilder7-s"
    Application_VndPreminet -> "application/vnd.preminet"
    Application_VndPreviewsystemsBox -> "application/vnd.previewsystems.box"
    Application_VndProteusMagazine -> "application/vnd.proteus.magazine"
    Application_VndPublishareDeltaTree -> "application/vnd.publishare-delta-tree"
    Application_VndPviPtid1 -> "application/vnd.pvi.ptid1"
    Application_VndPwgMultiplexed -> "application/vnd.pwg-multiplexed"
    Application_VndPwgXhtmlPrintXml -> "application/vnd.pwg-xhtml-print+xml"
    Application_VndQualcommBrewAppRes -> "application/vnd.qualcomm.brew-app-res"
    Application_VndQuarkQuarkxpress -> "application/vnd.quark.quarkxpress"
    Application_VndQuobjectQuoxdocument -> "application/vnd.quobject-quoxdocument"
    Application_VndRadisysMomlXml -> "application/vnd.radisys.moml+xml"
    Application_VndRadisysMsmlAuditConfXml -> "application/vnd.radisys.msml-audit-conf+xml"
    Application_VndRadisysMsmlAuditConnXml -> "application/vnd.radisys.msml-audit-conn+xml"
    Application_VndRadisysMsmlAuditDialogXml -> "application/vnd.radisys.msml-audit-dialog+xml"
    Application_VndRadisysMsmlAuditStreamXml -> "application/vnd.radisys.msml-audit-stream+xml"
    Application_VndRadisysMsmlAuditXml -> "application/vnd.radisys.msml-audit+xml"
    Application_VndRadisysMsmlConfXml -> "application/vnd.radisys.msml-conf+xml"
    Application_VndRadisysMsmlDialogBaseXml -> "application/vnd.radisys.msml-dialog-base+xml"
    Application_VndRadisysMsmlDialogFaxDetectXml -> "application/vnd.radisys.msml-dialog-fax-detect+xml"
    Application_VndRadisysMsmlDialogFaxSendrecvXml -> "application/vnd.radisys.msml-dialog-fax-sendrecv+xml"
    Application_VndRadisysMsmlDialogGroupXml -> "application/vnd.radisys.msml-dialog-group+xml"
    Application_VndRadisysMsmlDialogSpeechXml -> "application/vnd.radisys.msml-dialog-speech+xml"
    Application_VndRadisysMsmlDialogTransformXml -> "application/vnd.radisys.msml-dialog-transform+xml"
    Application_VndRadisysMsmlDialogXml -> "application/vnd.radisys.msml-dialog+xml"
    Application_VndRadisysMsmlXml -> "application/vnd.radisys.msml+xml"
    Application_VndRainstorData -> "application/vnd.rainstor.data"
    Application_VndRapid -> "application/vnd.rapid"
    Application_VndRealvncBed -> "application/vnd.realvnc.bed"
    Application_VndRecordareMusicxml -> "application/vnd.recordare.musicxml"
    Application_VndRecordareMusicxmlXml -> "application/vnd.recordare.musicxml+xml"
    Application_VndRenlearnRlprint -> "application/vnd.renlearn.rlprint"
    Application_VndRigCryptonote -> "application/vnd.rig.cryptonote"
    Application_VndRimCod -> "application/vnd.rim.cod"
    Application_VndRnRealmedia -> "application/vnd.rn-realmedia"
    Application_VndRnRealmediaVbr -> "application/vnd.rn-realmedia-vbr"
    Application_VndRnRealplayer -> "application/vnd.rn-realplayer"
    Application_VndRoute66Link66Xml -> "application/vnd.route66.link66+xml"
    Application_VndRs274x -> "application/vnd.rs-274x"
    Application_VndRuckusDownload -> "application/vnd.ruckus.download"
    Application_VndS3sms -> "application/vnd.s3sms"
    Application_VndSailingtrackerTrack -> "application/vnd.sailingtracker.track"
    Application_VndSbmCid -> "application/vnd.sbm.cid"
    Application_VndSbmMid2 -> "application/vnd.sbm.mid2"
    Application_VndScribus -> "application/vnd.scribus"
    Application_VndSealed3df -> "application/vnd.sealed.3df"
    Application_VndSealedCsf -> "application/vnd.sealed.csf"
    Application_VndSealedDoc -> "application/vnd.sealed.doc"
    Application_VndSealedEml -> "application/vnd.sealed.eml"
    Application_VndSealedMht -> "application/vnd.sealed.mht"
    Application_VndSealedNet -> "application/vnd.sealed.net"
    Application_VndSealedPpt -> "application/vnd.sealed.ppt"
    Application_VndSealedTiff -> "application/vnd.sealed.tiff"
    Application_VndSealedXls -> "application/vnd.sealed.xls"
    Application_VndSealedmediaSoftsealHtml -> "application/vnd.sealedmedia.softseal.html"
    Application_VndSealedmediaSoftsealPdf -> "application/vnd.sealedmedia.softseal.pdf"
    Application_VndSeemail -> "application/vnd.seemail"
    Application_VndSema -> "application/vnd.sema"
    Application_VndSemd -> "application/vnd.semd"
    Application_VndSemf -> "application/vnd.semf"
    Application_VndShanaInformedFormdata -> "application/vnd.shana.informed.formdata"
    Application_VndShanaInformedFormtemplate -> "application/vnd.shana.informed.formtemplate"
    Application_VndShanaInformedInterchange -> "application/vnd.shana.informed.interchange"
    Application_VndShanaInformedPackage -> "application/vnd.shana.informed.package"
    Application_VndSimtechMindmapper -> "application/vnd.simtech-mindmapper"
    Application_VndSmaf -> "application/vnd.smaf"
    Application_VndSmartNotebook -> "application/vnd.smart.notebook"
    Application_VndSmartTeacher -> "application/vnd.smart.teacher"
    Application_VndSoftware602FillerFormXml -> "application/vnd.software602.filler.form+xml"
    Application_VndSoftware602FillerFormXmlZip -> "application/vnd.software602.filler.form-xml-zip"
    Application_VndSolentSdkmXml -> "application/vnd.solent.sdkm+xml"
    Application_VndSpotfireDxp -> "application/vnd.spotfire.dxp"
    Application_VndSpotfireSfs -> "application/vnd.spotfire.sfs"
    Application_VndSssCod -> "application/vnd.sss-cod"
    Application_VndSssDtf -> "application/vnd.sss-dtf"
    Application_VndSssNtf -> "application/vnd.sss-ntf"
    Application_VndStardivisionCalc -> "application/vnd.stardivision.calc"
    Application_VndStardivisionDraw -> "application/vnd.stardivision.draw"
    Application_VndStardivisionImpress -> "application/vnd.stardivision.impress"
    Application_VndStardivisionMath -> "application/vnd.stardivision.math"
    Application_VndStardivisionWriter -> "application/vnd.stardivision.writer"
    Application_VndStardivisionWriterGlobal -> "application/vnd.stardivision.writer-global"
    Application_VndStepmaniaPackage -> "application/vnd.stepmania.package"
    Application_VndStepmaniaStepchart -> "application/vnd.stepmania.stepchart"
    Application_VndStreetStream -> "application/vnd.street-stream"
    Application_VndSunWadlXml -> "application/vnd.sun.wadl+xml"
    Application_VndSunXmlCalc -> "application/vnd.sun.xml.calc"
    Application_VndSunXmlCalcTemplate -> "application/vnd.sun.xml.calc.template"
    Application_VndSunXmlDraw -> "application/vnd.sun.xml.draw"
    Application_VndSunXmlDrawTemplate -> "application/vnd.sun.xml.draw.template"
    Application_VndSunXmlImpress -> "application/vnd.sun.xml.impress"
    Application_VndSunXmlImpressTemplate -> "application/vnd.sun.xml.impress.template"
    Application_VndSunXmlMath -> "application/vnd.sun.xml.math"
    Application_VndSunXmlWriter -> "application/vnd.sun.xml.writer"
    Application_VndSunXmlWriterGlobal -> "application/vnd.sun.xml.writer.global"
    Application_VndSunXmlWriterTemplate -> "application/vnd.sun.xml.writer.template"
    Application_VndSusCalendar -> "application/vnd.sus-calendar"
    Application_VndSvd -> "application/vnd.svd"
    Application_VndSwiftviewIcs -> "application/vnd.swiftview-ics"
    Application_VndSymbianInstall -> "application/vnd.symbian.install"
    Application_VndSyncmlDmNotification -> "application/vnd.syncml.dm.notification"
    Application_VndSyncmlDmWbxml -> "application/vnd.syncml.dm+wbxml"
    Application_VndSyncmlDmXml -> "application/vnd.syncml.dm+xml"
    Application_VndSyncmlDsNotification -> "application/vnd.syncml.ds.notification"
    Application_VndSyncmlXml -> "application/vnd.syncml+xml"
    Application_VndTaoIntentModuleArchive -> "application/vnd.tao.intent-module-archive"
    Application_VndTcpdumpPcap -> "application/vnd.tcpdump.pcap"
    Application_VndTmobileLivetv -> "application/vnd.tmobile-livetv"
    Application_VndTridTpt -> "application/vnd.trid.tpt"
    Application_VndTriscapeMxs -> "application/vnd.triscape.mxs"
    Application_VndTrueapp -> "application/vnd.trueapp"
    Application_VndTruedoc -> "application/vnd.truedoc"
    Application_VndTveTrigger -> "application/vnd.tve-trigger"
    Application_VndUbisoftWebplayer -> "application/vnd.ubisoft.webplayer"
    Application_VndUfdl -> "application/vnd.ufdl"
    Application_VndUiqTheme -> "application/vnd.uiq.theme"
    Application_VndUmajin -> "application/vnd.umajin"
    Application_VndUnity -> "application/vnd.unity"
    Application_VndUomlXml -> "application/vnd.uoml+xml"
    Application_VndUplanetAlert -> "application/vnd.uplanet.alert"
    Application_VndUplanetAlertWbxml -> "application/vnd.uplanet.alert-wbxml"
    Application_VndUplanetBearerChoice -> "application/vnd.uplanet.bearer-choice"
    Application_VndUplanetBearerChoiceWbxml -> "application/vnd.uplanet.bearer-choice-wbxml"
    Application_VndUplanetCacheop -> "application/vnd.uplanet.cacheop"
    Application_VndUplanetCacheopWbxml -> "application/vnd.uplanet.cacheop-wbxml"
    Application_VndUplanetChannel -> "application/vnd.uplanet.channel"
    Application_VndUplanetChannelWbxml -> "application/vnd.uplanet.channel-wbxml"
    Application_VndUplanetList -> "application/vnd.uplanet.list"
    Application_VndUplanetListWbxml -> "application/vnd.uplanet.list-wbxml"
    Application_VndUplanetListcmd -> "application/vnd.uplanet.listcmd"
    Application_VndUplanetListcmdWbxml -> "application/vnd.uplanet.listcmd-wbxml"
    Application_VndUplanetSignal -> "application/vnd.uplanet.signal"
    Application_VndVcx -> "application/vnd.vcx"
    Application_VndVdStudy -> "application/vnd.vd-study"
    Application_VndVectorworks -> "application/vnd.vectorworks"
    Application_VndVerimatrixVcas -> "application/vnd.verimatrix.vcas"
    Application_VndVidsoftVidconference -> "application/vnd.vidsoft.vidconference"
    Application_VndVisio -> "application/vnd.visio"
    Application_VndVisionary -> "application/vnd.visionary"
    Application_VndVividenceScriptfile -> "application/vnd.vividence.scriptfile"
    Application_VndVsf -> "application/vnd.vsf"
    Application_VndWapSic -> "application/vnd.wap.sic"
    Application_VndWapSlc -> "application/vnd.wap.slc"
    Application_VndWapWbxml -> "application/vnd.wap.wbxml"
    Application_VndWapWmlc -> "application/vnd.wap.wmlc"
    Application_VndWapWmlscriptc -> "application/vnd.wap.wmlscriptc"
    Application_VndWebturbo -> "application/vnd.webturbo"
    Application_VndWfaWsc -> "application/vnd.wfa.wsc"
    Application_VndWmc -> "application/vnd.wmc"
    Application_VndWmfBootstrap -> "application/vnd.wmf.bootstrap"
    Application_VndWolframMathematica -> "application/vnd.wolfram.mathematica"
    Application_VndWolframMathematicaPackage -> "application/vnd.wolfram.mathematica.package"
    Application_VndWolframPlayer -> "application/vnd.wolfram.player"
    Application_VndWordperfect -> "application/vnd.wordperfect"
    Application_VndWqd -> "application/vnd.wqd"
    Application_VndWrqHp3000Labelled -> "application/vnd.wrq-hp3000-labelled"
    Application_VndWtStf -> "application/vnd.wt.stf"
    Application_VndWvCspWbxml -> "application/vnd.wv.csp+wbxml"
    Application_VndWvCspXml -> "application/vnd.wv.csp+xml"
    Application_VndWvSspXml -> "application/vnd.wv.ssp+xml"
    Application_VndXara -> "application/vnd.xara"
    Application_VndXfdl -> "application/vnd.xfdl"
    Application_VndXfdlWebform -> "application/vnd.xfdl.webform"
    Application_VndXmiXml -> "application/vnd.xmi+xml"
    Application_VndXmpieCpkg -> "application/vnd.xmpie.cpkg"
    Application_VndXmpieDpkg -> "application/vnd.xmpie.dpkg"
    Application_VndXmpiePlan -> "application/vnd.xmpie.plan"
    Application_VndXmpiePpkg -> "application/vnd.xmpie.ppkg"
    Application_VndXmpieXlim -> "application/vnd.xmpie.xlim"
    Application_VndYamahaHvDic -> "application/vnd.yamaha.hv-dic"
    Application_VndYamahaHvScript -> "application/vnd.yamaha.hv-script"
    Application_VndYamahaHvVoice -> "application/vnd.yamaha.hv-voice"
    Application_VndYamahaOpenscoreformat -> "application/vnd.yamaha.openscoreformat"
    Application_VndYamahaOpenscoreformatOsfpvgXml -> "application/vnd.yamaha.openscoreformat.osfpvg+xml"
    Application_VndYamahaRemoteSetup -> "application/vnd.yamaha.remote-setup"
    Application_VndYamahaSmafAudio -> "application/vnd.yamaha.smaf-audio"
    Application_VndYamahaSmafPhrase -> "application/vnd.yamaha.smaf-phrase"
    Application_VndYamahaThroughNgn -> "application/vnd.yamaha.through-ngn"
    Application_VndYamahaTunnelUdpencap -> "application/vnd.yamaha.tunnel-udpencap"
    Application_VndYellowriverCustomMenu -> "application/vnd.yellowriver-custom-menu"
    Application_VndZul -> "application/vnd.zul"
    Application_VndZzazzDeckXml -> "application/vnd.zzazz.deck+xml"
    Application_VocaltecMediaDesc -> "application/vocaltec-media-desc"
    Application_VocaltecMediaFile -> "application/vocaltec-media-file"
    Application_VoicexmlXml -> "application/voicexml+xml"
    Application_VqRtcpxr -> "application/vq-rtcpxr"
    Application_WatcherinfoXml -> "application/watcherinfo+xml"
    Application_WhoisppQuery -> "application/whoispp-query"
    Application_WhoisppResponse -> "application/whoispp-response"
    Application_Widget -> "application/widget"
    Application_Winhlp -> "application/winhlp"
    Application_Wita -> "application/wita"
    Application_Wordperfect -> "application/wordperfect"
    Application_Wordperfect51 -> "application/wordperfect5.1"
    Application_Wordperfect60 -> "application/wordperfect6.0"
    Application_Wordperfect61 -> "application/wordperfect6.1"
    Application_WsdlXml -> "application/wsdl+xml"
    Application_WspolicyXml -> "application/wspolicy+xml"
    Application_X123 -> "application/x-123"
    Application_X400Bp -> "application/x400-bp"
    Application_X7zCompressed -> "application/x-7z-compressed"
    Application_XAbiword -> "application/x-abiword"
    Application_XAceCompressed -> "application/x-ace-compressed"
    Application_XAim -> "application/x-aim"
    Application_XAmf -> "application/x-amf"
    Application_XAppleDiskimage -> "application/x-apple-diskimage"
    Application_XAuthorwareBin -> "application/x-authorware-bin"
    Application_XAuthorwareMap -> "application/x-authorware-map"
    Application_XAuthorwareSeg -> "application/x-authorware-seg"
    Application_XBcpio -> "application/x-bcpio"
    Application_XBinary -> "application/x-binary"
    Application_XBinhex40 -> "application/x-binhex40"
    Application_XBittorrent -> "application/x-bittorrent"
    Application_XBlorb -> "application/x-blorb"
    Application_XBsh -> "application/x-bsh"
    Application_XBytecodeElisp -> "application/x-bytecode.elisp"
    Application_XBytecodeElispCompiledelisp -> "application/x-bytecode.elisp(compiledelisp)"
    Application_XBytecodePython -> "application/x-bytecode.python"
    Application_XBzip -> "application/x-bzip"
    Application_XBzip2 -> "application/x-bzip2"
    Application_XCbr -> "application/x-cbr"
    Application_XCdf -> "application/x-cdf"
    Application_XCdlink -> "application/x-cdlink"
    Application_XCfsCompressed -> "application/x-cfs-compressed"
    Application_XChat -> "application/x-chat"
    Application_XChessPgn -> "application/x-chess-pgn"
    Application_XChm -> "application/x-chm"
    Application_XChromeExtension -> "application/x-chrome-extension"
    Application_XCmuRaster -> "application/x-cmu-raster"
    Application_XCocoa -> "application/x-cocoa"
    Application_XCompactpro -> "application/x-compactpro"
    Application_XCompress -> "application/x-compress"
    Application_XCompressed -> "application/x-compressed"
    Application_XConference -> "application/x-conference"
    Application_XCore -> "application/x-core"
    Application_XCpio -> "application/x-cpio"
    Application_XCpt -> "application/x-cpt"
    Application_XCsh -> "application/x-csh"
    Application_XDebianPackage -> "application/x-debian-package"
    Application_XDeepv -> "application/x-deepv"
    Application_XDgcCompressed -> "application/x-dgc-compressed"
    Application_XDirector -> "application/x-director"
    Application_XDms -> "application/x-dms"
    Application_XDoom -> "application/x-doom"
    Application_XDtbncxXml -> "application/x-dtbncx+xml"
    Application_XDtbookXml -> "application/x-dtbook+xml"
    Application_XDtbresourceXml -> "application/x-dtbresource+xml"
    Application_XDvi -> "application/x-dvi"
    Application_XElc -> "application/x-elc"
    Application_XEnvoy -> "application/x-envoy"
    Application_XEsrehber -> "application/x-esrehber"
    Application_XEva -> "application/x-eva"
    Application_XExcel -> "application/x-excel"
    Application_XExecutable -> "application/x-executable"
    Application_XFlac -> "application/x-flac"
    Application_XFont -> "application/x-font"
    Application_XFontBdf -> "application/x-font-bdf"
    Application_XFontDos -> "application/x-font-dos"
    Application_XFontFramemaker -> "application/x-font-framemaker"
    Application_XFontGhostscript -> "application/x-font-ghostscript"
    Application_XFontLibgrx -> "application/x-font-libgrx"
    Application_XFontLinuxPsf -> "application/x-font-linux-psf"
    Application_XFontOtf -> "application/x-font-otf"
    Application_XFontPcf -> "application/x-font-pcf"
    Application_XFontSnf -> "application/x-font-snf"
    Application_XFontSpeedo -> "application/x-font-speedo"
    Application_XFontSunosNews -> "application/x-font-sunos-news"
    Application_XFontTtf -> "application/x-font-ttf"
    Application_XFontType1 -> "application/x-font-type1"
    Application_XFontVfont -> "application/x-font-vfont"
    Application_XFontWoff -> "application/x-font-woff"
    Application_XFrame -> "application/x-frame"
    Application_XFreearc -> "application/x-freearc"
    Application_XFreelance -> "application/x-freelance"
    Application_XFuturesplash -> "application/x-futuresplash"
    Application_XGcaCompressed -> "application/x-gca-compressed"
    Application_XGlulx -> "application/x-glulx"
    Application_XGnumeric -> "application/x-gnumeric"
    Application_XGoSgf -> "application/x-go-sgf"
    Application_XGrampsXml -> "application/x-gramps-xml"
    Application_XGraphingCalculator -> "application/x-graphing-calculator"
    Application_XGsp -> "application/x-gsp"
    Application_XGss -> "application/x-gss"
    Application_XGtar -> "application/x-gtar"
    Application_XGzip -> "application/x-gzip"
    Application_XHdf -> "application/x-hdf"
    Application_XHelpfile -> "application/x-helpfile"
    Application_XHttpdImap -> "application/x-httpd-imap"
    Application_XHttpdPhp -> "application/x-httpd-php"
    Application_XHttpdPhp3 -> "application/x-httpd-php3"
    Application_XHttpdPhp3Preprocessed -> "application/x-httpd-php3-preprocessed"
    Application_XHttpdPhp4 -> "application/x-httpd-php4"
    Application_XHttpdPhpSource -> "application/x-httpd-php-source"
    Application_XIca -> "application/x-ica"
    Application_XIma -> "application/x-ima"
    Application_XInstallInstructions -> "application/x-install-instructions"
    Application_XInternetSignup -> "application/x-internet-signup"
    Application_XInternettSignup -> "application/x-internett-signup"
    Application_XInventor -> "application/x-inventor"
    Application_XIp2 -> "application/x-ip2"
    Application_XIphone -> "application/x-iphone"
    Application_XIso9660Image -> "application/x-iso9660-image"
    Application_XJavaApplet -> "application/x-java-applet"
    Application_XJavaArchive -> "application/x-java-archive"
    Application_XJavaBean -> "application/x-java-bean"
    Application_XJavaClass -> "application/x-java-class"
    Application_XJavaCommerce -> "application/x-java-commerce"
    Application_XJavaJnlpFile -> "application/x-java-jnlp-file"
    Application_XJavaSerializedObject -> "application/x-java-serialized-object"
    Application_XJavaVm -> "application/x-java-vm"
    Application_XJavascript -> "application/x-javascript"
    Application_XKchart -> "application/x-kchart"
    Application_XKdelnk -> "application/x-kdelnk"
    Application_XKillustrator -> "application/x-killustrator"
    Application_XKoan -> "application/x-koan"
    Application_XKpresenter -> "application/x-kpresenter"
    Application_XKsh -> "application/x-ksh"
    Application_XKspread -> "application/x-kspread"
    Application_XKword -> "application/x-kword"
    Application_XLatex -> "application/x-latex"
    Application_XLha -> "application/x-lha"
    Application_XLisp -> "application/x-lisp"
    Application_XLivescreen -> "application/x-livescreen"
    Application_XLotus -> "application/x-lotus"
    Application_XLotusscreencam -> "application/x-lotusscreencam"
    Application_XLuaBytecode -> "application/x-lua-bytecode"
    Application_XLzh -> "application/x-lzh"
    Application_XLzhCompressed -> "application/x-lzh-compressed"
    Application_XLzx -> "application/x-lzx"
    Application_XMacBinhex40 -> "application/x-mac-binhex40"
    Application_XMacbinary -> "application/x-macbinary"
    Application_XMagicCapPackage10 -> "application/x-magic-cap-package-1.0"
    Application_XMaker -> "application/x-maker"
    Application_XMathcad -> "application/x-mathcad"
    Application_XMeme -> "application/x-meme"
    Application_XMidi -> "application/x-midi"
    Application_XMie -> "application/x-mie"
    Application_XMif -> "application/x-mif"
    Application_XMixTransfer -> "application/x-mix-transfer"
    Application_XMobipocketEbook -> "application/x-mobipocket-ebook"
    Application_XMpegurl -> "application/x-mpegURL"
    Application_XMplayer2 -> "application/x-mplayer2"
    Application_XMsApplication -> "application/x-ms-application"
    Application_XMsShortcut -> "application/x-ms-shortcut"
    Application_XMsWmd -> "application/x-ms-wmd"
    Application_XMsWmz -> "application/x-ms-wmz"
    Application_XMsXbap -> "application/x-ms-xbap"
    Application_XMsaccess -> "application/x-msaccess"
    Application_XMsbinder -> "application/x-msbinder"
    Application_XMscardfile -> "application/x-mscardfile"
    Application_XMsclip -> "application/x-msclip"
    Application_XMsdosProgram -> "application/x-msdos-program"
    Application_XMsdownload -> "application/x-msdownload"
    Application_XMsexcel -> "application/x-msexcel"
    Application_XMsi -> "application/x-msi"
    Application_XMsmediaview -> "application/x-msmediaview"
    Application_XMsmetafile -> "application/x-msmetafile"
    Application_XMsmoney -> "application/x-msmoney"
    Application_XMspowerpoint -> "application/x-mspowerpoint"
    Application_XMspublisher -> "application/x-mspublisher"
    Application_XMsschedule -> "application/x-msschedule"
    Application_XMsterminal -> "application/x-msterminal"
    Application_XMswrite -> "application/x-mswrite"
    Application_XNaviAnimation -> "application/x-navi-animation"
    Application_XNavidoc -> "application/x-navidoc"
    Application_XNavimap -> "application/x-navimap"
    Application_XNavistyle -> "application/x-navistyle"
    Application_XNetcdf -> "application/x-netcdf"
    Application_XNewtonCompatiblePkg -> "application/x-newton-compatible-pkg"
    Application_XNokia9000CommunicatorAddOnSoftware -> "application/x-nokia-9000-communicator-add-on-software"
    Application_XNsProxyAutoconfig -> "application/x-ns-proxy-autoconfig"
    Application_XNwc -> "application/x-nwc"
    Application_XNzb -> "application/x-nzb"
    Application_XObject -> "application/x-object"
    Application_XOmc -> "application/x-omc"
    Application_XOmcdatamaker -> "application/x-omcdatamaker"
    Application_XOmcregerator -> "application/x-omcregerator"
    Application_XOzApplication -> "application/x-oz-application"
    Application_XPagemaker -> "application/x-pagemaker"
    Application_XPcl -> "application/x-pcl"
    Application_XPerfmon -> "application/x-perfmon"
    Application_XPixclscript -> "application/x-pixclscript"
    Application_XPkcs10 -> "application/x-pkcs10"
    Application_XPkcs12 -> "application/x-pkcs12"
    Application_XPkcs7Certificates -> "application/x-pkcs7-certificates"
    Application_XPkcs7Certreqresp -> "application/x-pkcs7-certreqresp"
    Application_XPkcs7Crl -> "application/x-pkcs7-crl"
    Application_XPkcs7Mime -> "application/x-pkcs7-mime"
    Application_XPkcs7Signature -> "application/x-pkcs7-signature"
    Application_XPointplus -> "application/x-pointplus"
    Application_XPortableAnymap -> "application/x-portable-anymap"
    Application_XProject -> "application/x-project"
    Application_XPythonCode -> "application/x-python-code"
    Application_XQpro -> "application/x-qpro"
    Application_XQuicktimeplayer -> "application/x-quicktimeplayer"
    Application_XRarCompressed -> "application/x-rar-compressed"
    Application_XRedhatPackageManager -> "application/x-redhat-package-manager"
    Application_XResearchInfoSystems -> "application/x-research-info-systems"
    Application_XRpm -> "application/x-rpm"
    Application_XRtf -> "application/x-rtf"
    Application_XRx -> "application/x-rx"
    Application_XSdp -> "application/x-sdp"
    Application_XSea -> "application/x-sea"
    Application_XSeelogo -> "application/x-seelogo"
    Application_XSh -> "application/x-sh"
    Application_XShar -> "application/x-shar"
    Application_XShellscript -> "application/x-shellscript"
    Application_XShockwaveFlash -> "application/x-shockwave-flash"
    Application_XSilverlightApp -> "application/x-silverlight-app"
    Application_XSit -> "application/x-sit"
    Application_XSprite -> "application/x-sprite"
    Application_XSql -> "application/x-sql"
    Application_XStuffit -> "application/x-stuffit"
    Application_XStuffitx -> "application/x-stuffitx"
    Application_XSubrip -> "application/x-subrip"
    Application_XSv4cpio -> "application/x-sv4cpio"
    Application_XSv4crc -> "application/x-sv4crc"
    Application_XT3vmImage -> "application/x-t3vm-image"
    Application_XTads -> "application/x-tads"
    Application_XTar -> "application/x-tar"
    Application_XTbook -> "application/x-tbook"
    Application_XTcl -> "application/x-tcl"
    Application_XTex -> "application/x-tex"
    Application_XTexGf -> "application/x-tex-gf"
    Application_XTexPk -> "application/x-tex-pk"
    Application_XTexTfm -> "application/x-tex-tfm"
    Application_XTexinfo -> "application/x-texinfo"
    Application_XTgif -> "application/x-tgif"
    Application_XTrash -> "application/x-trash"
    Application_XTroff -> "application/x-troff"
    Application_XTroffMan -> "application/x-troff-man"
    Application_XTroffMe -> "application/x-troff-me"
    Application_XTroffMs -> "application/x-troff-ms"
    Application_XTroffMsvideo -> "application/x-troff-msvideo"
    Application_XUstar -> "application/x-ustar"
    Application_XVideolan -> "application/x-videolan"
    Application_XVisio -> "application/x-visio"
    Application_XVndAudioexplosionMzz -> "application/x-vnd.audioexplosion.mzz"
    Application_XVndLsXpix -> "application/x-vnd.ls-xpix"
    Application_XVrml -> "application/x-vrml"
    Application_XWaisSource -> "application/x-wais-source"
    Application_XWebAppManifestJson -> "application/x-web-app-manifest+json"
    Application_XWingz -> "application/x-wingz"
    Application_XWinhelp -> "application/x-winhelp"
    Application_XWintalk -> "application/x-wintalk"
    Application_XWorld -> "application/x-world"
    Application_XWpwin -> "application/x-wpwin"
    Application_XWri -> "application/x-wri"
    Application_XX509CaCert -> "application/x-x509-ca-cert"
    Application_XX509UserCert -> "application/x-x509-user-cert"
    Application_XXcf -> "application/x-xcf"
    Application_XXfig -> "application/x-xfig"
    Application_XXliffXml -> "application/x-xliff+xml"
    Application_XXpinstall -> "application/x-xpinstall"
    Application_XXz -> "application/x-xz"
    Application_XZipCompressed -> "application/x-zip-compressed"
    Application_XZmachine -> "application/x-zmachine"
    Application_XamlXml -> "application/xaml+xml"
    Application_XcapAttXml -> "application/xcap-att+xml"
    Application_XcapCapsXml -> "application/xcap-caps+xml"
    Application_XcapDiffXml -> "application/xcap-diff+xml"
    Application_XcapElXml -> "application/xcap-el+xml"
    Application_XcapErrorXml -> "application/xcap-error+xml"
    Application_XcapNsXml -> "application/xcap-ns+xml"
    Application_XconConferenceInfoDiffXml -> "application/xcon-conference-info-diff+xml"
    Application_XconConferenceInfoXml -> "application/xcon-conference-info+xml"
    Application_XencXml -> "application/xenc+xml"
    Application_XhtmlVoiceXml -> "application/xhtml-voice+xml"
    Application_XhtmlXml -> "application/xhtml+xml"
    Application_Xml -> "application/xml"
    Application_XmlDtd -> "application/xml-dtd"
    Application_XmlExternalParsedEntity -> "application/xml-external-parsed-entity"
    Application_XmppXml -> "application/xmpp+xml"
    Application_XopXml -> "application/xop+xml"
    Application_XprocXml -> "application/xproc+xml"
    Application_XsltXml -> "application/xslt+xml"
    Application_XspfXml -> "application/xspf+xml"
    Application_XvXml -> "application/xv+xml"
    Application_Yang -> "application/yang"
    Application_YinXml -> "application/yin+xml"
    Application_YndMsPkipko -> "application/ynd.ms-pkipko"
    Application_Zip -> "application/zip"
    Audio_1dInterleavedParityfec -> "audio/1d-interleaved-parityfec"
    Audio_32kadpcm -> "audio/32kadpcm"
    Audio_3gpp -> "audio/3gpp"
    Audio_3gpp2 -> "audio/3gpp2"
    Audio_Ac3 -> "audio/ac3"
    Audio_Adpcm -> "audio/adpcm"
    Audio_Aiff -> "audio/aiff"
    Audio_Amr -> "audio/amr"
    Audio_AmrWb -> "audio/amr-wb"
    Audio_Asc -> "audio/asc"
    Audio_Atrac3 -> "audio/atrac3"
    Audio_AtracAdvancedLossless -> "audio/atrac-advanced-lossless"
    Audio_AtracX -> "audio/atrac-x"
    Audio_Basic -> "audio/basic"
    Audio_Bv16 -> "audio/bv16"
    Audio_Bv32 -> "audio/bv32"
    Audio_Clearmode -> "audio/clearmode"
    Audio_Cn -> "audio/cn"
    Audio_Dat12 -> "audio/dat12"
    Audio_Dls -> "audio/dls"
    Audio_DsrEs201108 -> "audio/dsr-es201108"
    Audio_DsrEs202050 -> "audio/dsr-es202050"
    Audio_DsrEs202211 -> "audio/dsr-es202211"
    Audio_DsrEs202212 -> "audio/dsr-es202212"
    Audio_Dv -> "audio/dv"
    Audio_Dvi4 -> "audio/dvi4"
    Audio_Eac3 -> "audio/eac3"
    Audio_Evrc -> "audio/evrc"
    Audio_Evrc0 -> "audio/evrc0"
    Audio_Evrc1 -> "audio/evrc1"
    Audio_EvrcQcp -> "audio/evrc-qcp"
    Audio_Evrcb -> "audio/evrcb"
    Audio_Evrcb0 -> "audio/evrcb0"
    Audio_Evrcb1 -> "audio/evrcb1"
    Audio_Evrcwb -> "audio/evrcwb"
    Audio_Evrcwb0 -> "audio/evrcwb0"
    Audio_Evrcwb1 -> "audio/evrcwb1"
    Audio_Example -> "audio/example"
    Audio_Flac -> "audio/flac"
    Audio_Fwdred -> "audio/fwdred"
    Audio_G719 -> "audio/g719"
    Audio_G722 -> "audio/g722"
    Audio_G7221 -> "audio/g.722.1"
    Audio_G723 -> "audio/g723"
    Audio_G72616 -> "audio/g726-16"
    Audio_G72624 -> "audio/g726-24"
    Audio_G72632 -> "audio/g726-32"
    Audio_G72640 -> "audio/g726-40"
    Audio_G728 -> "audio/g728"
    Audio_G729 -> "audio/g729"
    Audio_G7291 -> "audio/g7291"
    Audio_G729d -> "audio/g729d"
    Audio_G729e -> "audio/g729e"
    Audio_Gsm -> "audio/gsm"
    Audio_GsmEfr -> "audio/gsm-efr"
    Audio_GsmHr08 -> "audio/gsm-hr-08"
    Audio_Ilbc -> "audio/ilbc"
    Audio_IpMr_v25 -> "audio/ip-mr_v2.5"
    Audio_Isac -> "audio/isac"
    Audio_It -> "audio/it"
    Audio_L16 -> "audio/l16"
    Audio_L20 -> "audio/l20"
    Audio_L24 -> "audio/l24"
    Audio_L8 -> "audio/l8"
    Audio_Lpc -> "audio/lpc"
    Audio_Make -> "audio/make"
    Audio_MakeMyFunk -> "audio/make.my.funk"
    Audio_Mid -> "audio/mid"
    Audio_Midi -> "audio/midi"
    Audio_MobileXmf -> "audio/mobile-xmf"
    Audio_Mod -> "audio/mod"
    Audio_Mp4 -> "audio/mp4"
    Audio_Mp4aLatm -> "audio/mp4a-latm"
    Audio_Mpa -> "audio/mpa"
    Audio_MpaRobust -> "audio/mpa-robust"
    Audio_Mpeg -> "audio/mpeg"
    Audio_Mpeg3 -> "audio/mpeg3"
    Audio_Mpeg4Generic -> "audio/mpeg4-generic"
    Audio_Mpegurl -> "audio/mpegurl"
    Audio_Musepack -> "audio/musepack"
    Audio_Nspaudio -> "audio/nspaudio"
    Audio_Ogg -> "audio/ogg"
    Audio_Opus -> "audio/opus"
    Audio_Parityfec -> "audio/parityfec"
    Audio_Pcma -> "audio/pcma"
    Audio_PcmaWb -> "audio/pcma-wb"
    Audio_Pcmu -> "audio/pcmu"
    Audio_PcmuWb -> "audio/pcmu-wb"
    Audio_PrsSid -> "audio/prs.sid"
    Audio_Qcelp -> "audio/qcelp"
    Audio_Red -> "audio/red"
    Audio_RtpEncAescm128 -> "audio/rtp-enc-aescm128"
    Audio_RtpMidi -> "audio/rtp-midi"
    Audio_Rtx -> "audio/rtx"
    Audio_S3m -> "audio/s3m"
    Audio_Silk -> "audio/silk"
    Audio_Smv -> "audio/smv"
    Audio_Smv0 -> "audio/smv0"
    Audio_SmvQcp -> "audio/smv-qcp"
    Audio_SpMidi -> "audio/sp-midi"
    Audio_Speex -> "audio/speex"
    Audio_T140c -> "audio/t140c"
    Audio_T38 -> "audio/t38"
    Audio_TelephoneEvent -> "audio/telephone-event"
    Audio_Tone -> "audio/tone"
    Audio_TspAudio -> "audio/tsp-audio"
    Audio_Tsplayer -> "audio/tsplayer"
    Audio_Uemclip -> "audio/uemclip"
    Audio_Ulpfec -> "audio/ulpfec"
    Audio_Vdvi -> "audio/vdvi"
    Audio_VmrWb -> "audio/vmr-wb"
    Audio_Vnd3gppIufp -> "audio/vnd.3gpp.iufp"
    Audio_Vnd4sb -> "audio/vnd.4sb"
    Audio_VndAudiokoz -> "audio/vnd.audiokoz"
    Audio_VndCelp -> "audio/vnd.celp"
    Audio_VndCiscoNse -> "audio/vnd.cisco.nse"
    Audio_VndCmlesRadioEvents -> "audio/vnd.cmles.radio-events"
    Audio_VndCnsAnp1 -> "audio/vnd.cns.anp1"
    Audio_VndCnsInf1 -> "audio/vnd.cns.inf1"
    Audio_VndDeceAudio -> "audio/vnd.dece.audio"
    Audio_VndDigitalWinds -> "audio/vnd.digital-winds"
    Audio_VndDlnaAdts -> "audio/vnd.dlna.adts"
    Audio_VndDolbyHeaac1 -> "audio/vnd.dolby.heaac.1"
    Audio_VndDolbyHeaac2 -> "audio/vnd.dolby.heaac.2"
    Audio_VndDolbyMlp -> "audio/vnd.dolby.mlp"
    Audio_VndDolbyMps -> "audio/vnd.dolby.mps"
    Audio_VndDolbyPl2 -> "audio/vnd.dolby.pl2"
    Audio_VndDolbyPl2x -> "audio/vnd.dolby.pl2x"
    Audio_VndDolbyPl2z -> "audio/vnd.dolby.pl2z"
    Audio_VndDolbyPulse1 -> "audio/vnd.dolby.pulse.1"
    Audio_VndDra -> "audio/vnd.dra"
    Audio_VndDts -> "audio/vnd.dts"
    Audio_VndDtsHd -> "audio/vnd.dts.hd"
    Audio_VndDvbFile -> "audio/vnd.dvb.file"
    Audio_VndEveradPlj -> "audio/vnd.everad.plj"
    Audio_VndHnsAudio -> "audio/vnd.hns.audio"
    Audio_VndLucentVoice -> "audio/vnd.lucent.voice"
    Audio_VndMsPlayreadyMediaPya -> "audio/vnd.ms-playready.media.pya"
    Audio_VndNokiaMobileXmf -> "audio/vnd.nokia.mobile-xmf"
    Audio_VndNortelVbk -> "audio/vnd.nortel.vbk"
    Audio_VndNueraEcelp4800 -> "audio/vnd.nuera.ecelp4800"
    Audio_VndNueraEcelp7470 -> "audio/vnd.nuera.ecelp7470"
    Audio_VndNueraEcelp9600 -> "audio/vnd.nuera.ecelp9600"
    Audio_VndOctelSbc -> "audio/vnd.octel.sbc"
    Audio_VndQcelp -> "audio/vnd.qcelp"
    Audio_VndRhetorex32kadpcm -> "audio/vnd.rhetorex.32kadpcm"
    Audio_VndRip -> "audio/vnd.rip"
    Audio_VndSealedmediaSoftsealMpeg -> "audio/vnd.sealedmedia.softseal.mpeg"
    Audio_VndVmxCvsd -> "audio/vnd.vmx.cvsd"
    Audio_Voc -> "audio/voc"
    Audio_Vorbis -> "audio/vorbis"
    Audio_VorbisConfig -> "audio/vorbis-config"
    Audio_Voxware -> "audio/voxware"
    Audio_Wav -> "audio/wav"
    Audio_Webm -> "audio/webm"
    Audio_XAac -> "audio/x-aac"
    Audio_XAdpcm -> "audio/x-adpcm"
    Audio_XAiff -> "audio/x-aiff"
    Audio_XAu -> "audio/x-au"
    Audio_XCaf -> "audio/x-caf"
    Audio_XFlac -> "audio/x-flac"
    Audio_XGsm -> "audio/x-gsm"
    Audio_XJam -> "audio/x-jam"
    Audio_XLiveaudio -> "audio/x-liveaudio"
    Audio_XMatroska -> "audio/x-matroska"
    Audio_XMid -> "audio/x-mid"
    Audio_XMidi -> "audio/x-midi"
    Audio_XMod -> "audio/x-mod"
    Audio_XMpeg -> "audio/x-mpeg"
    Audio_XMpeg3 -> "audio/x-mpeg-3"
    Audio_XMpegurl -> "audio/x-mpegurl"
    Audio_XMpequrl -> "audio/x-mpequrl"
    Audio_XMsWax -> "audio/x-ms-wax"
    Audio_XMsWma -> "audio/x-ms-wma"
    Audio_XNspaudio -> "audio/x-nspaudio"
    Audio_XPnRealaudio -> "audio/x-pn-realaudio"
    Audio_XPnRealaudioPlugin -> "audio/x-pn-realaudio-plugin"
    Audio_XPsid -> "audio/x-psid"
    Audio_XRealaudio -> "audio/x-realaudio"
    Audio_XScpls -> "audio/x-scpls"
    Audio_XSd2 -> "audio/x-sd2"
    Audio_XTta -> "audio/x-tta"
    Audio_XTwinvq -> "audio/x-twinvq"
    Audio_XTwinvqPlugin -> "audio/x-twinvq-plugin"
    Audio_XVndAudioexplosionMjuicemediafile -> "audio/x-vnd.audioexplosion.mjuicemediafile"
    Audio_XVoc -> "audio/x-voc"
    Audio_XWav -> "audio/x-wav"
    Audio_Xm -> "audio/xm"
    Chemical_XCdx -> "chemical/x-cdx"
    Chemical_XCif -> "chemical/x-cif"
    Chemical_XCmdf -> "chemical/x-cmdf"
    Chemical_XCml -> "chemical/x-cml"
    Chemical_XCsml -> "chemical/x-csml"
    Chemical_XPdb -> "chemical/x-pdb"
    Chemical_XXyz -> "chemical/x-xyz"
    Content_Unknown -> "content/unknown"
    Drawing_XDwf -> "drawing/x-dwf"
    Drawing_XDwfOld -> "drawing/x-dwf(old)"
    Font_Opentype -> "font/opentype"
    IWorld_IVrml -> "i-world/i-vrml"
    Image_Bmp -> "image/bmp"
    Image_Cgm -> "image/cgm"
    Image_CisCod -> "image/cis-cod"
    Image_CmuRaster -> "image/cmu-raster"
    Image_Example -> "image/example"
    Image_Fif -> "image/fif"
    Image_Fits -> "image/fits"
    Image_Florian -> "image/florian"
    Image_G3fax -> "image/g3fax"
    Image_Gif -> "image/gif"
    Image_Ief -> "image/ief"
    Image_Jp2 -> "image/jp2"
    Image_Jpeg -> "image/jpeg"
    Image_Jpm -> "image/jpm"
    Image_Jpx -> "image/jpx"
    Image_Jutvision -> "image/jutvision"
    Image_Ktx -> "image/ktx"
    Image_Naplps -> "image/naplps"
    Image_Pcx -> "image/pcx"
    Image_Pict -> "image/pict"
    Image_Pipeg -> "image/pipeg"
    Image_Pjpeg -> "image/pjpeg"
    Image_Png -> "image/png"
    Image_PrsBtif -> "image/prs.btif"
    Image_PrsPti -> "image/prs.pti"
    Image_Sgi -> "image/sgi"
    Image_SvgXml -> "image/svg+xml"
    Image_T38 -> "image/t38"
    Image_Tiff -> "image/tiff"
    Image_TiffFx -> "image/tiff-fx"
    Image_Vasa -> "image/vasa"
    Image_VndAdobePhotoshop -> "image/vnd.adobe.photoshop"
    Image_VndCnsInf2 -> "image/vnd.cns.inf2"
    Image_VndDeceGraphic -> "image/vnd.dece.graphic"
    Image_VndDjvu -> "image/vnd.djvu"
    Image_VndDvbSubtitle -> "image/vnd.dvb.subtitle"
    Image_VndDwg -> "image/vnd.dwg"
    Image_VndDxf -> "image/vnd.dxf"
    Image_VndFastbidsheet -> "image/vnd.fastbidsheet"
    Image_VndFpx -> "image/vnd.fpx"
    Image_VndFst -> "image/vnd.fst"
    Image_VndFujixeroxEdmicsMmr -> "image/vnd.fujixerox.edmics-mmr"
    Image_VndFujixeroxEdmicsRlc -> "image/vnd.fujixerox.edmics-rlc"
    Image_VndGlobalgraphicsPgb -> "image/vnd.globalgraphics.pgb"
    Image_VndMicrosoftIcon -> "image/vnd.microsoft.icon"
    Image_VndMix -> "image/vnd.mix"
    Image_VndMsModi -> "image/vnd.ms-modi"
    Image_VndMsPhoto -> "image/vnd.ms-photo"
    Image_VndNetFpx -> "image/vnd.net-fpx"
    Image_VndRadiance -> "image/vnd.radiance"
    Image_VndRnRealflash -> "image/vnd.rn-realflash"
    Image_VndRnRealpix -> "image/vnd.rn-realpix"
    Image_VndSealedPng -> "image/vnd.sealed.png"
    Image_VndSealedmediaSoftsealGif -> "image/vnd.sealedmedia.softseal.gif"
    Image_VndSealedmediaSoftsealJpg -> "image/vnd.sealedmedia.softseal.jpg"
    Image_VndSvf -> "image/vnd.svf"
    Image_VndWapWbmp -> "image/vnd.wap.wbmp"
    Image_VndXiff -> "image/vnd.xiff"
    Image_Webp -> "image/webp"
    Image_X3ds -> "image/x-3ds"
    Image_XCmuRast -> "image/x-cmu-rast"
    Image_XCmuRaster -> "image/x-cmu-raster"
    Image_XCmx -> "image/x-cmx"
    Image_XCoreldraw -> "image/x-coreldraw"
    Image_XCoreldrawpattern -> "image/x-coreldrawpattern"
    Image_XCoreldrawtemplate -> "image/x-coreldrawtemplate"
    Image_XCorelphotopaint -> "image/x-corelphotopaint"
    Image_XDwg -> "image/x-dwg"
    Image_XFreehand -> "image/x-freehand"
    Image_XIcon -> "image/x-icon"
    Image_XJg -> "image/x-jg"
    Image_XJng -> "image/x-jng"
    Image_XJps -> "image/x-jps"
    Image_XMrsidImage -> "image/x-mrsid-image"
    Image_XMsBmp -> "image/x-ms-bmp"
    Image_XNiff -> "image/x-niff"
    Image_XPcx -> "image/x-pcx"
    Image_XPhotoshop -> "image/x-photoshop"
    Image_XPict -> "image/x-pict"
    Image_XPortableAnymap -> "image/x-portable-anymap"
    Image_XPortableBitmap -> "image/x-portable-bitmap"
    Image_XPortableGraymap -> "image/x-portable-graymap"
    Image_XPortableGreymap -> "image/x-portable-greymap"
    Image_XPortablePixmap -> "image/x-portable-pixmap"
    Image_XQuicktime -> "image/x-quicktime"
    Image_XRgb -> "image/x-rgb"
    Image_XTga -> "image/x-tga"
    Image_XTiff -> "image/x-tiff"
    Image_XWindowsBmp -> "image/x-windows-bmp"
    Image_XXbitmap -> "image/x-xbitmap"
    Image_XXbm -> "image/x-xbm"
    Image_XXpixmap -> "image/x-xpixmap"
    Image_XXwd -> "image/x-xwd"
    Image_XXwindowdump -> "image/x-xwindowdump"
    Image_Xbm -> "image/xbm"
    Image_Xpm -> "image/xpm"
    Inode_Blockdevice -> "inode/blockdevice"
    Inode_Chardevice -> "inode/chardevice"
    Inode_Directory -> "inode/directory"
    Inode_DirectoryLocked -> "inode/directory-locked"
    Inode_Fifo -> "inode/fifo"
    Inode_Socket -> "inode/socket"
    Message_Cpim -> "message/cpim"
    Message_DeliveryStatus -> "message/delivery-status"
    Message_DispositionNotification -> "message/disposition-notification"
    Message_Example -> "message/example"
    Message_ExternalBody -> "message/external-body"
    Message_FeedbackReport -> "message/feedback-report"
    Message_Global -> "message/global"
    Message_GlobalDeliveryStatus -> "message/global-delivery-status"
    Message_GlobalDispositionNotification -> "message/global-disposition-notification"
    Message_GlobalHeaders -> "message/global-headers"
    Message_Http -> "message/http"
    Message_ImdnXml -> "message/imdn+xml"
    Message_News -> "message/news"
    Message_Partial -> "message/partial"
    Message_Rfc822 -> "message/rfc822"
    Message_SHttp -> "message/s-http"
    Message_Sip -> "message/sip"
    Message_Sipfrag -> "message/sipfrag"
    Message_TrackingStatus -> "message/tracking-status"
    Message_VndSiSimp -> "message/vnd.si.simp"
    Model_Example -> "model/example"
    Model_Iges -> "model/iges"
    Model_Mesh -> "model/mesh"
    Model_VndColladaXml -> "model/vnd.collada+xml"
    Model_VndDwf -> "model/vnd.dwf"
    Model_VndFlatland3dml -> "model/vnd.flatland.3dml"
    Model_VndGdl -> "model/vnd.gdl"
    Model_VndGsGdl -> "model/vnd.gs-gdl"
    Model_VndGtw -> "model/vnd.gtw"
    Model_VndMomlXml -> "model/vnd.moml+xml"
    Model_VndMts -> "model/vnd.mts"
    Model_VndParasolidTransmitBinary -> "model/vnd.parasolid.transmit.binary"
    Model_VndParasolidTransmitText -> "model/vnd.parasolid.transmit.text"
    Model_VndVtu -> "model/vnd.vtu"
    Model_Vrml -> "model/vrml"
    Model_X3dBinary -> "model/x3d+binary"
    Model_X3dVrml -> "model/x3d+vrml"
    Model_X3dXml -> "model/x3d+xml"
    Model_XPov -> "model/x-pov"
    Multipart_Alternative -> "multipart/alternative"
    Multipart_Appledouble -> "multipart/appledouble"
    Multipart_Byteranges -> "multipart/byteranges"
    Multipart_Digest -> "multipart/digest"
    Multipart_Encrypted -> "multipart/encrypted"
    Multipart_Example -> "multipart/example"
    Multipart_FormData -> "multipart/form-data"
    Multipart_HeaderSet -> "multipart/header-set"
    Multipart_Mixed -> "multipart/mixed"
    Multipart_Parallel -> "multipart/parallel"
    Multipart_Related -> "multipart/related"
    Multipart_Report -> "multipart/report"
    Multipart_Signed -> "multipart/signed"
    Multipart_VoiceMessage -> "multipart/voice-message"
    Multipart_XGzip -> "multipart/x-gzip"
    Multipart_XUstar -> "multipart/x-ustar"
    Multipart_XZip -> "multipart/x-zip"
    Music_Crescendo -> "music/crescendo"
    Music_XKaraoke -> "music/x-karaoke"
    Paleovu_XPv -> "paleovu/x-pv"
    Text_1dInterleavedParityfec -> "text/1d-interleaved-parityfec"
    Text_Asp -> "text/asp"
    Text_CacheManifest -> "text/cache-manifest"
    Text_Calendar -> "text/calendar"
    Text_CommaSeparatedValues -> "text/comma-separated-values"
    Text_Css -> "text/css"
    Text_Csv -> "text/csv"
    Text_Directory -> "text/directory"
    Text_Dns -> "text/dns"
    Text_Ecmascript -> "text/ecmascript"
    Text_English -> "text/english"
    Text_Enriched -> "text/enriched"
    Text_EventStream -> "text/event-stream"
    Text_Example -> "text/example"
    Text_Fwdred -> "text/fwdred"
    Text_H323 -> "text/h323"
    Text_Html -> "text/html"
    Text_Iuls -> "text/iuls"
    Text_Javascript -> "text/javascript"
    Text_Mathml -> "text/mathml"
    Text_Mcf -> "text/mcf"
    Text_N3 -> "text/n3"
    Text_Parityfec -> "text/parityfec"
    Text_Pascal -> "text/pascal"
    Text_Plain -> "text/plain"
    Text_PlainBas -> "text/plain-bas"
    Text_PrsFallensteinRst -> "text/prs.fallenstein.rst"
    Text_PrsLinesTag -> "text/prs.lines.tag"
    Text_Red -> "text/red"
    Text_Rfc822Headers -> "text/rfc822-headers"
    Text_Richtext -> "text/richtext"
    Text_Rtf -> "text/rtf"
    Text_RtpEncAescm128 -> "text/rtp-enc-aescm128"
    Text_Rtx -> "text/rtx"
    Text_Scriplet -> "text/scriplet"
    Text_Scriptlet -> "text/scriptlet"
    Text_Sgml -> "text/sgml"
    Text_T140 -> "text/t140"
    Text_TabSeparatedValues -> "text/tab-separated-values"
    Text_Texmacs -> "text/texmacs"
    Text_Troff -> "text/troff"
    Text_Turtle -> "text/turtle"
    Text_Ulpfec -> "text/ulpfec"
    Text_UriList -> "text/uri-list"
    Text_Vcard -> "text/vcard"
    Text_VndAbc -> "text/vnd.abc"
    Text_VndCurl -> "text/vnd.curl"
    Text_VndCurlDcurl -> "text/vnd.curl.dcurl"
    Text_VndCurlMcurl -> "text/vnd.curl.mcurl"
    Text_VndCurlScurl -> "text/vnd.curl.scurl"
    Text_VndDmclientscript -> "text/vnd.DMClientScript"
    Text_VndDvbSubtitle -> "text/vnd.dvb.subtitle"
    Text_VndEsmertecThemeDescriptor -> "text/vnd.esmertec.theme-descriptor"
    Text_VndFlatland3dml -> "text/vnd.flatland.3dml"
    Text_VndFly -> "text/vnd.fly"
    Text_VndFmiFlexstor -> "text/vnd.fmi.flexstor"
    Text_VndGraphviz -> "text/vnd.graphviz"
    Text_VndIn3d3dml -> "text/vnd.in3d.3dml"
    Text_VndIn3dSpot -> "text/vnd.in3d.spot"
    Text_VndIptcNewsml -> "text/vnd.IPTC.NewsML"
    Text_VndIptcNitf -> "text/vnd.IPTC.NITF"
    Text_VndLatexZ -> "text/vnd.latex-z"
    Text_VndMotorolaReflex -> "text/vnd.motorola.reflex"
    Text_VndMsMediapackage -> "text/vnd.ms-mediapackage"
    Text_VndNet2phoneCommcenterCommand -> "text/vnd.net2phone.commcenter.command"
    Text_VndRadisysMsmlBasicLayout -> "text/vnd.radisys.msml-basic-layout"
    Text_VndRnRealtext -> "text/vnd.rn-realtext"
    Text_VndSiUricatalogue -> "text/vnd.si.uricatalogue"
    Text_VndSunJ2meAppDescriptor -> "text/vnd.sun.j2me.app-descriptor"
    Text_VndTrolltechLinguist -> "text/vnd.trolltech.linguist"
    Text_VndWapSi -> "text/vnd.wap.si"
    Text_VndWapSl -> "text/vnd.wap.sl"
    Text_VndWapWml -> "text/vnd.wap.wml"
    Text_VndWapWmlscript -> "text/vnd.wap.wmlscript"
    Text_Vtt -> "text/vtt"
    Text_Webviewhtml -> "text/webviewhtml"
    Text_XAsm -> "text/x-asm"
    Text_XAudiosoftIntra -> "text/x-audiosoft-intra"
    Text_XC -> "text/x-c"
    Text_XCHdr -> "text/x-c++hdr"
    Text_XCSrc -> "text/x-c++src"
    Text_XChdr -> "text/x-chdr"
    Text_XComponent -> "text/x-component"
    Text_XCrontab -> "text/x-crontab"
    Text_XCsh -> "text/x-csh"
    Text_XCsrc -> "text/x-csrc"
    Text_XFortran -> "text/x-fortran"
    Text_XH -> "text/x-h"
    Text_XJava -> "text/x-java"
    Text_XJavaSource -> "text/x-java-source"
    Text_XLaAsf -> "text/x-la-asf"
    Text_XLua -> "text/x-lua"
    Text_XM -> "text/x-m"
    Text_XMakefile -> "text/x-makefile"
    Text_XMarkdown -> "text/x-markdown"
    Text_XMoc -> "text/x-moc"
    Text_XNfo -> "text/x-nfo"
    Text_XOpml -> "text/x-opml"
    Text_XPascal -> "text/x-pascal"
    Text_XPcsGcd -> "text/x-pcs-gcd"
    Text_XPerl -> "text/x-perl"
    Text_XPython -> "text/x-python"
    Text_XScript -> "text/x-script"
    Text_XScriptCsh -> "text/x-script.csh"
    Text_XScriptElisp -> "text/x-script.elisp"
    Text_XScriptGuile -> "text/x-script.guile"
    Text_XScriptKsh -> "text/x-script.ksh"
    Text_XScriptLisp -> "text/x-script.lisp"
    Text_XScriptPerl -> "text/x-script.perl"
    Text_XScriptPerlModule -> "text/x-script.perl-module"
    Text_XScriptPhyton -> "text/x-script.phyton"
    Text_XScriptRexx -> "text/x-script.rexx"
    Text_XScriptScheme -> "text/x-script.scheme"
    Text_XScriptSh -> "text/x-script.sh"
    Text_XScriptTcl -> "text/x-script.tcl"
    Text_XScriptTcsh -> "text/x-script.tcsh"
    Text_XScriptZsh -> "text/x-script.zsh"
    Text_XServerParsedHtml -> "text/x-server-parsed-html"
    Text_XSetext -> "text/x-setext"
    Text_XSfv -> "text/x-sfv"
    Text_XSgml -> "text/x-sgml"
    Text_XSh -> "text/x-sh"
    Text_XSpeech -> "text/x-speech"
    Text_XTcl -> "text/x-tcl"
    Text_XTex -> "text/x-tex"
    Text_XUil -> "text/x-uil"
    Text_XUuencode -> "text/x-uuencode"
    Text_XVcalendar -> "text/x-vcalendar"
    Text_XVcard -> "text/x-vcard"
    Text_Xml -> "text/xml"
    Text_XmlExternalParsedEntity -> "text/xml-external-parsed-entity"
    Unknown_Unknown -> "unknown/unknown"
    Video_1dInterleavedParityfec -> "video/1d-interleaved-parityfec"
    Video_3gpp -> "video/3gpp"
    Video_3gpp2 -> "video/3gpp2"
    Video_3gppTt -> "video/3gpp-tt"
    Video_Animaflex -> "video/animaflex"
    Video_Avi -> "video/avi"
    Video_AvsVideo -> "video/avs-video"
    Video_Bmpeg -> "video/bmpeg"
    Video_Bt656 -> "video/bt656"
    Video_Celb -> "video/celb"
    Video_Dl -> "video/dl"
    Video_Dv -> "video/dv"
    Video_Example -> "video/example"
    Video_Flc -> "video/flc"
    Video_Fli -> "video/fli"
    Video_Gl -> "video/gl"
    Video_H261 -> "video/h261"
    Video_H263 -> "video/h263"
    Video_H2631998 -> "video/h263-1998"
    Video_H2632000 -> "video/h263-2000"
    Video_H264 -> "video/h264"
    Video_H264Rcdo -> "video/h264-rcdo"
    Video_H264Svc -> "video/h264-svc"
    Video_Jpeg -> "video/jpeg"
    Video_Jpeg2000 -> "video/jpeg2000"
    Video_Jpm -> "video/jpm"
    Video_Mj2 -> "video/mj2"
    Video_Mp1s -> "video/mp1s"
    Video_Mp2p -> "video/mp2p"
    Video_Mp2t -> "video/MP2T"
    Video_Mp4 -> "video/mp4"
    Video_Mp4vEs -> "video/mp4v-es"
    Video_Mpeg -> "video/mpeg"
    Video_Mpeg4Generic -> "video/mpeg4-generic"
    Video_Mpv -> "video/mpv"
    Video_Msvideo -> "video/msvideo"
    Video_Nv -> "video/nv"
    Video_Ogg -> "video/ogg"
    Video_Parityfec -> "video/parityfec"
    Video_Pointer -> "video/pointer"
    Video_Quicktime -> "video/quicktime"
    Video_Raw -> "video/raw"
    Video_RtpEncAescm128 -> "video/rtp-enc-aescm128"
    Video_Rtx -> "video/rtx"
    Video_Smpte292m -> "video/smpte292m"
    Video_Ulpfec -> "video/ulpfec"
    Video_Vc1 -> "video/vc1"
    Video_Vdo -> "video/vdo"
    Video_Vivo -> "video/vivo"
    Video_VndCctv -> "video/vnd.cctv"
    Video_VndDeceHd -> "video/vnd.dece.hd"
    Video_VndDeceMobile -> "video/vnd.dece.mobile"
    Video_VndDeceMp4 -> "video/vnd.dece.mp4"
    Video_VndDecePd -> "video/vnd.dece.pd"
    Video_VndDeceSd -> "video/vnd.dece.sd"
    Video_VndDeceVideo -> "video/vnd.dece.video"
    Video_VndDirectvMpeg -> "video/vnd.directv.mpeg"
    Video_VndDirectvMpegTts -> "video/vnd.directv.mpeg-tts"
    Video_VndDlnaMpegTts -> "video/vnd.dlna.mpeg-tts"
    Video_VndDvbFile -> "video/vnd.dvb.file"
    Video_VndFvt -> "video/vnd.fvt"
    Video_VndHnsVideo -> "video/vnd.hns.video"
    Video_VndIptvforum1dparityfec1010 -> "video/vnd.iptvforum.1dparityfec-1010"
    Video_VndIptvforum1dparityfec2005 -> "video/vnd.iptvforum.1dparityfec-2005"
    Video_VndIptvforum2dparityfec1010 -> "video/vnd.iptvforum.2dparityfec-1010"
    Video_VndIptvforum2dparityfec2005 -> "video/vnd.iptvforum.2dparityfec-2005"
    Video_VndIptvforumTtsavc -> "video/vnd.iptvforum.ttsavc"
    Video_VndIptvforumTtsmpeg2 -> "video/vnd.iptvforum.ttsmpeg2"
    Video_VndMotorolaVideo -> "video/vnd.motorola.video"
    Video_VndMotorolaVideop -> "video/vnd.motorola.videop"
    Video_VndMpegurl -> "video/vnd.mpegurl"
    Video_VndMsPlayreadyMediaPyv -> "video/vnd.ms-playready.media.pyv"
    Video_VndMts -> "video/vnd.mts"
    Video_VndNokiaInterleavedMultimedia -> "video/vnd.nokia.interleaved-multimedia"
    Video_VndNokiaVideovoip -> "video/vnd.nokia.videovoip"
    Video_VndObjectvideo -> "video/vnd.objectvideo"
    Video_VndRnRealvideo -> "video/vnd.rn-realvideo"
    Video_VndSealedMpeg1 -> "video/vnd.sealed.mpeg1"
    Video_VndSealedMpeg4 -> "video/vnd.sealed.mpeg4"
    Video_VndSealedSwf -> "video/vnd.sealed.swf"
    Video_VndSealedmediaSoftsealMov -> "video/vnd.sealedmedia.softseal.mov"
    Video_VndUvvuMp4 -> "video/vnd.uvvu.mp4"
    Video_VndVivo -> "video/vnd.vivo"
    Video_Vosaic -> "video/vosaic"
    Video_Webm -> "video/webm"
    Video_XAmtDemorun -> "video/x-amt-demorun"
    Video_XAmtShowrun -> "video/x-amt-showrun"
    Video_XAtomic3dFeature -> "video/x-atomic3d-feature"
    Video_XDl -> "video/x-dl"
    Video_XDv -> "video/x-dv"
    Video_XF4v -> "video/x-f4v"
    Video_XFli -> "video/x-fli"
    Video_XFlv -> "video/x-flv"
    Video_XGl -> "video/x-gl"
    Video_XIsvideo -> "video/x-isvideo"
    Video_XLaAsf -> "video/x-la-asf"
    Video_XM4v -> "video/x-m4v"
    Video_XMatroska -> "video/x-matroska"
    Video_XMng -> "video/x-mng"
    Video_XMotionJpeg -> "video/x-motion-jpeg"
    Video_XMpeg -> "video/x-mpeg"
    Video_XMpeq2a -> "video/x-mpeq2a"
    Video_XMsAsf -> "video/x-ms-asf"
    Video_XMsAsfPlugin -> "video/x-ms-asf-plugin"
    Video_XMsVob -> "video/x-ms-vob"
    Video_XMsWm -> "video/x-ms-wm"
    Video_XMsWmv -> "video/x-ms-wmv"
    Video_XMsWmx -> "video/x-ms-wmx"
    Video_XMsWvx -> "video/x-ms-wvx"
    Video_XMsvideo -> "video/x-msvideo"
    Video_XQtc -> "video/x-qtc"
    Video_XScm -> "video/x-scm"
    Video_XSgiMovie -> "video/x-sgi-movie"
    Video_XSmv -> "video/x-smv"
    Windows_Metafile -> "windows/metafile"
    Www_Mime -> "www/mime"
    XConference_XCooltalk -> "x-conference/x-cooltalk"
    XMusic_XMidi -> "x-music/x-midi"
    XWorld_X3dmf -> "x-world/x-3dmf"
    XWorld_XSvr -> "x-world/x-svr"
    XWorld_XVrml -> "x-world/x-vrml"
    XWorld_XVrt -> "x-world/x-vrt"
    Xgl_Drawing -> "xgl/drawing"
    Xgl_Movie -> "xgl/movie"

mimeTypeToText :: MIME_Type -> T.Text
mimeTypeToText mimeType =
  case mimeType of
    Application_1dInterleavedParityfec -> "application/1d-interleaved-parityfec"
    Application_3gppImsXml -> "application/3gpp-ims+xml"
    Application_Acad -> "application/acad"
    Application_Activemessage -> "application/activemessage"
    Application_AndrewInset -> "application/andrew-inset"
    Application_Applefile -> "application/applefile"
    Application_Applixware -> "application/applixware"
    Application_Arj -> "application/arj"
    Application_AtomXml -> "application/atom+xml"
    Application_AtomcatXml -> "application/atomcat+xml"
    Application_Atomicmail -> "application/atomicmail"
    Application_AtomsvcXml -> "application/atomsvc+xml"
    Application_AuthPolicyXml -> "application/auth-policy+xml"
    Application_Base64 -> "application/base64"
    Application_BatchSmtp -> "application/batch-SMTP"
    Application_BeepXml -> "application/beep+xml"
    Application_Binhex -> "application/binhex"
    Application_Binhex4 -> "application/binhex4"
    Application_Book -> "application/book"
    Application_CalendarXml -> "application/calendar+xml"
    Application_Cals1840 -> "application/cals-1840"
    Application_CcmpXml -> "application/ccmp+xml"
    Application_CcxmlXml -> "application/ccxml+xml"
    Application_Cdf -> "application/cdf"
    Application_CdmiCapability -> "application/cdmi-capability"
    Application_CdmiContainer -> "application/cdmi-container"
    Application_CdmiDomain -> "application/cdmi-domain"
    Application_CdmiObject -> "application/cdmi-object"
    Application_CdmiQueue -> "application/cdmi-queue"
    Application_Cea2018Xml -> "application/cea-2018+xml"
    Application_CellmlXml -> "application/cellml+xml"
    Application_Cfw -> "application/cfw"
    Application_Clariscad -> "application/clariscad"
    Application_CnrpXml -> "application/cnrp+xml"
    Application_Commonground -> "application/commonground"
    Application_ConferenceInfoXml -> "application/conference-info+xml"
    Application_CplXml -> "application/cpl+xml"
    Application_CstaXml -> "application/csta+xml"
    Application_CstadataXml -> "application/cstadata+xml"
    Application_CuSeeme -> "application/cu-seeme"
    Application_Cybercash -> "application/cybercash"
    Application_DavmountXml -> "application/davmount+xml"
    Application_DcaRft -> "application/dca-rft"
    Application_DecDx -> "application/dec-dx"
    Application_DialogInfoXml -> "application/dialog-info+xml"
    Application_Dicom -> "application/dicom"
    Application_Dns -> "application/dns"
    Application_DocbookXml -> "application/docbook+xml"
    Application_Drafting -> "application/drafting"
    Application_DskppXml -> "application/dskpp+xml"
    Application_Dsptype -> "application/dsptype"
    Application_DsscDer -> "application/dssc+der"
    Application_DsscXml -> "application/dssc+xml"
    Application_Dvcs -> "application/dvcs"
    Application_Dxf -> "application/dxf"
    Application_Ecmascript -> "application/ecmascript"
    Application_EdiConsent -> "application/edi-consent"
    Application_EdiX12 -> "application/edi-x12"
    Application_Edifact -> "application/edifact"
    Application_EmmaXml -> "application/emma+xml"
    Application_Envoy -> "application/envoy"
    Application_EppXml -> "application/epp+xml"
    Application_EpubZip -> "application/epub+zip"
    Application_Eshop -> "application/eshop"
    Application_Example -> "application/example"
    Application_Excel -> "application/excel"
    Application_Exi -> "application/exi"
    Application_Fastinfoset -> "application/fastinfoset"
    Application_Fastsoap -> "application/fastsoap"
    Application_Fits -> "application/fits"
    Application_FontTdpfr -> "application/font-tdpfr"
    Application_FontWoff -> "application/font-woff"
    Application_Fractals -> "application/fractals"
    Application_FrameworkAttributesXml -> "application/framework-attributes+xml"
    Application_Freeloader -> "application/freeloader"
    Application_Futuresplash -> "application/futuresplash"
    Application_Ghostview -> "application/ghostview"
    Application_GmlXml -> "application/gml+xml"
    Application_Gnutar -> "application/gnutar"
    Application_GpxXml -> "application/gpx+xml"
    Application_Groupwise -> "application/groupwise"
    Application_Gxf -> "application/gxf"
    Application_H224 -> "application/h224"
    Application_HeldXml -> "application/held+xml"
    Application_Hlp -> "application/hlp"
    Application_Hta -> "application/hta"
    Application_Http -> "application/http"
    Application_Hyperstudio -> "application/hyperstudio"
    Application_IDeas -> "application/i-deas"
    Application_IbeKeyRequestXml -> "application/ibe-key-request+xml"
    Application_IbePkgReplyXml -> "application/ibe-pkg-reply+xml"
    Application_IbePpData -> "application/ibe-pp-data"
    Application_Iges -> "application/iges"
    Application_ImIscomposingXml -> "application/im-iscomposing+xml"
    Application_Index -> "application/index"
    Application_IndexCmd -> "application/index.cmd"
    Application_IndexObj -> "application/index.obj"
    Application_IndexResponse -> "application/index.response"
    Application_IndexVnd -> "application/index.vnd"
    Application_Inf -> "application/inf"
    Application_InkmlXml -> "application/inkml+xml"
    Application_InternetPropertyStream -> "application/internet-property-stream"
    Application_Iotp -> "application/iotp"
    Application_Ipfix -> "application/ipfix"
    Application_Ipp -> "application/ipp"
    Application_Isup -> "application/isup"
    Application_Java -> "application/java"
    Application_JavaArchive -> "application/java-archive"
    Application_JavaByteCode -> "application/java-byte-code"
    Application_JavaSerializedObject -> "application/java-serialized-object"
    Application_JavaVm -> "application/java-vm"
    Application_Javascript -> "application/javascript"
    Application_Json -> "application/json"
    Application_JsonmlJson -> "application/jsonml+json"
    Application_KpmlRequestXml -> "application/kpml-request+xml"
    Application_KpmlResponseXml -> "application/kpml-response+xml"
    Application_Lha -> "application/lha"
    Application_LostXml -> "application/lost+xml"
    Application_Lzx -> "application/lzx"
    Application_MacBinary -> "application/mac-binary"
    Application_MacBinhex -> "application/mac-binhex"
    Application_MacBinhex40 -> "application/mac-binhex40"
    Application_MacCompactpro -> "application/mac-compactpro"
    Application_Macbinary -> "application/macbinary"
    Application_Macwriteii -> "application/macwriteii"
    Application_MadsXml -> "application/mads+xml"
    Application_Marc -> "application/marc"
    Application_MarcxmlXml -> "application/marcxml+xml"
    Application_Mathematica -> "application/mathematica"
    Application_MathematicaOld -> "application/mathematica-old"
    Application_MathmlContentXml -> "application/mathml-content+xml"
    Application_MathmlPresentationXml -> "application/mathml-presentation+xml"
    Application_MathmlXml -> "application/mathml+xml"
    Application_Mbedlet -> "application/mbedlet"
    Application_MbmsAssociatedProcedureDescriptionXml -> "application/mbms-associated-procedure-description+xml"
    Application_MbmsDeregisterXml -> "application/mbms-deregister+xml"
    Application_MbmsEnvelopeXml -> "application/mbms-envelope+xml"
    Application_MbmsMskResponseXml -> "application/mbms-msk-response+xml"
    Application_MbmsMskXml -> "application/mbms-msk+xml"
    Application_MbmsProtectionDescriptionXml -> "application/mbms-protection-description+xml"
    Application_MbmsReceptionReportXml -> "application/mbms-reception-report+xml"
    Application_MbmsRegisterResponseXml -> "application/mbms-register-response+xml"
    Application_MbmsRegisterXml -> "application/mbms-register+xml"
    Application_MbmsUserServiceDescriptionXml -> "application/mbms-user-service-description+xml"
    Application_Mbox -> "application/mbox"
    Application_Mcad -> "application/mcad"
    Application_Media_controlXml -> "application/media_control+xml"
    Application_MediaservercontrolXml -> "application/mediaservercontrol+xml"
    Application_Metalink4Xml -> "application/metalink4+xml"
    Application_MetalinkXml -> "application/metalink+xml"
    Application_MetsXml -> "application/mets+xml"
    Application_Mikey -> "application/mikey"
    Application_Mime -> "application/mime"
    Application_ModsXml -> "application/mods+xml"
    Application_MossKeys -> "application/moss-keys"
    Application_MossSignature -> "application/moss-signature"
    Application_MosskeyData -> "application/mosskey-data"
    Application_MosskeyRequest -> "application/mosskey-request"
    Application_Mp21 -> "application/mp21"
    Application_Mp4 -> "application/mp4"
    Application_Mpeg4Generic -> "application/mpeg4-generic"
    Application_Mpeg4Iod -> "application/mpeg4-iod"
    Application_Mpeg4IodXmt -> "application/mpeg4-iod-xmt"
    Application_Msaccess -> "application/msaccess"
    Application_MscIvrXml -> "application/msc-ivr+xml"
    Application_MscMixerXml -> "application/msc-mixer+xml"
    Application_Msonenote -> "application/msonenote"
    Application_Mspowerpoint -> "application/mspowerpoint"
    Application_Msword -> "application/msword"
    Application_Mswrite -> "application/mswrite"
    Application_Mxf -> "application/mxf"
    Application_Nasdata -> "application/nasdata"
    Application_Netmc -> "application/netmc"
    Application_NewsCheckgroups -> "application/news-checkgroups"
    Application_NewsGroupinfo -> "application/news-groupinfo"
    Application_NewsMessageId -> "application/news-message-id"
    Application_NewsTransmission -> "application/news-transmission"
    Application_Nss -> "application/nss"
    Application_OcspRequest -> "application/ocsp-request"
    Application_OcspResponse -> "application/ocsp-response"
    Application_OctetStream -> "application/octet-stream"
    Application_Oda -> "application/oda"
    Application_OebpsPackageXml -> "application/oebps-package+xml"
    Application_Ogg -> "application/ogg"
    Application_Olescript -> "application/olescript"
    Application_OmdocXml -> "application/omdoc+xml"
    Application_Onenote -> "application/onenote"
    Application_Oxps -> "application/oxps"
    Application_Parityfec -> "application/parityfec"
    Application_PatchOpsErrorXml -> "application/patch-ops-error+xml"
    Application_Pdf -> "application/pdf"
    Application_PgpEncrypted -> "application/pgp-encrypted"
    Application_PgpKeys -> "application/pgp-keys"
    Application_PgpSignature -> "application/pgp-signature"
    Application_PicsRules -> "application/pics-rules"
    Application_PidfDiffXml -> "application/pidf-diff+xml"
    Application_PidfXml -> "application/pidf+xml"
    Application_Pkcs10 -> "application/pkcs10"
    Application_Pkcs12 -> "application/pkcs-12"
    Application_Pkcs7Mime -> "application/pkcs7-mime"
    Application_Pkcs7Signature -> "application/pkcs7-signature"
    Application_Pkcs8 -> "application/pkcs8"
    Application_PkcsCrl -> "application/pkcs-crl"
    Application_PkixAttrCert -> "application/pkix-attr-cert"
    Application_PkixCert -> "application/pkix-cert"
    Application_PkixCrl -> "application/pkix-crl"
    Application_PkixPkipath -> "application/pkix-pkipath"
    Application_Pkixcmp -> "application/pkixcmp"
    Application_Plain -> "application/plain"
    Application_PlsXml -> "application/pls+xml"
    Application_PocSettingsXml -> "application/poc-settings+xml"
    Application_Postscript -> "application/postscript"
    Application_Powerpoint -> "application/powerpoint"
    Application_Pro_eng -> "application/pro_eng"
    Application_PrsAlvestrandTitraxSheet -> "application/prs.alvestrand.titrax-sheet"
    Application_PrsCww -> "application/prs.cww"
    Application_PrsNprend -> "application/prs.nprend"
    Application_PrsPlucker -> "application/prs.plucker"
    Application_PrsRdfXmlCrypt -> "application/prs.rdf-xml-crypt"
    Application_PrsXsfXml -> "application/prs.xsf+xml"
    Application_PskcXml -> "application/pskc+xml"
    Application_Qsig -> "application/qsig"
    Application_Rar -> "application/rar"
    Application_RdfXml -> "application/rdf+xml"
    Application_ReginfoXml -> "application/reginfo+xml"
    Application_RelaxNgCompactSyntax -> "application/relax-ng-compact-syntax"
    Application_RemotePrinting -> "application/remote-printing"
    Application_ResourceListsDiffXml -> "application/resource-lists-diff+xml"
    Application_ResourceListsXml -> "application/resource-lists+xml"
    Application_RingingTones -> "application/ringing-tones"
    Application_Riscos -> "application/riscos"
    Application_RlmiXml -> "application/rlmi+xml"
    Application_RlsServicesXml -> "application/rls-services+xml"
    Application_RpkiGhostbusters -> "application/rpki-ghostbusters"
    Application_RpkiManifest -> "application/rpki-manifest"
    Application_RpkiRoa -> "application/rpki-roa"
    Application_RpkiUpdown -> "application/rpki-updown"
    Application_RsdXml -> "application/rsd+xml"
    Application_RssXml -> "application/rss+xml"
    Application_Rtf -> "application/rtf"
    Application_Rtx -> "application/rtx"
    Application_SamlassertionXml -> "application/samlassertion+xml"
    Application_SamlmetadataXml -> "application/samlmetadata+xml"
    Application_SbmlXml -> "application/sbml+xml"
    Application_ScvpCvRequest -> "application/scvp-cv-request"
    Application_ScvpCvResponse -> "application/scvp-cv-response"
    Application_ScvpVpRequest -> "application/scvp-vp-request"
    Application_ScvpVpResponse -> "application/scvp-vp-response"
    Application_Sdp -> "application/sdp"
    Application_Sea -> "application/sea"
    Application_Set -> "application/set"
    Application_SetPayment -> "application/set-payment"
    Application_SetPaymentInitiation -> "application/set-payment-initiation"
    Application_SetRegistration -> "application/set-registration"
    Application_SetRegistrationInitiation -> "application/set-registration-initiation"
    Application_Sgml -> "application/sgml"
    Application_SgmlOpenCatalog -> "application/sgml-open-catalog"
    Application_ShfXml -> "application/shf+xml"
    Application_Sieve -> "application/sieve"
    Application_SimpleFilterXml -> "application/simple-filter+xml"
    Application_SimpleMessageSummary -> "application/simple-message-summary"
    Application_Simplesymbolcontainer -> "application/simplesymbolcontainer"
    Application_Sla -> "application/sla"
    Application_Slate -> "application/slate"
    Application_Smil -> "application/smil"
    Application_SmilXml -> "application/smil+xml"
    Application_SoapFastinfoset -> "application/soap+fastinfoset"
    Application_SoapXml -> "application/soap+xml"
    Application_Solids -> "application/solids"
    Application_Sounder -> "application/sounder"
    Application_SparqlQuery -> "application/sparql-query"
    Application_SparqlResultsXml -> "application/sparql-results+xml"
    Application_SpiritsEventXml -> "application/spirits-event+xml"
    Application_Srgs -> "application/srgs"
    Application_SrgsXml -> "application/srgs+xml"
    Application_SruXml -> "application/sru+xml"
    Application_SsdlXml -> "application/ssdl+xml"
    Application_SsmlXml -> "application/ssml+xml"
    Application_Step -> "application/step"
    Application_Streamingmedia -> "application/streamingmedia"
    Application_TampApexUpdate -> "application/tamp-apex-update"
    Application_TampApexUpdateConfirm -> "application/tamp-apex-update-confirm"
    Application_TampCommunityUpdate -> "application/tamp-community-update"
    Application_TampCommunityUpdateConfirm -> "application/tamp-community-update-confirm"
    Application_TampError -> "application/tamp-error"
    Application_TampSequenceAdjust -> "application/tamp-sequence-adjust"
    Application_TampSequenceAdjustConfirm -> "application/tamp-sequence-adjust-confirm"
    Application_TampStatusQuery -> "application/tamp-status-query"
    Application_TampStatusResponse -> "application/tamp-status-response"
    Application_TampUpdate -> "application/tamp-update"
    Application_TampUpdateConfirm -> "application/tamp-update-confirm"
    Application_TeiXml -> "application/tei+xml"
    Application_ThraudXml -> "application/thraud+xml"
    Application_TimestampQuery -> "application/timestamp-query"
    Application_TimestampReply -> "application/timestamp-reply"
    Application_TimestampedData -> "application/timestamped-data"
    Application_Toolbook -> "application/toolbook"
    Application_TveTrigger -> "application/tve-trigger"
    Application_Ulpfec -> "application/ulpfec"
    Application_VcardXml -> "application/vcard+xml"
    Application_Vda -> "application/vda"
    Application_Vemmi -> "application/vemmi"
    Application_VividenceScriptfile -> "application/vividence.scriptfile"
    Application_Vnd3gpp2BcmcsinfoXml -> "application/vnd.3gpp2.bcmcsinfo+xml"
    Application_Vnd3gpp2Sms -> "application/vnd.3gpp2.sms"
    Application_Vnd3gpp2Tcap -> "application/vnd.3gpp2.tcap"
    Application_Vnd3gppBsfXml -> "application/vnd.3gpp.bsf+xml"
    Application_Vnd3gppPicBwLarge -> "application/vnd.3gpp.pic-bw-large"
    Application_Vnd3gppPicBwSmall -> "application/vnd.3gpp.pic-bw-small"
    Application_Vnd3gppPicBwVar -> "application/vnd.3gpp.pic-bw-var"
    Application_Vnd3gppSms -> "application/vnd.3gpp.sms"
    Application_Vnd3mPostItNotes -> "application/vnd.3M.Post-it-Notes"
    Application_VndAccpacSimplyAso -> "application/vnd.accpac.simply.aso"
    Application_VndAccpacSimplyImp -> "application/vnd.accpac.simply.imp"
    Application_VndAcucobol -> "application/vnd.acucobol"
    Application_VndAcucorp -> "application/vnd.acucorp"
    Application_VndAdobeAirApplicationInstallerPackageZip -> "application/vnd.adobe.air-application-installer-package+zip"
    Application_VndAdobeFormscentralFcdt -> "application/vnd.adobe.formscentral.fcdt"
    Application_VndAdobeFxp -> "application/vnd.adobe.fxp"
    Application_VndAdobePartialUpload -> "application/vnd.adobe.partial-upload"
    Application_VndAdobeXdpXml -> "application/vnd.adobe.xdp+xml"
    Application_VndAdobeXfdf -> "application/vnd.adobe.xfdf"
    Application_VndAetherImp -> "application/vnd.aether.imp"
    Application_VndAhBarcode -> "application/vnd.ah-barcode"
    Application_VndAheadSpace -> "application/vnd.ahead.space"
    Application_VndAirzipFilesecureAzf -> "application/vnd.airzip.filesecure.azf"
    Application_VndAirzipFilesecureAzs -> "application/vnd.airzip.filesecure.azs"
    Application_VndAmazonEbook -> "application/vnd.amazon.ebook"
    Application_VndAmericandynamicsAcc -> "application/vnd.americandynamics.acc"
    Application_VndAmigaAmi -> "application/vnd.amiga.ami"
    Application_VndAmundsenMazeXml -> "application/vnd.amundsen.maze+xml"
    Application_VndAndroidPackageArchive -> "application/vnd.android.package-archive"
    Application_VndAnserWebCertificateIssueInitiation -> "application/vnd.anser-web-certificate-issue-initiation"
    Application_VndAnserWebFundsTransferInitiation -> "application/vnd.anser-web-funds-transfer-initiation"
    Application_VndAntixGameComponent -> "application/vnd.antix.game-component"
    Application_VndAppleInstallerXml -> "application/vnd.apple.installer+xml"
    Application_VndAppleMpegurl -> "application/vnd.apple.mpegurl"
    Application_VndArastraSwi -> "application/vnd.arastra.swi"
    Application_VndAristanetworksSwi -> "application/vnd.aristanetworks.swi"
    Application_VndAstraeaSoftwareIota -> "application/vnd.astraea-software.iota"
    Application_VndAudiograph -> "application/vnd.audiograph"
    Application_VndAutopackage -> "application/vnd.autopackage"
    Application_VndAvistarXml -> "application/vnd.avistar+xml"
    Application_VndBlueiceMultipass -> "application/vnd.blueice.multipass"
    Application_VndBluetoothEpOob -> "application/vnd.bluetooth.ep.oob"
    Application_VndBmi -> "application/vnd.bmi"
    Application_VndBusinessobjects -> "application/vnd.businessobjects"
    Application_VndCabJscript -> "application/vnd.cab-jscript"
    Application_VndCanonCpdl -> "application/vnd.canon-cpdl"
    Application_VndCanonLips -> "application/vnd.canon-lips"
    Application_VndCendioThinlincClientconf -> "application/vnd.cendio.thinlinc.clientconf"
    Application_VndChemdrawXml -> "application/vnd.chemdraw+xml"
    Application_VndChipnutsKaraokeMmd -> "application/vnd.chipnuts.karaoke-mmd"
    Application_VndCinderella -> "application/vnd.cinderella"
    Application_VndCirpackIsdnExt -> "application/vnd.cirpack.isdn-ext"
    Application_VndClaymore -> "application/vnd.claymore"
    Application_VndCloantoRp9 -> "application/vnd.cloanto.rp9"
    Application_VndClonkC4group -> "application/vnd.clonk.c4group"
    Application_VndCluetrustCartomobileConfig -> "application/vnd.cluetrust.cartomobile-config"
    Application_VndCluetrustCartomobileConfigPkg -> "application/vnd.cluetrust.cartomobile-config-pkg"
    Application_VndCollectionJson -> "application/vnd.collection+json"
    Application_VndCommerceBattelle -> "application/vnd.commerce-battelle"
    Application_VndCommonspace -> "application/vnd.commonspace"
    Application_VndComsocaller -> "application/vnd.comsocaller"
    Application_VndContactCmsg -> "application/vnd.contact.cmsg"
    Application_VndCosmocaller -> "application/vnd.cosmocaller"
    Application_VndCrickClicker -> "application/vnd.crick.clicker"
    Application_VndCrickClickerKeyboard -> "application/vnd.crick.clicker.keyboard"
    Application_VndCrickClickerPalette -> "application/vnd.crick.clicker.palette"
    Application_VndCrickClickerTemplate -> "application/vnd.crick.clicker.template"
    Application_VndCrickClickerWordbank -> "application/vnd.crick.clicker.wordbank"
    Application_VndCriticaltoolsWbsXml -> "application/vnd.criticaltools.wbs+xml"
    Application_VndCtcPosml -> "application/vnd.ctc-posml"
    Application_VndCtctWsXml -> "application/vnd.ctct.ws+xml"
    Application_VndCupsPdf -> "application/vnd.cups-pdf"
    Application_VndCupsPostscript -> "application/vnd.cups-postscript"
    Application_VndCupsPpd -> "application/vnd.cups-ppd"
    Application_VndCupsRaster -> "application/vnd.cups-raster"
    Application_VndCupsRaw -> "application/vnd.cups-raw"
    Application_VndCurl -> "application/vnd.curl"
    Application_VndCurlCar -> "application/vnd.curl.car"
    Application_VndCurlPcurl -> "application/vnd.curl.pcurl"
    Application_VndCybank -> "application/vnd.cybank"
    Application_VndDart -> "application/vnd.dart"
    Application_VndDataVisionRdz -> "application/vnd.data-vision.rdz"
    Application_VndDeceData -> "application/vnd.dece.data"
    Application_VndDeceTtmlXml -> "application/vnd.dece.ttml+xml"
    Application_VndDeceUnspecified -> "application/vnd.dece.unspecified"
    Application_VndDeceZip -> "application/vnd.dece.zip"
    Application_VndDenovoFcselayoutLink -> "application/vnd.denovo.fcselayout-link"
    Application_VndDirBiPlateDlNosuffix -> "application/vnd.dir-bi.plate-dl-nosuffix"
    Application_VndDna -> "application/vnd.dna"
    Application_VndDolbyMlp -> "application/vnd.dolby.mlp"
    Application_VndDolbyMobile1 -> "application/vnd.dolby.mobile.1"
    Application_VndDolbyMobile2 -> "application/vnd.dolby.mobile.2"
    Application_VndDpgraph -> "application/vnd.dpgraph"
    Application_VndDreamfactory -> "application/vnd.dreamfactory"
    Application_VndDsKeypoint -> "application/vnd.ds-keypoint"
    Application_VndDvbAit -> "application/vnd.dvb.ait"
    Application_VndDvbDvbj -> "application/vnd.dvb.dvbj"
    Application_VndDvbEsgcontainer -> "application/vnd.dvb.esgcontainer"
    Application_VndDvbIpdcdftnotifaccess -> "application/vnd.dvb.ipdcdftnotifaccess"
    Application_VndDvbIpdcesgaccess -> "application/vnd.dvb.ipdcesgaccess"
    Application_VndDvbIpdcesgaccess2 -> "application/vnd.dvb.ipdcesgaccess2"
    Application_VndDvbIpdcesgpdd -> "application/vnd.dvb.ipdcesgpdd"
    Application_VndDvbIpdcroaming -> "application/vnd.dvb.ipdcroaming"
    Application_VndDvbIptvAlfecBase -> "application/vnd.dvb.iptv.alfec-base"
    Application_VndDvbIptvAlfecEnhancement -> "application/vnd.dvb.iptv.alfec-enhancement"
    Application_VndDvbNotifAggregateRootXml -> "application/vnd.dvb.notif-aggregate-root+xml"
    Application_VndDvbNotifContainerXml -> "application/vnd.dvb.notif-container+xml"
    Application_VndDvbNotifGenericXml -> "application/vnd.dvb.notif-generic+xml"
    Application_VndDvbNotifIaMsglistXml -> "application/vnd.dvb.notif-ia-msglist+xml"
    Application_VndDvbNotifIaRegistrationRequestXml -> "application/vnd.dvb.notif-ia-registration-request+xml"
    Application_VndDvbNotifIaRegistrationResponseXml -> "application/vnd.dvb.notif-ia-registration-response+xml"
    Application_VndDvbNotifInitXml -> "application/vnd.dvb.notif-init+xml"
    Application_VndDvbPfr -> "application/vnd.dvb.pfr"
    Application_VndDvbService -> "application/vnd.dvb.service"
    Application_VndDxr -> "application/vnd.dxr"
    Application_VndDynageo -> "application/vnd.dynageo"
    Application_VndEasykaraokeCdgdownload -> "application/vnd.easykaraoke.cdgdownload"
    Application_VndEcdisUpdate -> "application/vnd.ecdis-update"
    Application_VndEcowinChart -> "application/vnd.ecowin.chart"
    Application_VndEcowinFilerequest -> "application/vnd.ecowin.filerequest"
    Application_VndEcowinFileupdate -> "application/vnd.ecowin.fileupdate"
    Application_VndEcowinSeries -> "application/vnd.ecowin.series"
    Application_VndEcowinSeriesrequest -> "application/vnd.ecowin.seriesrequest"
    Application_VndEcowinSeriesupdate -> "application/vnd.ecowin.seriesupdate"
    Application_VndEmclientAccessrequestXml -> "application/vnd.emclient.accessrequest+xml"
    Application_VndEnliven -> "application/vnd.enliven"
    Application_VndEprintsDataXml -> "application/vnd.eprints.data+xml"
    Application_VndEpsonEsf -> "application/vnd.epson.esf"
    Application_VndEpsonMsf -> "application/vnd.epson.msf"
    Application_VndEpsonQuickanime -> "application/vnd.epson.quickanime"
    Application_VndEpsonSalt -> "application/vnd.epson.salt"
    Application_VndEpsonSsf -> "application/vnd.epson.ssf"
    Application_VndEricssonQuickcall -> "application/vnd.ericsson.quickcall"
    Application_VndEszigno3Xml -> "application/vnd.eszigno3+xml"
    Application_VndEtsiAocXml -> "application/vnd.etsi.aoc+xml"
    Application_VndEtsiCugXml -> "application/vnd.etsi.cug+xml"
    Application_VndEtsiIptvcommandXml -> "application/vnd.etsi.iptvcommand+xml"
    Application_VndEtsiIptvdiscoveryXml -> "application/vnd.etsi.iptvdiscovery+xml"
    Application_VndEtsiIptvprofileXml -> "application/vnd.etsi.iptvprofile+xml"
    Application_VndEtsiIptvsadBcXml -> "application/vnd.etsi.iptvsad-bc+xml"
    Application_VndEtsiIptvsadCodXml -> "application/vnd.etsi.iptvsad-cod+xml"
    Application_VndEtsiIptvsadNpvrXml -> "application/vnd.etsi.iptvsad-npvr+xml"
    Application_VndEtsiIptvserviceXml -> "application/vnd.etsi.iptvservice+xml"
    Application_VndEtsiIptvsyncXml -> "application/vnd.etsi.iptvsync+xml"
    Application_VndEtsiIptvueprofileXml -> "application/vnd.etsi.iptvueprofile+xml"
    Application_VndEtsiMcidXml -> "application/vnd.etsi.mcid+xml"
    Application_VndEtsiOverloadControlPolicyDatasetXml -> "application/vnd.etsi.overload-control-policy-dataset+xml"
    Application_VndEtsiSciXml -> "application/vnd.etsi.sci+xml"
    Application_VndEtsiSimservsXml -> "application/vnd.etsi.simservs+xml"
    Application_VndEtsiTslDer -> "application/vnd.etsi.tsl.der"
    Application_VndEtsiTslXml -> "application/vnd.etsi.tsl+xml"
    Application_VndEudoraData -> "application/vnd.eudora.data"
    Application_VndEzpixAlbum -> "application/vnd.ezpix-album"
    Application_VndEzpixPackage -> "application/vnd.ezpix-package"
    Application_VndFSecureMobile -> "application/vnd.f-secure.mobile"
    Application_VndFdf -> "application/vnd.fdf"
    Application_VndFdsnMseed -> "application/vnd.fdsn.mseed"
    Application_VndFdsnSeed -> "application/vnd.fdsn.seed"
    Application_VndFfsns -> "application/vnd.ffsns"
    Application_VndFints -> "application/vnd.fints"
    Application_VndFlographit -> "application/vnd.flographit"
    Application_VndFluxtimeClip -> "application/vnd.fluxtime.clip"
    Application_VndFontFontforgeSfd -> "application/vnd.font-fontforge-sfd"
    Application_VndFramemaker -> "application/vnd.framemaker"
    Application_VndFrogansFnc -> "application/vnd.frogans.fnc"
    Application_VndFrogansLtf -> "application/vnd.frogans.ltf"
    Application_VndFscWeblaunch -> "application/vnd.fsc.weblaunch"
    Application_VndFujitsuOasys -> "application/vnd.fujitsu.oasys"
    Application_VndFujitsuOasys2 -> "application/vnd.fujitsu.oasys2"
    Application_VndFujitsuOasys3 -> "application/vnd.fujitsu.oasys3"
    Application_VndFujitsuOasysgp -> "application/vnd.fujitsu.oasysgp"
    Application_VndFujitsuOasysprs -> "application/vnd.fujitsu.oasysprs"
    Application_VndFujixeroxArt4 -> "application/vnd.fujixerox.art4"
    Application_VndFujixeroxArtEx -> "application/vnd.fujixerox.art-ex"
    Application_VndFujixeroxDdd -> "application/vnd.fujixerox.ddd"
    Application_VndFujixeroxDocuworks -> "application/vnd.fujixerox.docuworks"
    Application_VndFujixeroxDocuworksBinder -> "application/vnd.fujixerox.docuworks.binder"
    Application_VndFujixeroxHbpl -> "application/vnd.fujixerox.hbpl"
    Application_VndFutMisnet -> "application/vnd.fut-misnet"
    Application_VndFuzzysheet -> "application/vnd.fuzzysheet"
    Application_VndGenomatixTuxedo -> "application/vnd.genomatix.tuxedo"
    Application_VndGeocubeXml -> "application/vnd.geocube+xml"
    Application_VndGeogebraFile -> "application/vnd.geogebra.file"
    Application_VndGeogebraTool -> "application/vnd.geogebra.tool"
    Application_VndGeometryExplorer -> "application/vnd.geometry-explorer"
    Application_VndGeonext -> "application/vnd.geonext"
    Application_VndGeoplan -> "application/vnd.geoplan"
    Application_VndGeospace -> "application/vnd.geospace"
    Application_VndGlobalplatformCardContentMgt -> "application/vnd.globalplatform.card-content-mgt"
    Application_VndGlobalplatformCardContentMgtResponse -> "application/vnd.globalplatform.card-content-mgt-response"
    Application_VndGmx -> "application/vnd.gmx"
    Application_VndGoogleEarthKmlXml -> "application/vnd.google-earth.kml+xml"
    Application_VndGoogleEarthKmz -> "application/vnd.google-earth.kmz"
    Application_VndGrafeq -> "application/vnd.grafeq"
    Application_VndGridmp -> "application/vnd.gridmp"
    Application_VndGrooveAccount -> "application/vnd.groove-account"
    Application_VndGrooveHelp -> "application/vnd.groove-help"
    Application_VndGrooveIdentityMessage -> "application/vnd.groove-identity-message"
    Application_VndGrooveInjector -> "application/vnd.groove-injector"
    Application_VndGrooveToolMessage -> "application/vnd.groove-tool-message"
    Application_VndGrooveToolTemplate -> "application/vnd.groove-tool-template"
    Application_VndGrooveVcard -> "application/vnd.groove-vcard"
    Application_VndHalJson -> "application/vnd.hal+json"
    Application_VndHalXml -> "application/vnd.hal+xml"
    Application_VndHandheldEntertainmentXml -> "application/vnd.handheld-entertainment+xml"
    Application_VndHbci -> "application/vnd.hbci"
    Application_VndHclBireports -> "application/vnd.hcl-bireports"
    Application_VndHheLessonPlayer -> "application/vnd.hhe.lesson-player"
    Application_VndHpHpgl -> "application/vnd.hp-HPGL"
    Application_VndHpHpid -> "application/vnd.hp-hpid"
    Application_VndHpHps -> "application/vnd.hp-hps"
    Application_VndHpJlyt -> "application/vnd.hp-jlyt"
    Application_VndHpPcl -> "application/vnd.hp-PCL"
    Application_VndHpPclxl -> "application/vnd.hp-PCLXL"
    Application_VndHttphone -> "application/vnd.httphone"
    Application_VndHydrostatixSofData -> "application/vnd.hydrostatix.sof-data"
    Application_VndHzn3dCrossword -> "application/vnd.hzn-3d-crossword"
    Application_VndIbmAfplinedata -> "application/vnd.ibm.afplinedata"
    Application_VndIbmElectronicMedia -> "application/vnd.ibm.electronic-media"
    Application_VndIbmMinipay -> "application/vnd.ibm.MiniPay"
    Application_VndIbmModcap -> "application/vnd.ibm.modcap"
    Application_VndIbmRightsManagement -> "application/vnd.ibm.rights-management"
    Application_VndIbmSecureContainer -> "application/vnd.ibm.secure-container"
    Application_VndIccprofile -> "application/vnd.iccprofile"
    Application_VndIgloader -> "application/vnd.igloader"
    Application_VndImmervisionIvp -> "application/vnd.immervision-ivp"
    Application_VndImmervisionIvu -> "application/vnd.immervision-ivu"
    Application_VndInformedcontrolRmsXml -> "application/vnd.informedcontrol.rms+xml"
    Application_VndInformixVisionary -> "application/vnd.informix-visionary"
    Application_VndInfotechProject -> "application/vnd.infotech.project"
    Application_VndInfotechProjectXml -> "application/vnd.infotech.project+xml"
    Application_VndInnopathWampNotification -> "application/vnd.innopath.wamp.notification"
    Application_VndInsorsIgm -> "application/vnd.insors.igm"
    Application_VndInterconFormnet -> "application/vnd.intercon.formnet"
    Application_VndIntergeo -> "application/vnd.intergeo"
    Application_VndIntertrustDigibox -> "application/vnd.intertrust.digibox"
    Application_VndIntertrustNncp -> "application/vnd.intertrust.nncp"
    Application_VndIntuQbo -> "application/vnd.intu.qbo"
    Application_VndIntuQfx -> "application/vnd.intu.qfx"
    Application_VndIptcG2ConceptitemXml -> "application/vnd.iptc.g2.conceptitem+xml"
    Application_VndIptcG2KnowledgeitemXml -> "application/vnd.iptc.g2.knowledgeitem+xml"
    Application_VndIptcG2NewsitemXml -> "application/vnd.iptc.g2.newsitem+xml"
    Application_VndIptcG2NewsmessageXml -> "application/vnd.iptc.g2.newsmessage+xml"
    Application_VndIptcG2PackageitemXml -> "application/vnd.iptc.g2.packageitem+xml"
    Application_VndIptcG2PlanningitemXml -> "application/vnd.iptc.g2.planningitem+xml"
    Application_VndIpunpluggedRcprofile -> "application/vnd.ipunplugged.rcprofile"
    Application_VndIrepositoryPackageXml -> "application/vnd.irepository.package+xml"
    Application_VndIsXpr -> "application/vnd.is-xpr"
    Application_VndIsacFcs -> "application/vnd.isac.fcs"
    Application_VndJam -> "application/vnd.jam"
    Application_VndJapannetDirectoryService -> "application/vnd.japannet-directory-service"
    Application_VndJapannetJpnstoreWakeup -> "application/vnd.japannet-jpnstore-wakeup"
    Application_VndJapannetPaymentWakeup -> "application/vnd.japannet-payment-wakeup"
    Application_VndJapannetRegistration -> "application/vnd.japannet-registration"
    Application_VndJapannetRegistrationWakeup -> "application/vnd.japannet-registration-wakeup"
    Application_VndJapannetSetstoreWakeup -> "application/vnd.japannet-setstore-wakeup"
    Application_VndJapannetVerification -> "application/vnd.japannet-verification"
    Application_VndJapannetVerificationWakeup -> "application/vnd.japannet-verification-wakeup"
    Application_VndJcpJavameMidletRms -> "application/vnd.jcp.javame.midlet-rms"
    Application_VndJisp -> "application/vnd.jisp"
    Application_VndJoostJodaArchive -> "application/vnd.joost.joda-archive"
    Application_VndKahootz -> "application/vnd.kahootz"
    Application_VndKdeKarbon -> "application/vnd.kde.karbon"
    Application_VndKdeKchart -> "application/vnd.kde.kchart"
    Application_VndKdeKformula -> "application/vnd.kde.kformula"
    Application_VndKdeKivio -> "application/vnd.kde.kivio"
    Application_VndKdeKontour -> "application/vnd.kde.kontour"
    Application_VndKdeKpresenter -> "application/vnd.kde.kpresenter"
    Application_VndKdeKspread -> "application/vnd.kde.kspread"
    Application_VndKdeKword -> "application/vnd.kde.kword"
    Application_VndKenameaapp -> "application/vnd.kenameaapp"
    Application_VndKidspiration -> "application/vnd.kidspiration"
    Application_VndKinar -> "application/vnd.kinar"
    Application_VndKoan -> "application/vnd.koan"
    Application_VndKodakDescriptor -> "application/vnd.kodak-descriptor"
    Application_VndLasLasXml -> "application/vnd.las.las+xml"
    Application_VndLibertyRequestXml -> "application/vnd.liberty-request+xml"
    Application_VndLlamagraphicsLifeBalanceDesktop -> "application/vnd.llamagraphics.life-balance.desktop"
    Application_VndLlamagraphicsLifeBalanceExchangeXml -> "application/vnd.llamagraphics.life-balance.exchange+xml"
    Application_VndLotus123 -> "application/vnd.lotus-1-2-3"
    Application_VndLotusApproach -> "application/vnd.lotus-approach"
    Application_VndLotusFreelance -> "application/vnd.lotus-freelance"
    Application_VndLotusNotes -> "application/vnd.lotus-notes"
    Application_VndLotusOrganizer -> "application/vnd.lotus-organizer"
    Application_VndLotusScreencam -> "application/vnd.lotus-screencam"
    Application_VndLotusWordpro -> "application/vnd.lotus-wordpro"
    Application_VndMacportsPortpkg -> "application/vnd.macports.portpkg"
    Application_VndMarlinDrmActiontokenXml -> "application/vnd.marlin.drm.actiontoken+xml"
    Application_VndMarlinDrmConftokenXml -> "application/vnd.marlin.drm.conftoken+xml"
    Application_VndMarlinDrmLicenseXml -> "application/vnd.marlin.drm.license+xml"
    Application_VndMarlinDrmMdcf -> "application/vnd.marlin.drm.mdcf"
    Application_VndMcd -> "application/vnd.mcd"
    Application_VndMedcalcdata -> "application/vnd.medcalcdata"
    Application_VndMediastationCdkey -> "application/vnd.mediastation.cdkey"
    Application_VndMeridianSlingshot -> "application/vnd.meridian-slingshot"
    Application_VndMfer -> "application/vnd.mfer"
    Application_VndMfmp -> "application/vnd.mfmp"
    Application_VndMicrografxFlo -> "application/vnd.micrografx.flo"
    Application_VndMicrografxIgx -> "application/vnd.micrografx.igx"
    Application_VndMif -> "application/vnd.mif"
    Application_VndMinisoftHp3000Save -> "application/vnd.minisoft-hp3000-save"
    Application_VndMitsubishiMistyGuardTrustweb -> "application/vnd.mitsubishi.misty-guard.trustweb"
    Application_VndMobiusDaf -> "application/vnd.mobius.daf"
    Application_VndMobiusDis -> "application/vnd.mobius.dis"
    Application_VndMobiusMbk -> "application/vnd.mobius.mbk"
    Application_VndMobiusMqy -> "application/vnd.mobius.mqy"
    Application_VndMobiusMsl -> "application/vnd.mobius.msl"
    Application_VndMobiusPlc -> "application/vnd.mobius.plc"
    Application_VndMobiusTxf -> "application/vnd.mobius.txf"
    Application_VndMophunApplication -> "application/vnd.mophun.application"
    Application_VndMophunCertificate -> "application/vnd.mophun.certificate"
    Application_VndMotorolaFlexsuite -> "application/vnd.motorola.flexsuite"
    Application_VndMotorolaFlexsuiteAdsi -> "application/vnd.motorola.flexsuite.adsi"
    Application_VndMotorolaFlexsuiteFis -> "application/vnd.motorola.flexsuite.fis"
    Application_VndMotorolaFlexsuiteGotap -> "application/vnd.motorola.flexsuite.gotap"
    Application_VndMotorolaFlexsuiteKmr -> "application/vnd.motorola.flexsuite.kmr"
    Application_VndMotorolaFlexsuiteTtc -> "application/vnd.motorola.flexsuite.ttc"
    Application_VndMotorolaFlexsuiteWem -> "application/vnd.motorola.flexsuite.wem"
    Application_VndMotorolaIprm -> "application/vnd.motorola.iprm"
    Application_VndMozillaXulXml -> "application/vnd.mozilla.xul+xml"
    Application_VndMsArtgalry -> "application/vnd.ms-artgalry"
    Application_VndMsAsf -> "application/vnd.ms-asf"
    Application_VndMsCabCompressed -> "application/vnd.ms-cab-compressed"
    Application_VndMsColorIccprofile -> "application/vnd.ms-color.iccprofile"
    Application_VndMsExcel -> "application/vnd.ms-excel"
    Application_VndMsExcelAddinMacroenabled12 -> "application/vnd.ms-excel.addin.macroEnabled.12"
    Application_VndMsExcelSheetBinaryMacroenabled12 -> "application/vnd.ms-excel.sheet.binary.macroEnabled.12"
    Application_VndMsExcelSheetMacroenabled12 -> "application/vnd.ms-excel.sheet.macroEnabled.12"
    Application_VndMsExcelTemplateMacroenabled12 -> "application/vnd.ms-excel.template.macroEnabled.12"
    Application_VndMsFontobject -> "application/vnd.ms-fontobject"
    Application_VndMsHtmlhelp -> "application/vnd.ms-htmlhelp"
    Application_VndMsIms -> "application/vnd.ms-ims"
    Application_VndMsLrm -> "application/vnd.ms-lrm"
    Application_VndMsOfficeActivexXml -> "application/vnd.ms-office.activex+xml"
    Application_VndMsOfficetheme -> "application/vnd.ms-officetheme"
    Application_VndMsOpentype -> "application/vnd.ms-opentype"
    Application_VndMsOutlook -> "application/vnd.ms-outlook"
    Application_VndMsPackageObfuscatedOpentype -> "application/vnd.ms-package.obfuscated-opentype"
    Application_VndMsPkiCertstore -> "application/vnd.ms-pki.certstore"
    Application_VndMsPkiPko -> "application/vnd.ms-pki.pko"
    Application_VndMsPkiSeccat -> "application/vnd.ms-pki.seccat"
    Application_VndMsPkiStl -> "application/vnd.ms-pki.stl"
    Application_VndMsPkicertstore -> "application/vnd.ms-pkicertstore"
    Application_VndMsPkiseccat -> "application/vnd.ms-pkiseccat"
    Application_VndMsPkistl -> "application/vnd.ms-pkistl"
    Application_VndMsPlayreadyInitiatorXml -> "application/vnd.ms-playready.initiator+xml"
    Application_VndMsPowerpoint -> "application/vnd.ms-powerpoint"
    Application_VndMsPowerpointAddinMacroenabled12 -> "application/vnd.ms-powerpoint.addin.macroEnabled.12"
    Application_VndMsPowerpointPresentationMacroenabled12 -> "application/vnd.ms-powerpoint.presentation.macroEnabled.12"
    Application_VndMsPowerpointSlideMacroenabled12 -> "application/vnd.ms-powerpoint.slide.macroEnabled.12"
    Application_VndMsPowerpointSlideshowMacroenabled12 -> "application/vnd.ms-powerpoint.slideshow.macroEnabled.12"
    Application_VndMsPowerpointTemplateMacroenabled12 -> "application/vnd.ms-powerpoint.template.macroEnabled.12"
    Application_VndMsPrintingPrintticketXml -> "application/vnd.ms-printing.printticket+xml"
    Application_VndMsProject -> "application/vnd.ms-project"
    Application_VndMsTnef -> "application/vnd.ms-tnef"
    Application_VndMsWmdrmLicChlgReq -> "application/vnd.ms-wmdrm.lic-chlg-req"
    Application_VndMsWmdrmLicResp -> "application/vnd.ms-wmdrm.lic-resp"
    Application_VndMsWmdrmMeterChlgReq -> "application/vnd.ms-wmdrm.meter-chlg-req"
    Application_VndMsWmdrmMeterResp -> "application/vnd.ms-wmdrm.meter-resp"
    Application_VndMsWordDocumentMacroenabled12 -> "application/vnd.ms-word.document.macroEnabled.12"
    Application_VndMsWordTemplateMacroenabled12 -> "application/vnd.ms-word.template.macroEnabled.12"
    Application_VndMsWorks -> "application/vnd.ms-works"
    Application_VndMsWpl -> "application/vnd.ms-wpl"
    Application_VndMsXpsdocument -> "application/vnd.ms-xpsdocument"
    Application_VndMseq -> "application/vnd.mseq"
    Application_VndMsign -> "application/vnd.msign"
    Application_VndMultiadCreator -> "application/vnd.multiad.creator"
    Application_VndMultiadCreatorCif -> "application/vnd.multiad.creator.cif"
    Application_VndMusicNiff -> "application/vnd.music-niff"
    Application_VndMusician -> "application/vnd.musician"
    Application_VndMuveeStyle -> "application/vnd.muvee.style"
    Application_VndMynfc -> "application/vnd.mynfc"
    Application_VndNcdControl -> "application/vnd.ncd.control"
    Application_VndNcdReference -> "application/vnd.ncd.reference"
    Application_VndNervana -> "application/vnd.nervana"
    Application_VndNetfpx -> "application/vnd.netfpx"
    Application_VndNeurolanguageNlu -> "application/vnd.neurolanguage.nlu"
    Application_VndNitf -> "application/vnd.nitf"
    Application_VndNoblenetDirectory -> "application/vnd.noblenet-directory"
    Application_VndNoblenetSealer -> "application/vnd.noblenet-sealer"
    Application_VndNoblenetWeb -> "application/vnd.noblenet-web"
    Application_VndNokiaCatalogs -> "application/vnd.nokia.catalogs"
    Application_VndNokiaConfigurationMessage -> "application/vnd.nokia.configuration-message"
    Application_VndNokiaConmlWbxml -> "application/vnd.nokia.conml+wbxml"
    Application_VndNokiaConmlXml -> "application/vnd.nokia.conml+xml"
    Application_VndNokiaIptvConfigXml -> "application/vnd.nokia.iptv.config+xml"
    Application_VndNokiaIsdsRadioPresets -> "application/vnd.nokia.isds-radio-presets"
    Application_VndNokiaLandmarkWbxml -> "application/vnd.nokia.landmark+wbxml"
    Application_VndNokiaLandmarkXml -> "application/vnd.nokia.landmark+xml"
    Application_VndNokiaLandmarkcollectionXml -> "application/vnd.nokia.landmarkcollection+xml"
    Application_VndNokiaNGageAcXml -> "application/vnd.nokia.n-gage.ac+xml"
    Application_VndNokiaNGageData -> "application/vnd.nokia.n-gage.data"
    Application_VndNokiaNGageSymbianInstall -> "application/vnd.nokia.n-gage.symbian.install"
    Application_VndNokiaNcd -> "application/vnd.nokia.ncd"
    Application_VndNokiaPcdWbxml -> "application/vnd.nokia.pcd+wbxml"
    Application_VndNokiaPcdXml -> "application/vnd.nokia.pcd+xml"
    Application_VndNokiaRadioPreset -> "application/vnd.nokia.radio-preset"
    Application_VndNokiaRadioPresets -> "application/vnd.nokia.radio-presets"
    Application_VndNokiaRingingTone -> "application/vnd.nokia.ringing-tone"
    Application_VndNovadigmEdm -> "application/vnd.novadigm.EDM"
    Application_VndNovadigmEdx -> "application/vnd.novadigm.EDX"
    Application_VndNovadigmExt -> "application/vnd.novadigm.EXT"
    Application_VndNttLocalFileTransfer -> "application/vnd.ntt-local.file-transfer"
    Application_VndNttLocalSipTa_remote -> "application/vnd.ntt-local.sip-ta_remote"
    Application_VndNttLocalSipTa_tcp_stream -> "application/vnd.ntt-local.sip-ta_tcp_stream"
    Application_VndOasisOpendocumentChart -> "application/vnd.oasis.opendocument.chart"
    Application_VndOasisOpendocumentChartTemplate -> "application/vnd.oasis.opendocument.chart-template"
    Application_VndOasisOpendocumentDatabase -> "application/vnd.oasis.opendocument.database"
    Application_VndOasisOpendocumentFormula -> "application/vnd.oasis.opendocument.formula"
    Application_VndOasisOpendocumentFormulaTemplate -> "application/vnd.oasis.opendocument.formula-template"
    Application_VndOasisOpendocumentGraphics -> "application/vnd.oasis.opendocument.graphics"
    Application_VndOasisOpendocumentGraphicsTemplate -> "application/vnd.oasis.opendocument.graphics-template"
    Application_VndOasisOpendocumentImage -> "application/vnd.oasis.opendocument.image"
    Application_VndOasisOpendocumentImageTemplate -> "application/vnd.oasis.opendocument.image-template"
    Application_VndOasisOpendocumentPresentation -> "application/vnd.oasis.opendocument.presentation"
    Application_VndOasisOpendocumentPresentationTemplate -> "application/vnd.oasis.opendocument.presentation-template"
    Application_VndOasisOpendocumentSpreadsheet -> "application/vnd.oasis.opendocument.spreadsheet"
    Application_VndOasisOpendocumentSpreadsheetTemplate -> "application/vnd.oasis.opendocument.spreadsheet-template"
    Application_VndOasisOpendocumentText -> "application/vnd.oasis.opendocument.text"
    Application_VndOasisOpendocumentTextMaster -> "application/vnd.oasis.opendocument.text-master"
    Application_VndOasisOpendocumentTextTemplate -> "application/vnd.oasis.opendocument.text-template"
    Application_VndOasisOpendocumentTextWeb -> "application/vnd.oasis.opendocument.text-web"
    Application_VndObn -> "application/vnd.obn"
    Application_VndOftnL10nJson -> "application/vnd.oftn.l10n+json"
    Application_VndOipfContentaccessdownloadXml -> "application/vnd.oipf.contentaccessdownload+xml"
    Application_VndOipfContentaccessstreamingXml -> "application/vnd.oipf.contentaccessstreaming+xml"
    Application_VndOipfCspgHexbinary -> "application/vnd.oipf.cspg-hexbinary"
    Application_VndOipfDaeSvgXml -> "application/vnd.oipf.dae.svg+xml"
    Application_VndOipfDaeXhtmlXml -> "application/vnd.oipf.dae.xhtml+xml"
    Application_VndOipfMippvcontrolmessageXml -> "application/vnd.oipf.mippvcontrolmessage+xml"
    Application_VndOipfPaeGem -> "application/vnd.oipf.pae.gem"
    Application_VndOipfSpdiscoveryXml -> "application/vnd.oipf.spdiscovery+xml"
    Application_VndOipfSpdlistXml -> "application/vnd.oipf.spdlist+xml"
    Application_VndOipfUeprofileXml -> "application/vnd.oipf.ueprofile+xml"
    Application_VndOipfUserprofileXml -> "application/vnd.oipf.userprofile+xml"
    Application_VndOlpcSugar -> "application/vnd.olpc-sugar"
    Application_VndOmaBcastAssociatedProcedureParameterXml -> "application/vnd.oma.bcast.associated-procedure-parameter+xml"
    Application_VndOmaBcastDrmTriggerXml -> "application/vnd.oma.bcast.drm-trigger+xml"
    Application_VndOmaBcastImdXml -> "application/vnd.oma.bcast.imd+xml"
    Application_VndOmaBcastLtkm -> "application/vnd.oma.bcast.ltkm"
    Application_VndOmaBcastNotificationXml -> "application/vnd.oma.bcast.notification+xml"
    Application_VndOmaBcastProvisioningtrigger -> "application/vnd.oma.bcast.provisioningtrigger"
    Application_VndOmaBcastSgboot -> "application/vnd.oma.bcast.sgboot"
    Application_VndOmaBcastSgddXml -> "application/vnd.oma.bcast.sgdd+xml"
    Application_VndOmaBcastSgdu -> "application/vnd.oma.bcast.sgdu"
    Application_VndOmaBcastSimpleSymbolContainer -> "application/vnd.oma.bcast.simple-symbol-container"
    Application_VndOmaBcastSmartcardTriggerXml -> "application/vnd.oma.bcast.smartcard-trigger+xml"
    Application_VndOmaBcastSprovXml -> "application/vnd.oma.bcast.sprov+xml"
    Application_VndOmaBcastStkm -> "application/vnd.oma.bcast.stkm"
    Application_VndOmaCabAddressBookXml -> "application/vnd.oma.cab-address-book+xml"
    Application_VndOmaCabFeatureHandlerXml -> "application/vnd.oma.cab-feature-handler+xml"
    Application_VndOmaCabPccXml -> "application/vnd.oma.cab-pcc+xml"
    Application_VndOmaCabUserPrefsXml -> "application/vnd.oma.cab-user-prefs+xml"
    Application_VndOmaDcd -> "application/vnd.oma.dcd"
    Application_VndOmaDcdc -> "application/vnd.oma.dcdc"
    Application_VndOmaDd2Xml -> "application/vnd.oma.dd2+xml"
    Application_VndOmaDrmRisdXml -> "application/vnd.oma.drm.risd+xml"
    Application_VndOmaGroupUsageListXml -> "application/vnd.oma.group-usage-list+xml"
    Application_VndOmaPalXml -> "application/vnd.oma.pal+xml"
    Application_VndOmaPocDetailedProgressReportXml -> "application/vnd.oma.poc.detailed-progress-report+xml"
    Application_VndOmaPocFinalReportXml -> "application/vnd.oma.poc.final-report+xml"
    Application_VndOmaPocGroupsXml -> "application/vnd.oma.poc.groups+xml"
    Application_VndOmaPocInvocationDescriptorXml -> "application/vnd.oma.poc.invocation-descriptor+xml"
    Application_VndOmaPocOptimizedProgressReportXml -> "application/vnd.oma.poc.optimized-progress-report+xml"
    Application_VndOmaPush -> "application/vnd.oma.push"
    Application_VndOmaScidmMessagesXml -> "application/vnd.oma.scidm.messages+xml"
    Application_VndOmaScwsConfig -> "application/vnd.oma-scws-config"
    Application_VndOmaScwsHttpRequest -> "application/vnd.oma-scws-http-request"
    Application_VndOmaScwsHttpResponse -> "application/vnd.oma-scws-http-response"
    Application_VndOmaXcapDirectoryXml -> "application/vnd.oma.xcap-directory+xml"
    Application_VndOmadsEmailXml -> "application/vnd.omads-email+xml"
    Application_VndOmadsFileXml -> "application/vnd.omads-file+xml"
    Application_VndOmadsFolderXml -> "application/vnd.omads-folder+xml"
    Application_VndOmalocSuplInit -> "application/vnd.omaloc-supl-init"
    Application_VndOpenofficeorgExtension -> "application/vnd.openofficeorg.extension"
    Application_VndOpenxmlformatsOfficedocumentCustomPropertiesXml -> "application/vnd.openxmlformats-officedocument.custom-properties+xml"
    Application_VndOpenxmlformatsOfficedocumentCustomxmlpropertiesXml -> "application/vnd.openxmlformats-officedocument.customxmlproperties+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingXml -> "application/vnd.openxmlformats-officedocument.drawing+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartXml -> "application/vnd.openxmlformats-officedocument.drawingml.chart+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartshapesXml -> "application/vnd.openxmlformats-officedocument.drawingml.chartshapes+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramcolorsXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramcolors+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramdataXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramdata+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramlayoutXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramlayout+xml"
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramstyleXml -> "application/vnd.openxmlformats-officedocument.drawingml.diagramstyle+xml"
    Application_VndOpenxmlformatsOfficedocumentExtendedPropertiesXml -> "application/vnd.openxmlformats-officedocument.extended-properties+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentauthorsXml -> "application/vnd.openxmlformats-officedocument.presentationml.commentauthors+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentsXml -> "application/vnd.openxmlformats-officedocument.presentationml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlHandoutmasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.handoutmaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesmasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.notesmaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesslideXml -> "application/vnd.openxmlformats-officedocument.presentationml.notesslide+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentation -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentationMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPrespropsXml -> "application/vnd.openxmlformats-officedocument.presentationml.presprops+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlide -> "application/vnd.openxmlformats-officedocument.presentationml.slide"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideXml -> "application/vnd.openxmlformats-officedocument.presentationml.slide+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidelayoutXml -> "application/vnd.openxmlformats-officedocument.presentationml.slidelayout+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidemasterXml -> "application/vnd.openxmlformats-officedocument.presentationml.slidemaster+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshow -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshowMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideupdateinfoXml -> "application/vnd.openxmlformats-officedocument.presentationml.slideupdateinfo+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTablestylesXml -> "application/vnd.openxmlformats-officedocument.presentationml.tablestyles+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTagsXml -> "application/vnd.openxmlformats-officedocument.presentationml.tags+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplate -> "application/vnd.openxmlformats-officedocument.presentationml.template"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.presentationml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentPresentationmlViewpropsXml -> "application/vnd.openxmlformats-officedocument.presentationml.viewprops+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCalcchainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.calcchain+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlChartsheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCommentsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlConnectionsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.connections+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlDialogsheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlExternallinkXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.externallink+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcachedefinitionXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotcachedefinition+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcacherecordsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotcacherecords+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivottableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.pivottable+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlQuerytableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.querytable+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionheadersXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionheaders+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionlogXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionlog+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSharedstringsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedstrings+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheet -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetMainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetmetadataXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheetmetadata+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlStylesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTableXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTablesinglecellsXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.tablesinglecells+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlUsernamesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.usernames+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlVolatiledependenciesXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.volatiledependencies+xml"
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlWorksheetXml -> "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    Application_VndOpenxmlformatsOfficedocumentThemeXml -> "application/vnd.openxmlformats-officedocument.theme+xml"
    Application_VndOpenxmlformatsOfficedocumentThemeoverrideXml -> "application/vnd.openxmlformats-officedocument.themeoverride+xml"
    Application_VndOpenxmlformatsOfficedocumentVmldrawing -> "application/vnd.openxmlformats-officedocument.vmldrawing"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlCommentsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocument -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentGlossaryXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentMainXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlEndnotesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFonttableXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.fonttable+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFooterXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFootnotesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlNumberingXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlSettingsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlStylesXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplate -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplateMainXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml"
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlWebsettingsXml -> "application/vnd.openxmlformats-officedocument.wordprocessingml.websettings+xml"
    Application_VndOpenxmlformatsPackageCorePropertiesXml -> "application/vnd.openxmlformats-package.core-properties+xml"
    Application_VndOpenxmlformatsPackageDigitalSignatureXmlsignatureXml -> "application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml"
    Application_VndOpenxmlformatsPackageRelationshipsXml -> "application/vnd.openxmlformats-package.relationships+xml"
    Application_VndOsaNetdeploy -> "application/vnd.osa.netdeploy"
    Application_VndOsgeoMapguidePackage -> "application/vnd.osgeo.mapguide.package"
    Application_VndOsgiBundle -> "application/vnd.osgi.bundle"
    Application_VndOsgiDp -> "application/vnd.osgi.dp"
    Application_VndOsgiSubsystem -> "application/vnd.osgi.subsystem"
    Application_VndOtpsCtKipXml -> "application/vnd.otps.ct-kip+xml"
    Application_VndPalm -> "application/vnd.palm"
    Application_VndPaosXml -> "application/vnd.paos.xml"
    Application_VndPawaafile -> "application/vnd.pawaafile"
    Application_VndPgFormat -> "application/vnd.pg.format"
    Application_VndPgOsasli -> "application/vnd.pg.osasli"
    Application_VndPiaccessApplicationLicence -> "application/vnd.piaccess.application-licence"
    Application_VndPicsel -> "application/vnd.picsel"
    Application_VndPmiWidget -> "application/vnd.pmi.widget"
    Application_VndPocGroupAdvertisementXml -> "application/vnd.poc.group-advertisement+xml"
    Application_VndPocketlearn -> "application/vnd.pocketlearn"
    Application_VndPowerbuilder6 -> "application/vnd.powerbuilder6"
    Application_VndPowerbuilder6S -> "application/vnd.powerbuilder6-s"
    Application_VndPowerbuilder7 -> "application/vnd.powerbuilder7"
    Application_VndPowerbuilder75 -> "application/vnd.powerbuilder75"
    Application_VndPowerbuilder75S -> "application/vnd.powerbuilder75-s"
    Application_VndPowerbuilder7S -> "application/vnd.powerbuilder7-s"
    Application_VndPreminet -> "application/vnd.preminet"
    Application_VndPreviewsystemsBox -> "application/vnd.previewsystems.box"
    Application_VndProteusMagazine -> "application/vnd.proteus.magazine"
    Application_VndPublishareDeltaTree -> "application/vnd.publishare-delta-tree"
    Application_VndPviPtid1 -> "application/vnd.pvi.ptid1"
    Application_VndPwgMultiplexed -> "application/vnd.pwg-multiplexed"
    Application_VndPwgXhtmlPrintXml -> "application/vnd.pwg-xhtml-print+xml"
    Application_VndQualcommBrewAppRes -> "application/vnd.qualcomm.brew-app-res"
    Application_VndQuarkQuarkxpress -> "application/vnd.quark.quarkxpress"
    Application_VndQuobjectQuoxdocument -> "application/vnd.quobject-quoxdocument"
    Application_VndRadisysMomlXml -> "application/vnd.radisys.moml+xml"
    Application_VndRadisysMsmlAuditConfXml -> "application/vnd.radisys.msml-audit-conf+xml"
    Application_VndRadisysMsmlAuditConnXml -> "application/vnd.radisys.msml-audit-conn+xml"
    Application_VndRadisysMsmlAuditDialogXml -> "application/vnd.radisys.msml-audit-dialog+xml"
    Application_VndRadisysMsmlAuditStreamXml -> "application/vnd.radisys.msml-audit-stream+xml"
    Application_VndRadisysMsmlAuditXml -> "application/vnd.radisys.msml-audit+xml"
    Application_VndRadisysMsmlConfXml -> "application/vnd.radisys.msml-conf+xml"
    Application_VndRadisysMsmlDialogBaseXml -> "application/vnd.radisys.msml-dialog-base+xml"
    Application_VndRadisysMsmlDialogFaxDetectXml -> "application/vnd.radisys.msml-dialog-fax-detect+xml"
    Application_VndRadisysMsmlDialogFaxSendrecvXml -> "application/vnd.radisys.msml-dialog-fax-sendrecv+xml"
    Application_VndRadisysMsmlDialogGroupXml -> "application/vnd.radisys.msml-dialog-group+xml"
    Application_VndRadisysMsmlDialogSpeechXml -> "application/vnd.radisys.msml-dialog-speech+xml"
    Application_VndRadisysMsmlDialogTransformXml -> "application/vnd.radisys.msml-dialog-transform+xml"
    Application_VndRadisysMsmlDialogXml -> "application/vnd.radisys.msml-dialog+xml"
    Application_VndRadisysMsmlXml -> "application/vnd.radisys.msml+xml"
    Application_VndRainstorData -> "application/vnd.rainstor.data"
    Application_VndRapid -> "application/vnd.rapid"
    Application_VndRealvncBed -> "application/vnd.realvnc.bed"
    Application_VndRecordareMusicxml -> "application/vnd.recordare.musicxml"
    Application_VndRecordareMusicxmlXml -> "application/vnd.recordare.musicxml+xml"
    Application_VndRenlearnRlprint -> "application/vnd.renlearn.rlprint"
    Application_VndRigCryptonote -> "application/vnd.rig.cryptonote"
    Application_VndRimCod -> "application/vnd.rim.cod"
    Application_VndRnRealmedia -> "application/vnd.rn-realmedia"
    Application_VndRnRealmediaVbr -> "application/vnd.rn-realmedia-vbr"
    Application_VndRnRealplayer -> "application/vnd.rn-realplayer"
    Application_VndRoute66Link66Xml -> "application/vnd.route66.link66+xml"
    Application_VndRs274x -> "application/vnd.rs-274x"
    Application_VndRuckusDownload -> "application/vnd.ruckus.download"
    Application_VndS3sms -> "application/vnd.s3sms"
    Application_VndSailingtrackerTrack -> "application/vnd.sailingtracker.track"
    Application_VndSbmCid -> "application/vnd.sbm.cid"
    Application_VndSbmMid2 -> "application/vnd.sbm.mid2"
    Application_VndScribus -> "application/vnd.scribus"
    Application_VndSealed3df -> "application/vnd.sealed.3df"
    Application_VndSealedCsf -> "application/vnd.sealed.csf"
    Application_VndSealedDoc -> "application/vnd.sealed.doc"
    Application_VndSealedEml -> "application/vnd.sealed.eml"
    Application_VndSealedMht -> "application/vnd.sealed.mht"
    Application_VndSealedNet -> "application/vnd.sealed.net"
    Application_VndSealedPpt -> "application/vnd.sealed.ppt"
    Application_VndSealedTiff -> "application/vnd.sealed.tiff"
    Application_VndSealedXls -> "application/vnd.sealed.xls"
    Application_VndSealedmediaSoftsealHtml -> "application/vnd.sealedmedia.softseal.html"
    Application_VndSealedmediaSoftsealPdf -> "application/vnd.sealedmedia.softseal.pdf"
    Application_VndSeemail -> "application/vnd.seemail"
    Application_VndSema -> "application/vnd.sema"
    Application_VndSemd -> "application/vnd.semd"
    Application_VndSemf -> "application/vnd.semf"
    Application_VndShanaInformedFormdata -> "application/vnd.shana.informed.formdata"
    Application_VndShanaInformedFormtemplate -> "application/vnd.shana.informed.formtemplate"
    Application_VndShanaInformedInterchange -> "application/vnd.shana.informed.interchange"
    Application_VndShanaInformedPackage -> "application/vnd.shana.informed.package"
    Application_VndSimtechMindmapper -> "application/vnd.simtech-mindmapper"
    Application_VndSmaf -> "application/vnd.smaf"
    Application_VndSmartNotebook -> "application/vnd.smart.notebook"
    Application_VndSmartTeacher -> "application/vnd.smart.teacher"
    Application_VndSoftware602FillerFormXml -> "application/vnd.software602.filler.form+xml"
    Application_VndSoftware602FillerFormXmlZip -> "application/vnd.software602.filler.form-xml-zip"
    Application_VndSolentSdkmXml -> "application/vnd.solent.sdkm+xml"
    Application_VndSpotfireDxp -> "application/vnd.spotfire.dxp"
    Application_VndSpotfireSfs -> "application/vnd.spotfire.sfs"
    Application_VndSssCod -> "application/vnd.sss-cod"
    Application_VndSssDtf -> "application/vnd.sss-dtf"
    Application_VndSssNtf -> "application/vnd.sss-ntf"
    Application_VndStardivisionCalc -> "application/vnd.stardivision.calc"
    Application_VndStardivisionDraw -> "application/vnd.stardivision.draw"
    Application_VndStardivisionImpress -> "application/vnd.stardivision.impress"
    Application_VndStardivisionMath -> "application/vnd.stardivision.math"
    Application_VndStardivisionWriter -> "application/vnd.stardivision.writer"
    Application_VndStardivisionWriterGlobal -> "application/vnd.stardivision.writer-global"
    Application_VndStepmaniaPackage -> "application/vnd.stepmania.package"
    Application_VndStepmaniaStepchart -> "application/vnd.stepmania.stepchart"
    Application_VndStreetStream -> "application/vnd.street-stream"
    Application_VndSunWadlXml -> "application/vnd.sun.wadl+xml"
    Application_VndSunXmlCalc -> "application/vnd.sun.xml.calc"
    Application_VndSunXmlCalcTemplate -> "application/vnd.sun.xml.calc.template"
    Application_VndSunXmlDraw -> "application/vnd.sun.xml.draw"
    Application_VndSunXmlDrawTemplate -> "application/vnd.sun.xml.draw.template"
    Application_VndSunXmlImpress -> "application/vnd.sun.xml.impress"
    Application_VndSunXmlImpressTemplate -> "application/vnd.sun.xml.impress.template"
    Application_VndSunXmlMath -> "application/vnd.sun.xml.math"
    Application_VndSunXmlWriter -> "application/vnd.sun.xml.writer"
    Application_VndSunXmlWriterGlobal -> "application/vnd.sun.xml.writer.global"
    Application_VndSunXmlWriterTemplate -> "application/vnd.sun.xml.writer.template"
    Application_VndSusCalendar -> "application/vnd.sus-calendar"
    Application_VndSvd -> "application/vnd.svd"
    Application_VndSwiftviewIcs -> "application/vnd.swiftview-ics"
    Application_VndSymbianInstall -> "application/vnd.symbian.install"
    Application_VndSyncmlDmNotification -> "application/vnd.syncml.dm.notification"
    Application_VndSyncmlDmWbxml -> "application/vnd.syncml.dm+wbxml"
    Application_VndSyncmlDmXml -> "application/vnd.syncml.dm+xml"
    Application_VndSyncmlDsNotification -> "application/vnd.syncml.ds.notification"
    Application_VndSyncmlXml -> "application/vnd.syncml+xml"
    Application_VndTaoIntentModuleArchive -> "application/vnd.tao.intent-module-archive"
    Application_VndTcpdumpPcap -> "application/vnd.tcpdump.pcap"
    Application_VndTmobileLivetv -> "application/vnd.tmobile-livetv"
    Application_VndTridTpt -> "application/vnd.trid.tpt"
    Application_VndTriscapeMxs -> "application/vnd.triscape.mxs"
    Application_VndTrueapp -> "application/vnd.trueapp"
    Application_VndTruedoc -> "application/vnd.truedoc"
    Application_VndTveTrigger -> "application/vnd.tve-trigger"
    Application_VndUbisoftWebplayer -> "application/vnd.ubisoft.webplayer"
    Application_VndUfdl -> "application/vnd.ufdl"
    Application_VndUiqTheme -> "application/vnd.uiq.theme"
    Application_VndUmajin -> "application/vnd.umajin"
    Application_VndUnity -> "application/vnd.unity"
    Application_VndUomlXml -> "application/vnd.uoml+xml"
    Application_VndUplanetAlert -> "application/vnd.uplanet.alert"
    Application_VndUplanetAlertWbxml -> "application/vnd.uplanet.alert-wbxml"
    Application_VndUplanetBearerChoice -> "application/vnd.uplanet.bearer-choice"
    Application_VndUplanetBearerChoiceWbxml -> "application/vnd.uplanet.bearer-choice-wbxml"
    Application_VndUplanetCacheop -> "application/vnd.uplanet.cacheop"
    Application_VndUplanetCacheopWbxml -> "application/vnd.uplanet.cacheop-wbxml"
    Application_VndUplanetChannel -> "application/vnd.uplanet.channel"
    Application_VndUplanetChannelWbxml -> "application/vnd.uplanet.channel-wbxml"
    Application_VndUplanetList -> "application/vnd.uplanet.list"
    Application_VndUplanetListWbxml -> "application/vnd.uplanet.list-wbxml"
    Application_VndUplanetListcmd -> "application/vnd.uplanet.listcmd"
    Application_VndUplanetListcmdWbxml -> "application/vnd.uplanet.listcmd-wbxml"
    Application_VndUplanetSignal -> "application/vnd.uplanet.signal"
    Application_VndVcx -> "application/vnd.vcx"
    Application_VndVdStudy -> "application/vnd.vd-study"
    Application_VndVectorworks -> "application/vnd.vectorworks"
    Application_VndVerimatrixVcas -> "application/vnd.verimatrix.vcas"
    Application_VndVidsoftVidconference -> "application/vnd.vidsoft.vidconference"
    Application_VndVisio -> "application/vnd.visio"
    Application_VndVisionary -> "application/vnd.visionary"
    Application_VndVividenceScriptfile -> "application/vnd.vividence.scriptfile"
    Application_VndVsf -> "application/vnd.vsf"
    Application_VndWapSic -> "application/vnd.wap.sic"
    Application_VndWapSlc -> "application/vnd.wap.slc"
    Application_VndWapWbxml -> "application/vnd.wap.wbxml"
    Application_VndWapWmlc -> "application/vnd.wap.wmlc"
    Application_VndWapWmlscriptc -> "application/vnd.wap.wmlscriptc"
    Application_VndWebturbo -> "application/vnd.webturbo"
    Application_VndWfaWsc -> "application/vnd.wfa.wsc"
    Application_VndWmc -> "application/vnd.wmc"
    Application_VndWmfBootstrap -> "application/vnd.wmf.bootstrap"
    Application_VndWolframMathematica -> "application/vnd.wolfram.mathematica"
    Application_VndWolframMathematicaPackage -> "application/vnd.wolfram.mathematica.package"
    Application_VndWolframPlayer -> "application/vnd.wolfram.player"
    Application_VndWordperfect -> "application/vnd.wordperfect"
    Application_VndWqd -> "application/vnd.wqd"
    Application_VndWrqHp3000Labelled -> "application/vnd.wrq-hp3000-labelled"
    Application_VndWtStf -> "application/vnd.wt.stf"
    Application_VndWvCspWbxml -> "application/vnd.wv.csp+wbxml"
    Application_VndWvCspXml -> "application/vnd.wv.csp+xml"
    Application_VndWvSspXml -> "application/vnd.wv.ssp+xml"
    Application_VndXara -> "application/vnd.xara"
    Application_VndXfdl -> "application/vnd.xfdl"
    Application_VndXfdlWebform -> "application/vnd.xfdl.webform"
    Application_VndXmiXml -> "application/vnd.xmi+xml"
    Application_VndXmpieCpkg -> "application/vnd.xmpie.cpkg"
    Application_VndXmpieDpkg -> "application/vnd.xmpie.dpkg"
    Application_VndXmpiePlan -> "application/vnd.xmpie.plan"
    Application_VndXmpiePpkg -> "application/vnd.xmpie.ppkg"
    Application_VndXmpieXlim -> "application/vnd.xmpie.xlim"
    Application_VndYamahaHvDic -> "application/vnd.yamaha.hv-dic"
    Application_VndYamahaHvScript -> "application/vnd.yamaha.hv-script"
    Application_VndYamahaHvVoice -> "application/vnd.yamaha.hv-voice"
    Application_VndYamahaOpenscoreformat -> "application/vnd.yamaha.openscoreformat"
    Application_VndYamahaOpenscoreformatOsfpvgXml -> "application/vnd.yamaha.openscoreformat.osfpvg+xml"
    Application_VndYamahaRemoteSetup -> "application/vnd.yamaha.remote-setup"
    Application_VndYamahaSmafAudio -> "application/vnd.yamaha.smaf-audio"
    Application_VndYamahaSmafPhrase -> "application/vnd.yamaha.smaf-phrase"
    Application_VndYamahaThroughNgn -> "application/vnd.yamaha.through-ngn"
    Application_VndYamahaTunnelUdpencap -> "application/vnd.yamaha.tunnel-udpencap"
    Application_VndYellowriverCustomMenu -> "application/vnd.yellowriver-custom-menu"
    Application_VndZul -> "application/vnd.zul"
    Application_VndZzazzDeckXml -> "application/vnd.zzazz.deck+xml"
    Application_VocaltecMediaDesc -> "application/vocaltec-media-desc"
    Application_VocaltecMediaFile -> "application/vocaltec-media-file"
    Application_VoicexmlXml -> "application/voicexml+xml"
    Application_VqRtcpxr -> "application/vq-rtcpxr"
    Application_WatcherinfoXml -> "application/watcherinfo+xml"
    Application_WhoisppQuery -> "application/whoispp-query"
    Application_WhoisppResponse -> "application/whoispp-response"
    Application_Widget -> "application/widget"
    Application_Winhlp -> "application/winhlp"
    Application_Wita -> "application/wita"
    Application_Wordperfect -> "application/wordperfect"
    Application_Wordperfect51 -> "application/wordperfect5.1"
    Application_Wordperfect60 -> "application/wordperfect6.0"
    Application_Wordperfect61 -> "application/wordperfect6.1"
    Application_WsdlXml -> "application/wsdl+xml"
    Application_WspolicyXml -> "application/wspolicy+xml"
    Application_X123 -> "application/x-123"
    Application_X400Bp -> "application/x400-bp"
    Application_X7zCompressed -> "application/x-7z-compressed"
    Application_XAbiword -> "application/x-abiword"
    Application_XAceCompressed -> "application/x-ace-compressed"
    Application_XAim -> "application/x-aim"
    Application_XAmf -> "application/x-amf"
    Application_XAppleDiskimage -> "application/x-apple-diskimage"
    Application_XAuthorwareBin -> "application/x-authorware-bin"
    Application_XAuthorwareMap -> "application/x-authorware-map"
    Application_XAuthorwareSeg -> "application/x-authorware-seg"
    Application_XBcpio -> "application/x-bcpio"
    Application_XBinary -> "application/x-binary"
    Application_XBinhex40 -> "application/x-binhex40"
    Application_XBittorrent -> "application/x-bittorrent"
    Application_XBlorb -> "application/x-blorb"
    Application_XBsh -> "application/x-bsh"
    Application_XBytecodeElisp -> "application/x-bytecode.elisp"
    Application_XBytecodeElispCompiledelisp -> "application/x-bytecode.elisp(compiledelisp)"
    Application_XBytecodePython -> "application/x-bytecode.python"
    Application_XBzip -> "application/x-bzip"
    Application_XBzip2 -> "application/x-bzip2"
    Application_XCbr -> "application/x-cbr"
    Application_XCdf -> "application/x-cdf"
    Application_XCdlink -> "application/x-cdlink"
    Application_XCfsCompressed -> "application/x-cfs-compressed"
    Application_XChat -> "application/x-chat"
    Application_XChessPgn -> "application/x-chess-pgn"
    Application_XChm -> "application/x-chm"
    Application_XChromeExtension -> "application/x-chrome-extension"
    Application_XCmuRaster -> "application/x-cmu-raster"
    Application_XCocoa -> "application/x-cocoa"
    Application_XCompactpro -> "application/x-compactpro"
    Application_XCompress -> "application/x-compress"
    Application_XCompressed -> "application/x-compressed"
    Application_XConference -> "application/x-conference"
    Application_XCore -> "application/x-core"
    Application_XCpio -> "application/x-cpio"
    Application_XCpt -> "application/x-cpt"
    Application_XCsh -> "application/x-csh"
    Application_XDebianPackage -> "application/x-debian-package"
    Application_XDeepv -> "application/x-deepv"
    Application_XDgcCompressed -> "application/x-dgc-compressed"
    Application_XDirector -> "application/x-director"
    Application_XDms -> "application/x-dms"
    Application_XDoom -> "application/x-doom"
    Application_XDtbncxXml -> "application/x-dtbncx+xml"
    Application_XDtbookXml -> "application/x-dtbook+xml"
    Application_XDtbresourceXml -> "application/x-dtbresource+xml"
    Application_XDvi -> "application/x-dvi"
    Application_XElc -> "application/x-elc"
    Application_XEnvoy -> "application/x-envoy"
    Application_XEsrehber -> "application/x-esrehber"
    Application_XEva -> "application/x-eva"
    Application_XExcel -> "application/x-excel"
    Application_XExecutable -> "application/x-executable"
    Application_XFlac -> "application/x-flac"
    Application_XFont -> "application/x-font"
    Application_XFontBdf -> "application/x-font-bdf"
    Application_XFontDos -> "application/x-font-dos"
    Application_XFontFramemaker -> "application/x-font-framemaker"
    Application_XFontGhostscript -> "application/x-font-ghostscript"
    Application_XFontLibgrx -> "application/x-font-libgrx"
    Application_XFontLinuxPsf -> "application/x-font-linux-psf"
    Application_XFontOtf -> "application/x-font-otf"
    Application_XFontPcf -> "application/x-font-pcf"
    Application_XFontSnf -> "application/x-font-snf"
    Application_XFontSpeedo -> "application/x-font-speedo"
    Application_XFontSunosNews -> "application/x-font-sunos-news"
    Application_XFontTtf -> "application/x-font-ttf"
    Application_XFontType1 -> "application/x-font-type1"
    Application_XFontVfont -> "application/x-font-vfont"
    Application_XFontWoff -> "application/x-font-woff"
    Application_XFrame -> "application/x-frame"
    Application_XFreearc -> "application/x-freearc"
    Application_XFreelance -> "application/x-freelance"
    Application_XFuturesplash -> "application/x-futuresplash"
    Application_XGcaCompressed -> "application/x-gca-compressed"
    Application_XGlulx -> "application/x-glulx"
    Application_XGnumeric -> "application/x-gnumeric"
    Application_XGoSgf -> "application/x-go-sgf"
    Application_XGrampsXml -> "application/x-gramps-xml"
    Application_XGraphingCalculator -> "application/x-graphing-calculator"
    Application_XGsp -> "application/x-gsp"
    Application_XGss -> "application/x-gss"
    Application_XGtar -> "application/x-gtar"
    Application_XGzip -> "application/x-gzip"
    Application_XHdf -> "application/x-hdf"
    Application_XHelpfile -> "application/x-helpfile"
    Application_XHttpdImap -> "application/x-httpd-imap"
    Application_XHttpdPhp -> "application/x-httpd-php"
    Application_XHttpdPhp3 -> "application/x-httpd-php3"
    Application_XHttpdPhp3Preprocessed -> "application/x-httpd-php3-preprocessed"
    Application_XHttpdPhp4 -> "application/x-httpd-php4"
    Application_XHttpdPhpSource -> "application/x-httpd-php-source"
    Application_XIca -> "application/x-ica"
    Application_XIma -> "application/x-ima"
    Application_XInstallInstructions -> "application/x-install-instructions"
    Application_XInternetSignup -> "application/x-internet-signup"
    Application_XInternettSignup -> "application/x-internett-signup"
    Application_XInventor -> "application/x-inventor"
    Application_XIp2 -> "application/x-ip2"
    Application_XIphone -> "application/x-iphone"
    Application_XIso9660Image -> "application/x-iso9660-image"
    Application_XJavaApplet -> "application/x-java-applet"
    Application_XJavaArchive -> "application/x-java-archive"
    Application_XJavaBean -> "application/x-java-bean"
    Application_XJavaClass -> "application/x-java-class"
    Application_XJavaCommerce -> "application/x-java-commerce"
    Application_XJavaJnlpFile -> "application/x-java-jnlp-file"
    Application_XJavaSerializedObject -> "application/x-java-serialized-object"
    Application_XJavaVm -> "application/x-java-vm"
    Application_XJavascript -> "application/x-javascript"
    Application_XKchart -> "application/x-kchart"
    Application_XKdelnk -> "application/x-kdelnk"
    Application_XKillustrator -> "application/x-killustrator"
    Application_XKoan -> "application/x-koan"
    Application_XKpresenter -> "application/x-kpresenter"
    Application_XKsh -> "application/x-ksh"
    Application_XKspread -> "application/x-kspread"
    Application_XKword -> "application/x-kword"
    Application_XLatex -> "application/x-latex"
    Application_XLha -> "application/x-lha"
    Application_XLisp -> "application/x-lisp"
    Application_XLivescreen -> "application/x-livescreen"
    Application_XLotus -> "application/x-lotus"
    Application_XLotusscreencam -> "application/x-lotusscreencam"
    Application_XLuaBytecode -> "application/x-lua-bytecode"
    Application_XLzh -> "application/x-lzh"
    Application_XLzhCompressed -> "application/x-lzh-compressed"
    Application_XLzx -> "application/x-lzx"
    Application_XMacBinhex40 -> "application/x-mac-binhex40"
    Application_XMacbinary -> "application/x-macbinary"
    Application_XMagicCapPackage10 -> "application/x-magic-cap-package-1.0"
    Application_XMaker -> "application/x-maker"
    Application_XMathcad -> "application/x-mathcad"
    Application_XMeme -> "application/x-meme"
    Application_XMidi -> "application/x-midi"
    Application_XMie -> "application/x-mie"
    Application_XMif -> "application/x-mif"
    Application_XMixTransfer -> "application/x-mix-transfer"
    Application_XMobipocketEbook -> "application/x-mobipocket-ebook"
    Application_XMpegurl -> "application/x-mpegURL"
    Application_XMplayer2 -> "application/x-mplayer2"
    Application_XMsApplication -> "application/x-ms-application"
    Application_XMsShortcut -> "application/x-ms-shortcut"
    Application_XMsWmd -> "application/x-ms-wmd"
    Application_XMsWmz -> "application/x-ms-wmz"
    Application_XMsXbap -> "application/x-ms-xbap"
    Application_XMsaccess -> "application/x-msaccess"
    Application_XMsbinder -> "application/x-msbinder"
    Application_XMscardfile -> "application/x-mscardfile"
    Application_XMsclip -> "application/x-msclip"
    Application_XMsdosProgram -> "application/x-msdos-program"
    Application_XMsdownload -> "application/x-msdownload"
    Application_XMsexcel -> "application/x-msexcel"
    Application_XMsi -> "application/x-msi"
    Application_XMsmediaview -> "application/x-msmediaview"
    Application_XMsmetafile -> "application/x-msmetafile"
    Application_XMsmoney -> "application/x-msmoney"
    Application_XMspowerpoint -> "application/x-mspowerpoint"
    Application_XMspublisher -> "application/x-mspublisher"
    Application_XMsschedule -> "application/x-msschedule"
    Application_XMsterminal -> "application/x-msterminal"
    Application_XMswrite -> "application/x-mswrite"
    Application_XNaviAnimation -> "application/x-navi-animation"
    Application_XNavidoc -> "application/x-navidoc"
    Application_XNavimap -> "application/x-navimap"
    Application_XNavistyle -> "application/x-navistyle"
    Application_XNetcdf -> "application/x-netcdf"
    Application_XNewtonCompatiblePkg -> "application/x-newton-compatible-pkg"
    Application_XNokia9000CommunicatorAddOnSoftware -> "application/x-nokia-9000-communicator-add-on-software"
    Application_XNsProxyAutoconfig -> "application/x-ns-proxy-autoconfig"
    Application_XNwc -> "application/x-nwc"
    Application_XNzb -> "application/x-nzb"
    Application_XObject -> "application/x-object"
    Application_XOmc -> "application/x-omc"
    Application_XOmcdatamaker -> "application/x-omcdatamaker"
    Application_XOmcregerator -> "application/x-omcregerator"
    Application_XOzApplication -> "application/x-oz-application"
    Application_XPagemaker -> "application/x-pagemaker"
    Application_XPcl -> "application/x-pcl"
    Application_XPerfmon -> "application/x-perfmon"
    Application_XPixclscript -> "application/x-pixclscript"
    Application_XPkcs10 -> "application/x-pkcs10"
    Application_XPkcs12 -> "application/x-pkcs12"
    Application_XPkcs7Certificates -> "application/x-pkcs7-certificates"
    Application_XPkcs7Certreqresp -> "application/x-pkcs7-certreqresp"
    Application_XPkcs7Crl -> "application/x-pkcs7-crl"
    Application_XPkcs7Mime -> "application/x-pkcs7-mime"
    Application_XPkcs7Signature -> "application/x-pkcs7-signature"
    Application_XPointplus -> "application/x-pointplus"
    Application_XPortableAnymap -> "application/x-portable-anymap"
    Application_XProject -> "application/x-project"
    Application_XPythonCode -> "application/x-python-code"
    Application_XQpro -> "application/x-qpro"
    Application_XQuicktimeplayer -> "application/x-quicktimeplayer"
    Application_XRarCompressed -> "application/x-rar-compressed"
    Application_XRedhatPackageManager -> "application/x-redhat-package-manager"
    Application_XResearchInfoSystems -> "application/x-research-info-systems"
    Application_XRpm -> "application/x-rpm"
    Application_XRtf -> "application/x-rtf"
    Application_XRx -> "application/x-rx"
    Application_XSdp -> "application/x-sdp"
    Application_XSea -> "application/x-sea"
    Application_XSeelogo -> "application/x-seelogo"
    Application_XSh -> "application/x-sh"
    Application_XShar -> "application/x-shar"
    Application_XShellscript -> "application/x-shellscript"
    Application_XShockwaveFlash -> "application/x-shockwave-flash"
    Application_XSilverlightApp -> "application/x-silverlight-app"
    Application_XSit -> "application/x-sit"
    Application_XSprite -> "application/x-sprite"
    Application_XSql -> "application/x-sql"
    Application_XStuffit -> "application/x-stuffit"
    Application_XStuffitx -> "application/x-stuffitx"
    Application_XSubrip -> "application/x-subrip"
    Application_XSv4cpio -> "application/x-sv4cpio"
    Application_XSv4crc -> "application/x-sv4crc"
    Application_XT3vmImage -> "application/x-t3vm-image"
    Application_XTads -> "application/x-tads"
    Application_XTar -> "application/x-tar"
    Application_XTbook -> "application/x-tbook"
    Application_XTcl -> "application/x-tcl"
    Application_XTex -> "application/x-tex"
    Application_XTexGf -> "application/x-tex-gf"
    Application_XTexPk -> "application/x-tex-pk"
    Application_XTexTfm -> "application/x-tex-tfm"
    Application_XTexinfo -> "application/x-texinfo"
    Application_XTgif -> "application/x-tgif"
    Application_XTrash -> "application/x-trash"
    Application_XTroff -> "application/x-troff"
    Application_XTroffMan -> "application/x-troff-man"
    Application_XTroffMe -> "application/x-troff-me"
    Application_XTroffMs -> "application/x-troff-ms"
    Application_XTroffMsvideo -> "application/x-troff-msvideo"
    Application_XUstar -> "application/x-ustar"
    Application_XVideolan -> "application/x-videolan"
    Application_XVisio -> "application/x-visio"
    Application_XVndAudioexplosionMzz -> "application/x-vnd.audioexplosion.mzz"
    Application_XVndLsXpix -> "application/x-vnd.ls-xpix"
    Application_XVrml -> "application/x-vrml"
    Application_XWaisSource -> "application/x-wais-source"
    Application_XWebAppManifestJson -> "application/x-web-app-manifest+json"
    Application_XWingz -> "application/x-wingz"
    Application_XWinhelp -> "application/x-winhelp"
    Application_XWintalk -> "application/x-wintalk"
    Application_XWorld -> "application/x-world"
    Application_XWpwin -> "application/x-wpwin"
    Application_XWri -> "application/x-wri"
    Application_XX509CaCert -> "application/x-x509-ca-cert"
    Application_XX509UserCert -> "application/x-x509-user-cert"
    Application_XXcf -> "application/x-xcf"
    Application_XXfig -> "application/x-xfig"
    Application_XXliffXml -> "application/x-xliff+xml"
    Application_XXpinstall -> "application/x-xpinstall"
    Application_XXz -> "application/x-xz"
    Application_XZipCompressed -> "application/x-zip-compressed"
    Application_XZmachine -> "application/x-zmachine"
    Application_XamlXml -> "application/xaml+xml"
    Application_XcapAttXml -> "application/xcap-att+xml"
    Application_XcapCapsXml -> "application/xcap-caps+xml"
    Application_XcapDiffXml -> "application/xcap-diff+xml"
    Application_XcapElXml -> "application/xcap-el+xml"
    Application_XcapErrorXml -> "application/xcap-error+xml"
    Application_XcapNsXml -> "application/xcap-ns+xml"
    Application_XconConferenceInfoDiffXml -> "application/xcon-conference-info-diff+xml"
    Application_XconConferenceInfoXml -> "application/xcon-conference-info+xml"
    Application_XencXml -> "application/xenc+xml"
    Application_XhtmlVoiceXml -> "application/xhtml-voice+xml"
    Application_XhtmlXml -> "application/xhtml+xml"
    Application_Xml -> "application/xml"
    Application_XmlDtd -> "application/xml-dtd"
    Application_XmlExternalParsedEntity -> "application/xml-external-parsed-entity"
    Application_XmppXml -> "application/xmpp+xml"
    Application_XopXml -> "application/xop+xml"
    Application_XprocXml -> "application/xproc+xml"
    Application_XsltXml -> "application/xslt+xml"
    Application_XspfXml -> "application/xspf+xml"
    Application_XvXml -> "application/xv+xml"
    Application_Yang -> "application/yang"
    Application_YinXml -> "application/yin+xml"
    Application_YndMsPkipko -> "application/ynd.ms-pkipko"
    Application_Zip -> "application/zip"
    Audio_1dInterleavedParityfec -> "audio/1d-interleaved-parityfec"
    Audio_32kadpcm -> "audio/32kadpcm"
    Audio_3gpp -> "audio/3gpp"
    Audio_3gpp2 -> "audio/3gpp2"
    Audio_Ac3 -> "audio/ac3"
    Audio_Adpcm -> "audio/adpcm"
    Audio_Aiff -> "audio/aiff"
    Audio_Amr -> "audio/amr"
    Audio_AmrWb -> "audio/amr-wb"
    Audio_Asc -> "audio/asc"
    Audio_Atrac3 -> "audio/atrac3"
    Audio_AtracAdvancedLossless -> "audio/atrac-advanced-lossless"
    Audio_AtracX -> "audio/atrac-x"
    Audio_Basic -> "audio/basic"
    Audio_Bv16 -> "audio/bv16"
    Audio_Bv32 -> "audio/bv32"
    Audio_Clearmode -> "audio/clearmode"
    Audio_Cn -> "audio/cn"
    Audio_Dat12 -> "audio/dat12"
    Audio_Dls -> "audio/dls"
    Audio_DsrEs201108 -> "audio/dsr-es201108"
    Audio_DsrEs202050 -> "audio/dsr-es202050"
    Audio_DsrEs202211 -> "audio/dsr-es202211"
    Audio_DsrEs202212 -> "audio/dsr-es202212"
    Audio_Dv -> "audio/dv"
    Audio_Dvi4 -> "audio/dvi4"
    Audio_Eac3 -> "audio/eac3"
    Audio_Evrc -> "audio/evrc"
    Audio_Evrc0 -> "audio/evrc0"
    Audio_Evrc1 -> "audio/evrc1"
    Audio_EvrcQcp -> "audio/evrc-qcp"
    Audio_Evrcb -> "audio/evrcb"
    Audio_Evrcb0 -> "audio/evrcb0"
    Audio_Evrcb1 -> "audio/evrcb1"
    Audio_Evrcwb -> "audio/evrcwb"
    Audio_Evrcwb0 -> "audio/evrcwb0"
    Audio_Evrcwb1 -> "audio/evrcwb1"
    Audio_Example -> "audio/example"
    Audio_Flac -> "audio/flac"
    Audio_Fwdred -> "audio/fwdred"
    Audio_G719 -> "audio/g719"
    Audio_G722 -> "audio/g722"
    Audio_G7221 -> "audio/g.722.1"
    Audio_G723 -> "audio/g723"
    Audio_G72616 -> "audio/g726-16"
    Audio_G72624 -> "audio/g726-24"
    Audio_G72632 -> "audio/g726-32"
    Audio_G72640 -> "audio/g726-40"
    Audio_G728 -> "audio/g728"
    Audio_G729 -> "audio/g729"
    Audio_G7291 -> "audio/g7291"
    Audio_G729d -> "audio/g729d"
    Audio_G729e -> "audio/g729e"
    Audio_Gsm -> "audio/gsm"
    Audio_GsmEfr -> "audio/gsm-efr"
    Audio_GsmHr08 -> "audio/gsm-hr-08"
    Audio_Ilbc -> "audio/ilbc"
    Audio_IpMr_v25 -> "audio/ip-mr_v2.5"
    Audio_Isac -> "audio/isac"
    Audio_It -> "audio/it"
    Audio_L16 -> "audio/l16"
    Audio_L20 -> "audio/l20"
    Audio_L24 -> "audio/l24"
    Audio_L8 -> "audio/l8"
    Audio_Lpc -> "audio/lpc"
    Audio_Make -> "audio/make"
    Audio_MakeMyFunk -> "audio/make.my.funk"
    Audio_Mid -> "audio/mid"
    Audio_Midi -> "audio/midi"
    Audio_MobileXmf -> "audio/mobile-xmf"
    Audio_Mod -> "audio/mod"
    Audio_Mp4 -> "audio/mp4"
    Audio_Mp4aLatm -> "audio/mp4a-latm"
    Audio_Mpa -> "audio/mpa"
    Audio_MpaRobust -> "audio/mpa-robust"
    Audio_Mpeg -> "audio/mpeg"
    Audio_Mpeg3 -> "audio/mpeg3"
    Audio_Mpeg4Generic -> "audio/mpeg4-generic"
    Audio_Mpegurl -> "audio/mpegurl"
    Audio_Musepack -> "audio/musepack"
    Audio_Nspaudio -> "audio/nspaudio"
    Audio_Ogg -> "audio/ogg"
    Audio_Opus -> "audio/opus"
    Audio_Parityfec -> "audio/parityfec"
    Audio_Pcma -> "audio/pcma"
    Audio_PcmaWb -> "audio/pcma-wb"
    Audio_Pcmu -> "audio/pcmu"
    Audio_PcmuWb -> "audio/pcmu-wb"
    Audio_PrsSid -> "audio/prs.sid"
    Audio_Qcelp -> "audio/qcelp"
    Audio_Red -> "audio/red"
    Audio_RtpEncAescm128 -> "audio/rtp-enc-aescm128"
    Audio_RtpMidi -> "audio/rtp-midi"
    Audio_Rtx -> "audio/rtx"
    Audio_S3m -> "audio/s3m"
    Audio_Silk -> "audio/silk"
    Audio_Smv -> "audio/smv"
    Audio_Smv0 -> "audio/smv0"
    Audio_SmvQcp -> "audio/smv-qcp"
    Audio_SpMidi -> "audio/sp-midi"
    Audio_Speex -> "audio/speex"
    Audio_T140c -> "audio/t140c"
    Audio_T38 -> "audio/t38"
    Audio_TelephoneEvent -> "audio/telephone-event"
    Audio_Tone -> "audio/tone"
    Audio_TspAudio -> "audio/tsp-audio"
    Audio_Tsplayer -> "audio/tsplayer"
    Audio_Uemclip -> "audio/uemclip"
    Audio_Ulpfec -> "audio/ulpfec"
    Audio_Vdvi -> "audio/vdvi"
    Audio_VmrWb -> "audio/vmr-wb"
    Audio_Vnd3gppIufp -> "audio/vnd.3gpp.iufp"
    Audio_Vnd4sb -> "audio/vnd.4sb"
    Audio_VndAudiokoz -> "audio/vnd.audiokoz"
    Audio_VndCelp -> "audio/vnd.celp"
    Audio_VndCiscoNse -> "audio/vnd.cisco.nse"
    Audio_VndCmlesRadioEvents -> "audio/vnd.cmles.radio-events"
    Audio_VndCnsAnp1 -> "audio/vnd.cns.anp1"
    Audio_VndCnsInf1 -> "audio/vnd.cns.inf1"
    Audio_VndDeceAudio -> "audio/vnd.dece.audio"
    Audio_VndDigitalWinds -> "audio/vnd.digital-winds"
    Audio_VndDlnaAdts -> "audio/vnd.dlna.adts"
    Audio_VndDolbyHeaac1 -> "audio/vnd.dolby.heaac.1"
    Audio_VndDolbyHeaac2 -> "audio/vnd.dolby.heaac.2"
    Audio_VndDolbyMlp -> "audio/vnd.dolby.mlp"
    Audio_VndDolbyMps -> "audio/vnd.dolby.mps"
    Audio_VndDolbyPl2 -> "audio/vnd.dolby.pl2"
    Audio_VndDolbyPl2x -> "audio/vnd.dolby.pl2x"
    Audio_VndDolbyPl2z -> "audio/vnd.dolby.pl2z"
    Audio_VndDolbyPulse1 -> "audio/vnd.dolby.pulse.1"
    Audio_VndDra -> "audio/vnd.dra"
    Audio_VndDts -> "audio/vnd.dts"
    Audio_VndDtsHd -> "audio/vnd.dts.hd"
    Audio_VndDvbFile -> "audio/vnd.dvb.file"
    Audio_VndEveradPlj -> "audio/vnd.everad.plj"
    Audio_VndHnsAudio -> "audio/vnd.hns.audio"
    Audio_VndLucentVoice -> "audio/vnd.lucent.voice"
    Audio_VndMsPlayreadyMediaPya -> "audio/vnd.ms-playready.media.pya"
    Audio_VndNokiaMobileXmf -> "audio/vnd.nokia.mobile-xmf"
    Audio_VndNortelVbk -> "audio/vnd.nortel.vbk"
    Audio_VndNueraEcelp4800 -> "audio/vnd.nuera.ecelp4800"
    Audio_VndNueraEcelp7470 -> "audio/vnd.nuera.ecelp7470"
    Audio_VndNueraEcelp9600 -> "audio/vnd.nuera.ecelp9600"
    Audio_VndOctelSbc -> "audio/vnd.octel.sbc"
    Audio_VndQcelp -> "audio/vnd.qcelp"
    Audio_VndRhetorex32kadpcm -> "audio/vnd.rhetorex.32kadpcm"
    Audio_VndRip -> "audio/vnd.rip"
    Audio_VndSealedmediaSoftsealMpeg -> "audio/vnd.sealedmedia.softseal.mpeg"
    Audio_VndVmxCvsd -> "audio/vnd.vmx.cvsd"
    Audio_Voc -> "audio/voc"
    Audio_Vorbis -> "audio/vorbis"
    Audio_VorbisConfig -> "audio/vorbis-config"
    Audio_Voxware -> "audio/voxware"
    Audio_Wav -> "audio/wav"
    Audio_Webm -> "audio/webm"
    Audio_XAac -> "audio/x-aac"
    Audio_XAdpcm -> "audio/x-adpcm"
    Audio_XAiff -> "audio/x-aiff"
    Audio_XAu -> "audio/x-au"
    Audio_XCaf -> "audio/x-caf"
    Audio_XFlac -> "audio/x-flac"
    Audio_XGsm -> "audio/x-gsm"
    Audio_XJam -> "audio/x-jam"
    Audio_XLiveaudio -> "audio/x-liveaudio"
    Audio_XMatroska -> "audio/x-matroska"
    Audio_XMid -> "audio/x-mid"
    Audio_XMidi -> "audio/x-midi"
    Audio_XMod -> "audio/x-mod"
    Audio_XMpeg -> "audio/x-mpeg"
    Audio_XMpeg3 -> "audio/x-mpeg-3"
    Audio_XMpegurl -> "audio/x-mpegurl"
    Audio_XMpequrl -> "audio/x-mpequrl"
    Audio_XMsWax -> "audio/x-ms-wax"
    Audio_XMsWma -> "audio/x-ms-wma"
    Audio_XNspaudio -> "audio/x-nspaudio"
    Audio_XPnRealaudio -> "audio/x-pn-realaudio"
    Audio_XPnRealaudioPlugin -> "audio/x-pn-realaudio-plugin"
    Audio_XPsid -> "audio/x-psid"
    Audio_XRealaudio -> "audio/x-realaudio"
    Audio_XScpls -> "audio/x-scpls"
    Audio_XSd2 -> "audio/x-sd2"
    Audio_XTta -> "audio/x-tta"
    Audio_XTwinvq -> "audio/x-twinvq"
    Audio_XTwinvqPlugin -> "audio/x-twinvq-plugin"
    Audio_XVndAudioexplosionMjuicemediafile -> "audio/x-vnd.audioexplosion.mjuicemediafile"
    Audio_XVoc -> "audio/x-voc"
    Audio_XWav -> "audio/x-wav"
    Audio_Xm -> "audio/xm"
    Chemical_XCdx -> "chemical/x-cdx"
    Chemical_XCif -> "chemical/x-cif"
    Chemical_XCmdf -> "chemical/x-cmdf"
    Chemical_XCml -> "chemical/x-cml"
    Chemical_XCsml -> "chemical/x-csml"
    Chemical_XPdb -> "chemical/x-pdb"
    Chemical_XXyz -> "chemical/x-xyz"
    Content_Unknown -> "content/unknown"
    Drawing_XDwf -> "drawing/x-dwf"
    Drawing_XDwfOld -> "drawing/x-dwf(old)"
    Font_Opentype -> "font/opentype"
    IWorld_IVrml -> "i-world/i-vrml"
    Image_Bmp -> "image/bmp"
    Image_Cgm -> "image/cgm"
    Image_CisCod -> "image/cis-cod"
    Image_CmuRaster -> "image/cmu-raster"
    Image_Example -> "image/example"
    Image_Fif -> "image/fif"
    Image_Fits -> "image/fits"
    Image_Florian -> "image/florian"
    Image_G3fax -> "image/g3fax"
    Image_Gif -> "image/gif"
    Image_Ief -> "image/ief"
    Image_Jp2 -> "image/jp2"
    Image_Jpeg -> "image/jpeg"
    Image_Jpm -> "image/jpm"
    Image_Jpx -> "image/jpx"
    Image_Jutvision -> "image/jutvision"
    Image_Ktx -> "image/ktx"
    Image_Naplps -> "image/naplps"
    Image_Pcx -> "image/pcx"
    Image_Pict -> "image/pict"
    Image_Pipeg -> "image/pipeg"
    Image_Pjpeg -> "image/pjpeg"
    Image_Png -> "image/png"
    Image_PrsBtif -> "image/prs.btif"
    Image_PrsPti -> "image/prs.pti"
    Image_Sgi -> "image/sgi"
    Image_SvgXml -> "image/svg+xml"
    Image_T38 -> "image/t38"
    Image_Tiff -> "image/tiff"
    Image_TiffFx -> "image/tiff-fx"
    Image_Vasa -> "image/vasa"
    Image_VndAdobePhotoshop -> "image/vnd.adobe.photoshop"
    Image_VndCnsInf2 -> "image/vnd.cns.inf2"
    Image_VndDeceGraphic -> "image/vnd.dece.graphic"
    Image_VndDjvu -> "image/vnd.djvu"
    Image_VndDvbSubtitle -> "image/vnd.dvb.subtitle"
    Image_VndDwg -> "image/vnd.dwg"
    Image_VndDxf -> "image/vnd.dxf"
    Image_VndFastbidsheet -> "image/vnd.fastbidsheet"
    Image_VndFpx -> "image/vnd.fpx"
    Image_VndFst -> "image/vnd.fst"
    Image_VndFujixeroxEdmicsMmr -> "image/vnd.fujixerox.edmics-mmr"
    Image_VndFujixeroxEdmicsRlc -> "image/vnd.fujixerox.edmics-rlc"
    Image_VndGlobalgraphicsPgb -> "image/vnd.globalgraphics.pgb"
    Image_VndMicrosoftIcon -> "image/vnd.microsoft.icon"
    Image_VndMix -> "image/vnd.mix"
    Image_VndMsModi -> "image/vnd.ms-modi"
    Image_VndMsPhoto -> "image/vnd.ms-photo"
    Image_VndNetFpx -> "image/vnd.net-fpx"
    Image_VndRadiance -> "image/vnd.radiance"
    Image_VndRnRealflash -> "image/vnd.rn-realflash"
    Image_VndRnRealpix -> "image/vnd.rn-realpix"
    Image_VndSealedPng -> "image/vnd.sealed.png"
    Image_VndSealedmediaSoftsealGif -> "image/vnd.sealedmedia.softseal.gif"
    Image_VndSealedmediaSoftsealJpg -> "image/vnd.sealedmedia.softseal.jpg"
    Image_VndSvf -> "image/vnd.svf"
    Image_VndWapWbmp -> "image/vnd.wap.wbmp"
    Image_VndXiff -> "image/vnd.xiff"
    Image_Webp -> "image/webp"
    Image_X3ds -> "image/x-3ds"
    Image_XCmuRast -> "image/x-cmu-rast"
    Image_XCmuRaster -> "image/x-cmu-raster"
    Image_XCmx -> "image/x-cmx"
    Image_XCoreldraw -> "image/x-coreldraw"
    Image_XCoreldrawpattern -> "image/x-coreldrawpattern"
    Image_XCoreldrawtemplate -> "image/x-coreldrawtemplate"
    Image_XCorelphotopaint -> "image/x-corelphotopaint"
    Image_XDwg -> "image/x-dwg"
    Image_XFreehand -> "image/x-freehand"
    Image_XIcon -> "image/x-icon"
    Image_XJg -> "image/x-jg"
    Image_XJng -> "image/x-jng"
    Image_XJps -> "image/x-jps"
    Image_XMrsidImage -> "image/x-mrsid-image"
    Image_XMsBmp -> "image/x-ms-bmp"
    Image_XNiff -> "image/x-niff"
    Image_XPcx -> "image/x-pcx"
    Image_XPhotoshop -> "image/x-photoshop"
    Image_XPict -> "image/x-pict"
    Image_XPortableAnymap -> "image/x-portable-anymap"
    Image_XPortableBitmap -> "image/x-portable-bitmap"
    Image_XPortableGraymap -> "image/x-portable-graymap"
    Image_XPortableGreymap -> "image/x-portable-greymap"
    Image_XPortablePixmap -> "image/x-portable-pixmap"
    Image_XQuicktime -> "image/x-quicktime"
    Image_XRgb -> "image/x-rgb"
    Image_XTga -> "image/x-tga"
    Image_XTiff -> "image/x-tiff"
    Image_XWindowsBmp -> "image/x-windows-bmp"
    Image_XXbitmap -> "image/x-xbitmap"
    Image_XXbm -> "image/x-xbm"
    Image_XXpixmap -> "image/x-xpixmap"
    Image_XXwd -> "image/x-xwd"
    Image_XXwindowdump -> "image/x-xwindowdump"
    Image_Xbm -> "image/xbm"
    Image_Xpm -> "image/xpm"
    Inode_Blockdevice -> "inode/blockdevice"
    Inode_Chardevice -> "inode/chardevice"
    Inode_Directory -> "inode/directory"
    Inode_DirectoryLocked -> "inode/directory-locked"
    Inode_Fifo -> "inode/fifo"
    Inode_Socket -> "inode/socket"
    Message_Cpim -> "message/cpim"
    Message_DeliveryStatus -> "message/delivery-status"
    Message_DispositionNotification -> "message/disposition-notification"
    Message_Example -> "message/example"
    Message_ExternalBody -> "message/external-body"
    Message_FeedbackReport -> "message/feedback-report"
    Message_Global -> "message/global"
    Message_GlobalDeliveryStatus -> "message/global-delivery-status"
    Message_GlobalDispositionNotification -> "message/global-disposition-notification"
    Message_GlobalHeaders -> "message/global-headers"
    Message_Http -> "message/http"
    Message_ImdnXml -> "message/imdn+xml"
    Message_News -> "message/news"
    Message_Partial -> "message/partial"
    Message_Rfc822 -> "message/rfc822"
    Message_SHttp -> "message/s-http"
    Message_Sip -> "message/sip"
    Message_Sipfrag -> "message/sipfrag"
    Message_TrackingStatus -> "message/tracking-status"
    Message_VndSiSimp -> "message/vnd.si.simp"
    Model_Example -> "model/example"
    Model_Iges -> "model/iges"
    Model_Mesh -> "model/mesh"
    Model_VndColladaXml -> "model/vnd.collada+xml"
    Model_VndDwf -> "model/vnd.dwf"
    Model_VndFlatland3dml -> "model/vnd.flatland.3dml"
    Model_VndGdl -> "model/vnd.gdl"
    Model_VndGsGdl -> "model/vnd.gs-gdl"
    Model_VndGtw -> "model/vnd.gtw"
    Model_VndMomlXml -> "model/vnd.moml+xml"
    Model_VndMts -> "model/vnd.mts"
    Model_VndParasolidTransmitBinary -> "model/vnd.parasolid.transmit.binary"
    Model_VndParasolidTransmitText -> "model/vnd.parasolid.transmit.text"
    Model_VndVtu -> "model/vnd.vtu"
    Model_Vrml -> "model/vrml"
    Model_X3dBinary -> "model/x3d+binary"
    Model_X3dVrml -> "model/x3d+vrml"
    Model_X3dXml -> "model/x3d+xml"
    Model_XPov -> "model/x-pov"
    Multipart_Alternative -> "multipart/alternative"
    Multipart_Appledouble -> "multipart/appledouble"
    Multipart_Byteranges -> "multipart/byteranges"
    Multipart_Digest -> "multipart/digest"
    Multipart_Encrypted -> "multipart/encrypted"
    Multipart_Example -> "multipart/example"
    Multipart_FormData -> "multipart/form-data"
    Multipart_HeaderSet -> "multipart/header-set"
    Multipart_Mixed -> "multipart/mixed"
    Multipart_Parallel -> "multipart/parallel"
    Multipart_Related -> "multipart/related"
    Multipart_Report -> "multipart/report"
    Multipart_Signed -> "multipart/signed"
    Multipart_VoiceMessage -> "multipart/voice-message"
    Multipart_XGzip -> "multipart/x-gzip"
    Multipart_XUstar -> "multipart/x-ustar"
    Multipart_XZip -> "multipart/x-zip"
    Music_Crescendo -> "music/crescendo"
    Music_XKaraoke -> "music/x-karaoke"
    Paleovu_XPv -> "paleovu/x-pv"
    Text_1dInterleavedParityfec -> "text/1d-interleaved-parityfec"
    Text_Asp -> "text/asp"
    Text_CacheManifest -> "text/cache-manifest"
    Text_Calendar -> "text/calendar"
    Text_CommaSeparatedValues -> "text/comma-separated-values"
    Text_Css -> "text/css"
    Text_Csv -> "text/csv"
    Text_Directory -> "text/directory"
    Text_Dns -> "text/dns"
    Text_Ecmascript -> "text/ecmascript"
    Text_English -> "text/english"
    Text_Enriched -> "text/enriched"
    Text_EventStream -> "text/event-stream"
    Text_Example -> "text/example"
    Text_Fwdred -> "text/fwdred"
    Text_H323 -> "text/h323"
    Text_Html -> "text/html"
    Text_Iuls -> "text/iuls"
    Text_Javascript -> "text/javascript"
    Text_Mathml -> "text/mathml"
    Text_Mcf -> "text/mcf"
    Text_N3 -> "text/n3"
    Text_Parityfec -> "text/parityfec"
    Text_Pascal -> "text/pascal"
    Text_Plain -> "text/plain"
    Text_PlainBas -> "text/plain-bas"
    Text_PrsFallensteinRst -> "text/prs.fallenstein.rst"
    Text_PrsLinesTag -> "text/prs.lines.tag"
    Text_Red -> "text/red"
    Text_Rfc822Headers -> "text/rfc822-headers"
    Text_Richtext -> "text/richtext"
    Text_Rtf -> "text/rtf"
    Text_RtpEncAescm128 -> "text/rtp-enc-aescm128"
    Text_Rtx -> "text/rtx"
    Text_Scriplet -> "text/scriplet"
    Text_Scriptlet -> "text/scriptlet"
    Text_Sgml -> "text/sgml"
    Text_T140 -> "text/t140"
    Text_TabSeparatedValues -> "text/tab-separated-values"
    Text_Texmacs -> "text/texmacs"
    Text_Troff -> "text/troff"
    Text_Turtle -> "text/turtle"
    Text_Ulpfec -> "text/ulpfec"
    Text_UriList -> "text/uri-list"
    Text_Vcard -> "text/vcard"
    Text_VndAbc -> "text/vnd.abc"
    Text_VndCurl -> "text/vnd.curl"
    Text_VndCurlDcurl -> "text/vnd.curl.dcurl"
    Text_VndCurlMcurl -> "text/vnd.curl.mcurl"
    Text_VndCurlScurl -> "text/vnd.curl.scurl"
    Text_VndDmclientscript -> "text/vnd.DMClientScript"
    Text_VndDvbSubtitle -> "text/vnd.dvb.subtitle"
    Text_VndEsmertecThemeDescriptor -> "text/vnd.esmertec.theme-descriptor"
    Text_VndFlatland3dml -> "text/vnd.flatland.3dml"
    Text_VndFly -> "text/vnd.fly"
    Text_VndFmiFlexstor -> "text/vnd.fmi.flexstor"
    Text_VndGraphviz -> "text/vnd.graphviz"
    Text_VndIn3d3dml -> "text/vnd.in3d.3dml"
    Text_VndIn3dSpot -> "text/vnd.in3d.spot"
    Text_VndIptcNewsml -> "text/vnd.IPTC.NewsML"
    Text_VndIptcNitf -> "text/vnd.IPTC.NITF"
    Text_VndLatexZ -> "text/vnd.latex-z"
    Text_VndMotorolaReflex -> "text/vnd.motorola.reflex"
    Text_VndMsMediapackage -> "text/vnd.ms-mediapackage"
    Text_VndNet2phoneCommcenterCommand -> "text/vnd.net2phone.commcenter.command"
    Text_VndRadisysMsmlBasicLayout -> "text/vnd.radisys.msml-basic-layout"
    Text_VndRnRealtext -> "text/vnd.rn-realtext"
    Text_VndSiUricatalogue -> "text/vnd.si.uricatalogue"
    Text_VndSunJ2meAppDescriptor -> "text/vnd.sun.j2me.app-descriptor"
    Text_VndTrolltechLinguist -> "text/vnd.trolltech.linguist"
    Text_VndWapSi -> "text/vnd.wap.si"
    Text_VndWapSl -> "text/vnd.wap.sl"
    Text_VndWapWml -> "text/vnd.wap.wml"
    Text_VndWapWmlscript -> "text/vnd.wap.wmlscript"
    Text_Vtt -> "text/vtt"
    Text_Webviewhtml -> "text/webviewhtml"
    Text_XAsm -> "text/x-asm"
    Text_XAudiosoftIntra -> "text/x-audiosoft-intra"
    Text_XC -> "text/x-c"
    Text_XCHdr -> "text/x-c++hdr"
    Text_XCSrc -> "text/x-c++src"
    Text_XChdr -> "text/x-chdr"
    Text_XComponent -> "text/x-component"
    Text_XCrontab -> "text/x-crontab"
    Text_XCsh -> "text/x-csh"
    Text_XCsrc -> "text/x-csrc"
    Text_XFortran -> "text/x-fortran"
    Text_XH -> "text/x-h"
    Text_XJava -> "text/x-java"
    Text_XJavaSource -> "text/x-java-source"
    Text_XLaAsf -> "text/x-la-asf"
    Text_XLua -> "text/x-lua"
    Text_XM -> "text/x-m"
    Text_XMakefile -> "text/x-makefile"
    Text_XMarkdown -> "text/x-markdown"
    Text_XMoc -> "text/x-moc"
    Text_XNfo -> "text/x-nfo"
    Text_XOpml -> "text/x-opml"
    Text_XPascal -> "text/x-pascal"
    Text_XPcsGcd -> "text/x-pcs-gcd"
    Text_XPerl -> "text/x-perl"
    Text_XPython -> "text/x-python"
    Text_XScript -> "text/x-script"
    Text_XScriptCsh -> "text/x-script.csh"
    Text_XScriptElisp -> "text/x-script.elisp"
    Text_XScriptGuile -> "text/x-script.guile"
    Text_XScriptKsh -> "text/x-script.ksh"
    Text_XScriptLisp -> "text/x-script.lisp"
    Text_XScriptPerl -> "text/x-script.perl"
    Text_XScriptPerlModule -> "text/x-script.perl-module"
    Text_XScriptPhyton -> "text/x-script.phyton"
    Text_XScriptRexx -> "text/x-script.rexx"
    Text_XScriptScheme -> "text/x-script.scheme"
    Text_XScriptSh -> "text/x-script.sh"
    Text_XScriptTcl -> "text/x-script.tcl"
    Text_XScriptTcsh -> "text/x-script.tcsh"
    Text_XScriptZsh -> "text/x-script.zsh"
    Text_XServerParsedHtml -> "text/x-server-parsed-html"
    Text_XSetext -> "text/x-setext"
    Text_XSfv -> "text/x-sfv"
    Text_XSgml -> "text/x-sgml"
    Text_XSh -> "text/x-sh"
    Text_XSpeech -> "text/x-speech"
    Text_XTcl -> "text/x-tcl"
    Text_XTex -> "text/x-tex"
    Text_XUil -> "text/x-uil"
    Text_XUuencode -> "text/x-uuencode"
    Text_XVcalendar -> "text/x-vcalendar"
    Text_XVcard -> "text/x-vcard"
    Text_Xml -> "text/xml"
    Text_XmlExternalParsedEntity -> "text/xml-external-parsed-entity"
    Unknown_Unknown -> "unknown/unknown"
    Video_1dInterleavedParityfec -> "video/1d-interleaved-parityfec"
    Video_3gpp -> "video/3gpp"
    Video_3gpp2 -> "video/3gpp2"
    Video_3gppTt -> "video/3gpp-tt"
    Video_Animaflex -> "video/animaflex"
    Video_Avi -> "video/avi"
    Video_AvsVideo -> "video/avs-video"
    Video_Bmpeg -> "video/bmpeg"
    Video_Bt656 -> "video/bt656"
    Video_Celb -> "video/celb"
    Video_Dl -> "video/dl"
    Video_Dv -> "video/dv"
    Video_Example -> "video/example"
    Video_Flc -> "video/flc"
    Video_Fli -> "video/fli"
    Video_Gl -> "video/gl"
    Video_H261 -> "video/h261"
    Video_H263 -> "video/h263"
    Video_H2631998 -> "video/h263-1998"
    Video_H2632000 -> "video/h263-2000"
    Video_H264 -> "video/h264"
    Video_H264Rcdo -> "video/h264-rcdo"
    Video_H264Svc -> "video/h264-svc"
    Video_Jpeg -> "video/jpeg"
    Video_Jpeg2000 -> "video/jpeg2000"
    Video_Jpm -> "video/jpm"
    Video_Mj2 -> "video/mj2"
    Video_Mp1s -> "video/mp1s"
    Video_Mp2p -> "video/mp2p"
    Video_Mp2t -> "video/MP2T"
    Video_Mp4 -> "video/mp4"
    Video_Mp4vEs -> "video/mp4v-es"
    Video_Mpeg -> "video/mpeg"
    Video_Mpeg4Generic -> "video/mpeg4-generic"
    Video_Mpv -> "video/mpv"
    Video_Msvideo -> "video/msvideo"
    Video_Nv -> "video/nv"
    Video_Ogg -> "video/ogg"
    Video_Parityfec -> "video/parityfec"
    Video_Pointer -> "video/pointer"
    Video_Quicktime -> "video/quicktime"
    Video_Raw -> "video/raw"
    Video_RtpEncAescm128 -> "video/rtp-enc-aescm128"
    Video_Rtx -> "video/rtx"
    Video_Smpte292m -> "video/smpte292m"
    Video_Ulpfec -> "video/ulpfec"
    Video_Vc1 -> "video/vc1"
    Video_Vdo -> "video/vdo"
    Video_Vivo -> "video/vivo"
    Video_VndCctv -> "video/vnd.cctv"
    Video_VndDeceHd -> "video/vnd.dece.hd"
    Video_VndDeceMobile -> "video/vnd.dece.mobile"
    Video_VndDeceMp4 -> "video/vnd.dece.mp4"
    Video_VndDecePd -> "video/vnd.dece.pd"
    Video_VndDeceSd -> "video/vnd.dece.sd"
    Video_VndDeceVideo -> "video/vnd.dece.video"
    Video_VndDirectvMpeg -> "video/vnd.directv.mpeg"
    Video_VndDirectvMpegTts -> "video/vnd.directv.mpeg-tts"
    Video_VndDlnaMpegTts -> "video/vnd.dlna.mpeg-tts"
    Video_VndDvbFile -> "video/vnd.dvb.file"
    Video_VndFvt -> "video/vnd.fvt"
    Video_VndHnsVideo -> "video/vnd.hns.video"
    Video_VndIptvforum1dparityfec1010 -> "video/vnd.iptvforum.1dparityfec-1010"
    Video_VndIptvforum1dparityfec2005 -> "video/vnd.iptvforum.1dparityfec-2005"
    Video_VndIptvforum2dparityfec1010 -> "video/vnd.iptvforum.2dparityfec-1010"
    Video_VndIptvforum2dparityfec2005 -> "video/vnd.iptvforum.2dparityfec-2005"
    Video_VndIptvforumTtsavc -> "video/vnd.iptvforum.ttsavc"
    Video_VndIptvforumTtsmpeg2 -> "video/vnd.iptvforum.ttsmpeg2"
    Video_VndMotorolaVideo -> "video/vnd.motorola.video"
    Video_VndMotorolaVideop -> "video/vnd.motorola.videop"
    Video_VndMpegurl -> "video/vnd.mpegurl"
    Video_VndMsPlayreadyMediaPyv -> "video/vnd.ms-playready.media.pyv"
    Video_VndMts -> "video/vnd.mts"
    Video_VndNokiaInterleavedMultimedia -> "video/vnd.nokia.interleaved-multimedia"
    Video_VndNokiaVideovoip -> "video/vnd.nokia.videovoip"
    Video_VndObjectvideo -> "video/vnd.objectvideo"
    Video_VndRnRealvideo -> "video/vnd.rn-realvideo"
    Video_VndSealedMpeg1 -> "video/vnd.sealed.mpeg1"
    Video_VndSealedMpeg4 -> "video/vnd.sealed.mpeg4"
    Video_VndSealedSwf -> "video/vnd.sealed.swf"
    Video_VndSealedmediaSoftsealMov -> "video/vnd.sealedmedia.softseal.mov"
    Video_VndUvvuMp4 -> "video/vnd.uvvu.mp4"
    Video_VndVivo -> "video/vnd.vivo"
    Video_Vosaic -> "video/vosaic"
    Video_Webm -> "video/webm"
    Video_XAmtDemorun -> "video/x-amt-demorun"
    Video_XAmtShowrun -> "video/x-amt-showrun"
    Video_XAtomic3dFeature -> "video/x-atomic3d-feature"
    Video_XDl -> "video/x-dl"
    Video_XDv -> "video/x-dv"
    Video_XF4v -> "video/x-f4v"
    Video_XFli -> "video/x-fli"
    Video_XFlv -> "video/x-flv"
    Video_XGl -> "video/x-gl"
    Video_XIsvideo -> "video/x-isvideo"
    Video_XLaAsf -> "video/x-la-asf"
    Video_XM4v -> "video/x-m4v"
    Video_XMatroska -> "video/x-matroska"
    Video_XMng -> "video/x-mng"
    Video_XMotionJpeg -> "video/x-motion-jpeg"
    Video_XMpeg -> "video/x-mpeg"
    Video_XMpeq2a -> "video/x-mpeq2a"
    Video_XMsAsf -> "video/x-ms-asf"
    Video_XMsAsfPlugin -> "video/x-ms-asf-plugin"
    Video_XMsVob -> "video/x-ms-vob"
    Video_XMsWm -> "video/x-ms-wm"
    Video_XMsWmv -> "video/x-ms-wmv"
    Video_XMsWmx -> "video/x-ms-wmx"
    Video_XMsWvx -> "video/x-ms-wvx"
    Video_XMsvideo -> "video/x-msvideo"
    Video_XQtc -> "video/x-qtc"
    Video_XScm -> "video/x-scm"
    Video_XSgiMovie -> "video/x-sgi-movie"
    Video_XSmv -> "video/x-smv"
    Windows_Metafile -> "windows/metafile"
    Www_Mime -> "www/mime"
    XConference_XCooltalk -> "x-conference/x-cooltalk"
    XMusic_XMidi -> "x-music/x-midi"
    XWorld_X3dmf -> "x-world/x-3dmf"
    XWorld_XSvr -> "x-world/x-svr"
    XWorld_XVrml -> "x-world/x-vrml"
    XWorld_XVrt -> "x-world/x-vrt"
    Xgl_Drawing -> "xgl/drawing"
    Xgl_Movie -> "xgl/movie"

mimeTypeExtensions :: MIME_Type -> [T.Text]
mimeTypeExtensions mimeType =
  case mimeType of
    Application_1dInterleavedParityfec -> []
    Application_3gppImsXml -> []
    Application_Acad -> [ "dwg" ]
    Application_Activemessage -> []
    Application_AndrewInset -> [ "ez" ]
    Application_Applefile -> []
    Application_Applixware -> [ "aw" ]
    Application_Arj -> [ "arj" ]
    Application_AtomXml -> [ "atom", "xml" ]
    Application_AtomcatXml -> [ "atomcat" ]
    Application_Atomicmail -> []
    Application_AtomsvcXml -> [ "atomsvc" ]
    Application_AuthPolicyXml -> []
    Application_Base64 -> [ "mm", "mme" ]
    Application_BatchSmtp -> []
    Application_BeepXml -> []
    Application_Binhex -> [ "hqx" ]
    Application_Binhex4 -> [ "hqx" ]
    Application_Book -> [ "boo", "book" ]
    Application_CalendarXml -> []
    Application_Cals1840 -> []
    Application_CcmpXml -> []
    Application_CcxmlXml -> [ "ccxml" ]
    Application_Cdf -> [ "cdf" ]
    Application_CdmiCapability -> [ "cdmia" ]
    Application_CdmiContainer -> [ "cdmic" ]
    Application_CdmiDomain -> [ "cdmid" ]
    Application_CdmiObject -> [ "cdmio" ]
    Application_CdmiQueue -> [ "cdmiq" ]
    Application_Cea2018Xml -> []
    Application_CellmlXml -> []
    Application_Cfw -> []
    Application_Clariscad -> [ "ccad" ]
    Application_CnrpXml -> []
    Application_Commonground -> [ "dp" ]
    Application_ConferenceInfoXml -> []
    Application_CplXml -> []
    Application_CstaXml -> []
    Application_CstadataXml -> []
    Application_CuSeeme -> [ "cu", "csm" ]
    Application_Cybercash -> []
    Application_DavmountXml -> [ "davmount" ]
    Application_DcaRft -> []
    Application_DecDx -> []
    Application_DialogInfoXml -> []
    Application_Dicom -> []
    Application_Dns -> []
    Application_DocbookXml -> [ "dbk" ]
    Application_Drafting -> [ "drw" ]
    Application_DskppXml -> []
    Application_Dsptype -> [ "tsp" ]
    Application_DsscDer -> [ "dssc" ]
    Application_DsscXml -> [ "xdssc" ]
    Application_Dvcs -> []
    Application_Dxf -> [ "dxf" ]
    Application_Ecmascript -> [ "es", "ecma", "js" ]
    Application_EdiConsent -> []
    Application_EdiX12 -> []
    Application_Edifact -> []
    Application_EmmaXml -> [ "emma" ]
    Application_Envoy -> [ "evy" ]
    Application_EppXml -> []
    Application_EpubZip -> [ "epub" ]
    Application_Eshop -> []
    Application_Example -> []
    Application_Excel -> [ "xl", "xla", "xlb", "xlc", "xld", "xlk", "xll", "xlm", "xls", "xlt", "xlv", "xlw" ]
    Application_Exi -> [ "exi" ]
    Application_Fastinfoset -> []
    Application_Fastsoap -> []
    Application_Fits -> []
    Application_FontTdpfr -> [ "pfr" ]
    Application_FontWoff -> [ "woff" ]
    Application_Fractals -> [ "fif" ]
    Application_FrameworkAttributesXml -> []
    Application_Freeloader -> [ "frl" ]
    Application_Futuresplash -> [ "spl" ]
    Application_Ghostview -> []
    Application_GmlXml -> [ "gml" ]
    Application_Gnutar -> [ "tgz" ]
    Application_GpxXml -> [ "gpx" ]
    Application_Groupwise -> [ "vew" ]
    Application_Gxf -> [ "gxf" ]
    Application_H224 -> []
    Application_HeldXml -> []
    Application_Hlp -> [ "hlp" ]
    Application_Hta -> [ "hta" ]
    Application_Http -> []
    Application_Hyperstudio -> [ "stk" ]
    Application_IDeas -> [ "unv" ]
    Application_IbeKeyRequestXml -> []
    Application_IbePkgReplyXml -> []
    Application_IbePpData -> []
    Application_Iges -> [ "iges", "igs" ]
    Application_ImIscomposingXml -> []
    Application_Index -> []
    Application_IndexCmd -> []
    Application_IndexObj -> []
    Application_IndexResponse -> []
    Application_IndexVnd -> []
    Application_Inf -> [ "inf" ]
    Application_InkmlXml -> [ "ink", "inkml" ]
    Application_InternetPropertyStream -> [ "acx" ]
    Application_Iotp -> []
    Application_Ipfix -> [ "ipfix" ]
    Application_Ipp -> []
    Application_Isup -> []
    Application_Java -> [ "class" ]
    Application_JavaArchive -> [ "jar" ]
    Application_JavaByteCode -> [ "class" ]
    Application_JavaSerializedObject -> [ "ser" ]
    Application_JavaVm -> [ "class" ]
    Application_Javascript -> [ "js" ]
    Application_Json -> [ "json" ]
    Application_JsonmlJson -> [ "jsonml" ]
    Application_KpmlRequestXml -> []
    Application_KpmlResponseXml -> []
    Application_Lha -> [ "lha" ]
    Application_LostXml -> [ "lostxml" ]
    Application_Lzx -> [ "lzx" ]
    Application_MacBinary -> [ "bin" ]
    Application_MacBinhex -> [ "hqx" ]
    Application_MacBinhex40 -> [ "hqx" ]
    Application_MacCompactpro -> [ "cpt" ]
    Application_Macbinary -> [ "bin" ]
    Application_Macwriteii -> []
    Application_MadsXml -> [ "mads" ]
    Application_Marc -> [ "mrc" ]
    Application_MarcxmlXml -> [ "mrcx" ]
    Application_Mathematica -> [ "ma", "nb", "mb" ]
    Application_MathematicaOld -> []
    Application_MathmlContentXml -> []
    Application_MathmlPresentationXml -> []
    Application_MathmlXml -> [ "mathml" ]
    Application_Mbedlet -> [ "mbd" ]
    Application_MbmsAssociatedProcedureDescriptionXml -> []
    Application_MbmsDeregisterXml -> []
    Application_MbmsEnvelopeXml -> []
    Application_MbmsMskResponseXml -> []
    Application_MbmsMskXml -> []
    Application_MbmsProtectionDescriptionXml -> []
    Application_MbmsReceptionReportXml -> []
    Application_MbmsRegisterResponseXml -> []
    Application_MbmsRegisterXml -> []
    Application_MbmsUserServiceDescriptionXml -> []
    Application_Mbox -> [ "mbox" ]
    Application_Mcad -> [ "mcd" ]
    Application_Media_controlXml -> []
    Application_MediaservercontrolXml -> [ "mscml" ]
    Application_Metalink4Xml -> [ "meta4" ]
    Application_MetalinkXml -> [ "metalink" ]
    Application_MetsXml -> [ "mets" ]
    Application_Mikey -> []
    Application_Mime -> [ "aps" ]
    Application_ModsXml -> [ "mods" ]
    Application_MossKeys -> []
    Application_MossSignature -> []
    Application_MosskeyData -> []
    Application_MosskeyRequest -> []
    Application_Mp21 -> [ "m21", "mp21" ]
    Application_Mp4 -> [ "mp4", "m4p", "mp4s" ]
    Application_Mpeg4Generic -> []
    Application_Mpeg4Iod -> []
    Application_Mpeg4IodXmt -> []
    Application_Msaccess -> [ "mdb" ]
    Application_MscIvrXml -> []
    Application_MscMixerXml -> []
    Application_Msonenote -> [ "one", "onetoc2", "onetmp", "onepkg" ]
    Application_Mspowerpoint -> [ "pot", "pps", "ppt", "ppz" ]
    Application_Msword -> [ "doc", "dot", "w6w", "wiz", "word" ]
    Application_Mswrite -> [ "wri" ]
    Application_Mxf -> [ "mxf" ]
    Application_Nasdata -> []
    Application_Netmc -> [ "mcp" ]
    Application_NewsCheckgroups -> []
    Application_NewsGroupinfo -> []
    Application_NewsMessageId -> []
    Application_NewsTransmission -> []
    Application_Nss -> []
    Application_OcspRequest -> []
    Application_OcspResponse -> []
    Application_OctetStream -> [ "bin", "dms", "lrf", "mar", "so", "dist", "distz", "pkg", "bpk", "dump", "elc", "a", "arc", "arj", "com", "exe", "lha", "lhx", "lzh", "lzx", "o", "psd", "saveme", "uu", "zoo", "class", "buffer", "deploy", "hqx", "obj", "lib", "zip", "gz", "dmg", "iso" ]
    Application_Oda -> [ "oda" ]
    Application_OebpsPackageXml -> [ "opf" ]
    Application_Ogg -> [ "ogx", "ogg" ]
    Application_Olescript -> [ "axs" ]
    Application_OmdocXml -> [ "omdoc" ]
    Application_Onenote -> [ "onetoc", "onetoc2", "onetmp", "onepkg" ]
    Application_Oxps -> [ "oxps" ]
    Application_Parityfec -> []
    Application_PatchOpsErrorXml -> [ "xer" ]
    Application_Pdf -> [ "pdf" ]
    Application_PgpEncrypted -> [ "pgp" ]
    Application_PgpKeys -> [ "key" ]
    Application_PgpSignature -> [ "asc", "pgp", "sig" ]
    Application_PicsRules -> [ "prf" ]
    Application_PidfDiffXml -> []
    Application_PidfXml -> []
    Application_Pkcs10 -> [ "p10" ]
    Application_Pkcs12 -> [ "p12" ]
    Application_Pkcs7Mime -> [ "p7m", "p7c" ]
    Application_Pkcs7Signature -> [ "p7s" ]
    Application_Pkcs8 -> [ "p8" ]
    Application_PkcsCrl -> [ "crl" ]
    Application_PkixAttrCert -> [ "ac" ]
    Application_PkixCert -> [ "cer", "crt" ]
    Application_PkixCrl -> [ "crl" ]
    Application_PkixPkipath -> [ "pkipath" ]
    Application_Pkixcmp -> [ "pki" ]
    Application_Plain -> [ "text" ]
    Application_PlsXml -> [ "pls" ]
    Application_PocSettingsXml -> []
    Application_Postscript -> [ "ai", "eps", "ps" ]
    Application_Powerpoint -> [ "ppt" ]
    Application_Pro_eng -> [ "part", "prt" ]
    Application_PrsAlvestrandTitraxSheet -> []
    Application_PrsCww -> [ "cww" ]
    Application_PrsNprend -> []
    Application_PrsPlucker -> []
    Application_PrsRdfXmlCrypt -> []
    Application_PrsXsfXml -> []
    Application_PskcXml -> [ "pskcxml" ]
    Application_Qsig -> []
    Application_Rar -> [ "rar" ]
    Application_RdfXml -> [ "rdf" ]
    Application_ReginfoXml -> [ "rif" ]
    Application_RelaxNgCompactSyntax -> [ "rnc" ]
    Application_RemotePrinting -> []
    Application_ResourceListsDiffXml -> [ "rld" ]
    Application_ResourceListsXml -> [ "rl" ]
    Application_RingingTones -> [ "rng" ]
    Application_Riscos -> []
    Application_RlmiXml -> []
    Application_RlsServicesXml -> [ "rs" ]
    Application_RpkiGhostbusters -> [ "gbr" ]
    Application_RpkiManifest -> [ "mft" ]
    Application_RpkiRoa -> [ "roa" ]
    Application_RpkiUpdown -> []
    Application_RsdXml -> [ "rsd" ]
    Application_RssXml -> [ "rss", "xml" ]
    Application_Rtf -> [ "rtf", "rtx" ]
    Application_Rtx -> []
    Application_SamlassertionXml -> []
    Application_SamlmetadataXml -> []
    Application_SbmlXml -> [ "sbml" ]
    Application_ScvpCvRequest -> [ "scq" ]
    Application_ScvpCvResponse -> [ "scs" ]
    Application_ScvpVpRequest -> [ "spq" ]
    Application_ScvpVpResponse -> [ "spp" ]
    Application_Sdp -> [ "sdp" ]
    Application_Sea -> [ "sea" ]
    Application_Set -> [ "set" ]
    Application_SetPayment -> []
    Application_SetPaymentInitiation -> [ "setpay" ]
    Application_SetRegistration -> []
    Application_SetRegistrationInitiation -> [ "setreg" ]
    Application_Sgml -> []
    Application_SgmlOpenCatalog -> []
    Application_ShfXml -> [ "shf" ]
    Application_Sieve -> []
    Application_SimpleFilterXml -> []
    Application_SimpleMessageSummary -> []
    Application_Simplesymbolcontainer -> []
    Application_Sla -> [ "stl" ]
    Application_Slate -> []
    Application_Smil -> [ "smi", "smil" ]
    Application_SmilXml -> [ "smi", "smil" ]
    Application_SoapFastinfoset -> []
    Application_SoapXml -> []
    Application_Solids -> [ "sol" ]
    Application_Sounder -> [ "sdr" ]
    Application_SparqlQuery -> [ "rq" ]
    Application_SparqlResultsXml -> [ "srx" ]
    Application_SpiritsEventXml -> []
    Application_Srgs -> [ "gram" ]
    Application_SrgsXml -> [ "grxml" ]
    Application_SruXml -> [ "sru" ]
    Application_SsdlXml -> [ "ssdl" ]
    Application_SsmlXml -> [ "ssml" ]
    Application_Step -> [ "step", "stp" ]
    Application_Streamingmedia -> [ "ssm" ]
    Application_TampApexUpdate -> []
    Application_TampApexUpdateConfirm -> []
    Application_TampCommunityUpdate -> []
    Application_TampCommunityUpdateConfirm -> []
    Application_TampError -> []
    Application_TampSequenceAdjust -> []
    Application_TampSequenceAdjustConfirm -> []
    Application_TampStatusQuery -> []
    Application_TampStatusResponse -> []
    Application_TampUpdate -> []
    Application_TampUpdateConfirm -> []
    Application_TeiXml -> [ "tei", "teicorpus" ]
    Application_ThraudXml -> [ "tfi" ]
    Application_TimestampQuery -> []
    Application_TimestampReply -> []
    Application_TimestampedData -> [ "tsd" ]
    Application_Toolbook -> [ "tbk" ]
    Application_TveTrigger -> []
    Application_Ulpfec -> []
    Application_VcardXml -> []
    Application_Vda -> [ "vda" ]
    Application_Vemmi -> []
    Application_VividenceScriptfile -> []
    Application_Vnd3gpp2BcmcsinfoXml -> []
    Application_Vnd3gpp2Sms -> []
    Application_Vnd3gpp2Tcap -> [ "tcap" ]
    Application_Vnd3gppBsfXml -> []
    Application_Vnd3gppPicBwLarge -> [ "plb" ]
    Application_Vnd3gppPicBwSmall -> [ "psb" ]
    Application_Vnd3gppPicBwVar -> [ "pvb" ]
    Application_Vnd3gppSms -> []
    Application_Vnd3mPostItNotes -> [ "pwn" ]
    Application_VndAccpacSimplyAso -> [ "aso" ]
    Application_VndAccpacSimplyImp -> [ "imp" ]
    Application_VndAcucobol -> [ "acu" ]
    Application_VndAcucorp -> [ "atc", "acutc" ]
    Application_VndAdobeAirApplicationInstallerPackageZip -> [ "air" ]
    Application_VndAdobeFormscentralFcdt -> [ "fcdt" ]
    Application_VndAdobeFxp -> [ "fxp", "fxpl" ]
    Application_VndAdobePartialUpload -> []
    Application_VndAdobeXdpXml -> [ "xdp" ]
    Application_VndAdobeXfdf -> [ "xfdf" ]
    Application_VndAetherImp -> []
    Application_VndAhBarcode -> []
    Application_VndAheadSpace -> [ "ahead" ]
    Application_VndAirzipFilesecureAzf -> [ "azf" ]
    Application_VndAirzipFilesecureAzs -> [ "azs" ]
    Application_VndAmazonEbook -> [ "azw" ]
    Application_VndAmericandynamicsAcc -> [ "acc" ]
    Application_VndAmigaAmi -> [ "ami" ]
    Application_VndAmundsenMazeXml -> []
    Application_VndAndroidPackageArchive -> [ "apk" ]
    Application_VndAnserWebCertificateIssueInitiation -> [ "cii" ]
    Application_VndAnserWebFundsTransferInitiation -> [ "fti" ]
    Application_VndAntixGameComponent -> [ "atx" ]
    Application_VndAppleInstallerXml -> [ "mpkg" ]
    Application_VndAppleMpegurl -> [ "m3u8" ]
    Application_VndArastraSwi -> [ "swi" ]
    Application_VndAristanetworksSwi -> [ "swi" ]
    Application_VndAstraeaSoftwareIota -> [ "iota" ]
    Application_VndAudiograph -> [ "aep" ]
    Application_VndAutopackage -> []
    Application_VndAvistarXml -> []
    Application_VndBlueiceMultipass -> [ "mpm" ]
    Application_VndBluetoothEpOob -> []
    Application_VndBmi -> [ "bmi" ]
    Application_VndBusinessobjects -> [ "rep" ]
    Application_VndCabJscript -> []
    Application_VndCanonCpdl -> []
    Application_VndCanonLips -> []
    Application_VndCendioThinlincClientconf -> []
    Application_VndChemdrawXml -> [ "cdxml" ]
    Application_VndChipnutsKaraokeMmd -> [ "mmd" ]
    Application_VndCinderella -> [ "cdy" ]
    Application_VndCirpackIsdnExt -> []
    Application_VndClaymore -> [ "cla" ]
    Application_VndCloantoRp9 -> [ "rp9" ]
    Application_VndClonkC4group -> [ "c4g", "c4d", "c4f", "c4p", "c4u" ]
    Application_VndCluetrustCartomobileConfig -> [ "c11amc" ]
    Application_VndCluetrustCartomobileConfigPkg -> [ "c11amz" ]
    Application_VndCollectionJson -> []
    Application_VndCommerceBattelle -> []
    Application_VndCommonspace -> [ "csp" ]
    Application_VndComsocaller -> []
    Application_VndContactCmsg -> [ "cdbcmsg" ]
    Application_VndCosmocaller -> [ "cmc" ]
    Application_VndCrickClicker -> [ "clkx" ]
    Application_VndCrickClickerKeyboard -> [ "clkk" ]
    Application_VndCrickClickerPalette -> [ "clkp" ]
    Application_VndCrickClickerTemplate -> [ "clkt" ]
    Application_VndCrickClickerWordbank -> [ "clkw" ]
    Application_VndCriticaltoolsWbsXml -> [ "wbs" ]
    Application_VndCtcPosml -> [ "pml" ]
    Application_VndCtctWsXml -> []
    Application_VndCupsPdf -> []
    Application_VndCupsPostscript -> []
    Application_VndCupsPpd -> [ "ppd" ]
    Application_VndCupsRaster -> []
    Application_VndCupsRaw -> []
    Application_VndCurl -> []
    Application_VndCurlCar -> [ "car" ]
    Application_VndCurlPcurl -> [ "pcurl" ]
    Application_VndCybank -> []
    Application_VndDart -> [ "dart" ]
    Application_VndDataVisionRdz -> [ "rdz" ]
    Application_VndDeceData -> [ "uvf", "uvvf", "uvd", "uvvd" ]
    Application_VndDeceTtmlXml -> [ "uvt", "uvvt" ]
    Application_VndDeceUnspecified -> [ "uvx", "uvvx" ]
    Application_VndDeceZip -> [ "uvz", "uvvz" ]
    Application_VndDenovoFcselayoutLink -> [ "fe_launch" ]
    Application_VndDirBiPlateDlNosuffix -> []
    Application_VndDna -> [ "dna" ]
    Application_VndDolbyMlp -> [ "mlp" ]
    Application_VndDolbyMobile1 -> []
    Application_VndDolbyMobile2 -> []
    Application_VndDpgraph -> [ "dpg" ]
    Application_VndDreamfactory -> [ "dfac" ]
    Application_VndDsKeypoint -> [ "kpxx" ]
    Application_VndDvbAit -> [ "ait" ]
    Application_VndDvbDvbj -> []
    Application_VndDvbEsgcontainer -> []
    Application_VndDvbIpdcdftnotifaccess -> []
    Application_VndDvbIpdcesgaccess -> []
    Application_VndDvbIpdcesgaccess2 -> []
    Application_VndDvbIpdcesgpdd -> []
    Application_VndDvbIpdcroaming -> []
    Application_VndDvbIptvAlfecBase -> []
    Application_VndDvbIptvAlfecEnhancement -> []
    Application_VndDvbNotifAggregateRootXml -> []
    Application_VndDvbNotifContainerXml -> []
    Application_VndDvbNotifGenericXml -> []
    Application_VndDvbNotifIaMsglistXml -> []
    Application_VndDvbNotifIaRegistrationRequestXml -> []
    Application_VndDvbNotifIaRegistrationResponseXml -> []
    Application_VndDvbNotifInitXml -> []
    Application_VndDvbPfr -> []
    Application_VndDvbService -> [ "svc" ]
    Application_VndDxr -> []
    Application_VndDynageo -> [ "geo" ]
    Application_VndEasykaraokeCdgdownload -> []
    Application_VndEcdisUpdate -> []
    Application_VndEcowinChart -> [ "mag" ]
    Application_VndEcowinFilerequest -> []
    Application_VndEcowinFileupdate -> []
    Application_VndEcowinSeries -> []
    Application_VndEcowinSeriesrequest -> []
    Application_VndEcowinSeriesupdate -> []
    Application_VndEmclientAccessrequestXml -> []
    Application_VndEnliven -> [ "nml" ]
    Application_VndEprintsDataXml -> []
    Application_VndEpsonEsf -> [ "esf" ]
    Application_VndEpsonMsf -> [ "msf" ]
    Application_VndEpsonQuickanime -> [ "qam" ]
    Application_VndEpsonSalt -> [ "slt" ]
    Application_VndEpsonSsf -> [ "ssf" ]
    Application_VndEricssonQuickcall -> []
    Application_VndEszigno3Xml -> [ "es3", "et3" ]
    Application_VndEtsiAocXml -> []
    Application_VndEtsiCugXml -> []
    Application_VndEtsiIptvcommandXml -> []
    Application_VndEtsiIptvdiscoveryXml -> []
    Application_VndEtsiIptvprofileXml -> []
    Application_VndEtsiIptvsadBcXml -> []
    Application_VndEtsiIptvsadCodXml -> []
    Application_VndEtsiIptvsadNpvrXml -> []
    Application_VndEtsiIptvserviceXml -> []
    Application_VndEtsiIptvsyncXml -> []
    Application_VndEtsiIptvueprofileXml -> []
    Application_VndEtsiMcidXml -> []
    Application_VndEtsiOverloadControlPolicyDatasetXml -> []
    Application_VndEtsiSciXml -> []
    Application_VndEtsiSimservsXml -> []
    Application_VndEtsiTslDer -> []
    Application_VndEtsiTslXml -> []
    Application_VndEudoraData -> []
    Application_VndEzpixAlbum -> [ "ez2" ]
    Application_VndEzpixPackage -> [ "ez3" ]
    Application_VndFSecureMobile -> []
    Application_VndFdf -> [ "fdf" ]
    Application_VndFdsnMseed -> [ "mseed" ]
    Application_VndFdsnSeed -> [ "seed", "dataless" ]
    Application_VndFfsns -> []
    Application_VndFints -> []
    Application_VndFlographit -> [ "gph" ]
    Application_VndFluxtimeClip -> [ "ftc" ]
    Application_VndFontFontforgeSfd -> []
    Application_VndFramemaker -> [ "fm", "frame", "maker", "book" ]
    Application_VndFrogansFnc -> [ "fnc" ]
    Application_VndFrogansLtf -> [ "ltf" ]
    Application_VndFscWeblaunch -> [ "fsc" ]
    Application_VndFujitsuOasys -> [ "oas" ]
    Application_VndFujitsuOasys2 -> [ "oa2" ]
    Application_VndFujitsuOasys3 -> [ "oa3" ]
    Application_VndFujitsuOasysgp -> [ "fg5" ]
    Application_VndFujitsuOasysprs -> [ "bh2" ]
    Application_VndFujixeroxArt4 -> []
    Application_VndFujixeroxArtEx -> []
    Application_VndFujixeroxDdd -> [ "ddd" ]
    Application_VndFujixeroxDocuworks -> [ "xdw" ]
    Application_VndFujixeroxDocuworksBinder -> [ "xbd" ]
    Application_VndFujixeroxHbpl -> []
    Application_VndFutMisnet -> []
    Application_VndFuzzysheet -> [ "fzs" ]
    Application_VndGenomatixTuxedo -> [ "txd" ]
    Application_VndGeocubeXml -> []
    Application_VndGeogebraFile -> [ "ggb" ]
    Application_VndGeogebraTool -> [ "ggt" ]
    Application_VndGeometryExplorer -> [ "gex", "gre" ]
    Application_VndGeonext -> [ "gxt" ]
    Application_VndGeoplan -> [ "g2w" ]
    Application_VndGeospace -> [ "g3w" ]
    Application_VndGlobalplatformCardContentMgt -> []
    Application_VndGlobalplatformCardContentMgtResponse -> []
    Application_VndGmx -> [ "gmx" ]
    Application_VndGoogleEarthKmlXml -> [ "kml" ]
    Application_VndGoogleEarthKmz -> [ "kmz" ]
    Application_VndGrafeq -> [ "gqf", "gqs" ]
    Application_VndGridmp -> []
    Application_VndGrooveAccount -> [ "gac" ]
    Application_VndGrooveHelp -> [ "ghf" ]
    Application_VndGrooveIdentityMessage -> [ "gim" ]
    Application_VndGrooveInjector -> [ "grv" ]
    Application_VndGrooveToolMessage -> [ "gtm" ]
    Application_VndGrooveToolTemplate -> [ "tpl" ]
    Application_VndGrooveVcard -> [ "vcg" ]
    Application_VndHalJson -> []
    Application_VndHalXml -> [ "hal" ]
    Application_VndHandheldEntertainmentXml -> [ "zmm" ]
    Application_VndHbci -> [ "hbci" ]
    Application_VndHclBireports -> []
    Application_VndHheLessonPlayer -> [ "les" ]
    Application_VndHpHpgl -> [ "hgl", "hpg", "hpgl" ]
    Application_VndHpHpid -> [ "hpid" ]
    Application_VndHpHps -> [ "hps" ]
    Application_VndHpJlyt -> [ "jlt" ]
    Application_VndHpPcl -> [ "pcl" ]
    Application_VndHpPclxl -> [ "pclxl" ]
    Application_VndHttphone -> []
    Application_VndHydrostatixSofData -> [ "sfd-hdstx" ]
    Application_VndHzn3dCrossword -> [ "x3d" ]
    Application_VndIbmAfplinedata -> []
    Application_VndIbmElectronicMedia -> []
    Application_VndIbmMinipay -> [ "mpy" ]
    Application_VndIbmModcap -> [ "afp", "listafp", "list3820" ]
    Application_VndIbmRightsManagement -> [ "irm" ]
    Application_VndIbmSecureContainer -> [ "sc" ]
    Application_VndIccprofile -> [ "icc", "icm" ]
    Application_VndIgloader -> [ "igl" ]
    Application_VndImmervisionIvp -> [ "ivp" ]
    Application_VndImmervisionIvu -> [ "ivu" ]
    Application_VndInformedcontrolRmsXml -> []
    Application_VndInformixVisionary -> []
    Application_VndInfotechProject -> []
    Application_VndInfotechProjectXml -> []
    Application_VndInnopathWampNotification -> []
    Application_VndInsorsIgm -> [ "igm" ]
    Application_VndInterconFormnet -> [ "xpw", "xpx" ]
    Application_VndIntergeo -> [ "i2g" ]
    Application_VndIntertrustDigibox -> []
    Application_VndIntertrustNncp -> []
    Application_VndIntuQbo -> [ "qbo" ]
    Application_VndIntuQfx -> [ "qfx" ]
    Application_VndIptcG2ConceptitemXml -> []
    Application_VndIptcG2KnowledgeitemXml -> []
    Application_VndIptcG2NewsitemXml -> []
    Application_VndIptcG2NewsmessageXml -> []
    Application_VndIptcG2PackageitemXml -> []
    Application_VndIptcG2PlanningitemXml -> []
    Application_VndIpunpluggedRcprofile -> [ "rcprofile" ]
    Application_VndIrepositoryPackageXml -> [ "irp" ]
    Application_VndIsXpr -> [ "xpr" ]
    Application_VndIsacFcs -> [ "fcs" ]
    Application_VndJam -> [ "jam" ]
    Application_VndJapannetDirectoryService -> []
    Application_VndJapannetJpnstoreWakeup -> []
    Application_VndJapannetPaymentWakeup -> []
    Application_VndJapannetRegistration -> []
    Application_VndJapannetRegistrationWakeup -> []
    Application_VndJapannetSetstoreWakeup -> []
    Application_VndJapannetVerification -> []
    Application_VndJapannetVerificationWakeup -> []
    Application_VndJcpJavameMidletRms -> [ "rms" ]
    Application_VndJisp -> [ "jisp" ]
    Application_VndJoostJodaArchive -> [ "joda" ]
    Application_VndKahootz -> [ "ktz", "ktr" ]
    Application_VndKdeKarbon -> [ "karbon" ]
    Application_VndKdeKchart -> [ "chrt" ]
    Application_VndKdeKformula -> [ "kfo" ]
    Application_VndKdeKivio -> [ "flw" ]
    Application_VndKdeKontour -> [ "kon" ]
    Application_VndKdeKpresenter -> [ "kpr", "kpt" ]
    Application_VndKdeKspread -> [ "ksp" ]
    Application_VndKdeKword -> [ "kwd", "kwt" ]
    Application_VndKenameaapp -> [ "htke" ]
    Application_VndKidspiration -> [ "kia" ]
    Application_VndKinar -> [ "kne", "knp" ]
    Application_VndKoan -> [ "skp", "skd", "skt", "skm" ]
    Application_VndKodakDescriptor -> [ "sse" ]
    Application_VndLasLasXml -> [ "lasxml" ]
    Application_VndLibertyRequestXml -> []
    Application_VndLlamagraphicsLifeBalanceDesktop -> [ "lbd" ]
    Application_VndLlamagraphicsLifeBalanceExchangeXml -> [ "lbe" ]
    Application_VndLotus123 -> [ "123" ]
    Application_VndLotusApproach -> [ "apr" ]
    Application_VndLotusFreelance -> [ "pre" ]
    Application_VndLotusNotes -> [ "nsf" ]
    Application_VndLotusOrganizer -> [ "org" ]
    Application_VndLotusScreencam -> [ "scm" ]
    Application_VndLotusWordpro -> [ "lwp" ]
    Application_VndMacportsPortpkg -> [ "portpkg" ]
    Application_VndMarlinDrmActiontokenXml -> []
    Application_VndMarlinDrmConftokenXml -> []
    Application_VndMarlinDrmLicenseXml -> []
    Application_VndMarlinDrmMdcf -> []
    Application_VndMcd -> [ "mcd" ]
    Application_VndMedcalcdata -> [ "mc1" ]
    Application_VndMediastationCdkey -> [ "cdkey" ]
    Application_VndMeridianSlingshot -> []
    Application_VndMfer -> [ "mwf" ]
    Application_VndMfmp -> [ "mfm" ]
    Application_VndMicrografxFlo -> [ "flo" ]
    Application_VndMicrografxIgx -> [ "igx" ]
    Application_VndMif -> [ "mif" ]
    Application_VndMinisoftHp3000Save -> []
    Application_VndMitsubishiMistyGuardTrustweb -> []
    Application_VndMobiusDaf -> [ "daf" ]
    Application_VndMobiusDis -> [ "dis" ]
    Application_VndMobiusMbk -> [ "mbk" ]
    Application_VndMobiusMqy -> [ "mqy" ]
    Application_VndMobiusMsl -> [ "msl" ]
    Application_VndMobiusPlc -> [ "plc" ]
    Application_VndMobiusTxf -> [ "txf" ]
    Application_VndMophunApplication -> [ "mpn" ]
    Application_VndMophunCertificate -> [ "mpc" ]
    Application_VndMotorolaFlexsuite -> []
    Application_VndMotorolaFlexsuiteAdsi -> []
    Application_VndMotorolaFlexsuiteFis -> []
    Application_VndMotorolaFlexsuiteGotap -> []
    Application_VndMotorolaFlexsuiteKmr -> []
    Application_VndMotorolaFlexsuiteTtc -> []
    Application_VndMotorolaFlexsuiteWem -> []
    Application_VndMotorolaIprm -> []
    Application_VndMozillaXulXml -> [ "xul" ]
    Application_VndMsArtgalry -> [ "cil" ]
    Application_VndMsAsf -> []
    Application_VndMsCabCompressed -> [ "cab" ]
    Application_VndMsColorIccprofile -> []
    Application_VndMsExcel -> [ "xls", "xlm", "xla", "xlc", "xlt", "xlb", "xll", "xlw" ]
    Application_VndMsExcelAddinMacroenabled12 -> [ "xlam", "xlam" ]
    Application_VndMsExcelSheetBinaryMacroenabled12 -> [ "xlsb", "xlsb" ]
    Application_VndMsExcelSheetMacroenabled12 -> [ "xlsm", "xlsm" ]
    Application_VndMsExcelTemplateMacroenabled12 -> [ "xltm", "xltm" ]
    Application_VndMsFontobject -> [ "eot" ]
    Application_VndMsHtmlhelp -> [ "chm" ]
    Application_VndMsIms -> [ "ims" ]
    Application_VndMsLrm -> [ "lrm" ]
    Application_VndMsOfficeActivexXml -> []
    Application_VndMsOfficetheme -> [ "thmx" ]
    Application_VndMsOpentype -> []
    Application_VndMsOutlook -> [ "msg" ]
    Application_VndMsPackageObfuscatedOpentype -> []
    Application_VndMsPkiCertstore -> [ "sst" ]
    Application_VndMsPkiPko -> [ "pko" ]
    Application_VndMsPkiSeccat -> [ "cat" ]
    Application_VndMsPkiStl -> [ "stl" ]
    Application_VndMsPkicertstore -> [ "sst" ]
    Application_VndMsPkiseccat -> [ "cat" ]
    Application_VndMsPkistl -> [ "stl" ]
    Application_VndMsPlayreadyInitiatorXml -> []
    Application_VndMsPowerpoint -> [ "ppt", "pps", "pot", "ppa", "pwz" ]
    Application_VndMsPowerpointAddinMacroenabled12 -> [ "ppam", "ppam" ]
    Application_VndMsPowerpointPresentationMacroenabled12 -> [ "pptm", "potm", "pptm", "potm" ]
    Application_VndMsPowerpointSlideMacroenabled12 -> [ "sldm", "sldm" ]
    Application_VndMsPowerpointSlideshowMacroenabled12 -> [ "ppsm", "ppsm" ]
    Application_VndMsPowerpointTemplateMacroenabled12 -> [ "potm", "potm" ]
    Application_VndMsPrintingPrintticketXml -> []
    Application_VndMsProject -> [ "mpp", "mpt" ]
    Application_VndMsTnef -> []
    Application_VndMsWmdrmLicChlgReq -> []
    Application_VndMsWmdrmLicResp -> []
    Application_VndMsWmdrmMeterChlgReq -> []
    Application_VndMsWmdrmMeterResp -> []
    Application_VndMsWordDocumentMacroenabled12 -> [ "docm", "docm" ]
    Application_VndMsWordTemplateMacroenabled12 -> [ "dotm", "dotm" ]
    Application_VndMsWorks -> [ "wps", "wks", "wcm", "wdb" ]
    Application_VndMsWpl -> [ "wpl" ]
    Application_VndMsXpsdocument -> [ "xps" ]
    Application_VndMseq -> [ "mseq" ]
    Application_VndMsign -> []
    Application_VndMultiadCreator -> []
    Application_VndMultiadCreatorCif -> []
    Application_VndMusicNiff -> []
    Application_VndMusician -> [ "mus" ]
    Application_VndMuveeStyle -> [ "msty" ]
    Application_VndMynfc -> [ "taglet" ]
    Application_VndNcdControl -> []
    Application_VndNcdReference -> []
    Application_VndNervana -> []
    Application_VndNetfpx -> []
    Application_VndNeurolanguageNlu -> [ "nlu" ]
    Application_VndNitf -> [ "ntf", "nitf" ]
    Application_VndNoblenetDirectory -> [ "nnd" ]
    Application_VndNoblenetSealer -> [ "nns" ]
    Application_VndNoblenetWeb -> [ "nnw" ]
    Application_VndNokiaCatalogs -> []
    Application_VndNokiaConfigurationMessage -> [ "ncm" ]
    Application_VndNokiaConmlWbxml -> []
    Application_VndNokiaConmlXml -> []
    Application_VndNokiaIptvConfigXml -> []
    Application_VndNokiaIsdsRadioPresets -> []
    Application_VndNokiaLandmarkWbxml -> []
    Application_VndNokiaLandmarkXml -> []
    Application_VndNokiaLandmarkcollectionXml -> []
    Application_VndNokiaNGageAcXml -> []
    Application_VndNokiaNGageData -> [ "ngdat" ]
    Application_VndNokiaNGageSymbianInstall -> [ "n-gage" ]
    Application_VndNokiaNcd -> []
    Application_VndNokiaPcdWbxml -> []
    Application_VndNokiaPcdXml -> []
    Application_VndNokiaRadioPreset -> [ "rpst" ]
    Application_VndNokiaRadioPresets -> [ "rpss" ]
    Application_VndNokiaRingingTone -> [ "rng" ]
    Application_VndNovadigmEdm -> [ "edm", "edm" ]
    Application_VndNovadigmEdx -> [ "edx", "edx" ]
    Application_VndNovadigmExt -> [ "ext", "ext" ]
    Application_VndNttLocalFileTransfer -> []
    Application_VndNttLocalSipTa_remote -> []
    Application_VndNttLocalSipTa_tcp_stream -> []
    Application_VndOasisOpendocumentChart -> [ "odc" ]
    Application_VndOasisOpendocumentChartTemplate -> [ "otc" ]
    Application_VndOasisOpendocumentDatabase -> [ "odb" ]
    Application_VndOasisOpendocumentFormula -> [ "odf" ]
    Application_VndOasisOpendocumentFormulaTemplate -> [ "odft" ]
    Application_VndOasisOpendocumentGraphics -> [ "odg" ]
    Application_VndOasisOpendocumentGraphicsTemplate -> [ "otg" ]
    Application_VndOasisOpendocumentImage -> [ "odi" ]
    Application_VndOasisOpendocumentImageTemplate -> [ "oti" ]
    Application_VndOasisOpendocumentPresentation -> [ "odp" ]
    Application_VndOasisOpendocumentPresentationTemplate -> [ "otp" ]
    Application_VndOasisOpendocumentSpreadsheet -> [ "ods" ]
    Application_VndOasisOpendocumentSpreadsheetTemplate -> [ "ots" ]
    Application_VndOasisOpendocumentText -> [ "odt" ]
    Application_VndOasisOpendocumentTextMaster -> [ "odm", "otm" ]
    Application_VndOasisOpendocumentTextTemplate -> [ "ott" ]
    Application_VndOasisOpendocumentTextWeb -> [ "oth" ]
    Application_VndObn -> []
    Application_VndOftnL10nJson -> []
    Application_VndOipfContentaccessdownloadXml -> []
    Application_VndOipfContentaccessstreamingXml -> []
    Application_VndOipfCspgHexbinary -> []
    Application_VndOipfDaeSvgXml -> []
    Application_VndOipfDaeXhtmlXml -> []
    Application_VndOipfMippvcontrolmessageXml -> []
    Application_VndOipfPaeGem -> []
    Application_VndOipfSpdiscoveryXml -> []
    Application_VndOipfSpdlistXml -> []
    Application_VndOipfUeprofileXml -> []
    Application_VndOipfUserprofileXml -> []
    Application_VndOlpcSugar -> [ "xo" ]
    Application_VndOmaBcastAssociatedProcedureParameterXml -> []
    Application_VndOmaBcastDrmTriggerXml -> []
    Application_VndOmaBcastImdXml -> []
    Application_VndOmaBcastLtkm -> []
    Application_VndOmaBcastNotificationXml -> []
    Application_VndOmaBcastProvisioningtrigger -> []
    Application_VndOmaBcastSgboot -> []
    Application_VndOmaBcastSgddXml -> []
    Application_VndOmaBcastSgdu -> []
    Application_VndOmaBcastSimpleSymbolContainer -> []
    Application_VndOmaBcastSmartcardTriggerXml -> []
    Application_VndOmaBcastSprovXml -> []
    Application_VndOmaBcastStkm -> []
    Application_VndOmaCabAddressBookXml -> []
    Application_VndOmaCabFeatureHandlerXml -> []
    Application_VndOmaCabPccXml -> []
    Application_VndOmaCabUserPrefsXml -> []
    Application_VndOmaDcd -> []
    Application_VndOmaDcdc -> []
    Application_VndOmaDd2Xml -> [ "dd2" ]
    Application_VndOmaDrmRisdXml -> []
    Application_VndOmaGroupUsageListXml -> []
    Application_VndOmaPalXml -> []
    Application_VndOmaPocDetailedProgressReportXml -> []
    Application_VndOmaPocFinalReportXml -> []
    Application_VndOmaPocGroupsXml -> []
    Application_VndOmaPocInvocationDescriptorXml -> []
    Application_VndOmaPocOptimizedProgressReportXml -> []
    Application_VndOmaPush -> []
    Application_VndOmaScidmMessagesXml -> []
    Application_VndOmaScwsConfig -> []
    Application_VndOmaScwsHttpRequest -> []
    Application_VndOmaScwsHttpResponse -> []
    Application_VndOmaXcapDirectoryXml -> []
    Application_VndOmadsEmailXml -> []
    Application_VndOmadsFileXml -> []
    Application_VndOmadsFolderXml -> []
    Application_VndOmalocSuplInit -> []
    Application_VndOpenofficeorgExtension -> [ "oxt" ]
    Application_VndOpenxmlformatsOfficedocumentCustomPropertiesXml -> []
    Application_VndOpenxmlformatsOfficedocumentCustomxmlpropertiesXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlChartshapesXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramcolorsXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramdataXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramlayoutXml -> []
    Application_VndOpenxmlformatsOfficedocumentDrawingmlDiagramstyleXml -> []
    Application_VndOpenxmlformatsOfficedocumentExtendedPropertiesXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentauthorsXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlCommentsXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlHandoutmasterXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesmasterXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlNotesslideXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentation -> [ "pptx" ]
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPresentationMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlPrespropsXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlide -> [ "sldx" ]
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidelayoutXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlidemasterXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshow -> [ "ppsx" ]
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideshowMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlSlideupdateinfoXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTablestylesXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTagsXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplate -> [ "potx" ]
    Application_VndOpenxmlformatsOfficedocumentPresentationmlTemplateMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentPresentationmlViewpropsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCalcchainXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlChartsheetXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlCommentsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlConnectionsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlDialogsheetXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlExternallinkXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcachedefinitionXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivotcacherecordsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlPivottableXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlQuerytableXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionheadersXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlRevisionlogXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSharedstringsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheet -> [ "xlsx" ]
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlSheetmetadataXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlStylesXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTableXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTablesinglecellsXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplate -> [ "xltx" ]
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlTemplateMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlUsernamesXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlVolatiledependenciesXml -> []
    Application_VndOpenxmlformatsOfficedocumentSpreadsheetmlWorksheetXml -> []
    Application_VndOpenxmlformatsOfficedocumentThemeXml -> []
    Application_VndOpenxmlformatsOfficedocumentThemeoverrideXml -> []
    Application_VndOpenxmlformatsOfficedocumentVmldrawing -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlCommentsXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocument -> [ "docx" ]
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentGlossaryXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlDocumentMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlEndnotesXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFonttableXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFooterXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlFootnotesXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlNumberingXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlSettingsXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlStylesXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplate -> [ "dotx" ]
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlTemplateMainXml -> []
    Application_VndOpenxmlformatsOfficedocumentWordprocessingmlWebsettingsXml -> []
    Application_VndOpenxmlformatsPackageCorePropertiesXml -> []
    Application_VndOpenxmlformatsPackageDigitalSignatureXmlsignatureXml -> []
    Application_VndOpenxmlformatsPackageRelationshipsXml -> []
    Application_VndOsaNetdeploy -> []
    Application_VndOsgeoMapguidePackage -> [ "mgp" ]
    Application_VndOsgiBundle -> []
    Application_VndOsgiDp -> [ "dp" ]
    Application_VndOsgiSubsystem -> [ "esa" ]
    Application_VndOtpsCtKipXml -> []
    Application_VndPalm -> [ "pdb", "pqa", "oprc" ]
    Application_VndPaosXml -> []
    Application_VndPawaafile -> [ "paw" ]
    Application_VndPgFormat -> [ "str" ]
    Application_VndPgOsasli -> [ "ei6" ]
    Application_VndPiaccessApplicationLicence -> []
    Application_VndPicsel -> [ "efif" ]
    Application_VndPmiWidget -> [ "wg" ]
    Application_VndPocGroupAdvertisementXml -> []
    Application_VndPocketlearn -> [ "plf" ]
    Application_VndPowerbuilder6 -> [ "pbd" ]
    Application_VndPowerbuilder6S -> []
    Application_VndPowerbuilder7 -> []
    Application_VndPowerbuilder75 -> []
    Application_VndPowerbuilder75S -> []
    Application_VndPowerbuilder7S -> []
    Application_VndPreminet -> []
    Application_VndPreviewsystemsBox -> [ "box" ]
    Application_VndProteusMagazine -> [ "mgz" ]
    Application_VndPublishareDeltaTree -> [ "qps" ]
    Application_VndPviPtid1 -> [ "ptid" ]
    Application_VndPwgMultiplexed -> []
    Application_VndPwgXhtmlPrintXml -> []
    Application_VndQualcommBrewAppRes -> []
    Application_VndQuarkQuarkxpress -> [ "qxd", "qxt", "qwd", "qwt", "qxl", "qxb" ]
    Application_VndQuobjectQuoxdocument -> []
    Application_VndRadisysMomlXml -> []
    Application_VndRadisysMsmlAuditConfXml -> []
    Application_VndRadisysMsmlAuditConnXml -> []
    Application_VndRadisysMsmlAuditDialogXml -> []
    Application_VndRadisysMsmlAuditStreamXml -> []
    Application_VndRadisysMsmlAuditXml -> []
    Application_VndRadisysMsmlConfXml -> []
    Application_VndRadisysMsmlDialogBaseXml -> []
    Application_VndRadisysMsmlDialogFaxDetectXml -> []
    Application_VndRadisysMsmlDialogFaxSendrecvXml -> []
    Application_VndRadisysMsmlDialogGroupXml -> []
    Application_VndRadisysMsmlDialogSpeechXml -> []
    Application_VndRadisysMsmlDialogTransformXml -> []
    Application_VndRadisysMsmlDialogXml -> []
    Application_VndRadisysMsmlXml -> []
    Application_VndRainstorData -> []
    Application_VndRapid -> []
    Application_VndRealvncBed -> [ "bed" ]
    Application_VndRecordareMusicxml -> [ "mxl" ]
    Application_VndRecordareMusicxmlXml -> [ "musicxml" ]
    Application_VndRenlearnRlprint -> []
    Application_VndRigCryptonote -> [ "cryptonote" ]
    Application_VndRimCod -> [ "cod" ]
    Application_VndRnRealmedia -> [ "rm" ]
    Application_VndRnRealmediaVbr -> [ "rmvb" ]
    Application_VndRnRealplayer -> [ "rnx" ]
    Application_VndRoute66Link66Xml -> [ "link66" ]
    Application_VndRs274x -> []
    Application_VndRuckusDownload -> []
    Application_VndS3sms -> []
    Application_VndSailingtrackerTrack -> [ "st" ]
    Application_VndSbmCid -> []
    Application_VndSbmMid2 -> []
    Application_VndScribus -> []
    Application_VndSealed3df -> []
    Application_VndSealedCsf -> []
    Application_VndSealedDoc -> []
    Application_VndSealedEml -> []
    Application_VndSealedMht -> []
    Application_VndSealedNet -> []
    Application_VndSealedPpt -> []
    Application_VndSealedTiff -> []
    Application_VndSealedXls -> []
    Application_VndSealedmediaSoftsealHtml -> []
    Application_VndSealedmediaSoftsealPdf -> []
    Application_VndSeemail -> [ "see" ]
    Application_VndSema -> [ "sema" ]
    Application_VndSemd -> [ "semd" ]
    Application_VndSemf -> [ "semf" ]
    Application_VndShanaInformedFormdata -> [ "ifm" ]
    Application_VndShanaInformedFormtemplate -> [ "itp" ]
    Application_VndShanaInformedInterchange -> [ "iif" ]
    Application_VndShanaInformedPackage -> [ "ipk" ]
    Application_VndSimtechMindmapper -> [ "twd", "twds" ]
    Application_VndSmaf -> [ "mmf" ]
    Application_VndSmartNotebook -> []
    Application_VndSmartTeacher -> [ "teacher" ]
    Application_VndSoftware602FillerFormXml -> []
    Application_VndSoftware602FillerFormXmlZip -> []
    Application_VndSolentSdkmXml -> [ "sdkm", "sdkd" ]
    Application_VndSpotfireDxp -> [ "dxp" ]
    Application_VndSpotfireSfs -> [ "sfs" ]
    Application_VndSssCod -> []
    Application_VndSssDtf -> []
    Application_VndSssNtf -> []
    Application_VndStardivisionCalc -> [ "sdc" ]
    Application_VndStardivisionDraw -> [ "sda" ]
    Application_VndStardivisionImpress -> [ "sdd", "sdp" ]
    Application_VndStardivisionMath -> [ "smf" ]
    Application_VndStardivisionWriter -> [ "sdw", "vor" ]
    Application_VndStardivisionWriterGlobal -> [ "sgl" ]
    Application_VndStepmaniaPackage -> [ "smzip" ]
    Application_VndStepmaniaStepchart -> [ "sm" ]
    Application_VndStreetStream -> []
    Application_VndSunWadlXml -> []
    Application_VndSunXmlCalc -> [ "sxc" ]
    Application_VndSunXmlCalcTemplate -> [ "stc" ]
    Application_VndSunXmlDraw -> [ "sxd" ]
    Application_VndSunXmlDrawTemplate -> [ "std" ]
    Application_VndSunXmlImpress -> [ "sxi" ]
    Application_VndSunXmlImpressTemplate -> [ "sti" ]
    Application_VndSunXmlMath -> [ "sxm" ]
    Application_VndSunXmlWriter -> [ "sxw" ]
    Application_VndSunXmlWriterGlobal -> [ "sxg" ]
    Application_VndSunXmlWriterTemplate -> [ "stw" ]
    Application_VndSusCalendar -> [ "sus", "susp" ]
    Application_VndSvd -> [ "svd" ]
    Application_VndSwiftviewIcs -> []
    Application_VndSymbianInstall -> [ "sis", "sisx" ]
    Application_VndSyncmlDmNotification -> []
    Application_VndSyncmlDmWbxml -> [ "bdm" ]
    Application_VndSyncmlDmXml -> [ "xdm" ]
    Application_VndSyncmlDsNotification -> []
    Application_VndSyncmlXml -> [ "xsm" ]
    Application_VndTaoIntentModuleArchive -> [ "tao" ]
    Application_VndTcpdumpPcap -> [ "pcap", "cap", "dmp" ]
    Application_VndTmobileLivetv -> [ "tmo" ]
    Application_VndTridTpt -> [ "tpt" ]
    Application_VndTriscapeMxs -> [ "mxs" ]
    Application_VndTrueapp -> [ "tra" ]
    Application_VndTruedoc -> []
    Application_VndTveTrigger -> []
    Application_VndUbisoftWebplayer -> []
    Application_VndUfdl -> [ "ufd", "ufdl" ]
    Application_VndUiqTheme -> [ "utz" ]
    Application_VndUmajin -> [ "umj" ]
    Application_VndUnity -> [ "unityweb" ]
    Application_VndUomlXml -> [ "uoml" ]
    Application_VndUplanetAlert -> []
    Application_VndUplanetAlertWbxml -> []
    Application_VndUplanetBearerChoice -> []
    Application_VndUplanetBearerChoiceWbxml -> []
    Application_VndUplanetCacheop -> []
    Application_VndUplanetCacheopWbxml -> []
    Application_VndUplanetChannel -> []
    Application_VndUplanetChannelWbxml -> []
    Application_VndUplanetList -> []
    Application_VndUplanetListWbxml -> []
    Application_VndUplanetListcmd -> []
    Application_VndUplanetListcmdWbxml -> []
    Application_VndUplanetSignal -> []
    Application_VndVcx -> [ "vcx" ]
    Application_VndVdStudy -> []
    Application_VndVectorworks -> []
    Application_VndVerimatrixVcas -> []
    Application_VndVidsoftVidconference -> []
    Application_VndVisio -> [ "vsd", "vst", "vss", "vsw" ]
    Application_VndVisionary -> [ "vis" ]
    Application_VndVividenceScriptfile -> []
    Application_VndVsf -> [ "vsf" ]
    Application_VndWapSic -> [ "sic" ]
    Application_VndWapSlc -> [ "slc" ]
    Application_VndWapWbxml -> [ "wbxml" ]
    Application_VndWapWmlc -> [ "wmlc" ]
    Application_VndWapWmlscriptc -> [ "wmlsc" ]
    Application_VndWebturbo -> [ "wtb" ]
    Application_VndWfaWsc -> []
    Application_VndWmc -> []
    Application_VndWmfBootstrap -> []
    Application_VndWolframMathematica -> []
    Application_VndWolframMathematicaPackage -> []
    Application_VndWolframPlayer -> [ "nbp" ]
    Application_VndWordperfect -> [ "wpd" ]
    Application_VndWqd -> [ "wqd" ]
    Application_VndWrqHp3000Labelled -> []
    Application_VndWtStf -> [ "stf" ]
    Application_VndWvCspWbxml -> []
    Application_VndWvCspXml -> []
    Application_VndWvSspXml -> []
    Application_VndXara -> [ "xar", "web" ]
    Application_VndXfdl -> [ "xfdl" ]
    Application_VndXfdlWebform -> []
    Application_VndXmiXml -> []
    Application_VndXmpieCpkg -> []
    Application_VndXmpieDpkg -> []
    Application_VndXmpiePlan -> []
    Application_VndXmpiePpkg -> []
    Application_VndXmpieXlim -> []
    Application_VndYamahaHvDic -> [ "hvd" ]
    Application_VndYamahaHvScript -> [ "hvs" ]
    Application_VndYamahaHvVoice -> [ "hvp" ]
    Application_VndYamahaOpenscoreformat -> [ "osf" ]
    Application_VndYamahaOpenscoreformatOsfpvgXml -> [ "osfpvg" ]
    Application_VndYamahaRemoteSetup -> []
    Application_VndYamahaSmafAudio -> [ "saf" ]
    Application_VndYamahaSmafPhrase -> [ "spf" ]
    Application_VndYamahaThroughNgn -> []
    Application_VndYamahaTunnelUdpencap -> []
    Application_VndYellowriverCustomMenu -> [ "cmp" ]
    Application_VndZul -> [ "zir", "zirz" ]
    Application_VndZzazzDeckXml -> [ "zaz" ]
    Application_VocaltecMediaDesc -> [ "vmd" ]
    Application_VocaltecMediaFile -> [ "vmf" ]
    Application_VoicexmlXml -> [ "vxml" ]
    Application_VqRtcpxr -> []
    Application_WatcherinfoXml -> []
    Application_WhoisppQuery -> []
    Application_WhoisppResponse -> []
    Application_Widget -> [ "wgt" ]
    Application_Winhlp -> [ "hlp" ]
    Application_Wita -> []
    Application_Wordperfect -> [ "wp", "wp5", "wp6", "wpd" ]
    Application_Wordperfect51 -> [ "wp5" ]
    Application_Wordperfect60 -> [ "w60", "wp5" ]
    Application_Wordperfect61 -> [ "w61" ]
    Application_WsdlXml -> [ "wsdl" ]
    Application_WspolicyXml -> [ "wspolicy" ]
    Application_X123 -> [ "wk1", "wk" ]
    Application_X400Bp -> []
    Application_X7zCompressed -> [ "7z" ]
    Application_XAbiword -> [ "abw" ]
    Application_XAceCompressed -> [ "ace" ]
    Application_XAim -> [ "aim" ]
    Application_XAmf -> []
    Application_XAppleDiskimage -> [ "dmg" ]
    Application_XAuthorwareBin -> [ "aab", "x32", "u32", "vox" ]
    Application_XAuthorwareMap -> [ "aam" ]
    Application_XAuthorwareSeg -> [ "aas" ]
    Application_XBcpio -> [ "bcpio" ]
    Application_XBinary -> [ "bin" ]
    Application_XBinhex40 -> [ "hqx" ]
    Application_XBittorrent -> [ "torrent" ]
    Application_XBlorb -> [ "blb", "blorb" ]
    Application_XBsh -> [ "bsh", "sh", "shar" ]
    Application_XBytecodeElisp -> [ "elc" ]
    Application_XBytecodeElispCompiledelisp -> [ "elc" ]
    Application_XBytecodePython -> [ "pyc", "pyc" ]
    Application_XBzip -> [ "bz" ]
    Application_XBzip2 -> [ "bz2", "boz" ]
    Application_XCbr -> [ "cbr", "cba", "cbt", "cbz", "cb7" ]
    Application_XCdf -> [ "cdf" ]
    Application_XCdlink -> [ "vcd" ]
    Application_XCfsCompressed -> [ "cfs" ]
    Application_XChat -> [ "chat", "cha" ]
    Application_XChessPgn -> [ "pgn" ]
    Application_XChm -> [ "chm" ]
    Application_XChromeExtension -> [ "crx" ]
    Application_XCmuRaster -> [ "ras" ]
    Application_XCocoa -> [ "cco" ]
    Application_XCompactpro -> [ "cpt" ]
    Application_XCompress -> [ "z" ]
    Application_XCompressed -> [ "gz", "tgz", "z", "zip" ]
    Application_XConference -> [ "nsc" ]
    Application_XCore -> []
    Application_XCpio -> [ "cpio" ]
    Application_XCpt -> [ "cpt" ]
    Application_XCsh -> [ "csh" ]
    Application_XDebianPackage -> [ "deb", "udeb" ]
    Application_XDeepv -> [ "deepv" ]
    Application_XDgcCompressed -> [ "dgc" ]
    Application_XDirector -> [ "dir", "dcr", "dxr", "cst", "cct", "cxt", "w3d", "fgd", "swa" ]
    Application_XDms -> [ "dms" ]
    Application_XDoom -> [ "wad" ]
    Application_XDtbncxXml -> [ "ncx" ]
    Application_XDtbookXml -> [ "dtb" ]
    Application_XDtbresourceXml -> [ "res" ]
    Application_XDvi -> [ "dvi" ]
    Application_XElc -> [ "elc" ]
    Application_XEnvoy -> [ "env", "evy" ]
    Application_XEsrehber -> [ "es" ]
    Application_XEva -> [ "eva" ]
    Application_XExcel -> [ "xla", "xlb", "xlc", "xld", "xlk", "xll", "xlm", "xls", "xlt", "xlv", "xlw" ]
    Application_XExecutable -> []
    Application_XFlac -> [ "flac" ]
    Application_XFont -> [ "pfa", "pfb", "gsf", "pcf", "pcf.Z" ]
    Application_XFontBdf -> [ "bdf" ]
    Application_XFontDos -> []
    Application_XFontFramemaker -> []
    Application_XFontGhostscript -> [ "gsf" ]
    Application_XFontLibgrx -> []
    Application_XFontLinuxPsf -> [ "psf" ]
    Application_XFontOtf -> [ "otf" ]
    Application_XFontPcf -> [ "pcf" ]
    Application_XFontSnf -> [ "snf" ]
    Application_XFontSpeedo -> []
    Application_XFontSunosNews -> []
    Application_XFontTtf -> [ "ttf", "ttc" ]
    Application_XFontType1 -> [ "pfa", "pfb", "pfm", "afm" ]
    Application_XFontVfont -> []
    Application_XFontWoff -> [ "woff" ]
    Application_XFrame -> [ "mif" ]
    Application_XFreearc -> [ "arc" ]
    Application_XFreelance -> [ "pre" ]
    Application_XFuturesplash -> [ "spl" ]
    Application_XGcaCompressed -> [ "gca" ]
    Application_XGlulx -> [ "ulx" ]
    Application_XGnumeric -> [ "gnumeric" ]
    Application_XGoSgf -> [ "sgf" ]
    Application_XGrampsXml -> [ "gramps" ]
    Application_XGraphingCalculator -> [ "gcf" ]
    Application_XGsp -> [ "gsp" ]
    Application_XGss -> [ "gss" ]
    Application_XGtar -> [ "gtar", "tgz", "taz" ]
    Application_XGzip -> [ "gz", "gzip", "tgz" ]
    Application_XHdf -> [ "hdf" ]
    Application_XHelpfile -> [ "help", "hlp" ]
    Application_XHttpdImap -> [ "imap" ]
    Application_XHttpdPhp -> [ "phtml", "pht", "php" ]
    Application_XHttpdPhp3 -> [ "php3" ]
    Application_XHttpdPhp3Preprocessed -> [ "php3p" ]
    Application_XHttpdPhp4 -> [ "php4" ]
    Application_XHttpdPhpSource -> [ "phps" ]
    Application_XIca -> [ "ica" ]
    Application_XIma -> [ "ima" ]
    Application_XInstallInstructions -> [ "install" ]
    Application_XInternetSignup -> [ "ins", "isp" ]
    Application_XInternettSignup -> [ "ins" ]
    Application_XInventor -> [ "iv" ]
    Application_XIp2 -> [ "ip" ]
    Application_XIphone -> [ "iii" ]
    Application_XIso9660Image -> [ "iso" ]
    Application_XJavaApplet -> []
    Application_XJavaArchive -> [ "jar" ]
    Application_XJavaBean -> []
    Application_XJavaClass -> [ "class" ]
    Application_XJavaCommerce -> [ "jcm" ]
    Application_XJavaJnlpFile -> [ "jnlp" ]
    Application_XJavaSerializedObject -> [ "ser" ]
    Application_XJavaVm -> [ "class" ]
    Application_XJavascript -> [ "js" ]
    Application_XKchart -> [ "chrt" ]
    Application_XKdelnk -> []
    Application_XKillustrator -> [ "kil" ]
    Application_XKoan -> [ "skd", "skm", "skp", "skt" ]
    Application_XKpresenter -> [ "kpr", "kpt" ]
    Application_XKsh -> [ "ksh" ]
    Application_XKspread -> [ "ksp" ]
    Application_XKword -> [ "kwd", "kwt" ]
    Application_XLatex -> [ "latex", "ltx" ]
    Application_XLha -> [ "lha" ]
    Application_XLisp -> [ "lsp" ]
    Application_XLivescreen -> [ "ivy" ]
    Application_XLotus -> [ "wq1" ]
    Application_XLotusscreencam -> [ "scm" ]
    Application_XLuaBytecode -> [ "luac" ]
    Application_XLzh -> [ "lzh" ]
    Application_XLzhCompressed -> [ "lzh", "lha" ]
    Application_XLzx -> [ "lzx" ]
    Application_XMacBinhex40 -> [ "hqx" ]
    Application_XMacbinary -> [ "bin" ]
    Application_XMagicCapPackage10 -> [ "mc$" ]
    Application_XMaker -> [ "frm", "maker", "frame", "fm", "fb", "book", "fbdoc" ]
    Application_XMathcad -> [ "mcd" ]
    Application_XMeme -> [ "mm" ]
    Application_XMidi -> [ "mid", "midi" ]
    Application_XMie -> [ "mie" ]
    Application_XMif -> [ "mif" ]
    Application_XMixTransfer -> [ "nix" ]
    Application_XMobipocketEbook -> [ "prc", "mobi" ]
    Application_XMpegurl -> [ "m3u8" ]
    Application_XMplayer2 -> [ "asx" ]
    Application_XMsApplication -> [ "application" ]
    Application_XMsShortcut -> [ "lnk" ]
    Application_XMsWmd -> [ "wmd" ]
    Application_XMsWmz -> [ "wmz" ]
    Application_XMsXbap -> [ "xbap" ]
    Application_XMsaccess -> [ "mdb" ]
    Application_XMsbinder -> [ "obd" ]
    Application_XMscardfile -> [ "crd" ]
    Application_XMsclip -> [ "clp" ]
    Application_XMsdosProgram -> [ "com", "exe", "bat", "dll" ]
    Application_XMsdownload -> [ "exe", "dll", "com", "bat", "msi" ]
    Application_XMsexcel -> [ "xla", "xls", "xlw" ]
    Application_XMsi -> [ "msi" ]
    Application_XMsmediaview -> [ "mvb", "m13", "m14" ]
    Application_XMsmetafile -> [ "wmf", "wmz", "emf", "emz" ]
    Application_XMsmoney -> [ "mny" ]
    Application_XMspowerpoint -> [ "ppt" ]
    Application_XMspublisher -> [ "pub" ]
    Application_XMsschedule -> [ "scd" ]
    Application_XMsterminal -> [ "trm" ]
    Application_XMswrite -> [ "wri" ]
    Application_XNaviAnimation -> [ "ani" ]
    Application_XNavidoc -> [ "nvd" ]
    Application_XNavimap -> [ "map" ]
    Application_XNavistyle -> [ "stl" ]
    Application_XNetcdf -> [ "nc", "cdf" ]
    Application_XNewtonCompatiblePkg -> [ "pkg" ]
    Application_XNokia9000CommunicatorAddOnSoftware -> [ "aos" ]
    Application_XNsProxyAutoconfig -> [ "pac" ]
    Application_XNwc -> [ "nwc" ]
    Application_XNzb -> [ "nzb" ]
    Application_XObject -> [ "o" ]
    Application_XOmc -> [ "omc" ]
    Application_XOmcdatamaker -> [ "omcd" ]
    Application_XOmcregerator -> [ "omcr" ]
    Application_XOzApplication -> [ "oza" ]
    Application_XPagemaker -> [ "pm4", "pm5" ]
    Application_XPcl -> [ "pcl" ]
    Application_XPerfmon -> [ "pma", "pmc", "pml", "pmr", "pmw" ]
    Application_XPixclscript -> [ "plx" ]
    Application_XPkcs10 -> [ "p10" ]
    Application_XPkcs12 -> [ "p12", "pfx" ]
    Application_XPkcs7Certificates -> [ "p7b", "spc" ]
    Application_XPkcs7Certreqresp -> [ "p7r" ]
    Application_XPkcs7Crl -> [ "crl" ]
    Application_XPkcs7Mime -> [ "p7c", "p7m" ]
    Application_XPkcs7Signature -> [ "p7a", "p7s" ]
    Application_XPointplus -> [ "css" ]
    Application_XPortableAnymap -> [ "pnm" ]
    Application_XProject -> [ "mpc", "mpt", "mpv", "mpx" ]
    Application_XPythonCode -> [ "pyc", "pyo" ]
    Application_XQpro -> [ "wb1" ]
    Application_XQuicktimeplayer -> [ "qtl" ]
    Application_XRarCompressed -> [ "rar" ]
    Application_XRedhatPackageManager -> [ "rpm" ]
    Application_XResearchInfoSystems -> [ "ris" ]
    Application_XRpm -> [ "rpm" ]
    Application_XRtf -> [ "rtf" ]
    Application_XRx -> []
    Application_XSdp -> [ "sdp" ]
    Application_XSea -> [ "sea" ]
    Application_XSeelogo -> [ "sl" ]
    Application_XSh -> [ "sh" ]
    Application_XShar -> [ "shar", "sh" ]
    Application_XShellscript -> []
    Application_XShockwaveFlash -> [ "swf", "swfl" ]
    Application_XSilverlightApp -> [ "xap" ]
    Application_XSit -> [ "sit" ]
    Application_XSprite -> [ "spr", "sprite" ]
    Application_XSql -> [ "sql" ]
    Application_XStuffit -> [ "sit" ]
    Application_XStuffitx -> [ "sitx" ]
    Application_XSubrip -> [ "srt" ]
    Application_XSv4cpio -> [ "sv4cpio" ]
    Application_XSv4crc -> [ "sv4crc" ]
    Application_XT3vmImage -> [ "t3" ]
    Application_XTads -> [ "gam" ]
    Application_XTar -> [ "tar" ]
    Application_XTbook -> [ "sbk", "tbk" ]
    Application_XTcl -> [ "tcl" ]
    Application_XTex -> [ "tex" ]
    Application_XTexGf -> [ "gf" ]
    Application_XTexPk -> [ "pk" ]
    Application_XTexTfm -> [ "tfm" ]
    Application_XTexinfo -> [ "texinfo", "texi" ]
    Application_XTgif -> [ "obj" ]
    Application_XTrash -> [ "~", "%", "bak", "old", "sik" ]
    Application_XTroff -> [ "roff", "t", "tr" ]
    Application_XTroffMan -> [ "man" ]
    Application_XTroffMe -> [ "me" ]
    Application_XTroffMs -> [ "ms" ]
    Application_XTroffMsvideo -> [ "avi" ]
    Application_XUstar -> [ "ustar" ]
    Application_XVideolan -> []
    Application_XVisio -> [ "vsd", "vst", "vsw" ]
    Application_XVndAudioexplosionMzz -> [ "mzz" ]
    Application_XVndLsXpix -> [ "xpix" ]
    Application_XVrml -> [ "vrml" ]
    Application_XWaisSource -> [ "src", "wsrc" ]
    Application_XWebAppManifestJson -> [ "webapp" ]
    Application_XWingz -> [ "wz" ]
    Application_XWinhelp -> [ "hlp" ]
    Application_XWintalk -> [ "wtk" ]
    Application_XWorld -> [ "svr", "wrl" ]
    Application_XWpwin -> [ "wpd" ]
    Application_XWri -> [ "wri" ]
    Application_XX509CaCert -> [ "der", "cer", "crt" ]
    Application_XX509UserCert -> [ "crt" ]
    Application_XXcf -> [ "xcf" ]
    Application_XXfig -> [ "fig" ]
    Application_XXliffXml -> [ "xlf" ]
    Application_XXpinstall -> [ "xpi" ]
    Application_XXz -> [ "xz" ]
    Application_XZipCompressed -> [ "zip" ]
    Application_XZmachine -> [ "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8" ]
    Application_XamlXml -> [ "xaml" ]
    Application_XcapAttXml -> []
    Application_XcapCapsXml -> []
    Application_XcapDiffXml -> [ "xdf" ]
    Application_XcapElXml -> []
    Application_XcapErrorXml -> []
    Application_XcapNsXml -> []
    Application_XconConferenceInfoDiffXml -> []
    Application_XconConferenceInfoXml -> []
    Application_XencXml -> [ "xenc" ]
    Application_XhtmlVoiceXml -> []
    Application_XhtmlXml -> [ "xhtml", "xht" ]
    Application_Xml -> [ "xml", "xsl", "xpdl" ]
    Application_XmlDtd -> [ "dtd" ]
    Application_XmlExternalParsedEntity -> []
    Application_XmppXml -> []
    Application_XopXml -> [ "xop" ]
    Application_XprocXml -> [ "xpl" ]
    Application_XsltXml -> [ "xslt" ]
    Application_XspfXml -> [ "xspf" ]
    Application_XvXml -> [ "mxml", "xhvml", "xvml", "xvm" ]
    Application_Yang -> [ "yang" ]
    Application_YinXml -> [ "yin" ]
    Application_YndMsPkipko -> [ "pko" ]
    Application_Zip -> [ "zip" ]
    Audio_1dInterleavedParityfec -> []
    Audio_32kadpcm -> []
    Audio_3gpp -> []
    Audio_3gpp2 -> []
    Audio_Ac3 -> []
    Audio_Adpcm -> [ "adp" ]
    Audio_Aiff -> [ "aif", "aifc", "aiff" ]
    Audio_Amr -> []
    Audio_AmrWb -> []
    Audio_Asc -> []
    Audio_Atrac3 -> []
    Audio_AtracAdvancedLossless -> []
    Audio_AtracX -> []
    Audio_Basic -> [ "au", "snd" ]
    Audio_Bv16 -> []
    Audio_Bv32 -> []
    Audio_Clearmode -> []
    Audio_Cn -> []
    Audio_Dat12 -> []
    Audio_Dls -> []
    Audio_DsrEs201108 -> []
    Audio_DsrEs202050 -> []
    Audio_DsrEs202211 -> []
    Audio_DsrEs202212 -> []
    Audio_Dv -> []
    Audio_Dvi4 -> []
    Audio_Eac3 -> []
    Audio_Evrc -> []
    Audio_Evrc0 -> []
    Audio_Evrc1 -> []
    Audio_EvrcQcp -> []
    Audio_Evrcb -> []
    Audio_Evrcb0 -> []
    Audio_Evrcb1 -> []
    Audio_Evrcwb -> []
    Audio_Evrcwb0 -> []
    Audio_Evrcwb1 -> []
    Audio_Example -> []
    Audio_Flac -> [ "flac" ]
    Audio_Fwdred -> []
    Audio_G719 -> []
    Audio_G722 -> []
    Audio_G7221 -> []
    Audio_G723 -> []
    Audio_G72616 -> []
    Audio_G72624 -> []
    Audio_G72632 -> []
    Audio_G72640 -> []
    Audio_G728 -> []
    Audio_G729 -> []
    Audio_G7291 -> []
    Audio_G729d -> []
    Audio_G729e -> []
    Audio_Gsm -> []
    Audio_GsmEfr -> []
    Audio_GsmHr08 -> []
    Audio_Ilbc -> []
    Audio_IpMr_v25 -> []
    Audio_Isac -> []
    Audio_It -> [ "it" ]
    Audio_L16 -> []
    Audio_L20 -> []
    Audio_L24 -> []
    Audio_L8 -> []
    Audio_Lpc -> []
    Audio_Make -> [ "funk", "my", "pfunk" ]
    Audio_MakeMyFunk -> [ "pfunk" ]
    Audio_Mid -> [ "rmi", "mid" ]
    Audio_Midi -> [ "mid", "midi", "kar", "rmi" ]
    Audio_MobileXmf -> []
    Audio_Mod -> [ "mod" ]
    Audio_Mp4 -> [ "mp4a", "m4a" ]
    Audio_Mp4aLatm -> []
    Audio_Mpa -> []
    Audio_MpaRobust -> []
    Audio_Mpeg -> [ "mpga", "mp2", "mp2a", "mp3", "m2a", "mpa", "mpg", "m3a", "mpega", "m4a" ]
    Audio_Mpeg3 -> [ "mp3" ]
    Audio_Mpeg4Generic -> []
    Audio_Mpegurl -> [ "m3u" ]
    Audio_Musepack -> []
    Audio_Nspaudio -> [ "la", "lma" ]
    Audio_Ogg -> [ "oga", "ogg", "spx" ]
    Audio_Opus -> []
    Audio_Parityfec -> []
    Audio_Pcma -> []
    Audio_PcmaWb -> []
    Audio_Pcmu -> []
    Audio_PcmuWb -> []
    Audio_PrsSid -> [ "sid" ]
    Audio_Qcelp -> []
    Audio_Red -> []
    Audio_RtpEncAescm128 -> []
    Audio_RtpMidi -> []
    Audio_Rtx -> []
    Audio_S3m -> [ "s3m" ]
    Audio_Silk -> [ "sil" ]
    Audio_Smv -> []
    Audio_Smv0 -> []
    Audio_SmvQcp -> []
    Audio_SpMidi -> []
    Audio_Speex -> []
    Audio_T140c -> []
    Audio_T38 -> []
    Audio_TelephoneEvent -> []
    Audio_Tone -> []
    Audio_TspAudio -> [ "tsi" ]
    Audio_Tsplayer -> [ "tsp" ]
    Audio_Uemclip -> []
    Audio_Ulpfec -> []
    Audio_Vdvi -> []
    Audio_VmrWb -> []
    Audio_Vnd3gppIufp -> []
    Audio_Vnd4sb -> []
    Audio_VndAudiokoz -> []
    Audio_VndCelp -> []
    Audio_VndCiscoNse -> []
    Audio_VndCmlesRadioEvents -> []
    Audio_VndCnsAnp1 -> []
    Audio_VndCnsInf1 -> []
    Audio_VndDeceAudio -> [ "uva", "uvva" ]
    Audio_VndDigitalWinds -> [ "eol" ]
    Audio_VndDlnaAdts -> []
    Audio_VndDolbyHeaac1 -> []
    Audio_VndDolbyHeaac2 -> []
    Audio_VndDolbyMlp -> []
    Audio_VndDolbyMps -> []
    Audio_VndDolbyPl2 -> []
    Audio_VndDolbyPl2x -> []
    Audio_VndDolbyPl2z -> []
    Audio_VndDolbyPulse1 -> []
    Audio_VndDra -> [ "dra" ]
    Audio_VndDts -> [ "dts" ]
    Audio_VndDtsHd -> [ "dtshd" ]
    Audio_VndDvbFile -> []
    Audio_VndEveradPlj -> []
    Audio_VndHnsAudio -> []
    Audio_VndLucentVoice -> [ "lvp" ]
    Audio_VndMsPlayreadyMediaPya -> [ "pya" ]
    Audio_VndNokiaMobileXmf -> []
    Audio_VndNortelVbk -> []
    Audio_VndNueraEcelp4800 -> [ "ecelp4800" ]
    Audio_VndNueraEcelp7470 -> [ "ecelp7470" ]
    Audio_VndNueraEcelp9600 -> [ "ecelp9600" ]
    Audio_VndOctelSbc -> []
    Audio_VndQcelp -> [ "qcp" ]
    Audio_VndRhetorex32kadpcm -> []
    Audio_VndRip -> [ "rip" ]
    Audio_VndSealedmediaSoftsealMpeg -> []
    Audio_VndVmxCvsd -> []
    Audio_Voc -> [ "voc" ]
    Audio_Vorbis -> []
    Audio_VorbisConfig -> []
    Audio_Voxware -> [ "vox" ]
    Audio_Wav -> [ "wav" ]
    Audio_Webm -> [ "weba" ]
    Audio_XAac -> [ "aac" ]
    Audio_XAdpcm -> [ "snd" ]
    Audio_XAiff -> [ "aif", "aiff", "aifc" ]
    Audio_XAu -> [ "au" ]
    Audio_XCaf -> [ "caf" ]
    Audio_XFlac -> [ "flac" ]
    Audio_XGsm -> [ "gsd", "gsm" ]
    Audio_XJam -> [ "jam" ]
    Audio_XLiveaudio -> [ "lam" ]
    Audio_XMatroska -> [ "mka" ]
    Audio_XMid -> [ "mid", "midi" ]
    Audio_XMidi -> [ "mid", "midi" ]
    Audio_XMod -> [ "mod" ]
    Audio_XMpeg -> [ "mp2" ]
    Audio_XMpeg3 -> [ "mp3" ]
    Audio_XMpegurl -> [ "m3u" ]
    Audio_XMpequrl -> [ "m3u" ]
    Audio_XMsWax -> [ "wax" ]
    Audio_XMsWma -> [ "wma" ]
    Audio_XNspaudio -> [ "la", "lma" ]
    Audio_XPnRealaudio -> [ "ram", "ra", "rm", "rmm", "rmp" ]
    Audio_XPnRealaudioPlugin -> [ "rmp", "ra", "rpm" ]
    Audio_XPsid -> [ "sid" ]
    Audio_XRealaudio -> [ "ra" ]
    Audio_XScpls -> [ "pls" ]
    Audio_XSd2 -> [ "sd2" ]
    Audio_XTta -> []
    Audio_XTwinvq -> [ "vqf" ]
    Audio_XTwinvqPlugin -> [ "vqe", "vql" ]
    Audio_XVndAudioexplosionMjuicemediafile -> [ "mjf" ]
    Audio_XVoc -> [ "voc" ]
    Audio_XWav -> [ "wav" ]
    Audio_Xm -> [ "xm" ]
    Chemical_XCdx -> [ "cdx" ]
    Chemical_XCif -> [ "cif" ]
    Chemical_XCmdf -> [ "cmdf" ]
    Chemical_XCml -> [ "cml" ]
    Chemical_XCsml -> [ "csml" ]
    Chemical_XPdb -> [ "pdb", "xyz" ]
    Chemical_XXyz -> [ "xyz" ]
    Content_Unknown -> []
    Drawing_XDwf -> [ "dwf" ]
    Drawing_XDwfOld -> [ "dwf" ]
    Font_Opentype -> [ "otf" ]
    IWorld_IVrml -> [ "ivr" ]
    Image_Bmp -> [ "bmp", "bm" ]
    Image_Cgm -> [ "cgm" ]
    Image_CisCod -> [ "cod" ]
    Image_CmuRaster -> [ "ras", "rast" ]
    Image_Example -> []
    Image_Fif -> [ "fif" ]
    Image_Fits -> []
    Image_Florian -> [ "flo", "turbot" ]
    Image_G3fax -> [ "g3" ]
    Image_Gif -> [ "gif" ]
    Image_Ief -> [ "ief", "iefs" ]
    Image_Jp2 -> []
    Image_Jpeg -> [ "jpeg", "jpg", "jfif", "jfif-tbnl", "jpe" ]
    Image_Jpm -> []
    Image_Jpx -> []
    Image_Jutvision -> [ "jut" ]
    Image_Ktx -> [ "ktx" ]
    Image_Naplps -> [ "nap", "naplps" ]
    Image_Pcx -> [ "pcx" ]
    Image_Pict -> [ "pic", "pict" ]
    Image_Pipeg -> [ "jfif" ]
    Image_Pjpeg -> [ "jfif", "jpe", "jpeg", "jpg" ]
    Image_Png -> [ "png", "x-png" ]
    Image_PrsBtif -> [ "btif" ]
    Image_PrsPti -> []
    Image_Sgi -> [ "sgi" ]
    Image_SvgXml -> [ "svg", "svgz" ]
    Image_T38 -> []
    Image_Tiff -> [ "tiff", "tif" ]
    Image_TiffFx -> []
    Image_Vasa -> [ "mcf" ]
    Image_VndAdobePhotoshop -> [ "psd" ]
    Image_VndCnsInf2 -> []
    Image_VndDeceGraphic -> [ "uvi", "uvvi", "uvg", "uvvg" ]
    Image_VndDjvu -> [ "djvu", "djv" ]
    Image_VndDvbSubtitle -> [ "sub" ]
    Image_VndDwg -> [ "dwg", "dxf", "svf" ]
    Image_VndDxf -> [ "dxf" ]
    Image_VndFastbidsheet -> [ "fbs" ]
    Image_VndFpx -> [ "fpx", "fpix" ]
    Image_VndFst -> [ "fst" ]
    Image_VndFujixeroxEdmicsMmr -> [ "mmr" ]
    Image_VndFujixeroxEdmicsRlc -> [ "rlc" ]
    Image_VndGlobalgraphicsPgb -> []
    Image_VndMicrosoftIcon -> []
    Image_VndMix -> []
    Image_VndMsModi -> [ "mdi" ]
    Image_VndMsPhoto -> [ "wdp" ]
    Image_VndNetFpx -> [ "npx", "fpx" ]
    Image_VndRadiance -> []
    Image_VndRnRealflash -> [ "rf" ]
    Image_VndRnRealpix -> [ "rp" ]
    Image_VndSealedPng -> []
    Image_VndSealedmediaSoftsealGif -> []
    Image_VndSealedmediaSoftsealJpg -> []
    Image_VndSvf -> []
    Image_VndWapWbmp -> [ "wbmp" ]
    Image_VndXiff -> [ "xif" ]
    Image_Webp -> [ "webp" ]
    Image_X3ds -> [ "3ds" ]
    Image_XCmuRast -> [ "ras" ]
    Image_XCmuRaster -> [ "ras" ]
    Image_XCmx -> [ "cmx" ]
    Image_XCoreldraw -> [ "cdr" ]
    Image_XCoreldrawpattern -> [ "pat" ]
    Image_XCoreldrawtemplate -> [ "cdt" ]
    Image_XCorelphotopaint -> [ "cpt" ]
    Image_XDwg -> [ "dwg", "dxf", "svf" ]
    Image_XFreehand -> [ "fh", "fhc", "fh4", "fh5", "fh7" ]
    Image_XIcon -> [ "ico" ]
    Image_XJg -> [ "art" ]
    Image_XJng -> [ "jng" ]
    Image_XJps -> [ "jps" ]
    Image_XMrsidImage -> [ "sid" ]
    Image_XMsBmp -> [ "bmp" ]
    Image_XNiff -> [ "nif", "niff" ]
    Image_XPcx -> [ "pcx" ]
    Image_XPhotoshop -> [ "psd" ]
    Image_XPict -> [ "pic", "pct" ]
    Image_XPortableAnymap -> [ "pnm" ]
    Image_XPortableBitmap -> [ "pbm" ]
    Image_XPortableGraymap -> [ "pgm" ]
    Image_XPortableGreymap -> [ "pgm" ]
    Image_XPortablePixmap -> [ "ppm" ]
    Image_XQuicktime -> [ "qif", "qti", "qtif" ]
    Image_XRgb -> [ "rgb" ]
    Image_XTga -> [ "tga" ]
    Image_XTiff -> [ "tif", "tiff" ]
    Image_XWindowsBmp -> [ "bmp" ]
    Image_XXbitmap -> [ "xbm", "xpm" ]
    Image_XXbm -> [ "xbm" ]
    Image_XXpixmap -> [ "xpm", "pm" ]
    Image_XXwd -> [ "xwd" ]
    Image_XXwindowdump -> [ "xwd" ]
    Image_Xbm -> [ "xbm" ]
    Image_Xpm -> [ "xpm" ]
    Inode_Blockdevice -> []
    Inode_Chardevice -> []
    Inode_Directory -> []
    Inode_DirectoryLocked -> []
    Inode_Fifo -> []
    Inode_Socket -> []
    Message_Cpim -> []
    Message_DeliveryStatus -> []
    Message_DispositionNotification -> []
    Message_Example -> []
    Message_ExternalBody -> []
    Message_FeedbackReport -> []
    Message_Global -> []
    Message_GlobalDeliveryStatus -> []
    Message_GlobalDispositionNotification -> []
    Message_GlobalHeaders -> []
    Message_Http -> []
    Message_ImdnXml -> []
    Message_News -> []
    Message_Partial -> []
    Message_Rfc822 -> [ "eml", "mht", "mhtml", "mime", "nws" ]
    Message_SHttp -> []
    Message_Sip -> []
    Message_Sipfrag -> []
    Message_TrackingStatus -> []
    Message_VndSiSimp -> []
    Model_Example -> []
    Model_Iges -> [ "igs", "iges" ]
    Model_Mesh -> [ "msh", "mesh", "silo" ]
    Model_VndColladaXml -> [ "dae" ]
    Model_VndDwf -> [ "dwf" ]
    Model_VndFlatland3dml -> []
    Model_VndGdl -> [ "gdl" ]
    Model_VndGsGdl -> []
    Model_VndGtw -> [ "gtw" ]
    Model_VndMomlXml -> []
    Model_VndMts -> [ "mts" ]
    Model_VndParasolidTransmitBinary -> []
    Model_VndParasolidTransmitText -> []
    Model_VndVtu -> [ "vtu" ]
    Model_Vrml -> [ "wrl", "vrml", "wrz" ]
    Model_X3dBinary -> [ "x3db", "x3dbz" ]
    Model_X3dVrml -> [ "x3dv", "x3dvz" ]
    Model_X3dXml -> [ "x3d", "x3dz" ]
    Model_XPov -> [ "pov" ]
    Multipart_Alternative -> []
    Multipart_Appledouble -> []
    Multipart_Byteranges -> []
    Multipart_Digest -> []
    Multipart_Encrypted -> []
    Multipart_Example -> []
    Multipart_FormData -> []
    Multipart_HeaderSet -> []
    Multipart_Mixed -> []
    Multipart_Parallel -> []
    Multipart_Related -> []
    Multipart_Report -> []
    Multipart_Signed -> []
    Multipart_VoiceMessage -> []
    Multipart_XGzip -> [ "gzip" ]
    Multipart_XUstar -> [ "ustar" ]
    Multipart_XZip -> [ "zip" ]
    Music_Crescendo -> [ "mid", "midi" ]
    Music_XKaraoke -> [ "kar" ]
    Paleovu_XPv -> [ "pvu" ]
    Text_1dInterleavedParityfec -> []
    Text_Asp -> [ "asp" ]
    Text_CacheManifest -> [ "appcache", "manifest" ]
    Text_Calendar -> [ "ics", "ifb", "icz" ]
    Text_CommaSeparatedValues -> [ "csv" ]
    Text_Css -> [ "css" ]
    Text_Csv -> [ "csv" ]
    Text_Directory -> []
    Text_Dns -> []
    Text_Ecmascript -> [ "js" ]
    Text_English -> []
    Text_Enriched -> []
    Text_EventStream -> [ "event-stream" ]
    Text_Example -> []
    Text_Fwdred -> []
    Text_H323 -> [ "323" ]
    Text_Html -> [ "html", "acgi", "htm", "htmls", "htx", "shtml", "stm" ]
    Text_Iuls -> [ "uls" ]
    Text_Javascript -> [ "js" ]
    Text_Mathml -> [ "mml" ]
    Text_Mcf -> [ "mcf" ]
    Text_N3 -> [ "n3" ]
    Text_Parityfec -> []
    Text_Pascal -> [ "pas" ]
    Text_Plain -> [ "txt", "text", "conf", "def", "list", "log", "c", "c++", "cc", "com", "cxx", "f", "f90", "for", "g", "h", "hh", "idc", "jav", "java", "lst", "m", "mar", "pl", "sdml", "bas", "in", "asc", "diff", "pot", "el", "ksh" ]
    Text_PlainBas -> [ "par" ]
    Text_PrsFallensteinRst -> []
    Text_PrsLinesTag -> [ "dsc" ]
    Text_Red -> []
    Text_Rfc822Headers -> []
    Text_Richtext -> [ "rtx", "rt", "rtf" ]
    Text_Rtf -> [ "rtf" ]
    Text_RtpEncAescm128 -> []
    Text_Rtx -> []
    Text_Scriplet -> [ "wsc" ]
    Text_Scriptlet -> [ "sct", "wsc" ]
    Text_Sgml -> [ "sgml", "sgm" ]
    Text_T140 -> []
    Text_TabSeparatedValues -> [ "tsv" ]
    Text_Texmacs -> [ "tm", "ts" ]
    Text_Troff -> [ "t", "tr", "roff", "man", "me", "ms" ]
    Text_Turtle -> [ "ttl" ]
    Text_Ulpfec -> []
    Text_UriList -> [ "uri", "uris", "uni", "unis", "urls" ]
    Text_Vcard -> [ "vcard" ]
    Text_VndAbc -> [ "abc" ]
    Text_VndCurl -> [ "curl" ]
    Text_VndCurlDcurl -> [ "dcurl" ]
    Text_VndCurlMcurl -> [ "mcurl" ]
    Text_VndCurlScurl -> [ "scurl" ]
    Text_VndDmclientscript -> []
    Text_VndDvbSubtitle -> [ "sub" ]
    Text_VndEsmertecThemeDescriptor -> []
    Text_VndFlatland3dml -> []
    Text_VndFly -> [ "fly" ]
    Text_VndFmiFlexstor -> [ "flx" ]
    Text_VndGraphviz -> [ "gv" ]
    Text_VndIn3d3dml -> [ "3dml" ]
    Text_VndIn3dSpot -> [ "spot" ]
    Text_VndIptcNewsml -> []
    Text_VndIptcNitf -> []
    Text_VndLatexZ -> []
    Text_VndMotorolaReflex -> []
    Text_VndMsMediapackage -> []
    Text_VndNet2phoneCommcenterCommand -> []
    Text_VndRadisysMsmlBasicLayout -> []
    Text_VndRnRealtext -> [ "rt" ]
    Text_VndSiUricatalogue -> []
    Text_VndSunJ2meAppDescriptor -> [ "jad" ]
    Text_VndTrolltechLinguist -> []
    Text_VndWapSi -> [ "si" ]
    Text_VndWapSl -> [ "sl" ]
    Text_VndWapWml -> [ "wml" ]
    Text_VndWapWmlscript -> [ "wmls" ]
    Text_Vtt -> [ "vtt" ]
    Text_Webviewhtml -> [ "htt" ]
    Text_XAsm -> [ "s", "asm" ]
    Text_XAudiosoftIntra -> [ "aip" ]
    Text_XC -> [ "c", "cc", "cxx", "cpp", "h", "hh", "dic" ]
    Text_XCHdr -> [ "h++", "hpp", "hxx", "hh" ]
    Text_XCSrc -> [ "c++", "cpp", "cxx", "cc" ]
    Text_XChdr -> [ "h" ]
    Text_XComponent -> [ "htc" ]
    Text_XCrontab -> []
    Text_XCsh -> [ "csh" ]
    Text_XCsrc -> [ "c" ]
    Text_XFortran -> [ "f", "for", "f77", "f90" ]
    Text_XH -> [ "h", "hh" ]
    Text_XJava -> [ "java" ]
    Text_XJavaSource -> [ "java", "jav" ]
    Text_XLaAsf -> [ "lsx" ]
    Text_XLua -> [ "lua" ]
    Text_XM -> [ "m" ]
    Text_XMakefile -> []
    Text_XMarkdown -> [ "markdown", "md", "mkd" ]
    Text_XMoc -> [ "moc" ]
    Text_XNfo -> [ "nfo" ]
    Text_XOpml -> [ "opml" ]
    Text_XPascal -> [ "p", "pas" ]
    Text_XPcsGcd -> [ "gcd" ]
    Text_XPerl -> [ "pl", "pm" ]
    Text_XPython -> [ "py" ]
    Text_XScript -> [ "hlb" ]
    Text_XScriptCsh -> [ "csh" ]
    Text_XScriptElisp -> [ "el" ]
    Text_XScriptGuile -> [ "scm" ]
    Text_XScriptKsh -> [ "ksh" ]
    Text_XScriptLisp -> [ "lsp" ]
    Text_XScriptPerl -> [ "pl" ]
    Text_XScriptPerlModule -> [ "pm" ]
    Text_XScriptPhyton -> [ "py" ]
    Text_XScriptRexx -> [ "rexx" ]
    Text_XScriptScheme -> [ "scm" ]
    Text_XScriptSh -> [ "sh" ]
    Text_XScriptTcl -> [ "tcl" ]
    Text_XScriptTcsh -> [ "tcsh" ]
    Text_XScriptZsh -> [ "zsh" ]
    Text_XServerParsedHtml -> [ "shtml", "ssi" ]
    Text_XSetext -> [ "etx" ]
    Text_XSfv -> [ "sfv" ]
    Text_XSgml -> [ "sgm", "sgml" ]
    Text_XSh -> [ "sh" ]
    Text_XSpeech -> [ "spc", "talk" ]
    Text_XTcl -> [ "tcl", "tk" ]
    Text_XTex -> [ "tex", "ltx", "sty", "cls" ]
    Text_XUil -> [ "uil" ]
    Text_XUuencode -> [ "uu", "uue" ]
    Text_XVcalendar -> [ "vcs" ]
    Text_XVcard -> [ "vcf" ]
    Text_Xml -> [ "xml" ]
    Text_XmlExternalParsedEntity -> []
    Unknown_Unknown -> []
    Video_1dInterleavedParityfec -> []
    Video_3gpp -> [ "3gp" ]
    Video_3gpp2 -> [ "3g2" ]
    Video_3gppTt -> []
    Video_Animaflex -> [ "afl" ]
    Video_Avi -> [ "avi" ]
    Video_AvsVideo -> [ "avs" ]
    Video_Bmpeg -> []
    Video_Bt656 -> []
    Video_Celb -> []
    Video_Dl -> [ "dl" ]
    Video_Dv -> []
    Video_Example -> []
    Video_Flc -> [ "flc", "fli" ]
    Video_Fli -> [ "flc", "fli" ]
    Video_Gl -> [ "gl" ]
    Video_H261 -> [ "h261" ]
    Video_H263 -> [ "h263" ]
    Video_H2631998 -> []
    Video_H2632000 -> []
    Video_H264 -> [ "h264" ]
    Video_H264Rcdo -> []
    Video_H264Svc -> []
    Video_Jpeg -> [ "jpgv" ]
    Video_Jpeg2000 -> []
    Video_Jpm -> [ "jpm", "jpgm" ]
    Video_Mj2 -> [ "mj2", "mjp2" ]
    Video_Mp1s -> []
    Video_Mp2p -> []
    Video_Mp2t -> [ "ts" ]
    Video_Mp4 -> [ "mp4", "mp4v", "mpg4" ]
    Video_Mp4vEs -> []
    Video_Mpeg -> [ "mpeg", "mpg", "mpe", "m1v", "m2v", "mp2", "mp3", "mpa", "mpv2" ]
    Video_Mpeg4Generic -> []
    Video_Mpv -> []
    Video_Msvideo -> [ "avi" ]
    Video_Nv -> []
    Video_Ogg -> [ "ogv" ]
    Video_Parityfec -> []
    Video_Pointer -> []
    Video_Quicktime -> [ "qt", "moov", "mov" ]
    Video_Raw -> []
    Video_RtpEncAescm128 -> []
    Video_Rtx -> []
    Video_Smpte292m -> []
    Video_Ulpfec -> []
    Video_Vc1 -> []
    Video_Vdo -> [ "vdo" ]
    Video_Vivo -> [ "viv", "vivo" ]
    Video_VndCctv -> []
    Video_VndDeceHd -> [ "uvh", "uvvh" ]
    Video_VndDeceMobile -> [ "uvm", "uvvm" ]
    Video_VndDeceMp4 -> []
    Video_VndDecePd -> [ "uvp", "uvvp" ]
    Video_VndDeceSd -> [ "uvs", "uvvs" ]
    Video_VndDeceVideo -> [ "uvv", "uvvv" ]
    Video_VndDirectvMpeg -> []
    Video_VndDirectvMpegTts -> []
    Video_VndDlnaMpegTts -> []
    Video_VndDvbFile -> [ "dvb" ]
    Video_VndFvt -> [ "fvt" ]
    Video_VndHnsVideo -> []
    Video_VndIptvforum1dparityfec1010 -> []
    Video_VndIptvforum1dparityfec2005 -> []
    Video_VndIptvforum2dparityfec1010 -> []
    Video_VndIptvforum2dparityfec2005 -> []
    Video_VndIptvforumTtsavc -> []
    Video_VndIptvforumTtsmpeg2 -> []
    Video_VndMotorolaVideo -> []
    Video_VndMotorolaVideop -> []
    Video_VndMpegurl -> [ "mxu", "m4u" ]
    Video_VndMsPlayreadyMediaPyv -> [ "pyv" ]
    Video_VndMts -> []
    Video_VndNokiaInterleavedMultimedia -> []
    Video_VndNokiaVideovoip -> []
    Video_VndObjectvideo -> []
    Video_VndRnRealvideo -> [ "rv" ]
    Video_VndSealedMpeg1 -> []
    Video_VndSealedMpeg4 -> []
    Video_VndSealedSwf -> []
    Video_VndSealedmediaSoftsealMov -> []
    Video_VndUvvuMp4 -> [ "uvu", "uvvu" ]
    Video_VndVivo -> [ "viv", "vivo" ]
    Video_Vosaic -> [ "vos" ]
    Video_Webm -> [ "webm" ]
    Video_XAmtDemorun -> [ "xdr" ]
    Video_XAmtShowrun -> [ "xsr" ]
    Video_XAtomic3dFeature -> [ "fmf" ]
    Video_XDl -> [ "dl" ]
    Video_XDv -> [ "dif", "dv" ]
    Video_XF4v -> [ "f4v" ]
    Video_XFli -> [ "fli" ]
    Video_XFlv -> [ "flv" ]
    Video_XGl -> [ "gl" ]
    Video_XIsvideo -> [ "isu" ]
    Video_XLaAsf -> [ "lsf", "lsx" ]
    Video_XM4v -> [ "m4v" ]
    Video_XMatroska -> [ "mkv", "mk3d", "mks" ]
    Video_XMng -> [ "mng" ]
    Video_XMotionJpeg -> [ "mjpg" ]
    Video_XMpeg -> [ "mp2", "mp3" ]
    Video_XMpeq2a -> [ "mp2" ]
    Video_XMsAsf -> [ "asf", "asx", "asr" ]
    Video_XMsAsfPlugin -> [ "asx" ]
    Video_XMsVob -> [ "vob" ]
    Video_XMsWm -> [ "wm" ]
    Video_XMsWmv -> [ "wmv" ]
    Video_XMsWmx -> [ "wmx" ]
    Video_XMsWvx -> [ "wvx" ]
    Video_XMsvideo -> [ "avi" ]
    Video_XQtc -> [ "qtc" ]
    Video_XScm -> [ "scm" ]
    Video_XSgiMovie -> [ "movie", "mv" ]
    Video_XSmv -> [ "smv" ]
    Windows_Metafile -> [ "wmf" ]
    Www_Mime -> [ "mime" ]
    XConference_XCooltalk -> [ "ice" ]
    XMusic_XMidi -> [ "mid", "midi" ]
    XWorld_X3dmf -> [ "3dm", "3dmf", "qd3", "qd3d" ]
    XWorld_XSvr -> [ "svr" ]
    XWorld_XVrml -> [ "vrml", "wrl", "wrz", "flr", "xaf", "xof", "vrm" ]
    XWorld_XVrt -> [ "vrt" ]
    Xgl_Drawing -> [ "xgz" ]
    Xgl_Movie -> [ "xmz" ]
