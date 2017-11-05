program LazEmuteca;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,
  runtimetypeinfocontrols,
  PascalScriptFCL,
  PascalScriptLCL,
  // CHX units
  uCHXDlgUtils,
  // CHX abstract classes
  uaCHXConfig,
  uaCHXStorable,
  // CHX frames
  ufCHXChkLstPropEditor,
  ufCHXFrame,
  ufCHXImgListPreview,
  ufCHXImgViewer,
  ufCHXListPreview,
  ufCHXMultiFolderEditor,
  ufCHXPropEditor,
  ufCHXScriptManager,
  ufCHXStrLstPreview,
  ufCHXTagTree,
  ufCHXTxtListPreview,
  // CHX Script Engine
  ufSMAskMultiFile,
  // Emuteca units
  // Emuteca abstract classes
  uaEmutecaCustomGroup,
  uaEmutecaCustomManager,
  uaEmutecaCustomSoft,
  uaEmutecaCustomSystem,
  // Emuteca classes
  ucEmuteca,
  ucEmutecaConfig,
  ucEmutecaEmulator,
  ucEmutecaEmulatorList,
  ucEmutecaEmulatorManager,
  ucEmutecaGroup,
  ucEmutecaGroupList,
  ucEmutecaGroupManager,
  ucEmutecaScriptEngine,
  ucEmutecaSoftList,
  ucEmutecaSoftManager,
  ucEmutecaSoftware,
  ucEmutecaSystem,
  ucEmutecaSystemList,
  ucEmutecaSystemManager,
  // Emuteca frames
  ufEmutecaActAddFolder,
  ufEmutecaActAddSoft,
  ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData,
  ufEmutecaEmulatorEditor,
  ufEmutecaGroupCBX,
  ufEmutecaGroupEditor,
  ufEmutecaSoftEditor,
  ufEmutecaSoftTree,
  ufEmutecaSystemCBX,
  ufEmutecaSystemEditor,
  ufEmutecaSystemImgEditor,
  ufEmutecaSystemITFEditor,
  ufEmutecaSystemMVFEditor,
  // Emuteca threads
  utEmutecaGetSoftSHA1,
  // LazEmuteca units
  uGUIConfig,
  uLEmuTKCommon,
  // Lazmuteca abstract classes
  uafLEmuTKSoftFoldersPreview,
  // Lazmuteca classes
  // LazEmuteca frames
  ufESMGroupList,
  ufESMSoftList,
  ufLEmuTKEmuManager,
  ufLEmuTKFullEmuEditor,
  ufLEmuTKFullSoftEditor,
  ufLEmuTKFullSystemEditor,
  ufLEmuTKIcnSoftTree,
  ufLEmuTKIcnSysCBX,
  ufLEmuTKMain,
  ufLEmuTKMediaManager,
  ufLEmuTKScriptManager,
  ufLEmuTKSoftImgPreview,
  ufLEmuTKSoftMedia,
  ufLEmuTKSoftTxtPreview,
  ufLEmuTKSysManager,
  ufLEmuTKSysPreview,
  // LazEmuteca forms
  ufrLEmuTKAbout,
  ufrLEmuTKMain,
  // LazEmuteca threads
  utLEmuTKCacheGrpIcons,
  utLEmuTKCacheSysIcons, uPSI_CHXBasic;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmLEmuTKMain, frmLEmuTKMain);
  Application.Run;
end.
