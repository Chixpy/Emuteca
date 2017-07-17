program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, ucEmutecaGroup, ucEmutecaConfig, ufrLEmuTKMain,
  ucEmutecaEmulatorManager, ucEmutecaGroupManager, ucEmutecaSystemManager,
  ucEmutecaEmulator, ufEmutecaSoftListOld, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaScriptManager, ucEmutecaSoftManager, uGUIConfig, uaCHXStorable,
  uaCHXConfig, ufEmutecaSystemImgEditor, ucEmutecaScriptEngine, ufESMSoftList,
ufEmutecaSystemCBXOld, ufESMGroupList, ufLEmuTKChkSoftList, ufEmutecaGroupCBXOld,
  ufLEmuTKIcnSoftList, ufEmutecaSystemEditor,
  ufLEmuTKFullSystemEditor, ufCHXChkLstPropEditor, ufLEmuTKSysManager,
  ufLEmuTKEmuManager, ufEmutecaSoftEditor, ufLEmuTKFullEmuEditor,
  ufEmutecaEmulatorEditor, 
ufLEmuTKSoftMediaOld, ufEmutecaActAddSoft, ufLEmuTKMain,
  ufLEmuTKSoftImgPreview,
  ufLEmuTKSoftTxtPreview, ufCHXForm, ufLEmuTKIcnGrpList, ufrLEmuTKExportData,
  ufLEmuTKExportData, ufEmutecaActAddFolder, uCHXDlgUtils, ucEmutecaSoftList,
  ucEmutecaSystemList, ucEmutecaGroupList, ucEmutecaEmulatorList, 
uaEmutecaCustomGroup, uaEmutecaCustomSoft, uaEmutecaCustomSystem, 
ufrLEmuTKAbout, ufLEmuTKIcnSysCBX, ufCHXFrame, 
ufCHXPropEditor, ufEmutecaSoftTree, ufLEmuTKIcnSoftTree, ufCHXImgViewer, 
ufCHXTagTree, ufEmutecaSystemPanel, ufLEmuTKPreviewList;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmLEmuTKMain, frmLEmuTKMain);
  Application.Run;
end.

